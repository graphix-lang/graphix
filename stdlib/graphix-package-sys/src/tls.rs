use crate::{Halves, StreamKind, get_stream, wrap_tls};
use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::{
    ExecCtx, Rt, UserEvent, errf,
    image::{self, ImageBuf},
};
use graphix_package_core::{
    CachedArgsAsync, CachedVals, Calls, EvalCachedAsync, ImageState,
    TLS_HANDSHAKE_TIMEOUT,
};
use netidx_core::pack::PackError;
use netidx_value::Value;
use std::{
    ptr,
    sync::{Arc, Weak},
};
use tokio::time::timeout;
use tokio_rustls::{TlsAcceptor, TlsConnector};

/// A handshake past `TLS_HANDSHAKE_TIMEOUT`: the stream is dropped.
fn timed_out() -> Value {
    errf!(
        "TLSError",
        "TLS handshake timed out after {}s",
        TLS_HANDSHAKE_TIMEOUT.as_secs()
    )
}

/// The stream a site last started a handshake on: a site upgrades each
/// stream once, so a new certificate, key or CA applies to later streams
/// and a wake restarts nothing.
#[derive(Debug, Default)]
// CR claude for eric: [bug/medium] A sleep of the arm holding sys::tls::accept or
// connect destroys the socket. `CachedArgsAsync::sleep` aborts the eval in flight. By
// then `TlsAcceptEv::eval` / `TlsConnectEv::eval` has already moved the TCP stream out
// of its `Halves`, so dropping the handshake future closes the socket, and the peer's
// handshake ends in EOF. At the wake `Started::take_new` sees the same stream and
// starts nothing, so the site never answers, and the empty `Halves` answers "stream
// unavailable" to every other holder. A half-done handshake cannot hand the TCP stream
// back, so a drop guard alone does not fix it. The choice is either an async mode whose
// sleep lets the eval finish and drops only the reply, or a rule that a sleep spends
// the stream and the site answers an error. Whichever it is should also cover
// `FileSeekEv`, `FileFstatEv` and `FileTruncateEv` in fs.rs. They have the same
// take…await…put window, and an abort inside it drops the file for every holder. Probe:
// design/review-2026-10-12/repro/x-concurrency-02.sh. (x-concurrency-02)
struct Started(Weak<Halves>);

impl Started {
    /// The stream argument, when it is one this site has not started on.
    fn take_new(&mut self, cached: &CachedVals) -> Option<Arc<Halves>> {
        let sv = get_stream(cached, 2)?;
        if ptr::eq(self.0.as_ptr(), Arc::as_ptr(&sv)) {
            return None;
        }
        self.0 = Arc::downgrade(&sv);
        Some(sv)
    }
}

/// A started handshake exists only once a cycle has run.
macro_rules! started_image_state {
    ($($t:ident),*) => {$(
        impl ImageState for $t {
            fn image_encode(&self, _buf: &mut ImageBuf) -> Result<(), PackError> {
                if self.0.0.strong_count() > 0 {
                    return Err(PackError::Application(image::NOT_QUIESCENT));
                }
                Ok(())
            }

            fn image_decode<R: Rt, E: UserEvent>(
                _ctx: &mut ExecCtx<'_, R, E>,
                _buf: &mut &[u8],
            ) -> Result<Self, PackError> {
                Ok(Self::default())
            }
        }
    )*};
}

started_image_state!(TlsConnectEv, TlsAcceptEv);

#[derive(Debug, Default)]
pub(crate) struct TlsConnectEv(Started);

impl EvalCachedAsync for TlsConnectEv {
    type Args = (Option<Bytes>, ArcStr, Arc<Halves>);

    const NAME: &str = "sys_tls_connect";
    const CALLS: Calls = Calls::Concurrent;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let ca_cert = match cached.0.first()? {
            None => return None,
            Some(Value::Null) => None,
            // CR claude for claude: [risk/low] A ca_cert that does not cast to bytes
            // becomes `None`, and `None` means "trust the webpki roots". A pinned CA
            // that cannot be read therefore stops pinning with no error: this fails
            // open. The signature `[bytes, null]` keeps it unreachable today, but the
            // security property should not depend on that. Match `Some(Value::Bytes(b))
            // => Some(b.clone())` and make any other value a `TLSError` ("ca_cert must
            // be bytes"), as an unparsable PEM already is. (x-security-09)
            Some(v) => v.clone().cast_to::<Bytes>().ok(),
        };
        let hostname = cached.get::<ArcStr>(1)?;
        let sv = self.0.take_new(cached)?;
        Some((ca_cert, hostname, sv))
    }

    fn eval((ca_cert, hostname, sv): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let tcp = match sv.take().await {
                Some(StreamKind::Tcp(tcp)) => tcp,
                Some(other) => {
                    sv.put(other).await;
                    return errf!("TLSError", "stream is not a plain TCP stream");
                }
                None => return errf!("TLSError", "stream unavailable"),
            };
            let mut root_store = rustls::RootCertStore::empty();
            match &ca_cert {
                Some(pem) => {
                    let certs: Vec<_> = match rustls_pemfile::certs(&mut &**pem).collect()
                    {
                        Ok(c) => c,
                        Err(e) => {
                            sv.put(StreamKind::Tcp(tcp)).await;
                            return errf!("TLSError", "invalid ca_cert PEM: {e}");
                        }
                    };
                    for cert in certs {
                        if let Err(e) = root_store.add(cert) {
                            sv.put(StreamKind::Tcp(tcp)).await;
                            return errf!("TLSError", "invalid CA cert: {e}");
                        }
                    }
                }
                None => {
                    root_store.extend(webpki_roots::TLS_SERVER_ROOTS.iter().cloned());
                }
            }
            let config = Arc::new(
                rustls::ClientConfig::builder()
                    .with_root_certificates(root_store)
                    .with_no_client_auth(),
            );
            let connector = TlsConnector::from(config);
            let server_name = match rustls::pki_types::ServerName::try_from(
                hostname.as_str().to_owned(),
            ) {
                Ok(sn) => sn,
                Err(e) => {
                    sv.put(StreamKind::Tcp(tcp)).await;
                    return errf!("TLSError", "invalid hostname: {e}");
                }
            };
            match timeout(TLS_HANDSHAKE_TIMEOUT, connector.connect(server_name, tcp))
                .await
            {
                // The upgrade CONSUMES the TCP handle: the session moves into its
                // own handle and the caller's `TcpStream` is left empty, so a stray
                // plaintext read errors. A failed handshake spends the socket.
                Ok(Ok(tls_stream)) => {
                    wrap_tls(StreamKind::Tls(tokio_rustls::TlsStream::Client(tls_stream)))
                }
                Ok(Err(e)) => errf!("TLSError", "TLS handshake failed: {e}"),
                Err(_) => timed_out(),
            }
        }
    }
}

pub(crate) type TlsConnect = CachedArgsAsync<TlsConnectEv>;

#[derive(Debug, Default)]
pub(crate) struct TlsAcceptEv(Started);

impl EvalCachedAsync for TlsAcceptEv {
    type Args = (Bytes, Bytes, Arc<Halves>);

    const NAME: &str = "sys_tls_accept";
    const CALLS: Calls = Calls::Concurrent;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let cert = cached.get::<Bytes>(0)?;
        let key = cached.get::<Bytes>(1)?;
        let sv = self.0.take_new(cached)?;
        Some((cert, key, sv))
    }

    fn eval((cert_pem, key_pem, sv): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let certs: Vec<_> = match rustls_pemfile::certs(&mut &*cert_pem).collect() {
                Ok(c) => c,
                Err(e) => return errf!("TLSError", "invalid cert PEM: {e}"),
            };
            let key = match rustls_pemfile::private_key(&mut &*key_pem) {
                Ok(Some(k)) => k,
                Ok(None) => return errf!("TLSError", "no private key found in key PEM"),
                Err(e) => return errf!("TLSError", "invalid key PEM: {e}"),
            };
            let config = match rustls::ServerConfig::builder()
                .with_no_client_auth()
                .with_single_cert(certs, key)
            {
                Ok(c) => c,
                Err(e) => return errf!("TLSError", "TLS config error: {e}"),
            };
            let acceptor = TlsAcceptor::from(Arc::new(config));
            let tcp = match sv.take().await {
                Some(StreamKind::Tcp(tcp)) => tcp,
                Some(other) => {
                    sv.put(other).await;
                    return errf!("TLSError", "stream is not a plain TCP stream");
                }
                None => return errf!("TLSError", "stream unavailable"),
            };
            match timeout(TLS_HANDSHAKE_TIMEOUT, acceptor.accept(tcp)).await {
                // consumes the TCP handle, as `connect` does
                Ok(Ok(tls_stream)) => {
                    wrap_tls(StreamKind::Tls(tokio_rustls::TlsStream::Server(tls_stream)))
                }
                Ok(Err(e)) => errf!("TLSError", "TLS accept failed: {e}"),
                Err(_) => timed_out(),
            }
        }
    }
}

pub(crate) type TlsAccept = CachedArgsAsync<TlsAcceptEv>;

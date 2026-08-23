use crate::{StreamKind, get_stream, wrap_tls};
use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::errf;
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx_value::Value;
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio_rustls::{TlsAcceptor, TlsConnector};

// ── TlsConnect ────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TlsConnectEv;

impl EvalCachedAsync for TlsConnectEv {
    type Args = (Option<Bytes>, ArcStr, Arc<Mutex<Option<StreamKind>>>);

    const NAME: &str = "sys_tls_connect";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let ca_cert = match cached.0.first()? {
            None => return None,
            Some(Value::Null) => None,
            Some(v) => v.clone().cast_to::<Bytes>().ok(),
        };
        let hostname = cached.get::<ArcStr>(1)?;
        let sv = get_stream(cached, 2)?;
        Some((ca_cert, hostname, sv))
    }

    fn eval((ca_cert, hostname, sv): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let tcp = {
                let mut guard = sv.lock().await;
                match guard.take() {
                    Some(StreamKind::Tcp(tcp)) => tcp,
                    Some(other) => {
                        *guard = Some(other);
                        return errf!("TLSError", "stream is not a plain TCP stream");
                    }
                    None => return errf!("TLSError", "stream unavailable"),
                }
            };
            let mut root_store = rustls::RootCertStore::empty();
            match &ca_cert {
                Some(pem) => {
                    let certs: Vec<_> = match rustls_pemfile::certs(&mut &**pem).collect()
                    {
                        Ok(c) => c,
                        Err(e) => {
                            *sv.lock().await = Some(StreamKind::Tcp(tcp));
                            return errf!("TLSError", "invalid ca_cert PEM: {e}");
                        }
                    };
                    for cert in certs {
                        if let Err(e) = root_store.add(cert) {
                            *sv.lock().await = Some(StreamKind::Tcp(tcp));
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
                    *sv.lock().await = Some(StreamKind::Tcp(tcp));
                    return errf!("TLSError", "invalid hostname: {e}");
                }
            };
            match connector.connect(server_name, tcp).await {
                // The upgrade CONSUMES the TCP handle: the session
                // moves into a handle of its own and the caller's
                // `TcpStream` is left empty, so a stray plaintext
                // read on it is an error rather than a silent read of
                // the encrypted session. (The error paths above put
                // the socket back — a failed upgrade leaves the
                // caller's stream exactly as it was.)
                Ok(tls_stream) => {
                    wrap_tls(StreamKind::Tls(tokio_rustls::TlsStream::Client(tls_stream)))
                }
                Err(e) => {
                    errf!("TLSError", "TLS handshake failed: {e}")
                }
            }
        }
    }
}

pub(crate) type TlsConnect = CachedArgsAsync<TlsConnectEv>;

// ── TlsAccept ─────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TlsAcceptEv;

impl EvalCachedAsync for TlsAcceptEv {
    type Args = (Bytes, Bytes, Arc<Mutex<Option<StreamKind>>>);

    const NAME: &str = "sys_tls_accept";

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let cert = cached.get::<Bytes>(0)?;
        let key = cached.get::<Bytes>(1)?;
        let sv = get_stream(cached, 2)?;
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
            let tcp = {
                let mut guard = sv.lock().await;
                match guard.take() {
                    Some(StreamKind::Tcp(tcp)) => tcp,
                    Some(other) => {
                        *guard = Some(other);
                        return errf!("TLSError", "stream is not a plain TCP stream");
                    }
                    None => return errf!("TLSError", "stream unavailable"),
                }
            };
            match acceptor.accept(tcp).await {
                // consumes the TCP handle, as `connect` does
                Ok(tls_stream) => {
                    wrap_tls(StreamKind::Tls(tokio_rustls::TlsStream::Server(tls_stream)))
                }
                Err(e) => errf!("TLSError", "TLS accept failed: {e}"),
            }
        }
    }
}

pub(crate) type TlsAccept = CachedArgsAsync<TlsAcceptEv>;

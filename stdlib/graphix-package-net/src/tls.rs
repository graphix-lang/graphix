use arcstr::ArcStr;
use crate::tcp::{
    StreamInner, TcpStreamValue, get_stream_value, STREAM_WRAPPER,
};
use bytes::Bytes;
use graphix_compiler::errf;
use graphix_package_core::{deftype, CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx_value::Value;
use std::sync::Arc;
use tokio_rustls::{TlsConnector, TlsAcceptor};

// ── TlsConnect ────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TlsConnectEv;

impl EvalCachedAsync for TlsConnectEv {
    const NAME: &str = "net_tls_connect";
    deftype!("fn(?#ca_cert:[bytes, null], string, string) -> Result<string, `TLSError(string)>");
    type Args = (Option<Bytes>, arcstr::ArcStr, TcpStreamValue);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let ca_cert = cached.get::<Bytes>(0);
        let hostname = cached.get::<arcstr::ArcStr>(1)?;
        let sv = get_stream_value(cached, 2)?;
        Some((ca_cert, hostname, sv))
    }

    fn eval(
        (ca_cert, hostname, sv): Self::Args,
    ) -> impl Future<Output = Value> + Send {
        async move {
            // take the plain stream out of the mutex
            let tcp = {
                let mut guard = sv.stream.lock().await;
                match guard.take() {
                    Some(StreamInner::Plain(tcp)) => tcp,
                    Some(other) => {
                        *guard = Some(other);
                        return errf!("TLSError", "stream is already TLS");
                    }
                    None => return errf!("TLSError", "stream unavailable"),
                }
            };
            // build root cert store
            let mut root_store =
                rustls::RootCertStore::empty();
            match &ca_cert {
                Some(pem) => {
                    let certs: Vec<_> =
                        match rustls_pemfile::certs(&mut &**pem).collect() {
                            Ok(c) => c,
                            Err(e) => {
                                // put the stream back
                                *sv.stream.lock().await =
                                    Some(StreamInner::Plain(tcp));
                                return errf!(
                                    "TLSError",
                                    "invalid ca_cert PEM: {e}"
                                );
                            }
                        };
                    for cert in certs {
                        if let Err(e) = root_store.add(cert) {
                            *sv.stream.lock().await =
                                Some(StreamInner::Plain(tcp));
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
                    *sv.stream.lock().await = Some(StreamInner::Plain(tcp));
                    return errf!("TLSError", "invalid hostname: {e}");
                }
            };
            match connector.connect(server_name, tcp).await {
                Ok(tls_stream) => {
                    *sv.stream.lock().await = Some(StreamInner::Tls(
                        tokio_rustls::TlsStream::Client(tls_stream),
                    ));
                    STREAM_WRAPPER.wrap(sv)
                }
                Err(e) => {
                    // stream is consumed on error, can't put it back
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
    const NAME: &str = "net_tls_accept";
    deftype!("fn(#cert:bytes, #key:bytes, string) -> Result<string, `TLSError(string)>");
    type Args = (Bytes, Bytes, TcpStreamValue);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let cert = cached.get::<Bytes>(0)?;
        let key = cached.get::<Bytes>(1)?;
        let sv = get_stream_value(cached, 2)?;
        Some((cert, key, sv))
    }

    fn eval(
        (cert_pem, key_pem, sv): Self::Args,
    ) -> impl Future<Output = Value> + Send {
        async move {
            // parse certs and key
            let certs: Vec<_> = match rustls_pemfile::certs(&mut &*cert_pem).collect()
            {
                Ok(c) => c,
                Err(e) => return errf!("TLSError", "invalid cert PEM: {e}"),
            };
            let key = match rustls_pemfile::private_key(&mut &*key_pem) {
                Ok(Some(k)) => k,
                Ok(None) => {
                    return errf!("TLSError", "no private key found in key PEM")
                }
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
            // take the plain stream out
            let tcp = {
                let mut guard = sv.stream.lock().await;
                match guard.take() {
                    Some(StreamInner::Plain(tcp)) => tcp,
                    Some(other) => {
                        *guard = Some(other);
                        return errf!("TLSError", "stream is already TLS");
                    }
                    None => return errf!("TLSError", "stream unavailable"),
                }
            };
            match acceptor.accept(tcp).await {
                Ok(tls_stream) => {
                    *sv.stream.lock().await = Some(StreamInner::Tls(
                        tokio_rustls::TlsStream::Server(tls_stream),
                    ));
                    STREAM_WRAPPER.wrap(sv)
                }
                Err(e) => errf!("TLSError", "TLS accept failed: {e}"),
            }
        }
    }
}

pub(crate) type TlsAccept = CachedArgsAsync<TlsAcceptEv>;

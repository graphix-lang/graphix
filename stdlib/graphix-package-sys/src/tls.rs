use crate::{get_stream_value, StreamKind, StreamValue, STREAM_WRAPPER};
use arcstr::ArcStr;
use bytes::Bytes;
use graphix_compiler::errf;
use graphix_package_core::{CachedArgsAsync, CachedVals, EvalCachedAsync};
use netidx_value::Value;
use std::sync::Arc;
use tokio_rustls::{TlsAcceptor, TlsConnector};

// ── TlsConnect ────────────────────────────────────────────────

#[derive(Debug, Default)]
pub(crate) struct TlsConnectEv;

impl EvalCachedAsync for TlsConnectEv {
    const NAME: &str = "sys_tls_connect";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Option<Bytes>, ArcStr, StreamValue);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let ca_cert = match cached.0.first()? {
            None => return None,
            Some(Value::Null) => None,
            Some(v) => v.clone().cast_to::<Bytes>().ok(),
        };
        let hostname = cached.get::<ArcStr>(1)?;
        let sv = get_stream_value(cached, 2)?;
        Some((ca_cert, hostname, sv))
    }

    fn eval((ca_cert, hostname, sv): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let tcp = {
                let mut guard = sv.inner.lock().await;
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
                            *sv.inner.lock().await = Some(StreamKind::Tcp(tcp));
                            return errf!("TLSError", "invalid ca_cert PEM: {e}");
                        }
                    };
                    for cert in certs {
                        if let Err(e) = root_store.add(cert) {
                            *sv.inner.lock().await = Some(StreamKind::Tcp(tcp));
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
                    *sv.inner.lock().await = Some(StreamKind::Tcp(tcp));
                    return errf!("TLSError", "invalid hostname: {e}");
                }
            };
            match connector.connect(server_name, tcp).await {
                Ok(tls_stream) => {
                    *sv.inner.lock().await = Some(StreamKind::Tls(
                        tokio_rustls::TlsStream::Client(tls_stream),
                    ));
                    STREAM_WRAPPER.wrap(sv)
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
    const NAME: &str = "sys_tls_accept";
    const NEEDS_CALLSITE: bool = false;
    type Args = (Bytes, Bytes, StreamValue);

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let cert = cached.get::<Bytes>(0)?;
        let key = cached.get::<Bytes>(1)?;
        let sv = get_stream_value(cached, 2)?;
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
                let mut guard = sv.inner.lock().await;
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
                Ok(tls_stream) => {
                    *sv.inner.lock().await = Some(StreamKind::Tls(
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

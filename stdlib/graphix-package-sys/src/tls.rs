use crate::{BufferedRead, Halves, StreamKind, get_stream, wrap_tls};
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
    time::Duration,
};
use tokio::{net::TcpStream, time::timeout};
use tokio_rustls::{TlsAcceptor, TlsConnector};

/// A successful take empties the old handle; the returned stream owns its bytes.
async fn take_tcp(stream: &Halves) -> Result<BufferedRead<TcpStream>, Value> {
    stream.stop_lines();
    match stream.take().await {
        Some(whole) => whole
            .into_tcp()
            .ok_or_else(|| errf!("TLSError", "stream is not a plain TCP stream")),
        None => Err(errf!("TLSError", "stream unavailable")),
    }
}

async fn handshake<T, F: Future<Output = Result<T, Value>>>(
    stream: &Halves,
    limit: Duration,
    exchange: impl FnOnce(BufferedRead<TcpStream>) -> F,
) -> Result<T, Value> {
    match timeout(limit, async {
        let tcp = take_tcp(stream).await?;
        exchange(tcp).await
    })
    .await
    {
        Ok(result) => result,
        Err(_) => Err(timed_out()),
    }
}

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

/// A TLS config and the arguments it was built from, or the error they
/// gave: rebuilt only when they change, so a site's connections share its
/// session cache.
type Built<K, C> = Option<(K, Result<Arc<C>, Value>)>;

/// The config `key` builds, from `slot` when it was built from `key`.
fn built<K: PartialEq, C>(
    slot: &mut Built<K, C>,
    key: K,
    build: impl FnOnce(&K) -> Result<C, Value>,
) -> Result<Arc<C>, Value> {
    match slot {
        Some((k, c)) if *k == key => c.clone(),
        _ => {
            let c = build(&key).map(Arc::new);
            *slot = Some((key, c.clone()));
            c
        }
    }
}

/// A started handshake exists only once a cycle has run; a config is
/// built again.
macro_rules! started_image_state {
    ($($t:ident),*) => {$(
        impl ImageState for $t {
            fn image_encode(&self, _buf: &mut ImageBuf) -> Result<(), PackError> {
                if self.started.0.strong_count() > 0 {
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
pub(crate) struct TlsConnectEv {
    started: Started,
    config: Built<Option<Bytes>, rustls::ClientConfig>,
}

/// A client config trusting `ca_cert`'s certificates, else the webpki
/// roots.
fn client_config(ca_cert: &Option<Bytes>) -> Result<rustls::ClientConfig, Value> {
    let mut root_store = rustls::RootCertStore::empty();
    match ca_cert {
        Some(pem) => {
            let certs: Vec<_> = rustls_pemfile::certs(&mut &**pem)
                .collect::<Result<_, _>>()
                .map_err(|e| errf!("TLSError", "invalid ca_cert PEM: {e}"))?;
            for cert in certs {
                root_store
                    .add(cert)
                    .map_err(|e| errf!("TLSError", "invalid CA cert: {e}"))?;
            }
        }
        None => root_store.extend(webpki_roots::TLS_SERVER_ROOTS.iter().cloned()),
    }
    Ok(rustls::ClientConfig::builder()
        .with_root_certificates(root_store)
        .with_no_client_auth())
}

impl EvalCachedAsync for TlsConnectEv {
    type Args = Result<(Arc<rustls::ClientConfig>, ArcStr, Arc<Halves>), Value>;

    const NAME: &str = "sys_tls_connect";
    const CANCEL_SAFE: bool = false;
    const CALLS: Calls = Calls::Concurrent;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let ca_cert = match cached.value(0)? {
            Value::Null => None,
            Value::Bytes(b) => Some((**b).clone()),
            _ => {
                return Some(Err(errf!("TLSError", "ca_cert must be bytes or null")));
            }
        };
        let hostname = cached.get::<ArcStr>(1)?;
        let sv = self.started.take_new(cached)?;
        Some(built(&mut self.config, ca_cert, client_config).map(|c| (c, hostname, sv)))
    }

    fn eval(args: Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let (config, hostname, sv) = match args {
                Ok(args) => args,
                Err(error) => return error,
            };
            let connector = TlsConnector::from(config);
            let server_name = match rustls::pki_types::ServerName::try_from(
                hostname.as_str().to_owned(),
            ) {
                Ok(sn) => sn,
                Err(e) => {
                    return errf!("TLSError", "invalid hostname: {e}");
                }
            };
            match handshake(&sv, TLS_HANDSHAKE_TIMEOUT, |tcp| async move {
                connector
                    .connect(server_name, tcp)
                    .await
                    .map_err(|e| errf!("TLSError", "TLS handshake failed: {e}"))
            })
            .await
            {
                Ok(tls_stream) => {
                    wrap_tls(StreamKind::Tls(tokio_rustls::TlsStream::Client(tls_stream)))
                }
                Err(error) => error,
            }
        }
    }
}

pub(crate) type TlsConnect = CachedArgsAsync<TlsConnectEv>;

#[derive(Debug, Default)]
pub(crate) struct TlsAcceptEv {
    started: Started,
    config: Built<(Bytes, Bytes), rustls::ServerConfig>,
}

/// A server config presenting the certificate chain in `cert_pem` with
/// the key in `key_pem`.
fn server_config(
    (cert_pem, key_pem): &(Bytes, Bytes),
) -> Result<rustls::ServerConfig, Value> {
    let certs: Vec<_> = rustls_pemfile::certs(&mut &**cert_pem)
        .collect::<Result<_, _>>()
        .map_err(|e| errf!("TLSError", "invalid cert PEM: {e}"))?;
    let key = match rustls_pemfile::private_key(&mut &**key_pem) {
        Ok(Some(k)) => k,
        Ok(None) => return Err(errf!("TLSError", "no private key found in key PEM")),
        Err(e) => return Err(errf!("TLSError", "invalid key PEM: {e}")),
    };
    rustls::ServerConfig::builder()
        .with_no_client_auth()
        .with_single_cert(certs, key)
        .map_err(|e| errf!("TLSError", "TLS config error: {e}"))
}

impl EvalCachedAsync for TlsAcceptEv {
    type Args = (Result<Arc<rustls::ServerConfig>, Value>, Arc<Halves>);

    const NAME: &str = "sys_tls_accept";
    const CANCEL_SAFE: bool = false;
    const CALLS: Calls = Calls::Concurrent;

    fn prepare_args(&mut self, cached: &CachedVals) -> Option<Self::Args> {
        let cert = cached.get::<Bytes>(0)?;
        let key = cached.get::<Bytes>(1)?;
        let sv = self.started.take_new(cached)?;
        Some((built(&mut self.config, (cert, key), server_config), sv))
    }

    fn eval((config, sv): Self::Args) -> impl Future<Output = Value> + Send {
        async move {
            let acceptor = match config {
                Ok(config) => TlsAcceptor::from(config),
                Err(error) => return error,
            };
            match handshake(&sv, TLS_HANDSHAKE_TIMEOUT, |tcp| async move {
                acceptor
                    .accept(tcp)
                    .await
                    .map_err(|e| errf!("TLSError", "TLS accept failed: {e}"))
            })
            .await
            {
                Ok(tls_stream) => {
                    wrap_tls(StreamKind::Tls(tokio_rustls::TlsStream::Server(tls_stream)))
                }
                Err(error) => error,
            }
        }
    }
}

pub(crate) type TlsAccept = CachedArgsAsync<TlsAcceptEv>;

#[cfg(test)]
mod test {
    use super::*;
    use arcstr::literal;
    use netidx_value::ValArray;

    #[tokio::test]
    async fn invalid_ca_does_not_consume_stream() {
        for ca in [
            Value::Bool(false),
            Value::I64(1),
            Value::String(literal!("not a certificate")),
            Value::Array(ValArray::from([Value::I64(1)])),
        ] {
            let file = tokio::fs::File::from_std(tempfile::tempfile().unwrap());
            let stream = crate::wrap_tcp(StreamKind::File(file));
            let halves = crate::stream_of(&stream).unwrap();
            let host = Value::String(literal!("localhost"));
            let cached = CachedVals::from_values([
                Some(ca),
                Some(host.clone()),
                Some(stream.clone()),
            ]);
            let mut connect = TlsConnectEv::default();
            let args = connect.prepare_args(&cached).unwrap();
            assert_eq!(
                TlsConnectEv::eval(args).await,
                errf!("TLSError", "ca_cert must be bytes or null")
            );
            assert!(halves.reader().await.is_some());
            let cached =
                CachedVals::from_values([Some(Value::Null), Some(host), Some(stream)]);
            assert!(connect.prepare_args(&cached).is_some());
        }
    }

    /// A site's connections share one config while its CA stands, and so
    /// its session cache (sys-io-04).
    #[tokio::test]
    async fn connections_share_a_config() {
        let mut connect = TlsConnectEv::default();
        let mut config = || {
            let file = tokio::fs::File::from_std(tempfile::tempfile().unwrap());
            let stream = crate::wrap_tcp(StreamKind::File(file));
            let host = Value::String(literal!("localhost"));
            let cached =
                CachedVals::from_values([Some(Value::Null), Some(host), Some(stream)]);
            connect.prepare_args(&cached).unwrap().unwrap().0
        };
        let (first, second) = (config(), config());
        assert!(Arc::ptr_eq(&first, &second));
    }

    #[tokio::test]
    async fn handshake_deadline_includes_stream_lock() {
        let file = tokio::fs::File::from_std(tempfile::tempfile().unwrap());
        let stream = Halves::new(StreamKind::File(file));
        let _reader = stream.reader().await;
        let result = timeout(
            Duration::from_millis(100),
            handshake(&stream, Duration::from_millis(5), |_| std::future::ready(Ok(()))),
        )
        .await
        .expect("stream acquisition exceeded the handshake deadline");
        assert_eq!(result.unwrap_err(), timed_out());
    }
}

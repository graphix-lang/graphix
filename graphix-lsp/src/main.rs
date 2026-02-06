mod convert;
mod handlers;
mod server;
mod state;

use anyhow::Result;
use log::info;
use lsp_server::Connection;

fn main() -> Result<()> {
    env_logger::init();
    info!("Starting graphix-lsp");

    let (connection, io_threads) = Connection::stdio();
    server::run(connection)?;
    io_threads.join()?;

    info!("graphix-lsp shutting down");
    Ok(())
}

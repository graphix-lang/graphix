mod fs;

#[cfg(test)]
mod test;

graphix_derive::defpackage! {
    builtins => [
        fs::GxTempDir,
        fs::JoinPath,
        fs::metadata::IsFile,
        fs::metadata::IsDir,
        fs::metadata::Metadata,
        fs::watch::SetGlobals,
        fs::watch::WatchBuiltIn,
        fs::watch::WatchFullBuiltIn,
        fs::file::ReadAll,
        fs::file::ReadAllBin,
        fs::file::WriteAll,
        fs::file::WriteAllBin,
        fs::file::RemoveFile,
        fs::dir::ReadDir,
        fs::dir::CreateDir,
        fs::dir::RemoveDir,
    ],
}

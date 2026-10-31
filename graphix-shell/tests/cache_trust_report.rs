//! A cache the current user cannot trust is not used, and the run says so
//! on stderr once, whatever it refused, since a log is the only other
//! place that tells why every start is cold.
#![cfg(unix)]

mod common;
use common::graphix;
use std::{fs, os::unix::fs::PermissionsExt};

const PROGRAM: &str = "sys::exit(sys::time::after_idle(duration:10.ms, 0))\n";
const REPORT: &str = "not using the image cache";

#[test]
fn an_untrusted_cache_is_reported_once() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cache = dir.path().join("cache");
    fs::create_dir(&cache).expect("create the cache");
    fs::set_permissions(&cache, fs::Permissions::from_mode(0o777)).expect("chmod");
    let ran = graphix(dir.path(), PROGRAM, &[], &[]);
    assert!(ran.status.success(), "{}", ran.stderr);
    assert_eq!(ran.stderr.matches(REPORT).count(), 1, "{}", ran.stderr);
    assert!(ran.stderr.contains("must not be writable by others"), "{}", ran.stderr);
}

#[test]
fn a_group_writable_cache_base_is_quiet() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cache = dir.path().join("cache");
    fs::create_dir(&cache).expect("create the cache");
    fs::set_permissions(&cache, fs::Permissions::from_mode(0o775)).expect("chmod");
    for _ in 0..2 {
        let ran = graphix(dir.path(), PROGRAM, &[], &[]);
        assert!(ran.status.success(), "{}", ran.stderr);
        assert!(!ran.stderr.contains(REPORT), "{}", ran.stderr);
    }
    assert!(cache.join("graphix").is_dir());
}

#[test]
fn a_trusted_cache_is_quiet() {
    let dir = tempfile::tempdir().expect("tempdir");
    for _ in 0..2 {
        let ran = graphix(dir.path(), PROGRAM, &[], &[]);
        assert!(ran.status.success(), "{}", ran.stderr);
        assert!(!ran.stderr.contains(REPORT), "{}", ran.stderr);
    }
}

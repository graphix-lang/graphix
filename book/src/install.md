# Installing Graphix

To install the Graphix shell from source you need to install a rust build
environment. See [here](https://www.rust-lang.org/tools/install) for
instructions on how to do that for your platform. Once you have that set up, you
can just run

`cargo install graphix-shell`

That should build the `graphix` command and install it in your
~/.cargo/bin directory. Windows and Mac OS should work out of the box
as long as you have the prerequisites for rust installed.

## Linux Prerequisites

### Debian/Ubuntu

You need to install

- clang
- libkrb5-dev

### On Fedora

You need to install

- clang-devel
- krb5-devel

## Netidx

Graphix uses netidx to import and export data streams. Netidx works with zero
configuration for local use on a single machine - separate Graphix processes can
communicate with each other out of the box.

For more advanced setups involving multiple machines, authentication, or custom
resolver configurations, see the [netidx book](https://netidx.github.io/netidx-book)
for details on setting up a netidx environment.

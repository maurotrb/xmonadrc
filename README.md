# XMonad Configuration

This repository contains my [XMonad] [] configuration.
It is based on [Peter J. Jones] [] configuration.

## Using this Configuration

If you are interested in using `cabal` to build your XMonad
configuration then you might also be interested in the following
outline of the steps I take to build, install, and restart XMonad.

  1. Install the [Haskell Platform] [].

  2. Install `cabal-install >= 1.10`:

        $ cabal update
        $ cabal install cabal-install

  3. Use the `GNUmakefile` and `xmonadrc.cabal` files in this
     directory to build your XMonad configuration.

        $ make

  4. Install and restart XMonad (assumes XMonad is currently running):

        $ make restart

The details about how this works are in the `GNUmakefile` (correctly
naming the xmonad executable, installing, restarting) and
`xmonadrc.cabal` (which source files are included, build dependencies,
etc.)

[xmonad]: http://xmonad.org/
[haskell platform]: http://www.haskell.org/platform/
[peter j. jones]: https://github.com/pjones/xmonadrc

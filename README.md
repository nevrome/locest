[![CI](https://github.com/nevrome/locest/actions/workflows/main.yml/badge.svg?branch=main)](https://github.com/nevrome/locest/actions/workflows/main.yml)
[![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/nevrome/locest?include_prereleases)](https://github.com/nevrome/locest/releases)

# locest

This command line tool implements spatiotemporal interpolation and probabilistic similarity search of and in dependent variable fields from archaeological space-time observations. It serves as a platform to develop experimental applications beyond the functionality implemented and published in the [**mobest**](https://github.com/nevrome/mobest) R package.

:warning: For the moment locest is work-in-progress, has not yet reached a fully stable interface, and is not well documented.

### Setup

To install the development version of locest you can follow these steps:

1. Install implementations of the C libraries BLAS, LAPACK and GSL.
2. Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/README/). It is recommended to do this with [GHCup](https://www.haskell.org/ghcup/).
3. Clone this repository.
4. Execute `stack install` inside the repository to build the tool and copy the `locest` executable to `~/.local/bin` (which you may want to add to your path).

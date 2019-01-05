.githooks
=========
This directory contains git hook scripts. It can be installed by running
`make init` at root of the repository, or equivalently
`git config core.hooksPath .githooks`.

The [pre-commit](pre-commit) hook increments the `*parsley-version*` number
defined in [version.rkt](../parsley/version.rkt).
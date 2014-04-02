================
~~ tetrapak_deps
================

tetrapak_deps is a plugin for managing dependencies beetween Erlang/OTP applications. This is alpha software. Feel free open an issue.

Installation
============

Ensure you have installed erlang ( >= R15 ), git and curl

Oneliner:

    curl https://raw.github.com/travelping/tetrapak_deps/master/installer.sh | sh

For people, that have a problem with cloning repos with sudo, please use cloner.sh, build_install.sh extern and
see installer.sh for more information on use.

Format
======

    [dev]
    deps = [
        {yang,      {github, "travelping/yang", "7454882e0040298cc7708b6e7e32c7d9ea9eec3e"}}
    ]

It is possible to define shortcuts in tetrapak_deps.app configuration or in application configuration:

    [dev]
    shortcuts = [{github, git, "https://github.com/"}]

At the moment only git supported.

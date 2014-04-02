#!/bin/sh
cd tetrapak
make install
curl https://raw.github.com/travelping/tetrapak_deps/master/linker.escript > linker.escript
escript linker.escript
cd ../sh
tetrapak install -local
cd ../erlsemver
tetrapak install -local
cd ../redbug
tetrapak install -local
cd ../erlgit
tetrapak install -local
cd ../tetrapak_deps
tetrapak install -local
cd ..

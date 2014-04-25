#!/bin/sh
mkdir -p temp
cd temp

git clone https://github.com/travelping/tetrapak.git
git clone https://github.com/travelping/sh.git
git clone https://github.com/travelping/erlsemver.git
git clone https://github.com/liveforeverx/redbug.git
git clone https://github.com/travelping/erlgit.git
git clone https://github.com/travelping/tetrapak_deps.git

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

cd ../..
rm -rf temp

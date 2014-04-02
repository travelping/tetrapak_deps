#!/bin/sh
mkdir -p temp
cd temp
curl https://raw.github.com/travelping/tetrapak_deps/master/cloner.sh | sh
curl https://raw.github.com/travelping/tetrapak_deps/master/build_install.sh | sh
cd ..
rm -rf temp

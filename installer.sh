mkdir -p temp
cd temp
git clone https://github.com/travelping/tetrapak.git
cd tetrapak
make install
cd ..
git clone https://github.com/travelping/sh.git
cd sh
tetrapak install -local
cd ..
git clone https://github.com/travelping/erlsemver.git
cd erlsemver
tetrapak install -local
cd ..
git clone https://github.com/liveforeverx/redbug.git
cd redbug
tetrapak install -local
cd ..
git clone https://github.com/travelping/erlgit.git
cd erlgit
tetrapak install -local
cd ..
git clone https://github.com/travelping/tetrapak_deps.git
cd tetrapak_deps
tetrapak install -local
cd ../..
rm -rf temp

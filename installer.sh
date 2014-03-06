mkdir -p temp
cd temp
git clone https://github.com/travelping/tetrapak.git
cd tetrapak
make install
cd ..
rm -fr tetrapak
git clone https://github.com/travelping/sh.git
cd sh
tetrapak install:copy -local
cd ..
rm -fr sh
git clone https://github.com/travelping/erlsemver.git
cd erlsemver
tetrapak install:copy -local
cd ..
rm -fr erlsemver
git clone https://github.com/travelping/erlgit.git
cd erlgit
tetrapak install:copy -local
cd ..
rm -fr erlgit
git clone https://github.com/travelping/tetrapak_deps.git
cd tetrapak_deps
tetrapak install:copy -local
cd ..
rm -fr tetrapak_deps
cd ..
rm -r temp

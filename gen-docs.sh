#!/bin/bash
WDIR=$(pwd)
echo Running in ${WDIR}
mkdir -p ${WDIR}/docs
mkdir -p ${WDIR}/docs/api
rm -rf ${WDIR}/docs/api

# STD
mkdir -p ${WDIR}/docs/api/std
cd ${WDIR}/docs/api/std
blc -docs ${WDIR}/lib/bl/api/std/*.bl
mv out/* .
rm -r -f out
cd ${WDIR}

# build
mkdir -p ${WDIR}/docs/api/build
cd ${WDIR}/docs/api/build
blc -docs ${WDIR}/lib/bl/api/build/*.bl
mv out/* .
rm -r -f out
cd ${WDIR}

# builtin
mkdir -p ${WDIR}/docs/api/builtin
cd ${WDIR}/docs/api/builtin
blc -docs ${WDIR}/lib/bl/api/a.bl
mv out/* .
rm -r -f out
cd ${WDIR}

# os
mkdir -p ${WDIR}/docs/api/os

mkdir -p ${WDIR}/docs/api/os/linux
cd ${WDIR}/docs/api/os/linux
blc -docs ${WDIR}/lib/bl/api/os/linux/*.bl
mv out/* .
rm -r -f out
cd ${WDIR}

mkdir -p ${WDIR}/docs/api/os/macos
cd ${WDIR}/docs/api/os/macos
blc -docs ${WDIR}/lib/bl/api/os/macos/*.bl
mv out/* .
rm -r -f out
cd ${WDIR}

mkdir -p ${WDIR}/docs/api/os/windows
cd ${WDIR}/docs/api/os/windows
blc -docs ${WDIR}/lib/bl/api/os/windows/*.bl
mv out/* .
rm -r -f out
cd ${WDIR}

mkdir -p ${WDIR}/docs/api/os/posix
cd ${WDIR}/docs/api/os/posix
blc -docs ${WDIR}/lib/bl/api/os/posix/*.bl
mv out/* .
rm -r -f out
cd ${WDIR}

# modules
mkdir -p ${WDIR}/docs/api/modules
rm ${WDIR}/docs/api/modules/*.rst

for f in lib/bl/api/modules/*/*.rst
do
	echo "Processing $f file..."
	cp ${f} ${WDIR}/docs/api/modules
done

cd ${WDIR}

#!/bin/bash
WDIR=$(pwd)
echo Running in ${WDIR}
mkdir -p docs
mkdir -p docs/API

# build
cd lib/bl/api/build
echo
echo Process $(pwd)
bdg *.bl
mkdir -p ${WDIR}/docs/API/build
mv *.md ${WDIR}/docs/API/build
cd ${WDIR}

# STD
cd lib/bl/api/std
echo
echo Process $(pwd)
bdg *.bl
mkdir -p ${WDIR}/docs/API/std
mv *.md ${WDIR}/docs/API/std
cd ${WDIR}

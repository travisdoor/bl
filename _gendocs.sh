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

# OS
cd lib/bl/api/os
echo
echo Process $(pwd)
bdg docs.txt
mkdir -p ${WDIR}/docs/API/os
mv *.md ${WDIR}/docs/API/os
cd ${WDIR}

# Examples
cd examples/new
echo
echo Process $(pwd)

mkdir -p _tmp
for f in *.bl
do
	echo "Processing $f file..."
	OUT_FILE="_tmp/$f.md"
	#echo "# $f" >> $OUT_FILE
	echo '```c' >> $OUT_FILE
  	cat $f >> $OUT_FILE
	echo '```' >> $OUT_FILE
	cat $WDIR/_disqus.html >> $OUT_FILE
done

mkdir -p ${WDIR}/docs/Examples
mv _tmp/*.md ${WDIR}/docs/Examples
rm -r -f _tmp
cd ${WDIR}

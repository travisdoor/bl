#!/bin/bash
WDIR=$(pwd)
echo Running in ${WDIR}
mkdir -p docs
mkdir -p docs/api

# build
cd lib/bl/api/build
echo
echo Process $(pwd)
bdg *.bl
mkdir -p ${WDIR}/docs/api/build
rm ${WDIR}/docs/api/build/*.rst
mv *.rst ${WDIR}/docs/api/build
cd ${WDIR}

# STD
cd lib/bl/api/std
echo
echo Process $(pwd)
bdg *.bl
mkdir -p ${WDIR}/docs/api/std
rm ${WDIR}/docs/api/std/*.rst
mv *.rst ${WDIR}/docs/api/std
cd ${WDIR}

# Builtin
cd lib/bl/api
echo
echo Process $(pwd)
bdg *.bl
mkdir -p ${WDIR}/docs/api/builtin
rm ${WDIR}/docs/api/builtin/*.rst
mv *.rst ${WDIR}/docs/api/builtin
cd ${WDIR}

# OS
cd lib/bl/api/os
echo
echo Process $(pwd)
bdg docs.txt
mkdir -p ${WDIR}/docs/api/os
rm ${WDIR}/docs/api/os/*.rst
mv *.rst ${WDIR}/docs/api/os
cd ${WDIR}

# modules
mkdir -p ${WDIR}/docs/api/modules
rm ${WDIR}/docs/api/modules/*.rst

#cp lib/bl/api/modules/fs/*.rst ${WDIR}/docs/api/modules
#cp lib/bl/api/modules/sync/*.rst ${WDIR}/docs/api/modules
#cp lib/bl/api/modules/thread/*.rst ${WDIR}/docs/api/modules
#cp lib/bl/api/modules/dl/*.rst ${WDIR}/docs/api/modules

for f in lib/bl/api/modules/*/*.rst
do
	echo "Processing $f file..."
	cp ${f} ${WDIR}/docs/api/modules
done

# examples
cd examples
echo
echo Process $(pwd)

mkdir -p _tmp
for f in *.bl
do
	echo "Processing $f file..."
	OUT_FILE="_tmp/$f.md"
	echo "#" ${f#*_} >> $OUT_FILE 
	echo '```c' >> $OUT_FILE
  	cat $f >> $OUT_FILE
	echo '```' >> $OUT_FILE
	cat $WDIR/_disqus.html >> $OUT_FILE
done

mkdir -p ${WDIR}/docs/examples
mv _tmp/*.md ${WDIR}/docs/examples
mkdir -p ${WDIR}/docs/examples/docs
cp ${WDIR}/examples/docs/*.bl ${WDIR}/docs/examples/docs
rm -r -f _tmp
cd ${WDIR}

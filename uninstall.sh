#!/bin/bash
MANIFEST_FILE=build/install_manifest.txt
echo Remove all files based on $MANIFEST_FILE
xargs rm -v < ${MANIFEST_FILE}

#!/bin/bash
rm -r -f ../build
mkdir ../build
cmake .. -B../build -DCMAKE_BUILD_TYPE=Release
sudo make install -C ../build

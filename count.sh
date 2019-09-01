#!/bin/bash
cloc . --by-file --include-lang=C,C++,C/C++\ Header --exclude-dir=build --exclude-dir=deps

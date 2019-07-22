#!/bin/bash
cloc . --include-lang=C,C/C++\ Header --exclude-dir=build --exclude-dir=deps

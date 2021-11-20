#!/bin/bash

blc --silent -run generate.bl -o "api"

pandoc  "language.md" \
        `find api -name "*.md"` \
        --toc \
        --toc-depth=2 \
        --top-level-division=chapter \
        --css styles.css \
        --metadata=title:"Biscuit Language Reference" \
        --metadata=author:"Martin Dorazil" \
        --metadata=lang:"en-US" \
        --standalone \
        -o "documentation.html"

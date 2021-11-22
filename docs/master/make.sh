#!/bin/bash

blc --silent -run generate.bl -o "tmp"

pandoc  "installation.md" \
        --toc \
        --toc-depth=2 \
        --top-level-division=chapter \
        --css ../static/styles.css \
        --metadata=title:"Biscuit Language (`blc --version`)" \
        --metadata=lang:"en-US" \
        --include-in-header="header.html" \
        --include-before-body="menu.html" \
        --standalone \
        -o "index.html"

pandoc  "compiler.md" \
        --toc \
        --toc-depth=2 \
        --top-level-division=chapter \
        --css ../static/styles.css \
        --metadata=title:"Biscuit Language (`blc --version`)" \
        --metadata=lang:"en-US" \
        --include-in-header="header.html" \
        --include-before-body="menu.html" \
        --standalone \
        -o "compiler.html"

pandoc  `find tmp -name "*.md"` \
        --toc \
        --toc-depth=2 \
        --top-level-division=chapter \
        --css ../static/styles.css \
        --metadata=title:"Biscuit Language (`blc --version`)" \
        --metadata=lang:"en-US" \
        --include-in-header="header.html" \
        --include-before-body="menu.html" \
        --standalone \
        -o "language.html"

rm -r tmp

echo DONE!

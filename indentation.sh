for p in $(find . -iname "*.bl")
do
	echo "Fix: '$p'"
    unexpand -t 4 --first-only "$p" > "$p.tab"
	mv "$p.tab" "$p"
done
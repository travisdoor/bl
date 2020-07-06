#!/bin/bash
echo 
echo "**************************"
echo "***  Compile examples  ***"
echo "**************************"
echo 
for f in ../examples/*.bl
do
	blc -no-bin -no-warning $f
done


echo 
echo "**************************"
echo "*** Running test cases ***"
echo "**************************"
echo 
blc -rt -no-warning src/main.test.bl && ./out.exe

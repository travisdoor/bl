#!/bin/bash
echo 
echo "************************"
echo "*** Running examples ***"
echo "************************"
echo 
blc -no-bin -force-test-to-llvm -run-tests -no-warning ../examples/dummy.bl


echo 
echo "**************************"
echo "*** Running test cases ***"
echo "**************************"
echo 
blc -no-bin -force-test-to-llvm -run-tests -no-warning src/test_dummy.bl

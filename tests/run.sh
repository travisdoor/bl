#!/bin/bash
echo 
echo "***********************"
echo "*** Compiling demos ***"
echo "***********************"
echo 
blc -no-bin ../demos/vulkan_demo/src/vulkan_demo.bl 
blc -no-bin ../demos/simple_sdl_game/src/skyshooter.bl 


echo 
echo "**************************"
echo "*** Running test cases ***"
echo "**************************"
echo 
blc -no-bin -force-test-to-llvm -run-tests -no-warning src/main.bl ../examples/main.bl

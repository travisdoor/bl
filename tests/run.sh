#!/bin/bash
blc -no-bin -force-test-to-llvm -run-tests -no-warning src/test_basics.bl
blc -no-bin -force-test-to-llvm -run-tests -no-warning src/test_fib.bl 
blc -no-bin -force-test-to-llvm -run-tests -no-warning src/examples/pointers.bl src/examples/variables.bl src/examples/constants.bl src/examples/named_functions.bl src/examples/anonymous_function.bl src/examples/blocks.bl src/examples/ifs.bl src/examples/loops.bl src/examples/break_continue.bl src/examples/array_type.bl src/examples/array_string.bl src/examples/type_cast.bl 

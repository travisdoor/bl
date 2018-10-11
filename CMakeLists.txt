cmake_minimum_required(VERSION 3.0)
project(bl VERSION 0.3.2 LANGUAGES C)

set(CMAKE_C_STANDARD 99)
set(HAVE_64_BIT 1)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
SET(LLVM_PATH "" CACHE STRING "Path to LLVM.")

# Set a default build type if none was specified
set(DEFAULT_BUILD_TYPE "Release")
set(CMAKE_C_FLAGS_DEBUG "-DBL_DEBUG")

if(NOT CMAKE_BUILD_TYPE)
    message(STATUS "Setting build type to '${DEFAULT_BUILD_TYPE}' as none was specified.")
    set(CMAKE_BUILD_TYPE ${DEFAULT_BUILD_TYPE})
endif()

# setup output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

# deps
find_package(BObject 1.0 REQUIRED)
find_package(LLVM REQUIRED CONFIG)

set(HEADER_FILES
        src/bldebug.h
        src/error.h 
        src/config.h
	src/messages.h
        src/builder.h
        src/blmemory.h
        src/assembly.h
        src/token.h
        src/tokens.h
        src/unit.h
        src/stages.h
        src/common.h 
        src/scope.h
        src/eval.h
        src/ast.h)

set(SOURCE_FILES
        src/main.c
        src/eval.c
        src/tokens.c
        src/file_loader.c
        src/unit.c
        src/ast_printer.c
        src/token.c
        src/token_printer.c
        src/assembly.c
        src/builder.c
        src/lexer.c
        src/bldebug.c
        src/parser.c
        src/ir.c
        src/linker.c
        src/jit_exec.c
        src/test_exec.c
        src/bc_writer.c
        src/native_bin.c
        src/checker.c
        src/scope.c
        src/common.c 
        src/post.c 
        src/ast.c)

add_executable(blc ${SOURCE_FILES} ${HEADER_FILES})

if (MSVC)
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /O2 /wd4996 /MDd")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} /Od /Z7 /wd4996 /MDd")

    # Link LLVM from custom path
    include_directories(${LLVM_PATH}/include)
    link_directories(${LLVM_PATH}/lib)

    target_link_libraries(blc PUBLIC
	LLVMAArch64AsmParser       
	LLVMAArch64AsmPrinter      
	LLVMAArch64CodeGen         
	LLVMAArch64Desc            
	LLVMAArch64Disassembler    
	LLVMAArch64Info            
	LLVMAArch64Utils           
	LLVMAMDGPUAsmParser        
	LLVMAMDGPUAsmPrinter       
	LLVMAMDGPUCodeGen          
	LLVMAMDGPUDesc             
	LLVMAMDGPUDisassembler     
	LLVMAMDGPUInfo             
	LLVMAMDGPUUtils            
	LLVMAnalysis               
	LLVMARMAsmParser           
	LLVMARMAsmPrinter          
	LLVMARMCodeGen             
	LLVMARMDesc                
	LLVMARMDisassembler        
	LLVMARMInfo                
	LLVMARMUtils               
	LLVMAsmParser              
	LLVMAsmPrinter             
	LLVMBinaryFormat           
	LLVMBitReader              
	LLVMBitWriter              
	LLVMBPFAsmParser           
	LLVMBPFAsmPrinter          
	LLVMBPFCodeGen             
	LLVMBPFDesc                
	LLVMBPFDisassembler        
	LLVMBPFInfo                
	LLVMCodeGen                
	LLVMCore                   
	LLVMCoroutines             
	LLVMCoverage               
	LLVMDebugInfoCodeView      
	LLVMDebugInfoDWARF         
	LLVMDebugInfoMSF           
	LLVMDebugInfoPDB           
	LLVMDemangle               
	LLVMDlltoolDriver          
	LLVMExecutionEngine        
	LLVMFuzzMutate             
	LLVMGlobalISel             
	LLVMHexagonAsmParser       
	LLVMHexagonCodeGen         
	LLVMHexagonDesc            
	LLVMHexagonDisassembler    
	LLVMHexagonInfo            
	LLVMInstCombine            
	LLVMInstrumentation        
	LLVMInterpreter            
	LLVMipo                    
	LLVMIRReader               
	LLVMLanaiAsmParser         
	LLVMLanaiAsmPrinter        
	LLVMLanaiCodeGen           
	LLVMLanaiDesc              
	LLVMLanaiDisassembler      
	LLVMLanaiInfo              
	LLVMLibDriver              
	LLVMLineEditor             
	LLVMLinker                 
	LLVMLTO                    
	LLVMMC                     
	LLVMMCDisassembler         
	LLVMMCJIT                  
	LLVMMCParser               
	LLVMMipsAsmParser          
	LLVMMipsAsmPrinter         
	LLVMMipsCodeGen            
	LLVMMipsDesc               
	LLVMMipsDisassembler       
	LLVMMipsInfo               
	LLVMMIRParser              
	LLVMMSP430AsmPrinter       
	LLVMMSP430CodeGen          
	LLVMMSP430Desc             
	LLVMMSP430Info             
	LLVMNVPTXAsmPrinter        
	LLVMNVPTXCodeGen           
	LLVMNVPTXDesc              
	LLVMNVPTXInfo              
	LLVMObjCARCOpts            
	LLVMObject                 
	LLVMObjectYAML             
	LLVMOption                 
	LLVMOrcJIT                 
	LLVMPasses                 
	LLVMPowerPCAsmParser       
	LLVMPowerPCAsmPrinter      
	LLVMPowerPCCodeGen         
	LLVMPowerPCDesc            
	LLVMPowerPCDisassembler    
	LLVMPowerPCInfo            
	LLVMProfileData            
	LLVMRuntimeDyld            
	LLVMScalarOpts             
	LLVMSelectionDAG           
	LLVMSparcAsmParser         
	LLVMSparcAsmPrinter        
	LLVMSparcCodeGen           
	LLVMSparcDesc              
	LLVMSparcDisassembler      
	LLVMSparcInfo              
	LLVMSupport                
	LLVMSymbolize              
	LLVMSystemZAsmParser       
	LLVMSystemZAsmPrinter      
	LLVMSystemZCodeGen         
	LLVMSystemZDesc            
	LLVMSystemZDisassembler    
	LLVMSystemZInfo            
	LLVMTableGen               
	LLVMTarget                 
	LLVMTransformUtils         
	LLVMVectorize              
	LLVMWindowsManifest        
	LLVMX86AsmParser           
	LLVMX86AsmPrinter          
	LLVMX86CodeGen             
	LLVMX86Desc                
	LLVMX86Disassembler        
	LLVMX86Info                
	LLVMX86Utils               
	LLVMXCoreAsmPrinter        
	LLVMXCoreCodeGen           
	LLVMXCoreDesc              
	LLVMXCoreDisassembler      
	LLVMXCoreInfo              
	LLVMXRay  
	Shlwapi
	)
else()
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -Wall -Wno-missing-braces -O0 -fsanitize=address -fsanitize=address -Wdeprecated-declarations")

    target_link_libraries(blc PUBLIC LLVM)
endif()

target_link_libraries(blc PUBLIC bobject)
target_include_directories(blc PRIVATE src)

configure_file(${CMAKE_SOURCE_DIR}/src/version.h.in ${CMAKE_SOURCE_DIR}/src/version.h)



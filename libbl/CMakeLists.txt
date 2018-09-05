set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_C_VISIBILITY_PRESET hidden)

# setup output directories
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

# deps
find_package(BObject 1.0 REQUIRED)
find_package(LLVM REQUIRED CONFIG)

set(HEADER_FILES
        include/bl/bl.h
        include/bl/unit.h
        include/bl/assembly.h
        include/bl/builder.h
        include/bl/bldebug.h
        include/bl/error.h 
        include/bl/config.h
	include/bl/messages.h)

set(HEADER_FILES_IMPL
        src/builder_impl.h
        src/blmemory_impl.h
        src/assembly_impl.h
        src/token_impl.h
        src/tokens_impl.h
        src/unit_impl.h
        src/stages_impl.h
        src/common_impl.h 
        src/scope_impl.h
        src/ast_impl.h)

set(SOURCE_FILES
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
        src/bc_writer.c
        src/native_bin.c
        src/checker.c
        src/scope.c
        src/common.c 
        src/post.c 
        src/ast.c)

add_library(bl SHARED ${SOURCE_FILES} ${HEADER_FILES} ${HEADER_FILES_IMPL})
target_compile_definitions(bl PRIVATE -DBL_COMPILING_DLL)

#llvm_config(bl all)
target_link_libraries(bl PUBLIC bobject)

if (MSVC)
  target_link_libraries(bl PUBLIC
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
  target_link_libraries(bl PUBLIC LLVM)
endif()


target_include_directories(bl PUBLIC include)
target_include_directories(bl PRIVATE src)

configure_file(${CMAKE_SOURCE_DIR}/libbl/include/bl/version.h.in ${CMAKE_SOURCE_DIR}/libbl/include/bl/version.h)

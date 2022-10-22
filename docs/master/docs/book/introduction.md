# Introduction

*Note that the following documentation may be in some cases incomplete and we'll try to improve it soon...*

Biscuit Language (BL) is an experimental, general-purpose, compiled or interpreted programming language inspired by new C/C++ followers such as Odin, JAI and Zig. The main goal of the language is to create a simple, explicit and pleasant environment for programmers looking for something more abstract than C but not bloated as C++. 

Due to this, some of the popular and common concepts are not supported in the language, i.e. there are no *objects*, complicated inheritance or exceptions in BL. However, some higher-level features such as polymorphic (template) functions, function overloading, runtime type information and compile-time execution are present.

BL compiler is open-source software (distributed under the MIT license) written in C. The compiler itself comes with a slowly growing _Standard_ Library* providing various functionality out of the box. The BL compiler uses *LLVM* backend to produce the final optimized executable.

Currently, all major 64-bit operating systems are supported (Windows, Linux and Mac), however, most of the language development is done on Windows and the compiler or *Standard Library* may be less reliable on other platforms. 

The following sections of this document should guide you through the language from simple, fundamental basics to more complicated concepts. We suppose the reader is familiar with other programming languages such as C or C++ and does have some basic knowledge of how to use a terminal and other programming tools. 

So let's start with *Hello World* program...

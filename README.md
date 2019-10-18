<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=BRSWZ2U7A2TXG&source=url"><img src="https://img.shields.io/badge/Donate-PayPal-green.svg"></a>

# About

The Biscuit Language (BL) is simple imperative programming language using LLVM backend implemented in C. Language syntax and all it's features are still in development and not ready for "real" use yet. Biscuit is designed to be simple, fast and explicit.

Contact email: [biscuitlang@gmail.com](mailto:biscuitlang@gmail.com)


## Goals

-   manual memory management
-   pointers
-   no exceptions
-   fast compilation
-   full compile-time execution (integrated interpreter)
-   no OOP
-   types as values in compile-time
-   use of the LLVM backend
-   multiplatform

## Example

    main :: fn () s32 {
      return fib(10);
    };
    
    fib :: fn (n: s32) s32 {
      if n == 0 || n == 1 {
        return n;
      } else {
        return fib(n-1) + fib(n-2);
      }
    
      return -1;
    };

## Instalation

   Instructions are [here](http://biscuitlang.org)

## Recent blogposts

   * [Compiler internals](http://biscuitlang.org/blogpost-18-oct-2019.html)
   * [Plans for the next version](http://biscuitlang.org/blogpost-9-oct-2019.html)




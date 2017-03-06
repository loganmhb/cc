# cc

A compiler for a tiny, nearly useless language which is a subset of Kaleidescope.

## Usage

    # Test program:
    lein run test.kl test.ll
    clang test.ll
    ./a.out
    
    # Hello world:
    lein run hello.kl hello.ll
    clang hello.ll
    ./a.out

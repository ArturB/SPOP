# SPOP

## Basic usage
 - Build project by invoking
   > stack build
 - Main executable, called wolf.exe, is defined in app/Main.hs
 - Game executable can be runned by
   > stack exec wolf -- n
 - n is initial position of the wolf. Position must be positive integer from 1 to 4 and denotes wolf position on checkerboard first row, from left to right.
 
 ## Code structure
  - Game logic is defined in src.
  - Haddock HTML reference is in doc. Update documentation by:
    - on Windows: stack haddock; cp .\.stack-work\install\d0705508\doc\SPOP-0.1.0.0 .\doc
    - on Linux:   stack haddock; cp .\.stack-work\install\x86_64-linux\doc\SPOP-0.1.0.0 .\doc
  - Package properties are defined in package.yaml. Properties include project name, structure (list of executables and source files) and dependencies. 


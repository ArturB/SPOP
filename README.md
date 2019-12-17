# SPOP

## Basic usage
 - Build project by invoking
   > stack build
 - Install main executable to PATH with 
   > stack install
 - Main executable, called wolf.exe, is defined in app/Main.hs
 - By default, running game with no parameters, starts new game with AI playing with itself. You may display available CLI options with
   > wolf --help \
   > stack exec wolf -- --help
 
 ## Code structure
  - Game logic is defined in src.
  - Haddock HTML reference is in doc. Update documentation by:
    - on Windows: stack haddock; cp -r .stack-work\install\d0705508\doc\SPOP-0.1.0.0 .\doc
    - on Linux:   stack haddock; cp -r .stack-work\install\x86_64-linux\doc\SPOP-0.1.0.0 .\doc
  - Package properties are defined in package.yaml. Properties include project name, structure (list of executables and source files) and dependencies. Adding new dependency or new source file requires to add it into package.yaml also. 


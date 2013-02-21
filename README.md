* To set up:

  You need the haskell platform: http://www.haskell.org/platform/  

  You may need to install dependencies:  
  cabal install GLUT quickcheck  

  Build where you checked out:  
  runghc Setup configure --user  
  runghc Setup build  
  runghc Setup install  

* To run:

  $HOME/.cabal/bin/demo

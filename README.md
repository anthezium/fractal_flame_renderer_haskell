* ##### What is this?
  This is a renderer for fractals written in Haskell.  It uses an Iterated Function System and color palette to render and display a fractal.

  It is an incomplete implementation of Scott Draves' flam3 algorithm, described here: http://flam3.com/flame.pdf and here: http://code.google.com/p/flam3/wiki/Introduction
  
  It's a work in progress.  I'm working towards producing equivalent results to Draves' flam3-render. 

* ##### To set up:
  ###### You need the haskell platform (available here): 
    http://www.haskell.org/platform/  

  ###### On Ubuntu, you can also install the haskell platform via packages:
    sudo apt-get install haskell-platform

  ###### On Ubuntu, if you've installed the haskell platform via packages (rather than from hackage), some of the dependencies are also in packages:
    sudo apt-get install libghc-mtl-dev libghc-random-dev libghc-stm-dev libghc-syb-dev libghc-transformers-dev libghc-utf8-string-dev libghc-x11-dev libghc-x11-xft-dev

  ###### You may need to install dependencies from hackage:  
    cabal install GLUT quickcheck hxt regexpr 

  ###### Build where you checked out:  
    runghc Setup configure --user  
    runghc Setup build  
    runghc Setup install  

* ##### To run:

    $HOME/.cabal/bin/demo

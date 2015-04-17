; This assumes Ubuntu LTS with sbcl package installed, and https://github.com/slime/slime cloned into ./slime.
; YMMV.  Greatly :)

(load "slime/swank-loader.lisp")
(swank-loader:init)
(swank:create-server :port 4005 :dont-close t)

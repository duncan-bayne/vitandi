(require 'asdf)

(require :sb-posix)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(ql:quickload '("drakma" "cl-json"))

(load "vitandi.asd")
(asdf:operate 'asdf:load-op 'vitandi)

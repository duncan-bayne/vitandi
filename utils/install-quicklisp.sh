#!/bin/sh

set -e

if [ ! -d ~/quicklisp ]; then
    curl -O http://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp --eval "(progn (quicklisp-quickstart:install)(quit))"
fi

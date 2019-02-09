#!/bin/bash

eval $(opam config env)
dune build main.bc.js index.html style.css
open $(pwd)/_build/default/index.html

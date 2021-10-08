#!/bin/bash

# move to location of this script
cd "$( dirname "${BASH_SOURCE[0]}" )"

# install opam depenencies
opam --yes install \
    dune base stdio ppx_jane ppx_inline_test  \
    merlin ocp-indent ocaml-lsp-server ocamlformat ocamlformat-rpc

# make user copy of main.ml from example
if [ ! -f dometyl/bin/main.ml ]
then
    cp dometyl/bin/main.ml.example dometyl/bin/main.ml
fi

# clone scad-ml into parallel directory
cd ..
if [ -d "scad-ml" ]
then
    cd scad-ml
    git pull
else
    git clone https://github.com/geoffder/scad-ml
    cd scad-ml
fi

# build and install Scad_ml module in opam switch
dune build
opam install ./scad_ml.opam

# return to dometyl and build
cd ../dometyl-keyboard/dometyl
dune build

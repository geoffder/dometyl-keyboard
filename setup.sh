#!/bin/bash

# exit on failure
set -e

# move to location of this script
base_dir="$( dirname "${BASH_SOURCE[0]}" )"
cd "$base_dir"

# auto-confirm opam prompts unless operating under cygwin
opam_exec="opam --yes"
if [ "$(expr substr $(uname -s) 1 6)" == "CYGWIN" ]
then
   opam_exec="opam"
fi

# install opam depenencies
$opam_exec --yes install \
    dune base stdio ppx_jane ppx_inline_test ppxlib \
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
    git clone https://github.com/namachan10777/scad-ml
    cd scad-ml
fi

# build and install Scad_ml module in opam switch
dune build
opam install ./scad_ml.opam

# clone ppx_deriving_scad into parallel directory
cd ..
if [ -d "ppx_deriving_scad" ]
then
    cd ppx_deriving_scad
    git pull
else
    git clone https://github.com/geoffder/ppx_deriving_scad
    cd ppx_deriving_scad
fi

# build and install the [@@deriving scad] ppx in opam switch
dune build
opam install ./ppx_deriving_scad.opam

# return to dometyl and build
cd "$base_dir"
dune build

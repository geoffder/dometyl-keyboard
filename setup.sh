#!/bin/bash

# exit on failure
set -e

# move to location of this script
base_dir="$( dirname "${BASH_SOURCE[0]}" )"
cd "$base_dir"

# auto-confirm opam prompts unless operating under cygwin
os=$(uname -s)
opam_exec="opam --yes"
if [ "${os:0:6}" == "CYGWIN" ]
then
   opam_exec="opam"
fi

# install opam depenencies
$opam_exec --yes install \
    dune base stdio ppx_jane scad_ml ppx_deriving_scad \
    merlin ocp-indent ocaml-lsp-server ocamlformat ocamlformat-rpc

# make user copy of main.ml from example
if [ ! -f dometyl/bin/main.ml ]
then
    cp dometyl/bin/main.ml.example dometyl/bin/main.ml
fi

# return to dometyl and build
cd "$base_dir"
dune build

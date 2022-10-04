#!/bin/bash

# exit on failure
set -e

# move to location of this script
base_dir="$( dirname "${BASH_SOURCE[0]}" )"
cd "$base_dir/dometyl"

# auto-confirm opam prompts unless operating under cygwin
os=$(uname -s)
opam_exec="opam --yes"
if [ "${os:0:6}" == "CYGWIN" ]
then
   opam_exec="opam"
fi

# install project dependencies
$opam_exec install ./dometyl.opam --deps-only --with-doc

# vendor scad_ml fork
git clone https://github.com/geoffder/scad-ml

# not strictly necessary, but user may want to be able to use scad_ml elsewhere
cd scad-ml
$opam_exec install ./scad_ml.opam
cd ..

# install opam dev depenencies
$opam_exec install \
    merlin ocp-indent ocaml-lsp-server ocamlformat ocamlformat-rpc

# build dometyl
dune build

cd "$base_dir"

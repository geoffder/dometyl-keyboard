#!/bin/bash

dune build @doc
if [ -d "_build/default/_doc/_html" ]
then
    rm -rf "../docs"
    cp -r "_build/default/_doc/_html" "../docs"
fi

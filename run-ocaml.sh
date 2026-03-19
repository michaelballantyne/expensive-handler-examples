#!/bin/bash
eval $(opam env --switch=default) && ocamlopt -o sieve_ocaml sieve.ml && ./sieve_ocaml
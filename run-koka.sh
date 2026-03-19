#!/bin/bash
koka --optimize=3 -o koka_sieve sieve.kk; chmod +x koka_sieve; time ./koka_sieve
#!/bin/zsh
for a in $(grep -o -E '^[a-zA-Z_0-9#]+' src/Name/Prim.hs); do
    echo -n "$a: "
    grep -l -E "\<$a\>" src/**/*.hs | grep -v '^src/Name/Prim' | wc -l
done;

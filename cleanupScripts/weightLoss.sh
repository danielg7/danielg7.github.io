#!/bin/bash

cd _posts/blog || exit

for file in *; do
  sed -i 's/weightloss/- weightloss/g' "$file"
done

#!/bin/bash

cd _posts/blog

for file in *; do
  if [[ "$file" =~ \.markdown$ ]]; then
    echo "${file::-9}"  
    # "$file" > "${file::8}.tmp"
  fi
done



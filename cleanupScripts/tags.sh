#!/bin/bash

cd _posts/blog

for file in *; do
  sed -i 's,http://www.chipoglesby.com/tag/,http://www.chipoglesby.com/tag/#,g' $file
done

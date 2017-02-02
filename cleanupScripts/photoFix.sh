#!/bin/bash

cd _posts/blog || exit

for file in *; do
  sed -i 's,http://www.chipoglesby.com/wp-content/uploads/,https://storage.googleapis.com/www.chipoglesby.com/wp-content/uploads/,g' "$file"
done


#!/usr/bin/env bash

rm -r build

mkdir build
mkdir build/js
mkdir build/img

elm-make src/Main.elm --output build/js/divine_or_benign.js

if [ $? -eq 0 ]
then
  cp ./img/* ./build/img
  cp ./src/style.css ./build/style.css
  cp ./src/htmlmain.js ./build/js/htmlmain.js
  cp ./src/index.html ./build/index.html
fi

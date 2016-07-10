#!/usr/bin/env bash

rm -r build

mkdir build

elm-make src/Main.elm --output build/js/divine_or_benign.js

if [ $? -eq 0 ]
then
  cp -R ./img ./build/
  cp -R ./src/js ./build/
  cp -R ./src/css ./build/
  cp ./src/index.html ./build/index.html
  cp ./src/favicon.png ./build/favicon.png
fi

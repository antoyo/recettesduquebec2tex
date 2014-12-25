#!/bin/bash

PROJECT_NAME="Recipes2tex"

ARGS="-Wall"

BUILD_DIR="_build"
SRC_DIR="src"

build() {
    mkdir -p $BUILD_DIR
    ghc -o $BUILD_DIR/$PROJECT_NAME -outputdir $BUILD_DIR $ARGS $SRC_DIR/*.hs
}

clean() {
    rm -Rf $BUILD_DIR
}

if [[ -z $@ ]]; then
    func=build
else
    func=$@
fi

$func

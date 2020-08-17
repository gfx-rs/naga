#/bin/bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

cargo build --all-features --example convert
CONVERT="$DIR/../../target/debug/examples/convert"

OUT_FILE=$DIR/simple.out.ron

PREFER_GLSL_NEW=1 $CONVERT $DIR/simple.vert $OUT_FILE
diff $DIR/simple-expected.ron $OUT_FILE

PREFER_GLSL_NEW=1 $CONVERT $DIR/simple.wgsl $OUT_FILE
diff $DIR/simple-expected.ron $OUT_FILE

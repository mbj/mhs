#!/bin/sh

set -e

curl --location --output stack https://github.com/commercialhaskell/stack/releases/download/v2.15.1/stack-2.15.1-linux-x86_64-bin

echo '8e094ed173bd58a1afc19abe178013c4d40317180204deb88f4ae98be12610d4  stack' | sha256sum -c

chmod +x ./stack

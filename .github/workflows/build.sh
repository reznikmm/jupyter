#!/bin/bash

# SPDX-FileCopyrightText: 2024 Max Reznik <reznikmm@gmail.com>
#
# SPDX-License-Identifier: MIT

set -x -e

TARGET=`node -e "console.log(process.platform + '-'+ process.arch)"`
echo TARGET=$TARGET
export PKG_CONFIG_PATH=$PWD/prefix/lib/pkgconfig/
cd libzmq
./autogen.sh
./configure --enable-static --disable-shared --prefix=$PWD/../prefix
make all install
cd -
alr action -r post-fetch
sed -i.bak -e '/Library_Kind/s/dynamic/static/' alire/cache/dependencies/matreshka_league_*/gnat/*.gpr
sed -i.bak -e '/libzmq.gpr/s/^/--  /'           alire/cache/dependencies/zeromq_ada_*/zmq.gpr
gprbuild -XLIBRARY_TYPE=static jupyter_ada_kernel.gpr \
  -largs `pkg-config --static --libs libzmq`
zip -j -r ada_kernel-$TARGET-${GITHUB_REF##*/}.zip .bin/ kernels/ada/

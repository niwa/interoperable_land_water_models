#!/usr/bin/env bash

echo "Building for win64"

# Directories
root="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
build_dir=$root/build
bin_dir=$root/bin
toolchain_file=$root/toolchain.cmake
source_dir="$(cd $root/../../src; pwd)"


echo "Build directory: $build_dir"
echo "Bin directory: $bin_dir"
echo "Source directory: $source_dir"
echo "Toolchain file: $toolchain_file"
echo "------------------------------------------------------------"

mkdir -p $build_dir
cd $build_dir

cmake -D CMAKE_RUNTIME_OUTPUT_DIRECTORY=$bin_dir\
      -D CMAKE_TOOLCHAIN_FILE=$toolchain_file \
      $source_dir

make
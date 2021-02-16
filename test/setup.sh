#! /usr/bin/env bash

tar xvf test/workdir.tar.gz

sudo mkdir test/workdir/only-roots-dir1
sudo touch test/workdir/only-roots-dir1/roots-file1
sudo chmod 700 test/workdir/only-roots-dir1

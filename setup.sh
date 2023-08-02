#!/bin/bash

# This is a setup script for installing muf

sudo apt install build-essential -y
sudo apt install libopenblas-dev -y
sudo apt install liblapacke-dev libopenblas-dev libplplot-dev libshp-dev pkg-config libexpat1-dev libgtk2.0-dev -y

sudo apt install opam -y
opam init -y
eval $(opam env)
opam switch create ci ocaml-base-compiler.4.13.1
eval $(opam env)

# muf zelus
git clone https://github.com/INRIA/zelus.git
cd zelus
git checkout muf
opam pin -k path . -y
opam install zelus -y
cd ..


# probzelus
git clone https://github.com/psg-mit/probzelus-ci.git
cd probzelus-ci
opam pin -k path -n zelus-libs -y
opam pin -k path -n probzelus -y
opam install probzelus -y
# opam install zelus-owl-plplot zelus-io -y
cd ..

# muf
git clone https://github.com/psg-mit/muf-ci.git
cd muf-ci
git checkout reimplement
opam pin -y -k path .
opam install muf -y
cd ..

eval $(opam env)

# python
sudo apt install python3-pip -y
pip3 install matplotlib


#!/bin/bash

# This is a setup script for installing siren

sudo apt update

sudo apt install build-essential -y
sudo apt install libopenblas-dev -y
sudo apt install liblapacke-dev libopenblas-dev libplplot-dev libshp-dev pkg-config libexpat1-dev libgtk2.0-dev -y

sudo apt install opam -y
opam init -y
eval $(opam env)
opam switch create siren ocaml-base-compiler.4.13.1
eval $(opam env)

# probzelus
git clone https://github.com/IBM/probzelus
cd probzelus
opam pin -k path -n zelus-libs -y
opam pin -k path -n probzelus -y

# TODO: change first line of probzelus-ci/probzelus/configure to
#!/usr/bin/env ocaml

opam install probzelus -y
# opam install zelus-owl-plplot zelus-io -y
cd ..

# siren
git clone https://github.com/psg-mit/siren.git
cd siren
opam pin -y -k path .
opam install siren -y
cd ..

eval $(opam env)

# python
sudo apt install python3-pip -y
pip3 install matplotlib


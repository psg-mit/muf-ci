#!/bin/bash

# This is a setup script for installing siren

sudo apt update

sudo apt install build-essential -y
sudo apt install libopenblas-dev -y
sudo apt install liblapacke-dev libopenblas-dev libplplot-dev libshp-dev pkg-config libexpat1-dev libgtk2.0-dev -y
sudo apt install build-essential zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libreadline-dev libffi-dev libsqlite3-dev wget libbz2-dev -y

# install python3.10
wget https://www.python.org/ftp/python/3.10.0/Python-3.10.0.tgz
tar -xvf Python-3.10.0.tgz
cd Python-3.10.0
sudo ./configure --enable-optimizations
sudo make -j 60
sudo make altinstall
cd ..

sudo apt install git tmux -y

# siren
git clone https://github.com/psg-mit/siren.git
cd siren
python3.10 -m venv venv
source venv/bin/activate
pip install -r requirements.txt

tmux
source venv/bin/activate
#!/bin/bash

# This is a setup script for installing siren

sudo apt update

sudo apt install build-essential -y
sudo apt install libopenblas-dev -y
sudo apt install liblapacke-dev libopenblas-dev libplplot-dev libshp-dev pkg-config libexpat1-dev libgtk2.0-dev -y
sudo apt install python3-pip -y

sudo apt install git -y

# siren
git clone https://github.com/psg-mit/siren.git
cd siren
pip install -r requirements.txt

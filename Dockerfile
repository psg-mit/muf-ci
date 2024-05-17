
# FROM python:3.12-slim as compiler
# WORKDIR /app
# RUN python -m venv /opt/venv
# # Enable venv
# ENV PATH="/opt/venv/bin:$PATH"
# COPY ./requirements.txt /app/requirements.txt
# RUN pip install -Ur requirements.txt

FROM ubuntu:latest

ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8


RUN apt update
RUN apt install -y zsh
SHELL ["bash", "-l", "-c"]

RUN apt install build-essential libopenblas-dev liblapacke-dev libplplot-dev libshp-dev pkg-config libexpat1-dev libgtk2.0-dev zlib1g-dev libncurses5-dev libgdbm-dev libnss3-dev libssl-dev libreadline-dev libffi-dev libsqlite3-dev wget libbz2-dev -y

# RUN apt install software-properties-common -y
# RUN add-apt-repository ppa:deadsnakes/ppa
# RUN apt update
# RUN apt install python3.12 python3-pip -y

RUN wget https://www.python.org/ftp/python/3.11.0/Python-3.11.0.tgz
RUN tar -xvf Python-3.11.0.tgz
WORKDIR /Python-3.11.0
RUN ./configure --enable-optimizations
RUN make -j 8
RUN make altinstall
RUN python3.11 -m ensurepip --upgrade
RUN echo 'alias python=python3.11' >> ~/.bashrc
RUN echo 'alias pip=pip3.11' >> ~/.bashrc
RUN source ~/.bashrc

WORKDIR /root/siren
COPY . .

# COPY --from=compiler /opt/venv /opt/venv

# RUN virtualenv -p python3.12 /opt/venv
# Enable venv
ENV PATH="/usr/local/bin:$PATH"
# ENV PYTHONPATH="/opt/venv:$PYTHONPATH"
# ENV VIRTUAL_ENV="/opt/venv"
# RUN pip3.11 install -r requirements.txt
RUN pip3.11 install -e .

# Enable venv
# ENV PATH="/opt/venv:$PATH"



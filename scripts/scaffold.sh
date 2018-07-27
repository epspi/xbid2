#!/bin/bash

sudo rm -rf data
mkdir data
mkdir data/current
mkdir data/expired
R -e "source('global.R'); Rescrape(F)"
sudo restart shiny-server

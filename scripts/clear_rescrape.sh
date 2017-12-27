#!/bin/bash

sudo rm -rf data/current/*
R -e "source('global.R'); Rescrape(F)"
sudo restart shiny-server

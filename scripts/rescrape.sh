#!/bin/bash

R -e "source('global.R'); Rescrape(F)"
systemctl restart shiny-server

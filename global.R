#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is the global script for the GeocodeR shiny app.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: Used to set all global options
#-------- Used to upload all required libraries
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package install and load~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
library(magrittr)
library(tidyverse)
library(shiny)
library(shinycssloaders)
library(janitor)
library(httr)
library(DT)
library(sf)
library(tmap)
library(leaflet)
library(lubridate)
library(mapview)
library(shinyalert)

tmap::tmap_mode("view")

#setting global options~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
options(shiny.maxRequestSize = 15*1024^2)

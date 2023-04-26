################################################################################
# Makefile for building and running docker containers


# PRODUCTION version -----------------------------------------------------------
production_build:
	docker build --no-cache -t mazamascience/airmonitor:0.3.10 -t mazamascience/airmonitor:latest .

production_publish:
	docker login && docker push mazamascience/airmonitor:0.3.10


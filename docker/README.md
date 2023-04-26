## Create a Docker Image ##

A quick refresher on docker commands is available at the [docker cheatsheet](https://github.com/wsargent/docker-cheat-sheet).

> _NOTE:_ Issue on new Macs with M1 chips: "failed to solve with frontend dockerfile.v0", 
> Partial solution involves issuing the following commands at the command line:
> `export DOCKER_BUILDKIT=0`
> `export COMPOSE_DOCKER_CLI_BUILD=0`
>
> For now, don't build on machines with M1 chips.

A docker image with all required prerequisites can be built with the `Makefile` in this directory:

```
make production_build
```

You should then be able to see something like the following:

```
% docker images | grep airmonitor
mazamascience/airmonitor     0.3.10    b488a1a4273a   About a minute ago   4.42GB
mazamascience/airmonitor     latest    b488a1a4273a   About a minute ago   4.42GB
...
```

## Test the Docker Image ##

Having built the docker image we can now test it. The following output was 
obtained on April 26, 2023:

```
docker run -ti mazamascience/airmonitor R --vanilla
...
monitor_loadLatest() %>% 
  monitor_filterByDistance(-122.33, 47.60, 2000) %>%
  monitor_getData() %>% 
  tail()
               datetime 51b9bcb4eaac7c9d_840530330030
236 2023-04-26 15:00:00                             6
237 2023-04-26 16:00:00                             8
238 2023-04-26 17:00:00                             8
239 2023-04-26 18:00:00                            15
240 2023-04-26 19:00:00                            NA
241 2023-04-26 20:00:00                            NA
...
monitor_loadLatest() %>% 
  monitor_filterByDistance(-122.33, 47.60, 2000) %>%
  monitor_getMeta() %>% 
  dplyr::glimpse(width = 75)
  Rows: 1
Columns: 61
$ deviceDeploymentID       <chr> "51b9bcb4eaac7c9d_840530330030"
$ deviceID                 <chr> "840530330030"
$ deviceType               <chr> NA
$ deviceDescription        <chr> NA
$ deviceExtra              <chr> NA
$ pollutant                <chr> "PM2.5"
$ units                    <chr> "UG/M3"
$ dataIngestSource         <chr> "AirNow"
$ dataIngestURL            <chr> "https://www.airnowapi.org/aq/data/"
$ dataIngestUnitID         <chr> NA
$ dataIngestExtra          <chr> NA
$ dataIngestDescription    <chr> NA
$ locationID               <chr> "51b9bcb4eaac7c9d"
$ locationName             <chr> "Seattle-10th & Weller"
$ longitude                <dbl> -122.3197
$ latitude                 <dbl> 47.59722
$ elevation                <dbl> 41.8
$ countryCode              <chr> "US"
$ stateCode                <chr> "WA"
$ countyName               <chr> "King"
$ timezone                 <chr> "America/Los_Angeles"
...
```

## Publish the Docker Image ##

```
docker login
...
docker push mazamascience/airmonitor:0.3.10
```


## Download the Docker Image ##

A recent image can also be obtained from DockerHub with:

```
docker pull mazamascience/airmonitor:0.3.10
```



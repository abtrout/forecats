#!/usr/bin/env bash

function deploy() {
  cd target/scala-2.11
  scp forecats.jar forecats:~/forecats/
  scp {resource_managed/main/resources/www/app.js,classes/www/{index.html,sprites.svg,style.css}} forecats:~/forecats/www
  ssh forecats '~/forecats/run.sh'
  cd ../../
}

sbt assembly && deploy

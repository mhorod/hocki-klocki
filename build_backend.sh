#!/bin/env bash

cd app
sbt assembly
cd ..
cp app/target/scala-3.3.4/app.jar webapp/dfl/app.jar

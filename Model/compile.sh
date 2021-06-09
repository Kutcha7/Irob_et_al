#!/bin/bash

# Compile the model
# g++ version 10.0 used for manuscript
# All source files are located in ./Source

g++ -march=native -O3 -o ecohyd ./Source/*.cpp ./Source/*.h

# Run model

./ecohyd
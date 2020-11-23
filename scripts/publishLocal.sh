#!/bin/bash
sbt spec/publishLocal +apiJS/publishLocal +apiJVM/publishLocal +coreJVM/publishLocal +coreJS/publishLocal +pluginsJS/publishLocal +pluginsJVM/publishLocal


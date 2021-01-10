#!/bin/bash
sbt spec/publishLocal +apiJVM/publishLocal +apiJS/publishLocal +coreJVM/publishLocal +coreJS/publishLocal +pluginsJVM/publishLocal +pluginsJS/publishLocal


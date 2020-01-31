#!/bin/bash

if [ -x "$(command -v mvn)" ]; then
  echo "[CF] Initializing Maven project"
  mvn -q initialize
  echo "[CF] Compiling project artifacts"
  mvn -q clean scala:compile
  echo "[CF] Building application .jar"
  mvn -q package
  chmod u+x target/ExecCloudFORMAL.jar
  echo "[CF] Creating alias 'CloudFORMAL'"
  alias CloudFORMAL="scala -J-Xmx1G target/ExecCloudFORMAL.jar"
  echo "[CF] You can now use the tool by typing 'CloudFORMAL <opts>' "
  echo "[CF] Or 'scala -J-Xmx1G target/ExecCloudFORMAL.jar <opts>'"
  echo "[CF] Like this ('CloudFORMAL --help'): "
  echo ""
  CloudFORMAL --help
else 
  echo "[CF] Maven not found, please install it first"
fi
  

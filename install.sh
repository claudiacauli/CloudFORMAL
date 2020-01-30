#!/bin/bash

if [ -x "$(command -v mvn)" ]; then
  echo "[CloudF] Maven already installed"
  echo "[CloudF] Building Application .jar"
  mvn -q initialize clean scala:compile package
  echo "[CloudF] Moving .jar to home folder"
  mv target/ExecCloudFORMAL.jar ~/ExecCloudFORMAL.jar
  echo "[CloudF] Chmod to Executable"
  chmod u+x ~/ExecCloudFORMAL.jar
  echo "[CloudF] Created alias to 'CloudFORMAL' "
  alias CloudFORMAL="scala -J-Xmx1G ~/ExecCloudFORMAL.jar"
  echo "[CloudF] You can now use the tool by typing 'CloudFORMAL <opts>' "
  echo "[CloudF] Like this: "
  echo ""
  CloudFORMAL --help
else 
  echo "[CloudF] Maven not found, please install Maven first"
fi

@echo off
bash -c ifconfig | grep 172 | awk "{ print $2 }"

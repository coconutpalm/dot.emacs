@echo off
ipconfig | grep 172 | awk -F: '{ print $2 }'
rem bash -c ifconfig | grep 172 | awk "{ print $2 }"

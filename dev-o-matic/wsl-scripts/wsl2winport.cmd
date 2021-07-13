@echo off
echo Mapping wsl:%1 to win:%2
echo WSL's IP is:
set-wslip
@echo on

netsh interface portproxy add v4tov4 listenport=%2 listenaddress=0.0.0.0 connectport=%1 connectaddress=%WSLIP%

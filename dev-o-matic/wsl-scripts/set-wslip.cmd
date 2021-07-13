@echo off
FOR /F "tokens=*" %%g IN (
  'wslip'
) do (SET WSLIP=%%g)
echo %WSLIP%

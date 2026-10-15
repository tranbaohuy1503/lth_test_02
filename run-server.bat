@echo off
setlocal
echo Starting Tank Battle Server (Port 9123)...
cabal run --builddir=C:\cabal-build\tank-battle tank-server -- %*
endlocal

@echo off
setlocal
echo Starting Tank Battle Client (Gloss 2D UI)...
cabal run --builddir=C:\cabal-build\tank-battle tank-client -- %*
endlocal

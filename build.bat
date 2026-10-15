@echo off
setlocal
echo ============================================================
echo   Building Multiplayer Tank Battle Arena with Cabal
echo ============================================================
cabal build --builddir=C:\cabal-build\tank-battle
if %ERRORLEVEL% EQU 0 (
    echo [SUCCESS] Build completed successfully!
) else (
    echo [ERROR] Build failed!
)
endlocal

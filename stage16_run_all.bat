@echo off
setlocal enabledelayedexpansion
pushd %~dp0
set "PY=python"
echo [1/6] make dummy records...
%PY% scripts\stage16_make_dummy_records.py || goto :err
echo [2/6] summarize metrics...
%PY% scripts\stage16_summarize.py || goto :err
echo [3/6] export latex (core)...
%PY% scripts\stage16_export_latex.py || goto :err
echo [4/6] export latex (extra)...
%PY% scripts\stage16_export_latex_extra.py || goto :err
echo [5/6] export latex (latency)...
%PY% scripts\stage16_export_latex_latency.py || goto :err
echo [6/6] combine latex tables...
%PY% scripts\stage16_export_all_tex.py || goto :err
echo [?] validate records...
%PY% scripts\stage16_validate_records.py || goto :err
echo.
echo ===== DONE Stage-16 =====
echo Outputs:
echo   build\metrics\summary_stage16.json
echo   build\metrics\summary_stage16.csv
echo   build\metrics\stage16_table.tex
echo   build\metrics\stage16_table_extra.tex
echo   build\metrics\stage16_table_latency.tex
echo   build\metrics\stage16_tables.tex
echo   build\metrics\stage16_validation.txt
echo =========================
popd
exit /b 0
:err
echo.
echo *** ERROR: %errorlevel% ***
popd
exit /b %errorlevel%

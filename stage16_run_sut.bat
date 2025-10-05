@echo off
setlocal enabledelayedexpansion
pushd %~dp0
set "PY=python"
echo [1/5] run SUT harness (pre/post, overwrite)...
%PY% scripts\stage16_run_prepost.py --variant sut-default --mode overwrite --retriever bm25 --top-k 8 --use-nlq || goto :err
echo [2/5] summarize metrics...
%PY% scripts\stage16_summarize.py || goto :err
echo [3/5] export LaTeX (core)...
%PY% scripts\stage16_export_latex.py || goto :err
echo [4/5] export LaTeX (extra + latency)...
%PY% scripts\stage16_export_latex_extra.py || goto :err
%PY% scripts\stage16_export_latex_latency.py || goto :err
%PY% scripts\stage16_export_all_tex.py || goto :err
echo [5/5] validate records...
%PY% scripts\stage16_validate_records.py || goto :err
echo.
echo ===== DONE Stage-16 (SUT) =====
echo Outputs:
echo   build\logs\eval_records.jsonl
echo   build\metrics\summary_stage16.json
echo   build\metrics\summary_stage16.csv
echo   build\metrics\stage16_table.tex
echo   build\metrics\stage16_table_extra.tex
echo   build\metrics\stage16_table_latency.tex
echo   build\metrics\stage16_tables.tex
echo   build\metrics\stage16_validation.txt
echo =================================
popd
exit /b 0
:err
echo.
echo *** ERROR: %errorlevel% ***
popd
exit /b %errorlevel%

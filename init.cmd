@echo off
setlocal enabledelayedexpansion
set proce=%NUMBER_OF_PROCESSORS%
set quant=758

echo proce=%proce%
echo quant=%quant%

set /a quantidade=%quant%/%proce%
set inicio=1
set fim=%quantidade%
start cmd /k Rscript Main.R 1 189
start cmd /k Rscript Main.R 190 378
start cmd /k Rscript Main.R 379 567
start cmd /k Rscript Main.R 568 758
## Test environments
* local OS X install, R 3.4.1
* win-builder (devel and release)
* travis CI, R 3.4.0

## R CMD check results
There were no ERRORs or WARNINGs on Windows, OS X, or Linux. There were two NOTEs, and we have addressed both. The first NOTE was related to the spelling of "Preprocessing", which we believe is correctly spelled. The second was related to the UTF-8 encoding of example data, which has been changed to ASCII. 

## Downstream dependencies
The package update does not cause any issues because it does not have any downstream dependencies. 



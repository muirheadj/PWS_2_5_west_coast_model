#!/usr/bin/env tcsh

set iFr = 2
set iTo = 2
set BootID = 1
set i = $iFr
while ($i <= $iTo)

	Rscript src/01-model_control.R $i $BootID
	@ i++
end
* Replication code undergraduate thesis
* Jorge M. Orozco

// Instalattion of dasp package (Distributive analysis)
net from "C:\Users\jorge\Downloads\dasp\dasp3"
net install dasp_p1, force
net install dasp_p2, force
net install dasp_p3, force
net install dasp_p4, force
net install dasp_p5, force
net install dasp_p6, force
cap addmenu profile.do _daspmenu
cap add_data_examples

// Processins databases
* convert files to .dta
local dir "C:\Users\jorge\Documents\tesis-pregrado\databases\GEIH" 
cd `"`dir'"'

local files: dir "`dir'" files "*.sav"
foreach file of local files {
    display "`file'"
    
    // Importar y convertir a .dta
    import spss using "`file'", clear
    local newfile = subinstr("`file'", ".sav", ".dta", .)
    save "`newfile'", replace
    display "`file' convertido a `newfile'"
    
    // Eliminar archivo .sav original
    erase "`file'"
    display "`file' ha sido eliminado"
}

* standardize names
local files: dir "`dir'" files "*.dta"

foreach file of local files {
    use "`file'", clear
    
    display "Procesando: `file'"
    describe, short
    
    foreach var of varlist _all {
        local newname = lower("`var'")  
        local newname = strtoname("`newname'")  
        rename `var' `newname'
    }
    
    describe, short
    save "`file'", replace
    display "`file' actualizado y guardado"
}

* create the full database
local files: dir "`dir'" files "*.dta"

// add year
foreach file of local files {
    use "`file'", clear
    local year = substr("`file'", 8, 4)  
    gen year = `year'
    save "`file'", replace
    display "Procesado: `file' con year = `year'"
}

* fix and select variables

use hogares2014.dta, clear
gen mes_str = ""
replace mes_str = "01" if mes == 1  // Enero
replace mes_str = "02" if mes == 2  // Febrero
replace mes_str = "03" if mes == 3  // Marzo
replace mes_str = "04" if mes == 4  // Abril
replace mes_str = "05" if mes == 5  // Mayo
replace mes_str = "06" if mes == 6  // Junio
replace mes_str = "07" if mes == 7  // Julio
replace mes_str = "08" if mes == 8  // Agosto
replace mes_str = "09" if mes == 9  // Septiembre
replace mes_str = "10" if mes == 10 // Octubre
replace mes_str = "11" if mes == 11 // Noviembre
replace mes_str = "12" if mes == 12 // Diciembre
drop mes
rename mes_str mes
save hogares2014.dta, replace

use hogares2015.dta, clear
gen mes_str = ""
replace mes_str = "01" if mes == 1  // Enero
replace mes_str = "02" if mes == 2  // Febrero
replace mes_str = "03" if mes == 3  // Marzo
replace mes_str = "04" if mes == 4  // Abril
replace mes_str = "05" if mes == 5  // Mayo
replace mes_str = "06" if mes == 6  // Junio
replace mes_str = "07" if mes == 7  // Julio
replace mes_str = "08" if mes == 8  // Agosto
replace mes_str = "09" if mes == 9  // Septiembre
replace mes_str = "10" if mes == 10 // Octubre
replace mes_str = "11" if mes == 11 // Noviembre
replace mes_str = "12" if mes == 12 // Diciembre
drop mes
rename mes_str mes
rename fex_dpto fex_dpto_c
save hogares2015.dta, replace

use hogares2016.dta, clear
gen mes_str = ""
replace mes_str = "01" if mes == 1  // Enero
replace mes_str = "02" if mes == 2  // Febrero
replace mes_str = "03" if mes == 3  // Marzo
replace mes_str = "04" if mes == 4  // Abril
replace mes_str = "05" if mes == 5  // Mayo
replace mes_str = "06" if mes == 6  // Junio
replace mes_str = "07" if mes == 7  // Julio
replace mes_str = "08" if mes == 8  // Agosto
replace mes_str = "09" if mes == 9  // Septiembre
replace mes_str = "10" if mes == 10 // Octubre
replace mes_str = "11" if mes == 11 // Noviembre
replace mes_str = "12" if mes == 12 // Diciembre
drop mes
rename mes_str mes
rename fex_dpto fex_dpto_c
save hogares2016.dta, replace

use hogares2017.dta, clear
gen mes_str = ""
replace mes_str = "01" if mes == 1  // Enero
replace mes_str = "02" if mes == 2  // Febrero
replace mes_str = "03" if mes == 3  // Marzo
replace mes_str = "04" if mes == 4  // Abril
replace mes_str = "05" if mes == 5  // Mayo
replace mes_str = "06" if mes == 6  // Junio
replace mes_str = "07" if mes == 7  // Julio
replace mes_str = "08" if mes == 8  // Agosto
replace mes_str = "09" if mes == 9  // Septiembre
replace mes_str = "10" if mes == 10 // Octubre
replace mes_str = "11" if mes == 11 // Noviembre
replace mes_str = "12" if mes == 12 // Diciembre
drop mes
rename mes_str mes
rename fex_dpto fex_dpto_c
save hogares2017.dta, replace

use hogares2018.dta, clear
gen mes_str = ""
replace mes_str = "01" if mes == 1  // Enero
replace mes_str = "02" if mes == 2  // Febrero
replace mes_str = "03" if mes == 3  // Marzo
replace mes_str = "04" if mes == 4  // Abril
replace mes_str = "05" if mes == 5  // Mayo
replace mes_str = "06" if mes == 6  // Junio
replace mes_str = "07" if mes == 7  // Julio
replace mes_str = "08" if mes == 8  // Agosto
replace mes_str = "09" if mes == 9  // Septiembre
replace mes_str = "10" if mes == 10 // Octubre
replace mes_str = "11" if mes == 11 // Noviembre
replace mes_str = "12" if mes == 12 // Diciembre
drop mes
rename mes_str mes
rename fex_dpto fex_dpto_c
rename depto dpto
save hogares2018.dta, replace

use hogares2019.dta, clear
rename fex_dpto fex_dpto_c
rename depto dpto
save hogares2019.dta, replace

use hogares2020.dta, clear
rename fex_dpto fex_dpto_c
rename depto dpto
gen p5010 = ""
gen p5140 = ""
save hogares2020.dta, replace

use hogares2021.dta, clear
rename fex_dpto fex_dpto_c
gen p5010 = ""
gen p5140 = ""
gen p5000 = ""
save hogares2021.dta, replace

// Obtener lista de archivos .dta
local files: dir "`dir'" files "*.dta"

// Loop para procesar cada archivo
foreach file of local files {
    use "`file'", clear
    
    // Eliminar las variables especificadas
    drop directorio secuencia_p clase dpto p5000 p5010 p5090 p5100 p5130 p5140 nper ingtotug ingtotugarr pobre indigente npobres nindigentes fex_dpto_c
    
    // Crear la variable 'fex'
    gen fex = fex_c * npersug
    
    // Guardar cambios reemplazando el archivo original
    save "`file'", replace
    
    display "Procesado: `file'"
}

// append years
use "hogares2012.dta", clear
append using "hogares2013.dta"
append using "hogares2014.dta"
append using "hogares2015.dta"
append using "hogares2016.dta"
append using "hogares2017.dta"
append using "hogares2018.dta"
append using "hogares2019.dta"
append using "hogares2020.dta"
append using "hogares2021.dta"

* add correct poverty lines 
use lineas_20122018.dta, clear
gen mes_str = ""
replace mes_str = "01" if mes == 1  // Enero
replace mes_str = "02" if mes == 2  // Febrero
replace mes_str = "03" if mes == 3  // Marzo
replace mes_str = "04" if mes == 4  // Abril
replace mes_str = "05" if mes == 5  // Mayo
replace mes_str = "06" if mes == 6  // Junio
replace mes_str = "07" if mes == 7  // Julio
replace mes_str = "08" if mes == 8  // Agosto
replace mes_str = "09" if mes == 9  // Septiembre
replace mes_str = "10" if mes == 10 // Octubre
replace mes_str = "11" if mes == 11 // Noviembre
replace mes_str = "12" if mes == 12 // Diciembre
drop mes 
rename mes_str mes
rename año year
rename lp lpnew
rename li linew
keep if mes=="01"
save lineas_20122018.dta, replace

replace dominio = upper(dominio)
replace dominio="MONTERIA" if dominio=="MONTERíA"
replace dominio="QUIBDO" if dominio=="QUíBDó"
replace dominio="POPAYAN" if dominio=="POPAYáN"
replace dominio="MEDELLIN" if dominio=="MEDELLíN"
replace dominio="BOGOTA" if dominio=="BOGOTá"
replace dominio="IBAGUE" if dominio=="IBAGUé"

merge m:1 dominio year using "C:\Users\jorge\Documents\tesis-pregrado\databases\GEIH\lineas_20122018.dta"

* only barranquilla

keep if dominio=="BARRANQUILLA"
gsort year mes

* create the database of welfare analysis
drop fex_c npersug semestre _merge

replace lpnew = 324974.42 if year==2019
replace lpnew = 339894.86 if year==2020
replace lpnew = 352582.86 if year==2021
drop li lp linew
save barranquilla.dta, replace

// Pens curve
use barranquilla.dta, clear
keep if year==2012 | year==2018 | year==2016 | year==2021
clorenz ingpcug, hsize(fex) hgroup(year) type(gen) min(0) max(1)

// TIP curves
use barranquilla.dta, clear
* 2012
keep if year==2012
cpoverty ingpcug, hsize(fex) curve(cpg) pline(241767) min(0) max(1) type(not)
* 2016
use barranquilla.dta, clear
keep if year==2016
cpoverty ingpcug, hsize(fex) curve(cpg) pline(296404) min(0) max(1) type(not)

* 2018
use barranquilla.dta, clear
keep if year==2018
cpoverty ingpcug, hsize(fex) curve(cpg) pline(312530) min(0) max(1) type(not)

* 2021
use barranquilla.dta, clear
keep if year==20021
cpoverty ingpcug, hsize(fex) curve(cpg) pline(352582) min(0) max(1) type(not)

// Shorrocks decomposition
ssc install skdecomp
* 2012 - 2014
use barranquilla.dta, clear
keep if year==2012 | year==2014
skdecomp ingpcug [w=fex], by(year) varpl(lpnew) in (fgt0)

* 2014 - 2016
use barranquilla.dta, clear
keep if year==2014 | year==2016
skdecomp ingpcug [w=fex], by(year) varpl(lpnew) in (fgt0)

* 2016 - 2018
use barranquilla.dta, clear
keep if year==2016 | year==2018
skdecomp ingpcug [w=fex], by(year) varpl(lpnew) in (fgt0)

* 2019 - 2021
use barranquilla.dta, clear
keep if year==2019 | year==2021
skdecomp ingpcug [w=fex], by(year) varpl(lpnew) in (fgt0)



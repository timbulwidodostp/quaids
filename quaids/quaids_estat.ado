*! version 1.0.0  29dec2011

program quaids_estat

	version 12
	
	if "`e(cmd)'" != "quaids" {
		exit 301
	}
	
	gettoken key 0 : 0, parse(", ")
	local lkey = length(`"`key'"')
	
	if `"`key'"' == substr("expenditure", 1, max(3, `lkey')) {
		DoExp `0'
	}
	else if `"`key'"' == substr("uncompensated", 1, max(6, `lkey')) {
		DoUncomp `0'
	}
	else if `"`key'"' == substr("compensated", 1, max(4, `lkey')) {
		DoComp `0'
	}
	else {
		di as error "invalid subcommand `key'"
		exit 321
	}
	
end

program DoExp, rclass

	syntax [anything(name = vlist id = "newvarlist")] [if] [in] [, atmeans]
	
	marksample touse

	if "`atmeans'" == "" {
		_stubstar2names `vlist', nvars(`=e(ngoods)')
		local vars `s(varlist)'
		local typs `s(typelist)'
				// make our own names vlist_i instead of vlisti
		if `s(stub)' {	
			local vars ""
			local vlist : subinstr local vlist "*" ""
			forvalues i = 1/`=e(ngoods)' {
				local vars `vars' `vlist'_`i'
			}
		}
		forvalues i = 1/`=e(ngoods)' {
			local v : word `i' of `vars'
			local t : word `i' of `types'
			qui gen `t' `v' = .
		}
	}
	else {
		if `"`vlist'"' != "" {
			di in smcl as error 			///
				"cannot specify varlist with {opt atmeans}"
			exit 198
		}
		tempname vars
	}	
	

	if "`e(lnprices)'" != "" {
		local lnp `e(lnprices)'
	}
	else {
		local i 1
		foreach var of varlist `e(prices)' {
			tempvar vv`i'
			qui gen double `vv`i'' = ln(`var')
			local lnp `lnp' `vv`i''
			local `++i'
		}
	}
	if "`e(lnexpenditure)'" != "" {
		local lnexp `e(lnexpenditure)'
	}
	else {
		tempvar exp
		qui gen double `exp' = ln(`e(expenditure)')
		local lnexp `exp'
	}

	local ndemo = `e(ndemos)'
	if `ndemo' > 0 {
		local demos `e(demographics)'
	}
	mata:_quaids__expelas("`touse'", "`e(quadratic)'", 		///
			      "`atmeans'", "`lnp'", "`lnexp'", 		///
			      `ndemo', "`demos'", "`vars'")
	if "`atmeans'" == "" {
		local i 1
		foreach var of varlist `vars' {
			lab var `var' "Expenditure elasticity: good `i'"
			local `++i'
		}
	}
	else {
		return matrix expelas = `vars'
	}

end


program DoUncomp, rclass

	syntax [anything(name = vlist id = "newvarlist")] [if] [in] [, atmeans]
	
	marksample touse
	
	if "`atmeans'" == "" {
		_stubstar2names `vlist', nvars(`=e(ngoods)^2')
		local vars `s(varlist)'
		local typs `s(typelist)'
			// make our own names vlist_i instead of vlisti
		if `s(stub)' {	
			local vars ""
			local vlist : subinstr local vlist "*" ""
			forvalues i = 1/`=e(ngoods)' {
				forvalues j = 1/`=e(ngoods)' {
					local vars `vars' `vlist'_`i'_`j'
				}
			}
		}
		forvalues i = 1/`=e(ngoods)^2' {
			local v : word `i' of `vars'
			local t : word `i' of `types'
			qui gen `t' `v' = .
		}
	}
	else {
		if `"`vlist'"' != "" {
			di in smcl as error 			///
				"cannot specify varlist with {opt atmeans}"
			exit 198
		}
		tempname vars
	}
	
	if "`e(lnprices)'" != "" {
		local lnp `e(lnprices)'
	}
	else {
		local i 1
		foreach var of varlist `e(prices)' {
			tempvar vv`i'
			qui gen double `vv`i'' = ln(`var')
			local lnp `lnp' `vv`i''
			local `++i'
		}
	}
	if "`e(lnexpenditure)'" != "" {
		local lnexp `e(lnexpenditure)'
	}
	else {
		tempvar exp
		qui gen double `exp' = ln(`e(expenditure)')
		local lnexp `exp'
	}
	
	local ndemo = `e(ndemos)'
	if `ndemo' > 0 {
		local demos `e(demographics)'
	}
	mata:_quaids__uncompelas("`touse'", "`e(quadratic)'",		///
				 "`atmeans'", "`lnp'", "`lnexp'",	///
				 `ndemo', "`demos'", "`vars'")
	if "`atmeans'" == "" {
		loc m 1
		forvalues i = 1/`=e(ngoods)' {
			forvalues j = 1/`=e(ngoods)' {
				local v : word `m' of `vars'
				lab var `v' 				///
"Uncompensated elasticity: good `i', price `j'"
				local vars `vars' `vlist'_`i'_`j'
				local `++m'
			}
		}
	}
	else {
		return matrix uncompelas = `vars'
	}

end

program DoComp, rclass

	syntax [anything(name = vlist id = "newvarlist")] [if] [in] [, atmeans]
	
	marksample touse
	
	if "`atmeans'" != "" {
		if `"`vlist'"' != "" {
			di in smcl as error 			///
				"cannot specify varlist with {opt atmeans}"
			exit 198
		}
		local r = e(ngoods)
		tempname expe uncompe sharebar compe
		DoExp if `touse', atmeans
		mat `expe' = r(expelas)
		DoUncomp if `touse', atmeans
		mat `uncompe' = r(uncompelas)
		mata:_quaids__sharebar("`sharebar'", "`touse'")
		mat `compe' = J(`r', `r', 0)
		forvalues i = 1/`r' {
			forvalues j = 1/`r' {
				mat `compe'[`i', `j'] =			///
					`uncompe'[`i', `j'] +		///
					`expe'[1,`i']*`sharebar'[1,`j']
			}
		}
		
		return matrix compelas = `compe'
		exit
		
	}
	
	// Not atmeans from here on down.
	local ng = e(ngoods)
	local ngsq = `ng'^2
	_stubstar2names `vlist', nvars(`ngsq')
	local vars `s(varlist)'
	local typs `s(typelist)'
	
	if `s(stub)' {		// make our own names vlist_i instead of vlisti
		local vars ""
		local vlist : subinstr local vlist "*" ""
		forvalues i = 1/`ng' {
			forvalues j = 1/`ng' {
				local vars `vars' `vlist'_`i'_`j'
			}
		}
	}
	
	forvalues i = 1/`ngsq' {
		local v : word `i' of `vars'
		local t : word `i' of `types'
		local tv `tv' `t' `v'
	}
	forvalues i = 1/`ng' {
		tempvar ee`i'
		local eevars `eevars' double `ee`i''
	}
	
	DoUncomp `tv' if `touse'
	DoExp `eevars' if `touse'
	
	forvalues i = 1/`ng' {
		forvalues j = 1/`ng' {
			local offset = `i'*(`ng' - 1) + `j'
			local vij : word `offset' of `vars'
			local sharej : word `j' of `e(lhs)'
			qui replace `vij' = `vij' + `ee`i''*`sharej'
		}
	}
end


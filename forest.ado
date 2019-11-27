//! version 1.0 31DEC2018  DIME Analytics bbdaniels@gmail.com

// Forest - Stata module to visualize results from multiple regressions on a single independent variable.

cap prog drop forest
prog def forest

syntax anything /// syntax – forest reg d1 d2 d3 = treatment
	[if] [in] [fw pw iw aw] ///
	, [*] /// regression options
	 [or] /// odds-ratios
	 [d]  /// cohen's d
   Treatment(string asis) ///
	 [Controls(varlist fv ts)] ///
	 [GRAPHopts(string asis)] ///
	 [WEIGHTs(string asis)] ///
	 [Bonferroni] [bh] // FWER corrections

version 13.1

preserve
marksample touse, novarlist
keep if `touse'
qui {
	// Set up

		if "`weights'" != "" {
			local weight "[`weights']"
		}

		tempvar dv

		if "`d'" == "d" local std "Standardized "

		if "`or'" == "or" {
			local l0 : label (`treatment') 0
			local l1 : label (`treatment') 1
		}
		else {
		  local tlab : var label `treatment'
		}

	// Set up Bonferroni
	if "`bonferroni'" != "" {
    // Get Bonferroni critical value
		local level = round(`=100-(5/`=`: word count `anything''-1')',0.01)
    // Round to 2 digits (required by reg)
    local level : di %3.2f `level'
    // Implement using level() option NOTE: Do other specs use different options?
		local bonferroni = "level(`level')"
		di `"Bonferroni correction showing significance levels at: `level'%"'
	}

	// Set up depvars
	tokenize `anything'
		local cmd = "`1'"
		mac shift

	// Loop over depvars
	cap mat drop results
	local x = 1
	qui while "`1'" != "" {
		di "`1'"

		// Get label
		local theLabel : var lab `1'
		local theLabels = `"`theLabels' `x' "`theLabel'""'

		// Standardize if d option
		if "`d'" == "d" {
			cap drop `dv'
			egen `dv' = std(`1')
			local 1 = "`dv'"
		}

		// Regression
		`cmd' `1' `treatment' ///
      `controls' ///
      [`weight'`exp'] ///
      , `options' `or' `bonferroni'

      // Store results
			mat a = r(table)'
			mat a = a[1,....]
			mat results = nullmat(results) ///
				\ a , `x'

	local ++x
	mac shift
	}

// Graph
clear
svmat results , n(col)

	// Setup
	if "`or'" == "or" {
		local log `"xline(1,lc(black) lw(thin)) xscale(log) xlab(.01 "1/100" .1 `""1/10" "{&larr} Favors `l0'""' 1 "1" 10 `""10" "Favors `l1'{&rarr}""' 100 "100")"'
		gen x1=100
		gen x2=1/100
	}
	else {
		local log `"xtit({&larr} `std'Effect of `tlab' {&rarr}) xline(0,lc(black) lw(thin) lp(dash))"'
		gen x1=0
		gen x2=0
	}

  // Implement Benjamini-Hochberg
  if "`bh'" != "" {
    egen bh_rank = rank(pvalue)
    gen bh_crit = (bh_rank/_N)*0.05 // BH crit at alpha = 0.05
    gen bh_elig = pvalue if pvalue < bh_crit
    egen bh_max = max(bh_elig)
    gen bh_sig = "*" if pvalue <= bh_max
    local bhplot = "(scatter pos b , mlabpos(12) mlabgap(*-.75) mlab(bh_sig) m(none) mlabc(black) mlabsize(large))"
    local note `""* Significant Benjamini-Hochberg p-value at FWER {&alpha} = 0.05.""'
  }

  // Chart anchors
    gen pos = _n
		gen y1 = 0
		gen y2 = `x'

	// Graph
	tw ///
		(scatter y1 x1 , m(none)) ///
		(scatter y2 x2 , m(none)) ///
		(rspike  ll ul pos , horizontal lc(gs12)) ///
		(scatter pos b , mc(black)) ///
    `bhplot' ///
		, `graphopts' `log' yscale(reverse) ylab(`theLabels',angle(0) notick nogrid) ytit(" ") legend(off) ///
      note(`note' , span)

}
end

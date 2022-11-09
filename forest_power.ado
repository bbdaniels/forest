
cap prog drop forest_power
prog def forest_power

syntax anything [if] [in], ///
  at(string asis) ///
  [reps(integer 10000)] ///
  [seed(integer 123456)] ///
  [graph_opts(string asis)] ///
  [Saving(string asis)] /// Save table
  [noms] /// no details
  [*] // Options to pass to [forest]

qui {
preserve
  clear
  tempfile all_results
  save `all_results' , emptyok
restore


// Get all regression results --------------------------------------------------
foreach x of numlist `at' {
  preserve

    marksample touse
    expand `x'
    forest `anything' if `touse' == 1, forestpower `options'
    gen multiple = `x'
    append using `all_results'
      save `all_results' , replace

  restore
}

// Simulate many normal draws
preserve
  use `all_results' , clear
  isid label multiple, sort
  set seed `seed'
  expand `reps'

  gen rand = rnormal()
    gen b_new  = b + rand*se
    gen rsig   = (b_new > (ul-b)) | (b_new < (ll-b))
    gen type_s = ((b_new > 0 & b < 0) | (b_new < 0 & b > 0)) if rsig
    gen type_m = abs(b_new / b) if rsig
    collapse (mean) rsig type_s type_m multiple, by(label c2)

  if `"`saving'"' != `""' {
    lab var rsig "Power"
    lab var type_s "Type S"
    lab var type_m "Type M"
    lab var multiple "Sample Multiple"
    lab var label "Variable"
    sort label multiple
    export excel label multiple rsig type_s type_m  using `saving' , replace first(varl)
  }

  levelsof label , local(levels)
  local i = 0
  foreach l in `levels' {
    local ++i
    local g1 `"`g1' (line rsig multiple if label == "`l'")"'
    local g2 `"`g2' (line type_s multiple if label == "`l'")"'
    local g3 `"`g3' (line type_m multiple if label == "`l'")"'
      local legend `"`legend' `i' "`l'" "'
  }

  sort c2
    local nn = c2[1]

  tempname ga gb gc

if "`ms'" == "" {
  tw `g1' , nodraw saving(`gc'.gph , replace) legend(on size(small) symxsize(small) order(`legend')) ///
    title("Power (vs N = `nn')") ylab(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%" 1 "100%") yline(0.8) ///
    xtit("Sample Size Multiple") ytit("") xscale(r(1)) xlab(#6) legend(size(small))
  tw `g2' , nodraw saving(`ga'.gph , replace) ///
    title("Probability of Wrong Sign When Significant")  ///
    xtit("") ytit("") xscale(r(1)) xlab(#6)
  tw `g3' , nodraw saving(`gb'.gph , replace) ///
   title("Exaggeration Factor When Significant") ///
   xtit("") ytit("") xscale(r(1)) xlab(#6)

  graph combine `gb'.gph `ga'.gph , c(1) nodraw saving(`ga'.gph , replace)
  graph combine `gc'.gph `ga'.gph , c(2)

  !rm `ga'.gph `gb'.gph `gc'.gph
}
else {
  tw `g1' , legend(on pos(3) size(small) symxsize(small) order(`legend')) ///
    title("Power (vs N = `nn')") ylab(0 "0%" .2 "20%" .4 "40%" .6 "60%" .8 "80%" 1 "100%") yline(0.8) ///
    xtit("Sample Size Multiple") ytit("") xscale(r(1)) xlab(#6) legend(size(small))
}

}

end

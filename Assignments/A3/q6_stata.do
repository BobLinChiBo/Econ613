log using "./q6_stata", replace
set maxvar 32000
use "./q6_sample_stata.dta" 
cmset id choice_rev
cmclogit dummy relative_quality
margins, dydx(relative_quality)
log close
translate q6_stata.smcl q6_stata.pdf
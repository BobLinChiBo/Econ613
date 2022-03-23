log using "./q5_stata" , replace
set maxvar 32000
use "./q5_sample_stata.dta" , clear
encode choice_rev, gen(choice_rev_f)
mlogit choice_rev_f score, baseoutcome(1) 
margins, dydx(score) pr(out(1))
log close
translate q5_stata.smcl q5_stata.pdf
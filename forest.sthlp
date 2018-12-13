{smcl}
{* Dec 31st 2018}
{hline}
Help for {hi:forest}
{hline}

{title:Description}

{p}{cmd: forest} reports results from multiple regressions on a single independent variable as a modified "forest" chart.
This chart shows the effect of a single treatment variable of interest on a range of independent variables.
It can display raw coefficients, standardized effect sizes (Cohen's {it:d}), or odds ratios (from logistic regressions).{p_end}

{title:Syntax}

{p 2 4}{cmd: forest} {bf:estimator} {it:depvars} = {it: treatment}
{break} [{help if}] [{help in}] , [{opth weight(weight)}] [{opth c:ontrols(varlist)}]
{break} [{opth graph:opts(twoway_options)}] [{bf:or|d}]
{break} [{opt b:onferroni}] [{it:est_options}]

{synoptset 16 tabbed}{...}
{marker Options}{...}
{synopthdr:Syntax}
{synoptline}
{synopt:{opt estimator}}Indicates the estimation command to be utilized.{p_end}
{synopt:{it:depvars}}List the left-hand-side variables.{p_end}
{synopt:{it:treatment}}List the independent variable of interest.{p_end}
{break}
{synopt:{bf:or|d}}Request effect sizes as odds ratios (by exponentiating regression coefficients where possible)
or in terms of Cohen's {it:d} (by standardizing the dependent variables). (Choose only one.){p_end}
{synopt:{opt b:onferroni}}Request confidence intervals calculated with Bonferroni correction for simultaneous comparisons.
This is calculated by adjusting the significance level to (100-5/({it:number of regressions})).{p_end}
{break}
{synopt:{opt weight()}}Specify weights.{p_end}
{synopt:{opt c:ontrols()}}Specify controls.{p_end}
{synopt:{opt graph:opts()}}Set any desired options for the graph.{p_end}
{break}
{synopt:{it:est_options}}Specify any options needed for the estimator.{p_end}
{synoptline}

{title:Examples}

{stata sysuse auto, clear : sysuse auto, clear}
{stata gen check = rep78 > 3 : gen check = rep78 > 3}
{stata gen check2 = 1-foreign : gen check2 = 1-foreign}
{stata label val check origin : label val check origin}

{stata forest reg headroom foreign = rep78 : forest reg headroom foreign = rep78}
{stata forest reg headroom foreign = rep78 , d : forest reg headroom foreign = rep78 , d}
{stata forest logit check2 foreign = check , or : forest logit check2 foreign = check , or}

{title:Author}

Benjamin Daniels
bdaniels@worldbank.org

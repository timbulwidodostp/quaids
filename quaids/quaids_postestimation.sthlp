{smcl}
{* *! version 1.0.0  29dec2011}{...}
{cmd:help quaids postestimation}{right: ({browse "http://www.stata-journal.com/article.html?article=st0268":SJ12-3: st0268})}
{hline}

{title:Title}

{p2colset 5 30 32 2}{...}
{p2col :{cmd:quaids postestimation} {hline 2}}Postestimation tools for quaids{p_end}
{p2colreset}{...}


{title:Description}

{pstd}
The following postestimation commands are available after {cmd:quaids}:

{synoptset 21}{...}
{p2coldent :Command}Description{p_end}
{synoptline}
{synopt :{helpb quaids postestimation##expelas:estat expenditure}}expenditure elasticities{p_end}
{synopt :{helpb quaids postestimation##comelas:estat compensated}}compensated price elasticities{p_end}
{synopt :{helpb quaids postestimation##uncelas:estat uncompensated}}uncompensated price elasticities{p_end}
INCLUDE help post_estimates
INCLUDE help post_lincom
{synopt :{helpb quaids postestimation##predict:predict}}predicted expenditure shares{p_end}
INCLUDE help post_predictnl
INCLUDE help post_test
INCLUDE help post_testnl
{synoptline}
{p2colreset}{...}


{marker predict}{...}
{title:Syntax for predict}

{p 8 16 2}
{cmd:predict} 
[{it:type}]
{c -(}{it:stub}{cmd:*}|{it:newvarlist}{c )-}
{ifin}

{pstd}
These statistics are available both in and out of sample; type 
{cmd:predict ... if e(sample) ...} if wanted only for the estimation
sample.

{pstd}
You must specify a variable {it:stub} or {it:k} new variables, where {it:k}
is the number of goods in the demand system.


{marker expelas}{...}
{title:Syntax for estat expenditure}

{p 8 16 2}
{cmd:estat} {cmdab:exp:enditure}
[{it:type}]
{c -(}{it:stub}{cmd:*}|{it:newvarlist}{c )-}
{ifin}

{pstd}
or

{p 8 16 2}
{cmd:estat} {cmdab:exp:enditure}
{ifin}{cmd:, atmeans}

{pstd}
The first syntax computes expenditure elasticities for each observation
in the dataset or for the subsample defined by the {cmd:if} or {cmd:in}
conditions if specified.  You must specify a variable {it:stub} or {it:k}
new variables.

{pstd}
The second syntax, for which you specify the {opt atmeans} option but do
not specify a variable {it:stub} or a list of variables, computes the 1 x
{it:k} vector of expenditure elasticities calculated when all the
variables in the model are set to their sample means.


{marker comelas}{...}
{title:Syntax for estat compensated}

{p 8 16 2}
{cmd:estat} {cmdab:comp:ensated}
[{it:type}]
{c -(}{it:stub}{cmd:*}|{it:newvarlist}{c )-}
{ifin}

{pstd}
or

{p 8 16 2}
{cmd:estat} {cmdab:comp:ensated}
{ifin}{cmd:, atmeans}

{pstd}
The first syntax computes compensated price elasticities for each
observation in the dataset or for the subsample defined by the {cmd:if}
or {cmd:in} conditions if specified.  You must specify a variable {it:stub} or
{it:k}^2 new variables.

{pstd}
The second syntax, for which you specify the {opt atmeans} option but do
not specify a variable {it:stub} or a list of variables, computes the {it:k} x
{it:k} matrix of compensated price elasticities calculated when all
the variables in the model are set to their sample means.  The
element in row {it:i}, column {it:j} of the matrix contains the elasticity
of good {it:i} with respect to changes in the price of good {it:j}.


{marker uncelas}{...}
{title:Syntax for estat uncompensated}

{p 8 16 2}
{cmd:estat} {cmdab:uncomp:ensated}
[{it:type}]
{c -(}{it:stub}{cmd:*}|{it:newvarlist}{c )-}
{ifin}

{pstd}
or

{p 8 16 2}
{cmd:estat} {cmdab:uncomp:ensated}
{ifin}{cmd:, atmeans}

{pstd}
The first syntax computes uncompensated price elasticities for each
observation in the dataset or for the subsample defined by the {cmd:if}
or {cmd:in} conditions if specified.  You must specify a variable
{it:stub} or {it:k}^2 new variables.

{pstd}
The second syntax, for which you specify the {opt atmeans} option but do
not specify a variable {it:stub} or a list of variables, computes the
{it:k} x {it:k} matrix of uncompensated price elasticities calculated
when all the variables in the model are set to their sample means.
The element in row {it:i}, column {it:j} of the matrix contains the
elasticity of good {it:i} with respect to changes in the price of good
{it:j}.


{title:Examples}

{pstd}Setup{p_end}
{phang2}{cmd:. webuse food}{p_end}
{phang2}{cmd:. quaids w1-w4, anot(10) lnprices(lnp1-lnp4) expenditure(expfd)}{p_end}

{pstd}
Calculate observation-level predicted expenditure shares and elasticities{p_end}
{phang2}{cmd:. predict what*}{p_end}
{phang2}{cmd:. estat expenditure mu*}{p_end}
{phang2}{cmd:. estat compensated ce*}{p_end}
{phang2}{cmd:. estat uncompensated ue*}{p_end}

{pstd}
Calculate elasticities at variable means{p_end}
{phang2}{cmd:. estat expenditure, atmeans}{p_end}
{phang2}{cmd:. matrix list r(expelas)}{p_end}

{phang2}{cmd:. estat compensated, atmeans}{p_end}
{phang2}{cmd:. matrix list r(compelas)}{p_end}

{phang2}{cmd:. estat uncompensated, atmeans}{p_end}
{phang2}{cmd:. matrix list r(uncompelas)}{p_end}


{title:Saved results}

{pstd}
{cmd:estat expenditure} saves the following in {cmd:r()} if {opt atmeans} is specified:{p_end}

{phang2}
{cmd:r(expelas)}{space 5}vector of expenditure elasticities{p_end}

{pstd}
{cmd:estat compensated} saves the following in {cmd:r()} if {opt atmeans} is specified:{p_end}

{phang2}
{cmd:r(compelas)}{space 4}matrix of compensated price
elasticities{p_end}

{phang2}
The element in row {it:i}, column {it:j} of {cmd:r(compelas)} contains the
compensated price elasticity of good {it:i} with respect to the price of
good {it:j}.

{pstd}
{cmd:estat uncompensated} saves the following in {cmd:r()} if {opt atmeans} is specified:{p_end}

{phang2}
{cmd:r(uncompelas)}{space 2}matrix of uncompensated price elasticities{p_end}

{phang2}
The element in row {it:i}, column {it:j} of {cmd:r(uncompelas)} contains the 
uncompensated price elasticity of good {it:i} with respect to the price of good
{it:j}.


{title:Author}

{pstd}Brian P. Poi{p_end}
{pstd}StataCorp LP{p_end}
{pstd}College Station, TX{p_end}
{pstd}bpoi@stata.com{p_end}


{title:Also see}

{p 4 14 2}Article:  {it:Stata Journal}, volume 12, number 3: {browse "http://www.stata-journal.com/article.html?article=st0268":st0268}

{p 7 14 2}Help:  {helpb quaids}{p_end}

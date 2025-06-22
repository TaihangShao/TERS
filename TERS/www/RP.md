### Function for Royston-Parmar models

$$
g(S(t)) = s(x, {\gamma})
$$ where $x=\log(t)$ and $s()$ is a natural cubic spline function with $m$ internal knots: $$
  s(x,{\gamma}) = \gamma_0 + \gamma_1 x + \gamma_2 v_1(x) + \ldots + \gamma_{m+1} v_m(x)   
$$ where $v_j(x)$ is the $j$th *basis* function, $$v_j(x) = (x - k_j)^3_+ - \lambda_j(x - k_{min})^3_+ - (1 - \lambda_j) (x - k_{max})^3_+, 
\qquad
\lambda_j = \frac{k_{max} - k_j}{k_{max} - k_{min}} $$ and $(x - a)_+ = max(0, x - a)$.

The link function relating the survivor function to the spline is:

$$
g(S(t)) =  \begin{array}{ll}
\log(-\log(S(t))) &\verb+(scale="hazard")+ \\
\log(S(t)^{-1} - 1)      &                  \verb+(scale="odds")+ \\
\Phi^{-1}(S(t)) &     \verb+(scale="normal")+ \\
                      \end{array}
                    $$

Note: These formulas were extracted from the introduction of r package *"flexsurv"*.

#### Citation

1.  Jackson, C. (2016). flexsurv: A Platform for Parametric Survival Modeling in R. *Journal of Statistical Software*, *70*(8), 1--33. <https://doi.org/10.18637/jss.v070.i08>

2.  Royston P, Parmar MK. Flexible parametric proportional-hazards and proportional-odds models for censored survival data, with application to prognostic modelling and estimation of treatment effects. Stat Med. 2002 2002-08-15;21(15):2175-97.

3.  Shao T, Zhao M, Liang L, Shi L, Tang W. Impact of Extrapolation Model Choices on the Structural Uncertainty in Economic Evaluations for Cancer Immunotherapy: A Case Study of Checkmate 067. Pharmacoecon Open. 2023;7(3):383-392.

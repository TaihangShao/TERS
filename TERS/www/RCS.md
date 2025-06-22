### Function for Restricted Cubic Splines

$$
𝞰_i = 𝞬_0+𝞬_1x+𝞬_2𝛃_1+... +𝞬_{i+1}𝛃_i
$$

$$
B_{im}=(x-k_m)^3_+-𝞴_m(x-k_{min})^3_+-(1-𝞴_m)(x-k_{max})^3_+
$$

$$
𝞴_m = {{k_{max}-k_m} \over {k_{max}-k_{min}}}
$$

Where$B_{im}$ refers to the basis function, $B_i=(B_{i0},B_{i1},...,B_{im})$. *k* is referred to the knot (generally one to five). Although RCS was flexible and could increase the complexity of the model by increasing the number of knots, it would lead to local over-fitting.

#### Citation

1.  Rutherford MJ, Crowther MJ, Lambert PC. The use of restricted cubic splines to approximate complex hazard functions in the analysis of time-to-event data: a simulation study. J Stat Comput Sim. 2015;85(4-6):777-93.

2.  Kearns B, Stevenson MD, Triantafyllopoulos K, Manca A. Generalized Linear Models for Flexible Parametric Modeling of the Hazard Function. Med Decis Making. 2019 2019-10-01;39(7):867-78.

3.  Shao T, Zhao M, Liang L, Shi L, Tang W. Impact of Extrapolation Model Choices on the Structural Uncertainty in Economic Evaluations for Cancer Immunotherapy: A Case Study of Checkmate 067. Pharmacoecon Open. 2023;7(3):383-392.

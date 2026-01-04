### Peak Symmetry

Calculates common asymmetry metrics for chromatographic data including the United States Pharmacopoeia (USP)/European
Pharmacopoeia (EP) **Tailing Factor** ($T_f$), and the **Asymmetry Factor** ($A_s$). Additionally, the 
**Total Peak Analysis (TPA)** workflow proposed by Wahab *et al.* ([2017](https://doi.org/10.1016/j.chroma.2017.06.031)) 
is also implemented for **baseline-resolved** peaks.

The **Basic** tab presents the user with a choice of **Modelling Method**, including:

**Asymmetry Factor $A_s$**
: Calculated as the ratio between peak half-widths at 10% peak height on the **trailing** ($B_{10}$) and **leading** 
($A_{10}$) edges of the peak. Thus, values **greater than 1** indicate tailing, values less than one indicate fronting,
and a tailing factor of exactly 1 is characteristic of a perfectly symmetrical peak.

$$ A_s = B_{10}/A_{10} $$

**USP Tailing Factor $T_f$**
: Another common metric which uses half-widths at **5% peak height**.

$$ T_f = \frac{(A_5+B_5)}{2A_5} $$

**Total Peak Analysis (TPA)**
: Irrespective of whether $T_f$ or $A_s$ is used, acceptance criteria outlined in various pharmacopoeias list a range of **0.8-1.8**
as acceptable. In practice, values of **0.9-1.2** are routinely achieved and desirable for typical test analytes.
Both $T_f$ and $A_s$ provide information only about the **relative** amount of tailing or fronting, but fail to accommodate cases
where both are present. For example, a widened peak with both tailing and fronting (a so-called *Eiffel Tower* effect) may have $T_f$
and $A_s$ values close to 1 despite being heavily distorted from a Gaussian profile. In order to provide separate measures of the absolute
and relative contributions of tailing and fronting to the total peak shape, as well as to effectively visualize the results, this module
also implements the **Total Peak Analysis (TPA)** workflow developed by Wahab *et al.* ([2017](https://doi.org/10.1016/j.chroma.2017.06.031)) 
as a simple visual and quantitative assessment tool. First, the Gaussian standard deviation $\sigma$ is estimated from a peak (with a maximum signal **normalized to unity**) using
the peak width at a chosen percentage of peak height ($W_H$):

$$ \sigma = W_H/(2\sqrt{2ln(1/H)}) $$

The estimation of $\sigma$ takes advantage of the fact that the tops of chromatographic peaks (after **80-85%** peak height) usually follow a
Gaussian distribution closely even for heavily-distorted peaks. Thus, a Gaussian model is constructed using the estimated $\sigma$,
a peak maximum equal to 1, and the actual peak retention time. A linear constrained solver is then used to ensure the top 15% of the
Gaussian model values are either equal to or enclosed by the actual chromatographic peak. Finally, absolute and relative (%) residuals are
calculated separately for the leading and trailing edges of the peak, providing separate quantitative measures of the degree of peak fronting
and tailing. Currently, the TPA procedure is limited to **only assess baseline-resolved peaks**. Each peak is also assessed for
its suitability for TPA based on residual sums to the left and right of the peak apex. The Gaussian model is supposed to be completely enclosed
by the original chromatographic peak, but this is seldom the case, resulting in negative residuals where the model is outside the retention time
boundaries of the peak. If **>50%** of residuals are found to be negative on either side of the peak, it is considered to be unsuitable
for TPA.
<br></br>
#### Other basic options 
**Peaks**
: Allows the user to input one or more numeric values to specify peak indices which should be processed. A **Visual Select**
option is also available for interactive peak selection.

**Determine critical width automatically**
: A checkbox specifying whether or not the critical width parameter used for baseline correction should be estimated
automatically, or specified manually (as an integer).

**Show widths**
: The algorithm calculates widths and half-widths at specific points along the peak. This switch toggles their inclusion
in the **Results Table**.
<br></br>
#### Advanced options
Additional options are available in the **Advanced** tab.

**Optimization Method**
: Specifies the method to use for iterative optimization. Only relevant to **Total Peak Analysis (TPA)**. Available
methods include **Non-Linear Programming** and **Nelder-Mead**.

**Resolution**
: Increasing the resolution will result in more detailed output graphics. **Only relevant to TPA**.

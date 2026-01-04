### Integration

This module is designed to calculate baselines for individual peaks and integrate their areas using the trapezoidal rule.
The workflow assesses peaks based on their resolution. First, baselines for fully-resolved peaks are calculated via the 
FastChrom algorithm (see Johnsen *et al.*, [2013](https://doi.org/10.1039/c3an36276k)). Common baselines for groups of 
unresolved peaks are also derived in the same manner.

The **Basic** tab presents several methods for integration, 
most of which were described by Waters Corporation ([2017](https://www.waters.com/content/dam/waters/en/library/white-papers/2007/720000494en.pdf)).
Boundaries between each pair of unresolved peaks may be separated by a simple **Perpendicular Drop** baseline. 
Perpendicular Drop tends to increasingly distort peak areas with decreasing resolution. 
Alternatively, peaks boundaries are tested for suitability for one of three "skimmed" baseline approaches:
**Tangent Skim**, **Exponential Skim**, or **Gaussian Skim**. This is done by assessing a series of conditions:

1. Is the peak boundary classified as either fused or a shoulder?
2. Is at least one peak within the assessed pair classified as fused or a shoulder?

If either of these initial conditions are false, **no skim is attempted**. Otherwise, additional conditions are evaluated:

3. Is the earlier-occurring peak the parent peak or a child peak (decided based on the height of peak maxima)? 
This determines whether to apply a **front** or a **tail** skim.

4. Is the apex of the child peak lower than the closest (along the x-axis) inflection point of the parent peak? 
If this conditions is false, **Exponential or Gaussian** skimming are not attempted.

5. Is the *Skim-Valley ratio* (**child peak height over that of the inter-peak boundary/valley**) lower than the threshold 
set in skim?

6. Is the height ratio of the parent peak to the child peak higher than the *Dyson Criterion*? 
If either of the conditions dependent are false, **skimming is not carried out**.

7. Is the outer boundary of the child peak higher than the inter-peak boundary? 
If true, tangent skimming is not possible.

These conditions also determine the identity of the parent and child peak as well as the appropriate skim type 
(front or tail) for each peak pair. Thus, if a boundary is determined to be suitable based on the above conditions, 
construction and optimization of a skimmed baseline of the type specified in method is attempted.

For front tangent skim off the parent peak, straight lines are drawn from the inter-peak boundary to each point 
between the beginning and the maximum of the child peak (which occurs earlier in this case). For a tail tangent skim, 
lines are instead drawn between the inter-peak boundary and each point between the maximum and end of the child peak 
(which is now the later-occurring peak). Lines whose y-values are **>2%** of the maximum child peak signal at any point 
are discarded. Among the rest, the line whose end-point is closest to the outer boundary of the child peak and where 
**<40%** of the values are within **1%** of the corresponding chromatographic signal value is selected.

For **Exponential Skim**, the following equation is used to build an exponential curve extending from the inflection point 
of the parent peak closest to the inter-peak boundary towards either the start (for front skim) or end (for tail skim) 
of the child peak.

$$ H_{ex} = H_0\times exp^{(-B\times(t_R-t_0))} + A\times t_R + C $$

Where $H_{ex}$ is the exponential curve value, $H_0$ is the height (signal) at the inter-peak boundary (e.g. valley),
$B$ is the exponential growth/decay function (negative for **tail** skim), $A$ is the slope of the parent peak baseline,
$C$ is the baseline offset of the parent peak, and $t_R$ along with $t_0$ are retention times at $H_b$ and the
inter-peak boundary, respectively. The initial exponential curve is constructed with values of $B$ and $C$ both set to **0**.
The offset constant $C$ is then determined by the different between the result and the signal at the inter-peak boundary,
and constant $B$ of the exponential fit is optimized for minimum **Euclidean Distance** between the curve and original 
chromatographic signal **in the parent peak region** (spanning from the closest inflection point to the inter-peak boundary). 
Finally, the exponential curve spanning from the inter-peak boundary to the outer boundary of the child peak is plotted 
using optimized constants $B$ and $C$. This is used as the skimmed baseline.

For **Gaussian Skim**, the following general form of the Gaussian curve is used to construct a model between the apex of 
the parent peak and the inter-peak boundary.

$$ H_{gs} = H_p\times exp^{-(\frac{t_R-t_0}{\sigma})^2} $$

Where \$H_{gs}$ is the Gaussian curve value, $H_p$ is the parent peak maximum signal, $t_0$ is the corresponding retention
time, $t_R$ is the retention time at $H_{gs}$, and $\sigma$ is the standard deviation of the Gaussian curve (estimated here
as the half-width of the peak at inflection points). The Gaussian curve is iteratively optimized until **Euclidean Distance** 
between the resulting curve and original parent peak is minimized. The final curve is then constructed using optimized 
parameters between the **parent** peak apex and the outer boundary of the **child** peak. 
The model is then checked for two conditions:

1. Is the lowest point of the Gaussian curve higher than **1%** of the parent peak maximum?
2. Are any points of the curve in the **child** peak region above the original signal (i.e. does the curve cross the chromatogram)?

If either of the above is true, the Gaussian curve is rejected, no further skim is attempted, and a Perpendicular Dropline 
is instead constructed. Otherwise, the Gaussian curve is truncated from the **parent** peak maximum until the point where 
the curve is of consistently lower signal (i.e. height) than the original chromatogram.

Once all the baselines are constructed, the **Trapezoidal Rule** is used to integrate all peaks and calculate their 
peak areas ($PA$).

$$ PA = \sum{(x_{i+1}-x_i)\times(y_{i+1}+y_i)/2} $$

The **Results Table** classifies the integration type for each peak as **PD** (Perpendicular Drop),
**TS** (Tangent Skim), **ES** (Exponential Skim), or **GS** (Gaussian Skim).

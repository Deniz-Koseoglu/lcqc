### Baseline Correction

This module carries out baseline correction on the imported chromatogram using various algorithms. The **Basic** tab
houses a dropdown list for selecting between various correction methods:

**Iterative Asymmetric Least Squares (ALS)**
: Iterative convergence based on weighted Eilers (i.e. Whittaker-Henderson) smoothing with a finite difference
penalty of order 2 (Eilers, [2003](https://doi.org/10.1021/ac034173t)). For detailed explanation and examples see,
for example, Peng *et al.* ([2010](https://www.doi.org/10.1016/j.aca.2010.08.033)).

**Chang's Method**
: A robust algorithm for signal-dense chromatograms and spectra developed by Chang *et al.* ([2007](https://doi.org/10.1016/j.jmr.2007.05.008)).

**Iterative Smoothing Splines with Root Error Adjustment (ISREA)**
: Uses smoothing splines to estimate the baseline (Xue *et al.*, [2021](https://www.doi.org/10.1177/0003702820955245)).

**Modified Polynomial Fit (ModPolyFit)**
: Iterative baseline correction algorithm based on automated polynomial fitting developed by 
Lieber & Mahadevan-Jansen ([2003](https://www.doi.org/10.1366/000370203322554518)).

Baseline correction can also be skipped entirely by selecting **None** (the default).
<br></br>
#### Advanced options

The **Advanced** tab changes contextually based on the selected correction method and includes various method-specific
options:

1. **ALS** options include the second derivative constraint (**Lambda**),
weights of positive residuals (**p**), error tolerance (precision) required to reach convergence (**prec**),
and the maximum number of iterations (**Max Iterations**).

2. **Chang's Method** options include the position of the baseline relative to the noise component (**Threshold**),
the high-pass filter parameter (**alpha**), the fraction of low-intensity fragments of the filtered signal that are 
assumed to be baseline (**Baseline Fraction**), the number of segments to divide the filtered signal into (**Segments**),
the signal window size in points (**Signal Window**), and the method used for baseline fitting (linear or cubic; **Fit Method**).

3. **ISREA** options include the convergence criterion from **0.0001** to **10** (**Eta**) and the maximum number of
iterations (**Max Iterations**).

4. **ModPolyFit** options include the degree of polynomial fit (**Degree**), the precision required for convergence
(**Precision**), and the maximum number of iterations (**Max Iterations**).

All methods also include the option to remove negative values from the corrected signal (**Remove Negatives**) such
that the baseline signal values never exceed those of the original.

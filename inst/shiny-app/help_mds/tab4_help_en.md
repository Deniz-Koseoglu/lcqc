### Peak Detection

This is by far the most complex module within LCQC and is used to automatically detect, filter, and classify
chromatographic peaks using 1-dimensional data (only retention time and signal). Peak apices, inflection points,
upslope points, as well as start and end points are determined using a combination of custom and industry-standard
algorithms.

The **Basic** tab includes the following options:
<br></br>

#### Amplitude Threshold Method
Selection of **one or multiple** methods to use for estimation of the **amplitude threshold**, 
below which peak detection is not carried out. Available methods are:

**Z-Scores**
: Calculates global mean values of both the moving average and the moving standard deviation of the signal 
and uses a sensitivity parameter (see **Advanced options**) to calculate the amplitude limit:

$$ AmpLim = \overline{MX} + (1/sens)\times\overline{SD} $$

**Simple Quantiles**
: Amplitude limit is simply determined as the quantile at the specified probability percentage between **0.001 and 50**.

**Relative Differences**
: first calculates the quantile range of input data, derives the maximum difference between successive quantiles, 
locates the earliest (lowest) quantile where a percentage of this difference between **0.001 and 50** is exceeded, 
and uses this quantile as the amplitude limit.
<br></br>

#### Detect and Mitigate Peak Bunching
Detects and removes bunched peak apices using a custom algorithm.
<br></br>

#### Derivative-based peak detection method
Method to use for calculation of inferior (low) and superior (high) first/second derivative thresholds to use 
for peak detection. Available methods are:

**Noise core estimator**
: follows the widely-practised noise-core approach (e.g. Waters Corporation, [2017](https://www.waters.com/content/dam/waters/en/library/white-papers/2007/720000494en.pdf)). 
The noise core is determined as the standard deviation $SD$ distance from the mean $\overline{X}$ of derivatives:

$$ T=\overline{X}\pm sens1\times SD/sens2 $$

Many industry-standard approaches recommend a value of 4 for $sens1$, stating that $\pm 4SD$ best defines chromatographic noise.

**Vaz *et al.* ([2016](https://doi.org/10.5935/0103-5053.20160076)) method**
: Differences between the global median of derivative signal $M_D$ and the derivative signal are calculated and a new
median $M_{new}$ calculated from these results. Finally, the inferior and superior thresholds $T$ are calculated as follows:

$$T=M_D\pm sens1\times M_{new}/sens2$$

Here, $sens1$ is an empirical factor with which the threshold range widens/increases.
Conversely, $sens2$ is inversely related to the threshold range.

**Z-score based**: A slight variation reliant solely on the median of derivatives:

$$T=M_D+sens1\times M_D/sens2$$
<br></br>
#### Outlier Detection method

This dropdown list contains varios methods for removing outliers from the noise signal. These include:

**Interquartile Range**
: All values outside of $Q_1-1.5\times IQR$ and $Q_3+1.5\times IQR$ are removed (where $Q_1$, 
$Q_3$, and $IQR$ are the first quartile, third quartile, and inter-quartile range, respectively).

**Quantile-based**
: All values outside of the 2.5% and 97.5% quantiles are removed.

**Standard deviation-based**
: All values outside $\overline{X}\pm 2.24\times SD$ are removed (where $\overline{x}$ and 
$SD$ are the mean and standard deviation of the derivatives, respectively).
<br></br>
#### Peak Rejection Filters
Four different filters are available for filtering out peaks after initial detection based on the **signal/noise (S/N) ratio**,
peak **height**, inflection point **width**, and peak **area**. These filters are applied in two stages.
Height and S/N ratio filters are applied before peak classification, while peak width and area filters are applied
afterwards. Logic toggles for both filtration stages are also available (**OR** logic is used by default).
<br></br>
#### Advanced options
The **Advanced** tab contains various parameters for fine-tuning of peak detection.

**Sensitivity Parameters**
: These parameters control the sensitivity of peak detection based on derivatives and the signal (amplitude limit).
The **First Derivative** and **Second Derivative** sensitivity values are equivalent to $sens1$ and $sens2$ in the 
equations above, while **Amplitude Limit Sensitivity** is used when calculating Z-scores.

**Z-score Parameters**
: Various parameters that control the detection of peak regions using z-scores. The **Moving Average/SD Lag (integer)**
parameter is the number of observations to base the moving average and standard deviation on. The **Threshold** parameter
is the z-score (factor to multiply standard deviation of the moving average by) at which the algorithm signals the 
presence of positive or negative peaks. Finally, the **Sensitivity to average and SD** parameter is a factor between 
**0 and 1** denoting the relative influence/weight that new data points have on the calculation of moving average 
and moving standard deviation.

**FastChrom Baseline Correction**
: A toggle that enables or disables additional baseline correction using the FastChrom algorithm developed by
Johnsen *et al.* ([2013](https://doi.org/10.1039/c3an36276k)). The corresponding **critical width** parameter must
also be set.

**Boundary Verification Parameters**
: Specify the number of points to survey on either side to confirm (or reject) detected signal and derivative 
extremes (i.e. maxima and minima).

**ApexTrack Baseline Expansion Parameters**
: Here, the user can specify the **liftoff** and **touchdown** parameters of the ApexTrack algorithm as developed
and described by the Waters Corporation ([2017](https://www.waters.com/content/dam/waters/en/library/white-papers/2007/720000494en.pdf)).

**Zero Crossing Parameters**
: Specify the number of points to survey on either side to confirm (or reject) detected zero crossings 
(downcrosses and upcrosses) for signal and derivatives.

#### Peak Classification
All detected peaks listed in the **Peak Table** are classified into **one of four** categories:

1. **B**: Baseline-resolved peaks.
2. **F**: Fused peaks.
3. **S**: Shoulder peaks.
4. **R**: Round peaks. Occur when peaks of nearly equal height fuse at very low (but not zero) resolution.

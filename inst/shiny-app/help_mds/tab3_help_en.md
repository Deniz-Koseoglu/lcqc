### Smoothing

This module is designed for signal as well as first and second derivative smoothing after baseline correction 
is carried out (if at all). The **Basic** tab includes several smoothing methods:

True to its name, **Rectangular** smoothing is a simple *rectangular boxcar* algorithm, which replaces 
each point with the mean of several neighboring points. For example, for a 3-point smooth:

$$S_j=\frac{Y_{j-1}+Y_j+Y_{j+1}}{3}$$

**Triangular** smoothing implements a more complex weighted algorithm. For example, a 5-point smooth becomes:

$$S_j=\frac{Y_{j-2}+2Y_{j-1}+3Y_j+2Y_{j+1}+Y_{j+2}}{9}$$

Finally, the Savitsky-Golay (Savitsky & Golay, [1964](https://doi.org/10.1021/ac60214a047)) smoothing methods, 
also known as digital smoothing polynomial filters, implement moving average-based smoothing via a 
**quadratic** or **quartic** polynomial regression. The latter usually performs better with narrower peaks, 
but may distort wider peaks.
<br></br>
### Advanced options

Here, the number of smoothing **points** and **passes** may be set (defaults to 3 and 3). Alternatively,
these may also be determined automatically using an iterative algorithm developed for LCQC. If automatic
determination is selected, a best-guess number of points and passes must be provided to begin optimization.

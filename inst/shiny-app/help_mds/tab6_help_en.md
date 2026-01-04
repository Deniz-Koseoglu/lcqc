### Deconvolution

This module implements **Iterative Curve Fitting (ICF)** for modeling of chromatographic peaks. ICF fits several Gaussian 
and modified Gaussian models to either a single or a group of chromatographic peaks and minimizes the associated 
**Root Mean Squared Error (RMSE)** of the resulting model through iterative optimization. Thus, a model that resembles 
actual chromatographic signal as closely as possible within the constraints of the chosen model is obtained. ICF is 
especially useful as a peak deconvolution method for moderately or even completely fused peaks.

The options included are described below.
<br></br>
#### Deconvolution Method
The workflow currently implements **4 popular ICF models** and offers active selection of the most appropriate model 
for each examined peak or a group of up to **5 fused peaks**. The available models are described below.

**Simple Gaussian**
: The simplest model based on the following equation (Nikitas *et al.*, [2001](https://doi.org/10.1016/S0021-9673(01)00524-6)):

$$ H_{GS} = H_m\times e^{-((t_R-t_m)/2\sigma)^2} $$ 

Where $H_m$ and $t_R$ are the signal and retention time at peak maximum, respectively, $t_R$ is the current retention time,
and $\sigma$ is the standard deviation of the curve, effectively approximated as half of the peak width at inflection point height.
The Gaussian model thus has only 3 parameters that require optimization and is therefore computationally cheap, 
but **cannot accommodate fronting or tailing** phenomena often observed in chromatographic data.

**Expontentially Modified Gaussian (EMG)**
: The EMG model (e.g. Li, [1995](https://www.doi.org/10.1093/chromsci/33.10.568); 
Nikitas *et al.*, [2001](https://doi.org/10.1016/S0021-9673(01)00524-6); 
Caballero *et al.*, [2002](https://doi.org/10.1016/S0021-9673(02)00194-2); 
Kalambet *et al.*, [2011](https://doi.org/10.1002/cem.1343)) incorporates an exponential component into the Gaussian 
model and is by far the most popular approach to modeling chromatographic peaks with tailing and fronting:

$$ H_{EMG} = A\times e^{0.5\times(\sigma/\tau)^2 - ((t_R-t_0)/\tau)}\times\mathcal{P}((t_R-t_0)/\sigma - \sigma/\tau)/\tau $$

Sometimes also written as (Li, [1997](https://www.doi.org/10.1021/ac970481d)):

$$ H_{EMG} = A/2\tau\times e^{(\sigma^2/2\tau^2 + (t_0-t_R)/\tau)}\times(1+erf((t_R-t_0)/\sqrt{2\sigma} - \sigma/\sqrt{2\tau})) $$ 

Where new parameters $A$ and $\tau$ are, respectively, the peak area and the exponential time constant, which is 
negative for fronting peaks and positive for tailing peaks. The EMG model takes longer than GS or EGH to compute 
and appears to be **less stable**.

**Exponential-Gaussian Hybrid**
: The EGH model (Lan & Jorgenson, [2001](https://doi.org/10.1016/S0021-9673(01)00594-5); 
Li, [2002](https://doi.org/10.1016/S0021-9673(02)00090-0)) is a simplified empirical equation that also accommodates 
fronting and tailing peaks by including a truncated exponential component into the equation:

$$ H_{egh} = H_m\times e^{-(t_{R2}-t_0)^2/(2\sigma^2 + \tau\times(t_{R2}-t_0))} $$

Where $t_{R2}$ are those retention times where $(2\sigma^2 + \tau\times(t_R-t_0)) > 0$. EGH **converges significantly faster** 
than EMG, but somewhat lacks flexibility for modeling fronting peaks (similarly to EMG).

**Empirically Transformed Gaussian**
: Finally, the ETG model (Li, [1995](https://www.doi.org/10.1093/chromsci/33.10.568), 
[1997](https://www.doi.org/10.1021/ac970481d), [2002](https://doi.org/10.1016/S0021-9673(02)00090-0)) is unique in 
incorporating 6 parameters describing the **leading** ($k_l$, $\lambda_l$, and $\alpha$) and **trailing** ($k_r$, 
$\lambda_r$, and $\beta$) peak edges, respectively. Including the peak height $H_m$, a total of 7 parameters are 
optimized. Additionally, the function requires estimates of the left and right inflection point times ($t_l$ and
$t_r$) for each peak, which remain constant and are not optimized:

$$ H_{etg} = (2H_me^{0.5})/((1 + \lambda_l e^{k_l(t_l - t)})^\alpha + (1 + \lambda_r e^{k_r(t - t_r)})^\beta - 1) $$

The ETG model offers a unique advantage of loose coupling between the descriptions of leading and trailing peak edges, which
are only related by one peak amplitude parameter (derived from peak height). Additionally, despite the iterative optimization
of **7 parameters**, the fitting procedure converges rapidly (is **not** computationally expensive).

All of the above functions are submitted for iterative optimization via a penalty function based on the 
**Root Mean Squared Error (RMSE)**, which may be represented as follows:

$$ RMSE = \sum{\sqrt{(y_i - \hat{y}_i)^2/n}} $$

Where $y_i$, $\hat{y}_i$, and $n$ are the original signal, modeled curve, and sample size (number of points in the 
original data), respectively. This error metric is used to evaluate model performance and **select the best-suited model** 
among GS, EMG, EGH, and ETG during iterative optimization.

#### Other basic options

**Determine critical width automatically**
: The workflow uses the FastChrom algorithm (Johnsen *et al.*, [2013](https://doi.org/10.1039/c3an36276k)) to build baselines for each peak. The algorithm
utilizes a critical width parameter, which results in less strict baseline correction rules as it is increased. This
parameter can either be set manually (as an integer) or determined automatically based on the checkbox value.

**Model baseline-resolved peaks**
: By default, baseline-resolved peaks are not modelled since this is not necessary for integrating their peak area
accurately. However, they may be modelled by activating this checkbox.
<br></br>
#### Advanced options
The **Advanced** tab presents additional options:

**Optimization Method**
: The algorithm(s) used to reach convergence during iterative fitting. Options include **Nelder-Mead**,
**Broyden-Fletcher-Goldfarb-Shanno (BFGS)** and its box-constrained variant, as well as **Simulated Annealing**.
All algorithms are used by default, with the best-performing one chosen for each peak based on the **RMSE**.

**EMG representation**
: Selects the form of the EMG equation, where **EMG1** is the default and **EMG2** is the form written by
Lee *et al.* ([1997](https://www.doi.org/10.1021/ac970481d)). See EMG equations above for more details.

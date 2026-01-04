### Theoretical Plates

This module calculates the theoretical plate number $N$, **Height Equivalent to a Theoretical Plate ($HETP$)**,
reduced plate height $h$, and separation impedance $E$ using various methods such as **5-sigma** (**S5**),
**European/British Pharmacopoeia** (**EP**), Area-Height (**AH**), and **Exponentially-Modified Gaussian** (**EMG**).

The number of theoretical plates $N$ is a widely-used column performance index where a higher number of theoretical plates
represents better column efficiency. Several methods of calculation are implemented LCQC. The **Full Width (FW)** method
uses the base width $W_b$ of a Gaussian peak defined as the baseline intercepts of tangent lines for a perfect Gaussian
peak, which occur at **13.4% peak height**.

$$ N_{FW} = 16(t_R/W_b)^2 $$

The **5-sigma (S5)** method utilises the fact that a perfectly Gaussian peak is exactly 5-sigma (i.e. 5 standard deviations)
wide at a **4.4% height** (Bidlingmeyer & Warren Jr., [1984](https://www.doi.org/10.1021/ac00278a002); 
Villalon, [2023](https://doi.org/10.1021/acs.jchemed.2c00588)) and uses the 5-sigma measure to determine width.
This helps cope with tailing often encountered in chromatography, which distorts the peak from its Gaussian shape.

$$ N_{5\sigma} = 25(t_R/W_{5\sigma})^2 $$

The **European Pharmacopoeia (EP)** method is perhaps the most popular and uses the peak width at half-height ($W_{50}$) along
with retention time $t_R$. Since the coefficient used varies from **5.55** in the German (DAB), British (BP), and European (EP)
Pharmacopoeias to **5.54** in the Japanese Pharmacopoeia (*Rev. 15, April 2006*), a mean of these (**5.545**) was implemented herein.

$$ N_{EP} = 5.545(t_R/W_{50})^2 $$

An alternative equation uses the theoretical Gaussian inflection point width at ca. **60.7%** peak height.

$$ N_{inf} = 4(t_R/w_{60.7})^2 $$

True to its name, the **Area-Height (AH)** method utilizes peak area \eqn{A} and height \eqn{H} to calculate \eqn{N}.

$$ N_{AH} = 2\pi(t_RH/A)^2 $$

All of the above methods are based on a true Gaussian peak shape, which is almost never encountered in practical chromatography.
Tailing and (less prevalent) fronting phenomena distort the peak shape and cause errors in calculation. A simple equation that
results in "approximately accurate" (Meyer, [2010](https://www.doi.org/10.1002/0470032677))
values of $N$ and based on the Exponentially-Modified Gaussian (EMG) model
was proposed by Foley & Dorsey ([1983](https://www.doi.org/10.1021/ac00255a033)).
The equation uses the peak width ($W_{10}$) and half-widths at 10% height
($b$ and $a$ for trailing and leading width, respectively). The number of theoretical plates obtained via this equation
is usually lower than that from other methods.

$$ N_{EMG} = (41.7(t_R/W_{10})^2)/(b/a + 1.25) $$

Also calculated is the plate height, also known as **Height Equivalent to a Theoretical Plate (HETP)** is the distance in mm (or µm)
over which chromatographic equilibrium is achieved. This is simply related to $N$ and column length $L$ by:

$$ HETP = L/N $$

From HETP, the reduced plate height $h$ may also be calculated provided the average particle size of the stationary phase
$d_p$ (in µm) is known; $h$ is a dimensionless parameter and may be used to compare columns of different length
and particle size more easily. Values of $h$ should be in range of **2 to 5 (lower is better)**. For example, at a value
of 3, complete chromatographic equilibrium is obtained over 3 layers of stationary phase (Meyer, [2010](https://www.doi.org/10.1002/0470032677)).

$$ h = HETP/d_p $$

Finally, Separation Impedance $E$ is a measure of column "quality" (efficiency) that incorporates back pressure ($\Delta p$),
number of theoretical plates, dynamic viscosity of the mobile phase ($\eta$, in mPas), and breakthrough time.
Values of **>10000** are imperative for a liquid chromatography process to be considered "high performance" 
(Meyer, [2010](https://www.doi.org/10.1002/0470032677)).
In LCQC, $E$ may either be calculated separately for each individual value of $N$ (much like $HETP$ and $h$)
when **Impedance Method** includes the option **Individual**. In this case, the following equation is used:

$$ E = (\Delta p t_0)/(N^2\eta) $$

Additionally, a universal equation independent of $N$ may be used when the method is set to **Universal** (default).

$$ E = (10^8/5.54^2)\times(\Delta p t_0/\eta)\times(W_{50}/t_R)^4 $$

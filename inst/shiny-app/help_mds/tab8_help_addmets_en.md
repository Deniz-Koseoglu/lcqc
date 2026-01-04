### Column-Specific Metrics

This module uses various column characteristics such as length, particle size,
flow rate, internal diameter (among others) to calculate various column performance indicators.

The **Linear Velocity** $\mu$ of the mobile phase is simply related to column length $L_c$ and true breakthrough time $t_0$.

$$ \mu = L_c/t_0 $$

It may be beneficial to verify breakthrough time in this equation, i.e. determine whether the non-retained analyte is really passing
through the column at the velocity of the mobile phase, by calculating **Packing Porosity** $\epsilon$. The packing porosity of
**bonded** silica-based stationary phases is *ca.* 0.65. Using this value as reference, $\epsilon$ may be calculated
simply from $t_0\text{ }(s)$, $L_c\text{ }(mm)$, flow rate $F\text{ }(mL\text{ }min^{-1})$, and inner column diameter
$d_c^2\text{ }(mm^2)$.

$$ \epsilon = 21\times[(Ft_0)/(d_c^2L_c)] $$

Significantly higher or lower values of $\epsilon$ (>1 and <0.5, respectively) indicate retention of the analyte or its exclusion
from the pores of the stationary phase (Meyer, [2010]()).

Another useful metric is **Permeability** $K$, which incorporates back pressure $\Delta\rho\text{ }(bar)$. A large value of $K$
indicates poor column packing, while the reverse is characteristic of a leak. Values of $15\text{ }mm^2\text{ }s^{-1}\text{ }bar^{-1}$
are typical for bonded phases at a fast flow rate of $1.6\text{ }mL\text{ }min^{-1}$.

$$ K = L_c^2/(\Delta\rho t_0) $$

**Specific Permeability** $K^{\circ}\text{ }(mm^{-2})$ may also be calculated (typically $4.0\times 10^{-8}\text{ }mm^2$ for bonded
phases) by incorporating flow rate, dynamic viscosity, column length, inner diameter, and back pressure.

$$ K^{\circ} = 21\times 10^-8\times[(F\eta L_c)/(d_c^2\Delta\rho)] $$

Finally, $K$ may be represented as a dimensionless metric called Flow Resistance $\Phi$, which facilitates comparison of different columns.
One of the additional required parameters is particle size $d_p\text{ }(\mu m)$.

$$ \Phi = 4.7\times[(\Delta\rho d_p^2 d_c^2)/(L_c \eta F)] $$

A typical $\Phi$ value of **1000** is observed for packed HPLC columns. Significant upward and downward deviations (e.g. **>2000 or <500**)
are indicative of a **blockage or voids in the packing**, respectively.

For calculation of theoretical plates and additional dimensionless metrics such as reduced plate height and Separation Impedance
to assess column performance, see the **Theoretical Plates** module.


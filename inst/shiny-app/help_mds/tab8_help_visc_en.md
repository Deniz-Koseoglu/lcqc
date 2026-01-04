### Viscosity Calculation

This module estimates the temperature-dependent dynamic viscosity $\eta$ (in mPas) for pure
organic solvents and their mixtures commonly used as mobile phases for High-Performance Liquid Chromatography (HPLC).
It is also possible to estimate viscosities of aqueous mobile phases containing methanol (MeOH) or acetonitrile (MeCN) based on
bilinear interpolation of experimental data collected from various sources.

Dynamic viscosity and density of various organic solvents is calculated using data taken from the Dortmund Data Bank (DDB, 
[2024](https://www.ddbst.com/calculation.html)) and packaged with LCQC. Specifically, dynamic viscosity $\eta$ (mPas) 
is calculated using the **Vogel Equation** (García-Colín et al., [1989](https://www.doi.org/10.1103/PhysRevB.40.7040)), which uses 
experimentally derived empirical coefficients $A_V$, $B_V$, and $C_V$. The result is also dependent on temperature, given as 
$T_K$ in Kelvin.A full list of coefficients and the temperature ranges (**in degrees Celsius**) within which they are valid 
is shown below.

<table class="table table-striped table-bordered" style="font-size: 0.85em;">
<thead>
<tr>
<th style="text-align: center;">Name</th>
<th style="text-align: center;">CAS</th>
<th style="text-align: center;">Formula</th>
<th style="text-align: center;">\(RMM\)</th>
<th style="text-align: center;">\(A_V\)</th>
<th style="text-align: center;">\(B_V\)</th>
<th style="text-align: center;">\(C_V\)</th>
<th style="text-align: center;">\(T_{min}\) (&deg;C)</th>
<th style="text-align: center;">\(T_{max}\) (&deg;C)</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: center;">n-Butanol</td>
<td style="text-align: center;">71-36-3</td>
<td style="text-align: center;">C4H10O</td>
<td style="text-align: center;">74.123</td>
<td style="text-align: center;">-6.25679</td>
<td style="text-align: center;">1957.350</td>
<td style="text-align: center;">-24.9679</td>
<td style="text-align: center;">-87.15</td>
<td style="text-align: center;">199.85</td>
</tr>
<tr>
<td style="text-align: center;">Isopropanol</td>
<td style="text-align: center;">67-63-0</td>
<td style="text-align: center;">C3H8O</td>
<td style="text-align: center;">60.096</td>
<td style="text-align: center;">-9.65162</td>
<td style="text-align: center;">3602.990</td>
<td style="text-align: center;">49.2069</td>
<td style="text-align: center;">-10.15</td>
<td style="text-align: center;">77.85</td>
</tr>
<tr>
<td style="text-align: center;">Acetone</td>
<td style="text-align: center;">67-64-1</td>
<td style="text-align: center;">C3H6O</td>
<td style="text-align: center;">58.080</td>
<td style="text-align: center;">-3.37955</td>
<td style="text-align: center;">553.403</td>
<td style="text-align: center;">-46.9657</td>
<td style="text-align: center;">-90.15</td>
<td style="text-align: center;">199.85</td>
</tr>
<tr>
<td style="text-align: center;">Acetonitrile</td>
<td style="text-align: center;">75-05-8</td>
<td style="text-align: center;">C2H3N</td>
<td style="text-align: center;">41.053</td>
<td style="text-align: center;">-3.15868</td>
<td style="text-align: center;">459.982</td>
<td style="text-align: center;">-78.3648</td>
<td style="text-align: center;">-40.15</td>
<td style="text-align: center;">99.85</td>
</tr>
<tr>
<td style="text-align: center;">Benzene</td>
<td style="text-align: center;">71-43-2</td>
<td style="text-align: center;">C6H6</td>
<td style="text-align: center;">78.114</td>
<td style="text-align: center;">-7.92779</td>
<td style="text-align: center;">4313.540</td>
<td style="text-align: center;">283.2900</td>
<td style="text-align: center;">-0.15</td>
<td style="text-align: center;">274.85</td>
</tr>
<tr>
<td style="text-align: center;">Chloroform</td>
<td style="text-align: center;">67-66-3</td>
<td style="text-align: center;">CHCl3</td>
<td style="text-align: center;">119.370</td>
<td style="text-align: center;">-3.65891</td>
<td style="text-align: center;">966.038</td>
<td style="text-align: center;">19.0192</td>
<td style="text-align: center;">-64.15</td>
<td style="text-align: center;">60.85</td>
</tr>
<tr>
<td style="text-align: center;">Cyclohexane</td>
<td style="text-align: center;">110-82-7</td>
<td style="text-align: center;">C6H12</td>
<td style="text-align: center;">84.162</td>
<td style="text-align: center;">-6.29460</td>
<td style="text-align: center;">2340.160</td>
<td style="text-align: center;">80.1952</td>
<td style="text-align: center;">5.85</td>
<td style="text-align: center;">234.85</td>
</tr>
<tr>
<td style="text-align: center;">Diethyl Ether</td>
<td style="text-align: center;">60-29-7</td>
<td style="text-align: center;">C4H10O</td>
<td style="text-align: center;">74.123</td>
<td style="text-align: center;">-5.13316</td>
<td style="text-align: center;">1286.030</td>
<td style="text-align: center;">55.8587</td>
<td style="text-align: center;">-116.15</td>
<td style="text-align: center;">99.85</td>
</tr>
<tr>
<td style="text-align: center;">Ethanol</td>
<td style="text-align: center;">64-17-5</td>
<td style="text-align: center;">C2H6O</td>
<td style="text-align: center;">46.069</td>
<td style="text-align: center;">-7.37146</td>
<td style="text-align: center;">2770.250</td>
<td style="text-align: center;">74.6787</td>
<td style="text-align: center;">-114.15</td>
<td style="text-align: center;">242.85</td>
</tr>
<tr>
<td style="text-align: center;">Ethyl Acetate</td>
<td style="text-align: center;">141-78-6</td>
<td style="text-align: center;">C4H8O2</td>
<td style="text-align: center;">88.106</td>
<td style="text-align: center;">-16.84550</td>
<td style="text-align: center;">25611.000</td>
<td style="text-align: center;">1302.4800</td>
<td style="text-align: center;">-0.15</td>
<td style="text-align: center;">219.85</td>
</tr>
<tr>
<td style="text-align: center;">Methanol</td>
<td style="text-align: center;">67-56-1</td>
<td style="text-align: center;">CH4O</td>
<td style="text-align: center;">32.040</td>
<td style="text-align: center;">-6.75620</td>
<td style="text-align: center;">2337.240</td>
<td style="text-align: center;">84.0853</td>
<td style="text-align: center;">-90.15</td>
<td style="text-align: center;">189.85</td>
</tr>
<tr>
<td style="text-align: center;">Tetrahydrofuran</td>
<td style="text-align: center;">109-99-9</td>
<td style="text-align: center;">C4H8O</td>
<td style="text-align: center;">72.107</td>
<td style="text-align: center;">-4.20583</td>
<td style="text-align: center;">1147.480</td>
<td style="text-align: center;">34.8631</td>
<td style="text-align: center;">-75.15</td>
<td style="text-align: center;">79.85</td>
</tr>
<tr>
<td style="text-align: center;">Water</td>
<td style="text-align: center;">7732-18-5</td>
<td style="text-align: center;">H2O</td>
<td style="text-align: center;">18.015</td>
<td style="text-align: center;">-3.71880</td>
<td style="text-align: center;">578.919</td>
<td style="text-align: center;">-137.5460</td>
<td style="text-align: center;">-0.15</td>
<td style="text-align: center;">99.85</td>
</tr>
</tbody>
</table>

Thus, to calculate dynamic viscosity:

$$\eta\text{ }(mPas) = exp[A_V+(B_V/(C_V + T_K))]$$

Similarly, density $\rho$ (kg m^-3^) is calculated via the **DIPPR105 equation**, which incorporates 4 empirical coefficients
($A$, $B$, $C$, and $D$) alongside temperature (DDB, [2024](https://www.ddbst.com/calculation.html); 
Silva et al., [2018](https://doi.org/10.1002/jctb.5526)). A list of these coefficients used in LCQC is displayed below.

<table class="table table-striped table-bordered" style="font-size: 0.85em;">
<thead>
<tr>
<th style="text-align: center;">Name</th>
<th style="text-align: center;">CAS</th>
<th style="text-align: center;">Formula</th>
<th style="text-align: center;">RMM</th>
<th style="text-align: center;">\(A\)</th>
<th style="text-align: center;">\(B\)</th>
<th style="text-align: center;">\(C\)</th>
<th style="text-align: center;">\(D\)</th>
<th style="text-align: center;">\(T_{min}\) (&deg;C)</th>
<th style="text-align: center;">\(T_{max}\) (&deg;C)</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: center;">n-Butanol</td>
<td style="text-align: center;">71-36-3</td>
<td style="text-align: center;">C4H10O</td>
<td style="text-align: center;">74.123</td>
<td style="text-align: center;">9.87035</td>
<td style="text-align: center;">0.0998069</td>
<td style="text-align: center;">568.017</td>
<td style="text-align: center;">0.126276</td>
<td style="text-align: center;">-60.15</td>
<td style="text-align: center;">284.85</td>
</tr>
<tr>
<td style="text-align: center;">Isopropanol</td>
<td style="text-align: center;">67-63-0</td>
<td style="text-align: center;">C3H8O</td>
<td style="text-align: center;">60.096</td>
<td style="text-align: center;">74.52370</td>
<td style="text-align: center;">0.2734200</td>
<td style="text-align: center;">508.300</td>
<td style="text-align: center;">0.235299</td>
<td style="text-align: center;">-87.15</td>
<td style="text-align: center;">233.85</td>
</tr>
<tr>
<td style="text-align: center;">Acetone</td>
<td style="text-align: center;">67-64-1</td>
<td style="text-align: center;">C3H6O</td>
<td style="text-align: center;">58.080</td>
<td style="text-align: center;">57.62140</td>
<td style="text-align: center;">0.2339550</td>
<td style="text-align: center;">507.803</td>
<td style="text-align: center;">0.254167</td>
<td style="text-align: center;">-90.15</td>
<td style="text-align: center;">233.85</td>
</tr>
<tr>
<td style="text-align: center;">Acetonitrile</td>
<td style="text-align: center;">75-05-8</td>
<td style="text-align: center;">C2H3N</td>
<td style="text-align: center;">41.053</td>
<td style="text-align: center;">76.91380</td>
<td style="text-align: center;">0.2678180</td>
<td style="text-align: center;">547.850</td>
<td style="text-align: center;">0.353687</td>
<td style="text-align: center;">-20.15</td>
<td style="text-align: center;">273.85</td>
</tr>
<tr>
<td style="text-align: center;">Benzene</td>
<td style="text-align: center;">71-43-2</td>
<td style="text-align: center;">C6H6</td>
<td style="text-align: center;">78.114</td>
<td style="text-align: center;">80.36820</td>
<td style="text-align: center;">0.2664570</td>
<td style="text-align: center;">561.650</td>
<td style="text-align: center;">0.287970</td>
<td style="text-align: center;">-0.15</td>
<td style="text-align: center;">287.85</td>
</tr>
<tr>
<td style="text-align: center;">Chloroform</td>
<td style="text-align: center;">67-66-3</td>
<td style="text-align: center;">CHCl3</td>
<td style="text-align: center;">119.370</td>
<td style="text-align: center;">102.95700</td>
<td style="text-align: center;">0.2312030</td>
<td style="text-align: center;">536.236</td>
<td style="text-align: center;">0.244763</td>
<td style="text-align: center;">-70.15</td>
<td style="text-align: center;">261.85</td>
</tr>
<tr>
<td style="text-align: center;">Cyclohexane</td>
<td style="text-align: center;">110-82-7</td>
<td style="text-align: center;">C6H12</td>
<td style="text-align: center;">84.162</td>
<td style="text-align: center;">30.73260</td>
<td style="text-align: center;">0.1744220</td>
<td style="text-align: center;">580.003</td>
<td style="text-align: center;">0.229418</td>
<td style="text-align: center;">7.85</td>
<td style="text-align: center;">199.85</td>
</tr>
<tr>
<td style="text-align: center;">Diethyl Ether</td>
<td style="text-align: center;">60-29-7</td>
<td style="text-align: center;">C4H10O</td>
<td style="text-align: center;">74.123</td>
<td style="text-align: center;">70.63610</td>
<td style="text-align: center;">0.2678200</td>
<td style="text-align: center;">466.578</td>
<td style="text-align: center;">0.282430</td>
<td style="text-align: center;">-120.15</td>
<td style="text-align: center;">192.85</td>
</tr>
<tr>
<td style="text-align: center;">Ethanol</td>
<td style="text-align: center;">64-17-5</td>
<td style="text-align: center;">C2H6O</td>
<td style="text-align: center;">46.069</td>
<td style="text-align: center;">99.39740</td>
<td style="text-align: center;">0.3107290</td>
<td style="text-align: center;">513.180</td>
<td style="text-align: center;">0.305143</td>
<td style="text-align: center;">-82.15</td>
<td style="text-align: center;">239.85</td>
</tr>
<tr>
<td style="text-align: center;">Ethyl Acetate</td>
<td style="text-align: center;">141-78-6</td>
<td style="text-align: center;">C4H8O2</td>
<td style="text-align: center;">88.106</td>
<td style="text-align: center;">72.56800</td>
<td style="text-align: center;">0.2475260</td>
<td style="text-align: center;">523.354</td>
<td style="text-align: center;">0.266298</td>
<td style="text-align: center;">-20.15</td>
<td style="text-align: center;">248.85</td>
</tr>
<tr>
<td style="text-align: center;">Methanol</td>
<td style="text-align: center;">67-56-1</td>
<td style="text-align: center;">CH4O</td>
<td style="text-align: center;">32.040</td>
<td style="text-align: center;">54.56600</td>
<td style="text-align: center;">0.2332110</td>
<td style="text-align: center;">513.160</td>
<td style="text-align: center;">0.208875</td>
<td style="text-align: center;">-92.15</td>
<td style="text-align: center;">239.85</td>
</tr>
<tr>
<td style="text-align: center;">Tetrahydrofuran</td>
<td style="text-align: center;">109-99-9</td>
<td style="text-align: center;">C4H8O</td>
<td style="text-align: center;">72.107</td>
<td style="text-align: center;">178.54900</td>
<td style="text-align: center;">0.3895300</td>
<td style="text-align: center;">536.671</td>
<td style="text-align: center;">0.450984</td>
<td style="text-align: center;">-70.15</td>
<td style="text-align: center;">259.85</td>
</tr>
<tr>
<td style="text-align: center;">Water</td>
<td style="text-align: center;">7732-18-5</td>
<td style="text-align: center;">H2O</td>
<td style="text-align: center;">18.015</td>
<td style="text-align: center;">0.14395</td>
<td style="text-align: center;">0.0112000</td>
<td style="text-align: center;">649.727</td>
<td style="text-align: center;">0.051070</td>
<td style="text-align: center;">-0.15</td>
<td style="text-align: center;">374.85</td>
</tr>
</tbody>
</table>


The following equation is then utilized for density calculation:

$$\rho\text{ }(kg\text{ }m^{-3}) = A/B^{1+(1-T_K/C)^D}$$

When a mixture of components is provided in **Mobile phase components**, they have to be accompanied by mole, mass,
or volumetric (default) fractions given in appropriate fields. Whichever fraction type is chosen,
the others are calculated and included in the output. For example, conversion to $F_{mass}$ and $F_{mol}$ from
$F_{vol}$ is carried out using density $\rho$ and relative molecular mass ($RMM$, g/mol) as follows for each component.

$$F_{mass} = F_{vol}\times\rho$$
$$F_{mol} = F_{mass}/RMM$$

The overall viscosity $\eta_{total}$ of the mixtures is calculated using the *Linear Blend Rule* **for purely organic mixtures**.
Briefly, the contributing **mole** fraction ($F_{mol}$) of each component $i$ is multiplied by the corresponding viscosity
value of the pure component, and results for all components obtained in this manner are summed.

$$\eta_{total} = \sum{F_{mol(i)}*\eta_i}$$

For **organic-water mixtures**, the Linear Blend Rule is not applicable due to the strong interactions between water molecules
(Snyder et al., [1997](https://www.doi.org/10.1002/9781118592014.app2)). For this reason, LCQC is *currently* limited to estimating 
the viscosity of such mixtures via **bilinear interpolation** of experimental data obtained for the two most popular aqueous 
mobile phases in HPLC, **Methanol-Water** and **Acetonitrile-Water**, from various sources 
(Snyder et al., [2009](https://doi.org/10.1002/9780470508183.app1); Thompson et al., [2006](https://doi.org/10.1016/j.chroma.2006.09.006);
Teutenberg et al., [2009](https://doi.org/10.1016/j.chroma.2009.09.075); Wohlfarth & Wohlfahrt, [2001](https://www.doi.org/10.1007/10639275_6)).

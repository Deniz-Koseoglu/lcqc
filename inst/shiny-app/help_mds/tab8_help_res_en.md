### Resolution

This module calculates chromatographic resolution using the full width, width at 50%, and separation factor methods.
For a detailed review, see Meyer ([2010](https://www.doi.org/10.1002/0470032677)).

Chromatographic resolution $R$ may be calculated between two neighbouring peaks using various approaches which incorporate, in one
form or another, information about both peak apices and widths. Only the peak apices are separated at $R = 1$, while near-complete
resolution of similarly-sized peaks is achieved at $R >= 1.5$. For pairs of earlier-eluting (**Peaks 1**) and a **later-eluting** peaks 
 (**Peaks 2**), the simpler and more widespread calculation methods involve the use of retention times $t_{R1}$ and $t_{R2}$, 
and corresponding peak widths at either the base ($W_1$ and $W_2$) or 50% peak height ($W_{0.5h1}$ and $W_{0.5h2}$). 
Three different equations commonly utilized with these parameters are presented below. The first of these uses the full peak width at 
the base and is therefore the most conservative measure that is relatively more affected by non-Gaussian peak shapes, often resulting 
in lower values than those obtained by other methods.

$$ R = (t_{R2}-t_{R1})/(0.5\times(W_1+W_2)) $$

$$ R = 1.176\times[(t_{R2}-t_{R1})/(W_{0.5h1}+W_{0.5h2})] $$

$$ R = (t_{R2}-t_{R1})/[1.7\times 0.5\times(W_{0.5h1}+W_{0.5h2})] $$

**For isocratic separations**, where the number of theoretical plates between adjacent peaks should largely be the same, the value
of $R$ may also be related to peak retention factors ($k_1$ and $k_2$), separation factor $\alpha$, and average number of
theoretical plates $\overline{N}$ by the following equation (sometimes referred to as the **Fundamental Resolution Equation**):

$$ R = (\sqrt{\overline{N}}/4)\times[(\alpha-1)/\alpha]\times[k_2/(1+k_2)] $$

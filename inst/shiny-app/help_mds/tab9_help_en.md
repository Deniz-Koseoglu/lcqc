### Reporting

This module is used to export concise .PDF performance reports for HPLC columns to serve as a **quality assurance certificate**.
The provided options are mostly self-explanatory and are listed below.

#### Report Options
This module allows the user to select which peak data to include in the report, with options to assign names and concentrations.

**Chart Selection**
: Select the source of the chromatogram to display in the report. When **Peak Integration** or **Peak Deconvolution** is selected,
data is taken from modules **5. Integration** or **6. Deconvolution**, respectively.

**Enter Peak Indices**
: Allows the user to enter the **indices** peaks to be included in the report, separated either by a **colon** (:) or **commas** (,).
An interactive peak selector pop-up window is also available under **Visual Select**.

**Enter Peak Names**
: Facilitates entry of peak names (**optional**). Also available in **Visual Select**.

**Enter Peak Concentrations**
: Facilitates entry of peak concentrations (**optional**). Also available in **Visual Select**.

**Concentration units**
: Used to specify a text string to use as the concentration unit (e.g. **ppm**).
<br></br>
#### Theoretical Plates
The user may select which theoretical plate metrics to include in the report in this section. Currently, metrics from 4 different methods
may be included. These are: **European Pharmacopoeia (EP)**, **Area-Height (AH)**, **5-Sigma (S5)**, and/or **Exponentially-Modified Gaussian (EMG)**.
Data are taken from module **8. Performance Metrics**.
<br></br>
#### Asymmetry Metrics
Similarly to the previous section, asymmetry metrics to be included are chosen here, such as the **Asymmetry Factor (As)**,
**USP Tailing Factor (Tf)**, and/or **Total Peak Analysis (TPA)** plots (if available). Note that for the latter, up to **6 plots**
are currently supported. Data are taken from module **7. Peak Symmetry**.
<br></br>
#### Column-Specific Metrics
The checkbox allows optional inclusion of column-specific additional metrics from module **8. Performance Metrics** (if available).
<br></br>
#### Report Content
This section allows the user to select static information to display in the report, such as the column serial number, mobile phase,
document number, name of the quality control operator etc. Example placeholders are provided.
<br></br>
#### Output Settings
In this section, a custom company logo (in **.PNG** or **.JPG** format) may be uploaded to be displayed in the document header.
The aspect ratio of the chromatogram may also be chosen here. Finally, the report may be exported after selecting an **existing**
output directory.

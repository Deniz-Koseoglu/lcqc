### File Data Import

This module facilitates the import of ASCII (e.g. .txt) files into LCQC. The initial **Data Source** drop-down list is
used to select between two import modes:


**CSV File**
: A simple chromatogram text file containing two mandatory columns, including **retention time** and **signal**.

**Shimadzu TXT File**
: The ASCII output file obtained from a Shimadzu chromatograph software LabSolutions or GCMSSolutions.
<br/><br/>
#### CSV File import

Once a .CSV file is selected using the **Browse** button, the time and signal columns may be selected using their
respective drop-down lists. Initially, the first and second detected columns are automatically assigned as time
and signal, respectively. The lower and upper **Retention Time (RT)** cutoffs may also be specified. Finally,
the **Advanced** tab includes options for decimal and column separators.
<br/><br/>
#### Shimadzu TXT File import

This importer function works specifically with complex ASCII files exported from [LabSolutions](https://www.shimadzu.com/an/products/software-informatics/labsolutions-series/index.html) software by
Shimadzu Corporation. All options from the **.CSV File** importer remain, including selectors for **Time** and **Signal**
columns, **Retention Time** cutoff limits, as well as **decimal and column separators**. ==Additional Options== include:

#### Basic tab
1. **Chromatogram Mode:** Selection of chromatograph type from which data originated (GC-FID, GC-MS, or HPLC).
2. **Choose Peak Table:** Allows the user to select which peak table to display (among those detected).

#### Advanced tab
3. **Extract Similarity Table (GC-MS only):** Choose whether to retrieve the table containing **similarity scores** for
MS-detected peaks.
4. **Extract Peak Table**: Choose whether to retrieve the qualitative peak table.
5. **Retrieve Peak Names**: Should peak names be extracted separately?
6. **Retrieve CAS numbers (GC-SM only)**: Should CAS numbers be retrieved separately?
7. **Retrieve Metadata**: Should analysis metadata be retrieved?
8. **Fix Column Names**: Should column names be made syntactically correct?
9. **Filter Columns**: Removes all but the most relevant columns from the **peak table**.

# Risk-optimal active learning with informative abstention

R code and reproducibility materials for the manuscript:

**Risk-optimal active learning with informative abstention**

Fariborz Setoudehtazangi, and Geoffrey J. McLachlan.

The paper studies label acquisition when a queried oracle may abstain.
The acquisition criterion accounts for information supplied by both a
returned class label and the observed response or abstention outcome.

## Files

- `Simulation_response_term_ablation.R`:
  binary logistic simulations, response-outcome ablation,
  paired comparisons, numerical summaries, and simulation figures.
- `Real_data_corrected_curvature.R`:
  Consumer Complaints acquisition experiment, diagnostics,
  paired comparisons, and performance figures.
- `Figure_realdata_geometry_corrected.R`:
  real-data acquisition geometry from a completed Consumer Complaints run.

## Requirements

The real-data experiment was run using R 4.5.1 on Windows 11.
Package versions for completed runs are recorded in their output files.

Install the required packages:

```r
install.packages(c(
  "ggplot2", "patchwork", "scales",
  "readxl", "dplyr", "tidyr", "purrr", "stringr",
  "forcats", "quanteda", "Matrix", "irlba",
  "readr", "tibble"
))
```

Run the scripts with the repository root as the R working directory.

## Simulation studies

The simulation script compares:

- Random acquisition
- Entropy acquisition
- Ordinary classification-risk acquisition
- Response-discounted acquisition
- Response-aware acquisition
- Label-only acquisition using full information (ablation)

The ablation retains the full information matrix and joint estimation
used by response-aware acquisition, but omits the explicit
response-outcome term from the acquisition score.

The script includes oracle and adaptive acquisition studies under
noninformative, moderately informative, and strongly informative
response mechanisms.

The main settings are:

- 3,000 pool observations;
- a common random pilot of 100 queries;
- batches of 25 queries;
- total query budgets of 300, 600, and 900, including the pilot;
- 500 replications per scenario and study mode.

Oracle acquisition uses the true model parameters when computing
acquisition scores. Adaptive acquisition uses fitted parameters.
Both studies estimate the model from the acquired query-response data.

Run:

```r
source("Simulation_response_term_ablation.R")
```

Keep `DEBUG <- FALSE` for the reported main experiment.
Setting `DEBUG <- TRUE` produces a shorter diagnostic run.

The optional pilot-size sensitivity experiment is disabled by default
through `RUN_PILOT_SENSITIVITY <- FALSE`.

Simulation outputs are written to a new timestamped folder beginning with:

```text
simulation_response_term_ablation_FULL_
```

Outputs include simulation results, paired ablation comparisons,
numerical checks, and figures. The `FULL` prefix refers to the
main run with `DEBUG <- FALSE`.

For the ablation, differences are calculated as response-aware minus
label-only excess risk. Negative differences favour response-aware
acquisition. Holm correction is applied separately within the oracle
and adaptive studies, across the nine scenario–budget comparisons
in each study.

## Consumer Complaints data

Obtain the data from the original AL-RCTA repository:

https://github.com/varuntotakura/al_rcta

The real-data script expects either `al_rcta-main.zip` in this
repository's root or an extracted `al_rcta-main` directory.

The expected locations are:

```text
al_rcta-main/Data_ConsumerComplaints/Cleaned_Dataset_All.xlsx
al_rcta-main/Data_ConsumerComplaints/Annotations/
```

The annotations directory must contain the ten original worker
annotation files. External data remain subject to their providers' terms.

The analysis retains 2,999 usable complaints, six product categories,
and ten human oracles. It preserves each oracle's observed response
or abstention. When an oracle responds, the benchmark Product class
is revealed; the worker's potentially erroneous class annotation
is not used as the returned label.

This replay preserves human abstention patterns while using benchmark
labels upon response. It does not model errors in returned labels.

## Real-data experiment

Run:

```r
source("Real_data_corrected_curvature.R")
```

The experiment uses:

- 30 replications;
- ten human oracles;
- stratified splits with approximately 70% training and 30% test observations;
- a common pilot of 300 training observations within each replication;
- batches of 50 queries;
- additional query budgets of 150, 300, 450, and 600, excluding the pilot.

Competing methods share the split and pilot within each replication.
All methods use the same penalized joint observed-data likelihood.

The feature representation is transductive: unsupervised text
preprocessing uses all retained narratives before the splits are formed.
Test labels are withheld from model fitting and acquisition.

The risk-curvature calculation includes the fitted probability
of the tied classes. It uses a kernel approximation, regularization,
and stabilized matrix inversion.

The script writes results to a new timestamped folder beginning with:

```text
Consumer_Complaints_Corrected_Curvature_
```

Outputs include numerical checks, raw results, query traces, summaries,
Monte Carlo t intervals, and Holm-adjusted paired comparisons.

For aggregate results, errors are first averaged over the ten oracles
within each replication. Monte Carlo uncertainty is then calculated
across the 30 replications. The primary comparison family contains
16 response-aware versus comparator contrasts.

These intervals describe variation across repeated splits and
acquisitions for the fixed dataset and ten workers.

The real-data comparison between response-aware and
response-discounted acquisition evaluates complete acquisition rules
with different information matrices. It does not isolate the explicit
response-outcome term; that distinction is examined by the simulation
ablation.

## Real-data geometry figure

First complete the real-data experiment. The geometry script reads
these files from its output folder:

```text
02_all_raw_acquisition_results.csv
03_all_query_trace.csv
```

Before running the geometry script, open
`Figure_realdata_geometry_corrected.R` and set `RESULT_DIR`
to your completed output folder.

The supplied setting is:

```r
RESULT_DIR <- file.path(
  MAIN_DIR,
  "Consumer_Complaints_Corrected_Curvature_20261006_132351"
)
```

Replace the timestamped folder name with the name generated by
your completed run.

Then run:

```r
source("Figure_realdata_geometry_corrected.R")
```

The figure uses worker W8 at additional budget 600.
The representative replication is selected by proximity to the median
paired response-aware minus response-discounted test-error difference.

The figure is descriptive. Aggregate performance conclusions use
all replications.

## Computational notes

The full experiments require substantial computation.
Random seeds are specified in the scripts.

Simulation budgets include the 100 pilot queries.
Consumer Complaints budgets exclude the 300 pilot queries.

The real-data script creates a new output folder on each invocation.
Although checkpoint files are saved, sourcing it again does not
automatically resume the previous run.

The sequential, regularized real-data implementation illustrates the
criterion. It does not establish the assumptions or conclusion of the
two-stage oracle-attainment theorem.

## Citation

Setoudehtazangi, F., Nguyen, C. V., and McLachlan, G. J. (2026).
*Risk-optimal active learning with informative abstention*.
Manuscript.

## Contact

Please open a GitHub issue for questions about the code.

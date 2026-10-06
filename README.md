# Risk-optimal active learning with informative abstention

R code and reproducibility materials for the manuscript:

**Risk-optimal active learning with informative abstention**

Fariborz Setoudehtazangi.

The paper studies label acquisition when a queried oracle may abstain.
The acquisition criterion accounts for information supplied by both a
returned class label and the observed response or abstention outcome.

## Files

- `01_simulation_informative_abstention.R`:
  binary logistic simulation studies, numerical summaries, and simulation figures.
- `02_realdata_corrected_curvature.R`:
  corrected Consumer Complaints acquisition experiment, diagnostics,
  paired comparisons, and performance figures.
- `03_realdata_geometry.R`:
  real-data acquisition geometry from the corrected experiment.
- `LICENSE`:
  license for the code.

## Requirements

The corrected real-data experiment was run using R 4.5.1 on Windows 11.
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

## Simulation studies

The simulation script compares:

- Random acquisition
- Entropy acquisition
- Ordinary classification-risk acquisition
- Response-discounted acquisition
- Response-aware acquisition

It includes oracle and adaptive acquisition studies under
noninformative and informative response mechanisms.

The main settings are:

- 3,000 pool observations;
- a pilot of 100 queries;
- batches of 25 queries;
- additional query budgets of 300, 600, and 900;
- 500 replications per scenario and study mode.

Oracle acquisition uses the true model parameters when computing
acquisition scores. Adaptive acquisition uses fitted parameters.
Both studies estimate the model from the observations actually acquired.

Run R from the repository root:

```r
source("01_simulation_informative_abstention.R")
```

Keep `DEBUG <- FALSE` for the reported main experiment.
Setting `DEBUG <- TRUE` produces a shorter diagnostic run, not the
reported final results.

The optional pilot-size sensitivity experiment is disabled by default
through `RUN_PILOT_SENSITIVITY <- FALSE`.

Simulation outputs are written to:

```text
simulation_response_aware_revised/
```

Preserve completed outputs before running the simulation again.

## Consumer Complaints data

The real-data script expects the AL-RCTA data repository, supplied either
as `al_rcta-main.zip` in this repository's root or as an extracted
`al_rcta-main` directory.

The expected locations are:

```text
al_rcta-main/Data_ConsumerComplaints/Cleaned_Dataset_All.xlsx
al_rcta-main/Data_ConsumerComplaints/Annotations/
```

The annotations directory must contain the ten original worker
annotation files.

Obtain the data from the original AL-RCTA source cited in the manuscript.
External data remain subject to their providers' terms.

The analysis retains 2,999 usable complaints and ten human oracles.
It preserves each oracle's observed response or abstention.
When an oracle responds, the benchmark Product class is revealed;
the potentially noisy worker class is not used as the returned label.

Consequently, this is a replay of human response behaviour with
correct benchmark labels upon response. It is not an experiment
modelling erroneous returned labels.

## Corrected real-data experiment

Run:

```r
source("02_realdata_corrected_curvature.R")
```

The experiment uses:

- 30 replications;
- ten human oracles;
- a common pilot of 300 observations within each replication;
- batches of 50 queries;
- additional query budgets of 150, 300, 450, and 600.

Competing methods share the split and pilot within each replication.
All methods use the same penalized joint observed-data likelihood.

The feature representation is transductive: unsupervised text
preprocessing uses all retained narratives before the splits are formed.
Test labels are withheld from model fitting and acquisition.

The corrected risk-curvature calculation includes the fitted probability
of the tied classes. It uses a kernel approximation, regularization,
and stabilized matrix inversion.

The script writes results to a new timestamped folder beginning with:

```text
Consumer_Complaints_Corrected_Curvature_
```

It includes numerical checks, raw results, query traces, summaries,
Monte Carlo t intervals, and Holm-adjusted paired comparisons.

For aggregate results, errors are first averaged over the ten oracles
within each replication. Monte Carlo uncertainty is then calculated
across the 30 replications. The primary comparison family contains
16 response-aware versus comparator contrasts.

These intervals describe variation across the repeated experimental
splits and acquisitions for the fixed data and workers.

## Real-data geometry figure

The geometry script reads these files from a completed corrected run:

```text
02_all_raw_acquisition_results.csv
03_all_query_trace.csv
```

Before running it, set `RESULT_DIR` in
`03_realdata_geometry.R` to the completed output folder.

The supplied setting identifies the run used for the manuscript:

```r
RESULT_DIR <- file.path(
  MAIN_DIR,
  "Consumer_Complaints_Corrected_Curvature_20261006_132351"
)
```

A new reproduction produces a different timestamped folder name.
Use that new name when plotting its results.

Run:

```r
source("03_realdata_geometry.R")
```

The figure uses worker W8 at additional budget 600.
The representative replication is selected by proximity to the median
paired response-aware minus response-discounted test-error difference.

The figure is descriptive. Aggregate performance conclusions use
all replications, not the representative geometry run.

## Computational notes

The full experiments require substantial computation.
Random seeds are specified in the scripts.

The real-data script creates a new output folder on each invocation.
Although checkpoint files are saved, sourcing it again does not
automatically resume the previous run.

The sequential, regularized real-data implementation illustrates the
criterion; it is not a numerical verification of the two-stage
oracle-attainment theorem.

## Citation

Setoudehtazangi, F. (2026).
*Risk-optimal active learning with informative abstention*.
Manuscript.

Publication details will be updated following publication.

## License

The code is released under the MIT License.
External datasets are governed by their original providers' terms.

## Contact

Please open a GitHub issue for questions about the code.

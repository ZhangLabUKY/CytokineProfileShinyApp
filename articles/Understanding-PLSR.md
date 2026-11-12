# Understanding PLSR

## When to use PLSR

Partial Least Squares Regression (PLSR) is useful when your goal is
prediction of one numeric outcome from many cytokines at the same time.
In CytokineProfile Shiny, it is the right choice when you want to answer
questions such as:

- Which cytokines best predict a continuous response?
- Does the overall cytokine profile explain variation in a clinical
  score, concentration, or severity measure?
- Can I reduce many correlated predictors into a smaller number of
  components without losing the prediction goal?

PLSR is especially helpful when the predictors are numerous and
correlated, which is common in cytokine datasets.

## When not to use PLSR

PLSR is usually not the best first choice when:

- your outcome is categorical rather than numeric, in which case
  `Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)` or a
  classifier is a better fit
- your goal is unsupervised structure rather than prediction, in which
  case `Principal Component Analysis (PCA)` is a better first look
- you mainly want cytokine-by-cytokine significance testing rather than
  multivariable prediction

## Example context

A typical use case is predicting a numeric inflammatory or clinical
outcome from a panel of cytokines after Step 2 filtering has limited the
dataset to the cohort and variables of interest.

## What the app is showing

The PLSR workflow combines prediction-oriented and
interpretation-oriented outputs:

- `Scores Plot`
- `Predicted vs Observed`
- `Residuals vs Fitted`
- `Loadings Plots`
- `VIP Scores`
- optional `Cross-Validation`
- optional `VIP > 1: Scores`
- optional `VIP > 1: Cross-Validation`

These views should be read together. A model can produce visually
interesting components without actually predicting the outcome well.

## Which Step 4 arguments matter most

The highest-value controls are:

- **Numeric measurement to predict** selects the numeric outcome the
  model is trying to predict.
- **Measurements used to predict** selects the cytokines or other
  numeric predictors included in the model.
- `Number of Components`: how many latent components the model extracts.
- **Use fewer measurements per component** selects whether the model
  keeps all predictors or enforces variable selection.
- **Measurements to keep per component** selects how many predictors are
  retained per component when **Use fewer measurements per component**
  is turned on.
- **Check model performance (cross-validation)** and `Number of Folds`
  control how the app holds back samples to check predictions.
- **Color samples by (optional)** uses the same group colors across
  score, prediction, and residual plots. `Ellipse` adds group ellipses
  to score plots. The grouping column is excluded from the measurements
  the model can use.
- **Colors (optional)** assigns custom colors to the selected groups.
  When no grouping column is selected, score plots use a continuous
  scale for the observed response and the diagnostic plots use gray
  points.

In practice, **Numeric measurement to predict**, **Measurements used to
predict**, and `Number of Components` define the model, while **Use
fewer measurements per component** changes how simple or dense the
predictor set remains.

## How to read the main outputs

### Scores Plot

This plot shows the samples in the reduced component space. It is most
useful for asking whether samples with similar response behavior also
occupy similar positions in the latent space.

The **Measurement patterns (X)** panel positions samples using patterns
in the predictor measurements. The **Response patterns (Y)** panel shows
the response-side scores. The axes identify Component 1 and Component 2.
A one-component model shows samples along Component 1.

With a grouping column selected, the legend pairs each group with its
color swatch. Group colors match across the main scores, VIP scores,
predictions, and residuals. Samples with missing group labels appear in
gray and remain in the model. Without grouping, the **Observed \[outcome
name\]** legend shows a fixed viridis gradient from dark purple for low
values to yellow for high values. The main and VIP score plots use the
same low, midpoint, and high values, so a given color represents the
same observed value in both plots. The midpoint is halfway between the
displayed limits.

The heading identifies the response measurement. Beneath it, **Training
R²** measures the linear association between observed and fitted values.
It ranges from 0 to 1. Values near 0 indicate weak association, and
values near 1 indicate strong association. A high R² can coexist with
large prediction errors. **Training RMSE** is 0 or higher, with smaller
values indicating smaller errors on the outcome scale supplied to the
model. Use the original measurement units when the outcome was left
untransformed, and the transformed scale when preprocessing changed it.
Compare RMSE with the error you can tolerate for the measurement.

Interpretation checklist:

1.  Check whether samples with similar biology or known groups cluster
    loosely together.
2.  Look for extreme outliers that might dominate the model.
3.  Do not treat this plot alone as evidence of good prediction.

### Predicted vs Observed

This figure compares the observed response on the horizontal axis with
the model’s predictions on the vertical axis. The response column is
included in both axis labels. These are training predictions from
samples used to fit the model.

Both axes use the same limits and scale. The plot includes the full
range of observed values and finite predictions, including negative
predictions. Transparent points help show where samples overlap. In
ungrouped prediction and residual plots, each gray point represents one
sample.

- points close to the dashed diagonal have smaller training errors
- wide scatter indicates larger training errors
- systematic curvature or separation can suggest the model is missing
  structure

If this plot looks weak, then attractive scores or loadings plots should
be interpreted cautiously.

### Residuals vs Fitted

This plot helps you judge model misfit.

The horizontal axis shows predicted response values. The vertical axis
shows **Observed − predicted**, so a positive residual means the model
predicted too low and a negative residual means it predicted too high.
Points closer to zero have smaller errors.

- a roughly patternless cloud is more reassuring
- strong trends or funnels suggest the model error changes across the
  fitted range
- a few extreme residuals can indicate influential samples

### Loadings Plots

These plots show which cytokines contribute most strongly to each
component.

- larger absolute loadings indicate stronger contribution
- the sign is directional, not a p-value
- if the same predictors recur across components, they may be especially
  influential

### VIP Scores

VIP scores summarize predictor importance across the model. They are
useful for prioritization rather than formal hypothesis testing.

A common rule of thumb is that predictors above 1 are more influential,
which is why the app can also show `VIP > 1` views.

### Cross-Validation

The CV figure identifies the validation method and displays **CV R²**,
**Q² (total)**, and **scaled RMSEP** for the displayed component count.
Its two panels show Q² (total) and scaled RMSEP across component counts.

- **CV R²** measures the linear association between observed values and
  the cross-validated predictions produced by mixOmics. It ranges from 0
  to 1, from weak to strong linear association. Use it alongside
  prediction error.
- **Q² (total)** summarizes predictive performance through the displayed
  number of components relative to the reference error used by mixOmics.
  Its upper limit is 1. Values above 0 indicate improvement, values on
  the dashed zero line indicate no improvement, and values below 0
  indicate worse performance relative to that reference.
- **RMSEP (scaled)** is 0 or higher. Lower values indicate smaller
  cross-validated prediction errors on the standardized outcome scale.
  Compare results using the same outcome scaling and validation
  procedure. For repeated M-fold CV, the app takes the square root of
  the mean MSEP across repetitions.

Interpret the CV summaries alongside the error curves and the number of
components used. See the [mixOmics performance
implementation](https://raw.githubusercontent.com/mixOmicsTeam/mixOmics/master/R/perf.R)
for the metric definitions.

The **VIP \> 1** score and CV figures use metrics from their own
refitted model. CV for that preview assesses performance within the
feature panel already selected from the full dataset.

## Common cautions

Important limits to remember:

- good component separation does not automatically mean good prediction
- too many components can start modeling noise
- sparse models are easier to interpret, but too much sparsity can throw
  away useful signal
- correlated biomarkers can trade importance with one another, so
  variable rankings are not absolute
- PLSR is not a replacement for external validation

## How to reproduce the result in the app

1.  Filter the dataset to the samples and cytokines you want to model.
2.  Choose `Partial Least Squares Regression (PLSR)`.
3.  Set **Numeric measurement to predict** and confirm the
    **Measurements used to predict**.
4.  Choose `Number of Components`.
5.  Turn on **Use fewer measurements per component** only if you want a
    smaller selected predictor set.
6.  Add **Check model performance (cross-validation)** if you want a
    better sense of model stability.
7.  Read `Predicted vs Observed`, `Residuals vs Fitted`,
    `Loadings Plots`, and `VIP Scores` together.

## What to read next

Related articles:

- [Understanding
  PCA](https://shinyinfo.cytokineprofile.org/articles/Understanding-PCA.md)
  if your goal is unsupervised structure rather than prediction.
- [Understanding
  (s)PLS-DA](https://shinyinfo.cytokineprofile.org/articles/Understanding-sPLS-DA.md)
  if the outcome is categorical rather than numeric.
- [Understanding MINT
  sPLS-DA](https://shinyinfo.cytokineprofile.org/articles/Understanding-MINT-sPLS-DA.md)
  if the study spans multiple batches or cohorts.

------------------------------------------------------------------------

*Last updated:* November 12, 2026

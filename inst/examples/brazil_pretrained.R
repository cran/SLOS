library(SLOS)
set.seed(42)

# Your already pre-processed data (it must contain the same feature set the
# Brazilian model was trained on, plus the ICU identifier column).
# Here we use the sample data shipped with the package.
data(SampledData)

outcome <- "UnitLengthStay_trunc"

# Run SLOS with the existing Brazilian model
results_brazil <- SLOS(
  data       = sampled_data,
  icu_column = "UnitCode",
  outcome    = outcome
)

# Access the comparison plot
results_brazil$plot_SLOS_obs_prev

# Funnel plot is displayed automatically; recall it with:
plot(results_brazil$funnel_plot)

# EVALUATING EFFICIENCY

slos_values <- results_brazil$df_unit_slos$SLOS

general_slos <- sum(results_brazil$df_unit_slos$soma_los_obs) /
  sum(results_brazil$df_unit_slos$soma_los_esp)
general_slos

median_slos <- median(slos_values)
median_slos

q1_slos <- quantile(slos_values, 0.25)
q3_slos <- quantile(slos_values, 0.75)
q1_slos
q3_slos

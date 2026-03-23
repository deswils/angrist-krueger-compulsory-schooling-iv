# =============================================================================
# Replication: Angrist & Krueger (1991) — Returns to Education via IV
# =============================================================================
#
# Paper:  "Does Compulsory School Attendance Affect Schooling and Earnings?"
#         Angrist & Krueger (1991), Quarterly Journal of Economics
#
# Research question:
#   What is the causal return to an additional year of education on earnings?
#   OLS estimates are likely upward-biased due to ability bias (smarter people
#   get more schooling AND earn more). AK91 exploit quarter of birth (QOB) as
#   an instrumental variable — students born earlier in the year reach the
#   compulsory schooling cutoff sooner, so they can legally drop out with less
#   education. QOB affects earnings *only through* its effect on schooling.
#
# Instrument:  qob1 — indicator for being born in Q1 (January–March)
# Outcome:     lnw  — log weekly wage
# Endogenous:  s    — years of education
#
# Data:  ak91.csv — 1980 U.S. Census microdata, men born 1930–1939
#
# Sections:
#   1. Setup
#   2. Data loading & variable construction
#   3. Summary statistics: Wald estimates (replication of AK91 Table III, Panel B)
#   4. First-stage regression  (QOB -> schooling)
#   5. Reduced-form regression (QOB -> wages)
#   6. IV / 2SLS estimate      (schooling -> wages, instrumented by QOB)
#   7. OLS vs. IV comparison table
# =============================================================================


# =============================================================================
# 1. SETUP
# =============================================================================

mypacks <- c("tidyverse", "AER", "sandwich", "lmtest", "stargazer", "knitr")

packs      <- installed.packages()
install.me <- mypacks[!(mypacks %in% packs[, "Package"])]
if (length(install.me) >= 1) {
  install.packages(install.me, repos = "http://cran.us.r-project.org")
}
lapply(mypacks, library, character.only = TRUE)


# =============================================================================
# 2. DATA LOADING & VARIABLE CONSTRUCTION
# =============================================================================

ak91 <- read_csv("ak91.csv")

glimpse(ak91)
summary(ak91$s)  # years of education: distribution check

# Create Q1 birth indicator (instrument)
# Born in Q1 (Jan–Mar) = 1, all other quarters = 0
ak91 <- ak91 %>%
  mutate(qob1 = ifelse(qob == 1, 1, 0))


# =============================================================================
# 3. SUMMARY STATISTICS: WALD ESTIMATES (Replication of Table III, Panel B)
#
#    The Wald estimator is the ratio of the reduced-form effect (QOB on wages)
#    to the first-stage effect (QOB on schooling). It gives an IV estimate
#    under the assumption that QOB affects wages *only* through schooling.
# =============================================================================

# --- Wages by Q1 birth status ---
wage_stats <- ak91 %>%
  group_by(qob1) %>%
  summarize(mean_lnw = mean(lnw), var_lnw = var(lnw), n = n())

wage_mean_diff <- wage_stats$mean_lnw[2] - wage_stats$mean_lnw[1]
se_wage_diff   <- sqrt(wage_stats$var_lnw[2] / wage_stats$n[2] +
                       wage_stats$var_lnw[1] / wage_stats$n[1])

# --- Education by Q1 birth status ---
edu_stats <- ak91 %>%
  group_by(qob1) %>%
  summarize(mean_educ = mean(s), var_educ = var(s), n = n())

edu_mean_diff <- edu_stats$mean_educ[2] - edu_stats$mean_educ[1]
se_edu_diff   <- sqrt(edu_stats$var_educ[2] / edu_stats$n[2] +
                      edu_stats$var_educ[1] / edu_stats$n[1])

# --- Wald estimator ---
wald_est <- wage_mean_diff / edu_mean_diff

# Extract Wald SE programmatically from ivreg (= 2SLS SE with one instrument)
wald_iv    <- ivreg(lnw ~ s | qob1, data = ak91)
se_wald_est <- summary(wald_iv)$coefficients["s", "Std. Error"]

# --- OLS return to education (for comparison) ---
ols_model       <- lm(lnw ~ s, data = ak91)
ols_coef        <- coef(summary(ols_model))["s", "Estimate"]
ols_se          <- coef(summary(ols_model))["s", "Std. Error"]

# --- Assemble replication table ---
t_matrix <- matrix(
  c(
    round(wage_stats$mean_lnw[2], 4),  round(wage_stats$mean_lnw[1], 4),  round(wage_mean_diff, 4),
    "",                                  "",                                  paste0("(", round(se_wage_diff, 4), ")"),
    round(edu_stats$mean_educ[2], 4),   round(edu_stats$mean_educ[1], 4),   round(edu_mean_diff, 4),
    "",                                  "",                                  paste0("(", round(se_edu_diff, 4), ")"),
    "",                                  "",                                  round(wald_est, 4),
    "",                                  "",                                  paste0("(", round(se_wald_est, 4), ")"),
    "",                                  "",                                  round(ols_coef, 4),
    "",                                  "",                                  paste0("(", round(ols_se, 4), ")")
  ),
  ncol = 3, byrow = TRUE
)

colnames(t_matrix) <- c("Born in Q1", "Born in Q2–Q4", "Difference (Std. Error)")
rownames(t_matrix) <- c(
  "ln(weekly wage)", "",
  "Years of education", "",
  "Wald estimate of return to education", "",
  "OLS return to education", ""
)

cat("\n--- Table III, Panel B: Wald Estimates (1980 Census, Men Born 1930–1939) ---\n\n")
kable(t_matrix, align = c("r", "r", "r"),
      caption = "Replication of AK91 Table III, Panel B")


# =============================================================================
# 4. FIRST-STAGE REGRESSION
#
#    Does Q1 birth actually affect years of schooling?
#    We need a strong first stage (relevant instrument).
#    Expected: Q1 birth -> slightly fewer years of education.
# =============================================================================

first_stage <- lm(s ~ qob1, data = ak91)

cat("\n--- First Stage: Effect of Q1 Birth on Years of Education ---\n")
summary(first_stage)


# =============================================================================
# 5. REDUCED-FORM REGRESSION
#
#    Does Q1 birth affect log wages directly?
#    Under the exclusion restriction, any effect must run through schooling.
# =============================================================================

reduced_form <- lm(lnw ~ qob1, data = ak91)

cat("\n--- Reduced Form: Effect of Q1 Birth on Log Wages ---\n")
summary(reduced_form)


# =============================================================================
# 6. IV / 2SLS ESTIMATE
#
#    Two-stage least squares using qob1 as the instrument for education.
#    The IV estimate should be larger than OLS if ability bias inflates OLS,
#    or could differ depending on LATE vs. ATE considerations.
# =============================================================================

iv_model <- ivreg(lnw ~ s | qob1, data = ak91)

cat("\n--- IV / 2SLS: Return to Education (Instrumented by Q1 Birth) ---\n")
summary(iv_model, diagnostics = TRUE)  # diagnostics includes weak instrument test


# =============================================================================
# 7. OLS vs. IV COMPARISON TABLE
#
#    Side-by-side stargazer table to clearly communicate the bias comparison.
#    IV > OLS here would be consistent with downward ability bias or LATE effects.
# =============================================================================

cat("\n--- OLS vs. IV Comparison ---\n")
stargazer(
  ols_model, iv_model,
  type          = "text",
  title         = "Returns to Education: OLS vs. IV (AK91)",
  column.labels = c("OLS", "IV (2SLS)"),
  dep.var.labels = "Log Weekly Wage",
  covariate.labels = c("Years of Education"),
  keep           = "s",
  notes          = "Instrument: Born in Q1 (January–March). Data: 1980 Census, men born 1930–1939.",
  notes.align    = "l"
)

#' Create `vpcMod` object
#'
#' @param .data Data frame containing DVs and IVs.
#' @param dv Dependent variable (measured variable).
#' @param idyo_ivs Character vector of idiosyncratic variance variables.
#' @param shared_ivs Character vector of shared variance variables.
#' @param glmer Bool. Run a generalized linear mixed-effects regression rather than a linear mixed-effects regression.
#' @param additional_ivs Character vector of additional variance variables to include in the model.
#' @param save_fit Bool. Save fits to disk (can take up space when model is large).
#' @param fit_dir If `save_fit` is TRUE, where to save the fits.
#' @param fit_prefix Character. What to prefix to the fit file name if saved to disk.
#'
#' @return Object of class `vpcMod` containing variance components, formula call, and specified variables.
#' Optional side effect of saving fitted `lmerMod` or `glmerMod` to disk.
#' @export
compute_vpc <- function(.data, glmer = FLASE, dv, idyo_ivs, shared_ivs, additional_ivs,
                        save_fit = FALSE, fit_dir = NULL,
                        fit_prefix = NULL
                        ) {

  if (length(dv) != 1) {
    stop("dv must be single character vector.")
  }

  group_ivs <- c(idyo_ivs, shared_ivs, additional_ivs)

  # Construct variance IVs
  tmp_ls <- NULL
  for (i in seq_along(group_ivs)) {
    sv <- group_ivs[[i]]

    tmp_ls[i] <- glue::glue("(1|{sv})")
  }

  # Construct full formula for model
  var_vars <- glue::glue_collapse(tmp_ls, sep = " + ")
  fm <- glue::glue("{dv} ~ 1 + {var_vars}")

  if (glmer) {
    fit <- lme4::glmer(
      formula = formula(fm),
      family = "binomial",
      data = .data,
      control = glmerControl(optimizer = 'bobyqa')
    )

  } else {
    fit <- lme4::lmer(
      formula = formula(fm),
      data = .data,
      control = lmerControl(optimizer = 'bobyqa'),
      REML = TRUE
    )
  }


  # vcov is the VC (variance component), sdcor is square root of vcov, grp is the specific cluster/random effect
  vars <- data.frame(lme4::VarCorr(fit))

  # this calculates VPC (variance partitioning coefficient) which divides each vcov by the total variance = proportion of total variance
  vars$vpc <- vars$vcov/sum(vars$vcov)

  # model_converged <- fit$optinfo$message
  # model_message <- fit$optinfo$warnings
  # fit_sum <- summary(fit)

  if (save_fit) {

    if (is.null(fit_dir)) {
      stop("Please provide directory to save model fits.")
    }

    if (!dir.exists(fit_dir)) {
      dir.create(fit_dir)
      }

    if (is.null(fit_prefix)) {
      fit_prefix <- as.character(format(Sys.time(), "%m-%d-%Y-%H_%M_%S"))
    }

    saveRDS(fit, glue::glue("{fit_dir}/{fit_prefix}_fit.RDS"))
  }

  vpc_ret <- list(
    formula = fm,
    fit_saved = list(
      save_fit = save_fit,
      fit_dir = fit_dir,
      fit_name = glue::glue("{fit_dir}/{fit_prefix}_fit.RDS")
      ),
    # model_converged = model_converged,
    # model_message = model_message,
    shared_ivs = shared_ivs,
    idyo_ivs = idyo_ivs,
    additional_ivs = additional_ivs,
    variance_components = vars
  )

  return(
    structure(vpc_ret, class = "vpcMod")
  )
}


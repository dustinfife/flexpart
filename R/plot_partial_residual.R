#' Partial Residual Plot
#'
#' @param plot_formula a flexplot-style formula for plotting
#' @param lm_formula Optional. A lm-style formula for the model being fit
#' @param model Optional. A lm model
#' @param data The dataset
#' @param added_term a formula, which specifies which terms should be de-residualized. By default,
#' nothing is added back in (so the plot will just show the residuals of the model).
#' @param suppress_model A boolean. Should the visual fit of the model be suppressed? Defaults to T.
#' @param ... Other arguments passed to flexplot
#'
#' @return a partial residual plot
#' @export
#' @import flexplot dplyr ggplot2
#'
#' @examples
#' partial_residual_plot(weight.loss~therapy.type,
#'    lm_formula = weight.loss~therapy.type + motivation,
#'    data=exercise_data)
#' # prp with a model (showing regular residuals)
#' mod = lm(y~a + b + z + x, data=small)
#' partial_residual_plot(y~x, lm_formula = y~x + b, data=small)
#'
#' # prp, but we add back in the x effect
#' partial_residual_plot(y~x, model=mod, added_term=~x, data=small)
#'
#' # now show show a scatterplot of the partial residuals and overlay a loess line
#' partial_residual_plot(y~x, model=mod, added_term=~x, data=small, suppress_model=T, method="loess")
partial_residual_plot = function(plot_formula, lm_formula=NULL, model=NULL, data,
                                 added_term = NULL, suppress_model=F, ...) {


  # error messages and data checking
  if (is.null(lm_formula) & is.null(model)) stop("You must provide either a lm formula or a model")
  if (is.null(lm_formula)) lm_formula = formula(model)
  if (!is.null(added_term)) flexplot:::check_all_variables_exist_in_data(all.vars(added_term), data)
  flexplot:::check_all_variables_exist_in_data(all.vars(plot_formula), data)
  flexplot:::check_all_variables_exist_in_data(all.vars(lm_formula), data)
  flexplot:::check_variables_in_lm(plot_formula, lm_formula, check_both = TRUE)


  # remove missing data
  variables = all.vars(lm_formula)
  data = flexplot:::prep_data_for_avp(data, variables)

  if (is.null(model)) model = lm(lm_formula, data=data)

  # compute the partial residuals
  residual = partial_residual(model, added_term)

  # replace original dv with residual
  data[,all.vars(lm_formula)[1]] = residual

  ## create plot so we can get the "binned" variables (if they exist)
  plot_data = flexplot(plot_formula, data=data, suppress_smooth=T, ...)

  ## create dataset for prps
  k = create_prp_dataset(plot_data, plot_formula, model)
  k$predict = add_fit_to_prp(k, added_term, model, data)

  fitted_line = return_fitted_line(k, plot_data, model, suppress_model = suppress_model)


  # plot it
  y_label = paste0(paste0(lm_formula)[2], " ~ ", paste0(lm_formula)[3])

  ## suppress smooth if they leave it on defaults

  args = list(...)
  if (any(c("suppress_smooth", "method") %in% names(args))) return(flexplot(plot_formula, data=data,...) + fitted_line + labs(y=y_label))
  return(plot_data + fitted_line + labs(y=y_label))

}

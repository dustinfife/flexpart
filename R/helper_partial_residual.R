compute_mean_or_mode = function(x) {
  if (is.numeric(x)) return(mean(x, na.rm=T))
  return(names(sort(table(x)))[1])
}

# model = lm(health~weight.loss + motivation * therapy.type, data=exercise_data)
# all(return_matching_terms(~therapy.type*motivation, model)==c("motivation", "therapy.type", "motivation:therapy.type"))
# all(return_matching_terms(~therapy.type+motivation, model)==c("therapy.type","motivation"))
# all(return_matching_terms(~motivation*therapy.type, model)==c("motivation", "therapy.type", "motivation:therapy.type"))
return_matching_terms = function(added_terms, model) {
  # extract terms as a vector
  terms_added = attr(terms(added_terms), "term.labels")
  terms_model = attr(terms(model), "term.labels")
  interaction_components = grep(":", terms_added)
  # return "terms_added" if there are no interactions
  if (length(interaction_components)==0) return(terms_added)
  # return "terms_added" if all terms specified are in the model
  if (all(terms_added %in% terms_model)) return(terms_added)
  # loop through all interaction components and flip then check
  reordered_added_terms = terms_added %>% purrr::map_chr(reorder_interaction_terms)
  reordered_model_terms = terms_model %>% purrr::map_chr(reorder_interaction_terms)
  matching_terms = reordered_model_terms %in% reordered_added_terms
  return(terms_model[matching_terms])

}
# reorder_interaction_terms("a:c:b")=="a:b:c"
# reorder_interaction_terms("a")=="a"
reorder_interaction_terms = function(term) {
  # if it's not an interaction, return the term
  if (length(grep(":", term))==0) return (term)
  return(strsplit(term, ":", fixed=T) %>%
           unlist %>%
           sort %>%
           paste0(collapse=":"))
}
#model = lm(weight.loss~therapy.type * motivation + health, data=exercise_data)
#term = ~therapy.type*motivation
#term = c("motivation", "therapy.type")
partial_residual = function(model, term=NULL) {

  # extract model residuals
  res = residuals(model)
  if (is.null(term)) return(res)
  # get data
  data = flexplot:::extract_data_from_fitted_object(model)
  matrix_coded = terms_to_modelmatrix(term, data)
  # if the user specifies a*b but in the model it was b*a, there will be an error. Fix that
  # just identify which columns are identical
  keep_columns = keep_duplicates(model.matrix(model), matrix_coded)
  betas_of_interest = matrix(coef(model)[dimnames(keep_columns)[[2]]],
                             nrow=nrow(matrix_coded), ncol=ncol(keep_columns),
                             byrow=T)
  # return it
  if (ncol(matrix_coded)>1) return(res + rowSums(betas_of_interest*keep_columns) - coef(model)[1])
  return(res + betas_of_interest*matrix_coded- coef(model)[1])

}


terms_to_modelmatrix = function(term, data) {

  # convert terms to formula
  if (typeof(term)!= "language") term = formula(paste0("~", paste0(term, collapse="+")))

  # create model matrix
  matrix_coded = model.matrix(term, data=data)
  return(matrix_coded)
}

get_same_columns = function(original_model, new_model, return_unique = T) {
  columns_to_keep = which(duplicated(as.list(data.frame(original_model,new_model)), fromLast=TRUE))
  if (return_unique) return(original_model[,columns_to_keep])

  # return those not duplicated
  v = as.list(data.frame(original_model,new_model))
  return(keep_singles(v))
}

data_columns_as_list = function(...) {
  return(as.list(data.frame(...)))
}

# from https://stackoverflow.com/questions/37381174/r-removing-duplicate-elements-in-a-vector
keep_singles = function(...){
  v = data_columns_as_list(...)
  keep = which(!(v %in% v[duplicated(v)]))
  return(colnames(list(...)[[1]])[keep])
}
keep_duplicates = function(...){
  v = data_columns_as_list(...)
  keep = which(duplicated(v, fromLast=TRUE))
  return(list(...)[[1]][,keep])
}


return_term_location = function(model, term) {

  if (is.null(term)) stop("You must provide a term")
  variables = all.vars(formula(model))

  # identify which terms are not in the model
  notthere = which(!(term %in% variables))
  if (length(notthere)>0) stop(paste0("The term(s): ", paste0(term[notthere], collapse=", "), " are not in the model"))
  return(which(variables %in% term))
}

return_fitted_line = function(k, plot_data, model=NULL, suppress_model){

  # if there's no model (or they choose to suppress, return a blank geom)
  if (is.null(model) | suppress_model) return(geom_blank)

  # color line depending on if there's a group aesthetic
  if (is.null(plot_data$mapping$colour)) return(geom_line(data=k, aes(y=predict), colour="#8F0000", size=1.5))

  return(geom_line(data=k, aes(y=predict)))


}

create_prp_dataset = function(plot_data, plot_formula, model) {
  # identify variables with _binned in the name
  binned_vars = grep("_binned", names(plot_data$data), fixed=T, value=T)
  unbinned_name = gsub("_binned", "", binned_vars)

  # select all variables in the plot
  all_variables = all.vars(plot_formula)                # all variables in flexplot formula
  all_model_variables = all.vars(formula(model))        # all variables in model
  not_plotted_vars =
    all_model_variables[!all_model_variables
                        %in% all_variables]        # variables in model, but not plot

  # merge the means with the dataset and replace the original variable with the binned mean
  k = plot_data$data %>%
    group_by(across(all_of(binned_vars))) %>%
    summarize_at(unbinned_name, mean) %>%
    full_join(plot_data$data, by=binned_vars, suffix = c("", ".y")) %>%
    mutate(across(all_of(not_plotted_vars), compute_mean_or_mode)) %>%
    data.frame
  return(k)
}

add_fit_to_prp = function(k, added_term, model) {


  # identify which components go into the model
  # just use predict on the plot data
  if (!is.null(added_term)) {
    terms_2_predict = return_matching_terms(added_term, model)
    predict = rowSums(predict(model, newdata=k, terms = terms_2_predict, type = "terms"))
  } else {
    predict = 0
  }

  # fix the intercepts by making the means of prediction/residuals the same
  y = all.vars(formula(model))[1]
  predict = predict - (mean(predict) - mean(model$model[,y]))
  return(predict)
}

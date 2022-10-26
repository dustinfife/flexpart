require(flexplot)
partial_residual_plot(weight.loss~therapy.type,
   lm_formula = weight.loss~therapy.type + motivation,
   data=exercise_data)

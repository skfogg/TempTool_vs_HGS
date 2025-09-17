tsz_diff_channel <- function(model_results, bin_stats){
  tszs_only <- model_results[-1]

  temp_diffs <- lapply(tszs_only, function(x) model_results[[1]]$svValue - x$svValue)
  tsz_weights <- bin_stats$returning/sum(bin_stats$returning)

  weighted_temp_diffs <- list(0)
  for(i in 1:18){
    weighted_temp_diffs[[i]] <- temp_diffs[[i]]*(tsz_weights[i]*100)
  }

  return(data.frame(temp_diffs = temp_diffs,
                    weighted_temp_diffs = weighted_temp_diffs))
}

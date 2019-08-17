#' calculate Jackknife variance for a categorical variable
#'
#' It returns total of each category and corresponding variance if count is TRUE; otherwise returns proportion of each category and corresponding variance
#' @param df a dataframe, which contains a categorical variable, final_wgt and replicate weights--must be in this order
#' @param count a logical variable
#' @keywords Jackknife; variance; standard deviation
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}

#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
#' @importFrom dplyr summarise_all
#' @importFrom dplyr rename
#' @importFrom dplyr setdiff
#' @importFrom tidyr nest
#' @importFrom purrr map2_dbl

jk_variance_4_cat <- function(df, count = TRUE)
{# the columns of df must be in this order: 'the variable', 'final_wgt', 'rep_wgts'
 # if count == TRUE, then return estimated totals of each category and corresponding variances
 # if count == FALSE, then return estimated proportions of each category and corresponding variances
 old_names <- names(df)
 no_of_rep_wgt <- dim(df)[2] - 2
 names(df) <- c("a_vari", "wgt", sprintf("rep_wgt_%d", 1:no_of_rep_wgt))

 the_summary_df <-
   df %>%
   group_by(a_vari) %>%
   summarise_all(list(total = sum))

 co <- (no_of_rep_wgt - 1) / no_of_rep_wgt

 temp <-
   the_summary_df %>%
   nest(-a_vari, -wgt_total) %>%
   mutate(jk_variance = map2_dbl(wgt_total, data, function(x, y) co * sum((y - x) ^ 2))) %>%
   ungroup() %>%
   rename(!!rlang::sym(old_names[1]) := a_vari) %>%
   rename(est_count = wgt_total) %>%
   mutate(jk_sd = sqrt(jk_variance)) %>%
   select(!!rlang::sym(old_names[1]), est_count, jk_variance, jk_sd)

 if(count) return(temp)

 the_wgt_vars <- setdiff(names(the_summary_df), "a_vari")
 the_summary_df_a <-
   the_summary_df %>%
   ungroup() %>%
   mutate_at(.vars = the_wgt_vars, convert_2_prop)

 temp_a <-
   the_summary_df_a %>%
   nest(-a_vari, -wgt_total) %>%
   mutate(jk_variance = map2_dbl(wgt_total, data, function(x, y) co * sum((y - x) ^ 2))) %>%
   ungroup() %>%
   rename(!!rlang::sym(old_names[1]) := a_vari) %>%
   rename(est_proportion = wgt_total) %>%
   mutate(jk_sd = sqrt(jk_variance)) %>%
   select(!!rlang::sym(old_names[1]), est_proportion, jk_variance, jk_sd)

 return(temp_a)

}

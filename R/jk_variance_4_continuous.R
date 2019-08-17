#' calculate Jackknife variance for a continuous variable
#'
#' It returns estimated total and corresponding variance if total is TRUE; otherwise returns estimated mean and corresponding variance
#' @param df a dataframe, which contains a continuous variable, final_wgt and replicate weights--must be in this order
#' @param total a logical variable
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}

#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr select_at
#' @importFrom dplyr setdiff
#' @importFrom purrr map2_dbl
#' @importFrom purrr map_dbl
#' @importFrom dplyr mutate

jk_variance_4_continuous <- function(df, total = TRUE)
{# the columns of df must be in this order: 'the variable', 'final_wgt', 'rep_wgts'
 # if total == TRUE, then return estimated total and corresponding variance
 # if total == FALSE, then return estimated mean and corresponding variance
 old_names <- names(df)
 no_of_rep_wgt <- dim(df)[2] - 2
 names(df) <- c("a_vari", "wgt", sprintf("rep_wgt_%d", 1:no_of_rep_wgt))

 vari_df <-
    df %>%
    select(a_vari)

 the_wgt_vars <- setdiff(names(df), 'a_vari')
 wgt_df <-
    df %>%
    select_at(.vars = the_wgt_vars)

 the_N_vec <- map_dbl(wgt_df, sum)

 the_summary_vec <- map2_dbl(vari_df, wgt_df, function(x, y) sum(x * y))

 co <- (no_of_rep_wgt - 1) / no_of_rep_wgt

 var_of_total <- co * sum((the_summary_vec[2:(no_of_rep_wgt+1)] - the_summary_vec[1]) ^2)

 result_df <-
    data.frame(vari_name = old_names[1],
               est_total = the_summary_vec[1],
               jk_variance = var_of_total) %>%
    mutate(jk_sd = sqrt(jk_variance))

 if(total) return(result_df)

 the_summary_vec_a <- the_summary_vec / the_N_vec

 var_of_mean <- co * sum((the_summary_vec_a[2:(no_of_rep_wgt+1)] - the_summary_vec_a[1]) ^2)

 result_df_a <-
    data.frame(vari_name = old_names[1],
               est_mean = the_summary_vec_a[1],
               jk_variance = var_of_mean) %>%
    mutate(jk_sd = sqrt(jk_variance))

 return(result_df_a)

}

#' create DAGJK  replicate weights
#'
#' It returns DAGJK (Delete A Group Jackknife) replicate weights
#' @param df a dataframe, which contains unit_ID, psu_code, stratum_code and selection_wgt--must be in this ordr
#' @param  replicate_NO number of replicate weights
#' @param  random_nbr_seed random number seed
#' @keywords DAGJK; replicate weights
#' @export
#' @author Lingyun (Larry) Zhang \email{lyzhang10@gmail.com}

#' @importFrom magrittr %>%

DAGJK_replicate_wgts <- function(df, replicate_NO = 100, random_nbr_seed = 1234567)
{# the variables in df must be in this order:
 # 'unit_ID', 'psu_code', 'stratum_code' and 'selection_wgt'

 old_names <- names(df)
 names(df) <- c('unit_ID', 'psu_code', 'stratum_code', 'selection_wgt')

 # check if all strata have at least two psus
 strata <-
   df %>%
   dplyr::group_by(stratum_code) %>%
   dplyr::summarise(n = length(unique(psu_code)))

 psu_indi <- min(strata$n)

 if(psu_indi < 2) stop("Some strata only have one psu! So, firstly merge some similar strata!")

 # create replicate selection weights
 psu_df <-
   df  %>%
   dplyr::count(stratum_code, psu_code) %>%
   dplyr::select(-n)

 set.seed(random_nbr_seed)
 d <- length(psu_df$psu_code)
 rep_group_df <-
   psu_df %>%
   dplyr::mutate(random_nbr = runif(d)) %>%
   dplyr::arrange(stratum_code, random_nbr) %>%
   dplyr::mutate(psu_order_nbr = dplyr::row_number()) %>%
   dplyr::mutate(rep_grp_nbr = psu_order_nbr %% replicate_NO) %>%
   dplyr::mutate(rep_grp_nbr = ifelse(rep_grp_nbr == 0, replicate_NO, rep_grp_nbr)) %>%
   dplyr::ungroup() %>%
   dplyr::select(psu_code, rep_grp_nbr)

 joined_df <-
   df %>%
   dplyr::left_join(rep_group_df, by = "psu_code") %>%
   dplyr::arrange(unit_ID)

 data_with_rep_wgts <-
   df %>%
   dplyr::arrange(unit_ID)

 temp_1 <-
   joined_df %>%
   dplyr::group_by(stratum_code) %>%
   dplyr::summarise(total_psus = length(unique(psu_code)))

 for(i in 1:replicate_NO)
 {var_name <- rlang::sym(paste0("rep_wgt_", i))
  temp_2 <-
    joined_df %>%
    dplyr::filter(rep_grp_nbr == i) %>%
    dplyr::group_by(stratum_code) %>%
    dplyr::summarise(in_rep_grp =  length(unique(psu_code)))

 join_temps <-
   dplyr::left_join(temp_1, temp_2, by = "stratum_code") %>%
   tidyr::replace_na(list(in_rep_grp = 0))

 temp_rep_wgts <-
   joined_df %>%
   dplyr::left_join(join_temps, by = "stratum_code") %>%
   dplyr::mutate(zh = sqrt(replicate_NO / ((replicate_NO - 1) * total_psus * (total_psus - 1)))) %>%
   dplyr::mutate(!!var_name := dplyr::case_when(
     in_rep_grp == 0 ~ selection_wgt,
     in_rep_grp > 0 & total_psus >= replicate_NO & rep_grp_nbr == i ~ 0,
     in_rep_grp > 0 & total_psus >= replicate_NO & rep_grp_nbr != i ~ selection_wgt * total_psus / (total_psus - in_rep_grp),
     in_rep_grp > 0 & total_psus <  replicate_NO & rep_grp_nbr == i ~ selection_wgt * (1 - (total_psus - 1) * zh),
     TRUE ~ selection_wgt * (1 + zh)
   )) %>%
   dplyr::select(!!var_name)

 data_with_rep_wgts <- dplyr::bind_cols(data_with_rep_wgts, temp_rep_wgts)
 }

 result_df <-
   data_with_rep_wgts %>%
   dplyr::rename(!!rlang::sym(old_names[1]) := unit_ID,
                 !!rlang::sym(old_names[2]) := psu_code,
                 !!rlang::sym(old_names[3]) := stratum_code,
                 !!rlang::sym(old_names[4]) := selection_wgt)

 return(result_df)
}

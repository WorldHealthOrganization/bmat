
overwrite_w_frozen_vr_for_ss <- function(dat_merged,
                                         frozen_vr_data) {

# it goes here
# rename to continue using mat and envelope in end result
vrdat3 <-
  dat_merged %>%
  dplyr::rename(mat_fromwhovr = mat, env_fromwhovr = env)
res <- dplyr::full_join(vrdat3, frozen_vr_data,
                 by = c("iso" = "iso_alpha_3_code", "start" = "year_start"))

# get a final matvr and nonmatvr column
re2 <-
  res %>%
  dplyr::mutate(mat = ifelse(!is.na(pahocc_matvr), pahocc_matvr, mat_fromwhovr),
         env = ifelse(!is.na(pahocc_nonmatvr), pahocc_matvr + pahocc_nonmatvr, env_fromwhovr),
         updateinmat  = mat - mat_fromwhovr,
         updateinenv = env - env_fromwhovr,
         year = ifelse(!is.na(year), year, start + 0.5),
         end = ifelse(!is.na(end), end, start+1),
         include = ifelse(!is.na(include), include, TRUE))


# assume that isicd10 and propill are constant...
toimpute <-
  re2 %>%
  dplyr::group_by(iso) %>%
  dplyr::filter(!is.na(propill)) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::select(iso, propill, isicd10, propgrp2)
toimpute2 <- dplyr::left_join(re2, toimpute, by= c("iso" = "iso")) %>%
  dplyr::select(propill.y, isicd10.y, propgrp2.y)

# leave out propgr2 for now cause NA for some isos anyway
#toimpute
re3 <-
  re2 %>%
  dplyr::mutate(propill = ifelse(is.na(propill), toimpute2$propill.y, propill)) %>%
  dplyr::mutate(isicd10 = ifelse(is.na(isicd10), toimpute2$isicd10.y, isicd10))

# aug 27: impute missing propills to make sure we can get usabilut for PSE
# just set to 0... to include it
re3 <- re3 %>%
  dplyr::mutate(propill = ifelse(is.na(propill), 0, propill))

}
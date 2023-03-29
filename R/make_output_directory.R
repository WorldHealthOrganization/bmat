make_output_directory_return_path <- function(round_name, iso, global_run = TRUE, bmis_or_bmat){
  if (!dir.exists(here::here("output"))) dir.create(here::here("output"))
  if (!dir.exists(here::here("output", round_name))) dir.create(here::here("output", round_name))
  if(global_run) {
    if (!dir.exists(here::here("output", round_name, paste0(bmis_or_bmat, "_global")))) dir.create(here::here("output", round_name, paste0(bmis_or_bmat, "_global")))
    main_path <- file.path("output", round_name,  paste0(bmis_or_bmat, "_global"))
  }
  if(!global_run) {
    if (!dir.exists(here::here("output", round_name, paste0(bmis_or_bmat, "_onecountry")))) dir.create(here::here("output", round_name, paste0(bmis_or_bmat, "_onecountry")))
    if (!dir.exists(here::here("output", round_name, paste0(bmis_or_bmat, "_onecountry"), iso))) dir.create(here::here("output", round_name, paste0(bmis_or_bmat, "_onecountry"), iso))
    main_path <- file.path("output", round_name,  paste0(bmis_or_bmat, "_onecountry"), iso)
  } 

  return(main_path)
}
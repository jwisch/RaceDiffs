normalize<-function(PARAM){(PARAM - mean(PARAM))/sd(PARAM)}




BootstrappedP_race<-function(SAMPLES, PARAM, DF, FORMULA, NULLMODEL){
  
  #Now bootstrapping
  
  resamples <- SAMPLES
  
  boot_education <- DF %>% 
    modelr::bootstrap(resamples)
  (
    boot_lin_reg <- boot_education %>% 
      mutate(regressions = 
               map(strap, 
                   ~lm(FORMULA, 
                       data = .))) 
  )
  
  (
    tidied <- boot_lin_reg %>% 
      mutate(tidy_lm = 
               map(regressions, broom::tidy))
  )
  
  list_mods <- tidied %>% 
    pull(tidy_lm)
  mods_df <- map2_df(list_mods, 
                     seq(1, resamples), 
                     ~mutate(.x, resample = .y))
  (
    r.std.error <- mods_df %>% 
      group_by(term) %>% 
      summarise(r.std.error = sd(estimate))
  )
  # NULLMODEL %>% 
  #   broom::tidy() %>% 
  #   full_join(r.std.error) %>% 
  #   select(term, estimate, std.error, r.std.error)
  
  mods_df<-data.frame(mods_df)
  ANSWER<-mean(mods_df[mods_df$term == PARAM, "p.value"])
  return(ANSWER)}

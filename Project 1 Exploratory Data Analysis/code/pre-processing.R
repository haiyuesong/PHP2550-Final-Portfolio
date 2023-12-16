library(tidyverse)

# first process child data
setwd("/Users/alice/Dropbox/2550x/Project 1/")
child_df <- read.csv("K01BB.csv")

child_df <- child_df %>%
  select(c(participant_id:su_interview_complete))  %>%
  filter(redcap_event_name == "child_baseline_arm_1")

# select demographic variables
child_df <- child_df %>% 
  select(-c(participant_id, part, lastgrade, redcap_event_name, famid, 
            visit_date, time, redcap_survey_identifier, enroll_timestamp, 
            handednesst, tgender, sexorient, whichlang, nativelang, traceoth,
            usborn, relation, guardian, livewith___0:livewith___7, 
            attendance, demographics_complete, langpref, pacemaker, 
            longlive)) %>%
  rename(taian = trace___0, tasian = trace___1, tnhpi = trace___2, 
         tblack = trace___3, twhite = trace___4, trace_other = trace___5)

# drop brief because scoring difficult
child_df <- child_df %>%
  select(-c(brief_ysr_timestamp:brief_ysr_complete))

# cigarette usage summarize
child_df <- child_df %>%
  mutate(cig_ever = suc1, num_cigs_30 = suc11) %>%
  select(-c(suc1:honc10))

# e-cig usage summarize
child_df <- child_df %>%
  mutate(e_cig_ever = ecig1, num_e_cigs_30 = ecig4) %>%
  select(-c(ecig1:ehonc10))

# marijuana usage summarize
child_df <- child_df %>%
  mutate(mj_ever = mj1, num_mj_30 = mj8) %>%
  select(-c(mj1:mpi29))

# alchohol usage summarize
child_df <- child_df %>%
  mutate(alc_ever = alc2, num_alc_30 = alc7) %>%
  select(-c(alc1:alcsus3))

# other drugs and norms - dropping 
child_df <- child_df %>%
  select(-c(odrg1:othdrglist,
            perceived_norms_peers_timestamp:perceived_norms_peers_complete,
            substance_use_cigarettes_timesta:substance_use_other_drug_use_com))

# brief problem monitor scoring
child_df <- child_df %>%
  mutate(bpm_att = rowSums(dplyr::select(., c(bpm1,bpm3,bpm4,bpm5,bpm10))),
         bpm_ext = rowSums(dplyr::select(., c(bpm2,bpm6,bpm7,bpm8,bpm15,
                                              bpm16,bpm17))),
         bpm_int = rowSums(dplyr::select(., c(bpm9,bpm11,bpm12,bpm13,bpm18,
                                              bpm19)))) %>%
  select(-c(brief_problem_monitor_timestamp:brief_problem_monitor_complete))

# emotional regulation
child_df <- child_df %>%
  mutate(erq_cog = rowMeans(dplyr::select(., c(erq1,erq3,erq5,erq7,
                                                 erq8,erq10))),
         erq_exp = rowMeans(dplyr::select(., c(erq2,erq4,erq6,
                                                 erq9)))) %>%
  select(-c(emotion_regulation_questionnaire:emotion_regulation_questionnair1))

# physical - dropping for the purpose of this research
child_df <- child_df %>%
  select(-c(physical_development_scale_ysr_t:physical_development_scale_ysr_c,
            height1:body_measurements_complete))

# life stress - dropping for the purpose of this research
child_df <- child_df %>%
  select(-c(life_stress_ysr_timestamp:life_stress_ysr_complete))

# parental monitoring scoring
child_df <- child_df %>%
  mutate(pmq_parental_knowledge = (pmq1+pmq2+pmq3+pmq4+pmq5+pmq6+
                                     pmq7+pmq8+(5-pmq9))/9,
         pmq_child_disclosure = (pmqcd1+pmqcd2+(5-pmqcd3)+(5-pmqcd4)+pmqcd5)/5,
         pmq_parental_solicitation = rowMeans(dplyr::select(., pmqps1:pmqps5)),
         pmq_parental_control = rowMeans(dplyr::select(., pmqpc1:pmqpc5))) %>%
  select(-c(parental_monitoring_questionnair:parental_monitoring_questionnai1))

# dysregulation - drop to simplify analysis
child_df <- child_df %>%
  select(-c(dysregulation_inventory_ysr_time:dysregulation_inventory_ysr_comp))

# early adolescent temperament - drop to simplify analysis
child_df <- child_df %>%
  select(-c(early_adolescent_temperament_que:early_adolescent_temperament_qu1))

# alcohol and substance abuse - too few observed so remove
child_df <- child_df %>%
  select(-c(miniaud1:minikid_sud_2_complete))

# remove remaining diet questions for purposes of this research
child_df <- child_df %>%
  select(-c(intuitive_eating_scale_timestamp:su_interview_complete))

# parent df
parent_df <- read.csv("K01BB.csv") %>%
  filter(redcap_event_name == "parent_baseline_arm_2") %>%
  select(c(parent_id, page:chart23)) 

# demographics
parent_df <- parent_df %>%
  select(-c(pgender, marstat, handednessp, plang1:plang3,
            praceoth, ppacemaker, pusa, pedu1:pedu3,
            prelation:parent_demographics_complete, govtasst___0:govtasst___5,
            parent_demographics_asd_timestam, 
            parent_demographics_asd_complete)) %>%
  rename(paian = prace___0, pasian = prace___1, pnhpi = prace___2, 
         pblack = prace___3, pwhite = prace___4, prace_other = prace___5)

# brief - dropping for difficulty scoring
parent_df <- parent_df %>%
  select(-c(brief_p_on_c_timestamp:brief_p_on_c_complete))

# swan - p on c
############## New: check all NAs' parent IDs
parent_df$parent_id[which(
  rowSums(is.na(parent_df %>%
                  select(swan_p_on_c_timestamp:swan_p_on_c_complete)))==18)]
##############
parent_df <- parent_df %>%
  mutate(swan_inattentive = rowSums(dplyr::select(., swan1:swan9), 
                                    na.rm=TRUE),
         swan_hyperactive = rowSums(dplyr::select(., swan10:swan18), 
                                    na.rm=TRUE)) %>%
  select(-c(swan_p_on_c_timestamp:swan_p_on_c_complete))

# connors - drop because swan will be similar
parent_df <- parent_df %>%
  select(-c(connors_p_on_c_timestamp:connors_p_on_c_complete))

# pbpm - parent answering about child
parent_df <- parent_df %>%
  mutate(bpm_att_p = rowSums(dplyr::select(., c(pbpm1,pbpm3,pbpm4,pbpm5,pbpm10))),
         bpm_ext_p = rowSums(dplyr::select(., c(pbpm2,pbpm6,pbpm7,pbpm8,pbpm15,
                                              pbpm16,pbpm17))),
         bpm_int_p = rowSums(dplyr::select(., c(pbpm9,pbpm11,pbpm12,pbpm13,pbpm18,
                                              pbpm19)))) %>%
  select(-c(bpm_p_on_c_timestamp:bpm_p_on_c_complete))

# alc and drug use
parent_df <- parent_df %>%
  mutate(magic2 = ifelse(magic1 == 0, 0, magic2),
         magic5 = ifelse(magic4 == 0, 0, magic5),
         smoke_exposure_6mo = pmax(magic2, magic5),
         magic8 = ifelse(magic7 == 0, 0, magic8),
         magic11 = ifelse(magic10 == 0, 0, magic11),
         smoke_exposure_12mo = pmax(magic8, magic11),
         magic14 = ifelse(magic13 == 0, 0, magic14),
         magic17 = ifelse(magic16 == 0, 0, magic17),
         smoke_exposure_2yr = pmax(magic14, magic17),
         magic20 = ifelse(magic19 == 0, 0, magic20),
         magic23 = ifelse(magic22 == 0, 0, magic23),
         smoke_exposure_3yr = pmax(magic20, magic23),
         magic26 = ifelse(magic25 == 0, 0, magic26),
         magic29 = ifelse(magic28 == 0, 0, magic29),
         smoke_exposure_4yr = pmax(magic26, magic29),
         magic32 = ifelse(magic31 == 0, 0, magic32),
         magic35 = ifelse(magic34 == 0, 0, magic35),
         smoke_exposure_5yr = pmax(magic32, magic35)
          ) %>%
  select(-c(nidaliftetime___1:inject,penncig2:penn_state_ecigarette_dependenc1,
           penn_state_cigarette_dependence_, 
           nida_quick_screen_timestamp,
           nida_quick_screen_complete, magic_timestamp:magic_complete)) %>%
  rename(mom_numcig = penncig1)
    
         

# brief - dropping because difficulty scoring
parent_df <- parent_df %>%
  select(-c(briefa_timestamp:briefa_complete))

# parental monitoring - parent answering on child
parent_df <- parent_df %>%
  mutate(ppmq_parental_knowledge = (ppmq1+ppmq2+ppmq3+ppmq4+ppmq5+ppmq6+
                                     ppmq7+ppmq8+(5-ppmq9))/9,
         ppmq_child_disclosure = (ppmqcd1+ppmqcd2+(5-ppmqcd3)+(5-ppmqcd4)
                                  +ppmqcd5)/5,
         ppmq_parental_solicitation = rowMeans(dplyr::select(., ppmqps1:ppmqps5)),
         ppmq_parental_control = rowMeans(dplyr::select(., ppmqpc1:ppmqpc5))) %>%
  select(-c(ppmq1:ppmqps5,parental_monitoring_questionnai2,
            parental_monitoring_questionnai3))

# chaos - dropping for purposes of this research
parent_df <- parent_df %>%
  select(-c(chaos_timestamp:chaos_complete))

# bpm adult
parent_df <- parent_df %>%
  mutate(bpm_att_a = rowSums(dplyr::select(., c(abpm1,abpm6,abpm7,abpm8,abpm9,
                                                abpm12))),
         bpm_ext_a = rowSums(dplyr::select(., c(abpm3,abpm13,abpm14,abpm17,
                                                abpm18))),
         bpm_int_a = rowSums(dplyr::select(., c(abpm2,abpm4,abpm5,abpm10,abpm15,
                                              abpm16)))) %>%
  select(-c(brief_problem_monitoradult_times:brief_problem_monitoradult_compl))

# parent emotional regulation 
parent_df <- parent_df %>%
  mutate(erq_cog_a = rowMeans(dplyr::select(., c(perq1,perq3,perq5,perq7,
                                                 perq8,perq10))),
         erq_exp_a = rowMeans(dplyr::select(., c(perq2,perq4,perq6,
                                                 perq9)))) %>%
  select(-c(emotion_regulation_questionnair2:emotion_regulation_questionnair3))

# adult temperament - drop to simplify analysis
parent_df <- parent_df %>%
  select(-c(adult_temperament_questionnaire_:adult_temperament_questionnaire1))

# etq - drop to simplify analysis
parent_df <- parent_df %>%
  select(-c(eatq_p_on_c_timestamp:eatq_p_on_c_complete))

# stress - dropping for purposes of this research
parent_df <- parent_df %>%
  select(-c(nih_toolbox_stress_timestamp:teen_birthday_complete))

# reported smoking during pregnancy and postpartum
parent_df <- parent_df %>% 
  select(-c(BBID:ethn2, bl_6:bl_280, s2_10:s2_280, s3_6:s3_280, 
            s4_6:s4_280,  s5_6:s5_280, s6_6:s6_280, s7_6:s7_280, 
            chart21A:chart23) )   %>%
  rename(mom_smoke_16wk = bl_5,
         mom_smoke_22wk = s2_5, 
         mom_smoke_32wk = s3_5,
         mom_smoke_pp1 = s4_5,
         mom_smoke_pp2 = s5_5,
         mom_smoke_pp12wk = s6_5,
         mom_smoke_pp6mo = s7_5,
         cotimean_34wk = wk34cot_cotimean,
         cotimean_pp6mo = mo6momcot_cotimean,
         cotimean_pp6mo_baby = mo6babcot_cotimean)


new_df <- inner_join(parent_df, child_df, by = "parent_id")
write.csv(new_df, "project1.csv", row.names=FALSE)



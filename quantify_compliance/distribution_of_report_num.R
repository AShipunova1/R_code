source(r"(~\R_code_github\quantify_compliance\quantify_compliance_start.R)")
## ---- count reports ----
str(compl_clean_sa_vs_gom_plus_dual)
compl_clean_sa_vs_gom_plus_dual_rep_cnt <-
  compl_clean_sa_vs_gom_plus_dual %>%
  mutate(
    report_cnts = ifelse(
      permit == "sa_only",
      captainreports__ + negativereports__,
      captainreports__ + gom_permitteddeclarations__
    )
  )
str(compl_clean_sa_vs_gom_plus_dual_rep_cnt)

## --- check ----
id_reports <-
  compl_clean_sa_vs_gom_plus_dual_rep_cnt %>%
  filter(week_start == "2022-12-26",
         permit == "gom",
         tolower(compliant_) == "no") %>%
  # glimpse()
  # Rows: 47
  select(report_cnts, vessel_official_number)
# %>%
glimpse()

# vessel_id_order = reorder(vessel_official_number, 
            # as.integer(factor(report_cnts)), FUN = min)

# mutate(order = fct_reorder(as.factor(week_num), year)) %>% 
  
# id_reports %>%
#   mutate(vessel_id_order = reorder(vessel_official_number,
#                                    as.integer(factor(report_cnts)), FUN = min
#                                    ) 
         
ggplot(id_reports, aes(x = reorder(report_cnts,
                                   as.integer(factor(report_cnts)),
                                   FUN = min)
                       )
       ) +
  geom_histogram(binwidth = .5,
                 colour = "black",
                 fill = "white") +
  geom_density() +
  geom_vline(
    aes(xintercept = mean(report_cnts, na.rm = T)),
    # Ignore NA values for mean
    color = "red",
    linetype = "dashed",
    linewidth = 1
  )

  
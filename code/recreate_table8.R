#### libraries ----------------------------------------------------------- ####
library(pacman)
p_load(bartik.weight, tidyverse, haven, fixest, sandwich, modelsummary, 
       kableExtra, broom)

### read data ------------------------------------------------------------ ####
hgl_raw = haven::read_dta("data/finaldata.dta")

# drop Hawaii and Alaska
hgl_master = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15))

### some goodies for later ----------------------------------------------- ####
countries = c("uk", "ire", "it", "ger", "pol", "rus", "eur", "mex", "pr", "can", 
              "cam", "sam", "car", "cub", "chi", "ind", "asi", "wor")

regions = c("plains", "mideast", "glakes", "southeast", "southwest", 
            "mountain", "farwest") 

regions80 = hgl_raw %>% select(contains(regions) & ends_with("80")) %>% names()

oth_vars  = c("dncollshare", "dage", "dldod", "larea", "lpop1940",
              "lpcpersinc1940")

oth_vars_nocol  = c("dage", "dldod", "larea", "lpop1940",
                    "lpcpersinc1940")

dummies_year = hgl_raw %>%
  select(starts_with("ydum")) %>%
  select(-c("ydum6", "ydum9", "ydum1", "ydum2")) %>%
  names()

dummies_state = hgl_raw %>%
  select(starts_with("sdum")) %>%
  select(-c("sdum2", "sdum9", "sdum12", "sdum22", "sdum25", "sdum29")) %>%
  names()

dummies_elect = hgl_raw %>%
  select(starts_with("elect1980")) %>%
  select(-elect1980_40) %>%
  names()

sharen = hgl_raw %>%
  select(starts_with("sharen1940")) %>%
  names()

controls = c(oth_vars, dummies_year, regions)
controls80 = c(oth_vars, dummies_year, regions80)
controls_nocol = c(oth_vars_nocol, dummies_year, regions)
controls_nocol80 = c(oth_vars_nocol, dummies_year, regions80)
controls2 = c(oth_vars, dummies_year)
controls_post = c("dnpostshare", oth_vars_nocol, dummies_year, regions)
controls_post80 = c("dnpostshare", oth_vars_nocol, dummies_year, regions80)

### recreate instrument ----------------------------------------------- ####
hgl_wide = hgl_raw %>%
  filter(year %in% c(1950:2000),
         !state %in% c(2, 15)) %>% # HGL say data 1950-2000
  select(year, state, starts_with("dcoll"), starts_with("sharen1940"), dinstr1940pp, popl) %>%
  select(contains("noah"), starts_with("sharen1940"), state, year, dinstr1940pp, popl) %>%
  # select(!contains("noah") & contains(countries), starts_with("sharen1940"), state, year, popl, dinstr1940pp) %>%
  pivot_longer(cols = -c(year, state, popl, dinstr1940pp),
               names_to = "iso",
               values_to = "values") %>%
  mutate(country = case_when(
    str_detect(iso, "dcollnoah") == TRUE ~ str_remove(iso, "dcollnoah"),
    # str_detect(iso, "dcoll") == TRUE ~ str_remove(iso, "dcoll"),
    str_detect(iso, "sharen1940") == TRUE ~ str_remove(iso, "sharen1940"),
    TRUE ~ "NA")) %>%
  group_by(state, year, country) %>%
  mutate(instrument = prod(values)) %>%
  ungroup() %>%
  group_by(year, state) %>%
  mutate(dinstrument = sum(instrument) / popl) 

hgl_test = hgl_raw %>% 
  left_join(hgl_wide %>% 
              select(dinstrument, year, state) %>% 
              group_by(year, state) %>% 
              summarize(dinstrument = mean(dinstrument)), 
            by = c("year", "state"))

# Column 4 actually requires an extra instrument

hgl_wide_post = hgl_raw %>%
  filter(year %in% c(1950:2000),
         !state %in% c(2, 15)) %>% # HGL say data 1950-2000
  select(year, state, starts_with("dpost"), starts_with("sharen1940"), dinstr1940pp, popl) %>%
  select(contains("noah"), starts_with("sharen1940"), state, year, dinstr1940pp, popl) %>%
  # select(!contains("noah") & contains(countries), starts_with("sharen1940"), state, year, popl, dinstr1940pp) %>%
  pivot_longer(cols = -c(year, state, popl, dinstr1940pp),
               names_to = "iso",
               values_to = "values") %>%
  mutate(country = case_when(
    str_detect(iso, "dpostnoah") == TRUE ~ str_remove(iso, "dpostnoah"),
    # str_detect(iso, "dcoll") == TRUE ~ str_remove(iso, "dcoll"),
    str_detect(iso, "sharen1940") == TRUE ~ str_remove(iso, "sharen1940"),
    TRUE ~ "NA")) %>%
  group_by(state, year, country) %>%
  mutate(instrument = prod(values)) %>%
  ungroup() %>%
  group_by(year, state) %>%
  mutate(dinstrumentpost = sum(instrument) / popl) 

hgl_post = hgl_raw %>% 
  left_join(hgl_wide_post %>% 
              select(dinstrumentpost, year, state) %>% 
              group_by(year, state) %>% 
              summarize(dinstrumentpost = mean(dinstrumentpost)), 
            by = c("year", "state"))

### Table 8 ----------------------------------------------- ####
# Panel A
# Column 2 ORIGINAL
pac2_og = feols(dlmpatentspc ~ .[controls_nocol] + .[dummies_elect] | dfcollshare ~ dinstr1940pp, 
               data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
               weights = ~pop_w1,
               cluster = ~state)

# Column 2 MINE
pac2_me = feols(dlmpatentspc ~ .[controls_nocol] + .[dummies_elect] | dfcollshare ~ dinstrument, 
               data = hgl_test %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
               weights = ~pop_w1,
               cluster = ~state)

# Column 4 ORIGINAL
pac4_og = feols(dlmpatentspc ~ .[controls_nocol] + .[dummies_elect] | dfpostshare ~ dinstrpost1940pp, 
               data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
               weights = ~pop_w1,
               cluster = ~state)

# Column 4 MINE
pac4_me = feols(dlmpatentspc ~ .[controls_nocol] + .[dummies_elect] | dfpostshare ~ dinstrumentpost, 
               data = hgl_post %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
               weights = ~pop_w1,
               cluster = ~state)

# Panel B
# Column 2 ORIGINAL
pbc2_og = feols(dlmpatentspc ~ .[controls_nocol80] | dfcollshare ~ dinstr1940pp, 
               data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
               weights = ~pop_w1,
               cluster = ~state) 

# %>%
#   tidy()

# Column 2 MINE
pbc2_me = feols(dlmpatentspc ~ .[controls_nocol80] | dfcollshare ~ dinstrument, 
               data = hgl_test %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
               weights = ~pop_w1,
               cluster = ~state) 

# %>%
#   tidy()

# Column 4 ORIGINAL
pbc4_og = feols(dlmpatentspc ~ .[controls_nocol80] | dfpostshare ~ dinstrpost1940pp, 
                data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
                weights = ~pop_w1,
                cluster = ~state) 

# %>% 
#   tidy() %>%
#   mutate(term = ifelse(term == "fit_dfpostshare", "fit_dfcollshare", term))

# Column 4 MINE
pbc4_me = feols(dlmpatentspc ~ .[controls_nocol80] | dfpostshare ~ dinstrumentpost, 
               data = hgl_post %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
               weights = ~pop_w1,
               cluster = ~state) 

pa = list(pac2_og, pac2_me, pac4_og, pac4_me) 
pb = list(pbc2_og, pbc2_me, pbc4_og, pbc4_me)

tablify = function(q, panel_text){
  assign(deparse(substitute(q)), modelsummary(models = q, stars = T, 
                                              coef_map = c("fit_dfpostshare" = "fit_dfcollshare",
                                                           "fit_dfcollshare" = panel_text), 
                                              output = "data.frame",
                                              gof_map = NA)  %>% 
           mutate(term = ifelse(statistic == "modelsummary_tmp2", " ", term)) %>%
           select(matches("term|^model"))
  )
}


pa_table = tablify(pa, "\\textit{Panel A. Include BEA region dummies percent electrical workers}")
pb_table = tablify(pb, "\\textit{Panel B. Include BEA region dummies $\\times$ post-1980; }")

tables = bind_rows(pa_table, pb_table) %>%
  mutate(term = ifelse(row_number() %% 2, term, " "),
         term = case_when(
           row_number() == 2 ~ "\\vspace{1em} \\hspace{0.5em} \\textit{1980 $\\times$ year dummies; IV excludes share college natives}",
           row_number() == 4 ~ "\\vspace{1em} \\hspace{0.5em}  \\textit{IV excludes share college natives}",
           TRUE ~ term)
  )

tables %>% 
  kbl(col.names = c(" ", "(1)", "(2)", "(3)", "(4)"), booktabs = T, 
      align = c("l","c","c", "c", "c"), format = "latex", escape = F, linesep = "\\addlinespace") %>%  
  # kable_classic(full_width = T, html_font = "Cambria") %>%  # creating a latex-like output style
  add_header_above(c(" " = 1, "HGL" = 1, "Constructed" = 1, "HGL" = 1, "Constructed" = 1), line = F, align = "c") %>%
  add_header_above(c(" ", "College graduates" = 2, "Post-college" = 2)) %>%
  row_spec(c(2), hline_after = T) %>%
  save_kable(file = "tables/table8.tex", self_contained = FALSE) 

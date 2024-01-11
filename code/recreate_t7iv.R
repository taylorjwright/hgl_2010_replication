#### libraries ----------------------------------------------------------- ####
library(pacman)
p_load(bartik.weight, tidyverse, haven, fixest, sandwich, modelsummary, 
       kableExtra, broom, purrr, rlang)

### read data ------------------------------------------------------------ ####
hgl_raw = haven::read_dta("data/finaldata.dta")

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

### Recreate instrument ----------------------------------------------- ####
hgl_wide = hgl_raw %>%
  filter(year %in% c(1950:2000),
         !state %in% c(2, 15)) %>% # HGL say data 1950-2000
  select(year, state, starts_with("dcoll"), starts_with("sharen1940"), dinstr1940pp, popl) %>%
  select(contains("noah"), starts_with("sharen1940"), state, year, dinstr1940pp, popl) %>%
  pivot_longer(cols = -c(year, state, popl, dinstr1940pp),
               names_to = "iso",
               values_to = "values") %>%
  mutate(country = case_when(
    str_detect(iso, "dcollnoah") == TRUE ~ str_remove(iso, "dcollnoah"),
    str_detect(iso, "sharen1940") == TRUE ~ str_remove(iso, "sharen1940"),
    TRUE ~ "NA")) %>%
  group_by(state, year, country) %>%
  mutate(instrument = prod(values)) %>%
  ungroup() %>%
  group_by(year, state) %>%
  mutate(dinstrument = sum(instrument) / popl) 

hgl_inst = hgl_raw %>%
  left_join(hgl_wide %>% 
              select(dinstrument, year, state) %>% 
              group_by(year, state) %>% 
              summarize(dinstrument = mean(dinstrument)), 
            by = c("year", "state"))

### Table 7 ----------------------------------------------- ####

# Panel A Column OG
pac2_og = feols(dlmpatentspc ~ .[controls2] | dfcollshare ~ dinstr1940pp , 
               # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
               data = hgl_inst %>% filter(!state %in% c(2, 15)),
               weights = ~pop_w1,
               cluster = ~state)

# Panel A Column 2
pac2_me = feols(dlmpatentspc ~ .[controls2] | dfcollshare ~ dinstrument , 
               # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
               data = hgl_inst%>% filter(!state %in% c(2, 15)),
               weights = ~pop_w1,
               cluster = ~state)

# Panel B Column 2 OG
pbc2_og = feols(dlmpatentspc ~ .[controls2] | dfcollshare ~ dinstr1940pp , 
               # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 6, 15)), 
               data = hgl_inst %>% filter(!state %in% c(2, 6, 15)), 
               weights = ~pop_w1,
               cluster = ~state)

# Panel B Column 2 
pbc2_me = feols(dlmpatentspc ~ .[controls2] | dfcollshare ~ dinstrument , 
                # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 6, 15)), 
                data = hgl_inst %>% filter(!state %in% c(2, 6, 15)), 
                weights = ~pop_w1,
                cluster = ~state)

# Panel C Column 2 OG
pcc2_og = feols(dlmpatentspc ~ .[controls2] | dfcollshare ~ dinstr1940pp , 
               # data = hgl_raw %>% filter(year %in% c(1950:1990), !state %in% c(2, 15)), 
               data = hgl_inst %>% filter(year %in% c(1950:1990), !state %in% c(2, 15)), 
               weights = ~pop_w1,
               cluster = ~state)

# Panel C Column 2
pcc2_me = feols(dlmpatentspc ~ .[controls2] | dfcollshare ~ dinstrument , 
                # data = hgl_raw %>% filter(year %in% c(1950:1990), !state %in% c(2, 15)), 
                data = hgl_inst %>% filter(year %in% c(1950:1990), !state %in% c(2, 15)), 
                weights = ~pop_w1,
                cluster = ~state)


# Panel D Column 2 OG
pdc2_og = feols(dlmpatentspc ~ .[controls] | dfcollshare ~ dinstr1940pp , 
               # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
               data = hgl_inst %>% filter(!state %in% c(2, 15)),
               weights = ~pop_w1,
               cluster = ~state)

# Panel D Column 2
pdc2_me = feols(dlmpatentspc ~ .[controls] | dfcollshare ~ dinstrument , 
                # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
                data = hgl_inst %>% filter(!state %in% c(2, 15)),
                weights = ~pop_w1,
                cluster = ~state)


# Panel E Column 2 OG
pec2_og = feols(dlmpatentspc ~ .[controls2] + .[dummies_state] | dfcollshare ~ dinstr1940pp , 
               # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
               data = hgl_inst %>% filter(!state %in% c(2, 15)),
               weights = ~pop_w1,
               cluster = ~state)

# Panel E Column 2
pec2_me = feols(dlmpatentspc ~ .[controls2] + .[dummies_state] | dfcollshare ~ dinstrument , 
                # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
                data = hgl_inst %>% filter(!state %in% c(2, 15)),
                weights = ~pop_w1,
                cluster = ~state)


# Panel F Column 2 OG
pfc2_og = feols(dlmpatentspc ~ .[controls] + .[dummies_elect]| dfcollshare ~ dinstr1940pp , 
               # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
               data = hgl_inst %>% filter(!state %in% c(2, 15)),
               weights = ~pop_w1,
               cluster = ~state)

# Panel F Column 2
pfc2_me = feols(dlmpatentspc ~ .[controls] + .[dummies_elect]| dfcollshare ~ dinstrument , 
                # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
                data = hgl_inst %>% filter(!state %in% c(2, 15)),
                weights = ~pop_w1,
                cluster = ~state)

# Panel G Column 2
pgc2_og = feols(dlmpatentspc ~ .[controls] + .[dummies_elect] + .[sharen] | dfcollshare ~ dinstr1940pp , 
               # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
               data = hgl_inst %>% filter(!state %in% c(2, 15)),
               weights = ~pop_w1,
               cluster = ~state)

# Panel G Column 2 OG
pgc2_me = feols(dlmpatentspc ~ .[controls] + .[dummies_elect] + .[sharen] | dfcollshare ~ dinstrument , 
                # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
                data = hgl_inst %>% filter(!state %in% c(2, 15)),
                weights = ~pop_w1,
                cluster = ~state)


# Panel H Column 2
phc2_og = feols(dlmpatentspc ~ .[controls_nocol] + .[dummies_elect] | dfcollshare ~ dinstr1940pp , 
               # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)),
               data = hgl_inst %>% filter(!state %in% c(2, 15)),
               weights = ~pop_w1,
               cluster = ~state)

phc2_me = feols(dlmpatentspc ~ .[controls_nocol] + .[dummies_elect] | dfcollshare ~ dinstrument , 
                # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)),
                data = hgl_inst %>% filter(!state %in% c(2, 15)),
                weights = ~pop_w1,
                cluster = ~state)


# Panel I Column 2 OG
pic2_og = feols(dlmpatentspc ~ .[controls80] | dfcollshare ~ dinstr1940pp , 
               # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
               data = hgl_inst %>% filter(!state %in% c(2, 15)),
               weights = ~pop_w1,
               cluster = ~state)

# Panel I Column 2
pic2_me = feols(dlmpatentspc ~ .[controls80] | dfcollshare ~ dinstrument , 
                # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
                data = hgl_inst %>% filter(!state %in% c(2, 15)),
                weights = ~pop_w1,
                cluster = ~state)


# Panel J Column 2 OG
pjc2_og = feols(dlmpatentspc ~ .[controls_nocol80] | dfcollshare ~ dinstr1940pp , 
               # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
               data = hgl_inst %>% filter(!state %in% c(2, 15)),
               weights = ~pop_w1,
               cluster = ~state)

# Panel J Column 2
pjc2_me = feols(dlmpatentspc ~ .[controls_nocol80] | dfcollshare ~ dinstrument , 
                # data = hgl_raw %>% filter(year %in% c(1950:2000), !state %in% c(2, 15)), 
                data = hgl_inst %>% filter(!state %in% c(2, 15)),
                weights = ~pop_w1,
                cluster = ~state)

### Output ----------------------------------------------- ####

pa = list(pac2_og, pac2_me) 
pb = list(pbc2_og, pbc2_me) 
pc = list(pcc2_og, pcc2_me)
pd = list(pdc2_og, pdc2_me)
pe = list(pec2_og, pec2_me)
pf = list(pfc2_og, pfc2_me)
pg = list(pgc2_og, pgc2_me)
ph = list(phc2_og, phc2_me)
pi = list(pic2_og, pic2_me)
pj = list(pjc2_og, pjc2_me)

tablify = function(q, panel_text){
assign(deparse(substitute(q)), modelsummary(models = q, stars = T, 
             coef_map = c("fit_dfcollshare" = panel_text), 
             output = "data.frame",
             gof_map = NA)  %>% 
  mutate(term = ifelse(statistic == "modelsummary_tmp2", " ", term)) %>%
  select(matches("term|^model"))
)
}

# the tablify function
pa_table = tablify(pa, "\\textit{Panel A. Base Specification}")
pb_table = tablify(pb, "\\textit{Panel B. Base Specification without California}")
pc_table = tablify(pc, "\\textit{Panel C. Base specifications without year 2000}")
pd_table = tablify(pd, "\\textit{Panel D. Include BEA region dummies}")
pe_table = tablify(pe, "\\textit{Panel E. Include state dummies}")
# pf_table = tablify(pf, "\\shortstack{Include BEA region dummies and percent electrical \\\\ workers 1980 $\\times$ year dummies}")
# pg_table = tablify(pg, "\\shortstack{Include BEA region dummies and percent electrical \\\\ workers 1980 $\\times$ year dummies and 1940 immigrant shares ($\\lamda$)}")
# ph_table = tablify(ph, "\\shortstack{Include BEA region dummies and percent electrical \\\\ workers 1980 $\\times$ year dummies; exclude share college natives}")
pf_table = tablify(pf, "\\textit{Panel F. Include BEA region dummies and percent electrical}")
pg_table = tablify(pg, "\\textit{Panel G. Include BEA region dummies and percent electrical}")
ph_table = tablify(ph, "\\textit{Panel H. Include BEA region dummies and percent electrical}")
pi_table = tablify(pi, "\\textit{Panel I. Include BEA region dummies $\\times$ post-1980}")
pj_table = tablify(pj, "\\textit{Panel J. Include BEA region dummies $\\times$ post-1980; exclude share college natives}")

tables = bind_rows(pa_table, pb_table, pc_table, pd_table, pe_table, pf_table, pg_table, ph_table, pi_table, pj_table) %>%
  mutate(term = ifelse(row_number() %% 2, term, " "),
         term = case_when(
           row_number() == 12 ~ "\\vspace{1em} \\hspace{0.5em} \\textit{workers 1980 $\\times$ year dummies}",
           row_number() == 14 ~ "\\vspace{1em} \\hspace{0.5em}  \\textit{workers 1980 $\\times$ year dummies and 1940 immigrant shares ($\\lambda$)}",
           row_number() == 16 ~ "\\vspace{1em} \\hspace{0.5em}  \\textit{workers 1980 $\\times$ year dummies; exclude share college natives}",
           TRUE ~ term)
  )
  
tables %>% 
  kbl(col.names = c(" ", "(1)", "(2)"), booktabs = T, 
      align = c("l","c","c"), format = "latex", escape = F, linesep = "\\addlinespace") %>%  
  # kable_classic(full_width = T, html_font = "Cambria") %>%  # creating a latex-like output style
  add_header_above(c(" " = 1, "HGL" = 1, "Constructed" = 1), line = F, align = "c") %>%
  row_spec(c(2, 4, 6, 8, 10, 12, 14, 16, 18), hline_after = T) %>%
  save_kable(file = "tables/table7.tex", self_contained = FALSE) 

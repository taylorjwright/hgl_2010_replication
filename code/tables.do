use "~/Documents/replications/2010_hunt_immigration/data/finaldata.dta"

****************************************************
*                    Table 5                       *
****************************************************
* column 1
reg dlmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1950&year<=2000&state~=2&state~=15,cluster(state)

* column 2
reg dlmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 3
reg d3lmpatentspc d3fcollshare d3ncollshare d3age d3ldod larea lpop1940 lpcpersinc1940 ydum* if year>=1970&year<=2000&state~=2&state~=15 [weight=pop_w3],cluster(state)

* column 4
reg d5lmpatentspc d5fcollshare d5ncollshare d5age d5ldod larea lpop1940 lpcpersinc1940 ydum* if year>=1990&year<=2000&state~=2&state~=15 [weight=pop_w5],cluster(state)

****************************************************
*                    Table 6                       *
****************************************************
// Panel A (matchinstrreg, (matchinstrreg_lev)
* column 1
reg dlmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2
reg d5lmpatentspc d5fcollshare d5ncollshare d5age d5ldod larea lpop1940 lpcpersinc1940 ydum* if year>=1990&year<=2000&state~=2&state~=15 [weight=pop_w5],cluster(state)

* column 3
reg dmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 4
reg d5mpatentspc d5fcollshare d5ncollshare d5age d5ldod larea lpop1940 lpcpersinc1940 ydum* if year>=1990&year<=2000&state~=2&state~=15 [weight=pop_w5],cluster(state)

// Panel B (matchinstrreg, (matchinstrreg_lev)
* column 1
reg dlmpatentspc dfpostshare dnpostshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2
reg d5lmpatentspc d5fpostshare d5npostshare d5age d5ldod larea lpop1940 lpcpersinc1940 ydum* if year>=1990&year<=2000&state~=2&state~=15 [weight=pop_w5],cluster(state)

* column 3
reg dmpatentspc dfpostshare dnpostshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 4
reg d5mpatentspc d5fpostshare d5npostshare d5age d5ldod larea lpop1940 lpcpersinc1940 ydum* if year>=1990&year<=2000&state~=2&state~=15 [weight=pop_w5],cluster(state)


// Panel C (matchinstrreg, (matchinstrreg_lev)
* column 1
reg dlmpatentspc dfengscicshare dnengscicshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2
reg d5lmpatentspc d5fengscicshare d5nengscicshare d5age d5ldod larea lpop1940 lpcpersinc1940 ydum* if year>=1990&year<=2000&state~=2&state~=15 [weight=pop_w5],cluster(state)

* column 3
reg dmpatentspc dfengscicshare dnengscicshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 4
reg d5mpatentspc d5fengscicshare d5nengscicshare d5age d5ldod larea lpop1940 lpcpersinc1940 ydum* if year>=1990&year<=2000&state~=2&state~=15 [weight=pop_w5],cluster(state)

****************************************************
*                    Table 7                       *
****************************************************
// Panel A (matchinstrreg)
* column 1 (add area to give preferred spec)
reg dlmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2 (instrument with 1940 all base, no 1940 shares)
ivreg dlmpatentspc (dfcollshare=dinstr1940pp) dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state) first

// Panel B (matchinstrreg_noca)
* column 1 (add area to give preferred spec)
reg dlmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1950&year<=2000&state~=2&state~=15&state~=6 [weight=pop_w1],cluster(state)

* column 2 (instrument with 1940 all base, no 1940 shares)
ivreg dlmpatentspc (dfcollshare=dinstr1940pp) dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1950&year<=2000&state~=2&state~=15&state~=6 [weight=pop_w1],cluster(state) first

// Panel C (matchinstrreg_no2000)
* column 1 (add area to give preferred spec)
reg dlmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1950&year<=1990&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2 (instrument with 1940 all base, no 1940 shares)
ivreg dlmpatentspc (dfcollshare=dinstr1940pp) dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1950&year<=1990&state~=2&state~=15 [weight=pop_w1],cluster(state) first

// Panel D (matchinstrreg_bea)
* column 1 (add area to give preferred spec)
reg dlmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 mideast-farwest ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2 (instrument with 1940 all base, no 1940 shares)
ivreg dlmpatentspc (dfcollshare=dinstr1940pp) dncollshare dage dldod larea lpop1940 lpcpersinc1940 mideast-farwest ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state) first

// Panel E (matchinstrreg_state)
* column 1 (add area to give preferred spec)
reg dlmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 sdum* ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2 (instrument with 1940 all base, no 1940 shares)
ivreg dlmpatentspc (dfcollshare=dinstr1940pp) dncollshare dage dldod larea lpop1940 lpcpersinc1940 sdum* ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state) first

// Panel F (matchinstrreg_bea_ee)
* column 1 (add area to give preferred spec)
reg dlmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 mideast-farwest elect1980* ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2 (instrument with 1940 all base, no 1940 shares)
ivreg dlmpatentspc (dfcollshare=dinstr1940pp) dncollshare dage dldod larea lpop1940 lpcpersinc1940 mideast-farwest elect1980* ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state) first

// Panel G (matchinstrreg_bea_ee)
* column 1 (add 1940 shares - all edu)
reg dlmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 sharen1940* mideast-farwest elect1980* ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)
* column 2 (instrument with 1940 all base)
ivreg dlmpatentspc (dfcollshare=dinstr1940pp) dncollshare dage dldod larea lpop1940 lpcpersinc1940 sharen1940* mideast-farwest elect1980* ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state) first

// Panel H (matchinstrreg_bea_ee_nonat)
* column 1 (add area to give preferred spec)
reg dlmpatentspc dfcollshare dage dldod larea lpop1940 lpcpersinc1940 elect1980* mideast-farwest ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2 (instrument with 1940 all base, no 1940 shares)
ivreg dlmpatentspc (dfcollshare=dinstr1940pp) dage dldod larea lpop1940 lpcpersinc1940 elect1980* mideast-farwest ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state) first

// Panel I (matchinstrreg_bea80)
* column 1 (add area to give preferred spec)
reg dlmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 mideast80-farwest80 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2 (instrument with 1940 all base, no 1940 shares)
ivreg dlmpatentspc (dfcollshare=dinstr1940pp) dncollshare dage dldod larea lpop1940 lpcpersinc1940 mideast80-farwest80 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state) first

// Panel J (matchinstrreg_bea80_nonat)
* column 1 (add area to give preferred spec)
reg dlmpatentspc dfcollshare dage dldod larea lpop1940 lpcpersinc1940 mideast80-farwest80 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2 (instrument with 1940 all base, no 1940 shares)
ivreg dlmpatentspc (dfcollshare=dinstr1940pp) dage dldod larea lpop1940 lpcpersinc1940 mideast80-farwest80 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state) first

****************************************************
*                    Table 8                       *
****************************************************

// Panel A
* column 1 (matchinstrreg_bea_ee)
reg dlmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 mideast-farwest elect1980* ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2 (matchinstrreg_bea_ee_nonat)
ivreg dlmpatentspc (dfcollshare=dinstr1940pp) dage dldod larea lpop1940 lpcpersinc1940 elect1980* mideast-farwest ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state) first

* column 3 (matchinstrreg_bea_ee)
reg dlmpatentspc dfpostshare dnpostshare dage dldod larea lpop1940 lpcpersinc1940 mideast-farwest elect1980* ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 4 (matchinstrreg_bea_ee_nonat)
ivreg dlmpatentspc (dfpostshare=dinstrpost1940pp) dage dldod larea lpop1940 lpcpersinc1940 elect1980* mideast-farwest ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state) first

* column 5 (matchinstrreg_bea_ee)
reg dlmpatentspc dfengscicshare dnengscicshare dage dldod larea lpop1940 lpcpersinc1940 mideast-farwest elect1980* ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

// Panel B
* column 1 (matchinstrreg_bea80)
reg dlmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 mideast80-farwest80 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2 (matchinstrreg_bea80_nonat)
ivreg dlmpatentspc (dfcollshare=dinstr1940pp) dage dldod larea lpop1940 lpcpersinc1940 mideast80-farwest80 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state) first

* column 3 (matchinstrreg_bea80)
reg dlmpatentspc dfpostshare dnpostshare dage dldod larea lpop1940 lpcpersinc1940 mideast80-farwest80 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 4 (matchinstrreg_bea80_nonat)
ivreg dlmpatentspc (dfpostshare=dinstrpost1940pp) dage dldod larea lpop1940 lpcpersinc1940 mideast80-farwest80 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state) first

* column 5 (matchinstrreg_bea80)
reg dlmpatentspc dfengscicshare dnengscicshare dage dldod larea lpop1940 lpcpersinc1940 mideast80-farwest80 ydum* if year>=1950&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)


****************************************************
*                    Table 9                       *
****************************************************

// Panel A
* column 1
reg dlmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2
reg dlmpatentspc dfpostshare dnpostshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 3
reg dlmpatentspc dfengscicshare dnengscicshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

// Panel B (matchinstrreg_q)
* column 1
reg dlqmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2
reg dlqmpatentspc dfpostshare dnpostshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 3
reg dlqmpatentspc dfengscicshare dnengscicshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

// Panel C (matchinstrreg_q)
* column 1
reg dlqmpatentspc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 mideast-farwest ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2
reg dlqmpatentspc dfpostshare dnpostshare dage dldod larea lpop1940 lpcpersinc1940 mideast-farwest ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 3
reg dlqmpatentspc dfengscicshare dnengscicshare dage dldod larea lpop1940 lpcpersinc1940 mideast-farwest ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

// Panel D (matchinstrreg_comp)
* column 1
reg dlbpatcomppc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2
reg dlbpatcomppc dfpostshare dnpostshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 3
reg dlbpatcomppc dfengscicshare dnengscicshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

// Panel E (matchinstrreg_elec)
* column 1
reg dlbpatelecpc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2
reg dlbpatelecpc dfpostshare dnpostshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 3
reg dlbpatelecpc dfengscicshare dnengscicshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

// Panel F (matchinstrreg_drug)
* column 1
reg dlbpatdrugpc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2
reg dlbpatdrugpc dfpostshare dnpostshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 3
reg dlbpatdrugpc dfengscicshare dnengscicshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

// Panel G (matchinstrreg_chem)
* column 1
reg dlbpatchempc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2
reg dlbpatchempc dfpostshare dnpostshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 3
reg dlbpatchempc dfengscicshare dnengscicshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

// Panel H (matchinstrreg_mech)
* column 1
reg dlbpatmechpc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2
reg dlbpatmechpc dfpostshare dnpostshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 3
reg dlbpatmechpc dfengscicshare dnengscicshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

// Panel I (matchinstrreg_oth)
* column 1
reg dlbpatothpc dfcollshare dncollshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 2
reg dlbpatothpc dfpostshare dnpostshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

* column 3
reg dlbpatothpc dfengscicshare dnengscicshare dage dldod larea lpop1940 lpcpersinc1940 ydum* if year>=1980&year<=2000&state~=2&state~=15 [weight=pop_w1],cluster(state)

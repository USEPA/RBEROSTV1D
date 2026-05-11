set comid_all :=
{'166162418','7731471','7731473','7731483','7731485','7731505','7731507','7731513','7731515','7731531','7731533','7731535','7731547','7731583','7731625','7731633','7731645','7731647','7731649','7731651','7731653','7731689','7731691','7731697','7731709','7731711','7731731','7731747','7731749','7731769','7731771','7731779','7731783','7731789','7731797','7731805','7731829','7731835','7731837','7731841','7731867','7731871','7731879','7731901','7731907','7731909','7731925','7731927','7731953','7731989','7731991','7732057','7732073','7732079','7732095','7733183','7733187','7733191','7733193','7733199','7733201','7733203','7733205','7733207','7733209','7733361','7733363','7733365','7733373','7733397','7733405','7733407','7733409','7733427','7733429','7733455','7733457','7733459','7733471','7733477','7733481','7733489','7733557','7733563','7733571','7733587','7734093','7734361','7737483'};

set comid_all_N within comid_all := {'166162418','7731471','7731473','7731483','7731485','7731505','7731507','7731513','7731515','7731531','7731533','7731535','7731547','7731583','7731625','7731633','7731645','7731647','7731649','7731651','7731653','7731689','7731691','7731697','7731709','7731711','7731731','7731747','7731749','7731769','7731771','7731779','7731783','7731789','7731797','7731805','7731829','7731835','7731837','7731841','7731867','7731871','7731879','7731901','7731907','7731909','7731925','7731927','7731953','7731989','7731991','7732057','7732073','7732079','7732095','7733183','7733187','7733191','7733193','7733199','7733201','7733203','7733205','7733207','7733209','7733361','7733363','7733365','7733373','7733397','7733405','7733407','7733409','7733427','7733429','7733455','7733457','7733459','7733471','7733477','7733481','7733489','7733557','7733563','7733571','7733587','7734093','7734361','7737483'};

set comid_all_P within comid_all := {};

set comid_N1 within comid_all_N := {'166162418','7731471','7731473','7731483','7731485','7731505','7731507','7731513','7731515','7731531','7731533','7731535','7731547','7731583','7731625','7731633','7731645','7731647','7731649','7731651','7731653','7731689','7731691','7731697','7731709','7731711','7731731','7731747','7731749','7731769','7731771','7731779','7731783','7731789','7731797','7731805','7731829','7731835','7731837','7731841','7731867','7731871','7731879','7731901','7731907','7731909','7731925','7731927','7731953','7731989','7731991','7732057','7732073','7732079','7732095','7733183','7733187','7733191','7733193','7733199','7733201','7733203','7733205','7733207','7733209','7733361','7733363','7733365','7733373','7733397','7733405','7733407','7733409','7733427','7733429','7733455','7733457','7733459','7733471','7733477','7733481','7733489','7733557','7733563','7733571','7733587','7734093','7734361','7737483'};

set comid_P1 within comid_all_P := {};

set seasons := 1..4 ordered; 

set years := 1..1 ordered;


set urban_bmp :=
{'Biofiltration_w_Underdrain','Bioretention_Basin','Enhanced_Biofiltration_w_ISR','Extended_Dry_Detention_Basin','Grass_Swale_w_detention','Gravel_Wetland','Infiltration_Basin','Infiltration_Chamber','Infiltration_Trench','Porous_Pavement_w_subsurface_infiltration','Porous_Pavement_w_underdrain','Sand_Filter_w_underdrain','Wet_Pond'};

set urban_pervbmp within urban_bmp :=
{'Porous_Pavement_w_subsurface_infiltration','Porous_Pavement_w_underdrain'};

set ag_bmp :=
{'Conservation','Contour_Farming','Filterstrip','MIN_TILL','Ponds','Terrace_Waterway','Terrace_Only','Waterway_Only','Fert_20','Manure_Injection'};

set septic_bmp :=
{'none'};

set ripbuf_bmp :=
{'Grassed_Buffer','Forested_Buffer'};

set loads_N :=
{'point', 'urban', 'ag', 'septic', 'storage', 'other'};


set loads_P :=
{'point', 'urban', 'ag', 'storage', 'other'};
  
set area_sub :=
{'urban', 'ag'};
  
set urban_imp :=
{'urban'};
  
set urban_c :=
{'urban'};
  
set point_c :=
{'point'};
  
set cost_type :=
{'capital', 'operations'};

set coeff := 
 {'coeff'};

set loads_riparian := 
 {'riparian'};

set limits := 
 {'limits'};

set other_loads := 
 {'other_load'};

set septic_bmp_totals := 
 {'total_parcels'};

param baseloads_N1 {comid_N1, seasons, years, loads_N} >= 0;
param baseloads_P1 {comid_P1, seasons, years, loads_P} >= 0;
param riparianload_N1 {comid_N1, seasons, years, loads_riparian} >= 0;
param riparianload_P1 {comid_P1, seasons, years, loads_riparian} >= 0;

param area {comid_all,area_sub} >=0;

param urban_bmp_implementationpotential {comid_all, urban_bmp} >= 0;

param septic_bmp_implementationpotential {comid_all_N, septic_bmp} >= 0;
param septic_bmp_implementationtotal {comid_all_N, septic_bmp_totals} >= 0;

param unbuffered_banklength {comid_all, ripbuf_bmp} >= 0;
param total_banklength {comid_all} >= 0;
param ag_costs_capital {comid_all,ag_bmp};
param ag_costs_operations {comid_all,ag_bmp};
param point_costs {comid_all,cost_type};
param urban_costs {urban_bmp,cost_type};
param ripbuf_costs_capital {comid_all, ripbuf_bmp};
param ripbuf_costs_operations {comid_all, ripbuf_bmp};
param runoff_coeff_urban {comid_all,urban_imp} >=0;
param urban_cost_adjustment_coef {comid_all} >= 0;

param septic_costs_capital {comid_all_N, septic_bmp} >= 0;
param septic_costs_operations {comid_all_N, septic_bmp} >= 0;



param ag_effic_N {comid_all_N ,ag_bmp};
param point_effic_N {comid_all_N, point_c};
param urban_effic_N {comid_all_N, urban_bmp};
param septic_effic_N {comid_all_N, septic_bmp};


param ag_effic_P {comid_all_P ,ag_bmp};
param point_effic_P {comid_all_P, point_c};
param urban_effic_P {comid_all_P, urban_bmp};

param riparianremoval_N1 {c in comid_N1, s in seasons, y in years, r in ripbuf_bmp};
param riparianremoval_P1 {c in comid_P1, s in seasons, y in years, r in ripbuf_bmp};
param loads_lim_N1 {seasons, years, limits} >= 0;

param loads_lim_P1 {seasons, years, limits} >= 0;

param transfer_coefficients_N1 {comid_N1, seasons, years, coeff};

param transfer_coefficients_P1 {comid_P1, seasons, years, coeff};

param agcost_frac >=0;
param acfttoft3 >=0;
param pcp >=0;
param agBMP_minarea;

param urban_frac_min {urban_bmp} >=0, <= 1;
param urban_frac_max {u in urban_bmp} >= urban_frac_min[u], <= 1;

param ag_frac_min {ag_bmp} >= 0, <= 1;
param ag_frac_max {a in ag_bmp} >= ag_frac_min[a], <= 1;

param ripbuf_frac_min {ripbuf_bmp} >= 0, <= 1;
param ripbuf_frac_max {r in ripbuf_bmp} >= ripbuf_frac_min[r], <= 1;

param urban_design_depth {c in comid_all, u in urban_bmp} >= 0;
  
param ps_coef {c in comid_all} := point_costs[c,'capital'] + 
   point_costs[c,'operations'];

param urban_coef {c in comid_all, u in urban_bmp} := acfttoft3 * pcp *
   urban_design_depth[c,u] * runoff_coeff_urban[c,'urban'] * area[c,'urban'] *
   (urban_costs[u,'capital'] + urban_costs[u,'operations']) *
   urban_cost_adjustment_coef[c];

param ag_coef {c in comid_all, a in ag_bmp} := agcost_frac * area[c,'ag'] * 
(ag_costs_capital[c,a] + ag_costs_operations[c,a]) ;

param ripbuf_coef {c in comid_all, r in ripbuf_bmp} := 

agcost_frac * (ripbuf_costs_capital[c, r] + ripbuf_costs_operations[c, r]);
 

var agBMP_bin {comid_all, ag_bmp} binary;
var urbanBMP_bin {comid_all, urban_bmp} binary;
var point_dec {comid_all} binary;
var urban_frac {c in comid_all, u in urban_bmp} 
   >= urban_frac_min[u] * urban_bmp_implementationpotential[c, u]  
   <= urban_frac_max[u] * urban_bmp_implementationpotential[c, u] :=0;
var ag_frac {comid_all, a in ag_bmp} >= ag_frac_min[a] <= ag_frac_max[a] :=0;
var ripbuf_length {c in comid_all, r in ripbuf_bmp} 
   >= 0 
   <= unbuffered_banklength[c, r] * ripbuf_frac_max[r] := 0;
   

var inc_storage_load_N1{c in comid_N1, s in seasons, y in years} >= 0;
var inc_storage_load_P1{c in comid_P1, s in seasons, y in years} >= 0;

minimize cost: sum {c in comid_all} (ps_coef[c] * point_dec[c]) + 
sum {c in comid_all, u in urban_bmp} (urban_coef[c,u] * urban_frac[c,u]) + 
sum {c in comid_all, a in ag_bmp} (ag_coef[c,a] * ag_frac[c,a]) +
sum {c in comid_all, r in ripbuf_bmp} (ripbuf_coef[c, r] * ripbuf_length[c, r]);
  

subject to calc_inc_storage_load_N1{c in comid_N1, s in seasons, y in years}:

          inc_storage_load_N1[c,s,y] = if s=first(seasons) then (if y = first(years) then ((baseloads_N1[c,s,y,'storage'] + 

          baseloads_N1[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])) + 

          baseloads_N1[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_N[c,u] * urban_frac[c,u])) + 

          baseloads_N1[c,s,y,'point'] * (1 - (point_effic_N[c,'point'] * point_dec[c])) - 

          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N1[c,s,y,r]))) else ((inc_storage_load_N1[c,4,y-1] * transfer_coefficients_N1[c,4,y-1,'coeff']) + 

          baseloads_N1[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])) + 

          baseloads_N1[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_N[c,u] * urban_frac[c,u])) + 

          baseloads_N1[c,s,y,'point'] * (1 - (point_effic_N[c,'point'] * point_dec[c])) -  

          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N1[c,s,y,r]))) else ((inc_storage_load_N1[c,s-1,y] * transfer_coefficients_N1[c,s-1,y,'coeff']) + 

          baseloads_N1[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])) + 

          baseloads_N1[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_N[c,u] * urban_frac[c,u])) + 

          baseloads_N1[c,s,y,'point'] * (1 - (point_effic_N[c,'point'] * point_dec[c])) -  

          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N1[c,s,y,r]));

subject to calc_inc_storage_load_P1{c in comid_P1, s in seasons, y in years}:

      inc_storage_load_P1[c,s,y] = if s=first(seasons) then (if y = first(years) then (baseloads_P1[c,s,y,'storage'] + 

      baseloads_P1[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_P[c,a] * ag_frac[c,a])) + 

      baseloads_P1[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_P[c,u] * urban_frac[c,u])) + 
 
      baseloads_P1[c,s,y,'point'] * (1 - (point_effic_P[c,'point'] * point_dec[c])) - 

      sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_P1[c,s,y,r])) else ((inc_storage_load_P1[c,4,y-1] * transfer_coefficients_P1[c,4,y-1,'coeff']) + 

      baseloads_P1[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_P[c,a] * ag_frac[c,a])) + 

      baseloads_P1[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_P[c,u] * urban_frac[c,u])) + 

      baseloads_P1[c,s,y,'point'] * (1 - (point_effic_P[c,'point'] * point_dec[c])) - 

      sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_P1[c,s,y,r]))) else ((inc_storage_load_P1[c,s-1,y] * transfer_coefficients_P1[c,s-1,y,'coeff']) + 

      baseloads_P1[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_P[c,a] * ag_frac[c,a])) + 

      baseloads_P1[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_P[c,u] * urban_frac[c,u])) + 

      baseloads_P1[c,s,y,'point'] * (1 - (point_effic_P[c,'point'] * point_dec[c])) - 

      sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_P1[c,s,y,r]));

subject to total_loads_N1{s in seasons, y in years}:
sum{c in comid_N1} (inc_storage_load_N1[c,s,y]) <= loads_lim_N1[s,y,'limits'];

subject to riparian_loads_N1 {c in comid_N1, s in seasons, y in years}:
sum {r in ripbuf_bmp} (ripbuf_length[c, r] * riparianremoval_N1[c,s,y,r]) <= riparianload_N1[c,s,y,'riparian']; 

subject to riparian_loads_P1 {c in comid_P1, s in seasons, y in years}:
sum {r in ripbuf_bmp} (ripbuf_length[c, r] * riparianremoval_P1[c,s,y,r]) <= riparianload_P1[c,s,y,'riparian']; 

subject to ag_treat_min {c in comid_all, a in ag_bmp}:
ag_frac[c,a] * area[c,'ag'] >= agBMP_minarea * agBMP_bin[c,a];
  
subject to ag_frac_const {c in comid_all, a in ag_bmp}:
ag_frac[c,a] <= agBMP_bin[c,a];
  
subject to urban_frac_const {c in comid_all, u in urban_bmp}:
urban_frac[c,u] <= urbanBMP_bin[c,u];

subject to ag_frac_limit {c in comid_all}: 
sum {a in ag_bmp} ag_frac[c,a] <= 1;
# prevents multiple BMPs being implemented on the same area

subject to urban_frac_limit {c in comid_all}: 
sum {u in urban_bmp} urban_frac[c,u] <= 1;
# prevents multiple BMPs being implemented on the same area

subject to total_banks {c in comid_all}:
sum {r in ripbuf_bmp} ripbuf_length[c,r] <= max {r in ripbuf_bmp} 
   unbuffered_banklength[c,r];


subject to roads_and_parkinglots {c in comid_all}:
sum{up in urban_pervbmp} urban_frac[c, up] <= max {up in urban_pervbmp} 
   urban_bmp_implementationpotential[c,up];


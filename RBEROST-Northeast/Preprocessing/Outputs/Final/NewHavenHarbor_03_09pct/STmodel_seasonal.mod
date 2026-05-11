set comid_all :=
{'166182048','166182049','166182050','166445169','166445170','25075998','6174422','6174424','6174428','6174430','6174432','6174434','6174436','6174438','6174440','6174442','6174444','6174446','6174450','6174452','6174454','6174456','6174458','6174464','6174466','6174468','6174470','6174472','6174474','6174476','6174478','6174606','6174608','6174610','6174612','6174614','6174616','6174630','6174632','6174634','6174636','6174638','6174640','6174642','6174644','6174646','6174648','6174650','6174656','6174658','6174662','6174664','6176916','6176918','6176920','6176922','6176924','6176926','6176930','6176932','6176934','6176936','6176938','6176940','6176942','6176944','6176946','6176948','6176950','6176952','6176954','6176956','6176958','6176960','6176962','6176964','6176966','6176968','6176970','6176972','6176974','6176976','6176978','6176980','6176982','6176984','6176986','6176988','6176990','6176994','6176996','6176998','6177000','6177002','6177004','6177006','6177010','6177012','6177014','6177018','6177020','6177026','6177030','6177032','6177034','6177036','6177038','6177040','6177042','6177044','6177046','6177048','6177052','6177054','6177056','6177058','6177060','6177062','6177068','6177070','6177076','6177078','6177080','6177086','6177096','6177098','6177104','6177106','6177114','6177116','6177118','6177120','6177128','6177130','6177140','6177142','6177148','6177150','6177152','6177160','6177162','6177178','6177180','6177186','6177188','6177190','6177200','6177202','6177216','6177222','6177228','6177230','6177232','6177234','6177242','6177246','6177248','6177254','6177258','6177260','6177262','6177266','6177268','6177270','6177280','6177286','6177292','6177300','6177304','6177306','6177308','6177310','6177314','6177324','6177328','6177330','6177334','6177336','6177340','6177346','6177352','6177372','6177374','6177378','6177386','6177388','6177392','6177408','6177410','6177446','6177448','6177454','6177456','6177484','6177504','6177514','6177524','6177526','6177548','6177550','6177564','6177612','6177672','6177674','6177676','6177806','6177808','6177810','6177812','6177814','6177816','6177822','6177840','6177870','6177876','6177878','6177880','6177884','6177962','6177986','6177988','6177990','6177992','6177996','6177998','6178000','6178002','6178006','6178008','6178010','6178012','6178014','6178016','6178018','6178020','6178022','6178026','6178028','6178032','6178040','6178044','6178048','6178056','6178064','6178066','6178082','6178100','6178112','6178114','6178122','6178136','6178140','6178142','6178148','6178160','6178162','6178170','6178176','6178180','6178184','6178186','6178188','6178202','6178204','6178206','6178218','6178220','6178234','6178240','6178252','6178260','6178262','6178266','6178268','6178270','6178276','6178278','6178282','6178292','6178316','6178348','6178358','6178360','6178362','6178366','6178386','6178388','6178406','6178408','6178410','6178418','6178420','6178426','6178432','6178436','6178440','6178444','6178522','6178550','6178552','6178556','6178558','6178562','6178566','6178584','6178608','6178630','6178640','6178642','6181190'};

set comid_all_N within comid_all := {'166182048','166182049','166182050','166445169','166445170','25075998','6174422','6174424','6174428','6174430','6174432','6174434','6174436','6174438','6174440','6174442','6174444','6174446','6174450','6174452','6174454','6174456','6174458','6174464','6174466','6174468','6174470','6174472','6174474','6174476','6174478','6174606','6174608','6174610','6174612','6174614','6174616','6174630','6174632','6174634','6174636','6174638','6174640','6174642','6174644','6174646','6174648','6174650','6174656','6174658','6174662','6174664','6176916','6176918','6176920','6176922','6176924','6176926','6176930','6176932','6176934','6176936','6176938','6176940','6176942','6176944','6176946','6176948','6176950','6176952','6176954','6176956','6176958','6176960','6176962','6176964','6176966','6176968','6176970','6176972','6176974','6176976','6176978','6176980','6176982','6176984','6176986','6176988','6176990','6176994','6176996','6176998','6177000','6177002','6177004','6177006','6177010','6177012','6177014','6177018','6177020','6177026','6177030','6177032','6177034','6177036','6177038','6177040','6177042','6177044','6177046','6177048','6177052','6177054','6177056','6177058','6177060','6177062','6177068','6177070','6177076','6177078','6177080','6177086','6177096','6177098','6177104','6177106','6177114','6177116','6177118','6177120','6177128','6177130','6177140','6177142','6177148','6177150','6177152','6177160','6177162','6177178','6177180','6177186','6177188','6177190','6177200','6177202','6177216','6177222','6177228','6177230','6177232','6177234','6177242','6177246','6177248','6177254','6177258','6177260','6177262','6177266','6177268','6177270','6177280','6177286','6177292','6177300','6177304','6177306','6177308','6177310','6177314','6177324','6177328','6177330','6177334','6177336','6177340','6177346','6177352','6177372','6177374','6177378','6177386','6177388','6177392','6177408','6177410','6177446','6177448','6177454','6177456','6177484','6177504','6177514','6177524','6177526','6177548','6177550','6177564','6177612','6177672','6177674','6177676','6177806','6177808','6177810','6177812','6177814','6177816','6177822','6177840','6177870','6177876','6177878','6177880','6177884','6177962','6177986','6177988','6177990','6177992','6177996','6177998','6178000','6178002','6178006','6178008','6178010','6178012','6178014','6178016','6178018','6178020','6178022','6178026','6178028','6178032','6178040','6178044','6178048','6178056','6178064','6178066','6178082','6178100','6178112','6178114','6178122','6178136','6178140','6178142','6178148','6178160','6178162','6178170','6178176','6178180','6178184','6178186','6178188','6178202','6178204','6178206','6178218','6178220','6178234','6178240','6178252','6178260','6178262','6178266','6178268','6178270','6178276','6178278','6178282','6178292','6178316','6178348','6178358','6178360','6178362','6178366','6178386','6178388','6178406','6178408','6178410','6178418','6178420','6178426','6178432','6178436','6178440','6178444','6178522','6178550','6178552','6178556','6178558','6178562','6178566','6178584','6178608','6178630','6178640','6178642','6181190'};

set comid_all_P within comid_all := {};

set comid_N1 within comid_all_N := {'166182048','166182049','166182050','166445169','166445170','25075998','6174422','6174424','6174428','6174430','6174432','6174434','6174436','6174438','6174440','6174442','6174444','6174446','6174450','6174452','6174454','6174456','6174458','6174464','6174466','6174468','6174470','6174472','6174474','6174476','6174478','6174606','6174608','6174610','6174612','6174614','6174616','6174630','6174632','6174634','6174636','6174638','6174640','6174642','6174644','6174646','6174648','6174650','6174656','6174658','6174662','6174664','6176916','6176918','6176920','6176922','6176924','6176926','6176930','6176932','6176934','6176936','6176938','6176940','6176942','6176944','6176946','6176948','6176950','6176952','6176954','6176956','6176958','6176960','6176962','6176964','6176966','6176968','6176970','6176972','6176974','6176976','6176978','6176980','6176982','6176984','6176986','6176988','6176990','6176994','6176996','6176998','6177000','6177002','6177004','6177006','6177010','6177012','6177014','6177018','6177020','6177026','6177030','6177032','6177034','6177036','6177038','6177040','6177042','6177044','6177046','6177048','6177052','6177054','6177056','6177058','6177060','6177062','6177068','6177070','6177076','6177078','6177080','6177086','6177096','6177098','6177104','6177106','6177114','6177116','6177118','6177120','6177128','6177130','6177140','6177142','6177148','6177150','6177152','6177160','6177162','6177178','6177180','6177186','6177188','6177190','6177200','6177202','6177216','6177222','6177228','6177230','6177232','6177234','6177242','6177246','6177248','6177254','6177258','6177260','6177262','6177266','6177268','6177270','6177280','6177286','6177292','6177300','6177304','6177306','6177308','6177310','6177314','6177324','6177328','6177330','6177334','6177336','6177340','6177346','6177352','6177372','6177374','6177378','6177386','6177388','6177392','6177408','6177410','6177446','6177448','6177454','6177456','6177484','6177504','6177514','6177524','6177526','6177548','6177550','6177564','6177612','6177672','6177674','6177676','6177806','6177808','6177810','6177812','6177814','6177816','6177822','6177840','6177870','6177876','6177878','6177880','6177884','6177962','6177986','6177988','6177990','6177992','6177996','6177998','6178000','6178002','6178006','6178008','6178010','6178012','6178014','6178016','6178018','6178020','6178022','6178026','6178028','6178032','6178040','6178044','6178048','6178056','6178064','6178066','6178082','6178100','6178112','6178114','6178122','6178136','6178140','6178142','6178148','6178160','6178162','6178170','6178176','6178180','6178184','6178186','6178188','6178202','6178204','6178206','6178218','6178220','6178234','6178240','6178252','6178260','6178262','6178266','6178268','6178270','6178276','6178278','6178282','6178292','6178316','6178348','6178358','6178360','6178362','6178366','6178386','6178388','6178406','6178408','6178410','6178418','6178420','6178426','6178432','6178436','6178440','6178444','6178522','6178550','6178552','6178556','6178558','6178562','6178566','6178584','6178608','6178630','6178640','6178642','6181190'};

set comid_P1 within comid_all_P := {};

set seasons := 1..4 ordered; 

set years := 3..3 ordered;


set urban_bmp :=
{'Biofiltration_w_Underdrain','Bioretention_Basin','Enhanced_Biofiltration_w_ISR','Extended_Dry_Detention_Basin','Grass_Swale_w_detention','Gravel_Wetland','Infiltration_Basin','Infiltration_Chamber','Infiltration_Trench','Porous_Pavement_w_subsurface_infiltration','Porous_Pavement_w_underdrain','Sand_Filter_w_underdrain','Wet_Pond'};

set urban_pervbmp within urban_bmp :=
{'Porous_Pavement_w_subsurface_infiltration','Porous_Pavement_w_underdrain'};

set ag_bmp :=
{'Conservation','Contour_Farming','Filterstrip','MIN_TILL','Ponds','Terrace_Waterway','Terrace_Only','Waterway_Only','Fert_20','Manure_Injection','Cover_crops'};

set septic_bmp :=
{'class_1_upgrade', 'class_2_upgrade', 'class_3_upgrade', 'class_4_upgrade', 'class_5_upgrade', 'class_6_upgrade', 'class_7_upgrade', 'class_8_upgrade', 'Sewer_convert'};

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
   
param septic_coef {c in comid_all, e in septic_bmp} :=
   septic_costs_capital[c,e] + septic_costs_operations[c,e];

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
var septic_bin {comid_all_N, septic_bmp} binary;
var urban_frac {c in comid_all, u in urban_bmp} 
   >= urban_frac_min[u] * urban_bmp_implementationpotential[c, u]  
   <= urban_frac_max[u] * urban_bmp_implementationpotential[c, u] :=0;
var ag_frac {comid_all, a in ag_bmp} >= ag_frac_min[a] <= ag_frac_max[a] :=0;
var septic_frac {c in comid_all_N, e in septic_bmp} 
   >= 0 
   <= septic_bmp_implementationpotential[c,e] := 0;
var ripbuf_length {c in comid_all, r in ripbuf_bmp} 
   >= 0 
   <= unbuffered_banklength[c, r] * ripbuf_frac_max[r] := 0;
   

var inc_storage_load_N1{c in comid_N1, s in seasons, y in years} >= 0;
var inc_storage_load_P1{c in comid_P1, s in seasons, y in years} >= 0;

minimize cost: sum {c in comid_all} (ps_coef[c] * point_dec[c]) + 
sum {c in comid_all, u in urban_bmp} (urban_coef[c,u] * urban_frac[c,u]) + 
sum {c in comid_all, a in ag_bmp} (ag_coef[c,a] * ag_frac[c,a]) +
sum {c in comid_all, e in septic_bmp} (septic_coef[c,e] * septic_frac[c,e]) +
sum {c in comid_all, r in ripbuf_bmp} (ripbuf_coef[c, r] * ripbuf_length[c, r]);
  

subject to calc_inc_storage_load_N1{c in comid_N1, s in seasons, y in years}:

          inc_storage_load_N1[c,s,y] = if s=first(seasons) then (if y = first(years) then ((baseloads_N1[c,s,y,'storage'] + 

          baseloads_N1[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])) + 

          baseloads_N1[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_N[c,u] * urban_frac[c,u])) + 

          (baseloads_N1[c,s,y,'point'] + ((baseloads_N1[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_N[c,'Sewer_convert'])) * (1 - (point_effic_N[c,'point'] * point_dec[c])) + 
          (baseloads_N1[c,s,y,'septic'] - sum {e in septic_bmp} (baseloads_N1[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N[c,e]) - 

          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N1[c,s,y,r]))) else ((inc_storage_load_N1[c,4,y-1] * transfer_coefficients_N1[c,4,y-1,'coeff']) + 

          baseloads_N1[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])) + 

          baseloads_N1[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_N[c,u] * urban_frac[c,u])) + 

          (baseloads_N1[c,s,y,'point'] + ((baseloads_N1[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_N[c,'Sewer_convert'])) * (1 - (point_effic_N[c,'point'] * point_dec[c])) + 
          (baseloads_N1[c,s,y,'septic'] - sum {e in septic_bmp} (baseloads_N1[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N[c,e]) -  

          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N1[c,s,y,r]))) else ((inc_storage_load_N1[c,s-1,y] * transfer_coefficients_N1[c,s-1,y,'coeff']) + 

          baseloads_N1[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])) + 

          baseloads_N1[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_N[c,u] * urban_frac[c,u])) + 

          (baseloads_N1[c,s,y,'point'] + ((baseloads_N1[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_N[c,'Sewer_convert'])) * (1 - (point_effic_N[c,'point'] * point_dec[c])) + 
          (baseloads_N1[c,s,y,'septic'] - sum {e in septic_bmp} (baseloads_N1[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N[c,e]) -  

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

subject to septic_reduction_limit_N1 {c in comid_N1, s in seasons, y in years}:
sum {e in septic_bmp} (baseloads_N1[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N[c,e] <= baseloads_N1[c,s,y,'septic'];


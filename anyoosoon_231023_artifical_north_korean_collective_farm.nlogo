globals
[
  whole-yield-estimation
  whole-yield
  whole-population
  whole-food-per-captia
  whole-food-per-captia-estimation
  whole-food-pressure
  whole-food-pressure-estimation
  mean-soil-quality
  simulation-percent
  filenameset
  luchange-count
  luchange-count-on-tick
  growth-rate-of-population
  gov-yield
  gov-food
  gov-food-a-man
  food-aid

]

patches-own
[
  p_cover-type
  p_elevation
  p_slope
  p_rainfall
  p_temp
  p_as
  p_cur
  p_ndvi
  p_yield
  p_yield-potential
  p_yield-estimation
  p_sigun
  p_kfactor
  p_rfactor
  p_cfactor
  p_lsfactor
  p_pfactor
  p_usle
  p_usle_es
  p_ndvi2010
  p_soil-quality
  p_soil-potential
  p_soil-physical-point
  p_labor
  p_redundancy
  p_stratgy_changeable
  P_abandon-year
  p_agri-usable
  p_luchange_log
  p_biomass
  p_turtleown
  p_group
  p_dummy
  p_farmer_id
  p_time
  p_sq
]

turtles-own
[
  h_people
  h_labor_pop
  h_non_labor_pop
  h_group
  h_labor
  h_whole-food
  h_food-per-captia
  h_regional-food-pressure
  h_food-pressure
  h_whole-food-estimation
  h_food-per-captia-estimation
  h_regional-food-pressure-estimation
  h_food-pressure-estimation
  h_xcor
  h_ycor
  h_time
  h_redundancy

  h_yield
  h_yield_gov
  h_yield_mine
  h_food_gov
  h_food
  h_food-houshold
  h_hunger-time
  h_hunger
]


to setup
  ca
  reset-ticks
  crt household-num [set color black set size 2]
  setup-environment
  setup-human-agent
end

to setup-environment
  ask patches[set p_dummy 1]
  ask patches with[pycor < 60] [set p_cover-type 210 set p_elevation 0 set p_slope 0 set pcolor cyan];paddy rice field
  ask patches with[pycor >= 60 and pycor < 200] [set p_cover-type 220 set pcolor yellow set p_slope (pycor - 59) *(10 / 140) + (random-float 0.05)];other farm field
  ask patches with[pycor >= 200 and pycor < 202] [set p_cover-type 100 set pcolor red ];urban
  ask patches with[pycor >= 202][set p_cover-type 300 set pcolor 63]; forest


  ask patches with[pycor >= 200]
  [set p_slope ((pycor - 199) * (20 / 50)) + (10 * (1 + random-float 0.05))]
  ask patches
  [
   set p_elevation 10 * tan (p_slope)
    set p_elevation p_elevation * 10
  ]

  ;setting geomorphic parameter(dummy)
  ask patches[set p_as count neighbors with[p_elevation > [p_elevation] of myself] * 0.1]
  foreach sort-on [(- p_elevation)] patches
  [the-patch -> ask the-patch
    [ifelse count neighbors with [p_elevation > [p_elevation] of myself] > 0[set p_as [p_as] of max-one-of neighbors4[p_elevation] + 0.1][set p_as 0.1]]]
  ;setting lsfactor
  ask patches
  [let lfactor 1.4 * ((p_as / 22.13) ^ 0.4)
   let sfactor (((sin p_slope) / 0.0896) ^ 1.3)
   set p_lsfactor lfactor * sfactor
  ]


 ;setting group living land
  let group-set 1
  while[group-set < group-num + 1]
  [ask patches with[p_group = 0 and pxcor <= ((max-pxcor / group-num) * group-set) and pxcor > ((max-pxcor / group-num) * (group-set - 1)) ][set p_group group-set set pcolor (pcolor + 4) - ( group-set / 2)]
   set group-set group-set + 1]
   ; set pcolor (pcolor + 4) - group-set
   ; if count patches with[p_group = [p_group] of myself] >= (count patches / group-num) [set group-set group-set + 1]]

  ;setting cur
  ifelse SurfaceCurvature = true
  [ask patches
  [let curcount count patches with[pxcor >= ([pxcor] of myself - 3) and pxcor <= ([pxcor] of myself + 3) and pycor >= ([pycor] of myself - 3) and pycor <= ([pycor] of myself + 3)]
    set p_cur (sum [p_elevation] of patches with[pxcor >= ([pxcor] of myself - 3) and pxcor <= ([pxcor] of myself + 3) and pycor >= ([pycor] of myself - 3) and pycor <= ([pycor] of myself + 3)] - p_elevation ) / (curcount * 10)
  ]]
  [ask patches[set p_cur random-normal 1.6438894181869679 1.8382822415347304]]



  ; setting initial environmental state

  ask patches
  [ set p_rainfall random-normal 1046 83
    set p_temp random-normal 9.19 1.30
    set p_kfactor random-normal 0.25 0.02
    if p_lsfactor < 0 [set p_lsfactor 0]
      ]
end

to setup-human-agent

  ; setting agricultural group
  let group-set 1
  ask turtles with[h_group = 0]
  [set h_group group-set
   if (count turtles with[h_group = [h_group] of myself]) >= (household-num / group-num ) [set group-set group-set + 1]
  ]
  ask turtles[set color scale-color black h_group group-num 1]

  ; setting group living land


    ask turtles
  [move-to one-of patches with[p_cover-type = 100 and count turtles-here = 0 and p_group = [h_group] of myself]
   set h_labor_pop 2 set h_non_labor_pop 2 + random 2 set h_people h_labor_pop + h_non_labor_pop ;setting population
   ]
 set whole-population sum [h_people] of turtles

  end

To show-dem
 let max-z max [p_elevation] of patches
 let min-z min [p_elevation] of patches with [p_elevation > -9999]
 ask patches with [ p_elevation > -9999]
     [ set pcolor scale-color orange p_elevation max-z min-z]
End

To show-slope
 let max-z max [p_slope] of patches
 let min-z min [p_slope] of patches with [p_slope > -9999]
 ask patches with [ p_slope > -9999]
     [ set pcolor scale-color red p_slope max-z min-z]
End

to go

  if ticks > 0 [update-scenario]
  environmental-model
  human-decision
  tick
  if ticks >= stop-when [stop]
  if count turtles < household-num / 2
   [
    stop]

end

to update-scenario
  ;climate change scenario
  ;initial set
    ask patches
  [ set p_rainfall random-normal 1046 83
    set p_temp random-normal 9.19 1.30]
     ;land-usable change caused by climate change

  ;climate change
  if ClimateChangeScenario = "RCP2.6"
  [ask patches
    [let temp_increase ((random-normal 1 0.3) / 20) * ticks
     let precipation_increase_rate ((temp_increase * 0.04) / 20) * ticks
     set p_temp p_temp + temp_increase
     set p_rainfall p_rainfall * (1 + precipation_increase_rate)  ]
     ]
     ask patches with[p_agri-usable = 210][if p_slope < 5 and p_elevation < 100 and p_temp > (10 - (random-float 1)) [set p_agri-usable 2]]
     ask patches with[p_agri-usable = 220][if p_temp < (10 - (random-float 1)) [set p_agri-usable 21]]

    if ClimateChangeScenario = "RCP4.5"
  [ask patches
    [let temp_increase ((random-normal 1.4 0.3) / 20) * ticks
     let precipation_increase_rate ((temp_increase * 0.04) / 20) * ticks
     set p_temp p_temp + temp_increase
     set p_rainfall p_rainfall * (1 + precipation_increase_rate)  ]
     ]
     ask patches with[p_agri-usable = 210][if p_slope < 5 and p_elevation < 100 and p_temp > (10 - (random-float 1)) [set p_agri-usable 2]]
     ask patches with[p_agri-usable = 220][if p_temp < (10 - (random-float 1)) [set p_agri-usable 21]]

      if ClimateChangeScenario = "RCP6.0"
  [ask patches
    [let temp_increase ((random-normal 1.3 0.3) / 20) * ticks
     let precipation_increase_rate ((temp_increase * 0.04) / 20) * ticks
     set p_temp p_temp + temp_increase
     set p_rainfall p_rainfall * (1 + precipation_increase_rate)  ]
     ]
     ask patches with[p_agri-usable = 210][if p_slope < 5 and p_elevation < 100 and p_temp > (10 - (random-float 1)) [set p_agri-usable 2]]
     ask patches with[p_agri-usable = 220][if p_temp < (10 - (random-float 1)) [set p_agri-usable 21]]

      if ClimateChangeScenario = "RCP8.5"
  [ask patches
    [let temp_increase ((random-normal 2 0.3) / 20) * ticks
     let precipation_increase_rate ((temp_increase * 0.04) / 20) * ticks
     set p_temp p_temp + temp_increase
     set p_rainfall p_rainfall * (1 + precipation_increase_rate)  ]
     ]
     ask patches with[p_agri-usable = 210][if p_slope < 5 and p_elevation < 100 and p_temp > (10 - (random-float 1)) [set p_agri-usable 2]]
     ask patches with[p_agri-usable = 220][if p_temp < (10 - (random-float 1)) [set p_agri-usable 21]]

  ;abandon year of bare land
     ask patches with[p_slope > -9999][ifelse p_cover-type = 600 [set P_abandon-year P_abandon-year + 1][set P_abandon-year 0] ]
  ;ask patches with[p_cover-type = 210 or p_cover-type = 220][set p_labor 0.9 + ((random 10) * 0.01)]

end

to environmental-model
  calc-usle
  calc-ndvi
  calc-soil-quality
  calc-yield-potential
  calc-yield-estimation
end

to calc-usle
 ask patches with[p_slope > -9999]
 [
   set p_rfactor (p_rainfall * 0.35) + 38.5
   if p_cover-type = 100 [set p_cfactor 0  set p_pfactor 0 ]
   if p_cover-type = 210 [set p_cfactor 0.1 set p_pfactor 0.1]
   if p_cover-type = 220 [set p_cfactor 0.2 set p_pfactor 0.5]
   if p_cover-type = 300 [set p_cfactor 0.001 set p_pfactor 1]
   if p_cover-type = 400 [set p_cfactor 0.013  set p_pfactor 1 ]
   if p_cover-type = 500 [set p_cfactor 0  set p_pfactor 0 ]
   if p_cover-type = 600 [set p_cfactor 1  set p_pfactor 1 ]
   if p_cover-type = 700 [set p_cfactor 0  set p_pfactor 0 ]
   set p_usle p_rfactor * p_kfactor * p_lsfactor * p_cfactor * p_pfactor
   set p_usle p_usle * 100]
 show-usle
end

to calc-ndvi
  ask patches with[p_cover-type = 210]
  [let non-random 0
   if p_slope = 0 [set p_slope 0.0001]
   if p_elevation = 0 [set p_elevation 0.0001]
   if p_rainfall = 0 [set p_rainfall 0.0001]
   set non-random 1.464 + (-0.019 * (ln p_slope)) + (-0.026 * (ln p_elevation)) + (0.215 * (ln p_rainfall)) + (-0.023 * p_temp)
   set non-random ln non-random
   set p_ndvi (non-random * 0.158) + (0.842 * (0.6 + random-float 0.3))
  ]

  ask patches with[p_cover-type = 220]
  [let non-random 0
   if p_elevation = 0 [set p_elevation 0.0001]
   set non-random 2.115 + (0.005 * p_slope) + (-0.01 * p_as) + (0.02 * (ln p_elevation)) + (0.002 * p_temp)
   set non-random ln non-random
   set p_ndvi (non-random * 0.142) + (0.858 * (0.6 + random-float 0.3))
  ]

  ask patches with[p_cover-type = 300 or p_cover-type = 400 or p_cover-type = 500]
  [let non-random 0
       if p_slope = 0 [set p_slope 0.0001]
   if p_elevation = 0 [set p_elevation 0.0001]
   if p_rainfall = 0 [set p_rainfall 0.0001]
   set non-random 7111.178 + (-30.361 * p_as) + (83.839 * (ln p_slope)) + (213.554 * (ln p_elevation)) + (36.004 * p_temp) + (-1.107 * (ln p_rainfall))
   set non-random non-random / 10000
   set p_ndvi (non-random * 0.289) + (0.711 * (0.8 + random-float 0.2))
  ]

  ask patches with[p_cover-type = 100 or p_cover-type = 600 or p_cover-type = 700]
  [set p_ndvi 0]

    ask patches with[p_cover-type = -9999]
  [set p_ndvi -9999]

  show-ndvi

end

to calc-soil-quality
  ask patches with[p_slope > -9999]
  [if ticks = 0 [set p_soil-potential 0.3 + (random 2 * random-float 0.2) - (random 2 * random 2 * random-float 0.2)];(((random 3) - 1) * random-float 0.2)]
   if p_usle < 100 [ set p_soil-physical-point 0.5 ]
     if p_usle >= 100 and p_usle < 500 [ set p_soil-physical-point 0.45]
     if p_usle >= 500 and p_usle < 1000 [set p_soil-physical-point 0.4]
     if p_usle >= 1000 [set p_soil-physical-point 0.35 ]
   set p_soil-quality p_soil-physical-point + p_soil-potential]

    ask patches with[p_slope = -9999]
  [set p_soil-quality -9999]
  set mean-soil-quality mean [p_soil-quality] of patches with[p_slope > -9999]
  end

to calc-yield-potential ; Hong et al.(2009)
  ask patches with[p_cover-type = 210 or p_agri-usable = 2]
  [let random_num 0.6 + random-float 0.3
    set p_yield-potential (0.449 * (((1137.209 * p_ndvi) - 601.416) * 62.5)) + (0.551 * (0.449 * (((1137.209 * random_num) - 601.416) * 62.5)))
    if p_yield-potential < 0 [set p_yield-potential 0.0000001]
  ]
  ask patches with[p_cover-type = 220 or p_agri-usable = 21]
  [let random_num 0.6 + random-float 0.3
    set p_yield-potential (0.555 * (((0.66 * p_ndvi) - 0.641) * 6.25 * 1000)) + (0.445 * (0.449 * (((0.66 * random_num) - 0.641) * 6.25 * 1000)))
    if p_yield-potential < 0 [set p_yield-potential 0.0000001]
  ]

  ask patches with[p_agri-usable = -9999]
  [set p_yield-potential 0]
   ask patches with[p_slope = -9999]
  [set p_yield-potential -9999]

  ask patches[set p_yield-potential (p_yield-potential / 100)]

end

to calc-yield-estimation
    ask patches
  [ifelse p_cover-type = 210 or p_cover-type = 220
    [set p_yield-estimation p_yield-potential * p_labor  * p_soil-quality ] [set p_yield-estimation 0]
    if p_yield-potential < 0 [set p_yield-estimation 0]]
  set whole-yield-estimation sum [p_yield-estimation] of patches with[p_yield > 0]
end

to human-decision
  ask turtles[set h_time 8 * 300 * h_labor_pop] ; 8 hours per day, 300 days per year and labor population
  if ticks = 0 [food-pressure-estimation]
  land-allocation
  labor-Strategy
  land-use-decision
  calc-yield
  if ticks != 0[food-pressure]
  show-landuse
  ask patches with[p_cover-type = 210 or p_cover-type = 220][ifelse p_yield > p_yield-potential [set p_soil-potential p_soil-potential -  (0.5 * (p_labor - p_soil-quality))][set p_soil-potential p_soil-potential]]
  ask turtles with[h_food-pressure > 4 + ((random-float 0.2) - 0.1) and h_hunger = 0]
  [set h_hunger 1 set h_hunger-time h_hunger]
  ask turtles with[h_food-pressure > 4 + ((random-float 0.2) - 0.1) and h_hunger = 1]
  [set h_hunger-time h_hunger-time + 1]
  ask turtles with[h_food-pressure <= 4 + ((random-float 0.2) - 0.1) and h_hunger = 0]
  [set h_hunger 0 set h_hunger-time h_hunger]
  ask turtles with[h_hunger-time > 3][die]

end


to food-pressure-estimation

 ask turtles
[let yield_mean 34179.98 * 0.001 ; faostat(180619)
 let yield_std 8090.841 * 0.001 ; faostat(180619)
 let a count patches with[(p_cover-type = 210 or p_cover-type = 220) and p_group = [h_group] of myself]
    let b count turtles with[h_group =[h_group] of myself]
 set h_yield (a * random-normal yield_mean yield_std) / b
 set h_yield_gov (h_yield * 0.2) + (h_yield * (0.3 + random-float 0.1)) + (0.2 + random-float 0.1)
 set h_yield_mine h_yield - h_yield_gov]

set gov-yield sum [h_yield_gov] of turtles
set gov-food gov-yield + (food-aid * 1000 * 0.3)
set gov-food-a-man gov-food / whole-population
ask turtles
[set h_food_gov gov-food-a-man * h_people
 set h_food h_food_gov + h_yield_mine
 set h_food-houshold h_food / h_people
 set h_food-pressure (164.25 * h_people) / h_food-houshold]
set whole-food-pressure mean [h_food-pressure] of turtles

end

to land-allocation
  ask patches[set p_farmer_id -1]
  let loopnum 1
  while [loopnum <= group-num]
  [ while [count turtles with[h_group = loopnum] < count patches with[p_group = loopnum and p_cover-type >= 200 and p_cover-type < 300 and p_farmer_id = -1]]
    [ask turtles with[h_group = loopnum]
      [move-to one-of patches with[p_group = loopnum and p_cover-type >= 200 and p_cover-type < 300 and p_farmer_id = -1]
        ask patch-here[set p_farmer_id [who] of myself
      ]

    ]]
   if count patches with[p_group = loopnum and p_cover-type >= 200 and p_cover-type < 300 and p_farmer_id = -1] > 0
    [ask patches with[p_group = loopnum and p_cover-type >= 200 and p_cover-type < 300 and p_farmer_id = -1][set p_farmer_id [who] of one-of turtles with[h_group = loopnum]]]

    set loopnum loopnum + 1
  ]
end

to labor-strategy
  let paddy-labor-time labor-time-paddy + ((random 3 - 1) * (random-float 0.5))
  let non-paddy-labor-time labor-time-non-paddy + ((random 3 - 1) * (random-float 2))
  ask turtles with[h_time > 0]
  [foreach sort-on [p_soil-quality] patches with[p_farmer_id =[who] of myself]
    [the-patch -> move-to the-patch

        if [p_cover-type] of patch-here = 210 and h_time > paddy-labor-time [ask patch-here[set p_time paddy-labor-time] set h_time h_time - paddy-labor-time]
        if [p_cover-type] of patch-here = 210 and h_time <= paddy-labor-time [ask patch-here[set p_time [h_time] of myself] set h_time 0]
        if [p_cover-type] of patch-here = 220 and h_time > paddy-labor-time [ask patch-here[set p_time non-paddy-labor-time] set h_time h_time - non-paddy-labor-time]
        if [p_cover-type] of patch-here = 220 and h_time <= paddy-labor-time [ask patch-here[set p_time [h_time] of myself] set h_time 0]
        ]
    ]

  ask turtles with[h_time = 0 and h_food-pressure > 1]
  [ let st-criteria0 0.5 + ((random 3 - 1) * random-float 0.05)
    let st-criteria1 0.6 + ((random 3 - 1) * random-float 0.05)
    let st-criteria2 0.7 + ((random 3 - 1) * random-float 0.05)
    let st-criteria3 0.8 + ((random 3 - 1) * random-float 0.05)
    let st-criteria4 0.9 + ((random 3 - 1) * random-float 0.05)

    let labor-criteria1 0.1 + ((random 3 - 1) * random-float 0.05)
    let labor-criteria2 0.2 + ((random 3 - 1) * random-float 0.05)

    if count patches with[p_farmer_id =[who] of myself and p_soil-quality < st-criteria0] > 0
    [set h_time h_time + sum [p_time] of patches with[p_farmer_id =[who] of myself and p_soil-quality < st-criteria0]
     ask patches with[p_farmer_id =[who] of myself and p_soil-quality < st-criteria0][set p_time 0]
    ]
    if count patches with[p_farmer_id =[who] of myself and p_soil-quality >= st-criteria0 and p_soil-quality < st-criteria1] > 0
    [set h_time h_time + (sum [p_time] of patches with[p_farmer_id =[who] of myself and p_soil-quality >= st-criteria0 and p_soil-quality < st-criteria1]) * labor-criteria2
     ask patches with[p_farmer_id =[who] of myself and p_soil-quality >= st-criteria0 and p_soil-quality < st-criteria1][set p_time p_time * (1 - labor-criteria2)]
    ]
    if count patches with[p_farmer_id =[who] of myself and p_soil-quality >= st-criteria1 and p_soil-quality < st-criteria2] > 0
    [set h_time h_time + (sum [p_time] of patches with[p_farmer_id =[who] of myself and p_soil-quality >= st-criteria1 and p_soil-quality < st-criteria2]) * labor-criteria1
     ask patches with[p_farmer_id =[who] of myself and p_soil-quality >= st-criteria1 and p_soil-quality < st-criteria2][set p_time p_time * (1 - labor-criteria1)]
    ]
    if h_time > 0[
    foreach sort-on [(- p_soil-quality)] patches with[p_farmer_id =[who] of myself and st-criteria4 >= 0.9]
    [the-patch -> move-to the-patch
       ifelse h_time > [p_time] of patch-here * (1 + labor-criteria2)
        [set h_time h_time - ([p_time] of patch-here * (1 + labor-criteria2)) ask patch-here[set p_time p_time * (1 + labor-criteria2)]]
        [ask patch-here[set p_time p_time + [h_time] of myself] set h_time 0]
    ]
    ]

    ]
ask patches with[(p_cover-type = 210 or p_cover-type = 220) and p_time = 0][set p_cover-type 600  set p_luchange_log  p_luchange_log + 1]


end



to land-use-decision
  let paddy-labor-time labor-time-paddy + ((random 3 - 1) * (random-float 0.5))
  let non-paddy-labor-time labor-time-non-paddy + ((random 3 - 1) * (random-float 2))
   calc-yield-estimation
  ifelse ticks = 0 [food-pressure-estimation][food-pressure]
  let ignore-slope-limit 3.5 + ((random-float 0.2) - 0.1)
  ;land-use change procedure(non-agri->agri) when agent's food pressure is higher than 1.
  ask turtles with[h_food-pressure > 1 and h_time > 0][
  ask patches with[p_group = [h_group] of myself]
    [ if (p_cover-type = 300 or p_cover-type = 400 or p_cover-type = 600) and p_slope < slope-limit and p_soil-quality >= 0.5 [set p_stratgy_changeable 1] ]
    if h_food-pressure > ignore-slope-limit
    [  ask patches with[p_group = [h_group] of myself]
    [ if (p_cover-type = 300 or p_cover-type = 400 or p_cover-type = 600) and p_soil-quality >= 0.5 [set p_stratgy_changeable 1] ]]
    let endwhile count patches with[p_stratgy_changeable = 1 and p_group = [h_group] of myself]
    let loopnum 1
    let sum-time-myself sum [p_time] of patches with [p_group = [h_group] of myself]
    let endloop (h_food-pressure + ((random-float 0.2) - 0.1)) ^ 3.5;(h_food-pressure * 10) + ((random-float 2.5) - 5);round (sum-labor-myself *((1  + random-float 0.1)/ 100)                        )
   while [ h_time > 0 and endwhile > 0 and loopnum < endloop]
   [if (count patches with[p_stratgy_changeable = 1 and p_group = [h_group] of myself and count neighbors with[p_cover-type = 210 or p_cover-type = 220 or p_cover-type = 100] > 0]) != 0
     [ask one-of patches with[p_stratgy_changeable = 1 and p_group = [h_group] of myself and count neighbors with[p_cover-type = 210 or p_cover-type = 220 or p_cover-type = 100] > 0]
        [ifelse p_agri-usable = 2 [set p_cover-type 210 set p_farmer_id [who] of myself set p_time (paddy-labor-time * 0.5)  set p_stratgy_changeable 0 set p_luchange_log p_luchange_log + 1 ]
          [set p_cover-type 220 set p_farmer_id [who] of myself set p_time (non-paddy-labor-time * 0.5)  set p_stratgy_changeable 0 set p_luchange_log p_luchange_log + 1 ]]]
     set h_redundancy (h_redundancy + sum [p_redundancy] of patches with[p_farmer_id = [who] of myself])
    ask patches with[p_group = [h_group] of myself] [set p_redundancy 0]
    set endwhile count patches with[p_stratgy_changeable = 1 and p_group = [h_group] of myself]

    set loopnum loopnum + 1
    ]
   ask patches with[p_group = [h_group] of myself][set p_redundancy 0 set p_stratgy_changeable 0]  ]

end


to food-pressure
  if Food-aid-activate = true [set food-aid (164.25 * 400 * 4) * (Food-aid-percent / 100) ]
ask turtles
[set h_yield sum [p_yield] of patches with[p_farmer_id = [who] of myself]
 set h_yield_gov (h_yield * 0.2) + (h_yield * (0.3 + random-float 0.1)) + (0.2 + random-float 0.1)
 set h_yield_mine h_yield - h_yield_gov]

set gov-yield sum [h_yield_gov] of turtles
set gov-food gov-yield + food-aid
set gov-food-a-man gov-food / whole-population
ask turtles
[set h_food_gov gov-food-a-man * h_people
 set h_food h_food_gov + h_yield_mine
 set h_food-houshold h_food / h_people
 set h_food-pressure (164.25 * h_people) / h_food-houshold
    if h_food-pressure < 0[set h_food-pressure 0]]
set whole-food-pressure mean [h_food-pressure] of turtles



end

to calc-yield
  let paddy-labor-time labor-time-paddy + ((random 3 - 1) * (random-float 0.5))
  let non-paddy-labor-time labor-time-non-paddy + ((random 3 - 1) * (random-float 2))

    ask patches with[p_cover-type > -9999]
  [if p_cover-type = 210[set p_labor p_time / paddy-labor-time]
   if p_cover-type = 220[set p_labor p_time / non-paddy-labor-time]
    ifelse p_cover-type = 210 or p_cover-type = 220
    [set p_yield p_yield-potential * p_labor  * p_soil-quality] [set p_yield 0]
    if p_yield-potential < 0 [set p_yield 0]]
  ask patches with[p_rainfall > (2000 + random 100)]  [set p_yield 0]
  set whole-yield sum [p_yield] of patches with[p_yield > 0]

      ;set yield-sd (standard-deviation [p_yield] of patches with [p_yield > 0])
  ;set yield-mean (mean [p_yield] of patches with [p_yield > 0])
end


to show-ndvi
   ask patches with [ p_ndvi > -9999]
   [ if p_ndvi < 0 [ set pcolor white ]
     if p_ndvi >= 0 and p_ndvi < 0.6 [ set pcolor 69]
     if p_ndvi >= 0.6 and p_ndvi < 0.65 [set pcolor 67]
     if p_ndvi >= 0.65 and p_ndvi < 0.7 [set pcolor 66]
     if p_ndvi >= 0.7 and p_ndvi < 0.75 [set pcolor 65]
     if p_ndvi >= 0.75 and p_ndvi < 0.8[set pcolor 64]
     if p_ndvi >= 0.8 and p_ndvi < 0.85[set pcolor 63]
     if p_ndvi >= 0.85 and p_ndvi < 0.9[set pcolor 62]
     if p_ndvi >= 0.9 [set pcolor 61]
     ]
    ask patches with [ p_ndvi = -9999][set pcolor black]
end

to show-usle
   ask patches with [ p_usle > -9999]
   [ if p_usle < 100 [ set pcolor white ]
     if p_usle >= 100 and p_usle < 500 [ set pcolor yellow]
     if p_usle >= 500 and p_usle < 1000 [set pcolor orange]
     if p_usle >= 1000 [set pcolor red]]
     ;[ set pcolor scale-color green p_ndvi -1 1]
end

To show-landuse
 ask patches with [ p_cover-type > -9999]
     [ if p_cover-type = 100 [set pcolor red] ; urban areas

       if p_cover-type = 210 [set pcolor cyan] ; Paddy rice fields
       if p_cover-type = 220 [set pcolor yellow] ; Ppland crop fields
       if p_cover-type = 230 [set pcolor orange] ; 'Green-house' farms
       if p_cover-type = 240 [set pcolor violet] ; Orchards
       if p_cover-type = 250 [set pcolor grey] ; Other agriculture

       if p_cover-type = 300 [set pcolor 63] ; Forested land (dark green)

       if p_cover-type = 400 [set pcolor 67] ; Grassland (light green)

       if p_cover-type = 500 [set pcolor sky] ; Wetland

       if p_cover-type = 600 [set pcolor pink] ; Bare land

       if p_cover-type = 700 [set pcolor blue] ; Water surface
     ]
End

@#$#@#$#@
GRAPHICS-WINDOW
210
10
720
521
-1
-1
2.0
1
10
1
1
1
0
0
0
1
0
250
0
250
0
0
1
ticks
30.0

BUTTON
10
30
79
63
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
11
82
183
115
household-num
household-num
350
400
400.0
1
1
NIL
HORIZONTAL

SLIDER
12
130
184
163
group-num
group-num
5
10
10.0
1
1
NIL
HORIZONTAL

BUTTON
87
30
187
63
Simulation
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
11
177
183
210
slope-limit
slope-limit
15
30
15.0
1
1
NIL
HORIZONTAL

SLIDER
12
222
184
255
stop-when
stop-when
0
100
100.0
1
1
NIL
HORIZONTAL

PLOT
739
20
1002
216
Landuse Change
NIL
NIL
0.0
10.0
0.0
50.0
true
true
"" ""
PENS
"Paddy Rice" 1.0 0 -11221820 true "" "plot ((count patches with[p_cover-type = 210] / count patches) * 100)"
"Other Agri" 1.0 0 -1184463 true "" "plot ((count patches with[p_cover-type = 220] / count patches) * 100)"
"Forest" 1.0 0 -10899396 true "" "plot ((count patches with[p_cover-type = 300] / count patches) * 100)"
"Bareland" 1.0 0 -2064490 true "" "plot ((count patches with[p_cover-type = 600] / count patches)) * 100 "

PLOT
741
229
1001
399
Soil Quality
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean-soil-quality"

PLOT
741
410
941
560
Food Pressure
NIL
NIL
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot whole-food-pressure"

PLOT
1010
19
1257
217
Yield
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot whole-yield"

SWITCH
1268
300
1434
333
SurfaceCurvature
SurfaceCurvature
1
1
-1000

SLIDER
11
275
183
308
labor-time-paddy
labor-time-paddy
0
60
36.0
1
1
NIL
HORIZONTAL

SLIDER
10
316
205
349
labor-time-non-paddy
labor-time-non-paddy
0
80
48.0
1
1
NIL
HORIZONTAL

CHOOSER
12
370
194
415
ClimateChangeScenario
ClimateChangeScenario
"Not Change" "RCP2.6" "RCP4.5" "RCP6.0" "RCP8.5"
4

PLOT
1014
227
1261
396
Households
NIL
NIL
0.0
10.0
0.0
400.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

PLOT
958
414
1158
564
Temperature
NIL
NIL
0.0
10.0
8.0
15.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [p_temp] of patches"

PLOT
1178
415
1378
565
Precipitation
NIL
NIL
0.0
10.0
900.0
2000.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [p_rainfall] of patches"

SWITCH
11
424
181
457
Food-aid-activate
Food-aid-activate
0
1
-1000

SLIDER
12
470
184
503
Food-aid-Percent
Food-aid-Percent
0
100
62.5
0.5
1
NIL
HORIZONTAL

@#$#@#$#@
Please read these links for details.

Link 1(in English): https://doi.org/10.3390/land12040735
Link 2(in Korean with English abstract, see Chapter 4): https://snu-primo.hosted.exlibrisgroup.com/permalink/f/1l6eo7m/82SNU_SSPACE210371/134046



## WHAT IS IT?

This model simulates an "Artificial North Korean Collective Farm," which abstracts the farmland landscape of North Korea. The model is set in the 1960s, a period when North Korean authorities had established the collective farm system. It is an agent-based model comprising household agents, an environment representing land, soil, and food yield, and external factors reflecting policy. The primary objective of this model is to simulate the process of land degradation and declining food yield in North Korea, with a particular focus on the path to the significant North Korean famine in the 1990s. It seeks to identify strategies for mitigating land degradation and famine through the testing of various scenarios.

The concept of "Artificial North Korean Collective Farm" model is as [link](https://www.mdpi.com/land/land-12-00735/article_deploy/html/images/land-12-00735-g001.png).
  

## HOW IT WORKS
The model consist of environment, human agent, and external factor submodule.The cycle of the model is annual. The structure of the model is as [link](https://www.mdpi.com/land/land-12-00735/article_deploy/html/images/land-12-00735-g002.png).

The model operates through a multi-layered grid system in the environment submodule. This grid consists of a 250 by 250 array, with each grid cell covering an area of 100 square meters. Each layer within this grid structure represents distinct processes that play a role in both land degradation and land-use change.

The human agent submodule comprises 400 households, each equipped with specific attributes and decision-making mechanisms. These households have a direct influence on both land-use changes and soil quality. The environmental submodule's output is the determination of soil quality, which, in turn, impacts food yield. Consequently, this model is characterized by a feedback loop that describes the interconnected relationship between land degradation and food yield, with each iteration representing a single year in the simulation.


The external factor submodule consist of climate change, change of food aid reflecting international relations, and land-use restriction change. The climate change factor reflects RCP scenarions based on IPCC(2013). The change of food aid is the variable representing changes in food aid indicates the annual quantity of food that can be supplied or imported, and it is contingent upon the model's overall population support. To illustrate, setting this factor at 100% means that the entire additional food needed to sustain the population would be sourced from external (foreign) suppliers, while a setting of 0% would render the model a closed system. Consequently, this variable plays a pivotal role in shaping the pressure agents experience concerning food availability. The factor of land-use restriction change is related to the forest clearing to expand the farm in North Korea, such as "da-rak-bat(moutain clearing for farmland)".



## HOW TO USE IT
To begin simulating, users should adjust the scenario sliders and options on the left and then click the "Setup" button. Subsequently, the "Simulation" button can be pressed to observe the changes in the farm's land use. The central section of the interface visually represents the evolving land-use patterns, while the right section displays simulation statistics.

Here's an explanation of the sliders and options:

Initial Set of Agent Number ("household-num") and Group of Agent ("group-num"): These parameters are pre-configured based on typical information about North Korean cooperative farms. They are designed to be adjustable to facilitate studies exploring the farm's size and social structure in different contexts.

Slope Limit ("slope-limit"): This slider is related to external factors affecting land-use restrictions. It determines whether deforestation is permitted on steep-sloped forest areas (initially set at 15 degrees). Notably, households may disregard this policy if they face extreme food pressure (above a threshold of 3 in this model), reflecting real-world dynamics.

"Stop-When" Option: This option enables users to specify under which circumstances the model simulation should end, provided that the model world has not collapsed.

Labor Time Options ("labor-time-paddy" and "labor-time-non-paddy"): These options pertain to the annual standard labor time (LTreference) required for each land cover or crop to yield a harvest. Since this information is not readily available for North Korea, the initial values are derived from test simulations. Users can modify these values based on their own research or assumptions.

For a deeper understanding of certain aspects of the model's operation, particularly "ClimateChangeScenario," "Food-aid-activate," and "Food-aid-Percent," users are encouraged to refer to the "How it works" section, which elaborates on the role of external factors in the simulation.

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="basemodel" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>((count patches with[p_cover-type = 210] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 220] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 300] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 600] / count patches) * 100)</metric>
    <metric>sum [p_yield] of patches</metric>
    <metric>mean [p_soil-quality] of patches</metric>
    <metric>mean [h_food-pressure] of turtles</metric>
    <metric>mean [p_temp] of patches</metric>
    <metric>mean [p_rainfall] of patches</metric>
    <metric>ndvi_M3</metric>
    <metric>ndvi_M5</metric>
    <metric>ndvi_M7</metric>
    <metric>yield_M3</metric>
    <metric>yield_M5</metric>
    <metric>yield_M7</metric>
    <metric>soil-quality_m3</metric>
    <metric>soil-quality_m5</metric>
    <metric>soil-quality_m7</metric>
    <enumeratedValueSet variable="labor-time-non-paddy">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ClimateChangeScenario">
      <value value="&quot;RCP8.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-time-paddy">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope-limit">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-activate">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SurfaceCurvature">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="household-num">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-num">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-when">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ndvi_M3</metric>
    <metric>ndvi_M5</metric>
    <metric>ndvi_M7</metric>
    <metric>yield_M3</metric>
    <metric>yield_M5</metric>
    <metric>yield_M7</metric>
    <metric>soil-quality_M3</metric>
    <metric>soil-quality_M5</metric>
    <metric>soil-quality_M7</metric>
    <metric>((count patches with[p_cover-type = 210] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 220] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 300] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 600] / count patches) * 100)</metric>
    <metric>sum [p_yield] of patches</metric>
    <metric>mean [p_soil-quality] of patches</metric>
    <metric>mean [p_ndvi] of patches</metric>
    <metric>mean [h_food-pressure] of turtles</metric>
    <enumeratedValueSet variable="labor-time-non-paddy">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ClimateChangeScenario">
      <value value="&quot;RCP8.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-time-paddy">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SurfaceCurvature">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-activate">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope-limit">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="household-num">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-num">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-when">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-Percent">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="aid_50" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ndvi_M3</metric>
    <metric>ndvi_M5</metric>
    <metric>ndvi_M7</metric>
    <metric>yield_M3</metric>
    <metric>yield_M5</metric>
    <metric>yield_M7</metric>
    <metric>soil-quality_M3</metric>
    <metric>soil-quality_M5</metric>
    <metric>soil-quality_M7</metric>
    <metric>((count patches with[p_cover-type = 210] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 220] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 300] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 600] / count patches) * 100)</metric>
    <metric>sum [p_yield] of patches</metric>
    <metric>mean [p_soil-quality] of patches</metric>
    <metric>mean [p_ndvi] of patches</metric>
    <metric>mean [h_food-pressure] of turtles</metric>
    <enumeratedValueSet variable="labor-time-non-paddy">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ClimateChangeScenario">
      <value value="&quot;RCP8.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-time-paddy">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SurfaceCurvature">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-activate">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope-limit">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="household-num">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-num">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-when">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-Percent">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="aid_100" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ndvi_M3</metric>
    <metric>ndvi_M5</metric>
    <metric>ndvi_M7</metric>
    <metric>yield_M3</metric>
    <metric>yield_M5</metric>
    <metric>yield_M7</metric>
    <metric>soil-quality_M3</metric>
    <metric>soil-quality_M5</metric>
    <metric>soil-quality_M7</metric>
    <metric>((count patches with[p_cover-type = 210] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 220] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 300] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 600] / count patches) * 100)</metric>
    <metric>sum [p_yield] of patches</metric>
    <metric>mean [p_soil-quality] of patches</metric>
    <metric>mean [p_ndvi] of patches</metric>
    <metric>mean [h_food-pressure] of turtles</metric>
    <enumeratedValueSet variable="labor-time-non-paddy">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ClimateChangeScenario">
      <value value="&quot;RCP8.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-time-paddy">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SurfaceCurvature">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-activate">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope-limit">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="household-num">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-num">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-when">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-Percent">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="aid_25" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ndvi_M3</metric>
    <metric>ndvi_M5</metric>
    <metric>ndvi_M7</metric>
    <metric>yield_M3</metric>
    <metric>yield_M5</metric>
    <metric>yield_M7</metric>
    <metric>soil-quality_M3</metric>
    <metric>soil-quality_M5</metric>
    <metric>soil-quality_M7</metric>
    <metric>((count patches with[p_cover-type = 210] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 220] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 300] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 600] / count patches) * 100)</metric>
    <metric>sum [p_yield] of patches</metric>
    <metric>mean [p_soil-quality] of patches</metric>
    <metric>mean [p_ndvi] of patches</metric>
    <metric>mean [h_food-pressure] of turtles</metric>
    <enumeratedValueSet variable="labor-time-non-paddy">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ClimateChangeScenario">
      <value value="&quot;RCP8.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-time-paddy">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SurfaceCurvature">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-activate">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope-limit">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="household-num">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-num">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-when">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-Percent">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="aid_0" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ndvi_M3</metric>
    <metric>ndvi_M5</metric>
    <metric>ndvi_M7</metric>
    <metric>yield_M3</metric>
    <metric>yield_M5</metric>
    <metric>yield_M7</metric>
    <metric>soil-quality_M3</metric>
    <metric>soil-quality_M5</metric>
    <metric>soil-quality_M7</metric>
    <metric>((count patches with[p_cover-type = 210] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 220] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 300] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 600] / count patches) * 100)</metric>
    <metric>sum [p_yield] of patches</metric>
    <metric>mean [p_soil-quality] of patches</metric>
    <metric>mean [p_ndvi] of patches</metric>
    <metric>mean [h_food-pressure] of turtles</metric>
    <enumeratedValueSet variable="labor-time-non-paddy">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ClimateChangeScenario">
      <value value="&quot;RCP8.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-time-paddy">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SurfaceCurvature">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-activate">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope-limit">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="household-num">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-num">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-when">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-Percent">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="aid_75" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ndvi_M3</metric>
    <metric>ndvi_M5</metric>
    <metric>ndvi_M7</metric>
    <metric>yield_M3</metric>
    <metric>yield_M5</metric>
    <metric>yield_M7</metric>
    <metric>soil-quality_M3</metric>
    <metric>soil-quality_M5</metric>
    <metric>soil-quality_M7</metric>
    <metric>((count patches with[p_cover-type = 210] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 220] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 300] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 600] / count patches) * 100)</metric>
    <metric>sum [p_yield] of patches</metric>
    <metric>mean [p_soil-quality] of patches</metric>
    <metric>mean [p_ndvi] of patches</metric>
    <metric>mean [h_food-pressure] of turtles</metric>
    <enumeratedValueSet variable="labor-time-non-paddy">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ClimateChangeScenario">
      <value value="&quot;RCP8.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-time-paddy">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SurfaceCurvature">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-activate">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope-limit">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="household-num">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-num">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-when">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-Percent">
      <value value="75"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="aid_62.5" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ndvi_M3</metric>
    <metric>ndvi_M5</metric>
    <metric>ndvi_M7</metric>
    <metric>yield_M3</metric>
    <metric>yield_M5</metric>
    <metric>yield_M7</metric>
    <metric>soil-quality_M3</metric>
    <metric>soil-quality_M5</metric>
    <metric>soil-quality_M7</metric>
    <metric>((count patches with[p_cover-type = 210] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 220] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 300] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 600] / count patches) * 100)</metric>
    <metric>sum [p_yield] of patches</metric>
    <metric>mean [p_soil-quality] of patches</metric>
    <metric>mean [p_ndvi] of patches</metric>
    <metric>mean [h_food-pressure] of turtles</metric>
    <enumeratedValueSet variable="labor-time-non-paddy">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ClimateChangeScenario">
      <value value="&quot;RCP8.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-time-paddy">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SurfaceCurvature">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-activate">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope-limit">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="household-num">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-num">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-when">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-Percent">
      <value value="62.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="aid_87.5" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ndvi_M3</metric>
    <metric>ndvi_M5</metric>
    <metric>ndvi_M7</metric>
    <metric>yield_M3</metric>
    <metric>yield_M5</metric>
    <metric>yield_M7</metric>
    <metric>soil-quality_M3</metric>
    <metric>soil-quality_M5</metric>
    <metric>soil-quality_M7</metric>
    <metric>((count patches with[p_cover-type = 210] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 220] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 300] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 600] / count patches) * 100)</metric>
    <metric>sum [p_yield] of patches</metric>
    <metric>mean [p_soil-quality] of patches</metric>
    <metric>mean [p_ndvi] of patches</metric>
    <metric>mean [h_food-pressure] of turtles</metric>
    <enumeratedValueSet variable="labor-time-non-paddy">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ClimateChangeScenario">
      <value value="&quot;RCP8.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-time-paddy">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SurfaceCurvature">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-activate">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope-limit">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="household-num">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-num">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-when">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-Percent">
      <value value="87.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="aid_37.5" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ndvi_M3</metric>
    <metric>ndvi_M5</metric>
    <metric>ndvi_M7</metric>
    <metric>yield_M3</metric>
    <metric>yield_M5</metric>
    <metric>yield_M7</metric>
    <metric>soil-quality_M3</metric>
    <metric>soil-quality_M5</metric>
    <metric>soil-quality_M7</metric>
    <metric>((count patches with[p_cover-type = 210] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 220] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 300] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 600] / count patches) * 100)</metric>
    <metric>sum [p_yield] of patches</metric>
    <metric>mean [p_soil-quality] of patches</metric>
    <metric>mean [p_ndvi] of patches</metric>
    <metric>mean [h_food-pressure] of turtles</metric>
    <enumeratedValueSet variable="labor-time-non-paddy">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ClimateChangeScenario">
      <value value="&quot;RCP8.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-time-paddy">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SurfaceCurvature">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-activate">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope-limit">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="household-num">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-num">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-when">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-Percent">
      <value value="37.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="aid_12.5" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ndvi_M3</metric>
    <metric>ndvi_M5</metric>
    <metric>ndvi_M7</metric>
    <metric>yield_M3</metric>
    <metric>yield_M5</metric>
    <metric>yield_M7</metric>
    <metric>soil-quality_M3</metric>
    <metric>soil-quality_M5</metric>
    <metric>soil-quality_M7</metric>
    <metric>((count patches with[p_cover-type = 210] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 220] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 300] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 600] / count patches) * 100)</metric>
    <metric>sum [p_yield] of patches</metric>
    <metric>mean [p_soil-quality] of patches</metric>
    <metric>mean [p_ndvi] of patches</metric>
    <metric>mean [h_food-pressure] of turtles</metric>
    <enumeratedValueSet variable="labor-time-non-paddy">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ClimateChangeScenario">
      <value value="&quot;RCP8.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-time-paddy">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SurfaceCurvature">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-activate">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope-limit">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="household-num">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-num">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-when">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-Percent">
      <value value="37.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="nonaid" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ndvi_M3</metric>
    <metric>ndvi_M5</metric>
    <metric>ndvi_M7</metric>
    <metric>yield_M3</metric>
    <metric>yield_M5</metric>
    <metric>yield_M7</metric>
    <metric>soil-quality_M3</metric>
    <metric>soil-quality_M5</metric>
    <metric>soil-quality_M7</metric>
    <metric>((count patches with[p_cover-type = 210] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 220] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 300] / count patches) * 100)</metric>
    <metric>((count patches with[p_cover-type = 600] / count patches) * 100)</metric>
    <metric>sum [p_yield] of patches</metric>
    <metric>mean [p_soil-quality] of patches</metric>
    <metric>mean [p_ndvi] of patches</metric>
    <metric>mean [h_food-pressure] of turtles</metric>
    <enumeratedValueSet variable="labor-time-non-paddy">
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ClimateChangeScenario">
      <value value="&quot;RCP8.5&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-time-paddy">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SurfaceCurvature">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-activate">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="slope-limit">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="household-num">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-num">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stop-when">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Food-aid-Percent">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@

library(sets)

sets_options("universe", seq(from = 0, to = 100, by = 1))

variables <- set(
  weather = fuzzy_partition(
    varnames = c(bad = 40, ok = 70, perfect = 80),
    FUN = fuzzy_cone,
    radius = 10
  ),
  humidity = fuzzy_partition(
    varnames = c(dry = 30, good = 60, perfect = 80),
    sd = 3
  ),
  temperature = fuzzy_partition(
    varnames = c(cold = 30, good = 70, hot = 90),
    sd = 5
  ),
  precipitation = fuzzy_partition(
    varnames = c(no.rain = 40, little.rain = 70, rain = 90),
    sd = 10
    
  )
)

rules <- set(
  fuzzy_rule(temperature %is% hot && precipitation %is% little.rain, weather %is% ok),
  
  fuzzy_rule(temperature %is% hot && humidity %is% dry && precipitation %is% little.rain, weather %is% ok),
  
  fuzzy_rule(temperature %is% hot && humidity %is% wet && precipitation %is% rain, weather %is% bad),
  
  fuzzy_rule(temperature %is% good && humidity %is% dry && precipitation %is% no.rain, weather %is% perfect),
  
  fuzzy_rule(temperature %is% good || humidity %is% good || precipitation %is% little.rain, weather %is% ok),
  
  fuzzy_rule(temperature %is% cold, weather %is% bad)
)

system <- fuzzy_system(variables, rules)

plot(system)

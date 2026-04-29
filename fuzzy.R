library(sets)

sets_options("universe", seq(from = 0, to = 25, by = 0.1))

variables <- set(
  service = fuzzy_partition(
    varnames = c(poor = 0, good = 5, excellent = 10),
    sd = 1.5
  ),
  food = fuzzy_variable(
    rancid = fuzzy_trapezoid(corners = c(-2, 0, 2, 4)),
    delicious = fuzzy_trapezoid(corners = c(7, 9, 11, 13)) 
  ),
  tip = fuzzy_partition(
    varnames = c(cheap = 5, average = 12.5, generous = 20),
    FUN = fuzzy_cone,
    radius = 5
  )
)

rules <- set(
  fuzzy_rule(service %is% poor || food %is% rancid, tip %is% cheap),
  fuzzy_rule(service %is% good, tip %is% average),
  fuzzy_rule(service %is% excellent || food %is% delicious, tip %is% generous)
)

system <- fuzzy_system(variables, rules)
print(system)
plot(system)

fi <- fuzzy_inference(system, list(service = 3, food = 8))
plot(fi)

gset_defuzzify(fi, "centroid")

sets_options("universe", NULL)
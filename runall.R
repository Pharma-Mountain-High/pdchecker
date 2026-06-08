

#runall
r_scripts <- c("setup.R","PD1.R", "PD2.R", "PD3.R", "PD4.R","PD6.R","PD8.R","output.R")

for (script in r_scripts) {
  cat("\n正在执行:", script, "\n")
  source(script)
}

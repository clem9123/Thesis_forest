library(parallel)

# Set working directory where capsis is run from
setwd("C:/Capsis4")

# Number of cores to use
n_cores <- max(1, detectCores() - 2)

# Path where the output folders are located
output_base_path <- "data/forceps/clementine/Test_protocole"

# Delete old output directories if they exist
for (i in 1:10) {
  dir_to_delete <- file.path(output_base_path, sprintf("output-cmd_%d.txt", i))
  if (dir.exists(dir_to_delete)) {
    cat("Deleting:", dir_to_delete, "\n")
    unlink(dir_to_delete, recursive = TRUE, force = TRUE)
  }
}

# Build command list
cmds <- sprintf(
  'capsis -p script forceps.myscripts.brieuc.SimulationBrieucManagement data/forceps/clementine/Test_protocole/cmd_%d.txt',
  1:10
)

# Create parallel cluster
cl <- makeCluster(n_cores)

# Run commands in parallel
parLapply(cl, cmds, function(cmd) {
  cat("Running:", cmd, "\n")
  system(cmd, wait = TRUE)
})

# Stop the cluster
stopCluster(cl)


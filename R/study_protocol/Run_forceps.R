library(parallel)

# Base path to FORCEEPS working directory
forceeps_path = "C:/Capsis4/data/forceps/clementine/"
analyse_name = "Study_protocol"
base_path = paste0(forceeps_path, analyse_name, "/")
output_base_path <- paste0("data/forceps/clementine/", analyse_name, "/")

# Set working directory where capsis is run from
setwd("C:/Capsis4")

# Number of cores to use
n_cores <- max(1, detectCores() - 2)

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
  'capsis -p script forceps.myscripts.brieuc.SimulationBrieucManagement %s',
  file.path(output_base_path, sprintf("cmd_%d.txt", 1:10))
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


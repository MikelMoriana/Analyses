# This is a helper script to run the pipeline.

targets::tar_visnetwork(script = "_targets.R")
targets::tar_make(script = "_targets.R")

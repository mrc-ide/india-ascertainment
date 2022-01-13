
## -----------------------------------------------------------------------------
## A. BUNDLE CREATION
## -----------------------------------------------------------------------------

# how long is the mcmc for
n_mcmc <- 50000
date <- "2022-01-01"

# should tasks be run in parallel and use multiple chains.
# leave this as FALSE and see FAQs for more info on this
model <- "NIMUE"
replicates <- 10
rf <- 1

# get the states
states <- c(
  "Andaman and Nicobar Islands","Andhra Pradesh","Arunachal Pradesh","Assam",
  "Bihar","Chandigarh","Chhattisgarh","Dadra and Nagar Haveli and Daman and Diu",
  "Delhi","Goa","Gujarat","Haryana","Himachal Pradesh","Jammu and Kashmir",
  "Jharkhand","Karnataka","Kerala","Ladakh","Lakshadweep","Madhya Pradesh",
  "Maharashtra","Manipur","Meghalaya","Mizoram","Nagaland","Odisha","Puducherry",
  "Punjab","Rajasthan","Sikkim","Tamil Nadu","Telangana","Tripura","Uttar Pradesh",
  "Uttarakhand","West Bengal"
)

# varying between high and low seroreversion
# https://www.science.org/doi/10.1126/science.abj9932
# 50%, 30%, 10%
# 1/((60*(1/365) - log(0.70))/60)
dur_R <- c(70, 115, 222)

# increased probability of hospitalisation due to Delta
# 1·45 [1·08–1·95] https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(21)00475-8/fulltext
prob_hosp_multiplier <- c(1.95, 1.45, 1.08)

# Hospital capacity impact on IFR.
# 0.5 = 50% beds are available
# 1 = 100% beds available
# 10 will mean no bed limits reached essentially.
# no limits, multiplier = 10, default = 1, worst = 0.5
hosp_capacity_multiplier <- c(1, 1, 1)

# make the orderly bundles to be run on the cluster
path_bundles <- c(
  cp_path("analysis/orderly_bundles/raw_vaccine_final_worst"),
  cp_path("analysis/orderly_bundles/raw_vaccine_final_central"),
  cp_path("analysis/orderly_bundles/raw_vaccine_final_best")
)

for(i in 1:3) {

  dir.create(path_bundles[i], showWarnings = FALSE)

  # bundle these up - this will take like 2 mins to create all the zips.
  bundles <- lapply(
    states, function(x) {
      orderly::orderly_bundle_pack(
        path = path_bundles[i],
        name = "india_sub_national",
        parameters = list(
          state = x,
          date=date,
          rf=rf,
          replicates=replicates,
          dur_R=dur_R[i],
          model=model,
          prob_hosp_multiplier=prob_hosp_multiplier[i],
          n_mcmc=n_mcmc
        )
      )
    }
  )

  # now label these with the states and save the file paths
  names(bundles) <- states
  saveRDS(bundles, file.path(path_bundles[i], "bundles.rds"))

}

## -----------------------------------------------------------------------------
## B. CLUSTER SUBMITTING
## -----------------------------------------------------------------------------

# Setting Up Cluster From New

# Log in to didehpc
credentials = "C:/Users/ow813/.smbcredentials"
options(didehpc.cluster = "fi--didemrchnb",
        didehpc.username = "ow813")

## ------------------------------------
## 1. Package Installations
## ------------------------------------
# drat:::add("mrc-ide")
# install.packages("pkgdepends")
# install.packages("didehpc")
# install.packages("orderly")

## ------------------------------------
## 2. Setting up a cluster configuration
## ------------------------------------

options(didehpc.cluster = "fi--didemrchnb")

# not if T is not mapped then map network drive
didehpc::didehpc_config_global(temp=didehpc::path_mapping("tmp",
                                                          "T:",
                                                          "//fi--didef3.dide.ic.ac.uk/tmp",
                                                          "T:"),
                               home=didehpc::path_mapping("OJ",
                                                          "L:",
                                                          "//fi--didenas5/malaria",
                                                          "L:"),
                               credentials=credentials,
                               cluster = "fi--didemrchnb")

# Creating a Context
context_name <- cp_path("context")

ctx <- context::context_save(
  path = context_name,
  package_sources = conan::conan_sources(
    packages = c(
      "vimc/orderly", "mrc-ide/squire@v0.6.10", "mrc-ide/nimue",
      c('tinytex','knitr', 'tidyr', 'ggplot2', 'ggrepel', 'magrittr', 'dplyr', 'here', "lubridate", "rmarkdown",
        'stringdist','plotly', 'rvest', 'xml2', 'ggforce', 'countrycode', 'cowplot', 'RhpcBLASctl', 'nimue')
    ),
    repos = "https://ncov-ic.github.io/drat/")
)

# set up a specific config for here as we need to specify the large RAM nodes
config <- didehpc::didehpc_config(template = "24Core")
config$resource$parallel <- "FALSE"
config$resource$type <- "Cores"

# Configure the Queue
obj <- didehpc::queue_didehpc(ctx, config = config)

## ------------------------------------
## 3. Submit the jobs
## ------------------------------------

submit_jobs <- function(path_bundles) {

  workdir <- gsub("raw", "derived", path_bundles)
  dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
  workdir <- normalizePath(workdir)

  # Grabbing tasks to be run
  tasks <- readRDS(gsub("derived", "raw", file.path(workdir, "bundles.rds")))
  tasks <- as.character(vapply(tasks, "[[", character(1), "path"))

  # submit our tasks to the cluster
  split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
  bundle_name <- paste0(tail(rev(split_path(workdir)), 2), collapse = "_")
  grp <- obj$lapply(tasks, orderly::orderly_bundle_run, workdir = workdir,
                    name = bundle_name, overwrite = TRUE)

  return(grp)

}

grp_worst <- submit_jobs(path_bundles[1])
grp_central <- submit_jobs(path_bundles[2])
grp_best <- submit_jobs(path_bundles[3])

## ------------------------------------
## 4. Check on our jobs
## ------------------------------------

# check on their status
status <- grp$status()
table(status)

# see what has errorred
errs <- lapply(seq_along(which(status == "ERROR")), function(x){
  grp$tasks[[which(status == "ERROR")[x]]]$log()$body
})

# sometimes tasks say running or completed when in fact they have errored:
didehpc:::reconcile(obj, grp$ids)
status <- grp$status()
errs <- lapply(seq_along(which(status == "ERROR")), function(x){
  grp$tasks[[which(status == "ERROR")[x]]]$log()$body[[19]]
})

# do we just need to rerun some of the bundles
to_rerun <- which(grp$status() == "ERROR")
unlink(gsub("\\.zip", "", gsub("raw", "derived", grp$X[to_rerun])), recursive = TRUE)
obj$submit(grp$ids[to_rerun])

## ------------------------------
## 5. Functions to extract objects from zips for checking
## ------------------------------

# use the bundles paths to work out the path to the runs in derived

make_fitting_pdf <- function(grp_grab) {

  paths <- gsub("raw", "derived", grp_grab$X[grp_grab$status() == "COMPLETE"])

  # now extract the fitting.pdf files
  td <- file.path(tempdir(), grp_grab$name, uuid::UUIDgenerate())
  dir.create(td, showWarnings = FALSE, recursive = TRUE)

  fits <- lapply(paths, function(x) {
    if(file.exists(x)){
      zip::unzip(
        zipfile = x,
        files = file.path(gsub("\\.zip", "", basename(x)), "pack/fitting.pdf"),
        exdir = td
      )
    }
  })

  # get the filepaths for these
  pdfs <- grep("fitting", list.files(td, full.names = TRUE, recursive = TRUE), value = TRUE)

  # combine the files that are larger than 0b. Ob files are for countries that have
  # no COVID-19 deaths to date and as such don't have a fitting.pdf but this file is
  # created because it needs to be for orderly to finish the task
  qpdf::pdf_combine(
    input = pdfs[file.size(pdfs) > 0],
    output = cp_path(paste0("analysis/plots/fitting_vacc_", grp_grab$name,".pdf"))
  )

}

make_fitting_pdf(grp_best)
make_fitting_pdf(grp_central)
make_fitting_pdf(grp_worst)

## ------------------------------
## 6. Import Suitable Fits
## ------------------------------

out <- lapply(gsub("raw", "derived", grp_best$X), orderly::orderly_bundle_import)
out <- lapply(gsub("raw", "derived", grp_central$X), orderly::orderly_bundle_import)
out <- lapply(gsub("raw", "derived", grp_worst$X), orderly::orderly_bundle_import)

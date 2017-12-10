cat('\n\n');timestamp();cat('\n')
library(btergm)
library(parallel)
library(texreg)

data_dir <- '/home/sdowning/data/firm_nets_cem'
result_dir <- '/home/sdowning/compnet/results'

####################### DEFINE MODELS ###################################

m4 <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel") +
  nodecov("cent_pow_n0_5") + absdiff("cent_pow_n0_5") + 
  cycle(3) + cycle(4) + cycle(5) 

m3 <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) + 
  cycle(3) + cycle(4) + cycle(5) 

m2 <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) + 
  nodecov("cent_pow_n0_5") + absdiff("cent_pow_n0_5") 

m1 <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel")

m0 <-   nets ~ edges + gwesp(0, fixed = T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  edgecov(mmc) +
  memory(type = "stability", lag = 1) 
################################ end models#######################


files <- dir(path = data_dir, pattern = ".+_d3\\.rds$")
files.firms <- unname(sapply(files, function(firm) strsplit(firm, split = "_")[[1]][1]))

finished <- dir(path = result_dir, pattern = ".+_tergm_results_pd\\d+_d3_R\\d+_m4\\.")
finished.firms <- unname(sapply(finished, function(firm) strsplit(firm, split = "_")[[1]][1]))
cat(sprintf("\nalready finished (%s): %s", length(finished.firms), paste(finished.firms, collapse = "|")))

todo.firms <- files.firms[which(!(files.firms %in% finished.firms))]
cat(sprintf("\nremaining (%s): %s", length(todo.firms), paste(todo.firms, collapse = "|")))

files <- todo.firms[sapply(todo.firms, function(i) {
  return(!any(grepl(i, finished.firms, ignore.case = T)))
})]


for (i in 1:length(files)) {
  
  firm_i <- strsplit(files[i] , split = "_")[[1]][1]
  d <- 3
  
  data_file <- file.path(data_dir,files[i])
  nets <- readRDS(data_file)
  
  nPeriods <- 7  ## 5
  net_group <- 'cem'
  
  if (!("fits" %in% ls())) fits <- list()
  if (!(net_group %in% names(fits))) fits[[net_group]] <- list()
  if (!(firm_i %in% names(fits[[net_group]])) ) fits[[net_group]][[firm_i]] <- list()
  if (nPeriods < length(nets))   nets <- nets[(length(nets)-nPeriods+1):length(nets)] 
  
  cat("\n------------ estimating TERGM for:",firm_i,'--------------\n')
  cat(sprintf("Using %s cores", detectCores()))
  
  ## make MMC nets list
  mmc <- lapply(nets, function(net) as.matrix(net %n% 'mmc'))
  
  ##
  # DEFINE MODEL and MODEL NAME TO COMPUTE
  ## 
  mod <- m4
  m_x <- 'm4'
  ##
  # SET RESAMPLES
  ##
  R <- 1000
  
  
  ## RUN TERGM
  # fit <- btergm(mod, R=R, parallel = "multicore", ncpus = detectCores())
  fit <- tryCatch(
    btergm(mod, R=R, parallel = "multicore", ncpus = detectCores())
    , error = function(e)e
  )
  
  if (!inherits(fit, "error")) {
    ## SAVE SERIALIZED
    fits.file <- sprintf('/home/sdowning/compnet/results/fit_%s_pd%s_d%s_R%s_%s.rds', firm_i, nPeriods, d, R, m_x)
    saveRDS(fit, file=fits.file)
    
    ## SAVE FORMATTED REGRESSION TABLE
    html.file <- sprintf('/home/sdowning/compnet/results/%s_tergm_results_pd%s_d%s_R%s_%s.html',  firm_i, nPeriods, d, R, m_x)
    htmlreg(fit, digits = 3, file=html.file)
    
  } else {
    cat(sprintf("\nfirm %s error msg: %s\n", firm_i, fit))
  }

}



cat('finished successfully.')


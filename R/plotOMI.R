#' Calculate OMI for species
#'
#' Function to calculate the OMI of multiple species using certain climate data
#'
#' @param clim climate data
#' @param spec species data
#' @param filename filename of output
#' @return OMI
#' @examples
#' 
#' @export
plotOMI <- function(clim, spec, filename){
  
  # read climate file for niche analysis
  #clim <- read.delim("clim_cur_pollgrid.txt")
  
  # define vector with row numbers of NA values in climate data
  clim_NA <- which(is.na(clim$bio01))
  
  # in case there are NA values in climate data, remove these rows
  if(length(clim_NA) > 0) clim <- clim[-clim_NA,]
  
  ### MODIFY START #############################################################
  
  # read species occurrence data (dataframe with presences and absences; for 
  # details, check file structure in "birds_pollgrid.txt")
  
  #spec <- read.delim("birds_pollgrid.txt")
  #names(spec)
  
  # define columns with id and with species data (column numbers in spec)
  col_id_spec <- c(1,4:length(spec)) #here: 1 = id column, 5-7: species columns 
  
  # define id column names to merge climate and species file
  id_clim <- "CGRSNAME"
  id_spec <- "CGRSNAME" 
  
  ### MODIFY STOP ##############################################################
  
  #reduce spec file to id and species data
  spec_raw <- spec[,col_id_spec]
  
  #replace NAs in occurrence file by 0 (the locations all NA cells have been 
  #checked, and there is no reason to assume that any species is actually 
  #occurring there, therefore, all NAs can be replaced by 0
  spec_raw[is.na(spec_raw)] <- 0 
  
  #names(spec_raw)
  
  # merge spec and clim values in one dataframe
  clim_spec <- merge(clim, spec_raw, by.x = id_clim, by.y = id_spec)
  #names(clim_spec)
  
  # define final climate and species dataframes for niche analyses
  clim_niche <- clim_spec[,2:5]
  spec_niche <- clim_spec[,6:length(clim_spec)]
  
  spec_niche[is.na(spec_niche)] <- 0
  
  #--------------analyses-------------------------------------------------------
  
  # run PCA of climatic variables, keep 3 axes
  pca_clim <- dudi.pca(clim_niche, scale=TRUE, scannf=F, nf=3)
  
  # OMI analyses
  niches <- niche(pca_clim, spec_niche, scannf=F, nf=999)
  
  # niche parameters 
  niche_param <- niche.param(niches)
  
  #CHANGE: we don't need the lengthy rtest analyses
  
  # significance values
  #niche_rtest <- rtest(niches, nrepet=999)
  
  # niche breadth and position along first and second OMI axis
  omi1 <- sco.distri(niches$ls[,1], spec_niche)
  omi2 <- sco.distri(niches$ls[,2], spec_niche)
  
  # build dataframe with niche results (for species)
  # dataframe with niche positions and breadths
  niche_pos_breadth <- data.frame(omi1, omi2)
  names(niche_pos_breadth) <- c("omi1_mean", "omi1_var", "omi2_mean", "omi2_var")
  
  
  ### CHANGE: remove parts with rtest results (remove from analyses with "#")
  
  # dataframe with rtest values
  #niche_rtest_results <- data.frame(niche_rtest$obs, niche_rtest$alter,
  #                                  niche_rtest$expvar, niche_rtest$pvalue)
  #names(niche_rtest_results)<- c("obs", "alter", "std_obs", "expectation", 
  #                               "variance", "p")
  
  # join different result dataframes
  #niche_results <- data.frame(niche_pos_breadth, niche_param,
  #                    niche_rtest_results[-length(niche_rtest_results[,1]),])
  ### CHANGE: define new "niche_results" object
  niche_results <- data.frame(niche_pos_breadth, niche_param)
  
  
  #---NEW: Hypervolume analyses-------------------------------------------------
  
  #dataframe with CGRS names and lat-lon coordinates
  geo <- spec[,1:3]
  
  #add lat-lon coordinates to climate-species file
  clim_spec_xy <- merge(clim_spec, geo)
  #names(clim_spec_xy)
  
  #standardize climatic variables for hypervolume calculation
  clim_spec_xy$bio01s <- (clim_spec_xy$bio01 - 
                            mean(clim_spec_xy$bio01)) / sd(clim_spec_xy$bio01)
  clim_spec_xy$bio07s <- (clim_spec_xy$bio07 - 
                            mean(clim_spec_xy$bio07)) / sd(clim_spec_xy$bio07)
  clim_spec_xy$bio12s <- (clim_spec_xy$bio12 - 
                            mean(clim_spec_xy$bio12)) / sd(clim_spec_xy$bio12)
  clim_spec_xy$bio15s <- (clim_spec_xy$bio15 - 
                            mean(clim_spec_xy$bio15)) / sd(clim_spec_xy$bio15)
  
  #define dataframe of climate data for hypervolume calculation
  clim_hyp <- clim_spec_xy[,c("bio01s", "bio07s", "bio12s", "bio15s")]
  
  #create vector with species names
  species <- names(clim_spec_xy)[grep("_", names(clim_spec_xy))]
  
  #create new column for hypervolume value
  niche_results$hypvol <- NA
  
  #loop to calculate hypervolumes for each species
  for(i in 1:length(species))
  {
    #i <- 1
    sp_i <- species[i]
    occ_i <- which(clim_spec_xy[,sp_i] == 1)
    clim_hyp_i <- clim_hyp[occ_i,]
    hv_i <- hypervolume(clim_hyp_i, quantile=0.0, reps=1000,
                        bandwidth=estimate_bandwidth(clim_hyp_i), name=sp_i)
    vol_i <- get_volume(hv_i)
    niche_results[sp_i,"hypvol"] <- vol_i
  }
  
  
  #--------------save results---------------------------------------------------
  
  ### CHANGE: change output folder to "niche_REV1"
  niche_results_folder <- paste(workfolder, "/results/niche_REV1/", sep="")
  
  # set niche results folder as working directory
  setwd(niche_results_folder)
  
  # save pca summary
  capture.output(summary(pca_clim), file = "pca_clim_summary.txt")
  
  # correlations of climate variables with PCA axes
  capture.output(cor(pca_clim$li, clim_niche), file = "cor_clim_pca.txt")
  
  # save results of niche (OMI) analyses
  capture.output(summary(niches), file = "omi_summary.txt")
  capture.output(niches$tab, file="omi_tab.txt")
  capture.output(niches$li, file="omi_li.txt")
  capture.output(niches$l1, file="omi_l1.txt")
  capture.output(niches$co, file="omi_co.txt")
  capture.output(niches$c1, file="omi_c1.txt")
  capture.output(niches$ls, file="omi_ls.txt")
  capture.output(niches$as, file="omi_as.txt")
  
  write.table(niche_results, "niche_results.txt", sep="\t", quote=F)
  
  # correlations of climate variables with OMI axes
  capture.output(cor(niches$ls, clim_niche), file = "cor_clim_omi.txt")
  
  #--------------plot results---------------------------------------------------
  
  # plot PCA
  pdf("pca_clim.pdf")
  s.arrow(pca_clim$c1[,1:2], boxes=F)
  s.arrow(pca_clim$c1[,2:3], boxes=F)
  dev.off()
  
  # plot niche breadths and positions along OMI axes
  pdf("breadth_pos_omi.pdf")
  sco.distri(niches$ls[,1], spec_niche)
  sco.distri(niches$ls[,2], spec_niche)
  dev.off()
  
  # save plot with niches of all species in 2-dimensional omi space
  pdf("omi_all.pdf")
  # plot 2-dimensional OMI space
  plot(niches$ls[,1], niches$ls[,2], pch=16, col="grey", xlab="OMI 1", 
       ylab="OMI 2", cex.lab=1.3, las=1, 
       xlim=c(floor(min(niches$ls[,1])), ceiling(max(niches$ls[,1]))),
       ylim=c(floor(min(niches$ls[,2])), ceiling(max(niches$ls[,2]))))
  abline(h=0, lty=2)
  abline(v=0, lty=2)
  # add niche positions and breadths of all species in dataset
  for(i in 1:length(niche_pos_breadth[,1]))
  {
    plotOMI(niche_pos_breadth, species=row.names(niche_pos_breadth)[i], 
            labels=FALSE, pointtype=18, pointsize=0.8)
  }
  dev.off()
  
  #plotOMI: function to plot niche positions and breadths in 2-dimensional 
  #space of 1st and 2nd OMI axes
  plotOMI <- function(data, species, pointtype=18, pointsize=1.5, 
                      color="red", labels=TRUE, textsize=0.6)
    #data: data frame of niche breadths and positions along first and second
    #OMI axis
    #species: species to be plotted
  {
    #add points of niche position to plot
    points(data[species,"omi1_mean"], data[species,"omi2_mean"], 
           pch=pointtype, cex=pointsize, col=color)
    
    #add niche breadths as error bars
    segments(data[species,"omi1_mean"], 
             data[species,"omi2_mean"]-data[species,"omi2_var"], 
             data[species,"omi1_mean"],
             data[species,"omi2_mean"]+data[species,"omi2_var"], col=color)
    segments(data[species,"omi1_mean"]-data[species,"omi1_var"], 
             data[species,"omi2_mean"],
             data[species,"omi1_mean"]+data[species,"omi1_var"],
             data[species,"omi2_mean"], col=color)  
    if(labels == TRUE)
    {
      #add species name to plot
      text(data[species,"omi1_mean"]+0.1, data[species,"omi2_mean"]-0.2, 
           species, adj=c(0,0), cex=textsize, font=3)
    }    
  }
}

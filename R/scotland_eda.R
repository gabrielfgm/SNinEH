######################
# Load Scotland Data #
######################

library(Hmisc)
library(tidyverse)
library(igraph)

sc <- mdb.get("../data/Scotland.mdb")

# Break database into constituent dataframes
list2env(sc, envir = globalenv())

head(Affiliations)

c_d <- as.matrix(table(Affiliations$CompId, Affiliations$DirId))
dim(c_d)

c_c <- c_d %*% t(c_d)
dim(c_c)

cg <- graph_from_adjacency_matrix(c_c, mode = 'directed', diag = F)

# Read in company attribute data
imm <- readxl::read_xlsx("../data/data_imm.xlsx")
head(imm)


##################################
# Estimations of Network effects #
##################################

# First add in unconnected nodes
cons <- get.edgelist(cg)
cg <- graph_from_data_frame(cons, directed = FALSE, vertices = imm$compid)
cg <- simplify(cg)

# Now extract adjacency matrix
adjmat <- get.adjacency(cg, sparse = F)

# Now do eigen cent first
#imm <- imm[imm$compid %in% names(V(cg)),]
imm$eig_cent <- eigen_centrality(cg)$vector
imm$degree <- degree(cg)
imm$betweenness <- betweenness(cg)

# Now autoregression
library(sphet)
library(spdep)
library(texreg)

###################################################
# Comparing centrality measures vs autoregression #
###################################################

source("extract_helper_functions.R")

run_regs <- function(formula, depvar, type = "stsls", adjmat) {

  # get data
  dat <- na.omit(imm[, all.vars(formula)])
  
  # get indices of full obs
  ind <- rowSums(is.na(imm[, all.vars(formula)])) == 0
  
  # build weight matrix
  W <- mat2listw(adjmat[ind, ind], style = "W")
  
  if (type == "stsls") {
    sts <- stsls(formula, data = dat, listw = W, zero.policy = TRUE)
    sts$rsquared <- 1 - sts$sse/(nrow(dat) * var(dat[, all.vars(formula)[1]]))
  } else if (type == "gstsls") {
    sts <- gstslshet(formula, data = dat, listw = W, zero.policy = TRUE)
  } else if (type == "sacsarlm") {
    sts <- sacsarlm(formula, data = dat, listw = W, Durbin = TRUE, zero.policy = TRUE)
  } else if (type == "lagsarlm") {
    sts <- lagsarlm(formula, data = dat, listw = W, Durbin = TRUE, zero.policy = TRUE)
  } else {
    cat("Unknown regression type!")
  }
  
  sts
}

imm$railways <- as.numeric(imm$indid == 5)
imm$insurance <- as.numeric(imm$indid == 23)
imm$investment <- as.numeric(imm$indid == 24)
imm$banking <- as.numeric(imm$indid == 22)

lm2 <- lm(avg_div ~ degree + betweenness + eig_cent + capital + qualifications  + uncalled, data = imm)
sts2 <- run_regs(avg_div ~ capital + qualifications  + uncalled, imm$avg_div, type = "stsls", adjmat = adjmat)
gsts2 <- run_regs(avg_div ~ capital + qualifications  + uncalled, imm$avg_div, type = "gstsls", adjmat = adjmat)
sacsarlm2 <- run_regs(avg_div ~ capital + qualifications  + uncalled, imm$avg_div, type = "sacsarlm", adjmat = adjmat)
lm3 <- lm(avg_div ~ degree + betweenness + eig_cent + capital + qualifications + uncalled + railways + insurance + investment + banking, 
               data = imm[!is.na(imm$avg_div), ])
sts3 <- run_regs(avg_div ~ capital + qualifications  + uncalled + 
                   railways + insurance + investment + banking, imm$avg_div, type = "stsls", adjmat = adjmat)
gsts3 <- run_regs(avg_div ~ capital + qualifications  + uncalled + 
                    railways + insurance + investment + banking, imm$avg_div, type = "gstsls", adjmat = adjmat)
sacsarlm3 <- run_regs(avg_div ~ capital + qualifications  + uncalled + 
                        railways + insurance + investment + banking, imm$avg_div, type = "sacsarlm", adjmat = adjmat)


texreg(list(lm2, extract.stsls(sts2), extract.gstslshet(gsts2), sacsarlm2,
            lm3, extract.stsls(sts3), extract.gstslshet(gsts3), sacsarlm3), 
       custom.model.names = c("OLS", "STSLS", "GSTSLS", "SAC/SARAR", "OLS", "STSLS", "GSTSLS", "SAC/SARAR"),
       custom.coef.names = c(NA, NA, NA, "eigen", rep(NA, 3), "$\\rho$", "$\\rho$", "$\\lambda$", rep(NA, 13)),
       reorder.coef = c(2:4,8:9,5:7,10:20,1),
       caption = "Regression of average dividend yield on firm and network characteristics.
       Regressions are specified as follows: \\\\
       OLS w/ Cent.: $y = \\alpha + \\delta \\text{Cent.} + X \\beta + \\epsilon$ \\\\
       STSLS: $y = \\alpha + \\rho W y + X\\beta + \\epsilon$ \\\\
       GSTSLS: $y = \\alpha + \\rho W y + X\\beta + u$, $u = \\lambda W u + \\epsilon$ \\\\
       SAC/SARAR: $y = \\alpha + \\rho W y + X\\beta + W X \\phi + u$, $u = \\lambda W u + \\epsilon$",
       label = "tab:widereg", include.adjrs = F, include.rmse = F, include.aic = F,
       include.lr = F, file = "../tables/widereg2.tex",
       fontsize = "scriptsize",
       )


###########################
# Visualizing the Network #
###########################
# Set attributes
vid <- V(cg)$name
V(cg)$name <- as.character(Company$Company[as.character(Company$CompId) %in% vid])
V(cg)$ind <- Company$IndID[as.character(Company$CompId) %in% vid]
V(cg)$cap <- as.numeric(Company$Capital[as.character(Company$CompId) %in% vid])
V(cg)$prof <- as.numeric(imm$avg_div[as.character(imm$compid) %in% vid])

library(GGally)
library(intergraph)
library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(8, 'Set1')(19))
# use industrial categories
indcat <- data.frame(IndID = V(cg)$ind)
indcat <- left_join(indcat, Industry)

cat_cat <- c("Mining", "Heavy Man.", "Railways", "Energy", "Light Man.", "Banking", "Insurance", "Inv. Trusts and Property")
indcat$IndCat <- factor(indcat$IndCat)
levels(indcat$IndCat) <- cat_cat

V(cg)$indcat <- as.character(indcat$IndCat)

ggnet2(simplify(cg), mode = 'fruchtermanreingold', layout.exp = .3,
       size = "prof", size.legend = "Average Dividend Yield (%)", size.cut = 5, max_size = 10,
       color = "indcat", 
       fill = "indcat",
       palette = "Greys", 
       color.legend = "Industry Category", 
       na.rm = "prof"
       ) +
  geom_point(aes(size = size), shape = 21, color = "black") + 
  guides(color = guide_legend(override.aes = 
                                list(shape=21, 
                                     size=10, 
                                     fill=brewer.pal(8, "Greys"), 
                                     color = "black")))

ggsave(filename = "../figures/scottish_net_vis.jpg", scale = 1.5)

#########################
# Network Summary Stats #
#########################

# density
edge_density(cg)

# diameter
diameter(cg)

# shortest paths
dmat <- shortest.paths(cg) # unreachable nodes are Inf
dmat <- dmat[lower.tri(dmat)]
dmat <- dmat[is.finite(dmat)]

# Mean shortest path
mean(dmat)
# Shortest path standard deviation
sd(dmat)
# Max shortest path (same as diameter)
max(dmat)

# Degrees
degrees <- degree(cg)
# Mean degree
mean(degrees)

degree.df <- data.frame(degree = degrees, names = V(cg)$name)

degree.df %>% group_by(degree) %>% summarise(N = n(), Freq. = N/nrow(degree.df)) %>%
  ungroup() %>% mutate(`Percent Cum.` = cumsum(Freq.)*100)

# Top ten comps by degree
degree.df %>% arrange(-degree) %>% head(10L)

degree.df %>% mutate(upper.20 = degree >= quantile(degree, .8),
                     lower.20 = degree <= quantile(degree, .2)) %>%
  summarise(`Perc. Upper Quantile` = sum(degree[upper.20]/sum(degree)),
            `Perc. Lower Quantile` = sum(degree[lower.20]/sum(degree)))

#######################################################
# Estimations of Network effects w/ Measurement Error #
#######################################################

cg2 <- cg %>% delete.edges(E(cg)[from(V(cg)[V(cg)$indcat=="Banking"])])

# Now extract adjacency matrix
adjmat_nb <- get.adjacency(cg2, sparse = F)

# Now do eigen cent first
#imm <- imm[imm$compid %in% names(V(cg)),]
imm$eig_cent_nb <- eigen_centrality(cg2)$vector
imm$degree_nb <- degree(cg2)
imm$betweenness_nb <- betweenness(cg2)

########################################################################
# Comparing centrality measures vs autoregression w/ Measurement Error #
########################################################################

# Make wide table
lm2_nb <- lm(avg_div ~ degree_nb + betweenness_nb + eig_cent_nb + capital + qualifications  + uncalled, data = imm)
sts2_nb <- run_regs(avg_div ~ capital + qualifications  + uncalled, imm$avg_div, type = "stsls", adjmat = adjmat_nb)
gsts2_nb <- run_regs(avg_div ~ capital + qualifications  + uncalled, imm$avg_div, type = "gstsls", adjmat = adjmat_nb)
sacsarlm2_nb <- run_regs(avg_div ~ capital + qualifications  + uncalled, imm$avg_div, type = "sacsarlm", adjmat = adjmat_nb)

texreg(list(lm2_nb, extract.stsls(sts2_nb), extract.gstslshet(gsts2_nb), sacsarlm2_nb), 
       custom.model.names = c("OLS", "STSLS", "GSTSLS", "SAC/SARAR"),
       custom.coef.names = c(NA, "degree$_{nb}$", "betweenness$_{nb}$", "eigen$_{nb}$", 
                             rep(NA, 3), "$\\rho$", "$\\rho$", "$\\lambda$", rep(NA, 5)),
       reorder.coef = c(2:4,8:9,5:7,10:12,1),
       include.adjrs = F, include.rmse = F, include.aic = F,
       include.lr = F, file = "../tables/misspecreg.tex",
       caption = "Network regressions on a mis-measured network.",
       label = "tab:misspec",
       fontsize = "scriptsize")



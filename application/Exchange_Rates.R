# BoE_website: https://www.bankofengland.co.uk/boeapps/database/Rates.asp?Travel=NIxASx&into=GBP

## Fetch data from the Bank of England website
# 
#library(pdfetch)
#BoE_exchange_rates <- as.matrix(pdfetch_BOE(identifiers = c("XUDLADS","XUDLCDS","XUDLBK89", "XUDLBK25", "XUDLDKS","XUDLERS","XUDLHDS","XUDLBK33","XUDLBK97","XUDLBK78","XUDLJYS","XUDLBK83","XUDLNDS","XUDLNKS","XUDLBK47","XUDLBK85","XUDLSRS","XUDLSGS","XUDLZRS","XUDLBK93","XUDLSKS","XUDLSFS","XUDLTWS","XUDLBK87","XUDLBK95","XUDLUSS"), 
#                                            from =  "2005-10-01", to = "2020-09-30"))
#colnames(BoE_exchange_rates) <- ctry_codes

# save(BoE_exchange_rates, file = "~/Dropbox/structurelearning/code/Application/Exchange_Data/BoE_exchange_rates.RData")

load(file = "data/BoE_exchange_rates.RData")
data <- BoE_exchange_rates
ctry_codes <- c("AUS", "CAN", "CHN","CZE","DNK","EUR","HKG","HUN","IND","ISR","JPN","MYS","NZL","NOR","POL","RUS","SAU","SGP","ZAF","KOR","SWE","CHE","TWN","THA","TUR","USA")

library("igraph")
library("graphicalExtremes")
library("timeSeries")
library("fGarch")
library("tidyverse")

coords_tree <- read_rds("data/coords_exchange_rates.rds")

# create negative log ratios
nlr <-  apply(data, 2, FUN = function(x) diff(log(x))) #ndifflog

d <-  dim(nlr)[2]
n <-  dim(nlr)[1]

x <- nlr
fit.fGarch <- list()
residus <-  matrix(NA,n,d)
colnames(residus) <-  colnames(nlr)

for(i in 1:d){
  ga <- NULL
  form <- paste("~arma(0,2)+garch(1,1)")
  ga <- garchFit(formula=as.formula(form), data=x[,i], trace=FALSE, cond.dist="norm")
  residus[,i] <- ga@residuals/ga@sigma.t
  fit.fGarch[[i]] <- ga
  cat("Done for ",i,"/",d,"\n")
}


colnames(residus) <-  colnames(nlr)
abs.residus <- abs(residus)
data <- abs.residus 
graph.full <- make_full_graph(d)



p <- 0.95 # their value
G.est = emp_vario(data=data, p = p)
colnames(G.est) <- rownames(G.est) <- ctry_codes
MST.est <- igraph::mst(graph=graph.full, weights = 2- Gamma2chi(G.est[ends(graph.full,E(graph.full))]), algorithm = "prim")
vertex_attr(MST.est) <- list(name = colnames(data))
MST.est <- graphicalExtremes:::set_graph_parameters(MST.est)
E(MST.est)$width <- Gamma2chi(G.est[ends(MST.est,E(MST.est), names=FALSE)] ) * 10
igraph::V(MST.est)$color <- grDevices::adjustcolor(col = "#AAC1D8")
igraph::V(MST.est)$size <- 13

# pdf(file = paste("Exchange_rate_Tree.pdf", sep=""), width = 7)
# par(mar=c(4,4,1.6,1.5), mgp=c(2,0.6,0), pty="s", cex.lab=1.6, cex.axis = 1.3, cex.main=2, pch=1, cex=.9, lwd=1)
plot(MST.est, layout=coords_tree)
# dev.off()




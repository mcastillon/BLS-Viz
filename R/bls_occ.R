setwd("C:/Users/mcast_000/Documents/GitHub/BLS Viz")

options(stringsAsFactors = F)

library(data.table)

#### BLS dataset
bls_occ <- fread("Data/bls_occ.csv")
colnames(bls_occ)

library(plyr)
library(dplyr)

bls_occ <- data.table(bls_occ)

bls_occ

### area_type==4 gives us metro areas

bls_occ_msa <- filter(bls_occ, area_type==4)
bls_occ_msa[, occ_cat := substr(occ_code, 1, 2)]
setkey(bls_occ_msa, occ_cat)

count(bls_occ_msa, "group")

occ_groups <- bls_occ_msa %>%
			filter(group == "major") %>%
				select(occ_title, occ_cat) %>%
					unique() %>%
						setnames("occ_title", "major_occ")
setkey(occ_groups, occ_cat)

bls_occ_msa <- bls_occ_msa[occ_groups]
bls_occ_msa <- filter(bls_occ_msa, group == "detail")

bls_occ_msa

bls_occ_msa <- select(bls_occ_msa,
			area_title, occ_code, occ_title, tot_emp, jobs_1000,
			loc_quotient, starts_with("h_"), starts_with("a_"),
			major_occ)
#### a lot of variables have nonsense entries, change to NA

bls_occ_msa <- bls_occ_msa %>%
			mutate_each_(funs(as.numeric), c("tot_emp", "jobs_1000",
			"loc_quotient"))
ah_vars <- colnames(select(bls_occ_msa, starts_with("h_"), starts_with("a_")))
bls_occ_msa <- bls_occ_msa %>%
			mutate_each_q(funs(as.numeric), ah_vars)
#write.csv(bls_occ_msa, "Data/bls_occ_msa.csv", row.names=F)

library(reshape2)

occ_rs <- recast(bls_occ_msa, id.var=c("area_title", "occ_title"),
                 measure.var="tot_emp", fun.aggregate=sum,
                 fill=0, area_title ~ occ_title)
occ_rs[is.na(occ_rs)] <- 0
occ_rs <- data.table(occ_rs)
occ_rs[, tot_emp := rowSums(select(occ_rs, -area_title))]
### select top 100 in terms of total employment
occ_msa_top100 <- top_n(occ_rs, 100)
occ_msa_top100_scale <- select(occ_msa_top100, -area_title, -tot_emp) / occ_msa_top100$tot_emp

rownames(occ_msa_top100_scale) <- occ_msa_top100$area_title

dis <- dist(occ_msa_top100_scale, "euclidean")
hc <- hclust(dis, method = "average")
hc$labels <- rownames(occ_msa_top100_scale)

library(dynamicTreeCut)
library(cluster)

cu <- cutreeDynamic(hc, minClusterSize = 2, pamStage = T, deepSplit = 4,
			 distM=as.matrix(dis), method="hybrid")
summary(silhouette(cu, dis))
table(cu)

plot(hc)

library(ade4)

hcp <- hclust2phylog(hc)
radial.phylog(hcp)

#### taken from http://www.coppelia.io/2014/07/converting-an-r-hclust-object-into-a-d3-js-dendrogram/
HCtoJSON <- function(hc){
  
  labels <- hc$labels
  clus <- hc$clus
  merge<-data.frame(hc$merge)
  
  for (i in (1:nrow(merge))) {
    
    if (merge[i,1]<0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]),list(name=labels[-merge[i,2]])))")))}
    else if (merge[i,1]>0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node", merge[i,1], ", list(name=labels[-merge[i,2]])))")))}
    else if (merge[i,1]<0 & merge[i,2]>0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]), node", merge[i,2],"))")))}
    else if (merge[i,1]>0 & merge[i,2]>0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node",merge[i,1] , ", node" , merge[i,2]," ))")))}
  }
  
  eval(parse(text=paste0("JSON<-toJSON(node",nrow(merge), ")")))
  
  return(JSON)
}

hcjson <- HCtoJSON(hc)

cat(hcjson, file="src/hc.json")
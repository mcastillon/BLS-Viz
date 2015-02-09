setwd("C:/Users/mcast_000/Documents/GitHub/BLS Viz")

options(stringsAsFactors = F)

library(data.table)

#### BLS dataset available from http://www.bls.gov/oes/tables.htm
bls_occ <- fread("Data/bls_occ.csv")
colnames(bls_occ)

library(plyr)
library(dplyr)

bls_occ <- data.table(bls_occ)

bls_occ

### area_type==4 gives us metro areas
bls_occ_msa <- filter(bls_occ, area_type==4)
### occupation category
bls_occ_msa[, occ_cat := substr(occ_code, 1, 2)]
setkey(bls_occ_msa, occ_cat)

### a few different kinds of groups
count(bls_occ_msa, group)

### we'll take the major occupation group and the associated title
occ_groups <- bls_occ_msa %>%
			filter(group == "major") %>%
				select(occ_title, occ_cat) %>%
					unique() %>%
						setnames("occ_title", "major_occ")
setkey(occ_groups, occ_cat)

### and add it to the original dataset
bls_occ_msa <- bls_occ_msa[occ_groups]
### and filter to detailed groups

library(magrittr)

bls_occ_msa %<>% filter(group == "detail")

bls_occ_msa

bls_occ_msa %<>% select(area_title, occ_code, occ_title, tot_emp, jobs_1000,
            		loc_quotient, starts_with("h_"), starts_with("a_"),
                        major_occ)

#### a lot of variables have nonsense entries, change to NA
#### warnings are expected
bls_occ_msa %<>% mutate_each_(funs(as.numeric), c("tot_emp", "jobs_1000", "loc_quotient"))
ah_vars <- colnames(select(bls_occ_msa, starts_with("h_"), starts_with("a_")))
bls_occ_msa %<>% mutate_each_(funs(as.numeric), ah_vars)

library(reshape2)

### convert to be wide by total employment for a given occupation
occ_rs <- recast(bls_occ_msa, id.var=c("area_title", "major_occ"),
                 measure.var="tot_emp", fun.aggregate=sum,
                 fill=0, area_title ~ major_occ)
occ_rs[is.na(occ_rs)] <- 0
occ_rs <- data.table(occ_rs)
occ_rs[, tot_emp := rowSums(select(occ_rs, -area_title))]

### select top 100 in terms of total employment
# occ_msa_top100 <- top_n(occ_rs, 100)
# setkey(occ_msa_top100, area_title)
occ_msa_scale <- select(occ_rs, -area_title, -tot_emp) / occ_rs$tot_emp

rownames(occ_msa_scale) <- occ_rs$area_title

occ_dis <- dist(occ_msa_scale, "euclidean")
hc_occ <- hclust(occ_dis, method = "ward.D2")
hc_occ$labels <- rownames(occ_msa_scale)

plot(hc_occ)

occ_clus <- cutree(hc_occ, 14)

library(ade4)

hcp_occ <- hclust2phylog(hc_occ)
radial.phylog(hcp_occ)

library(rjson)

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

hc_occ_json <- HCtoJSON(hc_occ)

cat(hc_occ_json, file="src/hc_occ.json")

#### 2012 business patterns data from
#### http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=BP_2012_00A1&prodType=table

#### there should be a warning regarding converting numeric to character
#### we'll remedy that error programmatically
biz_pat_naics <- fread("Data/BP_2012_00A1_with_ann.csv")
setnames(biz_pat_naics, tolower(colnames(biz_pat_naics)))
names(biz_pat_naics)
setnames(biz_pat_naics, gsub("-", ".", colnames(biz_pat_naics)))

### a few columns have footnotes instead of values
### we will replace those values based off of approx medians from BP_2012_00A1.txt
biz_pat_naics

#### blegh thats an ugly nested ifelse
biz_pat_naics %<>%
  mutate(emp = ifelse(emp=="a", 10,
                      ifelse(emp=="b", 60,
                             ifelse(emp=="c", 175,
                                    ifelse(emp=="e", 375,
                                           ifelse(emp=="f", 750,
                                                  ifelse(emp=="h", 3750,
                                                         ifelse(emp=="g", 1750,
                                                                ifelse(emp=="k", 375000,
                                                                       ifelse(emp=="j", 17500,
                                                                              ifelse(emp=="i", 7500, emp)))))))))))
biz_pat_naics %<>% mutate(emp = as.numeric(emp))
count(biz_pat_naics, naics.display.label)
count(biz_pat_naics, geo.display.label)

library(stringr)

biz_pat_naics %<>% filter(grep("Metro Area", geo.display.label))
biz_pat_naics[, area_title := str_replace(geo.display.label, " Metro Area", "")]

### how well do metro areas line up across datasets?
biz_pat_areas <- count(biz_pat_naics, area_title)
bls_areas <- count(bls_occ_msa, area_title)
overlap_areas <- inner_join(biz_pat_areas, bls_areas, by = "area_title")
nrow(biz_pat_areas)
nrow(bls_areas)
nrow(overlap_areas)

naics_rs <- recast(biz_pat_naics, id.var=c("area_title", "naics.display.label"),
                   measure.var="emp", fun.aggregate=sum,
                   fill=0, area_title ~ naics.display.label)
naics_rs <- data.table(naics_rs)
naics_rs[, naics_emp := rowSums(select(naics_rs, -area_title))]

naics_msa_scale <- select(naics_rs, -area_title, -naics_emp) / naics_rs$naics_emp

rownames(naics_msa_scale) <- naics_rs$area_title

naics_dis <- dist(naics_msa_scale, "euclidean")
hc_naics <- hclust(naics_dis, method = "ward.D2")
hc_naics$labels <- rownames(naics_msa_scale)

naics_clus <- cutree(hc_naics, 14)

### how do the two different clusters compare?
lapply(1:14, function(i) {
	occ_clus[occ_clus==i]
})
lapply(1:14, function(i) {
	naics_clus[naics_clus==i]
})

hcp_naics <- hclust2phylog(hc_naics)
radial.phylog(hcp_naics)

hc_naics_json <- HCtoJSON(hc_naics)

cat(hc_naics_json, file="src/hc_naics.json")
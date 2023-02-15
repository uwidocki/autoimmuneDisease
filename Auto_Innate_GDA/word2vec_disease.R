rm(list = ls())

# MeSH and DiseaseNames in one list (commadelim)
DiseaseNames_w2v = read.table( "~/Desktop/PostDoc/00_Projects/GDDA/03_Output/Disease_Names.csv", sep = ",", header = T)

require(magrittr)
require(word2vec)

model     <- read.word2vec("/Users/deisygysi/Desktop/PostDoc/00_Projects/GDA/00_Data/pubmed2018_w2v_400D/pubmed2018_w2v_400D.bin")
terms     <- summary(model, "vocabulary")
embedding <- as.matrix(model) #%>% as.data.frame()
Dis = DiseaseNames_w2v$DiseaseName %>% unique()

emb = list()
pb <- txtProgressBar(min = 0, max = length(Dis), style = 3)

average_dis = function(disease, e = embedding2){
  require(magrittr)
  
  t = rownames(e)
  dis = disease %>% 
    stringr::str_split(., " ", simplify = TRUE) %>% 
    as.character()
  
  t_in_dis = sum(t %in% dis)
  if(t_in_dis > 0){
    v =  e[t %in% dis,] %>% 
      matrix(., ncol = ncol(e)) %>% 
      colMeans(., na.rm = TRUE) %>% 
      as.data.frame()
    names(v)= disease
    return(v)
  }
}

lalala = Dis %>% strsplit(., " ") %>% unlist() %>% unique()
embedding2 = embedding[rownames(embedding) %in% lalala, ]

average_dis(disease = Dis[1000], e = embedding2)

require(parallel)
cl = parallel::makePSOCKcluster(10)
clusterExport(cl, "average_dis")
clusterExport(cl, "embedding2")

U = clusterApplyLB(cl, Dis, average_dis)
U %<>% dplyr::bind_cols()

stopCluster(cl)


c = U %>%t%>% na.exclude() %>%  philentropy::distance(., method = "cosine") # matrix with distance between diseases
xxx = U %>%t%>% na.exclude() %>% row.names()
colnames(c) = row.names(c) = xxx
# 
data.table::fwrite(c, "~/Desktop/PostDoc/00_Projects/GDDA/03_Output/W2Vec_R.csv")

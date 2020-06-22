# PART 1: Generate data.frame
	# step-1. read in table, generate dataframe.
MDepths <- read.csv("Depths.csv", header=TRUE, sep = ",")

	# step-2. clean dataframe from the NA values
MDF <- na.omit(MDepths) 
row.has.na <- apply(MDF, 1, function(x){any(is.na(x))}) # check up
sum(row.has.na) # sum up all the NA: [1] 0
head(MDF) # look at and proof the dataframe

# PART-2. Hierarchical cluster analysis, dendrogram.
library(dendextend)
	# step-3. plot 1 dendrogram (here: by 25 clusters)
dend <- MDF[1:25,] %>%  scale %>% dist %>% # calculate a distance matrix, 
	hclust (method = "average") %>% 
	as.dendrogram %>% 
	set("labels", c(("profile"), rep(1:25), sep="")) %>%
	set("labels_col","blue") %>% set("labels_cex", c(.7)) %>%
	set("branches_k_color", k=5) %>% set("branches_lwd", 1) %>% 	
	set("nodes_pch", 19) %>%  set("nodes_cex", 1) %>%
	set("nodes_col", "plum1") %>%
	set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red")) 
dend %>% plot(main = "Mariana Trench: \nCluster Analysis Dendrogramm-1 of the Bathymetric Profiles \nUnsorted Dendrogramm")

	# step-4. plot 2nd dendrogram from the 1st one, sorted by cluster sizes
dend2 <- sort(dend)
dend2 %>%  set("branches_k_color", k=3) %>% set("branches_lwd", 1) %>%    	
	set("labels_col","blue") %>% set("labels_cex", c(.7)) %>%
	set("branches_k_color", k=5) %>% set("branches_lwd", 1) %>% 	
	set("nodes_pch", 19) %>%  set("nodes_cex", 1) %>%
	set("nodes_col", "plum1") %>% 	
	set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))
dend2 %>% plot(main = "Mariana Trench: \nCluster Analysis Dendrogramm-2 of the Bathymetric Profiles \nSorted Dendrogramm")

	# step-5. compare two dendrograms (unsorted and sorted)
tanglegram(dend, dend2)
tanglegram(dend, dend2) %>% plot(main = "Mariana Trench: \nComrapison of the Cluster Dendrogramms 1 and 2")

	# step-6. Hierarchical Clustering with P-Values via Multiscale Bootstrap Resampling
data(MDF) 
set.seed(518) 
result <- pvclust(MDF, method.dist="cor", method.hclust="average", nboot=10)
# Default plot of the result 
plot(result, main = "Mariana Trench Bathymetric Profiles 1-25: \nHierarchical Clustering with P-Values (AU/BP, %) \nvia Multiscale Bootstrap Resampling")
pvrect(result)

	# step-7. pvclust and dendextend - results as sorted dendrogram
result %>% as.dendrogram %>% 
	set("branches_k_color", k = 5, value = c("purple", "orange", "cyan1", "firebrick1", "springgreen")) %>%
 	plot(main = "Mariana Trench Bathymetric Profiles 1-25: Cluster Dendrogram\nwith AU/BP Values (%). nAU: Approximately Unbiased p-Value \n and BP: Bootstrap Probability")
result %>% text 
result %>% pvrect

# The end. the results can be saved as a pdf through "Save As" (here: 5 figures for each step from 3 to 7)

######################################## additional
# to check up how many elements are presented in each node
dend %>% get_nodes_attr("members", id = c(2,5))

dend <- MDF[1:25,] %>%  scale %>% dist %>% 
	hclust %>% as.dendrogram
set("branches_k_color", k=3) %>% set("branches_lwd", 1.2) %>%   
set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>%     
set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red")) 
plot(dend)

# simple dendrogram (through the hclust: not much controls, although it works. The 'dendextend' prsents better results)
model <- hclust(dist(MDF), "ave")
dhc <- as.dendrogram(model)
ddata <- dendro_data(dhc, type = "rectangle")

pclusters <- ggplot(segment(ddata1)) +    
	geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +    
	coord_flip() +    
	scale_y_reverse(expand = c(0.2, 0)) 
pclusters # x: 518 points, y - depths

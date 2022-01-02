################### Duke Dept of Neuroscience Genome Analysis Project #####################
#=============================================================================
#===========   Association Rules and Recommender Systems =====================
#=============================================================================

#setwd("c:/dev/genome")
#setwd("/root/Documents/R")
setwd("/media/DriveA/Development/Dev/Genome/combine/result_variation/snp")

if (!requireNamespace("BiocManager", quietly = TRUE))
	install.packages("BiocManager")

BiocManager::install("Rsamtools")
BiocManager::install("pasillaBamSubset")

#source("http://bioconductor.org/biocLite.R")
#biocLite("Rsamtools")

library(BiocManager)

update.packages(ask = FALSE)
install.packages("tidyverse")
install.packages("plyr")
install.packages("arules")
install.packages("arulesViz")
install.packages("dplyr")
install.packages("gcookbook")
install.packages("ggplot2")
install.packages("R.utils")
install.packages("odbc")
install.packages("DBI")
install.packages("sqlservr")
install.packages("data.table")
install.packages("RODBC")
install.packages("readxl")
library(tidyverse)
library(plyr)
library(readr)
library(arules)
library(arulesViz)
library(dplyr)
library(gcookbook)
library(ggplot2)
library(R.utils)
library(odbc)
library(DBI)
install.packages("remotes")
remotes::install_github("itssimon/sqlservr")
install.packages("vcfR")
library(vcfR)
library(data.table)
library(RODBC)
library(Rsamtools)
library(pasillaBamSubset)
# prepare for transaction data
install.packages("fastqcr")
library("fastqcr")
library("ShortRead")
install.packages("microseq")
library(microseq)
library(vcfR)
library(GenomicAlignments)

library("readxl")
library(Hmisc)
library(pasillaBamSubset)
library(Rsamtools)
library(ggrepel)
install.packages("factoextra")
library(factoextra)
install.packages("caTools")
library(caTools)

list <- c(42,79,86,85,35,27,87,7,25,84,85,44,124,25,83,85,84,31,40)

df <- data.frame(list)
df

boxplot(list, border="brown", horizontal=FALSE, notch=FALSE, col="orange")
ggplot(data=df, aes(y=list)) 

# FASTq files

fq.file <- file.path("D:/result/Patient2A/clean_data","210821_M105_V350019556_L03_B5GHUMehuxRAABA-509_2.fq.gz")
fdta1 <- readFastq(fq.file)
head(fdta1,100)
summary(fdta1)
str(fdta1)
fdta_sample_a <- fdta1[1:10,]
fdta_sample_a$Patient <- "2A"
summary(fdta_sample_a)
system.time(write.csv(fdta_sample_a,'out_210821_M105_V350019556_L03_B5GHUMehuxRAABA-509_2.csv',row.names=FALSE))

#Single Nucleotide Polymorism Variations
vcf_file <- read.vcfR('combine.snp.vcf.gz',verbose=FALSE)
str(vcf_file)
head(vcf_file,10)
chrom <- create.chromR(name='Supercontig',vcf=vcf_file,seq=dna,ann=gff)

#Extract GenoTypes

vcf_file_1 <- extract_gt_tidy(vcf_file, format_fields=NULL, format_types=TRUE, dot_is_NA=TRUE, alleles=TRUE, allele.sep="/", gt_column_prepend="gt_", verbose=TRUE)
str(vcf_file_1)
vcf_file_1 <- vcf_file_1 %>%
	select("Key","Indiv","gt_AD","gt_DP","gt_GQ","gt_GT","gt_PL","gt_GT_alleles")

system.time(write.csv(vcf_file_1,'out_combine.indel.vcf.gz.csv',row.names=FALSE,col.names=FALSE))

extract_info_tidy(vcf_file, info_fields=NULL, info_types=TRUE, info_sep=";")

# snp combine

combine_snp_annot <- read.csv('D:/result/combine/result_variation/snp/combine.snp.annot.csv')
str(combine_snp_annot)

combine_snp_cds_annot <- read.csv('D:/result/combine/result_variation/snp/combine.snp.cds_annot.csv')
str(combine_snp_cds_annot)

combine_snp_gene <- read.csv('D:/result/combine/result_variation/snp/combine.snp.gene.csv')
str(combine_snp_gene)

vcf_snp <- read.vcfR('D:/result/combine/result_variation/snp/combine.snp.vcf.gz')
vcf_snp_1 <- extract_gt_tidy(vcf_snp, format_fields=NULL, format_types=TRUE, dot_is_NA=TRUE, alleles=TRUE, allele.sep="/", gt_column_prepend="snp_", verbose=TRUE)
str(vcf_snp_1)
vcf_snp_1 <- vcf_snp_1 %>%
	select("Key","Indiv","snp_AD","snp_DP","snp_GQ","snp_GT","snp_PL","snp_GT_alleles")

summary(vcf_snp_1)
str(vcf_snp_1)

extract_info_tidy(vcf_snp, info_fields=NULL, info_types=TRUE, info_sep=";")


# Patient 1A 

cnv_file <- read.csv("Patient1A.cnv.csv")
str(cnv_file)

# Patient 1A indel

patient1a_indel_annot <- read.csv('D:/result/Patient10A/result_variation/indel/Patient10A.indel.annot.csv')
str(patient1a_indel_annot)
patient1a_indel_annot_1 <- patient1a_indel_annot %>%
	 select("Func", "Biotype", "Impact", "Chr")

hist(patient1a_indel_annot_1["Impact"])

# BAM Files

which <- IRangesList(seq1=IRanges(1000,2000), seq2=IRanges(c(100,1000),c(1000,2000)))
what <- c("rname","strand","pos","qwidth", "seq")

#param0 <- ScanBamParam(which=which, what=what)

param0 <- ScanBamParam()
param1 <- ScanBamParam(what=scanBamWhat(), which=which)

param2 <- ScanBamParam(what=scanBamWhat(),
				   flag=scanBamFlag(isMinusStrand=FALSE))

param3 <- ScanBamParam(what=c("rname", "strand", "pos", "qwidth"))

fl <- system.file("extdata", "ex1.bam", package="Rsamtools", mustWork=TRUE)
res <- scanBam(fl, param=param2)[[1]]
lapply(res, head) # Show the DNA Sequence graphically


param4 <- ScanBamParam(tag=c("NM", "H1"), what="flag")
bam4 <- scanBam(fl, param=param4)
str(bam4[[1]][["tag"]])

## tagFilter
param5 <- ScanBamParam(tag=c("NM", "H1"), tagFilter=list(NM=c(2, 3, 4)))
bam5 <- scanBam(fl, param=param5)
table(bam5[[1]][["tag"]][["NM"]])


bam <- scanBam("D:/result/Patient1A/result_alignment/Patient1A.bam", param=param)

## flag utils
flag <- scanBamFlag(isUnmappedQuery=FALSE, isMinusStrand=TRUE)

param6 <- ScanBamParam(what="flag")
bam6 <- scanBam(fl, param=param6)
flag6 <- bam6[[1]][["flag"]]
head(bamFlagAsBitMatrix(flag6[1:9]))
colSums(bamFlagAsBitMatrix(flag6))
bamFlagAsBitMatrix(flag6)

fq.file <- file.path("C:/Dev/Genome","210822_M052_V350016552_L01_B5GHUMehuxRAADA-525_1.fq.gz")
fdta2 <- readFastq(fq.file)
fdta_sample_b <- fdta2[1:10,] 
summary(fdta_sample_b)
str(fdta_sample_b)
fdta_sample_b$Patient <- "2A"
system.time(write.csv(fdta_sample_b,'out_210822_M052_V350016552_L01_B5GHUMehuxRAADA-525_1.csv',row.names=FALSE,col.names=FALSE))

#*******************************

filename <- untreated1_chr1()

(bf <- BamFile("D:/result/Patient1A/result_alignment/Patient1A.bam"))
(bf <- BamFile("D:/result/Patient1A/result_alignment/Patient1A.bam",yieldSize=1000))

seqinfo(bf)
(sl <- seqlengths(bf))
#quickBamFlagSummary(bf)  -- Realloc cound not re-allocate memory problem
(gr <- GRanges("chr4",IRanges(1, sl["chr4"])))
countBam(bf, param=ScanBamParam(which = gr))

reads <- scanBam(BamFile("D:/result/Patient1A/result_alignment/Patient1A.bam", yieldSize=5))
class(reads)
names(reads[[1]])
reads[[1]]$pos # the aligned start position
reads[[1]]$rname # the chromosome
reads[[1]]$strand # the strand
reads[[1]]$qwidth # the width of the read
reads[[1]]$seq # the sequence of the read

gr <- GRanges("chr4",IRanges(500000, 700000))
reads <- scanBam(bf, param=ScanBamParam(what=c("pos","strand"), which=gr))

hist(reads[[1]]$pos)

readsByStrand <- split(reads[[1]]$pos, reads[[1]]$strand)
myHist <- function(x) table(cut(x, 50:70 * 10000 ))
tab <- sapply(readsByStrand, myHist)
barplot(t(tab))

(ga <- readGAlignments(bf)) # allocation of memory issue.  If the BAM file is too large.

#ChIP-seq Workflow Analysis

#*******************************************

print(bf)
length(ga)
start_first<- start(bf)[1]
end_last<-end(bf)[length(bf)]
cvg <- coverage(bf) 

granges(ga[1])

print(peaks)
max_idx <- which.max(score(peaks))
max_peak_chrom <- chrom(peaks)[max_idx]
max_peak_range <- ranges(max_peak_chrom)

gr <- GRanges("chr4", IRanges(700000, 800000))
(fo <- findOverlaps(ga, gr)) # which reads over this range



#*************************************

combine_indel_annot <- read.csv("D:/result/combine/result_variation/indel/combine.indel.annot.csv", header=TRUE)
#bcp Sequences_2 in "c:\Dev\genome\out_210822_M052_V350016552_L01_B5GHUMehuxRAADA-525_1.csv" -S "tcp:dukegenome.database.windows.net; Database=GeneomeDuke" -U "dukegenome" -P "z1U09187322460288!a" -q -c -F 2  -t ,
#bcp Sequences_2 in "c:\Dev\genome\out_210821_M105_V350019556_L03_B5GHUMehuxRAABA-509_2.csv" -S "tcp:dukegenome.database.windows.net; Database=GeneomeDuke" -U "dukegenome" -P "z1U09187322460288!a" -q -c -F 2  -t ,

str(combine_indel_annot)

combine_indel_annot_1 <- combine_indel_annot %>%
	select('Func','Biotype','Impact','Chr')
	#mutate(residual = quality - .fitted) %>%
	#mutate(sq_residual = residual^2) %>%
	#summarize(mse = mean(sq_residual)) %>%
	#summarize(rmse = sqrt(mse))
str(combine_indel_annot_1)

hist.data.frame(combine_indel_annot_1['Func'])
hist.data.frame(combine_indel_annot_1['Biotype'])
hist.data.frame(combine_indel_annot_1['Impact'])
hist.data.frame(combine_indel_annot_1['Chr'])

combine_indel_cds_annot <- read.csv("D:/result/combine/result_variation/indel/combine.indel.cds_annot.csv", header=TRUE)
str(combine_indel_cds_annot)
hist.data.frame(combine_indel_cds_annot['Func'])
hist.data.frame(combine_indel_cds_annot['Biotype'])
hist.data.frame(combine_indel_cds_annot['Impact'])
hist.data.frame(combine_indel_cds_annot['Chr'])

# Visualization and Description

ggplot(data = train) +
	geom_smooth(mapping = aes(y=count, x=duration, color=protocol_type), show.legend = FALSE) +
	scale_x_log10()

ggplot(data=train) +
	geom_point(mapping = aes(y=count, x=duration)) +
	geom_smooth(mapping = aes(y=count, x=duration)) +
	scale_x_log10() +
	scale_y_log10()

ggplot(data=train, mapping=aes(x=duration, y=count)) +
	geom_point(mapping = aes(color=service)) +
	geom_smooth() +
	scale_x_log10() +
	scale_y_log10()

ggplot(data = train) +
	geom_bar(mapping = aes(x=class, fill=service),
			 position = "dodge"
	)

ggplot(data = train) +
	geom_boxplot(mapping = aes(x=service, y=dst_host_count)) +
	coord_flip()


#**************************************
fdta_combine <- rbind(fdta1, fdta2)
fdta_combine2 <- fdta_combine[1:10,]
str(fdta_combine2)
fdta_combine2$Patient <- "1A"
str(fdta_combine2)

#Driver={ODBC Driver 13 for SQL Server};Server=tcp:dukegenome.database.windows.net,1433;Database=GeneomeDuke;Uid=dukegenome;Pwd={your_password_here};Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;

con <- DBI::dbConnect(odbc::odbc(),
					  Driver   = "{ODBC Driver 17 for SQL Server}",
					  Server   = "tcp:dukegenome.database.windows.net",
					  Database = "GeneomeDuke",
					  UID      = "dukegenome",
					  PWD      = "z1U09187322460288!a",
					  Port     = 1433)

con <- DBI::dbConnect(odbc::odbc(), "dukegenome", UID= "dukegenome", PWD = "z1U09187322460288!a")

# odbc Writing
system.time(dbWriteTable(con, "Sequences_2", fdta_combine2[1:10,], append=TRUE))

odbcConnect("dukegenome", uid = "dukegenome", pwd = "z1U09187322460288!a")

dbconn <- odbcDriverConnect("Driver=ODBC Driver 17 for SQL Server;
                Server=tcp:dukegenome.database.windows.net; Database=GeneomeDuke; 
                Uid=dukegenome; Pwd=z1U09187322460288!a")


#db_bcp(fdta_combine2, 'GeneomeDuke.dbo.Sequences_2', conn=dbconn, truncate = TRUE, preserve_empty_strings = TRUE, sep = "^|~", eol = "^|\r\n", paranoid = FALSE, preserve_memory = FALSE, tmp_dir=".")
#db_bcp(fdta_combine2, 'GeneomeDuke.dbo.Sequences_2', conn=dbconn)

sql <- "select * from dbo.Sequences_2"
sqlQuery(dbconn, sql)

fdta_combine2
system.time(write.csv(fdta_combine[4831001:32000000,],'fdta_out3.csv',row.names=FALSE))

n_occur <- data.frame(table(seqN$Sequence)) 
n_occur[n_occur$Freq > 1,]

n_occur <- data.frame(table(seqN$Header)) 
n_occur[n_occur$Freq > 1,]


# Load CSV

indel_combine <- read.csv('./result_variation/indel/combine.indel.annot.csv')
str(indel_combine)
indel_combine_cds <- read.csv('./result_variation/indel/combine.indel.cds_annot.csv')
str(indel_combine_cds)
indel_combine_gene <- read.csv('./result_variation/indel/combine.indel.gene.csv')
str(indel_combine_gene)
snp_combine <- read.csv('./result_variation/snp/combine.snp.annot.csv')
str(snp_combine)
snp_combine_cds <- read.csv('./result_variation/snp/combine.snp.cds_annot.csv')
str(snp_combine_cds)
snp_combine_gene <- read.csv('./result_variation/snp/combine.snp.gene.csv')
str(snp_combine_gene)

patient1a_cnv_rep_anno <- read.csv('Patient1A.CNV.vep.anno.csv')
patient1a_cnv_rep_anno %>% gather() %>% head()
ggplot(gather(patient1a_cnv_rep_anno), aes(value)) +
	geom_histogram(stat= "count") +
	facet_wrap(~key)
str(patient1a_cnv_rep_anno)
hist(patient1a_cnv_rep_anno[BIOTYPE])


# Machine Logic Code - Association Analysis

my_files <- list.files(path=my_dir, pattern="*.csv",full.names=TRUE)
my_files

chromo_full <- ldply(my_files, read_csv)

chromo_small <- chromo_full%>%
	filter(field1 > 100000)

summary(chromo_small)

head(chromo_small,100)

chromo_association <- chromo_small %>%
	select('<transactionID>', '<Item>') 
#rename(transactionID=screen_name, Item=value) 
#rename(Item=retweet_screen_name, TransactionID=retweet_screen_name)

str(chromo_association)
head(chromo_association,100)

chromo_association2 <- chromo_association[!duplicated(chromo_association), ]
chromo_association2 <- chromo_association2 %>%
	na.omit()

head(chromo_association2,100)

write.csv(chromo_association2, "association1.csv", row.names=FALSE)

my_basket1 <- read.transactions("association1.csv", format="single", sep=",", cols = c("<item>","<transactionID>"), header= TRUE)

inspect(my_basket1)

summary(my_basket1)

itemFrequencyPlot(my_basket1)
#data <- as(data,"transactions")

rules <- apriori(my_basket1, parameter = list(supp=0.015, conf=0.9, maxlen=10))

summary(rules)
inspect(rules)

rules <- sort(rules, by = 'confidence', decreasing = TRUE)
inspect(rules[1:10])

itemFrequencyPlot(my_basket1)
## (2) Summarize and visualize transaction data 

rules <- apriori(my_basket1, parameter=list(supp=0.01, conf=0.8, maxlen=10, minlen=2))

summary(rules)
inspect(rules)

rules <- sort(rules, by = "confidence")

## (3) Apply the Apriori algorithm

## Remove redundant rules

is.redundant(rules)

inspect(rules[is.redundant(rules)])

rules2 <- rules[!is.redundant(rules)]
inspect(rules2)

plot(rules2)
plot(rules2, method = "graph")
plot(rules[1:10], method = "graph")

summary(my_basket1)

tweet_rules <- apriori(my_basket1, parameter = list(supp=0.01, conf=0.8, maxlen=4), appearance=list(default="lhs", rhs = "JackJShepherd"))

chromo_rules <- sort(chromo_rules, by = "confidence", decreasing = TRUE)
inspect(chromo_rules)
plot(chromo_rules, method="graph")

# K-means Clustering
#======================== Week 7. Cluster Analysis ===========================


## (1) Import mtcars.csv and inspect the data

combine_indel_annot <- read.csv("D:/result/combine/result_variation/indel/combine.indel.annot.csv", header=TRUE)
str(combine_indel_annot)
combine_indel_annot_1 <- combine_indel_annot %>%
	select('Func','Biotype','Impact','Chr')
str(combine_indel_annot_1)
summary(combine_indel_annot_1)

combine_indel_annot_cds_1 <- combine_indel_cds_annot %>%
	select('Func','Biotype','Impact','Chr')
str(combine_indel_annot_cds_1)
summary(combine_indel_annot_cds_1)

n_occur <- data.frame(table(combine_indel_annot_1$Chr))
n_occur

## (2) Cluster the vehicles in the dataset (using either K-means or hierarchical clustering). 
##     You determine the number of clusters and the variables to be used for clustering.

# K-means clustering

str(cnv_file)
cnv_file_1 <- cnv_file[c(1,3)]
str(cnv_file_1)
cnv_file_1 <- na.omit(cnv_file_1)  
hist(cnv_file_1$CNV_size, breaks=6, xlim=c(0,5000000))
hist(cnv_file_1)
 
ggplot(data = cnv_file_1) +
	geom_point(mapping = aes(y=CNV_size, x=CNV_type, color=CNV_type, size=10))  
 
ggplot(data = cnv_file_1) +
	geom_smooth(mapping = aes(y=CNV_size, x=CNV_type, color=CNV_type), show.legend = TRUE)  

ggplot(data = cnv_file_1) +
	geom_bar(mapping = aes(x=CNV_type, fill=CNV_type),
			 position = "dodge"
	)


kmeans_result <- kmeans(cnv_file_1[!is.na(cnv_file_1)], centers = 4, iter.max = 25, nstart = 10)

kmeans_result$cluster

str(kmeans_result)

cnv_file_2 <- cnv_file %>% 
	mutate(cluster_kmeans = kmeans_result$cluster)

 


# Hierarchical clustering

d_matrix <- dist(cnv_file_1, method = "euclidean")

hc_result <- hclust(d_matrix, method = "single")

plot(hc_result)

hc_cluster <- cutree(hc_result, k = 4)

hc_cluster

## (3) Summarize the characteristics of each cluster.



## (4) Create a scatter plot with mpg on the x axis and hp on the y axis. 
##     Color the observations based on the clusters revealed in the previous question.



ggplot(my_mtcars, aes(x=mpg, y=hp)) +
	geom_point(aes(color = as.factor(cluster_kmeans))) +
	labs(title = "K-Means Clustering", color = "Cluster") +
	geom_text_repel(aes(label = model), force = 5)

combine_indel_annot_1[, c(-1, -2)]


### Determining the optimal number of clusters


fviz_nbclust(combine_indel_annot_1[ , c(-1, -2)], kmeans, method = "wss") +
	labs(subtitle = "Elbow Method")

fviz_nbclust(my_mtcars[ , c(-1, -2)], kmeans, method = "silhouette") +
	labs(subtitle = "Silhouette Method")


kmeans_result2 <- kmeans(my_mtcars[ , c(-1, -2)], centers = 2, iter.max = 25, nstart = 10)

my_mtcars <- my_mtcars %>% 
	mutate(cluster_kmeans2 = kmeans_result2$cluster)

ggplot(my_mtcars, aes(x=mpg, y=hp)) +
	geom_point(aes(color = as.factor(cluster_kmeans2))) +
	labs(title = "K-Means Clustering", color = "Cluster") +
	geom_text_repel(aes(label = model), force = 5)



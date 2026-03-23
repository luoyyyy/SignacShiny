suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(Matrix))

suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(shinybusy))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(SingleR))
suppressPackageStartupMessages(library(plyr))

suppressPackageStartupMessages(library(GenomicRanges))
suppressPackageStartupMessages(library(Signac))
suppressPackageStartupMessages(library(Seurat))
suppressPackageStartupMessages(library(irlba))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
# suppressPackageStartupMessages(library(Matrix))
# suppressPackageStartupMessages(library(openxlsx))
suppressPackageStartupMessages(library(shinyBS))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plotly))

suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(stringr))
# suppressPackageStartupMessages(library(gridExtra))

suppressPackageStartupMessages(library(JASPAR2020))
# suppressPackageStartupMessages(library(TFBSTools) # 会报错
# suppressPackageStartupMessages(library(monocle3))
# suppressPackageStartupMessages(library(motifmatchr))
suppressPackageStartupMessages(library(clusterProfiler))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(EnsDb.Hsapiens.v75))
suppressPackageStartupMessages(library(EnsDb.Hsapiens.v86))
suppressPackageStartupMessages(library(EnsDb.Mmusculus.v79))
#suppressPackageStartupMessages(library(BSgenome.Hsapiens.UCSC.hg19))
# suppressPackageStartupMessages(library(BSgenome.Hsapiens.UCSC.hg38))
# suppressPackageStartupMessages(library(BSgenome.Mmusculus.UCSC.mm10))

cat("Finish library Necessary Packages!\n")

plot_GO_bar <- function(go_result, display_number = c(10, 10, 10), title = "GO Terms Enrichment") {
  BP <- go_result[which(go_result$ONTOLOGY=='BP'),]
  BP <- BP[order(BP$pvalue, decreasing = F),,drop=F] #drop保留之前的数据类型
  
  CC <- go_result[which(go_result$ONTOLOGY=='CC'),]
  CC <- CC[order(CC$pvalue, decreasing = F),,drop=F] #drop保留之前的数据类型
  
  MF <- go_result[which(go_result$ONTOLOGY=='MF'),]
  MF <- MF[order(MF$pvalue, decreasing = F),,drop=F] #drop保留之前的数据类型
  ###选取前5条
  display_number = c(10, 10, 10)
  go_result_BP = as.data.frame(BP)[1:display_number[1], ]
  go_result_CC = as.data.frame(CC)[1:display_number[2], ]
  go_result_MF = as.data.frame(MF)[1:display_number[3], ]
  
  #将提取的各组数据进行整合
  go_enrich = data.frame(
    ID=c(go_result_BP$ID, go_result_CC$ID, go_result_MF$ID),  #指定ego_result_BP、ego_result_CC、ego_result_MFID为ID                        
    Description=c(go_result_BP$Description,go_result_CC$Description,go_result_MF$Description),
    Count=c(go_result_BP$Count, go_result_CC$Count, go_result_MF$Count), #指定ego_result_BP、ego_result_CC、ego_result_MF的Count为GeneNumber
    type=factor(c(rep("Biological Process", display_number[1]), #设置biological process、cellular component、molecular function 的展示顺序
                  rep("Cellular Component", display_number[2]),
                  rep("Molecular Function", display_number[3])),
                levels=c("Biological Process", "Cellular Component","Molecular Function" )))
  
  #转成因子，防止重新排列
  go_enrich$type_order = factor(go_enrich$Description,levels=go_enrich$Description,ordered = T)
  # 计算文本的长度，用于设置x轴刻度
  #go_enrich$text_length <- nchar(go_enrich$Description)
  head(go_enrich)
  p <- ggplot(go_enrich,
              aes(x=type_order,y=Count, fill=type)) +  #x、y轴定义；根据type填充颜色
    geom_bar(stat="identity", width=0.8) +  #柱状图宽度
    scale_fill_manual(values = c("#Fbdf9d", '#c6dc89', "#cebaf0") ) + #柱状图填充颜色
    xlab("") + #x轴标签
    ylab("Counts") +  #y轴标签
    labs(title = "GO Terms Enrich")+ #设置标题
    theme_bw() +
    theme(text=element_text(size = 20),
          plot.title = element_text(hjust = 0.5),
          legend.position = "top")+
    theme(axis.text.x=element_text(face = "bold", color="black",angle = 0,vjust = 0, hjust = 1 )) 
  p+scale_x_discrete(labels=function(x) str_wrap(x,width = 60))+coord_flip() # x轴文字太长，换行，xy轴转置
  
}
# options(future.plan = future::multiprocess, future.workers = 10)
# plan("multiprocess", workers = 10)
# options(future.globals.maxSize = 40000 * 1024^2) 
# future::plan("multiprocess", workers = 10)
options(shiny.maxRequestSize = 100000000000 * 1024^2)


# pbmc<-readRDS("")

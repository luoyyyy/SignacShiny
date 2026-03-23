set.seed(1234)

server <- function(input,output,session){
  # server global var
  # base
  cat(R.Version()$version.string," \033[0m\n");
  cat(getwd()," \033[0m\n");
  pbmc <<- NULL
  pbmcs_1 <<- NULL
  pbmcs <<- NULL
  Selectdata<<- NULL
  Selectreference_data <<-NULL
  pbmc_exists <<- F
  pbmc1<<-NULL
  Ident11<<-NULL
  Ident12<<-NULL
  Ident41<<-NULL
  Ident13<<-NULL
  Ident42<<-NULL
  cells<<-NULL
  CELL<<-NULL
  motifs<<-NULL
  motifs1<<-NULL
  cell_counts_after<<-NULL
  cell_counts<<-NULL
  gene.activities<<-NULL
  unique_clusters<<-NULL
  gene_names_overlapping<<-NULL
  feature_name<<-NULL
  open_ident<<-NULL
  open_ident1<<-NULL
  open_ident2<<-NULL
  open_ident21<<-NULL
  Species_type<<-NULL
  assay1<<-NULL
  assay2<<-NULL
  marker_list_str2 <<-NULL
  # Dynamic variables(动态变量)
  assaysList <<- NULL
  identsList <<- NULL
  peaksFeatures <<- NULL
  genesFeatures <<- NULL
  # plots
  FM_VinPlot<<-NULL
  FM_FeaturePlot<<-NULL
  GO_Plot1<<-NULL
  GO_Plot2<<-NULL
  ident1_ego1<<-NULL
  ident1_ego2<<-NULL
  qcVlnPlots<<-NULL
  qcVlnPlot <<- NULL
  qcTSSPlot <<- NULL
  qcFragmentHistogram <<- NULL
  dDimplot<<-NULL
  dbFeaturePlot <<- NULL
  grGenomicRegionsPlot <<- NULL
  mtMotifPlot1 <<- NULL
  mtMotifPlot2 <<- NULL
  fpFootPrintingPlot <<- NULL
  # tables
  FindAllMarkers_result <<- NULL
  FindMarkers_result1 <<- NULL
  MotifTopMarkers1 <<- NULL
  MotifTopMarkers2 <<- NULL
  enriched.motifs1 <<- NULL
  enriched.motifs2 <<- NULL
  ClusterMarkers_result1<<- NULL
  Motif_data1<<-NULL
  Motif_data2<<-NULL
  # 菜单控制-------------------------------------------------------
  if(F){
    shinyjs::disable("submit_NLDR")
    shinyjs::disable("submit_Clustering")
    shinyjs::disable("submit_GeneActivity")
    shinyjs::disable(selector = ".sidebar li a[data-value='one']")
    shinyjs::disable(selector = ".sidebar li a[data-value='ones']")
    shinyjs::disable(selector = ".sidebar li a[data-value='two']")
    shinyjs::disable(selector = ".sidebar li a[data-value='twos']")
    shinyjs::disable(selector = ".sidebar li a[data-value='threeone']")
    shinyjs::disable(selector = ".sidebar li a[data-value='threetwo']")
    shinyjs::disable(selector = ".sidebar li a[data-value='four']")
    shinyjs::disable(selector = ".sidebar li a[data-value='sixone']")
    shinyjs::disable(selector = ".sidebar li a[data-value='sixtwo']")
    shinyjs::disable(selector = ".sidebar li a[data-value='five']")
    shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
  }

  
  
 # reactive--------------------------------------------------------
  count <- reactiveVal(NULL)
  fragments <- reactiveVal(NULL)
  tbi <- reactiveVal(NULL)
  metadata <- reactiveVal(NULL)
  MotifMarkers2<- reactiveVal(NULL)
  MotifMarkers1<- reactiveVal(NULL)
  values<-reactiveValues(deDepthCorPlot=NULL)
  values<-reactiveValues(Normalization=NULL)
  # values<-reactiveValues(submit_GeneActivity=FALSE)
  values<-reactiveValues(dpDimPlot=NULL)
  values<-reactiveValues(submit_Clustering=NULL)
  values<-reactiveValues(gene_name=NULL)
  values<-reactiveValues(submit_ClusterMarkers=FALSE)
  
  # values<-reactiveValues(SelectReference_data=NULL)
 # valuebox加载------------------------------------------------------------
  pref<<-reactiveVal(NULL)
  renderUIes <- function(qc,filter){  
    list(  
      vbox_load = renderValueBox({
        color<-if (!is.null(pbmc)) "yellow" else "orange"
        icon_name <- if (!is.null(pbmc)) "check" else "hourglass-half" 
        valueBox(value = NULL,subtitle =tags$p("Load Data ",style = "font-size:14px;font-weight:bold"),
                 color = color,
                 icon = icon(icon_name,style = "font-size: 25px"))}),
      vbox_qc = renderValueBox({
        color<-if (!is.null(qcFragmentHistogram) || !is.null(qcTSSPlot)) "yellow" else "orange"
        icon_name <- if (!is.null(qcFragmentHistogram)|| !is.null(qcTSSPlot)) "check" else "hourglass-half" 
        valueBox(value = NULL,subtitle =tags$p("Quality Control ",style = "font-size:14px;font-weight:bold"),
                 color = color,
                 icon = icon(icon_name,style = "font-size: 25px"))}),
      vbox_filter_cell = renderValueBox({
        color<-if (!is.null(qcVlnPlots)) "yellow" else "orange"
        icon_name <- if (!is.null(qcVlnPlots)) "check" else "hourglass-half"
        valueBox(value=NULL,subtitle =tags$p("Filter Cell",style = "font-size:14px;font-weight:bold"),
                 color=color,icon = icon(icon_name,style = "font-size: 25px"))}),
      vbox_NDR=renderValueBox({
        color<-if (!is.null(values$dpDimPlot)) "yellow" else "orange"
        icon_name <- if (!is.null(values$dpDimPlot)) "check" else "hourglass-half"
        valueBox(value=NULL,subtitle =tags$p("Dim Reduction &Clustering",style="font-size:14px;font-weight:bold"),
                 color=color,icon = icon(icon_name,style = "font-size: 25px"))}),
      vbox_geneactivity = renderValueBox({
        color<-if (!is.null(gene.activities)) "yellow" else "orange"
        icon_name <- if (!is.null(gene.activities)) "check" else "hourglass-half" 
        valueBox(value = NULL,subtitle =tags$p("Creat Gene Activity Matrix ",style = "font-size:14px;font-weight:bold"),
                 color = color,
                 icon = icon(icon_name,style = "font-size: 25px"))}),
      vbox_findAllmaker=renderValueBox({
        color<-if (!is.null(FindAllMarkers_result)) "yellow" else "orange"
        icon_name <- if (!is.null(FindAllMarkers_result)) "check" else "hourglass-half"
        valueBox(value = NULL,subtitle =tags$p("One vs Rest Cluster",style="font-size:14px;font-weight:bold"),
                 color=color,icon = icon(icon_name,style = "font-size: 25px"))}),
      vbox_findIdentsmaker=renderValueBox({
        color<-if (!is.null(FindMarkers_result1)) "yellow" else "orange"
        icon_name <- if (!is.null(FindMarkers_result1)) "check" else "hourglass-half"
        valueBox(value = NULL,subtitle =tags$p("One vs One Cluster",style="font-size:14px;font-weight:bold"),
                 color=color,icon = icon(icon_name,style = "font-size: 25px"))}),
      vbox_annotation=renderValueBox({
        color<-if (!is.null(cells)) "yellow" else "orange"
        icon_name <- if (!is.null(cells)) "check" else "hourglass-half"
        valueBox(value = NULL,subtitle =tags$p("Cell Annotation",style="font-size:14px;font-weight:bold"),
                 color=color,icon = icon(icon_name,style = "font-size: 25px"))}),
      vbox_calling_peak=renderValueBox({
        color<-if (!is.null(grGenomicRegionsPlot)) "yellow" else "orange"
        icon_name <- if (!is.null(grGenomicRegionsPlot)) "check" else "hourglass-half"
        valueBox(value = NULL,subtitle =tags$p("Calling Peak",style="font-size:14px;font-weight:bold"),
                 color=color,icon = icon(icon_name,style = "font-size: 25px"))}),
      vbox_GO_plot=renderValueBox({
        color<-if (!is.null(ident1_ego1)) "yellow" else "orange"
        icon_name <- if (!is.null(ident1_ego1)) "check" else "hourglass-half"
        valueBox(value = NULL,subtitle =tags$p("GO Analysis",style="font-size:14px;font-weight:bold"),
                 color=color,icon = icon(icon_name,style = "font-size: 25px"))}),
      vbox_motifs_analysis=renderValueBox({
        color<-if (!is.null(enriched.motifs1)) "yellow" else "orange"
        icon_name <- if (!is.null(enriched.motifs1)) "check" else "hourglass-half"
        valueBox(value = NULL,subtitle =tags$p("Motif All Table",style="font-size:14px;font-weight:bold"),
                 color=color,icon = icon(icon_name,style = "font-size: 25px"))}),
      vbox_motifs_ident_analysis=renderValueBox({
        color<-if (!is.null(enriched.motifs2)) "yellow" else "orange"
        icon_name <- if (!is.null(enriched.motifs2)) "check" else "hourglass-half"
        valueBox(value = NULL,subtitle =tags$p("Motif Idents Table",style="font-size:14px;font-weight:bold"),
                 color=color,icon = icon(icon_name,style = "font-size: 25px"))}),
      vbox_footprint=renderValueBox({
        color<-if (!is.null(fpFootPrintingPlot)) "yellow" else "orange"
        icon_name <- if (!is.null(fpFootPrintingPlot)) "check" else "hourglass-half"
        valueBox(value = NULL,subtitle =tags$p("Foot Printing",style="font-size:14px;font-weight:bold"),
                 color=color,icon = icon(icon_name,style = "font-size: 25px"))})
    )} 
  
  observeEvent(input$inTabset,{
    if(input$inTabset=="one"){pref("tab1")}
    if(input$inTabset=="ones"){pref("tab2")}
    if(input$inTabset=="two"){pref("tab3")}
    if(input$inTabset=="twos"){pref("tab4")}
    if(input$inTabset=="threeone"){pref("tab5")}
    if(input$inTabset=="threetwo"){pref("tab6")}
    if(input$inTabset=="four"){pref("tab7")}
    if(input$inTabset=="sixone"){pref("tab8")}
    if(input$inTabset=="sixtwo"){pref("tab9")}
    if(input$inTabset=="five"){pref("tab10")}
    if(input$inTabset=="seven"){pref("tab11")}
  })
  observe({
    prefix<-pref()
    ui_elements<-renderUIes(prefix)
    for (name in names(ui_elements)) {  
      output[[paste0(name, "_", prefix)]] <- ui_elements[[name]]  
    }})
  observe({
    invalidateLater(1000)
    if(!is.null(gene.activities) || !is.null(pbmc) || !is.null(qcFragmentHistogram) || !is.null(qcTSSPlot) || !is.null(qcVlnPlots)|| !is.null(values$dpDimPlot)|| !is.null(FindAllMarkers_result)
       || !is.null(FindMarkers_result1)|| !is.null(cells)|| !is.null(grGenomicRegionsPlot)|| !is.null(enriched.motifs1)
       || !is.null(enriched.motifs2)|| !is.null(ident1_ego1)|| !is.null(fpFootPrintingPlot)){
      prefix<-pref()
      ui_elements<-renderUIes(prefix)  
      for (name in names(ui_elements)) {  
        output[[paste0(name, "_", prefix)]] <- ui_elements[[name]]  
        
      }
    }})

  #library加载---------------------------------------------------
  observe({
    if(is.null(pbmcs)){hideTab(inputId = "qVinplot",target = "After Filter")}
    if(is.null(FindMarkers_result1)){
      hideTab(inputId = "GO_setting1",target = "One vs One")
      hideTab(inputId = "GO_Plot",target = "One vs One")
      hideTab(inputId = "GO_Table",target = "One vs One")}
    if(input$GO_setting1=="One vs Rest"){
      hideTab(inputId = "GO_Plot",target = "One vs One")
      hideTab(inputId = "GO_Table",target = "One vs One")
      }
    if(input$GO_setting1=="One vs One"){
      showTab(inputId = "GO_Plot",target = "One vs One")
      showTab(inputId = "GO_Table",target = "One vs One")
      }
    print(input$SelectRefer_data)
    if(input$SelectRefer_data=="EnsDb.Hsapiens.v86"){
      suppressPackageStartupMessages(library(BSgenome.Hsapiens.UCSC.hg38))
      suppressPackageStartupMessages(library(BSgenome.Hsapiens.NCBI.GRCh38))
      Species_type<<-"Homo"
      library(org.Hs.eg.db)
    }
    if(input$SelectRefer_data=="EnsDb.Hsapiens.v75"){
      suppressPackageStartupMessages(library(BSgenome.Hsapiens.UCSC.hg19))
      Species_type<<-"Homo"
      library(org.Hs.eg.db)
    }
    if(input$SelectRefer_data=="EnsDb.Mmusculus.v79"){
      suppressPackageStartupMessages(library(BSgenome.Mmusculus.UCSC.mm10))
      Species_type<<-"Mus"
      library(org.Mm.eg.db)
    }
    #".v86"".v75"".v79"
    # input$SelectRefer_data<<-unlist(strsplit(input$SelectRefer_data,"\\."))[length(unlist(strsplit(input$SelectRefer_data,"\\.")))]
    # print(input$SelectRefer_data)
    # 
  })
  # 提示窗——--------------------------------------------------
  observeEvent(input$inTabset, {
    req(input$inTabset)  # 确保 input$inTabset 有效
    # 清理之前的 popover
    removePopover(session, "Selectdata")
    if (input$inTabset == "strat"){
    }
  })
  # 数据加载--------------------------------------------------------
  observe({
    if(input$Selectdata == "Custom Data"){
      removePopover(session, "Selectdata")
      updateSelectInput(session,"SelectRefer_data", choices = list("EnsDb.Hsapiens.v75","EnsDb.Hsapiens.v86","EnsDb.Mmusculus.v79"),selected = "EnsDb.Hsapiens.v75")
      # shinyjs::enable("SelectRefer_data")
      output$FileInputs <- renderUI({
        tagList(
          fileInput("fileMatrix", "Raw Data：", placeholder = "filtered_peak_bc_matrix.h5",accept = c('.h5', '.h5ad', '.h5seurat', '.rds')),
          fileInput("fileFragments", "Fragments File：",placeholder="fragments.tsv.gz", accept = c("*/*")),
          fileInput("fileFragmentsTbi", "Index：",placeholder="fragments.tsv.gz.tbi", accept = c(".tbi")),
          fileInput("fileSinglecell","Metadata：",placeholder= "singlecell.csv", accept = c(".csv"))
        )})}
    if(input$Selectdata == "Example Data"){
      print(input$inTabset)
      updateSelectInput(session,"SelectRefer_data", 
                        choices = c("EnsDb.Hsapiens.v75"), 
                        selected = "EnsDb.Hsapiens.v75")
         output$FileInputs <- renderUI({
        tagList(
          div(
            HTML(paste(
              '<p>You can learn more about the Example Data on the <a href="https://cf.10xgenomics.com/samples/cell-atac/1.1.0/atac_pbmc_500_nextgem/atac_pbmc_500_nextgem_web_summary.html" target="_blank">website</a></p>',
              '<p>Here are the download links for the Example Data:</p>',
              '<ul style="list-style-type:none; padding-left:0;">',
              '<li><a href="https://cf.10xgenomics.com/samples/cell-atac/1.1.0/atac_pbmc_500_nextgem/atac_pbmc_500_nextgem_filtered_peak_bc_matrix.h5" target="_blank">atac_pbmc_500_nextgem_filtered_peak_bc_matrix.h5</a></li>',
              '<li><a href="https://cf.10xgenomics.com/samples/cell-atac/1.1.0/atac_pbmc_500_nextgem/atac_pbmc_500_nextgem_fragments.tsv.gz" target="_blank">atac_pbmc_500_nextgem_fragments.tsv.gz</a></li>',
              '<li><a href="https://cf.10xgenomics.com/samples/cell-atac/1.1.0/atac_pbmc_500_nextgem/atac_pbmc_500_nextgem_fragments.tsv.gz.tbi" target="_blank">atac_pbmc_500_nextgem_fragments.tsv.gz.tbi</a></li>',
              '<li><a href="https://cf.10xgenomics.com/samples/cell-atac/1.1.0/atac_pbmc_500_nextgem/atac_pbmc_500_nextgem_singlecell.csv" target="_blank">atac_pbmc_500_nextgem_singlecell.csv</a></li>',
              '</ul>'
            )),
            style ="color:black; font-style:calibri;word-spacing:2;line-height:1.5;baseline：middle;",
            align = "justify"
          )
        )
      })
      }
  })
 
  observeEvent(input$submit_Start,{
    cat("\033[31m","Create SeuratObject! \033[0m\n");
    id<-showNotification("Load Data Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    clean(T)
    print(input$inTabset)
    if(input$Selectdata == "Example Data"){
      removePopover(session, "Selectdata")
      print("...............................111")
      progressSweetAlert(
        session = session,
        id = "simulationProgress1",
        title = div("Load Data Runing...,",br(),
                    "It will take some time, please be patient and wait..."),
        display_pct = TRUE,
        value = 1
      )
      Sys.sleep(0.8)
      for (i in 1:20) {
        updateProgressBar(
          session = session,
          id = "simulationProgress1",
          value = i,
          title = "Load Data Runing...",
          total = 99
        )
        Sys.sleep(0.1)
      }
      pbmc<<-readRDS("data/pbmc.RDS")
      for (i in 20:75) {
        updateProgressBar(
          session = session,
          id = "simulationProgress1",
          value = i,
          title = "Load Data Runing...",
          total = 99
        )
        Sys.sleep(0.1)
      }
      print(pbmc)
      sf_data<<-"BSgenome.Hsapiens.UCSC.hg19"
      pbmc[['peaks']] #[[]]用于获取列表中的元素
      print(head(Annotation(pbmc),2))
      print(pbmc)
      for (i in 75:99) {
        updateProgressBar(
          session = session,
          id = "simulationProgress1",
          value = i,
          title = "Load Data Runing...",
          total = 99
        )
        Sys.sleep(0.1)
      }
      print(colnames(pbmc@meta.data))
      print(ls())
      pbmc_exists <<- T
      updateProgressBar(session = session,id = "simulationProgress1",title = "Load Data Runing...",value = 100)
      closeSweetAlert(session = session)
      sendSweetAlert(  
        title = "Done",  
        text = "Load Data Completed",  
        type = "success",
        
      )
      showNotification("Load Data End of run！", type = "message")
      shinyjs::enable(selector = ".sidebar li a[data-value='one']")
      js.1 <- '
            $(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=one]").css("color", "#E6E7E8");
            });
          '
      shinyjs::runjs(js.1)
      updateTabItems(session, "inTabset", selected = "one")
    }
    if(input$Selectdata == "Custom Data"){
      progressSweetAlert(
        session = session,
        id = "simulationProgress2",
        title = div("Load Data Runing...,",br(),
                    "It will take some time, please be patient and wait..."),
        display_pct = TRUE,
        value = 1
      )
      Sys.sleep(0.8)
      for (i in 1:20) {
        updateProgressBar(
          session = session,
          id = "simulationProgress1",
          value = i,
          title = "Load Data Runing...",
          total = 100
        )
        Sys.sleep(0.1)
      }
      print("-----------------------------------222")
      req(input$fileMatrix, input$fileFragments, input$fileFragmentsTbi, input$fileSinglecell)
      # 创建一个临时文件夹
      tempFolder <- tempdir()
      # 将上传的文件保存到临时文件夹中
      f1 <<- file.path(tempFolder, input$fileMatrix$name)
      f2 <<- file.path(tempFolder, input$fileFragments$name)
      f3 <<- file.path(tempFolder, input$fileFragmentsTbi$name)
      f4 <<- file.path(tempFolder, input$fileSinglecell$name)
      print(f4)
      # 将文件移动到临时文件夹中
      file.copy(input$fileMatrix$datapath, f1)
      file.copy(input$fileFragments$datapath, f2)
      file.copy(input$fileFragmentsTbi$datapath, f3)
      file.copy(input$fileSinglecell$datapath, f4)
      print("2")
      tryCatch({
        count(Read10X_h5(f1))
      },error = function(e){
        print(e)
      })
      tryCatch({
        metadata(read.csv(f4,header = TRUE, row.names = 1))
      },error = function(e){
        print(e)
      })
      for (i in 20:35) {
        updateProgressBar(
          session = session,
          id = "simulationProgress2",
          value = i,
          title = "Load Data Runing...",
          total = 100
        )
        Sys.sleep(0.1)
      }
      # updateProgressBar(session = session,id = "simulationProgress2",value = 20)
      tryCatch({
        # counts <- count()
        cat("\033[31m","  Read Singlecell.csv! \033[0m\n");
        print(input$fileFragments$datapath)
        print(input$fileFragmentsTbi$datapath)
        cat("\033[31m","  CreateChromatinAssay1! \033[0m\n");
        chrom_assay <-CreateChromatinAssay(
            counts = count(),
            sep = c(":", "-"),
            # genome = 'hg19',
            fragments = f2,
            min.cells = 10,
            min.features = 200)
        cat("\033[31m","  CreateSeuratObject! \033[0m\n");
        pbmc <<- CreateSeuratObject(counts = chrom_assay, assay = "peaks", meta.data = metadata())
        for (i in 35:50 ){
          updateProgressBar(
            session = session,
            id = "simulationProgress2",
            value = i,
            title = "Load Data Runing...",
            total = 100)
          Sys.sleep(0.1)} 
        cat("\033[31m","  Add Gene annotation to SeuratObject! \033[0m\n");
        if(input$SelectRefer_data=="EnsDb.Hsapiens.v86"){
          print(startsWith(rownames(pbmc@assays[["peaks"]]@data)[1], "chr"))
          if (startsWith(rownames(pbmc@assays[["peaks"]]@data)[1], "chr")) {
            annotations <<- GetGRangesFromEnsDb(ensdb = EnsDb.Hsapiens.v86)
            seqlevels(annotations) <- paste0('chr', seqlevels(annotations))
            sf_data<<-"BSgenome.Hsapiens.UCSC.hg38"
            print("BSgenome.Hsapiens.UCSC.hg38")
          } else {
            annotations <<- GetGRangesFromEnsDb(ensdb = EnsDb.Hsapiens.v86)
            sf_data<<-"BSgenome.Hsapiens.NCBI.GRCh38"
            print("BSgenome.Hsapiens.NCBI.GRCh38")
          }
          genome(annotations) <- unlist(strsplit(sf_data,"\\."))[length(unlist(strsplit(sf_data,"\\.")))]
          Annotation(pbmc) <<- annotations
          print("V86")
        }
        if(input$SelectRefer_data=="EnsDb.Hsapiens.v75" ){
          annotations <<- GetGRangesFromEnsDb(ensdb = EnsDb.Hsapiens.v75)
          seqlevels(annotations) <- paste0('chr', seqlevels(annotations))
          sf_data<<-"BSgenome.Hsapiens.UCSC.hg19"
          print("BSgenome.Hsapiens.UCSC.hg19")
          genome(annotations) <- unlist(strsplit(sf_data,"\\."))[length(unlist(strsplit(sf_data,"\\.")))]
          Annotation(pbmc) <<- annotations
        }
        if(input$SelectRefer_data=="EnsDb.Mmusculus.v79"){
          annotations <<- GetGRangesFromEnsDb(ensdb = EnsDb.Mmusculus.v79)
          seqlevels(annotations) <- paste0('chr', seqlevels(annotations))
          sf_data<<-"BSgenome.Mmusculus.UCSC.mm10"
          print("BSgenome.Mmusculus.UCSC.mm10")
          genome(annotations) <- unlist(strsplit(sf_data,"\\."))[length(unlist(strsplit(sf_data,"\\.")))]
          Annotation(pbmc) <<- annotations
        }
        rm(metadata,chrom_assay,annotations)
        pbmc_exists <<- T
        for (i in 50:65) {
          updateProgressBar(session = session,id = "simulationProgress2",value = i,title = "Load Data Runing...",total = 100)
          Sys.sleep(0.1)
          }
        pbmc <<- NucleosomeSignal(object = pbmc)
        pbmc <<- TSSEnrichment(object = pbmc, fast = FALSE)
        for (i in 65:90) {
          updateProgressBar(
            session = session,id = "simulationProgress2",value = i,title = "Load Data Runing...",total = 100)
            Sys.sleep(0.1)
        }
        pbmc$pct_reads_in_peaks <<- pbmc$peak_region_fragments / pbmc$passed_filters * 100
        for (i in 90:98) {
          updateProgressBar(
            session = session,
            id = "simulationProgress2",
            value = i,
            title = "Load Data Runing...",
            total = 100
          )
          Sys.sleep(0.1)
        }
        pbmc$blacklist_ratio <<- pbmc$blacklist_region_fragments / pbmc$peak_region_fragments
        
        updateProgressBar(session = session, id = "simulationProgress2", title = "Load Data Runing...",value = 100 )
        closeSweetAlert(session = session)
        sendSweetAlert(title = "Done",text = "Load Data Completed",type = "success")
        showNotification("Load Data End of run！", type = "message")
        shinyjs::enable(selector = ".sidebar li a[data-value='one']")
        js.1 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=one]").css("color", "#E6E7E8");});'
        shinyjs::runjs(js.1)
        updateTabItems(session, "inTabset", selected = "one")
      },error = function(e){
        print(e)
      })}
      output$vbox_cell_tab1<-renderValueBox({
        if(!is.null(pbmc)){
          if(!is.null(pbmc@meta.data)){
            cell_counts<<-as.character(length( pbmc@meta.data[["peak_region_fragments"]]))
            valueBox(value =cell_counts,subtitle ="cells uploaded",
                     color = "yellow")}}
      })
    })
  
 #Quality control-----------------------------------------------
  observeEvent(input$submit_Qualitycontrol,{
    # 显示运行中的通知消息
    id<-showNotification("Quality Control Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    output$QCDSPlot <- renderPlot({
      cat("\033[31m","QC:DSPlot \033[0m\n");
      if(input$Selectdata == "Example Data"){
        qcDensityScatterPlot<<-readRDS("data/qcDensityScatterPlot.rds")
      }
      if(input$Selectdata == "Custom Data"){
        qcDensityScatterPlot <<- DensityScatter(pbmc, x = 'nCount_peaks', y = 'TSS.enrichment', log_x = TRUE, quantiles = TRUE)
      }
      return(qcDensityScatterPlot)
    })
    output$QCtssPlot <- renderPlot({
      cat("\033[31m","  QC:TSSPlot \033[0m\n");
      if(input$Selectdata == "Example Data"){
        print("Example data")
        qcTSSPlot<<-readRDS("data/qcTSSPlot.rds")+ NoLegend()
      }
      if(input$Selectdata == "Custom Data"){
        qcTSSPlot <<- TSSPlot(pbmc) + NoLegend()
      }
      print(qcTSSPlot)
    })
    output$QCFragmentPlot <- renderPlot({
      cat("\033[31m","  QC:FragmentHistogram \033[0m\n");
      if(input$Selectdata == "Example Data"){
        qcFragmentHistogram<<-readRDS("data/qcFragmentHistogram.rds")
      }
      if(input$Selectdata == "Custom Data"){
        if(sf_data=="BSgenome.Hsapiens.NCBI.GRCh38"){
          qcFragmentHistogram <<- FragmentHistogram(object = pbmc,region = "1-1-2000000")
          print("GRCH38")
        }
        if(sf_data=="BSgenome.Hsapiens.UCSC.hg19"||sf_data=="BSgenome.Hsapiens.UCSC.hg38"){
          qcFragmentHistogram <<- FragmentHistogram(object = pbmc)
          print("hg")
        }
        if(sf_data=="BSgenome.Mmusculus.UCSC.mm10"){
          qcFragmentHistogram <<- FragmentHistogram(object = pbmc,region = "chr1-1-10000000")
          print("mm10")
        }
      }
      print(qcFragmentHistogram)
    })
    showNotification("Quality Control End of run！", type = "message")
    output$QCVlnPlot <- renderPlot({
      id<-showNotification("Filter Cells Running...", duration = NULL,closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      cat("\033[31m","  QC:VlnPlot \033[0m\n");
      qcVlnPlot <<- VlnPlot(pbmc,features = c('peak_region_fragments','TSS.enrichment', 'blacklist_ratio', 'nucleosome_signal','pct_reads_in_peaks'),
                            pt.size = Seurat:::AutoPointSize(data = pbmc),ncol = 5)
      print(qcVlnPlot)
    })
    updateTabsetPanel(session,"qVinplot",selected = "Before Filter")
    # output$text.cellsremain1<-renderText({
    #   if(!is.null(pbmc)){
    #     if(!is.null(pbmc@meta.data)){
    #       cell_counts<<-length( pbmc@meta.data[["peak_region_fragments"]])
    #       paste(cell_counts,"cells uploaded")}}
    # })
    
    
    shinyjs::enable(selector = ".sidebar li a[data-value='ones']")
    js.2 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=ones]").css("color", "#E6E7E8");});'
    shinyjs::runjs(js.2)
  })
  #FilterCell-----------------------------------------------------------------
  observeEvent(input$submit_FilterCells,{
    id<-showNotification("Filter Cells Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    clean_filter(TRUE)
    showTab(inputId = "qVinplot",target = "After Filter")
    cat("\033[31m","  QC:subset \033[0m\n");
    shinyjs::enable("After Filter")
    pbmcs_1 <<- subset(pbmc, subset = nCount_peaks > input$qcfragmentmin   &   nCount_peaks < input$qcfragmentmax &
                         pct_reads_in_peaks > input$qcpeaksmin   &
                         blacklist_ratio < input$qcratiomax  &
                         nucleosome_signal < input$qcsignalmax  &
                         TSS.enrichment > input$qctssmin  )
    output$QCVlnPlots <- renderPlot({
        input$submit_FilterCells
        cat("\033[31m","  QC:VlnPlots \033[0m\n");
        qcVlnPlots <<- VlnPlot(pbmcs_1,features = c('peak_region_fragments','TSS.enrichment', 'blacklist_ratio', 'nucleosome_signal','pct_reads_in_peaks'),
                               pt.size = Seurat:::AutoPointSize(data = pbmc),ncol = 5,group.by = input$qcGroup )
        print(qcVlnPlots)
        print(pbmcs_1)    
    })
    output$vbox_cell_tab2<-renderValueBox({
        req(pbmcs_1, pbmcs_1@meta.data)
        cell_counts_after<<-as.character(length( pbmcs_1@meta.data[["peak_region_fragments"]]))
        valueBox(value =cell_counts_after,subtitle ="cells after filtering",width = 12,
                 color = "yellow")
    })
    updateTabsetPanel(session,"qVinplot",selected = "After Filter")
    showNotification("Filter Cells End of run！", type = "message")
    shinyjs::enable(selector = ".sidebar li a[data-value='two']")
    js.3 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=two]").css("color", "#E6E7E8");});'
    shinyjs::runjs(js.3)
  })
  
  # Normalization And LDR ----------------------------------------------------------------------
  observeEvent(input$submit_Normalization,{
    clean_Nor(TRUE)
    id<-showNotification("Normalization Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    output$text.featuresremain<- renderUI({return(NULL)})
    cat("\033[31m","Normalization! \033[0m\n");
    cat("\033[31m","  RunTFIDF \033[0m\n");
    pbmcs_1 <<- RunTFIDF(pbmcs_1)  ;
    values$Normalization<<-1
    showNotification("Normalization End of run！", type = "message")
    
 })
  observe({
    if(!is.null(values$Normalization)){
      shinyjs::enable("submit_LDR")
    }else{
      shinyjs::disable("submit_LDR")
    }
  })
  observeEvent(input$submit_LDR,{
    id<-showNotification("Linear Dimensional Reduction Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    clean_LDR(TRUE)
    cat("\033[31m","  FindTopFeatures \033[0m\n");
    q0_value <- input$min.cutoff
    print(q0_value)
    q0_string <- paste("q", q0_value, sep = "")
    print(q0_string)
    pbmcs_1 <<- FindTopFeatures(pbmcs_1, min.cutoff = q0_string)  ;
    features_counts<<-length(pbmcs_1@assays[["peaks"]]@meta.features[["count"]])
    output$text.featuresremain <- renderUI({
      if (!is.null(pbmcs_1)) {
        if (!is.null(pbmcs_1@assays[["peaks"]])) {
          features_counts <- nrow(pbmcs_1@assays[["peaks"]]@counts)
          var_features <- length(pbmcs_1@assays[["peaks"]]@var.features)
          # 用 HTML + CSS 来美化输出
          HTML(paste0(
            "<div style='background-color: #f0f0f0; padding: 5px; 
                 border-radius: 8px; font-weight: bold; 
                 font-size: 15px; display: flex; 
                 justify-content: space-between; 
                 align-items: center;'>",
            "<span style='color: #337ab7; margin-left: 10px;'>Peaks: ", features_counts, "</span>",
            "<span style='color: #d9534f; margin-right: 10px;'>Variable Peaks: ", var_features, "</span>",
            "</div>"
          ))
        }
      }
    })
    
    print(pbmcs_1)
    cat("\033[31m","Linear dimensionality reduction! \033[0m\n");
    cat("\033[31m","  RunSVD \033[0m\n");
    pbmcs_1 <<- RunSVD(pbmcs_1)
    values$deDepthCorPlot <<- DepthCor(pbmcs_1,n=50)+theme(plot.title = element_text(size = rel(1)))
    # 修改 y 轴刻度
    values$deDepthCorPlot <- values$deDepthCorPlot + scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5))
    output$DepthCorPlot <- renderPlotly({
      print(plotly::ggplotly(values$deDepthCorPlot))
    })
    
    #updateActionButton(session, "submit_NLDR", disabled = FALSE)  
    showNotification("Linear Dimensional Reduction End of run！", type = "message")
    })
  observe({
    if(!is.null(values$deDepthCorPlot)){
      shinyjs::enable("submit_NLDR")
    }else{
      shinyjs::disable("submit_NLDR")
      
    }
  })
  
  # None linear dimensionality reduction ----------------------------------
  observeEvent(eventExpr = input$umapdim,{
    if(input$umapdim[1]!=2){
      updateSliderInput(session,"umapdim", label = "UMAP/T-SNE Dims:", min = 0, max = 50,value = c(2,input$umapdim[2]))
    }
  })
  observeEvent(input$reduction_method,{
    if(input$reduction_method!="UMAP"){
      updateRadioButtons(session,'reduction_method',label = "None Linear Dimension Reduction Method：",selected = "T-SNE")
    }
    if(input$reduction_method!="T-SNE"){
      updateRadioButtons(session,'reduction_method',label = "None Linear Dimension Reduction Method：",selected = "UMAP")
    }
  })
  observeEvent(input$submit_NLDR,{
    clean_NLDR(TRUE)
    print(values$submit_Clustering)
    print("-------------------------dd----------------")
    progressSweetAlert( session = session,id = "simulationProgress3",
                        title = div("None Linear Dimensional Reduction Running..,",br(),"It will take some time, please be patient and wait..."),
                        display_pct = TRUE,value = 10)
    Sys.sleep(0.8)
    id<-showNotification("None Linear Dimensional Reduction Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    for (i in 10:25) {
      updateProgressBar(
        session = session,
        id = "simulationProgress3",
        value = i,
        title = "It will take some time, please be patient and wait...",
        total = 100
      )
      Sys.sleep(0.1)
    }
      if(input$reduction_method=="UMAP"){
        cat("\033[31m","Nonlinear dimensionality reduction! \033[0m\n");
        cat("\033[31m","  RunUMAP \033[0m\n");
        pbmcs_1 <<- RunUMAP(object = pbmcs_1, reduction = 'lsi', dims = c(input$umapdim[1]:input$umapdim[2]))
        values$NdpDimPlot<<-DimPlot(pbmcs_1, reduction = "umap",label = FALSE)+NoLegend()
        output$ClusterDimPlot <- renderPlot({
          print(values$NdpDimPlot)
        })
      }
      if(input$reduction_method=="T-SNE"){
        cat("\033[31m","  RunTSNE \033[0m\n");
        pbmcs_1 <<- RunTSNE(object = pbmcs_1, reduction = 'lsi', dims = c(input$umapdim[1]:input$umapdim[2]))  
        values$NdpDimPlot<<-DimPlot(pbmcs_1, reduction = "tsne",label = FALSE)+NoLegend()
        output$ClusterDimPlot <- renderPlot({
          print(values$NdpDimPlot)
        })
      }
      for (i in 25:55) {
        updateProgressBar(
          session = session,
          id = "simulationProgress3",
          value = i,
          title = "It will take some time, please be patient and wait...",
          total = 100
        )
        Sys.sleep(0.1)
      }
      cat("\033[31m","Clustering! \033[0m\n");
      cat("\033[31m","  FindNeighbors \033[0m\n");
      pbmcs_1<<- FindNeighbors(object = pbmcs_1,reduction = 'lsi', dims = c(input$umapdim[1]:input$umapdim[2]))  ;
    for (i in 55:100) {
      updateProgressBar(
        session = session,
        id = "simulationProgress3",
        value = i,
        title = "It will take some time, please be patient and wait...",
        total = 100
      )
      Sys.sleep(0.1)
    }
    closeSweetAlert(session = session)
    sendSweetAlert(session = session,title = "DONE",
                   text = "None Linear Dimensional Reduction completed!",
                   type = "success")
    values$submit_Clustering<<-"start"
    print(values$submit_Clustering)
    print("------------------------de--------------------")
    showNotification("None Linear Dimensional Reduction End of run！", type = "message")
    
  })
  #Clustering---------------------------------------------
  observe({
    print(values$submit_Clustering)
    if(!is.null(values$submit_Clustering)){
      shinyjs::enable("submit_Clustering")
    }else{
      shinyjs::disable("submit_Clustering")
    }
  })
  observeEvent(input$submit_Clustering,{
    id<-showNotification("Clustering Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    clean_Clustering(T)
    cat("\033[31m","  FindClusters \033[0m\n");
    pbmcs<<- FindClusters(object = pbmcs_1, verbose = FALSE, algorithm = as.integer(input$clusteralgorithm),resolution = c(seq(0,2,.1)))
    if(input$reduction_method=="T-SNE"){
    values$dpDimPlot <<- DimPlot(object = pbmcs, label = TRUE,reduction = "tsne",label.size=8,group.by = paste0("peaks_snn_res.",input$resolution))
    }
    if(input$reduction_method=="UMAP"){
      values$dpDimPlot <<- DimPlot(object = pbmcs, label = TRUE,reduction = "umap",label.size=8,group.by = paste0("peaks_snn_res.",input$resolution))
    }
    # + NoLegend()
    output$ClusterDimPlot <- renderPlot({
      print(values$dpDimPlot)
    })
    
    if(!is.null(pbmcs)){
      assaysList <<- as.list(names(pbmcs@assays)); names(assaysList) <- names(pbmcs@assays)
      identsList <<- as.list(colnames(pbmcs@meta.data)[unlist(purrr::map(colnames(pbmcs@meta.data),function(x){is.factor(pbmcs@meta.data[[x]])}))]);
      names(identsList) <- colnames(pbmcs@meta.data)[unlist(purrr::map(colnames(pbmcs@meta.data),function(x){is.factor(pbmcs@meta.data[[x]])}))];
      if(input$AssayRadio3 == "peaks"){
        row_names<-sort(rownames(pbmcs@assays$peaks@counts))
        chromosomes <- unique(sub("-.*", "", grep(".*-", row_names, value = TRUE)))
        output$Chr_num<-renderUI({
          selectInput("Chr_num", label = "Chrx:", choices = chromosomes)
        })
        output$Feature3<-renderUI({
          selectInput("Feature3", label = "Peaks Name:",choices = row_names[grepl(paste0("^", input$Chr_num, "-"), row_names)])
        })
        output$gene_name<-renderUI({
          tagList()
        })
      }
      print(input$Feature3)
      }

    js.4 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=threeone]").css("color", "#E6E7E8");});'
    shinyjs::runjs(js.4)
    shinyjs::enable(selector = ".sidebar li a[data-value='threeone']")
    js.41 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threetwo]").css("color", "#E6E7E8");});'
    shinyjs::runjs(js.41)
    shinyjs::enable(selector = ".sidebar li a[data-value='threetwo']")
    showNotification("Clustering End of run！", type = "message")
    observeEvent(input$resolution,{if(!is.null(pbmcs)){
      ident <- paste0("peaks_snn_res.",input$resolution)
      unique_clusters <<- as.character(unique(FindAllMarkers_result$cluster))
      updateSelectInput(inputId="Ident11",label="ident.1:",choices=levels(pbmcs@meta.data[[ident]]) )
      updateSelectInput(inputId="Ident12",label="ident.2:",choices=levels(pbmcs@meta.data[[ident]]) )
     }
    })
  })
  observe({
    if(!is.null(values$dpDimPlot)){
      shinyjs::enable("submit_GeneActivity")
    }else{
      shinyjs::disable("submit_GeneActivity")
    }
  })
  # Create Gene Activity Matrix---------------------------------------------------------------
  observeEvent(input$submit_GeneActivity,{
    progressSweetAlert( session = session,id = "simulationProgress4",
                        title = div("Gene Activity Matrix Running..,",br(),"It will take some time, please be patient and wait..."),
                        display_pct = TRUE,value = 10)
    Sys.sleep(0.8)
    id<-showNotification("Gene Activity Matrix Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    cat("\033[31m","Create Gene Activity Matrix! \033[0m\n");
    gene.activities <<- GeneActivity(pbmcs)
    for (i in 10:30) {
      updateProgressBar(
        session = session,
        id = "simulationProgress4",
        value = i,
        title = "It will take some time, please be patient and wait...",
        total = 100
      )
      Sys.sleep(0.1)
    }
    pbmcs[['RNA']] <<- CreateAssayObject(counts = gene.activities)   ;rm(gene.activities)
    for (i in 30:50) {
      updateProgressBar(
        session = session,
        id = "simulationProgress4",
        value = i,
        title = "It will take some time, please be patient and wait...",
        total = 100
      )
      Sys.sleep(0.1)
    }
    pbmcs <<- NormalizeData(pbmcs, assay = 'RNA', normalization.method = 'LogNormalize',scale.factor = median(pbmcs$nCount_RNA))
    for (i in 50:80) {
      updateProgressBar(
        session = session,
        id = "simulationProgress4",
        value = i,
        title = "It will take some time, please be patient and wait...",
        total = 100
      )
      Sys.sleep(0.1)
    }
    if(!is.null(pbmcs)){
      assaysList <<- as.list(names(pbmcs@assays)); names(assaysList) <- names(pbmcs@assays)
      identsList <<- as.list(colnames(pbmcs@meta.data)[unlist(purrr::map(colnames(pbmcs@meta.data),function(x){is.factor(pbmcs@meta.data[[x]])}))]);
      names(identsList) <- colnames(pbmcs@meta.data)[unlist(purrr::map(colnames(pbmcs@meta.data),function(x){is.factor(pbmcs@meta.data[[x]])}))];
      updateRadioButtons(inputId="AssayRadio1",label="Assay:",choices=assaysList,selected = "peaks")
      updateRadioButtons(inputId="AssayRadio2",label="Assay:",choices=assaysList,selected = "peaks")
      updateRadioButtons(inputId="AssayRadio3",label="Assay:",choices=assaysList,selected = "peaks",inline = T)
      output$gene_name<-renderUI({selectInput("gene_name", label = "Gene Name:",choices =gene_names_overlapping,selected = "NULL")})
      if(!is.null(input$Feature3)&&!is.null(pbmcs)&&!is.null(pbmcs@assays$RNA)){
        #print(gene_names_overlapping)
        #gene_names_overlapping <<-NULL
        print("----------------df-ds---------------")
        start_end_strings <- strsplit(input$Feature3, '-')
        start_positions <<- as.numeric(sapply(start_end_strings, `[`, 2))- input$range_min * 1000
        end_positions <- as.numeric(sapply(start_end_strings, `[`, 3))+ input$range_min * 1000
        if(!is.na(start_positions)&&!is.na(end_positions)){
          print("----------------df-ds-sf--------------")
          annotation<-pbmcs@assays[["peaks"]]@annotation
          peak_of_interest <- GRanges(seqnames = input$Chr_num, ranges = IRanges(start = start_positions, end = end_positions), strand = "*")
          gene_names_overlapping <<- unique(annotation[subjectHits(findOverlaps(peak_of_interest, annotation))]$gene_name)
          gene_names_overlapping <<- gene_names_overlapping[gene_names_overlapping %in% rownames(pbmcs@assays[["RNA"]]@counts)]
          print(gene_names_overlapping)
          if(length(gene_names_overlapping) == 0){gene_names_overlapping <<-"NULL"}
          if(input$AssayRadio3== "peaks"&&!is.null(pbmcs@assays$RNA)){
            updateSelectizeInput(inputId = "gene_name", label = "Gene Name:", server = TRUE, choices = gene_names_overlapping)}
        }
      }
      
    for (i in 80:100) {
      updateProgressBar(
        session = session,
        id = "simulationProgress4",
        value = i,
        title = "It will take some time, please be patient and wait...",
        total = 100
      )
      Sys.sleep(0.1)
    }
    closeSweetAlert(session = session)
    sendSweetAlert(session = session,title = "DONE",
                   text = "Gene Activity Matrix  completed!",type = "success",)
    showNotification("Gene Activity Matrix End of run！", type = "message")
    updateTabItems(session, "inTabset", selected = "threeone")
    observe({
      if(!is.null(pbmcs[['RNA']])){
        shinyjs::enable(selector = ".sidebar li a[data-value='twos']")
        js.3.1 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=twos]").css("color", "#E6E7E8");});'
        shinyjs::runjs(js.3.1)
      }
      
    })
    }
  })
  

  # FindAllMarkers genes/peaks---------------------------------------------
  observeEvent(input$submit_FindAllMarkers,{
    clean_Findallmarker(TRUE)
    values$submit_ClusterMarkers<-FALSE
    progressSweetAlert(session = session,id = "simulationProgress5",
                       title = div("Find All Markers Running...,",br(),"It will take some time, please be patient and wait..."),
                       display_pct = TRUE,value =10)
    Sys.sleep(0.8)
    id<-showNotification("Find All Markers Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    cat("\033[31m","FindAllMarkers! \033[0m\n");
    assay1<<- input$AssayRadio1;
    DefaultAssay(pbmcs) <- assay1; 
    logfc <- input$LogfcThreshold1;           test <- input$TestUse1
    minPct <- input$MinPct1;                  onlyPos <- input$OnlyPos1
    if(onlyPos=="True"){onlyPos=T};          if(onlyPos=="False"){onlyPos=F}
    Idents(pbmcs) <- paste0("peaks_snn_res.",input$resolution)
    # if(assay=="peaks"){Idents(pbmcs) <- paste0("peaks_snn_res.",input$resolution)}
    # if(assay=="RNA"){Idents(pbmcs) <- paste0("RNA_snn_res.",input$resolution)}
    for (i in 10:25) {
      updateProgressBar(
        session = session,
        id = "simulationProgress5",
        value = i,
        title = "It will take some time, please be patient and wait...",
        total = 100
      )
      Sys.sleep(0.1)
    }
    if(assay1=="peaks"){
      updateSelectizeInput(inputId="FeaturePlot_gene_select",label="Peaks Name:",server = T,
                           choices=sort(rownames(pbmcs@assays$peaks@data)),options = list(maxItems=6),selected =sort(rownames(pbmcs@assays$peaks@data))[1] )
      FindAllMarkers_result<<-FindAllMarkers(pbmcs,logfc.threshold = logfc, assay = "peaks",
                                             min.pct = minPct, only.pos = onlyPos, verbose = T,
                                             test.use = test, latent.vars = 'nCount_peaks')
      FindAllMarkers_result[sapply(FindAllMarkers_result,is.numeric)] <- round(FindAllMarkers_result[sapply(FindAllMarkers_result,is.numeric)],4)
     }
    if(assay1=="RNA"){
      updateSelectizeInput(inputId="FeaturePlot_gene_select",label="Gene Name:",server = T,
                           choices=sort(rownames(pbmcs@assays$RNA@data)),options = list(maxItems=6),selected =sort(rownames(pbmcs@assays$RNA@data))[1])
      FindAllMarkers_result<<-FindAllMarkers(pbmcs, logfc.threshold = logfc, assay = "RNA",
                                             min.pct = minPct, only.pos = onlyPos, verbose = T,
                                             test.use = test)
      
    }
    
    for (i in 25:70) {
      updateProgressBar(
        session = session,
        id = "simulationProgress5",
        value = i,
        title = "It will take some time, please be patient and wait...",
        total = 100
      )
      Sys.sleep(0.1)
    }
    if(nrow(FindAllMarkers_result)!=0){
      FindAllMarkers_result[sapply(FindAllMarkers_result,is.numeric)] <- round(FindAllMarkers_result[sapply(FindAllMarkers_result,is.numeric)],4)
      colnames(FindAllMarkers_result)[7] <- "Marker"
      FindAllMarkers_result <- FindAllMarkers_result[, c("Marker","p_val", "avg_log2FC", "pct.1", "pct.2", "p_val_adj", "cluster")]
         }
    for (i in 70:90) {
      updateProgressBar(
        session = session,
        id = "simulationProgress5",
        value = i,
        title = "It will take some time, please be patient and wait...",
        total = 100
      )
      Sys.sleep(0.1)
    }
   # print(nrow(Markers_result()))
    output$FindAllMarkers_data <- DT::renderDataTable({
      if (nrow(FindAllMarkers_result)==0) {
        return(
          datatable(
            data = data.frame(Message = "No features pass min.pct threshold"),
            rownames = FALSE,selection="none",
            options = list(
              searching=T,ordering=T,
              pageLength = 10,
              autoWidth = TRUE
            ),
            colnames = c("Marker","p_val", "avg_log2FC", "pct.1", "pct.2", "p_val_adj", "cluster"),
            escape = FALSE
            # rownames = FALSE
          )
        )
      }else{
        data<-data.table(FindAllMarkers_result)
        if (input$Ident13!= "All") {
           data<-data[data$cluster == input$Ident13,]
        }
        if (!is.null(input$P_val_1) &&input$P_val_1 != 0.05) {
          data <- data[data$p_val_adj<input$P_val_1,]
        }
        if (!is.null(input$P_val_1) &&input$P_val_1 == 0.05) {
          data<-data[data$p_val_adj<0.05,]
        }
        if (!is.null(input$pct_1) && input$pct_1 != 0.7) {
          data <- data[data$pct.1 >input$pct_1,]
        }
        datatable(
        data, 
        rownames =FALSE,
        options = list(searching=T,ordering=T,
                       columnDefs = list(list(width="100px",targets = 1:6),list(className = 'dt-left',targets = 1:6),list(targets = 0, width="210px")),
                       lengthMenu=c(5,10,15,25,50,100),pageLength = 5)
        )}
      })
    for (i in 90:100) {
      updateProgressBar(
        session = session,
        id = "simulationProgress5",
        value = i,
        title = "It will take some time, please be patient and wait...",
        total = 100
      )
      Sys.sleep(0.1)
    }
    closeSweetAlert(session = session)
    if(nrow(FindAllMarkers_result)!=0){
      sendSweetAlert(session = session,title = "DONE",
                     text = "Find All Markers is completed!",type = "success")
    }
    if(nrow(FindAllMarkers_result)==0){
      sendSweetAlert(session = session,title = NULL,type="info",
                     text = "No features pass the min.pct threshold and logfc.threshold, you can adjust the thresholds accordingly." )}
    showNotification("Find All Markers End of run！", type = "message")
    updateTabsetPanel(session,"GO_setting1",selected = "One vs Rest")
  #模块解锁------------------------------------- 
    shinyjs::show("featurePlotBox")
    observe({
      if(!is.null(pbmcs@assays$RNA)){
        js.3.1 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=twos]").css("color", "#E6E7E8");});'
        shinyjs::runjs(js.3.1)
        shinyjs::enable(selector = ".sidebar li a[data-value='twos']")
      }
      
    })
    observe({
      if(assay1=="peaks"){
        js.5 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sixone]").css("color", "#E6E7E8");
        });
      '
        shinyjs::runjs(js.5)
        shinyjs::enable(selector = ".sidebar li a[data-value='sixone']")
      }
      if(assay1=="RNA"){
        shinyjs::disable(selector = ".sidebar li a[data-value='sixone']")
        js.51 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sixone]").css("color", "#637584");
        });
      '
        shinyjs::runjs(js.51)
        shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
        js.8 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#808080");
        });
      '
        shinyjs::runjs(js.8)
        shinyjs::disable(selector = ".sidebar li a[data-value='six']")
        js.511 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#808080");
        });
      '
        shinyjs::runjs(js.511)
      }
    })
    shinyjs::enable(selector = ".sidebar li a[data-value='threetwo']")
    js.41 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=threetwo]").css("color", "#E6E7E8");
        });
      '
    shinyjs::runjs(js.41)
    shinyjs::enable(selector = ".sidebar li a[data-value='four']")
    js.6 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=four]").css("color", "#E6E7E8");
        });
      '
    shinyjs::runjs(js.6)
    shinyjs::enable(selector = ".sidebar li a[data-value='five']")
    js.7 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=five]").css("color", "#E6E7E8");
        });
      '
    shinyjs::runjs(js.7)
    observeEvent(input$resolution,{if(!is.null(pbmcs)){
      ident <- paste0("peaks_snn_res.",input$resolution)
      unique_clusters <<- as.character(unique(FindAllMarkers_result$cluster))
      updateSelectInput(inputId="Ident13",label="Select Cluster:",choices=c("All",sort(unique_clusters)))
      updateSelectInput(inputId="idents_selected",label="idents:",choices=sort(unique_clusters))
      updateSelectInput(inputId='Ident_motif',label = "Select Cluster:", choices = sort(unique_clusters))
      updateSelectInput(inputId='idents_selected1',label = "Select Cluster:", choices = sort(unique_clusters))
      
        
    }
    })
    
  })
  
  
  #FeaturePlot--------------------------------------------------------
  observeEvent(input$submit_FeaturePlot,{
    dbFeaturePlot<<-FeaturePlot(object = pbmcs,features = c(input$FeaturePlot_gene_select),
                               pt.size = 0.1, max.cutoff = 'q95')
    output$FeaturePlot <- renderPlot({print(dbFeaturePlot)})
  })
 

  #Find Cluster Marker----------------------------------------------
  observeEvent(input$submit_FindMarkers,{
    clean_Findmarker(T)
    id<-showNotification("Find Idents Markers Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    cat("\033[31m","FindMarkers! \033[0m\n");
    assay2 <<- input$AssayRadio2;
    logfc <- input$LogfcThreshold2;           test <- input$TestUse2
    minPct <- input$MinPct2;                  onlyPos <- input$OnlyPos2
    if(onlyPos=="True"){onlyPos=T};          if(onlyPos=="False"){onlyPos=F}
    ident1 <<- input$Ident11;                 ident2 <<- input$Ident12
    if(ident1=="NULL"){ident1=NULL};         if(ident2=="NULL"){ident2=NULL}
    DefaultAssay(pbmcs) <- assay2;        Idents(pbmcs) <- paste0("peaks_snn_res.",input$resolution)
    
    if(ident1==ident2){
      cat("\033[31m","FindMarkers! \033[0m\n");
      sendSweetAlert(session = session,title = "Warning",
                     text = "ident.1 and ident.2 need to choose different clusters",
                     type = "warning" )
    }else{
      if(assay2=="peaks"){FindMarkers_result1 <<- FindMarkers(pbmcs,  ident.1 = ident1,ident.2 = ident2, logfc.threshold = logfc,
                                                             min.pct = minPct, only.pos = onlyPos, verbose = T,
                                                             test.use = test, latent.vars = 'nCount_peaks')
                        feature_name_list<<-rownames(pbmcs@assays$peaks@data)
                      }
      if(assay2=="RNA"){FindMarkers_result1 <<- FindMarkers(pbmcs, ident.1 = ident1,ident.2 = ident2, logfc.threshold = logfc,assay = "RNA",
                                                           min.pct = minPct, only.pos = onlyPos, verbose = T,
                                                           test.use = test,latent.vars = 'nCount_peaks')
                        feature_name_list<<-rownames(pbmcs@assays$RNA@data)
                         
      }
      FindMarkers_result1[sapply(FindMarkers_result1,is.numeric)] <- round(FindMarkers_result1[sapply(FindMarkers_result1,is.numeric)],4)
      FindMarkers_result1$Marker<-rownames(FindMarkers_result1)
      FindMarkers_result<- FindMarkers_result1[, c("Marker","p_val", "avg_log2FC", "pct.1", "pct.2", "p_val_adj")]
      
      output$CompareMarkers_data <- DT::renderDataTable({
        datatable(data = FindMarkers_result,selection = 'single',rownames = FALSE,
                  options = list(searching=T,ordering=T,
                                 columnDefs = list(list(width="100px",targets = 1:5),list(className = 'dt-left',targets = 1:5),list(targets = 0, width="210px")),
                                 lengthMenu=c(5,10,15,25,50,100),pageLength = 5))
      })
      if(is.null(FindMarkers_result1) && is.data.frame(FindMarkers_result1) && nrow(FindMarkers_result1) == 0){
        sendSweetAlert(session = session,title = NULL,type="info",
                       text = "No features pass the min.pct threshold and logfc.threshold, you can adjust the thresholds accordingly." )
        }
      showNotification("Find Idents Markers End of run！", type = "message")
    }
    output$marker_information <- renderUI({
      if (!is.null(pbmcs)) {
        if (!is.null(FindMarkers_result1)) {
          print(ident1)
          print(ident2)
          # 用 HTML + CSS 来美化输出
          HTML(paste0(
            "<div style='background-color: #f0f0f0; padding: 5px; 
                 border-radius: 8px; font-weight: bold; 
                 font-size: 15px; display: flex; 
                 justify-content: space-between; 
                 align-items: center;'>",
            "<span style='color: #337ab7; margin-left: 10px;'>Ident1: ", ident1, "</span>",
            "<span style='color: #337ab7; margin-left: 10px;'>vs</span>",
            "<span style='color: #d9534f; margin-right: 10px;'>Ident2: ", ident2, "</span>",
            "</div>"
          ))
        }
      }
    })
   
      if(!is.null(pbmcs) && !is.null(input$Ident11) && !is.null(input$Ident12)){
        updateSelectInput(inputId="Ident41",label="Select Cluster:",choices=c(input$Ident11,input$Ident12),selected=input$Ident11 )
        updateSelectInput(inputId="idents_selected2",label="Select Cluster:",choices=c(input$Ident11,input$Ident12),selected=input$Ident11 )
        
      } 
      
    showTab(inputId = "GO_Plot",target = "One vs One")
    showTab(inputId = "GO_Table",target = "One vs One")
    showTab(inputId = "GO_setting1",target = "One vs One")
    observe({
      print("relock------------------------------")
      # 只有在 enriched.motifs1 和 FindMarkers_result1 都不为 NULL 时才执行
      if (!is.null(FindMarkers_result1)) {
        if (nrow(FindMarkers_result1) != 0) {
          if (assay2 == "peaks") {
            shinyjs::enable(selector = ".sidebar li a[data-value='sixtwo']")
            js.51 <- '
        $(document).ready(function(){
          $("a[data-value=sixtwo]").css("color", "#E6E7E8");
        });
        '
            shinyjs::runjs(js.51)
          } else if (assay2 == "RNA") {
            shinyjs::disable(selector = ".sidebar li a[data-value='sixtwo']")
            js.51 <- '
        $(document).ready(function(){
          $("a[data-value=sixtwo]").css("color", "#637584");
        });
        '
            shinyjs::runjs(js.51)
            shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
            js.8 <- '
        $(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#637584");
        });
      '
            shinyjs::runjs(js.8)
          }
        } 
      }
    })
  })
  observe({
    selected_row <<- input$CompareMarkers_data_rows_selected
    print(selected_row)
    if(is.null(selected_row)){
      output$FM_VinPlot <- renderPlot({NULL})
      output$FM_FeaturePlot<-renderPlot({NULL })
    }else{
      feature_name <<- rownames(FindMarkers_result1[selected_row,])  
      print(paste("Selected feature:", feature_name))
      if (feature_name %in% feature_name_list) {
        FM_VinPlot<<- VlnPlot(
          object = pbmcs,
          features = feature_name,
          pt.size = 0.1,
          idents = c(ident1, ident2)
        )
        FM_FeaturePlot<<-FeaturePlot(
          object = pbmcs,
          features =  feature_name,
          pt.size = 0.1
        )
        output$FM_VinPlot <- renderPlot({print(FM_VinPlot)})
        output$FM_FeaturePlot<-renderPlot({ print(FM_FeaturePlot) })
      } else {
        print("Feature not found in Seurat object.")
        
      }
    }
  })
  
  # Cell Annotation------------------------------------------------------------------------------------
  output$Reference_select<-renderUI({
    type<-if(input$reference_type=="Human"){
      list("DatabaseImmuneCell.se","BlueprintEncode","HumanPrimaryCellAtlas","MonacoImmune","NovershternHematopoietic")
    }else{
      list("ImmGen","MouseRNAseq")
    }
    selectizeInput("reference_data",label="Reference Datasets:",
                   choices=type,selected="DatabaseImmuneCell.se")
  })
  observeEvent(input$submit_singleR,{
    id<-showNotification("Cell annotation Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    clean_annotation(TRUE)
    cat("cell annotation")
    ident<-paste0("peaks_snn_res.",input$resolution)
    if(input$reference_data=="DatabaseImmuneCell.se"){
      load("data/DatabaseImmuneCell.se.Rdata")
      CELL <- SingleR(test=as.matrix(pbmcs@assays$RNA@counts),       #输入表达矩阵
                      ref=DatabaseImmuneCell.se,                                #参考数据
                      labels=DatabaseImmuneCell.se$label.fine,
                      clusters= pbmcs@meta.data[[ident]])
    }
    if(input$reference_data=="BlueprintEncode"){
      load("data/BlueprintEncode.Rdata")
      CELL <- SingleR(test=as.matrix(pbmcs@assays$RNA@counts),       #输入表达矩阵
                      ref=BlueprintEncode,                                #参考数据
                      labels=BlueprintEncode$label.fine,
                      clusters= pbmcs@meta.data[[ident]])
    }
    if(input$reference_data=="HumanPrimaryCellAtlas"){
      load("data/HumanPrimaryCellAtlas.Rdata")
      CELL <- SingleR(test=as.matrix(pbmcs@assays$RNA@counts),       #输入表达矩阵
                      ref=HumanPrimaryCellAtlas,                                #参考数据
                      labels=HumanPrimaryCellAtlas$label.fine,
                      clusters= pbmcs@meta.data[[ident]])
    }
    if(input$reference_data=="MonacoImmune"){
      load("data/MonacoImmune.Rdata")
      CELL <- SingleR(test=as.matrix(pbmcs@assays$RNA@counts),       #输入表达矩阵
                      ref=MonacoImmune,                                #参考数据
                      labels=MonacoImmune$label.fine,
                      clusters= pbmcs@meta.data[[ident]])
    }
    if(input$reference_data=="NovershternHematopoietic"){
      load("data/NovershternHematopoietic.Rdata")
      CELL <- SingleR(test=as.matrix(pbmcs@assays$RNA@counts),       #输入表达矩阵
                      ref=NovershternHematopoietic,                                #参考数据
                      labels=NovershternHematopoietic$label.fine,
                      clusters= pbmcs@meta.data[[ident]])
    }
    if(input$reference_data=="ImmGen"){
      load("data/ImmGen.Rdata")#小鼠
      CELL <- SingleR(test=as.matrix(pbmcs@assays$RNA@counts),       #输入表达矩阵
                      ref=ImmGen,                                #参考数据
                      labels=ImmGen$label.fine,
                      clusters= pbmcs@meta.data[[ident]])
    }
    if(input$reference_data=="MouseRNAseq"){
      load("data/MouseRNAseq.Rdata")#小鼠
      CELL <- SingleR(test=as.matrix(pbmcs@assays$RNA@counts),       #输入表达矩阵
                      ref=MouseRNAseq,                                #参考数据
                      labels=MouseRNAseq$label.fine,
                      clusters= pbmcs@meta.data[[ident]])
    }
    cells<<-data.frame(CELL[,ncol(CELL):1])
    print(head(cells))
    output$singleR_data <- DT::renderDataTable({
      datatable(
        data = cells,
        options = list(
          dom='ftipr',
          pageLength = 5,
          autoWidth = TRUE,
          columnDefs = list(list(targets = 6, width = "210px")),
          scrollX = TRUE
        ))})
    cell_id<<-cells$pruned.labels
    names(cell_id) <<- levels(pbmcs@meta.data[[ident]])
    print(cell_id)
    cat("     cdass  ")
    Idents(pbmcs) <- ident
    pbmcs <- Seurat::RenameIdents(pbmcs, cell_id)
    pbmcs$ident <<- Idents(pbmcs)
    dDimPlot<<-DimPlot(object = pbmcs,label.size=5,label = TRUE)
    output$DimPlot <- renderPlot({
      print(dDimPlot)
    })
    showNotification("Cell annotation End of run！", type = "message")
  })
  
  
  #Calling peak------------------------------------------------------
  observe({                          #chr片段改变时更新Gene Name
        if(!is.null(input$Feature3)&&!is.null(pbmcs)&&!is.null(pbmcs@assays$RNA)){
          print("----------------df-ds---------------")
          start_end_strings <- strsplit(input$Feature3, '-')
          start_positions <<- as.numeric(sapply(start_end_strings, `[`, 2))- input$range_min * 1000
          end_positions <- as.numeric(sapply(start_end_strings, `[`, 3))+ input$range_min * 1000
          if(!is.na(start_positions)&&!is.na(end_positions)){
            print("----------------df-ds-sf--------------")
            annotation<-pbmcs@assays[["peaks"]]@annotation
            peak_of_interest <- GRanges(seqnames = input$Chr_num, ranges = IRanges(start = start_positions, end = end_positions), strand = "*")
            gene_names_overlapping <<- unique(annotation[subjectHits(findOverlaps(peak_of_interest, annotation))]$gene_name)
            gene_names_overlapping <<- gene_names_overlapping[gene_names_overlapping %in% rownames(pbmcs@assays[["RNA"]]@counts)]
            print(gene_names_overlapping)
            if(length(gene_names_overlapping) == 0){gene_names_overlapping <<-"NULL"}
            if(input$AssayRadio3== "peaks"&&!is.null(pbmcs@assays$RNA)){
              updateSelectizeInput(inputId = "gene_name", label = "Gene Name:", server = TRUE, choices = gene_names_overlapping)}
          }
        }
  })
  observeEvent(input$AssayRadio3,{
    if(input$AssayRadio3 == "peaks"){
      print("--------------fd---------------")
      if (!is.null(pbmcs)&&!is.null(pbmcs@assays$peaks) && !is.null(pbmcs@assays$peaks@counts)) {  
         if(is.null(pbmcs@assays$RNA)){
          print("O----------------------------------")
          row_names<-sort(rownames(pbmcs@assays$peaks@counts))
          chromosomes <- unique(sub("-.*", "", grep(".*-", row_names, value = TRUE)))
          output$Chr_num<-renderUI({
            selectInput("Chr_num", label = "Chrx:", choices = chromosomes)
          })
          output$Feature3<-renderUI({
            selectInput("Feature3", label = "Peaks Name:",choices = row_names[grepl(paste0("^", input$Chr_num, "-"), row_names)])
          })
          output$gene_name<-renderUI({
            tagList()
          })
         }
        if(!is.null(pbmcs@assays$RNA)){
          print("O-----------------------ef-----------")
            row_names<-sort(rownames(pbmcs@assays$peaks@counts)) ;    chromosomes <- unique(sub("-.*", "", grep(".*-", row_names, value = TRUE)))
            if(length(gene_names_overlapping) == 0){gene_names_overlapping <<-"NULL"}
            output$Chr_num<-renderUI({selectInput("Chr_num", label = "Chrx:", choices = chromosomes)})
            output$Feature3<-renderUI({selectInput("Feature3", label = "Peaks Name:",choices = row_names[grepl(paste0("^", input$Chr_num, "-"), row_names)])})
            output$gene_name<-renderUI({selectInput("gene_name", label = "Gene Name:",choices =gene_names_overlapping,selected = "NULL")})
          }
        }
      }
    if(input$AssayRadio3 == "RNA"){
      if (!is.null(pbmcs)&&!is.null(pbmcs@assays$RNA) && !is.null(pbmcs@assays$RNA@data)) {
        output$gene_name<-renderUI({selectInput("gene_name", label = "Gene Name:",choices =sort(rownames(pbmcs@assays$RNA@data)))})
        output$Chr_num<-renderUI({tagList()})
        output$Feature3<-renderUI({tagList()})
        }
      }
  })
  observeEvent(input$submit_GenomicRegionsPlot,{
    # cleanValuebox_four(TRUE)
    id<-showNotification("Plotting genomic regions Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    cat("\033[31m","Plotting genomic regions! \033[0m\n");
    print(input$gene_name)
      if(!is.null(cells)){
        if(input$AssayRadio3=="peaks"){
            if(is.null(pbmcs@assays$RNA)){
              grGenomicRegionsPlot <<- CoveragePlot(
                object = pbmcs,  
                region = isolate(input$Feature3),
                assay ="peaks",
                group.by ="ident",
                extend.upstream = abs(isolate(input$range_max))*1000, tile=T,
                extend.downstream = abs(isolate(input$range_min))*1000,
                tile.size = isolate(input$tile_size), ymax = isolate(input$ymax))
            }
            if(!is.null(pbmcs@assays$RNA)){
              if (input$gene_name=="NULL") {
                feature_name <- NULL
              } else {
                feature_name <- isolate(input$gene_name)
              }
              print(feature_name )
              grGenomicRegionsPlot <<- CoveragePlot(
                object = pbmcs, 
                region = isolate(input$Feature3),
                features = feature_name,
                assay ="peaks",
                expression.assay = "RNA",
                expression.slot = "data",
                group.by ="ident",
                tile=T,
                extend.upstream =abs(isolate(input$range_max))*1000,
                extend.downstream = abs(isolate(input$range_min))*1000,
                tile.size = isolate(input$tile_size), ymax = isolate(input$ymax))
            }}
          if(input$AssayRadio3=="RNA"){
            grGenomicRegionsPlot <<- CoveragePlot(
              object = pbmcs, 
              region = isolate(input$gene_name),
              features = isolate(input$gene_name),
              assay ="peaks",
              expression.assay = "RNA",
              expression.slot = "data",
              group.by ="ident",
              tile=T,
              extend.upstream =abs(isolate(input$range_max))*1000,
              extend.downstream = abs(isolate(input$range_min))*1000,
              tile.size = isolate(input$tile_size), ymax = isolate(input$ymax))
          }
        output$GenomicRegionsPlot <-renderPlot({
          print(grGenomicRegionsPlot)
        })
      }
      if(is.null(cells)){
          if(input$AssayRadio3=="peaks"){
            if(is.null(pbmcs@assays$RNA)){
              grGenomicRegionsPlot <<- CoveragePlot(
                object = pbmcs,  
                region = isolate(input$Feature3),
                assay ="peaks",
                group.by =isolate(paste0("peaks_snn_res.",input$resolution)),
                extend.upstream = abs(isolate(input$range_max))*1000, tile=T,
                extend.downstream = abs(isolate(input$range_min))*1000,
                tile.size = isolate(input$tile_size), ymax = isolate(input$ymax))
            }
              if(!is.null(pbmcs@assays$RNA)){
                if (input$gene_name=="NULL") {
                  feature_name <- NULL
                } else {
                  feature_name <- isolate(input$gene_name)
                }
              print(feature_name )
              grGenomicRegionsPlot <<- CoveragePlot(
                object = pbmcs, 
                region = isolate(input$Feature3),
                features = feature_name,
                assay ="peaks",
                expression.assay = "RNA",
                expression.slot = "data",
                group.by =isolate(paste0("peaks_snn_res.",input$resolution)),
                tile=T,
                extend.upstream =abs(isolate(input$range_max))*1000,
                extend.downstream = abs(isolate(input$range_min))*1000,
                tile.size = isolate(input$tile_size), ymax = isolate(input$ymax))
            }}
          if(input$AssayRadio3=="RNA"){
            grGenomicRegionsPlot <<- CoveragePlot(
              object = pbmcs, 
              region = isolate(input$gene_name),
              features = isolate(input$gene_name),
              assay ="peaks",
              expression.assay = "RNA",
              expression.slot = "data",
              group.by =isolate(paste0("peaks_snn_res.",input$resolution)),
              tile=T,
              extend.upstream =abs(isolate(input$range_max))*1000,
              extend.downstream = abs(isolate(input$range_min))*1000,
              tile.size = isolate(input$tile_size), ymax = isolate(input$ymax))
          }
    
        output$GenomicRegionsPlot <-renderPlot({
          print(grGenomicRegionsPlot)
        })
        
      }
      showNotification("Plotting genomic regions End of run！", type = "message")

    
  })
  observe({
    print("dsfj------------------dsg------")
    Select_ident<-input$Ident_motif;  
    Pvalue <- input$Pvalue
    if(!is.null(FindAllMarkers_result)){
    MotifMarkers1(FindAllMarkers_result[FindAllMarkers_result$cluster==Select_ident&
                                          FindAllMarkers_result$p_val_adj<Pvalue,])
    MotifTopMarkers1<<-MotifMarkers1()
    marker_list_str <- paste(MotifTopMarkers1$gene, collapse = "\n")
    updateTextAreaInput(session,"Marker_list1","Marker List:",
                        value = marker_list_str)
    }
  })
  
  # GO analysis---------------------------------------------------
  
  observe({
    print("go_dede")
    selected_idents1 <- input$idents_selected1
    print(selected_idents1)
    if(length(FindAllMarkers_result)!=0){
      if (!is.null(selected_idents1)) {
        open_ident<<- rownames(FindAllMarkers_result[FindAllMarkers_result$p_val_adj < 0.05& FindAllMarkers_result$cluster==selected_idents1, ])
        print(paste("open_ident:", open_ident))
      } 
    }
    if(length(open_ident) != 0 && assay1=="peaks" ){
      open_ident1<<- ClosestFeature(pbmcs, regions = open_ident)
    }
    if(length(open_ident) != 0 && assay1=="RNA"){
      open_ident1<<- bitr(open_ident,fromType = "SYMBOL",toType = "ENSEMBL",OrgDb = org.Hs.eg.db)
      colnames(open_ident1)<<-c("gene_name","gene_id")
    }
    print(paste("open_ident1:",open_ident1))
    open_ident11 <<- paste(open_ident, collapse = "\n")
    updateTextAreaInput(session, "Marker_list3", value = open_ident11)
    print(open_ident11)
    
  })
  observe({
    selected_idents2 <- input$idents_selected2
    print(selected_idents2)
    if(length(FindMarkers_result1)!=0){
      if (!is.null(selected_idents2)) {
        if(selected_idents2==input$Ident11){
          open_ident2<<- rownames(FindMarkers_result1[FindMarkers_result1$p_val_adj < 0.05& FindMarkers_result1$avg_log2FC > 1, ])
        }
        if(selected_idents2==input$Ident12){
          open_ident2<<- rownames(FindMarkers_result1[FindMarkers_result1$p_val_adj < 0.05& FindMarkers_result1$avg_log2FC < 1, ])
        }
      }}
    if(length(open_ident2)!=0 && assay2=="peak"){
      open_ident21 <<- ClosestFeature(pbmcs, regions = open_ident2)
    }
    if(length(open_ident2)!=0 && assay2=="RNA"){
      open_ident21<<- bitr(open_ident2,fromType = "SYMBOL",toType = "ENSEMBL",OrgDb = org.Hs.eg.db)
      colnames(open_ident21)<<-c("gene_name","gene_id")
    }
    open_ident22 <- paste(open_ident2, collapse = "\n")
    updateTextAreaInput(session, "Marker_list4", value = open_ident22)
  })
  observeEvent(input$submit_GO_analysis,{
    id<-showNotification("GO analysis Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    cat("\033[31m","  GO analysis! \033[0m\n");
    DefaultAssay(pbmcs) <- "peaks"
    if(Species_type=="Homo"){org.db<-org.Hs.eg.db}
    if(Species_type=="Mus"){org.db<-org.Mm.eg.db}
    print(input$Marker_list3)
    if(input$GO_setting1=="One vs Rest"){
      if(!is.null(input$Marker_list3)&&nzchar(input$Marker_list3)){
        clean_GO_analysis1(TRUE)
        progressSweetAlert(session = session,id = "simulationProgress5",
                           title = div("GO Analysis Running...,",br(),"It will take some time, please be patient and wait..."),
                           display_pct = TRUE,value =10);Sys.sleep(0.8)
        for (i in 10:25) {updateProgressBar(session = session,id = "simulationProgress5",value = i,
                                            title = "It will take some time, please be patient and wait...",total = 100);Sys.sleep(0.1)}
        closest_genes_ident<-strsplit(input$Marker_list3, "\n")[[1]]
        if(input$gene_id_type=="ENSEMBL"){
          if(assay1=="peaks"){
            closest_genes_ident<-ClosestFeature(pbmcs,region=closest_genes_ident)
            gene1<-closest_genes_ident$gene_id}
          if(assay1=="RNA"){
            closest_genes_ident<- bitr(closest_genes_ident,fromType = "SYMBOL",toType = "ENSEMBL",OrgDb = org.Hs.eg.db)
            gene1<-closest_genes_ident$ENSEMBL}
        }
        if(input$gene_id_type=="SYMBOL"){
          if(assay1=="peaks"){
            closest_genes_ident<-ClosestFeature(pbmcs,region=closest_genes_ident)
            gene1<-closest_genes_ident$gene_name}
          if(assay1=="RNA"){
            closest_genes_ident<- bitr(closest_genes_ident,fromType = "SYMBOL",toType = "ENSEMBL",OrgDb = org.Hs.eg.db)
            gene1<-closest_genes_ident$SYMBOL}
        }
        for (i in 25:50) {updateProgressBar(session = session,id = "simulationProgress5",value = i,
                                            title = "It will take some time, please be patient and wait...",total = 100);Sys.sleep(0.1)}
        ident1_ego1 <<- enrichGO(gene = gene1,
                                 keyType =input$gene_id_type,
                                 OrgDb = org.db,
                                 ont = "ALL",
                                 pAdjustMethod = "BH",
                                 pvalueCutoff = input$pvalueCutoff,
                                 qvalueCutoff = input$qvalueCutoff,
                                 minGSSize = input$minGSSize,
                                 maxGSSize = input$maxGSSize,
                                 readable = TRUE)
        for (i in 50:75) {updateProgressBar(session = session,id = "simulationProgress5",value = i,
                                            title = "It will take some time, please be patient and wait...",total = 100);Sys.sleep(0.1) }
        if(!is.null(ident1_ego1)&& nrow(as.data.frame(ident1_ego1))!=0){
          output$GO_data1 <- DT::renderDataTable({
            datatable(data =as.data.frame(ident1_ego1), rownames = FALSE,
                      options = list(pageLength = 10,
                                     autoWidth = TRUE,
                                     columnDefs = list(list(className = 'dt-left',targets = "_all"),list(targets = 2, width="420px")),
                                     scrollX = TRUE))
          })
          GO_Plot1<<-plot_GO_bar(ident1_ego1)
          output$GO_Plot1 <- renderPlot({print(GO_Plot1)})
          for (i in 75:100) {updateProgressBar(session = session,id = "simulationProgress5",
                                               value = i,title = "It will take some time, please be patient and wait...",total = 100);Sys.sleep(0.1)}
          closeSweetAlert(session = session)
          showNotification("GO analysis End of run！", type = "message")
        }else{
          output$GO_data1 <- DT::renderDataTable({NULL})
          output$GO_Plot1 <- renderPlot({NULL})
          sendSweetAlert(session = session,title = "Warning",
                         text = "GO enrichment analysis returned no results. It is recommended to try adjusting the related parameters.",
                         type = "warning" )}
      }else{
        sendSweetAlert(session = session,title = "Warning",
                       text = "Markers is empty. It is recommended to try adjusting the related parameters.",
                       type = "warning" )}
    }
    if(input$GO_setting1=="One vs One"){
      if(!is.null(input$Marker_list4)&&nzchar(input$Marker_list4)){
        clean_GO_analysis2(TRUE)
        progressSweetAlert(session = session,id = "simulationProgress5",
                           title = div("GO Analysis Running...,",br(),"It will take some time, please be patient and wait..."),
                           display_pct = TRUE,value =10);Sys.sleep(0.8)
        for (i in 10:25) {updateProgressBar(session = session,id = "simulationProgress5",value = i,
                                            title = "It will take some time, please be patient and wait...",total = 100);Sys.sleep(0.1)}
        
        closest_genes_ident<-strsplit(input$Marker_list4, "\n")[[1]]
        if(input$gene_id_type=="ENSEMBL"){
          if(assay2=="peaks"){
            closest_genes_ident<-ClosestFeature(pbmcs,region=closest_genes_ident)
            gene2<-closest_genes_ident$gene_id}
          if(assay2=="RNA"){
            closest_genes_ident<- bitr(closest_genes_ident,fromType = "SYMBOL",toType = "ENSEMBL",OrgDb = org.Hs.eg.db)
            gene2<-closest_genes_ident$ENSEMBL}
        }
        if(input$gene_id_type=="SYMBOL"){
          if(assay2=="peaks"){
            closest_genes_ident<-ClosestFeature(pbmcs,region=closest_genes_ident)
            gene2<-closest_genes_ident$gene_name}
          if(assay2=="RNA"){
            closest_genes_ident<- bitr(closest_genes_ident,fromType = "SYMBOL",toType = "ENSEMBL",OrgDb = org.Hs.eg.db)
            gene2<-closest_genes_ident$SYMBOL}
        }
        for (i in 25:50) {updateProgressBar(session = session,id = "simulationProgress5",value = i,
                                            title = "It will take some time, please be patient and wait...",total = 100);Sys.sleep(0.1)}
        ident1_ego2 <<- enrichGO(gene = gene2,
                                 keyType =input$gene_id_type,
                                 OrgDb = org.db,
                                 ont = "ALL",
                                 pAdjustMethod = "BH",
                                 pvalueCutoff = input$pvalueCutoff,
                                 qvalueCutoff = input$qvalueCutoff,
                                 minGSSize = input$minGSSize,
                                 maxGSSize = input$maxGSSize,
                                 readable = TRUE)
        for (i in 50:75) {updateProgressBar(session = session,id = "simulationProgress5",
                                            value = i, title = "It will take some time, please be patient and wait...",total = 100);Sys.sleep(0.1)}
        updateTabsetPanel(session,"GO_Plot",selected = "One vs One")
        updateTabsetPanel(session,"GO_Table",selected = "One vs One")
        if(!is.null(ident1_ego2)&& nrow(as.data.frame(ident1_ego2))!=0){
          output$GO_data2 <- DT::renderDataTable({
            datatable(data =as.data.frame(ident1_ego2), rownames = FALSE,
                      options = list(pageLength = 10,
                                     autoWidth = TRUE,
                                     columnDefs = list(list(className = 'dt-left',targets = "_all"),list(targets = 2, width="420px")),
                                     scrollX = TRUE))})
          GO_Plot2 <<-plot_GO_bar(ident1_ego2)
          output$GO_Plot2 <- renderPlot({print(GO_Plot2)})
          for (i in 75:100) {updateProgressBar(session = session,id = "simulationProgress5",
                                               value = i,title = "It will take some time, please be patient and wait...",total = 100);Sys.sleep(0.1)}
          closeSweetAlert(session = session)
          showNotification("GO analysis End of run！", type = "message")
        }else{
          output$GO_data2 <- DT::renderDataTable({NULL})
          output$GO_Plot2 <- renderPlot({NULL})
          sendSweetAlert(session = session,title = "Warning",
                         text = "GO enrichment analysis returned no results. It is recommended to try adjusting the related parameters.",
                         type = "warning" )}
      }else{
        sendSweetAlert(session = session,title = "Warning",
                       text = "Markers is empty. It is recommended to try adjusting the related parameters.",
                       type = "warning" )}
    }
    })
  
  
  # Motif All Table    >>--------------------------------------------
  observeEvent(input$submit_MotifFindAllMarkers,{
    clean_motif_all_table(TRUE)
    motif_markers1<-input$Marker_list1
    print(motif_markers1)
    MotifTopMarkers1<<- strsplit(motif_markers1, "\n")[[1]]
    print(MotifTopMarkers1)
    if(!is.null(MotifTopMarkers1)&&length(MotifTopMarkers1)!= 0){
    progressSweetAlert(session = session,
                       id = "simulationProgress6",
                       title = div("Motifs All Table Running...,",br(),"It will take some time, please be patient and wait..."),
                       display_pct = TRUE,
                       value = 10);Sys.sleep(0.8)
    id<-showNotification("Motifs All Table Running...",duration = NULL,closeButton = FALSE);on.exit(removeNotification(id), add = TRUE)
    tryCatch({
      cat("\033[31m","Motif analysis! \033[0m\n");
      if(!is.null(pbmcs[["peaks"]])){
         if(is.null(pbmcs[["peaks"]]@motifs)){   
            selected_genome <- get(sf_data) 
            if(Species_type=="Homo"){
              pfm <- TFBSTools::getMatrixSet(x = JASPAR2020, opts = list(collection = "CORE",species = 9606, all_versions = FALSE))
              for(i in 10:20){updateProgressBar(session = session,
                                                id = "simulationProgress6",
                                                value = i,
                                                title = "It will take some time, please be patient and wait...",
                                                total = 100);Sys.sleep(0.1)}
              pbmcs <<- AddMotifs(pbmcs,genome = selected_genome, pfm = pfm);  rm(pfm)}
            if(Species_type=="Mus"){
              pfm <- TFBSTools::getMatrixSet(x = JASPAR2020, opts = list(collection = "CORE",species =10090, all_versions = FALSE))
              for (i in 10:20) {updateProgressBar(session = session,
                                                  id = "simulationProgress6",
                                                  value = i,
                                                  title = "It will take some time, please be patient and wait...",
                                                  total = 100); Sys.sleep(0.1)}
              pbmcs <<- AddMotifs(pbmcs,genome = selected_genome, pfm = pfm);  rm(pfm)}
            }}
      for (i in 20:35) {
        updateProgressBar(session = session,
          id = "simulationProgress6",
          value = i,
          title = "It will take some time, please be patient and wait...",
          total = 100);Sys.sleep(0.1)}
      print(pbmcs@assays$peaks)
      observe({if(! is.null(pbmcs[["peaks"]])){
                if(! is.null(pbmcs[["peaks"]]@motifs)){
                  cat("\033[31m","【updata genes SelectizeInput1】 \033[0m\n");
                  genesCols <<- sort(unlist(pbmcs[["peaks"]]@motifs@motif.names))
                  names(genesCols) <- NULL
                  updateSelectizeInput(inputId="genes_selected1",label="Motif Name",choices=as.list(genesCols),options = list(maxItems=4),selected=genesCols[1],server = T)}}})
      for (i in 35:80) {updateProgressBar(session = session,
                                          id = "simulationProgress6",
                                          value = i,
                                          title = "It will take some time, please be patient and wait...",
                                          total = 100);Sys.sleep(0.1)}
      set.seed(1234)
      enriched.motifs1 <<- FindMotifs(object = pbmcs,features = MotifTopMarkers1)
      enriched.motifs1[sapply(enriched.motifs1,is.numeric)] <<- round(enriched.motifs1[sapply(enriched.motifs1,is.numeric)],4)
      for (i in 80:90) {updateProgressBar(session = session,
                                          id = "simulationProgress6",
                                          value = i,
                                          title = "It will take some time, please be patient and wait...",
                                          total = 100);Sys.sleep(0.1)}
      output$Motif_data1 <- DT::renderDataTable({
          datatable(data = enriched.motifs1, rownames = FALSE,selection = "single",
            options = list(pageLength = 10,
                           autoWidth = TRUE,
                           columnDefs = list(list(width="80px",targets ='_all'),list(className = 'dt-left',targets = "_all")),
                           scrollX = TRUE))})
      
  },error = function(e){
      print(e)
  })
  cat("\033[31m","  MotifTable1! \033[0m\n");
  shinyjs::enable(selector = ".sidebar li a[data-value='seven']")
  js.8 <- '$(document).ready(function(){$("a[data-value=seven]").css("color", "#E6E7E8");});'
  shinyjs::runjs(js.8)
  for (i in 90:100) {updateProgressBar(session = session,
                                      id = "simulationProgress6",
                                      value = i,
                                      title = "It will take some time, please be patient and wait...",
                                      total = 100);Sys.sleep(0.1)}
  closeSweetAlert(session = session)
  sendSweetAlert(session = session,title = "DONE",
                 text = "Motifs All Table is completed!",
                 type = "success")
  showNotification("Motifs All Table End of run！", type = "message")
  }else{
    sendSweetAlert(session = session,title = "Warning",
                     text = "Markers is empty. It is recommended to try adjusting the related parameters.",
                     type = "warning" )}
  })
 
  # Motif Plot_1---------------------------------------------------
  observe({
    selected_motif1 <<- input$Motif_data1_rows_selected
    if(!is.null(selected_motif1)){
      motifs_1 <<- rownames(enriched.motifs1[selected_motif1,])
      mtMotifPlot1 <<- MotifPlot(pbmcs,motifs = motifs_1,ncol=2)
      output$MotifPlot1 <- renderPlot({print(mtMotifPlot1)})
    }
    if(is.null(selected_motif1)){
      output$MotifPlot1 <- renderPlot({ NULL})
    }
    })
  
  #Motif idents table---------------------------------------------------------
  observe({
    print("dsfj1------------------dsg------")
    selected_idents3 <- input$Ident41
    print(selected_idents3)
    if(!is.null(FindMarkers_result1)){
      if(!is.null(selected_idents3)){
         if(selected_idents3==input$Ident11){
        print("ident1")
        MotifTopMarkers2<<-FindMarkers_result1[  FindMarkers_result1$p_val_adj<input$Pvalue1&FindMarkers_result1$pct.1>input$MinPct4,]
        # MotifMarkers2()
        print(MotifTopMarkers2)
        marker_list_str2 <- paste(rownames(MotifTopMarkers2), collapse = "\n")
        print(marker_list_str2)
        if(is.null(marker_list_str2)){
          print("sdga11------------------")
          print(Marker_list2_str2)
          updateTextAreaInput(session,"Marker_list2","Marker List:",
                              value = "NULL")
        }else{
          print("sdga1------------------")
          updateTextAreaInput(session,"Marker_list2","Marker List:",
                              value = marker_list_str2)
        }
        # open_ident2<<- rownames(FindMarkers_result1[FindMarkers_result1$p_val_adj < 2& FindMarkers_result1$avg_log2FC > 1, ])
      }
      if(selected_idents3==input$Ident12){
        print("ident2")
        MotifTopMarkers2<<-FindMarkers_result1[  FindMarkers_result1$p_val_adj<input$Pvalue1&
                                              FindMarkers_result1$pct.2>input$MinPct4,]
        # MotifTopMarkers2<<-MotifMarkers2()
        print(MotifTopMarkers2)
        marker_list_str2 <- paste(rownames(MotifTopMarkers2), collapse = "\n")
        print(marker_list_str2)
        if(is.null(marker_list_str2)){
          print("sdga21------------------")
          print(Marker_list2_str2)
          updateTextAreaInput(session,"Marker_list2","Marker List:",
                              value = "NULL")
        }else{
          print("sdga2------------------")
          updateTextAreaInput(session,"Marker_list2","Marker List:",
                              value = marker_list_str2)
        }
        # open_ident2<<- rownames(FindMarkers_result1[FindMarkers_result1$p_val_adj < 2& FindMarkers_result1$avg_log2FC < 1, ])
      }
        
      }
    }
  })
  
  observeEvent(input$submit_MotifFindMarkers,{
    clean_motif_table(TRUE)
    motif_markers2<<-input$Marker_list2
    print(motif_markers2)
    MotifTopMarkers2<<- strsplit(motif_markers2, "\n")[[1]]
    print(MotifTopMarkers2)
    if(!is.null(MotifTopMarkers2)&&length(MotifTopMarkers2)!= 0){
      id<-showNotification("Motifs Idents Table Running...", duration = NULL,closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      cat("\033[31m","  MotifTable2! \033[0m\n");
      selected_genome <- get(sf_data) 
      if(!is.null(pbmcs[["peaks"]])){
          if(is.null(pbmcs[["peaks"]]@motifs)){
              print("sd------------------")
              if(Species_type=="Homo"){
                pfm <- TFBSTools::getMatrixSet(x = JASPAR2020, opts = list(collection = "CORE",species = 9606, all_versions = FALSE))
                for (i in 10:20) { updateProgressBar(session = session,
                                    id = "simulationProgress6",
                                    value = i,
                                    title = "It will take some time, please be patient and wait...",
                                    total = 100 );Sys.sleep(0.1) }
                pbmcs <<- AddMotifs(pbmcs,genome = selected_genome, pfm = pfm);  rm(pfm)}
              if(Species_type=="Mus"){
                pfm <- TFBSTools::getMatrixSet(x = JASPAR2020, opts = list(collection = "CORE",species =10090, all_versions = FALSE))
                for (i in 10:20) {updateProgressBar(session = session,
                                                    id = "simulationProgress6",
                                                    value = i,
                                                    title = "It will take some time, please be patient and wait...",
                                                    total = 100
                  );Sys.sleep(0.1)}
                pbmcs <<- AddMotifs(pbmcs,genome = selected_genome, pfm = pfm);  rm(pfm)}
          }}
      # topPeaks <- input$AllMarkersTopPeaks4
      # if("peaks_snn_res.0.1" %in% identsList){Idents(pbmcs) <- "peaks_snn_res.0.1"}
      DefaultAssay(pbmcs) <- "peaks";    Idents(pbmcs) <- paste0("peaks_snn_res.",input$resolution)
      # MotifTopMarkers2 <<- (FindMarkers_result1  %>% top_n(n = topPeaks, wt = avg_log2FC))
      set.seed(1234)
      enriched.motifs2 <<- FindMotifs(object = pbmcs,features = MotifTopMarkers2)
      enriched.motifs2[sapply(enriched.motifs2,is.numeric)] <<- round(enriched.motifs2[sapply(enriched.motifs2,is.numeric)],4)
      output$Motif_data2 <- DT::renderDataTable({
          datatable(data = enriched.motifs2, rownames = FALSE,selection = "single",
                    options = list(pageLength = 10,
                                   autoWidth = TRUE,
                                   columnDefs = list(list(width="80px",targets ='_all'),list(className = 'dt-left',targets = "_all")),
                                   scrollX = TRUE
                    ))})
      
      observe({
        if(! is.null(pbmcs[["peaks"]])){
          if(! is.null(pbmcs[["peaks"]]@motifs)){
            cat("\033[31m","【updata genes SelectizeInput1】 \033[0m\n");
            genesCols <<- sort(unlist(pbmcs[["peaks"]]@motifs@motif.names))
            names(genesCols) <- NULL
            updateSelectizeInput(inputId="genes_selected1",label="Motif Name",choices=as.list(genesCols),
                                 options = list(maxItems=4),selected=genesCols[1],server = T)}}})
      
      showNotification("Motifs Idents Table End of run！", type = "message")
      shinyjs::enable(selector = ".sidebar li a[data-value='seven']")
      js.8 <- '$(document).ready(function(){$("a[data-value=seven]").css("color", "#E6E7E8");});'
        shinyjs::runjs(js.8)
    }else{
      sendSweetAlert(session = session,title = "Warning",
                     text = "Markers is empty. It is recommended to try adjusting the related parameters.",
                     type = "warning" )
    }
  })
   
  
  # Motif Plot_2---------------------------------------------------
  observe({
    selected_motif2<<- input$Motif_data2_rows_selected
    if(!is.null(selected_motif2)){
      motifs_2 <<- rownames(enriched.motifs2[selected_motif2,])
      mtMotifPlot2 <<- MotifPlot(pbmcs,motifs = motifs_2,ncol=2)
      output$MotifPlot2 <- renderPlot({print(mtMotifPlot2)})
    }
    if(is.null(selected_motif2)){
      output$MotifPlot2 <- renderPlot({ NULL})
    }
  })
  
  # FootPrinting-----------------------------------------------------
  observeEvent(input$submit_FootPrinting,{
    progressSweetAlert(session = session,id = "simulationProgress7",
                       title = div("FootPrinting Running.",br(),"It will take some time, please be patient and wait..."),
                       display_pct = TRUE,value =10)
    Sys.sleep(0.8)
    id<-showNotification("FootPrinting Running...", duration = NULL,closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    motifs1 <<- names(unlist(pbmcs[["peaks"]]@motifs@motif.names)[match(unlist(strsplit(input$genes_selected1,",")),unlist(pbmcs[["peaks"]]@motifs@motif.names))])
    for (i in 10:25) {
      updateProgressBar(
        session = session,
        id = "simulationProgress7",
        value = i,
        title = "It will take some time, please be patient and wait...",
        total = 100)
      Sys.sleep(0.1)}
    DefaultAssay(pbmcs) <- "peaks"
    observe({
    if(!is.null(cells)){
      Idents(pbmcs) <<- paste0("peaks_snn_res.",input$resolution)
      pbmcs <<- Seurat::RenameIdents(pbmcs, cell_id)
      cat("\033[31m","FootPrinting! \033[0m\n");
      selected_genome <- get(sf_data)
      print(pbmcs@assays$peaks)
      cat(".....................")
      tryCatch({
        pbmcs <<- Footprint(pbmcs, motif.name=motifs1 ,genome=selected_genome)
        for (i in 25:65) {
          updateProgressBar(
            session = session,
            id = "simulationProgress7",
            value = i,
            title = "It will take some time, please be patient and wait...",
            total = 100)
          Sys.sleep(0.1)}
      },error=function(e){
        cat("Error", e$message, "\n")
        sendSweetAlert(session = session,
                       #title = e$message,
                       text =div(" Not all hexamers represented in input region!",br()," It is possible that you need to modify the parameters 'fcoun_peaks' and 'fcoun_penkmax' in the filter cell module to obtain complete data.") ,
                       type = "error"
        )
        print(e)
      })      
      output$FootPrintingPlot <- renderPlot({
        Idents(pbmcs) <<- paste0("peaks_snn_res.",input$resolution)
        pbmcs <<- Seurat::RenameIdents(pbmcs, cell_id)
        fpFootPrintingPlot <<- PlotFootprint(pbmcs, features = motifs1)
        fpFootPrintingPlot <<- fpFootPrintingPlot + patchwork::plot_layout(ncol = input$ActivityNcol5)
        print(fpFootPrintingPlot)
        for (i in 65:85) {
          updateProgressBar(
            session = session,
            id = "simulationProgress7",
            value = i,
            title = "It will take some time, please be patient and wait...",
            total = 100
          )
          Sys.sleep(0.1)
        }
      })
    }
    if(is.null(cells)){
      cat("\033[31m","FootPrinting! \033[0m\n");
      selected_genome <- get(sf_data)
      print(pbmcs@assays$peaks)
      cat(".....................")
      tryCatch({
        pbmcs <<- Footprint(pbmcs, motif.name=motifs1 ,genome=selected_genome)
        for (i in 25:65) {
          updateProgressBar(
            session = session,
            id = "simulationProgress7",
            value = i,
            title = "It will take some time, please be patient and wait...",
            total = 100
          )
          Sys.sleep(0.1)
        }
      },error=function(e){
        cat("Error", e$message, "\n")
        sendSweetAlert(session = session,
                       title = e$message,
                       text =div(" Not all hexamers represented in input region!",br()," It is possible that you need to modify the parameters 'fcoun_peaks' and 'fcoun_penkmax' in the filter cell module to obtain complete data.") ,
                       type = "error")
        print(e)
      })
      output$FootPrintingPlot <- renderPlot({
        fpFootPrintingPlot <<- PlotFootprint(pbmcs, features = motifs1)
        fpFootPrintingPlot <<- fpFootPrintingPlot + patchwork::plot_layout(ncol = input$ActivityNcol5)
        print(fpFootPrintingPlot)
        for (i in 65:85) {
          updateProgressBar(
            session = session,
            id = "simulationProgress7",
            value = i,
            title = "It will take some time, please be patient and wait...",
            total = 100
          )
          Sys.sleep(0.1)
        }
        # updateProgressBar(session = session,id = "simulationProgress7", value = 85)
        
      })
    }
      for (i in 85:100) {
        updateProgressBar(
          session = session,
          id = "simulationProgress7",
          value = i,
          title = "It will take some time, please be patient and wait...",
          total = 100
        )
        Sys.sleep(0.1)
      }
    closeSweetAlert(session = session)
    sendSweetAlert(session = session,title = "DONE",
                   text = "FootPrinting is completed!",
                   type = "success")
    showNotification("FootPrinting End of run！", type = "message")
    
    })
  })
  
 #下载------------------------------------------------------------------ 
  
  output$DensityScatter_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")  
      paste0(formatted_time, "-", "DensityScatter",input$DensityScatter_PlotChoices)
    },
    content=function(file) {
      if(input$DensityScatter_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$DensityScatter_PlotWidth, height = input$DensityScatter_PlotHeight)}
      else if(input$DensityScatter_PlotChoices==".png")
      {png(file = file, width = input$DensityScatter_PlotWidth, height = input$DensityScatter_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$DensityScatter_PlotWidth, height = input$DensityScatter_PlotHeight,units="in",res=1000)}
      print(qcDensityScatterPlot)
      dev.off()
    })
  
  output$TSSPlot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S") 
      paste0(formatted_time,"-", "TSSPlot",input$TSSPlot_PlotChoices)
    },
    content=function(file) {
      if(input$TSSPlot_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$TSSPlot_PlotWidth, height = input$TSSPlot_PlotHeight)}
      else if(input$TSSPlot_PlotChoices==".png")
      {png(file = file, width = input$TSSPlot_PlotWidth, height = input$TSSPlot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$TSSPlot_PlotWidth, height = input$TSSPlot_PlotHeight,units="in",res=1000)}
      print(qcTSSPlot)
      dev.off()
    })
  
  output$FragmentPlot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "FragmentHistogramPlot",input$FragmentPlot_PlotChoices)
    },
    content=function(file) {
      if(input$FragmentPlot_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$FragmentPlot_PlotWidth, height = input$FragmentPlot_PlotHeight)}
      else if(input$FragmentPlot_PlotChoices==".png")
      {png(file = file, width = input$FragmentPlot_PlotWidth, height = input$FragmentPlot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$FragmentPlot_PlotWidth, height = input$FragmentPlot_PlotHeight,units="in",res=1000)}
      print(qcFragmentHistogram)
      dev.off()
    })
  
  output$QCVlnPlot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "VlnPlot",input$QCVlnPlot_PlotChoices)
    },
    content=function(file) {
      if(input$QCVlnPlot_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$QCVlnPlot_PlotWidth, height = input$QCVlnPlot_PlotHeight)}
      else if(input$QCVlnPlot_PlotChoices==".png")
      {png(file = file, width = input$QCVlnPlot_PlotWidth, height = input$QCVlnPlot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$QCVlnPlot_PlotWidth, height = input$QCVlnPlot_PlotHeight,units="in",res=1000)}
      print(qcVlnPlot)
      dev.off()
    })
  
  output$QCVlnPlots_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "QC_VlnPlot",input$QCVlnPlots_PlotChoices)
    },
    content=function(file) {
      if(input$QCVlnPlots_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$QCVlnPlots_PlotWidth, height = input$QCVlnPlots_PlotHeight)}
      else if(input$QCVlnPlots_PlotChoices==".png")
      {png(file = file, width = input$QCVlnPlots_PlotWidth, height = input$QCVlnPlots_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$QCVlnPlots_PlotWidth, height = input$QCVlnPlots_PlotHeight,units="in",res=1000)}
      print(qcVlnPlots)
      dev.off()
    })
  
  output$DepthCorPlot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "DepthCorPlot",input$DepthCorPlot_PlotChoices)
    },
    content=function(file) {
      if(input$DepthCorPlot_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$DepthCorPlot_PlotWidth, height = input$DepthCorPlot_PlotHeight)}
      else if(input$DepthCorPlot_PlotChoices==".png")
      {png(file = file, width = input$DepthCorPlot_PlotWidth, height = input$DepthCorPlot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$DepthCorPlot_PlotWidth, height = input$DepthCorPlot_PlotHeight,units="in",res=1000)}
      print(values$deDepthCorPlot)
      dev.off()
    })
  
  output$ClusterDimPlot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "Clustering",input$ClusterDimPlot_PlotChoices)
    },
    content=function(file) {
      if(input$ClusterDimPlot_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$ClusterDimPlot_PlotWidth, height = input$ClusterDimPlot_PlotHeight)}
      else if(input$ClusterDimPlot_PlotChoices==".png")
      {png(file = file, width = input$ClusterDimPlot_PlotWidth, height = input$ClusterDimPlot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$ClusterDimPlot_PlotWidth, height = input$ClusterDimPlot_PlotHeight,units="in",res=1000)}
      print(values$dpDimPlot)
      dev.off()
    })
  
  output$FeaturePlot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "FeaturePlot",input$FeaturePlot_PlotChoices)
    },
    content=function(file) {
      if(input$FeaturePlot_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$FeaturePlot_PlotWidth, height = input$FeaturePlot_PlotHeight)}
      else if(input$FeaturePlot_PlotChoices==".png")
      {png(file = file, width = input$FeaturePlot_PlotWidth, height = input$FeaturePlot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$FeaturePlot_PlotWidth, height = input$FeaturePlot_PlotHeight,units="in",res=1000)}
      print(dbFeaturePlot)
      dev.off()
    })
  output$GenomicRegionsPlot_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "grGenomicRegionsPlot",input$GenomicRegionsPlot_PlotChoices)
    },
    content=function(file) {
      if(input$GenomicRegionsPlot_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$GenomicRegionsPlot_PlotWidth, height = input$GenomicRegionsPlot_PlotHeight)}
      else if(input$GenomicRegionsPlot_PlotChoices==".png")
      {png(file = file, width = input$GenomicRegionsPlot_PlotWidth, height = input$GenomicRegionsPlot_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$GenomicRegionsPlot_PlotWidth, height = input$GenomicRegionsPlot_PlotHeight,units="in",res=1000)}
      print(grGenomicRegionsPlot)
      dev.off()
    })
  
  output$MotifPlot1_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "MotifPlot",input$MotifPlot1_PlotChoices)
    },
    content=function(file) {
      if(input$MotifPlot1_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$MotifPlot1_PlotWidth, height = input$MotifPlot1_PlotHeight)}
      else if(input$MotifPlot1_PlotChoices==".png")
      {png(file = file, width = input$MotifPlot1_PlotWidth, height = input$MotifPlot1_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$MotifPlot1_PlotWidth, height = input$MotifPlot1_PlotHeight,units="in",res=1000)}
      print(mtMotifPlot1)
      dev.off()
    })
  output$MotifPlot2_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "MotifPlot",input$MotifPlot2_PlotChoices)
    },
    content=function(file) {
      if(input$MotifPlot2_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$MotifPlot2_PlotWidth, height = input$MotifPlot2_PlotHeight)}
      else if(input$MotifPlot2_PlotChoices==".png")
      {png(file = file, width = input$MotifPlot2_PlotWidth, height = input$MotifPlot2_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$MotifPlot2_PlotWidth, height = input$MotifPlot2_PlotHeight,units="in",res=1000)}
      print(mtMotifPlot2)
      dev.off()
    })
  output$FootPrinting_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "FootPrinting",input$FootPrinting_PlotChoices)
    },
    content=function(file) {
      if(input$FootPrinting_PlotChoices==".pdf")
      {pdf(file = file,onefile=FALSE, width = input$FootPrinting_PlotWidth, height = input$FootPrinting_PlotHeight)}
      else if(input$FootPrinting_PlotChoices==".png")
      {png(file = file, width = input$FootPrinting_PlotWidth, height = input$FootPrinting_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$FootPrinting_PlotWidth, height = input$FootPrinting_PlotHeight,units="in",res=1000)}
      print(fpFootPrintingPlot)
      dev.off()
    })
   output$annotation_download_plot <- downloadHandler(
    filename <- function(){
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "Annotation",input$annotation_PlotChoices)
    },
    content=function(file) {
      if(input$annotation_PlotChoices==".pdf")
      {pdf(file = file, onefile=FALSE,width = input$annotation_PlotWidth, height = input$annotation_PlotHeight)}
      else if(input$annotation_PlotChoices==".png")
      {png(file = file, width = input$annotation_PlotWidth, height = input$annotation_PlotHeight,units="in",res=1000)}
      else
      {tiff(file = file, width = input$annotation_PlotWidth, height = input$annotation_PlotHeight,units="in",res=1000)}
      print(dDimPlot)
      dev.off()
    })
   output$FM_FeaturePlot_download_plot <- downloadHandler(
     filename <- function(){
       formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
       paste0(formatted_time, "-", "FM_FeaturePlot",input$FM_FeaturePlot_PlotChoices)
     },
     content=function(file) {
       if(input$FM_FeaturePlot_PlotChoices==".pdf")
       {pdf(file = file, width = input$FM_FeaturePlot_PlotWidth, height = input$FM_FeaturePlot_PlotHeight)}
       else if(input$FM_FeaturePlot_PlotChoices==".png")
       {png(file = file, width = input$FM_FeaturePlot_PlotWidth, height = input$FM_FeaturePlot_PlotHeight,units="in",res=1000)}
       else
       {tiff(file = file, width = input$FM_FeaturePlot_PlotWidth, height = input$FM_FeaturePlot_PlotHeight,units="in",res=1000)}
       print(FM_FeaturePlot)
       dev.off()
     })
   output$FM_VinPlot_download_plot <- downloadHandler(
     filename <- function(){
       formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
       paste0(formatted_time, "-", "FM_VinPlot",input$FM_VinPlot_PlotChoices)
     },
     content=function(file) {
       if(input$FM_VinPlot_PlotChoices==".pdf")
       {pdf(file = file, width = input$FM_VinPlot_PlotWidth, height = input$FM_VinPlot_PlotHeight)}
       else if(input$FM_VinPlot_PlotChoices==".png")
       {png(file = file, width = input$FM_VinPlot_PlotWidth, height = input$FM_VinPlot_PlotHeight,units="in",res=1000)}
       else
       {tiff(file = file, width = input$FM_VinPlot_PlotWidth, height = input$FM_VinPlotPlotHeight,units="in",res=1000)}
       print(FM_VinPlot)
       dev.off()
     })
  
   output$GO_Plot1_download_plot <- downloadHandler(
     filename <- function(){
       formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
       paste0(formatted_time, "-", "GO_Plot1",input$GO_Plot1_PlotChoices)
     },
     content=function(file) {
       if(input$GO_Plot1_PlotChoices==".pdf")
       {pdf(file = file, width = input$GO_Plot1_PlotWidth, height = input$GO_Plot1_PlotHeight)}
       else if(input$GO_Plot1_PlotChoices==".png")
       {png(file = file, width = input$GO_Plot1_PlotWidth, height = input$GO_Plot1_PlotHeight,units="in",res=1000)}
       else
       {tiff(file = file, width = input$GO_Plot1_PlotWidth, height = input$GO_Plot1_PlotHeight,units="in",res=1000)}
       print(GO_Plot1)
       dev.off()
     })
   output$GO_Plot2_download_plot <- downloadHandler(
     filename <- function(){
       formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
       paste0(formatted_time, "-", "GO_Plot2",input$GO_Plot2_PlotChoices)
     },
     content=function(file) {
       if(input$GO_Plot2_PlotChoices==".pdf")
       {pdf(file = file, width = input$GO_Plot2_PlotWidth, height = input$GO_Plot2_PlotHeight)}
       else if(input$GO_Plot2_PlotChoices==".png")
       {png(file = file, width = input$GO_Plot1_PlotWidth, height = input$GO_Plot2_PlotHeight,units="in",res=1000)}
       else
       {tiff(file = file, width = input$GO_Plot1_PlotWidth, height = input$GO_Plot2_PlotHeight,units="in",res=1000)}
       print(GO_Plot2)
       dev.off()
     })
  # table
  
  output$FindAllMarkers_download_data <- downloadHandler(
    filename = function() {
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "Deg_Findallmarkers_",input$AssayRadio1,"_all.csv")
    },
    content = function(file) {
      write.csv(FindAllMarkers_result, file)
    })
  output$annotation_download_data <- downloadHandler(
    filename = function() {
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "annotation_all.csv")
    },
    content = function(file) {
      write.csv(cells, file)
    })
  output$Motif_download_data1 <- downloadHandler(
    filename = function() {
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "Allmarkers_Findmotif.csv")
    },
    content = function(file) {
      write.csv(enriched.motifs1, file)
    })
  
  output$ClusterMarkers_download_data <- downloadHandler(
    filename = function() {
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "Deg_Findallmarkers_",input$Ident13,".csv")
    },
    content = function(file) {
      write.csv(ClusterMarkers_result1, file)
    })
  output$CompareMarkers_download_data <- downloadHandler(
    filename = function() {
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "Deg_Findmarkers_",input$Ident11,"_vs_",input$Ident12,"_all.csv")
    },
    content = function(file) {
      write.csv(FindMarkers_result1, file)
    })
  output$Motif_download_data2 <- downloadHandler(
    filename = function() {  
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "Top",input$AllMarkersTopPeaks3,"markers_findmotif_",input$Ident41,"_vs_",input$Ident42,".csv")
    },
    content = function(file) {
      write.csv(enriched.motifs2, file)
    })
  output$GO_download_data1 <- downloadHandler(
    filename = function() {  
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "GO_Table_",input$idents_selected1,"markers_findmotif_",input$Ident41,"_vs_",input$Ident42,".csv")
    },
    content = function(file) {
      write.csv(ident1_ego1, file)
    })
  output$GO_download_data2 <- downloadHandler(
    filename = function() {  
      formatted_time <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
      paste0(formatted_time, "-", "GO_Table_",input$idents_selected2,"markers_findmotif_",input$Ident41,"_vs_",input$Ident42,".csv")
    },
    content = function(file) {
      write.csv(ident1_ego2, file)
    })
  
  #清除---------------------------------------------------------------------
  clean<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      pbmc <<- NULL
      pbmcs_1 <<- NULL
      pbmcs <<- NULL
      Selectdata<<- NULL
      Selectreference_data <<-NULL
      sf_data<<-NULL
      pbmc_exists <<- F
      pbmc1<<-NULL
      Ident11<<-NULL
      Ident12<<-NULL
      Ident41<<-NULL
      Ident13<<-NULL
      Ident42<<-NULL
      ident1<<-NULL
      ident2<<-NULL
      cells<<-NULL
      CELL<<-NULL
      motifs1<<-NULL
      cell_counts_after<<-NULL
      cell_counts<<-NULL
      gene.activities<<-NULL
      unique_clusters<<-NULL
      gene_names_overlapping<<-NULL
      # Dynamic variables(动态变量)
      assaysList <<- NULL
      identsList <<- NULL
      # plots
      GO_Plot1<<-NULL
      GO_Plot2<<-NULL
      qcVlnPlots<<-NULL
      qcVlnPlot <<- NULL
      qcTSSPlot <<- NULL
      qcFragmentHistogram <<- NULL
      qcDensityScatterPlot<<-NULL
      dDimplot<<-NULL
      dbFeaturePlot <<- NULL
      grGenomicRegionsPlot <<- NULL
      mtMotifPlot1 <<- NULL
      mtMotifPlot2 <<- NULL
      fpFootPrintingPlot <<- NULL
      # tables
      FindAllMarkers_result <<- NULL
      FindMarkers_result1 <<- NULL
      MotifTopMarkers1 <<- NULL
      MotifTopMarkers2 <<- NULL
      enriched.motifs1 <<- NULL
      enriched.motifs2 <<- NULL
      ClusterMarkers_result1<<- NULL
      Motif_data1<<-NULL
      Motif_data2<<-NULL
      #output
      output$QCDSPlot<<- NULL
      output$QCtssPlot<<- NULL
      output$QCFragmentPlot<<- NULL
      output$QCVlnPlot<<- NULL
      output$text.cellsremain1<<- NULL
      output$QCVlnPlots<<- NULL
      output$text.cellsremain2<<- NULL
      output$DepthCorPlot<<- NULL
      output$ClusterDimPlot<<- NULL
      # output$Chr_num
      # output$Feature3
      output$FindAllMarkers_data<<- NULL
      output$FeaturePlot<<- NULL
      output$ClusterMarkers_data<<- NULL
      output$CompareMarkers_data<<- NULL
      output$singleR_data<<- NULL
      output$DimPlot<<- NULL
      output$GenomicRegionsPlot<<- NULL
      output$Motif_data1<<- NULL
      output$Motif_data2<<- NULL
      output$MotifPlot1<<- NULL
      output$MotifPlot2<<- NULL
      output$FootPrintingPlot<<- NULL
      output$vbox_cell_tab1<<-NULL
      output$vbox_cell_tab2<<-NULL
      output$GO_data1<<-NULL
      output$GO_data2<<-NULL
      ident1_ego1<<-NULL
      ident1_ego2<<-NULL
      # reactive--------------------------------------------------------
      count <- reactiveVal(NULL)
      fragments <- reactiveVal(NULL)
      tbi <- reactiveVal(NULL)
      metadata <- reactiveVal(NULL)
      values$deDepthCorPlot<<-NULL
      values$Normalization<<-NULL
      values$NdpDimPlot<<-NULL
      values$dpDimPlot<<-NULL
      values$submit_Clustering<<-NULL
      #values$SelectReference_data<<-NULL
      #values<-reactiveValues(submit_Clustering=NULL)
      values<-reactiveValues(submit_ClusterMarkers=FALSE)
      updateTabsetPanel(session,"qVinplot",selected = "Before filter")
      shinyjs::hide("featurePlotBox")
      shinyjs::disable("submit_NLDR")
      shinyjs::disable("submit_Clustering")
      shinyjs::disable("submit_GeneActivity")
      shinyjs::disable(selector = ".sidebar li a[data-value='one']")
      shinyjs::disable(selector = ".sidebar li a[data-value='ones']")
      shinyjs::disable(selector = ".sidebar li a[data-value='two']")
      shinyjs::disable(selector = ".sidebar li a[data-value='twos']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threeone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='four']")
      shinyjs::disable(selector = ".sidebar li a[data-value='sixone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='sixtwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='five']")
      shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      js.1 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=one]").css("color", "#808080"); });'
      shinyjs::runjs(js.1)
      js.2 <- '$(document).ready(function(){
              // 获取菜单2的禁用状态
                $("a[data-value=ones]").css("color", "#808080");}); '
      shinyjs::runjs(js.2)
      js.3.1 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=twos]").css("color", "#808080");});'
      shinyjs::runjs(js.3.1)
      js.4 <- ' $(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threeone]").css("color", "#637584");});'
      shinyjs::runjs(js.4)
      js.41 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.41)
      js.511 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#637584");});'
      shinyjs::runjs(js.511)
      js.5 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sixone]").css("color", "#637584");});'
      shinyjs::runjs(js.5)
      js.51 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sixtwo]").css("color", "#637584");});'
      shinyjs::runjs(js.51)
      js.6 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=four]").css("color", "#808080");});'
      shinyjs::runjs(js.6)
      js.7 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=five]").css("color", "#808080");});'
      shinyjs::runjs(js.7)
      js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#808080");});'
      shinyjs::runjs(js.8)
      js.3 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=two]").css("color", "#808080");});'
      shinyjs::runjs(js.3)
    }
  }
  
  clean_filter<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      pbmcs <<- NULL
      pbmcs_1<<-NULL
      Ident11<<-NULL
      Ident12<<-NULL
      Ident41<<-NULL
      Ident13<<-NULL
      Ident42<<-NULL
      cells<<-NULL
      CELL<<-NULL
      motifs1<<-NULL
      cell_counts_after<<-NULL
      cell_counts<<-NULL
      gene.activities<<-NULL
      unique_clusters<<-NULL
      gene_names_overlapping<<-NULL
      # Dynamic variables(动态变量)
      assaysList <<- NULL
      identsList <<- NULL
      # plots
      GO_Plot1<<-NULL
      GO_Plot2<<-NULL
      qcVlnPlots<<-NULL
      dDimplot<<-NULL
      dbFeaturePlot <<- NULL
      grGenomicRegionsPlot <<- NULL
      mtMotifPlot1 <<- NULL
      mtMotifPlot2 <<- NULL
      fpFootPrintingPlot <<- NULL
      ident1_ego1<<-NULL
      ident1_ego2<<-NULL
      # tables
      FindAllMarkers_result <<- NULL
      FindMarkers_result1 <<- NULL
      MotifTopMarkers1 <<- NULL
      MotifTopMarkers2 <<- NULL
      enriched.motifs1 <<- NULL
      enriched.motifs2 <<- NULL
      ClusterMarkers_result1<<- NULL
      Motif_data1<<-NULL
      Motif_data2<<-NULL
      #output---------------------------------------------
      output$QCVlnPlots<<- NULL
      output$text.cellsremain2<<- NULL
      output$DepthCorPlot<<- NULL
      output$ClusterDimPlot<<- NULL
      # output$Chr_num
      # output$Feature3
      output$FM_FeaturePlot<<-NULL
      output$FM_VinPlot<<-NULL
      output$FindAllMarkers_data<<- NULL
      output$FeaturePlot<<- NULL
      output$ClusterMarkers_data<<- NULL
      output$CompareMarkers_data<<- NULL
      output$singleR_data<<- NULL
      output$DimPlot<<- NULL
      output$GenomicRegionsPlot<<- NULL
      output$Motif_data1<<- NULL
      output$Motif_data2<<- NULL
      output$MotifPlot<<- NULL
      output$FootPrintingPlot<<- NULL
      output$GO_data1<<-NULL
      output$GO_data2<<-NULL
      output$GO_Plot1<<-NULL
      output$GO_Plot2<<-NULL
      output$text.featuresremain<- renderUI({return(NULL)})
      # reactive--------------------------------------------------------
      values$deDepthCorPlot<<-NULL
      values$Normalization<<-NULL
      values$NdpDimPlot<<-NULL
      values$dpDimPlot<<-NULL
      values$submit_Clustering<<-NULL
      #values<-reactiveValues(submit_Clustering=NULL)
      values<-reactiveValues(submit_ClusterMarkers=FALSE)
      shinyjs::hide("featurePlotBox")
      shinyjs::disable("submit_NLDR")
      shinyjs::disable("submit_Clustering")
      shinyjs::disable("submit_GeneActivity")
      shinyjs::disable(selector = ".sidebar li a[data-value='two']")
      shinyjs::disable(selector = ".sidebar li a[data-value='twos']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threeone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='four']")
      shinyjs::disable(selector = ".sidebar li a[data-value='sixone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='sixtwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='five']")
      shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      js.3.1 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=twos]").css("color", "#808080");});'
      shinyjs::runjs(js.3.1)
      js.4 <- ' $(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threeone]").css("color", "#637584");});'
      shinyjs::runjs(js.4)
      js.41 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.41)
      js.511 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#637584");});'
      shinyjs::runjs(js.511)
      js.5 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sixone]").css("color", "#637584");});'
      shinyjs::runjs(js.5)
      js.51 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sixtwo]").css("color", "#637584");});'
      shinyjs::runjs(js.51)
      js.6 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=four]").css("color", "#808080");});'
      shinyjs::runjs(js.6)
      js.7 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=five]").css("color", "#808080");});'
      shinyjs::runjs(js.7)
      js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#808080");});'
      shinyjs::runjs(js.8)
      js.3 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=tow]").css("color", "#808080");});'
      shinyjs::runjs(js.3)
    }
  }
  clean_Nor<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      Ident11<<-NULL
      Ident12<<-NULL
      Ident41<<-NULL
      Ident13<<-NULL
      Ident42<<-NULL
      cells<<-NULL
      CELL<<-NULL
      motifs1<<-NULL
      gene.activities<<-NULL
      unique_clusters<<-NULL
      # Dynamic variables(动态变量)
      assaysList <<- NULL
      identsList <<- NULL
      # plots
      GO_Plot1<<-NULL
      GO_Plot2<<-NULL
      dbFeaturePlot <<- NULL
      grGenomicRegionsPlot <<- NULL
      mtMotifPlot1 <<- NULL
      mtMotifPlot2 <<- NULL
      fpFootPrintingPlot <<- NULL
      # tables
      FindAllMarkers_result <<- NULL
      FindMarkers_result1 <<- NULL
      MotifTopMarkers1 <<- NULL
      MotifTopMarkers2 <<- NULL
      enriched.motifs1 <<- NULL
      enriched.motifs2 <<- NULL
      ClusterMarkers_result1<<- NULL
      Motif_data1<<-NULL
      Motif_data2<<-NULL
      ident1_ego1<<-NULL
      ident1_ego2<<-NULL
      gene_names_overlapping<<-NULL
      #output-------------------------------------------
      output$ClusterDimPlot<<- NULL
      # output$Chr_num
      # output$Feature3
      output$FM_FeaturePlot<<-NULL
      output$FM_VinPlot<<-NULL
      output$DepthCorPlot<<- NULL
      output$FindAllMarkers_data<<- NULL
      output$FeaturePlot<<- NULL
      output$ClusterMarkers_data<<- NULL
      output$CompareMarkers_data<<- NULL
      output$singleR_data<<- NULL
      output$DimPlot<<- NULL
      output$GenomicRegionsPlot<<- NULL
      output$Motif_data1<<- NULL
      output$Motif_data2<<- NULL
      output$MotifPlot<<- NULL
      output$FootPrintingPlot<<- NULL
      output$DepthCorPlot <<-NULL
      output$GO_data1<<-NULL
      output$GO_data2<<-NULL
      output$GO_Plot1<<-NULL
      output$GO_Plot2<<-NULL
      # reactive--------------------------------------------------------
      values$NdpDimPlot<<-NULL
      values$dpDimPlot<<-NULL
      values$deDepthCorPlot<<-NULL
      values$submit_Clustering<<-NULL
      #values<-reactiveValues(submit_Clustering=NULL)
      values<-reactiveValues(submit_ClusterMarkers=FALSE)
      shinyjs::hide("featurePlotBox")
      shinyjs::disable("submit_Clustering")
      shinyjs::disable("submit_GeneActivity")
      shinyjs::disable(selector = ".sidebar li a[data-value='twos']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threeone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='four']")
      shinyjs::disable(selector = ".sidebar li a[data-value='sixone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='sixtwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='five']")
      shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      js.3.1 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=twos]").css("color", "#808080");});'
      shinyjs::runjs(js.3.1)
      js.4 <- ' $(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threeone]").css("color", "#637584");});'
      shinyjs::runjs(js.4)
      js.41 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.41)
      js.511 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#637584");});'
      shinyjs::runjs(js.511)
      js.5 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sixone]").css("color", "#637584");});'
      shinyjs::runjs(js.5)
      js.51 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sixtwo]").css("color", "#637584");});'
      shinyjs::runjs(js.51)
      js.6 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=four]").css("color", "#808080");});'
      shinyjs::runjs(js.6)
      js.7 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=five]").css("color", "#808080");});'
      shinyjs::runjs(js.7)
      js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#808080");});'
      shinyjs::runjs(js.8)
    }
  }
  
  clean_LDR<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      Ident11<<-NULL
      Ident12<<-NULL
      Ident41<<-NULL
      Ident13<<-NULL
      Ident42<<-NULL
      cells<<-NULL
      CELL<<-NULL
      motifs1<<-NULL
      gene.activities<<-NULL
      unique_clusters<<-NULL
      # Dynamic variables(动态变量)
      assaysList <<- NULL
      identsList <<- NULL
      # plots
      GO_Plot1<<-NULL
      GO_Plot2<<-NULL
      dbFeaturePlot <<- NULL
      grGenomicRegionsPlot <<- NULL
      mtMotifPlot1 <<- NULL
      mtMotifPlot2 <<- NULL
      fpFootPrintingPlot <<- NULL
      # tables
      FindAllMarkers_result <<- NULL
      FindMarkers_result1 <<- NULL
      MotifTopMarkers1 <<- NULL
      MotifTopMarkers2 <<- NULL
      enriched.motifs1 <<- NULL
      enriched.motifs2 <<- NULL
      ClusterMarkers_result1<<- NULL
      Motif_data1<<-NULL
      Motif_data2<<-NULL
      ident1_ego1<<-NULL
      ident1_ego2<<-NULL
      gene_names_overlapping<<-NULL
      #output-------------------------------------------
      output$ClusterDimPlot<<- NULL
      # output$Chr_num
      # output$Feature3
      output$FM_FeaturePlot<<-NULL
      output$FM_VinPlot<<-NULL
      output$FindAllMarkers_data<<- NULL
      output$FeaturePlot<<- NULL
      output$ClusterMarkers_data<<- NULL
      output$CompareMarkers_data<<- NULL
      output$singleR_data<<- NULL
      output$DimPlot<<- NULL
      output$GenomicRegionsPlot<<- NULL
      output$Motif_data1<<- NULL
      output$Motif_data2<<- NULL
      output$MotifPlot<<- NULL
      output$FootPrintingPlot<<- NULL
      output$DepthCorPlot <<-NULL
      output$GO_data1<<-NULL
      output$GO_data2<<-NULL
      output$GO_Plot1<<-NULL
      output$GO_Plot2<<-NULL
      # reactive--------------------------------------------------------
      values$NdpDimPlot<<-NULL
      values$dpDimPlot<<-NULL
      values$submit_Clustering<<-NULL
      values$deDepthCorPlot<<-NULL
      #values<-reactiveValues(submit_Clustering=NULL)
      values<-reactiveValues(submit_ClusterMarkers=FALSE)
      shinyjs::hide("featurePlotBox")
      shinyjs::disable("submit_Clustering")
      shinyjs::disable("submit_GeneActivity")
      shinyjs::disable(selector = ".sidebar li a[data-value='twos']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threeone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='four']")
      shinyjs::disable(selector = ".sidebar li a[data-value='sixone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='sixtwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='five']")
      shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      js.3.1 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=twos]").css("color", "#808080");});'
      shinyjs::runjs(js.3.1)
      js.4 <- ' $(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threeone]").css("color", "#637584");});'
      shinyjs::runjs(js.4)
      js.41 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.41)
      js.511 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#637584");});'
      shinyjs::runjs(js.511)
      js.5 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sixone]").css("color", "#637584");});'
      shinyjs::runjs(js.5)
      js.51 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sixtwo]").css("color", "#637584");});'
      shinyjs::runjs(js.51)
      js.6 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=four]").css("color", "#808080");});'
      shinyjs::runjs(js.6)
      js.7 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=five]").css("color", "#808080");});'
      shinyjs::runjs(js.7)
      js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#808080");});'
      shinyjs::runjs(js.8)
    }
  }
  
  clean_NLDR<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      Ident11<<-NULL
      Ident12<<-NULL
      Ident41<<-NULL
      Ident13<<-NULL
      Ident42<<-NULL
      cells<<-NULL
      CELL<<-NULL
      motifs1<<-NULL
      gene.activities<<-NULL
      unique_clusters<<-NULL
      # Dynamic variables(动态变量)
      assaysList <<- NULL
      identsList <<- NULL
      # plots
      GO_Plot1<<-NULL
      GO_Plot2<<-NULL
      dbFeaturePlot <<- NULL
      grGenomicRegionsPlot <<- NULL
      mtMotifPlot1 <<- NULL
      mtMotifPlot2 <<- NULL
      fpFootPrintingPlot <<- NULL
      # tables
      FindAllMarkers_result <<- NULL
      FindMarkers_result1 <<- NULL
      MotifTopMarkers1 <<- NULL
      MotifTopMarkers2 <<- NULL
      enriched.motifs1 <<- NULL
      enriched.motifs2 <<- NULL
      ClusterMarkers_result1<<- NULL
      Motif_data1<<-NULL
      Motif_data2<<-NULL
      ident1_ego1<<-NULL
      ident1_ego2<<-NULL
      gene_names_overlapping<<-NULL
      #output-------------------------------------------
      output$ClusterDimPlot<<- NULL
      # output$Chr_num
      # output$Feature3
      output$FM_FeaturePlot<<-NULL
      output$FM_VinPlot<<-NULL
      output$FindAllMarkers_data<<- NULL
      output$FeaturePlot<<- NULL
      output$ClusterMarkers_data<<- NULL
      output$CompareMarkers_data<<- NULL
      output$singleR_data<<- NULL
      output$DimPlot<<- NULL
      output$GenomicRegionsPlot<<- NULL
      output$Motif_data1<<- NULL
      output$Motif_data2<<- NULL
      output$MotifPlot<<- NULL
      output$FootPrintingPlot<<- NULL
      output$GO_data1<<-NULL
      output$GO_data2<<-NULL
      output$GO_Plot1<<-NULL
      output$GO_Plot2<<-NULL
      # reactive--------------------------------------------------------
      values$NdpDimPlot<<-NULL
      values$dpDimPlot<<-NULL
      values$submit_Clustering<<-NULL
      #values<-reactiveValues(submit_Clustering=NULL)
      values<-reactiveValues(submit_ClusterMarkers=FALSE)
      shinyjs::hide("featurePlotBox")
      shinyjs::disable("submit_Clustering")
      shinyjs::disable("submit_GeneActivity")
      shinyjs::disable(selector = ".sidebar li a[data-value='twos']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threeone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='four']")
      shinyjs::disable(selector = ".sidebar li a[data-value='sixone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='sixtwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='five']")
      shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      js.3.1 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=twos]").css("color", "#808080");});'
      shinyjs::runjs(js.3.1)
      js.4 <- ' $(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threeone]").css("color", "#637584");});'
      shinyjs::runjs(js.4)
      js.41 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.41)
      js.511 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#637584");});'
      shinyjs::runjs(js.511)
      js.5 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sixone]").css("color", "#637584");});'
      shinyjs::runjs(js.5)
      js.51 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sixtwo]").css("color", "#637584");});'
      shinyjs::runjs(js.51)
      js.6 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=four]").css("color", "#808080");});'
      shinyjs::runjs(js.6)
      js.7 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=five]").css("color", "#808080");});'
      shinyjs::runjs(js.7)
      js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#808080");});'
      shinyjs::runjs(js.8)
    }
  }
  
  clean_Clustering<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      Ident11<<-NULL
      Ident12<<-NULL
      Ident41<<-NULL
      Ident13<<-NULL
      Ident42<<-NULL
      cells<<-NULL
      CELL<<-NULL
      motifs1<<-NULL
      gene.activities<<-NULL
      unique_clusters<<-NULL
      # Dynamic variables(动态变量)
      assaysList <<- NULL
      identsList <<- NULL
      # plots
      GO_Plot1<<-NULL
      GO_Plot2<<-NULL
      dbFeaturePlot <<- NULL
      grGenomicRegionsPlot <<- NULL
      mtMotifPlot1 <<- NULL
      mtMotifPlot2 <<- NULL
      fpFootPrintingPlot <<- NULL
      # tables
      FindAllMarkers_result <<- NULL
      FindMarkers_result1 <<- NULL
      MotifTopMarkers1 <<- NULL
      MotifTopMarkers2 <<- NULL
      enriched.motifs1 <<- NULL
      enriched.motifs2 <<- NULL
      ClusterMarkers_result1<<- NULL
      Motif_data1<<-NULL
      Motif_data2<<-NULL
      ident1_ego1<<-NULL
      ident1_ego2<<-NULL
      gene_names_overlapping<<-NULL
      #output--------------------------------------------------------
      output$ClusterDimPlot<<- NULL
      # output$Chr_num
      # output$Feature3
      output$FM_FeaturePlot<<-NULL
      output$FM_VinPlot<<-NULL
      output$FindAllMarkers_data<<- NULL
      output$FeaturePlot<<- NULL
      output$ClusterMarkers_data<<- NULL
      output$CompareMarkers_data<<- NULL
      output$singleR_data<<- NULL
      output$DimPlot<<- NULL
      output$GenomicRegionsPlot<<- NULL
      output$Motif_data1<<- NULL
      output$Motif_data2<<- NULL
      output$MotifPlot1<<- NULL
      output$MotifPlot2<<- NULL
      output$FootPrintingPlot<<- NULL
      output$GO_data1<<-NULL
      output$GO_data2<<-NULL
      output$GO_Plot1<<-NULL
      output$GO_Plot2<<-NULL
      # update--------------------------------------------------------
      if(!is.null(pbmcs)){
        assaysList <<- as.list(names(pbmcs@assays)); names(assaysList) <- names(pbmcs@assays)
        identsList <<- as.list(colnames(pbmcs@meta.data)[unlist(purrr::map(colnames(pbmcs@meta.data),function(x){is.factor(pbmcs@meta.data[[x]])}))]);
        names(identsList) <- colnames(pbmcs@meta.data)[unlist(purrr::map(colnames(pbmcs@meta.data),function(x){is.factor(pbmcs@meta.data[[x]])}))];
        updateRadioButtons(inputId="AssayRadio1",label="Assay:",choices=assaysList,selected = "peaks")
        updateRadioButtons(inputId="AssayRadio2",label="Assay:",choices=assaysList,selected = "peaks")
        updateRadioButtons(inputId="AssayRadio3",label="Assay:",choices=assaysList,selected = "peaks")
        
      }
      # reactive--------------------------------------------------------
      values<-reactiveValues(submit_ClusterMarkers=FALSE)
      shinyjs::hide("featurePlotBox")
      shinyjs::disable("submit_GeneActivity")
      shinyjs::disable(selector = ".sidebar li a[data-value='twos']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threeone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='four']")
      shinyjs::disable(selector = ".sidebar li a[data-value='sixone']")
      shinyjs::disable(selector = ".sidebar li a[data-value='sixtwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='five']")
      shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      js.3.1 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=twos]").css("color", "#808080");});'
      shinyjs::runjs(js.3.1)
      js.4 <- ' $(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threeone]").css("color", "#637584");});'
      shinyjs::runjs(js.4)
      js.41 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.41)
      js.511 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#637584");});'
      shinyjs::runjs(js.511)
      js.5 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sixone]").css("color", "#637584");});'
      shinyjs::runjs(js.5)
      js.51 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sixtwo]").css("color", "#637584");});'
      shinyjs::runjs(js.51)
      js.6 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=four]").css("color", "#808080");});'
      shinyjs::runjs(js.6)
      js.7 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=five]").css("color", "#808080");});'
      shinyjs::runjs(js.7)
      js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#808080");});'
      shinyjs::runjs(js.8)
    }
  }
  clean_Findallmarker<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      # Ident11<<-NULL
      # Ident12<<-NULL
      # Ident41<<-NULL
      # Ident13<<-NULL
      # Ident42<<-NULL
      cells<<-NULL
      CELL<<-NULL
      motifs1<<-NULL
      unique_clusters<<-NULL
      # plots
      GO_Plot1<<-NULL
      # GO_Plot2<<-NULL
      dbFeaturePlot <<- NULL
      grGenomicRegionsPlot <<- NULL
      mtMotifPlot1 <<- NULL
      ident1_ego1<<-NULL
      # mtMotifPlot2 <<- NULL
      # fpFootPrintingPlot <<- NULL
      # tables
      FindAllMarkers_result <<- NULL
      # FindMarkers_result1 <<- NULL
      MotifTopMarkers1 <<- NULL
      # MotifTopMarkers2 <<- NULL
      enriched.motifs1 <<- NULL
      # enriched.motifs2 <<- NULL
      ClusterMarkers_result1<<- NULL
      Motif_data1<<-NULL
      # Motif_data2<<-NULL
      #output--------------------------------------------------------
      # output$Chr_num
      # output$Feature3
      output$FindAllMarkers_data<<- NULL
      output$FeaturePlot<<- NULL
      # output$ClusterMarkers_data<<- NULL
      # output$CompareMarkers_data<<- NULL
      output$singleR_data<<- NULL
      output$DimPlot<<- NULL
      output$GenomicRegionsPlot<<- NULL
      output$Motif_data1<<- NULL
      # output$Motif_data2<<- NULL
      output$MotifPlot<<- NULL
      output$FootPrintingPlot<<- NULL
      output$GO_Plot1<<-NULL
      # output$GO_Plot2<<-NULL
      output$MotifPlot1<<- NULL
      # output$MotifPlot2<<- NULL
      output$GO_data1<<-NULL
      # reactive--------------------------------------------------------
      values<-reactiveValues(submit_ClusterMarkers=FALSE)
      shinyjs::disable(selector = ".sidebar li a[data-value='twos']")
      shinyjs::disable(selector = ".sidebar li a[data-value='threetwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='four']")
      shinyjs::disable(selector = ".sidebar li a[data-value='sixone']")
      # shinyjs::disable(selector = ".sidebar li a[data-value='sixtwo']")
      shinyjs::disable(selector = ".sidebar li a[data-value='five']")
      shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      js.3.1 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=twos]").css("color", "#808080");});'
      shinyjs::runjs(js.3.1)
      js.41 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
               $("a[data-value=threetwo]").css("color", "#637584");});'
      shinyjs::runjs(js.41)
      js.511 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=six]").css("color", "#637584");});'
      shinyjs::runjs(js.511)
      js.5 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=sixone]").css("color", "#637584");});'
      shinyjs::runjs(js.5)
      # js.51 <- '$(document).ready(function(){
      #     // 获取菜单2的禁用状态
      #       $("a[data-value=six two]").css("color", "#637584");});'
      # shinyjs::runjs(js.51)
      js.6 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=four]").css("color", "#808080");});'
      shinyjs::runjs(js.6)
      js.7 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=five]").css("color", "#808080");});'
      shinyjs::runjs(js.7)
      js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#808080");});'
      shinyjs::runjs(js.8)
    }
  }
  clean_Findmarker<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      # Ident11<<-NULL
      # Ident12<<-NULL
      Ident41<<-NULL
      Ident42<<-NULL
      cells<<-NULL
      CELL<<-NULL
      motifs1<<-NULL
      # plots
      # GO_Plot1<<-NULL
      GO_Plot2<<-NULL
      # mtMotifPlot1 <<- NULL
      mtMotifPlot2 <<- NULL
      fpFootPrintingPlot <<- NULL
      ident1_ego2<<-NULL
      # tables
      FindMarkers_result1 <<- NULL
      MotifTopMarkers2 <<- NULL
      enriched.motifs2 <<- NULL
      Motif_data2<<-NULL
      selected_row<<-NULL
      feature_name<<-NULL
      # output---------------------------------------------------------
      output$FM_FeaturePlot<<-NULL
      output$FM_VinPlot<<-NULL
      output$CompareMarkers_data<<- NULL
      output$Motif_data2<<- NULL
      output$FootPrintingPlot<<- NULL
      # output$MotifPlot1<<- NULL
      output$MotifPlot2<<- NULL
      output$GO_data2<<-NULL
      output$GO_Plot2<<-NULL
      # reactive--------------------------------------------------------
      # shinyjs::disable(selector = ".sidebar li a[data-value='sixtwo']")
      # shinyjs::disable(selector = ".sidebar li a[data-value='six']")
      # shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      # js.51 <- '$(document).ready(function(){
      #     // 获取菜单2的禁用状态
      #       $("a[data-value=sixtwo]").css("color", "#637584");});'
      # shinyjs::runjs(js.51)
      # js.7 <- '$(document).ready(function(){
      #     // 获取菜单2的禁用状态
      #       $("a[data-value=six]").css("color", "#808080");});'
      # shinyjs::runjs(js.7)
      # js.8 <- '$(document).ready(function(){
      #     // 获取菜单2的禁用状态
      #       $("a[data-value=seven]").css("color", "#808080");});'
      # shinyjs::runjs(js.8)
    }
  }
  
  clean_annotation<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      cells<<-NULL
      CELL<<-NULL
      motifs1<<-NULL
      # plots
      GO_Plot1 <<- NULL
      GO_Plot2 <<- NULL
      grGenomicRegionsPlot <<- NULL
      mtMotifPlot1 <<- NULL
      mtMotifPlot2 <<- NULL
      fpFootPrintingPlot <<- NULL
      # tables
      MotifTopMarkers1 <<- NULL
      MotifTopMarkers2 <<- NULL
      enriched.motifs1 <<- NULL
      enriched.motifs2 <<- NULL
      Motif_data1<<-NULL
      Motif_data2<<-NULL
      # output--------------------------------------------------------
      output$singleR_data<<- NULL
      output$DimPlot<<- NULL
      output$GenomicRegionsPlot<<- NULL
      output$Motif_data1<<- NULL
      output$Motif_data2<<- NULL
      output$MotifPlot<<- NULL
      output$FootPrintingPlot<<- NULL
      # # reactive--------------------------------------------------------
      # shinyjs::disable(selector = ".sidebar li a[data-value='four']")
      # shinyjs::disable(selector = ".sidebar li a[data-value='sixone']")
      # shinyjs::disable(selector = ".sidebar li a[data-value='sixtwo']")
      # shinyjs::disable(selector = ".sidebar li a[data-value='six']")
      # shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      
    }
  }
  
  clean_GO_analysis1<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      ident1_ego1<<-NULL
      GO_Plot1<<-NULL
      # output------------------------------------------------------
      output$ident1_ego1<<- NULL
      output$GO_Plot1<<- NULL
    }
  }
  clean_GO_analysis2<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      ident1_ego2<<-NULL
      GO_Plot2<<-NULL
      # output------------------------------------------------------
      output$ident1_ego2<<- NULL
      output$GO_Plot2<<- NULL
      
    }
  }
  clean_motif_all_table<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      motifs1<<-NULL
      # plots
      mtMotifPlot1 <<- NULL
      # mtMotifPlot2 <<- NULL
      fpFootPrintingPlot <<- NULL
      # tables
      # MotifTopMarkers1 <<- NULL
      # MotifTopMarkers2 <<- NULL
      enriched.motifs1 <<- NULL
      # enriched.motifs2 <<- NULL
      Motif_data1<<-NULL
      # Motif_data2<<-NULL
      # output------------------------------------------------------
      output$Motif_data1<<- NULL
      # output$Motif_data2<<- NULL
      output$MotifPlot1<<- NULL
      output$FootPrintingPlot<<- NULL
      # reactive--------------------------------------------------------
      # shinyjs::disable(selector = ".sidebar li a[data-value='six']")
       shinyjs::disable(selector = ".sidebar li a[data-value='seven']")
      # js.7 <- '$(document).ready(function(){
      #     // 获取菜单2的禁用状态
      #       $("a[data-value=six]").css("color", "#808080");});'
      # shinyjs::runjs(js.7)
      js.8 <- '$(document).ready(function(){
          // 获取菜单2的禁用状态
            $("a[data-value=seven]").css("color", "#808080");});'
      shinyjs::runjs(js.8)
    }
  }
  
  clean_motif_table<<-function(fromDataInput){
    if (fromDataInput){  
      cat("\033[31m","CLEAN! \033[0m\n");
      motifs1<<-NULL
      motifs2<<-NULL
      # plots
      mtMotifPlot2 <<- NULL
      fpFootPrintingPlot <<- NULL
      GO_Plot1<<-NULL
      GO_Plot2<<-NULL
      # tables
      # MotifTopMarkers2 <<- NULL
      enriched.motifs2 <<- NULL
      Motif_data2<<-NULL
      # output---------------------------------------------------------
      output$Motif_data2<<- NULL
      output$MotifPlot2<<- NULL
      output$FootPrintingPlot<<- NULL
      
    }
  }
  
  
}


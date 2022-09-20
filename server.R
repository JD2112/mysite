#####=================================================############
# Module1 : Heatmap figure
#####=================================================############
suppressMessages(library(ComplexHeatmap))
suppressMessages(library(extrafont))
suppressMessages(library(shiny))
suppressMessages(library(InteractiveComplexHeatmap))
suppressMessages(library(beanplot))
suppressMessages(library(circlize))

server <- function(input, output, session) {
  options(shiny.maxRequestSize=1000000*1024^2)
  ht_pos_obj = reactiveVal(NULL)
  ht_obj = reactiveVal(NULL)
  
    
    hm <- eventReactive(input$plot, {
      
    
      idhm <- showNotification("generating plot, please wait...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(idhm), add = TRUE)
      file1 <- input$inputdata
      ext <- tools::file_ext(file1$datapath)
      req(file1)
      validate(
        need(input$inputdata != "", "No data has been uploaded")
      )
      
      datahmdv <- read.delim(file1$datapath, sep = "\t", header = T, row.names = 1)
      #datahmdv <- read.delim("/mnt/SD2/Jyotirmoys/JD/Data/DV-miRNA/ForHeatMap.txt", sep = "\t", header = T, row.names = 1)
      ## Step1: Create the matrix
      hm.dv <- as.matrix(datahmdv)
      colnames(hm.dv) = c("hsa-miR-149","hsa-miR-200b")
      #Heatmap(hm.hladr)
      col_fun = circlize::colorRamp2(c(0,2,10), c("#006600", "#000000", "#FF0000"))
      lgd = Legend(col_fun = col_fun, title = "log2 FC", 
                   at = c(0, 2, 10, 65),
                   legend_height = unit(10,"in"))
      
      #ha = rowAnnotation(foo = anno_mark(at=c(1:81), labels = rownames(hm.dv)[1:81]))
      
      ha = rowAnnotation(
        foo = anno_text(rownames(hm.dv),
                        #location = 1,
                        #just = "right",
                        gp = gpar(fontsize = 12,
                                  fontfamily = "Times New Roman",
                                  fontface = "bold"))
      )
      
      ha2 = HeatmapAnnotation(Groups = colnames(hm.dv), 
                              show_legend = FALSE,
                              col = list(Groups = c("hsa-miR-149" = "#FF0000", 
                                                    "hsa-miR-200b" = "#7570b3")))
      
      ht_list = 
      Heatmap(hm.dv,   # input data matrix for analysis
              show_heatmap_legend = FALSE, # heatmap legend false because ha is running for row annotation
              row_names_side = "left", # side of the row names
              row_names_gp = gpar(fontsize = 12, # graphic parameters for the figure, set the font size for the row names
                                  fontfamily = "Times New Roman", # set the font family for the figure
                                  fontface = "bold"), # set the font face for the row names
              column_names_rot = 0,  # roatation of the column names
              column_names_centered = TRUE, # centering the column names
              column_names_gp = gpar(fontsize = 12,  # font size for column names
                                     fontfamily = "Times New Roman", # font family for column names
                                     fontface = "bold"), # font face for column names
              #top_annotation = ha2 # removing top annotation, required for grouping of samples
              cluster_rows = FALSE,  # removing row cluster information
              cluster_columns = FALSE, # removing column cluster information
              col = col_fun # adding color function as described before
    ) 
      
      ht1 = draw(ht_list, annotation_legend_list = lgd)
    })

  #}
  #####==================================================================================############
  #####=================================================############
  # Module-2 : violin plot
  #####=================================================############

  violinPlot <- eventReactive(input$plot, {
      library(ggplot2)
      library(tidyverse)

      idhm <- showNotification("generating plot, please wait...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(idhm), add = TRUE)
      
      file1 <- input$inputdata
      ext <- tools::file_ext(file1$datapath)
      req(file1)

      data <- read.delim(file1$datapath, sep = "\t", header = T, row.names = 1)
      con = "FC_hsa-miR-149"

      library(ggpubr)

      vio = ggplot(data, aes(x=group, y=value, fill=group)) + 
          geom_violin(trim=FALSE)+
          geom_jitter(position = position_jitter(0.1),
                      #color = ifelse(con, "darkgreen", "red"), 
                      size =3)+
          theme_classic()+
          scale_fill_manual(values = c("darkgreen", "red"))+
          labs(y="value", x="group")+
          stat_compare_means(method = "t.test")
      vio
  })
 #####==================================================================================############
  #####=================================================############
  # Module-3 : bean plot
  #####=================================================############ 
 beanPlot <- eventReactive(input$plot, {
   idhm <- showNotification("generating plot, please wait...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(idhm), add = TRUE)
      file1 <- input$inputdata
      ext <- tools::file_ext(file1$datapath)
      req(file1)

      data <- read.delim(file1$datapath, sep = "\t", header = T, row.names = 1)

      bean = beanplot(data)
 })
 
 
 #####=================================================############
 # Final graph : plot the graph
 #####=================================================############

    output$plot <- renderPlot({
      if(input$selin == "heatmap"){
        hm()
      } else if (input$selin == "bean") {
        beanPlot()
      } else {
        violinPlot()
      }
    })

}
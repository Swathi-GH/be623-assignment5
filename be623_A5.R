setwd("~/biocomputing/be623_assignment5")
getwd()

#Q1: Creating a Data Frame
circ_df<-data.frame(
  
  Sample = paste0("S",1:12), #generates S1 to S6
  Gene = c(rep("PER2",6),rep("BMAL1",6)), #repeats PER2 and BMAL1 six times
  ZT = rep(c(0,4,8,12,16,20),2), #6 values of ZT for each gene
  Expression = c(
    1.8,3.2,6.5,9.4,7.1,3.6,
    8.6,5.9,2.7,1.5,3.4,6.8
  )
)
str(circ_df) #structure and data types of data frame

#Q2: per2_rows and mean,median, standard deviation calculation
per2_rows<-circ_df[circ_df$Gene == "PER2",]
#PER2 gene column values stored in per2_rows

mean(per2_rows$Expression) #mean
median(per2_rows$Expression) #median
sd(per2_rows$Expression) #standard deviation

#Q3: Saving circ_df as csv file
write.csv(circ_df,"circadian_expression.csv", #saving content of circ_df as csv file
          row.names = FALSE) #row.names = FALSE, removes extra columns
circ_reloaded<-read.csv("circadian_expression.csv") #reloading the csv file
identical(circ_df$Expression, circ_reloaded$Expression) #checking whether reloaded content matches original data frame

#Q4: dplyr pipeline
install.packages("dplyr") #installs dplyr package
library(dplyr) #loads the package

#PER2 gene
circ_df %>%
  filter(Gene == "PER2") %>% #filters for the gene PER2
  arrange(desc(Expression)) #arranges expression values in descending order

#BMAL1 gene
circ_df %>%
  filter(Gene == "BMAL1") %>% #filters for the gene BMAL1
  arrange(desc(Expression)) #arranges expression values in descending order

#Q5: TRUE/FALSE Column
#stores the value based on the condition give. 
#Condition satisfied, then TRUE, else FALSE.

circ_df2<-circ_df %>%
  mutate(dark = ZT >=12) #dark column is added and all the values ZT>12 stored in circ_df2

circ_df2 %>%
  filter(dark == TRUE) %>% #filter for dark samples 
  group_by(Gene) %>%

    summarize(
    mean_expression = mean(Expression), #mean calculation
    n = n() #number of samples
  )

circ_df2 %>%
  filter(dark == FALSE) %>% #filter for light samples
  group_by(Gene) %>%
  summarize(
    mean_expression = mean(Expression), #mean calculation
    n = n() #number of samples
  )

#Q6: Scatter Plot 
install.packages("ggplot2")
library(ggplot2) #loads the library packages

rhythm_plot<-ggplot( #saving plot as variable rhythm_plot
  circ_df, #data frame
  aes(x= ZT, y= Expression, color = Gene) #defining x,y axis and entity
)+
  geom_point(size=3)+ #size of point in plot
  scale_color_manual( #to set point colour
    values = c("PER2" = "steelblue",
               "BMAL1" = "pink")
)+
  labs( #labeling plot
    title = "Circadian Gene Expression",
    x = "Zeitgeber time (h)",
    y = "Relative Expression",
    color = "Gene"
  )+
  theme_minimal() 

rhythm_plot #calling the variable

#to save the plot as image
ggsave("rhythm_plot.png", plot = rhythm_plot,
       width = 6, height = 4, dpi = 300) #setting image dimensions

#Q7: Box Plot

#plotting box plot for circ_df data frame
#x,y axis defined using aes

expression_boxplot<-ggplot(circ_df, aes(x = Gene, y = Expression, fill = Gene))+
  geom_boxplot()+ #for box plot
  scale_fill_manual( #fill color of box
    values = c("PER2" = "steelblue",
               "BMAL1" = "pink")
  )+
  labs( #labeling plot
    title = "Gene Expression Distribution",
    x = "Gene",
    y = "Relative Expression"
  )+
  theme_minimal()

expression_boxplot #calling the variable
#save box plot as image
ggsave("expression_boxplot.png", plot = expression_boxplot, 
       width = 6, height = 4,  dpi = 300)


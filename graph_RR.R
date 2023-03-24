#----------R code for plotting graph from the base model outputs created by the analysis.R code
#graph confintervals
library(egg)
library(scales)
library(hablar)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(stringr)

setwd("~/../OneDrive - The Ohio State University/GohlkeLab/Analysis_HHR/working_files/HHR/Graphs/new/")

AXIS_Y_SIZE = 12
LEGEND_SIZE = 14
Y_TITLE_SIZE = 14
LINE_WIDTH = .5 #erro bar line width
GRID_WIDTH = .9
POINT_WIDTH = 0.7 #error bar middle point width
DASH_WIDTH = .2
DOT_SIZE = .5
ERROR_BAR_TOP = .2
LABEL_SIZE = 13
WORD_WRAP = 15
PLOT_MARGIN = unit(c(.1,.2,-.2,.2), "cm")
LEGEND_MAR = margin(-0.7,0,.4,0,"cm")
STRIP_LINES = 0.2
DODGE_WIDTH = 0.7
AXIS_X_SIZE = 12

#read excel
allDf = read.xlsx("~/../OneDrive - The Ohio State University/GohlkeLab/Analysis_HHR/working_files/HHR/mergedOutputs/mergedOutputsAll.xlsx",sheet = 'junk results from models')

#strings define
Shortness.of.breath = 'Shortness of breath'
Concentration.problems='Concentration problems'
Any.Symptoms = 'Any Symptoms'
Runny.Nose = 'Runny Nose'
Skin.Rash = "Skin Rash"

OtherHomesFlooded = 'Other homes in\nblock flooded'
OnlyOtherHomesFlooded= 'Only Other Homes \nin block flooded'
Contact_Water = 'Reported \nwater contact'
fScanFlooded = 'Flood map\nbased flooding'
HomeFlooded = 'Reported\nhome flooding'

fScanDepthFt = "Inundation depth (ft)"
fScanInundDist = "Proximity to inundataion (km)"
fScanNdays = "Inundation peiod (days)"

#---- base model code -----
#subet base model alone
baseModel = filter(allDf,model=='baseModel') %>% as_tibble()

#subset requried terms
req_terms = c( "HomeFloodedTRUE", "OtherHomesFloodedTRUE", 'OnlyOtherHomesFloodedTRUE',
               "fScanFloodedTRUE", "Contact_WaterTRUE",
               "fScanInunDisCatlte1100", "fScanInunDisCatlte400", "fScanInunDisCatFlooded", 
               "fScanDepthCatlte1Dot5ft", "fScanDepthCatlte3ft", "fScanDepthCatgt3ft",
               "fScanNdaysCat1Day", "fScanNdaysCat2_3Days", "fScanNdaysCat4_14Days",
               "fScanDepthFt" , "fScanInundDist" ,"fScanNdays")


baseModel = baseModel %>% filter(term %in% req_terms)

#---- renames a few things ----
# rename terms
baseModel$term = gsub('TRUE','',baseModel$term)

baseModel$term = gsub('fScanInunDisCat','Distance ',baseModel$term)
baseModel$term = gsub('fScanNdaysCat','Flooded upto ',baseModel$term)
baseModel$term = gsub('fScanDepthCat','Depth ',baseModel$term)

baseModel$term = gsub('lte','<=',baseModel$term)
baseModel$term = gsub('gt','>',baseModel$term)
baseModel$term = gsub('Dot','.',baseModel$term)
baseModel$term = gsub('_','-',baseModel$term)


#rename outcomes
baseModel$outcome = gsub('ShortBreath',Shortness.of.breath,baseModel$outcome)
baseModel$outcome = gsub('Concentrate',Concentration.problems,baseModel$outcome)
baseModel$outcome = gsub('AnySymptoms',Any.Symptoms,baseModel$outcome)
baseModel$outcome = gsub('SkinRash',Skin.Rash,baseModel$outcome)
baseModel$outcome = gsub('RunnyNose',Runny.Nose,baseModel$outcome)

#exposure change terms
baseModel$exposure = gsub('fScanInunDisCat','Distance',baseModel$exposure)
baseModel$exposure = gsub('fScanNdaysCat','Days',baseModel$exposure)
baseModel$exposure = gsub('fScanDepthCat','Depth',baseModel$exposure)
baseModel$exposure = gsub('OnlyOtherHomesFlooded',OnlyOtherHomesFlooded,baseModel$exposure)
baseModel$exposure = gsub('OtherHomesFlooded',OtherHomesFlooded,baseModel$exposure)
baseModel$exposure = gsub('Contact_Water',Contact_Water,baseModel$exposure)
baseModel$exposure = gsub('fScanFlooded',fScanFlooded,baseModel$exposure)
baseModel$exposure = gsub('HomeFlooded',HomeFlooded,baseModel$exposure)
baseModel$exposure = gsub('fScanDepthFt',fScanDepthFt,baseModel$exposure)
baseModel$exposure = gsub('fScanNdays',fScanNdays,baseModel$exposure)
baseModel$exposure = gsub('fScanInundDist',fScanInundDist,baseModel$exposure)

baseModel$exposure = factor(baseModel$exposure,levels=c(Contact_Water, HomeFlooded, OtherHomesFlooded, OnlyOtherHomesFlooded, fScanFlooded,
                                    "Distance", "Days", "Depth",
                                    fScanDepthFt, fScanNdays, fScanInundDist))

#---- dichotomous plot  ----
dichRecs = filter(baseModel,exposure %in% c(Contact_Water,HomeFlooded,OnlyOtherHomesFlooded,fScanFlooded)) %>%
            mutate(exposure=factor(exposure,levels=c(Contact_Water, HomeFlooded,OnlyOtherHomesFlooded, fScanFlooded))) %>%
            #filter out Any outcomes
            filter(outcome!=Any.Symptoms)

AXIS_Y_SIZE = 12
LEGEND_SIZE = 13
Y_TITLE_SIZE = 13
LINE_WIDTH = .5 #erro bar line width
GRID_WIDTH = .9
POINT_WIDTH = 0.9 #error bar middle point width
DASH_WIDTH = .2
DOT_SIZE = .5
ERROR_BAR_TOP = .2
LABEL_SIZE = 13
WORD_WRAP = 15
PLOT_MARGIN = unit(c(.1,.2,-.2,.2), "cm")
LEGEND_MAR = margin(-0.7,0,0,-0.7,"cm")
STRIP_LINES = 0.2
DODGE_WIDTH = 0.7
AXIS_X_SIZE = 12

#word wrap
dichRecs$outcome = str_wrap(dichRecs$outcome,width = 12)
#draw plot
dichGraph = ggplot(dichRecs, aes(y = estimate, x = exposure,color=exposure,shape=exposure,)) + facet_wrap(~ outcome,nrow=2)+
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), size = LINE_WIDTH, width =
                  ERROR_BAR_TOP,position = position_dodge(width = DODGE_WIDTH)) +
  geom_point(size = POINT_WIDTH,position = position_dodge(width = DODGE_WIDTH)) +
  scale_shape_manual(values=c(15,17,16,18,8,16,17,15,18,8,9))+
  scale_color_manual(values=c( "#009E73","#D55E00","#0072B2","#CC79A7","#E69F00","#999999","#000000","#E69F00","#009E73","#F0E442"))+
  geom_hline(aes(yintercept = 1), size = DASH_WIDTH, linetype = "dashed")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", function(x) round(10^x,1)))+
  theme_bw() +   ylab("Risk ratio") + xlab("") + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.y = element_line(size = GRID_WIDTH),
        legend.position ='bottom',legend.margin=LEGEND_MAR,
        legend.text=element_text(size=LEGEND_SIZE),
        axis.text.x= element_blank(),#element_text(size = AXIS_X_SIZE,angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = AXIS_Y_SIZE,angle = 90,hjust = 0.5),axis.title = element_text(size = Y_TITLE_SIZE),
        strip.text = element_text(size=LABEL_SIZE),
        panel.spacing.x = unit(0,'cm'),panel.border = element_rect(size=STRIP_LINES,linetype = 'solid'),
        axis.line = element_line(linetype = 'solid',size=LINE_WIDTH),strip.background = element_rect(colour="black", fill="gray95",size = LINE_WIDTH),)

#ggsave("confintGraphDichon3.pdf",plot = dichGraph, width = unit(7.5,'cm'),height=unit(6.5,'cm'))
#ggsave("confintGraphDichon3.png",plot = dichGraph, width = unit(7.5,'cm'),height=unit(6.5,'cm'), dpi=400)

#---- Linear models plot  ----
linRecs = filter(baseModel,exposure %in% c(fScanDepthFt, fScanNdays, fScanInundDist)) %>%
  mutate(exposure=factor(exposure,levels=c(fScanDepthFt, fScanNdays, fScanInundDist)))

#scale fscanInundationDis by 1000 (change per m to per km)
linRecs[linRecs$exposure==fScanInundDist,c('estimate','conf.low','conf.high')] = linRecs[linRecs$exposure==fScanInundDist,c('estimate','conf.low','conf.high')] ** 1000 

AXIS_Y_SIZE = 12
LEGEND_SIZE = 13
Y_TITLE_SIZE = 13
LINE_WIDTH = .6 #erro bar line width
GRID_WIDTH = .9
POINT_WIDTH = 1.1 #error bar middle point width
DASH_WIDTH = .2
DOT_SIZE = .5
ERROR_BAR_TOP = .2
LABEL_SIZE = 13
PLOT_MARGIN = unit(c(.1,.2,-.2,.2), "cm")
LEGEND_MAR = margin(-0.7,0,0,-0.7,"cm")
STRIP_LINES = 0.2
DODGE_WIDTH = 0.7
AXIS_X_SIZE = 12

#word wrap
linRecs$outcome = str_wrap(linRecs$outcome,width = 12)
#draw plot
linGraph = ggplot(linRecs, aes(y = estimate, x = exposure,color=exposure, shape=exposure)) + facet_wrap(~ outcome,nrow=2)+
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), size = LINE_WIDTH, width =
                  ERROR_BAR_TOP,position = position_dodge(width = DODGE_WIDTH)) +
  geom_point(size = POINT_WIDTH,position = position_dodge(width = DODGE_WIDTH)) +
  scale_shape_manual(values=c(15,17,16,18,8,16,17,15,18,8,9))+
  scale_color_manual(values=c( "#009E73","#D55E00","#0072B2","#CC79A7","#E69F00","#999999","#000000","#E69F00","#009E73","#F0E442"))+
  geom_hline(aes(yintercept = 1), size = DASH_WIDTH, linetype = "dashed")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", function(x) round(10^x,2)))+
  theme_bw() +   ylab("Risk ratio") + xlab("") + 
  theme(panel.grid.major.x = element_blank(), #panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.y = element_line(size = GRID_WIDTH),
        legend.position ='bottom',legend.margin=LEGEND_MAR,
        legend.text=element_text(size=LEGEND_SIZE),
        axis.text.x= element_blank(),#element_text(size = AXIS_X_SIZE,angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = AXIS_Y_SIZE,angle = 90,hjust = 0.5),axis.title = element_text(size = Y_TITLE_SIZE),
        strip.text = element_text(size=LABEL_SIZE),
        panel.spacing.x = unit(0,'cm'),panel.border = element_rect(size=STRIP_LINES,linetype = 'solid'),
        axis.line = element_line(linetype = 'solid',size=LINE_WIDTH),strip.background = element_rect(colour="black", fill="gray95",size = LINE_WIDTH),)

#ggsave("confintGraphLinear.pdf",plot = linGraph, width = unit(7.5,'cm'),height=unit(5.5,'cm'))
#ggsave("confintGraphLinear.png",plot = linGraph, width = unit(7.5,'cm'),height=unit(5.5,'cm'))



#--------categorical plot----------------

catRecs = filter(baseModel,exposure %in% c(  'Depth', 'Days','Distance')) %>%
  mutate(exposure=factor(exposure,levels=c( 'Depth', 'Days','Distance'))) %>%
  filter(outcome!=Any.Symptoms & outcome!='Hospital')

catRecs$exposure <- recode(catRecs$exposure, 'Distance'='Distance\n(Inverse)')

catRecs$term <- recode(catRecs$term, 
                       'Flooded upto 1Day' = 'Flooded upto 1 day',
                       'Flooded upto 2-3Days' = 'Flooded upto 2-3 days',
                       'Flooded upto 4-14Days' = 'Flooded > 3 days',
                       'Distance <=400' = 'Closer to flood (>0 to <400m)',
                       'Distance <=1100' = 'Away from flood (400 to <1100m)',
                       'Distance Flooded' = 'Flooded',
                       'Depth <=1.5ft' = 'Flood Depth < 1.5 ft',
                       'Depth <=3ft' = 'Flood Depth 1.5-3 ft',
                       'Depth >3ft' = 'Flood Depth > 3 ft',
                       )
catRecs$term <- factor(catRecs$term, levels= c('Flood Depth < 1.5 ft','Flood Depth 1.5-3 ft','Flood Depth > 3 ft',
                                               'Flooded upto 1 day','Flooded upto 2-3 days','Flooded > 3 days',
                                               'Away from flood (400 to <1100m)','Closer to flood (>0 to <400m)','Flooded'))

AXIS_Y_SIZE = 12
LEGEND_SIZE = 10
Y_TITLE_SIZE = 13
LINE_WIDTH = .5 #erro bar line width
GRID_WIDTH = .9
POINT_WIDTH = 1.1 #error bar middle point width
DASH_WIDTH = .2
DOT_SIZE = .5
ERROR_BAR_TOP = .2
LABEL_SIZE = 13
PLOT_MARGIN = unit(c(.1,.2,-.2,.2), "cm")
LEGEND_MAR = margin(0,0,-8,-4.2,"cm")
STRIP_LINES = 0.2
DODGE_WIDTH = 0.7
AXIS_X_SIZE = 12

#word wrap
catRecs$outcome = str_wrap(catRecs$outcome,width = 15)
#draw plot
catGraph = ggplot(catRecs, aes(y = estimate, x = exposure,color=term, shape=exposure)) + facet_wrap(~ outcome,nrow=2)+
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), size = LINE_WIDTH, width =
                  ERROR_BAR_TOP,position = position_dodge(width = DODGE_WIDTH)) +
  geom_point(size = POINT_WIDTH,position = position_dodge(width = DODGE_WIDTH)) +
  scale_shape_manual(name = "Legend", values=c(15,17,18),guide='none')+#18,8,16,17,15,18))+#,8,9))+
  scale_color_manual(name = "Legend", values=c( "#009E73","#D55E00","#0072B2","#CC79A7","#E69F00","#999999","#000000","#E69F00","#009E73"))+#,"#F0E442"))+
  geom_hline(aes(yintercept = 1), size = DASH_WIDTH, linetype = "dashed")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", function(x) round(10^x,2)))+
  theme_bw() +   ylab("Risk ratio") + xlab("") + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.y = element_line(size = GRID_WIDTH),
        legend.position ='right',legend.margin=LEGEND_MAR,
        legend.text=element_text(size=LEGEND_SIZE),
        axis.text.x= element_text(size = AXIS_X_SIZE,angle = 90, vjust = 0),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = AXIS_Y_SIZE,angle = 90,hjust = 0.5),axis.title = element_text(size = Y_TITLE_SIZE),
        strip.text = element_text(size=LABEL_SIZE),
        panel.spacing.x = unit(0,'cm'),panel.border = element_rect(size=STRIP_LINES,linetype = 'solid'),
        axis.line = element_line(linetype = 'solid',size=LINE_WIDTH),strip.background = element_rect(colour="black", fill="gray95",size = LINE_WIDTH),)
catGraph
ggsave("confintGraphCat3.pdf",plot = catGraph, width = unit(9,'cm'),height=unit(6.5,'cm'))
ggsave("confintGraphCat3.png",plot = catGraph, width = unit(9,'cm'),height=unit(6.5,'cm'), dpi=400)

#----- for interaction with SVI -----
#subet base model alone
sviDf = filter(allDf,model=='sviInteractionModel' & otherCom == 'interaction term cat_SVI') %>% as_tibble()

#get required rows
req_terms = c( "HomeFlooded", "OtherHomesFlooded",
             "fScanFlooded", "Contact_Water")

#filer only interaction terms estimates | and for the above exposures alone | and just for cat SVI
sviDf = sviDf %>% filter(grepl(':',term)) %>% filter(exposure %in% req_terms) %>% filter(outcome != 'Hospital')


#create seperate variable for interaction term
sviDf$inter = str_split(sviDf$term,'SVI',simplify = T)[,2]
sviDf$inter = recode(sviDf$inter, 'midLow'='Mid-Low','midHigh'='Mid-High','highest'='Highest')
sviDf$inter = factor(sviDf$inter,levels = c('Mid-Low','Mid-High','Highest'))

#renames a few thing
#exposure change terms
sviDf$exposure = gsub('OtherHomesFlooded',OtherHomesFlooded,sviDf$exposure)
sviDf$exposure = gsub('Contact_Water',Contact_Water,sviDf$exposure)
sviDf$exposure = gsub('fScanFlooded',fScanFlooded,sviDf$exposure)
sviDf$exposure = gsub('HomeFlooded',HomeFlooded,sviDf$exposure)

#rename outcomes
sviDf$outcome = gsub('ShortBreath',Shortness.of.breath,sviDf$outcome)
sviDf$outcome = gsub('Concentrate',Concentration.problems,sviDf$outcome)
sviDf$outcome = gsub('AnySymptoms',Any.Symptoms,sviDf$outcome)
sviDf$outcome = gsub('SkinRash',Skin.Rash,sviDf$outcome)
sviDf$outcome = gsub('RunnyNose',Runny.Nose,sviDf$outcome)

#order exposure
sviDf = sviDf %>% mutate(exposure=factor(exposure,levels=c(Contact_Water, HomeFlooded, OtherHomesFlooded, fScanFlooded)))

AXIS_Y_SIZE = 12
LEGEND_SIZE = 13
Y_TITLE_SIZE = 13
LINE_WIDTH = .5 #erro bar line width
GRID_WIDTH = .9
POINT_WIDTH = 1.1 #error bar middle point width
DASH_WIDTH = .2
DOT_SIZE = .5
ERROR_BAR_TOP = .2
LABEL_SIZE = 13
PLOT_MARGIN = unit(c(.1,.2,-.2,.2), "cm")
LEGEND_MAR = margin(-0.7,0,0,-0.7,"cm")
STRIP_LINES = 0.2
DODGE_WIDTH = 0.7
AXIS_X_SIZE = 12
#word wrap
#sviDf$outcome = str_wrap(sviDf$outcome,width = 15)
#draw plot
sviGraph = ggplot(sviDf, aes(y = estimate, x = exposure,color=inter, shape=exposure)) + facet_wrap(~ outcome,nrow=2)+
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), size = LINE_WIDTH, width =
                  ERROR_BAR_TOP,position = position_dodge(width = DODGE_WIDTH)) +
  geom_point(size = POINT_WIDTH,position = position_dodge(width = DODGE_WIDTH)) +
  scale_shape_manual(values=c(15,17,16,18,8,16,17,15,18,8,9),guide='none')+
  scale_color_manual(name='CDC SVI',values=c( "#009E73","#D55E00","#0072B2","#CC79A7","#E69F00","#999999","#000000","#E69F00","#009E73","#F0E442"))+
  geom_hline(aes(yintercept = 1), size = DASH_WIDTH, linetype = "dashed")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", function(x) round(10^x,2)))+
  theme_bw() +   ylab("Risk ratio") + xlab("") + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size=LEGEND_SIZE,face = 'bold'),
        panel.grid.major.y = element_line(size = GRID_WIDTH),
        legend.position ='bottom',legend.margin=LEGEND_MAR,
        legend.text=element_text(size=LEGEND_SIZE),
        axis.text.x= element_text(size = AXIS_X_SIZE,angle = 90, hjust = 1,vjust=0.5),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = AXIS_Y_SIZE,angle = 90,hjust = 0.5),axis.title = element_text(size = Y_TITLE_SIZE),
        strip.text = element_text(size=LABEL_SIZE),
        panel.spacing.x = unit(0,'cm'),panel.border = element_rect(size=STRIP_LINES,linetype = 'solid'),
        axis.line = element_line(linetype = 'solid',size=LINE_WIDTH),strip.background = element_rect(colour="black", fill="gray95",size = LINE_WIDTH),)

ggsave("confintSVI.pdf",plot = sviGraph, width = unit(14,'cm'),height=unit(7,'cm'))
ggsave("confintSVI.png",plot = sviGraph, width = unit(14,'cm'),height=unit(7,'cm'))

#----- for interaction with demographics -----
#subet base model alone
demoDf = filter(allDf,model=='demogInteractionModel') %>% as_tibble()

#get required rows
req_terms = c( "HomeFlooded", "OtherHomesFlooded",
               "fScanFlooded", "Contact_Water")

#filer only interaction terms estimates | and for the above exposures alone | and just for cat SVI
demoDf = demoDf %>% filter(grepl(':',term)) %>% filter(exposure %in% req_terms) %>%
                    filter(outcome!='Hospital') %>%
                    filter(!(outcome=='Injury' & otherCom=='interaction term RaceGroup'))

#create seperate variable for interaction term
demoDf$term = str_split(demoDf$term,'TRUE:',simplify = T)[,2]

#renames a few thing
#exposure change terms
demoDf$exposure = gsub('OtherHomesFlooded',OtherHomesFlooded,demoDf$exposure)
demoDf$exposure = gsub('Contact_Water',Contact_Water,demoDf$exposure)
demoDf$exposure = gsub('fScanFlooded',fScanFlooded,demoDf$exposure)
demoDf$exposure = gsub('HomeFlooded',HomeFlooded,demoDf$exposure)

#rename outcomes
demoDf$outcome = gsub('ShortBreath',Shortness.of.breath,demoDf$outcome)
demoDf$outcome = gsub('Concentrate',Concentration.problems,demoDf$outcome)
demoDf$outcome = gsub('AnySymptoms',Any.Symptoms,demoDf$outcome)
demoDf$outcome = gsub('SkinRash',Skin.Rash,demoDf$outcome)
demoDf$outcome = gsub('RunnyNose',Runny.Nose,demoDf$outcome)

#rename terms
demoDf$term = gsub('Male|AgeGrp|RaceGroup|EducGroup','',demoDf$term) 
demoDf$otherCom = gsub('interaction term ','',demoDf$otherCom)

#order exposure
demoDf = demoDf %>% mutate(exposure=factor(exposure,levels=c(Contact_Water, HomeFlooded, OtherHomesFlooded, fScanFlooded)),
                           term=factor(term,levels= c("Female", "36-50", "51-60", "gt60",                 
                                                      "Non_Hispanic_AsianNOthers", "Non_Hispanic_Black", "Non_Hispanic_White", 
                                                      "collegeOrAssociates",  "highSchoolOrLess" )))
                    

AXIS_Y_SIZE = 11
LEGEND_SIZE = 10
Y_TITLE_SIZE = 11
LINE_WIDTH = .5 #erro bar line width
GRID_WIDTH = .9
POINT_WIDTH = 1.1 #error bar middle point width
DASH_WIDTH = .2
DOT_SIZE = .5
ERROR_BAR_TOP = .2
LABEL_SIZE = 13
PLOT_MARGIN = unit(c(.1,.2,-.2,.2), "cm")
LEGEND_MAR =  margin(-0.7,0,0,-0.2,"cm")
STRIP_LINES = 0.2
DODGE_WIDTH = 0.7
AXIS_X_SIZE = 10
#word wrap
#demoDf$outcome = str_wrap(demoDf$outcome,width = 15)
#draw plot
demoGraph = ggplot(demoDf , aes(y = estimate, x = exposure,color=term, shape=otherCom)) + facet_wrap(~ outcome,nrow=4)+
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), size = LINE_WIDTH, width =
                  ERROR_BAR_TOP,position = position_dodge(width = DODGE_WIDTH)) +
  geom_point(size = POINT_WIDTH,position = position_dodge(width = DODGE_WIDTH)) +
  scale_shape_manual(values=c(15,17,16,18,8,16,17,15,18,8,9))+
  scale_color_manual(values=c( "#009E73","#D55E00","#0072B2","#CC79A7","#E69F00","#999999","#000000","#E69F00","#009E73","#F0E442"))+
  geom_hline(aes(yintercept = 1), size = DASH_WIDTH, linetype = "dashed")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", function(x) round(10^x,2)))+
  theme_bw() +   ylab("Risk ratio") + xlab("") + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.y = element_line(size = GRID_WIDTH),
        legend.position ='right',legend.margin=LEGEND_MAR,
        legend.text=element_text(size=LEGEND_SIZE),
        axis.text.x= element_text(size = AXIS_X_SIZE,angle = 0, hjust = 0.5,vjust=0),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = AXIS_Y_SIZE,angle = 0,hjust = 0.5),axis.title = element_text(size = Y_TITLE_SIZE),
        strip.text = element_text(size=LABEL_SIZE),
        panel.spacing.x = unit(0,'cm'),panel.border = element_rect(size=STRIP_LINES,linetype = 'solid'),
        axis.line = element_line(linetype = 'solid',size=LINE_WIDTH),strip.background = element_rect(colour="black", fill="gray95",size = LINE_WIDTH),)

ggsave("confintDemo.pdf",plot = demoGraph, width = unit(12,'cm'),height=unit(10,'cm'))
ggsave("confintDemo.png",plot = demoGraph, width = unit(12,'cm'),height=unit(10,'cm'))

#---- sensitivity plot for AGU ---
compDf = read.xlsx("D:/NASAProjectFiles/BlitzerFiles/Analysis_HHR/working_files/HHR/mergedOutputs/comparisionMergedOutputs.xlsx",sheet = 'for Graphig')

#order exposure
compDf = compDf %>% mutate(term=factor(term,levels= c( "Home Flooded", "Flood map Flooded",
                                                       "Flood map Agreement alone")))

AXIS_Y_SIZE = 11
LEGEND_SIZE = 10
Y_TITLE_SIZE = 11
LINE_WIDTH = .5 #erro bar line width
GRID_WIDTH = .9
POINT_WIDTH = 1.1 #error bar middle point width
DASH_WIDTH = .2
DOT_SIZE = .5
ERROR_BAR_TOP = .2
LABEL_SIZE = 13
PLOT_MARGIN = unit(c(.1,.2,-.2,.2), "cm")
LEGEND_MAR =  margin(-0.7,0,0,-0.2,"cm")
STRIP_LINES = 0.2
DODGE_WIDTH = 0.7
AXIS_X_SIZE = 10
#word wrap
compDf$term = str_wrap(compDf$term,width = 10)
#draw plot
sensGraph = ggplot(compDf , aes(y = RR, x = term,color=term, shape=term)) + facet_wrap(~ outcome,nrow=1)+
  geom_errorbar(aes(ymax = conf.high, ymin = conf.low), size = LINE_WIDTH, width =
                  ERROR_BAR_TOP,position = position_dodge(width = DODGE_WIDTH)) +
  geom_point(size = POINT_WIDTH,position = position_dodge(width = DODGE_WIDTH)) +
  scale_shape_manual(values=c(15,17,16,18,8,16,17,15,18,8,9))+
  scale_color_manual(values=c( "#009E73","#D55E00","#0072B2","#CC79A7","#E69F00","#999999","#000000","#E69F00","#009E73","#F0E442"))+
  geom_hline(aes(yintercept = 1), size = DASH_WIDTH, linetype = "dashed")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", function(x) round(10^x,2)))+
  theme_bw() +   ylab("Risk ratio") + xlab("") + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.y = element_line(size = GRID_WIDTH),
        legend.position ='none',#legend.margin=LEGEND_MAR,
        #legend.text=element_text(size=LEGEND_SIZE),
        axis.text.x= element_text(size = AXIS_X_SIZE,angle = 0, hjust = 0.5,vjust=0),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = AXIS_Y_SIZE,angle = 0,hjust = 0.5),axis.title = element_text(size = Y_TITLE_SIZE),
        strip.text = element_text(size=LABEL_SIZE),
        panel.spacing.x = unit(0,'cm'),panel.border = element_rect(size=STRIP_LINES,linetype = 'solid'),
        axis.line = element_line(linetype = 'solid',size=LINE_WIDTH),strip.background = element_rect(colour="black", fill="gray95",size = LINE_WIDTH),)
sensGraph
ggsave("sensititivty.png",plot = sensGraph, width = unit(9,'cm'),height=unit(4,'cm'))

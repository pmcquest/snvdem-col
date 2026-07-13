# Produces Figure 2 and appendix figure 1
# Requires:
#     - full.Rdata
#     - country_aggregated.Rdata
# Produces:
#     - coef_plot_democracy.pdf
#     - coef_plot_democracy_appendix.pdf

# if not already installed
# install.packages("dplyr","dotwhisker","ggplot2","broom","lfe")

# please set your working directory to the /code folder in the parent directory which also contains the /data and /figures folders
# setwd()

library(dplyr)
require(dotwhisker)
library(ggplot2)
require(broom)
library(lfe)

#### FULL ####
# Use all forest data
load("../data/output/full.Rdata")
load("../data/output/country_aggregated.Rdata")

# filter to only terms I will show in figures
to_include = c("democracy_BX","pr","democracy_BX:pr","Ag.emp.l","democracy_BX:Ag.emp.l")

# generate tidy results
m1_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|FID + year|0|un + year, data=full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "All") %>% mutate(level = "cell") %>% mutate(forest = "full")
m2_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|un + year|0|un + year, data=country_full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "All") %>% mutate(level = "country") %>% mutate(forest = "full")
m3_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*Ag.emp.l|FID + year|0|un + year, data=full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Ag employment (pct)") %>% mutate(level = "cell") %>% mutate(forest = "full")
m4_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*Ag.emp.l|un + year|0|un + year, data=country_full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Ag employment (pct)") %>% mutate(level = "country") %>% mutate(forest = "full")
m5_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*pr|FID + year|0|un + year, data=full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Majoritarian") %>% mutate(level = "cell") %>% mutate(forest = "full")
m6_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*pr|un + year|0|un + year, data=country_full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Majoritarian") %>% mutate(level = "country") %>% mutate(forest = "full")
m3.1_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*Ag.pct.l|FID + year|0|un + year, data=full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Ag pct GDP") %>% mutate(level = "cell") %>% mutate(forest = "full")
m4.1_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*Ag.pct.l|un + year|0|un + year, data=country_full) %>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Ag pct GDP") %>% mutate(level = "country") %>% mutate(forest = "full")

#### TROPICAL ####
# filter to only tropical forest
tropical_sub=c("Tropical rainforest","Subtropical mountain system","Subtropical dry forest","Tropical dry forest","Tropical moist deciduous forest","tropical mountain system","Subtropical humid forest","Tropical shrubland")
full <- full %>% filter(GEZ_TERM %in% tropical_sub)

load("../data/output/country_aggregated_tropical.Rdata")
country_full<-country_full_tropical

# generate tidy results
m7_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|FID + year|0|un + year, data=full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "All") %>% mutate(level = "cell") %>% mutate(forest = "tropical")
m8_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|un + year|0|un + year, data=country_full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "All") %>% mutate(level = "country") %>% mutate(forest = "tropical")
m9_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*Ag.emp.l|FID + year|0|un + year, data=full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Ag employment (pct)") %>% mutate(level = "cell") %>% mutate(forest = "tropical")
m10_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*Ag.emp.l|un + year|0|un + year, data=country_full) %>%  
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Ag employment (pct)") %>% mutate(level = "country") %>% mutate(forest = "tropical")
m11_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*pr|FID + year|0|un + year, data=full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Majoritarian") %>% mutate(level = "cell") %>% mutate(forest = "tropical")
m12_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*pr|un + year|0|un + year, data=country_full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Majoritarian") %>% mutate(level = "country") %>% mutate(forest = "tropical")
m9.1_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*Ag.pct.l|FID + year|0|un + year, data=full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Ag pct GDP") %>% mutate(level = "cell") %>% mutate(forest = "tropical")
m10.1_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*Ag.pct.l|un + year|0|un + year, data=country_full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Ag pct GDP") %>% mutate(level = "country") %>% mutate(forest = "tropical")

#### NONTROPICAL ####
# filter to only nontropical forest
load("../data/output/full.Rdata")
full <- full %>% filter(!(GEZ_TERM %in% tropical_sub))

load("../data/output/country_aggregated_nontropical.Rdata")
country_full<-country_full_nontropical

# generate tidy results
m13_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|FID + year|0|un + year, data=full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "All") %>% mutate(level = "cell") %>% mutate(forest = "nontropical")
m14_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|un + year|0|un + year, data=country_full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "All") %>% mutate(level = "country") %>% mutate(forest = "nontropical")
m15_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*Ag.emp.l|FID + year|0|un + year, data=full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Ag employment (pct)") %>% mutate(level = "cell") %>% mutate(forest = "nontropical")
m16_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*Ag.emp.l|un + year|0|un + year, data=country_full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Ag employment (pct)") %>% mutate(level = "country") %>% mutate(forest = "nontropical")
m17_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*pr|un + year|0|un + year, data=country_full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Majoritarian") %>% mutate(level = "cell") %>% mutate(forest = "nontropical")
m18_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*pr|un + year|0|un + year, data=country_full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Majoritarian") %>% mutate(level = "country") %>% mutate(forest = "nontropical")
m15.1_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*Ag.pct.l|FID + year|0|un + year, data=full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Ag pct GDP") %>% mutate(level = "cell") %>% mutate(forest = "nontropical")
m16.1_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX*Ag.pct.l|un + year|0|un + year, data=country_full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "Ag pct GDP") %>% mutate(level = "country") %>% mutate(forest = "nontropical")

all_models<-rbind(m1_df,m2_df,m3_df,m3.1_df,m4_df,m4.1_df,m5_df,m6_df,m7_df,m8_df,m9_df,m9.1_df,m10_df,m10.1_df,m11_df,m12_df,m13_df,m14_df,m15_df,m15.1_df,m16_df,m16.1_df,m17_df,m18_df)
save(all_models, file="../data/output/democracy_model_results.Rdata")

#### PRODUCES FIGURE 3 ####

load(file="../data/output/democracy_model_results.Rdata")
models <- all_models %>% mutate(term = replace(term,term=="democracy_BX","Democracy")) %>%
    mutate(coef = term) %>%
    mutate(term = paste(term,level,sep = ", ")) %>%
    filter(model=="All" | model=="Majoritarian") %>% 
    filter(forest == "full") %>%
    filter(coef == "Democracy") %>% arrange(desc(model))
models$model <- factor(models$model, 
                       levels = c("All","Majoritarian"), 
                       labels = c("All","Majoritarian"))

plot2<-dwplot(na.omit(models),vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
              dot_args = list(aes(shape = model)),
              whisker_args = list(aes(linetype = model)))+ #facet_grid(.~ forest, drop=TRUE, space = "free") +
    theme_classic() + xlab("Percentage point change in forest cover per year") + ylab("") +
    # ggtitle("Relationship between democratic transitions and forest cover change") +
    scale_colour_grey(start = .1, end = .1, # if start and end same value, use same colour for all models 
                      name = "Model", 
                      breaks = c(0, 1),
                      labels = c("Majoritarian", "All")) + 
    theme(plot.title = element_text(face="bold"),
          legend.position = c(0.01, 0.01),
          legend.justification = c(0, 0),
          legend.background = element_rect(colour="transparent"),
          legend.title.align = .5,
          legend.title = element_blank())
ggsave(file="../figures/coef_plot_democracy.pdf",plot = plot2,device = "pdf",width = 7,height = 2,units = "in")


#### PRODUCES APPENDIX FIGURE 1 ####
models <- all_models %>% mutate(coef = term) %>%
    mutate(term = paste(term,level,sep = ", ")) %>%
    # filter(forest == "full") %>%
    filter(coef == "democracy_BX")

plot2<-dwplot(na.omit(models),vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
              dot_args = list(aes(shape = model)),
              whisker_args = list(aes(linetype = model)))+ facet_grid(.~ forest, drop=TRUE, space = "free") +
    theme_bw() + xlab("Coefficient Estimate") + ylab("") +
    # ggtitle("Relationship between democratic transitions and forest cover change") +
    scale_colour_grey(start = .1, end = .1, # if start and end same value, use same colour for all models 
                      name = "Model", 
                      breaks = c(0, 1),
                      labels = c("Majoritarian", "All")) + 
    theme(plot.title = element_text(face="bold"),
          legend.position = c(0, 0.01),
          legend.justification = c(0, 0),
          legend.background = element_rect(colour="grey80"),
          legend.title.align = .5)

addSmallLegend <- function(myPlot, pointSize = 0.6, textSize = 6, spaceLegend = 0.05) {
    myPlot +
        guides(shape = guide_legend(override.aes = list(size = pointSize)),
               color = guide_legend(override.aes = list(size = pointSize))) +
        theme(legend.title = element_blank(),
              legend.text  = element_text(size = textSize),
              legend.key.size = unit(spaceLegend, "lines"))
}

plot2<-addSmallLegend(plot2)

ggsave(file="../figures/coef_plot_democracy_appendix.pdf",plot = plot2,device = "pdf",width = 7,height = 4,units = "in")


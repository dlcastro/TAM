library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(ggthemes)

##Author: Daniel Lopes de Castro - http://decastro.me - https://orcid.org/0000-0001-8512-5520
## File Name: ggWrightMap.R


##How to use: first you must generate the rasch throught tam(data.file)

##############################################
# GGplot WrightMap function
ggWrightMap <- function(object, sort = TRUE, xmin = -6, xmax = 6, graph = "hist", side = FALSE,
                        title = 'Person-Item Wrigth Map',
                        x.item.lab = "Latent Dimension (Logit)",
                        y.item.lab = "Items",
                        y.person.lab = "Respondents",
                        title.exposure = "Item Exposure",
                        item.size = 10,
                        dodge = FALSE,
                        n.dodge = 1)
{

    #Extract Item and sort, if wanted
    Items <- object$item
    if(sort==TRUE){
        Items$item <- reorder(Items$item, -Items$xsi.item)
    } else{
        Items <- Items %>% mutate(ORDER =  row_number())
        Items$item <- reorder(Items$item, -Items$ORDER)
    }

    #Person Score
    Person <<- tam.wle(object)

    #Graph - Item-Map
    itemplot <- ggplot(Items, aes(x=xsi.item, y=item)) +
        geom_point() +
        ggthemes::theme_clean()+
        scale_x_continuous(n.breaks = 7, limits = c(xmin,xmax))+
        scale_y_discrete(guide = guide_axis(check.overlap = dodge, n.dodge = n.dodge))+
        theme(axis.text.y = element_text(size = item.size))+
        xlab(x.item.lab)+
        ylab(y.item.lab)

    #PersonMap - Density
    dens_person <- ggplot(Person, aes(x = theta)) +
        geom_density(alpha = 0.4) +
        scale_x_continuous(n.breaks = 7, limits = c(xmin,xmax))+
        ggthemes::theme_clean()+
        theme(axis.line.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.x=element_blank(),
              panel.grid.minor.x=element_blank(),
              panel.grid.major.x=element_blank(),
              panel.background =element_blank(),
              # plot.background = element_blank(),
              panel.grid = element_line(color="black"))+
        ylab(y.person.lab)+
        scale_y_continuous(labels = scales::percent)

    #PersonMap - Histogram
    hist_person <- ggplot(Person, aes(x = theta)) +
        scale_x_continuous(n.breaks = 7, limits = c(xmin,xmax))+
        geom_histogram(color="white") +
        ggthemes::theme_clean()+
        theme(axis.line.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.x=element_blank(),
              panel.grid.minor.x=element_blank(),
              panel.grid.major.x=element_blank(),
              panel.background =element_blank(),
              panel.grid = element_line(color="black"))+
        ylab(y.person.lab)

    #How many respondents have been exposed to each item?
    if(side==TRUE){
        lateral <- ggplot(Items) +
        geom_bar(aes(x=item, y=N/nrow(Person)),stat="identity", colour="white")+
        coord_flip()+
        ggthemes::theme_clean()+
        theme(axis.line.x=element_blank(),
              axis.text.y=element_blank(),
              panel.background =element_blank(),
              panel.grid = element_line(color="black"),
              plot.background = element_blank(),
              axis.title = element_blank(),
              plot.title = element_text(size=10))+
            scale_y_continuous(labels = scales::percent)+
        ggtitle(title.exposure)

        a.size <- 4
        b.size <- 1


    } else {
        lateral <- NULL
        a.size <- 100
        b.size <- 1
    }


    #GraphTitle
    TITLE <- plot_annotation(title = title,
                    theme = theme(plot.title = element_text(size = 14,hjust = 0.5)))
    LAYOUT <- plot_layout(ncol = 2, nrow = 2, widths = c(a.size, b.size), heights = c(1, 4))

    #PlotGraph without exposure
    ##Histogram
    if(graph=="hist"){
        print(hist_person + plot_spacer() + itemplot  + lateral + TITLE + LAYOUT)
    }

    ##Desinsity
    if(graph=="dens"){
       print(dens_person + plot_spacer() + itemplot  + lateral +
                 plot_layout(ncol = 2, nrow = 2, widths = c(a.size, b.size), heights = c(1, 4)))
    }



}






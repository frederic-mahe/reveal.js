require(dplyr)
require(tidyr)
require(ggplot2)
require(scales)
require(devtools)
require(ggmap)
require(plotly)
require(animation)



#
#	PopphylData: Pimp my graph
#

dp = read.table("data_and_script_part2/popphyl_data.csv",sep=",",header = T)

dp %>% tbl_df %>% View

# Relation between size and genetic diversity
png("images/popphyl/p1.png",units = 'in', width = 10, height = 5, res = 300)
dp %>% ggplot() + geom_point(aes(x=adult_size,y=piS))
dev.off()
# Add a logarithmic scale

png("images/popphyl/p2.png",units = 'in', width = 10, height = 5, res = 300)
dp %>% ggplot() + geom_point(aes(x=adult_size,y=piS)) + scale_x_log10(limits = c(0.1,100), breaks = c(1,10,100)) + scale_y_log10()
dev.off()
# add information on phylum


png("images/popphyl/p3.png",units = 'in', width = 10, height = 5, res = 300)
dp %>% ggplot(aes(x = adult_size, y = piS)) + geom_point(size = 3,aes(shape = Taxo_for_fig  )) + scale_x_log10(limits = c(0.1,100), breaks = c(1,10,100)) + scale_y_log10()
dev.off()
# add information on parental investment

png("images/popphyl/p4.png",units = 'in', width = 10, height = 5, res = 300)
baseplot = dp %>%  mutate(parental_invest = propagule_size / adult_size) %>% na.omit  %>% ggplot(aes(x=adult_size,y =piS)) + geom_point(size =3, aes(color = parental_invest, shape = Taxo_for_fig  )) + scale_x_log10(limits = c(0.1,100), breaks = c(1,10,100)) + scale_y_log10()
baseplot + scale_color_gradientn(colours = rainbow(7) , trans = "log", limits = c(0.0005,1), values =c (0.001,0.01,0.1,1), breaks = c(0.001,0.01,0.1,1), labels = comma)  + theme_bw()
dev.off()



png("images/popphyl/p5.png",units = 'in', width = 10, height = 5, res = 300)
# Add a regression line
baseplot + scale_color_gradientn(colours = rainbow(7) , trans = "log", limits = c(0.0005,1), values =c (0.001,0.01,0.1,1), breaks = c(0.001,0.01,0.1,1), labels = comma)  + theme_bw() + geom_smooth(method = "lm", se = TRUE, size = 0.7, color = "black", formula = y ~ x)
dev.off()


# Add the equation on the regression line:
# First a function to pass through annotate: (http://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph)

# Download and use function to add linear  regression equation on a plot
require(devtools)
source_gist("524eade46135f6348140")

# use the stat_smooth_func 


png("images/popphyl/p6.png",units = 'in', width = 10, height = 5, res = 300)
baseplot + scale_color_gradientn(colours = rainbow(7) , trans = "log", limits = c(0.0005,1), values =c (0.001,0.01,0.1,1), breaks = c(0.001,0.01,0.1,1), labels = comma)  + theme_bw() + geom_smooth(method = "lm", se = TRUE, size = 0.7, color = "black", formula = y ~ x) + stat_smooth_func(geom="text", method = "lm", hjust= 0, parse =TRUE )
dev.off()



png("images/popphyl/p7.png",units = 'in', width = 10, height = 5, res = 300)
# Pimp 
dp %>%  mutate(parental_invest = propagule_size / adult_size) %>% na.omit  %>% ggplot(aes(x=adult_size,y =piS)) + geom_point(size =3, aes(color = parental_invest, shape = Taxo_for_fig  )) + scale_x_log10(limits = c(0.1,100), breaks = c(1,10,100)) + scale_y_log10() + scale_color_gradientn(name = "Parental investment",colours = rainbow(7) , trans = "log", limits = c(0.0005,1), values =c (0.001,0.01,0.1,1), breaks = c(0.001,0.01,0.1,1), labels = comma) + scale_shape(name = "Taxonomic group") + theme_bw() + geom_smooth(method = "lm", se = TRUE, size = 0.7, color = "black", formula = y ~ x) + stat_smooth_func(geom="text", method = "lm", hjust= 0, parse =TRUE ) + xlab("Adult size (cm)") + ylab(expression(paste(pi,"s"))) +  theme(text= element_text(family="Bookman",size=14))
dev.off()

# The future ! 

plot_ly(dp %>%   mutate(parental_invest = propagule_size / adult_size) %>% na.omit , x = adult_size, y = piS, text = paste("Species: ", Species,"\n Family: ",Taxo_for_fig), color = log(parental_invest), mode = "markers")  %>% layout(xaxis = list(title = "Adult size " , type = "log"))  %>% layout(yaxis = list(title = "Pi_s", type = "log"))  


#
#	Spatial and temporal Dataset
#



# Import data)
data = read.table("data_and_script_part2/uniqDataOcapi.csv",sep="|",header=T,na.strings = "NA", fill=TRUE, strip.white=TRUE, blank.lines.skip = TRUE)
# Look at date type
str(data)

# Now filtering the data; formating some columns and adding new ones: 
td = data %>% tbl_df %>% na.omit %>% mutate(.,lat = as.numeric(LAT_IDENTIF)) %>% mutate(.,long = as.numeric(LONG_IDENTIF)) %>%  mutate(.,date = as.Date(DATEDEBUT,"%m/%d/%Y")) %>% mutate(.,datesp = date) %>% separate(.,datesp,c("year","month","day")) %>% select(.,ID_SITE,long,lat,date,year,month,ESPECE,NBINDIV,NBMALES,NBFEMELLES)

td

# Ready to look at ? "


png("images/culicoide/p1.png",units = 'in', width = 10, height = 5, res = 300)
# First look at the all dataset: boxplot of every month catches 
td %>% mutate(., months = format(date,"%m-%y"))  %>%   ggplot() + geom_boxplot(aes(group = months ,x=date,y= NBINDIV))  + scale_y_log10() + theme_bw()
dev.off()


# illustrate how to change breaks when x are dates

png("images/culicoide/p2.png",units = 'in', width = 10, height = 5, res = 300)
td %>% mutate(., months = format(date,"%m-%y"))  %>%   ggplot() + geom_boxplot(aes(group = months ,x=date,y= NBINDIV))  + scale_y_log10()  + theme_bw() + scale_x_date(breaks = "6 month", labels = date_format("%m-%y")) 
dev.off()



png("images/culicoide/p2.png",units = 'in', width = 10, height = 5, res = 300)
td %>% mutate(., months = format(date,"%m-%y"))  %>%   ggplot() + geom_boxplot(aes(group = months ,x=date,y= NBINDIV))  + scale_y_log10()  + theme_bw() + scale_x_date(breaks = "6 month", labels = date_format("%m-%y")) 
dev.off()


# Now in a completly different approch, I lose the date system.



png("images/culicoide/p3.png",units = 'in', width = 10, height = 5, res = 300)
td %>%  ggplot() + geom_boxplot(aes(x=month,y= NBINDIV ,fill=year)) + scale_y_log10() + scale_fill_brewer(palette = "Set1") + theme_bw()
dev.off()

# Get the species with most catches: 


td %>% group_by(ESPECE) %>% summarize(mean(NBINDIV)) %>% top_n(.,10)
# Plot data for one of them (imicola) and for three years


png("images/culicoide/p5.png",units = 'in', width = 10, height = 5, res = 300)
td %>% filter(ESPECE == "imicola")  %>% filter(year %in% c("2009","2010","2011"))  %>% group_by(year) %>%  ggplot() + geom_boxplot(aes(x=month,y= NBINDIV ,fill=year)) + scale_y_log10() + scale_fill_brewer(palette = "Set2") + theme_bw() 
dev.off()



png("images/culicoide/p6.png",units = 'in', width = 10, height = 5, res = 300)
# Subset data for the three species and the three years
tdfilt = td %>% filter(ESPECE %in% c("imicola","dewulfi","chiopterus"))  %>% filter(year %in% c("2009","2010","2011")) 
# Make one plot for each species: 
tdfilt %>% group_by(year) %>%  ggplot() + geom_boxplot(aes(x=month,y= NBINDIV ,fill=year)) + scale_y_log10() + scale_fill_brewer(palette = "Set2") + theme_bw() + facet_grid(ESPECE ~ .) 
dev.off()



png("images/culicoide/p7.png",units = 'in', width = 10, height = 5, res = 300)
# Make one plot for each species and for each year 
tdfilt %>% group_by(year) %>%  ggplot() + geom_boxplot(aes(x=month,y= NBINDIV ,fill=year)) + scale_y_log10() + scale_fill_brewer(palette = "Set2") + theme_bw() + facet_grid(year ~ ESPECE)  
dev.off()


png("images/culicoide/p7.png",units = 'in', width = 10, height = 5, res = 300)
# Show the equivalent with violin plot
tdfilt %>% group_by(year) %>%  ggplot() + geom_violin(aes(x=month,y= NBINDIV ,fill=year),scale = "area", trim = FALSE  ) + scale_y_log10() + scale_fill_brewer(palette = "Set2") + theme_bw() + facet_grid(year ~ ESPECE)  
dev.off()


# Geom jitter gives also an idea of the sample size ! 


png("images/culicoide/p8.png",units = 'in', width = 10, height = 5, res = 300)
tdfilt %>% group_by(year) %>%  ggplot() + geom_jitter(aes(x=month,y= NBFEMELLES ,color=year) ) + scale_y_log10() + scale_fill_brewer(palette = "Set2") + theme_bw() + facet_grid(year ~ ESPECE)  
dev.off()

# Here I define my own function. It's a hack to convert month from int to litteral string ...
monthformat = function(x){ return(format(as.Date(paste("1",x,sep="/"),format="%d/%m"),"%b"))}

# I'm not sure this is useful anymore.
library(scales)


png("images/culicoide/p9.png",units = 'in', width = 10, height = 5, res = 300)
# Now with some formatting (like axes and font)
tdfilt %>% group_by(year) %>%  ggplot() + geom_boxplot(aes(x=month,y= NBFEMELLES ,fill=year)) + scale_y_log10(labels = comma) + scale_x_discrete(labels = monthformat) +  scale_fill_brewer(palette = "Set2",name="année") + theme_bw() + facet_grid(year ~ ESPECE) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  xlab(NULL) + ylab("Captures /  Pièges / Nuit") + theme(text = element_text(family="Times", size = 14)) 
dev.off()

png("images/culicoide/p10.png",units = 'in', width = 10, height = 5, res = 300)
# Now we lose the color : useless
tdfilt %>% group_by(year) %>%  ggplot() + geom_boxplot(aes(x=month,y= NBFEMELLES )) + scale_y_log10(labels = comma) + scale_x_discrete(labels = monthformat) + theme_bw() + facet_grid(year ~ ESPECE) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  xlab(NULL) + ylab("Captures /  Pièges / Nuit") + theme(text = element_text(family="Times", size = 14)) 
dev.off()


png("images/culicoide/p11.png",units = 'in', width = 10, height = 5, res = 300)
# And use it to show the sex ratio of the catches
tdfilt %>% regroup(list("year","month")) %>% mutate(Sexratio = mean(NBFEMELLES/NBINDIV)) %>% group_by(year) %>%  ggplot() + geom_boxplot(aes(x=month,y= NBINDIV ,fill = Sexratio)) + scale_y_log10(labels = comma) + scale_x_discrete(labels = monthformat) + theme_bw() + facet_grid(year ~ ESPECE) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  xlab(NULL) + ylab("Captures /  Pièges / Nuit") + theme(text = element_text(family="Times", size = 14)) 
dev.off()

# Let's project the data on a map: 

png("images/culicoide/p12.png",units = 'in', width = 10, height = 5, res = 300)
France = get_map(location = "France",zoom = 5,  source = "google",maptype = "satellite",color = "bw")
ggmap(France) + geom_point(data = tdfilt, aes(x = long, y=lat)) + facet_grid(year ~ ESPECE)  
dev.off()

png("images/culicoide/p13.png",units = 'in', width = 10, height = 5, res = 300)
ggmap(France) + geom_point(data = tdfilt %>% regroup(list("year","ID_SITE","ESPECE")) %>% mutate(avg = log(mean(NBFEMELLES))), aes(x = long, y=lat, size = avg)) + facet_grid(year ~ ESPECE) + scale_x_continuous(limits = c(-6.5,9.5)) + scale_y_continuous(limits = c(41,51) ) + scale_size_continuous(name="Moyenne Captures/femelles/nuit",breaks=c(0,1,2,3,4,5),labels =c(1,10,100,1000,10000,100000))
dev.off()





# Okay, let's animate that : 

# First we need to handle the month:

months = unique(tdfilt$month)
months[sort.list(months)]

# And now save a GIF 

saveGIF({for(i in months[sort.list(months)]){ print(ggmap(France) + geom_point(data = tdfilt %>% filter(month == i) %>% regroup(list("year","ID_SITE","ESPECE")) %>% mutate(avg = log(mean(NBFEMELLES))), aes(x = long, y=lat, size = avg)) + facet_grid(year ~ ESPECE) + scale_x_continuous(limits = c(-6.5,9.5)) + scale_y_continuous(limits = c(41,51) ) +  ggtitle(monthformat(i)) + scale_size_continuous(name="Moyenne Captures/femelles/nuit", limits=c(0,6),breaks  = c(0,1,2,3,4,5),labels=c(1,10,100,1000,10000,100000)))}}, interval = 1, movie.name="Culicoids.gif")






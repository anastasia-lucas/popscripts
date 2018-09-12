
#Adapted from: 
#http://estebanmoro.org/2012/11/temporal-networks-with-igraph-and-r-with-20-lines-of-code/
#Tried to clean this up before posting, so feel free to open an issue if there are any bugs.

library(igraph)
library(png)

dat <- read.delim("raw.tsv")

map <- rbind(data.frame(Name=sort(c(as.character(levels(dat$from)), as.character(levels(dat$to))))), 
             data.frame(Name=c("Nick", "Wills", "Nysha", "Robby", "David"))
map$id <- seq_along(map$Name)

new <- merge(merge(dat, map, by.y="Name", by.x="from"), map, by.y="Name", by.x="to")[]
names(new) <- c("toName", "fromName", "episode", "time", "from", "to")
edges <- new[order(new$from),]

###Add images
#This includes all of the contestants, including those with no connections
#You can delete them from the rasters list and regenerate the map file if you
#don't want them to be displayed

rasters <- as.list(c(imgType1='',imgType2='',imgType3='',imgType4='',imgType5='',imgType6='',imgType7='',imgType8='',imgType9=''
                     ,imgType10='',imgType11='',imgType12='',imgType13='',imgType14='',imgType15='',imgType16='',imgType17='',imgType18=''
                     ,imgType19='',imgType20='',imgType21='',imgType22='',imgType23='',imgType24='',imgType25='',imgType26='',imgType27=''
                     ,imgType28='', imgType29='', imgType30='',imgType31='',imgType32='',imgType33='',imgType34='',imgType35=''))

rasters$imgType1 <- readPNG("images/Angela.png", native=TRUE)
rasters$imgType2 <- readPNG("images/Annaliese.png", native=TRUE)
rasters$imgType3 <- readPNG("images/Astrid.png", native=TRUE)
rasters$imgType4 <- readPNG("images/Benoit.png", native=TRUE)
rasters$imgType5 <- readPNG("images/Bibiana.png", native=TRUE)
rasters$imgType6 <- readPNG("images/Caroline.png", native=TRUE)
rasters$imgType7 <- readPNG("images/Cassandra.png", native=TRUE)
rasters$imgType8 <- readPNG("images/Chelsea.png", native=TRUE)
rasters$imgType9 <- readPNG("images/Chris.png", native=TRUE)
rasters$imgType10 <- readPNG("images/Christen.png", native=TRUE)
rasters$imgType11 <- readPNG("images/Colton.png", native=TRUE)
rasters$imgType12 <- readPNG("images/Connor.png", native=TRUE)
rasters$imgType13 <- readPNG("images/Diggy.png", native=TRUE)
rasters$imgType14 <- readPNG("images/Eric.png", native=TRUE)
rasters$imgType15 <- readPNG("images/Jacqueline.png", native=TRUE)
rasters$imgType16 <- readPNG("images/Jenna.png", native=TRUE)
rasters$imgType17 <- readPNG("images/Joe.png", native=TRUE)
rasters$imgType18 <- readPNG("images/John.png", native=TRUE)
rasters$imgType19 <- readPNG("images/JordanK.png", native=TRUE)
rasters$imgType20 <- readPNG("images/JordanM.png", native=TRUE)
rasters$imgType21 <- readPNG("images/Jubilee.png", native=TRUE)
rasters$imgType22 <- readPNG("images/Kamil.png", native=TRUE)
rasters$imgType23 <- readPNG("images/Kendall.png", native=TRUE)
rasters$imgType24 <- readPNG("images/Kenny.png", native=TRUE)
rasters$imgType25 <- readPNG("images/Kevin.png", native=TRUE)
rasters$imgType26 <- readPNG("images/Krystal.png", native=TRUE)
rasters$imgType27 <- readPNG("images/Leo.png", native=TRUE)
rasters$imgType28 <- readPNG("images/Olivia.png", native=TRUE)
rasters$imgType29 <- readPNG("images/Shushanna.png", native=TRUE)
rasters$imgType30 <- readPNG("images/Tia.png", native=TRUE)
rasters$imgType31 <- readPNG("images/Nick.png", native=TRUE)
rasters$imgType32 <- readPNG("images/Wills.png", native=TRUE)
rasters$imgType33 <- readPNG("images/Nysha.png", native=TRUE)
rasters$imgType34 <- readPNG("images/Robby.png", native=TRUE)
rasters$imgType35 <- readPNG("images/David.png", native=TRUE)

img_dat <- data.frame(from=map$id,type=c('imgType1','imgType2','imgType3','imgType4','imgType5','imgType6','imgType7',
                                         'imgType8','imgType9','imgType10','imgType11','imgType12','imgType13','imgType14',
                                         'imgType15','imgType16','imgType17','imgType18','imgType19','imgType20','imgType21',
                                         'imgType22','imgType23','imgType24','imgType25','imgType26','imgType27','imgType28',
                                         'imgType29','imgType30','imgType31','imgType32','imgType33','imgType34','imgType35'))

img_map <- merge(map, img_dat, by.x="id", by.y="from")
#newmap <- data.frame(id=c(31,32,33,34,35), Name=c("Nick", "Wills", "Nysha", "Robby", "David"), type=c("imgType31","imgType32","imgType33","imgType34","imgType35"))
#img_map <- rbind(img_map, newmap)

gg <- graph.data.frame(
  edges[, c(5,6,4)],directed=FALSE, 
  vertices = lkp_map)
V(gg)$raster <- rasters[V(gg)$type]

ti <- 0

gt <- delete_edges(gg,which(E(gg)$time > ti))
layout.old <- norm_coords(layout.graphopt(gt), xmin = -1, xmax = 1, ymin = -1, ymax = 1)

#total time of the dynamics
total_time <- max(E(gg)$time)
#This is the time interval for the animation. In this case is taken to be 1/10
#of the time (i.e. 10 snapshots) between adding two consecutive nodes
dt <- 0.1
#Output for each frame will be a png with HD size 1600x900 <img draggable="false" class="emoji" alt="🙂" src="https://s.w.org/images/core/emoji/11/svg/1f642.svg">
#Make sure to make the animation directory
png(file="animation/example%03d.png", width=1600,height=900)
#Time loop starts
for(time in seq(1,total_time,dt)){
  #remove edges which are not present
  gt <- delete_edges(gg,which(E(gg)$time > time))
  #gt <- delete_vertices(gg,which(V(gg)$time > time))
  
  #with the new graph, we update the layout a little bit
  layout.new <- layout_with_fr(gt,coords=layout.old,niter=10,start.temp=0.05,grid="nogrid")
  #plot the new graph
  plot(gt,layout=layout.new,vertex.shape="raster",vertex.label=V(gg)$Name,vertex.label.dist=1.25,vertex.label.color="black",vertex.size=5+1.5*as.numeric(sapply(ego(gt, order=1), function(x) length(x))),edge.width=1.5,asp=9/16,margin=-0.15)
  
  #use the new layout in the next round
  layout.old <- layout.new
}
dev.off()

#In the animation directory run this command to combine the images
#-pix_fmt yuv420p is needed for QuickTime (Mac)
#ffmpeg -r 10 -i example%03d.png -b:v 20M -pix_fmt yuv420p output.mp4




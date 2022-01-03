library('R.utils')

#TEMPORARY
clean_mog<-function(mog){
  months = mog[,2]
  wl = mog[,6]
  out = rep(NA, 1932)
  for (i in 1:161) {
    for (j in 1:12) {
      day = (i-1)*12 + j
      if (j != months[day]) {
        months = insert(months, day, j)
        wl = insert(wl, day, NA)
      }
      out[day] = wl[day]
    }
  }
  out
}
#TEMPORARY
addLakes<-function(WL){
    erie.mog = read.csv(paste("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                          "product=monthly_mean",
                          "&application=NOS.COOPS.TAC.WL",
                          "&begin_date=18600101",
                          "&end_date=20201201",
                          "&datum=IGLD",
                          "&station=9063063",
                          "&time_zone=lst_ldt",
                          "&units=metric",
                          "&format=csv", sep=""))
    erie.mog = clean_mog(erie.mog) # 04/1938 is missing

    sup.mog = read.csv(paste("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                          "product=monthly_mean",
                          "&application=NOS.COOPS.TAC.WL",
                          "&begin_date=18600101",
                          "&end_date=19800901",
                          "&datum=IGLD",
                          "&station=9099016",
                          "&time_zone=lst_ldt",
                          "&units=metric",
                          "&format=csv", sep=""))
    sup.mog = sup.mog[,6]
    sup.mog_new = read.csv(paste("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                            "product=monthly_mean",
                            "&application=NOS.COOPS.TAC.WL",
                            "&begin_date=19801001",
                            "&end_date=20201201",
                            "&datum=IGLD",
                            "&station=9099018",
                            "&time_zone=lst_ldt",
                            "&units=metric",
                            "&format=csv", sep=""))                     
    sup.mog_new = sup.mog_new[,6]
    sup.mog = append(sup.mog, sup.mog_new)

    mich.mog <- read.csv(paste("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                          "product=monthly_mean",
                          "&application=NOS.COOPS.TAC.WL",
                          "&begin_date=18600101",
                          "&end_date=20201201",
                          "&datum=IGLD",
                          "&station=9075014",
                          "&time_zone=lst_ldt",
                          "&units=metric",
                          "&format=csv", sep=""))
    mich.mog <- mich.mog[,6]

    ont.mog <- read.csv(paste("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?",
                          "product=monthly_mean",
                          "&application=NOS.COOPS.TAC.WL",
                          "&begin_date=18600101",
                          "&end_date=20201201",
                          "&datum=IGLD",
                          "&station=9052030",
                          "&time_zone=lst_ldt",
                          "&units=metric",
                          "&format=csv", sep=""))
    ont.mog <- ont.mog[,6]

    WL = cbind(WL, sup.mog, mich.mog, erie.mog, ont.mog)
    WL
}

#Plots water levels of each of the 4 great lakes from Jan 1860 through Dec 2020.
#Requires that WL has columns named 'months', 'sup', 'mich', 'erie', 'ont' containind data from 1860-2020
plot_water_levels<-function(WL){
    #first, create tables of values to be plotted for each lake.
    #tables are (num years)x14. First col is the year,
    #second col is the average across that year,
    #and cols 3-14 are the monthly values.
    sup.df = data.frame(matrix(ncol=14, nrow=0))
    mich.df = data.frame(matrix(ncol=14, nrow=0))
    erie.df = data.frame(matrix(ncol=14, nrow=0))
    ont.df = data.frame(matrix(ncol=14, nrow=0))
    colnames(sup.df) = c('year', 'avg', 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
    colnames(mich.df) = c('year', 'avg', 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
    colnames(erie.df) = c('year', 'avg', 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
    colnames(ont.df) = c('year', 'avg', 'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
    for (i in seq(1, nrow(WL), 12)) {
        sup.ave = 0
        mich.ave = 0
        erie.ave = 0
        ont.ave = 0
        for (j in 0:11) {
            sup.ave = sup.ave + WL$sup[i+j]
            mich.ave = mich.ave + WL$mich[i+j]
            erie.ave = erie.ave + WL$erie[i+j]
            ont.ave = ont.ave + WL$ont[i+j]
        }
        sup.ave = sup.ave/12
        mich.ave = mich.ave/12
        erie.ave = erie.ave/12
        ont.ave = ont.ave/12

        year = (i-1)/12 + 1860

        sup.df[nrow(sup.df)+1, ] = c(year, sup.ave, WL$sup[i:(i+11)])
        mich.df[nrow(mich.df)+1, ] = c(year, mich.ave, WL$mich[i:(i+11)])
        erie.df[nrow(erie.df)+1, ] = c(year, erie.ave, WL$erie[i:(i+11)])
        ont.df[nrow(ont.df)+1, ] = c(year, ont.ave, WL$ont[i:(i+11)])
    }

    #begin plotting
    out = 'Water_Levels.pdf'
    if (file.exists(out)) {
        file.remove(out)
    }
    pdf(file = out, width = 8, height = 7, onefile = FALSE)
    plot.new()
    par(mfrow = c(3,1), mar=c(0,0,0,0), oma=c(4,7,4,7))
    layout(matrix(c(1,2,2,3), nrow=4, ncol=1, byrow=TRUE))

    #superior
    plot(sup.df$year, sup.df[,3], pch=16, cex=0.8, col='lightblue', axes=FALSE, xlim=c(1855,2025), ylim=c(182.2,185.2))
    for (i in 4:14) {points(sup.df$year, sup.df[,i], cex=0.8, pch=16, col='lightblue')} #plot monthly values
    points(sup.df$year, sup.df$avg, pch=16, cex=0.5, col='darkblue')
    segments(1860, mean(sup.df$avg), 2020, mean(sup.df$avg), col='red')
    legend(1850, 184.7, legend='Lake Superior', bty='n', cex=1.5, xjust=0)
    axis(side=3, at=seq(1860, 2020, 10), labels=FALSE, tck=-0.05)
    axis(side=3, at=seq(1860, 2020, 2), labels=FALSE, tck=-0.03)
    axis(side=2, at=seq(183, 185, 1), tck=-0.05, cex.axis=1.2, las=2)
    axis(side=2, at=seq(182.4, 185, 0.2), tck=-0.03, labels=FALSE)

    #michigan/huron and erie
    plot(mich.df$year, mich.df[,3], pch=16, cex=0.8, col='lightblue', axes=FALSE, xlim=c(1855,2025), ylim=c(172.5,178.2))
    for (i in 4:14) {points(mich.df$year, mich.df[,i], pch=16, cex=0.8, col='lightblue')} #plot monthly values
    points(mich.df$year, mich.df$avg, pch=20, cex=0.5, col='darkblue', xaxt='n', yaxt='n')
    for (i in 3:14) {points(erie.df$year, erie.df[,i], pch=16, cex=0.8, col='lightblue')}
    points(erie.df$year, erie.df$avg, pch=16, cex=0.5, col='darkblue')
    segments(c(1860,1860), c(mean(mich.df$avg),mean(erie.df$avg, na.rm=TRUE)), c(2020,2020), c(mean(mich.df$avg),mean(erie.df$avg, na.rm=TRUE)), col='red')
    legend(1850, 178.5, legend='Lake Michigan and Huron', bty='n', cex=1.5, xjust=0)
    legend(1850, 175.6, legend='Lake Erie', bty='n', cex=1.5, xjust=0)
    axis(side=2, at=seq(173, 178, 1), tck=-0.028, cex.axis=1.2, las=2)
    axis(side=2, at=seq(173, 178, 0.2,), tck=-0.015, labels=FALSE)
    
    #ontario
    plot(ont.df$year, ont.df[,3], pch=16, cex=0.8, col='lightblue', axes=FALSE, xlim=c(1855,2025), ylim=c(73.5,76.2))
    for (i in 4:14) {points(ont.df$year, ont.df[,i], pch=16, cex=0.8, col='lightblue')} #plot monthly values
    points(ont.df$year, ont.df$avg, pch=16, cex=0.5, col='darkblue')
    segments(1860, mean(ont.df$avg), 2020, mean(ont.df$avg), col='red')
    legend(1850, 76.4, legend='Lake Ontario', bty='n', cex=1.5, xjust=0)
    axis(side=1, at=seq(1860, 2020, 10), labels=FALSE, tck=-0.05, line=1)
    axis(side=1, at=seq(1860, 2020, 2), labels=FALSE, tck=-0.03, line=1)
    axis(side=1, at=seq(1860, 2020, 20), tck=-0.05, cex.axis=1.2, line=1)
    axis(side=2, at=seq(74, 76, 1), tck=-0.05, cex.axis=1.2, las=2)
    axis(side=2, at=seq(73.6, 76, 0.2), tck=-0.03, labels=FALSE)

    box('inner', lwd=2)
    mtext('Water surface elevation (meters)', side=2, outer=TRUE, line=5, cex=1.2)

    dev.off()
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------

#WL is table containing all the lake data with a column per gauge
WL = data.frame(c(1:1932)) # number of months from 1860-2020

WL = addLakes(WL) #change later if we want to use our custom made table
colnames(WL) = c('months', 'sup', 'mich', 'erie', 'ont')

plot_water_levels(WL)

print('done')
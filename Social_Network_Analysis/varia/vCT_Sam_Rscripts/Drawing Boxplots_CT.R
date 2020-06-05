#### visualizing changes in social rates pre and post hurricane

  fudge <- .15 #adjust "jitter" when plotting box plots.
  col1 <- "#356b86"; col2 <- "#f49634" #adjust color
  
  ########################################################  
  ## For probability of being accompanied (not # partners)
  ########################################################  
  
  # open pdf file for later saving
  pdf(file="pNotAlone.pdf", width=3.75, height=4.25, onefile = T) #width and height of the graphics region in inches. One file if true = multiple graphs in one file

    {
      # Group KK Stripcharts
      # Post Hurricane
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 1], #select data: group KK, q1, post-hurricne
                 col=adjustcolor(col1, .075), #set color and alpha value (transparency)
                 method= "jitter", jitter= .15, #Method to separate coincident point. Add jitter to overlap.
                 vertical =T, #Draw plots vertically
                 xlim=c(0,5), ylim=c(0,1), #plot limits 
                 pch = 19, #specifies a symbol for plotting points (circles, triangles....)
                 add=F, #add chart to current plot
                 at=1 + fudge) #set location of graph. fudge parameter to make slight to the right or left of "at" location
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
                 xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=2 + fudge, axes=F)  
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
                 xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=3 + fudge, axes=F)
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
                 xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=4 + fudge, axes=F)
      
      # Pre Hurricane
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                 xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=1 - fudge, axes=F)
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                 xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=2 - fudge, axes=F)  
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                 xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=3 - fudge, axes=F)
      stripchart(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                 xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=4 - fudge, axes=F)
      
      # Group V boxplots  
      # Post Hurricane
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, #outline shows the outliers or not
              xlim=c(0,5), ylim=c(0,1), add=T, at=1 + fudge, axes=F)
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 + fudge, axes=F)
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 + fudge, axes=F)
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 + fudge, axes=F)
      
      # Pre Hurricane
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=1 - fudge, axes=F)
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 - fudge, axes=F)
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 - fudge, axes=F)
      boxplot(A3$pNotAlone[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 - fudge, axes=F)
    } # Group KK Boxplots
    {
    # Group V Stripcharts
          # Post Hurricane
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=F, at=1 + fudge)
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=2 + fudge, axes=F)  
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=3 + fudge, axes=F)
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=4 + fudge, axes=F)
          
          # Pre Hurricane
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=1 - fudge, axes=F)
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=2 - fudge, axes=F)  
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=3 - fudge, axes=F)
          stripchart(A3$pNotAlone[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
                     xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=4 - fudge, axes=F)
      
    # Group V boxplots  
            # Post Hurricane
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=1 + fudge, axes=F)
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 + fudge, axes=F)
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 + fudge, axes=F)
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 + fudge, axes=F)
    
            # Pre Hurricane
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=1 - fudge, axes=F)
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 - fudge, axes=F)
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 - fudge, axes=F)
            boxplot(A3$pNotAlone[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 - fudge, axes=F)
    } # Group V Boxplots
  
  dev.off() #close the plot
  browseURL("pNotAlone.pdf") #Load URL into an HTML browser
    
  
  ########################################################  
  ## For probability of being accompanied (not # partners)
  ######################################################## 
  
  pdf(file="pSocial.pdf", width=3.75, height=4.25, onefile = T)
  
  {
    # Group KK Stripcharts
    # Post Hurricane
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=F, at=1 + fudge)
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=T, at=2 + fudge, axes=F)  
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=T, at=3 + fudge, axes=F)
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=T, at=4 + fudge, axes=F)
    
    # Pre Hurricane
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=T, at=1 - fudge, axes=F)
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=T, at=2 - fudge, axes=F)  
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=T, at=3 - fudge, axes=F)
    stripchart(A3$pSocial[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=T, at=4 - fudge, axes=F)
    
    # Group KK boxplots  
    # Post Hurricane
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=1 + fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 + fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 + fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 + fudge, axes=F)
    
    # Pre Hurricane
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=1 - fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 - fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 - fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupKK" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 - fudge, axes=F)
  } # Group KK Boxplots
  {
    # Group V Stripcharts
    # Post Hurricane
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,.3), add=F, at=1 + fudge)
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=2 + fudge, axes=F)  
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=3 + fudge, axes=F)
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=4 + fudge, axes=F)
    
    # Pre Hurricane
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=1 - fudge, axes=F)
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=2 - fudge, axes=F)  
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=3 - fudge, axes=F)
    stripchart(A3$pSocial[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .075), method= "jitter", jitter= .15, vertical =T,
               xlim=c(0,5), pch = 19, ylim=c(0,1), add=T, at=4 - fudge, axes=F)
    
    # Group V boxplots  
    # Post Hurricane
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=1 + fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 + fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 + fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 1], col=adjustcolor(col1, .75), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 + fudge, axes=F)
    
    # Pre Hurricane
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 1 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=1 - fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 2 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=2 - fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 3 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=3 - fudge, axes=F)
    boxplot(A3$pSocial[A3$group=="groupV" & A3$Q == 4 & A3$isPost == 0], col=adjustcolor( col2, .8), outline=F, xlim=c(0,5), ylim=c(0,1), add=T, at=4 - fudge, axes=F)
  } # Group V Boxplots
  
  dev.off(); browseURL("pSocial.pdf")
  
#'PlotDeviation
#'@description Plots deviation from the combined results from the ListCorrelation and AddIntervalsToFolder function.
#'@export
#'@importFrom graphics abline lines points text
#'@param data Combind folder structur from the ListCorrelation and AddIntervalsToFolder function.
#'@param thick Thickness of age intervals from the functions bervore.
#'@param offset Size of the area in which all correlations are calculated from the functions bervore.
#'@param plotName Name of the Plot.
#'@param colors Colors of the Groups. One COlor for each group needed.
#'@return Retruns PlotDeviation. Legend is displayed as a text file in the console
#'@author Tim Kröger
#'@note This function has only been developed for the Alfred Wegener Institute Helmholtz Centre for Polar and Marine Research and should therefore only be used in combination with their database.

PlotDeviation=function(data,thick,offset,plotName="",colors=c("black")){

  minBoundry=NA
  maxBoundry=NA

  i=0

  for(h in 1:length(ls(data))){

    korrelations=data[[h]]

    for(j in 1:length(ls(korrelations))){

      i=i+1

      maxBoundry[i]=korrelations[[j]][[2]]
      minBoundry[i]=korrelations[[j]][[3]]

    }
  }

  plot(NA,xlim=c(0-offset,offset),ylim=c(min(minBoundry),max(maxBoundry)),main=plotName,ylab="correlation",
       xlab="yr offset")

  abline(v=0,lty=3)


  for(h in 1:length(ls(data))){

    korrelations=data[[h]]

    for(i in 1:length(ls(korrelations))){

      lines(as.numeric(names(korrelations[[i]][[1]])),korrelations[[i]][[1]],col=colors[h])

    }
  }

  value=NA
  age=NA

  for(h in 1:length(ls(data))){

    korrelations=data[[h]]

    for (i in 1:length(ls(korrelations))){

      value[i]=korrelations[[i]][[4]]
      age[i]=as.numeric(names(korrelations[[i]][[4]]))

    }

    points(mean(age),mean(value),col=colors[h],pch=4,cex=2,lwd=2)

  }

  textoutput=paste("\n\nLegend:\n\n",sep="")

  for(h in 1:length(ls(data))){

    textoutput[length(textoutput)+1]=paste(names(data)[h]," = ",colors[h],"\n",sep="")

  }

  textoutput[length(textoutput)+1]=paste("\n",sep="")

  z=0

  for(h in 1:length(ls(data))){

    korrelations=data[[h]]

    for (i in 1:length(ls(korrelations))){

      z=z+1

      text((offset*-1)-offset/40,
           korrelations[[i]][[1]][[1]]+abs(min(minBoundry)-max(maxBoundry))*0.015,
           label=z,
           col=colors[h])

      textoutput[length(textoutput)+1]=paste(z," = ",names(korrelations)[i],"\n",sep="")

    }
  }

  cat(textoutput,sep="")

}

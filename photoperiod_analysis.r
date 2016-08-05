# Preliminary script for reading in geolocator data and extracting daylengths
# based on date and latitude

library(lubridate)
library(geosphere)

d = read.csv('data/file-178761964.csv', stringsAsFactors = F, header = T)
d$date = as.POSIXct(strptime(d$timestamp, format = "%m/%d/%y %H:%M"))
d$jd = yday(d$date)
daylengths = c()
for (i in 1:nrow(d)) {
  daylengths = c(daylengths, daylength(d$location.lat[i], d$jd[i]))
}
d$daylength = daylengths

# annual photoperiod for 44 deg N
dl44N = daylength(43.9, 1:365)
dl11N = daylength(11.5, 1:365)

plotDaylength = function(data, individual, new = TRUE, ...) {
  temp = subset(d, individual.local.identifier == individual)
  temp = temp[order(temp$jd, decreasing = F),]
  if(new) {
    plot(temp$jd, temp$daylength, type = 'l', xlab = "Julian day", ylab = "Day length (h)", ...)
  } else {
    points(temp$jd, temp$daylength, type = 'l', ...)
  }
}

plotDaylength(d, "A", new = T, col = 'purple', lwd = 3, ylim = c(9,16))
points(1:365, dl40N, type = 'l', col = 'gray50', lty = 'dashed', lwd = 5)
points(1:365, dl11N, type = 'l', col = 'gray80', lty = 'dotted', lwd = 7)
legend("topleft", c('Blackpoll warbler', '43.9 N', '11.5 N'), lwd = c(3, 5, 7), 
       col = c('purple', 'gray50', 'gray80'), lty = c('solid', 'dashed', 'dotted'))
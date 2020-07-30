library(ncdf4)
fn <- 'spatial_rec_13179156_strahler3.nc'
fnTxt <- 'rivSpatial.csv'
nc <- nc_open(fn)

cols <- c("basarea", "uparea", "rchindex", "rchid", "dsrch_nrch", "dsrch_rchid", "start_lat", "end_lat", "start_lon", "end_lon", "numuprch", "uprch_nrch", "uprch_rchid", "rchslope", "rchman_n", "rchwidth", "rchlength")
d <- as.data.frame(sapply(cols[1:11],function(x) ncvar_get(nc,x)))
tmp <- as.data.frame(t(ncvar_get(nc,cols[12])))
names(tmp) <- paste0(cols[12],1:10)
d[,names(tmp)] <- tmp
tmp <- as.data.frame(t(ncvar_get(nc,cols[13])))
names(tmp) <- paste0(cols[13],1:10)
d[,names(tmp)] <- tmp
tmp <- as.data.frame(sapply(cols[14:17],function(x) ncvar_get(nc,x)))
d[,cols[14:17]] <- tmp
d[is.na(d)] <- -999
write.csv(d,file=fnTxt,quote=F,row.names=F)

#nrch
#nlake
basarea <- ncvar_get(nc,'basarea')
uparea <- ncvar_get(nc,'uparea')
rchindex <- ncvar_get(nc,'rchindex')
rchid <- ncvar_get(nc,'rchid')
dsrch_nrch <- ncvar_get(nc,'dsrch_nrch')
dsrch_rchid <- ncvar_get(nc,'dsrch_rchid')
start_lat <- ncvar_get(nc,'start_lat')
end_lat <- ncvar_get(nc,'end_lat')
start_lon <- ncvar_get(nc,'start_lon')
end_lon <- ncvar_get(nc,'end_lon')
numuprch <- ncvar_get(nc,'numuprch')
uprch_nrch <- ncvar_get(nc,'uprch_nrch')
uprch_rchid <- ncvar_get(nc,'uprch_rchid')

rchslope <- ncvar_get(nc,'rchslope')
rchman_n <- ncvar_get(nc,'rchman_n')
rchwidth <- ncvar_get(nc,'rchwidth')
rchlength <- ncvar_get(nc,'rchlength')

nrch <- length(basarea)
maxup <- dim(uprch_nrch)[2]




# basarea(nrch)
# uparea(nrch)

# rchindex(nrch)
# rchid(nrch)
# dsrch_nrch(nrch)
# dsrch_rchid(nrch)

# start_lat(nrch)
# end_lat(nrch)
# start_lon(nrch)
# end_lon(nrch)
# numuprch(nrch)
# uprch_nrch(nrch,maxup)
# uprch_rchid(nrch,maxup)

# rchslope(nrch)
# rchman_n(nrch)
# rchwidth(nrch)
# rchlength(nrch)


library(ncdf4)
fn <- 'restart_2000010121_2000010121_utc_topnet_13179156_strahler3.nc'
fnTxt <- 'rivState.csv'
nc <- nc_open(fn)

nMaxQPar <- 30

cols <- c("numqpar", "q_ratep", "qm_ratep", "tentryp", "t_exitp", "q_flagp")
cols <- c("numqpar", "q_ratep", "tentryp", "t_exitp", "q_flagp")

numqpar <- as.data.frame(ncvar_get(nc,'numqpar')); names(numqpar) <- cols[1]
q_ratep <- as.data.frame(t(ncvar_get(nc,'q_ratep'))); names(q_ratep) <- paste0(cols[2],1:ncol(q_ratep))
if(is.element("qm_ratep", names(nc$var))){
 qm_ratep <- as.data.frame(t(ncvar_get(nc,'qm_ratep'))); names(qm_ratep) <- paste0(cols[2],1:ncol(q_ratep))
 }else{
 qm_ratep <- q_ratep; names(qm_ratep) <- paste0("qm_ratep",1:ncol(q_ratep))
 }
tentryp <- as.data.frame(t(ncvar_get(nc,'tentryp'))); names(tentryp) <- paste0(cols[3],1:ncol(q_ratep))
t_exitp <- as.data.frame(t(ncvar_get(nc,'t_exitp'))); names(t_exitp) <- paste0(cols[4],1:ncol(q_ratep))
q_flagp <- as.data.frame(t(ncvar_get(nc,'q_flagp'))); names(q_flagp) <- paste0(cols[5],1:ncol(q_ratep))
d <- numqpar;
d[,names(q_ratep)] <- q_ratep
d[,names(qm_ratep)] <- qm_ratep
d[,names(tentryp)] <- tentryp
d[,names(t_exitp)] <- t_exitp
d[,names(q_flagp)] <- q_flagp


write.csv(d,file=fnTxt,quote=F,row.names=F)

#nrch
#nlake

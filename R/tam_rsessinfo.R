## File Name: tam_rsessinfo.R
## File Version: 0.06


#***********************************************
# session info
tam_rsessinfo <- function()
{
    si <- Sys.info()
    si2 <- utils::sessionInfo()
    paste0( si2$R.version$version.string, " ", si2$R.version$system
            , " | nodename=", si["nodename"], " | login=", si["login"] )
}
#************************************************


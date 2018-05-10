# #' Creates an empty site table
# #'
# #' With dummy data
# #'
# #' @author Reinhard Simon
# #' @export
# #' @return dataframe
# new_site_table <- function(){
#   n = 3
#   id <- 1:n
#   shortn <- paste("ID_",id)
#   altern <- rep("", n)
#   fulln <-  rep("", n)
#   local <-  rep("", n)
#   latd <-  rep(0, n)
#   lond <- rep(0, n)
#   elev <-  rep(0, n)
#   crops <-  rep("", n)
#   aez <-  rep("", n)
#   cont<-  rep("", n)
#   creg<-  rep("", n)
#   cntry<-  rep("", n)
#   adm4<-  rep("", n)
#   adm3<-  rep("", n)
#   adm2<-  rep("", n)
#   adm1<-  rep("", n)
#   comment<-  rep("", n)
#
#   res <- as.data.frame(cbind(
#     id, shortn, altern, fulln, local, latd, lond, elev, crops, aez,
#     cont, creg, cntry, adm4, adm3, adm2, adm1,  comment),
#     stringsAsFactors = FALSE)
#   res
# }
#
# #' Gets a site table.
# #'
# #' If not yet present creates a dummy one.
# #'
# #' @author Reinhard Simon
# #' @export
# #' @return dataframe
# get_site_table <- function(){
#   fns <- fbglobal::fname_sites()
#
#   if(!file.exists(fns)) {
#     base_dir <-  dirname(fns)
#     if(!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
#     table_sites <- new_site_table()
#     save(table_sites, file = fns)
#   }
#   load(fns)
#   table_sites
# }
#
# #' Posts a site table locally.
# #'
# #' Posts a data.frame containing location data.
# #'
# #' @author Reinhard Simon
# #' @param table_sites data.frame
# #' @export
# post_site_table <- function(table_sites){
#   save(table_sites, file = fbglobal::fname_sites())
# }
#

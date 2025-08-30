df <- read.delim('cwx0827.txt', skip = 1)
run_reorder <- function(input){
  df <- input[,c('Pos','Cp')]
  df$group <- substr(df$Pos, 1, 1)
  df_list <- split(df, df$group)
  trans_df_mat <- function(df){
    df_tmp <- df
    df_tmp$Cp <- df_tmp$Pos
    df_2col <- rbind(df_tmp,df)
    df_2col$x <- rep(c(1,2,3),16)
    df_2col$y <- c(1,1,1,3,3,3,5,5,5,7,7,7,9,9,9,
                   11,11,11,13,13,13,15,15,15,
                   2,2,2,4,4,4,6,6,6,8,8,8,10,10,10,
                   12,12,12,14,14,14,16,16,16)
    
    mat <- matrix(rep('',48),nrow = 3,ncol = 16)
    for (x in 1:nrow(mat)) {
      for (y in 1:ncol(mat)) {
        mat[x,y] <- df_2col[df_2col$x%in%x&df_2col$y%in%y,]$Cp
      }
    }
    return(mat)
  }
  
  mat_list <- lapply(df_list, function(x)trans_df_mat(x))
  mat_list_reorder <- mat_list[c('A','C','E','G','I','K','M','O',
                                 'B','D','F','H','J','L','N','P')]
  mat_bind <- as.data.frame(do.call(rbind,mat_list_reorder))
  mat_bind[is.na(mat_bind)] <- ""
  return(mat_bind)
}

df2 <- run_reorder(input = df)
write.csv(mat_bind, file = output_file, quote = F, row.names = F, col.names = F)

packages <- c('reshape2')
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(packages, library, character.only = TRUE)

load_data <- function(){
  fb_df <- read.csv('data/dataset_Facebook.csv', header = T, sep = ';')
  fb_df <- fb_df[,which(names(fb_df) %in% c('Type', 
                                            'Post.Month',
                                            'comment',
                                            'like',
                                            'share'))]
  colnames(fb_df) <- c('type', 'month', 'comment', 'like', 'share')
  return (fb_df)
}

scatter_matrix_data <- function(){
  all <- read.csv('data/dataset_Facebook.csv', header = T, sep = ';')
  subset <- all[,which(names(all) %in% c('Post.Month',
                                         'Post.Weekday',
                                         'Post.Hour',
                                         'comment',
                                         'like',
                                         'share',
                                         'Total.Interactions'))]
  colnames(subset) <- c('Month', 'Weekday', 'Hour', 'NumComments', 'NumLikes', 'NumShares', 'TotalInteractions')
  return (subset)
}
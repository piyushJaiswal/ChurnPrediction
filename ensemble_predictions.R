filenames = c("submission_xgb_1.csv", "submission_lightgbm_1.csv")
wts = c(0.5, 0.5)


final_df = data.table()
for(f in filenames){
  
  df = fread(paste0("../SUBMISSION/", f))
  df = df[order(UCIC_ID),]
  if(nrow(final_df)==0){
    final_df = copy(df)
    final_df$Responders = final_df$Responders*wts[which(filenames==f)]
  }else{
    final_df$Responders = final_df$Responders + df$Responders*wts[which(filenames==f)]
  }
  
}


write.csv(final_df, file = "../SUBMISSION/Submission_ens_1.csv", row.names = F)

predict_model_all = function(fit, d) {
  pars <- parTable(fit)
  pars <- pars[pars$op == '~',c(4,13)]
  pars <- data.frame(pars[,2], row.names=pars[,1])
  
  lv <- lavPredict(fit, type = "lv", newdata = d)
  
  predictions <- (
                  d["log_n_tokens_title"] * pars["log_n_tokens_title",]
                + d["log_n_tokens_content"] * pars["log_n_tokens_content",]
                + lv[,"complexity"] * pars["log_n_tokens_content",]
                + d["log_num_imgs"] * pars["log_num_imgs",]
                + d["log_num_videos"] * pars["log_num_videos",]
                + lv[,"sources"] * pars["sources",]
                + lv[,"popularity"] * pars["popularity",]
                + lv[,"subjectivity"] * pars["subjectivity",]
                + lv[,"polarity"] * pars["polarity",]
                + lv[,"positivity"] * pars["positivity",]
                + lv[,"negativity"] * pars["negativity",]
                + d["LDA_00"] * pars["LDA_00",]
                + d["LDA_01"] * pars["LDA_01",]
                + d["LDA_02"] * pars["LDA_02",]
                + d["LDA_03"] * pars["LDA_03",]
                + d["LDA_04"] * pars["LDA_04",]
                + d["data_channel_is_lifestyle"] * pars["data_channel_is_lifestyle",]
                + d["data_channel_is_entertainment"] * pars["data_channel_is_entertainment",]
                + d["data_channel_is_bus"] * pars["data_channel_is_bus",]
                + d["data_channel_is_socmed"] * pars["data_channel_is_socmed",]
                + d["data_channel_is_tech"] * pars["data_channel_is_tech",]
                + d["data_channel_is_world"] * pars["data_channel_is_world",]
                );
  
  error <- sum((d["log_shares"] - predictions)^2);
  
  return(error);
}
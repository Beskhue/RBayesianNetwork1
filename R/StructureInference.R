library(glasso)
source('ReadData.R')
source('Preprocess.R')

d <- readData()
d <- remove_data_errors(d)
d <- remove_empty_articles(d)
d <- preprocess(d)

fit <- data.frame(d$fit$log_n_tokens_title)
fit$log_n_tokens_content <- d$fit$log_n_tokens_content
fit$log_self_reference_avg_sharess <- d$fit$log_self_reference_avg_sharess
fit$log_num_imgs <- d$fit$log_num_imgs
fit$log_num_videos <- d$fit$log_num_videos
fit$log_num_hrefs <- d$fit$log_num_hrefs
fit$log_num_self_hrefs <- d$fit$log_num_self_hrefs
fit$data_channel_is_lifestyle <- d$fit$data_channel_is_lifestyle
fit$data_channel_is_entertainment <- d$fit$data_channel_is_entertainment
fit$data_channel_is_bus <- d$fit$data_channel_is_bus
fit$data_channel_is_socmed <- d$fit$data_channel_is_socmed
fit$data_channel_is_tech <- d$fit$data_channel_is_tech
fit$data_channel_is_world <- d$fit$data_channel_is_world


covariance <- cov(fit)
graph <- glasso(covariance, 0.1)

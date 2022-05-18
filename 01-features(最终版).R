## install tidyverse if not already
if (!requireNamespace("tidyverse", quietly = TRUE)) {
	install.packages("tidyverse")
}

## install remotes if not already
if (!requireNamespace("remotes", quietly = TRUE)) {
	install.packages("remotes")
}

## install rvest if not already
if (!requireNamespace("rvest", quietly = TRUE)) {
	install.packages("rvest")
}

## install/upgrade rtweet
if (!requireNamespace("textfeatures", quietly = TRUE) ||
		packageVersion("textfeatures") < "0.1.4") {
	remotes::install_github("mkearney/textfeatures")
}

## install/upgrade textfeatures
if (!requireNamespace("rtweet", quietly = TRUE) ||
		packageVersion("rtweet") < "0.6.7.9000") {
	remotes::install_github("mkearney/rtweet")
}

## load tidyverse set of packages
## note: instead of loading rtweet, textfeatures, & rvest (and xml2) packages,
##       the code below uses fully qualified namespaces (double colon).
library(tidyverse)

##因为特别怕误导班上的同学，今天回答完问题后一直担心
##晚上回宿舍后，我决定还是把作者的程序研究下，这样更安心
##这里先挂个VPN，然后在电脑设置里开启代理服务器，把下方url和port改成自己的ip和端口
##常用的端口是8080和10890，大家VPN不同所以最后显示的ip和端口不同，需要自行修改
library(httr)
set_config(
  use_proxy(url="127.0.0.1", port=10809))

## download Twitter profiles via CSPAN's cabinet list
##这里会弹出twitter授权，需要用自己的twitter登录，估计是注入cookies
##作者代码的使用时间是2018年，四年后可能the-cabinet人员会发生变动
cab_twits <- rtweet::lists_members(
	owner_user = "CSPAN", slug = "the-cabinet")

## get up to 3200 of most recent tweets for each
## 这里我选了3200篇，如果大家想要更快看到结果，可以把3200改成320
cab_tweets <- cab_twits %>%
	filter(screen_name != "realDonaldTrump") %>%
	pull(user_id) %>%
	map(rtweet::get_timeline, n = 3200) %>%
  bind_rows()

## scrape source code for op-ed
a<-httr::GET("https://www.nytimes.com/2018/09/05/opinion/trump-white-house-anonymous-resistance.html")
nyt <- xml2::read_html(a)

## return just the paragraph text
nyt_text <- nyt %>%
	rvest::html_nodes("p") %>%
	rvest::html_text() %>%
	.[3:31]

## create data set with just author (id) and text
data <- data_frame(
	id = c(cab_tweets$screen_name, rep("op-ed", length(nyt_text))),
	text = c(cab_tweets$text, nyt_text)
)

## feature extraction
tf <- textfeatures::textfeatures(data, word_dims = 80)
name<- data$id
name<- data_frame(name)
features<-tf
new_tf<-cbind(name,features)

## summarise by id
tfsum <- new_tf %>%
	group_by(name) %>%
	summarise_all(mean, na.rm = TRUE) %>%
	ungroup()

## vector of unique authors
authors <- unique(tfsum$name)

## create numeric vectors of equal length for each author
cols <- map(authors,
	~ filter(tfsum, name == .x) %>% select(-name) %>% as.list() %>% unlist()
)

##最终的数据集是mat这个变量，列是114个指标，行是35个账号(作者)
##大家如果感兴趣，可以直接导入rdata文件，右键点击rdata文件然后选择用rstudio打开即可
##这样前面所有变量就可以直接导入了，前面获取twitter的时间很长，文本特征提取的时间也很长
##所以我直接把变量打包存成了rdata，这样大家就可以从94行开始看起，也可直接运行115和1116行出结果
##这样就可以给大家省点时间，还能少安装4、5个r语言包节省电脑空间(右侧工作区的mat变量是最终变量)
## create matrix
mat <- cols %>%
	unlist() %>%
	as.numeric() %>%
	matrix(nrow = length(authors), byrow = TRUE)

## set row and column names
row.names(mat) <- authors

##t()表示把变量mat进行转置，这样列变成了35个账号(作者)，行变成了114个指标
##cor()函数把各列向量进行比较，所以原作者应该是把114个指标作为了每个账号的连续变量
##这样计算相关系数确实存在疑问，老师课上的质疑很正确，雨欣同学课后的提问也很有道理
##我觉得大家的疑点主要集中于下方四列，而且大家的直觉都非常准确：
##相关系数的计算应该是先确定两组变量(或称为指标)，然后进行个案的随机抽样，每个个案的取值构成两列连续变量
##然后两列连续变量之间进行相关系数的计算，但这个研究中作者是把个案和变量(此处更应称为指标)颠倒
##将100多个文本特征(指标)代替了原来本应随机抽样的个案，把35个作者(个案)间进行两两配对计算相关系数
##这想当于把整个矩阵反过来，倒过来用指标估计个案间的相似度，但是100多个文本特征并不符合随机抽样前提
## dipslay matrix
library(tidyverse)
cor(t(mat))[, "op-ed"] %>% sort()

##最后这些是用来保存数据集和相关系数结果的
a<-cor(t(mat))[, "op-ed"] %>% sort()
transform_mat<-data.frame(t(mat))
write.table (transform_mat, file ="results.csv",sep =",", row.names = FALSE,col.names = TRUE)
write.table (a, file ="correlation.csv",sep =",", row.names = TRUE,col.names = FALSE)

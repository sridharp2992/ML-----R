install.packages('Rfacebook',dependencies = T)
library('Rfacebook')
library('Rfacebook')
#create a facebook app and add http://localhost:1410/ to your app
fb_oauth <- fbOAuth(app_id="773900689425012",
                    app_secret="679ce6bfa52a5866abc675ba25f3b6c4")

fb_page <- getPage(page="subway", token=fb_oauth,n=50,feed = T,reactions = T)

plot(fb_page$shares_count,type = 'b',ylab = "#Share",xlab = 'FB post',col='red')
plot(fb_page$likes_count[1:50],type = 'b',ylab = "#Comments",xlab = 'FB post',col='green')
plot(fb_page$comments_count[1:50],type = 'b',ylab = "#Comments",xlab = 'FB post',col='blue')

barplot(c(sum(fb_page$shares_count),sum(fb_page$likes_count),sum(fb_page$comments_count)),
        names.arg = c('#Shares','#Likes','#Comments'),
        col=c('red','green','blue'))

#Linear Model
summary(lm(fb_page$shares_count~
             fb_page$likes_count+
             fb_page$comments_count
))


analysis=as.data.frame(cbind(likes=fb_page$likes_count,
                             message=fb_page$message,
                             create_time=fb_page$created_time,
                             today='2017-02-15',
                             type=fb_page$type,
                             link=fb_page$link,
                             comments=fb_page$comments_count,
                             shares=fb_page$shares_count,
                             love=fb_page$love_count,
                             haha=fb_page$haha_count,
                             wow=fb_page$wow_count,
                             sad=fb_page$sad_count,
                             angry=fb_page$angry_count
))


b <- sum(fb_page$likes_count)
c <- sum(fb_page$shares_count)
d <- sum(fb_page$comments_count)

unique(fb_page$from_name)
nrow(fb_page$id)


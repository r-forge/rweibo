

require(Rweibo)

# register application
registerApp(app_name = "mytest", "GDdmIQH6jh", "MCD8BKwGdgPHv") 

# create OAuth object
roauth <- createOAuth("mytest", "rweibo") 

# return the latest public weibos
res1 <- statuses.public_timeline(roauth, count = 5)
res1

# return the latest weibos of the authenticating user and his friends
res2 <- statuses.friends_timeline(roauth, count = 5)
res2

# post a new weibo
res3 <- statuses.update(roauth, status = "你好啊*!@#$&=+")

# repost a weibo
res4 <- statuses.repost(roauth, id = res3$idstr, status = "转一个啊")

# post a comment to a weibo
res5 <- comments.create(roauth, id = res4$idstr, comment = "评论一下啊")


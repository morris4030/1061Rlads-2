README第二組
---------
```{r}
library(tidyverse)
library(tidytext)
library(dplyr)
```


<h3>期中加分作業</h3>

對於這次的作業，我們想做的是一個對經濟學人雜誌(TheEconomist)粉絲專頁進行一些初步的分析。

https://www.facebook.com/TheEconomist

截至2017/11/15 上午1:50分，經濟學人粉絲專頁一共有8,368,367 人按讚，8,178,742 人追蹤。
文章數目也是相當的多，因此我們從公開(public)的文章中取出作為分析的對象。

<h3>透過Rfacebook得到資料</h3>

```{r, warning=FALSE}
#install.packages("Rfacebook")
library(Rfacebook)

#options("browser"= NULL)  # for some Windows users if error occurs
fb_oauth <- fbOAuth(
  app_id = "952921794845515",     #                     # 填入應用程式編號
  app_secret = "xxxxxxxxxxxxxxxxxxxxx",   
  extended_permissions = TRUE)     # 填入應用程式密鑰     

save(fb_oauth, file="fb_oauth")
load("fb_oauth")
```
首先，我們使用'getPage'的功能從粉專當中找出最近的500筆資料(之所以是500這個數字，是因為獲得更高的資料數需要更多的時間，而我們快趕不上這次作業的死線了)，然後將得到的Facebook時間訊息處理成R語言能夠使用的模式。

```{r}
#從經濟學人TheEconomist找資料

page <- getPage("TheEconomist", token=fb_oauth, n = 500, reactions = TRUE)
page[which.max(page$likes_count), ]

## 將Facebook的時間轉成R可以使用的模式

format.facebook.date <- function(datestring) {
    date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}

page$datetime <- format.facebook.date(page$created_time)
```

簡單的做個圖，看看有甚麼發現

```{r}
plot(page$comments_count, page$likes_count, xlab = "回應數量", ylab = "按讚數", main = "回應數與按讚數之間的關係")

plot(page$datetime, page$likes_count, xlab = "時間軸", ylab = "按讚數", main = "日期與按讚數的關係")
```
<center><img src="images/commentlike.png" style="width: 750px;"/></center>
<center><img src="images/datelike.png" style="width: 750px;"/></center>

稍微作圖會發現，回應數量跟按讚數似乎有一定的關係。

然後由於這裡目前只有500筆資料，因此時間軸有點短。如果能使用更多資料，找出日期時間與按讚人數的變化(例如:六日晚上可能較多人按讚?)，應該可以成為之後更有趣的研究。


<h3>針對新增的表情符號功能，與按讚數量的分析</h3>

另外，從上面的資料讓我們聯想到，Facebook最近新增的表情符號跟人們的反應行為有沒有關係呢?

```{r}
new.page <- drop_na(page)
print(sprintf("按讚次數跟回應次數的相關係數: %.4f", cor(new.page$likes_count, new.page$comments_count)))
print(sprintf("按讚次數跟分享次數的相關係數: %.4f", cor(new.page$likes_count, new.page$shares_count)))


print(sprintf("按讚次數跟愛心的相關係數: %.4f", cor(new.page$likes_count, new.page$love_count)))
print(sprintf("按讚次數跟大笑的相關係數: %.4f", cor(new.page$likes_count, new.page$haha_count)))
print(sprintf("按讚次數跟驚訝的相關係數: %.4f", cor(new.page$likes_count, new.page$wow_count)))
print(sprintf("按讚次數跟難過的相關係數: %.4f", cor(new.page$likes_count, new.page$sad_count)))
print(sprintf("按讚次數跟生氣的相關係數: %.4f", cor(new.page$likes_count, new.page$angry_count)))


print(sprintf("留言次數跟愛心的相關係數: %.4f", cor(new.page$comments_count, new.page$love_count)))
print(sprintf("留言次數跟大笑的相關係數: %.4f", cor(new.page$comments_count, new.page$haha_count)))
print(sprintf("留言次數跟驚訝的相關係數: %.4f", cor(new.page$comments_count, new.page$wow_count)))
print(sprintf("留言次數跟難過的相關係數: %.4f", cor(new.page$comments_count, new.page$sad_count)))
print(sprintf("留言次數跟生氣的相關係數: %.4f", cor(new.page$comments_count, new.page$angry_count)))
```
<center><img src="images/cor.png" style="width: 750px;"/></center>

再來，我們把強烈正相關的部分抓出來看看

<center><img src="images/redcor.png" style="width: 750px;"/></center>
發現竟然只有愛心還有驚訝會跟留言以及按讚數有正向的關係!! 另一方面，大笑、難過、生氣跟按讚留言之間的正向關係並不強。

想想也是，大概是因為只有愛心跟驚訝的新聞會讓人比較想感同身受或留言吧。難過跟生氣不想按讚完全可以理解，大笑看過笑完似乎也不會有想留言的衝動。

以上只是目前初步的分析得到的想法，仍然需要更進一步的資料以及統計方法才能得出更好的結論。


<h3>計算不同發佈方式的平均按讚數(如:連結網址、影片、照片)，以及其標準差</h3>

```{r}
page1 <- page %>%
  group_by(type) %>%
  summarise(avg = mean(likes_count), std = sd(likes_count)) %>%
  arrange(avg)
page2 <-count(group_by(page, type), type)
left_join(page1, page2)

```
<center><img src="images/avgstd.png" style="width: 750px;"/></center>


從以上的統計表格當中，我們可以看到影片的平均點讚數明顯的比單純的網址連結高出約3倍!!!

照片的按讚數也是相當的高，但是由於樣本不夠多，顯著性不強，因此先暫時不討論。

可能就是因為影片、照片這類非文字的資訊能夠觸及的人群較多，這或許也可以解釋為什麼近年來Facebook努力推動影片分享以及直播的功能。


<h3>使用tidytext分析標題內容，找出哪個標題關鍵字出現最多次</h3>

```{r}
page.message <- select(page, message)
page.message.words <- unnest_tokens(page.message, word, message)

data("stop_words")
cleaned.message.words <- page.message.words %>%
  anti_join(stop_words)

freq.word <- count(cleaned.message.words, word, sort = TRUE)
freq.word.final <- filter(freq.word, n > 10)

freq.word.final
```
<center><img src="images/wordn.png" style="width: 750px;"/></center>

由以上表格可以看出第一名的people以39次奪冠，去除掉2,3,4名跟雜誌本身名字有關的文字(註：1843為The Economist旗下的雜誌名)，得名的還有world, time, 美國總統Donald Trump等關鍵字。

另外也可以看出經濟學人雜誌最近較常關注的議題，像是歷史、人生、美國、政府等關鍵字也出現於其中。


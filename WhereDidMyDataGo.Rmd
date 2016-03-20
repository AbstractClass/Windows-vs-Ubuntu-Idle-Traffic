---
title: "Where is My Data Going?"
author: "Connor MacLeod"
date: "March 19, 2016"
output: html_document
---

# How bad is it really?

I decided to finally take on a problem for this assignment that's been nagging at me for a long time.  I always assumed that Windows used a lot more traffic at idle than my Ubuntu did, but I wanted to know exactly how much, and if I could reduce it somehow.  I have already turned off every telemetry setting I could find, but I keep hearing rumors about Windows sending data anyways.  So I came up with a plan.

On each OS do the following:

- Close all windows and non-essential programs in the tray (for Windows), like Skype, Steam, Dropbox, etc.

- Run Wireshark for 300s

- Filter down to only traffic from involving the host and do general data cleaning

- Import into R and start digging

***

## Step 1: Data Gathering

After running Wireshark on each OS, I have two csv files, 'TakeHomeWindows.csv' and 'TakeHomeUbuntu.csv'.  The next step is importing them into R.

```{r importData}
  windows <- read.csv("C:/Users/Connor/Documents/SRT411/TakeHomeWindows.csv")
  ubuntu <- read.csv("C:/Users/Connor/Documents/SRT411/TakeHomeUbuntu.csv")

  typeof(windows)
  typeof(ubuntu)
```

As you can see, the variables are in list form.  In order to be able to best work with them, it is ideal to put them in dplyr's tbl_df format.

```{r importDplyr, message=F}
  library(dplyr)
```

```{r tbldf}
  windf = tbl_df(data.frame(windows))
  ubndf = tbl_df(data.frame(ubuntu))
```

Ok, now we can begin cleaning this data to make it useful for us.

***

## Step 2: Cleaning Data

Let's take a look at the data in it's current state.

```{r sampledata}
  # Sample first 10 lines
  head(windf)
  head(ubndf)
  
  # Table stats
  str(windf)
  str(ubndf)
```

So three things are immediately clear to me from this data.  The first is that we have a superflous column.  The 'No.' column is useless to us because R already provides a numbered column.  The second is that there seems to be a lot of data picked up by Wireshark that doesn't even involve our host IP (192.168.1.109). Finally, and the total run times are slightly different.  So let's fix all of those.

```{r cleanData}
  # Remove Column -> Only packets going to or coming from our host -> Ensure total run time is < 300s
  cleanpcap = function(tbldf){
    tbldf = tbldf %>%
      select(-No.) %>%
      filter(
        (Source == "192.168.1.109" | Destination == "192.168.1.109")
        & Time <= 300)
  }
  
  windat <- cleanpcap(windf)
  ubndat <- cleanpcap(ubndf)
  
  head(windat)
  head(ubndat)
  
  str(windat)
  str(ubndat)
```

> Just as I expected.  Windows is using more than __Three Times__ as much data as Ubunut (511 packets vs 165).  Let's see if we can track down what's causing it.

***

## Step 3: Digging

We need to start out by setting a baseline with our Ubuntu, so we can tell what is different about Windows.  I plan to look at length, length over time, protocol, length vs protocol, and inbound vs. outbound.

```{r importGGplot, message=F}
  library(ggplot2)
  library(grid)
  library(gridExtra)
```

```{r basePlotUbuntu, warning=F, fig.width=10}
  ulength <- ggplot(ubndat, aes(Length, Protocol)) +
    geom_point(aes(color = Length), size = 4, alpha = 1/3) +
    scale_color_gradient(low = "blue", high = "red") +
    ggtitle("Packet Length by Protocol")
  
  uproto <- ggplot(ubndat, aes(Protocol)) +
    geom_bar(fill = "steelblue") +
    ggtitle("Protocol Usage") +
    theme_light()
  
  uprolen <- ggplot(ubndat, aes(factor(Protocol), Length)) +
    geom_bar(stat = "identity", position = "stack", fill = "steelblue") +
    xlab("Protocol") +
    ylab("Total Length") +
    ggtitle("Total Data Sent By Protocol") +
    theme_light()
  
  utimeline <- qplot(Time, Length, data = ubndat, geom = "area") +
    geom_area(fill = "steelblue") +
    theme_light() +
    ylim(0, 400) +
    ggtitle("Packet Length Over Time")
    
    
  ulength
  uproto
  uprolen
  utimeline
```

So here we can see that while most of the traffic is TCP (the dense, solid color), the largest packets were from HTTP/XML.  We also see that confirmed in the count graph.  TCP has well over ten times the packets of any other protocol.  However, when we look at total data sent per protocol, we see that HTTP/XML is actually responsible for more than it's fair share. Looking at the timeline, we see the large packets were all sent in the beginning.  Let's find out what protocol did that (I think it's HTTP).

```{r s1proto, fig.width=10}
  ulength2 <- ggplot(filter(ubndat, Time < 15), aes(Length, Protocol)) +
    geom_point(aes(color = Length), size = 4, alpha = 1/3) +
    scale_color_gradient(low = "blue", high = "red") +
    ggtitle("Packet Length by Protocol (Time < 15s)")
  
  grid.arrange(ulength, ulength2, ncol = 1)
```

So as we can see, most of it looks the same.  So the conclusion here is that the rest of the data after the intial burst was all of the other protocols we see.

My guess as to the source of this traffic spike is that I ran wireshark immediately after booting into Ubunut to minimise background processes.  This means that if anything runs on startup, wireshark would have caught the tail end of that, so in reality, the idle traffic is actually everything post 15s.

```{r trueIdleUbunut, fig.width=10}
  ggplot(filter(ubndat, Time > 15), aes(Length, Protocol)) +
    geom_point(aes(color = Length), size = 4, alpha = 1/3) +
    scale_color_gradient(low = "blue", high = "red") +
    ggtitle("Packet Length by Protocol (Time < 15s)")
```

> Wow.  That's impressive, very little traffic at all.  Let's see how Windows stands up.

***

```{r basePlotWindwos, warning=F, fig.width=10}
  wlength <- ggplot(windat, aes(Length, Protocol)) +
    geom_point(aes(color = Length), size = 4, alpha = 1/3) +
    scale_color_gradient(low = "blue", high = "red") +
    ggtitle("Packet Length by Protocol")
  
  wproto <- ggplot(windat, aes(Protocol)) +
    geom_bar(fill = "steelblue") +
    ggtitle("Protocol Usage") +
    theme_light()
  
  wprolen <- ggplot(windat, aes(factor(Protocol), Length)) +
    geom_bar(stat = "identity", position = "stack", fill = "steelblue") +
    xlab("Protocol") +
    ylab("Total Length") +
    ggtitle("Total Data Sent By Protocol") +
    theme_light()
  
  wtimeline <- qplot(Time, Length, data = windat, geom = "area") +
    geom_area(fill = "steelblue") +
    theme_light() +
    ylim(0, 400) +
    ggtitle("Packet Length Over Time")
    
    
  wlength
  wproto
  wprolen
  wtimeline
```

Interesting.  On the upside, at least most of the traffic being sent is encrypted through TLS.  On the downside, there are __a lot__ of TCP and TLS packets measuering in the ~1500 area, then they are scattered until the ~300 area.  Looking at the protocol usage we can see that although both TLS and TCP had high packet length, that TCP is used twice as much and sent twice as much data as TLS.  We can't even give saving grace in the timeline, it is intermittent thoughout.  Let's see if we can find out what is causing this.

***

## Step 4: The Cause

Let's start by just isolating TCP and TLS, since they are oviously the problem here.

```{r columnSelect}
  # Get Sources for the biggest TCP and TLS packets
  wins <- windat %>% 
    filter(Protocol == "TCP" | Protocol == "TLSv1.2") %>%
    arrange(desc(Length), Source)
```

I am going to venture a guess and assume that it's only a handful of IP's causing this.  Let's isolate them and see.

```{r isolateIP}
  # Get list of source IP's using big packets
  wins <- wins %>%
    filter(Length > 1400, Source != "192.168.1.109")
  
  unique(wins$Source)
```

> So those are the big bad IP's that are ruining my network.  Here's what I could get on them.

IP              | Details                     | Domain
-----------     | --------------------------- | ---------------------------
111.221.29.254  | Microsoft Corp, Singapore   | vortex-win.data.microsoft.com
13.107.5.88     | Microsoft Azure, US         | e-0009.e-msedge.net
131.253.61.66   | Microsoft Corp, US          | N/A
134.170.108.200 | Microsoft Corp, US          | by3302-e.1drc.com
137.116.74.190  | Microsoft Corp, US          | N/A
23.99.116.116   | Microsoft Azure, Hong Kong  | N/A
40.121.144.182  | Microsoft Acure, US         | N/A

Ugh.  Well that sucks.  All of the biggest packet hogs are Microsoft demanding my data.  If there was ever a reason to switch to a Linux or FreeBSD dsitro, this is it.

"But what if something is genreating a ton of smaller packets instead of a few large packets?" Good question, let's see.

```{r lengthByIP, fig.width=10}
  wins <- windat %>%
    filter(Source != "192.168.1.109") %>%
    mutate(Labels = substr(Source, 8, 15))
  
  ggplot(wins, aes(factor(Labels), Length)) +
    geom_bar(stat = "identity", position = "stack", fill = "steelblue") +
    xlab("Source IP") +
    ylab("Total Length") +
    ggtitle("Total Data Sent By Source") +
    theme_light()
    
```

> And still, our culprits are (in order): 131.253.61.66, 40.121.144.182, 134.170.108.200, 137.116.74.190 

> AKA Micosoft

***

## Conclusion

In the age of fast and plentiful data and internet, many argue that this may not be a 'big deal', but I personally disagree.  The fact that I can't turn this off is itself unsettling, but also the fact that they are using up my network traffic, just because I use their OS, is completely ridiculous to me.  You also have to consider the privacy implications here.  Do you know how much information can be transmitted per one packet of length ~1500?  And now consider that `r sum(wins[["Length"]])`  bytes (~136 Kilobytes) were transmitted in the 5 minutes I was recording.  That is approximately 17075 words in ASCII.  A 10 page essay about your computer habits is being sent to Micrososft __Every 5 Minutes__.  Looks like I'll be using Ubuntu a lot more often.




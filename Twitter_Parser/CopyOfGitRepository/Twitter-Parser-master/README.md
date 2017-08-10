Twitter-Parser
==============

What's the fastest way to get started?
--------------

Read the [User Guide](https://github.com/ToddBodnar/Twitter-Parser/releases/download/0.9/Twitter.Parser.User.Guide.pdf) supplied with the latest release.

What is this?
--------------

This is an opensource port of my main Tweet analysis tool that I've been using during my PhD. This program is designed to process large amounts of data from the Twitter API. It is middle-ware in that it is bridges that gap between raw Twitter data and statistical tools such as Weka or R. This app supports MySQL, PostgreSQL, Weka, R and generic csv output.


Why a port?
------------

The original version was designed for sole use on my lab equipment, so it includes a lot of access keys and assumptions about directories layout. Also the code was fairly messy and had a lot of unnecessary side projects included. (I just finished my BS in Computer Science when I made it, but this is way larger than anything I've built before.) This version should be fully portable and the code might be a bit cleaner ;)

Is it useful?
------------

I think so! It's been used as part of [data visualization](http://www.ploscompbiol.org/article/info%3Adoi%2F10.1371%2Fjournal.pcbi.1002616), [data science](http://arxiv.org/pdf/1404.3026.pdf) and as part of the backend for [CrowdBreaks](http://www.crowdbreaks.com/).


Is the JavaDoc available?
------------
[Yep!](http://toddbodnar.github.io/Twitter-Parser/)

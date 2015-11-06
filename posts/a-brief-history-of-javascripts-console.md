---
title: A Brief History of JavaScript's console
date: September 30, 2015
---

# A Brief History of JavaScript's console

You probably use it every day, yet there's no standard implementation. It was
introduced 10 years ago, but despite its age and widespread adoption
`console` is not part of ECMAScript.

Where did it come from? What did people do before `console.log`?

Older versions of Firefox, Opera, and Internet Explorer circa 2000 had introduced a
JavaScript "Console" that kept a log of errors and sometimes included a debugger for setting
breakpoints. You could evaluate JS in these consoles but had no way of writing
your own messages to the log.

![Mozilla JavaScript Console - &copy; alistapart.com](/images/console.png)

Writing to that console was the inspiration for `console.log`. Without it, developers used a combination of `window.alert`s and tools that wrote log messages to hidden DOM elements or attributes. One such tool
was fvlogger [released in 2005](http://alistapart.com/article/jslogging) by
David F. Miller of A List Apart. Other tools with an interface similar to
Apache log4j appeared in 2005, including
[log4js](http://log4javascript.org) by Tim Down and [JSLogger](http://www.mojavelinux.com/wiki/lib/exe/fetch.php?id=scriptsandbox&cache=cache&media=logger.js.txt)
by Dan Allen.

`console` appears to have been first introduced in its current form in 2006 by [Firebug](https://web.archive.org/web/20061228053918/http://www.getfirebug.com/logging.html),
a web development plugin for Firefox. Firebug provided an enhanced web console
capable of debugging, profiling, and DOM inspection. Similar plugins for other
browsers appeared shortly after. These third-party development consoles put pressure on browsers to incorporate
their features. Eventually, a console object was introduced in Internet
Explorer 8, Opera 9.5, and Safari 3 (not confirmed).

Today still there is no standardized implementation of console object and its methods. Even
`console.log` behaves slightly different across browsers. It's grown beyond the Firebug implementation, with methods for tables, timers, and stack traces. The Developer Tools
Working Group has begun a draft [console
standard](https://github.com/DeveloperToolsWG/console-object) to improve
parity between implementations. With widespread implementation across browsers and node.js, it looks like console is here to stay.

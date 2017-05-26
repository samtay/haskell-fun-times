# Final Project
Finger Daemons!

### 31.2 fingerd
The *finger* service is a means of figuring out how to contact people
on the same computer network and has been around since the pre-internet era.
It looks like this:
```shell
❯ finger samtay
Login: samtay         			Name: (null)
Directory: /home/samtay             	Shell: /usr/bin/zsh
On since Mon May 22 23:37 (EDT) on tty1   2 days 20 hours idle
     (messages off)
No mail.
No Plan.
```

The finger protocol operates over TCP (Transmission Control Protocol) just like a web browser.
However, the web has HTTP, an entire application protocol layered atop TCP, whereas
the finger protocol is just a single message text protocol.

#### [Debug](./src/Debug.hs)
The debug program is used to show us what the client sends. It is a TCP server,
similar to a web server that provides a web page, but lower level and limited to
sending raw text back and forth. It's set to run at port 79.

We can run it with sudo access like so:
```shell
$ sudo $(stack exec which debug)
```
and in another terminal, open a connection to port 79:
```shell
$ telnet localhost 79
Trying ::1...
Connection failed: Connection refused
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
```
and then whatever text we send gets sent back to us. Since we `cClose soc` in the
`logAndEcho` functino, the telnet connection is closed and needs to be reopened,
but the debug server stays alive.

In addition to telnet we can use a finger client:
```shell
-- finger client
❯ finger samtay@localhost
[localhost.localdomain]
samtay

-- debug server response
"samtay\r\n"
```

To recap, the whole purpose of the debug server is so that we know exactly
what information gets sent to the TCP server that we're about to build!

### 31.4 Slightly modernized fingerd
Historically `finger` returned data about users pertaining to the operating system.
We're going to update the source of data for `finger` using an embedded SQLite database.

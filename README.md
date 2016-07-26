# websockets
(WIP) An implementation of RFC 6455 (WebSockets) in Haskell

## _Nota bene_

This is a work-in-progress, and thus far I've gone as far as prototyping the plumbing
for thread management (servers and connections). There's loads to do still, in particular
test coverage as code is promoted from prototype/PoC to release candidate.

## Building from sources

You'll need [stack](https://docs.haskellstack.org/en/stable/README/) or [cabal](https://www.haskell.org/cabal/)
to build from sources. You may use the following instructions to build from sources using `stack`:

First clone this project from GitHub:

```language=bash
$ mkdir -p $PATH_TO_PROJECT
$ cd $PATH_TO_PROJECT
$ git clone https://github.com/fabiool/websockets.git
```

In the instructions above, `$PATH_TO_PROJECT` should be set to the location where
you'll want the websockets project to reside, for example:

```language=bash
export PATH_TO_PROJECT="$HOME/Projects/haskell`
```

### Build

To build, simply change to the `$PATH_TO_PROJECT/websockets` directory and issue:

```language=bash
$ cd $PATH_TO_PROJECT/websockets
$ stack build
```

### Execute

To execute use `stack`'s `exec` command. The argumens following a `--` (double dashes) will be
interpreted as program arguments as follows:

```language=bash
$ cd $PATH_TO_PROJECT/websockets
$ stack exec websockets-exec -- -h
Usage: websockets-exe [-h|-?|--help] [-v|--verbose] [-c|--client] -n[HOST]|--hostname=HOST -p[PORT]|--hostname=PORT
                      [-t[DIR]|--trace-dir=DIR] [-x[FILE]|--text=FILE] [-b[FILE]|--binary=FILE] [-i[NUM]|--instances=NUM]
                      [-m[NUM]|--max-connections=NUM] [-d[NUM]|--counter-delay=NUM] [-w[NUM]|--wait-time=NUM]
                      [-f[NUM]|--delay-factor=NUM]

  -h, -?   --help                 Show help
  -v       --verbose              Debug output on stderr
  -c       --client               Starts in client mode
  -n HOST  --host=HOST            HOST name to connect or bind to
  -p PORT  --port=PORT            PORT number to connect of bind to
  -t DIR   --trace-dir=DIR        Directory where to save received packets
  -x FILE  --text=FILE            Text content to be sent to the other party
  -b FILE  --binary=FILE          Binary content to be sent to the other party
  -i NUM   --instances=NUM        Number of concurrent instances to launch
  -m NUM   --max-connections=NUM  Maximun number of connections per server instance
  -d NUM   --counter-delay=NUM    Polls the thread group every 'countDelay' microsecs
  -w NUM   --wait-time=NUM        Wait time until trying againg for next available connection slot
  -f NUM   --delay-factor=NUM     Delay factor
```

## Known issues & TODO's

Amongst several others the following are noteworthy issues to be aware of:

1. The command prompt thread must be moved up from the server realm to the main thread realm.
   Currently you can still launch multiple-instances and stop them using the command prompt, but there'll be as many
   threads waiting for command as running instances.

   It also semantically wrong for it to remain attached to a running server. There should be only one "master"/admin
   thread to manage all running instances.

2. The Config code is quite long and repetitive. There ought to be better ways to achieve the same thing.

3. There are quite a few places where could use more polished Haskell and higher order abstractions.
   I'm getting there; slowly but steadily.

# BotScript

A script language for chatbot, I learn PureScript by making the project.

I start it from PureScript guides: [Getting-Started.md](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md).

Event:
- join
- left
- play
- over
- milk
- talk
- priv

Inst:
- event type   action
- match [...]
- title string
- descr string
- delay int
- sleep int
- timer time   action
- print string
- order string [src]
- goto  state

Reserve:
- mate
- host
- self
- room

Operator:
- +
- -
- *
- /
- =
- .

```

u = users[2]
state ex {
  name = value
  name = name + value

  title "hello world"
  descr "welcome to the new world"

  print "this is state 1"
  order "yellow"
  delay 3
  sleep 3

  timer 60m (){
    print "pass 60 minutes"
    for user : mate {
      print "$user alive"
    }
  }

  match {
    event join(){}
    event left(){}
  }

  event talk (user : "", msg : "^/play"){
    order ("$1" # msg)
  }

  goto state
}
```

```
list = []
state welcome {
  event join (user){
    if user in list then {
      print "welcome back $user"
    }
    else list.append(user)
  }
}

run welcome
```

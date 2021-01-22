module Test.Main where

import BotScript
import BotScriptEnv
import BotScriptParser
import BotScriptVM
import Data.Either
import Prelude

import Data.Array.ST.Iterator (next)
import Data.List (List(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Undefined (undefined)

testLoop = """
for i = 0
    i < 10
    i++
    print(i)

j = 0
while(j < 3){
    print(j);
    j++;
}
for(i of [1,2,3,4]) print(i);
for(j in {tom: 1, allen: 2}) print(j);
"""

testAjax = """
fetch("https://v1.hitokoto.cn")
  .then(response => response.json())
	.then(result => {
	print(result.hitokoto);
});
"""



testRecursion = """
f = (x) =>
  if(x <= 0) then 0
  else if(x == 1) then 1
  else f(x - 1) + f(x - 2)

[0, 1, 2, 3, 4, 5, 6].map(f)
"""

testLift = """
f = (a, b) => a + b;
print(f(1, 4)) // 5

g = { args[0] + args[1] };
print(g(1, 2)) // 3
"""

testGoing = """
state welcome {
    print("hello world");
    going bye
}

state bye {
    print("bye");
    // done.
}

going welcome
"""

testVisit = """
state welcome {
    print("hello world");
    going bye
}

state bye {
    print("bye");
    // because "visit welcome", so back to visit
}

visit welcome
// back from bye
print("done");
// done.
"""

guessNumber = """
valid = (digits) =>
    (new Set(digits.split(""))).size === 4

generate = () => {
    while(!valid(digits = String(Math.floor(1000 + Math.random() * 9000))));
    digits
    digits
}

gnjdg = (guess, callback) => {
  if (valid(guess)) then {
    d = theNumber.split("")
    g = guess.split("")
    c = g.map((v)=>d.includes(v)).reduce((a, b)=>a+b)
    a = g.map((v, idx)=>d[idx] === g[idx]).reduce((a, b)=>a+b)
    b  = c - a
    callback(
      if(a === 4) then
        "Your Number is Correct"
      else
        guess + ":" + String(a) + "A" + String(b) + "B"
    )
  } else callback("guess number must be 4 non-repeat digits" + guess);
}

event msg (user, cont: "^\\d\\d\\d\\d$") => gnjdg(cont, drrr.print)
event msg (user, cont: "^new$") => theNumber = generate()
print(theNumber = generate())
"""

wolf = """
names = []
players = {}
victim = []
vote = {}

announcement = "/me尚未開始遊戲"
announce = (msg) => {
  announcement = msg
  drrr.print(msg)
}

// 0 狼 1 平民 2 預言家 3 女巫 4 獵人
roleName = ["狼", "平民", "預言家", "女巫", "獵人"]
rolesMap = {
  5: [0, 1, 1, 2, 3],
  6: [0, 0, 1, 1, 2, 3],
  7: [0, 0, 1, 1, 1, 2, 3],
  8: [0, 0, 1, 1, 1, 2, 3, 4],
}


getRandom = (min,max) => {
  Math.floor(Math.random()*(max-min+1))+min
}

select = (cont, users) => {
  result = users.filter((u) => cont.includes(u))
  if result.length
  then result.reduce((a, b)=> if a.length > b.length then a else b)
  else ""
}

map = (obj, func) => Object.values(obj).map(func)
forEach = (obj, func) => Object.values(obj).forEach(func)
filter = (obj, func) => Object.values(obj).filter(func)

most = (arr) => {
  counts = arr.reduce((a, c) => {
    a[c] = (if a.hasOwnProperty(c) then a[c] else 0) + 1
    a
  }, Object())
  print(counts)
  maxCount = Math.max.apply(Object(), Object.values(counts))
  print(maxCount)
  Object.keys(counts).filter(k => counts[k] === maxCount)
}

passJudge = () => {
  wolf = filter(players, (p, index) => p.life && p.role == 0)
  people = filter(players, (p, index) => p.life && p.role != 0)
  if wolf.length && people.length then -1
  else if wolf.length then 0
  else 1
}

state prepare {
  victim = []
  names = []
  players = {}
  vote = {}
  event [msg, me] (user, cont: "^\\+1$") => {
    if names.includes(user) then
      drrr.print("/me" + user + " 已經加入了")
    else{
      names.push(user)
      drrr.print("/me" + user + " 加入遊戲")
    }
  }
  event [msg, me] (user, cont: "^-1$") => {
    if names.includes(user) then {
      names.splice(names.indexOf(user), 1);
      drrr.print("/me" + user + " 退出遊戲")
    }
    else drrr.print("/me" + user + " 不在遊戲中")
  }
  event [msg, me] (user, cont: "^/who$") => {
    if names.length then {
    	drrr.print("玩家：\n" + names.map((user, index) => String(index + 1) + ". " + user).join("\n"))
    } else drrr.print("/me 沒有玩家")
  }
  event [msg, me] (user, cont: "^/start$") => {
    if names.length in rolesMap then going prelude
    else drrr.print("/me需滿足 5 - 7 人, 目前 " + String(names.length) + "人")
  }
  announce("/me [+1] 加入, [-1] 退出, [/who] 參賽者, [/start] 開始")
}

newPlayer = (name, role) => {
  name: name,
  life: true,
  role: role,
  rname: roleName[role],
  diefor: ""
}

state prelude {
  drrr.print("玩家：\n" + names.map((user, index) => String(index + 1) + ". " + user).join("\n"))

  roles = rolesMap[names.length]
  roles.sort(()=>Math.random() - 0.5)

  names.forEach((name, index) => {
    players[name] = newPlayer(name, roles[index]);
  })

  wolves = filter(players, (p, index) => p.role == 0).map(p => p.name).join(", ")
  forEach(players, (p, index) => {
    if p.role then later index * 3500 drrr.dm(p.name, "你的身份是:" + p.rname)
    else later index * 3500 drrr.dm(p.name, "你是狼, 所有狼是：" + wolves)
  })

  later names.length * 4000 going night
}

state night {
  announce("/me天黑請閉眼")
  later 3500 going night_seer
}

state night_seer {

  announce("/me 預言家請睜眼，你想知道關於誰的訊息（*私信* [人名]）...")
  if filter(players, (p, idx) => p.role == 2 && p.life).length then {
    asked = 0

    forEach(players, (p, index) => {
      if p.life && p.role == 2 then
        later 1000 drrr.dm(p.name, "想知道什麼")
    })

    event dm (seer, cont) => {
      if seer in players then {
        if !asked && players[seer].role == 2 then {
          if players[seer].life then {
            if !asked then {
              the = select(cont, names)
              if the then {
                drrr.dm(seer, if players[the].role > 0 then "人" else "狼" )
                asked = 1
                later 3500 going night_wolf
              } else drrr.dm(seer, "沒這個人")
            } else drrr.dm(seer, "只能問一次")
          } else drrr.dm(seer, "你是死掉的預言家")
        } else drrr.dm(seer, "你不是預言家，不要吵")
      } else drrr.print("/me@" + seer + " 不要吵！")
    }
  } else later (getRandom(10, 30) * 1000) going night_wolf
}

state night_wolf {
  announce("/me 狼人請睜眼，請採取行動（*私信* [no] or [人名]）...")
  killed = 0

  forEach(players, (p, index) => {
    if p.life && p.role == 0 then
      later (2000 * index) drrr.dm(p.name, "要殺人嗎")
  })

  event dm (wolf, cont) => {
    if wolf in players then {
      if players[wolf].role == 0 then {
        if players[wolf].life then {
          if !cont.startsWith("no") then {
            if !killed then {
              the = select(cont, names)
              if the then {
                victim = [the]
                players[the].diefor = "bite"
                killed = 1
                later 3500 going night_witch
              } else drrr.dm(wolf, "沒這個人")
            } else drrr.dm(wolf, "只能殺一次")
          } else victim = []
        } else drrr.dm(user, "你是死掉的狼人")
      } else drrr.dm(user, "你不是狼人")
    } else drrr.print("/me@" + wolf + " 不要吵！")
  }
}

state night_witch {

  announce("/me 女巫請睜眼，請採取行動（*私信* [no] or [救] or [毒 人名]）...")
  if filter(players, (p, idx) => p.role == 3 && p.life).length then {
    poisoned = 0

    names.forEach((name, index) => {
      if players[name].role == 3 then {
        later 3500 drrr.dm(name, "受害者：" + victim.join(", "))
      }
    })
    event dm (witch, cont) => {
      if witch in players then {
        if players[witch].role == 3 then {
          if players[witch].life then {
            if !poisoned then {
              the = select(cont, names)
              if the == witch then {
                drrr.dm(witch, "不能自救")
              } else if cont.startsWith("no") then {
                poisoned = 1
                later 3500 going night_end
              } else if cont.startsWith("不救") then {
                poisoned = 1
                later 3500 going night_end
              } else if cont.includes("救") then {
                victim = []
                poisoned = 1
                later 3500 going night_end
              } else if cont.includes("毒") then {
                if the.length then {
                  if players[the].life then{
                    if !victim.includes(the)
                    then {
                      victim.push(user)
                      players[user].diefor = "poison"
                    }
                    poisoned = 1
                    later 3500 going night_end
                  } else drrr.dm(user, "這人已經死了")
                } else drrr.dm(user, "沒有這個人")
              } else drrr.dm(user, "未知的指令")
            } else drrr.dm(user, "只能毒或救一次")
          } else drrr.dm(user, "你是死掉的女巫")
        } else drrr.dm(user, "你不是女巫")
      } else drrr.print("/me@" + witch + " 不要吵！")
    }
  } else later (getRandom(10, 30) * 1000) going night_end
}

state night_end {

  show = 0

  go_next = {
    drrr.print("/me倖存者：" + filter(players, p => p.life).map(p => p.name).join(", "))
    if passJudge() < 0
    then later 3500 going day_discussion
    else later 3500 going game_over
  }

  if victim.length then {
    announce("/me天亮了," + victim.map((x)=>"@" + x).join(", ") + "死了")

    victim.forEach((name) => {
      if players[name].role == 4 && players[name].diefor == "bite"
      then visit hunter_ask
    })

    later 3500 {
      drrr.print("/me請大家默哀三十秒")
      later 30000 {
        victim.forEach((name) => {
          players[name].life = false;
        })

        if show
        then visit hunter_fire
        else go_next()
      }
    }
  } else {
    announce("/me天亮了 沒有人死")
    later 3500 go_next
  }
}

state hunter_ask {
  forEach(players, (p, index) => {
    if p.life && p.role == 4 then
      later 1000 drrr.dm(p.name, "十五秒內發任何訊息以亮獵人牌")
  })
  event [dm, msg, me] (hunter, cont) => {
    if hunter in players then {
      if players[hunter].role == 4 then {
        show = 1
      }
    }
  }
}

state hunter_fire {
  drrr.print("/me獵人 @" + hunter + " 在臨死前開了一槍並打到了...[人名]")
  event [msg, me] (hunter, cont) => {
      if hunter in players then {
        if players[hunter].role == 4 then {
            the = select(cont, names)
            if the then {
              if players[the].life then {
                players[the].life = false
                go_next()
              } else drrr.dm(user, "這個人已經死了")
            } else drrr.dm(user, "沒有這個人")
        }
      }
    }
}

state day_discussion {
  index = 0
  while (index < names.length) && (players[names[index]].life == 0) index++;
  announce("/me請 @" + names[index] + " 開始發言 ([over] 結尾)")
  event [msg, me] (user, cont) => {
    if names[index] == user then {
      if cont.includes("over") then {
        index++ // += bug?
        while (index < names.length) && (players[names[index]].life == 0) index++;
        if index >= names.length
        then later 3500 going day_vote
        else announce("/me請 @" + names[index] + " 開始發言 ([over] 結尾)")
      }
    }
  }
}

state day_vote {
  announce("/me請開始投票 ([/vote] 看已投票, [/urge] 催票, *私信* [人名] 或是 [no] 棄票)")
  vote = {}

  survivor = filter(players, (p, idx) => p.live)

  forEach(players, (p, index) => {
    if p.life then later (2000 * index) drrr.dm(p.name, "請私信我投票，選項有：\n" + survivor.map((u) => "@" + u.name).join("\n"))
  })

  event dm (user, cont) => {
    if user in players then {
      if players[user].life then {
        if vote.hasOwnProperty(user)
        then drrr.dm(user, "一人一票，落票無悔")
        else {
          the = select(cont, names)
          if the then {
            if players[the].life then {
              vote[user] = the
              drrr.dm(user, "ok, 你投了 " + the)
              if Object.keys(vote).length == filter(players, p => p.life).length
              then later 3500 going day_execute
            } else drrr.dm(user, the + " 已經是個死人了，不要鞭屍好嗎。")
          }
          else if cont.startsWith("no") then {
            vote[user] = "no"
            drrr.dm(user, "ok, 你棄票了")
            if Object.keys(vote).length == filter(players, p => p.life).length
            then later 3500 going day_execute
          } else drrr.dm(user, "沒這個人")
        }
      } else drrr.dm(user, "死人還想投票啊？")
    } else drrr.print("/me@" + user + " 不要吵！")
  }
  event [msg, me] (user, cont: "^/vote$") => {
    drrr.print("/me目前已投票：" + Object.keys(vote).join(", "))
  }
  event [msg, me] (user, cont: "^/urge$") => {
    drrr.print("/me" + survivor.filter(u => !(u.name in vote)).map((u) => "@" + u.name).join(", ") + " 快點投票！")
  }
}

state day_execute {

  show = 0

  go_next = {
    drrr.print("/me倖存者：" + filter(players, p => p.life).map(p => p.name).join(", "))
    if passJudge() < 0
    then later 3500 going night
    else later 3500 going game_over
  }

  louis = most(Object.values(vote).filter((x) => x != "no"))
  announce("/me處死最高得票人：" + louis.map((x) => "@" + x).join(", "))

  louis.forEach((name) => {
    if players[name].role == 4
    then visit hunter_ask
  })

  later 3500 {
    drrr.print("/me請大家默哀三十秒")
    later 30000 {
      louis.forEach((name) => {
        players[name].life = false;
        players[name].diefor = "vote";
      })
      if show
      then visit hunter_fire
      else go_next()
    }
  }
}

state game_over {
  cur_role = map(players, (p, idx) => p.name + "：" + p.rname).join("\n")
  if passJudge()
  then drrr.print("/me遊戲結束, 人類勝出")
  else drrr.print("/me遊戲結束, 狼人勝出")
  later 2000 drrr.print(cur_role)
}

event [msg, me, dm] (user, cont: "^/char$") => {
  if user in players then {
    wolves = filter(players, (p, index) => p.role == 0).join(", ")
    if players[user].role then drrr.dm(user, "你的身份是:" + players[user].rname)
    else drrr.dm(user, "你是狼, 所有狼是：" + wolves)
  } else drrr.dm(user, "你不是玩家")
}

event [msg, me, dm] (user, cont: "^/all$") => {
  if Object.keys(players).length
  then drrr.print("玩家：\n" + map(players, (p, index) => String(index + 1) + ". " + p.name + (if p.life then " 活" else " 死")).join("\n"))
  else drrr.print("/me沒有玩家，請開始遊戲")
}

event [msg, me, dm] (user, cont: "^/now$") => {
  drrr.print(announcement)
}

event [msg, me, dm] (user, cont: "^/help$") => {
  drrr.print("/help 本手冊\n/now 現在遊戲狀態\n/all 當前所有玩家\n/char 當前擔任角色\n/werewolf 開始報名（如有遊戲則重新）")
}

event [msg, me] (user, cont: "^/werewolf$") => going prepare
"""

execute ctx = case parse parseScript ctx of
    Right script -> do
       runVM script
       -- log $ machine.val.toString undefined
    Left err -> do
       log ("error: " <> show err)
       pure $ { val: none undefined
              , cur: ""
              , env: Top
              , exprs: Nil
              , states: []
              }

compile ctx = case parse parseScript ctx of
    Right script -> logShow script
    Left err -> log ("error: " <> show err)

execute' ctx = do
    machine <- execute ctx
    log $ "=> " <> stringify_ machine.val

testing parser context = case parse parser context of
    Right ir -> logShow ir
    Left err -> log ("error: " <> show err)

testAbs = do
  testing parseScript "()" -- fail
  testing parseScript "a" -- pass
  testing parseScript "a : \"h\"" -- fail
  testing parseScript "(a)" -- pass
  testing parseScript "(a, b)" -- fail
  testing parseScript "1" -- pass
  testing parseScript "\"hello\"" -- pass

doing = execute'

ctx = """
"""

main = do
  doing testLoop
  doing testAjax
  doing testRecursion
  doing testLift
  doing testGoing
  doing testVisit
  doing guessNumber
  doing wolf
  compile wolf
  doing ctx
  doing "0 && print(2)"
  doing "print(2) || print(2)"

module Test.Main where

import Prelude
import Text.Parsing.Parser.Combinators

import Control.Alt ((<|>))
import Data.Array.ST.Iterator (next)
import BotScript (stringify_)
import BotScriptEnv (Env(..))
import BotScriptParser (parse, parseScript)
import BotScriptVM (MachineState, none, runVM, newObject)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.List (List(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Text.Parsing.Parser (ParserT)
import Undefined (undefined)

validForL0 = """for(i = 0; i < 10; i++) print(i)"""
validForL1 = """for (i = 0; i < 10; i++) print(i)"""
validForL2 = """for i = 0; i < 10; i++ print(i)"""
validForIn0 = """for(i in [1,2,3]) print(i);"""
validForIn1 = """for (i in [1,2,3]) print(i);"""
validForOf0 = """for(i of [1,2,3]) print(i);"""
validForOf1 = """for (i of [1,2,3]) print(i);"""
errorForIn0 = """for (i in ) print(i);"""
errorForIn1 = """for (1 in [1]) print(i);"""
errorForOf0 = """for (i of ) print(i);"""
errorForOf1 = """for (1 of [1]) print(i);"""

testL = """
for (i of [1,2,3]) print(i);
for(i = 0; i < 10; i++) print(i)
"""


testLoop :: String
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

j = 0
while j < 3 {
    print(j);
    j++;
}

for i of [1,2,3,4] print(i);
for(j in {tom: 1, allen: 2}) print(j);
for j in {tom: 1, allen: 2} print(j);
"""

testAjax :: String
testAjax = """
fetch("https://v1.hitokoto.cn")
  .then(response => response.json())
	.then(result => {
	print(result.hitokoto);
});
"""

testRecursion :: String
testRecursion = """
f = (x) =>
  if(x <= 0) then 0
  else if(x == 1) then 1
  else f(x - 1) + f(x - 2)

[0, 1, 2, 3, 4, 5, 6].map(f)
"""

testLift :: String
testLift = """
f = (a, b) => a + b;
print(f(1, 4)) // 5

g = { args[0] + args[1] };
print(g(1, 2)) // 3
"""

testGoing :: String
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

testVisit :: String
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

guessNumber :: String
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

wolf :: String
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

execute :: String -> Effect MachineState
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
              , events: newObject undefined
              , timers: newObject undefined
              }

compile :: String -> Effect Unit
compile ctx = case parse parseScript ctx of
    Right script -> logShow script
    Left err -> log ("error: " <> show err)


doing :: String -> Effect Unit
doing ctx = do
    machine <- execute ctx
    log $ "=> " <> stringify_ machine.val

testing :: forall t3 t8. Show t8 => ParserT t3 Identity t8 -> t3 -> Effect Unit
testing parser context = case parse parser context of
    Right ir -> logShow ir
    Left err -> log ("error: " <> show err)

testAbs :: Effect Unit
testAbs = do
  testing parseScript "()" -- fail
  testing parseScript "a" -- pass
  testing parseScript "a : \"h\"" -- fail
  testing parseScript "(a)" -- pass
  testing parseScript "(a, b)" -- fail
  testing parseScript "1" -- pass
  testing parseScript "\"hello\"" -- pass

ctx'' :: String
ctx'' = """
a = {k: 3}
a.val = 1
c = 1
b = a.val++
d = c++;
console.log(a.val)
console.log(b)
console.log(c)
console.log(d)
"""

objctx :: String
objctx = """
// usage:
//   werewolf("zh");
//   werewolf("en");

language = "en"

i18n = {
  "en": {
    "notBeg": "Game not start yet.",
    "werewolves": "Werewolves",
    "villagers": "Villagers",
    "seer": "Seer",
    "witch": "Witch",
    "hunter": "Hunter",
    "joined": u => u + " has already joined",
    "joins" : u => u + " joins the game",
    "leaves": u => u + " leaves the game",
    "notIn": u => u + " is not in the game",
    "players": ns => "Players:\n" + ns,
    "noPlayer": "No player",
    "need5to8": len => "Need 5 - 8 people, " + len + " people current.",
    "noPlayerBeg": "No player, please start the game",
    "prepare": "[+1] join, [-1] leave, [/who] participator, [/start] start",
    "UrRole": r =>  "You are " + r,
    "UrWerewolf": wvs => "You are werewolf, all werewolves are " + wvs,
    "night": "Night, please close your eyes/night",
    "talkSeer": "Seer, open your eyes, whose info do you want to know (*dm* [name])...",
    "wantToKnow": "What do you want to know",
    "people": "people",
    "werewolf": "werewolf",
    "noSuchPeople": "No such people",
    "oneAsk": "Only one asking is allowed",
    "deadSeer": "You are dead seer",
    "notSeer": "You are not seer, BE QUIET",
    "beQuiet": u => "@" + u + " BE QUIET!",
    "talkWerewolves": "Werewolves, open your eyes, take actions (*dm* [no] or [name])...",
    "askKill": "Kill people?",
    "oneKill": "Only one kill is allowed",
    "deadWerewolf": "You are dead werewolf",
    "notWerewolf": "You are not werewolf",
    "talkWitch": "Witch, open your eyes, take actions (*dm* [no] or [save] or [poison name])...",
    "victims": vs => "Victim:" + vs,
    "can'tSaveSelf": "Cannot save yourself",
    "deadMan": "The man is dead",
    "unkCmd": "Unknown command",
    "onePoiSav": "Only poison or save once is allowed",
    "deadWitch": "You are dead witch",
    "notWitch": "You are not witch",
    "hunter15s": "send any message in 15 secs to show you are hunter",
    "fired": h => "Hunter @" + h + " fired...",
    "survivors": ss => "Survivor:" + ss,
    "hit": t => ", and hit " + t + " before he died",
    "morning": "Sun Arise/morning",
    "died": us => us + " died",
    "rip30s": "R.I.P for 30 secs",
    "morningSafe": "Morning, no one died",
    "speaking": n => "@" + n + " start speaking (end with [over])",
    "voting": "Please start voting ([/vote] check voted, [/urge] reminder, *dm* [name] or [no] abstain)",
    "voteNote": cans => "Please dm me to vote, candidates:\n" + cans,
    "oneVote": "Only one vote is allowed",
    "checkVote": t => "Ok, you vote " + t,
    "voteDead": t => t + " is already dead",
    "abstain": "Ok, abstain",
    "deadVote": "You are already dead",
    "curVote": us => "Current voted:" + us,
    "urgeVote": us => us + " vote, please!",
    "execute": us => "Execute:" + us,
    "peopleWin": "Game Over, people win",
    "werewolvesWin": "Game Over, werewolves win",
    "notPlayer": "You are not player",
    "alive": "Alive",
    "dead": "Dead",
    "manual": "/help The manual\n/now Game State\n/all All Players\n/char Your Role\n/werewolf Start Participating (Will restart if game is running)"
  },
  "zh": {
    "notBeg": "尚未開始遊戲",
    "werewolves": "狼",
    "villagers": "平民",
    "seer": "預言家",
    "witch": "女巫",
    "hunter": "獵人",
    "joined": u => u + " 已經加入了",
    "joins" : u => u + " 加入遊戲",
    "leaves": u => u + " 退出遊戲",
    "notIn": u => u + " 不在遊戲中",
    "players": ns => "玩家：\n" + ns,
    "noPlayer": "沒有玩家",
    "need5to8": len => "需滿足 5 - 8 人, 目前 " + len + "人",
    "noPlayerBeg": "No player, please start the game",
    "prepare": "/me [+1] 加入, [-1] 退出, [/who] 參賽者, [/start] 開始",
    "UrRole": r =>  "你的身份是：" + r,
    "UrWerewolf": wvs => "你是狼, 所有狼是：" + wvs,
    "night": "天黑請閉眼/night",
    "talkSeer": "預言家請睜眼，你想知道關於誰的訊息（*私信* [人名]）...",
    "wantToKnow": "想知道什麼",
    "people": "人",
    "werewolf": "狼",
    "noSuchPeople": "沒這個人",
    "oneAsk": "只能問一次",
    "deadSeer": "你是死掉的預言家",
    "notSeer": "你不是預言家，不要吵",
    "beQuiet": u => "@" + u + " 不要吵！",
    "talkWerewolves": "狼人請睜眼，請採取行動（*私信* [no] or [人名]）...",
    "askKill": "要殺人嗎？",
    "oneKill": "只能殺一次",
    "deadWerewolf": "你是死掉的狼人",
    "notWerewolf": "你不是狼人",
    "talkWitch": "女巫請睜眼，請採取行動（*私信* [no] or [save] or [poison 人名]）...",
    "victims": vs => "受害者：" + vs,
    "can'tSaveSelf": "不能自救",
    "deadMan": "這人已經死了",
    "unkCmd": "未知的指令",
    "onePoiSav": "只能毒或救一次",
    "deadWitch": "你是死掉的女巫",
    "notWitch": "你不是女巫",
    "hunter15s": "十五秒內發任何訊息以亮獵人牌",
    "fired": h => "獵人 @" + h + " 在臨死前開了一槍並打到了...",
    "hit": t => "這一槍落到了 @" + t + " 身上",
    "survivors": ss => "倖存者：" + ss,
    "morning": "東方漸泛魚肚白/morning",
    "died": us => us + " 死了",
    "rip30s": "請大家默哀三十秒",
    "morningSafe": "天亮了，沒有人死",
    "speaking": n => "請 @" + n + " 開始發言 ([over] 結尾)",
    "voting": "/me請開始投票 ([/vote] 看已投票, [/urge] 催票, *私信* [人名] 或是 [no] 棄票)",
    "voteNote": cans => "請私信我投票，選項有：\n" + cans,
    "oneVote": "一人一票，落票無悔",
    "checkVote": t => "ok, 你投了 " + t,
    "voteDead": t => t + " 已經是個死人了，不要鞭屍好嗎。",
    "abstain": "ok, 你棄票了",
    "deadVote": "死人還想投票啊？",
    "curVote": us => "目前已投票：" + us,
    "urgeVote": us => us + " 快點投票！",
    "execute": us => "處死最高得票人：" + us,
    "peopleWin": "遊戲結束, 人類勝出",
    "werewolvesWin": "遊戲結束, 狼人勝出",
    "notPlayer": "你不是玩家",
    "alive": "活",
    "dead": "死",
    "manual": "/help 本手冊\n/now 現在遊戲狀態\n/all 當前所有玩家\n/char 當前擔任角色\n/werewolf 開始報名（如有遊戲則重新）"
  }
}

T = (key) => i18n[language][key]

me = (str) => "/me" + str

names = []
players = {}
victim = []
vote = {}

announcement = me(T("notBeg"))
announce = (msg) => {
  announcement = msg
  drrr.print(msg)
}

scene = (desc) => {
  announcement = msg
  if room.host == user.id
  then drrr.descr(msg)
  else drrr.print(msg)
}

// 0 werewolf 1 villagers 2 seer 3 witch 4 hunter
roleName = [T("werewolves"), T("villagers"), T("seer"), T("witch"), T("hunter")]
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
    drrr.print(me(T("joined")(user)))
    else{
      names.push(user)
      drrr.print(me(T("joins")(user)))
    }
  }
  event [msg, me] (user, cont: "^-1$") => {
    if names.includes(user) then {
      names.splice(names.indexOf(user), 1);
      drrr.print(me(T("leaves")(user)))
    }
    else drrr.print(me(T("notIn")(user)))
  }
  event [msg, me] (user, cont: "^/who$") => {
    if names.length then {
      drrr.print(T("player") + ":\n" + names.map((user, index) => String(index + 1) + ". " + user).join("\n"))
    } else drrr.print(me(T("noPlayer")))
  }
  event [msg, me] (user, cont: "^/start$") => {
    if names.length in rolesMap then going prelude
    else drrr.print(me(T("need5to8")(String(names.length))))
  }
  announce(me(T("prepare")))
}

newPlayer = (name, role) => {
  name: name,
    life: true,
    role: role,
    rname: roleName[role],
    diefor: ""
}

state prelude {
  drrr.print(T("players")(names.map((user, index) => String(index + 1) + ". " + user).join("\n")))

  roles = rolesMap[names.length]
  roles.sort(()=>Math.random() - 0.5)

  names.forEach((name, index) => {
    players[name] = newPlayer(name, roles[index]);
  })

  wolves = filter(players, (p, index) => p.role == 0).map(p => p.name).join(", ")
  forEach(players, (p, index) => {
    if p.role then later index * 3500 drrr.dm(p.name, T("UrRole")(p.rname))
    else later index * 3500 drrr.dm(p.name, T("UrWerewolf")(wolves))
  })

  later names.length * 4000 going night
}

state night {
  scene(me(T("night")))
  later 3500 going night_seer
}

state night_seer {

  announce(me(T("talkSeer")))
  if filter(players, (p, idx) => p.role == 2 && p.life).length then {
    asked = 0

    forEach(players, (p, index) => {
      if p.life && p.role == 2 then
      later 1000 drrr.dm(p.name, T("wantToKnow"))
    })

    event dm (seer, cont) => {
      if seer in players then {
        if !asked && players[seer].role == 2 then {
          if players[seer].life then {
            if !asked then {
              the = select(cont, names)
              if the then {
                drrr.dm(seer, if players[the].role > 0 then T("people") else T("werewolf"))
                asked = 1
                later 3500 going night_wolf
              } else drrr.dm(seer, T("noSuchPeople"))
            } else drrr.dm(seer, T("oneAsk"))
          } else drrr.dm(seer, T("deadSeer"))
        } else drrr.dm(seer, T("notSeer"))
      } else drrr.print(me(T("beQuiet")(seer)))
    }
  } else later (getRandom(10, 30) * 1000) going night_wolf
}

state night_wolf {
  announce(me(T("talkWerewolves")))
  killed = 0

  forEach(players, (p, index) => {
    if p.life && p.role == 0 then
    later (2000 * index) drrr.dm(p.name, T("askKill"))
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
              } else drrr.dm(wolf, T("noSuchPeople"))
            } else drrr.dm(wolf, T("oneKill"))
          } else victim = []
        } else drrr.dm(user, T("deadWerewolf"))
      } else drrr.dm(user, T("notWerewolf"))
    } else drrr.print(me(T("beQuiet")(wolf)))
  }
}

state night_witch {

  announce(me(T("talkWitch")))
  if filter(players, (p, idx) => p.role == 3 && p.life).length then {
    poisoned = 0

    names.forEach((name, index) => {
      if players[name].role == 3 then {
        later 3500 drrr.dm(name, T("victims")(victim.join(", ")))
      }
    })
    event dm (witch, cont) => {
      if witch in players then {
        if players[witch].role == 3 then {
          if players[witch].life then {
            if !poisoned then {
              the = select(cont, names)
              if the == witch then {
                drrr.dm(witch, T("can'tSaveSelf"))
              } else if cont.startsWith("no") then {
                poisoned = 1
                later 3500 going night_end
              } else if cont.startsWith("ignore") then {
                poisoned = 1
                later 3500 going night_end
              } else if cont.includes("save") then {
                victim = []
                poisoned = 1
                later 3500 going night_end
              } else if cont.includes("poison") then {
                if the.length then {
                  if players[the].life then{
                    if !victim.includes(the)
                    then {
                      victim.push(user)
                      players[user].diefor = "poison"
                    }
                    poisoned = 1
                    later 3500 going night_end
                  } else drrr.dm(user, T("deadMan"))
                } else drrr.dm(user, T("noSuchPeople"))
              } else drrr.dm(user, T("unkCmd"))
            } else drrr.dm(user, T("onePoiSav"))
          } else drrr.dm(user, T("deadWitch"))
        } else drrr.dm(user, T("notWitch"))
      } else drrr.print(me(T("beQuiet")(witch)))
    }
  } else later (getRandom(10, 30) * 1000) going night_end
}

state hunter_ask {
  forEach(players, (p, index) => {
    if p.life && p.role == 4 then
    later 1000 drrr.dm(p.name, T("hunter15s"))
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
  drrr.print(me(T("fired")(hunter)))
  event [msg, me] (hunter, cont) => {
    if hunter in players then {
      if players[hunter].role == 4 then {
        the = select(cont, names)
        if the then {
          if players[the].life then {
            players[the].life = false
            drrr.print(T("hit")(the))
            go_next()
          } else drrr.dm(user, T("deadMan"))
        } else drrr.dm(user, T("noSuchPeople"))
      }
    }
  }
}

state night_end {

  show = 0

  go_next = {
    drrr.print(me(T("survivors")(filter(players, p => p.life).map(p => p.name).join(", "))))
    if passJudge() < 0
    then later 3500 going day_discussion
    else later 3500 going game_over
  }

  scene(T("morning"))

  later 3000 {
    if victim.length then {

      announce(me(T("died")(victim.map((x)=>"@" + x)).join(", ")))

      victim.forEach((name) => {
        if players[name].role == 4 && players[name].diefor == "bite"
        then visit hunter_ask
      })

      later 3500 {
        drrr.print(me(T("rip30s")))
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
      announce(me(T("morningSafe")))
      later 3500 go_next
    }
  }
}

state day_discussion {
  index = 0
  while (index < names.length) && (players[names[index]].life == 0) index++;
  announce(me(T("speaking")(names[index])))

  event [msg, me] (user, cont) => {
    if names[index] == user then {
      if cont.includes("over") then {
        index++ // += bug?
        while (index < names.length) && (players[names[index]].life == 0) index++;
        if index >= names.length
        then later 3500 going day_vote
        else announce(me(T("speaking")(names[index])))
      }
    }
  }
}

state day_vote {
  announce(me(T("voting")))
  vote = {}

  survivor = filter(players, (p, idx) => p.live)

  forEach(players, (p, index) => {
    if p.life then later (2000 * index) drrr.dm(p.name, T("voteNote")(survivor.map((u) => "@" + u.name).join("\n")))
  })

  event dm (user, cont) => {
    if user in players then {
      if players[user].life then {
        if vote.hasOwnProperty(user)
        then drrr.dm(user, T("oneVote"))
        else {
          the = select(cont, names)
          if the then {
            if players[the].life then {
              vote[user] = the
              drrr.dm(user, T("checkVote")(the))
              if Object.keys(vote).length == filter(players, p => p.life).length
              then later 3500 going day_execute
            } else drrr.dm(user, T("voteDead")(the))
          }
          else if cont.startsWith("no") then {
            vote[user] = "no"
            drrr.dm(user, T("abstain"))
            if Object.keys(vote).length == filter(players, p => p.life).length
            then later 3500 going day_execute
          } else drrr.dm(user, T("noSuchPeople"))
        }
      } else drrr.dm(user, T("deadVote"))
    } else drrr.print(me(T("beQuiet")(user)))
  }
  event [msg, me] (user, cont: "^/vote$") => {
    drrr.print(me(T("curVote")(Object.keys(vote).join(", "))))
  }
  event [msg, me] (user, cont: "^/urge$") => {
    drrr.print(me(T("urgeVote")(survivor.filter(u => !(u.name in vote)).map((u) => "@" + u.name).join(", "))))
  }
}

state day_execute {

  show = 0

  go_next = {
    drrr.print(me(T("survivors")(filter(players, p => p.life).map(p => p.name).join(", "))))
    if passJudge() < 0
    then later 3500 going night
    else later 3500 going game_over
  }

  louis = most(Object.values(vote).filter((x) => x != "no"))
  announce(me(T("execute")(louis.map((x) => "@" + x).join(", "))))

  louis.forEach((name) => {
    if players[name].role == 4
    then visit hunter_ask
  })

  later 3500 {
    drrr.print(me(T("rip30s")))
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
  cur_role = map(players, (p, idx) => p.name + ":" + p.rname).join("\n")
  if passJudge()
  then drrr.print(me(T("peopleWin")))
  else drrr.print(me(T("werewolvesWin")))
  later 2000 drrr.print(cur_role)
}

werewolf = (lang) => {

  if lang then {
    language = lang
    announcement = me(T("notBeg"))
    roleName = [T("werewolves"), T("villagers"), T("seer"), T("witch"), T("hunter")]
  }

  event [msg, me, dm] (user, cont: "^/char$") => {
    if user in players then {
      wolves = filter(players, (p, index) => p.role == 0).join(", ")
      if players[user].role then drrr.dm(user, T("UrRole")(players[user].rname))
      else drrr.dm(user, T("UrWerewolf")(wolves))
    } else drrr.dm(user, T("notPlayer"))
  }

  event [msg, me, dm] (user, cont: "^/all$") => {
    if Object.keys(players).length
    then drrr.print(T("players")(map(players, (p, index) => String(index + 1) + ". " + p.name + " " + (if p.life then T("alive") else T("dead"))).join("\n")))
    else drrr.print(me(T("noPlayerBeg")))
  }

  event [msg, me, dm] (user, cont: "^/now$") => {
    drrr.print(announcement)
  }

  event [msg, me, dm] (user, cont: "^/help$") => {
    drrr.print(T("manual"))
  }

  event [msg, me] (user, cont: "^/werewolf$") => going prepare
}"""

test :: String
-- test = "later (3000); (a, b) => console.log(2)"
test = """
k = {}
a = "a"
k[a] = 0
k[a]++
console.log(k)
"""
-- test = """
-- state day {}
-- t = a => "hello"
-- state night {}
-- """
-- test = """{ x: "asdf" => 3 }"""


{-
testString = "for (i = 0; i < 10; i++) print(i)"
testParser parser ctx = case parse parser ctx of
    Right script -> logShow script
    Left err -> log ("error: " <> show err)

main = do
  -- testParser (parseForIn parseExpr) validForIn0
  -- testParser (parseForOf parseExpr) validForOf0
  -- testParser (parseFLoop parseExpr) validForL0
  -- testParser (parseStmt parseExpr) validForL0
  -- testParser ((try (parseForOf parseExpr))
  --            <|> parseFLoop parseExpr) validForL0
  -- testParser (parseStmtExpr parseExpr) validForL0
  -- testParser (parseSimpleExpr parseExpr) validForL0
  -- testParser (parseExpr) validForL0
  -- testParser parseStringLiteral "\"hello world\""
  -- compile "10 (a, b) => { a + b } "
  compile validForL0
  compile validForL1
  compile validForL2
  compile validForOf0
  compile validForOf1
  compile validForIn0
  compile validForIn1
  compile testL
  compile testAjax
  compile testRecursion
  compile testLift
  compile testGoing
  compile testVisit
  compile guessNumber
  compile wolf
  compile ctx
  compile "0 && print(2)"
  compile "print(2) || print(2)"
  compile objctx
  compile test
  log "======= error below ======"
  compile errorForIn0
  compile errorForIn1
  compile errorForOf0
  compile errorForOf1
 -}
 
testMachine :: String
testMachine = """
-1
//k = a => a + 1
//names = []
//f = () => {
//  if true then {
//    console.log(__this__.env.value0.root.value0.root)
//    //console.log(__this__.env.value0.root.value0.root.value0.root)
//    console.log(k(3))
//    console.log(names)
//  }
//  console.log(names)
//}
//
//console.log(names)
//f()
//console.log(names)
"""

tmpTest = """
// usage:
//   werewolf("zh");
//   werewolf("en");

language = "en"

i18n = {
  "en": {
    "notBeg": "Game not start yet.",
    "werewolves": "Werewolves",
    "whiteWolf": "WhiteWolfKing",
    "blackWolf": "BlackWolfKing",
    "villagers": "Villagers",
    "seer": "Seer",
    "witch": "Witch",
    "hunter": "Hunter",
    "guard": "Guard",
    "idiot": "Idiot",
    "joined": u => u + " has already joined",
    "joins" : u => u + " joins the game",
    "leaves": u => u + " leaves the game",
    "notIn": u => u + " is not in the game",
    "players": ns => "Players:\n" + ns,
    "action" : "Please take action",
    "noPoison": "No poison left",
    "noMedicine": "No medicine left",
    "configNotFit": len => "Need 5 - 12 people, " + len + " people current.",
    "noPlayerBeg": "No player, please start the game",
    "prepare": "[+1] join [-1] out [/go] start [/?] other",
    "setting": "[/side] side mode\n[/clear] clear mode",
    "UrRole": r =>  "You are " + r,
    "UrWerewolf": wvs => "You are werewolf, all werewolves are " + wvs,
    "night": d => "The " + String(d) + ord(d) + " day. Night, please close your eyes.",
    "talkSeer": "Seer, open your eyes, whose info do you want to know (*dm* [name])...",
    "wantToKnow": "What do you want to know",
    "people": "people",
    "werewolf": "werewolf",
    "noSuchPeople": "No such people",
    "oneAsk": "Only one asking is allowed",
    "deadRole": r => "You are dead " + r,
    "dupProtect": "You cannot protect same people for two consecutive nights",
    "notRole": r => "You are not " + r,
    "beQuiet": u => "@" + u + " BE QUIET!",
    "talkWerewolves": "Werewolves, open your eyes, take actions (*dm* [no] or [name])...",
    "askKill": "Kill people?",
    "oneKill": "Only one kill is allowed",
    "talkWitch": "Witch, open your eyes, take actions (*dm* [no] or [save] or [poison name])...",
    "talkGuard": "Guard, open your eyes, take actions (*dm* [no] or [name])...",
    "victims": vs => "Victim:" + vs,
    "can'tSaveSelf": "Cannot save yourself",
    "deadMan": "The man is dead",
    "unkCmd": "Unknown command",
    "oneCmd": "Only one command is allowed",
    "hunter15s": "send any message in 15 secs to show you are hunter",
    "hunterShow": n => "The hunter @" + n + " is dead! ... and (other dead guys continue their last words)",
    "whiteWolfShow": n => "The WhiteWolfKing @" + n + " is dead! ... and (other dead guys continue their last words)",
    "idiot15s": "send any message in 15 secs to show you are idiot",
    "idiotShow": n => "@" + n + " is the idiot! Escaped from the execution!",
    "fired": h => h + " kill... (choose a player)",
    "survivors": ss => "Survivor:" + ss,
    "hit": t => ", and hit " + t + " before he died",
    "morning": d => "The " + String(d) + ord(d) + " day. Morning, Sun Arise.",
    "died": us => us + " died",
    "rip30s": "R.I.P for 30 secs",
    "morningSafe": "Morning, no one died",
    "speaking": n => "@" + n + " start speaking (end with [over]), [/skip] to skip the guy, [/expo name] cast white wolf king's skill",
    "voting": "Please start voting (can use dm) ([/vote] check voted, [/urge] reminder, [/execute] skip, [/vote name] or [/vote no] abstain)",
    "voteNote": cans => "Message to vote, candidates:\n" + cans,
    "oneVote": "Only one vote is allowed",
    "checkVote": t => "Ok, you vote " + t,
    "voteDead": t => t + " is already dead",
    "abstain": "Ok, abstain",
    "noRight": "You have no voting right",
    "curVote": us => "Current voted:" + us,
    "urgeVote": us => us + " vote, please!",
    "urgeShoot": us => us + " kill, please!",
    "reVote": us => "Top people:" + us + ", revote",
    "execute": us => "Execute:" + us,
    "noExecute": us => "Top people again:" + us + ", no execute",
    "draw": "Game Over, draw",
    "peopleWin": "Game Over, people win",
    "werewolvesWin": "Game Over, werewolves win",
    "notPlayer": "You are not player",
    "alive": "Alive",
    "dead": "Dead",
    "manual": "/help The manual\n/s Game State\n/w All Players\n/r Your Role\n/game Start Participating (Will restart if game is running)"
  },
  "zh": {
    "notBeg": "尚未開始遊戲",
    "werewolves": "狼",
    "whiteWolf": "白狼王",
    "blackWolf": "黑狼王",
    "villagers": "平民",
    "seer": "預言家",
    "witch": "女巫",
    "hunter": "獵人",
    "guard": "守衛",
    "idiot": "白痴",
    "joined": u => u + " 已經加入了",
    "joins" : u => u + " 加入遊戲",
    "leaves": u => u + " 退出遊戲",
    "notIn": u => u + " 不在遊戲中",
    "players": ns => "玩家：\n" + ns,
    "action" : "請採取行動",
    "noPoison": "沒有毒藥了",
    "noMedicine": "沒有解藥了",
    "configNotFit": len => "需滿足 5 - 12 人, 目前 " + len + " 人",
    "noPlayerBeg": "沒有玩家，請開始報名程序",
    "prepare": "[+1] 加入 [-1] 退出 [/go] 開始 [/?] 其他",
    "setting": "[/side] 屠邊\n[/clear] 屠城\n",
    "UrRole": r =>  "你的身份是：" + r,
    "UrWerewolf": wvs => "你是狼, 所有狼是：" + wvs,
    "night": d => "第 " + String(d) + " 天，天黑請閉眼/夜",
    "talkSeer": "預言家請睜眼，你想知道關於誰的訊息（*私信* [人名]）...",
    "wantToKnow": "想知道什麼",
    "people": "人",
    "werewolf": "狼",
    "noSuchPeople": "沒這個人",
    "oneAsk": "只能問一次",
    "deadRole": r => "你是死掉的" + r,
    "dupProtect": "不能連續保護同一人兩晚",
    "beQuiet": u => "@" + u + " 不要吵！",
    "talkWerewolves": "狼人請睜眼，請採取行動（*私信* [no] or [人名]）...",
    "askKill": "要殺人嗎？",
    "oneKill": "只能殺一次",
    "talkWitch": "女巫請睜眼，請採取行動（*私信* [no] or [save] or [poison 人名]）...",
    "talkGuard": "守衛請睜眼，請採取行動（*私信* [no] or [人名]）...",
    "victims": vs => "受害者：" + vs,
    "can'tSaveSelf": "不能自救",
    "deadMan": "這人已經死了",
    "unkCmd": "未知的指令",
    "oneCmd": "只能用一次指令",
    "hunter15s": "十五秒內發任何訊息以亮獵人牌",
    "hunterShow": n => "獵人 @" + n + " 死了！... 然後...（請其他死者繼續發表遺言）",
    "whiteWolfShow": n => "白狼王 @" + n + " 死了！... 然後...（請其他死者繼續發表遺言）",
    "idiot15s": "十五秒內發任何訊息以亮白痴牌",
    "idiotShow": n => "@" + n + " 的身份是白痴！他逃過了一劫！",
    "fired": h => h + " 在臨死前殺死了...（選一個玩家）",
    "hit": t => "這一槍落到了 @" + t + " 身上",
    "survivors": ss => "倖存者：" + ss,
    "morning": d => "第 " + String(d) + " 天，東方漸泛魚肚白/早上",
    "died": us => us + " 死了",
    "rip30s": "請大家默哀三十秒",
    "morningSafe": "天亮了，沒有人死",
    "speaking": n => "請 @" + n + " 開始發言 ([over] 結尾), [/skip] 跳過此人, , [/expo name] 使用白狼王技能",
    "voting": "請開始投票（可私信）([/vote] 看已投票, [/urge] 催票, [/execute] 跳過投票, [/vote 人名] 或是 [/vote no] 棄票)",
    "voteNote": cans => "發言以投票，選項有：\n" + cans,
    "oneVote": "一人一票，落票無悔",
    "checkVote": t => "ok, 你投了 " + t,
    "voteDead": t => t + " 已經是個死人了，不要鞭屍好嗎。",
    "abstain": "ok, 你棄票了",
    "noRight": "你沒有投票權",
    "curVote": us => "目前已投票：" + us,
    "urgeVote": us => us + " 快點投票！",
    "urgeShoot": us => us + " 快點殺人！",
    "reVote": us => "最高得票人：" + us + ", 重新投票",
    "execute": us => "處死最高得票人：" + us,
    "noExecute": us => "仍有多個最高票：" + us + ", 取消死刑",
    "draw": "遊戲結束，平局",
    "peopleWin": "遊戲結束，人類勝出",
    "werewolvesWin": "遊戲結束，狼人勝出",
    "notPlayer": "你不是玩家",
    "alive": "活",
    "dead": "死",
    "manual": "/help 本手冊\n/s 現在遊戲狀態\n/w 當前所有玩家\n/r 當前擔任角色\n/game 開始報名（如有遊戲則重新）"
  }
}

T = (key) => i18n[language][key]

me = (str) => "/me" + str

ord_enum = { 0: "th", 1: "st", 2: "nd", 3: "rd" }

ord = (num) => if num > 3 then "th" else ord_enum[num]

names = []
players = {}
victim = []
vote = {}
day = 1
poison = 1
medicine = 1
vote_times = 0
protect = ""

// 0: kill side, 1: clear people
kill_mode = 0

announcement = me(T("notBeg"))
announce = (msg) => {
  announcement = msg
  drrr.print(msg)
}

scene = (desc) => {
  announcement = desc
  if room.host == user.id
  then drrr.descr(desc.replace("/me", ""))
  else drrr.print(desc)
}

setAlive = (name, state) => {
  if room.gameRoom
  then drrr.alive(name, state)
}

setPlayer = (name, state) => {
  if room.gameRoom
  then drrr.player(name, state)
}

// 0 werewolf 1 villagers 2 seer 3 witch 4 hunter 5 guard
roles = []
roleName = {}

rWing = -2
rBing = -1
rWolf = 0
rVill = 1
rSeer = 2
rWitc = 3
rHunt = 4
rGuar = 5
rIdio = 6

isWolf = r => r <= 0
isGod = r => r > 1
isShooter = r => (r == rHunt || r == rBing)

rolesMap = {
  5: [[rWolf, rVill, rVill, rSeer, rWitc]],
  6: [[rWolf, rWolf, rVill, rVill, rSeer, rWitc],
      [rWolf, rWolf, rVill, rVill, rSeer, rHunt],
      [rWolf, rWolf, rVill, rVill, rSeer, rGuar],
      [rWolf, rWolf, rHunt, rHunt, rHunt, rHunt]],
  7: [[rWolf, rWolf, rVill, rVill, rVill, rSeer, rWitc],
      [rWolf, rWolf, rVill, rVill, rVill, rSeer, rHunt]],
  8: [[rWolf, rWolf, rVill, rVill, rVill, rSeer, rWitc, rHunt],
      [rWolf, rWolf, rWolf, rVill, rVill, rSeer, rWitc, rHunt]],
  9: [[rWolf, rWolf, rWolf, rVill, rVill, rVill, rSeer, rWitc, rHunt]],
  10: [[rWolf, rWolf, rWolf, rVill, rVill, rVill, rVill, rSeer, rWitc, rHunt],
      [rWolf, rWolf, rWolf, rVill, rVill, rVill, rVill, rSeer, rHunt, rGuar],
      [rWolf, rWolf, rWolf, rVill, rVill, rVill, rVill, rWitc, rHunt, rGuar],
      [rWolf, rWolf, rWolf, rHunt, rHunt, rHunt, rHunt, rHunt, rHunt, rHunt]],
  11: [[rWolf, rWolf, rWolf, rVill, rVill, rVill, rVill, rVill, rWitc, rHunt, rGuar]],
  12: [[rWolf, rWolf, rWolf, rWolf, rVill, rVill, rVill, rVill, rSeer, rWitc, rHunt, rGuar]]
}

initRoleName = () => {
  roleName = {}
  roleName[rWing] = T("whiteWolf")
  roleName[rBing] = T("blackWolf")
  roleName[rWolf] = T("werewolves")
  roleName[rVill] = T("villagers")
  roleName[rSeer] = T("seer")
  roleName[rWitc] = T("witch")
  roleName[rHunt] = T("hunter")
  roleName[rGuar] = T("guard")
  roleName[rIdio] = T("idiot")
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
  werewolves = filter(players, (p, index) => p.life && isWolf(p.role))
  people = filter(players, (p, index) => p.life && !isWolf(p.role))
  villagers = filter(players, (p, index) => p.life && p.role == rVill)
  gods = filter(players, (p, index) => p.life && isGod(p.role))

  if kill_mode
  then { // clear
    if werewolves.length && people.length then -1 // both alive
    else if werewolves.length then 0 // werewolves alive
    else if people.length then 1 // people alive
    else 2 // both zero, draw
  }
  else { // kill side
    if werewolves.length && gods.length && villagers then -1 // both alive
    else if werewolves.length then 0 // werewolves alive
    else if gods.length && villagers.length then 1 // people alive
    else 2 // both zero, draw
  }
}

state prepare {
  victim = []
  names = []
  players = {}
  vote = {}
  day = 1
  poison = 1
  medicine = 1
  vote_times = 0
  roles = []
  protect = ""

  event [msg, me] (user, cont: "^\\+1$") => {
    if names.includes(user) then
    drrr.print(me(T("joined")(user)))
    else{
      names.push(user)
      drrr.print(me(T("joins")(user)))
    }
  }
  event [msg, me] (user, cont: "^-1$") => {
    if names.includes(user) then {
      names.splice(names.indexOf(user), 1);
      drrr.print(me(T("leaves")(user)))
    }
    else drrr.print(me(T("notIn")(user)))
  }
  event [msg, me] (user, cont: "^/go$") => {
    if names.length in rolesMap then {
      if room.gameRoom then going initial
      else going prelude
    }
    else drrr.print(me(T("configNotFit")(String(names.length))))
  }
  event [msg, me] (user, cont: "^/\\?$") => {
    drrr.print(T("setting"))
  }
  event [msg, me] (user, cont: "^/side$") => {
    kill_mode = 0
    drrr.print(me("ok, side mode"))
  }
  event [msg, me] (user, cont: "^/clear$") => {
    kill_mode = 1
    drrr.print(me("ok, clear mode"))
  }
  announce(me(T("prepare")))
}

newPlayer = (name, role) => {
  name: name,
    life: true,
    role: role,
    rname: roleName[role],
    diefor: "",
    right: true
}

state initial {
  cnt = 1
  updateLoc(()=>{
    users.forEach((u, i) => {
      isHost = u.name == user.name
      if names.includes(u.name) != u.player || isHost
      then {
        setTimeout(() => setPlayer(u.name, !u.player || isHost), cnt++ * 1500)
      }
      else if u.player && !u.alive
      then setTimeout(() => setAlive(u.name, true), cnt++ * 1500)
    })
    setTimeout(() => going prelude, cnt++ * 1500)
  })
}

state prelude {

  drrr.print(T("players")(names.map((user, index) => String(index + 1) + ". " + user).join("\n")))

  comb = rolesMap[names.length]
  roles = comb[Math.floor(Math.random() * comb.length)].slice();

  later 2000 {

    drrr.print(roles.map((r) => roleName[r]).join("\n"))

    roles.sort(()=>Math.random() - 0.5)

    names.forEach((name, index) => {
      players[name] = newPlayer(name, roles[index]);
    })

    wolves = filter(players, (p, index) => p.role == rWolf).map(p => p.name).join(", ")
    forEach(players, (p, index) => {
      if p.role then later (index * 3500 + 1000) drrr.dm(p.name, T("UrRole")(p.rname))
      else later (index * 3500 + 1000) drrr.dm(p.name, T("UrWerewolf")(wolves))
    })

    later names.length * 4000 going night
  }
}

state night {
  scene(me(T("night")(day)))
  later 3500 going night_seer
}

state night_seer {

  theRole = 2

  if !roles.includes(theRole) then going night_guard

  announce(me(T("talkSeer")))
  if filter(players, (p, idx) => p.role == theRole && p.life).length then {
    asked = 0

    forEach(players, (p, index) => {
      if p.life && p.role == theRole then
      later 1500 drrr.dm(p.name, T("wantToKnow"))
    })

    event dm (seer, cont) => {
      if seer in players then {
        if !asked && players[seer].role == theRole then {
          if players[seer].life then {
            if !asked then {
              the = select(cont, names)
              if the then {
                drrr.dm(seer, if players[the].role > 0 then T("people") else T("werewolf"))
                asked = 1
                later 3500 going night_guard
              } else drrr.dm(seer, T("noSuchPeople"))
            } else drrr.dm(seer, T("oneAsk"))
          } else drrr.dm(seer, T("deadRole")(T("seer")))
        } else drrr.dm(seer, T("notRole")(T("seer")))
      } else drrr.print(me(T("beQuiet")(seer)))
    }

    event [dm, msg] (user, cont: "^/skip$") => {
      if user in players then going night_guard
    }

  } else later (getRandom(10, 30) * 1500) going night_guard
}

state night_guard {

  theRole = 5

  if !roles.includes(theRole) then going night_wolf

  announce(me(T("talkGuard")))

  if filter(players, (p, idx) => p.role == theRole && p.life).length then {

    used = 0

    names.forEach((name, index) => {
      if players[name].role == theRole then {
        later 3500 drrr.dm(name, T("action"))
      }
    })

    event dm (guard, cont) => {
      if guard in players then {
        if players[guard].role == theRole then {
          if players[guard].life then {
            if !used then {
              the = select(cont, names)
              if cont.startsWith("no") || cont.startsWith("ignore") then {
                used = 1
                protect = ""
                later 3500 going night_wolf
              } else if the.length then {
                if protect == the then
                  drrr.dm(user, T("dupProtect"))
                else if players[the].life then{
                  protect = the
                  later 3500 going night_wolf
                } else drrr.dm(user, T("deadMan"))
              } else drrr.dm(user, T("noSuchPeople"))
            } else drrr.dm(user, T("oneCmd"))
          } else drrr.dm(user, T("deadRole")(T("guard")))
        } else drrr.dm(user, T("notRole")(T("guard")))
      } else drrr.print(me(T("beQuiet")(guard)))
    }

    event [me, msg] (user, cont: "^/skip$") => {
      if user in players then {
        protect = ""
        going night_wolf
      }
    }

  } else later (getRandom(10, 30) * 1000) {
    protect = ""
    going night_wolf
  }
}

state night_wolf {
  announce(me(T("talkWerewolves")))

  theRole = 0
  killed = 0

  forEach(players, (p, index) => {
    if p.life && p.role == theRole then
    later (2000 * index + 1500) drrr.dm(p.name, T("askKill"))
  })

  event dm (wolf, cont) => {
    if wolf in players then {
      if players[wolf].role == theRole then {
        if players[wolf].life then {
          if !cont.startsWith("no") then {
            if !killed then {
              the = select(cont, names)
              if the then {
                victim = [the]
                players[the].diefor = "bite"
                killed = 1
                later 3500 going night_witch
              } else drrr.dm(wolf, T("noSuchPeople"))
            } else drrr.dm(wolf, T("oneKill"))
          } else {
            victim = []
            killed = 1
            later 3500 going night_witch
          }
        } else drrr.dm(user, T("deadRole")(T("werewolf")))
      } else drrr.dm(user, T("notRole")(T("werewolf")))
    } else drrr.print(me(T("beQuiet")(wolf)))
  }

  event [dm, msg] (user, cont: "^/skip$") => {
    if user in players then going night_witch
  }
}

state night_witch {

  theRole = 3

  if !roles.includes(theRole) then going night_end

  announce(me(T("talkWitch")))

  if filter(players, (p, idx) => p.role == theRole && p.life).length then {

    used = 0

    names.forEach((name, index) => {
      if players[name].role == theRole then {
        if poison then
          later 3500 drrr.dm(name, T("victims")(victim.join(", ")))
        else
          later 3500 drrr.dm(name, T("action"))
      }
    })

    event dm (witch, cont) => {
      if witch in players then {
        if players[witch].role == theRole then {
          if players[witch].life then {
            if !used then {
              the = select(cont, names)
              if cont.startsWith("no") then {
                used = 1
                later 3500 going night_end
              } else if cont.startsWith("ignore") then {
                used = 1
                later 3500 going night_end
              } else if cont.includes("save") then {
                if !medicine then {
                  drrr.dm(witch, T("noMedicine"))
                }
                else if victim.length && victim[0] == witch then
                  drrr.dm(witch, T("can'tSaveSelf"))
                else if victim.length && victim[0] == protect then {
                  used = 1
                  medicine = 0
                  players[victim[0]].diefor = "poison"
                } else {
                  victim = []
                  used = 1
                  medicine = 0
                  later 3500 going night_end
                }
              } else if cont.includes("poison") then {
                if the.length then {
                  if players[the].life then{
                    if poison then {
                      if !victim.includes(the)
                      then victim.push(the)
                      players[the].diefor = "poison"
                      used = 1
                      poison = 0
                      later 3500 going night_end
                    } else drrr.dm(witch, T("noPoison"))
                  } else drrr.dm(user, T("deadMan"))
                } else drrr.dm(user, T("noSuchPeople"))
              } else drrr.dm(user, T("unkCmd"))
            } else drrr.dm(user, T("oneCmd"))
          } else drrr.dm(user, T("deadRole")(T("witch")))
        } else drrr.dm(user, T("notRole")(T("witch")))
      } else drrr.print(me(T("beQuiet")(witch)))
    }

    event [me, msg] (user, cont: "^/skip$") => {
      if user in players then going night_end
    }

  } else later (getRandom(10, 30) * 1000) going night_end
}

// need pre-declared vars
// expo: Array of String
// dead: Array of String
state shooter_ask {
  forEach(players, (p, index) => {
    if dead.includes(p.name) && isShooter(p.role) then
    later (1500 * (index + 1)) drrr.dm(p.name, T("hunter15s"))
  })
  event [dm, msg, me] (hunter, cont) => {
    if hunter in players && dead.includes(hunter) then {
      if isShooter(players[hunter].role) then {
        expo.push(hunter)
        if players[hunter].role == rHunt
        then drrr.print(me(T("hunterShow")(hunter)))
        else if players[hunter].role == rWing
        then drrr.print(me(T("whiteWolfShow")(hunter)))
      }
    }
  }
}

state idiot_ask {
  forEach(players, (p, index) => {
    if p.life && p.role == rIdio then
    later 2000 drrr.dm(p.name, T("idiot15s"))
  })
  event [dm, msg, me] (idiot, cont) => {
    if idiot in players then {
      if players[idiot].role == rIdio then {
        drrr.print(me(T("idiotShow")(idiot)))
        players[idiot].life = true
        players[idiot].diefor = ""
        players[idiot].right = false
      }
    }
  }
}

// need pre-declared vars
// expo: Array of String
// dead: Array of String
state shooter_fire {
  hunter = Object.values(players).find(p => p.role == rHunt)
  drrr.print(me(T("fired")("@" + expo.join(", @"))))
  event [msg, me] (hunter, cont) => {
    if hunter in players then {
      if isShooter(players[hunter].role) && expo.includes(hunter) then {
        the = select(cont, names)
        if the then {
          if players[the].life then {
            players[the].life = false
            players[the].right = false
            players[the].diefor = "shoot"
            drrr.print(me(T("hit")(the)))
            setTimeout(() => setAlive(the, false), 1500)

            index = expo.indexOf(hunter);
            if index >= 0 then expo.splice(index, 1);

            if !expo.length then go_next()

          } else drrr.dm(user, T("deadMan"))
        } else drrr.dm(user, T("noSuchPeople"))
      }
    }
  }

  event [msg, me] (user, cont: "^/urge$") => {
    drrr.print(me(T("urgeShoot")("@" + expo.join(", @"))))
  }

  event [msg, me] (user, cont: "^/skip$") => {
    if user in players then go_next()
  }

}

state night_end {

  expo = []

  day = day + 1

  go_next = {
    drrr.print(me(T("survivors")(filter(players, p => p.life).map(p => p.name).join(", "))))
    if passJudge() < 0
    then later 3500 going day_discussion
    else later 3500 going game_over
  }

  scene(T("morning")(day))

  later 3000 {
    if victim.length then {

      if victim.includes(protect) then {
        if players[protect].diefor == "bite" then {
          players[protect].diefor = ""
          index = victim.indexOf(protect);
          if index >= 0 then victim.splice(index, 1);
        }
      }

      announce(me(T("died")(victim.map((x)=>"@" + x).join(", "))))

      later 2000 {

        exist_shooter = false
        victim.forEach((name, index) => {
          setTimeout(() => setAlive(name, false), index * 1500)
          if isShooter(players[name].role) && players[name].diefor != "poison"
          then exist_shooter = true
        })

        if exist_shooter then {
          dead = victim
          visit shooter_ask
        }

        later 5000 {
          drrr.print(me(T("rip30s")))
          later 30000 {
            victim.forEach((name) => {
              players[name].life = false
              players[name].right = false
            })

            if expo.length
            then {
              dead = victim
              visit shooter_fire
            }
            else go_next()
          }
        }
      }
    } else {
      announce(me(T("morningSafe")))
      later 3500 go_next()
    }
  }
}

state day_discussion {
  index = 0
  while (index < names.length) && (!players[names[index]].life) index++;
  announce(me(T("speaking")(names[index])))

  event [msg, me] (u, cont) => {
    if (names[index] == u && cont.includes("/expo")) then {
      the = select(cont, names)
      if the.length then {
        if players[the].life then{
          victim =[the, u]
          players[the].diefor = "bite"
          later 3500 going day_whitewolf
        } else drrr.print(me(T("deadMan")))
      } else drrr.print(me(T("noSuchPeople")))
    }
    else if (names[index] == u && cont.includes("over"))
      || ((names.includes(u) || u == user.name) && cont.startsWith("/skip")) then {
      index++ // += bug?
      while (index < names.length) && (!players[names[index]].life) index++;
      if index >= names.length
      then later 3500 going day_vote
      else announce(me(T("speaking")(names[index])))
    }
  }
}

state day_vote {

  announce(me(T("voting")))
  vote = {}

  vote_times++
  survivor = filter(players, (p, idx) => p.life)


  later 1500 drrr.print(T("voteNote")(survivor.map((u) => "@" + u.name).join("\n")))


  event [msg, me, dm] (user, cont: "^/vote\\s+\\S+|^/execute") => {
    cont = cont.replace("/vote", "").trim()
    if user in players then {
      if players[user].right then {
        if cont.startsWith("/execute")
        then later 1500 going day_count_vote
        else if vote.hasOwnProperty(user)
        then drrr.print(T("oneVote"))
        else {
          the = select(cont, names)
          if the then {
            if players[the].life then {
              vote[user] = the
              drrr.print(T("checkVote")(the))
              if Object.keys(vote).length == filter(players, p => p.right).length
              then later 3500 going day_count_vote
            } else drrr.print(T("voteDead")(the))
          }
          else if cont.startsWith("no") then {
            vote[user] = "no"
            drrr.print(T("abstain"))
            if Object.keys(vote).length == filter(players, p => p.right).length
            then later 3500 going day_count_vote
          } else drrr.print(T("noSuchPeople"))
        }
      } else drrr.print(T("noRight"))
    } else drrr.print(me(T("beQuiet")(user)))
  }
  event [msg, me, dm] (user, cont: "^/vote$", url, tc, req) => {
    if req.type == "dm"
    then drrr.dm(user, me(T("curVote")(Object.keys(vote).join(", "))))
    else drrr.print(me(T("curVote")(Object.keys(vote).join(", "))))
  }
  event [msg, me] (user, cont: "^/urge$") => {
    drrr.print(me(T("urgeVote")(survivor.filter(u => !(u.name in vote) && u.right).map((u) => "@" + u.name).join(", "))))
  }
}

state day_count_vote {
  drrr.print(Object.keys(vote).map(k => String(k) + " => " + String(vote[k])).join("\n"))
  setTimeout(() => going day_execute, 3000)
}

state day_whitewolf {

  expo = []

  go_next = {
    drrr.print(me(T("survivors")(filter(players, p => p.life).map(p => p.name).join(", "))))
    if passJudge() < 0
    then later 3500 going night
    else later 3500 going game_over
  }

  announce(me(T("died")(victim.map((x)=>"@" + x).join(", "))))

  later 2000 {

    exist_shooter = false
    victim.forEach((name, index) => {
      setTimeout(() => setAlive(name, false), index * 1500)
      if players[name].role == rHunt && players[name].diefor != "poison"
      then exist_shooter = true
    })

    if exist_shooter then {
      dead = victim
      visit shooter_ask
    }

    later 5000 {
      drrr.print(me(T("rip30s")))
      later 30000 {
        victim.forEach((name) => {
          players[name].life = false;
          players[name].right = false;
        })
        if expo.length
        then {
          dead = victim
          visit shooter_fire
        }
        else go_next()
      }
    }
  }
}

state day_execute {

  expo = []

  go_next = {
    drrr.print(me(T("survivors")(filter(players, p => p.life).map(p => p.name).join(", "))))
    if passJudge() < 0
    then later 3500 going night
    else later 3500 going game_over
  }

  louis = most(Object.values(vote).filter((x) => x != "no"))

  if louis.length > 1 then {
    if vote_times == 1 then {
      announce(me(T("reVote")(louis.map((x) => "@" + x).join(", "))))
      going day_vote
    }
    else {
      vote_times = 0
      announce(me(T("noExecute")(louis.map((x) => "@" + x).join(", "))))
      go_next()
    }
  }
  else {
    vote_times = 0
    announce(me(T("execute")(louis.map((x) => "@" + x).join(", "))))

    exist_shooter = false
    louis.forEach((name) => {
      if players[name].role == rHunt
      then exist_shooter = true
      if players[name].role == rIdio
      then visit idiot_ask
    })

    // may setTimeout for idiot
    if exist_shooter then {
      dead = louis
      visit shooter_ask
    }

    louis.forEach((name, index) => {
      players[name].life = false;
      players[name].right = false;
      players[name].diefor = "vote";
      setTimeout(() => setAlive(name, false), (index + 1) * 1500)
    })

    later ((louis.length + 2) * 1500) {
      drrr.print(me(T("rip30s")))
      later 30000 {
        if expo.length
        then {
          dead = louis
          visit shooter_fire
        }
        else go_next()
      }
    }
  }
}

state game_over {
  cur_role = map(players, (p, idx) => p.name + ":" + p.rname + " " + (if p.life then T("alive") else (T("dead") + " (" + p.diefor + ")"))).join("\n")
  result = passJudge()
  if result == 0
  then scene(me(T("werewolvesWin")))
  else if result == 1
  then scene(me(T("peopleWin")))
  else scene(me(T("draw")))
  later 2000 drrr.print(cur_role)
}

werewolf = (lang) => {

  if lang then {
    language = lang
    announcement = me(T("notBeg"))
    initRoleName()
  }

  event [msg, me, dm] (user, cont: "^/r$") => {
    if user in players then {
      wolves = filter(players, (p, index) => p.role == rWolf).map(p => p.name).join(", ")
      if players[user].role then drrr.dm(user, T("UrRole")(players[user].rname))
      else drrr.dm(user, T("UrWerewolf")(wolves))
    } else drrr.dm(user, T("notPlayer"))
  }

  event [msg, me, dm] (user, cont: "^/w$") => {
    if Object.keys(players).length
    then drrr.print(T("players")(map(players, (p, index) => String(index + 1) + ". " + p.name + " " + (if p.life then T("alive") else T("dead"))).join("\n")))
    else if names.length then
      drrr.print(T("players")(names.map((user, index) => String(index + 1) + ". " + user).join("\n")))
    else drrr.print(me(T("noPlayerBeg")))
  }

  event [msg, me, dm] (user, cont: "^/s$") => {
    drrr.print(announcement)
  }

  event [msg, me, dm] (user, cont: "^/help$") => {
    drrr.print(T("manual"))
  }

  event [msg, me] (user, cont: "^/game$") => going prepare

  event [msg, me] (user, cont: "^/next$") => later 1500 drrr.print("/me/skip")

  event join (user) => setPlayer(user, false)

  going prepare
}

console.log("need call werewolf(lang) to start it,\n\"zh\" and \"en\" are available now")

//   werewolf("zh");
//   werewolf("en");
"""

main :: Effect Unit
main = do
  compile tmpTest
  -- compile testMachine
  -- doing testMachine
  -- doing testLoop
  -- doing testAjax
  -- doing testRecursion
  -- doing testLift
  -- doing testGoing
  -- doing testVisit
  -- doing guessNumber
  -- doing wolf
  -- compile wolf
  -- doing ctx''
  -- doing "0 && print(2)"
  -- doing "print(2) || print(2)"
  -- compile objctx
  -- compile test

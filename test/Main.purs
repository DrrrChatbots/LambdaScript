module Test.Main where

import Prelude

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
for(i of [1,2,3,4]) print(i);
for(j in {tom: 1, allen: 2}) print(j);
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
test = "{x: { y : 2 }}"
-- test = """
-- state day {}
-- t = a => "hello"
-- state night {}
-- """
-- test = """{ x: "asdf" => 3 }"""

testMachine :: String
testMachine = """

k = a => a + 1
names = []
f = () => {
  if true then {
    console.log(__this__.env.value0.root.value0.root)
    //console.log(__this__.env.value0.root.value0.root.value0.root)
    console.log(k(3))
    console.log(names)
  }
  console.log(names)
}

console.log(names)
f()
console.log(names)
"""

main :: Effect Unit
main = do
  compile testMachine
  doing testMachine
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

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
victim = []
players = []
// 0 狼 1 平民 2 預言家 3 女巫 4 獵人
roleName = ["狼", "平民", "預言家", "女巫", "獵人"]
rolesMap = {
  5 : [0, 1, 1, 2, 3],
  6 : [0, 0, 1, 1, 2, 3],
  7 : [0, 0, 1, 1, 1, 2, 3]
//8: [0, 0, 1, 2, 3, 3, 3, 4],
}

roles = []
alive = []
vote = {}

select = (cont, users) => {
  the = ""
  for user of users
    if cont.includes(user)
    then the = user
  the
}

most = (arr) => {
  counts = arr.reduce((a, c) => {
    a[c] = (a[c] || 0) + 1
    a
  }, Object())
  maxCount = Math.max.apply(Object(), Object.values(counts))
  Object.keys(counts).filter(k => counts[k] === maxCount)
}

passJudge = () => {
  wolf = alive.filter((live, index) => live && roles[index] == 1)
  people = alive.filter((live, index)=> live && roles[index] != 1)
  if wolf.length && people.length
  then -1
  else if wolf.length
  then 0
  else 1
}

state prepare {
  roles = []
  alive = []
  event [msg, me] (user, cont: "^\\+1$") => {
    if players.includes(user) then
      drrr.print("/me" + user + " 已經加入了")
    else{
      players.push(user)
      drrr.print("/me" + user + " 加入遊戲")
    }
  }
  event [msg, me] (user, cont: "^-1$") => {
    if players.includes(user) then {
      players.splice(players.indexOf(user), 1);
      drrr.print("/me" + user + " 退出遊戲")
    }
    else drrr.print("/me" + user + " 不在遊戲中")
  }
  event [msg, me] (user, cont: "^/list$") => {
    if players.length then {
    	drrr.print("玩家：\n" + players.map((user, index) => String(index + 1) + ". " + user).join("\n"))
    } else drrr.print("/me 沒有玩家")
  }
  event [msg, me] (user, cont: "^/start$") => {
    if
      players.length in rolesMap
    then
      going prelude
    else
      drrr.print("/me需滿足 5 - 7 人, 目前 " + String(players.length) + "人")
  }
  drrr.print("/me +1 加入, -1 退出, /list 玩家列表, /start 開始遊戲")
}

state prelude {
  drrr.print("玩家：" + players.map((user, index) => String(index + 1) + ". " + user).join("\n"))

  roles = rolesMap[players.length]
  roles.sort(()=>Math.random() - 0.5)
  alive = roles.map((x) => true)

  players.forEach((user, index) => {
    if
      roles[index]
    then
      later index * 3500 drrr.dm(user, "你的身份是:" + roleName[roles[index]])
    else
      later index * 3500 drrr.dm("你是狼, 所有狼是：" +
        players.filter((user, index)=>roles[index] == 0).join(", "))
  })

  later players.length * 4000 going night
}

state night {
  drrr.print("/me天黑請閉眼")
  later 3500 going night_seer
}

state night_seer {
  drrr.print("/me 預言家請睜眼，你想知道關於誰的訊息（*私信* [人名]）...")
  event dm (seer, cont) => {
    if players.includes(seer) then {
      if roles[players.indexOf(seer)] == 2 then {
        the = select(cont, players)
        if the then {
          drrr.dm(seer,
            if
              roles[players.indexOf(user)] > 0
            then
              "人"
            else
              "狼"
          )
          later 3500 going night_wolf
        }
        else drrr.dm(seer, "沒這個人")
      }
    }
  }
}

state night_wolf {
  drrr.print("/me 狼人請睜眼，請採取行動（*私信* [no] or [人名]）...")
  event dm (wolf, cont) => {
    if players.includes(wolf) then {
      if roles[players.indexOf(wolf)] == 0 then {
        if cont.startsWith("no") then {
          victim = []
        }
        else {
          the = select(cont, players)
          if the then {
            victim = [user]
            later 3500 going night_witch
          }
          else drrr.dm(wolf, "沒這個人")
        }
      }
    }
  }
}

state night_witch {
  drrr.print("/me 女巫請睜眼，請採取行動（*私信* [no] or [救] or [毒 人名]）...")
  players.forEach((user, index) => {
    if roles[players.indexOf(user)] == 3 then {
      later 3500 drrr.dm(user, "受害者：" + victim.join(", "))
    }
  })
  event dm (witch, cont) => {
    if players.includes(witch) then {
      if roles[players.indexOf(witch)] == 3 then {
        if cont.startsWith("no") then {
          later 3500 going night_end
        }
        else if cont.includes("救") then {
          victim = []
          later 3500 going night_end
        }
        else if cont.includes("毒") then {
          for user of players {
            if cont.includes(user) then {
              victim.push(user)
              later 3500 going night_end
            }
          }
        }
      }
    }
  }
}

state night_end {
  if victim.length then {
    drrr.print("/me天亮了," + victim.join(", ") + "死了")
    players.forEach((user, index) => {
      if victim.includes(user) then alive[index] = false
    })
  }
  else drrr.print("/me天亮了 沒有人死")
  later 3500 {
    drrr.print("倖存者：" + players.filter((u, i) => alive[i]).join(", "))
    later 3500 going day_discussion
  }
}

state day_discussion {
  index = 0
  drrr.print("/me請" + players[index] + "開始發言 ([over] 結尾)")
  event [msg, me] (user, cont) => {
    if players.includes(user) then {
      if cont.includes("over") then
        index += 1
      if index > players.length then
        later 3500 going day_vote
      else
        drrr.print("/me請" + players[index] + "開始發言 ([over] 結尾)")
    }
  }
}

state day_vote {
  drrr.print("/me請開始投票 ([/vote] 看目前已投票玩家, *私信* [人名] 或是 [no] 棄票)")
  vote = {}
  event dm (user, cont) => {
    if players.includes(user) then {
      the = select(cont, players)
      if the then {
        vote[user] = the
        drrr.dm(user, "ok, 你投了 " + the)
        if Object.keys(vote).length == players.length then
          later 3500 going day_execute
      }
      else if cont.startsWith("no") then {
        vote[user] = "no"
        drrr.dm(user, "ok, 你棄票了")
        if Object.keys(vote).length == players.length
        then later 3500 going day_execute
      }
      else
        drrr.dm(user, "沒這個人")
    }
  }
  event [msg, me] (user, cont: "^/vote$") => {
    drrr.print("/me目前已投票：" + Object.keys(vote).join(", "))
  }
}

state day_execute {
  louis = most(Object.values(vote).filter((x) => x != "no"))
  drrr.print("/me最高得票人：" + String(louis))
  players.forEach((user, index) => {
    if louis.includes(user) then alive[index] = false
  })
  later 3500 {
    drrr.print("倖存者：" + players.filter((u, i) => alive[i]).join(", "))
    if passJudge() >= 0
    then
      later 3500 going night
    else
      later 3500 going game_over
  }
}

state game_over {
  if passJudge()
  then
    drrr.print("/me遊戲結束, 人類勝出")
  else
    drrr.print("/me遊戲結束, 狼人勝出")
}

going prepare
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
a.l = 3
print(a)
delete a.l
print(a.l)
print(aa)
"""

main = do
  -- doing testLoop
  -- doing testAjax
  -- doing testRecursion
  -- doing testLift
  -- doing testGoing
  -- doing testVisit
  -- doing guessNumber
  -- doing wolf
  -- compile wolf
  doing ctx
  -- doing "0 && print(2)"
  -- doing "print(2) || print(2)"
  -- doing "for(j in {tom: 1}) 1;"
  -- testAbs

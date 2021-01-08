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

ctx = """
a = {tom: 1,jon: 2}
//gnjdg = (guess, callback) => {
//}
"""

wolf = """
victim = []
players = []

// 0 狼 1 平民 2 預言家 3 女巫 4 獵人
roleName = ["狼", "平民", "預言家", "女巫", "獵人"]
rolesMap = Object()
rolesMap[5] = [0, 1, 1, 2, 3]
rolesMap[6] = [0, 0, 1, 1, 2, 3]
rolesMap[7] = [0, 0, 1, 1, 1, 2, 3]
//8: [0, 0, 1, 2, 3, 3, 3, 4],
// literal key, empty object

roles = []
alive = []
vote = Object()

select = (cont, users) => {
  the = ""
  for user of users
    if cont.includes(user)
      the = user
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
  //if wolf.length && people.length
  //  -1
  //else if wolf.length
  //  0
  //else
  //  1
}

state prepare {
  event [msg, me] (user, cont: "^+1$"){
    if players.includes(user)
      drrr.print("/me" + user + " 已經加入了")
    else{
      players.push(user)
      drrr.print(user + " 加入遊戲")
    }
  }
  event [msg, me] (user, cont: "^-1$"){
    if players.includes(user) {
      players.splice(players.indexOf(user), 1);
      drrr.print("/me" + user + " 退出遊戲")
    }
    else drrr.print(user + " 不在遊戲中")
  }
  event [msg, me] (user, cont: "^/list$"){
    drrr.print(players.map((user, index) => String(index) + ". " + user).join("\n"))
    drrr.print("玩家：")
  }
  event [msg, me] (user, cont: "^/start$"){
    if players.length in rolesMap
      going prelude
    else
      drrr.print("需滿足 5 - 7 人, 目前 " + String(players.length) + "人")
  }
  drrr.print("/me +1 加入, -1 退出, /list 玩家列表, /start 開始遊戲")
}

state prelude {
  drrr.print(players.map((user, index) => String(index) + ". " + user).join("\n"))
  drrr.print("玩家：")

  roles = rolesMap[players.length]
  roles.sort(()=>Math.random() - 0.5)
  alive = roles.map((x) => true)

  players.forEach((user, index) => {
    if roles[index]
      drrr.dm(user, "你的身份是:" + roleName[roles[index]])
    else
      drrr.dm("你是狼, 所有狼是：" +
        players.filter((user, index)=>roles[index] == 0).join(" "))
  })

  going night
}

state night {
  drrr.print("/me天黑請閉眼")
  later 3000 going night_seer
}

state night_seer {
  drrr.print("/me 預言家請睜眼，你想知道關於誰的訊息（人名）...")
  event dm (seer, cont){
    if players.includes(seer) {
      if roles[players.indexOf(seer)] == 2 {
        the = select(cont, players)
        if the {
          drrr.dm(seer,
            if roles[players.indexOf(user)] > 0
              "人"
            else
              "狼"
          )
          going night_wolf
        }
        else drrr.dm(seer, "沒這個人")
      }
    }
  }
}

state night_wolf {
  drrr.print("/me 狼人請睜眼，請採取行動（no or 人名）...")
  event dm (wolf, cont){
    if players.includes(wolf) {
      if roles[players.indexOf(wolf)] == 0 {
        if cont.startsWith("no") {
          victim = []
        }
        else {
          the = select(cont, players)
          if the {
            victim = [user]
            going night_witch
          }
          else drrr.dm(wolf, "沒這個人")
        }
      }
    }
  }
}

state night_witch {
  drrr.print("/me 女巫請睜眼，請採取行動（no or 救 + 人名 or 毒 + 人名）...")
  players.forEach((user, index) => {
    if roles[players.indexOf(user)] == 3 {
      drrr.dm(user, "受害者：" + String(victim))
    }
  })
  event dm (witch, cont){
    if players.includes(witch) {
      if roles[players.indexOf(witch)] == 3 {
        if cont.startsWith("no") {
          going night_end
        }
        else if cont.includes("救") {
          victim = []
          going night_end
        }
        else if cont.includes("毒") {
          for user of players {
            if cont.includes(user) {
              victim.push(user)
              going night_end
            }
          }
        }
      }
    }
  }
}

state night_end {
  if victim.length {
    drrr.print("/me天亮了," + String(victim) + "死了")
    players.forEach((user, index) => {
      if victim.includes(user) alive[index] = false
    })
  }
  else drrr.print("/me天亮了 沒有人死")
  drrr.print("倖存者：" + String(players.filter((u, i) => alive[i])))
  going day_discussion
}

state day_discussion {
  index = 0
  drrr.print("請" + players[index] + "開始發言 (over 結尾)")
  event [msg, me] (user, cont){
    if players.includes(user) {
      if cont.includes("over")
        index += 1
      if index > players.length
        going day_vote
      else
        drrr.print("請" + players[index] + "開始發言 (over 結尾)")
    }
  }
}

state day_vote {
  drrr.print("請開始投票 (/vote 看目前已投票玩家, 私信 人名 或是 no 棄票)")
  vote = Object()
  event dm (user, cont){
    if players.includes(user) {
      the = select(cont, players)
      if the {
        vote[user] = the
        drrr.dm(user, "ok, 你投了 " + the)
        if Object.keys(vote).length == players.length
          going day_execute
      }
      else if cont.startsWith("no") {
        vote[user] = "no"
        drrr.dm(user, "ok, 你棄票了")
        if Object.keys(vote).length == players.length
          going day_execute
      }
      else
        drrr.dm(user, "沒這個人")
    }
  }
  event [msg, me] (user, cont: "^/vote$"){
    drrr.print("目前已投票：" + String(Object.keys(vote)))
  }
}

state day_execute {
  louis = most(Object.values(vote).filter((x) => x != "no"))
  drrr.print("最高得票人：" + String(louis))
  players.forEach((user, index) => {
    if louis.includes(user) alive[index] = false
  })
  drrr.print("倖存者：" + String(players.filter((u, i) => alive[i])))
  if passJudge() >= 0
    going night
  else
    going game_over
}

state game_over {
  if passJudge()
    drrr.print("/me遊戲結束, 人類勝出")
  else
    drrr.print("/me遊戲結束, 狼人勝出")
}
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

-- doing = execute'
doing = execute'
main = do
  doing testLoop
  doing testAjax
  doing testRecursion
  doing testLift
  doing testGoing
  doing testVisit
  doing guessNumber
  doing ctx
  -- testing (parseObject parseExpr) ctx

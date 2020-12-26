
builtins = {
  'title': function(msg){
    sendTab({ fn: publish_message, args: { msg: msg} });
  },
  'descr': function(msg){
    sendTab({ fn: publish_message, args: { msg: msg} });
  },
  'print': function(msg){
    sendTab({ fn: publish_message, args: { msg: msg} });
  },
  'order': function(keyword, p1, p2){
    var idx = undefined, source = undefined;
    if(p1){ if(p1 in api) source = p1; else idx = p1; }
    if(p2){ if(p2 in api) source = p2; else idx = p2; }
    console.log(`play music[${source}][${idx}]: ${keyword}`);
    setTimeout(()=> play_search(
      get_music.bind(null, keyword, source),
      (msg) => sendTab({
        fn: publish_message,
        args: { msg: msg }
      }), idx
    ), 1000);
  }
}

builtins_test = {
  'title': function(msg){
    console.log(`title ${JSON.stringify(msg)}`);
  },
  'descr': function(msg){
    console.log(`descr ${JSON.stringify(msg)}`);
  },
  'print': function(msg){
    console.log(`print ${JSON.stringify(msg)}`);
  },
  'order': function(keyword, p1, p2){
    console.log(`order ${JSON.stringify(keyword)}`);
  }
}

var functions = builtins_test

for(key in functions){
  globalThis[key] = functions[key];
}

exports.invok = fn => syms => args => () => {
  //builtins_test[sym].apply(null, args.map((x)=>x['value0']))
  if(functions[fn])
    functions[fn].apply(null, args)
  else if(typeof(fn) == 'string'){
    sel = invokExternal(globalThis[fn])(syms)
    if(sel) sel.apply(null, args);
  }
  else{
    sel = invokExternal(fn)(syms)
    if(sel) sel.apply(fn, args)
  }
  console.log(`Value ${syms} ${JSON.stringify(args)}`);
}

invokExternal = namespace => syms => {
  var f = namespace;
  console.log(f);
  for(var key of syms){
    if(!f){
      console.log(`cannot find function ${syms}`)
      return undefined;
    }
    f = f[key];
  }
  return f;
}

exports.cur = ""
exports.events = {}

function padArray(array, length, fill){
  return length > array.length ?
    array.concat(Array(length - array.length).fill(fill)):
    array;
}

exports.listen = state => type => args => next => () => {
  //builtins[sym].apply(null, args.map((x)=>x['value0']))
  exports.events[state] = exports.events[state] || [];

  [user_regex, cont_regex] = padArray(args);

  exports.events[state].push([
    type, user_regex, cont_regex, next
  ])

  console.log(`Event ${type} ${JSON.stringify(args)}`);
}

exports.handle = function(){

}

function event_action(event, config, req){
  var rule = exports.events[""] || []

  if(exports.cur.length)
    rule = rule.concat(exports.events[exports.cur] || [])

  rules.map(([type, user_trip_regex, cont_regex, action])=> {
    if(((Array.isArray(type) && type.includes(event)) || type == event)
      && match_user(req.user, req.trip, user_trip_regex)
      && ((req.text === 'unknown' || req.text === undefined)
          || req.text.match(new RegExp(cont_regex)))){
        action([req.user, req.text]);
        //argfmt(arglist, req.user, req.text, req.url, (args)=>{
        //  return actions[action].apply(config, args);
        //});
    }
  });
}



drrr_builtins = {
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

botlang_builtins = {
  'title': function(msg){
    console.log(`title ${JSON.stringify(msg)}`);
  },
  'descr': function(msg){
    console.log(`descr ${JSON.stringify(msg)}`);
  },
  'print': function(...args){
    console.log.apply(null,
      args.map((e)=>e.valueOf ? e.valueOf() : e));
    //console.log(`print ${JSON.stringify(msg)}`);
  },
  'order': function(keyword, p1, p2){
    console.log(`order ${JSON.stringify(keyword)}`);
  },
  'new': function (func) {
    var res = {};
    if (func.prototype !== null) {
      res.__proto__ = func.prototype;
    }
    var ret = func.apply(res,
      Array.prototype.slice.call(arguments, 1));
    if ((typeof ret === "object"
      || typeof ret === "function")
      && ret !== null) {
      return ret;
    }
    return res;
  }
}

globalThis.drrr = {}
for(key in drrr_builtins){
  globalThis.drrr[key] = drrr_builtins[key];
}

for(key in botlang_builtins){
  globalThis[key] = botlang_builtins[key];
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

exports.clearAllEvent = () => {
  exports.cur = "";
  exports.events = {};
}

exports.unlisten = state => () => {
  exports.cur = state;
  exports.events[state] = [];
}

exports.listen = state => types => args => next => () => {
  //builtins[sym].apply(null, args.map((x)=>x['value0']))
  exports.events[state] = exports.events[state] || [];

  [user_regex, cont_regex] = padArray(args);

  exports.events[state].push([
    types, user_regex, cont_regex, next
  ])

  console.log(`Event ${types} ${JSON.stringify(args)}`);
}

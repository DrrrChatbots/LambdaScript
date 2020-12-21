exports.evalBin = toExpr => op => lval => rval => {
  [lval, rval] = [lval, rval].map((x)=>x['value0']);
  if(op == "/")
    return toExpr(lval / rval);
  else if(op == '*')
    return toExpr(lval * rval);
  else if(op == '+')
    return toExpr(lval + rval);
  else if(op == '-')
    return toExpr(lval - rval);
  else if(op == '>')
    return toExpr(lval > rval);
  else if(op == '>=')
    return toExpr(lval >= rval);
  else if(op == '<')
    return toExpr(lval < rval);
  else if(op == '<=')
    return toExpr(lval <= rval);
  else if(op == '==')
    return toExpr(lval == rval);
  else if(op == '!=')
    return toExpr(lval != rval);
  else if(op == '===')
    return toExpr(lval === rval);
  else if(op == '!==')
    return toExpr(lval !== rval);
  else if(op == '&&')
    return toExpr(lval && rval);
  else if(op == '||')
    return toExpr(lval || rval);
}

exports.evalUna = toExpr => op => val => {
  val = val['value0'];
  if(op == "!")
    return toExpr(!val);
  else if(op == '-')
    return toExpr(-val);
}

exports.evalRef = toExpr => val => idxs => {
  val = val['value0'];
  idxs = idxs.map((x)=>x['value0']);
  for(var key of idxs){
    if(!val) return toExpr(val);
    val = val[key];
  }
  return toExpr(val);
}

exports.toTerm =
  constructorName =>
  literal => {
    if(constructorName == "")
      return literal;
    else if(constructorName == "Array"){
      return (literal);
    }
    return (globalThis[constructorName](literal));
  }

exports.toExprFFI = constructors => val => {
  if(constructors[typeof(val)])
    return constructors[typeof(val)](val);
  else if(constructors[val.constructor.name.toLowerCase()])
    return constructors[val.constructor.name.toLowerCase()](val);
  else
    return constructors['undefined'](val);
}

exports.stringify_ = obj => {
  str = JSON.stringify(obj)
  if(obj === undefined)
    str = "undefined";
  else if(typeof obj == 'function')
    str = 'function' + (obj.name ? ' ' + obj.name : '');
  else if(str === undefined && obj.toString)
    str = obj.toString();
  else if(str === '{}' && obj.constructor
    && obj.constructor.name != 'Object')
    str = "[Object " + obj.constructor.name + "]"
  return str;
}

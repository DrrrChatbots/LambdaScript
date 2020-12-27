exports.toTerm =
  constructorName =>
  literal => {
    if(constructorName == "")
      return literal;
    else if(constructorName == "Array"){
      return Object(literal);
    }
    return Object(globalThis[constructorName](literal));
  }

exports.toExprFFI = constructors => val => {
  if(constructors[typeof(val)])
    return constructors[typeof(val)](val);
  else if(constructors[val.constructor.name.toLowerCase()])
    return constructors[val.constructor.name.toLowerCase()](val);
  else
    return constructors['undefined'](val);
}

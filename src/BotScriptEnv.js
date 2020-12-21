exports.updateTab = tab => sym => val => {
  tab[sym] = val;
  return true;
}

exports.updateTabRef = tab => sym => idxs => val => () => {
  idxs = idxs.map((x)=>x['value0']);
  if(idxs.length == 1)
    tab[sym][idxs[0]] = val;
  else{
    var ref = tab[sym];
    for(var i = 0; i < idxs.length - 1; i++)
      ref = ref[idxs[i]];
    ref[idxs[idxs.length - 1]] = val;
  }
  return true;
}

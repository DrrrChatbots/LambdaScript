exports.updateTab = sym => val => tab => {
  tab[sym] = val;
  return true;
}

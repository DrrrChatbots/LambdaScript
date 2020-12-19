exports.invok = sym => args => () => {
  args = args.map((x)=>x['value0'])
  console.log(`Invok ${sym} ${JSON.stringify(args)}`);
}

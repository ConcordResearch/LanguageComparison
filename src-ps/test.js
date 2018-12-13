// const map = new Map()
const map = {}

var start = Date.now()

for(var i = 0; i < 1000000; ++i) {
  // map.set(i, i * 10);
  map[i.toString()] = i * 10;
}

var end = Date.now()

console.log(end - start);
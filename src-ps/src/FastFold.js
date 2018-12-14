


exports.mutableArrayBind = function (arr) {
  return function (f) {
    var result = [];
    for (var i = 0, l = arr.length; i < l; i++) {
      Array.prototype.push.apply(result, f(arr[i]));
    }
    return result;
  };
};

// foreign import fastFoldr :: forall a b f. (a -> b -> b) -> b -> f a -> b
exports.fastFoldr = function (fn) { //(a -> MutableArray b -> MutableArray b)
  return function (b) { // Array b
    return function (fa) { //Array a
      var bb = b;
      for (var i = 0; i < fa.length; ++i) {
        bb = fn(fa[i])(bb);
      }
      return b;
    }
  } 
}

exports.unfreeze = function (arr) {
  return arr.slice();
}

exports.freeze = function (mutArr) {
  return mutArr.slice();
}

exports.push = function (a) {
  return function (mutArr) {
    mutArr.push(a);
    return mutArr;
  }
}

exports.now = function(){ return Date.now(); }

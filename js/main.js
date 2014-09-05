// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = new F(f);
}

function F(f) {
    this.f = f;
}

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f instanceof F) {
            return t.f = t.f.f();
        } else {
            return t.f;
        }
    } else {
        return t;
    }
}

/* Bounce
   Bonuce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        f = f.f();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
function imul(a, b) {
  // ignore high a * high a as the result will always be truncated
  var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
  var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
  var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
  return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['b']['i8'] = 'U'.charCodeAt(0);
    le['b']['i8'] = 'T'.charCodeAt(0);
    le['b']['i8'] = 'F'.charCodeAt(0);
    le['b']['i8'] = '-'.charCodeAt(0);
    le['b']['i8'] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;        
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=new T(function(){return B(unCStr("achievements"));}),_1=new T(function(){return [0,toJSStr(E(_0))];}),_2=new T(function(){return B(unCStr("lastFocus"));}),_3=new T(function(){return [0,toJSStr(E(_2))];}),_4=new T(function(){return B(unCStr("depend"));}),_5=new T(function(){return [0,toJSStr(E(_4))];}),_6=new T(function(){return B(unCStr("lps"));}),_7=new T(function(){return [0,toJSStr(E(_6))];}),_8=new T(function(){return B(unCStr("loves"));}),_9=new T(function(){return [0,toJSStr(E(_8))];}),_a=new T(function(){return B(unCStr("lpsCoeff"));}),_b=new T(function(){return [0,toJSStr(E(_a))];}),_c=new T(function(){return B(unCStr("dependCoeff"));}),_d=new T(function(){return [0,toJSStr(E(_c))];}),_e=new T(function(){return B(unCStr("maxLoves"));}),_f=new T(function(){return [0,toJSStr(E(_e))];}),_g=new T(function(){return B(unCStr("items"));}),_h=new T(function(){return [0,toJSStr(E(_g))];}),_i=function(_j){return [0,toJSStr(E(_j))];},_k=function(_l){return [1,new T(function(){return B(_i(_l));})];},_m=new T(function(){return [0,"value"];}),_n=true,_o=[2,_n],_p=new T(function(){return [0,"hasValue"];}),_q=[0,_p,_o],_r=false,_s=[2,_r],_t=[0,_p,_s],_u=[0],_v=[1,_t,_u],_w=[4,_v],_x=function(_y,_z){while(1){var _A=(function(_B,_C){var _D=E(_C);if(!_D[0]){_y=[1,[3,[1,[1,new T(function(){return [0,toJSStr(_D[2])];})],[1,new T(function(){var _E=E(_D[3]);return _E[0]==0?E(_w):[4,[1,_q,[1,[0,_m,new T(function(){return B(_k(_E[1]));})],_u]]];}),_u]]],new T(function(){return B(_x(_B,_D[5]));})];_z=_D[4];return null;}else{return E(_B);}})(_y,_z);if(_A!=null){return _A;}}},_F=function(_G){return [0,new T(function(){return [0,E(_G)[1]];})];},_H=function(_I,_J){while(1){var _K=(function(_L,_M){var _N=E(_M);switch(_N[0]){case 0:_I=new T(function(){return B(_H(_L,_N[4]));});_J=_N[3];return null;case 1:return [1,[3,[1,[0,[0,_N[1]]],[1,new T(function(){return B(_F(_N[2]));}),_u]]],_L];default:return E(_L);}})(_I,_J);if(_K!=null){return _K;}}},_O=function(_P,_Q){var _R=E(_P);return _R[0]==0?E(_Q):[1,_R[1],new T(function(){return B(_O(_R[2],_Q));})];},_S=function(_T){while(1){var _U=E(_T);if(!_U[0]){_T=[1,I_fromInt(_U[1])];continue;}else{return new F(function(){return I_toString(_U[1]);});}}},_V=function(_W,_X){return new F(function(){return _O(fromJSStr(B(_S(_W))),_X);});},_Y=function(_Z,_10){var _11=E(_Z);if(!_11[0]){var _12=_11[1],_13=E(_10);return _13[0]==0?_12<_13[1]:I_compareInt(_13[1],_12)>0;}else{var _14=_11[1],_15=E(_10);return _15[0]==0?I_compareInt(_14,_15[1])<0:I_compare(_14,_15[1])<0;}},_16=[0,41],_17=[0,40],_18=[0,0],_19=function(_1a,_1b,_1c){return _1a<=6?B(_V(_1b,_1c)):!B(_Y(_1b,_18))?B(_V(_1b,_1c)):[1,_17,new T(function(){return B(_O(fromJSStr(B(_S(_1b))),[1,_16,_1c]));})];},_1d=function(_1e,_1f,_1g,_1h,_1i,_1j,_1k,_1l,_1m){return [1,[0,_9,[0,_1e]],[1,[0,_7,[0,_1f]],[1,[0,_5,[0,_1g]],[1,[0,_3,[1,new T(function(){return [0,toJSStr(B(_19(0,_1h,_u)))];})]],[1,[0,_1,[3,new T(function(){return B(_x(_u,_1i));})]],[1,[0,_h,[3,new T(function(){var _1n=E(_1j);if(!_1n[0]){var _1o=_1n[3],_1p=_1n[4],_1q=_1n[2]>=0?B(_H(new T(function(){return B(_H(_u,_1p));}),_1o)):B(_H(new T(function(){return B(_H(_u,_1o));}),_1p));}else{var _1q=B(_H(_u,_1n));}return _1q;})]],[1,[0,_f,[0,_1k]],[1,[0,_d,[0,_1l]],[1,[0,_b,[0,_1m]],_u]]]]]]]]];},_1r=function(_1s){var _1t=E(_1s);return [4,B(_1d(_1t[1],_1t[2],_1t[3],_1t[4],_1t[6],_1t[7],_1t[8],_1t[9],_1t[10]))];},_1u=function(_1v,_1w){var _1x=E(_1w);return _1x[0]==0?[0]:[1,new T(function(){return B(A(_1v,[_1x[1]]));}),new T(function(){return B(_1u(_1v,_1x[2]));})];},_1y=function(_1z){return [3,new T(function(){return B(_1u(_1r,_1z));})];},_1A=new T(function(){return B(unCStr("Tried to deserialize long string to a Char"));}),_1B=[0,_1A],_1C=new T(function(){return B(unCStr("Tried to deserialize a non-string to a Char"));}),_1D=[0,_1C],_1E=function(_1F){var _1G=E(_1F);if(_1G[0]==1){var _1H=fromJSStr(E(_1G[1])[1]);return _1H[0]==0?E(_1B):E(_1H[2])[0]==0?[1,_1H[1]]:E(_1B);}else{return E(_1D);}},_1I=new T(function(){return B(unCStr("Tried to deserialize a non-JSString to a JSString"));}),_1J=[0,_1I],_1K=function(_1L){return new F(function(){return fromJSStr(E(_1L)[1]);});},_1M=function(_1N){var _1O=E(_1N);return _1O[0]==1?[1,new T(function(){return B(_1K(_1O[1]));})]:E(_1J);},_1P=function(_1Q){return [1,new T(function(){return [0,toJSStr([1,_1Q,_u])];})];},_1R=[0,_1P,_k,_1E,_1M],_1S=function(_1T){return E(E(_1T)[2]);},_1U=function(_1V,_1W){return [3,new T(function(){return B(_1u(new T(function(){return B(_1S(_1V));}),_1W));})];},_1X=[1,_u],_1Y=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_1Z=[0,_1Y],_20=function(_21){return E(E(_21)[4]);},_22=function(_23,_24){var _25=E(_24);if(_25[0]==3){var _26=function(_27){var _28=E(_27);if(!_28[0]){return E(_1X);}else{var _29=B(A(new T(function(){return B(_20(_23));}),[_28[1]]));if(!_29[0]){return [0,_29[1]];}else{var _2a=B(_26(_28[2]));return _2a[0]==0?[0,_2a[1]]:[1,[1,_29[1],_2a[1]]];}}};return new F(function(){return _26(_25[1]);});}else{return E(_1Z);}},_2b=function(_2c){return [0,new T(function(){return B(_1S(_2c));}),function(_2d){return new F(function(){return _1U(_2c,_2d);});},new T(function(){return B(_20(_2c));}),function(_2d){return new F(function(){return _22(_2c,_2d);});}];},_2e=new T(function(){return B(_2b(_1R));}),_2f=function(_2g){return E(E(_2g)[1]);},_2h=function(_2i,_2j){var _2k=E(_2j);return _2k[0]==0?E(_w):[4,[1,_q,[1,[0,_m,new T(function(){return B(A(_2f,[_2i,_2k[1]]));})],_u]]];},_2l=function(_2m,_2n){return [3,new T(function(){return B(_1u(function(_2d){return new F(function(){return _2h(_2m,_2d);});},_2n));})];},_2o=function(_2p,_2q){var _2r=strEq(E(_2p)[1],E(_2q)[1]),_2s=_2r;return E(_2s)==0?true:false;},_2t=function(_2u,_2v){var _2w=strEq(E(_2u)[1],E(_2v)[1]),_2x=_2w;return E(_2x)==0?false:true;},_2y=[0,_2t,_2o],_2z=[0],_2A=[1,_2z],_2B=new T(function(){return B(unCStr("Tried to deserialize a non-Bool to a Bool"));}),_2C=[0,_2B],_2D=new T(function(){return B(unCStr("Key not found"));}),_2E=[0,_2D],_2F=new T(function(){return B(unCStr("Tried to do lookup on non-object!"));}),_2G=[0,_2F],_2H=function(_2I){return E(E(_2I)[1]);},_2J=function(_2K,_2L,_2M){while(1){var _2N=E(_2M);if(!_2N[0]){return [0];}else{var _2O=E(_2N[1]);if(!B(A(_2H,[_2K,_2L,_2O[1]]))){_2M=_2N[2];continue;}else{return [1,_2O[2]];}}}},_2P=function(_2Q){return E(E(_2Q)[3]);},_2R=function(_2S,_2T){var _2U=E(_2T);if(_2U[0]==4){var _2V=_2U[1],_2W=B(_2J(_2y,_p,_2V));if(!_2W[0]){return E(_2E);}else{var _2X=E(_2W[1]);if(_2X[0]==2){if(!E(_2X[1])){return E(_2A);}else{var _2Y=B(_2J(_2y,_m,_2V));if(!_2Y[0]){return E(_2E);}else{var _2Z=B(A(_2P,[_2S,_2Y[1]]));return _2Z[0]==0?[0,_2Z[1]]:[1,[1,_2Z[1]]];}}}else{return E(_2C);}}}else{return E(_2G);}},_30=[1,_u],_31=[0,_1Y],_32=function(_33,_34){var _35=E(_34);if(_35[0]==3){var _36=function(_37){var _38=E(_37);if(!_38[0]){return E(_30);}else{var _39=B(_2R(_33,_38[1]));if(!_39[0]){return [0,_39[1]];}else{var _3a=B(_36(_38[2]));return _3a[0]==0?[0,_3a[1]]:[1,[1,_39[1],_3a[1]]];}}};return new F(function(){return _36(_35[1]);});}else{return E(_31);}},_3b=function(_3c){return [0,function(_2d){return new F(function(){return _2h(_3c,_2d);});},function(_2d){return new F(function(){return _2l(_3c,_2d);});},function(_2d){return new F(function(){return _2R(_3c,_2d);});},function(_2d){return new F(function(){return _32(_3c,_2d);});}];},_3d=new T(function(){return B(_3b(_2e));}),_3e=[1,_u],_3f=[0,_1Y],_3g=new T(function(){return B(unCStr("Tried to deserialize a non-array into a pair!"));}),_3h=[0,_3g],_3i=function(_3j,_3k,_3l){var _3m=E(_3l);if(_3m[0]==3){var _3n=E(_3m[1]);if(!_3n[0]){return E(_3h);}else{var _3o=E(_3n[2]);if(!_3o[0]){return E(_3h);}else{if(!E(_3o[2])[0]){var _3p=B(A(_2P,[_3j,_3n[1]]));if(!_3p[0]){return [0,_3p[1]];}else{var _3q=B(A(_2P,[_3k,_3o[1]]));return _3q[0]==0?[0,_3q[1]]:[1,[0,_3p[1],_3q[1]]];}}else{return E(_3h);}}}}else{return E(_3h);}},_3r=function(_3s,_3t,_3u){var _3v=E(_3u);if(_3v[0]==3){var _3w=function(_3x){var _3y=E(_3x);if(!_3y[0]){return E(_3e);}else{var _3z=B(_3i(_3s,_3t,_3y[1]));if(!_3z[0]){return [0,_3z[1]];}else{var _3A=B(_3w(_3y[2]));return _3A[0]==0?[0,_3A[1]]:[1,[1,_3z[1],_3A[1]]];}}};return new F(function(){return _3w(_3v[1]);});}else{return E(_3f);}},_3B=function(_3C){return [3,new T(function(){return B(_1u(_F,_3C));})];},_3D=new T(function(){return B(unCStr("The given Number can\'t be represented as an Int"));}),_3E=[0,_3D],_3F=new T(function(){return B(unCStr("Tried to deserialize a non-Number to an Int"));}),_3G=[0,_3F],_3H=function(_3I){var _3J=E(_3I);if(!_3J[0]){var _3K=E(_3J[1])[1],_3L=_3K&4294967295;return _3L!=_3K?E(_3E):[1,[0,_3L]];}else{return E(_3G);}},_3M=[0,_1Y],_3N=[1,_u],_3O=[0,_3D],_3P=[0,_3F],_3Q=function(_3R){var _3S=E(_3R);if(!_3S[0]){return E(_3N);}else{var _3T=E(_3S[1]);if(!_3T[0]){var _3U=E(_3T[1])[1],_3V=_3U&4294967295;if(_3V!=_3U){return E(_3O);}else{var _3W=B(_3Q(_3S[2]));return _3W[0]==0?[0,_3W[1]]:[1,[1,[0,_3V],_3W[1]]];}}else{return E(_3P);}}},_3X=function(_3Y){var _3Z=E(_3Y);return _3Z[0]==3?B(_3Q(_3Z[1])):E(_3M);},_40=[0,_F,_3B,_3H,_3X],_41=function(_42,_43){while(1){var _44=E(_42);if(!_44[0]){return E(_43)[0]==0?1:0;}else{var _45=E(_43);if(!_45[0]){return 2;}else{var _46=E(_44[1])[1],_47=E(_45[1])[1];if(_46!=_47){return _46>_47?2:0;}else{_42=_44[2];_43=_45[2];continue;}}}}},_48=[1],_49=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_4a=function(_4b){return new F(function(){return err(_49);});},_4c=new T(function(){return B(_4a(_));}),_4d=function(_4e,_4f,_4g,_4h){var _4i=E(_4g);if(!_4i[0]){var _4j=_4i[1],_4k=E(_4h);if(!_4k[0]){var _4l=_4k[1],_4m=_4k[2],_4n=_4k[3];if(_4l<=(imul(3,_4j)|0)){return [0,(1+_4j|0)+_4l|0,E(E(_4e)),_4f,E(_4i),E(_4k)];}else{var _4o=E(_4k[4]);if(!_4o[0]){var _4p=_4o[1],_4q=_4o[2],_4r=_4o[3],_4s=_4o[4],_4t=E(_4k[5]);if(!_4t[0]){var _4u=_4t[1];if(_4p>=(imul(2,_4u)|0)){var _4v=function(_4w){var _4x=E(_4e),_4y=E(_4o[5]);return _4y[0]==0?[0,(1+_4j|0)+_4l|0,E(_4q),_4r,E([0,(1+_4j|0)+_4w|0,E(_4x),_4f,E(_4i),E(_4s)]),E([0,(1+_4u|0)+_4y[1]|0,E(_4m),_4n,E(_4y),E(_4t)])]:[0,(1+_4j|0)+_4l|0,E(_4q),_4r,E([0,(1+_4j|0)+_4w|0,E(_4x),_4f,E(_4i),E(_4s)]),E([0,1+_4u|0,E(_4m),_4n,E(_48),E(_4t)])];},_4z=E(_4s);return _4z[0]==0?B(_4v(_4z[1])):B(_4v(0));}else{return [0,(1+_4j|0)+_4l|0,E(_4m),_4n,E([0,(1+_4j|0)+_4p|0,E(E(_4e)),_4f,E(_4i),E(_4o)]),E(_4t)];}}else{return E(_4c);}}else{return E(_4c);}}}else{return [0,1+_4j|0,E(E(_4e)),_4f,E(_4i),E(_48)];}}else{var _4A=E(_4h);if(!_4A[0]){var _4B=_4A[1],_4C=_4A[2],_4D=_4A[3],_4E=_4A[5],_4F=E(_4A[4]);if(!_4F[0]){var _4G=_4F[1],_4H=_4F[2],_4I=_4F[3],_4J=_4F[4],_4K=E(_4E);if(!_4K[0]){var _4L=_4K[1];if(_4G>=(imul(2,_4L)|0)){var _4M=function(_4N){var _4O=E(_4e),_4P=E(_4F[5]);return _4P[0]==0?[0,1+_4B|0,E(_4H),_4I,E([0,1+_4N|0,E(_4O),_4f,E(_48),E(_4J)]),E([0,(1+_4L|0)+_4P[1]|0,E(_4C),_4D,E(_4P),E(_4K)])]:[0,1+_4B|0,E(_4H),_4I,E([0,1+_4N|0,E(_4O),_4f,E(_48),E(_4J)]),E([0,1+_4L|0,E(_4C),_4D,E(_48),E(_4K)])];},_4Q=E(_4J);return _4Q[0]==0?B(_4M(_4Q[1])):B(_4M(0));}else{return [0,1+_4B|0,E(_4C),_4D,E([0,1+_4G|0,E(E(_4e)),_4f,E(_48),E(_4F)]),E(_4K)];}}else{return [0,3,E(_4H),_4I,E([0,1,E(E(_4e)),_4f,E(_48),E(_48)]),E([0,1,E(_4C),_4D,E(_48),E(_48)])];}}else{var _4R=E(_4E);return _4R[0]==0?[0,3,E(_4C),_4D,E([0,1,E(E(_4e)),_4f,E(_48),E(_48)]),E(_4R)]:[0,2,E(E(_4e)),_4f,E(_48),E(_4A)];}}else{return [0,1,E(E(_4e)),_4f,E(_48),E(_48)];}}},_4S=function(_4T,_4U){return [0,1,E(E(_4T)),_4U,E(_48),E(_48)];},_4V=function(_4W,_4X,_4Y){var _4Z=E(_4Y);if(!_4Z[0]){return new F(function(){return _4d(_4Z[2],_4Z[3],_4Z[4],B(_4V(_4W,_4X,_4Z[5])));});}else{return new F(function(){return _4S(_4W,_4X);});}},_50=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_51=function(_52){return new F(function(){return err(_50);});},_53=new T(function(){return B(_51(_));}),_54=function(_55,_56,_57,_58){var _59=E(_58);if(!_59[0]){var _5a=_59[1],_5b=E(_57);if(!_5b[0]){var _5c=_5b[1],_5d=_5b[2],_5e=_5b[3];if(_5c<=(imul(3,_5a)|0)){return [0,(1+_5c|0)+_5a|0,E(E(_55)),_56,E(_5b),E(_59)];}else{var _5f=E(_5b[4]);if(!_5f[0]){var _5g=_5f[1],_5h=E(_5b[5]);if(!_5h[0]){var _5i=_5h[1],_5j=_5h[2],_5k=_5h[3],_5l=_5h[4];if(_5i>=(imul(2,_5g)|0)){var _5m=function(_5n){var _5o=E(_5h[5]);return _5o[0]==0?[0,(1+_5c|0)+_5a|0,E(_5j),_5k,E([0,(1+_5g|0)+_5n|0,E(_5d),_5e,E(_5f),E(_5l)]),E([0,(1+_5a|0)+_5o[1]|0,E(E(_55)),_56,E(_5o),E(_59)])]:[0,(1+_5c|0)+_5a|0,E(_5j),_5k,E([0,(1+_5g|0)+_5n|0,E(_5d),_5e,E(_5f),E(_5l)]),E([0,1+_5a|0,E(E(_55)),_56,E(_48),E(_59)])];},_5p=E(_5l);return _5p[0]==0?B(_5m(_5p[1])):B(_5m(0));}else{return [0,(1+_5c|0)+_5a|0,E(_5d),_5e,E(_5f),E([0,(1+_5a|0)+_5i|0,E(E(_55)),_56,E(_5h),E(_59)])];}}else{return E(_53);}}else{return E(_53);}}}else{return [0,1+_5a|0,E(E(_55)),_56,E(_48),E(_59)];}}else{var _5q=E(_57);if(!_5q[0]){var _5r=_5q[1],_5s=_5q[2],_5t=_5q[3],_5u=_5q[5],_5v=E(_5q[4]);if(!_5v[0]){var _5w=_5v[1],_5x=E(_5u);if(!_5x[0]){var _5y=_5x[1],_5z=_5x[2],_5A=_5x[3],_5B=_5x[4];if(_5y>=(imul(2,_5w)|0)){var _5C=function(_5D){var _5E=E(_5x[5]);return _5E[0]==0?[0,1+_5r|0,E(_5z),_5A,E([0,(1+_5w|0)+_5D|0,E(_5s),_5t,E(_5v),E(_5B)]),E([0,1+_5E[1]|0,E(E(_55)),_56,E(_5E),E(_48)])]:[0,1+_5r|0,E(_5z),_5A,E([0,(1+_5w|0)+_5D|0,E(_5s),_5t,E(_5v),E(_5B)]),E([0,1,E(E(_55)),_56,E(_48),E(_48)])];},_5F=E(_5B);return _5F[0]==0?B(_5C(_5F[1])):B(_5C(0));}else{return [0,1+_5r|0,E(_5s),_5t,E(_5v),E([0,1+_5y|0,E(E(_55)),_56,E(_5x),E(_48)])];}}else{return [0,3,E(_5s),_5t,E(_5v),E([0,1,E(E(_55)),_56,E(_48),E(_48)])];}}else{var _5G=E(_5u);return _5G[0]==0?[0,3,E(_5G[2]),_5G[3],E([0,1,E(_5s),_5t,E(_48),E(_48)]),E([0,1,E(E(_55)),_56,E(_48),E(_48)])]:[0,2,E(E(_55)),_56,E(_5q),E(_48)];}}else{return [0,1,E(E(_55)),_56,E(_48),E(_48)];}}},_5H=function(_5I,_5J,_5K){var _5L=E(_5K);if(!_5L[0]){return new F(function(){return _54(_5L[2],_5L[3],B(_5H(_5I,_5J,_5L[4])),_5L[5]);});}else{return new F(function(){return _4S(_5I,_5J);});}},_5M=function(_5N,_5O,_5P,_5Q,_5R,_5S,_5T){return new F(function(){return _54(_5Q,_5R,B(_5H(_5N,_5O,_5S)),_5T);});},_5U=function(_5V,_5W,_5X,_5Y,_5Z,_60,_61,_62){var _63=E(_5X);if(!_63[0]){var _64=_63[1],_65=_63[2],_66=_63[3],_67=_63[4],_68=_63[5];if((imul(3,_64)|0)>=_5Y){if((imul(3,_5Y)|0)>=_64){return [0,(_64+_5Y|0)+1|0,E(E(_5V)),_5W,E(_63),E([0,_5Y,E(_5Z),_60,E(_61),E(_62)])];}else{return new F(function(){return _4d(_65,_66,_67,B(_5U(_5V,_5W,_68,_5Y,_5Z,_60,_61,_62)));});}}else{return new F(function(){return _54(_5Z,_60,B(_69(_5V,_5W,_64,_65,_66,_67,_68,_61)),_62);});}}else{return new F(function(){return _5M(_5V,_5W,_5Y,_5Z,_60,_61,_62);});}},_69=function(_6a,_6b,_6c,_6d,_6e,_6f,_6g,_6h){var _6i=E(_6h);if(!_6i[0]){var _6j=_6i[1],_6k=_6i[2],_6l=_6i[3],_6m=_6i[4],_6n=_6i[5];if((imul(3,_6c)|0)>=_6j){if((imul(3,_6j)|0)>=_6c){return [0,(_6c+_6j|0)+1|0,E(E(_6a)),_6b,E([0,_6c,E(_6d),_6e,E(_6f),E(_6g)]),E(_6i)];}else{return new F(function(){return _4d(_6d,_6e,_6f,B(_5U(_6a,_6b,_6g,_6j,_6k,_6l,_6m,_6n)));});}}else{return new F(function(){return _54(_6k,_6l,B(_69(_6a,_6b,_6c,_6d,_6e,_6f,_6g,_6m)),_6n);});}}else{return new F(function(){return _4V(_6a,_6b,[0,_6c,E(_6d),_6e,E(_6f),E(_6g)]);});}},_6o=function(_6p,_6q,_6r,_6s){var _6t=E(_6r);if(!_6t[0]){var _6u=_6t[1],_6v=_6t[2],_6w=_6t[3],_6x=_6t[4],_6y=_6t[5],_6z=E(_6s);if(!_6z[0]){var _6A=_6z[1],_6B=_6z[2],_6C=_6z[3],_6D=_6z[4],_6E=_6z[5];if((imul(3,_6u)|0)>=_6A){if((imul(3,_6A)|0)>=_6u){return [0,(_6u+_6A|0)+1|0,E(E(_6p)),_6q,E(_6t),E(_6z)];}else{return new F(function(){return _4d(_6v,_6w,_6x,B(_5U(_6p,_6q,_6y,_6A,_6B,_6C,_6D,_6E)));});}}else{return new F(function(){return _54(_6B,_6C,B(_69(_6p,_6q,_6u,_6v,_6w,_6x,_6y,_6D)),_6E);});}}else{return new F(function(){return _4V(_6p,_6q,_6t);});}}else{return new F(function(){return _5H(_6p,_6q,_6s);});}},_6F=function(_6G,_6H,_6I,_6J){var _6K=E(_6G);if(_6K==1){var _6L=E(_6J);return _6L[0]==0?[0,new T(function(){return [0,1,E(E(_6H)),_6I,E(_48),E(_48)];}),_u,_u]:B(_41(_6H,E(_6L[1])[1]))==0?[0,new T(function(){return [0,1,E(E(_6H)),_6I,E(_48),E(_48)];}),_6L,_u]:[0,new T(function(){return [0,1,E(E(_6H)),_6I,E(_48),E(_48)];}),_u,_6L];}else{var _6M=B(_6F(_6K>>1,_6H,_6I,_6J)),_6N=_6M[1],_6O=_6M[3],_6P=E(_6M[2]);if(!_6P[0]){return [0,_6N,_u,_6O];}else{var _6Q=E(_6P[1]),_6R=_6Q[1],_6S=_6Q[2],_6T=E(_6P[2]);if(!_6T[0]){return [0,new T(function(){return B(_4V(_6R,_6S,_6N));}),_u,_6O];}else{var _6U=E(_6T[1]),_6V=_6U[1];if(!B(_41(_6R,_6V))){var _6W=B(_6F(_6K>>1,_6V,_6U[2],_6T[2]));return [0,new T(function(){return B(_6o(_6R,_6S,_6N,_6W[1]));}),_6W[2],_6W[3]];}else{return [0,_6N,_u,_6P];}}}}},_6X=function(_6Y,_6Z,_70){var _71=E(_6Y),_72=E(_70);if(!_72[0]){var _73=_72[2],_74=_72[3],_75=_72[4],_76=_72[5];switch(B(_41(_71,_73))){case 0:return new F(function(){return _54(_73,_74,B(_6X(_71,_6Z,_75)),_76);});break;case 1:return [0,_72[1],E(_71),_6Z,E(_75),E(_76)];default:return new F(function(){return _4d(_73,_74,_75,B(_6X(_71,_6Z,_76)));});}}else{return [0,1,E(_71),_6Z,E(_48),E(_48)];}},_77=function(_78,_79){while(1){var _7a=E(_79);if(!_7a[0]){return E(_78);}else{var _7b=E(_7a[1]),_7c=B(_6X(_7b[1],_7b[2],_78));_79=_7a[2];_78=_7c;continue;}}},_7d=function(_7e,_7f,_7g,_7h){return new F(function(){return _77(B(_6X(_7f,_7g,_7e)),_7h);});},_7i=function(_7j,_7k,_7l){var _7m=E(_7k);return new F(function(){return _77(B(_6X(_7m[1],_7m[2],_7j)),_7l);});},_7n=function(_7o,_7p,_7q){while(1){var _7r=E(_7q);if(!_7r[0]){return E(_7p);}else{var _7s=E(_7r[1]),_7t=_7s[1],_7u=_7s[2],_7v=E(_7r[2]);if(!_7v[0]){return new F(function(){return _4V(_7t,_7u,_7p);});}else{var _7w=E(_7v[1]),_7x=_7w[1];if(!B(_41(_7t,_7x))){var _7y=B(_6F(_7o,_7x,_7w[2],_7v[2])),_7z=_7y[1],_7A=E(_7y[3]);if(!_7A[0]){var _7B=_7o<<1,_7C=B(_6o(_7t,_7u,_7p,_7z));_7q=_7y[2];_7o=_7B;_7p=_7C;continue;}else{return new F(function(){return _7i(B(_6o(_7t,_7u,_7p,_7z)),_7A[1],_7A[2]);});}}else{return new F(function(){return _7d(_7p,_7t,_7u,_7v);});}}}}},_7D=function(_7E,_7F,_7G,_7H,_7I){var _7J=E(_7I);if(!_7J[0]){return new F(function(){return _4V(_7G,_7H,_7F);});}else{var _7K=E(_7J[1]),_7L=_7K[1];if(!B(_41(_7G,_7L))){var _7M=B(_6F(_7E,_7L,_7K[2],_7J[2])),_7N=_7M[1],_7O=E(_7M[3]);if(!_7O[0]){return new F(function(){return _7n(_7E<<1,B(_6o(_7G,_7H,_7F,_7N)),_7M[2]);});}else{return new F(function(){return _7i(B(_6o(_7G,_7H,_7F,_7N)),_7O[1],_7O[2]);});}}else{return new F(function(){return _7d(_7F,_7G,_7H,_7J);});}}},_7P=function(_7Q){var _7R=E(_7Q);if(!_7R[0]){return [1];}else{var _7S=E(_7R[1]),_7T=_7S[1],_7U=_7S[2],_7V=E(_7R[2]);if(!_7V[0]){return [0,1,E(E(_7T)),_7U,E(_48),E(_48)];}else{var _7W=_7V[2],_7X=E(_7V[1]),_7Y=_7X[1],_7Z=_7X[2];if(!B(_41(_7T,_7Y))){return new F(function(){return _7D(1,[0,1,E(E(_7T)),_7U,E(_48),E(_48)],_7Y,_7Z,_7W);});}else{return new F(function(){return _7d([0,1,E(E(_7T)),_7U,E(_48),E(_48)],_7Y,_7Z,_7W);});}}}},_80=[2],_81=function(_82,_83,_84){var _85=E(_84);switch(_85[0]){case 0:var _86=_85[1],_87=_85[2],_88=_85[3],_89=_85[4],_8a=_87>>>0;if(((_82>>>0&((_8a-1>>>0^4294967295)>>>0^_8a)>>>0)>>>0&4294967295)==_86){return (_82>>>0&_8a)>>>0==0?[0,_86,_87,E(B(_81(_82,_83,_88))),E(_89)]:[0,_86,_87,E(_88),E(B(_81(_82,_83,_89)))];}else{var _8b=(_82>>>0^_86>>>0)>>>0,_8c=(_8b|_8b>>>1)>>>0,_8d=(_8c|_8c>>>2)>>>0,_8e=(_8d|_8d>>>4)>>>0,_8f=(_8e|_8e>>>8)>>>0,_8g=(_8f|_8f>>>16)>>>0,_8h=(_8g^_8g>>>1)>>>0&4294967295,_8i=_8h>>>0;return (_82>>>0&_8i)>>>0==0?[0,(_82>>>0&((_8i-1>>>0^4294967295)>>>0^_8i)>>>0)>>>0&4294967295,_8h,E([1,_82,_83]),E(_85)]:[0,(_82>>>0&((_8i-1>>>0^4294967295)>>>0^_8i)>>>0)>>>0&4294967295,_8h,E(_85),E([1,_82,_83])];}break;case 1:var _8j=_85[1];if(_82!=_8j){var _8k=(_82>>>0^_8j>>>0)>>>0,_8l=(_8k|_8k>>>1)>>>0,_8m=(_8l|_8l>>>2)>>>0,_8n=(_8m|_8m>>>4)>>>0,_8o=(_8n|_8n>>>8)>>>0,_8p=(_8o|_8o>>>16)>>>0,_8q=(_8p^_8p>>>1)>>>0&4294967295,_8r=_8q>>>0;return (_82>>>0&_8r)>>>0==0?[0,(_82>>>0&((_8r-1>>>0^4294967295)>>>0^_8r)>>>0)>>>0&4294967295,_8q,E([1,_82,_83]),E(_85)]:[0,(_82>>>0&((_8r-1>>>0^4294967295)>>>0^_8r)>>>0)>>>0&4294967295,_8q,E(_85),E([1,_82,_83])];}else{return [1,_82,_83];}break;default:return [1,_82,_83];}},_8s=function(_8t,_8u){while(1){var _8v=E(_8u);if(!_8v[0]){return E(_8t);}else{var _8w=E(_8v[1]),_8x=B(_81(E(_8w[1])[1],_8w[2],_8t));_8u=_8v[2];_8t=_8x;continue;}}},_8y=new T(function(){return B(unCStr("Control.Exception.Base"));}),_8z=new T(function(){return B(unCStr("base"));}),_8A=new T(function(){return B(unCStr("PatternMatchFail"));}),_8B=new T(function(){var _8C=hs_wordToWord64(18445595),_8D=_8C,_8E=hs_wordToWord64(52003073),_8F=_8E;return [0,_8D,_8F,[0,_8D,_8F,_8z,_8y,_8A],_u];}),_8G=function(_8H){return E(_8B);},_8I=function(_8J){return E(E(_8J)[1]);},_8K=function(_8L,_8M,_8N){var _8O=B(A(_8L,[_])),_8P=B(A(_8M,[_])),_8Q=hs_eqWord64(_8O[1],_8P[1]),_8R=_8Q;if(!E(_8R)){return [0];}else{var _8S=hs_eqWord64(_8O[2],_8P[2]),_8T=_8S;return E(_8T)==0?[0]:[1,_8N];}},_8U=function(_8V){var _8W=E(_8V);return new F(function(){return _8K(B(_8I(_8W[1])),_8G,_8W[2]);});},_8X=function(_8Y){return E(E(_8Y)[1]);},_8Z=function(_90,_91){return new F(function(){return _O(E(_90)[1],_91);});},_92=[0,44],_93=[0,93],_94=[0,91],_95=function(_96,_97,_98){var _99=E(_97);return _99[0]==0?B(unAppCStr("[]",_98)):[1,_94,new T(function(){return B(A(_96,[_99[1],new T(function(){var _9a=function(_9b){var _9c=E(_9b);return _9c[0]==0?E([1,_93,_98]):[1,_92,new T(function(){return B(A(_96,[_9c[1],new T(function(){return B(_9a(_9c[2]));})]));})];};return B(_9a(_99[2]));})]));})];},_9d=function(_9e,_9f){return new F(function(){return _95(_8Z,_9e,_9f);});},_9g=function(_9h,_9i,_9j){return new F(function(){return _O(E(_9i)[1],_9j);});},_9k=[0,_9g,_8X,_9d],_9l=new T(function(){return [0,_8G,_9k,_9m,_8U];}),_9m=function(_9n){return [0,_9l,_9n];},_9o=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_9p=function(_9q,_9r){return new F(function(){return die(new T(function(){return B(A(_9r,[_9q]));}));});},_9s=function(_9t,_9u){var _9v=E(_9u);if(!_9v[0]){return [0,_u,_u];}else{var _9w=_9v[1];if(!B(A(_9t,[_9w]))){return [0,_u,_9v];}else{var _9x=new T(function(){var _9y=B(_9s(_9t,_9v[2]));return [0,_9y[1],_9y[2]];});return [0,[1,_9w,new T(function(){return E(E(_9x)[1]);})],new T(function(){return E(E(_9x)[2]);})];}}},_9z=[0,32],_9A=[0,10],_9B=[1,_9A,_u],_9C=function(_9D){return E(E(_9D)[1])==124?false:true;},_9E=function(_9F,_9G){var _9H=B(_9s(_9C,B(unCStr(_9F)))),_9I=_9H[1],_9J=function(_9K,_9L){return new F(function(){return _O(_9K,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_O(_9G,new T(function(){return B(_O(_9L,_9B));})));})));}));});},_9M=E(_9H[2]);if(!_9M[0]){return new F(function(){return _9J(_9I,_u);});}else{return E(E(_9M[1])[1])==124?B(_9J(_9I,[1,_9z,_9M[2]])):B(_9J(_9I,_u));}},_9N=function(_9O){return new F(function(){return _9p([0,new T(function(){return B(_9E(_9O,_9o));})],_9m);});},_9P=new T(function(){return B(_9N("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_9Q=function(_9R,_9S){while(1){var _9T=(function(_9U,_9V){var _9W=E(_9U);switch(_9W[0]){case 0:var _9X=E(_9V);if(!_9X[0]){return [0];}else{_9R=B(A(_9W[1],[_9X[1]]));_9S=_9X[2];return null;}break;case 1:var _9Y=B(A(_9W[1],[_9V])),_9Z=_9V;_9R=_9Y;_9S=_9Z;return null;case 2:return [0];case 3:return [1,[0,_9W[1],_9V],new T(function(){return B(_9Q(_9W[2],_9V));})];default:return E(_9W[1]);}})(_9R,_9S);if(_9T!=null){return _9T;}}},_a0=function(_a1,_a2){var _a3=function(_a4){var _a5=E(_a2);if(_a5[0]==3){return [3,_a5[1],new T(function(){return B(_a0(_a1,_a5[2]));})];}else{var _a6=E(_a1);if(_a6[0]==2){return E(_a5);}else{var _a7=E(_a5);if(_a7[0]==2){return E(_a6);}else{var _a8=function(_a9){var _aa=E(_a7);if(_aa[0]==4){return [1,function(_ab){return [4,new T(function(){return B(_O(B(_9Q(_a6,_ab)),_aa[1]));})];}];}else{var _ac=E(_a6);if(_ac[0]==1){var _ad=_ac[1],_ae=E(_aa);return _ae[0]==0?[1,function(_af){return new F(function(){return _a0(B(A(_ad,[_af])),_ae);});}]:[1,function(_ag){return new F(function(){return _a0(B(A(_ad,[_ag])),new T(function(){return B(A(_ae[1],[_ag]));}));});}];}else{var _ah=E(_aa);return _ah[0]==0?E(_9P):[1,function(_ai){return new F(function(){return _a0(_ac,new T(function(){return B(A(_ah[1],[_ai]));}));});}];}}},_aj=E(_a6);switch(_aj[0]){case 1:var _ak=E(_a7);if(_ak[0]==4){return [1,function(_al){return [4,new T(function(){return B(_O(B(_9Q(B(A(_aj[1],[_al])),_al)),_ak[1]));})];}];}else{return new F(function(){return _a8(_);});}break;case 4:var _am=_aj[1],_an=E(_a7);switch(_an[0]){case 0:return [1,function(_ao){return [4,new T(function(){return B(_O(_am,new T(function(){return B(_9Q(_an,_ao));})));})];}];case 1:return [1,function(_ap){return [4,new T(function(){return B(_O(_am,new T(function(){return B(_9Q(B(A(_an[1],[_ap])),_ap));})));})];}];default:return [4,new T(function(){return B(_O(_am,_an[1]));})];}break;default:return new F(function(){return _a8(_);});}}}}},_aq=E(_a1);switch(_aq[0]){case 0:var _ar=E(_a2);if(!_ar[0]){return [0,function(_as){return new F(function(){return _a0(B(A(_aq[1],[_as])),new T(function(){return B(A(_ar[1],[_as]));}));});}];}else{return new F(function(){return _a3(_);});}break;case 3:return [3,_aq[1],new T(function(){return B(_a0(_aq[2],_a2));})];default:return new F(function(){return _a3(_);});}},_at=[0,41],_au=[1,_at,_u],_av=[0,40],_aw=[1,_av,_u],_ax=function(_ay,_az){while(1){var _aA=E(_ay);if(!_aA[0]){return E(_az)[0]==0?true:false;}else{var _aB=E(_az);if(!_aB[0]){return false;}else{if(E(_aA[1])[1]!=E(_aB[1])[1]){return false;}else{_ay=_aA[2];_az=_aB[2];continue;}}}}},_aC=function(_aD,_aE){return E(_aD)[1]!=E(_aE)[1];},_aF=function(_aG,_aH){return E(_aG)[1]==E(_aH)[1];},_aI=[0,_aF,_aC],_aJ=function(_aK,_aL){while(1){var _aM=E(_aK);if(!_aM[0]){return E(_aL)[0]==0?true:false;}else{var _aN=E(_aL);if(!_aN[0]){return false;}else{if(E(_aM[1])[1]!=E(_aN[1])[1]){return false;}else{_aK=_aM[2];_aL=_aN[2];continue;}}}}},_aO=function(_aP,_aQ){return !B(_aJ(_aP,_aQ))?true:false;},_aR=[0,_aJ,_aO],_aS=function(_aT,_aU){var _aV=E(_aT);switch(_aV[0]){case 0:return [0,function(_aW){return new F(function(){return _aS(B(A(_aV[1],[_aW])),_aU);});}];case 1:return [1,function(_aX){return new F(function(){return _aS(B(A(_aV[1],[_aX])),_aU);});}];case 2:return [2];case 3:return new F(function(){return _a0(B(A(_aU,[_aV[1]])),new T(function(){return B(_aS(_aV[2],_aU));}));});break;default:var _aY=function(_aZ){var _b0=E(_aZ);if(!_b0[0]){return [0];}else{var _b1=E(_b0[1]);return new F(function(){return _O(B(_9Q(B(A(_aU,[_b1[1]])),_b1[2])),new T(function(){return B(_aY(_b0[2]));}));});}},_b2=B(_aY(_aV[1]));return _b2[0]==0?[2]:[4,_b2];}},_b3=[2],_b4=function(_b5){return [3,_b5,_b3];},_b6=0,_b7=function(_b8,_b9){var _ba=E(_b8);if(!_ba){return new F(function(){return A(_b9,[_b6]);});}else{return [0,function(_bb){return E(new T(function(){return B(_b7(_ba-1|0,_b9));}));}];}},_bc=function(_bd,_be,_bf){return function(_bg){return new F(function(){return A(function(_bh,_bi,_bj){while(1){var _bk=(function(_bl,_bm,_bn){var _bo=E(_bl);switch(_bo[0]){case 0:var _bp=E(_bm);if(!_bp[0]){return E(_be);}else{_bh=B(A(_bo[1],[_bp[1]]));_bi=_bp[2];var _bq=_bn+1|0;_bj=_bq;return null;}break;case 1:var _br=B(A(_bo[1],[_bm])),_bs=_bm,_bq=_bn;_bh=_br;_bi=_bs;_bj=_bq;return null;case 2:return E(_be);case 3:return function(_bt){return new F(function(){return _b7(_bn,function(_bu){return E(new T(function(){return B(_aS(_bo,_bt));}));});});};default:return function(_bv){return new F(function(){return _aS(_bo,_bv);});};}})(_bh,_bi,_bj);if(_bk!=null){return _bk;}}},[new T(function(){return B(A(_bd,[_b4]));}),_bg,0,_bf]);});};},_bw=function(_bx){return new F(function(){return A(_bx,[_u]);});},_by=function(_bz,_bA){var _bB=function(_bC){var _bD=E(_bC);if(!_bD[0]){return E(_bw);}else{var _bE=_bD[1];return !B(A(_bz,[_bE]))?E(_bw):function(_bF){return [0,function(_bG){return E(new T(function(){return B(A(new T(function(){return B(_bB(_bD[2]));}),[function(_bH){return new F(function(){return A(_bF,[[1,_bE,_bH]]);});}]));}));}];};}};return function(_bI){return new F(function(){return A(_bB,[_bI,_bA]);});};},_bJ=[6],_bK=function(_bL){return E(_bL);},_bM=new T(function(){return B(unCStr("valDig: Bad base"));}),_bN=new T(function(){return B(err(_bM));}),_bO=function(_bP,_bQ){var _bR=function(_bS,_bT){var _bU=E(_bS);if(!_bU[0]){return function(_bV){return new F(function(){return A(_bV,[new T(function(){return B(A(_bT,[_u]));})]);});};}else{var _bW=E(_bU[1])[1],_bX=function(_bY){return function(_bZ){return [0,function(_c0){return E(new T(function(){return B(A(new T(function(){return B(_bR(_bU[2],function(_c1){return new F(function(){return A(_bT,[[1,_bY,_c1]]);});}));}),[_bZ]));}));}];};};switch(E(E(_bP)[1])){case 8:if(48>_bW){return function(_c2){return new F(function(){return A(_c2,[new T(function(){return B(A(_bT,[_u]));})]);});};}else{if(_bW>55){return function(_c3){return new F(function(){return A(_c3,[new T(function(){return B(A(_bT,[_u]));})]);});};}else{return new F(function(){return _bX([0,_bW-48|0]);});}}break;case 10:if(48>_bW){return function(_c4){return new F(function(){return A(_c4,[new T(function(){return B(A(_bT,[_u]));})]);});};}else{if(_bW>57){return function(_c5){return new F(function(){return A(_c5,[new T(function(){return B(A(_bT,[_u]));})]);});};}else{return new F(function(){return _bX([0,_bW-48|0]);});}}break;case 16:if(48>_bW){if(97>_bW){if(65>_bW){return function(_c6){return new F(function(){return A(_c6,[new T(function(){return B(A(_bT,[_u]));})]);});};}else{if(_bW>70){return function(_c7){return new F(function(){return A(_c7,[new T(function(){return B(A(_bT,[_u]));})]);});};}else{return new F(function(){return _bX([0,(_bW-65|0)+10|0]);});}}}else{if(_bW>102){if(65>_bW){return function(_c8){return new F(function(){return A(_c8,[new T(function(){return B(A(_bT,[_u]));})]);});};}else{if(_bW>70){return function(_c9){return new F(function(){return A(_c9,[new T(function(){return B(A(_bT,[_u]));})]);});};}else{return new F(function(){return _bX([0,(_bW-65|0)+10|0]);});}}}else{return new F(function(){return _bX([0,(_bW-97|0)+10|0]);});}}}else{if(_bW>57){if(97>_bW){if(65>_bW){return function(_ca){return new F(function(){return A(_ca,[new T(function(){return B(A(_bT,[_u]));})]);});};}else{if(_bW>70){return function(_cb){return new F(function(){return A(_cb,[new T(function(){return B(A(_bT,[_u]));})]);});};}else{return new F(function(){return _bX([0,(_bW-65|0)+10|0]);});}}}else{if(_bW>102){if(65>_bW){return function(_cc){return new F(function(){return A(_cc,[new T(function(){return B(A(_bT,[_u]));})]);});};}else{if(_bW>70){return function(_cd){return new F(function(){return A(_cd,[new T(function(){return B(A(_bT,[_u]));})]);});};}else{return new F(function(){return _bX([0,(_bW-65|0)+10|0]);});}}}else{return new F(function(){return _bX([0,(_bW-97|0)+10|0]);});}}}else{return new F(function(){return _bX([0,_bW-48|0]);});}}break;default:return E(_bN);}}};return function(_ce){return new F(function(){return A(_bR,[_ce,_bK,function(_cf){var _cg=E(_cf);return _cg[0]==0?[2]:B(A(_bQ,[_cg]));}]);});};},_ch=[0,10],_ci=[0,1],_cj=[0,2147483647],_ck=function(_cl,_cm){while(1){var _cn=E(_cl);if(!_cn[0]){var _co=_cn[1],_cp=E(_cm);if(!_cp[0]){var _cq=_cp[1],_cr=addC(_co,_cq);if(!E(_cr[2])){return [0,_cr[1]];}else{_cl=[1,I_fromInt(_co)];_cm=[1,I_fromInt(_cq)];continue;}}else{_cl=[1,I_fromInt(_co)];_cm=_cp;continue;}}else{var _cs=E(_cm);if(!_cs[0]){_cl=_cn;_cm=[1,I_fromInt(_cs[1])];continue;}else{return [1,I_add(_cn[1],_cs[1])];}}}},_ct=new T(function(){return B(_ck(_cj,_ci));}),_cu=function(_cv){var _cw=E(_cv);if(!_cw[0]){var _cx=E(_cw[1]);return _cx==(-2147483648)?E(_ct):[0, -_cx];}else{return [1,I_negate(_cw[1])];}},_cy=[0,10],_cz=[0,0],_cA=function(_cB){return [0,_cB];},_cC=function(_cD,_cE){while(1){var _cF=E(_cD);if(!_cF[0]){var _cG=_cF[1],_cH=E(_cE);if(!_cH[0]){var _cI=_cH[1];if(!(imul(_cG,_cI)|0)){return [0,imul(_cG,_cI)|0];}else{_cD=[1,I_fromInt(_cG)];_cE=[1,I_fromInt(_cI)];continue;}}else{_cD=[1,I_fromInt(_cG)];_cE=_cH;continue;}}else{var _cJ=E(_cE);if(!_cJ[0]){_cD=_cF;_cE=[1,I_fromInt(_cJ[1])];continue;}else{return [1,I_mul(_cF[1],_cJ[1])];}}}},_cK=function(_cL,_cM,_cN){while(1){var _cO=E(_cN);if(!_cO[0]){return E(_cM);}else{var _cP=B(_ck(B(_cC(_cM,_cL)),B(_cA(E(_cO[1])[1]))));_cN=_cO[2];_cM=_cP;continue;}}},_cQ=function(_cR){var _cS=new T(function(){return B(_a0(B(_a0([0,function(_cT){return E(E(_cT)[1])==45?[1,B(_bO(_ch,function(_cU){return new F(function(){return A(_cR,[[1,new T(function(){return B(_cu(B(_cK(_cy,_cz,_cU))));})]]);});}))]:[2];}],[0,function(_cV){return E(E(_cV)[1])==43?[1,B(_bO(_ch,function(_cW){return new F(function(){return A(_cR,[[1,new T(function(){return B(_cK(_cy,_cz,_cW));})]]);});}))]:[2];}])),new T(function(){return [1,B(_bO(_ch,function(_cX){return new F(function(){return A(_cR,[[1,new T(function(){return B(_cK(_cy,_cz,_cX));})]]);});}))];})));});return new F(function(){return _a0([0,function(_cY){return E(E(_cY)[1])==101?E(_cS):[2];}],[0,function(_cZ){return E(E(_cZ)[1])==69?E(_cS):[2];}]);});},_d0=function(_d1){return new F(function(){return A(_d1,[_2z]);});},_d2=function(_d3){return new F(function(){return A(_d3,[_2z]);});},_d4=function(_d5){return function(_d6){return E(E(_d6)[1])==46?[1,B(_bO(_ch,function(_d7){return new F(function(){return A(_d5,[[1,_d7]]);});}))]:[2];};},_d8=function(_d9){return [0,B(_d4(_d9))];},_da=function(_db){return new F(function(){return _bO(_ch,function(_dc){return [1,B(_bc(_d8,_d0,function(_dd){return [1,B(_bc(_cQ,_d2,function(_de){return new F(function(){return A(_db,[[5,[1,_dc,_dd,_de]]]);});}))];}))];});});},_df=function(_dg){return [1,B(_da(_dg))];},_dh=function(_di,_dj,_dk){while(1){var _dl=E(_dk);if(!_dl[0]){return false;}else{if(!B(A(_2H,[_di,_dj,_dl[1]]))){_dk=_dl[2];continue;}else{return true;}}}},_dm=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_dn=function(_do){return new F(function(){return _dh(_aI,_do,_dm);});},_dp=[0,8],_dq=[0,16],_dr=function(_ds){var _dt=function(_du){return new F(function(){return A(_ds,[[5,[0,_dp,_du]]]);});},_dv=function(_dw){return new F(function(){return A(_ds,[[5,[0,_dq,_dw]]]);});};return function(_dx){return E(E(_dx)[1])==48?E([0,function(_dy){switch(E(E(_dy)[1])){case 79:return [1,B(_bO(_dp,_dt))];case 88:return [1,B(_bO(_dq,_dv))];case 111:return [1,B(_bO(_dp,_dt))];case 120:return [1,B(_bO(_dq,_dv))];default:return [2];}}]):[2];};},_dz=function(_dA){return [0,B(_dr(_dA))];},_dB=function(_dC){var _dD=new T(function(){return B(A(_dC,[_dp]));}),_dE=new T(function(){return B(A(_dC,[_dq]));});return function(_dF){switch(E(E(_dF)[1])){case 79:return E(_dD);case 88:return E(_dE);case 111:return E(_dD);case 120:return E(_dE);default:return [2];}};},_dG=function(_dH){return [0,B(_dB(_dH))];},_dI=[0,92],_dJ=function(_dK){return new F(function(){return A(_dK,[_ch]);});},_dL=function(_dM,_dN){var _dO=jsShowI(_dM),_dP=_dO;return new F(function(){return _O(fromJSStr(_dP),_dN);});},_dQ=function(_dR,_dS,_dT){if(_dS>=0){return new F(function(){return _dL(_dS,_dT);});}else{return _dR<=6?B(_dL(_dS,_dT)):[1,_17,new T(function(){var _dU=jsShowI(_dS),_dV=_dU;return B(_O(fromJSStr(_dV),[1,_16,_dT]));})];}},_dW=function(_dX){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_dQ(9,_dX,_u));}))));});},_dY=function(_dZ){var _e0=E(_dZ);return _e0[0]==0?E(_e0[1]):I_toInt(_e0[1]);},_e1=function(_e2,_e3){var _e4=E(_e2);if(!_e4[0]){var _e5=_e4[1],_e6=E(_e3);return _e6[0]==0?_e5<=_e6[1]:I_compareInt(_e6[1],_e5)>=0;}else{var _e7=_e4[1],_e8=E(_e3);return _e8[0]==0?I_compareInt(_e7,_e8[1])<=0:I_compare(_e7,_e8[1])<=0;}},_e9=function(_ea){return [2];},_eb=function(_ec){var _ed=E(_ec);if(!_ed[0]){return E(_e9);}else{var _ee=_ed[1],_ef=E(_ed[2]);return _ef[0]==0?E(_ee):function(_eg){return new F(function(){return _a0(B(A(_ee,[_eg])),new T(function(){return B(A(new T(function(){return B(_eb(_ef));}),[_eg]));}));});};}},_eh=function(_ei){return [2];},_ej=function(_ek,_el){var _em=function(_en,_eo){var _ep=E(_en);if(!_ep[0]){return function(_eq){return new F(function(){return A(_eq,[_ek]);});};}else{var _er=E(_eo);return _er[0]==0?E(_eh):E(_ep[1])[1]!=E(_er[1])[1]?E(_eh):function(_es){return [0,function(_et){return E(new T(function(){return B(A(new T(function(){return B(_em(_ep[2],_er[2]));}),[_es]));}));}];};}};return function(_eu){return new F(function(){return A(_em,[_ek,_eu,_el]);});};},_ev=new T(function(){return B(unCStr("SOH"));}),_ew=[0,1],_ex=function(_ey){return [1,B(_ej(_ev,function(_ez){return E(new T(function(){return B(A(_ey,[_ew]));}));}))];},_eA=new T(function(){return B(unCStr("SO"));}),_eB=[0,14],_eC=function(_eD){return [1,B(_ej(_eA,function(_eE){return E(new T(function(){return B(A(_eD,[_eB]));}));}))];},_eF=function(_eG){return [1,B(_bc(_ex,_eC,_eG))];},_eH=new T(function(){return B(unCStr("NUL"));}),_eI=[0,0],_eJ=function(_eK){return [1,B(_ej(_eH,function(_eL){return E(new T(function(){return B(A(_eK,[_eI]));}));}))];},_eM=new T(function(){return B(unCStr("STX"));}),_eN=[0,2],_eO=function(_eP){return [1,B(_ej(_eM,function(_eQ){return E(new T(function(){return B(A(_eP,[_eN]));}));}))];},_eR=new T(function(){return B(unCStr("ETX"));}),_eS=[0,3],_eT=function(_eU){return [1,B(_ej(_eR,function(_eV){return E(new T(function(){return B(A(_eU,[_eS]));}));}))];},_eW=new T(function(){return B(unCStr("EOT"));}),_eX=[0,4],_eY=function(_eZ){return [1,B(_ej(_eW,function(_f0){return E(new T(function(){return B(A(_eZ,[_eX]));}));}))];},_f1=new T(function(){return B(unCStr("ENQ"));}),_f2=[0,5],_f3=function(_f4){return [1,B(_ej(_f1,function(_f5){return E(new T(function(){return B(A(_f4,[_f2]));}));}))];},_f6=new T(function(){return B(unCStr("ACK"));}),_f7=[0,6],_f8=function(_f9){return [1,B(_ej(_f6,function(_fa){return E(new T(function(){return B(A(_f9,[_f7]));}));}))];},_fb=new T(function(){return B(unCStr("BEL"));}),_fc=[0,7],_fd=function(_fe){return [1,B(_ej(_fb,function(_ff){return E(new T(function(){return B(A(_fe,[_fc]));}));}))];},_fg=new T(function(){return B(unCStr("BS"));}),_fh=[0,8],_fi=function(_fj){return [1,B(_ej(_fg,function(_fk){return E(new T(function(){return B(A(_fj,[_fh]));}));}))];},_fl=new T(function(){return B(unCStr("HT"));}),_fm=[0,9],_fn=function(_fo){return [1,B(_ej(_fl,function(_fp){return E(new T(function(){return B(A(_fo,[_fm]));}));}))];},_fq=new T(function(){return B(unCStr("LF"));}),_fr=[0,10],_fs=function(_ft){return [1,B(_ej(_fq,function(_fu){return E(new T(function(){return B(A(_ft,[_fr]));}));}))];},_fv=new T(function(){return B(unCStr("VT"));}),_fw=[0,11],_fx=function(_fy){return [1,B(_ej(_fv,function(_fz){return E(new T(function(){return B(A(_fy,[_fw]));}));}))];},_fA=new T(function(){return B(unCStr("FF"));}),_fB=[0,12],_fC=function(_fD){return [1,B(_ej(_fA,function(_fE){return E(new T(function(){return B(A(_fD,[_fB]));}));}))];},_fF=new T(function(){return B(unCStr("CR"));}),_fG=[0,13],_fH=function(_fI){return [1,B(_ej(_fF,function(_fJ){return E(new T(function(){return B(A(_fI,[_fG]));}));}))];},_fK=new T(function(){return B(unCStr("SI"));}),_fL=[0,15],_fM=function(_fN){return [1,B(_ej(_fK,function(_fO){return E(new T(function(){return B(A(_fN,[_fL]));}));}))];},_fP=new T(function(){return B(unCStr("DLE"));}),_fQ=[0,16],_fR=function(_fS){return [1,B(_ej(_fP,function(_fT){return E(new T(function(){return B(A(_fS,[_fQ]));}));}))];},_fU=new T(function(){return B(unCStr("DC1"));}),_fV=[0,17],_fW=function(_fX){return [1,B(_ej(_fU,function(_fY){return E(new T(function(){return B(A(_fX,[_fV]));}));}))];},_fZ=new T(function(){return B(unCStr("DC2"));}),_g0=[0,18],_g1=function(_g2){return [1,B(_ej(_fZ,function(_g3){return E(new T(function(){return B(A(_g2,[_g0]));}));}))];},_g4=new T(function(){return B(unCStr("DC3"));}),_g5=[0,19],_g6=function(_g7){return [1,B(_ej(_g4,function(_g8){return E(new T(function(){return B(A(_g7,[_g5]));}));}))];},_g9=new T(function(){return B(unCStr("DC4"));}),_ga=[0,20],_gb=function(_gc){return [1,B(_ej(_g9,function(_gd){return E(new T(function(){return B(A(_gc,[_ga]));}));}))];},_ge=new T(function(){return B(unCStr("NAK"));}),_gf=[0,21],_gg=function(_gh){return [1,B(_ej(_ge,function(_gi){return E(new T(function(){return B(A(_gh,[_gf]));}));}))];},_gj=new T(function(){return B(unCStr("SYN"));}),_gk=[0,22],_gl=function(_gm){return [1,B(_ej(_gj,function(_gn){return E(new T(function(){return B(A(_gm,[_gk]));}));}))];},_go=new T(function(){return B(unCStr("ETB"));}),_gp=[0,23],_gq=function(_gr){return [1,B(_ej(_go,function(_gs){return E(new T(function(){return B(A(_gr,[_gp]));}));}))];},_gt=new T(function(){return B(unCStr("CAN"));}),_gu=[0,24],_gv=function(_gw){return [1,B(_ej(_gt,function(_gx){return E(new T(function(){return B(A(_gw,[_gu]));}));}))];},_gy=new T(function(){return B(unCStr("EM"));}),_gz=[0,25],_gA=function(_gB){return [1,B(_ej(_gy,function(_gC){return E(new T(function(){return B(A(_gB,[_gz]));}));}))];},_gD=new T(function(){return B(unCStr("SUB"));}),_gE=[0,26],_gF=function(_gG){return [1,B(_ej(_gD,function(_gH){return E(new T(function(){return B(A(_gG,[_gE]));}));}))];},_gI=new T(function(){return B(unCStr("ESC"));}),_gJ=[0,27],_gK=function(_gL){return [1,B(_ej(_gI,function(_gM){return E(new T(function(){return B(A(_gL,[_gJ]));}));}))];},_gN=new T(function(){return B(unCStr("FS"));}),_gO=[0,28],_gP=function(_gQ){return [1,B(_ej(_gN,function(_gR){return E(new T(function(){return B(A(_gQ,[_gO]));}));}))];},_gS=new T(function(){return B(unCStr("GS"));}),_gT=[0,29],_gU=function(_gV){return [1,B(_ej(_gS,function(_gW){return E(new T(function(){return B(A(_gV,[_gT]));}));}))];},_gX=new T(function(){return B(unCStr("RS"));}),_gY=[0,30],_gZ=function(_h0){return [1,B(_ej(_gX,function(_h1){return E(new T(function(){return B(A(_h0,[_gY]));}));}))];},_h2=new T(function(){return B(unCStr("US"));}),_h3=[0,31],_h4=function(_h5){return [1,B(_ej(_h2,function(_h6){return E(new T(function(){return B(A(_h5,[_h3]));}));}))];},_h7=new T(function(){return B(unCStr("SP"));}),_h8=[0,32],_h9=function(_ha){return [1,B(_ej(_h7,function(_hb){return E(new T(function(){return B(A(_ha,[_h8]));}));}))];},_hc=new T(function(){return B(unCStr("DEL"));}),_hd=[0,127],_he=function(_hf){return [1,B(_ej(_hc,function(_hg){return E(new T(function(){return B(A(_hf,[_hd]));}));}))];},_hh=[1,_he,_u],_hi=[1,_h9,_hh],_hj=[1,_h4,_hi],_hk=[1,_gZ,_hj],_hl=[1,_gU,_hk],_hm=[1,_gP,_hl],_hn=[1,_gK,_hm],_ho=[1,_gF,_hn],_hp=[1,_gA,_ho],_hq=[1,_gv,_hp],_hr=[1,_gq,_hq],_hs=[1,_gl,_hr],_ht=[1,_gg,_hs],_hu=[1,_gb,_ht],_hv=[1,_g6,_hu],_hw=[1,_g1,_hv],_hx=[1,_fW,_hw],_hy=[1,_fR,_hx],_hz=[1,_fM,_hy],_hA=[1,_fH,_hz],_hB=[1,_fC,_hA],_hC=[1,_fx,_hB],_hD=[1,_fs,_hC],_hE=[1,_fn,_hD],_hF=[1,_fi,_hE],_hG=[1,_fd,_hF],_hH=[1,_f8,_hG],_hI=[1,_f3,_hH],_hJ=[1,_eY,_hI],_hK=[1,_eT,_hJ],_hL=[1,_eO,_hK],_hM=[1,_eJ,_hL],_hN=[1,_eF,_hM],_hO=new T(function(){return B(_eb(_hN));}),_hP=[0,1114111],_hQ=[0,34],_hR=[0,39],_hS=function(_hT){var _hU=new T(function(){return B(A(_hT,[_fc]));}),_hV=new T(function(){return B(A(_hT,[_fh]));}),_hW=new T(function(){return B(A(_hT,[_fm]));}),_hX=new T(function(){return B(A(_hT,[_fr]));}),_hY=new T(function(){return B(A(_hT,[_fw]));}),_hZ=new T(function(){return B(A(_hT,[_fB]));}),_i0=new T(function(){return B(A(_hT,[_fG]));});return new F(function(){return _a0([0,function(_i1){switch(E(E(_i1)[1])){case 34:return E(new T(function(){return B(A(_hT,[_hQ]));}));case 39:return E(new T(function(){return B(A(_hT,[_hR]));}));case 92:return E(new T(function(){return B(A(_hT,[_dI]));}));case 97:return E(_hU);case 98:return E(_hV);case 102:return E(_hZ);case 110:return E(_hX);case 114:return E(_i0);case 116:return E(_hW);case 118:return E(_hY);default:return [2];}}],new T(function(){return B(_a0([1,B(_bc(_dG,_dJ,function(_i2){return [1,B(_bO(_i2,function(_i3){var _i4=B(_cK(new T(function(){return B(_cA(E(_i2)[1]));}),_cz,_i3));return !B(_e1(_i4,_hP))?[2]:B(A(_hT,[new T(function(){var _i5=B(_dY(_i4));if(_i5>>>0>1114111){var _i6=B(_dW(_i5));}else{var _i6=[0,_i5];}var _i7=_i6,_i8=_i7,_i9=_i8;return _i9;})]));}))];}))],new T(function(){return B(_a0([0,function(_ia){return E(E(_ia)[1])==94?E([0,function(_ib){switch(E(E(_ib)[1])){case 64:return E(new T(function(){return B(A(_hT,[_eI]));}));case 65:return E(new T(function(){return B(A(_hT,[_ew]));}));case 66:return E(new T(function(){return B(A(_hT,[_eN]));}));case 67:return E(new T(function(){return B(A(_hT,[_eS]));}));case 68:return E(new T(function(){return B(A(_hT,[_eX]));}));case 69:return E(new T(function(){return B(A(_hT,[_f2]));}));case 70:return E(new T(function(){return B(A(_hT,[_f7]));}));case 71:return E(_hU);case 72:return E(_hV);case 73:return E(_hW);case 74:return E(_hX);case 75:return E(_hY);case 76:return E(_hZ);case 77:return E(_i0);case 78:return E(new T(function(){return B(A(_hT,[_eB]));}));case 79:return E(new T(function(){return B(A(_hT,[_fL]));}));case 80:return E(new T(function(){return B(A(_hT,[_fQ]));}));case 81:return E(new T(function(){return B(A(_hT,[_fV]));}));case 82:return E(new T(function(){return B(A(_hT,[_g0]));}));case 83:return E(new T(function(){return B(A(_hT,[_g5]));}));case 84:return E(new T(function(){return B(A(_hT,[_ga]));}));case 85:return E(new T(function(){return B(A(_hT,[_gf]));}));case 86:return E(new T(function(){return B(A(_hT,[_gk]));}));case 87:return E(new T(function(){return B(A(_hT,[_gp]));}));case 88:return E(new T(function(){return B(A(_hT,[_gu]));}));case 89:return E(new T(function(){return B(A(_hT,[_gz]));}));case 90:return E(new T(function(){return B(A(_hT,[_gE]));}));case 91:return E(new T(function(){return B(A(_hT,[_gJ]));}));case 92:return E(new T(function(){return B(A(_hT,[_gO]));}));case 93:return E(new T(function(){return B(A(_hT,[_gT]));}));case 94:return E(new T(function(){return B(A(_hT,[_gY]));}));case 95:return E(new T(function(){return B(A(_hT,[_h3]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_hO,[_hT]));})));})));}));});},_ic=function(_id){return new F(function(){return A(_id,[_b6]);});},_ie=function(_if){var _ig=E(_if);if(!_ig[0]){return E(_ic);}else{var _ih=_ig[2],_ii=E(E(_ig[1])[1]);switch(_ii){case 9:return function(_ij){return [0,function(_ik){return E(new T(function(){return B(A(new T(function(){return B(_ie(_ih));}),[_ij]));}));}];};case 10:return function(_il){return [0,function(_im){return E(new T(function(){return B(A(new T(function(){return B(_ie(_ih));}),[_il]));}));}];};case 11:return function(_in){return [0,function(_io){return E(new T(function(){return B(A(new T(function(){return B(_ie(_ih));}),[_in]));}));}];};case 12:return function(_ip){return [0,function(_iq){return E(new T(function(){return B(A(new T(function(){return B(_ie(_ih));}),[_ip]));}));}];};case 13:return function(_ir){return [0,function(_is){return E(new T(function(){return B(A(new T(function(){return B(_ie(_ih));}),[_ir]));}));}];};case 32:return function(_it){return [0,function(_iu){return E(new T(function(){return B(A(new T(function(){return B(_ie(_ih));}),[_it]));}));}];};case 160:return function(_iv){return [0,function(_iw){return E(new T(function(){return B(A(new T(function(){return B(_ie(_ih));}),[_iv]));}));}];};default:var _ix=u_iswspace(_ii),_iy=_ix;return E(_iy)==0?E(_ic):function(_iz){return [0,function(_iA){return E(new T(function(){return B(A(new T(function(){return B(_ie(_ih));}),[_iz]));}));}];};}}},_iB=function(_iC){var _iD=new T(function(){return B(_iB(_iC));}),_iE=[1,function(_iF){return new F(function(){return A(_ie,[_iF,function(_iG){return E([0,function(_iH){return E(E(_iH)[1])==92?E(_iD):[2];}]);}]);});}];return new F(function(){return _a0([0,function(_iI){return E(E(_iI)[1])==92?E([0,function(_iJ){var _iK=E(E(_iJ)[1]);switch(_iK){case 9:return E(_iE);case 10:return E(_iE);case 11:return E(_iE);case 12:return E(_iE);case 13:return E(_iE);case 32:return E(_iE);case 38:return E(_iD);case 160:return E(_iE);default:var _iL=u_iswspace(_iK),_iM=_iL;return E(_iM)==0?[2]:E(_iE);}}]):[2];}],[0,function(_iN){var _iO=E(_iN);return E(_iO[1])==92?E(new T(function(){return B(_hS(function(_iP){return new F(function(){return A(_iC,[[0,_iP,_n]]);});}));})):B(A(_iC,[[0,_iO,_r]]));}]);});},_iQ=function(_iR,_iS){return new F(function(){return _iB(function(_iT){var _iU=E(_iT),_iV=E(_iU[1]);if(E(_iV[1])==34){if(!E(_iU[2])){return E(new T(function(){return B(A(_iS,[[1,new T(function(){return B(A(_iR,[_u]));})]]));}));}else{return new F(function(){return _iQ(function(_iW){return new F(function(){return A(_iR,[[1,_iV,_iW]]);});},_iS);});}}else{return new F(function(){return _iQ(function(_iX){return new F(function(){return A(_iR,[[1,_iV,_iX]]);});},_iS);});}});});},_iY=new T(function(){return B(unCStr("_\'"));}),_iZ=function(_j0){var _j1=u_iswalnum(_j0),_j2=_j1;return E(_j2)==0?B(_dh(_aI,[0,_j0],_iY)):true;},_j3=function(_j4){return new F(function(){return _iZ(E(_j4)[1]);});},_j5=new T(function(){return B(unCStr(",;()[]{}`"));}),_j6=new T(function(){return B(unCStr(".."));}),_j7=new T(function(){return B(unCStr("::"));}),_j8=new T(function(){return B(unCStr("->"));}),_j9=[0,64],_ja=[1,_j9,_u],_jb=[0,126],_jc=[1,_jb,_u],_jd=new T(function(){return B(unCStr("=>"));}),_je=[1,_jd,_u],_jf=[1,_jc,_je],_jg=[1,_ja,_jf],_jh=[1,_j8,_jg],_ji=new T(function(){return B(unCStr("<-"));}),_jj=[1,_ji,_jh],_jk=[0,124],_jl=[1,_jk,_u],_jm=[1,_jl,_jj],_jn=[1,_dI,_u],_jo=[1,_jn,_jm],_jp=[0,61],_jq=[1,_jp,_u],_jr=[1,_jq,_jo],_js=[1,_j7,_jr],_jt=[1,_j6,_js],_ju=function(_jv){return new F(function(){return _a0([1,function(_jw){return E(_jw)[0]==0?E(new T(function(){return B(A(_jv,[_bJ]));})):[2];}],new T(function(){return B(_a0([0,function(_jx){return E(E(_jx)[1])==39?E([0,function(_jy){var _jz=E(_jy);switch(E(_jz[1])){case 39:return [2];case 92:return E(new T(function(){return B(_hS(function(_jA){return [0,function(_jB){return E(E(_jB)[1])==39?E(new T(function(){return B(A(_jv,[[0,_jA]]));})):[2];}];}));}));default:return [0,function(_jC){return E(E(_jC)[1])==39?E(new T(function(){return B(A(_jv,[[0,_jz]]));})):[2];}];}}]):[2];}],new T(function(){return B(_a0([0,function(_jD){return E(E(_jD)[1])==34?E(new T(function(){return B(_iQ(_bK,_jv));})):[2];}],new T(function(){return B(_a0([0,function(_jE){return !B(_dh(_aI,_jE,_j5))?[2]:B(A(_jv,[[2,[1,_jE,_u]]]));}],new T(function(){return B(_a0([0,function(_jF){return !B(_dh(_aI,_jF,_dm))?[2]:[1,B(_by(_dn,function(_jG){var _jH=[1,_jF,_jG];return !B(_dh(_aR,_jH,_jt))?B(A(_jv,[[4,_jH]])):B(A(_jv,[[2,_jH]]));}))];}],new T(function(){return B(_a0([0,function(_jI){var _jJ=E(_jI),_jK=_jJ[1],_jL=u_iswalpha(_jK),_jM=_jL;return E(_jM)==0?E(_jK)==95?[1,B(_by(_j3,function(_jN){return new F(function(){return A(_jv,[[3,[1,_jJ,_jN]]]);});}))]:[2]:[1,B(_by(_j3,function(_jO){return new F(function(){return A(_jv,[[3,[1,_jJ,_jO]]]);});}))];}],new T(function(){return [1,B(_bc(_dz,_df,_jv))];})));})));})));})));})));}));});},_jP=[0,0],_jQ=function(_jR,_jS){return function(_jT){return new F(function(){return A(_ie,[_jT,function(_jU){return E(new T(function(){return B(_ju(function(_jV){var _jW=E(_jV);return _jW[0]==2?!B(_ax(_jW[1],_aw))?[2]:E(new T(function(){return B(A(_jR,[_jP,function(_jX){return [1,function(_jY){return new F(function(){return A(_ie,[_jY,function(_jZ){return E(new T(function(){return B(_ju(function(_k0){var _k1=E(_k0);return _k1[0]==2?!B(_ax(_k1[1],_au))?[2]:E(new T(function(){return B(A(_jS,[_jX]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_k2=function(_k3,_k4,_k5){var _k6=function(_k7,_k8){return new F(function(){return _a0([1,function(_k9){return new F(function(){return A(_ie,[_k9,function(_ka){return E(new T(function(){return B(_ju(function(_kb){var _kc=E(_kb);if(_kc[0]==4){var _kd=E(_kc[1]);if(!_kd[0]){return new F(function(){return A(_k3,[_kc,_k7,_k8]);});}else{return E(E(_kd[1])[1])==45?E(_kd[2])[0]==0?E([1,function(_ke){return new F(function(){return A(_ie,[_ke,function(_kf){return E(new T(function(){return B(_ju(function(_kg){return new F(function(){return A(_k3,[_kg,_k7,function(_kh){return new F(function(){return A(_k8,[new T(function(){return B(_cu(_kh));})]);});}]);});}));}));}]);});}]):B(A(_k3,[_kc,_k7,_k8])):B(A(_k3,[_kc,_k7,_k8]));}}else{return new F(function(){return A(_k3,[_kc,_k7,_k8]);});}}));}));}]);});}],new T(function(){return [1,B(_jQ(_k6,_k8))];}));});};return new F(function(){return _k6(_k4,_k5);});},_ki=function(_kj,_kk){return [2];},_kl=function(_km){var _kn=E(_km);return _kn[0]==0?[1,new T(function(){return B(_cK(new T(function(){return B(_cA(E(_kn[1])[1]));}),_cz,_kn[2]));})]:E(_kn[2])[0]==0?E(_kn[3])[0]==0?[1,new T(function(){return B(_cK(_cy,_cz,_kn[1]));})]:[0]:[0];},_ko=function(_kp){var _kq=E(_kp);if(_kq[0]==5){var _kr=B(_kl(_kq[1]));return _kr[0]==0?E(_ki):function(_ks,_kt){return new F(function(){return A(_kt,[_kr[1]]);});};}else{return E(_ki);}},_ku=function(_kv){return [1,function(_kw){return new F(function(){return A(_ie,[_kw,function(_kx){return E([3,_kv,_b3]);}]);});}];},_ky=new T(function(){return B(_k2(_ko,_jP,_ku));}),_kz=[0,_2F],_kA=[0,_2D],_kB=new T(function(){return B(unCStr("Tried to deserialize a non-Number to a Double"));}),_kC=[0,_kB],_kD=[0,_1I],_kE=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_kF=new T(function(){return B(err(_kE));}),_kG=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_kH=new T(function(){return B(err(_kG));}),_kI=new T(function(){return [0,"lpsCoeff"];}),_kJ=new T(function(){return [0,"dependCoeff"];}),_kK=new T(function(){return [0,"maxLoves"];}),_kL=new T(function(){return [0,"items"];}),_kM=new T(function(){return [0,"achievements"];}),_kN=new T(function(){return [0,"lastFocus"];}),_kO=new T(function(){return [0,"depend"];}),_kP=new T(function(){return [0,"lps"];}),_kQ=new T(function(){return [0,"loves"];}),_kR=function(_kS){while(1){var _kT=(function(_kU){var _kV=E(_kU);if(!_kV[0]){return [0];}else{var _kW=_kV[2],_kX=E(_kV[1]);if(!E(_kX[2])[0]){return [1,_kX[1],new T(function(){return B(_kR(_kW));})];}else{_kS=_kW;return null;}}})(_kS);if(_kT!=null){return _kT;}}},_kY=function(_){var _kZ=jsEval("Date.now()"),_l0=_kZ;return new T(function(){var _l1=B(_kR(B(_9Q(_ky,new T(function(){return fromJSStr(_l0);})))));return _l1[0]==0?B(err(_kE)):E(_l1[2])[0]==0?E(_l1[1]):B(err(_kG));});},_l2=[0,0],_l3=[0,1],_l4=function(_){var _=0,_l5=B(_kY(_)),_l6=_l5;return [0,_l2,_l2,_l2,_l6,_r,_48,_80,_l2,_l3,_l3];},_l7=function(_l8){var _l9=B(A(_l8,[_])),_la=_l9;return E(_la);},_lb=new T(function(){return B(_l7(_l4));}),_lc=function(_ld){var _le=E(_ld);if(_le[0]==4){var _lf=_le[1],_lg=B(_2J(_2y,_kQ,_lf));if(!_lg[0]){return E(_kA);}else{var _lh=E(_lg[1]);if(!_lh[0]){var _li=_lh[1],_lj=B(_2J(_2y,_kP,_lf));if(!_lj[0]){return E(_kA);}else{var _lk=E(_lj[1]);if(!_lk[0]){var _ll=_lk[1],_lm=B(_2J(_2y,_kO,_lf));if(!_lm[0]){return E(_kA);}else{var _ln=E(_lm[1]);if(!_ln[0]){var _lo=_ln[1],_lp=B(_2J(_2y,_kN,_lf));if(!_lp[0]){return E(_kA);}else{var _lq=E(_lp[1]);if(_lq[0]==1){var _lr=_lq[1],_ls=function(_lt){var _lu=B(_2J(_2y,_kL,_lf));if(!_lu[0]){return E(_kA);}else{var _lv=B(_3r(_40,_40,_lu[1]));if(!_lv[0]){return [0,_lv[1]];}else{var _lw=_lv[1],_lx=B(_2J(_2y,_kK,_lf));if(!_lx[0]){return E(_kA);}else{var _ly=E(_lx[1]);if(!_ly[0]){var _lz=_ly[1],_lA=function(_lB){var _lC=function(_lD){return [1,new T(function(){var _lE=E(_lb),_lF=_lE[5],_lG=_lE[6],_lH=_lE[9],_lI=_lE[10],_lJ=new T(function(){var _lK=B(_kR(B(_9Q(_ky,new T(function(){return fromJSStr(E(_lr)[1]);})))));return _lK[0]==0?E(_kF):E(_lK[2])[0]==0?E(_lK[1]):E(_kH);}),_lL=E(_lD);if(!_lL[0]){var _lM=E(_lB),_lN=_lM[0]==0?[0,_li,_ll,_lo,_lJ,_lF,_lG,new T(function(){return B(_8s(_80,_lw));}),_lz,_lH,_lI]:[0,_li,_ll,_lo,_lJ,_lF,_lG,new T(function(){return B(_8s(_80,_lw));}),_lz,_lM[1],_lI];}else{var _lO=_lL[1],_lP=E(_lB),_lN=_lP[0]==0?[0,_li,_ll,_lo,_lJ,_lF,_lG,new T(function(){return B(_8s(_80,_lw));}),_lz,_lH,_lO]:[0,_li,_ll,_lo,_lJ,_lF,_lG,new T(function(){return B(_8s(_80,_lw));}),_lz,_lP[1],_lO];}var _lQ=_lN;return _lQ;})];},_lR=B(_2J(_2y,_kI,_lf));if(!_lR[0]){return new F(function(){return _lC(_2z);});}else{var _lS=E(_lR[1]);return _lS[0]==0?B(_lC([1,_lS[1]])):B(_lC(_2z));}},_lT=B(_2J(_2y,_kJ,_lf));if(!_lT[0]){return new F(function(){return _lA(_2z);});}else{var _lU=E(_lT[1]);return _lU[0]==0?B(_lA([1,_lU[1]])):B(_lA(_2z));}}else{return E(_kC);}}}}},_lV=B(_2J(_2y,_kM,_lf));if(!_lV[0]){return new F(function(){return _ls(_);});}else{var _lW=B(_3r(_2e,_3d,_lV[1]));if(!_lW[0]){return new F(function(){return _ls(_);});}else{var _lX=_lW[1],_lY=B(_2J(_2y,_kL,_lf));if(!_lY[0]){return E(_kA);}else{var _lZ=B(_3r(_40,_40,_lY[1]));if(!_lZ[0]){return [0,_lZ[1]];}else{var _m0=_lZ[1],_m1=B(_2J(_2y,_kK,_lf));if(!_m1[0]){return E(_kA);}else{var _m2=E(_m1[1]);if(!_m2[0]){var _m3=_m2[1],_m4=function(_m5){var _m6=function(_m7){return [1,new T(function(){var _m8=E(_lb),_m9=_m8[5],_ma=_m8[9],_mb=_m8[10],_mc=new T(function(){var _md=B(_kR(B(_9Q(_ky,new T(function(){return fromJSStr(E(_lr)[1]);})))));return _md[0]==0?E(_kF):E(_md[2])[0]==0?E(_md[1]):E(_kH);}),_me=E(_m7);if(!_me[0]){var _mf=E(_m5),_mg=_mf[0]==0?[0,_li,_ll,_lo,_mc,_m9,new T(function(){return B(_7P(_lX));}),new T(function(){return B(_8s(_80,_m0));}),_m3,_ma,_mb]:[0,_li,_ll,_lo,_mc,_m9,new T(function(){return B(_7P(_lX));}),new T(function(){return B(_8s(_80,_m0));}),_m3,_mf[1],_mb];}else{var _mh=_me[1],_mi=E(_m5),_mg=_mi[0]==0?[0,_li,_ll,_lo,_mc,_m9,new T(function(){return B(_7P(_lX));}),new T(function(){return B(_8s(_80,_m0));}),_m3,_ma,_mh]:[0,_li,_ll,_lo,_mc,_m9,new T(function(){return B(_7P(_lX));}),new T(function(){return B(_8s(_80,_m0));}),_m3,_mi[1],_mh];}var _mj=_mg;return _mj;})];},_mk=B(_2J(_2y,_kI,_lf));if(!_mk[0]){return new F(function(){return _m6(_2z);});}else{var _ml=E(_mk[1]);return _ml[0]==0?B(_m6([1,_ml[1]])):B(_m6(_2z));}},_mm=B(_2J(_2y,_kJ,_lf));if(!_mm[0]){return new F(function(){return _m4(_2z);});}else{var _mn=E(_mm[1]);return _mn[0]==0?B(_m4([1,_mn[1]])):B(_m4(_2z));}}else{return E(_kC);}}}}}}}else{return E(_kD);}}}else{return E(_kC);}}}else{return E(_kC);}}}else{return E(_kC);}}}else{return E(_kz);}},_mo=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_mp=[0,_mo],_mq=[1,_u],_mr=function(_ms){var _mt=E(_ms);if(!_mt[0]){return E(_mq);}else{var _mu=B(_lc(_mt[1]));if(!_mu[0]){return [0,_mu[1]];}else{var _mv=B(_mr(_mt[2]));return _mv[0]==0?[0,_mv[1]]:[1,[1,_mu[1],_mv[1]]];}}},_mw=function(_mx){var _my=E(_mx);return _my[0]==3?B(_mr(_my[1])):E(_mp);},_mz=[0,_1r,_1y,_lc,_mw],_mA=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_mB=new T(function(){return B(err(_mA));}),_mC=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_mD=new T(function(){return B(err(_mC));}),_mE=function(_mF,_mG){while(1){var _mH=E(_mF);if(!_mH[0]){return E(_mD);}else{var _mI=E(_mG);if(!_mI){return E(_mH[1]);}else{_mF=_mH[2];_mG=_mI-1|0;continue;}}}},_mJ=new T(function(){return B(unCStr("ACK"));}),_mK=new T(function(){return B(unCStr("BEL"));}),_mL=new T(function(){return B(unCStr("BS"));}),_mM=new T(function(){return B(unCStr("SP"));}),_mN=[1,_mM,_u],_mO=new T(function(){return B(unCStr("US"));}),_mP=[1,_mO,_mN],_mQ=new T(function(){return B(unCStr("RS"));}),_mR=[1,_mQ,_mP],_mS=new T(function(){return B(unCStr("GS"));}),_mT=[1,_mS,_mR],_mU=new T(function(){return B(unCStr("FS"));}),_mV=[1,_mU,_mT],_mW=new T(function(){return B(unCStr("ESC"));}),_mX=[1,_mW,_mV],_mY=new T(function(){return B(unCStr("SUB"));}),_mZ=[1,_mY,_mX],_n0=new T(function(){return B(unCStr("EM"));}),_n1=[1,_n0,_mZ],_n2=new T(function(){return B(unCStr("CAN"));}),_n3=[1,_n2,_n1],_n4=new T(function(){return B(unCStr("ETB"));}),_n5=[1,_n4,_n3],_n6=new T(function(){return B(unCStr("SYN"));}),_n7=[1,_n6,_n5],_n8=new T(function(){return B(unCStr("NAK"));}),_n9=[1,_n8,_n7],_na=new T(function(){return B(unCStr("DC4"));}),_nb=[1,_na,_n9],_nc=new T(function(){return B(unCStr("DC3"));}),_nd=[1,_nc,_nb],_ne=new T(function(){return B(unCStr("DC2"));}),_nf=[1,_ne,_nd],_ng=new T(function(){return B(unCStr("DC1"));}),_nh=[1,_ng,_nf],_ni=new T(function(){return B(unCStr("DLE"));}),_nj=[1,_ni,_nh],_nk=new T(function(){return B(unCStr("SI"));}),_nl=[1,_nk,_nj],_nm=new T(function(){return B(unCStr("SO"));}),_nn=[1,_nm,_nl],_no=new T(function(){return B(unCStr("CR"));}),_np=[1,_no,_nn],_nq=new T(function(){return B(unCStr("FF"));}),_nr=[1,_nq,_np],_ns=new T(function(){return B(unCStr("VT"));}),_nt=[1,_ns,_nr],_nu=new T(function(){return B(unCStr("LF"));}),_nv=[1,_nu,_nt],_nw=new T(function(){return B(unCStr("HT"));}),_nx=[1,_nw,_nv],_ny=[1,_mL,_nx],_nz=[1,_mK,_ny],_nA=[1,_mJ,_nz],_nB=new T(function(){return B(unCStr("ENQ"));}),_nC=[1,_nB,_nA],_nD=new T(function(){return B(unCStr("EOT"));}),_nE=[1,_nD,_nC],_nF=new T(function(){return B(unCStr("ETX"));}),_nG=[1,_nF,_nE],_nH=new T(function(){return B(unCStr("STX"));}),_nI=[1,_nH,_nG],_nJ=new T(function(){return B(unCStr("SOH"));}),_nK=[1,_nJ,_nI],_nL=new T(function(){return B(unCStr("NUL"));}),_nM=[1,_nL,_nK],_nN=[0,92],_nO=new T(function(){return B(unCStr("\\DEL"));}),_nP=new T(function(){return B(unCStr("\\a"));}),_nQ=new T(function(){return B(unCStr("\\\\"));}),_nR=new T(function(){return B(unCStr("\\SO"));}),_nS=new T(function(){return B(unCStr("\\r"));}),_nT=new T(function(){return B(unCStr("\\f"));}),_nU=new T(function(){return B(unCStr("\\v"));}),_nV=new T(function(){return B(unCStr("\\n"));}),_nW=new T(function(){return B(unCStr("\\t"));}),_nX=new T(function(){return B(unCStr("\\b"));}),_nY=function(_nZ,_o0){if(_nZ<=127){var _o1=E(_nZ);switch(_o1){case 92:return new F(function(){return _O(_nQ,_o0);});break;case 127:return new F(function(){return _O(_nO,_o0);});break;default:if(_o1<32){var _o2=E(_o1);switch(_o2){case 7:return new F(function(){return _O(_nP,_o0);});break;case 8:return new F(function(){return _O(_nX,_o0);});break;case 9:return new F(function(){return _O(_nW,_o0);});break;case 10:return new F(function(){return _O(_nV,_o0);});break;case 11:return new F(function(){return _O(_nU,_o0);});break;case 12:return new F(function(){return _O(_nT,_o0);});break;case 13:return new F(function(){return _O(_nS,_o0);});break;case 14:return new F(function(){return _O(_nR,new T(function(){var _o3=E(_o0);if(!_o3[0]){var _o4=[0];}else{var _o4=E(E(_o3[1])[1])==72?B(unAppCStr("\\&",_o3)):E(_o3);}return _o4;}));});break;default:return new F(function(){return _O([1,_nN,new T(function(){var _o5=_o2;return _o5>=0?B(_mE(_nM,_o5)):E(_mB);})],_o0);});}}else{return [1,[0,_o1],_o0];}}}else{return [1,_nN,new T(function(){var _o6=jsShowI(_nZ),_o7=_o6;return B(_O(fromJSStr(_o7),new T(function(){var _o8=E(_o0);if(!_o8[0]){var _o9=[0];}else{var _oa=E(_o8[1])[1];if(_oa<48){var _ob=E(_o8);}else{var _ob=_oa>57?E(_o8):B(unAppCStr("\\&",_o8));}var _oc=_ob,_od=_oc,_o9=_od;}return _o9;})));})];}},_oe=[0,39],_of=[1,_oe,_u],_og=new T(function(){return B(unCStr("\'\\\'\'"));}),_oh=function(_oi){var _oj=E(E(_oi)[1]);return _oj==39?E(_og):[1,_oe,new T(function(){return B(_nY(_oj,_of));})];},_ok=[0,34],_ol=new T(function(){return B(unCStr("\\\""));}),_om=function(_on,_oo){var _op=E(_on);if(!_op[0]){return E(_oo);}else{var _oq=_op[2],_or=E(E(_op[1])[1]);if(_or==34){return new F(function(){return _O(_ol,new T(function(){return B(_om(_oq,_oo));}));});}else{return new F(function(){return _nY(_or,new T(function(){return B(_om(_oq,_oo));}));});}}},_os=function(_ot,_ou){return [1,_ok,new T(function(){return B(_om(_ot,[1,_ok,_ou]));})];},_ov=function(_ow){return new F(function(){return _O(_og,_ow);});},_ox=function(_oy,_oz){var _oA=E(E(_oz)[1]);return _oA==39?E(_ov):function(_oB){return [1,_oe,new T(function(){return B(_nY(_oA,[1,_oe,_oB]));})];};},_oC=[0,_ox,_oh,_os],_oD=function(_oE){return E(E(_oE)[3]);},_oF=function(_oG,_oH){return new F(function(){return A(_oD,[_oG,_oH,_u]);});},_oI=function(_oJ,_oK,_oL){return new F(function(){return _95(new T(function(){return B(_oD(_oJ));}),_oK,_oL);});},_oM=function(_oN){return [0,function(_oO){return E(new T(function(){return B(_oD(_oN));}));},function(_ow){return new F(function(){return _oF(_oN,_ow);});},function(_oP,_ow){return new F(function(){return _oI(_oN,_oP,_ow);});}];},_oQ=new T(function(){return B(_oM(_oC));}),_oR=new T(function(){return B(unCStr("Just "));}),_oS=new T(function(){return B(unCStr("Nothing"));}),_oT=[0,11],_oU=function(_oV){return E(E(_oV)[1]);},_oW=function(_oX,_oY,_oZ,_p0){var _p1=E(_oZ);if(!_p1[0]){return new F(function(){return _O(_oS,_p0);});}else{var _p2=_p1[1];return E(_oY)[1]<=10?B(_O(_oR,new T(function(){return B(A(_oU,[_oX,_oT,_p2,_p0]));}))):[1,_17,new T(function(){return B(_O(_oR,new T(function(){return B(A(_oU,[_oX,_oT,_p2,[1,_16,_p0]]));})));})];}},_p3=[0,0],_p4=function(_p5,_p6){return new F(function(){return _oW(_p5,_p3,_p6,_u);});},_p7=function(_p8,_p9,_pa){return new F(function(){return _95(function(_oP,_ow){return new F(function(){return _oW(_p8,_p3,_oP,_ow);});},_p9,_pa);});},_pb=function(_pc){return [0,function(_pd,_oP,_ow){return new F(function(){return _oW(_pc,_pd,_oP,_ow);});},function(_ow){return new F(function(){return _p4(_pc,_ow);});},function(_oP,_ow){return new F(function(){return _p7(_pc,_oP,_ow);});}];},_pe=new T(function(){return B(_pb(_oQ));}),_pf=function(_pg){var _ph=jsShow(E(_pg)[1]),_pi=_ph;return new F(function(){return fromJSStr(_pi);});},_pj=function(_pk){return function(_bv){return new F(function(){return _O(new T(function(){return B(_pf(_pk));}),_bv);});};},_pl=function(_pm){return new F(function(){return _dQ(0,E(_pm)[1],_u);});},_pn=function(_po,_pp){return new F(function(){return _dQ(0,E(_po)[1],_pp);});},_pq=function(_pr,_ps){return new F(function(){return _95(_pn,_pr,_ps);});},_pt=function(_pu,_pv,_pw){return new F(function(){return _dQ(E(_pu)[1],E(_pv)[1],_pw);});},_px=[0,_pt,_pl,_pq],_py=function(_pz,_pA,_pB){return new F(function(){return A(_pz,[[1,_92,new T(function(){return B(A(_pA,[_pB]));})]]);});},_pC=new T(function(){return B(unCStr(": empty list"));}),_pD=new T(function(){return B(unCStr("Prelude."));}),_pE=function(_pF){return new F(function(){return err(B(_O(_pD,new T(function(){return B(_O(_pF,_pC));}))));});},_pG=new T(function(){return B(unCStr("foldr1"));}),_pH=new T(function(){return B(_pE(_pG));}),_pI=function(_pJ,_pK){var _pL=E(_pK);if(!_pL[0]){return E(_pH);}else{var _pM=_pL[1],_pN=E(_pL[2]);if(!_pN[0]){return E(_pM);}else{return new F(function(){return A(_pJ,[_pM,new T(function(){return B(_pI(_pJ,_pN));})]);});}}},_pO=function(_pP,_pQ,_pR,_pS){return new F(function(){return _95(function(_pT,_pU){var _pV=E(_pT);return [1,_17,new T(function(){return B(A(_pI,[_py,[1,new T(function(){return B(A(new T(function(){return B(_oU(_pP));}),[_p3,_pV[1]]));}),[1,new T(function(){return B(A(new T(function(){return B(_oU(_pQ));}),[_p3,_pV[2]]));}),_u]],[1,_16,_pU]]));})];},_pR,_pS);});},_pW=new T(function(){return B(unCStr("fromList "));}),_pX=function(_pY,_pZ){while(1){var _q0=(function(_q1,_q2){var _q3=E(_q2);switch(_q3[0]){case 0:_pY=new T(function(){return B(_pX(_q1,_q3[4]));});_pZ=_q3[3];return null;case 1:return [1,[0,[0,_q3[1]],_q3[2]],_q1];default:return E(_q1);}})(_pY,_pZ);if(_q0!=null){return _q0;}}},_q4=function(_q5){var _q6=E(_q5);if(!_q6[0]){var _q7=_q6[3],_q8=_q6[4];return _q6[2]>=0?B(_pX(new T(function(){return B(_pX(_u,_q8));}),_q7)):B(_pX(new T(function(){return B(_pX(_u,_q7));}),_q8));}else{return new F(function(){return _pX(_u,_q6);});}},_q9=function(_qa,_qb,_qc){var _qd=new T(function(){return B(_q4(_qc));});return _qb<=10?function(_qe){return new F(function(){return _O(_pW,new T(function(){return B(_pO(_px,_qa,_qd,_qe));}));});}:function(_qf){return [1,_17,new T(function(){return B(_O(_pW,new T(function(){return B(_pO(_px,_qa,_qd,[1,_16,_qf]));})));})];};},_qg=new T(function(){return B(unCStr("fromList "));}),_qh=function(_qi,_qj){while(1){var _qk=(function(_ql,_qm){var _qn=E(_qm);if(!_qn[0]){_qi=[1,[0,_qn[2],_qn[3]],new T(function(){return B(_qh(_ql,_qn[5]));})];_qj=_qn[4];return null;}else{return E(_ql);}})(_qi,_qj);if(_qk!=null){return _qk;}}},_qo=function(_qp,_qq,_qr,_qs){var _qt=new T(function(){return B(_qh(_u,_qs));});return _qr<=10?function(_qu){return new F(function(){return _O(_qg,new T(function(){return B(_pO(_qp,_qq,_qt,_qu));}));});}:function(_qv){return [1,_17,new T(function(){return B(_O(_qg,new T(function(){return B(_pO(_qp,_qq,_qt,[1,_16,_qv]));})));})];};},_qw=[0,45],_qx=function(_qy,_qz,_qA){var _qB=function(_qC){var _qD=new T(function(){return B(A(_qy,[[0, -_qA]]));});return E(_qz)[1]<=6?function(_qE){return [1,_qw,new T(function(){return B(A(_qD,[_qE]));})];}:function(_qF){return [1,_17,[1,_qw,new T(function(){return B(A(_qD,[[1,_16,_qF]]));})]];};};if(_qA>=0){var _qG=isDoubleNegativeZero(_qA),_qH=_qG;return E(_qH)==0?B(A(_qy,[[0,_qA]])):B(_qB(_));}else{return new F(function(){return _qB(_);});}},_qI=[0,0],_qJ=[0,125],_qK=new T(function(){return B(unCStr("_lpsCoeff = "));}),_qL=new T(function(){return B(unCStr(", "));}),_qM=new T(function(){return B(unCStr("_dependCoeff = "));}),_qN=new T(function(){return B(unCStr("_maxLoves = "));}),_qO=new T(function(){return B(unCStr("_items = "));}),_qP=new T(function(){return B(unCStr("_achieves = "));}),_qQ=new T(function(){return B(unCStr("_hasFocus = "));}),_qR=new T(function(){return B(unCStr("_lastFocus = "));}),_qS=new T(function(){return B(unCStr("_depend = "));}),_qT=new T(function(){return B(unCStr("_lps = "));}),_qU=new T(function(){return B(unCStr("_loves = "));}),_qV=new T(function(){return B(unCStr("Aichan {"));}),_qW=new T(function(){return B(unCStr("True"));}),_qX=new T(function(){return B(unCStr("False"));}),_qY=function(_qZ,_r0,_r1,_r2,_r3,_r4,_r5,_r6,_r7,_r8,_r9){var _ra=function(_rb){return new F(function(){return _O(_qV,new T(function(){return B(_O(_qU,new T(function(){return B(A(new T(function(){return B(_qx(_pj,_qI,E(_r0)[1]));}),[new T(function(){return B(_O(_qL,new T(function(){return B(_O(_qT,new T(function(){return B(A(new T(function(){return B(_qx(_pj,_qI,E(_r1)[1]));}),[new T(function(){return B(_O(_qL,new T(function(){return B(_O(_qS,new T(function(){return B(A(new T(function(){return B(_qx(_pj,_qI,E(_r2)[1]));}),[new T(function(){return B(_O(_qL,new T(function(){return B(_O(_qR,new T(function(){return B(_19(0,_r3,new T(function(){return B(_O(_qL,new T(function(){return B(_O(_qQ,new T(function(){var _rc=new T(function(){return B(_O(_qL,new T(function(){return B(_O(_qP,new T(function(){return B(A(new T(function(){return B(_qo(_oQ,_pe,0,_r5));}),[new T(function(){return B(_O(_qL,new T(function(){return B(_O(_qO,new T(function(){return B(A(new T(function(){return B(_q9(_px,0,_r6));}),[new T(function(){return B(_O(_qL,new T(function(){return B(_O(_qN,new T(function(){return B(A(new T(function(){return B(_qx(_pj,_qI,E(_r7)[1]));}),[new T(function(){return B(_O(_qL,new T(function(){return B(_O(_qM,new T(function(){return B(A(new T(function(){return B(_qx(_pj,_qI,E(_r8)[1]));}),[new T(function(){return B(_O(_qL,new T(function(){return B(_O(_qK,new T(function(){return B(A(new T(function(){return B(_qx(_pj,_qI,E(_r9)[1]));}),[[1,_qJ,_rb]]));})));})));})]));})));})));})]));})));})));})]));})));})));})]));})));})));});return !E(_r4)?B(_O(_qX,_rc)):B(_O(_qW,_rc));})));})));})));})));})));})]));})));})));})]));})));})));})]));})));}));});};return _qZ<11?E(_ra):function(_rd){return [1,_17,new T(function(){return B(_ra([1,_16,_rd]));})];};},_re=function(_rf){var _rg=E(_rf);return new F(function(){return A(_qY,[0,_rg[1],_rg[2],_rg[3],_rg[4],_rg[5],_rg[6],_rg[7],_rg[8],_rg[9],_rg[10],_u]);});},_rh=function(_ri,_rj,_rk,_){var _rl=rMV(_rj),_rm=_rl,_rn=B(A(_rk,[_rm,_])),_ro=_rn,_=wMV(_rj,new T(function(){return E(E(_ro)[2]);})),_rp=jsSetTimeout(_ri,function(_){var _rq=B(_rh(_ri,_rj,_rk,_)),_rr=_rq;return _b6;});return new F(function(){return rMV(_rj);});},_rs=new T(function(){return B(unCStr(" is not an element of the map"));}),_rt=function(_ru){return new F(function(){return err(B(unAppCStr("IntMap.!: key ",new T(function(){return B(_O(B(_dQ(0,_ru,_u)),_rs));}))));});},_rv=function(_rw,_rx){var _ry=new T(function(){return B(_rt(_rx));});return new F(function(){return (function(_rz){while(1){var _rA=E(_rz);switch(_rA[0]){case 0:var _rB=_rA[2]>>>0;if(((_rx>>>0&((_rB-1>>>0^4294967295)>>>0^_rB)>>>0)>>>0&4294967295)==_rA[1]){if(!((_rx>>>0&_rB)>>>0)){_rz=_rA[3];continue;}else{_rz=_rA[4];continue;}}else{return E(_ry);}break;case 1:return _rx!=_rA[1]?E(_ry):E(_rA[2]);default:return E(_ry);}}})(_rw);});},_rC=function(_rD,_rE){return new F(function(){return (function(_rF){while(1){var _rG=E(_rF);switch(_rG[0]){case 0:var _rH=_rG[2]>>>0;if(((_rD>>>0&((_rH-1>>>0^4294967295)>>>0^_rH)>>>0)>>>0&4294967295)==_rG[1]){if(!((_rD>>>0&_rH)>>>0)){_rF=_rG[3];continue;}else{_rF=_rG[4];continue;}}else{return false;}break;case 1:return _rD==_rG[1];default:return false;}}})(_rE);});},_rI=function(_rJ,_rK,_rL,_rM){var _rN=E(_rM);switch(_rN[0]){case 0:var _rO=_rN[1],_rP=_rN[2],_rQ=_rN[3],_rR=_rN[4],_rS=_rP>>>0;if(((_rK>>>0&((_rS-1>>>0^4294967295)>>>0^_rS)>>>0)>>>0&4294967295)==_rO){return (_rK>>>0&_rS)>>>0==0?[0,_rO,_rP,E(B(_rI(_rJ,_rK,_rL,_rQ))),E(_rR)]:[0,_rO,_rP,E(_rQ),E(B(_rI(_rJ,_rK,_rL,_rR)))];}else{var _rT=(_rK>>>0^_rO>>>0)>>>0,_rU=(_rT|_rT>>>1)>>>0,_rV=(_rU|_rU>>>2)>>>0,_rW=(_rV|_rV>>>4)>>>0,_rX=(_rW|_rW>>>8)>>>0,_rY=(_rX|_rX>>>16)>>>0,_rZ=(_rY^_rY>>>1)>>>0&4294967295,_s0=_rZ>>>0;return (_rK>>>0&_s0)>>>0==0?[0,(_rK>>>0&((_s0-1>>>0^4294967295)>>>0^_s0)>>>0)>>>0&4294967295,_rZ,E([1,_rK,_rL]),E(_rN)]:[0,(_rK>>>0&((_s0-1>>>0^4294967295)>>>0^_s0)>>>0)>>>0&4294967295,_rZ,E(_rN),E([1,_rK,_rL])];}break;case 1:var _s1=_rN[1];if(_rK!=_s1){var _s2=(_rK>>>0^_s1>>>0)>>>0,_s3=(_s2|_s2>>>1)>>>0,_s4=(_s3|_s3>>>2)>>>0,_s5=(_s4|_s4>>>4)>>>0,_s6=(_s5|_s5>>>8)>>>0,_s7=(_s6|_s6>>>16)>>>0,_s8=(_s7^_s7>>>1)>>>0&4294967295,_s9=_s8>>>0;return (_rK>>>0&_s9)>>>0==0?[0,(_rK>>>0&((_s9-1>>>0^4294967295)>>>0^_s9)>>>0)>>>0&4294967295,_s8,E([1,_rK,_rL]),E(_rN)]:[0,(_rK>>>0&((_s9-1>>>0^4294967295)>>>0^_s9)>>>0)>>>0&4294967295,_s8,E(_rN),E([1,_rK,_rL])];}else{return [1,_rK,new T(function(){return B(A(_rJ,[[0,_rK],_rL,_rN[2]]));})];}break;default:return [1,_rK,_rL];}},_sa=[0,1],_sb=function(_sc,_sd,_){var _se=jsCreateTextNode(toJSStr(E(_sc))),_sf=_se,_sg=jsAppendChild(_sf,E(_sd)[1]);return [0,_sf];},_sh=function(_si,_sj,_){var _sk=jsGet(_si,toJSStr(E(_sj))),_sl=_sk;return new T(function(){return fromJSStr(_sl);});},_sm=function(_sn,_so,_sp,_sq){return new F(function(){return A(_sn,[function(_){var _sr=jsSetAttr(E(_so)[1],toJSStr(E(_sp)),toJSStr(E(_sq)));return _b6;}]);});},_ss=function(_st,_su,_sv,_sw){return new F(function(){return A(_st,[function(_){var _sx=jsSet(E(_su)[1],toJSStr(E(_sv)),toJSStr(E(_sw)));return _b6;}]);});},_sy=[0,32],_sz=[1,_sy,_u],_sA=new T(function(){return B(unCStr("class"));}),_sB=new T(function(){return B(unCStr("list-group-item"));}),_sC=function(_sD){return new F(function(){return _l7(function(_){var _=0;return new F(function(){return eval(_sD);});});});},_sE=new T(function(){return B(_sC("(function(e,c){var first = e.firstChild; e.insertBefore(c,first);})"));}),_sF=function(_sG){return function(_sH,_){var _sI=B(A(new T(function(){return B(A(_sE,[E(E(_sG)[1])]));}),[E(E(_sH)[1]),_])),_sJ=_sI;return _b6;};},_sK=new T(function(){return B(unCStr(") "));}),_sL=new T(function(){return B(unCStr("li"));}),_sM=new T(function(){return B(unCStr("var d = new Date(); d.getHours() + \':\' + d.getMinutes() + \':\' + d.getSeconds()"));}),_sN=new T(function(){return B(unCStr("innerHTML"));}),_sO=[0,1],_sP=[0,49],_sQ=[1,_sP,_u],_sR=new T(function(){return B(unCStr("<br>"));}),_sS=new T(function(){return B(unCStr("log-group"));}),_sT=function(_sU,_sV){while(1){var _sW=E(_sU);if(!_sW[0]){return E(_sV);}else{_sU=_sW[2];var _sX=_sV+1|0;_sV=_sX;continue;}}},_sY=function(_sZ,_t0){while(1){var _t1=E(_sZ);if(!_t1){return E(_t0);}else{var _t2=E(_t0);if(!_t2[0]){return [0];}else{_sZ=_t1-1|0;_t0=_t2[2];continue;}}}},_t3=function(_t4,_t5,_t6){while(1){var _t7=E(_t5);if(!_t7[0]){return true;}else{var _t8=E(_t6);if(!_t8[0]){return false;}else{if(!B(A(_2H,[_t4,_t7[1],_t8[1]]))){return false;}else{_t5=_t7[2];_t6=_t8[2];continue;}}}}},_t9=function(_ta,_tb,_tc,_td){if(!B(_t3(_aI,_ta,[1,_tc,_td]))){return [1,_tc,new T(function(){return B(_te(_ta,_tb,_td));})];}else{return new F(function(){return _O(_tb,new T(function(){var _tf=B(_sT(_ta,0));if(_tf>=0){var _tg=B(_te(_ta,_tb,B(_sY(_tf,[1,_tc,_td]))));}else{var _tg=B(_t9(_ta,_tb,_tc,_td));}var _th=_tg,_ti=_th;return _ti;}));});}},_te=function(_tj,_tk,_tl){var _tm=E(_tl);if(!_tm[0]){return [0];}else{var _tn=_tm[1],_to=_tm[2];if(!B(_t3(_aI,_tj,_tm))){return [1,_tn,new T(function(){return B(_te(_tj,_tk,_to));})];}else{return new F(function(){return _O(_tk,new T(function(){var _tp=B(_sT(_tj,0));if(_tp>=0){var _tq=B(_te(_tj,_tk,B(_sY(_tp,_tm))));}else{var _tq=B(_t9(_tj,_tk,_tn,_to));}var _tr=_tq,_ts=_tr;return _ts;}));});}}},_tt=new T(function(){return B(unCStr("innerHTML"));}),_tu=new T(function(){return B(unCStr("span"));}),_tv=function(_tw,_tx,_ty,_){var _tz=jsCreateElem(toJSStr(E(_tu))),_tA=_tz,_tB=jsAppendChild(_tA,E(_ty)[1]),_tC=[0,_tA],_tD=B(A(_tw,[_tx,_tC,_])),_tE=_tD;return _tC;},_tF=new T(function(){return B(unCStr("strong"));}),_tG=function(_tH,_tI,_tJ,_){var _tK=jsCreateElem(toJSStr(E(_tF))),_tL=_tK,_tM=jsAppendChild(_tL,E(_tJ)[1]),_tN=[0,_tL],_tO=B(A(_tH,[_tI,_tN,_])),_tP=_tO;return _tN;},_tQ=function(_tR){return E(_tR);},_tS=new T(function(){return B(unCStr("unread-badge"));}),_tT=new T(function(){return B(unCStr(" could be found!"));}),_tU=function(_tV){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_O(_tV,_tT));}))));});},_tW=function(_tX,_tY,_){var _tZ=E(_sS),_u0=jsFind(toJSStr(_tZ)),_u1=_u0,_u2=E(_u1);if(!_u2[0]){return new F(function(){return _tU(_tZ);});}else{var _u3=jsEval(toJSStr(E(_sM))),_u4=_u3,_u5=jsCreateElem(toJSStr(E(_sL))),_u6=_u5,_u7=[0,_u6],_u8=B(A(_sm,[_bK,_u7,_sA,_sB,_])),_u9=_u8,_ua=B(_tG(_sb,new T(function(){return B(_O(_tX,new T(function(){return B(unAppCStr(" (",new T(function(){return B(_O(fromJSStr(_u4),_sK));})));})));}),_u7,_)),_ub=_ua,_uc=B(_tv(_tQ,function(_ud,_){var _ue=B(A(_ss,[_bK,_ud,_tt,new T(function(){return B(_te(_sR,_sz,_tY));}),_])),_uf=_ue;return _ud;},_u7,_)),_ug=_uc,_uh=B(A(_sF,[_u2[1],_u7,_])),_ui=_uh,_uj=E(_tS),_uk=jsFind(toJSStr(_uj)),_ul=_uk,_um=E(_ul);if(!_um[0]){return new F(function(){return _tU(_uj);});}else{var _un=E(_um[1]),_uo=_un[1],_up=B(_sh(_uo,_sN,_)),_uq=_up;if(!B(_ax(_uq,_u))){var _ur=B(_kR(B(_9Q(_ky,_uq))));if(!_ur[0]){return E(_kF);}else{if(!E(_ur[2])[0]){var _us=jsSet(_uo,toJSStr(E(_sN)),toJSStr(B(_19(0,B(_ck(_sO,_ur[1])),_u))));return _b6;}else{return E(_kH);}}}else{var _ut=B(A(_ss,[_bK,_un,_sN,_sQ,_])),_uu=_ut;return _b6;}}}},_uv=function(_uw){return E(_uw);},_ux=function(_uy,_uz){while(1){var _uA=E(_uy),_uB=E(_uz);if(!_uB[0]){switch(B(_41(_uA,_uB[2]))){case 0:_uy=_uA;_uz=_uB[4];continue;case 1:return [1,_uB[3]];default:_uy=_uA;_uz=_uB[5];continue;}}else{return [0];}}},_uC=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:336:3-8"));}),_uD=new T(function(){return B(unCStr("button"));}),_uE=new T(function(){return B(unCStr("type"));}),_uF=new T(function(){return B(unCStr("alert"));}),_uG=new T(function(){return B(unCStr("data-dismiss"));}),_uH=new T(function(){return B(unCStr("close"));}),_uI=new T(function(){return B(unCStr("aria-hidden"));}),_uJ=new T(function(){return B(unCStr("true"));}),_uK=new T(function(){return B(unCStr("&times;"));}),_uL=function(_uM,_){var _uN=B(A(_ss,[_bK,_uM,_tt,_uK,_])),_uO=_uN;return _uM;},_uP=new T(function(){return B(unCStr("sr-only"));}),_uQ=new T(function(){return B(unCStr("Close"));}),_uR=function(_uS,_){var _uT=B(_tv(_tQ,_uL,_uS,_)),_uU=_uT,_uV=B(A(_sm,[_bK,_uU,_uI,_uJ,_])),_uW=_uV,_uX=B(_tv(_sb,_uQ,_uS,_)),_uY=_uX,_uZ=B(A(_sm,[_bK,_uY,_sA,_uP,_])),_v0=_uZ;return _uS;},_v1=new T(function(){return B(unCStr("role"));}),_v2=new T(function(){return B(unCStr("alert alert-info fade in tip"));}),_v3=new T(function(){return B(unCStr("button"));}),_v4=function(_v5,_v6,_v7,_){var _v8=jsCreateElem(toJSStr(E(_v3))),_v9=_v8,_va=jsAppendChild(_v9,E(_v7)[1]),_vb=[0,_v9],_vc=B(A(_v5,[_v6,_vb,_])),_vd=_vc;return _vb;},_ve=new T(function(){return B(unCStr("div"));}),_vf=function(_vg,_vh,_vi,_){var _vj=jsCreateElem(toJSStr(E(_ve))),_vk=_vj,_vl=jsAppendChild(_vk,E(_vi)[1]),_vm=[0,_vk],_vn=B(A(_vg,[_vh,_vm,_])),_vo=_vn;return _vm;},_vp=new T(function(){return B(unCStr("id"));}),_vq=new T(function(){return B(unCStr("\').alert(\'close\')"));}),_vr=new T(function(){return B(unCStr("alerts"));}),_vs=function(_vt,_vu,_){var _vv=B(_kY(_)),_vw=_vv,_vx=E(_vr),_vy=jsFind(toJSStr(_vx)),_vz=_vy,_vA=E(_vz);if(!_vA[0]){return new F(function(){return _tU(_vx);});}else{var _vB=_vA[1],_vC=B(A(_ss,[_bK,_vB,_tt,_u,_])),_vD=_vC,_vE=B(_vf(_tQ,function(_vF,_){var _vG=B(_v4(_tQ,_uR,_vF,_)),_vH=_vG,_vI=B(A(_sm,[_bK,_vH,_uE,_uD,_])),_vJ=_vI,_vK=B(A(_sm,[_bK,_vH,_sA,_uH,_])),_vL=_vK,_vM=B(A(_sm,[_bK,_vH,_uG,_uF,_])),_vN=_vM,_vO=B(_tv(_tQ,function(_vP,_){var _vQ=B(A(_ss,[_bK,_vP,_tt,_vu,_])),_vR=_vQ;return _vP;},_vF,_)),_vS=_vO;return _vF;},_vB,_)),_vT=_vE,_vU=E(_vT),_vV=jsSetAttr(_vU[1],toJSStr(E(_vp)),toJSStr(B(unAppCStr("alert-",new T(function(){return B(_19(0,_vw,_u));}))))),_vW=B(A(_sm,[_bK,_vU,_sA,_v2,_])),_vX=_vW,_vY=B(A(_sm,[_bK,_vU,_v1,_uF,_])),_vZ=_vY,_w0=jsSetTimeout(5000,function(_){var _w1=jsEval(toJSStr(B(unAppCStr("$(\'#alert-",new T(function(){return B(_O(B(_19(0,_vw,_u)),_vq));}))))),_w2=_w1;return _b6;});return new F(function(){return _tW(_vt,_vu,_);});}},_w3=new T(function(){return B(unCStr("Aichan"));}),_w4=new T(function(){return [0,toJSStr(_u)];}),_w5=[0,93],_w6=[1,_w5,_u],_w7=new T(function(){return [0,toJSStr(_w6)];}),_w8=[0,125],_w9=[1,_w8,_u],_wa=new T(function(){return [0,toJSStr(_w9)];}),_wb=[0,58],_wc=[1,_wb,_u],_wd=new T(function(){return [0,toJSStr(_wc)];}),_we=[0,44],_wf=[1,_we,_u],_wg=new T(function(){return [0,toJSStr(_wf)];}),_wh=new T(function(){return [0,"false"];}),_wi=function(_wj){var _wk=jsShow(E(_wj)[1]),_wl=_wk;return [0,_wl];},_wm=function(_wn){var _wo=jsStringify(E(_wn)[1]),_wp=_wo;return [0,_wp];},_wq=new T(function(){return [0,"null"];}),_wr=[0,91],_ws=[1,_wr,_u],_wt=new T(function(){return [0,toJSStr(_ws)];}),_wu=[0,123],_wv=[1,_wu,_u],_ww=new T(function(){return [0,toJSStr(_wv)];}),_wx=[0,34],_wy=[1,_wx,_u],_wz=new T(function(){return [0,toJSStr(_wy)];}),_wA=new T(function(){return [0,"true"];}),_wB=function(_wC,_wD){var _wE=E(_wD);switch(_wE[0]){case 0:return [0,new T(function(){return B(_wi(_wE[1]));}),_wC];case 1:return [0,new T(function(){return B(_wm(_wE[1]));}),_wC];case 2:return !E(_wE[1])?[0,_wh,_wC]:[0,_wA,_wC];case 3:var _wF=E(_wE[1]);return _wF[0]==0?[0,_wt,[1,_w7,_wC]]:[0,_wt,new T(function(){var _wG=B(_wB(new T(function(){var _wH=function(_wI){var _wJ=E(_wI);return _wJ[0]==0?E([1,_w7,_wC]):[1,_wg,new T(function(){var _wK=B(_wB(new T(function(){return B(_wH(_wJ[2]));}),_wJ[1]));return [1,_wK[1],_wK[2]];})];};return B(_wH(_wF[2]));}),_wF[1]));return [1,_wG[1],_wG[2]];})];case 4:var _wL=E(_wE[1]);if(!_wL[0]){return [0,_ww,[1,_wa,_wC]];}else{var _wM=E(_wL[1]);return [0,_ww,[1,new T(function(){return B(_wm(_wM[1]));}),[1,_wd,new T(function(){var _wN=B(_wB(new T(function(){var _wO=function(_wP){var _wQ=E(_wP);if(!_wQ[0]){return E([1,_wa,_wC]);}else{var _wR=E(_wQ[1]);return [1,_wg,[1,_wz,[1,_wR[1],[1,_wz,[1,_wd,new T(function(){var _wS=B(_wB(new T(function(){return B(_wO(_wQ[2]));}),_wR[2]));return [1,_wS[1],_wS[2]];})]]]]];}};return B(_wO(_wL[2]));}),_wM[2]));return [1,_wN[1],_wN[2]];})]]];}break;default:return [0,_wq,_wC];}},_wT=function(_wU){var _wV=jsCat(new T(function(){var _wW=B(_wB(_u,_wU));return [1,_wW[1],_wW[2]];}),E(_w4)[1]),_wX=_wV;return E(_wX);},_wY=new T(function(){return [0,"(function(k,v) {localStorage.setItem(k,v);})"];}),_wZ=function(_x0,_x1){return function(_x2,_){var _x3=B(A(new T(function(){return B(A(_sC,[E(_wY)[1],E(toJSStr(E(_x1)))]));}),[E(B(_wT(B(A(new T(function(){return B(_2f(_x0));}),[_x2]))))),_])),_x4=_x3;return _b6;};},_x5=new T(function(){return B(_wZ(_mz,_w3));}),_x6=new T(function(){return B(unCStr("game"));}),_x7=new T(function(){return B(unCStr("\u5b9f\u7e3e\u7372\u5f97: "));}),_x8=function(_x9,_xa){return function(_xb,_){var _xc=B(_vs(_x6,new T(function(){return B(_O(_x7,_xa));}),_)),_xd=_xc,_xe=new T(function(){var _xf=E(_xb);return [0,_xf[1],_xf[2],_xf[3],_xf[4],_xf[5],new T(function(){return B(_6X(_xa,[1,_x9],_xf[6]));}),_xf[7],_xf[8],_xf[9],_xf[10]];}),_xg=B(A(_x5,[_xe,_])),_xh=_xg;return new F(function(){return _xi(_xe,_);});};},_xj=function(_xk){var _xl=E(_xk);return _xl[0]==0?_xl[1]:I_toNumber(_xl[1]);},_xm=new T(function(){return B(unCStr("\u611b\u60c5\u304c"));}),_xn=new T(function(){return B(unCStr("\u3092\u8d85\u3048\u308b"));}),_xo=function(_xp,_xq){return function(_xr,_){var _xs=E(_xr);return E(_xs[1])[1]<=E(new T(function(){return [0,B(_xj(_xp))];}))[1]?[0,_b6,_xs]:B(A(new T(function(){return B(_x8(new T(function(){return B(_O(_xm,new T(function(){return B(_O(B(_19(0,_xp,_u)),_xn));})));}),_xq));}),[_xs,_]));};},_xt=new T(function(){return B(unCStr("\u30a2\u30a4\u3068\u306e\u906d\u9047"));}),_xu=[0,100],_xv=new T(function(){return B(_xo(_xu,_xt));}),_xw=new T(function(){return [0,_xt,_xv];}),_xx=[0,10000],_xy=new T(function(){return B(unCStr("\u611b\u3055\u308c\u6c17\u5206"));}),_xz=new T(function(){return B(_xo(_xx,_xy));}),_xA=new T(function(){return [0,_xy,_xz];}),_xB=[0,1000000],_xC=new T(function(){return B(unCStr("\u611b\u30e9\u30d6\u30e6\u30fc"));}),_xD=new T(function(){return B(_xo(_xB,_xC));}),_xE=new T(function(){return [0,_xC,_xD];}),_xF=[0,100000000],_xG=new T(function(){return B(unCStr("\u611b\u3055\u3093\u306e\u611b\u304c\u91cd\u3044"));}),_xH=new T(function(){return B(_xo(_xF,_xG));}),_xI=new T(function(){return [0,_xG,_xH];}),_xJ=[1,I_fromBits([1215752192,23])],_xK=new T(function(){return B(unCStr("\u611b\u3061\u3083\u3093\u30de\u30b9\u30bf\u30fc"));}),_xL=new T(function(){return B(_xo(_xJ,_xK));}),_xM=new T(function(){return [0,_xK,_xL];}),_xN=new T(function(){return B(unCStr("\u597d\u611f\u5ea6\u304c"));}),_xO=function(_xP,_xQ){return function(_xR,_){var _xS=E(_xR);return E(_xS[2])[1]<=E(new T(function(){return [0,B(_xj(_xP))];}))[1]?[0,_b6,_xS]:B(A(new T(function(){return B(_x8(new T(function(){return B(_O(_xN,new T(function(){return B(_O(B(_19(0,_xP,_u)),_xn));})));}),_xQ));}),[_xS,_]));};},_xT=new T(function(){return B(unCStr("\u307e\u305a\u306f\u304a\u53cb\u9054\u304b\u3089"));}),_xU=[0,10],_xV=new T(function(){return B(_xO(_xU,_xT));}),_xW=new T(function(){return [0,_xT,_xV];}),_xX=new T(function(){return B(unCStr("\u4e00\u7d1a\u30d5\u30e9\u30b0\u5efa\u7bc9\u58eb"));}),_xY=new T(function(){return B(_xO(_xu,_xX));}),_xZ=new T(function(){return [0,_xX,_xY];}),_y0=[0,1000],_y1=new T(function(){return B(unCStr("\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_y2=new T(function(){return B(_xO(_y0,_y1));}),_y3=new T(function(){return [0,_y1,_y2];}),_y4=new T(function(){return B(unCStr("\uff8a\uff72\uff8a\uff9f\uff70\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_y5=new T(function(){return B(_xO(_xx,_y4));}),_y6=new T(function(){return [0,_y4,_y5];}),_y7=new T(function(){return B(unCStr("\u5168\u958b\u3089\u3076\u3071\u308f\u30fc"));}),_y8=[0,100000],_y9=new T(function(){return B(_xO(_y8,_y7));}),_ya=new T(function(){return [0,_y7,_y9];}),_yb=new T(function(){return B(unCStr("\u4f9d\u5b58\u5ea6\u304c"));}),_yc=function(_yd,_ye){return function(_yf,_){var _yg=E(_yf);return E(_yg[3])[1]<=E(new T(function(){return [0,B(_xj(_yd))];}))[1]?[0,_b6,_yg]:B(A(new T(function(){return B(_x8(new T(function(){return B(_O(_yb,new T(function(){return B(_O(B(_19(0,_yd,_u)),_xn));})));}),_ye));}),[_yg,_]));};},_yh=new T(function(){return B(unCStr("\u4f9d\u5b58\u6ce8\u610f\u5831"));}),_yi=new T(function(){return B(_yc(_xu,_yh));}),_yj=new T(function(){return [0,_yh,_yi];}),_yk=new T(function(){return B(unCStr("\u4f9d\u5b58\u30c9\u30e9\u30c3\u30b0"));}),_yl=new T(function(){return B(_yc(_xx,_yk));}),_ym=new T(function(){return [0,_yk,_yl];}),_yn=[0,100],_yo=new T(function(){return B(unCStr("\u300d\u3092"));}),_yp=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u300c"));}),_yq=new T(function(){return B(unCStr("\u500b\u4ee5\u4e0a\u624b\u306b\u5165\u308c\u308b"));}),_yr=function(_ys,_yt){if(_ys<=_yt){var _yu=function(_yv){return [1,[0,_yv],new T(function(){if(_yv!=_yt){var _yw=B(_yu(_yv+1|0));}else{var _yw=[0];}var _yx=_yw;return _yx;})];};return new F(function(){return _yu(_ys);});}else{return [0];}},_yy=[0,23478],_yz=[1,_yy,_u],_yA=new T(function(){return B(unCStr("\u5bb6<br>\u597d\u611f\u5ea6 +12000"));}),_yB=new T(function(){return B(unCStr("fa-home"));}),_yC=[0,_yB,_yA,_yz],_yD=function(_yE,_yF,_){return [0,_b6,new T(function(){var _yG=E(_yF);return [0,_yG[1],new T(function(){return [0,E(_yG[2])[1]+12000];}),_yG[3],_yG[4],_yG[5],_yG[6],_yG[7],_yG[8],_yG[9],_yG[10]];})];},_yH=new T(function(){return B(unCStr("Negative exponent"));}),_yI=new T(function(){return B(err(_yH));}),_yJ=function(_yK,_yL,_yM){while(1){if(!(_yL%2)){var _yN=_yK*_yK,_yO=quot(_yL,2);_yK=_yN;_yL=_yO;continue;}else{var _yP=E(_yL);if(_yP==1){return _yK*_yM;}else{var _yN=_yK*_yK;_yL=quot(_yP-1|0,2);var _yQ=_yK*_yM;_yK=_yN;_yM=_yQ;continue;}}}},_yR=function(_yS,_yT){while(1){if(!(_yT%2)){var _yU=_yS*_yS,_yV=quot(_yT,2);_yS=_yU;_yT=_yV;continue;}else{var _yW=E(_yT);if(_yW==1){return E(_yS);}else{return new F(function(){return _yJ(_yS*_yS,quot(_yW-1|0,2),_yS);});}}}},_yX=function(_yY){var _yZ=I_decodeDouble(_yY);return [0,[1,_yZ[2]],_yZ[1]];},_z0=function(_z1){var _z2=hs_intToInt64(2147483647),_z3=_z2,_z4=hs_leInt64(_z1,_z3),_z5=_z4;if(!E(_z5)){return [1,I_fromInt64(_z1)];}else{var _z6=hs_intToInt64(-2147483648),_z7=_z6,_z8=hs_geInt64(_z1,_z7),_z9=_z8;if(!E(_z9)){return [1,I_fromInt64(_z1)];}else{var _za=hs_int64ToInt(_z1),_zb=_za;return new F(function(){return _cA(_zb);});}}},_zc=function(_zd){var _ze=hs_intToInt64(_zd),_zf=_ze;return E(_zf);},_zg=function(_zh){var _zi=E(_zh);return _zi[0]==0?B(_zc(_zi[1])):I_toInt64(_zi[1]);},_zj=[0,0],_zk=new T(function(){return [0,0/0];}),_zl=new T(function(){return [0,-1/0];}),_zm=new T(function(){return [0,1/0];}),_zn=[0,0],_zo=function(_zp,_zq){while(1){var _zr=E(_zp);if(!_zr[0]){_zp=[1,I_fromInt(_zr[1])];continue;}else{var _zs=E(_zq);if(!_zs[0]){_zp=_zr;_zq=[1,I_fromInt(_zs[1])];continue;}else{return new F(function(){return I_fromRat(_zr[1],_zs[1]);});}}}},_zt=function(_zu,_zv){var _zw=E(_zu);if(!_zw[0]){var _zx=_zw[1],_zy=E(_zv);return _zy[0]==0?_zx==_zy[1]:I_compareInt(_zy[1],_zx)==0?true:false;}else{var _zz=_zw[1],_zA=E(_zv);return _zA[0]==0?I_compareInt(_zz,_zA[1])==0?true:false:I_compare(_zz,_zA[1])==0?true:false;}},_zB=function(_zC,_zD){return !B(_zt(_zD,_zn))?[0,B(_zo(_zC,_zD))]:!B(_zt(_zC,_zn))?!B(_Y(_zC,_zn))?E(_zm):E(_zl):E(_zk);},_zE=[0,27],_zF=[0,20],_zG=new T(function(){return B(_zB(_zE,_zF));}),_zH=[0,-1],_zI=function(_zJ,_zK){while(1){var _zL=E(_zJ);if(!_zL[0]){_zJ=[1,I_fromInt(_zL[1])];continue;}else{return [1,I_shiftLeft(_zL[1],_zK)];}}},_zM=function(_zN,_zO){if(_zO>=0){var _zP=function(_zQ){var _zR=B(_yX(_zN*_zQ)),_zS=_zR[1],_zT=_zR[2];if(_zT>=0){return new F(function(){return _zI(_zS,_zT);});}else{var _zU= -_zT;if(_zU<=52){var _zV=hs_uncheckedIShiftRA64(B(_zg(_zS)),_zU),_zW=_zV;return new F(function(){return _z0(_zW);});}else{return !B(_Y(_zS,_zj))?E(_zj):E(_zH);}}},_zX=E(_zO);if(!_zX){return new F(function(){return _zP(1);});}else{return new F(function(){return _zP(B(_yR(E(_zG)[1],_zX)));});}}else{return E(_yI);}},_zY=function(_zZ){return new F(function(){return _zM(250000000,E(_zZ)[1]);});},_A0=[0,_zY,_yD,_yC],_A1=[0,36554],_A2=[1,_A1,_u],_A3=new T(function(){return B(unCStr("\u8eca<br>\u597d\u611f\u5ea6 +5000"));}),_A4=new T(function(){return B(unCStr("fa-car"));}),_A5=[0,_A4,_A3,_A2],_A6=function(_A7,_A8,_){return [0,_b6,new T(function(){var _A9=E(_A8);return [0,_A9[1],new T(function(){return [0,E(_A9[2])[1]+5000];}),_A9[3],_A9[4],_A9[5],_A9[6],_A9[7],_A9[8],_A9[9],_A9[10]];})];},_Aa=function(_Ab){return new F(function(){return _zM(10000000,E(_Ab)[1]);});},_Ac=[0,_Aa,_A6,_A5],_Ad=new T(function(){return B(unCStr("\u65c5\u884c"));}),_Ae=new T(function(){return B(unCStr("\u65c5\u884c<br>\u597d\u611f\u5ea6 +600"));}),_Af=new T(function(){return B(unCStr("fa-plane"));}),_Ag=[0,_Af,_Ae,_Ad],_Ah=function(_Ai,_Aj,_){return [0,_b6,new T(function(){var _Ak=E(_Aj);return [0,_Ak[1],new T(function(){return [0,E(_Ak[2])[1]+600];}),_Ak[3],_Ak[4],_Ak[5],_Ak[6],_Ak[7],_Ak[8],_Ak[9],_Ak[10]];})];},_Al=function(_Am){return new F(function(){return _zM(500000,E(_Am)[1]);});},_An=[0,_Al,_Ah,_Ag],_Ao=new T(function(){return B(unCStr("\u30d7\u30ec\u30bc\u30f3\u30c8"));}),_Ap=new T(function(){return B(unCStr("\u30d7\u30ec\u30bc\u30f3\u30c8<br>\u597d\u611f\u5ea6 +100"));}),_Aq=new T(function(){return B(unCStr("fa-gift"));}),_Ar=[0,_Aq,_Ap,_Ao],_As=function(_At,_Au,_){return [0,_b6,new T(function(){var _Av=E(_Au);return [0,_Av[1],new T(function(){return [0,E(_Av[2])[1]+100];}),_Av[3],_Av[4],_Av[5],_Av[6],_Av[7],_Av[8],_Av[9],_Av[10]];})];},_Aw=function(_Ax){return new F(function(){return _zM(20000,E(_Ax)[1]);});},_Ay=[0,_Aw,_As,_Ar],_Az=new T(function(){return B(unCStr("\u55ab\u8336\u5e97"));}),_AA=new T(function(){return B(unCStr("\u55ab\u8336\u5e97<br>\u597d\u611f\u5ea6 +10"));}),_AB=new T(function(){return B(unCStr("fa-coffee"));}),_AC=[0,_AB,_AA,_Az],_AD=function(_AE,_AF,_){return [0,_b6,new T(function(){var _AG=E(_AF);return [0,_AG[1],new T(function(){return [0,E(_AG[2])[1]+10];}),_AG[3],_AG[4],_AG[5],_AG[6],_AG[7],_AG[8],_AG[9],_AG[10]];})];},_AH=function(_AI){return new F(function(){return _zM(1000,E(_AI)[1]);});},_AJ=[0,_AH,_AD,_AC],_AK=new T(function(){return B(unCStr("\u30e1\u30fc\u30eb"));}),_AL=new T(function(){return B(unCStr("\u30e1\u30fc\u30eb<br>\u597d\u611f\u5ea6 +1.0"));}),_AM=new T(function(){return B(unCStr("fa-envelope"));}),_AN=[0,_AM,_AL,_AK],_AO=function(_AP,_AQ,_){return [0,_b6,new T(function(){var _AR=E(_AQ);return [0,_AR[1],new T(function(){return [0,E(_AR[2])[1]+1];}),_AR[3],_AR[4],_AR[5],_AR[6],_AR[7],_AR[8],_AR[9],_AR[10]];})];},_AS=function(_AT){return new F(function(){return _zM(50,E(_AT)[1]);});},_AU=[0,_AS,_AO,_AN],_AV=new T(function(){return B(unCStr("\u4f1a\u8a71"));}),_AW=new T(function(){return B(unCStr("\u4f1a\u8a71<br>\u597d\u611f\u5ea6 +0.2"));}),_AX=new T(function(){return B(unCStr("fa-comments-o"));}),_AY=[0,_AX,_AW,_AV],_AZ=function(_B0,_B1,_){return [0,_b6,new T(function(){var _B2=E(_B1);return [0,_B2[1],new T(function(){return [0,E(_B2[2])[1]+0.2];}),_B2[3],_B2[4],_B2[5],_B2[6],_B2[7],_B2[8],_B2[9],_B2[10]];})];},_B3=function(_B4){return new F(function(){return _zM(1,E(_B4)[1]);});},_B5=[0,_B3,_AZ,_AY],_B6=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u53f3\u76ee+"));}),_B7=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u53f3\u76ee<br>\u4f9d\u5b58\u5ea6\u30dc\u30fc\u30ca\u30b9\u304c100\u500d\u306b\u306a\u308a\u307e\u3059\u3002"));}),_B8=new T(function(){return B(unCStr("fa-eye-slash"));}),_B9=[0,_B8,_B7,_B6],_Ba=[0,100],_Bb=function(_Bc,_Bd,_){return [0,_b6,new T(function(){var _Be=E(_Bd);return [0,_Be[1],_Be[2],_Be[3],_Be[4],_Be[5],_Be[6],_Be[7],_Be[8],_Ba,_Be[10]];})];},_Bf=[0,10000000],_Bg=function(_Bh){return E(_Bf);},_Bi=[0,_Bg,_Bb,_B9],_Bj=function(_Bk,_Bl,_){return [0,_b6,new T(function(){var _Bm=E(_Bl);return [0,_Bm[1],_Bm[2],_Bm[3],_Bm[4],_Bm[5],_Bm[6],_Bm[7],_Bm[8],_Bm[9],_Ba];})];},_Bn=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u5de6\u76ee+"));}),_Bo=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u5de6\u76ee<br>\u4f9d\u5b58\u5ea6\u304c\u597d\u611f\u5ea6\u306b\u5909\u308f\u308b\u901f\u3055\u304c100\u500d\u306b\u306a\u308a\u307e\u3059\u3002"));}),_Bp=[0,_B8,_Bo,_Bn],_Bq=[0,_Bg,_Bj,_Bp],_Br=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u53f3\u76ee"));}),_Bs=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u53f3\u76ee<br>\u4f9d\u5b58\u5ea6\u30dc\u30fc\u30ca\u30b9\u304c10\u500d\u306b\u306a\u308a\u307e\u3059\u3002"));}),_Bt=new T(function(){return B(unCStr("fa-eye"));}),_Bu=[0,_Bt,_Bs,_Br],_Bv=[0,10],_Bw=function(_Bx,_By,_){return [0,_b6,new T(function(){var _Bz=E(_By);return [0,_Bz[1],_Bz[2],_Bz[3],_Bz[4],_Bz[5],_Bz[6],_Bz[7],_Bz[8],_Bv,_Bz[10]];})];},_BA=function(_BB){return E(_y8);},_BC=[0,_BA,_Bw,_Bu],_BD=function(_BE,_BF,_){return [0,_b6,new T(function(){var _BG=E(_BF);return [0,_BG[1],_BG[2],_BG[3],_BG[4],_BG[5],_BG[6],_BG[7],_BG[8],_BG[9],_Bv];})];},_BH=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u5de6\u76ee"));}),_BI=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u5de6\u76ee<br>\u4f9d\u5b58\u5ea6\u304c\u597d\u611f\u5ea6\u306b\u5909\u308f\u308b\u901f\u3055\u304c10\u500d\u306b\u306a\u308a\u307e\u3059\u3002"));}),_BJ=[0,_Bt,_BI,_BH],_BK=[0,_BA,_BD,_BJ],_BL=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u30b7\u30e7\u30c3\u30d7"));}),_BM=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u30b7\u30e7\u30c3\u30d7<br>\u30a2\u30a4\u30c6\u30e0\u304c\u8cfc\u5165\u3067\u304d\u308b\u3088\u3046\u306b\u306a\u308a\u307e\u3059\u3002"));}),_BN=new T(function(){return B(unCStr("fa-shopping-cart"));}),_BO=[0,_BN,_BM,_BL],_BP=function(_BQ,_BR,_BS,_BT){return new F(function(){return A(_BQ,[function(_){var _BU=jsSetStyle(E(_BR)[1],toJSStr(E(_BS)),toJSStr(E(_BT)));return _b6;}]);});},_BV=function(_BW,_){var _BX=B(_kY(_)),_BY=_BX;return [0,_b6,new T(function(){var _BZ=E(_BW);return [0,_BZ[1],_BZ[2],_BZ[3],_BY,_BZ[5],_BZ[6],_BZ[7],_BZ[8],_BZ[9],_BZ[10]];})];},_C0=new T(function(){return B(unCStr("block"));}),_C1=function(_C2,_){return [0,_b6,_C2];},_C3=function(_C4,_C5,_){var _C6=B(A(_C4,[_])),_C7=_C6;return new F(function(){return A(_C5,[_C7,_]);});},_C8=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_C9=new T(function(){return B(unCStr("base"));}),_Ca=new T(function(){return B(unCStr("IOException"));}),_Cb=new T(function(){var _Cc=hs_wordToWord64(4053623282),_Cd=_Cc,_Ce=hs_wordToWord64(3693590983),_Cf=_Ce;return [0,_Cd,_Cf,[0,_Cd,_Cf,_C9,_C8,_Ca],_u];}),_Cg=function(_Ch){return E(_Cb);},_Ci=function(_Cj){var _Ck=E(_Cj);return new F(function(){return _8K(B(_8I(_Ck[1])),_Cg,_Ck[2]);});},_Cl=new T(function(){return B(unCStr(": "));}),_Cm=[0,41],_Cn=new T(function(){return B(unCStr(" ("));}),_Co=new T(function(){return B(unCStr("already exists"));}),_Cp=new T(function(){return B(unCStr("does not exist"));}),_Cq=new T(function(){return B(unCStr("protocol error"));}),_Cr=new T(function(){return B(unCStr("failed"));}),_Cs=new T(function(){return B(unCStr("invalid argument"));}),_Ct=new T(function(){return B(unCStr("inappropriate type"));}),_Cu=new T(function(){return B(unCStr("hardware fault"));}),_Cv=new T(function(){return B(unCStr("unsupported operation"));}),_Cw=new T(function(){return B(unCStr("timeout"));}),_Cx=new T(function(){return B(unCStr("resource vanished"));}),_Cy=new T(function(){return B(unCStr("interrupted"));}),_Cz=new T(function(){return B(unCStr("resource busy"));}),_CA=new T(function(){return B(unCStr("resource exhausted"));}),_CB=new T(function(){return B(unCStr("end of file"));}),_CC=new T(function(){return B(unCStr("illegal operation"));}),_CD=new T(function(){return B(unCStr("permission denied"));}),_CE=new T(function(){return B(unCStr("user error"));}),_CF=new T(function(){return B(unCStr("unsatisified constraints"));}),_CG=new T(function(){return B(unCStr("system error"));}),_CH=function(_CI,_CJ){switch(E(_CI)){case 0:return new F(function(){return _O(_Co,_CJ);});break;case 1:return new F(function(){return _O(_Cp,_CJ);});break;case 2:return new F(function(){return _O(_Cz,_CJ);});break;case 3:return new F(function(){return _O(_CA,_CJ);});break;case 4:return new F(function(){return _O(_CB,_CJ);});break;case 5:return new F(function(){return _O(_CC,_CJ);});break;case 6:return new F(function(){return _O(_CD,_CJ);});break;case 7:return new F(function(){return _O(_CE,_CJ);});break;case 8:return new F(function(){return _O(_CF,_CJ);});break;case 9:return new F(function(){return _O(_CG,_CJ);});break;case 10:return new F(function(){return _O(_Cq,_CJ);});break;case 11:return new F(function(){return _O(_Cr,_CJ);});break;case 12:return new F(function(){return _O(_Cs,_CJ);});break;case 13:return new F(function(){return _O(_Ct,_CJ);});break;case 14:return new F(function(){return _O(_Cu,_CJ);});break;case 15:return new F(function(){return _O(_Cv,_CJ);});break;case 16:return new F(function(){return _O(_Cw,_CJ);});break;case 17:return new F(function(){return _O(_Cx,_CJ);});break;default:return new F(function(){return _O(_Cy,_CJ);});}},_CK=[0,125],_CL=new T(function(){return B(unCStr("{handle: "));}),_CM=function(_CN,_CO,_CP,_CQ,_CR,_CS){var _CT=new T(function(){var _CU=new T(function(){return B(_CH(_CO,new T(function(){var _CV=E(_CQ);return _CV[0]==0?E(_CS):B(_O(_Cn,new T(function(){return B(_O(_CV,[1,_Cm,_CS]));})));})));}),_CW=E(_CP);return _CW[0]==0?E(_CU):B(_O(_CW,new T(function(){return B(_O(_Cl,_CU));})));}),_CX=E(_CR);if(!_CX[0]){var _CY=E(_CN);if(!_CY[0]){return E(_CT);}else{var _CZ=E(_CY[1]);return _CZ[0]==0?B(_O(_CL,new T(function(){return B(_O(_CZ[1],[1,_CK,new T(function(){return B(_O(_Cl,_CT));})]));}))):B(_O(_CL,new T(function(){return B(_O(_CZ[1],[1,_CK,new T(function(){return B(_O(_Cl,_CT));})]));})));}}else{return new F(function(){return _O(_CX[1],new T(function(){return B(_O(_Cl,_CT));}));});}},_D0=function(_D1){var _D2=E(_D1);return new F(function(){return _CM(_D2[1],_D2[2],_D2[3],_D2[4],_D2[6],_u);});},_D3=function(_D4,_D5){var _D6=E(_D4);return new F(function(){return _CM(_D6[1],_D6[2],_D6[3],_D6[4],_D6[6],_D5);});},_D7=function(_D8,_D9){return new F(function(){return _95(_D3,_D8,_D9);});},_Da=function(_Db,_Dc,_Dd){var _De=E(_Dc);return new F(function(){return _CM(_De[1],_De[2],_De[3],_De[4],_De[6],_Dd);});},_Df=[0,_Da,_D0,_D7],_Dg=new T(function(){return [0,_Cg,_Df,_Dh,_Ci];}),_Dh=function(_Di){return [0,_Dg,_Di];},_Dj=7,_Dk=function(_Dl){return [0,_2z,_Dj,_u,_Dl,_2z,_2z];},_Dm=function(_Dn,_){return new F(function(){return die(new T(function(){return B(_Dh(new T(function(){return B(_Dk(_Dn));})));}));});},_Do=function(_Dp,_){return new F(function(){return _Dm(_Dp,_);});},_Dq=function(_Dr,_){return _Dr;},_Ds=function(_Dt,_Du,_){var _Dv=B(A(_Dt,[_])),_Dw=_Dv;return new F(function(){return A(_Du,[_]);});},_Dx=[0,_C3,_Ds,_Dq,_Do],_Dy=[0,_Dx,_bK],_Dz=function(_DA){return E(E(_DA)[1]);},_DB=function(_DC){return E(E(_DC)[1]);},_DD=function(_DE){return E(E(_DE)[2]);},_DF=function(_DG){return E(E(_DG)[3]);},_DH=function(_DI,_DJ){var _DK=new T(function(){return B(_Dz(_DI));});return function(_DL){return new F(function(){return A(new T(function(){return B(_DB(_DK));}),[new T(function(){return B(A(_DD,[_DI,_DJ]));}),function(_DM){return new F(function(){return A(new T(function(){return B(_DF(_DK));}),[[0,_DM,_DL]]);});}]);});};},_DN=function(_DO){return new F(function(){return _DH(_Dy,_DO);});},_DP=new T(function(){return B(unCStr("display"));}),_DQ=new T(function(){return B(unCStr("monitor"));}),_DR=function(_DS){return function(_DT,_){var _DU=E(_DS),_DV=jsFind(toJSStr(_DU)),_DW=_DV,_DX=E(_DW);if(!_DX[0]){return new F(function(){return _tU(_DU);});}else{var _DY=B(A(_BP,[_DN,_DX[1],_DP,_C0,_DT,_])),_DZ=_DY;return new F(function(){return A(new T(function(){return !B(_ax(_DS,_DQ))?E(_C1):E(_BV);}),[new T(function(){return E(E(_DZ)[2]);}),_]);});}};},_E0=new T(function(){return B(unCStr("item-shop"));}),_E1=new T(function(){return B(_DR(_E0));}),_E2=function(_E3){return E(_E1);},_E4=function(_E5){return E(_sO);},_E6=[0,_E4,_E2,_BO],_E7=new T(function(){return B(unCStr("\u3055\u3041\u59cb\u3081\u3088\u3046"));}),_E8=new T(function(){return B(unCStr("\u3055\u3041\u59cb\u3081\u3088\u3046<br>\u30b2\u30fc\u30e0\u3092\u59cb\u3081\u307e\u3057\u3087\u3046\u3002\u53f3\u306e\u30dc\u30bf\u30f3\u304b\u3089\u3053\u306e\u30a2\u30a4\u30c6\u30e0\u3092\u8cfc\u5165\u3057\u3066\u304f\u3060\u3055\u3044\u3002"));}),_E9=new T(function(){return B(unCStr("fa-power-off"));}),_Ea=[0,_E9,_E8,_E7],_Eb=new T(function(){return B(_DR(_DQ));}),_Ec=function(_Ed){return E(_Eb);},_Ee=function(_Ef){return E(_zj);},_Eg=[0,_Ee,_Ec,_Ea],_Eh=new T(function(){return B(unCStr("\u30c7\u30fc\u30bf\u306e\u6d88\u53bb"));}),_Ei=new T(function(){return B(unCStr("\u30c7\u30fc\u30bf\u306e\u6d88\u53bb<br>\u5168\u3066\u306e\u30c7\u30fc\u30bf\u304c\u6d88\u53bb\u3055\u308c\u307e\u3059\u3002\u3053\u306e\u64cd\u4f5c\u306f\u53d6\u308a\u6d88\u305b\u307e\u305b\u3093\u3002"));}),_Ej=new T(function(){return B(unCStr("fa-trash"));}),_Ek=[0,_Ej,_Ei,_Eh],_El=function(_Em){return E(_xu);},_En=new T(function(){return B(unCStr("none"));}),_Eo=function(_Ep,_Eq,_){var _Er=B(_kY(_)),_Es=_Er,_Et=[0,_l2,_l2,_l2,_Es,_r,_48,_80,_l2,_l3,_l3],_Eu=B(A(_x5,[_Et,_])),_Ev=_Eu,_Ew=B(_xi(_Et,_)),_Ex=_Ew,_Ey=E(_DQ),_Ez=jsFind(toJSStr(_Ey)),_EA=_Ez,_EB=E(_EA);if(!_EB[0]){return new F(function(){return _tU(_Ey);});}else{var _EC=B(A(_BP,[_DN,_EB[1],_DP,_En,new T(function(){return E(E(_Ex)[2]);}),_])),_ED=_EC,_EE=E(_E0),_EF=jsFind(toJSStr(_EE)),_EG=_EF,_EH=E(_EG);return _EH[0]==0?B(_tU(_EE)):B(A(_BP,[_DN,_EH[1],_DP,_En,new T(function(){return E(E(_ED)[2]);}),_]));}},_EI=new T(function(){return [0,_El,_Eo,_Ek];}),_EJ=new T(function(){return B(unCStr("\u521d\u671f\u5316"));}),_EK=new T(function(){return B(unCStr("\u521d\u671f\u5316<br>\u5b9f\u7e3e\u3092\u9664\u304f\u5168\u3066\u306e\u30c7\u30fc\u30bf\u304c\u521d\u671f\u5316\u3055\u308c\u307e\u3059"));}),_EL=new T(function(){return B(unCStr("fa-history"));}),_EM=[0,_EL,_EK,_EJ],_EN=function(_EO){return E(_xU);},_EP=function(_EQ,_ER,_){var _ES=B(_kY(_)),_ET=_ES,_EU=[0,_l2,_l2,_l2,_ET,_r,new T(function(){return E(E(_ER)[6]);}),_80,_l2,_l3,_l3],_EV=B(A(_x5,[_EU,_])),_EW=_EV,_EX=B(_xi(_EU,_)),_EY=_EX,_EZ=E(_DQ),_F0=jsFind(toJSStr(_EZ)),_F1=_F0,_F2=E(_F1);if(!_F2[0]){return new F(function(){return _tU(_EZ);});}else{var _F3=B(A(_BP,[_DN,_F2[1],_DP,_En,new T(function(){return E(E(_EY)[2]);}),_])),_F4=_F3,_F5=E(_E0),_F6=jsFind(toJSStr(_F5)),_F7=_F6,_F8=E(_F7);return _F8[0]==0?B(_tU(_F5)):B(A(_BP,[_DN,_F8[1],_DP,_En,new T(function(){return E(E(_F4)[2]);}),_]));}},_F9=new T(function(){return [0,_EN,_EP,_EM];}),_Fa=function(_Fb,_Fc,_Fd){return _Fd<=_Fc?[1,[0,_Fb],new T(function(){var _Fe=_Fc-_Fb|0,_Ff=function(_Fg){return _Fg>=(_Fd-_Fe|0)?[1,[0,_Fg],new T(function(){return B(_Ff(_Fg+_Fe|0));})]:[1,[0,_Fg],_u];};return B(_Ff(_Fc));})]:_Fd<=_Fb?[1,[0,_Fb],_u]:[0];},_Fh=function(_Fi,_Fj,_Fk){return _Fk>=_Fj?[1,[0,_Fi],new T(function(){var _Fl=_Fj-_Fi|0,_Fm=function(_Fn){return _Fn<=(_Fk-_Fl|0)?[1,[0,_Fn],new T(function(){return B(_Fm(_Fn+_Fl|0));})]:[1,[0,_Fn],_u];};return B(_Fm(_Fj));})]:_Fk>=_Fi?[1,[0,_Fi],_u]:[0];},_Fo=function(_Fp,_Fq){return _Fq<_Fp?B(_Fa(_Fp,_Fq,-2147483648)):B(_Fh(_Fp,_Fq,2147483647));},_Fr=new T(function(){return B(_Fo(-1,-2));}),_Fs=new T(function(){var _Ft=E(_Fr);return _Ft[0]==0?[0]:[1,[0,_Ft[1],_Eg],new T(function(){var _Fu=E(_Ft[2]);return _Fu[0]==0?[0]:[1,[0,_Fu[1],_E6],new T(function(){var _Fv=E(_Fu[2]);return _Fv[0]==0?[0]:[1,[0,_Fv[1],_BC],new T(function(){var _Fw=E(_Fv[2]);return _Fw[0]==0?[0]:[1,[0,_Fw[1],_BK],new T(function(){var _Fx=E(_Fw[2]);return _Fx[0]==0?[0]:[1,[0,_Fx[1],_Bi],new T(function(){var _Fy=E(_Fx[2]);return _Fy[0]==0?[0]:[1,[0,_Fy[1],_Bq],new T(function(){var _Fz=E(_Fy[2]);return _Fz[0]==0?[0]:[1,[0,_Fz[1],_F9],new T(function(){var _FA=E(_Fz[2]);return _FA[0]==0?[0]:[1,[0,_FA[1],_EI],_u];})];})];})];})];})];})];})];}),_FB=new T(function(){var _FC=B(_yr(1,2147483647));return _FC[0]==0?E(_Fs):[1,[0,_FC[1],_B5],new T(function(){var _FD=E(_FC[2]);return _FD[0]==0?E(_Fs):[1,[0,_FD[1],_AU],new T(function(){var _FE=E(_FD[2]);return _FE[0]==0?E(_Fs):[1,[0,_FE[1],_AJ],new T(function(){var _FF=E(_FE[2]);return _FF[0]==0?E(_Fs):[1,[0,_FF[1],_Ay],new T(function(){var _FG=E(_FF[2]);return _FG[0]==0?E(_Fs):[1,[0,_FG[1],_An],new T(function(){var _FH=E(_FG[2]);return _FH[0]==0?E(_Fs):[1,[0,_FH[1],_Ac],new T(function(){var _FI=E(_FH[2]);return _FI[0]==0?E(_Fs):[1,[0,_FI[1],_A0],_Fs];})];})];})];})];})];})];}),_FJ=new T(function(){return B(_8s(_80,_FB));}),_FK=function(_FL,_FM,_FN){return function(_FO,_){var _FP=E(_FO),_FQ=_FP[7],_FR=E(_FL)[1];return !B(_rC(_FR,_FQ))?[0,_b6,_FP]:B(_rv(_FQ,_FR))[1]<E(_FM)[1]?[0,_b6,_FP]:B(A(new T(function(){return B(_x8(new T(function(){return B(_O(_yp,new T(function(){return B(_O(E(B(_rv(_FJ,E(_FL)[1]))[3])[3],new T(function(){return B(_O(_yo,new T(function(){return B(_O(B(_dQ(0,E(_FM)[1],_u)),_yq));})));})));})));}),_FN));}),[_FP,_]));};},_FS=new T(function(){return B(unCStr("\u304a\u3057\u3083\u3079\u308a\u611b\u3061\u3083\u3093"));}),_FT=new T(function(){return B(_FK(_sa,_yn,_FS));}),_FU=new T(function(){return [0,_FS,_FT];}),_FV=[0,200],_FW=new T(function(){return B(unCStr("\u3042\u3001\u3046\u3093"));}),_FX=new T(function(){return B(_FK(_sa,_FV,_FW));}),_FY=new T(function(){return [0,_FW,_FX];}),_FZ=[0,50],_G0=[0,3],_G1=new T(function(){return B(unCStr("\u55ab\u8336\u5e97\u306e\u30dd\u30a4\u30f3\u30c8\u30ab\u30fc\u30c9"));}),_G2=new T(function(){return B(_FK(_G0,_FZ,_G1));}),_G3=new T(function(){return [0,_G1,_G2];}),_G4=[0,4],_G5=new T(function(){return B(unCStr("\u611b\u3068\u3044\u3046\u540d\u306e\u30d7\u30ec\u30bc\u30f3\u30c8"));}),_G6=new T(function(){return B(_FK(_G4,_FZ,_G5));}),_G7=new T(function(){return [0,_G5,_G6];}),_G8=function(_G9,_Ga){while(1){var _Gb=E(_Ga);if(!_Gb[0]){return true;}else{if(!B(A(_G9,[_Gb[1]]))){return false;}else{_Ga=_Gb[2];continue;}}}},_Gc=new T(function(){return B(unCStr("\u5168\u3066\u306e\u901a\u5e38\u30a2\u30a4\u30c6\u30e0\u3092"));}),_Gd=function(_Ge,_Gf){while(1){var _Gg=(function(_Gh,_Gi){var _Gj=E(_Gi);switch(_Gj[0]){case 0:_Ge=new T(function(){return B(_Gd(_Gh,_Gj[4]));});_Gf=_Gj[3];return null;case 1:return [1,[0,_Gj[1]],_Gh];default:return E(_Gh);}})(_Ge,_Gf);if(_Gg!=null){return _Gg;}}},_Gk=function(_Gl){var _Gm=E(_Gl);if(!_Gm[0]){var _Gn=_Gm[3],_Go=_Gm[4];return _Gm[2]>=0?B(_Gd(new T(function(){return B(_Gd(_u,_Go));}),_Gn)):B(_Gd(new T(function(){return B(_Gd(_u,_Gn));}),_Go));}else{return new F(function(){return _Gd(_u,_Gm);});}},_Gp=function(_Gq,_Gr){while(1){var _Gs=E(_Gr);switch(_Gs[0]){case 0:var _Gt=_Gs[3],_Gu=B(_Gp(_Gq,_Gs[4]));if(_Gu[0]==2){_Gr=_Gt;continue;}else{var _Gv=B(_Gp(_Gq,_Gt));return _Gv[0]==2?E(_Gu):[0,_Gs[1],_Gs[2],E(_Gv),E(_Gu)];}break;case 1:return !B(A(_Gq,[[0,_Gs[1]],_Gs[2]]))?[2]:E(_Gs);default:return [2];}}},_Gw=function(_Gx,_Gy){return E(_Gx)[1]>0;},_Gz=new T(function(){return B(_Gp(_Gw,_FJ));}),_GA=new T(function(){return B(_Gk(_Gz));}),_GB=function(_GC,_GD){return function(_GE,_){var _GF=new T(function(){return E(E(_GE)[7]);});return !B(_G8(function(_GG){var _GH=E(_GG)[1];return !B(_rC(_GH,_GF))?false:B(_rv(_GF,_GH))[1]>=E(_GC)[1];},_GA))?[0,_b6,_GE]:B(A(new T(function(){return B(_x8(new T(function(){return B(_O(_Gc,new T(function(){return B(_O(B(_dQ(0,E(_GC)[1],_u)),_yq));})));}),_GD));}),[_GE,_]));};},_GI=new T(function(){return B(unCStr("\u30b3\u30f3\u30d7\u30ea\u30fc\u30c8"));}),_GJ=new T(function(){return B(_GB(_sa,_GI));}),_GK=new T(function(){return [0,_GI,_GJ];}),_GL=[0,10],_GM=new T(function(){return B(unCStr("\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30f3\u30d7\u30ea\u30fc\u30c8"));}),_GN=new T(function(){return B(_GB(_GL,_GM));}),_GO=new T(function(){return [0,_GM,_GN];}),_GP=new T(function(){return [1,_GO,_u];}),_GQ=new T(function(){return [1,_GK,_GP];}),_GR=new T(function(){return [1,_G7,_GQ];}),_GS=new T(function(){return [1,_G3,_GR];}),_GT=new T(function(){return [1,_FY,_GS];}),_GU=new T(function(){return [1,_FU,_GT];}),_GV=new T(function(){return [1,_ym,_GU];}),_GW=new T(function(){return [1,_yj,_GV];}),_GX=new T(function(){return [1,_ya,_GW];}),_GY=new T(function(){return [1,_y6,_GX];}),_GZ=new T(function(){return [1,_y3,_GY];}),_H0=new T(function(){return [1,_xZ,_GZ];}),_H1=new T(function(){return [1,_xW,_H0];}),_H2=new T(function(){return [1,_xM,_H1];}),_H3=new T(function(){return [1,_xI,_H2];}),_H4=new T(function(){return [1,_xE,_H3];}),_H5=new T(function(){return [1,_xA,_H4];}),_H6=new T(function(){return [1,_xw,_H5];}),_H7=new T(function(){return B(unCStr("</tbody>"));}),_H8=new T(function(){return B(unCStr("<tr><td>%s</td><td>%s</td></tr>"));}),_H9=new T(function(){return B(unCStr("<thead><tr><th>\u5b9f\u7e3e\u540d</th><th>\u5185\u5bb9</th></tr></thead>"));}),_Ha=function(_Hb,_Hc){while(1){var _Hd=E(_Hb);if(!_Hd[0]){return E(_Hc);}else{_Hb=_Hd[2];var _He=[1,_Hd[1],_Hc];_Hc=_He;continue;}}},_Hf=function(_Hg){var _Hh=E(_Hg)[1];return [0,Math.log(_Hh+(_Hh+1)*Math.sqrt((_Hh-1)/(_Hh+1)))];},_Hi=function(_Hj){var _Hk=E(_Hj)[1];return [0,Math.log(_Hk+Math.sqrt(1+_Hk*_Hk))];},_Hl=function(_Hm){var _Hn=E(_Hm)[1];return [0,0.5*Math.log((1+_Hn)/(1-_Hn))];},_Ho=function(_Hp,_Hq){return [0,Math.log(E(_Hq)[1])/Math.log(E(_Hp)[1])];},_Hr=[0,3.141592653589793],_Hs=function(_Ht){var _Hu=E(_Ht);return new F(function(){return _zB(_Hu[1],_Hu[2]);});},_Hv=function(_Hw){return [0,1/E(_Hw)[1]];},_Hx=function(_Hy){var _Hz=E(_Hy),_HA=_Hz[1];return _HA<0?[0, -_HA]:E(_Hz);},_HB=function(_HC){return [0,B(_xj(_HC))];},_HD=[0,0],_HE=[0,1],_HF=[0,-1],_HG=function(_HH){var _HI=E(E(_HH)[1]);return _HI==0?E(_HD):_HI<=0?E(_HF):E(_HE);},_HJ=function(_HK,_HL){return [0,E(_HK)[1]-E(_HL)[1]];},_HM=function(_HN){return [0, -E(_HN)[1]];},_HO=function(_HP,_HQ){return [0,E(_HP)[1]+E(_HQ)[1]];},_HR=function(_HS,_HT){return [0,E(_HS)[1]*E(_HT)[1]];},_HU=[0,_HO,_HR,_HJ,_HM,_Hx,_HG,_HB],_HV=function(_HW,_HX){return [0,E(_HW)[1]/E(_HX)[1]];},_HY=[0,_HU,_HV,_Hv,_Hs],_HZ=function(_I0){return [0,Math.acos(E(_I0)[1])];},_I1=function(_I2){return [0,Math.asin(E(_I2)[1])];},_I3=function(_I4){return [0,Math.atan(E(_I4)[1])];},_I5=function(_I6){return [0,Math.cos(E(_I6)[1])];},_I7=function(_I8){return [0,cosh(E(_I8)[1])];},_I9=function(_Ia){return [0,Math.exp(E(_Ia)[1])];},_Ib=function(_Ic){return [0,Math.log(E(_Ic)[1])];},_Id=function(_Ie,_If){return [0,Math.pow(E(_Ie)[1],E(_If)[1])];},_Ig=function(_Ih){return [0,Math.sin(E(_Ih)[1])];},_Ii=function(_Ij){return [0,sinh(E(_Ij)[1])];},_Ik=function(_Il){return [0,Math.sqrt(E(_Il)[1])];},_Im=function(_In){return [0,Math.tan(E(_In)[1])];},_Io=function(_Ip){return [0,tanh(E(_Ip)[1])];},_Iq=[0,_HY,_Hr,_I9,_Ik,_Ib,_Id,_Ho,_Ig,_Im,_I5,_I1,_I3,_HZ,_Ii,_Io,_I7,_Hi,_Hl,_Hf],_Ir=function(_Is){var _It=E(_Is)[1];return [0,Math.log(_It+(_It+1)*Math.sqrt((_It-1)/(_It+1)))];},_Iu=function(_Iv){var _Iw=E(_Iv)[1];return [0,Math.log(_Iw+Math.sqrt(1+_Iw*_Iw))];},_Ix=function(_Iy){var _Iz=E(_Iy)[1];return [0,0.5*Math.log((1+_Iz)/(1-_Iz))];},_IA=function(_IB,_IC){return [0,Math.log(E(_IC)[1])/Math.log(E(_IB)[1])];},_ID=[0,3.141592653589793],_IE=new T(function(){return [0,0/0];}),_IF=new T(function(){return [0,-1/0];}),_IG=new T(function(){return [0,1/0];}),_IH=function(_II,_IJ){return !B(_zt(_IJ,_zn))?[0,B(_zo(_II,_IJ))]:!B(_zt(_II,_zn))?!B(_Y(_II,_zn))?E(_IG):E(_IF):E(_IE);},_IK=function(_IL){var _IM=E(_IL);return new F(function(){return _IH(_IM[1],_IM[2]);});},_IN=function(_IO){return [0,1/E(_IO)[1]];},_IP=function(_IQ){var _IR=E(_IQ),_IS=_IR[1];return _IS<0?[0, -_IS]:E(_IR);},_IT=function(_IU){var _IV=E(_IU);return _IV[0]==0?_IV[1]:I_toNumber(_IV[1]);},_IW=function(_IX){return [0,B(_IT(_IX))];},_IY=[0,0],_IZ=[0,1],_J0=[0,-1],_J1=function(_J2){var _J3=E(E(_J2)[1]);return _J3==0?E(_IY):_J3<=0?E(_J0):E(_IZ);},_J4=function(_J5,_J6){return [0,E(_J5)[1]-E(_J6)[1]];},_J7=function(_J8){return [0, -E(_J8)[1]];},_J9=function(_Ja,_Jb){return [0,E(_Ja)[1]+E(_Jb)[1]];},_Jc=function(_Jd,_Je){return [0,E(_Jd)[1]*E(_Je)[1]];},_Jf=[0,_J9,_Jc,_J4,_J7,_IP,_J1,_IW],_Jg=function(_Jh,_Ji){return [0,E(_Jh)[1]/E(_Ji)[1]];},_Jj=[0,_Jf,_Jg,_IN,_IK],_Jk=function(_Jl){return [0,Math.acos(E(_Jl)[1])];},_Jm=function(_Jn){return [0,Math.asin(E(_Jn)[1])];},_Jo=function(_Jp){return [0,Math.atan(E(_Jp)[1])];},_Jq=function(_Jr){return [0,Math.cos(E(_Jr)[1])];},_Js=function(_Jt){return [0,cosh(E(_Jt)[1])];},_Ju=function(_Jv){return [0,Math.exp(E(_Jv)[1])];},_Jw=function(_Jx){return [0,Math.log(E(_Jx)[1])];},_Jy=function(_Jz,_JA){return [0,Math.pow(E(_Jz)[1],E(_JA)[1])];},_JB=function(_JC){return [0,Math.sin(E(_JC)[1])];},_JD=function(_JE){return [0,sinh(E(_JE)[1])];},_JF=function(_JG){return [0,Math.sqrt(E(_JG)[1])];},_JH=function(_JI){return [0,Math.tan(E(_JI)[1])];},_JJ=function(_JK){return [0,tanh(E(_JK)[1])];},_JL=[0,_Jj,_ID,_Ju,_JF,_Jw,_Jy,_IA,_JB,_JH,_Jq,_Jm,_Jo,_Jk,_JD,_JJ,_Js,_Iu,_Ix,_Ir],_JM=function(_JN){var _JO=B(_yX(E(_JN)[1]));return [0,_JO[1],[0,_JO[2]]];},_JP=[0,53],_JQ=function(_JR){return E(_JP);},_JS=[0,2],_JT=function(_JU){return E(_JS);},_JV=[0,1024],_JW=[0,-1021],_JX=[0,_JW,_JV],_JY=function(_JZ){return E(_JX);},_K0=function(_K1){var _K2=isDoubleInfinite(E(_K1)[1]),_K3=_K2;return E(_K3)==0?false:true;},_K4=function(_K5){var _K6=isDoubleNaN(E(_K5)[1]),_K7=_K6;return E(_K7)==0?false:true;},_K8=function(_K9){var _Ka=isDoubleNegativeZero(E(_K9)[1]),_Kb=_Ka;return E(_Kb)==0?false:true;},_Kc=function(_Kd){var _Ke=decodeFloat(E(_Kd)[1]);return [0,new T(function(){return B(_cA(_Ke[1]));}),[0,_Ke[2]]];},_Kf=[0,24],_Kg=function(_Kh){return E(_Kf);},_Ki=function(_Kj){return E(_JS);},_Kk=[0,128],_Kl=[0,-125],_Km=[0,_Kl,_Kk],_Kn=function(_Ko){return E(_Km);},_Kp=function(_Kq){var _Kr=isFloatInfinite(E(_Kq)[1]),_Ks=_Kr;return E(_Ks)==0?false:true;},_Kt=function(_Ku){var _Kv=isFloatNaN(E(_Ku)[1]),_Kw=_Kv;return E(_Kw)==0?false:true;},_Kx=function(_Ky){var _Kz=isFloatNegativeZero(E(_Ky)[1]),_KA=_Kz;return E(_KA)==0?false:true;},_KB=function(_KC,_KD){return E(_KC)[1]!=E(_KD)[1]?true:false;},_KE=function(_KF,_KG){return E(_KF)[1]==E(_KG)[1];},_KH=[0,_KE,_KB],_KI=function(_KJ,_KK){return E(_KJ)[1]<E(_KK)[1];},_KL=function(_KM,_KN){return E(_KM)[1]<=E(_KN)[1];},_KO=function(_KP,_KQ){return E(_KP)[1]>E(_KQ)[1];},_KR=function(_KS,_KT){return E(_KS)[1]>=E(_KT)[1];},_KU=function(_KV,_KW){var _KX=E(_KV)[1],_KY=E(_KW)[1];return _KX>=_KY?_KX!=_KY?2:1:0;},_KZ=function(_L0,_L1){var _L2=E(_L0),_L3=E(_L1);return _L2[1]>_L3[1]?E(_L2):E(_L3);},_L4=function(_L5,_L6){var _L7=E(_L5),_L8=E(_L6);return _L7[1]>_L8[1]?E(_L8):E(_L7);},_L9=[0,_KH,_KU,_KI,_KR,_KO,_KL,_KZ,_L4],_La=[0,1],_Lb=new T(function(){var _Lc=newByteArr(256),_Ld=_Lc,_=_Ld["v"]["i8"][0]=8,_=B((function(_Le,_Lf,_Lg,_){while(1){if(_Lg>=256){if(_Le>=256){return E(_);}else{var _Lh=imul(2,_Le)|0,_Li=_Lf+1|0,_Lj=_Le;_Le=_Lh;_Lf=_Li;_Lg=_Lj;continue;}}else{var _=_Ld["v"]["i8"][_Lg]=_Lf,_Lj=_Lg+_Le|0;_Lg=_Lj;continue;}}})(2,0,1,_)),_Lk=_Ld,_Ll=_Lk;return [0,_Ll];}),_Lm=function(_Ln,_Lo){while(1){var _Lp=(function(_Lq,_Lr){var _Ls=hs_int64ToInt(_Lq),_Lt=_Ls,_Lu=E(_Lb)[1]["v"]["i8"][(255&_Lt>>>0)>>>0&4294967295];if(_Lr>_Lu){if(_Lu>=8){var _Lv=hs_uncheckedIShiftRA64(_Lq,8),_Lw=_Lv;_Ln=_Lw;var _Lx=_Lr-8|0;_Lo=_Lx;return null;}else{return [0,new T(function(){var _Ly=hs_uncheckedIShiftRA64(_Lq,_Lu),_Lz=_Ly;return B(_z0(_Lz));}),_Lr-_Lu|0];}}else{return [0,new T(function(){var _LA=hs_uncheckedIShiftRA64(_Lq,_Lr),_LB=_LA;return B(_z0(_LB));}),0];}})(_Ln,_Lo);if(_Lp!=null){return _Lp;}}},_LC=function(_LD){return I_toInt(_LD)>>>0;},_LE=function(_LF){var _LG=E(_LF);return _LG[0]==0?_LG[1]>>>0:B(_LC(_LG[1]));},_LH=function(_LI){var _LJ=B(_yX(_LI)),_LK=_LJ[1],_LL=_LJ[2];if(_LL<0){var _LM=function(_LN){if(!_LN){return [0,E(_LK),B(_zI(_La, -_LL))];}else{var _LO=B(_Lm(B(_zg(_LK)), -_LL));return [0,E(_LO[1]),B(_zI(_La,_LO[2]))];}};return (B(_LE(_LK))&1)>>>0==0?B(_LM(1)):B(_LM(0));}else{return [0,B(_zI(_LK,_LL)),_La];}},_LP=function(_LQ){var _LR=B(_LH(E(_LQ)[1]));return [0,E(_LR[1]),E(_LR[2])];},_LS=[0,_HU,_L9,_LP],_LT=function(_LU){return E(E(_LU)[1]);},_LV=[0,1],_LW=function(_LX){return new F(function(){return _yr(E(_LX)[1],2147483647);});},_LY=function(_LZ,_M0){return new F(function(){return _Fo(E(_LZ)[1],E(_M0)[1]);});},_M1=function(_M2,_M3,_M4){return _M3<_M2?B(_Fa(_M2,_M3,_M4)):B(_Fh(_M2,_M3,_M4));},_M5=function(_M6,_M7,_M8){return new F(function(){return _M1(E(_M6)[1],E(_M7)[1],E(_M8)[1]);});},_M9=function(_Ma,_Mb){return new F(function(){return _yr(E(_Ma)[1],E(_Mb)[1]);});},_Mc=function(_Md){return E(_Md);},_Me=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_Mf=new T(function(){return B(err(_Me));}),_Mg=function(_Mh){var _Mi=E(E(_Mh)[1]);return _Mi==(-2147483648)?E(_Mf):[0,_Mi-1|0];},_Mj=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_Mk=new T(function(){return B(err(_Mj));}),_Ml=function(_Mm){var _Mn=E(E(_Mm)[1]);return _Mn==2147483647?E(_Mk):[0,_Mn+1|0];},_Mo=[0,_Ml,_Mg,_Mc,_Mc,_LW,_LY,_M9,_M5],_Mp=function(_Mq,_Mr){if(_Mq<=0){if(_Mq>=0){return new F(function(){return quot(_Mq,_Mr);});}else{if(_Mr<=0){return new F(function(){return quot(_Mq,_Mr);});}else{return quot(_Mq+1|0,_Mr)-1|0;}}}else{if(_Mr>=0){if(_Mq>=0){return new F(function(){return quot(_Mq,_Mr);});}else{if(_Mr<=0){return new F(function(){return quot(_Mq,_Mr);});}else{return quot(_Mq+1|0,_Mr)-1|0;}}}else{return quot(_Mq-1|0,_Mr)-1|0;}}},_Ms=new T(function(){return B(unCStr("ArithException"));}),_Mt=new T(function(){return B(unCStr("GHC.Exception"));}),_Mu=new T(function(){return B(unCStr("base"));}),_Mv=new T(function(){var _Mw=hs_wordToWord64(4194982440),_Mx=_Mw,_My=hs_wordToWord64(3110813675),_Mz=_My;return [0,_Mx,_Mz,[0,_Mx,_Mz,_Mu,_Mt,_Ms],_u];}),_MA=function(_MB){return E(_Mv);},_MC=function(_MD){var _ME=E(_MD);return new F(function(){return _8K(B(_8I(_ME[1])),_MA,_ME[2]);});},_MF=new T(function(){return B(unCStr("arithmetic underflow"));}),_MG=new T(function(){return B(unCStr("arithmetic overflow"));}),_MH=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_MI=new T(function(){return B(unCStr("denormal"));}),_MJ=new T(function(){return B(unCStr("divide by zero"));}),_MK=new T(function(){return B(unCStr("loss of precision"));}),_ML=function(_MM){switch(E(_MM)){case 0:return E(_MG);case 1:return E(_MF);case 2:return E(_MK);case 3:return E(_MJ);case 4:return E(_MI);default:return E(_MH);}},_MN=function(_MO){return new F(function(){return _O(_MF,_MO);});},_MP=function(_MO){return new F(function(){return _O(_MG,_MO);});},_MQ=function(_MO){return new F(function(){return _O(_MH,_MO);});},_MR=function(_MO){return new F(function(){return _O(_MI,_MO);});},_MS=function(_MO){return new F(function(){return _O(_MJ,_MO);});},_MT=function(_MO){return new F(function(){return _O(_MK,_MO);});},_MU=function(_MV){switch(E(_MV)){case 0:return E(_MP);case 1:return E(_MN);case 2:return E(_MT);case 3:return E(_MS);case 4:return E(_MR);default:return E(_MQ);}},_MW=function(_MX,_MY){return new F(function(){return _95(_MU,_MX,_MY);});},_MZ=function(_N0,_N1){switch(E(_N1)){case 0:return E(_MP);case 1:return E(_MN);case 2:return E(_MT);case 3:return E(_MS);case 4:return E(_MR);default:return E(_MQ);}},_N2=[0,_MZ,_ML,_MW],_N3=new T(function(){return [0,_MA,_N2,_N4,_MC];}),_N4=function(_MO){return [0,_N3,_MO];},_N5=3,_N6=new T(function(){return B(_N4(_N5));}),_N7=new T(function(){return die(_N6);}),_N8=0,_N9=new T(function(){return B(_N4(_N8));}),_Na=new T(function(){return die(_N9);}),_Nb=function(_Nc,_Nd){var _Ne=E(_Nd);switch(_Ne){case -1:var _Nf=E(_Nc);return _Nf==(-2147483648)?E(_Na):B(_Mp(_Nf,-1));case 0:return E(_N7);default:return new F(function(){return _Mp(_Nc,_Ne);});}},_Ng=function(_Nh,_Ni){return [0,B(_Nb(E(_Nh)[1],E(_Ni)[1]))];},_Nj=[0,0],_Nk=[0,_Na,_Nj],_Nl=function(_Nm,_Nn){var _No=E(_Nm)[1],_Np=E(E(_Nn)[1]);switch(_Np){case -1:var _Nq=E(_No);if(_Nq==(-2147483648)){return E(_Nk);}else{if(_Nq<=0){if(_Nq>=0){var _Nr=quotRemI(_Nq,-1);return [0,[0,_Nr[1]],[0,_Nr[2]]];}else{var _Ns=quotRemI(_Nq,-1);return [0,[0,_Ns[1]],[0,_Ns[2]]];}}else{var _Nt=quotRemI(_Nq-1|0,-1);return [0,[0,_Nt[1]-1|0],[0,(_Nt[2]+(-1)|0)+1|0]];}}break;case 0:return E(_N7);default:if(_No<=0){if(_No>=0){var _Nu=quotRemI(_No,_Np);return [0,[0,_Nu[1]],[0,_Nu[2]]];}else{if(_Np<=0){var _Nv=quotRemI(_No,_Np);return [0,[0,_Nv[1]],[0,_Nv[2]]];}else{var _Nw=quotRemI(_No+1|0,_Np);return [0,[0,_Nw[1]-1|0],[0,(_Nw[2]+_Np|0)-1|0]];}}}else{if(_Np>=0){if(_No>=0){var _Nx=quotRemI(_No,_Np);return [0,[0,_Nx[1]],[0,_Nx[2]]];}else{if(_Np<=0){var _Ny=quotRemI(_No,_Np);return [0,[0,_Ny[1]],[0,_Ny[2]]];}else{var _Nz=quotRemI(_No+1|0,_Np);return [0,[0,_Nz[1]-1|0],[0,(_Nz[2]+_Np|0)-1|0]];}}}else{var _NA=quotRemI(_No-1|0,_Np);return [0,[0,_NA[1]-1|0],[0,(_NA[2]+_Np|0)+1|0]];}}}},_NB=function(_NC,_ND){var _NE=_NC%_ND;if(_NC<=0){if(_NC>=0){return E(_NE);}else{if(_ND<=0){return E(_NE);}else{var _NF=E(_NE);return _NF==0?0:_NF+_ND|0;}}}else{if(_ND>=0){if(_NC>=0){return E(_NE);}else{if(_ND<=0){return E(_NE);}else{var _NG=E(_NE);return _NG==0?0:_NG+_ND|0;}}}else{var _NH=E(_NE);return _NH==0?0:_NH+_ND|0;}}},_NI=function(_NJ,_NK){var _NL=E(E(_NK)[1]);switch(_NL){case -1:return E(_Nj);case 0:return E(_N7);default:return [0,B(_NB(E(_NJ)[1],_NL))];}},_NM=function(_NN,_NO){var _NP=E(_NN)[1],_NQ=E(E(_NO)[1]);switch(_NQ){case -1:var _NR=E(_NP);return _NR==(-2147483648)?E(_Na):[0,quot(_NR,-1)];case 0:return E(_N7);default:return [0,quot(_NP,_NQ)];}},_NS=function(_NT,_NU){var _NV=E(_NT)[1],_NW=E(E(_NU)[1]);switch(_NW){case -1:var _NX=E(_NV);if(_NX==(-2147483648)){return E(_Nk);}else{var _NY=quotRemI(_NX,-1);return [0,[0,_NY[1]],[0,_NY[2]]];}break;case 0:return E(_N7);default:var _NZ=quotRemI(_NV,_NW);return [0,[0,_NZ[1]],[0,_NZ[2]]];}},_O0=function(_O1,_O2){var _O3=E(E(_O2)[1]);switch(_O3){case -1:return E(_Nj);case 0:return E(_N7);default:return [0,E(_O1)[1]%_O3];}},_O4=function(_O5){return new F(function(){return _cA(E(_O5)[1]);});},_O6=function(_O7){return [0,E(B(_cA(E(_O7)[1]))),E(_LV)];},_O8=function(_O9,_Oa){return [0,imul(E(_O9)[1],E(_Oa)[1])|0];},_Ob=function(_Oc,_Od){return [0,E(_Oc)[1]+E(_Od)[1]|0];},_Oe=function(_Of,_Og){return [0,E(_Of)[1]-E(_Og)[1]|0];},_Oh=function(_Oi){var _Oj=E(_Oi),_Ok=_Oj[1];return _Ok<0?[0, -_Ok]:E(_Oj);},_Ol=function(_Om){return [0,B(_dY(_Om))];},_On=function(_Oo){return [0, -E(_Oo)[1]];},_Op=[0,-1],_Oq=[0,0],_Or=[0,1],_Os=function(_Ot){var _Ou=E(_Ot)[1];return _Ou>=0?E(_Ou)==0?E(_Oq):E(_Or):E(_Op);},_Ov=[0,_Ob,_O8,_Oe,_On,_Oh,_Os,_Ol],_Ow=function(_Ox,_Oy){return E(_Ox)[1]==E(_Oy)[1];},_Oz=function(_OA,_OB){return E(_OA)[1]!=E(_OB)[1];},_OC=[0,_Ow,_Oz],_OD=function(_OE,_OF){var _OG=E(_OE),_OH=E(_OF);return _OG[1]>_OH[1]?E(_OG):E(_OH);},_OI=function(_OJ,_OK){var _OL=E(_OJ),_OM=E(_OK);return _OL[1]>_OM[1]?E(_OM):E(_OL);},_ON=function(_OO,_OP){return _OO>=_OP?_OO!=_OP?2:1:0;},_OQ=function(_OR,_OS){return new F(function(){return _ON(E(_OR)[1],E(_OS)[1]);});},_OT=function(_OU,_OV){return E(_OU)[1]>=E(_OV)[1];},_OW=function(_OX,_OY){return E(_OX)[1]>E(_OY)[1];},_OZ=function(_P0,_P1){return E(_P0)[1]<=E(_P1)[1];},_P2=function(_P3,_P4){return E(_P3)[1]<E(_P4)[1];},_P5=[0,_OC,_OQ,_P2,_OT,_OW,_OZ,_OD,_OI],_P6=[0,_Ov,_P5,_O6],_P7=[0,_P6,_Mo,_NM,_O0,_Ng,_NI,_NS,_Nl,_O4],_P8=function(_P9){return E(E(_P9)[1]);},_Pa=function(_Pb,_Pc,_Pd){while(1){if(!(_Pc%2)){var _Pe=B(_cC(_Pb,_Pb)),_Pf=quot(_Pc,2);_Pb=_Pe;_Pc=_Pf;continue;}else{var _Pg=E(_Pc);if(_Pg==1){return new F(function(){return _cC(_Pb,_Pd);});}else{var _Pe=B(_cC(_Pb,_Pb));_Pc=quot(_Pg-1|0,2);var _Ph=B(_cC(_Pb,_Pd));_Pb=_Pe;_Pd=_Ph;continue;}}}},_Pi=function(_Pj,_Pk){while(1){if(!(_Pk%2)){var _Pl=B(_cC(_Pj,_Pj)),_Pm=quot(_Pk,2);_Pj=_Pl;_Pk=_Pm;continue;}else{var _Pn=E(_Pk);if(_Pn==1){return E(_Pj);}else{return new F(function(){return _Pa(B(_cC(_Pj,_Pj)),quot(_Pn-1|0,2),_Pj);});}}}},_Po=function(_Pp){return E(E(_Pp)[2]);},_Pq=function(_Pr){return E(E(_Pr)[1]);},_Ps=function(_Pt){return E(E(_Pt)[2]);},_Pu=[0,0],_Pv=[0,2],_Pw=function(_Px){return E(E(_Px)[7]);},_Py=function(_Pz,_PA,_PB,_PC,_PD){return new F(function(){return A(E(E(_PA)[1])[1],[new T(function(){return B(A(_PC,[_PD,new T(function(){return B(A(_Pw,[_Pz,_Pv]));})]));}),new T(function(){return B(A(_Pw,[_Pz,_Pu]));})]);});},_PE=function(_PF){return E(E(_PF)[3]);},_PG=new T(function(){return B(unCStr("Negative exponent"));}),_PH=new T(function(){return B(err(_PG));}),_PI=function(_PJ,_PK,_PL,_PM){var _PN=B(_LT(_PK)),_PO=_PN[1],_PP=E(_PN[2]);if(!B(A(_PP[3],[_PM,new T(function(){return B(A(_Pw,[_PO,_Pu]));})]))){if(!B(A(E(_PP[1])[1],[_PM,new T(function(){return B(A(_Pw,[_PO,_Pu]));})]))){var _PQ=B(_LT(_PK)),_PR=_PQ[1],_PS=new T(function(){return B(_LT(_PK));}),_PT=new T(function(){return B(_P8(_PS));});return new F(function(){return (function(_PU,_PV){while(1){var _PW=(function(_PX,_PY){var _PZ=E(_PK),_Q0=_PZ[3],_Q1=E(_PZ[1]);if(!B(_Py(_Q1[1],_Q1[2],_Q1[3],_PZ[4],_PY))){return !B(A(E(E(_PQ[2])[1])[1],[_PY,new T(function(){return B(A(_Pw,[_PR,_LV]));})]))?B((function(_Q2,_Q3,_Q4){while(1){var _Q5=(function(_Q6,_Q7,_Q8){var _Q9=E(_PK),_Qa=_Q9[3],_Qb=E(_Q9[1]);if(!B(_Py(_Qb[1],_Qb[2],_Qb[3],_Q9[4],_Q7))){if(!B(A(new T(function(){return B(_2H(new T(function(){return B(_Pq(new T(function(){return B(_Ps(_PS));})));})));}),[_Q7,new T(function(){return B(A(_Pw,[_PT,_LV]));})]))){_Q2=new T(function(){return B(A(new T(function(){return B(_Po(_PJ));}),[_Q6,_Q6]));});_Q3=new T(function(){return B(A(_Qa,[new T(function(){return B(A(new T(function(){return B(_PE(_PT));}),[_Q7,new T(function(){return B(A(_Pw,[_PT,_LV]));})]));}),new T(function(){return B(A(_Pw,[_PT,_Pv]));})]));});_Q4=new T(function(){return B(A(new T(function(){return B(_Po(_PJ));}),[_Q6,_Q8]));});return null;}else{return new F(function(){return A(new T(function(){return B(_Po(_PJ));}),[_Q6,_Q8]);});}}else{_Q2=new T(function(){return B(A(new T(function(){return B(_Po(_PJ));}),[_Q6,_Q6]));});_Q3=new T(function(){return B(A(_Qa,[_Q7,new T(function(){return B(A(_Pw,[_PT,_Pv]));})]));});var _Qc=_Q8;_Q4=_Qc;return null;}})(_Q2,_Q3,_Q4);if(_Q5!=null){return _Q5;}}})(new T(function(){return B(A(new T(function(){return B(_Po(_PJ));}),[_PX,_PX]));}),new T(function(){return B(A(_Q0,[new T(function(){return B(A(new T(function(){return B(_PE(_PR));}),[_PY,new T(function(){return B(A(_Pw,[_PR,_LV]));})]));}),new T(function(){return B(A(_Pw,[_PR,_Pv]));})]));}),_PX)):E(_PX);}else{_PU=new T(function(){return B(A(new T(function(){return B(_Po(_PJ));}),[_PX,_PX]));});_PV=new T(function(){return B(A(_Q0,[_PY,new T(function(){return B(A(_Pw,[_PR,_Pv]));})]));});return null;}})(_PU,_PV);if(_PW!=null){return _PW;}}})(_PL,_PM);});}else{return new F(function(){return A(_Pw,[_PJ,_LV]);});}}else{return E(_PH);}},_Qd=new T(function(){return B(err(_PG));}),_Qe=function(_Qf,_Qg){var _Qh=E(_Qf);return _Qh[0]==0?_Qh[1]*Math.pow(2,_Qg):I_toNumber(_Qh[1])*Math.pow(2,_Qg);},_Qi=function(_Qj,_Qk){while(1){var _Ql=E(_Qj);if(!_Ql[0]){var _Qm=E(_Ql[1]);if(_Qm==(-2147483648)){_Qj=[1,I_fromInt(-2147483648)];continue;}else{var _Qn=E(_Qk);if(!_Qn[0]){var _Qo=_Qn[1];return [0,[0,quot(_Qm,_Qo)],[0,_Qm%_Qo]];}else{_Qj=[1,I_fromInt(_Qm)];_Qk=_Qn;continue;}}}else{var _Qp=E(_Qk);if(!_Qp[0]){_Qj=_Ql;_Qk=[1,I_fromInt(_Qp[1])];continue;}else{var _Qq=I_quotRem(_Ql[1],_Qp[1]);return [0,[1,_Qq[1]],[1,_Qq[2]]];}}}},_Qr=function(_Qs,_Qt){var _Qu=B(_yX(_Qt)),_Qv=_Qu[1],_Qw=_Qu[2],_Qx=new T(function(){return B(_P8(new T(function(){return B(_LT(_Qs));})));});if(_Qw<0){var _Qy= -_Qw;if(_Qy>=0){var _Qz=E(_Qy),_QA=_Qz==0?E(_LV):B(_Pi(_JS,_Qz));if(!B(_zt(_QA,_zn))){var _QB=B(_Qi(_Qv,_QA));return [0,new T(function(){return B(A(_Pw,[_Qx,_QB[1]]));}),new T(function(){return [0,B(_Qe(_QB[2],_Qw))];})];}else{return E(_N7);}}else{return E(_Qd);}}else{return [0,new T(function(){return B(A(_Po,[_Qx,new T(function(){return B(A(_Pw,[_Qx,_Qv]));}),new T(function(){return B(_PI(_Qx,_P7,new T(function(){return B(A(_Pw,[_Qx,_JS]));}),[0,_Qw]));})]));}),_HD];}},_QC=function(_QD,_QE){var _QF=B(_Qr(_QD,E(_QE)[1])),_QG=_QF[1];if(E(_QF[2])[1]<=0){return E(_QG);}else{var _QH=E(B(_LT(_QD))[1]);return new F(function(){return A(_QH[1],[_QG,new T(function(){return B(A(_QH[7],[_La]));})]);});}},_QI=function(_QJ,_QK){var _QL=B(_Qr(_QJ,E(_QK)[1])),_QM=_QL[1];if(E(_QL[2])[1]>=0){return E(_QM);}else{var _QN=E(B(_LT(_QJ))[1]);return new F(function(){return A(_QN[3],[_QM,new T(function(){return B(A(_QN[7],[_La]));})]);});}},_QO=function(_QP,_QQ){var _QR=B(_Qr(_QP,E(_QQ)[1]));return [0,_QR[1],_QR[2]];},_QS=function(_QT,_QU){var _QV=B(_Qr(_QT,_QU)),_QW=_QV[1],_QX=E(_QV[2])[1],_QY=new T(function(){var _QZ=E(B(_LT(_QT))[1]),_R0=_QZ[7];return _QX>=0?B(A(_QZ[1],[_QW,new T(function(){return B(A(_R0,[_La]));})])):B(A(_QZ[3],[_QW,new T(function(){return B(A(_R0,[_La]));})]));});if(_QX<0){var _R1= -_QX-0.5;if(_R1>=0){if(!E(_R1)){var _R2=E(_QT),_R3=E(_R2[1]);return !B(_Py(_R3[1],_R3[2],_R3[3],_R2[4],_QW))?E(_QY):E(_QW);}else{return E(_QY);}}else{return E(_QW);}}else{var _R4=_QX-0.5;if(_R4>=0){if(!E(_R4)){var _R5=E(_QT),_R6=E(_R5[1]);return !B(_Py(_R6[1],_R6[2],_R6[3],_R5[4],_QW))?E(_QY):E(_QW);}else{return E(_QY);}}else{return E(_QW);}}},_R7=function(_R8,_R9){return new F(function(){return _QS(_R8,E(_R9)[1]);});},_Ra=function(_Rb,_Rc){return E(B(_Qr(_Rb,E(_Rc)[1]))[1]);},_Rd=[0,_LS,_HY,_QO,_Ra,_R7,_QC,_QI],_Re=function(_Rf,_Rg){return E(_Rf)[1]!=E(_Rg)[1]?true:false;},_Rh=function(_Ri,_Rj){return E(_Ri)[1]==E(_Rj)[1];},_Rk=[0,_Rh,_Re],_Rl=function(_Rm,_Rn){return E(_Rm)[1]<E(_Rn)[1];},_Ro=function(_Rp,_Rq){return E(_Rp)[1]<=E(_Rq)[1];},_Rr=function(_Rs,_Rt){return E(_Rs)[1]>E(_Rt)[1];},_Ru=function(_Rv,_Rw){return E(_Rv)[1]>=E(_Rw)[1];},_Rx=function(_Ry,_Rz){var _RA=E(_Ry)[1],_RB=E(_Rz)[1];return _RA>=_RB?_RA!=_RB?2:1:0;},_RC=function(_RD,_RE){var _RF=E(_RD),_RG=E(_RE);return _RF[1]>_RG[1]?E(_RF):E(_RG);},_RH=function(_RI,_RJ){var _RK=E(_RI),_RL=E(_RJ);return _RK[1]>_RL[1]?E(_RL):E(_RK);},_RM=[0,_Rk,_Rx,_Rl,_Ru,_Rr,_Ro,_RC,_RH],_RN=function(_RO,_RP){while(1){var _RQ=(function(_RR,_RS){var _RT=E(_Lb)[1]["v"]["i8"][(255&_RR>>>0)>>>0&4294967295];if(_RS>_RT){if(_RT>=8){var _RU=_RR>>8,_RV=_RS-8|0;_RO=_RU;_RP=_RV;return null;}else{return [0,new T(function(){return B(_cA(_RR>>_RT));}),_RS-_RT|0];}}else{return [0,new T(function(){return B(_cA(_RR>>_RS));}),0];}})(_RO,_RP);if(_RQ!=null){return _RQ;}}},_RW=function(_RX){var _RY=decodeFloat(_RX),_RZ=_RY[1],_S0=_RY[2];if(_S0<0){var _S1=function(_S2){if(!_S2){return [0,B(_cA(_RZ)),B(_zI(_La, -_S0))];}else{var _S3=B(_RN(_RZ, -_S0));return [0,E(_S3[1]),B(_zI(_La,_S3[2]))];}};return (_RZ>>>0&1)>>>0==0?B(_S1(1)):B(_S1(0));}else{return [0,B(_zI(B(_cA(_RZ)),_S0)),_La];}},_S4=function(_S5){var _S6=B(_RW(E(_S5)[1]));return [0,E(_S6[1]),E(_S6[2])];},_S7=[0,_Jf,_RM,_S4],_S8=[0,-1],_S9=[0,1],_Sa=function(_Sb,_Sc){var _Sd=E(_Sb);return _Sd[0]==0?_Sd[1]*Math.pow(2,_Sc):I_toNumber(_Sd[1])*Math.pow(2,_Sc);},_Se=[0,0],_Sf=function(_Sg,_Sh){var _Si=decodeFloat(_Sh),_Sj=_Si[1],_Sk=_Si[2],_Sl=new T(function(){return B(_P8(new T(function(){return B(_LT(_Sg));})));});if(_Sk<0){var _Sm=new T(function(){if(_Sj<0){var _Sn= -_Sk;if(_Sn<32){var _So=[0, -( -_Sj>>_Sn)];}else{var _So= -_Sj>=0?E(_Se):E(_S9);}var _Sp=_So,_Sq=_Sp,_Sr=_Sq;}else{var _Ss= -_Sk;if(_Ss<32){var _St=[0,_Sj>>_Ss];}else{var _St=_Sj>=0?E(_Se):E(_S8);}var _Su=_St,_Sv=_Su,_Sr=_Sv;}var _Sw=_Sr;return _Sw;});return [0,new T(function(){return B(A(_Pw,[_Sl,new T(function(){return B(_cA(E(_Sm)[1]));})]));}),new T(function(){var _Sx= -_Sk;if(_Sx<32){var _Sy=[0,B(_Sa(B(_cA(_Sj-(E(_Sm)[1]<<_Sx)|0)),_Sk))];}else{var _Sy=[0,B(_Sa(B(_cA(_Sj)),_Sk))];}var _Sz=_Sy,_SA=_Sz,_SB=_SA;return _SB;})];}else{return [0,new T(function(){return B(A(_Po,[_Sl,new T(function(){return B(A(_Pw,[_Sl,new T(function(){return B(_cA(_Sj));})]));}),new T(function(){return B(_PI(_Sl,_P7,new T(function(){return B(A(_Pw,[_Sl,_JS]));}),[0,_Sk]));})]));}),_IY];}},_SC=function(_SD,_SE){var _SF=B(_Sf(_SD,E(_SE)[1])),_SG=_SF[1];if(E(_SF[2])[1]<=0){return E(_SG);}else{var _SH=E(B(_LT(_SD))[1]);return new F(function(){return A(_SH[1],[_SG,new T(function(){return B(A(_SH[7],[_La]));})]);});}},_SI=function(_SJ,_SK){var _SL=B(_Sf(_SJ,E(_SK)[1])),_SM=_SL[1];if(E(_SL[2])[1]>=0){return E(_SM);}else{var _SN=E(B(_LT(_SJ))[1]);return new F(function(){return A(_SN[3],[_SM,new T(function(){return B(A(_SN[7],[_La]));})]);});}},_SO=function(_SP,_SQ){var _SR=B(_Sf(_SP,E(_SQ)[1]));return [0,_SR[1],_SR[2]];},_SS=function(_ST,_SU){var _SV=B(_Sf(_ST,_SU)),_SW=_SV[1],_SX=E(_SV[2])[1],_SY=new T(function(){var _SZ=E(B(_LT(_ST))[1]),_T0=_SZ[7];return _SX>=0?B(A(_SZ[1],[_SW,new T(function(){return B(A(_T0,[_La]));})])):B(A(_SZ[3],[_SW,new T(function(){return B(A(_T0,[_La]));})]));});if(_SX<0){var _T1= -_SX-0.5;if(_T1>=0){if(!E(_T1)){var _T2=E(_ST),_T3=E(_T2[1]);return !B(_Py(_T3[1],_T3[2],_T3[3],_T2[4],_SW))?E(_SY):E(_SW);}else{return E(_SY);}}else{return E(_SW);}}else{var _T4=_SX-0.5;if(_T4>=0){if(!E(_T4)){var _T5=E(_ST),_T6=E(_T5[1]);return !B(_Py(_T6[1],_T6[2],_T6[3],_T5[4],_SW))?E(_SY):E(_SW);}else{return E(_SY);}}else{return E(_SW);}}},_T7=function(_T8,_T9){return new F(function(){return _SS(_T8,E(_T9)[1]);});},_Ta=function(_Tb,_Tc){return E(B(_Sf(_Tb,E(_Tc)[1]))[1]);},_Td=[0,_S7,_Jj,_SO,_Ta,_T7,_SC,_SI],_Te=function(_Tf){return new F(function(){return err(B(unAppCStr("Char.intToDigit: not a digit ",new T(function(){if(_Tf>=0){var _Tg=jsShowI(_Tf),_Th=_Tg,_Ti=fromJSStr(_Th);}else{var _Tj=jsShowI(_Tf),_Tk=_Tj,_Ti=fromJSStr(_Tk);}var _Tl=_Ti;return _Tl;}))));});},_Tm=function(_Tn){var _To=function(_Tp){if(_Tn<10){return new F(function(){return _Te(_Tn);});}else{if(_Tn>15){return new F(function(){return _Te(_Tn);});}else{return (97+_Tn|0)-10|0;}}};if(_Tn<0){return new F(function(){return _To(_);});}else{if(_Tn>9){return new F(function(){return _To(_);});}else{return 48+_Tn|0;}}},_Tq=function(_Tr){return [0,B(_Tm(E(_Tr)[1]))];},_Ts=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_Tt=function(_Tu){return new F(function(){return _9p([0,new T(function(){return B(_9E(_Tu,_Ts));})],_9m);});},_Tv=new T(function(){return B(_Tt("GHC/Float.lhs:619:11-64|d : ds\'"));}),_Tw=function(_Tx,_Ty){if(E(_Tx)[1]<=0){var _Tz=B(_1u(_Tq,[1,_Se,_Ty]));return _Tz[0]==0?E(_Tv):[0,_Tz[1],_Tz[2]];}else{var _TA=B(_1u(_Tq,_Ty));return _TA[0]==0?E(_Tv):[0,_TA[1],_TA[2]];}},_TB=function(_TC){return E(E(_TC)[1]);},_TD=function(_TE){return E(E(_TE)[1]);},_TF=function(_TG){return E(E(_TG)[1]);},_TH=[0,48],_TI=[1,_TH,_u],_TJ=[0,46],_TK=function(_TL,_TM,_TN){while(1){var _TO=(function(_TP,_TQ,_TR){var _TS=E(_TP);if(!_TS){var _TT=B(_Ha(_TQ,_u));return _TT[0]==0?[1,_TH,[1,_TJ,new T(function(){var _TU=E(_TR);return _TU[0]==0?E(_TI):E(_TU);})]]:B(_O(_TT,[1,_TJ,new T(function(){var _TV=E(_TR);return _TV[0]==0?E(_TI):E(_TV);})]));}else{var _TW=E(_TR);if(!_TW[0]){_TL=_TS-1|0;var _TX=[1,_TH,_TQ];_TN=_u;_TM=_TX;return null;}else{_TL=_TS-1|0;var _TX=[1,_TW[1],_TQ];_TN=_TW[2];_TM=_TX;return null;}}})(_TL,_TM,_TN);if(_TO!=null){return _TO;}}},_TY=[0,0],_TZ=new T(function(){return B(unCStr(" out of range "));}),_U0=new T(function(){return B(unCStr("}.index: Index "));}),_U1=new T(function(){return B(unCStr("Ix{"));}),_U2=[1,_16,_u],_U3=[1,_16,_U2],_U4=function(_U5,_U6,_U7,_U8,_U9){return new F(function(){return err(B(_O(_U1,new T(function(){return B(_O(_U5,new T(function(){return B(_O(_U0,[1,_17,new T(function(){return B(A(_U9,[_TY,_U6,[1,_16,new T(function(){return B(_O(_TZ,[1,_17,[1,_17,new T(function(){return B(A(_pI,[_py,[1,new T(function(){return B(A(_U9,[_p3,_U7]));}),[1,new T(function(){return B(A(_U9,[_p3,_U8]));}),_u]],_U3]));})]]));})]]));})]));})));}))));});},_Ua=function(_Ub,_Uc,_Ud,_Ue){var _Uf=E(_Ud);return new F(function(){return _U4(_Ub,_Uc,_Uf[1],_Uf[2],E(_Ue)[1]);});},_Ug=function(_Uh,_Ui,_Uj,_Uk){return new F(function(){return _Ua(_Uk,_Uj,_Ui,_Uh);});},_Ul=new T(function(){return B(unCStr("Int"));}),_Um=function(_Un,_Uo,_Up){return new F(function(){return _Ug(_px,[0,_Uo,_Up],_Un,_Ul);});},_Uq=new T(function(){return B(unCStr("(Array.!): undefined array element"));}),_Ur=new T(function(){return B(err(_Uq));}),_Us=[0,1100],_Ut=[0,_Se,_Us],_Uu=function(_Uv){return new F(function(){return _Ug(_px,_Ut,[0,_Uv],_Ul);});},_Uw=function(_){var _Ux=newArr(1101,_Ur),_Uy=_Ux;return new F(function(){return (function(_Uz,_){while(1){var _UA=(function(_UB,_){if(0>_UB){return new F(function(){return _Uu(_UB);});}else{if(_UB>1100){return new F(function(){return _Uu(_UB);});}else{var _=_Uy[_UB]=new T(function(){if(_UB>=0){var _UC=E(_UB),_UD=_UC==0?E(_LV):B(_Pi(_JS,_UC));}else{var _UD=E(_Qd);}var _UE=_UD;return _UE;}),_UF=E(_UB);if(_UF==1100){var _UG=_Uy,_UH=_UG;return [0,E(_Se),E(_Us),1101,_UH];}else{_Uz=_UF+1|0;return null;}}}})(_Uz,_);if(_UA!=null){return _UA;}}})(0,_);});},_UI=function(_UJ){var _UK=B(A(_UJ,[_])),_UL=_UK;return E(_UL);},_UM=new T(function(){return B(_UI(_Uw));}),_UN=[0,10],_UO=[0,324],_UP=[0,_Se,_UO],_UQ=function(_UR){return new F(function(){return _Ug(_px,_UP,[0,_UR],_Ul);});},_US=function(_){var _UT=newArr(325,_Ur),_UU=_UT;return new F(function(){return (function(_UV,_){while(1){var _UW=(function(_UX,_){if(0>_UX){return new F(function(){return _UQ(_UX);});}else{if(_UX>324){return new F(function(){return _UQ(_UX);});}else{var _=_UU[_UX]=new T(function(){if(_UX>=0){var _UY=E(_UX),_UZ=_UY==0?E(_LV):B(_Pi(_UN,_UY));}else{var _UZ=E(_Qd);}var _V0=_UZ;return _V0;}),_V1=E(_UX);if(_V1==324){var _V2=_UU,_V3=_V2;return [0,E(_Se),E(_UO),325,_V3];}else{_UV=_V1+1|0;return null;}}}})(_UV,_);if(_UW!=null){return _UW;}}})(0,_);});},_V4=new T(function(){return B(_UI(_US));}),_V5=function(_V6,_V7){var _V8=[0,_V7],_V9=function(_Va){if(!B(_zt(_V6,_UN))){if(_V7>=0){var _Vb=E(_V7);return _Vb==0?E(_LV):B(_Pi(_V6,_Vb));}else{return E(_Qd);}}else{if(_V7>324){if(_V7>=0){var _Vc=E(_V7);return _Vc==0?E(_LV):B(_Pi(_V6,_Vc));}else{return E(_Qd);}}else{var _Vd=E(_V4),_Ve=E(_Vd[1]),_Vf=_Ve[1],_Vg=E(_Vd[2]);if(_Vf>_V7){return new F(function(){return _Um(_V8,_Ve,_Vg);});}else{if(_V7>_Vg[1]){return new F(function(){return _Um(_V8,_Ve,_Vg);});}else{return E(_Vd[4][_V7-_Vf|0]);}}}}};if(!B(_zt(_V6,_JS))){return new F(function(){return _V9(_);});}else{if(_V7<0){return new F(function(){return _V9(_);});}else{if(_V7>1100){return new F(function(){return _V9(_);});}else{var _Vh=E(_UM),_Vi=E(_Vh[1]),_Vj=_Vi[1],_Vk=E(_Vh[2]);if(_Vj>_V7){return new F(function(){return _Um(_V8,_Vi,_Vk);});}else{if(_V7>_Vk[1]){return new F(function(){return _Um(_V8,_Vi,_Vk);});}else{return E(_Vh[4][_V7-_Vj|0]);}}}}}},_Vl=function(_Vm,_Vn){var _Vo=E(_Vm);if(!_Vo[0]){var _Vp=_Vo[1],_Vq=E(_Vn);return _Vq[0]==0?_Vp>_Vq[1]:I_compareInt(_Vq[1],_Vp)<0;}else{var _Vr=_Vo[1],_Vs=E(_Vn);return _Vs[0]==0?I_compareInt(_Vr,_Vs[1])>0:I_compare(_Vr,_Vs[1])>0;}},_Vt=[1,_Se,_u],_Vu=function(_Vv,_Vw){while(1){var _Vx=E(_Vv);if(!_Vx[0]){var _Vy=E(_Vx[1]);if(_Vy==(-2147483648)){_Vv=[1,I_fromInt(-2147483648)];continue;}else{var _Vz=E(_Vw);if(!_Vz[0]){return [0,quot(_Vy,_Vz[1])];}else{_Vv=[1,I_fromInt(_Vy)];_Vw=_Vz;continue;}}}else{var _VA=_Vx[1],_VB=E(_Vw);return _VB[0]==0?[0,I_toInt(I_quot(_VA,I_fromInt(_VB[1])))]:[1,I_quot(_VA,_VB[1])];}}},_VC=function(_VD,_VE,_VF,_VG,_VH,_VI,_VJ,_VK){if(!B(A(_VD,[_VK,new T(function(){return B(A(_Pw,[B(_TD(B(_TB(_VE)))),_zn]));})]))){var _VL=new T(function(){return B(A(_VF,[_VK]));}),_VM=new T(function(){return B(A(_VG,[_VK]));}),_VN=new T(function(){return [0,E(B(A(_VH,[_VK]))[1])[1]-E(_VM)[1]|0];}),_VO=new T(function(){return B(A(_VI,[_VK]));}),_VP=new T(function(){return E(E(_VO)[2]);}),_VQ=new T(function(){var _VR=E(_VP),_VS=_VR[1],_VT=E(_VN)[1]-_VS|0;if(_VT<=0){var _VU=[0,new T(function(){return E(E(_VO)[1]);}),_VR];}else{var _VU=[0,new T(function(){var _VV=B(_V5(_VL,_VT));if(!B(_zt(_VV,_zn))){var _VW=B(_Vu(E(_VO)[1],_VV));}else{var _VW=E(_N7);}var _VX=_VW;return _VX;}),[0,_VS+_VT|0]];}var _VY=_VU,_VZ=_VY,_W0=_VZ,_W1=_W0;return _W1;}),_W2=new T(function(){return E(E(_VQ)[2]);}),_W3=new T(function(){return E(E(_VQ)[1]);}),_W4=new T(function(){var _W5=E(_W2)[1];if(_W5<0){if(_W5<=E(_VN)[1]){var _W6=[0,new T(function(){return B(_cC(_W3,_JS));}),new T(function(){return B(_cC(B(_V5(_VL, -_W5)),_JS));}),_La,_La];}else{var _W6=!B(_zt(_W3,B(_V5(_VL,E(_VM)[1]-1|0))))?[0,new T(function(){return B(_cC(_W3,_JS));}),new T(function(){return B(_cC(B(_V5(_VL, -_W5)),_JS));}),_La,_La]:[0,new T(function(){return B(_cC(B(_cC(_W3,_VL)),_JS));}),new T(function(){return B(_cC(B(_V5(_VL, -_W5+1|0)),_JS));}),_VL,_La];}var _W7=_W6,_W8=_W7,_W9=_W8;}else{var _Wa=new T(function(){return B(_V5(_VL,_W5));}),_W9=!B(_zt(_W3,B(_V5(_VL,E(_VM)[1]-1|0))))?[0,new T(function(){return B(_cC(B(_cC(_W3,_Wa)),_JS));}),_JS,_Wa,_Wa]:[0,new T(function(){return B(_cC(B(_cC(B(_cC(_W3,_Wa)),_VL)),_JS));}),new T(function(){return B(_cC(_JS,_VL));}),new T(function(){return B(_cC(_Wa,_VL));}),_Wa];}var _Wb=_W9,_Wc=_Wb;return _Wc;}),_Wd=new T(function(){return E(E(_W4)[2]);}),_We=new T(function(){return E(E(_W4)[3]);}),_Wf=new T(function(){return E(E(_W4)[1]);}),_Wg=new T(function(){var _Wh=new T(function(){return B(_ck(_Wf,_We));}),_Wi=function(_Wj){var _Wk=(Math.log(B(_IT(B(_ck(_W3,_La)))))+E(_W2)[1]*Math.log(B(_IT(_VL))))/Math.log(B(_IT(_VJ))),_Wl=_Wk&4294967295;return _Wl>=_Wk?E(_Wl):_Wl+1|0;},_Wm=function(_Wn){while(1){if(_Wn<0){if(!B(_e1(B(_cC(B(_V5(_VJ, -_Wn)),_Wh)),_Wd))){var _Wo=_Wn+1|0;_Wn=_Wo;continue;}else{return E(_Wn);}}else{if(!B(_e1(_Wh,B(_cC(B(_V5(_VJ,_Wn)),_Wd))))){var _Wo=_Wn+1|0;_Wn=_Wo;continue;}else{return E(_Wn);}}}};if(!B(_zt(_VL,_JS))){var _Wp=[0,B(_Wm(B(_Wi(_))))];}else{if(!B(_zt(_VJ,_UN))){var _Wq=[0,B(_Wm(B(_Wi(_))))];}else{var _Wr=(E(_VM)[1]-1|0)+E(_VP)[1]|0;if(_Wr<0){var _Ws=[0,B(_Wm(quot(imul(_Wr,8651)|0,28738)))];}else{var _Ws=[0,B(_Wm(quot(imul(_Wr,8651)|0,28738)+1|0))];}var _Wt=_Ws,_Wu=_Wt,_Wv=_Wu,_Ww=_Wv,_Wx=_Ww,_Wq=_Wx;}var _Wp=_Wq;}return _Wp;});return [0,new T(function(){var _Wy=E(_Wg)[1],_Wz=function(_WA,_WB,_WC,_WD,_WE){while(1){var _WF=(function(_WG,_WH,_WI,_WJ,_WK){if(!B(_zt(_WI,_zn))){var _WL=B(_Qi(B(_cC(_WH,_VJ)),_WI)),_WM=_WL[1],_WN=_WL[2],_WO=B(_cC(_WK,_VJ)),_WP=B(_cC(_WJ,_VJ));if(!B(_Y(_WN,_WO))){if(!B(_Vl(B(_ck(_WN,_WP)),_WI))){var _WQ=[1,_WM,_WG];_WB=_WN;var _WR=_WI;_WD=_WP;_WE=_WO;_WA=_WQ;_WC=_WR;return null;}else{return [1,new T(function(){return B(_ck(_WM,_La));}),_WG];}}else{return !B(_Vl(B(_ck(_WN,_WP)),_WI))?[1,_WM,_WG]:!B(_Y(B(_cC(_WN,_JS)),_WI))?[1,new T(function(){return B(_ck(_WM,_La));}),_WG]:[1,_WM,_WG];}}else{return E(_N7);}})(_WA,_WB,_WC,_WD,_WE);if(_WF!=null){return _WF;}}};if(_Wy<0){var _WS=B(_V5(_VJ, -_Wy)),_WT=B(_1u(_Ol,B(_Ha(B(_Wz(_u,B(_cC(_Wf,_WS)),_Wd,B(_cC(_We,_WS)),B(_cC(E(_W4)[4],_WS)))),_u))));}else{var _WT=B(_1u(_Ol,B(_Ha(B(_Wz(_u,_Wf,B(_cC(_Wd,B(_V5(_VJ,_Wy)))),_We,E(_W4)[4])),_u))));}var _WU=_WT,_WV=_WU;return _WV;}),_Wg];}else{return [0,_Vt,_Se];}},_WW=function(_WX){return E(_WX)[1]%2==0?true:false;},_WY=new T(function(){return B(unCStr("roundTo: bad Value"));}),_WZ=new T(function(){return B(err(_WY));}),_X0=function(_X1){return E(E(_X1)[1])==0?true:false;},_X2=function(_X3){return _X3>1?[1,_Se,new T(function(){return B(_X2(_X3-1|0));})]:E(_Vt);},_X4=function(_X5,_X6,_X7){var _X8=function(_X9,_Xa,_Xb){var _Xc=E(_Xb);if(!_Xc[0]){return [0,_Se,new T(function(){var _Xd=E(_X9)[1];return _Xd>0?B(_X2(_Xd)):[0];})];}else{var _Xe=_Xc[1],_Xf=_Xc[2],_Xg=E(E(_X9)[1]);if(!_Xg){var _Xh=E(_Xe)[1],_Xi=E(new T(function(){return [0,quot(E(_X5)[1],2)];}))[1];return _Xh!=_Xi?[0,new T(function(){return _Xh<_Xi?E(_Se):E(_S9);}),_u]:!E(_Xa)?[0,new T(function(){return _Xh<_Xi?E(_Se):E(_S9);}),_u]:!B(_G8(_X0,_Xf))?[0,new T(function(){return _Xh<_Xi?E(_Se):E(_S9);}),_u]:[0,_Se,_u];}else{var _Xj=B(_X8([0,_Xg-1|0],new T(function(){return B(_WW(_Xe));}),_Xf)),_Xk=_Xj[2],_Xl=E(_Xj[1])[1]+E(_Xe)[1]|0;return _Xl!=E(_X5)[1]?[0,_Se,[1,[0,_Xl],_Xk]]:[0,_S9,[1,_Se,_Xk]];}}},_Xm=B(_X8(_X6,_n,_X7));switch(E(E(_Xm[1])[1])){case 0:return E(_Xm);case 1:return [0,_S9,[1,_S9,_Xm[2]]];default:return E(_WZ);}},_Xn=function(_Xo,_Xp){var _Xq=E(_Xo);if(!_Xq){return [0,_u,_Xp];}else{var _Xr=E(_Xp);if(!_Xr[0]){return [0,_u,_u];}else{var _Xs=new T(function(){var _Xt=B(_Xn(_Xq-1|0,_Xr[2]));return [0,_Xt[1],_Xt[2]];});return [0,[1,_Xr[1],new T(function(){return E(E(_Xs)[1]);})],new T(function(){return E(E(_Xs)[2]);})];}}},_Xu=function(_Xv){return E(E(_Xv)[3]);},_Xw=0,_Xx=1,_Xy=[0,10],_Xz=new T(function(){return B(unCStr("e0"));}),_XA=function(_XB,_XC){var _XD=E(_XB);if(!_XD[0]){return E(_Xz);}else{var _XE=_XD[1];return _XC>1?[1,_XE,new T(function(){return B(_XA(_XD[2],_XC-1|0));})]:[1,_XE,_Xz];}},_XF=function(_XG,_XH){var _XI=E(_XH);return _XI[0]==0?[0]:[1,_XG,new T(function(){return B(_XF(_XI[1],_XI[2]));})];},_XJ=new T(function(){return B(unCStr("init"));}),_XK=new T(function(){return B(_pE(_XJ));}),_XL=new T(function(){return B(_Tt("GHC/Float.lhs:591:12-70|(d : ds\')"));}),_XM=[0,101],_XN=new T(function(){return B(unCStr("Infinity"));}),_XO=new T(function(){return B(unCStr("-Infinity"));}),_XP=new T(function(){return B(unCStr("NaN"));}),_XQ=new T(function(){return B(unCStr("formatRealFloat/doFmt/FFExponent: []"));}),_XR=new T(function(){return B(err(_XQ));}),_XS=new T(function(){return B(unCStr("0.0e0"));}),_XT=function(_XU){return E(E(_XU)[4]);},_XV=new T(function(){return [1,_TH,_XV];}),_XW=function(_XX,_XY,_XZ,_Y0,_Y1,_Y2,_Y3,_Y4,_Y5,_Y6,_Y7,_Y8){if(!B(A(_Y3,[_Y8]))){var _Y9=new T(function(){return B(_TD(new T(function(){return B(_TB(_XY));})));});if(!B(A(_Y4,[_Y8]))){var _Ya=function(_Yb,_Yc,_Yd){while(1){var _Ye=(function(_Yf,_Yg,_Yh){switch(E(_Yf)){case 0:var _Yi=E(_Y7);if(!_Yi[0]){var _Yj=B(_1u(_Tq,_Yg));if(!_Yj[0]){return E(_XR);}else{var _Yk=_Yj[2],_Yl=E(_Yj[1]),_Ym=function(_Yn){var _Yo=E(_Yk);return _Yo[0]==0?[1,_Yl,new T(function(){return B(unAppCStr(".0e",new T(function(){return B(_dQ(0,E(_Yh)[1]-1|0,_u));})));})]:[1,_Yl,[1,_TJ,new T(function(){return B(_O(_Yo,[1,_XM,new T(function(){return B(_dQ(0,E(_Yh)[1]-1|0,_u));})]));})]];};return E(_Yl[1])==48?E(_Yk)[0]==0?E(_XS):B(_Ym(_)):B(_Ym(_));}}else{var _Yp=new T(function(){var _Yq=E(_Yi[1]);return _Yq[1]>1?E(_Yq):E(_S9);}),_Yr=function(_Ys){var _Yt=new T(function(){var _Yu=B(_X4(_Xy,new T(function(){return [0,E(_Yp)[1]+1|0];}),_Yg));return [0,_Yu[1],_Yu[2]];}),_Yv=new T(function(){return E(E(_Yt)[1]);}),_Yw=new T(function(){if(E(_Yv)[1]<=0){var _Yx=B(_1u(_Tq,E(_Yt)[2])),_Yy=_Yx[0]==0?E(_XL):[0,_Yx[1],_Yx[2]];}else{var _Yz=E(E(_Yt)[2]);if(!_Yz[0]){var _YA=E(_XK);}else{var _YB=B(_1u(_Tq,B(_XF(_Yz[1],_Yz[2])))),_YA=_YB[0]==0?E(_XL):[0,_YB[1],_YB[2]];}var _YC=_YA,_Yy=_YC;}var _YD=_Yy,_YE=_YD;return _YE;});return [1,new T(function(){return E(E(_Yw)[1]);}),[1,_TJ,new T(function(){return B(_O(E(_Yw)[2],[1,_XM,new T(function(){return B(_dQ(0,(E(_Yh)[1]-1|0)+E(_Yv)[1]|0,_u));})]));})]];},_YF=E(_Yg);if(!_YF[0]){return new F(function(){return _Yr(_);});}else{return E(E(_YF[1])[1])==0?E(_YF[2])[0]==0?[1,_TH,[1,_TJ,new T(function(){var _YG=E(_Yp)[1];return _YG>0?B(_XA(_XV,_YG)):E(_Xz);})]]:B(_Yr(_)):B(_Yr(_));}}break;case 1:var _YH=E(_Y7);if(!_YH[0]){var _YI=E(_Yh)[1];return _YI>0?B(_TK(_YI,_u,new T(function(){return B(_1u(_Tq,_Yg));}))):B(unAppCStr("0.",new T(function(){var _YJ= -_YI;if(_YJ>0){var _YK=function(_YL){return _YL>1?[1,_TH,new T(function(){return B(_YK(_YL-1|0));})]:E([1,_TH,new T(function(){return B(_1u(_Tq,_Yg));})]);},_YM=B(_YK(_YJ));}else{var _YM=B(_1u(_Tq,_Yg));}var _YN=_YM,_YO=_YN;return _YO;})));}else{var _YP=_YH[1],_YQ=E(_Yh),_YR=_YQ[1];if(_YR<0){var _YS=new T(function(){var _YT= -_YR;if(_YT>0){var _YU=function(_YV){return _YV>1?[1,_Se,new T(function(){return B(_YU(_YV-1|0));})]:E([1,_Se,_Yg]);},_YW=B(_X4(_Xy,new T(function(){var _YX=E(_YP);return _YX[1]>0?E(_YX):E(_Se);}),B(_YU(_YT)))),_YY=B(_Tw(_YW[1],_YW[2]));}else{var _YZ=B(_X4(_Xy,new T(function(){var _Z0=E(_YP);return _Z0[1]>0?E(_Z0):E(_Se);}),_Yg)),_YY=B(_Tw(_YZ[1],_YZ[2]));}var _Z1=_YY,_Z2=_Z1;return _Z2;});return [1,new T(function(){return E(E(_YS)[1]);}),new T(function(){var _Z3=E(E(_YS)[2]);return _Z3[0]==0?[0]:[1,_TJ,_Z3];})];}else{var _Z4=B(_X4(_Xy,new T(function(){var _Z5=E(_YP)[1];if(_Z5>0){var _Z6=[0,_Z5+_YR|0];}else{var _Z6=E(_YQ);}var _Z7=_Z6,_Z8=_Z7;return _Z8;}),_Yg)),_Z9=_Z4[2],_Za=_YR+E(_Z4[1])[1]|0;if(_Za>=0){var _Zb=B(_Xn(_Za,new T(function(){return B(_1u(_Tq,_Z9));}))),_Zc=_Zb[2],_Zd=E(_Zb[1]);return _Zd[0]==0?[1,_TH,new T(function(){var _Ze=E(_Zc);return _Ze[0]==0?[0]:[1,_TJ,_Ze];})]:B(_O(_Zd,new T(function(){var _Zf=E(_Zc);return _Zf[0]==0?[0]:[1,_TJ,_Zf];})));}else{return [1,_TH,new T(function(){var _Zg=B(_1u(_Tq,_Z9));return _Zg[0]==0?[0]:[1,_TJ,_Zg];})];}}}break;default:var _Zh=E(_Yh),_Zi=_Zh[1];if(_Zi>=0){if(_Zi<=7){_Yb=_Xx;var _Zj=_Yg;_Yd=_Zh;_Yc=_Zj;return null;}else{_Yb=_Xw;var _Zj=_Yg;_Yd=_Zh;_Yc=_Zj;return null;}}else{_Yb=_Xw;var _Zj=_Yg;_Yd=_Zh;_Yc=_Zj;return null;}}})(_Yb,_Yc,_Yd);if(_Ye!=null){return _Ye;}}},_Zk=function(_Zl){return [1,_qw,new T(function(){var _Zm=B(_VC(E(E(E(E(_XX)[1])[2])[1])[1],_XY,_XZ,_Y0,_Y1,_Y2,_UN,new T(function(){return B(A(_XT,[_Y9,_Y8]));})));return B(_Ya(_Y6,_Zm[1],_Zm[2]));})];};if(!B(A(_Xu,[B(_Ps(B(_TF(_XX)))),_Y8,new T(function(){return B(A(_Pw,[_Y9,_zn]));})]))){if(!B(A(_Y5,[_Y8]))){var _Zn=B(_VC(E(E(E(E(_XX)[1])[2])[1])[1],_XY,_XZ,_Y0,_Y1,_Y2,_UN,_Y8));return new F(function(){return _Ya(_Y6,_Zn[1],_Zn[2]);});}else{return new F(function(){return _Zk(_);});}}else{return new F(function(){return _Zk(_);});}}else{return !B(A(_Xu,[B(_Ps(B(_TF(_XX)))),_Y8,new T(function(){return B(A(_Pw,[_Y9,_zn]));})]))?E(_XN):E(_XO);}}else{return E(_XP);}},_Zo=function(_Zp){var _Zq=u_towlower(_Zp),_Zr=_Zq;return _Zr>>>0>1114111?B(_dW(_Zr)):_Zr;},_Zs=function(_Zt){return new F(function(){return err(B(unAppCStr("Printf.printf: ",_Zt)));});},_Zu=new T(function(){return B(unCStr("bad argument"));}),_Zv=new T(function(){return B(_Zs(_Zu));}),_Zw=new T(function(){return B(unCStr("Printf.dfmt: impossible"));}),_Zx=new T(function(){return B(err(_Zw));}),_Zy=[0,45],_Zz=[1,_Zy,_u],_ZA=new T(function(){return B(err(_Zw));}),_ZB=function(_ZC,_ZD){var _ZE=E(_ZC);return _ZE[0]==0?function(_bv){return new F(function(){return _O(new T(function(){var _ZF=jsShow(E(_ZD)[1]),_ZG=_ZF;return fromJSStr(_ZG);}),_bv);});}:function(_bv){return new F(function(){return _O(new T(function(){var _ZH=E(E(_ZE[1])[1]);if(!_ZH){var _ZI=jsRound(E(_ZD)[1]),_ZJ=_ZI,_ZK=B(_yX(_ZJ)),_ZL=_ZK[1],_ZM=_ZK[2];if(_ZM>=0){var _ZN=jsShow(B(_xj(B(_zI(_ZL,_ZM))))),_ZO=_ZN,_ZP=fromJSStr(_ZO);}else{var _ZQ=hs_uncheckedIShiftRA64(B(_zg(_ZL)), -_ZM),_ZR=_ZQ,_ZS=jsShow(B(_xj(B(_z0(_ZR))))),_ZT=_ZS,_ZP=fromJSStr(_ZT);}var _ZU=_ZP,_ZV=_ZU,_ZW=_ZV,_ZX=_ZW;}else{if(_ZH>=0){var _ZY=B(_yR(10,_ZH)),_ZZ=jsRound(E(_ZD)[1]*_ZY),_100=_ZZ,_101=jsShow((_100&4294967295)/_ZY),_102=_101,_103=fromJSStr(_102);}else{var _103=E(_yI);}var _104=_103,_105=_104,_ZX=_105;}var _106=_ZX;return _106;}),_bv);});};},_107=function(_108,_109){var _10a=E(_108);return _10a[0]==0?function(_bv){return new F(function(){return _O(new T(function(){var _10b=B(_RW(E(_109)[1])),_10c=jsShow(B(_zB(_10b[1],_10b[2]))[1]),_10d=_10c;return fromJSStr(_10d);}),_bv);});}:function(_bv){return new F(function(){return _O(new T(function(){var _10e=E(E(_10a[1])[1]);if(!_10e){var _10f=jsRound(E(_109)[1]),_10g=_10f,_10h=decodeFloat(_10g),_10i=_10h[1],_10j=_10h[2];if(_10j>=0){var _10k=jsShow(B(_xj(B(_zI(B(_cA(_10i)),_10j))))),_10l=_10k,_10m=fromJSStr(_10l);}else{var _10n=jsShow(_10i>> -_10j),_10o=_10n,_10m=fromJSStr(_10o);}var _10p=_10m,_10q=_10p,_10r=_10q,_10s=_10r;}else{var _10t=B(_RW(E(_109)[1]));if(_10e>=0){var _10u=B(_yR(10,_10e)),_10v=jsRound(B(_zB(_10t[1],_10t[2]))[1]*_10u),_10w=_10v,_10x=jsShow((_10w&4294967295)/_10u),_10y=_10x,_10z=fromJSStr(_10y);}else{var _10z=E(_yI);}var _10A=_10z,_10B=_10A,_10C=_10B,_10D=_10C,_10s=_10D;}var _10E=_10s;return _10E;}),_bv);});};},_10F=function(_10G){var _10H=u_towupper(_10G),_10I=_10H;return _10I>>>0>1114111?B(_dW(_10I)):_10I;},_10J=function(_10K){return [0,B(_10F(E(_10K)[1]))];},_10L=function(_10M,_10N,_10O){var _10P=E(_10O);switch(_10P[0]){case 3:var _10Q=_10P[1],_10R=u_iswupper(_10M),_10S=_10R;switch(B(_Zo(_10M))){case 101:var _10T=B(_XW(_Td,_JL,_Ki,_Kg,_Kn,_Kc,_Kt,_Kp,_Kx,_Xw,new T(function(){var _10U=E(_10N);return _10U[1]>=0?[1,_10U]:[0];}),_10Q));break;case 102:var _10T=B(_XW(_Td,_JL,_Ki,_Kg,_Kn,_Kc,_Kt,_Kp,_Kx,_Xx,new T(function(){var _10V=E(_10N);return _10V[1]>=0?[1,_10V]:[0];}),_10Q));break;case 103:var _10W=E(_10N),_10T=_10W[1]>=0?B(A(_107,[[1,_10W],_10Q,_u])):B(A(_107,[_2z,_10Q,_u]));break;default:var _10T=E(_ZA);}var _10X=_10T,_10Y=E(_10S);if(!_10Y){var _10Z=E(_10X);if(!_10Z[0]){return [0,_u,_u];}else{var _110=_10Z[1],_111=_10Z[2],_112=E(_110),_113=_112[1],_114=E(_113);return _114==45?[0,_Zz,_111]:[0,_u,_10Z];}}else{var _115=B(_1u(_10J,_10X));if(!_115[0]){return [0,_u,_u];}else{var _116=_115[1],_117=_115[2],_118=E(_116),_119=_118[1],_11a=E(_119);return _11a==45?[0,_Zz,_117]:[0,_u,_115];}}break;case 4:var _11b=_10P[1],_11c=u_iswupper(_10M),_11d=_11c;switch(B(_Zo(_10M))){case 101:var _11e=B(_XW(_Rd,_Iq,_JT,_JQ,_JY,_JM,_K4,_K0,_K8,_Xw,new T(function(){var _11f=E(_10N);return _11f[1]>=0?[1,_11f]:[0];}),_11b));break;case 102:var _11e=B(_XW(_Rd,_Iq,_JT,_JQ,_JY,_JM,_K4,_K0,_K8,_Xx,new T(function(){var _11g=E(_10N);return _11g[1]>=0?[1,_11g]:[0];}),_11b));break;case 103:var _11h=E(_10N),_11e=_11h[1]>=0?B(A(_ZB,[[1,_11h],_11b,_u])):B(A(_ZB,[_2z,_11b,_u]));break;default:var _11e=E(_Zx);}var _11i=_11e,_11j=E(_11d);if(!_11j){var _11k=E(_11i);if(!_11k[0]){return [0,_u,_u];}else{var _11l=_11k[1],_11m=_11k[2],_11n=E(_11l),_11o=_11n[1],_11p=E(_11o);return _11p==45?[0,_Zz,_11m]:[0,_u,_11k];}}else{var _11q=B(_1u(_10J,_11i));if(!_11q[0]){return [0,_u,_u];}else{var _11r=_11q[1],_11s=_11q[2],_11t=E(_11r),_11u=_11t[1],_11v=E(_11u);return _11v==45?[0,_Zz,_11s]:[0,_u,_11q];}}break;default:return E(_Zv);}},_11w=[0,0],_11x=function(_11y){return new F(function(){return _19(0,_11y,_u);});},_11z=[0,48],_11A=function(_11B,_11C){var _11D=_11B-B(_sT(_11C,0))|0;if(_11D>0){var _11E=function(_11F){return _11F>1?[1,_11z,new T(function(){return B(_11E(_11F-1|0));})]:E([1,_11z,_11C]);};return new F(function(){return _11E(_11D);});}else{return E(_11C);}},_11G=[0,0],_11H=[0,-2147483648],_11I=function(_11J,_11K){while(1){var _11L=(function(_11M,_11N){var _11O=E(_11N);switch(_11O[0]){case 0:_11J=_11G;_11K=[2,_11H,new T(function(){return B(_cA(E(_11O[1])[1]));})];return null;case 2:var _11P=_11O[2];return !B(_Y(_11P,_11w))?[0,_u,new T(function(){return B(_11A(E(_11M)[1],B(_11x(_11P))));})]:[0,_Zz,new T(function(){return B(_11A(E(_11M)[1],B(_19(0,B(_cu(_11P)),_u))));})];default:return E(_Zv);}})(_11J,_11K);if(_11L!=null){return _11L;}}},_11Q=[1,_oe,_u],_11R=function(_11S){return new F(function(){return err(B(unAppCStr("Char.digitToInt: not a digit ",new T(function(){var _11T=E(_11S);return _11T==39?E(_og):[1,_oe,new T(function(){return B(_nY(_11T,_11Q));})];}))));});},_11U=function(_11V){var _11W=function(_11X){var _11Y=function(_11Z){if(_11V<65){return new F(function(){return _11R(_11V);});}else{if(_11V>70){return new F(function(){return _11R(_11V);});}else{return (_11V-65|0)+10|0;}}};if(_11V<97){return new F(function(){return _11Y(_);});}else{if(_11V>102){return new F(function(){return _11Y(_);});}else{return (_11V-97|0)+10|0;}}};if(_11V<48){return new F(function(){return _11W(_);});}else{if(_11V>57){return new F(function(){return _11W(_);});}else{return _11V-48|0;}}},_120=function(_121,_122){while(1){var _123=(function(_124,_125){var _126=E(_125);if(!_126[0]){return [0,_124,_u];}else{var _127=E(_126[1])[1];if(_127<48){return [0,_124,_126];}else{if(_127>57){return [0,_124,_126];}else{_121=new T(function(){return [0,(imul(E(_124)[1],10)|0)+B(_11U(_127))|0];});_122=_126[2];return null;}}}})(_121,_122);if(_123!=null){return _123;}}},_128=new T(function(){return B(unCStr("argument list ended prematurely"));}),_129=new T(function(){return B(_Zs(_128));}),_12a=[0,-1],_12b=function(_12c){return [0,E(_12c)[1]];},_12d=function(_12e){var _12f=E(_12e);switch(_12f[0]){case 0:return new F(function(){return _12b(_12f[1]);});break;case 2:return new F(function(){return _Ol(_12f[2]);});break;default:return E(_Zv);}},_12g=function(_12h,_12i,_12j,_12k,_12l){while(1){var _12m=(function(_12n,_12o,_12p,_12q,_12r){var _12s=E(_12q);if(!_12s[0]){return [0,_11G,_12a,_12n,_12o,_12p,_u,_12r];}else{var _12t=_12s[2],_12u=E(E(_12s[1])[1]);switch(_12u){case 42:var _12v=new T(function(){var _12w=E(_12r);return _12w[0]==0?E(_129):[0,_12w[2],new T(function(){return B(_12d(_12w[1]));})];}),_12x=new T(function(){var _12y=E(_12t);if(!_12y[0]){var _12z=[0,_12a,_u,new T(function(){return E(E(_12v)[1]);})];}else{if(E(E(_12y[1])[1])==46){var _12A=E(_12y[2]);if(!_12A[0]){var _12B=B(_120(_11G,_u)),_12C=[0,_12B[1],_12B[2],new T(function(){return E(E(_12v)[1]);})];}else{if(E(E(_12A[1])[1])==42){var _12D=new T(function(){var _12E=E(E(_12v)[1]);return _12E[0]==0?E(_129):[0,_12E[2],new T(function(){return B(_12d(_12E[1]));})];}),_12F=[0,new T(function(){return E(E(_12D)[2]);}),_12A[2],new T(function(){return E(E(_12D)[1]);})];}else{var _12G=B(_120(_11G,_12A)),_12F=[0,_12G[1],_12G[2],new T(function(){return E(E(_12v)[1]);})];}var _12H=_12F,_12C=_12H;}var _12I=_12C;}else{var _12I=[0,_12a,_12y,new T(function(){return E(E(_12v)[1]);})];}var _12J=_12I,_12z=_12J;}return _12z;});return [0,new T(function(){return E(E(_12v)[2]);}),new T(function(){return E(E(_12x)[1]);}),_12n,_12o,_12p,new T(function(){return E(E(_12x)[2]);}),new T(function(){return E(E(_12x)[3]);})];case 43:var _12K=_12n,_12L=_12o;_12j=_n;_12k=_12t;var _12M=_12r;_12h=_12K;_12i=_12L;_12l=_12M;return null;case 45:_12h=_n;var _12L=_12o,_12N=_12p;_12k=_12t;var _12M=_12r;_12i=_12L;_12j=_12N;_12l=_12M;return null;case 46:var _12O=new T(function(){var _12P=E(_12t);if(!_12P[0]){var _12Q=B(_120(_11G,_u)),_12R=[0,_12Q[1],_12Q[2],_12r];}else{if(E(E(_12P[1])[1])==42){var _12S=new T(function(){var _12T=E(_12r);return _12T[0]==0?E(_129):[0,_12T[2],new T(function(){return B(_12d(_12T[1]));})];}),_12U=[0,new T(function(){return E(E(_12S)[2]);}),_12P[2],new T(function(){return E(E(_12S)[1]);})];}else{var _12V=B(_120(_11G,_12P)),_12U=[0,_12V[1],_12V[2],_12r];}var _12W=_12U,_12R=_12W;}return _12R;});return [0,_11G,new T(function(){return E(E(_12O)[1]);}),_12n,_12o,_12p,new T(function(){return E(E(_12O)[2]);}),new T(function(){return E(E(_12O)[3]);})];case 48:var _12K=_12n;_12i=_n;var _12N=_12p;_12k=_12t;var _12M=_12r;_12h=_12K;_12j=_12N;_12l=_12M;return null;default:if(_12u<48){return [0,_11G,_12a,_12n,_12o,_12p,_12s,_12r];}else{if(_12u>57){return [0,_11G,_12a,_12n,_12o,_12p,_12s,_12r];}else{var _12X=new T(function(){var _12Y=B(_120(_11G,_12s));return [0,_12Y[1],_12Y[2]];}),_12Z=new T(function(){var _130=E(E(_12X)[2]);if(!_130[0]){var _131=[0,_12a,_u,_12r];}else{if(E(E(_130[1])[1])==46){var _132=E(_130[2]);if(!_132[0]){var _133=B(_120(_11G,_u)),_134=[0,_133[1],_133[2],_12r];}else{if(E(E(_132[1])[1])==42){var _135=new T(function(){var _136=E(_12r);return _136[0]==0?E(_129):[0,_136[2],new T(function(){return B(_12d(_136[1]));})];}),_137=[0,new T(function(){return E(E(_135)[2]);}),_132[2],new T(function(){return E(E(_135)[1]);})];}else{var _138=B(_120(_11G,_132)),_137=[0,_138[1],_138[2],_12r];}var _139=_137,_134=_139;}var _13a=_134;}else{var _13a=[0,_12a,_130,_12r];}var _13b=_13a,_131=_13b;}var _13c=_131;return _13c;});return [0,new T(function(){return E(E(_12X)[1]);}),new T(function(){return E(E(_12Z)[1]);}),_12n,_12o,_12p,new T(function(){return E(E(_12Z)[2]);}),new T(function(){return E(E(_12Z)[3]);})];}}}}})(_12h,_12i,_12j,_12k,_12l);if(_12m!=null){return _12m;}}},_13d=new T(function(){return B(unCStr("formatting string ended prematurely"));}),_13e=new T(function(){return B(_Zs(_13d));}),_13f=function(_13g,_13h){if(!B(_Y(_13h,_13g))){if(!B(_zt(_13g,_11w))){var _13i=B(_Qi(_13h,_13g));return new F(function(){return _O(B(_13f(_13g,_13i[1])),[1,new T(function(){return [0,B(_Tm(B(_dY(_13i[2]))))];}),_u]);});}else{return E(_N7);}}else{return [1,new T(function(){return [0,B(_Tm(B(_dY(_13h))))];}),_u];}},_13j=[0,2],_13k=function(_13l,_13m,_13n){var _13o=E(_13n);switch(_13o[0]){case 0:return new F(function(){return _13f(_13l,B(_cA(E(_13o[1])[1])));});break;case 2:var _13p=_13o[2],_13q=E(_13m)[1];if(!B(_Y(_13p,_11w))){return new F(function(){return _11A(_13q,B(_13f(_13l,_13p)));});}else{return new F(function(){return _11A(_13q,B(_13f(_13l,B(_ck(B(_cu(B(_cC(_13j,_13o[1])))),_13p)))));});}break;default:return E(_Zv);}},_13r=[0,37],_13s=[0,16],_13t=[0,10],_13u=[0,8],_13v=[0,43],_13w=[1,_13v,_u],_13x=[0,32],_13y=function(_13z){return new F(function(){return _Zs(new T(function(){return B(unAppCStr("bad formatting char ",[1,_13z,_u]));}));});},_13A=function(_13B,_13C){var _13D=E(_13B);if(!_13D){return [0];}else{var _13E=E(_13C);return _13E[0]==0?[0]:[1,_13E[1],new T(function(){return B(_13A(_13D-1|0,_13E[2]));})];}},_13F=function(_13G,_13H){var _13I=E(_13G);if(!_13I[0]){return E(_13H)[0]==0?[0]:E(_13e);}else{var _13J=_13I[2],_13K=E(_13I[1]);if(E(_13K[1])==37){var _13L=function(_13M){var _13N=E(_13H);if(!_13N[0]){return E(_129);}else{var _13O=B(_12g(_r,_r,_r,_13J,_13N)),_13P=_13O[2],_13Q=_13O[4],_13R=E(_13O[6]);if(!_13R[0]){return E(_13e);}else{var _13S=_13R[2],_13T=E(_13O[7]);if(!_13T[0]){return E(_129);}else{var _13U=_13T[1],_13V=_13T[2],_13W=E(_13R[1]),_13X=function(_13Y,_13Z){var _140=new T(function(){var _141=B(_sT(_13Z,0)),_142=B(_sT(_13Y,0)),_143=E(_13O[1])[1];if((_141+_142|0)>=_143){var _144=[0];}else{var _145=_143-(_141+_142|0)|0;if(_145>0){if(_145<0){var _146=[0];}else{var _147=new T(function(){return [1,new T(function(){return !E(_13Q)?E(_13x):E(_11z);}),_147];}),_146=B(_13A(_145,_147));}var _148=_146,_149=_148;}else{var _149=[0];}var _14a=_149,_14b=_14a,_14c=_14b,_144=_14c;}var _14d=_144,_14e=_14d,_14f=_14e,_14g=_14f,_14h=_14g;return _14h;});return !E(_13O[3])?!E(_13Q)?B(_O(_140,new T(function(){return B(_O(_13Y,_13Z));}))):B(_O(_13Y,new T(function(){return B(_O(_140,_13Z));}))):B(_O(_13Y,new T(function(){return B(_O(_13Z,_140));})));},_14i=function(_14j,_14k){var _14l=E(_14j);return _14l[0]==0?!E(_13O[5])?B(_13X(_u,_14k)):B(_13X(_13w,_14k)):B(_13X(_14l,_14k));};switch(E(_13W[1])){case 69:var _14m=B(_10L(69,_13P,_13U));return new F(function(){return _O(B(_14i(_14m[1],_14m[2])),new T(function(){return B(_13F(_13S,_13V));}));});break;case 71:var _14n=B(_10L(71,_13P,_13U));return new F(function(){return _O(B(_14i(_14n[1],_14n[2])),new T(function(){return B(_13F(_13S,_13V));}));});break;case 88:return new F(function(){return _O(B(_13X(_u,new T(function(){return B(_1u(_10J,B(_13k(_13s,_13P,_13U))));}))),new T(function(){return B(_13F(_13S,_13V));}));});break;case 99:return new F(function(){return _O(B(_13X(_u,[1,new T(function(){var _14o=E(_13U);switch(_14o[0]){case 0:var _14p=E(_14o[1])[1];if(_14p>>>0>1114111){var _14q=B(_dW(_14p));}else{var _14q=[0,_14p];}var _14r=_14q,_14s=_14r,_14t=_14s,_14u=_14t,_14v=_14u;break;case 2:var _14w=B(_dY(_14o[2]));if(_14w>>>0>1114111){var _14x=B(_dW(_14w));}else{var _14x=[0,_14w];}var _14y=_14x,_14z=_14y,_14A=_14z,_14v=_14A;break;default:var _14v=E(_Zv);}return _14v;}),_u])),new T(function(){return B(_13F(_13S,_13V));}));});break;case 100:var _14B=B(_11I(_13P,_13U));return new F(function(){return _O(B(_14i(_14B[1],_14B[2])),new T(function(){return B(_13F(_13S,_13V));}));});break;case 101:var _14C=B(_10L(101,_13P,_13U));return new F(function(){return _O(B(_14i(_14C[1],_14C[2])),new T(function(){return B(_13F(_13S,_13V));}));});break;case 102:var _14D=B(_10L(102,_13P,_13U));return new F(function(){return _O(B(_14i(_14D[1],_14D[2])),new T(function(){return B(_13F(_13S,_13V));}));});break;case 103:var _14E=B(_10L(103,_13P,_13U));return new F(function(){return _O(B(_14i(_14E[1],_14E[2])),new T(function(){return B(_13F(_13S,_13V));}));});break;case 105:var _14F=B(_11I(_13P,_13U));return new F(function(){return _O(B(_14i(_14F[1],_14F[2])),new T(function(){return B(_13F(_13S,_13V));}));});break;case 111:return new F(function(){return _O(B(_13X(_u,new T(function(){return B(_13k(_13u,_13P,_13U));}))),new T(function(){return B(_13F(_13S,_13V));}));});break;case 115:return new F(function(){return _O(B(_13X(_u,new T(function(){var _14G=E(_13U);if(_14G[0]==1){var _14H=_14G[1],_14I=E(_13P)[1];if(_14I<0){var _14J=E(_14H);}else{var _14J=_14I>0?B(_13A(_14I,_14H)):[0];}var _14K=_14J,_14L=_14K,_14M=_14L;}else{var _14M=E(_Zv);}return _14M;}))),new T(function(){return B(_13F(_13S,_13V));}));});break;case 117:return new F(function(){return _O(B(_13X(_u,new T(function(){return B(_13k(_13t,_13P,_13U));}))),new T(function(){return B(_13F(_13S,_13V));}));});break;case 120:return new F(function(){return _O(B(_13X(_u,new T(function(){return B(_13k(_13s,_13P,_13U));}))),new T(function(){return B(_13F(_13S,_13V));}));});break;default:return new F(function(){return _13y(_13W);});}}}}},_14N=E(_13J);if(!_14N[0]){return new F(function(){return _13L(_);});}else{if(E(E(_14N[1])[1])==37){return [1,_13r,new T(function(){return B(_13F(_14N[2],_13H));})];}else{return new F(function(){return _13L(_);});}}}else{return [1,_13K,new T(function(){return B(_13F(_13J,_13H));})];}}},_xi=function(_14O,_){var _14P=jsFind(toJSStr(E(_0))),_14Q=_14P,_14R=E(_14Q);if(!_14R[0]){return new F(function(){return _Do(_uC,_);});}else{var _14S=jsSet(E(_14R[1])[1],toJSStr(E(_sN)),toJSStr(B(_O(_H9,new T(function(){return B(unAppCStr("<tbody>",new T(function(){var _14T=function(_14U){var _14V=E(_14U);if(!_14V[0]){return [0];}else{var _14W=E(_14V[1]),_14X=function(_14Y){var _14Z=E(_14Y);return _14Z[0]==0?E(new T(function(){return B(_14T(_14V[2]));})):[1,_14Z[1],new T(function(){return B(_14X(_14Z[2]));})];};return new F(function(){return _14X(B(_13F(_H8,new T(function(){return B(_Ha([1,[1,new T(function(){return B(_1u(_uv,_14W[2]));})],[1,[1,new T(function(){return B(_1u(_uv,_14W[1]));})],_u]],_u));}))));});}};return B(_O(B(_14T(B(_1u(function(_150){var _151=E(_150)[1],_152=B(_ux(_151,new T(function(){return E(E(_14O)[6]);})));if(!_152[0]){return [0,_151,_u];}else{var _153=E(_152[1]);return _153[0]==0?[0,_151,_u]:[0,_151,_153[1]];}},_H6)))),_H7));})));})))));return [0,_b6,_14O];}},_154=new T(function(){return [0,"click"];}),_155=new T(function(){return B(unCStr("\u300d\u3092\u8cfc\u5165\u3057\u307e\u3057\u305f"));}),_156=new T(function(){return B(_9N("main.hs:(440,1)-(457,24)|function btnEvents"));}),_157=[0,12300],_158=function(_159,_15a,_15b){return new F(function(){return _Ob(_15a,_15b);});},_15c=new T(function(){return B(unCStr("-btn"));}),_15d=function(_15e,_15f,_){var _15g=E(_15f);if(!_15g[0]){return E(_156);}else{var _15h=E(_15g[1]),_15i=_15h[1],_15j=function(_,_15k){var _15l=E(_15k);if(!_15l[0]){return _b6;}else{var _15m=E(_154)[1],_15n=jsSetCB(E(_15l[1])[1],_15m,function(_15o,_15p,_){var _15q=E(_15e)[1],_15r=rMV(_15q),_15s=_15r,_15t=E(new T(function(){return B(_rv(_FJ,_15i));})),_15u=B(A(_15t[2],[_15h,new T(function(){var _15v=E(_15s),_15w=new T(function(){return B(_rI(_158,_15i,_sa,_15v[7]));});return [0,new T(function(){return [0,E(_15v[1])[1]-B(_xj(B(A(_15t[1],[new T(function(){return [0,B(_rv(_15w,_15i))[1]-1|0];})]))))];}),_15v[2],_15v[3],_15v[4],_15v[5],_15v[6],_15w,_15v[8],_15v[9],_15v[10]];}),_])),_15x=_15u,_15y=B(_tW(_x6,[1,_157,new T(function(){return B(_O(E(_15t[3])[3],_155));})],_)),_15z=_15y,_15A=new T(function(){return E(E(_15x)[2]);}),_15B=B(A(_x5,[_15A,_])),_15C=_15B,_15D=B(_xi(_15A,_)),_15E=_15D,_=wMV(_15q,new T(function(){return E(E(_15E)[2]);})),_15F=rMV(_15q),_15G=_15F,_15H=E(_15G),_15I=jsLog(toJSStr(B(A(_qY,[0,_15H[1],_15H[2],_15H[3],_15H[4],_15H[5],_15H[6],_15H[7],_15H[8],_15H[9],_15H[10],_u]))));return _b6;}),_15J=_15n,_15K=function(_15L,_15M,_){var _15N=E(_15M);if(!_15N[0]){return E(_156);}else{var _15O=E(_15N[1]),_15P=_15O[1],_15Q=function(_,_15R){var _15S=E(_15R);if(!_15S[0]){return _b6;}else{var _15T=jsSetCB(E(_15S[1])[1],_15m,function(_15U,_15V,_){var _15W=E(_15L)[1],_15X=rMV(_15W),_15Y=_15X,_15Z=E(new T(function(){return B(_rv(_FJ,_15P));})),_160=B(A(_15Z[2],[_15O,new T(function(){var _161=E(_15Y),_162=new T(function(){return B(_rI(_158,_15P,_sa,_161[7]));});return [0,new T(function(){return [0,E(_161[1])[1]-B(_xj(B(A(_15Z[1],[new T(function(){return [0,B(_rv(_162,_15P))[1]-1|0];})]))))];}),_161[2],_161[3],_161[4],_161[5],_161[6],_162,_161[8],_161[9],_161[10]];}),_])),_163=_160,_164=B(_tW(_x6,[1,_157,new T(function(){return B(_O(E(_15Z[3])[3],_155));})],_)),_165=_164,_166=new T(function(){return E(E(_163)[2]);}),_167=B(A(_x5,[_166,_])),_168=_167,_169=B(_xi(_166,_)),_16a=_169,_=wMV(_15W,new T(function(){return E(E(_16a)[2]);})),_16b=rMV(_15W),_16c=_16b,_16d=E(_16c),_16e=jsLog(toJSStr(B(A(_qY,[0,_16d[1],_16d[2],_16d[3],_16d[4],_16d[5],_16d[6],_16d[7],_16d[8],_16d[9],_16d[10],_u]))));return _b6;}),_16f=_15T;return new F(function(){return _15K(_15L,_15N[2],_);});}};if(_15P<=0){var _16g=jsFind(toJSStr(B(unAppCStr("item-sp-",new T(function(){if(_15P<0){var _16h=B(_O(B(_dQ(0, -_15P,_u)),_15c));}else{var _16h=B(_O(B(_dQ(0,_15P,_u)),_15c));}var _16i=_16h;return _16i;}))))),_16j=_16g;return new F(function(){return _15Q(_,_16j);});}else{var _16k=jsFind(toJSStr(B(unAppCStr("item-",new T(function(){return B(_O(B(_dQ(0,_15P,_u)),_15c));}))))),_16l=_16k;return new F(function(){return _15Q(_,_16l);});}}};return new F(function(){return _15K(_15e,_15g[2],_);});}};if(_15i<=0){var _16m=jsFind(toJSStr(B(unAppCStr("item-sp-",new T(function(){if(_15i<0){var _16n=B(_O(B(_dQ(0, -_15i,_u)),_15c));}else{var _16n=B(_O(B(_dQ(0,_15i,_u)),_15c));}var _16o=_16n;return _16o;}))))),_16p=_16m;return new F(function(){return _15j(_,_16p);});}else{var _16q=jsFind(toJSStr(B(unAppCStr("item-",new T(function(){return B(_O(B(_dQ(0,_15i,_u)),_15c));}))))),_16r=_16q;return new F(function(){return _15j(_,_16r);});}}},_16s=function(_){return _b6;},_16t=function(_16u){var _16v=E(_16u);if(!_16v[0]){return [0,_u,_u];}else{var _16w=E(_16v[1]),_16x=new T(function(){var _16y=B(_16t(_16v[2]));return [0,_16y[1],_16y[2]];});return E(_16w[1])[1]<=0?[0,new T(function(){return E(E(_16x)[1]);}),[1,_16w,new T(function(){return E(E(_16x)[2]);})]]:[0,[1,_16w,new T(function(){return E(E(_16x)[1]);})],new T(function(){return E(E(_16x)[2]);})];}},_16z=new T(function(){var _16A=B(_16t(_FB));return [0,_16A[1],_16A[2]];}),_16B=new T(function(){return E(E(_16z)[1]);}),_16C=new T(function(){return E(E(_16z)[2]);}),_16D=function(_16E){return _16E>0;},_16F=new T(function(){return B(_sC("(function(x) {return x === null;})"));}),_16G=new T(function(){return B(unCStr("No such value"));}),_16H=[0,_16G],_16I=new T(function(){return B(unCStr("Invalid JSON!"));}),_16J=[0,_16I],_16K=new T(function(){return [0,"(function(k) {return localStorage.getItem(k);})"];}),_16L=function(_16M,_16N,_){var _16O=B(A(_sC,[E(_16K)[1],E(toJSStr(E(_16N))),_])),_16P=_16O;return new T(function(){if(!B(_l7(function(_){var _=0,_16Q=B(A(_16F,[E(_16P),_])),_16R=_16Q;return new T(function(){return B(_16D(_16R));});}))){var _16S=String(_16P),_16T=_16S,_16U=jsParseJSON(_16T),_16V=_16U,_16W=E(_16V),_16X=_16W[0]==0?E(_16J):B(A(_2P,[_16M,_16W[1]]));}else{var _16X=E(_16H);}return _16X;});},_16Y=[0,10],_16Z=[1,_16Y,_u],_170=function(_171,_172,_){var _173=jsWriteHandle(E(_171)[1],toJSStr(E(_172)));return _b6;},_174=function(_175,_176,_){var _177=E(_175),_178=jsWriteHandle(_177[1],toJSStr(E(_176)));return new F(function(){return _170(_177,_16Z,_);});},_179=new T(function(){return B(unCStr("btn btn-default btn-buy"));}),_17a=new T(function(){return B(unCStr("item-list"));}),_17b=new T(function(){return B(unCStr("count"));}),_17c=new T(function(){return B(unCStr("tip"));}),_17d=new T(function(){return B(unCStr("list-group-item tooltips"));}),_17e=new T(function(){return B(unCStr("fa fa-plus-circle"));}),_17f=new T(function(){return B(unCStr(" loves"));}),_17g=[0,105],_17h=[1,_17g,_u],_17i=function(_17j,_17k,_17l,_){var _17m=jsCreateElem(toJSStr(_17h)),_17n=_17m,_17o=jsAppendChild(_17n,E(_17l)[1]),_17p=[0,_17n],_17q=B(A(_17j,[_17k,_17p,_])),_17r=_17q;return _17p;},_17s=new T(function(){return B(unCStr("li"));}),_17t=function(_17u,_17v,_17w,_){var _17x=jsCreateElem(toJSStr(E(_17s))),_17y=_17x,_17z=jsAppendChild(_17y,E(_17w)[1]),_17A=[0,_17y],_17B=B(A(_17u,[_17v,_17A,_])),_17C=_17B;return _17A;},_17D=[0,48],_17E=[1,_17D,_u],_17F=new T(function(){return B(unCStr("id"));}),_17G=new T(function(){return B(unCStr("-icon"));}),_17H=new T(function(){return B(unCStr("-num"));}),_17I=new T(function(){return B(unCStr("-box"));}),_17J=new T(function(){return B(unCStr("-cost"));}),_17K=function(_17L,_17M,_17N,_17O){var _17P=new T(function(){return B(unAppCStr("item-",new T(function(){var _17Q=E(_17L)[1];return _17Q<=0?B(unAppCStr("sp-",new T(function(){if(_17Q<0){var _17R=B(_dQ(0, -_17Q,_u));}else{var _17R=B(_dQ(0,_17Q,_u));}var _17S=_17R;return _17S;}))):B(_dQ(0,_17Q,_u));})));});return function(_17T,_){var _17U=B(_17t(_tQ,function(_17V,_){var _17W=B(_tv(_tQ,function(_17X,_){var _17Y=B(A(_ss,[_bK,_17X,_tt,_17N,_])),_17Z=_17Y;return _17X;},_17V,_)),_180=_17W,_181=B(A(_sm,[_bK,_180,_sA,_17c,_])),_182=_181,_183=B(_tv(_tQ,function(_184,_){var _185=B(_tv(_tQ,function(_186,_){var _187=B(_17i(_sb,_u,_186,_)),_188=_187,_189=B(A(_sm,[_bK,_188,_sA,new T(function(){return B(unAppCStr("fa ",_17M));}),_])),_18a=_189,_18b=B(_sb(_sz,_186,_)),_18c=_18b;return _186;},_184,_)),_18d=_185,_18e=B(A(_sm,[_bK,_18d,_17F,new T(function(){return B(_O(_17P,_17G));}),_])),_18f=_18e,_18g=B(_tv(_sb,_u,_184,_)),_18h=_18g,_18i=B(A(_sm,[_bK,_18h,_17F,new T(function(){return B(_O(_17P,_17H));}),_])),_18j=_18i;return _184;},_17V,_)),_18k=_183,_18l=B(A(_sm,[_bK,_18k,_sA,_17b,_])),_18m=_18l,_18n=B(_tv(_sb,_17O,_17V,_)),_18o=_18n,_18p=B(A(_sm,[_bK,_18o,_17F,new T(function(){return B(_O(_17P,_17I));}),_])),_18q=_18p,_18r=B(A(_sm,[_bK,_18o,_sA,_17a,_])),_18s=_18r,_18t=B(_v4(_tQ,function(_18u,_){var _18v=B(_17i(_sb,_u,_18u,_)),_18w=_18v,_18x=B(A(_sm,[_bK,_18w,_sA,_17e,_])),_18y=_18x,_18z=B(_sb(_sz,_18u,_)),_18A=_18z,_18B=B(_tv(_sb,_17E,_18u,_)),_18C=_18B,_18D=B(A(_sm,[_bK,_18C,_17F,new T(function(){return B(_O(_17P,_17J));}),_])),_18E=_18D,_18F=B(_sb(_17f,_18u,_)),_18G=_18F;return _18u;},_17V,_)),_18H=_18t,_18I=B(A(_sm,[_bK,_18H,_uE,_uD,_])),_18J=_18I,_18K=B(A(_sm,[_bK,_18H,_17F,new T(function(){return B(_O(_17P,_15c));}),_])),_18L=_18K,_18M=B(A(_sm,[_bK,_18H,_sA,_179,_])),_18N=_18M;return _17V;},_17T,_)),_18O=_17U,_18P=B(A(_sm,[_bK,_18O,_vp,_17P,_])),_18Q=_18P,_18R=B(A(_sm,[_bK,_18O,_sA,_17d,_])),_18S=_18R;return _18O;};},_18T=new T(function(){return B(unCStr("\u30bb\u30fc\u30d6\u3057\u307e\u3057\u305f"));}),_18U=new T(function(){return B(unCStr("auto"));}),_18V=function(_18W,_){var _18X=B(A(_x5,[_18W,_])),_18Y=_18X,_18Z=B(_xi(_18W,_)),_190=_18Z,_191=B(_vs(_18U,_18T,_)),_192=_191;return [0,_192,new T(function(){return E(E(_190)[2]);})];},_193=new T(function(){return B(unCStr("%.2f"));}),_194=[1,_ok,_u],_195=function(_196,_){var _197=jsEval(toJSStr(B(unAppCStr("document.title = ",[1,_ok,new T(function(){return B(_om(B(_1u(_uv,B(_13F(_193,new T(function(){return B(_Ha([1,[4,new T(function(){return E(E(_196)[1]);})],_u],_u));}))))),_194));})])))),_198=_197;return [0,_b6,_196];},_199=function(_19a,_19b,_19c){while(1){var _19d=(function(_19e,_19f,_19g){var _19h=E(_19g);if(!_19h[0]){return [0,_19e,_19f];}else{var _19i=_19h[1];_19a=new T(function(){var _19j=E(E(_19i)[1]);switch(_19j){case -1:var _19k=[0,0];break;case 0:var _19k=E(_N7);break;default:var _19k=[0,B(_NB(E(_19e)[1],_19j))];}var _19l=_19k;return _19l;});var _19m=[1,new T(function(){return [0,B(_Nb(E(_19e)[1],E(_19i)[1]))];}),_19f];_19c=_19h[2];_19b=_19m;return null;}})(_19a,_19b,_19c);if(_19d!=null){return _19d;}}},_19n=function(_19o,_19p,_19q,_19r){return new F(function(){return _199(new T(function(){var _19s=E(E(_19q)[1]);switch(_19s){case -1:var _19t=[0,0];break;case 0:var _19t=E(_N7);break;default:var _19t=[0,B(_NB(E(_19o)[1],_19s))];}var _19u=_19t;return _19u;}),[1,new T(function(){return [0,B(_Nb(E(_19o)[1],E(_19q)[1]))];}),_19p],_19r);});},_19v=function(_19w,_19x){var _19y=E(_19w);if(!_19y[0]){return [0];}else{var _19z=_19y[1];return _19x>1?[1,_19z,new T(function(){return B(_19v(_19y[2],_19x-1|0));})]:[1,_19z,_u];}},_19A=new T(function(){return B(_19(0,_zj,_u));}),_19B=new T(function(){return B(_19(0,_zH,_u));}),_19C=function(_19D,_19E){var _19F=E(_19E);if(!_19F[0]){return [0,_u,_u];}else{var _19G=_19F[1];if(!B(A(_19D,[_19G]))){var _19H=new T(function(){var _19I=B(_19C(_19D,_19F[2]));return [0,_19I[1],_19I[2]];});return [0,[1,_19G,new T(function(){return E(E(_19H)[1]);})],new T(function(){return E(E(_19H)[2]);})];}else{return [0,_u,_19F];}}},_19J=function(_19K,_19L){var _19M=function(_19N,_19O){return !B(_ax(_19O,_u))?[0,_19N,new T(function(){var _19P=B(_19J(_19K,_19O));return [1,_19P[1],_19P[2]];})]:[0,_19N,_u];};if(_19K>=0){var _19Q=B(_Xn(_19K,_19L));return new F(function(){return _19M(_19Q[1],_19Q[2]);});}else{return new F(function(){return _19M(_u,_19L);});}},_19R=function(_19S){var _19T=E(_19S);if(!_19T[0]){return [0];}else{return new F(function(){return _O(_19T[1],new T(function(){return B(_19R(_19T[2]));}));});}},_19U=[0,44],_19V=[1,_19U,_u],_19W=function(_19X){return E(E(_19X)[1])==46?true:false;},_19Y=function(_19Z,_1a0){var _1a1=E(_1a0);return _1a1[0]==0?[0]:[1,_19Z,[1,_1a1[1],new T(function(){return B(_19Y(_19Z,_1a1[2]));})]];},_1a2=function(_1a3){var _1a4=new T(function(){var _1a5=B(_19C(_19W,_1a3));return [0,_1a5[1],_1a5[2]];}),_1a6=B(_19J(3,new T(function(){return B(_Ha(E(_1a4)[1],_u));})));return new F(function(){return _O(B(_Ha(B(_19R([1,_1a6[1],new T(function(){return B(_19Y(_19V,_1a6[2]));})])),_u)),new T(function(){return E(E(_1a4)[2]);}));});},_1a7=function(_1a8){return _1a8>1000?B(_1a2(new T(function(){var _1a9=B(_yX(_1a8)),_1aa=_1a9[1],_1ab=_1a9[2];if(_1ab>=0){var _1ac=B(_19(0,B(_zI(_1aa,_1ab)),_u));}else{var _1ad= -_1ab;if(_1ad<=52){var _1ae=hs_uncheckedIShiftRA64(B(_zg(_1aa)),_1ad),_1af=_1ae,_1ag=B(_19(0,B(_z0(_1af)),_u));}else{var _1ag=!B(_Y(_1aa,_zj))?E(_19A):E(_19B);}var _1ah=_1ag,_1ai=_1ah,_1ac=_1ai;}var _1aj=_1ac,_1ak=_1aj;return _1ak;}))):B(_1a2(new T(function(){return B(_19v(B(_13F(_193,new T(function(){return B(_Ha([1,[4,[0,_1a8]],_u],_u));}))),5));})));},_1al=function(_1am,_1an){return new F(function(){return (function(_1ao){while(1){var _1ap=E(_1ao);switch(_1ap[0]){case 0:var _1aq=_1ap[2]>>>0;if(((_1am>>>0&((_1aq-1>>>0^4294967295)>>>0^_1aq)>>>0)>>>0&4294967295)==_1ap[1]){if(!((_1am>>>0&_1aq)>>>0)){_1ao=_1ap[3];continue;}else{_1ao=_1ap[4];continue;}}else{return [0];}break;case 1:return _1am!=_1ap[1]?[0]:[1,_1ap[2]];default:return [0];}}})(_1an);});},_1ar=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:308:9-14"));}),_1as=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:295:7-12"));}),_1at=[1,_sa,_u],_1au=[0,5],_1av=[1,_1au,_1at],_1aw=[1,_GL,_1av],_1ax=[1,_FZ,_1aw],_1ay=[1,_yn,_1ax],_1az=[0,500],_1aA=[1,_1az,_1ay],_1aB=new T(function(){return B(_19R(_u));}),_1aC=new T(function(){return B(_1u(_uv,_1aB));}),_1aD=[0,-2147483648],_1aE=new T(function(){return B(unCStr("disabled"));}),_1aF=new T(function(){return B(unCStr("%s<span class=\"item-%d\">%s</span>"));}),_1aG=new T(function(){return B(_sC("(function(e,c){e.removeAttribute(c);})"));}),_1aH=function(_1aI){return function(_1aJ,_){var _1aK=B(A(new T(function(){return B(A(_1aG,[E(E(_1aI)[1])]));}),[E(toJSStr(E(_1aJ))),_])),_1aL=_1aK;return _b6;};},_1aM=function(_1aN,_1aO){var _1aP=E(_1aN);if(!_1aP[0]){return [0];}else{var _1aQ=E(_1aO);return _1aQ[0]==0?[0]:[1,[0,_1aP[1],_1aQ[1]],new T(function(){return B(_1aM(_1aP[2],_1aQ[2]));})];}},_1aR=function(_,_1aS){var _1aT=jsFind(toJSStr(E(_6))),_1aU=_1aT,_1aV=E(_1aU);if(!_1aV[0]){return new F(function(){return _Do(_1as,_);});}else{var _1aW=E(_sN),_1aX=toJSStr(_1aW),_1aY=E(E(_1aS)[2]),_1aZ=_1aY[7],_1b0=jsSet(E(_1aV[1])[1],_1aX,toJSStr(B(_1a7(E(_1aY[2])[1])))),_1b1=jsFind(toJSStr(E(_8))),_1b2=_1b1,_1b3=E(_1b2);if(!_1b3[0]){return new F(function(){return _Do(_1as,_);});}else{var _1b4=E(_1aY[1])[1],_1b5=jsSet(E(_1b3[1])[1],_1aX,toJSStr(B(_1a7(_1b4)))),_1b6=jsFind(toJSStr(E(_4))),_1b7=_1b6,_1b8=E(_1b7);if(!_1b8[0]){return new F(function(){return _Do(_1as,_);});}else{var _1b9=jsSet(E(_1b8[1])[1],_1aX,toJSStr(B(_1a7(E(_1aY[3])[1])))),_1ba=function(_1bb){var _1bc=E(_1bb);return _1bc[0]==0?E(_C1):function(_1bd,_){var _1be=B(A(new T(function(){var _1bf=E(_1bc[1]),_1bg=_1bf[1],_1bh=E(_1bf[2])[1],_1bi=new T(function(){var _1bj=E(_1bg)[1];return _1bj<=0?B(unAppCStr("item-sp-",new T(function(){if(_1bj<0){var _1bk=B(_dQ(0, -_1bj,_u));}else{var _1bk=B(_dQ(0,_1bj,_u));}var _1bl=_1bk;return _1bl;}))):B(unAppCStr("item-",new T(function(){return B(_dQ(0,_1bj,_u));})));}),_1bm=new T(function(){var _1bn=B(_1al(E(_1bg)[1],_1aZ));return _1bn[0]==0?E(_qI):E(_1bn[1]);});return function(_1bo,_){var _1bp=B(A(new T(function(){if(E(_1bg)[1]<=0){var _1bq=E(_C1);}else{var _1bq=function(_1br,_){var _1bs=E(new T(function(){return B(_O(_1bi,_17I));})),_1bt=jsFind(toJSStr(_1bs)),_1bu=_1bt,_1bv=E(_1bu);if(!_1bv[0]){return new F(function(){return _tU(_1bs);});}else{var _1bw=jsFind(toJSStr(E(new T(function(){return B(_O(_1bi,_17G));})))),_1bx=_1bw,_1by=E(_1bx);if(!_1by[0]){return new F(function(){return _Do(_1ar,_);});}else{var _1bz=jsGet(E(_1by[1])[1],toJSStr(_1aW)),_1bA=_1bz,_1bB=new T(function(){return fromJSStr(_1bA);}),_1bC=function(_1bD){return _1bD>1?[1,_1bB,new T(function(){return B(_1bC(_1bD-1|0));})]:E([1,_1bB,_u]);},_1bE=jsSet(E(_1bv[1])[1],_1aX,toJSStr(B((function(_1bF,_1bG){while(1){var _1bH=(function(_1bI,_1bJ){var _1bK=E(_1bJ);if(!_1bK[0]){return E(_1bI);}else{var _1bL=E(_1bK[1]);_1bF=B(_1u(_uv,B(_13F(_1aF,new T(function(){return B(_Ha([1,[1,new T(function(){var _1bM=E(_1bL[2])[1];if(_1bM>0){var _1bN=B(_1u(_uv,B(_19R(B(_1bC(_1bM))))));}else{var _1bN=E(_1aC);}var _1bO=_1bN,_1bP=_1bO;return _1bP;})],[1,[2,_1aD,new T(function(){return B(_cA(E(_1bL[1])[1]));})],[1,[1,new T(function(){return B(_1u(_uv,_1bI));})],_u]]],_u));})))));_1bG=_1bK[2];return null;}})(_1bF,_1bG);if(_1bH!=null){return _1bH;}}})(_u,new T(function(){return B(_1aM(_1aA,new T(function(){return B(_Ha(B(_19n(_1bm,_u,_1az,_1ay))[2],_u));})));}))))),_1bQ=E(new T(function(){return B(_O(_1bi,_17H));})),_1bR=jsFind(toJSStr(_1bQ)),_1bS=_1bR,_1bT=E(_1bS);return _1bT[0]==0?B(_tU(_1bQ)):B(A(_ss,[_DN,_1bT[1],_1aW,new T(function(){return B(_dQ(0,E(_1bm)[1],_u));}),_1br,_]));}}};}var _1bU=_1bq,_1bV=_1bU;return _1bV;}),[_1bo,_])),_1bW=_1bp,_1bX=E(new T(function(){return B(_O(_1bi,_15c));})),_1bY=jsFind(toJSStr(_1bX)),_1bZ=_1bY,_1c0=E(_1bZ);if(!_1c0[0]){return new F(function(){return _tU(_1bX);});}else{var _1c1=_1c0[1];if(!E(new T(function(){var _1c2=E(_1bg)[1];if(_1c2<=0){if(!B(_rC(_1c2,_1aZ))){var _1c3=B(_xj(B(A(_1bh,[_1bm]))))<=_1b4;}else{if(B(_rv(_1aZ,_1c2))[1]>=1){var _1c4=false;}else{var _1c4=B(_xj(B(A(_1bh,[_1bm]))))<=_1b4;}var _1c5=_1c4,_1c6=_1c5,_1c3=_1c6;}var _1c7=_1c3;}else{var _1c7=B(_xj(B(A(_1bh,[_1bm]))))<=_1b4;}var _1c8=_1c7,_1c9=_1c8;return _1c9;}))){var _1ca=B(A(_sm,[_DN,_1c1,_1aE,_1aE,new T(function(){return E(E(_1bW)[2]);}),_])),_1cb=_1ca,_1cc=B(_O(_1bi,_17J)),_1cd=jsFind(toJSStr(_1cc)),_1ce=_1cd,_1cf=E(_1ce);if(!_1cf[0]){return new F(function(){return _tU(_1cc);});}else{var _1cg=jsSet(E(_1cf[1])[1],toJSStr(_1aW),toJSStr(B(_1a2(new T(function(){return B(_19(0,B(A(_1bh,[_1bm])),_u));}))))),_1ch=function(_1ci,_){var _1cj=E(_1bi),_1ck=jsFind(toJSStr(_1cj)),_1cl=_1ck,_1cm=E(_1cl);if(!_1cm[0]){return new F(function(){return _tU(_1cj);});}else{var _1cn=E(_1cm[1]),_1co=E(_DP),_1cp=jsGetStyle(_1cn[1],toJSStr(_1co)),_1cq=_1cp;return !B(_aJ(fromJSStr(_1cq),_C0))?B(A(_BP,[_DN,_1cn,_1co,_C0,_1ci,_])):[0,_b6,_1ci];}};if(!B(_rC(E(_1bg)[1],_1aZ))){var _1cr=E(E(_1cb)[2]);return B(_xj(B(A(_1bh,[_qI]))))>3*E(_1cr[8])[1]?[0,_b6,_1cr]:B(_1ch(_1cr,_));}else{return new F(function(){return _1ch(new T(function(){return E(E(_1cb)[2]);}),_);});}}}else{var _1cs=B(A(_1aH,[_1c1,_1aE,_])),_1ct=_1cs,_1cu=B(_O(_1bi,_17J)),_1cv=jsFind(toJSStr(_1cu)),_1cw=_1cv,_1cx=E(_1cw);if(!_1cx[0]){return new F(function(){return _tU(_1cu);});}else{var _1cy=jsSet(E(_1cx[1])[1],toJSStr(_1aW),toJSStr(B(_1a2(new T(function(){return B(_19(0,B(A(_1bh,[_1bm])),_u));}))))),_1cz=function(_1cA,_){var _1cB=E(_1bi),_1cC=jsFind(toJSStr(_1cB)),_1cD=_1cC,_1cE=E(_1cD);if(!_1cE[0]){return new F(function(){return _tU(_1cB);});}else{var _1cF=E(_1cE[1]),_1cG=E(_DP),_1cH=jsGetStyle(_1cF[1],toJSStr(_1cG)),_1cI=_1cH;return !B(_aJ(fromJSStr(_1cI),_C0))?B(A(_BP,[_DN,_1cF,_1cG,_C0,_1cA,_])):[0,_b6,_1cA];}};if(!B(_rC(E(_1bg)[1],_1aZ))){var _1cJ=E(E(_1bW)[2]);return B(_xj(B(A(_1bh,[_qI]))))>3*E(_1cJ[8])[1]?[0,_b6,_1cJ]:B(_1cz(_1cJ,_));}else{return new F(function(){return _1cz(new T(function(){return E(E(_1bW)[2]);}),_);});}}}}};}),[_1bd,_])),_1cK=_1be;return new F(function(){return A(new T(function(){return B(_1ba(_1bc[2]));}),[new T(function(){return E(E(_1cK)[2]);}),_]);});};};return new F(function(){return A(_1ba,[_FB,_1aY,_]);});}}}},_1cL=new T(function(){return B(unCStr("Map.!: given key is not an element in the map"));}),_1cM=new T(function(){return B(err(_1cL));}),_1cN=function(_1cO,_1cP){while(1){var _1cQ=E(_1cO),_1cR=E(_1cP);if(!_1cR[0]){switch(B(_41(_1cQ,_1cR[2]))){case 0:_1cO=_1cQ;_1cP=_1cR[4];continue;case 1:return E(_1cR[3]);default:_1cO=_1cQ;_1cP=_1cR[5];continue;}}else{return E(_1cM);}}},_1cS=function(_1cT,_1cU){while(1){var _1cV=E(_1cT),_1cW=E(_1cU);if(!_1cW[0]){switch(B(_41(_1cV,_1cW[2]))){case 0:_1cT=_1cV;_1cU=_1cW[4];continue;case 1:return true;default:_1cT=_1cV;_1cU=_1cW[5];continue;}}else{return false;}}},_1cX=function(_1cY,_1cZ,_){while(1){var _1d0=(function(_1d1,_1d2,_){var _1d3=E(_1d1);if(!_1d3[0]){return [0,_b6,_1d2];}else{var _1d4=_1d3[2],_1d5=E(_1d3[1]),_1d6=_1d5[1],_1d7=_1d5[2],_1d8=E(_1d2),_1d9=_1d8[6];if(!B(_1cS(_1d6,_1d9))){var _1da=B(A(_1d7,[_1d8,_])),_1db=_1da;_1cY=_1d4;_1cZ=new T(function(){return E(E(_1db)[2]);});return null;}else{if(!B(_1cN(_1d6,_1d9))[0]){var _1dc=B(A(_1d7,[_1d8,_])),_1dd=_1dc;_1cY=_1d4;_1cZ=new T(function(){return E(E(_1dd)[2]);});return null;}else{return new F(function(){return _1de(_1d4,_1d8[1],_1d8[2],_1d8[3],_1d8[4],_1d8[5],_1d9,_1d8[7],_1d8[8],_1d8[9],_1d8[10],_);});}}}})(_1cY,_1cZ,_);if(_1d0!=null){return _1d0;}}},_1de=function(_1df,_1dg,_1dh,_1di,_1dj,_1dk,_1dl,_1dm,_1dn,_1do,_1dp,_){while(1){var _1dq=(function(_1dr,_1ds,_1dt,_1du,_1dv,_1dw,_1dx,_1dy,_1dz,_1dA,_1dB,_){var _1dC=E(_1dr);if(!_1dC[0]){return [0,_b6,[0,_1ds,_1dt,_1du,_1dv,_1dw,_1dx,_1dy,_1dz,_1dA,_1dB]];}else{var _1dD=_1dC[2],_1dE=E(_1dC[1]),_1dF=_1dE[1],_1dG=_1dE[2];if(!B(_1cS(_1dF,_1dx))){var _1dH=B(A(_1dG,[[0,_1ds,_1dt,_1du,_1dv,_1dw,_1dx,_1dy,_1dz,_1dA,_1dB],_])),_1dI=_1dH;return new F(function(){return _1cX(_1dD,new T(function(){return E(E(_1dI)[2]);}),_);});}else{if(!B(_1cN(_1dF,_1dx))[0]){var _1dJ=B(A(_1dG,[[0,_1ds,_1dt,_1du,_1dv,_1dw,_1dx,_1dy,_1dz,_1dA,_1dB],_])),_1dK=_1dJ;return new F(function(){return _1cX(_1dD,new T(function(){return E(E(_1dK)[2]);}),_);});}else{_1df=_1dD;var _1dL=_1ds,_1dM=_1dt,_1dN=_1du,_1dO=_1dv,_1dP=_1dw,_1dQ=_1dx,_1dR=_1dy,_1dS=_1dz,_1dT=_1dA,_1dU=_1dB;_1dg=_1dL;_1dh=_1dM;_1di=_1dN;_1dj=_1dO;_1dk=_1dP;_1dl=_1dQ;_1dm=_1dR;_1dn=_1dS;_1do=_1dT;_1dp=_1dU;return null;}}}})(_1df,_1dg,_1dh,_1di,_1dj,_1dk,_1dl,_1dm,_1dn,_1do,_1dp,_);if(_1dq!=null){return _1dq;}}},_1dV=new T(function(){return B(unCStr("reset"));}),_1dW=new T(function(){return B(unCStr("resetAll"));}),_1dX=[1,_1dW,_u],_1dY=[1,_1dV,_1dX],_1dZ=[1,_E0,_1dY],_1e0=[1,_DQ,_1dZ],_1e1=new T(function(){return B(_1aM(_1e0,_Fr));}),_1e2=new T(function(){return B(_7P(_1e1));}),_1e3=new T(function(){return B(_1cN(_DQ,_1e2));}),_1e4=function(_1e5){return new F(function(){return err(B(unAppCStr("docFocused: ",[1,_ok,new T(function(){return B(_om(_1e5,_194));})])));});},_1e6=new T(function(){return B(unCStr("\u304a\u304b\u3048\u308a\u306a\u3055\u3044\uff01<br>(\u4f9d\u5b58\u5ea6\u30dc\u30fc\u30ca\u30b9 +"));}),_1e7=new T(function(){return B(unCStr("false"));}),_1e8=new T(function(){return B(unCStr("document.hasFocus()"));}),_1e9=[0,41],_1ea=[1,_1e9,_u],_1eb=new T(function(){return B(unCStr("\u611b\u3061\u3083\u3093"));}),_1ec=function(_1ed,_1ee){while(1){var _1ef=E(_1ed);if(!_1ef[0]){var _1eg=_1ef[1],_1eh=E(_1ee);if(!_1eh[0]){var _1ei=_1eh[1],_1ej=subC(_1eg,_1ei);if(!E(_1ej[2])){return [0,_1ej[1]];}else{_1ed=[1,I_fromInt(_1eg)];_1ee=[1,I_fromInt(_1ei)];continue;}}else{_1ed=[1,I_fromInt(_1eg)];_1ee=_1eh;continue;}}else{var _1ek=E(_1ee);if(!_1ek[0]){_1ed=_1ef;_1ee=[1,I_fromInt(_1ek[1])];continue;}else{return [1,I_sub(_1ef[1],_1ek[1])];}}}},_1el=function(_1em,_){var _1en=E(_1em),_1eo=_1en[1],_1ep=_1en[2],_1eq=_1en[3],_1er=_1en[4],_1es=_1en[6],_1et=_1en[7],_1eu=_1en[8],_1ev=_1en[9],_1ew=_1en[10];if(!B(_rC(E(_1e3)[1],_1et))){return new F(function(){return _1aR(_,[0,_b6,_1en]);});}else{var _1ex=jsEval(toJSStr(E(_1e8))),_1ey=_1ex,_1ez=B(_kY(_)),_1eA=_1ez,_1eB=fromJSStr(_1ey);if(!B(_ax(_1eB,_1e7))){if(!B(_ax(_1eB,_uJ))){return new F(function(){return _1e4(_1eB);});}else{var _1eC=new T(function(){return [0,B(_xj(B(_1ec(_1eA,_1er))))];});if(!E(_1en[5])){var _1eD=new T(function(){return [0,E(_1eC)[1]/1000/50*E(_1ev)[1]];}),_1eE=B(_vs(_1eb,new T(function(){return B(_O(_1e6,new T(function(){return B(_O(B(_1a7(E(_1eD)[1])),_1ea));})));}),_)),_1eF=_1eE,_1eG=B(_1de(_H6,new T(function(){return [0,E(_1eo)[1]+E(_1ep)[1]/30];}),new T(function(){return [0,E(_1ep)[1]+E(_1eC)[1]/1000/100*E(_1ew)[1]];}),new T(function(){return [0,E(_1eq)[1]+E(_1eD)[1]+E(_1eC)[1]/1000/1000*E(_1ev)[1]];}),_1eA,_n,_1es,_1et,_1eu,_1ev,_1ew,_)),_1eH=_1eG,_1eI=E(E(_1eH)[2]),_1eJ=E(_1eI[1]);return _1eJ[1]<=E(_1eI[8])[1]?B(_1aR(_,[0,_b6,_1eI])):B(_1aR(_,[0,_b6,[0,_1eJ,_1eI[2],_1eI[3],_1eI[4],_1eI[5],_1eI[6],_1eI[7],_1eJ,_1eI[9],_1eI[10]]]));}else{var _1eK=B(_1de(_H6,new T(function(){return [0,E(_1eo)[1]+E(_1ep)[1]/30];}),new T(function(){return [0,E(_1ep)[1]+E(_1eC)[1]/1000/100*E(_1ew)[1]];}),new T(function(){return [0,E(_1eq)[1]+E(_1eC)[1]/1000/1000*E(_1ev)[1]];}),_1eA,_n,_1es,_1et,_1eu,_1ev,_1ew,_)),_1eL=_1eK,_1eM=E(E(_1eL)[2]),_1eN=E(_1eM[1]);return _1eN[1]<=E(_1eM[8])[1]?B(_1aR(_,[0,_b6,_1eM])):B(_1aR(_,[0,_b6,[0,_1eN,_1eM[2],_1eM[3],_1eM[4],_1eM[5],_1eM[6],_1eM[7],_1eN,_1eM[9],_1eM[10]]]));}}}else{var _1eO=E(_1eq)[1],_1eP=new T(function(){return [0,1.0e-2*E(_1ew)[1]];});if(_1eO<=0){var _1eQ=B(_1de(_H6,new T(function(){return [0,E(_1eo)[1]+E(_1ep)[1]/30];}),_1ep,new T(function(){var _1eR=_1eO-E(_1eP)[1];return _1eR>0?[0,_1eR]:E(_l2);}),_1er,_r,_1es,_1et,_1eu,_1ev,_1ew,_)),_1eS=_1eQ,_1eT=E(E(_1eS)[2]),_1eU=E(_1eT[1]);return _1eU[1]<=E(_1eT[8])[1]?B(_1aR(_,[0,_b6,_1eT])):B(_1aR(_,[0,_b6,[0,_1eU,_1eT[2],_1eT[3],_1eT[4],_1eT[5],_1eT[6],_1eT[7],_1eU,_1eT[9],_1eT[10]]]));}else{var _1eV=B(_1de(_H6,new T(function(){return [0,E(_1eo)[1]+E(_1ep)[1]/30];}),new T(function(){return [0,E(_1ep)[1]+E(_1eP)[1]];}),new T(function(){var _1eW=_1eO-E(_1eP)[1];return _1eW>0?[0,_1eW]:E(_l2);}),_1er,_r,_1es,_1et,_1eu,_1ev,_1ew,_)),_1eX=_1eV,_1eY=E(E(_1eX)[2]),_1eZ=E(_1eY[1]);return _1eZ[1]<=E(_1eY[8])[1]?B(_1aR(_,[0,_b6,_1eY])):B(_1aR(_,[0,_b6,[0,_1eZ,_1eY[2],_1eY[3],_1eY[4],_1eY[5],_1eY[6],_1eY[7],_1eZ,_1eY[9],_1eY[10]]]));}}}},_1f0=function(_1f1){return new F(function(){return _1cN(_1f1,_1e2);});},_1f2=new T(function(){return B(_1u(_1f0,_1dY));}),_1f3=function(_1f4){return E(E(_1f4)[2]);},_1f5=function(_1f6,_1f7,_1f8){while(1){var _1f9=E(_1f8);if(!_1f9[0]){return true;}else{if(!B(A(_1f3,[_1f6,_1f7,_1f9[1]]))){return false;}else{_1f8=_1f9[2];continue;}}}},_1fa=function(_1fb,_1fc){return new F(function(){return _1f5(_OC,_1fb,_1f2);});},_1fd=new T(function(){return B(_yr(1,2147483647));}),_1fe=function(_){var _=0,_1ff=jsMkStdout(),_1fg=_1ff;return [0,_1fg];},_1fh=new T(function(){return B(_l7(_1fe));}),_1fi=function(_){var _1fj=B(_kY(_)),_1fk=_1fj,_1fl=B(_16L(_mz,_w3,_)),_1fm=_1fl,_1fn=nMV(new T(function(){var _1fo=E(_1fm);return _1fo[0]==0?[0,_l2,_l2,_l2,_1fk,_r,_48,_80,_l2,_l3,_l3]:E(_1fo[1]);})),_1fp=_1fn,_1fq=B(unCStr("list-group")),_1fr=jsFind(toJSStr(_1fq)),_1fs=_1fr,_1ft=E(_1fs);if(!_1ft[0]){return new F(function(){return _tU(_1fq);});}else{var _1fu=B((function(_1fv,_){while(1){var _1fw=E(_1fv);if(!_1fw[0]){return _b6;}else{var _1fx=E(_1fw[1]),_1fy=E(E(_1fx[2])[3]),_1fz=B(A(_17K,[_1fx[1],_1fy[1],_1fy[2],_1fy[3],_1ft[1],_])),_1fA=_1fz;_1fv=_1fw[2];continue;}}})(_16B,_)),_1fB=_1fu,_1fC=B(unCStr("list-sp-group")),_1fD=jsFind(toJSStr(_1fC)),_1fE=_1fD,_1fF=E(_1fE);if(!_1fF[0]){return new F(function(){return _tU(_1fC);});}else{var _1fG=B((function(_1fH,_){while(1){var _1fI=E(_1fH);if(!_1fI[0]){return _b6;}else{var _1fJ=E(_1fI[1]),_1fK=E(E(_1fJ[2])[3]),_1fL=B(A(_17K,[_1fJ[1],_1fK[1],_1fK[2],_1fK[3],_1fF[1],_])),_1fM=_1fL;_1fH=_1fI[2];continue;}}})(_16C,_)),_1fN=_1fG,_1fO=[0,_1fp],_1fP=B(_15d(_1fO,_1fd,_)),_1fQ=_1fP,_1fR=B(_15d(_1fO,_Fr,_)),_1fS=_1fR,_1fT=function(_){var _1fU=B(_rh(33,_1fp,_1el,_)),_1fV=_1fU,_1fW=B(_174(_1fh,B(_re(_1fV)),_)),_1fX=_1fW,_1fY=B(_rh(1000,_1fp,_195,_)),_1fZ=_1fY,_1g0=B(_174(_1fh,B(_re(_1fZ)),_)),_1g1=_1g0,_1g2=B(_rh(60000,_1fp,_18V,_)),_1g3=_1g2;return new F(function(){return _174(_1fh,B(_re(_1g3)),_);});},_1g4=function(_1g5,_1g6,_){while(1){var _1g7=(function(_1g8,_1g9,_){var _1ga=E(_1g9);switch(_1ga[0]){case 0:_1g5=function(_){return new F(function(){return _1g4(_1g8,_1ga[4],_);});};_1g6=_1ga[3];return null;case 1:var _1gb=_1ga[1],_1gc=rMV(_1fp),_1gd=_1gc,_1ge=E(_1gd),_1gf=_1ge[7];if(!B(_rC(_1gb,_1gf))){var _=wMV(_1fp,_1ge);return new F(function(){return A(_1g8,[_]);});}else{if(B(_rv(_1gf,_1gb))[1]<=0){var _=wMV(_1fp,_1ge);return new F(function(){return A(_1g8,[_]);});}else{var _1gg=B(A(E(_1ga[2])[2],[[0,_1gb],_1ge,_])),_1gh=_1gg,_=wMV(_1fp,new T(function(){return E(E(_1gh)[2]);}));return new F(function(){return A(_1g8,[_]);});}}break;default:return new F(function(){return A(_1g8,[_]);});}})(_1g5,_1g6,_);if(_1g7!=null){return _1g7;}}},_1gi=B(_Gp(_1fa,_FJ));if(!_1gi[0]){var _1gj=_1gi[3],_1gk=_1gi[4];if(_1gi[2]>=0){var _1gl=B(_1g4(function(_){return new F(function(){return _1g4(_16s,_1gk,_);});},_1gj,_)),_1gm=_1gl;return new F(function(){return _1fT(_);});}else{var _1gn=B(_1g4(function(_){return new F(function(){return _1g4(_16s,_1gj,_);});},_1gk,_)),_1go=_1gn;return new F(function(){return _1fT(_);});}}else{var _1gp=B(_1g4(_16s,_1gi,_)),_1gq=_1gp;return new F(function(){return _1fT(_);});}}}},_1gr=function(_){return new F(function(){return _1fi(_);});};
var hasteMain = function() {B(A(_1gr, [0]));};window.onload = hasteMain;
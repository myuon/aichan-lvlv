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

var _0=new T(function(){return B(unCStr("achievements"));}),_1=new T(function(){return [0,toJSStr(E(_0))];}),_2=new T(function(){return B(unCStr("lastFocus"));}),_3=new T(function(){return [0,toJSStr(E(_2))];}),_4=new T(function(){return B(unCStr("depend"));}),_5=new T(function(){return [0,toJSStr(E(_4))];}),_6=new T(function(){return B(unCStr("lps"));}),_7=new T(function(){return [0,toJSStr(E(_6))];}),_8=new T(function(){return B(unCStr("loves"));}),_9=new T(function(){return [0,toJSStr(E(_8))];}),_a=new T(function(){return B(unCStr("lpsCoeff"));}),_b=new T(function(){return [0,toJSStr(E(_a))];}),_c=new T(function(){return B(unCStr("dependCoeff"));}),_d=new T(function(){return [0,toJSStr(E(_c))];}),_e=new T(function(){return B(unCStr("maxLoves"));}),_f=new T(function(){return [0,toJSStr(E(_e))];}),_g=new T(function(){return B(unCStr("itemsWithMap"));}),_h=new T(function(){return [0,toJSStr(E(_g))];}),_i=function(_j){return [0,toJSStr(E(_j))];},_k=function(_l){return [1,new T(function(){return B(_i(_l));})];},_m=new T(function(){return [0,"value"];}),_n=true,_o=[2,_n],_p=new T(function(){return [0,"hasValue"];}),_q=[0,_p,_o],_r=false,_s=[2,_r],_t=[0,_p,_s],_u=[0],_v=[1,_t,_u],_w=[4,_v],_x=function(_y,_z){while(1){var _A=(function(_B,_C){var _D=E(_C);if(!_D[0]){_y=[1,[3,[1,[1,new T(function(){return [0,toJSStr(_D[2])];})],[1,new T(function(){var _E=E(_D[3]);return _E[0]==0?E(_w):[4,[1,_q,[1,[0,_m,new T(function(){return B(_k(_E[1]));})],_u]]];}),_u]]],new T(function(){return B(_x(_B,_D[5]));})];_z=_D[4];return null;}else{return E(_B);}})(_y,_z);if(_A!=null){return _A;}}},_F=function(_G){return [0,new T(function(){return [0,E(_G)[1]];})];},_H=function(_I,_J){while(1){var _K=(function(_L,_M){var _N=E(_M);if(!_N[0]){_I=[1,[3,[1,[1,new T(function(){return [0,toJSStr(_N[2])];})],[1,new T(function(){return B(_F(_N[3]));}),_u]]],new T(function(){return B(_H(_L,_N[5]));})];_J=_N[4];return null;}else{return E(_L);}})(_I,_J);if(_K!=null){return _K;}}},_O=function(_P,_Q){var _R=E(_P);return _R[0]==0?E(_Q):[1,_R[1],new T(function(){return B(_O(_R[2],_Q));})];},_S=function(_T){while(1){var _U=E(_T);if(!_U[0]){_T=[1,I_fromInt(_U[1])];continue;}else{return new F(function(){return I_toString(_U[1]);});}}},_V=function(_W,_X){return new F(function(){return _O(fromJSStr(B(_S(_W))),_X);});},_Y=function(_Z,_10){var _11=E(_Z);if(!_11[0]){var _12=_11[1],_13=E(_10);return _13[0]==0?_12<_13[1]:I_compareInt(_13[1],_12)>0;}else{var _14=_11[1],_15=E(_10);return _15[0]==0?I_compareInt(_14,_15[1])<0:I_compare(_14,_15[1])<0;}},_16=[0,41],_17=[0,40],_18=[0,0],_19=function(_1a,_1b,_1c){return _1a<=6?B(_V(_1b,_1c)):!B(_Y(_1b,_18))?B(_V(_1b,_1c)):[1,_17,new T(function(){return B(_O(fromJSStr(B(_S(_1b))),[1,_16,_1c]));})];},_1d=function(_1e,_1f,_1g,_1h,_1i,_1j,_1k,_1l,_1m){return [1,[0,_9,[0,_1e]],[1,[0,_7,[0,_1f]],[1,[0,_5,[0,_1g]],[1,[0,_3,[1,new T(function(){return [0,toJSStr(B(_19(0,_1h,_u)))];})]],[1,[0,_1,[3,new T(function(){return B(_x(_u,_1i));})]],[1,[0,_h,[3,new T(function(){return B(_H(_u,_1j));})]],[1,[0,_f,[0,_1k]],[1,[0,_d,[0,_1l]],[1,[0,_b,[0,_1m]],_u]]]]]]]]];},_1n=function(_1o){var _1p=E(_1o);return [4,B(_1d(_1p[1],_1p[2],_1p[3],_1p[4],_1p[6],_1p[7],_1p[8],_1p[9],_1p[10]))];},_1q=function(_1r,_1s){var _1t=E(_1s);return _1t[0]==0?[0]:[1,new T(function(){return B(A(_1r,[_1t[1]]));}),new T(function(){return B(_1q(_1r,_1t[2]));})];},_1u=function(_1v){return [3,new T(function(){return B(_1q(_1n,_1v));})];},_1w=new T(function(){return B(unCStr("Tried to deserialize long string to a Char"));}),_1x=[0,_1w],_1y=new T(function(){return B(unCStr("Tried to deserialize a non-string to a Char"));}),_1z=[0,_1y],_1A=function(_1B){var _1C=E(_1B);if(_1C[0]==1){var _1D=fromJSStr(E(_1C[1])[1]);return _1D[0]==0?E(_1x):E(_1D[2])[0]==0?[1,_1D[1]]:E(_1x);}else{return E(_1z);}},_1E=new T(function(){return B(unCStr("Tried to deserialize a non-JSString to a JSString"));}),_1F=[0,_1E],_1G=function(_1H){return new F(function(){return fromJSStr(E(_1H)[1]);});},_1I=function(_1J){var _1K=E(_1J);return _1K[0]==1?[1,new T(function(){return B(_1G(_1K[1]));})]:E(_1F);},_1L=function(_1M){return [1,new T(function(){return [0,toJSStr([1,_1M,_u])];})];},_1N=[0,_1L,_k,_1A,_1I],_1O=function(_1P){return E(E(_1P)[2]);},_1Q=function(_1R,_1S){return [3,new T(function(){return B(_1q(new T(function(){return B(_1O(_1R));}),_1S));})];},_1T=[1,_u],_1U=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_1V=[0,_1U],_1W=function(_1X){return E(E(_1X)[4]);},_1Y=function(_1Z,_20){var _21=E(_20);if(_21[0]==3){var _22=function(_23){var _24=E(_23);if(!_24[0]){return E(_1T);}else{var _25=B(A(new T(function(){return B(_1W(_1Z));}),[_24[1]]));if(!_25[0]){return [0,_25[1]];}else{var _26=B(_22(_24[2]));return _26[0]==0?[0,_26[1]]:[1,[1,_25[1],_26[1]]];}}};return new F(function(){return _22(_21[1]);});}else{return E(_1V);}},_27=function(_28){return [0,new T(function(){return B(_1O(_28));}),function(_29){return new F(function(){return _1Q(_28,_29);});},new T(function(){return B(_1W(_28));}),function(_29){return new F(function(){return _1Y(_28,_29);});}];},_2a=new T(function(){return B(_27(_1N));}),_2b=function(_2c){return E(E(_2c)[1]);},_2d=function(_2e,_2f){var _2g=E(_2f);return _2g[0]==0?E(_w):[4,[1,_q,[1,[0,_m,new T(function(){return B(A(_2b,[_2e,_2g[1]]));})],_u]]];},_2h=function(_2i,_2j){return [3,new T(function(){return B(_1q(function(_29){return new F(function(){return _2d(_2i,_29);});},_2j));})];},_2k=function(_2l,_2m){var _2n=strEq(E(_2l)[1],E(_2m)[1]),_2o=_2n;return E(_2o)==0?true:false;},_2p=function(_2q,_2r){var _2s=strEq(E(_2q)[1],E(_2r)[1]),_2t=_2s;return E(_2t)==0?false:true;},_2u=[0,_2p,_2k],_2v=[0],_2w=[1,_2v],_2x=new T(function(){return B(unCStr("Tried to deserialize a non-Bool to a Bool"));}),_2y=[0,_2x],_2z=new T(function(){return B(unCStr("Key not found"));}),_2A=[0,_2z],_2B=new T(function(){return B(unCStr("Tried to do lookup on non-object!"));}),_2C=[0,_2B],_2D=function(_2E){return E(E(_2E)[1]);},_2F=function(_2G,_2H,_2I){while(1){var _2J=E(_2I);if(!_2J[0]){return [0];}else{var _2K=E(_2J[1]);if(!B(A(_2D,[_2G,_2H,_2K[1]]))){_2I=_2J[2];continue;}else{return [1,_2K[2]];}}}},_2L=function(_2M){return E(E(_2M)[3]);},_2N=function(_2O,_2P){var _2Q=E(_2P);if(_2Q[0]==4){var _2R=_2Q[1],_2S=B(_2F(_2u,_p,_2R));if(!_2S[0]){return E(_2A);}else{var _2T=E(_2S[1]);if(_2T[0]==2){if(!E(_2T[1])){return E(_2w);}else{var _2U=B(_2F(_2u,_m,_2R));if(!_2U[0]){return E(_2A);}else{var _2V=B(A(_2L,[_2O,_2U[1]]));return _2V[0]==0?[0,_2V[1]]:[1,[1,_2V[1]]];}}}else{return E(_2y);}}}else{return E(_2C);}},_2W=[1,_u],_2X=[0,_1U],_2Y=function(_2Z,_30){var _31=E(_30);if(_31[0]==3){var _32=function(_33){var _34=E(_33);if(!_34[0]){return E(_2W);}else{var _35=B(_2N(_2Z,_34[1]));if(!_35[0]){return [0,_35[1]];}else{var _36=B(_32(_34[2]));return _36[0]==0?[0,_36[1]]:[1,[1,_35[1],_36[1]]];}}};return new F(function(){return _32(_31[1]);});}else{return E(_2X);}},_37=function(_38){return [0,function(_29){return new F(function(){return _2d(_38,_29);});},function(_29){return new F(function(){return _2h(_38,_29);});},function(_29){return new F(function(){return _2N(_38,_29);});},function(_29){return new F(function(){return _2Y(_38,_29);});}];},_39=new T(function(){return B(_37(_2a));}),_3a=[1,_u],_3b=[0,_1U],_3c=new T(function(){return B(unCStr("Tried to deserialize a non-array into a pair!"));}),_3d=[0,_3c],_3e=function(_3f,_3g,_3h){var _3i=E(_3h);if(_3i[0]==3){var _3j=E(_3i[1]);if(!_3j[0]){return E(_3d);}else{var _3k=E(_3j[2]);if(!_3k[0]){return E(_3d);}else{if(!E(_3k[2])[0]){var _3l=B(A(_2L,[_3f,_3j[1]]));if(!_3l[0]){return [0,_3l[1]];}else{var _3m=B(A(_2L,[_3g,_3k[1]]));return _3m[0]==0?[0,_3m[1]]:[1,[0,_3l[1],_3m[1]]];}}else{return E(_3d);}}}}else{return E(_3d);}},_3n=function(_3o,_3p,_3q){var _3r=E(_3q);if(_3r[0]==3){var _3s=function(_3t){var _3u=E(_3t);if(!_3u[0]){return E(_3a);}else{var _3v=B(_3e(_3o,_3p,_3u[1]));if(!_3v[0]){return [0,_3v[1]];}else{var _3w=B(_3s(_3u[2]));return _3w[0]==0?[0,_3w[1]]:[1,[1,_3v[1],_3w[1]]];}}};return new F(function(){return _3s(_3r[1]);});}else{return E(_3b);}},_3x=function(_3y){return [3,new T(function(){return B(_1q(_F,_3y));})];},_3z=new T(function(){return B(unCStr("The given Number can\'t be represented as an Int"));}),_3A=[0,_3z],_3B=new T(function(){return B(unCStr("Tried to deserialize a non-Number to an Int"));}),_3C=[0,_3B],_3D=function(_3E){var _3F=E(_3E);if(!_3F[0]){var _3G=E(_3F[1])[1],_3H=_3G&4294967295;return _3H!=_3G?E(_3A):[1,[0,_3H]];}else{return E(_3C);}},_3I=[0,_1U],_3J=[1,_u],_3K=[0,_3z],_3L=[0,_3B],_3M=function(_3N){var _3O=E(_3N);if(!_3O[0]){return E(_3J);}else{var _3P=E(_3O[1]);if(!_3P[0]){var _3Q=E(_3P[1])[1],_3R=_3Q&4294967295;if(_3R!=_3Q){return E(_3K);}else{var _3S=B(_3M(_3O[2]));return _3S[0]==0?[0,_3S[1]]:[1,[1,[0,_3R],_3S[1]]];}}else{return E(_3L);}}},_3T=function(_3U){var _3V=E(_3U);return _3V[0]==3?B(_3M(_3V[1])):E(_3I);},_3W=[0,_F,_3x,_3D,_3T],_3X=function(_3Y,_3Z){while(1){var _40=E(_3Y);if(!_40[0]){return E(_3Z)[0]==0?1:0;}else{var _41=E(_3Z);if(!_41[0]){return 2;}else{var _42=E(_40[1])[1],_43=E(_41[1])[1];if(_42!=_43){return _42>_43?2:0;}else{_3Y=_40[2];_3Z=_41[2];continue;}}}}},_44=[1],_45=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_46=function(_47){return new F(function(){return err(_45);});},_48=new T(function(){return B(_46(_));}),_49=function(_4a,_4b,_4c,_4d){var _4e=E(_4c);if(!_4e[0]){var _4f=_4e[1],_4g=E(_4d);if(!_4g[0]){var _4h=_4g[1],_4i=_4g[2],_4j=_4g[3];if(_4h<=(imul(3,_4f)|0)){return [0,(1+_4f|0)+_4h|0,E(E(_4a)),_4b,E(_4e),E(_4g)];}else{var _4k=E(_4g[4]);if(!_4k[0]){var _4l=_4k[1],_4m=_4k[2],_4n=_4k[3],_4o=_4k[4],_4p=E(_4g[5]);if(!_4p[0]){var _4q=_4p[1];if(_4l>=(imul(2,_4q)|0)){var _4r=function(_4s){var _4t=E(_4a),_4u=E(_4k[5]);return _4u[0]==0?[0,(1+_4f|0)+_4h|0,E(_4m),_4n,E([0,(1+_4f|0)+_4s|0,E(_4t),_4b,E(_4e),E(_4o)]),E([0,(1+_4q|0)+_4u[1]|0,E(_4i),_4j,E(_4u),E(_4p)])]:[0,(1+_4f|0)+_4h|0,E(_4m),_4n,E([0,(1+_4f|0)+_4s|0,E(_4t),_4b,E(_4e),E(_4o)]),E([0,1+_4q|0,E(_4i),_4j,E(_44),E(_4p)])];},_4v=E(_4o);return _4v[0]==0?B(_4r(_4v[1])):B(_4r(0));}else{return [0,(1+_4f|0)+_4h|0,E(_4i),_4j,E([0,(1+_4f|0)+_4l|0,E(E(_4a)),_4b,E(_4e),E(_4k)]),E(_4p)];}}else{return E(_48);}}else{return E(_48);}}}else{return [0,1+_4f|0,E(E(_4a)),_4b,E(_4e),E(_44)];}}else{var _4w=E(_4d);if(!_4w[0]){var _4x=_4w[1],_4y=_4w[2],_4z=_4w[3],_4A=_4w[5],_4B=E(_4w[4]);if(!_4B[0]){var _4C=_4B[1],_4D=_4B[2],_4E=_4B[3],_4F=_4B[4],_4G=E(_4A);if(!_4G[0]){var _4H=_4G[1];if(_4C>=(imul(2,_4H)|0)){var _4I=function(_4J){var _4K=E(_4a),_4L=E(_4B[5]);return _4L[0]==0?[0,1+_4x|0,E(_4D),_4E,E([0,1+_4J|0,E(_4K),_4b,E(_44),E(_4F)]),E([0,(1+_4H|0)+_4L[1]|0,E(_4y),_4z,E(_4L),E(_4G)])]:[0,1+_4x|0,E(_4D),_4E,E([0,1+_4J|0,E(_4K),_4b,E(_44),E(_4F)]),E([0,1+_4H|0,E(_4y),_4z,E(_44),E(_4G)])];},_4M=E(_4F);return _4M[0]==0?B(_4I(_4M[1])):B(_4I(0));}else{return [0,1+_4x|0,E(_4y),_4z,E([0,1+_4C|0,E(E(_4a)),_4b,E(_44),E(_4B)]),E(_4G)];}}else{return [0,3,E(_4D),_4E,E([0,1,E(E(_4a)),_4b,E(_44),E(_44)]),E([0,1,E(_4y),_4z,E(_44),E(_44)])];}}else{var _4N=E(_4A);return _4N[0]==0?[0,3,E(_4y),_4z,E([0,1,E(E(_4a)),_4b,E(_44),E(_44)]),E(_4N)]:[0,2,E(E(_4a)),_4b,E(_44),E(_4w)];}}else{return [0,1,E(E(_4a)),_4b,E(_44),E(_44)];}}},_4O=function(_4P,_4Q){return [0,1,E(E(_4P)),_4Q,E(_44),E(_44)];},_4R=function(_4S,_4T,_4U){var _4V=E(_4U);if(!_4V[0]){return new F(function(){return _49(_4V[2],_4V[3],_4V[4],B(_4R(_4S,_4T,_4V[5])));});}else{return new F(function(){return _4O(_4S,_4T);});}},_4W=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_4X=function(_4Y){return new F(function(){return err(_4W);});},_4Z=new T(function(){return B(_4X(_));}),_50=function(_51,_52,_53,_54){var _55=E(_54);if(!_55[0]){var _56=_55[1],_57=E(_53);if(!_57[0]){var _58=_57[1],_59=_57[2],_5a=_57[3];if(_58<=(imul(3,_56)|0)){return [0,(1+_58|0)+_56|0,E(E(_51)),_52,E(_57),E(_55)];}else{var _5b=E(_57[4]);if(!_5b[0]){var _5c=_5b[1],_5d=E(_57[5]);if(!_5d[0]){var _5e=_5d[1],_5f=_5d[2],_5g=_5d[3],_5h=_5d[4];if(_5e>=(imul(2,_5c)|0)){var _5i=function(_5j){var _5k=E(_5d[5]);return _5k[0]==0?[0,(1+_58|0)+_56|0,E(_5f),_5g,E([0,(1+_5c|0)+_5j|0,E(_59),_5a,E(_5b),E(_5h)]),E([0,(1+_56|0)+_5k[1]|0,E(E(_51)),_52,E(_5k),E(_55)])]:[0,(1+_58|0)+_56|0,E(_5f),_5g,E([0,(1+_5c|0)+_5j|0,E(_59),_5a,E(_5b),E(_5h)]),E([0,1+_56|0,E(E(_51)),_52,E(_44),E(_55)])];},_5l=E(_5h);return _5l[0]==0?B(_5i(_5l[1])):B(_5i(0));}else{return [0,(1+_58|0)+_56|0,E(_59),_5a,E(_5b),E([0,(1+_56|0)+_5e|0,E(E(_51)),_52,E(_5d),E(_55)])];}}else{return E(_4Z);}}else{return E(_4Z);}}}else{return [0,1+_56|0,E(E(_51)),_52,E(_44),E(_55)];}}else{var _5m=E(_53);if(!_5m[0]){var _5n=_5m[1],_5o=_5m[2],_5p=_5m[3],_5q=_5m[5],_5r=E(_5m[4]);if(!_5r[0]){var _5s=_5r[1],_5t=E(_5q);if(!_5t[0]){var _5u=_5t[1],_5v=_5t[2],_5w=_5t[3],_5x=_5t[4];if(_5u>=(imul(2,_5s)|0)){var _5y=function(_5z){var _5A=E(_5t[5]);return _5A[0]==0?[0,1+_5n|0,E(_5v),_5w,E([0,(1+_5s|0)+_5z|0,E(_5o),_5p,E(_5r),E(_5x)]),E([0,1+_5A[1]|0,E(E(_51)),_52,E(_5A),E(_44)])]:[0,1+_5n|0,E(_5v),_5w,E([0,(1+_5s|0)+_5z|0,E(_5o),_5p,E(_5r),E(_5x)]),E([0,1,E(E(_51)),_52,E(_44),E(_44)])];},_5B=E(_5x);return _5B[0]==0?B(_5y(_5B[1])):B(_5y(0));}else{return [0,1+_5n|0,E(_5o),_5p,E(_5r),E([0,1+_5u|0,E(E(_51)),_52,E(_5t),E(_44)])];}}else{return [0,3,E(_5o),_5p,E(_5r),E([0,1,E(E(_51)),_52,E(_44),E(_44)])];}}else{var _5C=E(_5q);return _5C[0]==0?[0,3,E(_5C[2]),_5C[3],E([0,1,E(_5o),_5p,E(_44),E(_44)]),E([0,1,E(E(_51)),_52,E(_44),E(_44)])]:[0,2,E(E(_51)),_52,E(_5m),E(_44)];}}else{return [0,1,E(E(_51)),_52,E(_44),E(_44)];}}},_5D=function(_5E,_5F,_5G){var _5H=E(_5G);if(!_5H[0]){return new F(function(){return _50(_5H[2],_5H[3],B(_5D(_5E,_5F,_5H[4])),_5H[5]);});}else{return new F(function(){return _4O(_5E,_5F);});}},_5I=function(_5J,_5K,_5L,_5M,_5N,_5O,_5P){return new F(function(){return _50(_5M,_5N,B(_5D(_5J,_5K,_5O)),_5P);});},_5Q=function(_5R,_5S,_5T,_5U,_5V,_5W,_5X,_5Y){var _5Z=E(_5T);if(!_5Z[0]){var _60=_5Z[1],_61=_5Z[2],_62=_5Z[3],_63=_5Z[4],_64=_5Z[5];if((imul(3,_60)|0)>=_5U){if((imul(3,_5U)|0)>=_60){return [0,(_60+_5U|0)+1|0,E(E(_5R)),_5S,E(_5Z),E([0,_5U,E(_5V),_5W,E(_5X),E(_5Y)])];}else{return new F(function(){return _49(_61,_62,_63,B(_5Q(_5R,_5S,_64,_5U,_5V,_5W,_5X,_5Y)));});}}else{return new F(function(){return _50(_5V,_5W,B(_65(_5R,_5S,_60,_61,_62,_63,_64,_5X)),_5Y);});}}else{return new F(function(){return _5I(_5R,_5S,_5U,_5V,_5W,_5X,_5Y);});}},_65=function(_66,_67,_68,_69,_6a,_6b,_6c,_6d){var _6e=E(_6d);if(!_6e[0]){var _6f=_6e[1],_6g=_6e[2],_6h=_6e[3],_6i=_6e[4],_6j=_6e[5];if((imul(3,_68)|0)>=_6f){if((imul(3,_6f)|0)>=_68){return [0,(_68+_6f|0)+1|0,E(E(_66)),_67,E([0,_68,E(_69),_6a,E(_6b),E(_6c)]),E(_6e)];}else{return new F(function(){return _49(_69,_6a,_6b,B(_5Q(_66,_67,_6c,_6f,_6g,_6h,_6i,_6j)));});}}else{return new F(function(){return _50(_6g,_6h,B(_65(_66,_67,_68,_69,_6a,_6b,_6c,_6i)),_6j);});}}else{return new F(function(){return _4R(_66,_67,[0,_68,E(_69),_6a,E(_6b),E(_6c)]);});}},_6k=function(_6l,_6m,_6n,_6o){var _6p=E(_6n);if(!_6p[0]){var _6q=_6p[1],_6r=_6p[2],_6s=_6p[3],_6t=_6p[4],_6u=_6p[5],_6v=E(_6o);if(!_6v[0]){var _6w=_6v[1],_6x=_6v[2],_6y=_6v[3],_6z=_6v[4],_6A=_6v[5];if((imul(3,_6q)|0)>=_6w){if((imul(3,_6w)|0)>=_6q){return [0,(_6q+_6w|0)+1|0,E(E(_6l)),_6m,E(_6p),E(_6v)];}else{return new F(function(){return _49(_6r,_6s,_6t,B(_5Q(_6l,_6m,_6u,_6w,_6x,_6y,_6z,_6A)));});}}else{return new F(function(){return _50(_6x,_6y,B(_65(_6l,_6m,_6q,_6r,_6s,_6t,_6u,_6z)),_6A);});}}else{return new F(function(){return _4R(_6l,_6m,_6p);});}}else{return new F(function(){return _5D(_6l,_6m,_6o);});}},_6B=function(_6C,_6D,_6E,_6F){var _6G=E(_6C);if(_6G==1){var _6H=E(_6F);return _6H[0]==0?[0,new T(function(){return [0,1,E(E(_6D)),_6E,E(_44),E(_44)];}),_u,_u]:B(_3X(_6D,E(_6H[1])[1]))==0?[0,new T(function(){return [0,1,E(E(_6D)),_6E,E(_44),E(_44)];}),_6H,_u]:[0,new T(function(){return [0,1,E(E(_6D)),_6E,E(_44),E(_44)];}),_u,_6H];}else{var _6I=B(_6B(_6G>>1,_6D,_6E,_6F)),_6J=_6I[1],_6K=_6I[3],_6L=E(_6I[2]);if(!_6L[0]){return [0,_6J,_u,_6K];}else{var _6M=E(_6L[1]),_6N=_6M[1],_6O=_6M[2],_6P=E(_6L[2]);if(!_6P[0]){return [0,new T(function(){return B(_4R(_6N,_6O,_6J));}),_u,_6K];}else{var _6Q=E(_6P[1]),_6R=_6Q[1];if(!B(_3X(_6N,_6R))){var _6S=B(_6B(_6G>>1,_6R,_6Q[2],_6P[2]));return [0,new T(function(){return B(_6k(_6N,_6O,_6J,_6S[1]));}),_6S[2],_6S[3]];}else{return [0,_6J,_u,_6L];}}}}},_6T=function(_6U,_6V,_6W){var _6X=E(_6U),_6Y=E(_6W);if(!_6Y[0]){var _6Z=_6Y[2],_70=_6Y[3],_71=_6Y[4],_72=_6Y[5];switch(B(_3X(_6X,_6Z))){case 0:return new F(function(){return _50(_6Z,_70,B(_6T(_6X,_6V,_71)),_72);});break;case 1:return [0,_6Y[1],E(_6X),_6V,E(_71),E(_72)];default:return new F(function(){return _49(_6Z,_70,_71,B(_6T(_6X,_6V,_72)));});}}else{return [0,1,E(_6X),_6V,E(_44),E(_44)];}},_73=function(_74,_75){while(1){var _76=E(_75);if(!_76[0]){return E(_74);}else{var _77=E(_76[1]),_78=B(_6T(_77[1],_77[2],_74));_75=_76[2];_74=_78;continue;}}},_79=function(_7a,_7b,_7c,_7d){return new F(function(){return _73(B(_6T(_7b,_7c,_7a)),_7d);});},_7e=function(_7f,_7g,_7h){var _7i=E(_7g);return new F(function(){return _73(B(_6T(_7i[1],_7i[2],_7f)),_7h);});},_7j=function(_7k,_7l,_7m){while(1){var _7n=E(_7m);if(!_7n[0]){return E(_7l);}else{var _7o=E(_7n[1]),_7p=_7o[1],_7q=_7o[2],_7r=E(_7n[2]);if(!_7r[0]){return new F(function(){return _4R(_7p,_7q,_7l);});}else{var _7s=E(_7r[1]),_7t=_7s[1];if(!B(_3X(_7p,_7t))){var _7u=B(_6B(_7k,_7t,_7s[2],_7r[2])),_7v=_7u[1],_7w=E(_7u[3]);if(!_7w[0]){var _7x=_7k<<1,_7y=B(_6k(_7p,_7q,_7l,_7v));_7m=_7u[2];_7k=_7x;_7l=_7y;continue;}else{return new F(function(){return _7e(B(_6k(_7p,_7q,_7l,_7v)),_7w[1],_7w[2]);});}}else{return new F(function(){return _79(_7l,_7p,_7q,_7r);});}}}}},_7z=function(_7A,_7B,_7C,_7D,_7E){var _7F=E(_7E);if(!_7F[0]){return new F(function(){return _4R(_7C,_7D,_7B);});}else{var _7G=E(_7F[1]),_7H=_7G[1];if(!B(_3X(_7C,_7H))){var _7I=B(_6B(_7A,_7H,_7G[2],_7F[2])),_7J=_7I[1],_7K=E(_7I[3]);if(!_7K[0]){return new F(function(){return _7j(_7A<<1,B(_6k(_7C,_7D,_7B,_7J)),_7I[2]);});}else{return new F(function(){return _7e(B(_6k(_7C,_7D,_7B,_7J)),_7K[1],_7K[2]);});}}else{return new F(function(){return _79(_7B,_7C,_7D,_7F);});}}},_7L=function(_7M){var _7N=E(_7M);if(!_7N[0]){return [1];}else{var _7O=E(_7N[1]),_7P=_7O[1],_7Q=_7O[2],_7R=E(_7N[2]);if(!_7R[0]){return [0,1,E(E(_7P)),_7Q,E(_44),E(_44)];}else{var _7S=_7R[2],_7T=E(_7R[1]),_7U=_7T[1],_7V=_7T[2];if(!B(_3X(_7P,_7U))){return new F(function(){return _7z(1,[0,1,E(E(_7P)),_7Q,E(_44),E(_44)],_7U,_7V,_7S);});}else{return new F(function(){return _79([0,1,E(E(_7P)),_7Q,E(_44),E(_44)],_7U,_7V,_7S);});}}}},_7W=function(_7X,_7Y){var _7Z=jsShowI(_7X),_80=_7Z;return new F(function(){return _O(fromJSStr(_80),_7Y);});},_81=function(_82,_83,_84){if(_83>=0){return new F(function(){return _7W(_83,_84);});}else{return _82<=6?B(_7W(_83,_84)):[1,_17,new T(function(){var _85=jsShowI(_83),_86=_85;return B(_O(fromJSStr(_86),[1,_16,_84]));})];}},_87=new T(function(){return B(unCStr(" is not an element of the map"));}),_88=function(_89){return new F(function(){return err(B(unAppCStr("IntMap.!: key ",new T(function(){return B(_O(B(_81(0,_89,_u)),_87));}))));});},_8a=function(_8b,_8c){var _8d=new T(function(){return B(_88(_8c));});return new F(function(){return (function(_8e){while(1){var _8f=E(_8e);switch(_8f[0]){case 0:var _8g=_8f[2]>>>0;if(((_8c>>>0&((_8g-1>>>0^4294967295)>>>0^_8g)>>>0)>>>0&4294967295)==_8f[1]){if(!((_8c>>>0&_8g)>>>0)){_8e=_8f[3];continue;}else{_8e=_8f[4];continue;}}else{return E(_8d);}break;case 1:return _8c!=_8f[1]?E(_8d):E(_8f[2]);default:return E(_8d);}}})(_8b);});},_8h=function(_8i,_8j){return new F(function(){return _8a(_8i,E(_8j)[1]);});},_8k=[2],_8l=function(_8m,_8n,_8o){var _8p=E(_8o);switch(_8p[0]){case 0:var _8q=_8p[1],_8r=_8p[2],_8s=_8p[3],_8t=_8p[4],_8u=_8r>>>0;if(((_8m>>>0&((_8u-1>>>0^4294967295)>>>0^_8u)>>>0)>>>0&4294967295)==_8q){return (_8m>>>0&_8u)>>>0==0?[0,_8q,_8r,E(B(_8l(_8m,_8n,_8s))),E(_8t)]:[0,_8q,_8r,E(_8s),E(B(_8l(_8m,_8n,_8t)))];}else{var _8v=(_8m>>>0^_8q>>>0)>>>0,_8w=(_8v|_8v>>>1)>>>0,_8x=(_8w|_8w>>>2)>>>0,_8y=(_8x|_8x>>>4)>>>0,_8z=(_8y|_8y>>>8)>>>0,_8A=(_8z|_8z>>>16)>>>0,_8B=(_8A^_8A>>>1)>>>0&4294967295,_8C=_8B>>>0;return (_8m>>>0&_8C)>>>0==0?[0,(_8m>>>0&((_8C-1>>>0^4294967295)>>>0^_8C)>>>0)>>>0&4294967295,_8B,E([1,_8m,_8n]),E(_8p)]:[0,(_8m>>>0&((_8C-1>>>0^4294967295)>>>0^_8C)>>>0)>>>0&4294967295,_8B,E(_8p),E([1,_8m,_8n])];}break;case 1:var _8D=_8p[1];if(_8m!=_8D){var _8E=(_8m>>>0^_8D>>>0)>>>0,_8F=(_8E|_8E>>>1)>>>0,_8G=(_8F|_8F>>>2)>>>0,_8H=(_8G|_8G>>>4)>>>0,_8I=(_8H|_8H>>>8)>>>0,_8J=(_8I|_8I>>>16)>>>0,_8K=(_8J^_8J>>>1)>>>0&4294967295,_8L=_8K>>>0;return (_8m>>>0&_8L)>>>0==0?[0,(_8m>>>0&((_8L-1>>>0^4294967295)>>>0^_8L)>>>0)>>>0&4294967295,_8K,E([1,_8m,_8n]),E(_8p)]:[0,(_8m>>>0&((_8L-1>>>0^4294967295)>>>0^_8L)>>>0)>>>0&4294967295,_8K,E(_8p),E([1,_8m,_8n])];}else{return [1,_8m,_8n];}break;default:return [1,_8m,_8n];}},_8M=function(_8N,_8O){while(1){var _8P=E(_8O);if(!_8P[0]){return E(_8N);}else{var _8Q=E(_8P[1]),_8R=B(_8l(E(_8Q[1])[1],_8Q[2],_8N));_8O=_8P[2];_8N=_8R;continue;}}},_8S=new T(function(){return B(unCStr("lefteye"));}),_8T=[0,9],_8U=[0,_8T,_8S],_8V=new T(function(){return B(unCStr("monitor"));}),_8W=[0,-1],_8X=[0,_8W,_8V],_8Y=new T(function(){return B(unCStr("itemshop"));}),_8Z=[0,-2],_90=[0,_8Z,_8Y],_91=[0,-3],_92=new T(function(){return B(unCStr("righteye"));}),_93=[0,_91,_92],_94=[0,-4],_95=[0,_94,_8S],_96=new T(function(){return B(unCStr("restart"));}),_97=[0,-8],_98=[0,_97,_96],_99=[1,_98,_u],_9a=new T(function(){return B(unCStr("reset"));}),_9b=[0,-7],_9c=[0,_9b,_9a],_9d=[1,_9c,_99],_9e=[0,-6],_9f=[0,_9e,_8S],_9g=[1,_9f,_9d],_9h=[0,-5],_9i=[0,_9h,_92],_9j=[1,_9i,_9g],_9k=[1,_95,_9j],_9l=[1,_93,_9k],_9m=[1,_90,_9l],_9n=[1,_8X,_9m],_9o=[1,_8U,_9n],_9p=[0,8],_9q=[0,_9p,_92],_9r=[1,_9q,_9o],_9s=new T(function(){return B(unCStr("house"));}),_9t=[0,7],_9u=[0,_9t,_9s],_9v=[1,_9u,_9r],_9w=new T(function(){return B(unCStr("car"));}),_9x=[0,6],_9y=[0,_9x,_9w],_9z=[1,_9y,_9v],_9A=new T(function(){return B(unCStr("trip"));}),_9B=[0,5],_9C=[0,_9B,_9A],_9D=[1,_9C,_9z],_9E=new T(function(){return B(unCStr("gift"));}),_9F=[0,4],_9G=[0,_9F,_9E],_9H=[1,_9G,_9D],_9I=new T(function(){return B(unCStr("coffee"));}),_9J=[0,3],_9K=[0,_9J,_9I],_9L=[1,_9K,_9H],_9M=new T(function(){return B(unCStr("mail"));}),_9N=[0,2],_9O=[0,_9N,_9M],_9P=[1,_9O,_9L],_9Q=new T(function(){return B(unCStr("chat"));}),_9R=[0,1],_9S=[0,_9R,_9Q],_9T=[1,_9S,_9P],_9U=new T(function(){return B(_8M(_8k,_9T));}),_9V=function(_9W){var _9X=E(_9W);return [0,new T(function(){return B(_8h(_9U,_9X[1]));}),_9X[2]];},_9Y=new T(function(){return [0,"items"];}),_9Z=new T(function(){return [0,"lpsCoeff"];}),_a0=new T(function(){return [0,"dependCoeff"];}),_a1=new T(function(){return [0,"maxLoves"];}),_a2=new T(function(){return [0,"itemsWithMap"];}),_a3=new T(function(){return [0,"achievements"];}),_a4=new T(function(){return [0,"lastFocus"];}),_a5=new T(function(){return [0,"depend"];}),_a6=new T(function(){return [0,"lps"];}),_a7=new T(function(){return [0,"loves"];}),_a8=new T(function(){return B(unCStr("Control.Exception.Base"));}),_a9=new T(function(){return B(unCStr("base"));}),_aa=new T(function(){return B(unCStr("PatternMatchFail"));}),_ab=new T(function(){var _ac=hs_wordToWord64(18445595),_ad=_ac,_ae=hs_wordToWord64(52003073),_af=_ae;return [0,_ad,_af,[0,_ad,_af,_a9,_a8,_aa],_u];}),_ag=function(_ah){return E(_ab);},_ai=function(_aj){return E(E(_aj)[1]);},_ak=function(_al,_am,_an){var _ao=B(A(_al,[_])),_ap=B(A(_am,[_])),_aq=hs_eqWord64(_ao[1],_ap[1]),_ar=_aq;if(!E(_ar)){return [0];}else{var _as=hs_eqWord64(_ao[2],_ap[2]),_at=_as;return E(_at)==0?[0]:[1,_an];}},_au=function(_av){var _aw=E(_av);return new F(function(){return _ak(B(_ai(_aw[1])),_ag,_aw[2]);});},_ax=function(_ay){return E(E(_ay)[1]);},_az=function(_aA,_aB){return new F(function(){return _O(E(_aA)[1],_aB);});},_aC=[0,44],_aD=[0,93],_aE=[0,91],_aF=function(_aG,_aH,_aI){var _aJ=E(_aH);return _aJ[0]==0?B(unAppCStr("[]",_aI)):[1,_aE,new T(function(){return B(A(_aG,[_aJ[1],new T(function(){var _aK=function(_aL){var _aM=E(_aL);return _aM[0]==0?E([1,_aD,_aI]):[1,_aC,new T(function(){return B(A(_aG,[_aM[1],new T(function(){return B(_aK(_aM[2]));})]));})];};return B(_aK(_aJ[2]));})]));})];},_aN=function(_aO,_aP){return new F(function(){return _aF(_az,_aO,_aP);});},_aQ=function(_aR,_aS,_aT){return new F(function(){return _O(E(_aS)[1],_aT);});},_aU=[0,_aQ,_ax,_aN],_aV=new T(function(){return [0,_ag,_aU,_aW,_au];}),_aW=function(_aX){return [0,_aV,_aX];},_aY=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_aZ=function(_b0,_b1){return new F(function(){return die(new T(function(){return B(A(_b1,[_b0]));}));});},_b2=function(_b3,_b4){var _b5=E(_b4);if(!_b5[0]){return [0,_u,_u];}else{var _b6=_b5[1];if(!B(A(_b3,[_b6]))){return [0,_u,_b5];}else{var _b7=new T(function(){var _b8=B(_b2(_b3,_b5[2]));return [0,_b8[1],_b8[2]];});return [0,[1,_b6,new T(function(){return E(E(_b7)[1]);})],new T(function(){return E(E(_b7)[2]);})];}}},_b9=[0,32],_ba=[0,10],_bb=[1,_ba,_u],_bc=function(_bd){return E(E(_bd)[1])==124?false:true;},_be=function(_bf,_bg){var _bh=B(_b2(_bc,B(unCStr(_bf)))),_bi=_bh[1],_bj=function(_bk,_bl){return new F(function(){return _O(_bk,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_O(_bg,new T(function(){return B(_O(_bl,_bb));})));})));}));});},_bm=E(_bh[2]);if(!_bm[0]){return new F(function(){return _bj(_bi,_u);});}else{return E(E(_bm[1])[1])==124?B(_bj(_bi,[1,_b9,_bm[2]])):B(_bj(_bi,_u));}},_bn=function(_bo){return new F(function(){return _aZ([0,new T(function(){return B(_be(_bo,_aY));})],_aW);});},_bp=new T(function(){return B(_bn("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_bq=function(_br,_bs){while(1){var _bt=(function(_bu,_bv){var _bw=E(_bu);switch(_bw[0]){case 0:var _bx=E(_bv);if(!_bx[0]){return [0];}else{_br=B(A(_bw[1],[_bx[1]]));_bs=_bx[2];return null;}break;case 1:var _by=B(A(_bw[1],[_bv])),_bz=_bv;_br=_by;_bs=_bz;return null;case 2:return [0];case 3:return [1,[0,_bw[1],_bv],new T(function(){return B(_bq(_bw[2],_bv));})];default:return E(_bw[1]);}})(_br,_bs);if(_bt!=null){return _bt;}}},_bA=function(_bB,_bC){var _bD=function(_bE){var _bF=E(_bC);if(_bF[0]==3){return [3,_bF[1],new T(function(){return B(_bA(_bB,_bF[2]));})];}else{var _bG=E(_bB);if(_bG[0]==2){return E(_bF);}else{var _bH=E(_bF);if(_bH[0]==2){return E(_bG);}else{var _bI=function(_bJ){var _bK=E(_bH);if(_bK[0]==4){return [1,function(_bL){return [4,new T(function(){return B(_O(B(_bq(_bG,_bL)),_bK[1]));})];}];}else{var _bM=E(_bG);if(_bM[0]==1){var _bN=_bM[1],_bO=E(_bK);return _bO[0]==0?[1,function(_bP){return new F(function(){return _bA(B(A(_bN,[_bP])),_bO);});}]:[1,function(_bQ){return new F(function(){return _bA(B(A(_bN,[_bQ])),new T(function(){return B(A(_bO[1],[_bQ]));}));});}];}else{var _bR=E(_bK);return _bR[0]==0?E(_bp):[1,function(_bS){return new F(function(){return _bA(_bM,new T(function(){return B(A(_bR[1],[_bS]));}));});}];}}},_bT=E(_bG);switch(_bT[0]){case 1:var _bU=E(_bH);if(_bU[0]==4){return [1,function(_bV){return [4,new T(function(){return B(_O(B(_bq(B(A(_bT[1],[_bV])),_bV)),_bU[1]));})];}];}else{return new F(function(){return _bI(_);});}break;case 4:var _bW=_bT[1],_bX=E(_bH);switch(_bX[0]){case 0:return [1,function(_bY){return [4,new T(function(){return B(_O(_bW,new T(function(){return B(_bq(_bX,_bY));})));})];}];case 1:return [1,function(_bZ){return [4,new T(function(){return B(_O(_bW,new T(function(){return B(_bq(B(A(_bX[1],[_bZ])),_bZ));})));})];}];default:return [4,new T(function(){return B(_O(_bW,_bX[1]));})];}break;default:return new F(function(){return _bI(_);});}}}}},_c0=E(_bB);switch(_c0[0]){case 0:var _c1=E(_bC);if(!_c1[0]){return [0,function(_c2){return new F(function(){return _bA(B(A(_c0[1],[_c2])),new T(function(){return B(A(_c1[1],[_c2]));}));});}];}else{return new F(function(){return _bD(_);});}break;case 3:return [3,_c0[1],new T(function(){return B(_bA(_c0[2],_bC));})];default:return new F(function(){return _bD(_);});}},_c3=[0,41],_c4=[1,_c3,_u],_c5=[0,40],_c6=[1,_c5,_u],_c7=function(_c8,_c9){while(1){var _ca=E(_c8);if(!_ca[0]){return E(_c9)[0]==0?true:false;}else{var _cb=E(_c9);if(!_cb[0]){return false;}else{if(E(_ca[1])[1]!=E(_cb[1])[1]){return false;}else{_c8=_ca[2];_c9=_cb[2];continue;}}}}},_cc=function(_cd,_ce){return E(_cd)[1]!=E(_ce)[1];},_cf=function(_cg,_ch){return E(_cg)[1]==E(_ch)[1];},_ci=[0,_cf,_cc],_cj=function(_ck,_cl){while(1){var _cm=E(_ck);if(!_cm[0]){return E(_cl)[0]==0?true:false;}else{var _cn=E(_cl);if(!_cn[0]){return false;}else{if(E(_cm[1])[1]!=E(_cn[1])[1]){return false;}else{_ck=_cm[2];_cl=_cn[2];continue;}}}}},_co=function(_cp,_cq){return !B(_cj(_cp,_cq))?true:false;},_cr=[0,_cj,_co],_cs=function(_ct,_cu){var _cv=E(_ct);switch(_cv[0]){case 0:return [0,function(_cw){return new F(function(){return _cs(B(A(_cv[1],[_cw])),_cu);});}];case 1:return [1,function(_cx){return new F(function(){return _cs(B(A(_cv[1],[_cx])),_cu);});}];case 2:return [2];case 3:return new F(function(){return _bA(B(A(_cu,[_cv[1]])),new T(function(){return B(_cs(_cv[2],_cu));}));});break;default:var _cy=function(_cz){var _cA=E(_cz);if(!_cA[0]){return [0];}else{var _cB=E(_cA[1]);return new F(function(){return _O(B(_bq(B(A(_cu,[_cB[1]])),_cB[2])),new T(function(){return B(_cy(_cA[2]));}));});}},_cC=B(_cy(_cv[1]));return _cC[0]==0?[2]:[4,_cC];}},_cD=[2],_cE=function(_cF){return [3,_cF,_cD];},_cG=0,_cH=function(_cI,_cJ){var _cK=E(_cI);if(!_cK){return new F(function(){return A(_cJ,[_cG]);});}else{return [0,function(_cL){return E(new T(function(){return B(_cH(_cK-1|0,_cJ));}));}];}},_cM=function(_cN,_cO,_cP){return function(_cQ){return new F(function(){return A(function(_cR,_cS,_cT){while(1){var _cU=(function(_cV,_cW,_cX){var _cY=E(_cV);switch(_cY[0]){case 0:var _cZ=E(_cW);if(!_cZ[0]){return E(_cO);}else{_cR=B(A(_cY[1],[_cZ[1]]));_cS=_cZ[2];var _d0=_cX+1|0;_cT=_d0;return null;}break;case 1:var _d1=B(A(_cY[1],[_cW])),_d2=_cW,_d0=_cX;_cR=_d1;_cS=_d2;_cT=_d0;return null;case 2:return E(_cO);case 3:return function(_d3){return new F(function(){return _cH(_cX,function(_d4){return E(new T(function(){return B(_cs(_cY,_d3));}));});});};default:return function(_d5){return new F(function(){return _cs(_cY,_d5);});};}})(_cR,_cS,_cT);if(_cU!=null){return _cU;}}},[new T(function(){return B(A(_cN,[_cE]));}),_cQ,0,_cP]);});};},_d6=function(_d7){return new F(function(){return A(_d7,[_u]);});},_d8=function(_d9,_da){var _db=function(_dc){var _dd=E(_dc);if(!_dd[0]){return E(_d6);}else{var _de=_dd[1];return !B(A(_d9,[_de]))?E(_d6):function(_df){return [0,function(_dg){return E(new T(function(){return B(A(new T(function(){return B(_db(_dd[2]));}),[function(_dh){return new F(function(){return A(_df,[[1,_de,_dh]]);});}]));}));}];};}};return function(_di){return new F(function(){return A(_db,[_di,_da]);});};},_dj=[6],_dk=function(_dl){return E(_dl);},_dm=new T(function(){return B(unCStr("valDig: Bad base"));}),_dn=new T(function(){return B(err(_dm));}),_do=function(_dp,_dq){var _dr=function(_ds,_dt){var _du=E(_ds);if(!_du[0]){return function(_dv){return new F(function(){return A(_dv,[new T(function(){return B(A(_dt,[_u]));})]);});};}else{var _dw=E(_du[1])[1],_dx=function(_dy){return function(_dz){return [0,function(_dA){return E(new T(function(){return B(A(new T(function(){return B(_dr(_du[2],function(_dB){return new F(function(){return A(_dt,[[1,_dy,_dB]]);});}));}),[_dz]));}));}];};};switch(E(E(_dp)[1])){case 8:if(48>_dw){return function(_dC){return new F(function(){return A(_dC,[new T(function(){return B(A(_dt,[_u]));})]);});};}else{if(_dw>55){return function(_dD){return new F(function(){return A(_dD,[new T(function(){return B(A(_dt,[_u]));})]);});};}else{return new F(function(){return _dx([0,_dw-48|0]);});}}break;case 10:if(48>_dw){return function(_dE){return new F(function(){return A(_dE,[new T(function(){return B(A(_dt,[_u]));})]);});};}else{if(_dw>57){return function(_dF){return new F(function(){return A(_dF,[new T(function(){return B(A(_dt,[_u]));})]);});};}else{return new F(function(){return _dx([0,_dw-48|0]);});}}break;case 16:if(48>_dw){if(97>_dw){if(65>_dw){return function(_dG){return new F(function(){return A(_dG,[new T(function(){return B(A(_dt,[_u]));})]);});};}else{if(_dw>70){return function(_dH){return new F(function(){return A(_dH,[new T(function(){return B(A(_dt,[_u]));})]);});};}else{return new F(function(){return _dx([0,(_dw-65|0)+10|0]);});}}}else{if(_dw>102){if(65>_dw){return function(_dI){return new F(function(){return A(_dI,[new T(function(){return B(A(_dt,[_u]));})]);});};}else{if(_dw>70){return function(_dJ){return new F(function(){return A(_dJ,[new T(function(){return B(A(_dt,[_u]));})]);});};}else{return new F(function(){return _dx([0,(_dw-65|0)+10|0]);});}}}else{return new F(function(){return _dx([0,(_dw-97|0)+10|0]);});}}}else{if(_dw>57){if(97>_dw){if(65>_dw){return function(_dK){return new F(function(){return A(_dK,[new T(function(){return B(A(_dt,[_u]));})]);});};}else{if(_dw>70){return function(_dL){return new F(function(){return A(_dL,[new T(function(){return B(A(_dt,[_u]));})]);});};}else{return new F(function(){return _dx([0,(_dw-65|0)+10|0]);});}}}else{if(_dw>102){if(65>_dw){return function(_dM){return new F(function(){return A(_dM,[new T(function(){return B(A(_dt,[_u]));})]);});};}else{if(_dw>70){return function(_dN){return new F(function(){return A(_dN,[new T(function(){return B(A(_dt,[_u]));})]);});};}else{return new F(function(){return _dx([0,(_dw-65|0)+10|0]);});}}}else{return new F(function(){return _dx([0,(_dw-97|0)+10|0]);});}}}else{return new F(function(){return _dx([0,_dw-48|0]);});}}break;default:return E(_dn);}}};return function(_dO){return new F(function(){return A(_dr,[_dO,_dk,function(_dP){var _dQ=E(_dP);return _dQ[0]==0?[2]:B(A(_dq,[_dQ]));}]);});};},_dR=[0,10],_dS=[0,1],_dT=[0,2147483647],_dU=function(_dV,_dW){while(1){var _dX=E(_dV);if(!_dX[0]){var _dY=_dX[1],_dZ=E(_dW);if(!_dZ[0]){var _e0=_dZ[1],_e1=addC(_dY,_e0);if(!E(_e1[2])){return [0,_e1[1]];}else{_dV=[1,I_fromInt(_dY)];_dW=[1,I_fromInt(_e0)];continue;}}else{_dV=[1,I_fromInt(_dY)];_dW=_dZ;continue;}}else{var _e2=E(_dW);if(!_e2[0]){_dV=_dX;_dW=[1,I_fromInt(_e2[1])];continue;}else{return [1,I_add(_dX[1],_e2[1])];}}}},_e3=new T(function(){return B(_dU(_dT,_dS));}),_e4=function(_e5){var _e6=E(_e5);if(!_e6[0]){var _e7=E(_e6[1]);return _e7==(-2147483648)?E(_e3):[0, -_e7];}else{return [1,I_negate(_e6[1])];}},_e8=[0,10],_e9=[0,0],_ea=function(_eb){return [0,_eb];},_ec=function(_ed,_ee){while(1){var _ef=E(_ed);if(!_ef[0]){var _eg=_ef[1],_eh=E(_ee);if(!_eh[0]){var _ei=_eh[1];if(!(imul(_eg,_ei)|0)){return [0,imul(_eg,_ei)|0];}else{_ed=[1,I_fromInt(_eg)];_ee=[1,I_fromInt(_ei)];continue;}}else{_ed=[1,I_fromInt(_eg)];_ee=_eh;continue;}}else{var _ej=E(_ee);if(!_ej[0]){_ed=_ef;_ee=[1,I_fromInt(_ej[1])];continue;}else{return [1,I_mul(_ef[1],_ej[1])];}}}},_ek=function(_el,_em,_en){while(1){var _eo=E(_en);if(!_eo[0]){return E(_em);}else{var _ep=B(_dU(B(_ec(_em,_el)),B(_ea(E(_eo[1])[1]))));_en=_eo[2];_em=_ep;continue;}}},_eq=function(_er){var _es=new T(function(){return B(_bA(B(_bA([0,function(_et){return E(E(_et)[1])==45?[1,B(_do(_dR,function(_eu){return new F(function(){return A(_er,[[1,new T(function(){return B(_e4(B(_ek(_e8,_e9,_eu))));})]]);});}))]:[2];}],[0,function(_ev){return E(E(_ev)[1])==43?[1,B(_do(_dR,function(_ew){return new F(function(){return A(_er,[[1,new T(function(){return B(_ek(_e8,_e9,_ew));})]]);});}))]:[2];}])),new T(function(){return [1,B(_do(_dR,function(_ex){return new F(function(){return A(_er,[[1,new T(function(){return B(_ek(_e8,_e9,_ex));})]]);});}))];})));});return new F(function(){return _bA([0,function(_ey){return E(E(_ey)[1])==101?E(_es):[2];}],[0,function(_ez){return E(E(_ez)[1])==69?E(_es):[2];}]);});},_eA=function(_eB){return new F(function(){return A(_eB,[_2v]);});},_eC=function(_eD){return new F(function(){return A(_eD,[_2v]);});},_eE=function(_eF){return function(_eG){return E(E(_eG)[1])==46?[1,B(_do(_dR,function(_eH){return new F(function(){return A(_eF,[[1,_eH]]);});}))]:[2];};},_eI=function(_eJ){return [0,B(_eE(_eJ))];},_eK=function(_eL){return new F(function(){return _do(_dR,function(_eM){return [1,B(_cM(_eI,_eA,function(_eN){return [1,B(_cM(_eq,_eC,function(_eO){return new F(function(){return A(_eL,[[5,[1,_eM,_eN,_eO]]]);});}))];}))];});});},_eP=function(_eQ){return [1,B(_eK(_eQ))];},_eR=function(_eS,_eT,_eU){while(1){var _eV=E(_eU);if(!_eV[0]){return false;}else{if(!B(A(_2D,[_eS,_eT,_eV[1]]))){_eU=_eV[2];continue;}else{return true;}}}},_eW=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_eX=function(_eY){return new F(function(){return _eR(_ci,_eY,_eW);});},_eZ=[0,8],_f0=[0,16],_f1=function(_f2){var _f3=function(_f4){return new F(function(){return A(_f2,[[5,[0,_eZ,_f4]]]);});},_f5=function(_f6){return new F(function(){return A(_f2,[[5,[0,_f0,_f6]]]);});};return function(_f7){return E(E(_f7)[1])==48?E([0,function(_f8){switch(E(E(_f8)[1])){case 79:return [1,B(_do(_eZ,_f3))];case 88:return [1,B(_do(_f0,_f5))];case 111:return [1,B(_do(_eZ,_f3))];case 120:return [1,B(_do(_f0,_f5))];default:return [2];}}]):[2];};},_f9=function(_fa){return [0,B(_f1(_fa))];},_fb=function(_fc){var _fd=new T(function(){return B(A(_fc,[_eZ]));}),_fe=new T(function(){return B(A(_fc,[_f0]));});return function(_ff){switch(E(E(_ff)[1])){case 79:return E(_fd);case 88:return E(_fe);case 111:return E(_fd);case 120:return E(_fe);default:return [2];}};},_fg=function(_fh){return [0,B(_fb(_fh))];},_fi=[0,92],_fj=function(_fk){return new F(function(){return A(_fk,[_dR]);});},_fl=function(_fm){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_81(9,_fm,_u));}))));});},_fn=function(_fo){var _fp=E(_fo);return _fp[0]==0?E(_fp[1]):I_toInt(_fp[1]);},_fq=function(_fr,_fs){var _ft=E(_fr);if(!_ft[0]){var _fu=_ft[1],_fv=E(_fs);return _fv[0]==0?_fu<=_fv[1]:I_compareInt(_fv[1],_fu)>=0;}else{var _fw=_ft[1],_fx=E(_fs);return _fx[0]==0?I_compareInt(_fw,_fx[1])<=0:I_compare(_fw,_fx[1])<=0;}},_fy=function(_fz){return [2];},_fA=function(_fB){var _fC=E(_fB);if(!_fC[0]){return E(_fy);}else{var _fD=_fC[1],_fE=E(_fC[2]);return _fE[0]==0?E(_fD):function(_fF){return new F(function(){return _bA(B(A(_fD,[_fF])),new T(function(){return B(A(new T(function(){return B(_fA(_fE));}),[_fF]));}));});};}},_fG=function(_fH){return [2];},_fI=function(_fJ,_fK){var _fL=function(_fM,_fN){var _fO=E(_fM);if(!_fO[0]){return function(_fP){return new F(function(){return A(_fP,[_fJ]);});};}else{var _fQ=E(_fN);return _fQ[0]==0?E(_fG):E(_fO[1])[1]!=E(_fQ[1])[1]?E(_fG):function(_fR){return [0,function(_fS){return E(new T(function(){return B(A(new T(function(){return B(_fL(_fO[2],_fQ[2]));}),[_fR]));}));}];};}};return function(_fT){return new F(function(){return A(_fL,[_fJ,_fT,_fK]);});};},_fU=new T(function(){return B(unCStr("SOH"));}),_fV=[0,1],_fW=function(_fX){return [1,B(_fI(_fU,function(_fY){return E(new T(function(){return B(A(_fX,[_fV]));}));}))];},_fZ=new T(function(){return B(unCStr("SO"));}),_g0=[0,14],_g1=function(_g2){return [1,B(_fI(_fZ,function(_g3){return E(new T(function(){return B(A(_g2,[_g0]));}));}))];},_g4=function(_g5){return [1,B(_cM(_fW,_g1,_g5))];},_g6=new T(function(){return B(unCStr("NUL"));}),_g7=[0,0],_g8=function(_g9){return [1,B(_fI(_g6,function(_ga){return E(new T(function(){return B(A(_g9,[_g7]));}));}))];},_gb=new T(function(){return B(unCStr("STX"));}),_gc=[0,2],_gd=function(_ge){return [1,B(_fI(_gb,function(_gf){return E(new T(function(){return B(A(_ge,[_gc]));}));}))];},_gg=new T(function(){return B(unCStr("ETX"));}),_gh=[0,3],_gi=function(_gj){return [1,B(_fI(_gg,function(_gk){return E(new T(function(){return B(A(_gj,[_gh]));}));}))];},_gl=new T(function(){return B(unCStr("EOT"));}),_gm=[0,4],_gn=function(_go){return [1,B(_fI(_gl,function(_gp){return E(new T(function(){return B(A(_go,[_gm]));}));}))];},_gq=new T(function(){return B(unCStr("ENQ"));}),_gr=[0,5],_gs=function(_gt){return [1,B(_fI(_gq,function(_gu){return E(new T(function(){return B(A(_gt,[_gr]));}));}))];},_gv=new T(function(){return B(unCStr("ACK"));}),_gw=[0,6],_gx=function(_gy){return [1,B(_fI(_gv,function(_gz){return E(new T(function(){return B(A(_gy,[_gw]));}));}))];},_gA=new T(function(){return B(unCStr("BEL"));}),_gB=[0,7],_gC=function(_gD){return [1,B(_fI(_gA,function(_gE){return E(new T(function(){return B(A(_gD,[_gB]));}));}))];},_gF=new T(function(){return B(unCStr("BS"));}),_gG=[0,8],_gH=function(_gI){return [1,B(_fI(_gF,function(_gJ){return E(new T(function(){return B(A(_gI,[_gG]));}));}))];},_gK=new T(function(){return B(unCStr("HT"));}),_gL=[0,9],_gM=function(_gN){return [1,B(_fI(_gK,function(_gO){return E(new T(function(){return B(A(_gN,[_gL]));}));}))];},_gP=new T(function(){return B(unCStr("LF"));}),_gQ=[0,10],_gR=function(_gS){return [1,B(_fI(_gP,function(_gT){return E(new T(function(){return B(A(_gS,[_gQ]));}));}))];},_gU=new T(function(){return B(unCStr("VT"));}),_gV=[0,11],_gW=function(_gX){return [1,B(_fI(_gU,function(_gY){return E(new T(function(){return B(A(_gX,[_gV]));}));}))];},_gZ=new T(function(){return B(unCStr("FF"));}),_h0=[0,12],_h1=function(_h2){return [1,B(_fI(_gZ,function(_h3){return E(new T(function(){return B(A(_h2,[_h0]));}));}))];},_h4=new T(function(){return B(unCStr("CR"));}),_h5=[0,13],_h6=function(_h7){return [1,B(_fI(_h4,function(_h8){return E(new T(function(){return B(A(_h7,[_h5]));}));}))];},_h9=new T(function(){return B(unCStr("SI"));}),_ha=[0,15],_hb=function(_hc){return [1,B(_fI(_h9,function(_hd){return E(new T(function(){return B(A(_hc,[_ha]));}));}))];},_he=new T(function(){return B(unCStr("DLE"));}),_hf=[0,16],_hg=function(_hh){return [1,B(_fI(_he,function(_hi){return E(new T(function(){return B(A(_hh,[_hf]));}));}))];},_hj=new T(function(){return B(unCStr("DC1"));}),_hk=[0,17],_hl=function(_hm){return [1,B(_fI(_hj,function(_hn){return E(new T(function(){return B(A(_hm,[_hk]));}));}))];},_ho=new T(function(){return B(unCStr("DC2"));}),_hp=[0,18],_hq=function(_hr){return [1,B(_fI(_ho,function(_hs){return E(new T(function(){return B(A(_hr,[_hp]));}));}))];},_ht=new T(function(){return B(unCStr("DC3"));}),_hu=[0,19],_hv=function(_hw){return [1,B(_fI(_ht,function(_hx){return E(new T(function(){return B(A(_hw,[_hu]));}));}))];},_hy=new T(function(){return B(unCStr("DC4"));}),_hz=[0,20],_hA=function(_hB){return [1,B(_fI(_hy,function(_hC){return E(new T(function(){return B(A(_hB,[_hz]));}));}))];},_hD=new T(function(){return B(unCStr("NAK"));}),_hE=[0,21],_hF=function(_hG){return [1,B(_fI(_hD,function(_hH){return E(new T(function(){return B(A(_hG,[_hE]));}));}))];},_hI=new T(function(){return B(unCStr("SYN"));}),_hJ=[0,22],_hK=function(_hL){return [1,B(_fI(_hI,function(_hM){return E(new T(function(){return B(A(_hL,[_hJ]));}));}))];},_hN=new T(function(){return B(unCStr("ETB"));}),_hO=[0,23],_hP=function(_hQ){return [1,B(_fI(_hN,function(_hR){return E(new T(function(){return B(A(_hQ,[_hO]));}));}))];},_hS=new T(function(){return B(unCStr("CAN"));}),_hT=[0,24],_hU=function(_hV){return [1,B(_fI(_hS,function(_hW){return E(new T(function(){return B(A(_hV,[_hT]));}));}))];},_hX=new T(function(){return B(unCStr("EM"));}),_hY=[0,25],_hZ=function(_i0){return [1,B(_fI(_hX,function(_i1){return E(new T(function(){return B(A(_i0,[_hY]));}));}))];},_i2=new T(function(){return B(unCStr("SUB"));}),_i3=[0,26],_i4=function(_i5){return [1,B(_fI(_i2,function(_i6){return E(new T(function(){return B(A(_i5,[_i3]));}));}))];},_i7=new T(function(){return B(unCStr("ESC"));}),_i8=[0,27],_i9=function(_ia){return [1,B(_fI(_i7,function(_ib){return E(new T(function(){return B(A(_ia,[_i8]));}));}))];},_ic=new T(function(){return B(unCStr("FS"));}),_id=[0,28],_ie=function(_if){return [1,B(_fI(_ic,function(_ig){return E(new T(function(){return B(A(_if,[_id]));}));}))];},_ih=new T(function(){return B(unCStr("GS"));}),_ii=[0,29],_ij=function(_ik){return [1,B(_fI(_ih,function(_il){return E(new T(function(){return B(A(_ik,[_ii]));}));}))];},_im=new T(function(){return B(unCStr("RS"));}),_in=[0,30],_io=function(_ip){return [1,B(_fI(_im,function(_iq){return E(new T(function(){return B(A(_ip,[_in]));}));}))];},_ir=new T(function(){return B(unCStr("US"));}),_is=[0,31],_it=function(_iu){return [1,B(_fI(_ir,function(_iv){return E(new T(function(){return B(A(_iu,[_is]));}));}))];},_iw=new T(function(){return B(unCStr("SP"));}),_ix=[0,32],_iy=function(_iz){return [1,B(_fI(_iw,function(_iA){return E(new T(function(){return B(A(_iz,[_ix]));}));}))];},_iB=new T(function(){return B(unCStr("DEL"));}),_iC=[0,127],_iD=function(_iE){return [1,B(_fI(_iB,function(_iF){return E(new T(function(){return B(A(_iE,[_iC]));}));}))];},_iG=[1,_iD,_u],_iH=[1,_iy,_iG],_iI=[1,_it,_iH],_iJ=[1,_io,_iI],_iK=[1,_ij,_iJ],_iL=[1,_ie,_iK],_iM=[1,_i9,_iL],_iN=[1,_i4,_iM],_iO=[1,_hZ,_iN],_iP=[1,_hU,_iO],_iQ=[1,_hP,_iP],_iR=[1,_hK,_iQ],_iS=[1,_hF,_iR],_iT=[1,_hA,_iS],_iU=[1,_hv,_iT],_iV=[1,_hq,_iU],_iW=[1,_hl,_iV],_iX=[1,_hg,_iW],_iY=[1,_hb,_iX],_iZ=[1,_h6,_iY],_j0=[1,_h1,_iZ],_j1=[1,_gW,_j0],_j2=[1,_gR,_j1],_j3=[1,_gM,_j2],_j4=[1,_gH,_j3],_j5=[1,_gC,_j4],_j6=[1,_gx,_j5],_j7=[1,_gs,_j6],_j8=[1,_gn,_j7],_j9=[1,_gi,_j8],_ja=[1,_gd,_j9],_jb=[1,_g8,_ja],_jc=[1,_g4,_jb],_jd=new T(function(){return B(_fA(_jc));}),_je=[0,1114111],_jf=[0,34],_jg=[0,39],_jh=function(_ji){var _jj=new T(function(){return B(A(_ji,[_gB]));}),_jk=new T(function(){return B(A(_ji,[_gG]));}),_jl=new T(function(){return B(A(_ji,[_gL]));}),_jm=new T(function(){return B(A(_ji,[_gQ]));}),_jn=new T(function(){return B(A(_ji,[_gV]));}),_jo=new T(function(){return B(A(_ji,[_h0]));}),_jp=new T(function(){return B(A(_ji,[_h5]));});return new F(function(){return _bA([0,function(_jq){switch(E(E(_jq)[1])){case 34:return E(new T(function(){return B(A(_ji,[_jf]));}));case 39:return E(new T(function(){return B(A(_ji,[_jg]));}));case 92:return E(new T(function(){return B(A(_ji,[_fi]));}));case 97:return E(_jj);case 98:return E(_jk);case 102:return E(_jo);case 110:return E(_jm);case 114:return E(_jp);case 116:return E(_jl);case 118:return E(_jn);default:return [2];}}],new T(function(){return B(_bA([1,B(_cM(_fg,_fj,function(_jr){return [1,B(_do(_jr,function(_js){var _jt=B(_ek(new T(function(){return B(_ea(E(_jr)[1]));}),_e9,_js));return !B(_fq(_jt,_je))?[2]:B(A(_ji,[new T(function(){var _ju=B(_fn(_jt));if(_ju>>>0>1114111){var _jv=B(_fl(_ju));}else{var _jv=[0,_ju];}var _jw=_jv,_jx=_jw,_jy=_jx;return _jy;})]));}))];}))],new T(function(){return B(_bA([0,function(_jz){return E(E(_jz)[1])==94?E([0,function(_jA){switch(E(E(_jA)[1])){case 64:return E(new T(function(){return B(A(_ji,[_g7]));}));case 65:return E(new T(function(){return B(A(_ji,[_fV]));}));case 66:return E(new T(function(){return B(A(_ji,[_gc]));}));case 67:return E(new T(function(){return B(A(_ji,[_gh]));}));case 68:return E(new T(function(){return B(A(_ji,[_gm]));}));case 69:return E(new T(function(){return B(A(_ji,[_gr]));}));case 70:return E(new T(function(){return B(A(_ji,[_gw]));}));case 71:return E(_jj);case 72:return E(_jk);case 73:return E(_jl);case 74:return E(_jm);case 75:return E(_jn);case 76:return E(_jo);case 77:return E(_jp);case 78:return E(new T(function(){return B(A(_ji,[_g0]));}));case 79:return E(new T(function(){return B(A(_ji,[_ha]));}));case 80:return E(new T(function(){return B(A(_ji,[_hf]));}));case 81:return E(new T(function(){return B(A(_ji,[_hk]));}));case 82:return E(new T(function(){return B(A(_ji,[_hp]));}));case 83:return E(new T(function(){return B(A(_ji,[_hu]));}));case 84:return E(new T(function(){return B(A(_ji,[_hz]));}));case 85:return E(new T(function(){return B(A(_ji,[_hE]));}));case 86:return E(new T(function(){return B(A(_ji,[_hJ]));}));case 87:return E(new T(function(){return B(A(_ji,[_hO]));}));case 88:return E(new T(function(){return B(A(_ji,[_hT]));}));case 89:return E(new T(function(){return B(A(_ji,[_hY]));}));case 90:return E(new T(function(){return B(A(_ji,[_i3]));}));case 91:return E(new T(function(){return B(A(_ji,[_i8]));}));case 92:return E(new T(function(){return B(A(_ji,[_id]));}));case 93:return E(new T(function(){return B(A(_ji,[_ii]));}));case 94:return E(new T(function(){return B(A(_ji,[_in]));}));case 95:return E(new T(function(){return B(A(_ji,[_is]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_jd,[_ji]));})));})));}));});},_jB=function(_jC){return new F(function(){return A(_jC,[_cG]);});},_jD=function(_jE){var _jF=E(_jE);if(!_jF[0]){return E(_jB);}else{var _jG=_jF[2],_jH=E(E(_jF[1])[1]);switch(_jH){case 9:return function(_jI){return [0,function(_jJ){return E(new T(function(){return B(A(new T(function(){return B(_jD(_jG));}),[_jI]));}));}];};case 10:return function(_jK){return [0,function(_jL){return E(new T(function(){return B(A(new T(function(){return B(_jD(_jG));}),[_jK]));}));}];};case 11:return function(_jM){return [0,function(_jN){return E(new T(function(){return B(A(new T(function(){return B(_jD(_jG));}),[_jM]));}));}];};case 12:return function(_jO){return [0,function(_jP){return E(new T(function(){return B(A(new T(function(){return B(_jD(_jG));}),[_jO]));}));}];};case 13:return function(_jQ){return [0,function(_jR){return E(new T(function(){return B(A(new T(function(){return B(_jD(_jG));}),[_jQ]));}));}];};case 32:return function(_jS){return [0,function(_jT){return E(new T(function(){return B(A(new T(function(){return B(_jD(_jG));}),[_jS]));}));}];};case 160:return function(_jU){return [0,function(_jV){return E(new T(function(){return B(A(new T(function(){return B(_jD(_jG));}),[_jU]));}));}];};default:var _jW=u_iswspace(_jH),_jX=_jW;return E(_jX)==0?E(_jB):function(_jY){return [0,function(_jZ){return E(new T(function(){return B(A(new T(function(){return B(_jD(_jG));}),[_jY]));}));}];};}}},_k0=function(_k1){var _k2=new T(function(){return B(_k0(_k1));}),_k3=[1,function(_k4){return new F(function(){return A(_jD,[_k4,function(_k5){return E([0,function(_k6){return E(E(_k6)[1])==92?E(_k2):[2];}]);}]);});}];return new F(function(){return _bA([0,function(_k7){return E(E(_k7)[1])==92?E([0,function(_k8){var _k9=E(E(_k8)[1]);switch(_k9){case 9:return E(_k3);case 10:return E(_k3);case 11:return E(_k3);case 12:return E(_k3);case 13:return E(_k3);case 32:return E(_k3);case 38:return E(_k2);case 160:return E(_k3);default:var _ka=u_iswspace(_k9),_kb=_ka;return E(_kb)==0?[2]:E(_k3);}}]):[2];}],[0,function(_kc){var _kd=E(_kc);return E(_kd[1])==92?E(new T(function(){return B(_jh(function(_ke){return new F(function(){return A(_k1,[[0,_ke,_n]]);});}));})):B(A(_k1,[[0,_kd,_r]]));}]);});},_kf=function(_kg,_kh){return new F(function(){return _k0(function(_ki){var _kj=E(_ki),_kk=E(_kj[1]);if(E(_kk[1])==34){if(!E(_kj[2])){return E(new T(function(){return B(A(_kh,[[1,new T(function(){return B(A(_kg,[_u]));})]]));}));}else{return new F(function(){return _kf(function(_kl){return new F(function(){return A(_kg,[[1,_kk,_kl]]);});},_kh);});}}else{return new F(function(){return _kf(function(_km){return new F(function(){return A(_kg,[[1,_kk,_km]]);});},_kh);});}});});},_kn=new T(function(){return B(unCStr("_\'"));}),_ko=function(_kp){var _kq=u_iswalnum(_kp),_kr=_kq;return E(_kr)==0?B(_eR(_ci,[0,_kp],_kn)):true;},_ks=function(_kt){return new F(function(){return _ko(E(_kt)[1]);});},_ku=new T(function(){return B(unCStr(",;()[]{}`"));}),_kv=new T(function(){return B(unCStr(".."));}),_kw=new T(function(){return B(unCStr("::"));}),_kx=new T(function(){return B(unCStr("->"));}),_ky=[0,64],_kz=[1,_ky,_u],_kA=[0,126],_kB=[1,_kA,_u],_kC=new T(function(){return B(unCStr("=>"));}),_kD=[1,_kC,_u],_kE=[1,_kB,_kD],_kF=[1,_kz,_kE],_kG=[1,_kx,_kF],_kH=new T(function(){return B(unCStr("<-"));}),_kI=[1,_kH,_kG],_kJ=[0,124],_kK=[1,_kJ,_u],_kL=[1,_kK,_kI],_kM=[1,_fi,_u],_kN=[1,_kM,_kL],_kO=[0,61],_kP=[1,_kO,_u],_kQ=[1,_kP,_kN],_kR=[1,_kw,_kQ],_kS=[1,_kv,_kR],_kT=function(_kU){return new F(function(){return _bA([1,function(_kV){return E(_kV)[0]==0?E(new T(function(){return B(A(_kU,[_dj]));})):[2];}],new T(function(){return B(_bA([0,function(_kW){return E(E(_kW)[1])==39?E([0,function(_kX){var _kY=E(_kX);switch(E(_kY[1])){case 39:return [2];case 92:return E(new T(function(){return B(_jh(function(_kZ){return [0,function(_l0){return E(E(_l0)[1])==39?E(new T(function(){return B(A(_kU,[[0,_kZ]]));})):[2];}];}));}));default:return [0,function(_l1){return E(E(_l1)[1])==39?E(new T(function(){return B(A(_kU,[[0,_kY]]));})):[2];}];}}]):[2];}],new T(function(){return B(_bA([0,function(_l2){return E(E(_l2)[1])==34?E(new T(function(){return B(_kf(_dk,_kU));})):[2];}],new T(function(){return B(_bA([0,function(_l3){return !B(_eR(_ci,_l3,_ku))?[2]:B(A(_kU,[[2,[1,_l3,_u]]]));}],new T(function(){return B(_bA([0,function(_l4){return !B(_eR(_ci,_l4,_eW))?[2]:[1,B(_d8(_eX,function(_l5){var _l6=[1,_l4,_l5];return !B(_eR(_cr,_l6,_kS))?B(A(_kU,[[4,_l6]])):B(A(_kU,[[2,_l6]]));}))];}],new T(function(){return B(_bA([0,function(_l7){var _l8=E(_l7),_l9=_l8[1],_la=u_iswalpha(_l9),_lb=_la;return E(_lb)==0?E(_l9)==95?[1,B(_d8(_ks,function(_lc){return new F(function(){return A(_kU,[[3,[1,_l8,_lc]]]);});}))]:[2]:[1,B(_d8(_ks,function(_ld){return new F(function(){return A(_kU,[[3,[1,_l8,_ld]]]);});}))];}],new T(function(){return [1,B(_cM(_f9,_eP,_kU))];})));})));})));})));})));}));});},_le=[0,0],_lf=function(_lg,_lh){return function(_li){return new F(function(){return A(_jD,[_li,function(_lj){return E(new T(function(){return B(_kT(function(_lk){var _ll=E(_lk);return _ll[0]==2?!B(_c7(_ll[1],_c6))?[2]:E(new T(function(){return B(A(_lg,[_le,function(_lm){return [1,function(_ln){return new F(function(){return A(_jD,[_ln,function(_lo){return E(new T(function(){return B(_kT(function(_lp){var _lq=E(_lp);return _lq[0]==2?!B(_c7(_lq[1],_c4))?[2]:E(new T(function(){return B(A(_lh,[_lm]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_lr=function(_ls,_lt,_lu){var _lv=function(_lw,_lx){return new F(function(){return _bA([1,function(_ly){return new F(function(){return A(_jD,[_ly,function(_lz){return E(new T(function(){return B(_kT(function(_lA){var _lB=E(_lA);if(_lB[0]==4){var _lC=E(_lB[1]);if(!_lC[0]){return new F(function(){return A(_ls,[_lB,_lw,_lx]);});}else{return E(E(_lC[1])[1])==45?E(_lC[2])[0]==0?E([1,function(_lD){return new F(function(){return A(_jD,[_lD,function(_lE){return E(new T(function(){return B(_kT(function(_lF){return new F(function(){return A(_ls,[_lF,_lw,function(_lG){return new F(function(){return A(_lx,[new T(function(){return B(_e4(_lG));})]);});}]);});}));}));}]);});}]):B(A(_ls,[_lB,_lw,_lx])):B(A(_ls,[_lB,_lw,_lx]));}}else{return new F(function(){return A(_ls,[_lB,_lw,_lx]);});}}));}));}]);});}],new T(function(){return [1,B(_lf(_lv,_lx))];}));});};return new F(function(){return _lv(_lt,_lu);});},_lH=function(_lI,_lJ){return [2];},_lK=function(_lL){var _lM=E(_lL);return _lM[0]==0?[1,new T(function(){return B(_ek(new T(function(){return B(_ea(E(_lM[1])[1]));}),_e9,_lM[2]));})]:E(_lM[2])[0]==0?E(_lM[3])[0]==0?[1,new T(function(){return B(_ek(_e8,_e9,_lM[1]));})]:[0]:[0];},_lN=function(_lO){var _lP=E(_lO);if(_lP[0]==5){var _lQ=B(_lK(_lP[1]));return _lQ[0]==0?E(_lH):function(_lR,_lS){return new F(function(){return A(_lS,[_lQ[1]]);});};}else{return E(_lH);}},_lT=function(_lU){return [1,function(_lV){return new F(function(){return A(_jD,[_lV,function(_lW){return E([3,_lU,_cD]);}]);});}];},_lX=new T(function(){return B(_lr(_lN,_le,_lT));}),_lY=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_lZ=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_m0=function(_m1){while(1){var _m2=(function(_m3){var _m4=E(_m3);if(!_m4[0]){return [0];}else{var _m5=_m4[2],_m6=E(_m4[1]);if(!E(_m6[2])[0]){return [1,_m6[1],new T(function(){return B(_m0(_m5));})];}else{_m1=_m5;return null;}}})(_m1);if(_m2!=null){return _m2;}}},_m7=function(_){var _m8=jsEval("Date.now()"),_m9=_m8;return new T(function(){var _ma=B(_m0(B(_bq(_lX,new T(function(){return fromJSStr(_m9);})))));return _ma[0]==0?B(err(_lZ)):E(_ma[2])[0]==0?E(_ma[1]):B(err(_lY));});},_mb=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_mc=new T(function(){return B(unCStr("base"));}),_md=new T(function(){return B(unCStr("IOException"));}),_me=new T(function(){var _mf=hs_wordToWord64(4053623282),_mg=_mf,_mh=hs_wordToWord64(3693590983),_mi=_mh;return [0,_mg,_mi,[0,_mg,_mi,_mc,_mb,_md],_u];}),_mj=function(_mk){return E(_me);},_ml=function(_mm){var _mn=E(_mm);return new F(function(){return _ak(B(_ai(_mn[1])),_mj,_mn[2]);});},_mo=new T(function(){return B(unCStr(": "));}),_mp=[0,41],_mq=new T(function(){return B(unCStr(" ("));}),_mr=new T(function(){return B(unCStr("already exists"));}),_ms=new T(function(){return B(unCStr("does not exist"));}),_mt=new T(function(){return B(unCStr("protocol error"));}),_mu=new T(function(){return B(unCStr("failed"));}),_mv=new T(function(){return B(unCStr("invalid argument"));}),_mw=new T(function(){return B(unCStr("inappropriate type"));}),_mx=new T(function(){return B(unCStr("hardware fault"));}),_my=new T(function(){return B(unCStr("unsupported operation"));}),_mz=new T(function(){return B(unCStr("timeout"));}),_mA=new T(function(){return B(unCStr("resource vanished"));}),_mB=new T(function(){return B(unCStr("interrupted"));}),_mC=new T(function(){return B(unCStr("resource busy"));}),_mD=new T(function(){return B(unCStr("resource exhausted"));}),_mE=new T(function(){return B(unCStr("end of file"));}),_mF=new T(function(){return B(unCStr("illegal operation"));}),_mG=new T(function(){return B(unCStr("permission denied"));}),_mH=new T(function(){return B(unCStr("user error"));}),_mI=new T(function(){return B(unCStr("unsatisified constraints"));}),_mJ=new T(function(){return B(unCStr("system error"));}),_mK=function(_mL,_mM){switch(E(_mL)){case 0:return new F(function(){return _O(_mr,_mM);});break;case 1:return new F(function(){return _O(_ms,_mM);});break;case 2:return new F(function(){return _O(_mC,_mM);});break;case 3:return new F(function(){return _O(_mD,_mM);});break;case 4:return new F(function(){return _O(_mE,_mM);});break;case 5:return new F(function(){return _O(_mF,_mM);});break;case 6:return new F(function(){return _O(_mG,_mM);});break;case 7:return new F(function(){return _O(_mH,_mM);});break;case 8:return new F(function(){return _O(_mI,_mM);});break;case 9:return new F(function(){return _O(_mJ,_mM);});break;case 10:return new F(function(){return _O(_mt,_mM);});break;case 11:return new F(function(){return _O(_mu,_mM);});break;case 12:return new F(function(){return _O(_mv,_mM);});break;case 13:return new F(function(){return _O(_mw,_mM);});break;case 14:return new F(function(){return _O(_mx,_mM);});break;case 15:return new F(function(){return _O(_my,_mM);});break;case 16:return new F(function(){return _O(_mz,_mM);});break;case 17:return new F(function(){return _O(_mA,_mM);});break;default:return new F(function(){return _O(_mB,_mM);});}},_mN=[0,125],_mO=new T(function(){return B(unCStr("{handle: "));}),_mP=function(_mQ,_mR,_mS,_mT,_mU,_mV){var _mW=new T(function(){var _mX=new T(function(){return B(_mK(_mR,new T(function(){var _mY=E(_mT);return _mY[0]==0?E(_mV):B(_O(_mq,new T(function(){return B(_O(_mY,[1,_mp,_mV]));})));})));}),_mZ=E(_mS);return _mZ[0]==0?E(_mX):B(_O(_mZ,new T(function(){return B(_O(_mo,_mX));})));}),_n0=E(_mU);if(!_n0[0]){var _n1=E(_mQ);if(!_n1[0]){return E(_mW);}else{var _n2=E(_n1[1]);return _n2[0]==0?B(_O(_mO,new T(function(){return B(_O(_n2[1],[1,_mN,new T(function(){return B(_O(_mo,_mW));})]));}))):B(_O(_mO,new T(function(){return B(_O(_n2[1],[1,_mN,new T(function(){return B(_O(_mo,_mW));})]));})));}}else{return new F(function(){return _O(_n0[1],new T(function(){return B(_O(_mo,_mW));}));});}},_n3=function(_n4){var _n5=E(_n4);return new F(function(){return _mP(_n5[1],_n5[2],_n5[3],_n5[4],_n5[6],_u);});},_n6=function(_n7,_n8){var _n9=E(_n7);return new F(function(){return _mP(_n9[1],_n9[2],_n9[3],_n9[4],_n9[6],_n8);});},_na=function(_nb,_nc){return new F(function(){return _aF(_n6,_nb,_nc);});},_nd=function(_ne,_nf,_ng){var _nh=E(_nf);return new F(function(){return _mP(_nh[1],_nh[2],_nh[3],_nh[4],_nh[6],_ng);});},_ni=[0,_nd,_n3,_na],_nj=new T(function(){return [0,_mj,_ni,_nk,_ml];}),_nk=function(_nl){return [0,_nj,_nl];},_nm=7,_nn=function(_no){return [0,_2v,_nm,_u,_no,_2v,_2v];},_np=function(_nq,_){return new F(function(){return die(new T(function(){return B(_nk(new T(function(){return B(_nn(_nq));})));}));});},_nr=function(_ns,_){return new F(function(){return _np(_ns,_);});},_nt=[0,0],_nu=[0,0],_nv=function(_nw,_nx,_ny){var _nz=function(_nA,_nB){return new F(function(){return _bA([1,function(_nC){return new F(function(){return A(_jD,[_nC,function(_nD){return E(new T(function(){return B(_kT(function(_nE){var _nF=E(_nE);if(_nF[0]==4){var _nG=E(_nF[1]);if(!_nG[0]){return new F(function(){return A(_nw,[_nF,_nA,_nB]);});}else{return E(E(_nG[1])[1])==45?E(_nG[2])[0]==0?E([1,function(_nH){return new F(function(){return A(_jD,[_nH,function(_nI){return E(new T(function(){return B(_kT(function(_nJ){return new F(function(){return A(_nw,[_nJ,_nA,function(_nK){return new F(function(){return A(_nB,[new T(function(){return [0, -E(_nK)[1]];})]);});}]);});}));}));}]);});}]):B(A(_nw,[_nF,_nA,_nB])):B(A(_nw,[_nF,_nA,_nB]));}}else{return new F(function(){return A(_nw,[_nF,_nA,_nB]);});}}));}));}]);});}],new T(function(){return [1,B(_lf(_nz,_nB))];}));});};return new F(function(){return _nz(_nx,_ny);});},_nL=function(_nM,_nN){return [2];},_nO=function(_nP){var _nQ=E(_nP);if(_nQ[0]==5){var _nR=B(_lK(_nQ[1]));return _nR[0]==0?E(_nL):function(_nS,_nT){return new F(function(){return A(_nT,[new T(function(){return [0,B(_fn(_nR[1]))];})]);});};}else{return E(_nL);}},_nU=new T(function(){return B(_nv(_nO,_le,_lT));}),_nV=[0,1],_nW=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:98:3-10"));}),_nX=function(_){var _nY=B(_m7(_)),_nZ=_nY,_o0=jsFind("unread-badge"),_o1=_o0,_o2=E(_o1);if(!_o2[0]){return new F(function(){return _nr(_nW,_);});}else{var _o3=jsGet(E(_o2[1])[1],"innerHTML"),_o4=_o3;return [0,_nt,_nt,_nt,_nZ,_r,_44,_44,_nt,_nV,_nV,new T(function(){var _o5=new T(function(){return fromJSStr(_o4);});if(!B(_c7(_o5,_u))){var _o6=B(_m0(B(_bq(_nU,_o5)))),_o7=_o6[0]==0?B(err(_lZ)):E(_o6[2])[0]==0?E(_o6[1]):B(err(_lY));}else{var _o7=E(_nu);}return _o7;})];}},_o8=function(_){var _=0;return new F(function(){return _nX(_);});},_o9=function(_oa){var _ob=B(A(_oa,[_])),_oc=_ob;return E(_oc);},_od=new T(function(){return B(_o9(_o8));}),_oe=new T(function(){return B(err(_lY));}),_of=new T(function(){return B(err(_lZ));}),_og=[0,_1E],_oh=new T(function(){return B(unCStr("Tried to deserialize a non-Number to a Double"));}),_oi=[0,_oh],_oj=[0,_2z],_ok=[0,_2B],_ol=function(_om){var _on=E(_om);if(_on[0]==4){var _oo=_on[1],_op=B(_2F(_2u,_a7,_oo));if(!_op[0]){return E(_oj);}else{var _oq=E(_op[1]);if(!_oq[0]){var _or=_oq[1],_os=B(_2F(_2u,_a6,_oo));if(!_os[0]){return E(_oj);}else{var _ot=E(_os[1]);if(!_ot[0]){var _ou=_ot[1],_ov=B(_2F(_2u,_a5,_oo));if(!_ov[0]){return E(_oj);}else{var _ow=E(_ov[1]);if(!_ow[0]){var _ox=_ow[1],_oy=B(_2F(_2u,_a4,_oo));if(!_oy[0]){return E(_oj);}else{var _oz=E(_oy[1]);if(_oz[0]==1){var _oA=_oz[1],_oB=function(_oC){var _oD=function(_oE){var _oF=B(_2F(_2u,_a1,_oo));if(!_oF[0]){return E(_oj);}else{var _oG=E(_oF[1]);if(!_oG[0]){var _oH=_oG[1],_oI=function(_oJ){var _oK=function(_oL){var _oM=function(_oN){return [1,new T(function(){var _oO=E(_od),_oP=_oO[5],_oQ=_oO[6],_oR=_oO[7],_oS=_oO[9],_oT=_oO[10],_oU=_oO[11],_oV=new T(function(){var _oW=B(_m0(B(_bq(_lX,new T(function(){return fromJSStr(E(_oA)[1]);})))));return _oW[0]==0?E(_of):E(_oW[2])[0]==0?E(_oW[1]):E(_oe);}),_oX=E(_oN);if(!_oX[0]){var _oY=E(_oE);if(!_oY[0]){var _oZ=E(_oL);if(!_oZ[0]){var _p0=E(_oJ),_p1=_p0[0]==0?[0,_or,_ou,_ox,_oV,_oP,_oQ,_oR,_oH,_oS,_oT,_oU]:[0,_or,_ou,_ox,_oV,_oP,_oQ,_oR,_oH,_p0[1],_oT,_oU];}else{var _p2=_oZ[1],_p3=E(_oJ),_p1=_p3[0]==0?[0,_or,_ou,_ox,_oV,_oP,_oQ,_oR,_oH,_oS,_p2,_oU]:[0,_or,_ou,_ox,_oV,_oP,_oQ,_oR,_oH,_p3[1],_p2,_oU];}var _p4=_p1;}else{var _p5=_oY[1],_p6=E(_oL);if(!_p6[0]){var _p7=E(_oJ),_p8=_p7[0]==0?[0,_or,_ou,_ox,_oV,_oP,_oQ,_p5,_oH,_oS,_oT,_oU]:[0,_or,_ou,_ox,_oV,_oP,_oQ,_p5,_oH,_p7[1],_oT,_oU];}else{var _p9=_p6[1],_pa=E(_oJ),_p8=_pa[0]==0?[0,_or,_ou,_ox,_oV,_oP,_oQ,_p5,_oH,_oS,_p9,_oU]:[0,_or,_ou,_ox,_oV,_oP,_oQ,_p5,_oH,_pa[1],_p9,_oU];}var _p4=_p8;}var _pb=_p4;}else{var _pc=_oX[1],_pd=E(_oL);if(!_pd[0]){var _pe=E(_oJ),_pf=_pe[0]==0?[0,_or,_ou,_ox,_oV,_oP,_oQ,new T(function(){return B(_7L(B(_1q(_9V,_pc))));}),_oH,_oS,_oT,_oU]:[0,_or,_ou,_ox,_oV,_oP,_oQ,new T(function(){return B(_7L(B(_1q(_9V,_pc))));}),_oH,_pe[1],_oT,_oU];}else{var _pg=_pd[1],_ph=E(_oJ),_pf=_ph[0]==0?[0,_or,_ou,_ox,_oV,_oP,_oQ,new T(function(){return B(_7L(B(_1q(_9V,_pc))));}),_oH,_oS,_pg,_oU]:[0,_or,_ou,_ox,_oV,_oP,_oQ,new T(function(){return B(_7L(B(_1q(_9V,_pc))));}),_oH,_ph[1],_pg,_oU];}var _pb=_pf;}var _pi=_pb;return _pi;})];},_pj=B(_2F(_2u,_9Y,_oo));if(!_pj[0]){return new F(function(){return _oM(_2v);});}else{var _pk=B(_3n(_3W,_3W,_pj[1]));return _pk[0]==0?B(_oM(_2v)):B(_oM([1,_pk[1]]));}},_pl=B(_2F(_2u,_9Z,_oo));if(!_pl[0]){return new F(function(){return _oK(_2v);});}else{var _pm=E(_pl[1]);return _pm[0]==0?B(_oK([1,_pm[1]])):B(_oK(_2v));}},_pn=B(_2F(_2u,_a0,_oo));if(!_pn[0]){return new F(function(){return _oI(_2v);});}else{var _po=E(_pn[1]);return _po[0]==0?B(_oI([1,_po[1]])):B(_oI(_2v));}}else{return E(_oi);}}},_pp=B(_2F(_2u,_a2,_oo));if(!_pp[0]){return new F(function(){return _oD(_2v);});}else{var _pq=B(_3n(_2a,_3W,_pp[1]));return _pq[0]==0?B(_oD(_2v)):B(_oD([1,new T(function(){return B(_7L(_pq[1]));})]));}},_pr=B(_2F(_2u,_a3,_oo));if(!_pr[0]){return new F(function(){return _oB(_);});}else{var _ps=B(_3n(_2a,_39,_pr[1]));if(!_ps[0]){return new F(function(){return _oB(_);});}else{var _pt=_ps[1],_pu=function(_pv){var _pw=B(_2F(_2u,_a1,_oo));if(!_pw[0]){return E(_oj);}else{var _px=E(_pw[1]);if(!_px[0]){var _py=_px[1],_pz=function(_pA){var _pB=function(_pC){var _pD=function(_pE){return [1,new T(function(){var _pF=E(_od),_pG=_pF[5],_pH=_pF[7],_pI=_pF[9],_pJ=_pF[10],_pK=_pF[11],_pL=new T(function(){var _pM=B(_m0(B(_bq(_lX,new T(function(){return fromJSStr(E(_oA)[1]);})))));return _pM[0]==0?E(_of):E(_pM[2])[0]==0?E(_pM[1]):E(_oe);}),_pN=E(_pE);if(!_pN[0]){var _pO=E(_pv);if(!_pO[0]){var _pP=E(_pC);if(!_pP[0]){var _pQ=E(_pA),_pR=_pQ[0]==0?[0,_or,_ou,_ox,_pL,_pG,new T(function(){return B(_7L(_pt));}),_pH,_py,_pI,_pJ,_pK]:[0,_or,_ou,_ox,_pL,_pG,new T(function(){return B(_7L(_pt));}),_pH,_py,_pQ[1],_pJ,_pK];}else{var _pS=_pP[1],_pT=E(_pA),_pR=_pT[0]==0?[0,_or,_ou,_ox,_pL,_pG,new T(function(){return B(_7L(_pt));}),_pH,_py,_pI,_pS,_pK]:[0,_or,_ou,_ox,_pL,_pG,new T(function(){return B(_7L(_pt));}),_pH,_py,_pT[1],_pS,_pK];}var _pU=_pR;}else{var _pV=_pO[1],_pW=E(_pC);if(!_pW[0]){var _pX=E(_pA),_pY=_pX[0]==0?[0,_or,_ou,_ox,_pL,_pG,new T(function(){return B(_7L(_pt));}),_pV,_py,_pI,_pJ,_pK]:[0,_or,_ou,_ox,_pL,_pG,new T(function(){return B(_7L(_pt));}),_pV,_py,_pX[1],_pJ,_pK];}else{var _pZ=_pW[1],_q0=E(_pA),_pY=_q0[0]==0?[0,_or,_ou,_ox,_pL,_pG,new T(function(){return B(_7L(_pt));}),_pV,_py,_pI,_pZ,_pK]:[0,_or,_ou,_ox,_pL,_pG,new T(function(){return B(_7L(_pt));}),_pV,_py,_q0[1],_pZ,_pK];}var _pU=_pY;}var _q1=_pU;}else{var _q2=_pN[1],_q3=E(_pC);if(!_q3[0]){var _q4=E(_pA),_q5=_q4[0]==0?[0,_or,_ou,_ox,_pL,_pG,new T(function(){return B(_7L(_pt));}),new T(function(){return B(_7L(B(_1q(_9V,_q2))));}),_py,_pI,_pJ,_pK]:[0,_or,_ou,_ox,_pL,_pG,new T(function(){return B(_7L(_pt));}),new T(function(){return B(_7L(B(_1q(_9V,_q2))));}),_py,_q4[1],_pJ,_pK];}else{var _q6=_q3[1],_q7=E(_pA),_q5=_q7[0]==0?[0,_or,_ou,_ox,_pL,_pG,new T(function(){return B(_7L(_pt));}),new T(function(){return B(_7L(B(_1q(_9V,_q2))));}),_py,_pI,_q6,_pK]:[0,_or,_ou,_ox,_pL,_pG,new T(function(){return B(_7L(_pt));}),new T(function(){return B(_7L(B(_1q(_9V,_q2))));}),_py,_q7[1],_q6,_pK];}var _q1=_q5;}var _q8=_q1;return _q8;})];},_q9=B(_2F(_2u,_9Y,_oo));if(!_q9[0]){return new F(function(){return _pD(_2v);});}else{var _qa=B(_3n(_3W,_3W,_q9[1]));return _qa[0]==0?B(_pD(_2v)):B(_pD([1,_qa[1]]));}},_qb=B(_2F(_2u,_9Z,_oo));if(!_qb[0]){return new F(function(){return _pB(_2v);});}else{var _qc=E(_qb[1]);return _qc[0]==0?B(_pB([1,_qc[1]])):B(_pB(_2v));}},_qd=B(_2F(_2u,_a0,_oo));if(!_qd[0]){return new F(function(){return _pz(_2v);});}else{var _qe=E(_qd[1]);return _qe[0]==0?B(_pz([1,_qe[1]])):B(_pz(_2v));}}else{return E(_oi);}}},_qf=B(_2F(_2u,_a2,_oo));if(!_qf[0]){return new F(function(){return _pu(_2v);});}else{var _qg=B(_3n(_2a,_3W,_qf[1]));return _qg[0]==0?B(_pu(_2v)):B(_pu([1,new T(function(){return B(_7L(_qg[1]));})]));}}}}else{return E(_og);}}}else{return E(_oi);}}}else{return E(_oi);}}}else{return E(_oi);}}}else{return E(_ok);}},_qh=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_qi=[0,_qh],_qj=[1,_u],_qk=function(_ql){var _qm=E(_ql);if(!_qm[0]){return E(_qj);}else{var _qn=B(_ol(_qm[1]));if(!_qn[0]){return [0,_qn[1]];}else{var _qo=B(_qk(_qm[2]));return _qo[0]==0?[0,_qo[1]]:[1,[1,_qn[1],_qo[1]]];}}},_qp=function(_qq){var _qr=E(_qq);return _qr[0]==3?B(_qk(_qr[1])):E(_qi);},_qs=[0,_1n,_1u,_ol,_qp],_qt=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_qu=new T(function(){return B(err(_qt));}),_qv=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_qw=new T(function(){return B(err(_qv));}),_qx=function(_qy,_qz){while(1){var _qA=E(_qy);if(!_qA[0]){return E(_qw);}else{var _qB=E(_qz);if(!_qB){return E(_qA[1]);}else{_qy=_qA[2];_qz=_qB-1|0;continue;}}}},_qC=new T(function(){return B(unCStr("ACK"));}),_qD=new T(function(){return B(unCStr("BEL"));}),_qE=new T(function(){return B(unCStr("BS"));}),_qF=new T(function(){return B(unCStr("SP"));}),_qG=[1,_qF,_u],_qH=new T(function(){return B(unCStr("US"));}),_qI=[1,_qH,_qG],_qJ=new T(function(){return B(unCStr("RS"));}),_qK=[1,_qJ,_qI],_qL=new T(function(){return B(unCStr("GS"));}),_qM=[1,_qL,_qK],_qN=new T(function(){return B(unCStr("FS"));}),_qO=[1,_qN,_qM],_qP=new T(function(){return B(unCStr("ESC"));}),_qQ=[1,_qP,_qO],_qR=new T(function(){return B(unCStr("SUB"));}),_qS=[1,_qR,_qQ],_qT=new T(function(){return B(unCStr("EM"));}),_qU=[1,_qT,_qS],_qV=new T(function(){return B(unCStr("CAN"));}),_qW=[1,_qV,_qU],_qX=new T(function(){return B(unCStr("ETB"));}),_qY=[1,_qX,_qW],_qZ=new T(function(){return B(unCStr("SYN"));}),_r0=[1,_qZ,_qY],_r1=new T(function(){return B(unCStr("NAK"));}),_r2=[1,_r1,_r0],_r3=new T(function(){return B(unCStr("DC4"));}),_r4=[1,_r3,_r2],_r5=new T(function(){return B(unCStr("DC3"));}),_r6=[1,_r5,_r4],_r7=new T(function(){return B(unCStr("DC2"));}),_r8=[1,_r7,_r6],_r9=new T(function(){return B(unCStr("DC1"));}),_ra=[1,_r9,_r8],_rb=new T(function(){return B(unCStr("DLE"));}),_rc=[1,_rb,_ra],_rd=new T(function(){return B(unCStr("SI"));}),_re=[1,_rd,_rc],_rf=new T(function(){return B(unCStr("SO"));}),_rg=[1,_rf,_re],_rh=new T(function(){return B(unCStr("CR"));}),_ri=[1,_rh,_rg],_rj=new T(function(){return B(unCStr("FF"));}),_rk=[1,_rj,_ri],_rl=new T(function(){return B(unCStr("VT"));}),_rm=[1,_rl,_rk],_rn=new T(function(){return B(unCStr("LF"));}),_ro=[1,_rn,_rm],_rp=new T(function(){return B(unCStr("HT"));}),_rq=[1,_rp,_ro],_rr=[1,_qE,_rq],_rs=[1,_qD,_rr],_rt=[1,_qC,_rs],_ru=new T(function(){return B(unCStr("ENQ"));}),_rv=[1,_ru,_rt],_rw=new T(function(){return B(unCStr("EOT"));}),_rx=[1,_rw,_rv],_ry=new T(function(){return B(unCStr("ETX"));}),_rz=[1,_ry,_rx],_rA=new T(function(){return B(unCStr("STX"));}),_rB=[1,_rA,_rz],_rC=new T(function(){return B(unCStr("SOH"));}),_rD=[1,_rC,_rB],_rE=new T(function(){return B(unCStr("NUL"));}),_rF=[1,_rE,_rD],_rG=[0,92],_rH=new T(function(){return B(unCStr("\\DEL"));}),_rI=new T(function(){return B(unCStr("\\a"));}),_rJ=new T(function(){return B(unCStr("\\\\"));}),_rK=new T(function(){return B(unCStr("\\SO"));}),_rL=new T(function(){return B(unCStr("\\r"));}),_rM=new T(function(){return B(unCStr("\\f"));}),_rN=new T(function(){return B(unCStr("\\v"));}),_rO=new T(function(){return B(unCStr("\\n"));}),_rP=new T(function(){return B(unCStr("\\t"));}),_rQ=new T(function(){return B(unCStr("\\b"));}),_rR=function(_rS,_rT){if(_rS<=127){var _rU=E(_rS);switch(_rU){case 92:return new F(function(){return _O(_rJ,_rT);});break;case 127:return new F(function(){return _O(_rH,_rT);});break;default:if(_rU<32){var _rV=E(_rU);switch(_rV){case 7:return new F(function(){return _O(_rI,_rT);});break;case 8:return new F(function(){return _O(_rQ,_rT);});break;case 9:return new F(function(){return _O(_rP,_rT);});break;case 10:return new F(function(){return _O(_rO,_rT);});break;case 11:return new F(function(){return _O(_rN,_rT);});break;case 12:return new F(function(){return _O(_rM,_rT);});break;case 13:return new F(function(){return _O(_rL,_rT);});break;case 14:return new F(function(){return _O(_rK,new T(function(){var _rW=E(_rT);if(!_rW[0]){var _rX=[0];}else{var _rX=E(E(_rW[1])[1])==72?B(unAppCStr("\\&",_rW)):E(_rW);}return _rX;}));});break;default:return new F(function(){return _O([1,_rG,new T(function(){var _rY=_rV;return _rY>=0?B(_qx(_rF,_rY)):E(_qu);})],_rT);});}}else{return [1,[0,_rU],_rT];}}}else{return [1,_rG,new T(function(){var _rZ=jsShowI(_rS),_s0=_rZ;return B(_O(fromJSStr(_s0),new T(function(){var _s1=E(_rT);if(!_s1[0]){var _s2=[0];}else{var _s3=E(_s1[1])[1];if(_s3<48){var _s4=E(_s1);}else{var _s4=_s3>57?E(_s1):B(unAppCStr("\\&",_s1));}var _s5=_s4,_s6=_s5,_s2=_s6;}return _s2;})));})];}},_s7=[0,39],_s8=[1,_s7,_u],_s9=new T(function(){return B(unCStr("\'\\\'\'"));}),_sa=function(_sb){var _sc=E(E(_sb)[1]);return _sc==39?E(_s9):[1,_s7,new T(function(){return B(_rR(_sc,_s8));})];},_sd=[0,34],_se=new T(function(){return B(unCStr("\\\""));}),_sf=function(_sg,_sh){var _si=E(_sg);if(!_si[0]){return E(_sh);}else{var _sj=_si[2],_sk=E(E(_si[1])[1]);if(_sk==34){return new F(function(){return _O(_se,new T(function(){return B(_sf(_sj,_sh));}));});}else{return new F(function(){return _rR(_sk,new T(function(){return B(_sf(_sj,_sh));}));});}}},_sl=function(_sm,_sn){return [1,_sd,new T(function(){return B(_sf(_sm,[1,_sd,_sn]));})];},_so=function(_sp){return new F(function(){return _O(_s9,_sp);});},_sq=function(_sr,_ss){var _st=E(E(_ss)[1]);return _st==39?E(_so):function(_su){return [1,_s7,new T(function(){return B(_rR(_st,[1,_s7,_su]));})];};},_sv=[0,_sq,_sa,_sl],_sw=function(_sx){return E(E(_sx)[3]);},_sy=function(_sz,_sA){return new F(function(){return A(_sw,[_sz,_sA,_u]);});},_sB=function(_sC,_sD,_sE){return new F(function(){return _aF(new T(function(){return B(_sw(_sC));}),_sD,_sE);});},_sF=function(_sG){return [0,function(_sH){return E(new T(function(){return B(_sw(_sG));}));},function(_sp){return new F(function(){return _sy(_sG,_sp);});},function(_sI,_sp){return new F(function(){return _sB(_sG,_sI,_sp);});}];},_sJ=new T(function(){return B(_sF(_sv));}),_sK=new T(function(){return B(unCStr("Just "));}),_sL=new T(function(){return B(unCStr("Nothing"));}),_sM=[0,11],_sN=function(_sO){return E(E(_sO)[1]);},_sP=function(_sQ,_sR,_sS,_sT){var _sU=E(_sS);if(!_sU[0]){return new F(function(){return _O(_sL,_sT);});}else{var _sV=_sU[1];return E(_sR)[1]<=10?B(_O(_sK,new T(function(){return B(A(_sN,[_sQ,_sM,_sV,_sT]));}))):[1,_17,new T(function(){return B(_O(_sK,new T(function(){return B(A(_sN,[_sQ,_sM,_sV,[1,_16,_sT]]));})));})];}},_sW=[0,0],_sX=function(_sY,_sZ){return new F(function(){return _sP(_sY,_sW,_sZ,_u);});},_t0=function(_t1,_t2,_t3){return new F(function(){return _aF(function(_sI,_sp){return new F(function(){return _sP(_t1,_sW,_sI,_sp);});},_t2,_t3);});},_t4=function(_t5){return [0,function(_t6,_sI,_sp){return new F(function(){return _sP(_t5,_t6,_sI,_sp);});},function(_sp){return new F(function(){return _sX(_t5,_sp);});},function(_sI,_sp){return new F(function(){return _t0(_t5,_sI,_sp);});}];},_t7=new T(function(){return B(_t4(_sJ));}),_t8=function(_t9){var _ta=jsShow(E(_t9)[1]),_tb=_ta;return new F(function(){return fromJSStr(_tb);});},_tc=function(_td){return function(_d5){return new F(function(){return _O(new T(function(){return B(_t8(_td));}),_d5);});};},_te=function(_tf){return new F(function(){return _81(0,E(_tf)[1],_u);});},_tg=function(_th,_ti){return new F(function(){return _81(0,E(_th)[1],_ti);});},_tj=function(_tk,_tl){return new F(function(){return _aF(_tg,_tk,_tl);});},_tm=function(_tn,_to,_tp){return new F(function(){return _81(E(_tn)[1],E(_to)[1],_tp);});},_tq=[0,_tm,_te,_tj],_tr=function(_ts,_tt,_tu){return new F(function(){return A(_ts,[[1,_aC,new T(function(){return B(A(_tt,[_tu]));})]]);});},_tv=new T(function(){return B(unCStr(": empty list"));}),_tw=new T(function(){return B(unCStr("Prelude."));}),_tx=function(_ty){return new F(function(){return err(B(_O(_tw,new T(function(){return B(_O(_ty,_tv));}))));});},_tz=new T(function(){return B(unCStr("foldr1"));}),_tA=new T(function(){return B(_tx(_tz));}),_tB=function(_tC,_tD){var _tE=E(_tD);if(!_tE[0]){return E(_tA);}else{var _tF=_tE[1],_tG=E(_tE[2]);if(!_tG[0]){return E(_tF);}else{return new F(function(){return A(_tC,[_tF,new T(function(){return B(_tB(_tC,_tG));})]);});}}},_tH=function(_tI,_tJ,_tK,_tL){return new F(function(){return _aF(function(_tM,_tN){var _tO=E(_tM);return [1,_17,new T(function(){return B(A(_tB,[_tr,[1,new T(function(){return B(A(new T(function(){return B(_sN(_tI));}),[_sW,_tO[1]]));}),[1,new T(function(){return B(A(new T(function(){return B(_sN(_tJ));}),[_sW,_tO[2]]));}),_u]],[1,_16,_tN]]));})];},_tK,_tL);});},_tP=new T(function(){return B(unCStr("fromList "));}),_tQ=function(_tR,_tS){while(1){var _tT=(function(_tU,_tV){var _tW=E(_tV);if(!_tW[0]){_tR=[1,[0,_tW[2],_tW[3]],new T(function(){return B(_tQ(_tU,_tW[5]));})];_tS=_tW[4];return null;}else{return E(_tU);}})(_tR,_tS);if(_tT!=null){return _tT;}}},_tX=function(_tY,_tZ,_u0,_u1){var _u2=new T(function(){return B(_tQ(_u,_u1));});return _u0<=10?function(_u3){return new F(function(){return _O(_tP,new T(function(){return B(_tH(_tY,_tZ,_u2,_u3));}));});}:function(_u4){return [1,_17,new T(function(){return B(_O(_tP,new T(function(){return B(_tH(_tY,_tZ,_u2,[1,_16,_u4]));})));})];};},_u5=[0,45],_u6=function(_u7,_u8,_u9){var _ua=function(_ub){var _uc=new T(function(){return B(A(_u7,[[0, -_u9]]));});return E(_u8)[1]<=6?function(_ud){return [1,_u5,new T(function(){return B(A(_uc,[_ud]));})];}:function(_ue){return [1,_17,[1,_u5,new T(function(){return B(A(_uc,[[1,_16,_ue]]));})]];};};if(_u9>=0){var _uf=isDoubleNegativeZero(_u9),_ug=_uf;return E(_ug)==0?B(A(_u7,[[0,_u9]])):B(_ua(_));}else{return new F(function(){return _ua(_);});}},_uh=[0,125],_ui=new T(function(){return B(unCStr("_logUnread = "));}),_uj=new T(function(){return B(unCStr(", "));}),_uk=new T(function(){return B(unCStr("_lpsCoeff = "));}),_ul=new T(function(){return B(unCStr("_dependCoeff = "));}),_um=new T(function(){return B(unCStr("_maxLoves = "));}),_un=new T(function(){return B(unCStr("_items = "));}),_uo=new T(function(){return B(unCStr("_achieves = "));}),_up=new T(function(){return B(unCStr("_hasFocus = "));}),_uq=new T(function(){return B(unCStr("_lastFocus = "));}),_ur=new T(function(){return B(unCStr("_depend = "));}),_us=new T(function(){return B(unCStr("_lps = "));}),_ut=new T(function(){return B(unCStr("_loves = "));}),_uu=new T(function(){return B(unCStr("Aichan {"));}),_uv=new T(function(){return B(unCStr("True"));}),_uw=new T(function(){return B(unCStr("False"));}),_ux=function(_uy,_uz,_uA,_uB,_uC,_uD,_uE,_uF,_uG,_uH,_uI,_uJ){var _uK=function(_uL){return new F(function(){return _O(_uu,new T(function(){return B(_O(_ut,new T(function(){return B(A(new T(function(){return B(_u6(_tc,_nu,E(_uz)[1]));}),[new T(function(){return B(_O(_uj,new T(function(){return B(_O(_us,new T(function(){return B(A(new T(function(){return B(_u6(_tc,_nu,E(_uA)[1]));}),[new T(function(){return B(_O(_uj,new T(function(){return B(_O(_ur,new T(function(){return B(A(new T(function(){return B(_u6(_tc,_nu,E(_uB)[1]));}),[new T(function(){return B(_O(_uj,new T(function(){return B(_O(_uq,new T(function(){return B(_19(0,_uC,new T(function(){return B(_O(_uj,new T(function(){return B(_O(_up,new T(function(){var _uM=new T(function(){return B(_O(_uj,new T(function(){return B(_O(_uo,new T(function(){return B(A(new T(function(){return B(_tX(_sJ,_t7,0,_uE));}),[new T(function(){return B(_O(_uj,new T(function(){return B(_O(_un,new T(function(){return B(A(new T(function(){return B(_tX(_sJ,_tq,0,_uF));}),[new T(function(){return B(_O(_uj,new T(function(){return B(_O(_um,new T(function(){return B(A(new T(function(){return B(_u6(_tc,_nu,E(_uG)[1]));}),[new T(function(){return B(_O(_uj,new T(function(){return B(_O(_ul,new T(function(){return B(A(new T(function(){return B(_u6(_tc,_nu,E(_uH)[1]));}),[new T(function(){return B(_O(_uj,new T(function(){return B(_O(_uk,new T(function(){return B(A(new T(function(){return B(_u6(_tc,_nu,E(_uI)[1]));}),[new T(function(){return B(_O(_uj,new T(function(){return B(_O(_ui,new T(function(){return B(_81(0,E(_uJ)[1],[1,_uh,_uL]));})));})));})]));})));})));})]));})));})));})]));})));})));})]));})));})));})]));})));})));});return !E(_uD)?B(_O(_uw,_uM)):B(_O(_uv,_uM));})));})));})));})));})));})]));})));})));})]));})));})));})]));})));}));});};return _uy<11?E(_uK):function(_uN){return [1,_17,new T(function(){return B(_uK([1,_16,_uN]));})];};},_uO=function(_uP){var _uQ=E(_uP);return new F(function(){return A(_ux,[0,_uQ[1],_uQ[2],_uQ[3],_uQ[4],_uQ[5],_uQ[6],_uQ[7],_uQ[8],_uQ[9],_uQ[10],_uQ[11],_u]);});},_uR=new T(function(){return B(unCStr("Map.!: given key is not an element in the map"));}),_uS=new T(function(){return B(err(_uR));}),_uT=function(_uU,_uV){while(1){var _uW=E(_uU),_uX=E(_uV);if(!_uX[0]){switch(B(_3X(_uW,_uX[2]))){case 0:_uU=_uW;_uV=_uX[4];continue;case 1:return E(_uX[3]);default:_uU=_uW;_uV=_uX[5];continue;}}else{return E(_uS);}}},_uY=function(_uZ,_v0,_v1,_){var _v2=rMV(_v0),_v3=_v2,_v4=B(A(_v1,[_v3,_])),_v5=_v4,_=wMV(_v0,new T(function(){return E(E(_v5)[2]);})),_v6=jsSetTimeout(_uZ,function(_){var _v7=B(_uY(_uZ,_v0,_v1,_)),_v8=_v7;return _cG;});return new F(function(){return rMV(_v0);});},_v9=function(_va,_vb){while(1){var _vc=E(_va),_vd=E(_vb);if(!_vd[0]){switch(B(_3X(_vc,_vd[2]))){case 0:_va=_vc;_vb=_vd[4];continue;case 1:return true;default:_va=_vc;_vb=_vd[5];continue;}}else{return false;}}},_ve=function(_vf){return E(_vf);},_vg=function(_vh,_vi){if(_vh<=0){if(_vh>=0){return new F(function(){return quot(_vh,_vi);});}else{if(_vi<=0){return new F(function(){return quot(_vh,_vi);});}else{return quot(_vh+1|0,_vi)-1|0;}}}else{if(_vi>=0){if(_vh>=0){return new F(function(){return quot(_vh,_vi);});}else{if(_vi<=0){return new F(function(){return quot(_vh,_vi);});}else{return quot(_vh+1|0,_vi)-1|0;}}}else{return quot(_vh-1|0,_vi)-1|0;}}},_vj=new T(function(){return B(unCStr("ArithException"));}),_vk=new T(function(){return B(unCStr("GHC.Exception"));}),_vl=new T(function(){return B(unCStr("base"));}),_vm=new T(function(){var _vn=hs_wordToWord64(4194982440),_vo=_vn,_vp=hs_wordToWord64(3110813675),_vq=_vp;return [0,_vo,_vq,[0,_vo,_vq,_vl,_vk,_vj],_u];}),_vr=function(_vs){return E(_vm);},_vt=function(_vu){var _vv=E(_vu);return new F(function(){return _ak(B(_ai(_vv[1])),_vr,_vv[2]);});},_vw=new T(function(){return B(unCStr("arithmetic underflow"));}),_vx=new T(function(){return B(unCStr("arithmetic overflow"));}),_vy=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_vz=new T(function(){return B(unCStr("denormal"));}),_vA=new T(function(){return B(unCStr("divide by zero"));}),_vB=new T(function(){return B(unCStr("loss of precision"));}),_vC=function(_vD){switch(E(_vD)){case 0:return E(_vx);case 1:return E(_vw);case 2:return E(_vB);case 3:return E(_vA);case 4:return E(_vz);default:return E(_vy);}},_vE=function(_vF){return new F(function(){return _O(_vw,_vF);});},_vG=function(_vF){return new F(function(){return _O(_vx,_vF);});},_vH=function(_vF){return new F(function(){return _O(_vy,_vF);});},_vI=function(_vF){return new F(function(){return _O(_vz,_vF);});},_vJ=function(_vF){return new F(function(){return _O(_vA,_vF);});},_vK=function(_vF){return new F(function(){return _O(_vB,_vF);});},_vL=function(_vM){switch(E(_vM)){case 0:return E(_vG);case 1:return E(_vE);case 2:return E(_vK);case 3:return E(_vJ);case 4:return E(_vI);default:return E(_vH);}},_vN=function(_vO,_vP){return new F(function(){return _aF(_vL,_vO,_vP);});},_vQ=function(_vR,_vS){switch(E(_vS)){case 0:return E(_vG);case 1:return E(_vE);case 2:return E(_vK);case 3:return E(_vJ);case 4:return E(_vI);default:return E(_vH);}},_vT=[0,_vQ,_vC,_vN],_vU=new T(function(){return [0,_vr,_vT,_vV,_vt];}),_vV=function(_vF){return [0,_vU,_vF];},_vW=3,_vX=new T(function(){return B(_vV(_vW));}),_vY=new T(function(){return die(_vX);}),_vZ=0,_w0=new T(function(){return B(_vV(_vZ));}),_w1=new T(function(){return die(_w0);}),_w2=function(_w3,_w4){var _w5=E(_w4);switch(_w5){case -1:var _w6=E(_w3);return _w6==(-2147483648)?E(_w1):B(_vg(_w6,-1));case 0:return E(_vY);default:return new F(function(){return _vg(_w3,_w5);});}},_w7=function(_w8,_w9){var _wa=_w8%_w9;if(_w8<=0){if(_w8>=0){return E(_wa);}else{if(_w9<=0){return E(_wa);}else{var _wb=E(_wa);return _wb==0?0:_wb+_w9|0;}}}else{if(_w9>=0){if(_w8>=0){return E(_wa);}else{if(_w9<=0){return E(_wa);}else{var _wc=E(_wa);return _wc==0?0:_wc+_w9|0;}}}else{var _wd=E(_wa);return _wd==0?0:_wd+_w9|0;}}},_we=function(_wf,_wg,_wh){while(1){var _wi=(function(_wj,_wk,_wl){var _wm=E(_wl);if(!_wm[0]){return [0,_wj,_wk];}else{var _wn=_wm[1];_wf=new T(function(){var _wo=E(E(_wn)[1]);switch(_wo){case -1:var _wp=[0,0];break;case 0:var _wp=E(_vY);break;default:var _wp=[0,B(_w7(E(_wj)[1],_wo))];}var _wq=_wp;return _wq;});var _wr=[1,new T(function(){return [0,B(_w2(E(_wj)[1],E(_wn)[1]))];}),_wk];_wh=_wm[2];_wg=_wr;return null;}})(_wf,_wg,_wh);if(_wi!=null){return _wi;}}},_ws=function(_wt,_wu,_wv,_ww){return new F(function(){return _we(new T(function(){var _wx=E(E(_wv)[1]);switch(_wx){case -1:var _wy=[0,0];break;case 0:var _wy=E(_vY);break;default:var _wy=[0,B(_w7(E(_wt)[1],_wx))];}var _wz=_wy;return _wz;}),[1,new T(function(){return [0,B(_w2(E(_wt)[1],E(_wv)[1]))];}),_wu],_ww);});},_wA=function(_wB,_wC){while(1){var _wD=E(_wB),_wE=E(_wC);if(!_wE[0]){switch(B(_3X(_wD,_wE[2]))){case 0:_wB=_wD;_wC=_wE[4];continue;case 1:return [1,_wE[3]];default:_wB=_wD;_wC=_wE[5];continue;}}else{return [0];}}},_wF=function(_wG,_wH,_wI,_wJ){return new F(function(){return A(_wG,[function(_){var _wK=jsSetAttr(E(_wH)[1],toJSStr(E(_wI)),toJSStr(E(_wJ)));return _cG;}]);});},_wL=function(_wM,_wN,_wO,_wP){return new F(function(){return A(_wM,[function(_){var _wQ=jsSet(E(_wN)[1],toJSStr(E(_wO)),toJSStr(E(_wP)));return _cG;}]);});},_wR=function(_wS,_wT,_wU,_wV){return new F(function(){return A(_wS,[function(_){var _wW=jsSetStyle(E(_wT)[1],toJSStr(E(_wU)),toJSStr(E(_wV)));return _cG;}]);});},_wX=[1,_9R,_u],_wY=[1,_9B,_wX],_wZ=[0,10],_x0=[1,_wZ,_wY],_x1=[0,50],_x2=[1,_x1,_x0],_x3=[0,100],_x4=[1,_x3,_x2],_x5=[0,500],_x6=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:341:9-14"));}),_x7=function(_x8){var _x9=E(_x8);if(!_x9[0]){return [0];}else{return new F(function(){return _O(_x9[1],new T(function(){return B(_x7(_x9[2]));}));});}},_xa=[1,_x5,_x4],_xb=function(_xc){var _xd=E(_xc);return _xd[0]==0?_xd[1]:I_toNumber(_xd[1]);},_xe=new T(function(){return B(unCStr("disabled"));}),_xf=new T(function(){return B(unCStr("block"));}),_xg=new T(function(){return B(unCStr("display"));}),_xh=new T(function(){return B(unCStr("innerHTML"));}),_xi=new T(function(){return B(unCStr("%s<span class=\"item-%d\">%s</span>"));}),_xj=[0,-2147483648],_xk=new T(function(){return B(_x7(_u));}),_xl=new T(function(){return B(_1q(_ve,_xk));}),_xm=function(_xn,_xo,_){var _xp=B(A(_xn,[_])),_xq=_xp;return new F(function(){return A(_xo,[_xq,_]);});},_xr=function(_xs,_){return _xs;},_xt=function(_xu,_xv,_){var _xw=B(A(_xu,[_])),_xx=_xw;return new F(function(){return A(_xv,[_]);});},_xy=[0,_xm,_xt,_xr,_nr],_xz=[0,_xy,_dk],_xA=function(_xB){return E(E(_xB)[1]);},_xC=function(_xD){return E(E(_xD)[1]);},_xE=function(_xF){return E(E(_xF)[2]);},_xG=function(_xH){return E(E(_xH)[3]);},_xI=function(_xJ,_xK){var _xL=new T(function(){return B(_xA(_xJ));});return function(_xM){return new F(function(){return A(new T(function(){return B(_xC(_xL));}),[new T(function(){return B(A(_xE,[_xJ,_xK]));}),function(_xN){return new F(function(){return A(new T(function(){return B(_xG(_xL));}),[[0,_xN,_xM]]);});}]);});};},_xO=function(_xP){return new F(function(){return _xI(_xz,_xP);});},_xQ=function(_xR,_){return [0,_cG,_xR];},_xS=new T(function(){return B(unCStr("-cost"));}),_xT=new T(function(){return B(unCStr("-btn"));}),_xU=new T(function(){return B(unCStr("-box"));}),_xV=new T(function(){return B(unCStr("-num"));}),_xW=new T(function(){return B(unCStr("-icon"));}),_xX=function(_xY){return E(E(_xY)[1]);},_xZ=new T(function(){return B(unCStr("Negative exponent"));}),_y0=new T(function(){return B(err(_xZ));}),_y1=function(_y2,_y3,_y4){while(1){if(!(_y3%2)){var _y5=_y2*_y2,_y6=quot(_y3,2);_y2=_y5;_y3=_y6;continue;}else{var _y7=E(_y3);if(_y7==1){return _y2*_y4;}else{var _y5=_y2*_y2;_y3=quot(_y7-1|0,2);var _y8=_y2*_y4;_y2=_y5;_y4=_y8;continue;}}}},_y9=function(_ya,_yb){while(1){if(!(_yb%2)){var _yc=_ya*_ya,_yd=quot(_yb,2);_ya=_yc;_yb=_yd;continue;}else{var _ye=E(_yb);if(_ye==1){return E(_ya);}else{return new F(function(){return _y1(_ya*_ya,quot(_ye-1|0,2),_ya);});}}}},_yf=function(_yg){var _yh=I_decodeDouble(_yg);return [0,[1,_yh[2]],_yh[1]];},_yi=function(_yj){var _yk=hs_intToInt64(2147483647),_yl=_yk,_ym=hs_leInt64(_yj,_yl),_yn=_ym;if(!E(_yn)){return [1,I_fromInt64(_yj)];}else{var _yo=hs_intToInt64(-2147483648),_yp=_yo,_yq=hs_geInt64(_yj,_yp),_yr=_yq;if(!E(_yr)){return [1,I_fromInt64(_yj)];}else{var _ys=hs_int64ToInt(_yj),_yt=_ys;return new F(function(){return _ea(_yt);});}}},_yu=function(_yv){var _yw=hs_intToInt64(_yv),_yx=_yw;return E(_yx);},_yy=function(_yz){var _yA=E(_yz);return _yA[0]==0?B(_yu(_yA[1])):I_toInt64(_yA[1]);},_yB=[0,0],_yC=new T(function(){return [0,0/0];}),_yD=new T(function(){return [0,-1/0];}),_yE=new T(function(){return [0,1/0];}),_yF=[0,0],_yG=function(_yH,_yI){while(1){var _yJ=E(_yH);if(!_yJ[0]){_yH=[1,I_fromInt(_yJ[1])];continue;}else{var _yK=E(_yI);if(!_yK[0]){_yH=_yJ;_yI=[1,I_fromInt(_yK[1])];continue;}else{return new F(function(){return I_fromRat(_yJ[1],_yK[1]);});}}}},_yL=function(_yM,_yN){var _yO=E(_yM);if(!_yO[0]){var _yP=_yO[1],_yQ=E(_yN);return _yQ[0]==0?_yP==_yQ[1]:I_compareInt(_yQ[1],_yP)==0?true:false;}else{var _yR=_yO[1],_yS=E(_yN);return _yS[0]==0?I_compareInt(_yR,_yS[1])==0?true:false:I_compare(_yR,_yS[1])==0?true:false;}},_yT=function(_yU,_yV){return !B(_yL(_yV,_yF))?[0,B(_yG(_yU,_yV))]:!B(_yL(_yU,_yF))?!B(_Y(_yU,_yF))?E(_yE):E(_yD):E(_yC);},_yW=[0,5],_yX=[0,4],_yY=new T(function(){return B(_yT(_yW,_yX));}),_yZ=[0,-1],_z0=function(_z1,_z2){while(1){var _z3=E(_z1);if(!_z3[0]){_z1=[1,I_fromInt(_z3[1])];continue;}else{return [1,I_shiftLeft(_z3[1],_z2)];}}},_z4=function(_z5,_z6){if(_z6>=0){var _z7=function(_z8){var _z9=B(_yf(_z5*_z8)),_za=_z9[1],_zb=_z9[2];if(_zb>=0){return new F(function(){return _z0(_za,_zb);});}else{var _zc= -_zb;if(_zc<=52){var _zd=hs_uncheckedIShiftRA64(B(_yy(_za)),_zc),_ze=_zd;return new F(function(){return _yi(_ze);});}else{return !B(_Y(_za,_yB))?E(_yB):E(_yZ);}}},_zf=E(_z6);if(!_zf){return new F(function(){return _z7(1);});}else{return new F(function(){return _z7(B(_y9(E(_yY)[1],_zf)));});}}else{return E(_y0);}},_zg=function(_zh){return new F(function(){return _z4(1,E(_zh)[1]);});},_zi=new T(function(){return B(unCStr("\u4f1a\u8a71"));}),_zj=new T(function(){return B(unCStr("\u4f1a\u8a71<br>\u597d\u611f\u5ea6 +0.2"));}),_zk=new T(function(){return B(unCStr("fa-comments-o"));}),_zl=[0,_zk,_zj,_zi],_zm=function(_zn,_zo,_){return [0,_cG,new T(function(){var _zp=E(_zo);return [0,_zp[1],new T(function(){return [0,E(_zp[2])[1]+0.2];}),_zp[3],_zp[4],_zp[5],_zp[6],_zp[7],_zp[8],_zp[9],_zp[10],_zp[11]];})];},_zq=[0,_zg,_zm,_zl],_zr=[0,_9Q,_zq],_zs=function(_zt){return new F(function(){return _z4(50,E(_zt)[1]);});},_zu=new T(function(){return B(unCStr("\u30e1\u30fc\u30eb"));}),_zv=new T(function(){return B(unCStr("\u30e1\u30fc\u30eb<br>\u597d\u611f\u5ea6 +1.0"));}),_zw=new T(function(){return B(unCStr("fa-envelope"));}),_zx=[0,_zw,_zv,_zu],_zy=function(_zz,_zA,_){return [0,_cG,new T(function(){var _zB=E(_zA);return [0,_zB[1],new T(function(){return [0,E(_zB[2])[1]+1];}),_zB[3],_zB[4],_zB[5],_zB[6],_zB[7],_zB[8],_zB[9],_zB[10],_zB[11]];})];},_zC=[0,_zs,_zy,_zx],_zD=[0,_9M,_zC],_zE=function(_zF){return new F(function(){return _z4(1000,E(_zF)[1]);});},_zG=new T(function(){return B(unCStr("\u55ab\u8336\u5e97"));}),_zH=new T(function(){return B(unCStr("\u55ab\u8336\u5e97<br>\u597d\u611f\u5ea6 +10"));}),_zI=new T(function(){return B(unCStr("fa-coffee"));}),_zJ=[0,_zI,_zH,_zG],_zK=function(_zL,_zM,_){return [0,_cG,new T(function(){var _zN=E(_zM);return [0,_zN[1],new T(function(){return [0,E(_zN[2])[1]+10];}),_zN[3],_zN[4],_zN[5],_zN[6],_zN[7],_zN[8],_zN[9],_zN[10],_zN[11]];})];},_zO=[0,_zE,_zK,_zJ],_zP=[0,_9I,_zO],_zQ=function(_zR){return new F(function(){return _z4(20000,E(_zR)[1]);});},_zS=new T(function(){return B(unCStr("\u30d7\u30ec\u30bc\u30f3\u30c8"));}),_zT=new T(function(){return B(unCStr("\u30d7\u30ec\u30bc\u30f3\u30c8<br>\u597d\u611f\u5ea6 +100"));}),_zU=new T(function(){return B(unCStr("fa-gift"));}),_zV=[0,_zU,_zT,_zS],_zW=function(_zX,_zY,_){return [0,_cG,new T(function(){var _zZ=E(_zY);return [0,_zZ[1],new T(function(){return [0,E(_zZ[2])[1]+100];}),_zZ[3],_zZ[4],_zZ[5],_zZ[6],_zZ[7],_zZ[8],_zZ[9],_zZ[10],_zZ[11]];})];},_A0=[0,_zQ,_zW,_zV],_A1=[0,_9E,_A0],_A2=function(_A3){return new F(function(){return _z4(500000,E(_A3)[1]);});},_A4=new T(function(){return B(unCStr("\u65c5\u884c"));}),_A5=new T(function(){return B(unCStr("\u65c5\u884c<br>\u597d\u611f\u5ea6 +1000"));}),_A6=new T(function(){return B(unCStr("fa-plane"));}),_A7=[0,_A6,_A5,_A4],_A8=function(_A9,_Aa,_){return [0,_cG,new T(function(){var _Ab=E(_Aa);return [0,_Ab[1],new T(function(){return [0,E(_Ab[2])[1]+1000];}),_Ab[3],_Ab[4],_Ab[5],_Ab[6],_Ab[7],_Ab[8],_Ab[9],_Ab[10],_Ab[11]];})];},_Ac=[0,_A2,_A8,_A7],_Ad=[0,_9A,_Ac],_Ae=function(_Af){return new F(function(){return _z4(10000000,E(_Af)[1]);});},_Ag=[0,36554],_Ah=[1,_Ag,_u],_Ai=new T(function(){return B(unCStr("\u8eca<br>\u597d\u611f\u5ea6 +15000"));}),_Aj=new T(function(){return B(unCStr("fa-car"));}),_Ak=[0,_Aj,_Ai,_Ah],_Al=function(_Am,_An,_){return [0,_cG,new T(function(){var _Ao=E(_An);return [0,_Ao[1],new T(function(){return [0,E(_Ao[2])[1]+15000];}),_Ao[3],_Ao[4],_Ao[5],_Ao[6],_Ao[7],_Ao[8],_Ao[9],_Ao[10],_Ao[11]];})];},_Ap=[0,_Ae,_Al,_Ak],_Aq=[0,_9w,_Ap],_Ar=function(_As){return new F(function(){return _z4(250000000,E(_As)[1]);});},_At=[0,23478],_Au=[1,_At,_u],_Av=new T(function(){return B(unCStr("\u5bb6<br>\u597d\u611f\u5ea6 +200000"));}),_Aw=new T(function(){return B(unCStr("fa-home"));}),_Ax=[0,_Aw,_Av,_Au],_Ay=function(_Az,_AA,_){return [0,_cG,new T(function(){var _AB=E(_AA);return [0,_AB[1],new T(function(){return [0,E(_AB[2])[1]+200000];}),_AB[3],_AB[4],_AB[5],_AB[6],_AB[7],_AB[8],_AB[9],_AB[10],_AB[11]];})];},_AC=[0,_Ar,_Ay,_Ax],_AD=[0,_9s,_AC],_AE=function(_AF){var _AG=B(_yf(100000*_AF)),_AH=_AG[1],_AI=_AG[2];if(_AI>=0){return new F(function(){return _z0(_AH,_AI);});}else{var _AJ= -_AI;if(_AJ<=52){var _AK=hs_uncheckedIShiftRA64(B(_yy(_AH)),_AJ),_AL=_AK;return new F(function(){return _yi(_AL);});}else{return !B(_Y(_AH,_yB))?E(_yB):E(_yZ);}}},_AM=new T(function(){return B(_AE(1));}),_AN=function(_AO){if(_AO>=0){var _AP=E(_AO);if(!_AP){return E(_AM);}else{return new F(function(){return _AE(B(_y9(100,_AP)));});}}else{return E(_y0);}},_AQ=function(_AR){return new F(function(){return _AN(E(_AR)[1]);});},_AS=new T(function(){return B(unCStr("fa-eye"));}),_AT=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u53f3\u76ee"));}),_AU=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u53f3\u76ee<br>\u4f9d\u5b58\u5ea6\u30dc\u30fc\u30ca\u30b9\u304c\u5897\u3048\u307e\u3059\u3002"));}),_AV=[0,_AS,_AU,_AT],_AW=[0,1],_AX=function(_AY){return function(_AZ,_){return [0,_cG,new T(function(){var _B0=E(_AZ);return [0,_B0[1],_B0[2],_B0[3],_B0[4],_B0[5],_B0[6],_B0[7],_B0[8],new T(function(){var _B1=E(_AY)[1];if(_B1>=0){var _B2=E(_B1);if(!_B2){var _B3=E(_AW);}else{var _B3=[0,B(_y9(10,_B2))];}var _B4=_B3;}else{var _B4=E(_y0);}var _B5=_B4,_B6=_B5;return _B6;}),_B0[10],_B0[11]];})];};},_B7=[0,_AQ,_AX,_AV],_B8=[0,_92,_B7],_B9=function(_Ba){var _Bb=B(_yf(100000*_Ba)),_Bc=_Bb[1],_Bd=_Bb[2];if(_Bd>=0){return new F(function(){return _z0(_Bc,_Bd);});}else{var _Be= -_Bd;if(_Be<=52){var _Bf=hs_uncheckedIShiftRA64(B(_yy(_Bc)),_Be),_Bg=_Bf;return new F(function(){return _yi(_Bg);});}else{return !B(_Y(_Bc,_yB))?E(_yB):E(_yZ);}}},_Bh=new T(function(){return B(_B9(1));}),_Bi=function(_Bj){if(_Bj>=0){var _Bk=E(_Bj);if(!_Bk){return E(_Bh);}else{return new F(function(){return _B9(B(_y9(100,_Bk)));});}}else{return E(_y0);}},_Bl=function(_Bm){return new F(function(){return _Bi(E(_Bm)[1]);});},_Bn=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u5de6\u76ee"));}),_Bo=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u5de6\u76ee<br>\u4f9d\u5b58\u5ea6\u304c\u597d\u611f\u5ea6\u306b\u5909\u308f\u308b\u901f\u3055\u304c\u901f\u304f\u306a\u308a\u307e\u3059\u3002"));}),_Bp=[0,_AS,_Bo,_Bn],_Bq=function(_Br){return function(_Bs,_){return [0,_cG,new T(function(){var _Bt=E(_Bs);return [0,_Bt[1],_Bt[2],_Bt[3],_Bt[4],_Bt[5],_Bt[6],_Bt[7],_Bt[8],_Bt[9],new T(function(){var _Bu=E(_Br)[1];if(_Bu>=0){var _Bv=E(_Bu);if(!_Bv){var _Bw=E(_AW);}else{var _Bw=[0,B(_y9(10,_Bv))];}var _Bx=_Bw;}else{var _Bx=E(_y0);}var _By=_Bx,_Bz=_By;return _Bz;}),_Bt[11]];})];};},_BA=[0,_Bl,_Bq,_Bp],_BB=[0,_8S,_BA],_BC=function(_BD,_){var _BE=B(_m7(_)),_BF=_BE;return [0,_cG,new T(function(){var _BG=E(_BD);return [0,_BG[1],_BG[2],_BG[3],_BF,_BG[5],_BG[6],_BG[7],_BG[8],_BG[9],_BG[10],_BG[11]];})];},_BH=new T(function(){return B(unCStr(" could be found!"));}),_BI=function(_BJ){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_O(_BJ,_BH));}))));});},_BK=function(_BL){return function(_BM,_){var _BN=E(_BL),_BO=jsFind(toJSStr(_BN)),_BP=_BO,_BQ=E(_BP);if(!_BQ[0]){return new F(function(){return _BI(_BN);});}else{var _BR=B(A(_wR,[_xO,_BQ[1],_xg,_xf,_BM,_])),_BS=_BR;return new F(function(){return A(new T(function(){return !B(_c7(_BL,_8V))?E(_xQ):E(_BC);}),[new T(function(){return E(E(_BS)[2]);}),_]);});}};},_BT=new T(function(){return B(_BK(_8V));}),_BU=function(_BV){return E(_BT);},_BW=function(_BX){return E(_yB);},_BY=new T(function(){return B(unCStr("\u3055\u3041\u59cb\u3081\u3088\u3046"));}),_BZ=new T(function(){return B(unCStr("\u3055\u3041\u59cb\u3081\u3088\u3046<br>\u30b2\u30fc\u30e0\u3092\u59cb\u3081\u307e\u3057\u3087\u3046\u3002\u53f3\u306e\u30dc\u30bf\u30f3\u304b\u3089\u3053\u306e\u30a2\u30a4\u30c6\u30e0\u3092\u8cfc\u5165\u3057\u3066\u304f\u3060\u3055\u3044\u3002"));}),_C0=new T(function(){return B(unCStr("fa-power-off"));}),_C1=[0,_C0,_BZ,_BY],_C2=[0,_BW,_BU,_C1],_C3=[0,_8V,_C2],_C4=[0,1],_C5=function(_C6){return E(_C4);},_C7=new T(function(){return B(unCStr("item-shop"));}),_C8=new T(function(){return B(_BK(_C7));}),_C9=function(_Ca){return E(_C8);},_Cb=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u30b7\u30e7\u30c3\u30d7"));}),_Cc=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u30b7\u30e7\u30c3\u30d7<br>\u30a2\u30a4\u30c6\u30e0\u304c\u8cfc\u5165\u3067\u304d\u308b\u3088\u3046\u306b\u306a\u308a\u307e\u3059\u3002"));}),_Cd=new T(function(){return B(unCStr("fa-shopping-cart"));}),_Ce=[0,_Cd,_Cc,_Cb],_Cf=[0,_C5,_C9,_Ce],_Cg=[0,_8Y,_Cf],_Ch=new T(function(){return B(unCStr("fa-moon-o"));}),_Ci=[0,22812],_Cj=[1,_Ci,_u],_Ck=new T(function(){return B(unCStr("\u591c<br>\u591c\u304c\u8a2a\u308c\u307e\u3059\u3002<br><strong>\u203b\u6ce8\u610f \u3053\u306e\u30a2\u30a4\u30c6\u30e0\u306e\u52b9\u679c\u306f\u4fdd\u5b58\u3055\u308c\u307e\u305b\u3093\u3002\u30d6\u30e9\u30a6\u30b6\u304c\u30ea\u30ed\u30fc\u30c9\u3055\u308c\u308b\u3068\u5143\u306b\u623b\u308a\u307e\u3059\u3002</strong>"));}),_Cl=[0,_Ch,_Ck,_Cj],_Cm=function(_Cn){return E(_yW);},_Co=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:369:3-8"));}),_Cp=new T(function(){return B(unCStr("Aichan"));}),_Cq=new T(function(){return [0,toJSStr(_u)];}),_Cr=[0,93],_Cs=[1,_Cr,_u],_Ct=new T(function(){return [0,toJSStr(_Cs)];}),_Cu=[0,125],_Cv=[1,_Cu,_u],_Cw=new T(function(){return [0,toJSStr(_Cv)];}),_Cx=[0,58],_Cy=[1,_Cx,_u],_Cz=new T(function(){return [0,toJSStr(_Cy)];}),_CA=[0,44],_CB=[1,_CA,_u],_CC=new T(function(){return [0,toJSStr(_CB)];}),_CD=new T(function(){return [0,"false"];}),_CE=function(_CF){var _CG=jsShow(E(_CF)[1]),_CH=_CG;return [0,_CH];},_CI=function(_CJ){var _CK=jsStringify(E(_CJ)[1]),_CL=_CK;return [0,_CL];},_CM=new T(function(){return [0,"null"];}),_CN=[0,91],_CO=[1,_CN,_u],_CP=new T(function(){return [0,toJSStr(_CO)];}),_CQ=[0,123],_CR=[1,_CQ,_u],_CS=new T(function(){return [0,toJSStr(_CR)];}),_CT=[0,34],_CU=[1,_CT,_u],_CV=new T(function(){return [0,toJSStr(_CU)];}),_CW=new T(function(){return [0,"true"];}),_CX=function(_CY,_CZ){var _D0=E(_CZ);switch(_D0[0]){case 0:return [0,new T(function(){return B(_CE(_D0[1]));}),_CY];case 1:return [0,new T(function(){return B(_CI(_D0[1]));}),_CY];case 2:return !E(_D0[1])?[0,_CD,_CY]:[0,_CW,_CY];case 3:var _D1=E(_D0[1]);return _D1[0]==0?[0,_CP,[1,_Ct,_CY]]:[0,_CP,new T(function(){var _D2=B(_CX(new T(function(){var _D3=function(_D4){var _D5=E(_D4);return _D5[0]==0?E([1,_Ct,_CY]):[1,_CC,new T(function(){var _D6=B(_CX(new T(function(){return B(_D3(_D5[2]));}),_D5[1]));return [1,_D6[1],_D6[2]];})];};return B(_D3(_D1[2]));}),_D1[1]));return [1,_D2[1],_D2[2]];})];case 4:var _D7=E(_D0[1]);if(!_D7[0]){return [0,_CS,[1,_Cw,_CY]];}else{var _D8=E(_D7[1]);return [0,_CS,[1,new T(function(){return B(_CI(_D8[1]));}),[1,_Cz,new T(function(){var _D9=B(_CX(new T(function(){var _Da=function(_Db){var _Dc=E(_Db);if(!_Dc[0]){return E([1,_Cw,_CY]);}else{var _Dd=E(_Dc[1]);return [1,_CC,[1,_CV,[1,_Dd[1],[1,_CV,[1,_Cz,new T(function(){var _De=B(_CX(new T(function(){return B(_Da(_Dc[2]));}),_Dd[2]));return [1,_De[1],_De[2]];})]]]]];}};return B(_Da(_D7[2]));}),_D8[2]));return [1,_D9[1],_D9[2]];})]]];}break;default:return [0,_CM,_CY];}},_Df=function(_Dg){var _Dh=jsCat(new T(function(){var _Di=B(_CX(_u,_Dg));return [1,_Di[1],_Di[2]];}),E(_Cq)[1]),_Dj=_Dh;return E(_Dj);},_Dk=function(_Dl){return new F(function(){return _o9(function(_){var _=0;return new F(function(){return eval(_Dl);});});});},_Dm=new T(function(){return [0,"(function(k,v) {localStorage.setItem(k,v);})"];}),_Dn=function(_Do,_Dp){return function(_Dq,_){var _Dr=B(A(new T(function(){return B(A(_Dk,[E(_Dm)[1],E(toJSStr(E(_Dp)))]));}),[E(B(_Df(B(A(new T(function(){return B(_2b(_Do));}),[_Dq]))))),_])),_Ds=_Dr;return _cG;};},_Dt=new T(function(){return B(_Dn(_qs,_Cp));}),_Du=new T(function(){return B(unCStr("game"));}),_Dv=new T(function(){return B(unCStr("\u5b9f\u7e3e\u7372\u5f97: "));}),_Dw=function(_Dx,_Dy,_){var _Dz=jsCreateTextNode(toJSStr(E(_Dx))),_DA=_Dz,_DB=jsAppendChild(_DA,E(_Dy)[1]);return [0,_DA];},_DC=new T(function(){return B(unCStr("Close"));}),_DD=new T(function(){return B(unCStr("sr-only"));}),_DE=new T(function(){return B(unCStr("innerHTML"));}),_DF=new T(function(){return B(unCStr("&times;"));}),_DG=function(_DH,_){var _DI=B(A(_wL,[_dk,_DH,_DE,_DF,_])),_DJ=_DI;return _DH;},_DK=new T(function(){return B(unCStr("true"));}),_DL=new T(function(){return B(unCStr("aria-hidden"));}),_DM=new T(function(){return B(unCStr("class"));}),_DN=new T(function(){return B(unCStr("span"));}),_DO=function(_DP,_DQ,_DR,_){var _DS=jsCreateElem(toJSStr(E(_DN))),_DT=_DS,_DU=jsAppendChild(_DT,E(_DR)[1]),_DV=[0,_DT],_DW=B(A(_DP,[_DQ,_DV,_])),_DX=_DW;return _DV;},_DY=function(_DZ){return E(_DZ);},_E0=function(_E1,_){var _E2=B(_DO(_DY,_DG,_E1,_)),_E3=_E2,_E4=B(A(_wF,[_dk,_E3,_DL,_DK,_])),_E5=_E4,_E6=B(_DO(_Dw,_DC,_E1,_)),_E7=_E6,_E8=B(A(_wF,[_dk,_E7,_DM,_DD,_])),_E9=_E8;return _E1;},_Ea=new T(function(){return B(unCStr("close"));}),_Eb=new T(function(){return B(unCStr("data-dismiss"));}),_Ec=new T(function(){return B(unCStr("button"));}),_Ed=new T(function(){return B(unCStr("type"));}),_Ee=new T(function(){return B(unCStr("alert"));}),_Ef=new T(function(){return B(unCStr("role"));}),_Eg=new T(function(){return B(unCStr("alert alert-info fade in tip"));}),_Eh=new T(function(){return B(unCStr("button"));}),_Ei=function(_Ej,_Ek,_El,_){var _Em=jsCreateElem(toJSStr(E(_Eh))),_En=_Em,_Eo=jsAppendChild(_En,E(_El)[1]),_Ep=[0,_En],_Eq=B(A(_Ej,[_Ek,_Ep,_])),_Er=_Eq;return _Ep;},_Es=new T(function(){return B(unCStr("div"));}),_Et=function(_Eu,_Ev,_Ew,_){var _Ex=jsCreateElem(toJSStr(E(_Es))),_Ey=_Ex,_Ez=jsAppendChild(_Ey,E(_Ew)[1]),_EA=[0,_Ey],_EB=B(A(_Eu,[_Ev,_EA,_])),_EC=_EB;return _EA;},_ED=new T(function(){return B(unCStr("id"));}),_EE=new T(function(){return B(unCStr("alerts"));}),_EF=new T(function(){return B(unCStr("\').alert(\'close\')"));}),_EG=[0,32],_EH=[1,_EG,_u],_EI=new T(function(){return B(unCStr("list-group-item"));}),_EJ=new T(function(){return B(_Dk("(function(e,c){var first = e.firstChild; e.insertBefore(c,first);})"));}),_EK=function(_EL){return function(_EM,_){var _EN=B(A(new T(function(){return B(A(_EJ,[E(E(_EL)[1])]));}),[E(E(_EM)[1]),_])),_EO=_EN;return _cG;};},_EP=new T(function(){return B(unCStr("unread-badge"));}),_EQ=new T(function(){return B(unCStr("<br>"));}),_ER=new T(function(){return B(unCStr("log-group"));}),_ES=new T(function(){return B(unCStr(") "));}),_ET=new T(function(){return B(unCStr("li"));}),_EU=new T(function(){return B(unCStr("var d = new Date(); d.getHours() + \':\' + d.getMinutes() + \':\' + d.getSeconds()"));}),_EV=function(_EW,_EX){while(1){var _EY=E(_EW);if(!_EY[0]){return E(_EX);}else{_EW=_EY[2];var _EZ=_EX+1|0;_EX=_EZ;continue;}}},_F0=function(_F1,_F2){while(1){var _F3=E(_F1);if(!_F3){return E(_F2);}else{var _F4=E(_F2);if(!_F4[0]){return [0];}else{_F1=_F3-1|0;_F2=_F4[2];continue;}}}},_F5=function(_F6,_F7,_F8){while(1){var _F9=E(_F7);if(!_F9[0]){return true;}else{var _Fa=E(_F8);if(!_Fa[0]){return false;}else{if(!B(A(_2D,[_F6,_F9[1],_Fa[1]]))){return false;}else{_F7=_F9[2];_F8=_Fa[2];continue;}}}}},_Fb=function(_Fc,_Fd,_Fe,_Ff){if(!B(_F5(_ci,_Fc,[1,_Fe,_Ff]))){return [1,_Fe,new T(function(){return B(_Fg(_Fc,_Fd,_Ff));})];}else{return new F(function(){return _O(_Fd,new T(function(){var _Fh=B(_EV(_Fc,0));if(_Fh>=0){var _Fi=B(_Fg(_Fc,_Fd,B(_F0(_Fh,[1,_Fe,_Ff]))));}else{var _Fi=B(_Fb(_Fc,_Fd,_Fe,_Ff));}var _Fj=_Fi,_Fk=_Fj;return _Fk;}));});}},_Fg=function(_Fl,_Fm,_Fn){var _Fo=E(_Fn);if(!_Fo[0]){return [0];}else{var _Fp=_Fo[1],_Fq=_Fo[2];if(!B(_F5(_ci,_Fl,_Fo))){return [1,_Fp,new T(function(){return B(_Fg(_Fl,_Fm,_Fq));})];}else{return new F(function(){return _O(_Fm,new T(function(){var _Fr=B(_EV(_Fl,0));if(_Fr>=0){var _Fs=B(_Fg(_Fl,_Fm,B(_F0(_Fr,_Fo))));}else{var _Fs=B(_Fb(_Fl,_Fm,_Fp,_Fq));}var _Ft=_Fs,_Fu=_Ft;return _Fu;}));});}}},_Fv=new T(function(){return B(unCStr("strong"));}),_Fw=function(_Fx,_Fy,_Fz,_){var _FA=jsCreateElem(toJSStr(E(_Fv))),_FB=_FA,_FC=jsAppendChild(_FB,E(_Fz)[1]),_FD=[0,_FB],_FE=B(A(_Fx,[_Fy,_FD,_])),_FF=_FE;return _FD;},_FG=function(_FH,_FI){return function(_FJ,_){var _FK=E(_ER),_FL=jsFind(toJSStr(_FK)),_FM=_FL,_FN=E(_FM);if(!_FN[0]){return new F(function(){return _BI(_FK);});}else{var _FO=jsEval(toJSStr(E(_EU))),_FP=_FO,_FQ=jsCreateElem(toJSStr(E(_ET))),_FR=_FQ,_FS=[0,_FR],_FT=B(A(_wF,[_dk,_FS,_DM,_EI,_])),_FU=_FT,_FV=B(_Fw(_Dw,new T(function(){return B(_O(_FH,new T(function(){return B(unAppCStr(" (",new T(function(){return B(_O(fromJSStr(_FP),_ES));})));})));}),_FS,_)),_FW=_FV,_FX=B(_DO(_DY,function(_FY,_){var _FZ=B(A(_wL,[_dk,_FY,_DE,new T(function(){return B(_Fg(_EQ,_EH,_FI));}),_])),_G0=_FZ;return _FY;},_FS,_)),_G1=_FX,_G2=B(A(_EK,[_FN[1],_FS,_])),_G3=_G2,_G4=E(_EP),_G5=jsFind(toJSStr(_G4)),_G6=_G5,_G7=E(_G6);if(!_G7[0]){return new F(function(){return _BI(_G4);});}else{var _G8=E(_FJ),_G9=E(_G8[11])[1]+1|0,_Ga=jsSet(E(_G7[1])[1],toJSStr(E(_xh)),toJSStr(B(_81(0,_G9,_u))));return [0,_cG,[0,_G8[1],_G8[2],_G8[3],_G8[4],_G8[5],_G8[6],_G8[7],_G8[8],_G8[9],_G8[10],[0,_G9]]];}}};},_Gb=function(_Gc,_Gd){return function(_Ge,_){var _Gf=B(_m7(_)),_Gg=_Gf,_Gh=E(_EE),_Gi=jsFind(toJSStr(_Gh)),_Gj=_Gi,_Gk=E(_Gj);if(!_Gk[0]){return new F(function(){return _BI(_Gh);});}else{var _Gl=_Gk[1],_Gm=B(A(_wL,[_dk,_Gl,_DE,_u,_])),_Gn=_Gm,_Go=B(_Et(_DY,function(_Gp,_){var _Gq=B(_Ei(_DY,_E0,_Gp,_)),_Gr=_Gq,_Gs=B(A(_wF,[_dk,_Gr,_Ed,_Ec,_])),_Gt=_Gs,_Gu=B(A(_wF,[_dk,_Gr,_DM,_Ea,_])),_Gv=_Gu,_Gw=B(A(_wF,[_dk,_Gr,_Eb,_Ee,_])),_Gx=_Gw,_Gy=B(_DO(_DY,function(_Gz,_){var _GA=B(A(_wL,[_dk,_Gz,_DE,_Gd,_])),_GB=_GA;return _Gz;},_Gp,_)),_GC=_Gy;return _Gp;},_Gl,_)),_GD=_Go,_GE=E(_GD),_GF=jsSetAttr(_GE[1],toJSStr(E(_ED)),toJSStr(B(unAppCStr("alert-",new T(function(){return B(_19(0,_Gg,_u));}))))),_GG=B(A(_wF,[_dk,_GE,_DM,_Eg,_])),_GH=_GG,_GI=B(A(_wF,[_dk,_GE,_Ef,_Ee,_])),_GJ=_GI,_GK=jsSetTimeout(5000,function(_){var _GL=jsEval(toJSStr(B(unAppCStr("$(\'#alert-",new T(function(){return B(_O(B(_19(0,_Gg,_u)),_EF));}))))),_GM=_GL;return _cG;});return new F(function(){return A(new T(function(){return B(_FG(_Gc,_Gd));}),[_Ge,_]);});}};},_GN=function(_GO,_GP){return function(_GQ,_){var _GR=B(A(new T(function(){return B(_Gb(_Du,new T(function(){return B(_O(_Dv,_GP));})));}),[new T(function(){var _GS=E(_GQ);return [0,_GS[1],_GS[2],_GS[3],_GS[4],_GS[5],new T(function(){return B(_6T(_GP,[1,_GO],_GS[6]));}),_GS[7],_GS[8],_GS[9],_GS[10],_GS[11]];}),_])),_GT=_GR,_GU=new T(function(){return E(E(_GT)[2]);}),_GV=B(A(_Dt,[_GU,_])),_GW=_GV,_GX=B(_GY(_GU,_)),_GZ=_GX;return new F(function(){return _H0(new T(function(){return E(E(_GZ)[2]);}),_);});};},_H1=new T(function(){return B(unCStr("\u3092\u8d85\u3048\u308b"));}),_H2=new T(function(){return B(unCStr("\u611b\u60c5\u304c"));}),_H3=function(_H4,_H5){return function(_H6,_){var _H7=E(_H6);return E(_H7[1])[1]<=E(new T(function(){return [0,B(_xb(_H4))];}))[1]?[0,_cG,_H7]:B(A(new T(function(){return B(_GN(new T(function(){return B(_O(_H2,new T(function(){return B(_O(B(_19(0,_H4,_u)),_H1));})));}),_H5));}),[_H7,_]));};},_H8=new T(function(){return B(unCStr("\u30a2\u30a4\u3068\u306e\u906d\u9047"));}),_H9=[0,100],_Ha=new T(function(){return B(_H3(_H9,_H8));}),_Hb=new T(function(){return [0,_H8,_Ha];}),_Hc=new T(function(){return B(unCStr("\u611b\u3055\u308c\u6c17\u5206"));}),_Hd=[0,10000],_He=new T(function(){return B(_H3(_Hd,_Hc));}),_Hf=new T(function(){return [0,_Hc,_He];}),_Hg=new T(function(){return B(unCStr("\u611b\u30e9\u30d6\u30e6\u30fc"));}),_Hh=[0,1000000],_Hi=new T(function(){return B(_H3(_Hh,_Hg));}),_Hj=new T(function(){return [0,_Hg,_Hi];}),_Hk=new T(function(){return B(unCStr("\u611b\u3055\u3093\u306e\u611b\u304c\u91cd\u3044"));}),_Hl=[0,100000000],_Hm=new T(function(){return B(_H3(_Hl,_Hk));}),_Hn=new T(function(){return [0,_Hk,_Hm];}),_Ho=[1,I_fromBits([1215752192,23])],_Hp=new T(function(){return B(unCStr("\u611b\u3061\u3083\u3093\u30de\u30b9\u30bf\u30fc"));}),_Hq=new T(function(){return B(_H3(_Ho,_Hp));}),_Hr=new T(function(){return [0,_Hp,_Hq];}),_Hs=new T(function(){return B(unCStr("\u597d\u611f\u5ea6\u304c"));}),_Ht=function(_Hu,_Hv){return function(_Hw,_){var _Hx=E(_Hw);return E(_Hx[2])[1]<=E(new T(function(){return [0,B(_xb(_Hu))];}))[1]?[0,_cG,_Hx]:B(A(new T(function(){return B(_GN(new T(function(){return B(_O(_Hs,new T(function(){return B(_O(B(_19(0,_Hu,_u)),_H1));})));}),_Hv));}),[_Hx,_]));};},_Hy=new T(function(){return B(unCStr("\u307e\u305a\u306f\u304a\u53cb\u9054\u304b\u3089"));}),_Hz=[0,10],_HA=new T(function(){return B(_Ht(_Hz,_Hy));}),_HB=new T(function(){return [0,_Hy,_HA];}),_HC=new T(function(){return B(unCStr("\u4e00\u7d1a\u30d5\u30e9\u30b0\u5efa\u7bc9\u58eb"));}),_HD=new T(function(){return B(_Ht(_H9,_HC));}),_HE=new T(function(){return [0,_HC,_HD];}),_HF=[0,1000],_HG=new T(function(){return B(unCStr("\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_HH=new T(function(){return B(_Ht(_HF,_HG));}),_HI=new T(function(){return [0,_HG,_HH];}),_HJ=new T(function(){return B(unCStr("\uff8a\uff72\uff8a\uff9f\uff70\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_HK=new T(function(){return B(_Ht(_Hd,_HJ));}),_HL=new T(function(){return [0,_HJ,_HK];}),_HM=[0,100000],_HN=new T(function(){return B(unCStr("\u5168\u958b\u3089\u3076\u3071\u308f\u30fc"));}),_HO=new T(function(){return B(_Ht(_HM,_HN));}),_HP=new T(function(){return [0,_HN,_HO];}),_HQ=new T(function(){return B(unCStr("\u4f9d\u5b58\u5ea6\u304c"));}),_HR=function(_HS,_HT){return function(_HU,_){var _HV=E(_HU);return E(_HV[3])[1]<=E(new T(function(){return [0,B(_xb(_HS))];}))[1]?[0,_cG,_HV]:B(A(new T(function(){return B(_GN(new T(function(){return B(_O(_HQ,new T(function(){return B(_O(B(_19(0,_HS,_u)),_H1));})));}),_HT));}),[_HV,_]));};},_HW=new T(function(){return B(unCStr("\u4f9d\u5b58\u6ce8\u610f\u5831"));}),_HX=new T(function(){return B(_HR(_H9,_HW));}),_HY=new T(function(){return [0,_HW,_HX];}),_HZ=new T(function(){return B(unCStr("\u4f9d\u5b58\u30c9\u30e9\u30c3\u30b0"));}),_I0=new T(function(){return B(_HR(_Hd,_HZ));}),_I1=new T(function(){return [0,_HZ,_I0];}),_I2=new T(function(){return B(unCStr("\u500b\u4ee5\u4e0a\u624b\u306b\u5165\u308c\u308b"));}),_I3=new T(function(){return B(unCStr("\u300d\u3092"));}),_I4=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u300c"));}),_I5=new T(function(){return B(_7L(_I6));}),_I7=function(_I8,_I9,_Ia){return function(_Ib,_){var _Ic=E(_Ib),_Id=_Ic[7];return !B(_v9(_I8,_Id))?[0,_cG,_Ic]:B(_uT(_I8,_Id))[1]<E(_I9)[1]?[0,_cG,_Ic]:B(A(new T(function(){return B(_GN(new T(function(){return B(_O(_I4,new T(function(){return B(_O(E(B(_uT(_I8,_I5))[3])[3],new T(function(){return B(_O(_I3,new T(function(){return B(_O(B(_81(0,E(_I9)[1],_u)),_I2));})));})));})));}),_Ia));}),[_Ic,_]));};},_Ie=new T(function(){return B(unCStr("\u304a\u3057\u3083\u3079\u308a\u611b\u3061\u3083\u3093"));}),_If=new T(function(){return B(_I7(_9Q,_x3,_Ie));}),_Ig=new T(function(){return [0,_Ie,_If];}),_Ih=new T(function(){return B(unCStr("\u3042\u3001\u3046\u3093"));}),_Ii=[0,200],_Ij=new T(function(){return B(_I7(_9Q,_Ii,_Ih));}),_Ik=new T(function(){return [0,_Ih,_Ij];}),_Il=new T(function(){return B(unCStr("\u55ab\u8336\u5e97\u306e\u30dd\u30a4\u30f3\u30c8\u30ab\u30fc\u30c9"));}),_Im=new T(function(){return B(_I7(_9I,_x1,_Il));}),_In=new T(function(){return [0,_Il,_Im];}),_Io=new T(function(){return B(unCStr("\u611b\u3068\u3044\u3046\u540d\u306e\u30d7\u30ec\u30bc\u30f3\u30c8"));}),_Ip=new T(function(){return B(_I7(_9E,_x1,_Io));}),_Iq=new T(function(){return [0,_Io,_Ip];}),_Ir=function(_Is,_It){while(1){var _Iu=E(_It);if(!_Iu[0]){return true;}else{if(!B(A(_Is,[_Iu[1]]))){return false;}else{_It=_Iu[2];continue;}}}},_Iv=new T(function(){return B(unCStr("\u5168\u3066\u306e\u901a\u5e38\u30a2\u30a4\u30c6\u30e0\u3092"));}),_Iw=function(_Ix,_Iy){return function(_Iz,_){var _IA=new T(function(){return E(E(_Iz)[7]);});return !B(_Ir(function(_IB){return !B(_v9(_IB,_IA))?false:B(_uT(_IB,_IA))[1]>=E(_Ix)[1];},_IC))?[0,_cG,_Iz]:B(A(new T(function(){return B(_GN(new T(function(){return B(_O(_Iv,new T(function(){return B(_O(B(_81(0,E(_Ix)[1],_u)),_I2));})));}),_Iy));}),[_Iz,_]));};},_ID=new T(function(){return B(unCStr("\u30b3\u30f3\u30d7\u30ea\u30fc\u30c8"));}),_IE=new T(function(){return B(_Iw(_9R,_ID));}),_IF=new T(function(){return [0,_ID,_IE];}),_IG=new T(function(){return B(unCStr("\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30f3\u30d7\u30ea\u30fc\u30c8"));}),_IH=new T(function(){return B(_Iw(_wZ,_IG));}),_II=new T(function(){return [0,_IG,_IH];}),_IJ=new T(function(){return B(unCStr("\u9577\u3044\u9577\u3044\u30ed\u30b0"));}),_IK=new T(function(){return B(unCStr("\u30ed\u30b0\u306e\u884c\u6570\u304c"));}),_IL=new T(function(){return B(_81(0,100,_u));}),_IM=new T(function(){return B(_O(_IL,_H1));}),_IN=new T(function(){return B(_O(_IK,_IM));}),_IO=new T(function(){return B(_GN(_IN,_IJ));}),_IP=function(_IQ,_){var _IR=E(_IQ);return E(_IR[11])[1]<100?[0,_cG,_IR]:B(A(_IO,[_IR,_]));},_IS=new T(function(){return [0,_IJ,_IP];}),_IT=new T(function(){return [1,_IS,_u];}),_IU=new T(function(){return [1,_II,_IT];}),_IV=new T(function(){return [1,_IF,_IU];}),_IW=new T(function(){return [1,_Iq,_IV];}),_IX=new T(function(){return [1,_In,_IW];}),_IY=new T(function(){return [1,_Ik,_IX];}),_IZ=new T(function(){return [1,_Ig,_IY];}),_J0=new T(function(){return [1,_I1,_IZ];}),_J1=new T(function(){return [1,_HY,_J0];}),_J2=new T(function(){return [1,_HP,_J1];}),_J3=new T(function(){return [1,_HL,_J2];}),_J4=new T(function(){return [1,_HI,_J3];}),_J5=new T(function(){return [1,_HE,_J4];}),_J6=new T(function(){return [1,_HB,_J5];}),_J7=new T(function(){return [1,_Hr,_J6];}),_J8=new T(function(){return [1,_Hn,_J7];}),_J9=new T(function(){return [1,_Hj,_J8];}),_Ja=new T(function(){return [1,_Hf,_J9];}),_Jb=new T(function(){return [1,_Hb,_Ja];}),_Jc=new T(function(){return B(unCStr("<tr><td>%s</td><td>%s</td></tr>"));}),_Jd=new T(function(){return B(unCStr("<thead><tr><th>\u5b9f\u7e3e\u540d</th><th>\u5185\u5bb9</th></tr></thead>"));}),_Je=new T(function(){return B(unCStr("</tbody>"));}),_Jf=function(_Jg,_Jh){while(1){var _Ji=E(_Jg);if(!_Ji[0]){return E(_Jh);}else{_Jg=_Ji[2];var _Jj=[1,_Ji[1],_Jh];_Jh=_Jj;continue;}}},_Jk=function(_Jl){var _Jm=E(_Jl)[1];return [0,Math.log(_Jm+(_Jm+1)*Math.sqrt((_Jm-1)/(_Jm+1)))];},_Jn=function(_Jo){var _Jp=E(_Jo)[1];return [0,Math.log(_Jp+Math.sqrt(1+_Jp*_Jp))];},_Jq=function(_Jr){var _Js=E(_Jr)[1];return [0,0.5*Math.log((1+_Js)/(1-_Js))];},_Jt=function(_Ju,_Jv){return [0,Math.log(E(_Jv)[1])/Math.log(E(_Ju)[1])];},_Jw=[0,3.141592653589793],_Jx=function(_Jy){var _Jz=E(_Jy);return new F(function(){return _yT(_Jz[1],_Jz[2]);});},_JA=function(_JB){return [0,1/E(_JB)[1]];},_JC=function(_JD){var _JE=E(_JD),_JF=_JE[1];return _JF<0?[0, -_JF]:E(_JE);},_JG=function(_JH){return [0,B(_xb(_JH))];},_JI=[0,0],_JJ=[0,1],_JK=[0,-1],_JL=function(_JM){var _JN=E(E(_JM)[1]);return _JN==0?E(_JI):_JN<=0?E(_JK):E(_JJ);},_JO=function(_JP,_JQ){return [0,E(_JP)[1]-E(_JQ)[1]];},_JR=function(_JS){return [0, -E(_JS)[1]];},_JT=function(_JU,_JV){return [0,E(_JU)[1]+E(_JV)[1]];},_JW=function(_JX,_JY){return [0,E(_JX)[1]*E(_JY)[1]];},_JZ=[0,_JT,_JW,_JO,_JR,_JC,_JL,_JG],_K0=function(_K1,_K2){return [0,E(_K1)[1]/E(_K2)[1]];},_K3=[0,_JZ,_K0,_JA,_Jx],_K4=function(_K5){return [0,Math.acos(E(_K5)[1])];},_K6=function(_K7){return [0,Math.asin(E(_K7)[1])];},_K8=function(_K9){return [0,Math.atan(E(_K9)[1])];},_Ka=function(_Kb){return [0,Math.cos(E(_Kb)[1])];},_Kc=function(_Kd){return [0,cosh(E(_Kd)[1])];},_Ke=function(_Kf){return [0,Math.exp(E(_Kf)[1])];},_Kg=function(_Kh){return [0,Math.log(E(_Kh)[1])];},_Ki=function(_Kj,_Kk){return [0,Math.pow(E(_Kj)[1],E(_Kk)[1])];},_Kl=function(_Km){return [0,Math.sin(E(_Km)[1])];},_Kn=function(_Ko){return [0,sinh(E(_Ko)[1])];},_Kp=function(_Kq){return [0,Math.sqrt(E(_Kq)[1])];},_Kr=function(_Ks){return [0,Math.tan(E(_Ks)[1])];},_Kt=function(_Ku){return [0,tanh(E(_Ku)[1])];},_Kv=[0,_K3,_Jw,_Ke,_Kp,_Kg,_Ki,_Jt,_Kl,_Kr,_Ka,_K6,_K8,_K4,_Kn,_Kt,_Kc,_Jn,_Jq,_Jk],_Kw=function(_Kx){var _Ky=E(_Kx)[1];return [0,Math.log(_Ky+(_Ky+1)*Math.sqrt((_Ky-1)/(_Ky+1)))];},_Kz=function(_KA){var _KB=E(_KA)[1];return [0,Math.log(_KB+Math.sqrt(1+_KB*_KB))];},_KC=function(_KD){var _KE=E(_KD)[1];return [0,0.5*Math.log((1+_KE)/(1-_KE))];},_KF=function(_KG,_KH){return [0,Math.log(E(_KH)[1])/Math.log(E(_KG)[1])];},_KI=[0,3.141592653589793],_KJ=new T(function(){return [0,0/0];}),_KK=new T(function(){return [0,-1/0];}),_KL=new T(function(){return [0,1/0];}),_KM=function(_KN,_KO){return !B(_yL(_KO,_yF))?[0,B(_yG(_KN,_KO))]:!B(_yL(_KN,_yF))?!B(_Y(_KN,_yF))?E(_KL):E(_KK):E(_KJ);},_KP=function(_KQ){var _KR=E(_KQ);return new F(function(){return _KM(_KR[1],_KR[2]);});},_KS=function(_KT){return [0,1/E(_KT)[1]];},_KU=function(_KV){var _KW=E(_KV),_KX=_KW[1];return _KX<0?[0, -_KX]:E(_KW);},_KY=function(_KZ){var _L0=E(_KZ);return _L0[0]==0?_L0[1]:I_toNumber(_L0[1]);},_L1=function(_L2){return [0,B(_KY(_L2))];},_L3=[0,0],_L4=[0,1],_L5=[0,-1],_L6=function(_L7){var _L8=E(E(_L7)[1]);return _L8==0?E(_L3):_L8<=0?E(_L5):E(_L4);},_L9=function(_La,_Lb){return [0,E(_La)[1]-E(_Lb)[1]];},_Lc=function(_Ld){return [0, -E(_Ld)[1]];},_Le=function(_Lf,_Lg){return [0,E(_Lf)[1]+E(_Lg)[1]];},_Lh=function(_Li,_Lj){return [0,E(_Li)[1]*E(_Lj)[1]];},_Lk=[0,_Le,_Lh,_L9,_Lc,_KU,_L6,_L1],_Ll=function(_Lm,_Ln){return [0,E(_Lm)[1]/E(_Ln)[1]];},_Lo=[0,_Lk,_Ll,_KS,_KP],_Lp=function(_Lq){return [0,Math.acos(E(_Lq)[1])];},_Lr=function(_Ls){return [0,Math.asin(E(_Ls)[1])];},_Lt=function(_Lu){return [0,Math.atan(E(_Lu)[1])];},_Lv=function(_Lw){return [0,Math.cos(E(_Lw)[1])];},_Lx=function(_Ly){return [0,cosh(E(_Ly)[1])];},_Lz=function(_LA){return [0,Math.exp(E(_LA)[1])];},_LB=function(_LC){return [0,Math.log(E(_LC)[1])];},_LD=function(_LE,_LF){return [0,Math.pow(E(_LE)[1],E(_LF)[1])];},_LG=function(_LH){return [0,Math.sin(E(_LH)[1])];},_LI=function(_LJ){return [0,sinh(E(_LJ)[1])];},_LK=function(_LL){return [0,Math.sqrt(E(_LL)[1])];},_LM=function(_LN){return [0,Math.tan(E(_LN)[1])];},_LO=function(_LP){return [0,tanh(E(_LP)[1])];},_LQ=[0,_Lo,_KI,_Lz,_LK,_LB,_LD,_KF,_LG,_LM,_Lv,_Lr,_Lt,_Lp,_LI,_LO,_Lx,_Kz,_KC,_Kw],_LR=function(_LS){var _LT=B(_yf(E(_LS)[1]));return [0,_LT[1],[0,_LT[2]]];},_LU=[0,53],_LV=function(_LW){return E(_LU);},_LX=[0,2],_LY=function(_LZ){return E(_LX);},_M0=[0,1024],_M1=[0,-1021],_M2=[0,_M1,_M0],_M3=function(_M4){return E(_M2);},_M5=function(_M6){var _M7=isDoubleInfinite(E(_M6)[1]),_M8=_M7;return E(_M8)==0?false:true;},_M9=function(_Ma){var _Mb=isDoubleNaN(E(_Ma)[1]),_Mc=_Mb;return E(_Mc)==0?false:true;},_Md=function(_Me){var _Mf=isDoubleNegativeZero(E(_Me)[1]),_Mg=_Mf;return E(_Mg)==0?false:true;},_Mh=function(_Mi){var _Mj=decodeFloat(E(_Mi)[1]);return [0,new T(function(){return B(_ea(_Mj[1]));}),[0,_Mj[2]]];},_Mk=[0,24],_Ml=function(_Mm){return E(_Mk);},_Mn=function(_Mo){return E(_LX);},_Mp=[0,128],_Mq=[0,-125],_Mr=[0,_Mq,_Mp],_Ms=function(_Mt){return E(_Mr);},_Mu=function(_Mv){var _Mw=isFloatInfinite(E(_Mv)[1]),_Mx=_Mw;return E(_Mx)==0?false:true;},_My=function(_Mz){var _MA=isFloatNaN(E(_Mz)[1]),_MB=_MA;return E(_MB)==0?false:true;},_MC=function(_MD){var _ME=isFloatNegativeZero(E(_MD)[1]),_MF=_ME;return E(_MF)==0?false:true;},_MG=function(_MH,_MI){return E(_MH)[1]!=E(_MI)[1]?true:false;},_MJ=function(_MK,_ML){return E(_MK)[1]==E(_ML)[1];},_MM=[0,_MJ,_MG],_MN=function(_MO,_MP){return E(_MO)[1]<E(_MP)[1];},_MQ=function(_MR,_MS){return E(_MR)[1]<=E(_MS)[1];},_MT=function(_MU,_MV){return E(_MU)[1]>E(_MV)[1];},_MW=function(_MX,_MY){return E(_MX)[1]>=E(_MY)[1];},_MZ=function(_N0,_N1){var _N2=E(_N0)[1],_N3=E(_N1)[1];return _N2>=_N3?_N2!=_N3?2:1:0;},_N4=function(_N5,_N6){var _N7=E(_N5),_N8=E(_N6);return _N7[1]>_N8[1]?E(_N7):E(_N8);},_N9=function(_Na,_Nb){var _Nc=E(_Na),_Nd=E(_Nb);return _Nc[1]>_Nd[1]?E(_Nd):E(_Nc);},_Ne=[0,_MM,_MZ,_MN,_MW,_MT,_MQ,_N4,_N9],_Nf=[0,1],_Ng=new T(function(){var _Nh=newByteArr(256),_Ni=_Nh,_=_Ni["v"]["i8"][0]=8,_=B((function(_Nj,_Nk,_Nl,_){while(1){if(_Nl>=256){if(_Nj>=256){return E(_);}else{var _Nm=imul(2,_Nj)|0,_Nn=_Nk+1|0,_No=_Nj;_Nj=_Nm;_Nk=_Nn;_Nl=_No;continue;}}else{var _=_Ni["v"]["i8"][_Nl]=_Nk,_No=_Nl+_Nj|0;_Nl=_No;continue;}}})(2,0,1,_)),_Np=_Ni,_Nq=_Np;return [0,_Nq];}),_Nr=function(_Ns,_Nt){while(1){var _Nu=(function(_Nv,_Nw){var _Nx=hs_int64ToInt(_Nv),_Ny=_Nx,_Nz=E(_Ng)[1]["v"]["i8"][(255&_Ny>>>0)>>>0&4294967295];if(_Nw>_Nz){if(_Nz>=8){var _NA=hs_uncheckedIShiftRA64(_Nv,8),_NB=_NA;_Ns=_NB;var _NC=_Nw-8|0;_Nt=_NC;return null;}else{return [0,new T(function(){var _ND=hs_uncheckedIShiftRA64(_Nv,_Nz),_NE=_ND;return B(_yi(_NE));}),_Nw-_Nz|0];}}else{return [0,new T(function(){var _NF=hs_uncheckedIShiftRA64(_Nv,_Nw),_NG=_NF;return B(_yi(_NG));}),0];}})(_Ns,_Nt);if(_Nu!=null){return _Nu;}}},_NH=function(_NI){return I_toInt(_NI)>>>0;},_NJ=function(_NK){var _NL=E(_NK);return _NL[0]==0?_NL[1]>>>0:B(_NH(_NL[1]));},_NM=function(_NN){var _NO=B(_yf(_NN)),_NP=_NO[1],_NQ=_NO[2];if(_NQ<0){var _NR=function(_NS){if(!_NS){return [0,E(_NP),B(_z0(_Nf, -_NQ))];}else{var _NT=B(_Nr(B(_yy(_NP)), -_NQ));return [0,E(_NT[1]),B(_z0(_Nf,_NT[2]))];}};return (B(_NJ(_NP))&1)>>>0==0?B(_NR(1)):B(_NR(0));}else{return [0,B(_z0(_NP,_NQ)),_Nf];}},_NU=function(_NV){var _NW=B(_NM(E(_NV)[1]));return [0,E(_NW[1]),E(_NW[2])];},_NX=[0,_JZ,_Ne,_NU],_NY=function(_NZ){return E(E(_NZ)[1]);},_O0=[0,1],_O1=function(_O2,_O3){if(_O2<=_O3){var _O4=function(_O5){return [1,[0,_O5],new T(function(){if(_O5!=_O3){var _O6=B(_O4(_O5+1|0));}else{var _O6=[0];}var _O7=_O6;return _O7;})];};return new F(function(){return _O4(_O2);});}else{return [0];}},_O8=function(_O9){return new F(function(){return _O1(E(_O9)[1],2147483647);});},_Oa=function(_Ob,_Oc,_Od){return _Od<=_Oc?[1,[0,_Ob],new T(function(){var _Oe=_Oc-_Ob|0,_Of=function(_Og){return _Og>=(_Od-_Oe|0)?[1,[0,_Og],new T(function(){return B(_Of(_Og+_Oe|0));})]:[1,[0,_Og],_u];};return B(_Of(_Oc));})]:_Od<=_Ob?[1,[0,_Ob],_u]:[0];},_Oh=function(_Oi,_Oj,_Ok){return _Ok>=_Oj?[1,[0,_Oi],new T(function(){var _Ol=_Oj-_Oi|0,_Om=function(_On){return _On<=(_Ok-_Ol|0)?[1,[0,_On],new T(function(){return B(_Om(_On+_Ol|0));})]:[1,[0,_On],_u];};return B(_Om(_Oj));})]:_Ok>=_Oi?[1,[0,_Oi],_u]:[0];},_Oo=function(_Op,_Oq){return _Oq<_Op?B(_Oa(_Op,_Oq,-2147483648)):B(_Oh(_Op,_Oq,2147483647));},_Or=function(_Os,_Ot){return new F(function(){return _Oo(E(_Os)[1],E(_Ot)[1]);});},_Ou=function(_Ov,_Ow,_Ox){return _Ow<_Ov?B(_Oa(_Ov,_Ow,_Ox)):B(_Oh(_Ov,_Ow,_Ox));},_Oy=function(_Oz,_OA,_OB){return new F(function(){return _Ou(E(_Oz)[1],E(_OA)[1],E(_OB)[1]);});},_OC=function(_OD,_OE){return new F(function(){return _O1(E(_OD)[1],E(_OE)[1]);});},_OF=function(_OG){return E(_OG);},_OH=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_OI=new T(function(){return B(err(_OH));}),_OJ=function(_OK){var _OL=E(E(_OK)[1]);return _OL==(-2147483648)?E(_OI):[0,_OL-1|0];},_OM=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_ON=new T(function(){return B(err(_OM));}),_OO=function(_OP){var _OQ=E(E(_OP)[1]);return _OQ==2147483647?E(_ON):[0,_OQ+1|0];},_OR=[0,_OO,_OJ,_OF,_OF,_O8,_Or,_OC,_Oy],_OS=function(_OT,_OU){return [0,B(_w2(E(_OT)[1],E(_OU)[1]))];},_OV=[0,0],_OW=[0,_w1,_OV],_OX=function(_OY,_OZ){var _P0=E(_OY)[1],_P1=E(E(_OZ)[1]);switch(_P1){case -1:var _P2=E(_P0);if(_P2==(-2147483648)){return E(_OW);}else{if(_P2<=0){if(_P2>=0){var _P3=quotRemI(_P2,-1);return [0,[0,_P3[1]],[0,_P3[2]]];}else{var _P4=quotRemI(_P2,-1);return [0,[0,_P4[1]],[0,_P4[2]]];}}else{var _P5=quotRemI(_P2-1|0,-1);return [0,[0,_P5[1]-1|0],[0,(_P5[2]+(-1)|0)+1|0]];}}break;case 0:return E(_vY);default:if(_P0<=0){if(_P0>=0){var _P6=quotRemI(_P0,_P1);return [0,[0,_P6[1]],[0,_P6[2]]];}else{if(_P1<=0){var _P7=quotRemI(_P0,_P1);return [0,[0,_P7[1]],[0,_P7[2]]];}else{var _P8=quotRemI(_P0+1|0,_P1);return [0,[0,_P8[1]-1|0],[0,(_P8[2]+_P1|0)-1|0]];}}}else{if(_P1>=0){if(_P0>=0){var _P9=quotRemI(_P0,_P1);return [0,[0,_P9[1]],[0,_P9[2]]];}else{if(_P1<=0){var _Pa=quotRemI(_P0,_P1);return [0,[0,_Pa[1]],[0,_Pa[2]]];}else{var _Pb=quotRemI(_P0+1|0,_P1);return [0,[0,_Pb[1]-1|0],[0,(_Pb[2]+_P1|0)-1|0]];}}}else{var _Pc=quotRemI(_P0-1|0,_P1);return [0,[0,_Pc[1]-1|0],[0,(_Pc[2]+_P1|0)+1|0]];}}}},_Pd=function(_Pe,_Pf){var _Pg=E(E(_Pf)[1]);switch(_Pg){case -1:return E(_OV);case 0:return E(_vY);default:return [0,B(_w7(E(_Pe)[1],_Pg))];}},_Ph=function(_Pi,_Pj){var _Pk=E(_Pi)[1],_Pl=E(E(_Pj)[1]);switch(_Pl){case -1:var _Pm=E(_Pk);return _Pm==(-2147483648)?E(_w1):[0,quot(_Pm,-1)];case 0:return E(_vY);default:return [0,quot(_Pk,_Pl)];}},_Pn=function(_Po,_Pp){var _Pq=E(_Po)[1],_Pr=E(E(_Pp)[1]);switch(_Pr){case -1:var _Ps=E(_Pq);if(_Ps==(-2147483648)){return E(_OW);}else{var _Pt=quotRemI(_Ps,-1);return [0,[0,_Pt[1]],[0,_Pt[2]]];}break;case 0:return E(_vY);default:var _Pu=quotRemI(_Pq,_Pr);return [0,[0,_Pu[1]],[0,_Pu[2]]];}},_Pv=function(_Pw,_Px){var _Py=E(E(_Px)[1]);switch(_Py){case -1:return E(_OV);case 0:return E(_vY);default:return [0,E(_Pw)[1]%_Py];}},_Pz=function(_PA){return new F(function(){return _ea(E(_PA)[1]);});},_PB=function(_PC){return [0,E(B(_ea(E(_PC)[1]))),E(_O0)];},_PD=function(_PE,_PF){return [0,imul(E(_PE)[1],E(_PF)[1])|0];},_PG=function(_PH,_PI){return [0,E(_PH)[1]+E(_PI)[1]|0];},_PJ=function(_PK,_PL){return [0,E(_PK)[1]-E(_PL)[1]|0];},_PM=function(_PN){var _PO=E(_PN),_PP=_PO[1];return _PP<0?[0, -_PP]:E(_PO);},_PQ=function(_PR){return [0,B(_fn(_PR))];},_PS=function(_PT){return [0, -E(_PT)[1]];},_PU=[0,-1],_PV=[0,0],_PW=[0,1],_PX=function(_PY){var _PZ=E(_PY)[1];return _PZ>=0?E(_PZ)==0?E(_PV):E(_PW):E(_PU);},_Q0=[0,_PG,_PD,_PJ,_PS,_PM,_PX,_PQ],_Q1=function(_Q2,_Q3){return E(_Q2)[1]==E(_Q3)[1];},_Q4=function(_Q5,_Q6){return E(_Q5)[1]!=E(_Q6)[1];},_Q7=[0,_Q1,_Q4],_Q8=function(_Q9,_Qa){var _Qb=E(_Q9),_Qc=E(_Qa);return _Qb[1]>_Qc[1]?E(_Qb):E(_Qc);},_Qd=function(_Qe,_Qf){var _Qg=E(_Qe),_Qh=E(_Qf);return _Qg[1]>_Qh[1]?E(_Qh):E(_Qg);},_Qi=function(_Qj,_Qk){return _Qj>=_Qk?_Qj!=_Qk?2:1:0;},_Ql=function(_Qm,_Qn){return new F(function(){return _Qi(E(_Qm)[1],E(_Qn)[1]);});},_Qo=function(_Qp,_Qq){return E(_Qp)[1]>=E(_Qq)[1];},_Qr=function(_Qs,_Qt){return E(_Qs)[1]>E(_Qt)[1];},_Qu=function(_Qv,_Qw){return E(_Qv)[1]<=E(_Qw)[1];},_Qx=function(_Qy,_Qz){return E(_Qy)[1]<E(_Qz)[1];},_QA=[0,_Q7,_Ql,_Qx,_Qo,_Qr,_Qu,_Q8,_Qd],_QB=[0,_Q0,_QA,_PB],_QC=[0,_QB,_OR,_Ph,_Pv,_OS,_Pd,_Pn,_OX,_Pz],_QD=function(_QE){return E(E(_QE)[1]);},_QF=function(_QG,_QH,_QI){while(1){if(!(_QH%2)){var _QJ=B(_ec(_QG,_QG)),_QK=quot(_QH,2);_QG=_QJ;_QH=_QK;continue;}else{var _QL=E(_QH);if(_QL==1){return new F(function(){return _ec(_QG,_QI);});}else{var _QJ=B(_ec(_QG,_QG));_QH=quot(_QL-1|0,2);var _QM=B(_ec(_QG,_QI));_QG=_QJ;_QI=_QM;continue;}}}},_QN=function(_QO,_QP){while(1){if(!(_QP%2)){var _QQ=B(_ec(_QO,_QO)),_QR=quot(_QP,2);_QO=_QQ;_QP=_QR;continue;}else{var _QS=E(_QP);if(_QS==1){return E(_QO);}else{return new F(function(){return _QF(B(_ec(_QO,_QO)),quot(_QS-1|0,2),_QO);});}}}},_QT=function(_QU){return E(E(_QU)[2]);},_QV=function(_QW){return E(E(_QW)[1]);},_QX=function(_QY){return E(E(_QY)[2]);},_QZ=[0,0],_R0=[0,2],_R1=function(_R2){return E(E(_R2)[7]);},_R3=function(_R4,_R5,_R6,_R7,_R8){return new F(function(){return A(E(E(_R5)[1])[1],[new T(function(){return B(A(_R7,[_R8,new T(function(){return B(A(_R1,[_R4,_R0]));})]));}),new T(function(){return B(A(_R1,[_R4,_QZ]));})]);});},_R9=function(_Ra){return E(E(_Ra)[3]);},_Rb=new T(function(){return B(unCStr("Negative exponent"));}),_Rc=new T(function(){return B(err(_Rb));}),_Rd=function(_Re,_Rf,_Rg,_Rh){var _Ri=B(_NY(_Rf)),_Rj=_Ri[1],_Rk=E(_Ri[2]);if(!B(A(_Rk[3],[_Rh,new T(function(){return B(A(_R1,[_Rj,_QZ]));})]))){if(!B(A(E(_Rk[1])[1],[_Rh,new T(function(){return B(A(_R1,[_Rj,_QZ]));})]))){var _Rl=B(_NY(_Rf)),_Rm=_Rl[1],_Rn=new T(function(){return B(_NY(_Rf));}),_Ro=new T(function(){return B(_QD(_Rn));});return new F(function(){return (function(_Rp,_Rq){while(1){var _Rr=(function(_Rs,_Rt){var _Ru=E(_Rf),_Rv=_Ru[3],_Rw=E(_Ru[1]);if(!B(_R3(_Rw[1],_Rw[2],_Rw[3],_Ru[4],_Rt))){return !B(A(E(E(_Rl[2])[1])[1],[_Rt,new T(function(){return B(A(_R1,[_Rm,_O0]));})]))?B((function(_Rx,_Ry,_Rz){while(1){var _RA=(function(_RB,_RC,_RD){var _RE=E(_Rf),_RF=_RE[3],_RG=E(_RE[1]);if(!B(_R3(_RG[1],_RG[2],_RG[3],_RE[4],_RC))){if(!B(A(new T(function(){return B(_2D(new T(function(){return B(_QV(new T(function(){return B(_QX(_Rn));})));})));}),[_RC,new T(function(){return B(A(_R1,[_Ro,_O0]));})]))){_Rx=new T(function(){return B(A(new T(function(){return B(_QT(_Re));}),[_RB,_RB]));});_Ry=new T(function(){return B(A(_RF,[new T(function(){return B(A(new T(function(){return B(_R9(_Ro));}),[_RC,new T(function(){return B(A(_R1,[_Ro,_O0]));})]));}),new T(function(){return B(A(_R1,[_Ro,_R0]));})]));});_Rz=new T(function(){return B(A(new T(function(){return B(_QT(_Re));}),[_RB,_RD]));});return null;}else{return new F(function(){return A(new T(function(){return B(_QT(_Re));}),[_RB,_RD]);});}}else{_Rx=new T(function(){return B(A(new T(function(){return B(_QT(_Re));}),[_RB,_RB]));});_Ry=new T(function(){return B(A(_RF,[_RC,new T(function(){return B(A(_R1,[_Ro,_R0]));})]));});var _RH=_RD;_Rz=_RH;return null;}})(_Rx,_Ry,_Rz);if(_RA!=null){return _RA;}}})(new T(function(){return B(A(new T(function(){return B(_QT(_Re));}),[_Rs,_Rs]));}),new T(function(){return B(A(_Rv,[new T(function(){return B(A(new T(function(){return B(_R9(_Rm));}),[_Rt,new T(function(){return B(A(_R1,[_Rm,_O0]));})]));}),new T(function(){return B(A(_R1,[_Rm,_R0]));})]));}),_Rs)):E(_Rs);}else{_Rp=new T(function(){return B(A(new T(function(){return B(_QT(_Re));}),[_Rs,_Rs]));});_Rq=new T(function(){return B(A(_Rv,[_Rt,new T(function(){return B(A(_R1,[_Rm,_R0]));})]));});return null;}})(_Rp,_Rq);if(_Rr!=null){return _Rr;}}})(_Rg,_Rh);});}else{return new F(function(){return A(_R1,[_Re,_O0]);});}}else{return E(_Rc);}},_RI=new T(function(){return B(err(_Rb));}),_RJ=function(_RK,_RL){var _RM=E(_RK);return _RM[0]==0?_RM[1]*Math.pow(2,_RL):I_toNumber(_RM[1])*Math.pow(2,_RL);},_RN=function(_RO,_RP){while(1){var _RQ=E(_RO);if(!_RQ[0]){var _RR=E(_RQ[1]);if(_RR==(-2147483648)){_RO=[1,I_fromInt(-2147483648)];continue;}else{var _RS=E(_RP);if(!_RS[0]){var _RT=_RS[1];return [0,[0,quot(_RR,_RT)],[0,_RR%_RT]];}else{_RO=[1,I_fromInt(_RR)];_RP=_RS;continue;}}}else{var _RU=E(_RP);if(!_RU[0]){_RO=_RQ;_RP=[1,I_fromInt(_RU[1])];continue;}else{var _RV=I_quotRem(_RQ[1],_RU[1]);return [0,[1,_RV[1]],[1,_RV[2]]];}}}},_RW=function(_RX,_RY){var _RZ=B(_yf(_RY)),_S0=_RZ[1],_S1=_RZ[2],_S2=new T(function(){return B(_QD(new T(function(){return B(_NY(_RX));})));});if(_S1<0){var _S3= -_S1;if(_S3>=0){var _S4=E(_S3),_S5=_S4==0?E(_O0):B(_QN(_LX,_S4));if(!B(_yL(_S5,_yF))){var _S6=B(_RN(_S0,_S5));return [0,new T(function(){return B(A(_R1,[_S2,_S6[1]]));}),new T(function(){return [0,B(_RJ(_S6[2],_S1))];})];}else{return E(_vY);}}else{return E(_RI);}}else{return [0,new T(function(){return B(A(_QT,[_S2,new T(function(){return B(A(_R1,[_S2,_S0]));}),new T(function(){return B(_Rd(_S2,_QC,new T(function(){return B(A(_R1,[_S2,_LX]));}),[0,_S1]));})]));}),_JI];}},_S7=function(_S8,_S9){var _Sa=B(_RW(_S8,E(_S9)[1])),_Sb=_Sa[1];if(E(_Sa[2])[1]<=0){return E(_Sb);}else{var _Sc=E(B(_NY(_S8))[1]);return new F(function(){return A(_Sc[1],[_Sb,new T(function(){return B(A(_Sc[7],[_Nf]));})]);});}},_Sd=function(_Se,_Sf){var _Sg=B(_RW(_Se,E(_Sf)[1])),_Sh=_Sg[1];if(E(_Sg[2])[1]>=0){return E(_Sh);}else{var _Si=E(B(_NY(_Se))[1]);return new F(function(){return A(_Si[3],[_Sh,new T(function(){return B(A(_Si[7],[_Nf]));})]);});}},_Sj=function(_Sk,_Sl){var _Sm=B(_RW(_Sk,E(_Sl)[1]));return [0,_Sm[1],_Sm[2]];},_Sn=function(_So,_Sp){var _Sq=B(_RW(_So,_Sp)),_Sr=_Sq[1],_Ss=E(_Sq[2])[1],_St=new T(function(){var _Su=E(B(_NY(_So))[1]),_Sv=_Su[7];return _Ss>=0?B(A(_Su[1],[_Sr,new T(function(){return B(A(_Sv,[_Nf]));})])):B(A(_Su[3],[_Sr,new T(function(){return B(A(_Sv,[_Nf]));})]));});if(_Ss<0){var _Sw= -_Ss-0.5;if(_Sw>=0){if(!E(_Sw)){var _Sx=E(_So),_Sy=E(_Sx[1]);return !B(_R3(_Sy[1],_Sy[2],_Sy[3],_Sx[4],_Sr))?E(_St):E(_Sr);}else{return E(_St);}}else{return E(_Sr);}}else{var _Sz=_Ss-0.5;if(_Sz>=0){if(!E(_Sz)){var _SA=E(_So),_SB=E(_SA[1]);return !B(_R3(_SB[1],_SB[2],_SB[3],_SA[4],_Sr))?E(_St):E(_Sr);}else{return E(_St);}}else{return E(_Sr);}}},_SC=function(_SD,_SE){return new F(function(){return _Sn(_SD,E(_SE)[1]);});},_SF=function(_SG,_SH){return E(B(_RW(_SG,E(_SH)[1]))[1]);},_SI=[0,_NX,_K3,_Sj,_SF,_SC,_S7,_Sd],_SJ=function(_SK,_SL){return E(_SK)[1]!=E(_SL)[1]?true:false;},_SM=function(_SN,_SO){return E(_SN)[1]==E(_SO)[1];},_SP=[0,_SM,_SJ],_SQ=function(_SR,_SS){return E(_SR)[1]<E(_SS)[1];},_ST=function(_SU,_SV){return E(_SU)[1]<=E(_SV)[1];},_SW=function(_SX,_SY){return E(_SX)[1]>E(_SY)[1];},_SZ=function(_T0,_T1){return E(_T0)[1]>=E(_T1)[1];},_T2=function(_T3,_T4){var _T5=E(_T3)[1],_T6=E(_T4)[1];return _T5>=_T6?_T5!=_T6?2:1:0;},_T7=function(_T8,_T9){var _Ta=E(_T8),_Tb=E(_T9);return _Ta[1]>_Tb[1]?E(_Ta):E(_Tb);},_Tc=function(_Td,_Te){var _Tf=E(_Td),_Tg=E(_Te);return _Tf[1]>_Tg[1]?E(_Tg):E(_Tf);},_Th=[0,_SP,_T2,_SQ,_SZ,_SW,_ST,_T7,_Tc],_Ti=function(_Tj,_Tk){while(1){var _Tl=(function(_Tm,_Tn){var _To=E(_Ng)[1]["v"]["i8"][(255&_Tm>>>0)>>>0&4294967295];if(_Tn>_To){if(_To>=8){var _Tp=_Tm>>8,_Tq=_Tn-8|0;_Tj=_Tp;_Tk=_Tq;return null;}else{return [0,new T(function(){return B(_ea(_Tm>>_To));}),_Tn-_To|0];}}else{return [0,new T(function(){return B(_ea(_Tm>>_Tn));}),0];}})(_Tj,_Tk);if(_Tl!=null){return _Tl;}}},_Tr=function(_Ts){var _Tt=decodeFloat(_Ts),_Tu=_Tt[1],_Tv=_Tt[2];if(_Tv<0){var _Tw=function(_Tx){if(!_Tx){return [0,B(_ea(_Tu)),B(_z0(_Nf, -_Tv))];}else{var _Ty=B(_Ti(_Tu, -_Tv));return [0,E(_Ty[1]),B(_z0(_Nf,_Ty[2]))];}};return (_Tu>>>0&1)>>>0==0?B(_Tw(1)):B(_Tw(0));}else{return [0,B(_z0(B(_ea(_Tu)),_Tv)),_Nf];}},_Tz=function(_TA){var _TB=B(_Tr(E(_TA)[1]));return [0,E(_TB[1]),E(_TB[2])];},_TC=[0,_Lk,_Th,_Tz],_TD=[0,-1],_TE=[0,1],_TF=function(_TG,_TH){var _TI=E(_TG);return _TI[0]==0?_TI[1]*Math.pow(2,_TH):I_toNumber(_TI[1])*Math.pow(2,_TH);},_TJ=[0,0],_TK=function(_TL,_TM){var _TN=decodeFloat(_TM),_TO=_TN[1],_TP=_TN[2],_TQ=new T(function(){return B(_QD(new T(function(){return B(_NY(_TL));})));});if(_TP<0){var _TR=new T(function(){if(_TO<0){var _TS= -_TP;if(_TS<32){var _TT=[0, -( -_TO>>_TS)];}else{var _TT= -_TO>=0?E(_TJ):E(_TE);}var _TU=_TT,_TV=_TU,_TW=_TV;}else{var _TX= -_TP;if(_TX<32){var _TY=[0,_TO>>_TX];}else{var _TY=_TO>=0?E(_TJ):E(_TD);}var _TZ=_TY,_U0=_TZ,_TW=_U0;}var _U1=_TW;return _U1;});return [0,new T(function(){return B(A(_R1,[_TQ,new T(function(){return B(_ea(E(_TR)[1]));})]));}),new T(function(){var _U2= -_TP;if(_U2<32){var _U3=[0,B(_TF(B(_ea(_TO-(E(_TR)[1]<<_U2)|0)),_TP))];}else{var _U3=[0,B(_TF(B(_ea(_TO)),_TP))];}var _U4=_U3,_U5=_U4,_U6=_U5;return _U6;})];}else{return [0,new T(function(){return B(A(_QT,[_TQ,new T(function(){return B(A(_R1,[_TQ,new T(function(){return B(_ea(_TO));})]));}),new T(function(){return B(_Rd(_TQ,_QC,new T(function(){return B(A(_R1,[_TQ,_LX]));}),[0,_TP]));})]));}),_L3];}},_U7=function(_U8,_U9){var _Ua=B(_TK(_U8,E(_U9)[1])),_Ub=_Ua[1];if(E(_Ua[2])[1]<=0){return E(_Ub);}else{var _Uc=E(B(_NY(_U8))[1]);return new F(function(){return A(_Uc[1],[_Ub,new T(function(){return B(A(_Uc[7],[_Nf]));})]);});}},_Ud=function(_Ue,_Uf){var _Ug=B(_TK(_Ue,E(_Uf)[1])),_Uh=_Ug[1];if(E(_Ug[2])[1]>=0){return E(_Uh);}else{var _Ui=E(B(_NY(_Ue))[1]);return new F(function(){return A(_Ui[3],[_Uh,new T(function(){return B(A(_Ui[7],[_Nf]));})]);});}},_Uj=function(_Uk,_Ul){var _Um=B(_TK(_Uk,E(_Ul)[1]));return [0,_Um[1],_Um[2]];},_Un=function(_Uo,_Up){var _Uq=B(_TK(_Uo,_Up)),_Ur=_Uq[1],_Us=E(_Uq[2])[1],_Ut=new T(function(){var _Uu=E(B(_NY(_Uo))[1]),_Uv=_Uu[7];return _Us>=0?B(A(_Uu[1],[_Ur,new T(function(){return B(A(_Uv,[_Nf]));})])):B(A(_Uu[3],[_Ur,new T(function(){return B(A(_Uv,[_Nf]));})]));});if(_Us<0){var _Uw= -_Us-0.5;if(_Uw>=0){if(!E(_Uw)){var _Ux=E(_Uo),_Uy=E(_Ux[1]);return !B(_R3(_Uy[1],_Uy[2],_Uy[3],_Ux[4],_Ur))?E(_Ut):E(_Ur);}else{return E(_Ut);}}else{return E(_Ur);}}else{var _Uz=_Us-0.5;if(_Uz>=0){if(!E(_Uz)){var _UA=E(_Uo),_UB=E(_UA[1]);return !B(_R3(_UB[1],_UB[2],_UB[3],_UA[4],_Ur))?E(_Ut):E(_Ur);}else{return E(_Ut);}}else{return E(_Ur);}}},_UC=function(_UD,_UE){return new F(function(){return _Un(_UD,E(_UE)[1]);});},_UF=function(_UG,_UH){return E(B(_TK(_UG,E(_UH)[1]))[1]);},_UI=[0,_TC,_Lo,_Uj,_UF,_UC,_U7,_Ud],_UJ=function(_UK){return new F(function(){return err(B(unAppCStr("Char.intToDigit: not a digit ",new T(function(){if(_UK>=0){var _UL=jsShowI(_UK),_UM=_UL,_UN=fromJSStr(_UM);}else{var _UO=jsShowI(_UK),_UP=_UO,_UN=fromJSStr(_UP);}var _UQ=_UN;return _UQ;}))));});},_UR=function(_US){var _UT=function(_UU){if(_US<10){return new F(function(){return _UJ(_US);});}else{if(_US>15){return new F(function(){return _UJ(_US);});}else{return (97+_US|0)-10|0;}}};if(_US<0){return new F(function(){return _UT(_);});}else{if(_US>9){return new F(function(){return _UT(_);});}else{return 48+_US|0;}}},_UV=function(_UW){return [0,B(_UR(E(_UW)[1]))];},_UX=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_UY=function(_UZ){return new F(function(){return _aZ([0,new T(function(){return B(_be(_UZ,_UX));})],_aW);});},_V0=new T(function(){return B(_UY("GHC/Float.lhs:619:11-64|d : ds\'"));}),_V1=function(_V2,_V3){if(E(_V2)[1]<=0){var _V4=B(_1q(_UV,[1,_TJ,_V3]));return _V4[0]==0?E(_V0):[0,_V4[1],_V4[2]];}else{var _V5=B(_1q(_UV,_V3));return _V5[0]==0?E(_V0):[0,_V5[1],_V5[2]];}},_V6=function(_V7){return E(E(_V7)[1]);},_V8=function(_V9){return E(E(_V9)[1]);},_Va=function(_Vb){return E(E(_Vb)[1]);},_Vc=[0,48],_Vd=[1,_Vc,_u],_Ve=[0,46],_Vf=function(_Vg,_Vh,_Vi){while(1){var _Vj=(function(_Vk,_Vl,_Vm){var _Vn=E(_Vk);if(!_Vn){var _Vo=B(_Jf(_Vl,_u));return _Vo[0]==0?[1,_Vc,[1,_Ve,new T(function(){var _Vp=E(_Vm);return _Vp[0]==0?E(_Vd):E(_Vp);})]]:B(_O(_Vo,[1,_Ve,new T(function(){var _Vq=E(_Vm);return _Vq[0]==0?E(_Vd):E(_Vq);})]));}else{var _Vr=E(_Vm);if(!_Vr[0]){_Vg=_Vn-1|0;var _Vs=[1,_Vc,_Vl];_Vi=_u;_Vh=_Vs;return null;}else{_Vg=_Vn-1|0;var _Vs=[1,_Vr[1],_Vl];_Vi=_Vr[2];_Vh=_Vs;return null;}}})(_Vg,_Vh,_Vi);if(_Vj!=null){return _Vj;}}},_Vt=[0,0],_Vu=new T(function(){return B(unCStr(" out of range "));}),_Vv=new T(function(){return B(unCStr("}.index: Index "));}),_Vw=new T(function(){return B(unCStr("Ix{"));}),_Vx=[1,_16,_u],_Vy=[1,_16,_Vx],_Vz=function(_VA,_VB,_VC,_VD,_VE){return new F(function(){return err(B(_O(_Vw,new T(function(){return B(_O(_VA,new T(function(){return B(_O(_Vv,[1,_17,new T(function(){return B(A(_VE,[_Vt,_VB,[1,_16,new T(function(){return B(_O(_Vu,[1,_17,[1,_17,new T(function(){return B(A(_tB,[_tr,[1,new T(function(){return B(A(_VE,[_sW,_VC]));}),[1,new T(function(){return B(A(_VE,[_sW,_VD]));}),_u]],_Vy]));})]]));})]]));})]));})));}))));});},_VF=function(_VG,_VH,_VI,_VJ){var _VK=E(_VI);return new F(function(){return _Vz(_VG,_VH,_VK[1],_VK[2],E(_VJ)[1]);});},_VL=function(_VM,_VN,_VO,_VP){return new F(function(){return _VF(_VP,_VO,_VN,_VM);});},_VQ=new T(function(){return B(unCStr("Int"));}),_VR=function(_VS,_VT,_VU){return new F(function(){return _VL(_tq,[0,_VT,_VU],_VS,_VQ);});},_VV=new T(function(){return B(unCStr("(Array.!): undefined array element"));}),_VW=new T(function(){return B(err(_VV));}),_VX=[0,1100],_VY=[0,_TJ,_VX],_VZ=function(_W0){return new F(function(){return _VL(_tq,_VY,[0,_W0],_VQ);});},_W1=function(_){var _W2=newArr(1101,_VW),_W3=_W2;return new F(function(){return (function(_W4,_){while(1){var _W5=(function(_W6,_){if(0>_W6){return new F(function(){return _VZ(_W6);});}else{if(_W6>1100){return new F(function(){return _VZ(_W6);});}else{var _=_W3[_W6]=new T(function(){if(_W6>=0){var _W7=E(_W6),_W8=_W7==0?E(_O0):B(_QN(_LX,_W7));}else{var _W8=E(_RI);}var _W9=_W8;return _W9;}),_Wa=E(_W6);if(_Wa==1100){var _Wb=_W3,_Wc=_Wb;return [0,E(_TJ),E(_VX),1101,_Wc];}else{_W4=_Wa+1|0;return null;}}}})(_W4,_);if(_W5!=null){return _W5;}}})(0,_);});},_Wd=function(_We){var _Wf=B(A(_We,[_])),_Wg=_Wf;return E(_Wg);},_Wh=new T(function(){return B(_Wd(_W1));}),_Wi=[0,10],_Wj=[0,324],_Wk=[0,_TJ,_Wj],_Wl=function(_Wm){return new F(function(){return _VL(_tq,_Wk,[0,_Wm],_VQ);});},_Wn=function(_){var _Wo=newArr(325,_VW),_Wp=_Wo;return new F(function(){return (function(_Wq,_){while(1){var _Wr=(function(_Ws,_){if(0>_Ws){return new F(function(){return _Wl(_Ws);});}else{if(_Ws>324){return new F(function(){return _Wl(_Ws);});}else{var _=_Wp[_Ws]=new T(function(){if(_Ws>=0){var _Wt=E(_Ws),_Wu=_Wt==0?E(_O0):B(_QN(_Wi,_Wt));}else{var _Wu=E(_RI);}var _Wv=_Wu;return _Wv;}),_Ww=E(_Ws);if(_Ww==324){var _Wx=_Wp,_Wy=_Wx;return [0,E(_TJ),E(_Wj),325,_Wy];}else{_Wq=_Ww+1|0;return null;}}}})(_Wq,_);if(_Wr!=null){return _Wr;}}})(0,_);});},_Wz=new T(function(){return B(_Wd(_Wn));}),_WA=function(_WB,_WC){var _WD=[0,_WC],_WE=function(_WF){if(!B(_yL(_WB,_Wi))){if(_WC>=0){var _WG=E(_WC);return _WG==0?E(_O0):B(_QN(_WB,_WG));}else{return E(_RI);}}else{if(_WC>324){if(_WC>=0){var _WH=E(_WC);return _WH==0?E(_O0):B(_QN(_WB,_WH));}else{return E(_RI);}}else{var _WI=E(_Wz),_WJ=E(_WI[1]),_WK=_WJ[1],_WL=E(_WI[2]);if(_WK>_WC){return new F(function(){return _VR(_WD,_WJ,_WL);});}else{if(_WC>_WL[1]){return new F(function(){return _VR(_WD,_WJ,_WL);});}else{return E(_WI[4][_WC-_WK|0]);}}}}};if(!B(_yL(_WB,_LX))){return new F(function(){return _WE(_);});}else{if(_WC<0){return new F(function(){return _WE(_);});}else{if(_WC>1100){return new F(function(){return _WE(_);});}else{var _WM=E(_Wh),_WN=E(_WM[1]),_WO=_WN[1],_WP=E(_WM[2]);if(_WO>_WC){return new F(function(){return _VR(_WD,_WN,_WP);});}else{if(_WC>_WP[1]){return new F(function(){return _VR(_WD,_WN,_WP);});}else{return E(_WM[4][_WC-_WO|0]);}}}}}},_WQ=function(_WR,_WS){var _WT=E(_WR);if(!_WT[0]){var _WU=_WT[1],_WV=E(_WS);return _WV[0]==0?_WU>_WV[1]:I_compareInt(_WV[1],_WU)<0;}else{var _WW=_WT[1],_WX=E(_WS);return _WX[0]==0?I_compareInt(_WW,_WX[1])>0:I_compare(_WW,_WX[1])>0;}},_WY=[1,_TJ,_u],_WZ=function(_X0,_X1){while(1){var _X2=E(_X0);if(!_X2[0]){var _X3=E(_X2[1]);if(_X3==(-2147483648)){_X0=[1,I_fromInt(-2147483648)];continue;}else{var _X4=E(_X1);if(!_X4[0]){return [0,quot(_X3,_X4[1])];}else{_X0=[1,I_fromInt(_X3)];_X1=_X4;continue;}}}else{var _X5=_X2[1],_X6=E(_X1);return _X6[0]==0?[0,I_toInt(I_quot(_X5,I_fromInt(_X6[1])))]:[1,I_quot(_X5,_X6[1])];}}},_X7=function(_X8,_X9,_Xa,_Xb,_Xc,_Xd,_Xe,_Xf){if(!B(A(_X8,[_Xf,new T(function(){return B(A(_R1,[B(_V8(B(_V6(_X9)))),_yF]));})]))){var _Xg=new T(function(){return B(A(_Xa,[_Xf]));}),_Xh=new T(function(){return B(A(_Xb,[_Xf]));}),_Xi=new T(function(){return [0,E(B(A(_Xc,[_Xf]))[1])[1]-E(_Xh)[1]|0];}),_Xj=new T(function(){return B(A(_Xd,[_Xf]));}),_Xk=new T(function(){return E(E(_Xj)[2]);}),_Xl=new T(function(){var _Xm=E(_Xk),_Xn=_Xm[1],_Xo=E(_Xi)[1]-_Xn|0;if(_Xo<=0){var _Xp=[0,new T(function(){return E(E(_Xj)[1]);}),_Xm];}else{var _Xp=[0,new T(function(){var _Xq=B(_WA(_Xg,_Xo));if(!B(_yL(_Xq,_yF))){var _Xr=B(_WZ(E(_Xj)[1],_Xq));}else{var _Xr=E(_vY);}var _Xs=_Xr;return _Xs;}),[0,_Xn+_Xo|0]];}var _Xt=_Xp,_Xu=_Xt,_Xv=_Xu,_Xw=_Xv;return _Xw;}),_Xx=new T(function(){return E(E(_Xl)[2]);}),_Xy=new T(function(){return E(E(_Xl)[1]);}),_Xz=new T(function(){var _XA=E(_Xx)[1];if(_XA<0){if(_XA<=E(_Xi)[1]){var _XB=[0,new T(function(){return B(_ec(_Xy,_LX));}),new T(function(){return B(_ec(B(_WA(_Xg, -_XA)),_LX));}),_Nf,_Nf];}else{var _XB=!B(_yL(_Xy,B(_WA(_Xg,E(_Xh)[1]-1|0))))?[0,new T(function(){return B(_ec(_Xy,_LX));}),new T(function(){return B(_ec(B(_WA(_Xg, -_XA)),_LX));}),_Nf,_Nf]:[0,new T(function(){return B(_ec(B(_ec(_Xy,_Xg)),_LX));}),new T(function(){return B(_ec(B(_WA(_Xg, -_XA+1|0)),_LX));}),_Xg,_Nf];}var _XC=_XB,_XD=_XC,_XE=_XD;}else{var _XF=new T(function(){return B(_WA(_Xg,_XA));}),_XE=!B(_yL(_Xy,B(_WA(_Xg,E(_Xh)[1]-1|0))))?[0,new T(function(){return B(_ec(B(_ec(_Xy,_XF)),_LX));}),_LX,_XF,_XF]:[0,new T(function(){return B(_ec(B(_ec(B(_ec(_Xy,_XF)),_Xg)),_LX));}),new T(function(){return B(_ec(_LX,_Xg));}),new T(function(){return B(_ec(_XF,_Xg));}),_XF];}var _XG=_XE,_XH=_XG;return _XH;}),_XI=new T(function(){return E(E(_Xz)[2]);}),_XJ=new T(function(){return E(E(_Xz)[3]);}),_XK=new T(function(){return E(E(_Xz)[1]);}),_XL=new T(function(){var _XM=new T(function(){return B(_dU(_XK,_XJ));}),_XN=function(_XO){var _XP=(Math.log(B(_KY(B(_dU(_Xy,_Nf)))))+E(_Xx)[1]*Math.log(B(_KY(_Xg))))/Math.log(B(_KY(_Xe))),_XQ=_XP&4294967295;return _XQ>=_XP?E(_XQ):_XQ+1|0;},_XR=function(_XS){while(1){if(_XS<0){if(!B(_fq(B(_ec(B(_WA(_Xe, -_XS)),_XM)),_XI))){var _XT=_XS+1|0;_XS=_XT;continue;}else{return E(_XS);}}else{if(!B(_fq(_XM,B(_ec(B(_WA(_Xe,_XS)),_XI))))){var _XT=_XS+1|0;_XS=_XT;continue;}else{return E(_XS);}}}};if(!B(_yL(_Xg,_LX))){var _XU=[0,B(_XR(B(_XN(_))))];}else{if(!B(_yL(_Xe,_Wi))){var _XV=[0,B(_XR(B(_XN(_))))];}else{var _XW=(E(_Xh)[1]-1|0)+E(_Xk)[1]|0;if(_XW<0){var _XX=[0,B(_XR(quot(imul(_XW,8651)|0,28738)))];}else{var _XX=[0,B(_XR(quot(imul(_XW,8651)|0,28738)+1|0))];}var _XY=_XX,_XZ=_XY,_Y0=_XZ,_Y1=_Y0,_Y2=_Y1,_XV=_Y2;}var _XU=_XV;}return _XU;});return [0,new T(function(){var _Y3=E(_XL)[1],_Y4=function(_Y5,_Y6,_Y7,_Y8,_Y9){while(1){var _Ya=(function(_Yb,_Yc,_Yd,_Ye,_Yf){if(!B(_yL(_Yd,_yF))){var _Yg=B(_RN(B(_ec(_Yc,_Xe)),_Yd)),_Yh=_Yg[1],_Yi=_Yg[2],_Yj=B(_ec(_Yf,_Xe)),_Yk=B(_ec(_Ye,_Xe));if(!B(_Y(_Yi,_Yj))){if(!B(_WQ(B(_dU(_Yi,_Yk)),_Yd))){var _Yl=[1,_Yh,_Yb];_Y6=_Yi;var _Ym=_Yd;_Y8=_Yk;_Y9=_Yj;_Y5=_Yl;_Y7=_Ym;return null;}else{return [1,new T(function(){return B(_dU(_Yh,_Nf));}),_Yb];}}else{return !B(_WQ(B(_dU(_Yi,_Yk)),_Yd))?[1,_Yh,_Yb]:!B(_Y(B(_ec(_Yi,_LX)),_Yd))?[1,new T(function(){return B(_dU(_Yh,_Nf));}),_Yb]:[1,_Yh,_Yb];}}else{return E(_vY);}})(_Y5,_Y6,_Y7,_Y8,_Y9);if(_Ya!=null){return _Ya;}}};if(_Y3<0){var _Yn=B(_WA(_Xe, -_Y3)),_Yo=B(_1q(_PQ,B(_Jf(B(_Y4(_u,B(_ec(_XK,_Yn)),_XI,B(_ec(_XJ,_Yn)),B(_ec(E(_Xz)[4],_Yn)))),_u))));}else{var _Yo=B(_1q(_PQ,B(_Jf(B(_Y4(_u,_XK,B(_ec(_XI,B(_WA(_Xe,_Y3)))),_XJ,E(_Xz)[4])),_u))));}var _Yp=_Yo,_Yq=_Yp;return _Yq;}),_XL];}else{return [0,_WY,_TJ];}},_Yr=function(_Ys){return E(_Ys)[1]%2==0?true:false;},_Yt=new T(function(){return B(unCStr("roundTo: bad Value"));}),_Yu=new T(function(){return B(err(_Yt));}),_Yv=function(_Yw){return E(E(_Yw)[1])==0?true:false;},_Yx=function(_Yy){return _Yy>1?[1,_TJ,new T(function(){return B(_Yx(_Yy-1|0));})]:E(_WY);},_Yz=function(_YA,_YB,_YC){var _YD=function(_YE,_YF,_YG){var _YH=E(_YG);if(!_YH[0]){return [0,_TJ,new T(function(){var _YI=E(_YE)[1];return _YI>0?B(_Yx(_YI)):[0];})];}else{var _YJ=_YH[1],_YK=_YH[2],_YL=E(E(_YE)[1]);if(!_YL){var _YM=E(_YJ)[1],_YN=E(new T(function(){return [0,quot(E(_YA)[1],2)];}))[1];return _YM!=_YN?[0,new T(function(){return _YM<_YN?E(_TJ):E(_TE);}),_u]:!E(_YF)?[0,new T(function(){return _YM<_YN?E(_TJ):E(_TE);}),_u]:!B(_Ir(_Yv,_YK))?[0,new T(function(){return _YM<_YN?E(_TJ):E(_TE);}),_u]:[0,_TJ,_u];}else{var _YO=B(_YD([0,_YL-1|0],new T(function(){return B(_Yr(_YJ));}),_YK)),_YP=_YO[2],_YQ=E(_YO[1])[1]+E(_YJ)[1]|0;return _YQ!=E(_YA)[1]?[0,_TJ,[1,[0,_YQ],_YP]]:[0,_TE,[1,_TJ,_YP]];}}},_YR=B(_YD(_YB,_n,_YC));switch(E(E(_YR[1])[1])){case 0:return E(_YR);case 1:return [0,_TE,[1,_TE,_YR[2]]];default:return E(_Yu);}},_YS=function(_YT,_YU){var _YV=E(_YT);if(!_YV){return [0,_u,_YU];}else{var _YW=E(_YU);if(!_YW[0]){return [0,_u,_u];}else{var _YX=new T(function(){var _YY=B(_YS(_YV-1|0,_YW[2]));return [0,_YY[1],_YY[2]];});return [0,[1,_YW[1],new T(function(){return E(E(_YX)[1]);})],new T(function(){return E(E(_YX)[2]);})];}}},_YZ=function(_Z0){return E(E(_Z0)[3]);},_Z1=0,_Z2=1,_Z3=[0,10],_Z4=new T(function(){return B(unCStr("e0"));}),_Z5=function(_Z6,_Z7){var _Z8=E(_Z6);if(!_Z8[0]){return E(_Z4);}else{var _Z9=_Z8[1];return _Z7>1?[1,_Z9,new T(function(){return B(_Z5(_Z8[2],_Z7-1|0));})]:[1,_Z9,_Z4];}},_Za=function(_Zb,_Zc){var _Zd=E(_Zc);return _Zd[0]==0?[0]:[1,_Zb,new T(function(){return B(_Za(_Zd[1],_Zd[2]));})];},_Ze=new T(function(){return B(unCStr("init"));}),_Zf=new T(function(){return B(_tx(_Ze));}),_Zg=new T(function(){return B(_UY("GHC/Float.lhs:591:12-70|(d : ds\')"));}),_Zh=[0,101],_Zi=new T(function(){return B(unCStr("Infinity"));}),_Zj=new T(function(){return B(unCStr("-Infinity"));}),_Zk=new T(function(){return B(unCStr("NaN"));}),_Zl=new T(function(){return B(unCStr("formatRealFloat/doFmt/FFExponent: []"));}),_Zm=new T(function(){return B(err(_Zl));}),_Zn=new T(function(){return B(unCStr("0.0e0"));}),_Zo=function(_Zp){return E(E(_Zp)[4]);},_Zq=new T(function(){return [1,_Vc,_Zq];}),_Zr=function(_Zs,_Zt,_Zu,_Zv,_Zw,_Zx,_Zy,_Zz,_ZA,_ZB,_ZC,_ZD){if(!B(A(_Zy,[_ZD]))){var _ZE=new T(function(){return B(_V8(new T(function(){return B(_V6(_Zt));})));});if(!B(A(_Zz,[_ZD]))){var _ZF=function(_ZG,_ZH,_ZI){while(1){var _ZJ=(function(_ZK,_ZL,_ZM){switch(E(_ZK)){case 0:var _ZN=E(_ZC);if(!_ZN[0]){var _ZO=B(_1q(_UV,_ZL));if(!_ZO[0]){return E(_Zm);}else{var _ZP=_ZO[2],_ZQ=E(_ZO[1]),_ZR=function(_ZS){var _ZT=E(_ZP);return _ZT[0]==0?[1,_ZQ,new T(function(){return B(unAppCStr(".0e",new T(function(){return B(_81(0,E(_ZM)[1]-1|0,_u));})));})]:[1,_ZQ,[1,_Ve,new T(function(){return B(_O(_ZT,[1,_Zh,new T(function(){return B(_81(0,E(_ZM)[1]-1|0,_u));})]));})]];};return E(_ZQ[1])==48?E(_ZP)[0]==0?E(_Zn):B(_ZR(_)):B(_ZR(_));}}else{var _ZU=new T(function(){var _ZV=E(_ZN[1]);return _ZV[1]>1?E(_ZV):E(_TE);}),_ZW=function(_ZX){var _ZY=new T(function(){var _ZZ=B(_Yz(_Z3,new T(function(){return [0,E(_ZU)[1]+1|0];}),_ZL));return [0,_ZZ[1],_ZZ[2]];}),_100=new T(function(){return E(E(_ZY)[1]);}),_101=new T(function(){if(E(_100)[1]<=0){var _102=B(_1q(_UV,E(_ZY)[2])),_103=_102[0]==0?E(_Zg):[0,_102[1],_102[2]];}else{var _104=E(E(_ZY)[2]);if(!_104[0]){var _105=E(_Zf);}else{var _106=B(_1q(_UV,B(_Za(_104[1],_104[2])))),_105=_106[0]==0?E(_Zg):[0,_106[1],_106[2]];}var _107=_105,_103=_107;}var _108=_103,_109=_108;return _109;});return [1,new T(function(){return E(E(_101)[1]);}),[1,_Ve,new T(function(){return B(_O(E(_101)[2],[1,_Zh,new T(function(){return B(_81(0,(E(_ZM)[1]-1|0)+E(_100)[1]|0,_u));})]));})]];},_10a=E(_ZL);if(!_10a[0]){return new F(function(){return _ZW(_);});}else{return E(E(_10a[1])[1])==0?E(_10a[2])[0]==0?[1,_Vc,[1,_Ve,new T(function(){var _10b=E(_ZU)[1];return _10b>0?B(_Z5(_Zq,_10b)):E(_Z4);})]]:B(_ZW(_)):B(_ZW(_));}}break;case 1:var _10c=E(_ZC);if(!_10c[0]){var _10d=E(_ZM)[1];return _10d>0?B(_Vf(_10d,_u,new T(function(){return B(_1q(_UV,_ZL));}))):B(unAppCStr("0.",new T(function(){var _10e= -_10d;if(_10e>0){var _10f=function(_10g){return _10g>1?[1,_Vc,new T(function(){return B(_10f(_10g-1|0));})]:E([1,_Vc,new T(function(){return B(_1q(_UV,_ZL));})]);},_10h=B(_10f(_10e));}else{var _10h=B(_1q(_UV,_ZL));}var _10i=_10h,_10j=_10i;return _10j;})));}else{var _10k=_10c[1],_10l=E(_ZM),_10m=_10l[1];if(_10m<0){var _10n=new T(function(){var _10o= -_10m;if(_10o>0){var _10p=function(_10q){return _10q>1?[1,_TJ,new T(function(){return B(_10p(_10q-1|0));})]:E([1,_TJ,_ZL]);},_10r=B(_Yz(_Z3,new T(function(){var _10s=E(_10k);return _10s[1]>0?E(_10s):E(_TJ);}),B(_10p(_10o)))),_10t=B(_V1(_10r[1],_10r[2]));}else{var _10u=B(_Yz(_Z3,new T(function(){var _10v=E(_10k);return _10v[1]>0?E(_10v):E(_TJ);}),_ZL)),_10t=B(_V1(_10u[1],_10u[2]));}var _10w=_10t,_10x=_10w;return _10x;});return [1,new T(function(){return E(E(_10n)[1]);}),new T(function(){var _10y=E(E(_10n)[2]);return _10y[0]==0?[0]:[1,_Ve,_10y];})];}else{var _10z=B(_Yz(_Z3,new T(function(){var _10A=E(_10k)[1];if(_10A>0){var _10B=[0,_10A+_10m|0];}else{var _10B=E(_10l);}var _10C=_10B,_10D=_10C;return _10D;}),_ZL)),_10E=_10z[2],_10F=_10m+E(_10z[1])[1]|0;if(_10F>=0){var _10G=B(_YS(_10F,new T(function(){return B(_1q(_UV,_10E));}))),_10H=_10G[2],_10I=E(_10G[1]);return _10I[0]==0?[1,_Vc,new T(function(){var _10J=E(_10H);return _10J[0]==0?[0]:[1,_Ve,_10J];})]:B(_O(_10I,new T(function(){var _10K=E(_10H);return _10K[0]==0?[0]:[1,_Ve,_10K];})));}else{return [1,_Vc,new T(function(){var _10L=B(_1q(_UV,_10E));return _10L[0]==0?[0]:[1,_Ve,_10L];})];}}}break;default:var _10M=E(_ZM),_10N=_10M[1];if(_10N>=0){if(_10N<=7){_ZG=_Z2;var _10O=_ZL;_ZI=_10M;_ZH=_10O;return null;}else{_ZG=_Z1;var _10O=_ZL;_ZI=_10M;_ZH=_10O;return null;}}else{_ZG=_Z1;var _10O=_ZL;_ZI=_10M;_ZH=_10O;return null;}}})(_ZG,_ZH,_ZI);if(_ZJ!=null){return _ZJ;}}},_10P=function(_10Q){return [1,_u5,new T(function(){var _10R=B(_X7(E(E(E(E(_Zs)[1])[2])[1])[1],_Zt,_Zu,_Zv,_Zw,_Zx,_Wi,new T(function(){return B(A(_Zo,[_ZE,_ZD]));})));return B(_ZF(_ZB,_10R[1],_10R[2]));})];};if(!B(A(_YZ,[B(_QX(B(_Va(_Zs)))),_ZD,new T(function(){return B(A(_R1,[_ZE,_yF]));})]))){if(!B(A(_ZA,[_ZD]))){var _10S=B(_X7(E(E(E(E(_Zs)[1])[2])[1])[1],_Zt,_Zu,_Zv,_Zw,_Zx,_Wi,_ZD));return new F(function(){return _ZF(_ZB,_10S[1],_10S[2]);});}else{return new F(function(){return _10P(_);});}}else{return new F(function(){return _10P(_);});}}else{return !B(A(_YZ,[B(_QX(B(_Va(_Zs)))),_ZD,new T(function(){return B(A(_R1,[_ZE,_yF]));})]))?E(_Zi):E(_Zj);}}else{return E(_Zk);}},_10T=function(_10U){var _10V=u_towlower(_10U),_10W=_10V;return _10W>>>0>1114111?B(_fl(_10W)):_10W;},_10X=function(_10Y){return new F(function(){return err(B(unAppCStr("Printf.printf: ",_10Y)));});},_10Z=new T(function(){return B(unCStr("bad argument"));}),_110=new T(function(){return B(_10X(_10Z));}),_111=new T(function(){return B(unCStr("Printf.dfmt: impossible"));}),_112=new T(function(){return B(err(_111));}),_113=[0,45],_114=[1,_113,_u],_115=new T(function(){return B(err(_111));}),_116=function(_117,_118){var _119=E(_117);return _119[0]==0?function(_d5){return new F(function(){return _O(new T(function(){var _11a=jsShow(E(_118)[1]),_11b=_11a;return fromJSStr(_11b);}),_d5);});}:function(_d5){return new F(function(){return _O(new T(function(){var _11c=E(E(_119[1])[1]);if(!_11c){var _11d=jsRound(E(_118)[1]),_11e=_11d,_11f=B(_yf(_11e)),_11g=_11f[1],_11h=_11f[2];if(_11h>=0){var _11i=jsShow(B(_xb(B(_z0(_11g,_11h))))),_11j=_11i,_11k=fromJSStr(_11j);}else{var _11l=hs_uncheckedIShiftRA64(B(_yy(_11g)), -_11h),_11m=_11l,_11n=jsShow(B(_xb(B(_yi(_11m))))),_11o=_11n,_11k=fromJSStr(_11o);}var _11p=_11k,_11q=_11p,_11r=_11q,_11s=_11r;}else{if(_11c>=0){var _11t=B(_y9(10,_11c)),_11u=jsRound(E(_118)[1]*_11t),_11v=_11u,_11w=jsShow((_11v&4294967295)/_11t),_11x=_11w,_11y=fromJSStr(_11x);}else{var _11y=E(_y0);}var _11z=_11y,_11A=_11z,_11s=_11A;}var _11B=_11s;return _11B;}),_d5);});};},_11C=function(_11D,_11E){var _11F=E(_11D);return _11F[0]==0?function(_d5){return new F(function(){return _O(new T(function(){var _11G=B(_Tr(E(_11E)[1])),_11H=jsShow(B(_yT(_11G[1],_11G[2]))[1]),_11I=_11H;return fromJSStr(_11I);}),_d5);});}:function(_d5){return new F(function(){return _O(new T(function(){var _11J=E(E(_11F[1])[1]);if(!_11J){var _11K=jsRound(E(_11E)[1]),_11L=_11K,_11M=decodeFloat(_11L),_11N=_11M[1],_11O=_11M[2];if(_11O>=0){var _11P=jsShow(B(_xb(B(_z0(B(_ea(_11N)),_11O))))),_11Q=_11P,_11R=fromJSStr(_11Q);}else{var _11S=jsShow(_11N>> -_11O),_11T=_11S,_11R=fromJSStr(_11T);}var _11U=_11R,_11V=_11U,_11W=_11V,_11X=_11W;}else{var _11Y=B(_Tr(E(_11E)[1]));if(_11J>=0){var _11Z=B(_y9(10,_11J)),_120=jsRound(B(_yT(_11Y[1],_11Y[2]))[1]*_11Z),_121=_120,_122=jsShow((_121&4294967295)/_11Z),_123=_122,_124=fromJSStr(_123);}else{var _124=E(_y0);}var _125=_124,_126=_125,_127=_126,_128=_127,_11X=_128;}var _129=_11X;return _129;}),_d5);});};},_12a=function(_12b){var _12c=u_towupper(_12b),_12d=_12c;return _12d>>>0>1114111?B(_fl(_12d)):_12d;},_12e=function(_12f){return [0,B(_12a(E(_12f)[1]))];},_12g=function(_12h,_12i,_12j){var _12k=E(_12j);switch(_12k[0]){case 3:var _12l=_12k[1],_12m=u_iswupper(_12h),_12n=_12m;switch(B(_10T(_12h))){case 101:var _12o=B(_Zr(_UI,_LQ,_Mn,_Ml,_Ms,_Mh,_My,_Mu,_MC,_Z1,new T(function(){var _12p=E(_12i);return _12p[1]>=0?[1,_12p]:[0];}),_12l));break;case 102:var _12o=B(_Zr(_UI,_LQ,_Mn,_Ml,_Ms,_Mh,_My,_Mu,_MC,_Z2,new T(function(){var _12q=E(_12i);return _12q[1]>=0?[1,_12q]:[0];}),_12l));break;case 103:var _12r=E(_12i),_12o=_12r[1]>=0?B(A(_11C,[[1,_12r],_12l,_u])):B(A(_11C,[_2v,_12l,_u]));break;default:var _12o=E(_115);}var _12s=_12o,_12t=E(_12n);if(!_12t){var _12u=E(_12s);if(!_12u[0]){return [0,_u,_u];}else{var _12v=_12u[1],_12w=_12u[2],_12x=E(_12v),_12y=_12x[1],_12z=E(_12y);return _12z==45?[0,_114,_12w]:[0,_u,_12u];}}else{var _12A=B(_1q(_12e,_12s));if(!_12A[0]){return [0,_u,_u];}else{var _12B=_12A[1],_12C=_12A[2],_12D=E(_12B),_12E=_12D[1],_12F=E(_12E);return _12F==45?[0,_114,_12C]:[0,_u,_12A];}}break;case 4:var _12G=_12k[1],_12H=u_iswupper(_12h),_12I=_12H;switch(B(_10T(_12h))){case 101:var _12J=B(_Zr(_SI,_Kv,_LY,_LV,_M3,_LR,_M9,_M5,_Md,_Z1,new T(function(){var _12K=E(_12i);return _12K[1]>=0?[1,_12K]:[0];}),_12G));break;case 102:var _12J=B(_Zr(_SI,_Kv,_LY,_LV,_M3,_LR,_M9,_M5,_Md,_Z2,new T(function(){var _12L=E(_12i);return _12L[1]>=0?[1,_12L]:[0];}),_12G));break;case 103:var _12M=E(_12i),_12J=_12M[1]>=0?B(A(_116,[[1,_12M],_12G,_u])):B(A(_116,[_2v,_12G,_u]));break;default:var _12J=E(_112);}var _12N=_12J,_12O=E(_12I);if(!_12O){var _12P=E(_12N);if(!_12P[0]){return [0,_u,_u];}else{var _12Q=_12P[1],_12R=_12P[2],_12S=E(_12Q),_12T=_12S[1],_12U=E(_12T);return _12U==45?[0,_114,_12R]:[0,_u,_12P];}}else{var _12V=B(_1q(_12e,_12N));if(!_12V[0]){return [0,_u,_u];}else{var _12W=_12V[1],_12X=_12V[2],_12Y=E(_12W),_12Z=_12Y[1],_130=E(_12Z);return _130==45?[0,_114,_12X]:[0,_u,_12V];}}break;default:return E(_110);}},_131=[0,0],_132=function(_133){return new F(function(){return _19(0,_133,_u);});},_134=[0,48],_135=function(_136,_137){var _138=_136-B(_EV(_137,0))|0;if(_138>0){var _139=function(_13a){return _13a>1?[1,_134,new T(function(){return B(_139(_13a-1|0));})]:E([1,_134,_137]);};return new F(function(){return _139(_138);});}else{return E(_137);}},_13b=[0,0],_13c=[0,-2147483648],_13d=function(_13e,_13f){while(1){var _13g=(function(_13h,_13i){var _13j=E(_13i);switch(_13j[0]){case 0:_13e=_13b;_13f=[2,_13c,new T(function(){return B(_ea(E(_13j[1])[1]));})];return null;case 2:var _13k=_13j[2];return !B(_Y(_13k,_131))?[0,_u,new T(function(){return B(_135(E(_13h)[1],B(_132(_13k))));})]:[0,_114,new T(function(){return B(_135(E(_13h)[1],B(_19(0,B(_e4(_13k)),_u))));})];default:return E(_110);}})(_13e,_13f);if(_13g!=null){return _13g;}}},_13l=[1,_s7,_u],_13m=function(_13n){return new F(function(){return err(B(unAppCStr("Char.digitToInt: not a digit ",new T(function(){var _13o=E(_13n);return _13o==39?E(_s9):[1,_s7,new T(function(){return B(_rR(_13o,_13l));})];}))));});},_13p=function(_13q){var _13r=function(_13s){var _13t=function(_13u){if(_13q<65){return new F(function(){return _13m(_13q);});}else{if(_13q>70){return new F(function(){return _13m(_13q);});}else{return (_13q-65|0)+10|0;}}};if(_13q<97){return new F(function(){return _13t(_);});}else{if(_13q>102){return new F(function(){return _13t(_);});}else{return (_13q-97|0)+10|0;}}};if(_13q<48){return new F(function(){return _13r(_);});}else{if(_13q>57){return new F(function(){return _13r(_);});}else{return _13q-48|0;}}},_13v=function(_13w,_13x){while(1){var _13y=(function(_13z,_13A){var _13B=E(_13A);if(!_13B[0]){return [0,_13z,_u];}else{var _13C=E(_13B[1])[1];if(_13C<48){return [0,_13z,_13B];}else{if(_13C>57){return [0,_13z,_13B];}else{_13w=new T(function(){return [0,(imul(E(_13z)[1],10)|0)+B(_13p(_13C))|0];});_13x=_13B[2];return null;}}}})(_13w,_13x);if(_13y!=null){return _13y;}}},_13D=new T(function(){return B(unCStr("argument list ended prematurely"));}),_13E=new T(function(){return B(_10X(_13D));}),_13F=[0,-1],_13G=function(_13H){return [0,E(_13H)[1]];},_13I=function(_13J){var _13K=E(_13J);switch(_13K[0]){case 0:return new F(function(){return _13G(_13K[1]);});break;case 2:return new F(function(){return _PQ(_13K[2]);});break;default:return E(_110);}},_13L=function(_13M,_13N,_13O,_13P,_13Q){while(1){var _13R=(function(_13S,_13T,_13U,_13V,_13W){var _13X=E(_13V);if(!_13X[0]){return [0,_13b,_13F,_13S,_13T,_13U,_u,_13W];}else{var _13Y=_13X[2],_13Z=E(E(_13X[1])[1]);switch(_13Z){case 42:var _140=new T(function(){var _141=E(_13W);return _141[0]==0?E(_13E):[0,_141[2],new T(function(){return B(_13I(_141[1]));})];}),_142=new T(function(){var _143=E(_13Y);if(!_143[0]){var _144=[0,_13F,_u,new T(function(){return E(E(_140)[1]);})];}else{if(E(E(_143[1])[1])==46){var _145=E(_143[2]);if(!_145[0]){var _146=B(_13v(_13b,_u)),_147=[0,_146[1],_146[2],new T(function(){return E(E(_140)[1]);})];}else{if(E(E(_145[1])[1])==42){var _148=new T(function(){var _149=E(E(_140)[1]);return _149[0]==0?E(_13E):[0,_149[2],new T(function(){return B(_13I(_149[1]));})];}),_14a=[0,new T(function(){return E(E(_148)[2]);}),_145[2],new T(function(){return E(E(_148)[1]);})];}else{var _14b=B(_13v(_13b,_145)),_14a=[0,_14b[1],_14b[2],new T(function(){return E(E(_140)[1]);})];}var _14c=_14a,_147=_14c;}var _14d=_147;}else{var _14d=[0,_13F,_143,new T(function(){return E(E(_140)[1]);})];}var _14e=_14d,_144=_14e;}return _144;});return [0,new T(function(){return E(E(_140)[2]);}),new T(function(){return E(E(_142)[1]);}),_13S,_13T,_13U,new T(function(){return E(E(_142)[2]);}),new T(function(){return E(E(_142)[3]);})];case 43:var _14f=_13S,_14g=_13T;_13O=_n;_13P=_13Y;var _14h=_13W;_13M=_14f;_13N=_14g;_13Q=_14h;return null;case 45:_13M=_n;var _14g=_13T,_14i=_13U;_13P=_13Y;var _14h=_13W;_13N=_14g;_13O=_14i;_13Q=_14h;return null;case 46:var _14j=new T(function(){var _14k=E(_13Y);if(!_14k[0]){var _14l=B(_13v(_13b,_u)),_14m=[0,_14l[1],_14l[2],_13W];}else{if(E(E(_14k[1])[1])==42){var _14n=new T(function(){var _14o=E(_13W);return _14o[0]==0?E(_13E):[0,_14o[2],new T(function(){return B(_13I(_14o[1]));})];}),_14p=[0,new T(function(){return E(E(_14n)[2]);}),_14k[2],new T(function(){return E(E(_14n)[1]);})];}else{var _14q=B(_13v(_13b,_14k)),_14p=[0,_14q[1],_14q[2],_13W];}var _14r=_14p,_14m=_14r;}return _14m;});return [0,_13b,new T(function(){return E(E(_14j)[1]);}),_13S,_13T,_13U,new T(function(){return E(E(_14j)[2]);}),new T(function(){return E(E(_14j)[3]);})];case 48:var _14f=_13S;_13N=_n;var _14i=_13U;_13P=_13Y;var _14h=_13W;_13M=_14f;_13O=_14i;_13Q=_14h;return null;default:if(_13Z<48){return [0,_13b,_13F,_13S,_13T,_13U,_13X,_13W];}else{if(_13Z>57){return [0,_13b,_13F,_13S,_13T,_13U,_13X,_13W];}else{var _14s=new T(function(){var _14t=B(_13v(_13b,_13X));return [0,_14t[1],_14t[2]];}),_14u=new T(function(){var _14v=E(E(_14s)[2]);if(!_14v[0]){var _14w=[0,_13F,_u,_13W];}else{if(E(E(_14v[1])[1])==46){var _14x=E(_14v[2]);if(!_14x[0]){var _14y=B(_13v(_13b,_u)),_14z=[0,_14y[1],_14y[2],_13W];}else{if(E(E(_14x[1])[1])==42){var _14A=new T(function(){var _14B=E(_13W);return _14B[0]==0?E(_13E):[0,_14B[2],new T(function(){return B(_13I(_14B[1]));})];}),_14C=[0,new T(function(){return E(E(_14A)[2]);}),_14x[2],new T(function(){return E(E(_14A)[1]);})];}else{var _14D=B(_13v(_13b,_14x)),_14C=[0,_14D[1],_14D[2],_13W];}var _14E=_14C,_14z=_14E;}var _14F=_14z;}else{var _14F=[0,_13F,_14v,_13W];}var _14G=_14F,_14w=_14G;}var _14H=_14w;return _14H;});return [0,new T(function(){return E(E(_14s)[1]);}),new T(function(){return E(E(_14u)[1]);}),_13S,_13T,_13U,new T(function(){return E(E(_14u)[2]);}),new T(function(){return E(E(_14u)[3]);})];}}}}})(_13M,_13N,_13O,_13P,_13Q);if(_13R!=null){return _13R;}}},_14I=new T(function(){return B(unCStr("formatting string ended prematurely"));}),_14J=new T(function(){return B(_10X(_14I));}),_14K=function(_14L,_14M){if(!B(_Y(_14M,_14L))){if(!B(_yL(_14L,_131))){var _14N=B(_RN(_14M,_14L));return new F(function(){return _O(B(_14K(_14L,_14N[1])),[1,new T(function(){return [0,B(_UR(B(_fn(_14N[2]))))];}),_u]);});}else{return E(_vY);}}else{return [1,new T(function(){return [0,B(_UR(B(_fn(_14M))))];}),_u];}},_14O=[0,2],_14P=function(_14Q,_14R,_14S){var _14T=E(_14S);switch(_14T[0]){case 0:return new F(function(){return _14K(_14Q,B(_ea(E(_14T[1])[1])));});break;case 2:var _14U=_14T[2],_14V=E(_14R)[1];if(!B(_Y(_14U,_131))){return new F(function(){return _135(_14V,B(_14K(_14Q,_14U)));});}else{return new F(function(){return _135(_14V,B(_14K(_14Q,B(_dU(B(_e4(B(_ec(_14O,_14T[1])))),_14U)))));});}break;default:return E(_110);}},_14W=[0,37],_14X=[0,16],_14Y=[0,10],_14Z=[0,8],_150=[0,43],_151=[1,_150,_u],_152=[0,32],_153=function(_154){return new F(function(){return _10X(new T(function(){return B(unAppCStr("bad formatting char ",[1,_154,_u]));}));});},_155=function(_156,_157){var _158=E(_156);if(!_158){return [0];}else{var _159=E(_157);return _159[0]==0?[0]:[1,_159[1],new T(function(){return B(_155(_158-1|0,_159[2]));})];}},_15a=function(_15b,_15c){var _15d=E(_15b);if(!_15d[0]){return E(_15c)[0]==0?[0]:E(_14J);}else{var _15e=_15d[2],_15f=E(_15d[1]);if(E(_15f[1])==37){var _15g=function(_15h){var _15i=E(_15c);if(!_15i[0]){return E(_13E);}else{var _15j=B(_13L(_r,_r,_r,_15e,_15i)),_15k=_15j[2],_15l=_15j[4],_15m=E(_15j[6]);if(!_15m[0]){return E(_14J);}else{var _15n=_15m[2],_15o=E(_15j[7]);if(!_15o[0]){return E(_13E);}else{var _15p=_15o[1],_15q=_15o[2],_15r=E(_15m[1]),_15s=function(_15t,_15u){var _15v=new T(function(){var _15w=B(_EV(_15u,0)),_15x=B(_EV(_15t,0)),_15y=E(_15j[1])[1];if((_15w+_15x|0)>=_15y){var _15z=[0];}else{var _15A=_15y-(_15w+_15x|0)|0;if(_15A>0){if(_15A<0){var _15B=[0];}else{var _15C=new T(function(){return [1,new T(function(){return !E(_15l)?E(_152):E(_134);}),_15C];}),_15B=B(_155(_15A,_15C));}var _15D=_15B,_15E=_15D;}else{var _15E=[0];}var _15F=_15E,_15G=_15F,_15H=_15G,_15z=_15H;}var _15I=_15z,_15J=_15I,_15K=_15J,_15L=_15K,_15M=_15L;return _15M;});return !E(_15j[3])?!E(_15l)?B(_O(_15v,new T(function(){return B(_O(_15t,_15u));}))):B(_O(_15t,new T(function(){return B(_O(_15v,_15u));}))):B(_O(_15t,new T(function(){return B(_O(_15u,_15v));})));},_15N=function(_15O,_15P){var _15Q=E(_15O);return _15Q[0]==0?!E(_15j[5])?B(_15s(_u,_15P)):B(_15s(_151,_15P)):B(_15s(_15Q,_15P));};switch(E(_15r[1])){case 69:var _15R=B(_12g(69,_15k,_15p));return new F(function(){return _O(B(_15N(_15R[1],_15R[2])),new T(function(){return B(_15a(_15n,_15q));}));});break;case 71:var _15S=B(_12g(71,_15k,_15p));return new F(function(){return _O(B(_15N(_15S[1],_15S[2])),new T(function(){return B(_15a(_15n,_15q));}));});break;case 88:return new F(function(){return _O(B(_15s(_u,new T(function(){return B(_1q(_12e,B(_14P(_14X,_15k,_15p))));}))),new T(function(){return B(_15a(_15n,_15q));}));});break;case 99:return new F(function(){return _O(B(_15s(_u,[1,new T(function(){var _15T=E(_15p);switch(_15T[0]){case 0:var _15U=E(_15T[1])[1];if(_15U>>>0>1114111){var _15V=B(_fl(_15U));}else{var _15V=[0,_15U];}var _15W=_15V,_15X=_15W,_15Y=_15X,_15Z=_15Y,_160=_15Z;break;case 2:var _161=B(_fn(_15T[2]));if(_161>>>0>1114111){var _162=B(_fl(_161));}else{var _162=[0,_161];}var _163=_162,_164=_163,_165=_164,_160=_165;break;default:var _160=E(_110);}return _160;}),_u])),new T(function(){return B(_15a(_15n,_15q));}));});break;case 100:var _166=B(_13d(_15k,_15p));return new F(function(){return _O(B(_15N(_166[1],_166[2])),new T(function(){return B(_15a(_15n,_15q));}));});break;case 101:var _167=B(_12g(101,_15k,_15p));return new F(function(){return _O(B(_15N(_167[1],_167[2])),new T(function(){return B(_15a(_15n,_15q));}));});break;case 102:var _168=B(_12g(102,_15k,_15p));return new F(function(){return _O(B(_15N(_168[1],_168[2])),new T(function(){return B(_15a(_15n,_15q));}));});break;case 103:var _169=B(_12g(103,_15k,_15p));return new F(function(){return _O(B(_15N(_169[1],_169[2])),new T(function(){return B(_15a(_15n,_15q));}));});break;case 105:var _16a=B(_13d(_15k,_15p));return new F(function(){return _O(B(_15N(_16a[1],_16a[2])),new T(function(){return B(_15a(_15n,_15q));}));});break;case 111:return new F(function(){return _O(B(_15s(_u,new T(function(){return B(_14P(_14Z,_15k,_15p));}))),new T(function(){return B(_15a(_15n,_15q));}));});break;case 115:return new F(function(){return _O(B(_15s(_u,new T(function(){var _16b=E(_15p);if(_16b[0]==1){var _16c=_16b[1],_16d=E(_15k)[1];if(_16d<0){var _16e=E(_16c);}else{var _16e=_16d>0?B(_155(_16d,_16c)):[0];}var _16f=_16e,_16g=_16f,_16h=_16g;}else{var _16h=E(_110);}return _16h;}))),new T(function(){return B(_15a(_15n,_15q));}));});break;case 117:return new F(function(){return _O(B(_15s(_u,new T(function(){return B(_14P(_14Y,_15k,_15p));}))),new T(function(){return B(_15a(_15n,_15q));}));});break;case 120:return new F(function(){return _O(B(_15s(_u,new T(function(){return B(_14P(_14X,_15k,_15p));}))),new T(function(){return B(_15a(_15n,_15q));}));});break;default:return new F(function(){return _153(_15r);});}}}}},_16i=E(_15e);if(!_16i[0]){return new F(function(){return _15g(_);});}else{if(E(E(_16i[1])[1])==37){return [1,_14W,new T(function(){return B(_15a(_16i[2],_15c));})];}else{return new F(function(){return _15g(_);});}}}else{return [1,_15f,new T(function(){return B(_15a(_15e,_15c));})];}}},_H0=function(_16j,_){var _16k=jsFind(toJSStr(E(_0))),_16l=_16k,_16m=E(_16l);if(!_16m[0]){return new F(function(){return _nr(_Co,_);});}else{var _16n=jsSet(E(_16m[1])[1],toJSStr(E(_xh)),toJSStr(B(_O(_Jd,new T(function(){return B(unAppCStr("<tbody>",new T(function(){var _16o=function(_16p){var _16q=E(_16p);if(!_16q[0]){return [0];}else{var _16r=E(_16q[1]),_16s=function(_16t){var _16u=E(_16t);return _16u[0]==0?E(new T(function(){return B(_16o(_16q[2]));})):[1,_16u[1],new T(function(){return B(_16s(_16u[2]));})];};return new F(function(){return _16s(B(_15a(_Jc,new T(function(){return B(_Jf([1,[1,new T(function(){return B(_1q(_ve,_16r[2]));})],[1,[1,new T(function(){return B(_1q(_ve,_16r[1]));})],_u]],_u));}))));});}};return B(_O(B(_16o(B(_1q(function(_16v){var _16w=E(_16v)[1],_16x=B(_wA(_16w,new T(function(){return E(E(_16j)[6]);})));if(!_16x[0]){return [0,_16w,_u];}else{var _16y=E(_16x[1]);return _16y[0]==0?[0,_16w,_u]:[0,_16w,_16y[1]];}},_Jb)))),_Je));})));})))));return [0,_cG,_16j];}},_16z=new T(function(){return B(unCStr("night"));}),_16A=function(_){var _16B=B(A(_Dk,["document.body",_])),_16C=_16B;return [0,_16C];},_16D=function(_){return new F(function(){return _16A(_);});},_16E=function(_){var _=0;return new F(function(){return _16D(_);});},_16F=new T(function(){return B(_o9(_16E));}),_16G=new T(function(){return B(_Dk("(function(e,c,x){x?e.classList.add(c):e.classList.remove(c);})"));}),_16H=function(_){var _=0;return new F(function(){return A(_Dk,["false",_]);});},_16I=new T(function(){return B(_o9(_16H));}),_16J=function(_){var _=0;return new F(function(){return A(_Dk,["true",_]);});},_16K=new T(function(){return B(_o9(_16J));}),_16L=function(_16M){return function(_16N){return function(_16O,_){var _16P=B(A(new T(function(){return B(A(new T(function(){return B(A(_16G,[E(E(_16M)[1])]));}),[E(toJSStr(E(_16N)))]));}),[!E(_16O)?E(_16I):E(_16K),_])),_16Q=_16P;return _cG;};};},_16R=new T(function(){return B(_16L(_16F));}),_16S=new T(function(){return B(A(_16R,[_16z,_n]));}),_16T=function(_16U,_){var _16V=new T(function(){var _16W=E(_16U);return [0,_16W[1],_16W[2],_16W[3],_16W[4],_16W[5],_16W[6],new T(function(){return B(_6T(_16z,_nu,_16W[7]));}),_16W[8],_16W[9],_16W[10],_16W[11]];}),_16X=B(A(_Dt,[_16V,_])),_16Y=_16X,_16Z=B(_GY(_16V,_)),_170=_16Z,_171=B(_H0(new T(function(){return E(E(_170)[2]);}),_)),_172=_171,_173=B(A(_16S,[_])),_174=_173;return [0,_174,new T(function(){return E(E(_172)[2]);})];},_175=function(_176,_177,_){return new F(function(){return _16T(_177,_);});},_178=new T(function(){return [0,_Cm,_175,_Cl];}),_179=new T(function(){return [0,_16z,_178];}),_17a=new T(function(){return [1,_179,_u];}),_17b=function(_17c){return E(_H9);},_17d=new T(function(){return B(unCStr("\u30c7\u30fc\u30bf\u306e\u6d88\u53bb"));}),_17e=new T(function(){return B(unCStr("\u30c7\u30fc\u30bf\u306e\u6d88\u53bb<br>\u5168\u3066\u306e\u30c7\u30fc\u30bf\u304c\u6d88\u53bb\u3055\u308c\u307e\u3059\u3002\u3053\u306e\u64cd\u4f5c\u306f\u53d6\u308a\u6d88\u305b\u307e\u305b\u3093\u3002"));}),_17f=new T(function(){return B(unCStr("fa-trash"));}),_17g=[0,_17f,_17e,_17d],_17h=new T(function(){return B(unCStr("none"));}),_17i=function(_){var _17j=B(_nX(_)),_17k=_17j,_17l=B(A(_Dt,[_17k,_])),_17m=_17l,_17n=B(_GY(_17k,_)),_17o=_17n,_17p=B(_H0(new T(function(){return E(E(_17o)[2]);}),_)),_17q=_17p,_17r=E(_8V),_17s=jsFind(toJSStr(_17r)),_17t=_17s,_17u=E(_17t);if(!_17u[0]){return new F(function(){return _BI(_17r);});}else{var _17v=B(A(_wR,[_xO,_17u[1],_xg,_17h,new T(function(){return E(E(_17q)[2]);}),_])),_17w=_17v,_17x=E(_C7),_17y=jsFind(toJSStr(_17x)),_17z=_17y,_17A=E(_17z);return _17A[0]==0?B(_BI(_17x)):B(A(_wR,[_xO,_17A[1],_xg,_17h,new T(function(){return E(E(_17w)[2]);}),_]));}},_17B=function(_17C,_17D,_){return new F(function(){return _17i(_);});},_17E=new T(function(){return [0,_17b,_17B,_17g];}),_17F=new T(function(){return [0,_96,_17E];}),_17G=new T(function(){return [1,_17F,_17a];}),_17H=function(_17I){return E(_Hz);},_17J=function(_17K,_){var _17L=B(_nX(_)),_17M=_17L,_17N=new T(function(){var _17O=E(_17M);return [0,_17O[1],_17O[2],_17O[3],_17O[4],_17O[5],new T(function(){return E(E(_17K)[6]);}),_17O[7],_17O[8],_17O[9],_17O[10],_17O[11]];}),_17P=B(A(_Dt,[_17N,_])),_17Q=_17P,_17R=B(_GY(_17N,_)),_17S=_17R,_17T=B(_H0(new T(function(){return E(E(_17S)[2]);}),_)),_17U=_17T,_17V=E(_8V),_17W=jsFind(toJSStr(_17V)),_17X=_17W,_17Y=E(_17X);if(!_17Y[0]){return new F(function(){return _BI(_17V);});}else{var _17Z=B(A(_wR,[_xO,_17Y[1],_xg,_17h,new T(function(){return E(E(_17U)[2]);}),_])),_180=_17Z,_181=E(_C7),_182=jsFind(toJSStr(_181)),_183=_182,_184=E(_183);return _184[0]==0?B(_BI(_181)):B(A(_wR,[_xO,_184[1],_xg,_17h,new T(function(){return E(E(_180)[2]);}),_]));}},_185=function(_186,_187,_){return new F(function(){return _17J(_187,_);});},_188=new T(function(){return B(unCStr("\u521d\u671f\u5316"));}),_189=new T(function(){return B(unCStr("\u521d\u671f\u5316<br>\u5b9f\u7e3e\u3092\u9664\u304f\u5168\u3066\u306e\u30c7\u30fc\u30bf\u304c\u521d\u671f\u5316\u3055\u308c\u307e\u3059"));}),_18a=new T(function(){return B(unCStr("fa-history"));}),_18b=[0,_18a,_189,_188],_18c=new T(function(){return [0,_17H,_185,_18b];}),_18d=new T(function(){return [0,_9a,_18c];}),_18e=new T(function(){return [1,_18d,_17G];}),_18f=new T(function(){return B(unCStr("save"));}),_18g=new T(function(){return B(unCStr("\u30bb\u30fc\u30d6\u3057\u307e\u3057\u305f"));}),_18h=new T(function(){return B(unCStr("auto"));}),_18i=new T(function(){return B(_Gb(_18h,_18g));}),_18j=function(_18k,_){var _18l=new T(function(){var _18m=E(_18k);return [0,_18m[1],_18m[2],_18m[3],_18m[4],_18m[5],_18m[6],new T(function(){return B(_6T(_18f,_nu,_18m[7]));}),_18m[8],_18m[9],_18m[10],_18m[11]];}),_18n=B(A(_Dt,[_18l,_])),_18o=_18n,_18p=B(_GY(_18l,_)),_18q=_18p,_18r=B(_H0(new T(function(){return E(E(_18q)[2]);}),_)),_18s=_18r;return new F(function(){return A(_18i,[new T(function(){return E(E(_18s)[2]);}),_]);});},_18t=function(_18u,_18v,_){return new F(function(){return _18j(_18v,_);});},_18w=new T(function(){return B(unCStr("\u30bb\u30fc\u30d6"));}),_18x=new T(function(){return B(unCStr("\u30bb\u30fc\u30d6<br>\u30bb\u30fc\u30d6\u3057\u307e\u3059\u3002"));}),_18y=new T(function(){return B(unCStr("fa-save"));}),_18z=[0,_18y,_18x,_18w],_18A=new T(function(){return [0,_BW,_18t,_18z];}),_18B=new T(function(){return [0,_18f,_18A];}),_18C=new T(function(){return [1,_18B,_18e];}),_18D=new T(function(){return [1,_Cg,_18C];}),_18E=new T(function(){return [1,_C3,_18D];}),_18F=new T(function(){return [1,_BB,_18E];}),_18G=new T(function(){return [1,_B8,_18F];}),_18H=new T(function(){return [1,_AD,_18G];}),_18I=new T(function(){return [1,_Aq,_18H];}),_18J=new T(function(){return [1,_Ad,_18I];}),_18K=new T(function(){return [1,_A1,_18J];}),_18L=new T(function(){return [1,_zP,_18K];}),_18M=new T(function(){return [1,_zD,_18L];}),_I6=new T(function(){return [1,_zr,_18M];}),_18N=new T(function(){return B(_1q(_xX,_I6));}),_18O=function(_18P){return new F(function(){return _co(_18P,_8V);});},_18Q=function(_18R,_18S){var _18T=E(_18S);if(!_18T[0]){return [0];}else{var _18U=_18T[1];return !B(A(_18R,[_18U]))?[0]:[1,_18U,new T(function(){return B(_18Q(_18R,_18T[2]));})];}},_IC=new T(function(){return B(_18Q(_18O,_18N));}),_18V=new T(function(){return B(_Dk("(function(e,c){e.removeAttribute(c);})"));}),_18W=function(_18X){return function(_18Y,_){var _18Z=B(A(new T(function(){return B(A(_18V,[E(E(_18X)[1])]));}),[E(toJSStr(E(_18Y))),_])),_190=_18Z;return _cG;};},_191=function(_192,_193){var _194=E(_193);if(!_194[0]){return [0,_u,_u];}else{var _195=_194[1];if(!B(A(_192,[_195]))){var _196=new T(function(){var _197=B(_191(_192,_194[2]));return [0,_197[1],_197[2]];});return [0,[1,_195,new T(function(){return E(E(_196)[1]);})],new T(function(){return E(E(_196)[2]);})];}else{return [0,_u,_194];}}},_198=function(_199,_19a){var _19b=function(_19c,_19d){return !B(_c7(_19d,_u))?[0,_19c,new T(function(){var _19e=B(_198(_199,_19d));return [1,_19e[1],_19e[2]];})]:[0,_19c,_u];};if(_199>=0){var _19f=B(_YS(_199,_19a));return new F(function(){return _19b(_19f[1],_19f[2]);});}else{return new F(function(){return _19b(_u,_19a);});}},_19g=[0,44],_19h=[1,_19g,_u],_19i=function(_19j){return E(E(_19j)[1])==46?true:false;},_19k=function(_19l,_19m){var _19n=E(_19m);return _19n[0]==0?[0]:[1,_19l,[1,_19n[1],new T(function(){return B(_19k(_19l,_19n[2]));})]];},_19o=function(_19p){var _19q=new T(function(){var _19r=B(_191(_19i,_19p));return [0,_19r[1],_19r[2]];}),_19s=B(_198(3,new T(function(){return B(_Jf(E(_19q)[1],_u));})));return new F(function(){return _O(B(_Jf(B(_x7([1,_19s[1],new T(function(){return B(_19k(_19h,_19s[2]));})])),_u)),new T(function(){return E(E(_19q)[2]);}));});},_19t=function(_19u,_19v){var _19w=E(_19u);if(!_19w[0]){return [0];}else{var _19x=E(_19v);return _19x[0]==0?[0]:[1,[0,_19w[1],_19x[1]],new T(function(){return B(_19t(_19w[2],_19x[2]));})];}},_GY=function(_19y,_){var _19z=new T(function(){return E(E(_19y)[7]);}),_19A=function(_19B){var _19C=E(_19B);return _19C[0]==0?E(_xQ):function(_19D,_){var _19E=B(A(new T(function(){var _19F=E(_19C[1]),_19G=_19F[1],_19H=E(_19F[2])[1],_19I=new T(function(){return B(unAppCStr("item-",_19G));}),_19J=new T(function(){var _19K=B(_wA(_19G,_19z));return _19K[0]==0?E(_nu):E(_19K[1]);});return function(_19L,_){var _19M=B(A(new T(function(){if(!B(_eR(_cr,_19G,_IC))){var _19N=E(_xQ);}else{var _19N=function(_19O,_){var _19P=E(new T(function(){return B(_O(_19I,_xU));})),_19Q=jsFind(toJSStr(_19P)),_19R=_19Q,_19S=E(_19R);if(!_19S[0]){return new F(function(){return _BI(_19P);});}else{var _19T=jsFind(toJSStr(E(new T(function(){return B(_O(_19I,_xW));})))),_19U=_19T,_19V=E(_19U);if(!_19V[0]){return new F(function(){return _nr(_x6,_);});}else{var _19W=E(_xh),_19X=jsGet(E(_19V[1])[1],toJSStr(_19W)),_19Y=_19X,_19Z=new T(function(){return fromJSStr(_19Y);}),_1a0=function(_1a1){return _1a1>1?[1,_19Z,new T(function(){return B(_1a0(_1a1-1|0));})]:E([1,_19Z,_u]);},_1a2=jsSet(E(_19S[1])[1],toJSStr(_19W),toJSStr(B((function(_1a3,_1a4){while(1){var _1a5=(function(_1a6,_1a7){var _1a8=E(_1a7);if(!_1a8[0]){return E(_1a6);}else{var _1a9=E(_1a8[1]);_1a3=B(_1q(_ve,B(_15a(_xi,new T(function(){return B(_Jf([1,[1,new T(function(){var _1aa=E(_1a9[2])[1];if(_1aa>0){var _1ab=B(_1q(_ve,B(_x7(B(_1a0(_1aa))))));}else{var _1ab=E(_xl);}var _1ac=_1ab,_1ad=_1ac;return _1ad;})],[1,[2,_xj,new T(function(){return B(_ea(E(_1a9[1])[1]));})],[1,[1,new T(function(){return B(_1q(_ve,_1a6));})],_u]]],_u));})))));_1a4=_1a8[2];return null;}})(_1a3,_1a4);if(_1a5!=null){return _1a5;}}})(_u,new T(function(){return B(_19t(_xa,new T(function(){return B(_Jf(B(_ws(_19J,_u,_x5,_x4))[2],_u));})));}))))),_1ae=E(new T(function(){return B(_O(_19I,_xV));})),_1af=jsFind(toJSStr(_1ae)),_1ag=_1af,_1ah=E(_1ag);return _1ah[0]==0?B(_BI(_1ae)):B(A(_wL,[_xO,_1ah[1],_19W,new T(function(){return B(_81(0,E(_19J)[1],_u));}),_19O,_]));}}};}return _19N;}),[_19L,_])),_1ai=_19M,_1aj=E(new T(function(){return B(_O(_19I,_xT));})),_1ak=jsFind(toJSStr(_1aj)),_1al=_1ak,_1am=E(_1al);if(!_1am[0]){return new F(function(){return _BI(_1aj);});}else{var _1an=_1am[1];if(!E(new T(function(){if(!B(_eR(_cr,_19G,_IC))){if(!B(_v9(_19G,_19z))){var _1ao=B(_xb(B(A(_19H,[_19J]))))<=E(E(_19y)[1])[1];}else{if(B(_uT(_19G,_19z))[1]>=1){var _1ap=false;}else{var _1ap=B(_xb(B(A(_19H,[_19J]))))<=E(E(_19y)[1])[1];}var _1aq=_1ap,_1ar=_1aq,_1ao=_1ar;}var _1as=_1ao;}else{var _1as=B(_xb(B(A(_19H,[_19J]))))<=E(E(_19y)[1])[1];}return _1as;}))){var _1at=B(A(_wF,[_xO,_1an,_xe,_xe,new T(function(){return E(E(_1ai)[2]);}),_])),_1au=_1at,_1av=B(_O(_19I,_xS)),_1aw=jsFind(toJSStr(_1av)),_1ax=_1aw,_1ay=E(_1ax);if(!_1ay[0]){return new F(function(){return _BI(_1av);});}else{var _1az=jsSet(E(_1ay[1])[1],toJSStr(E(_xh)),toJSStr(B(_19o(new T(function(){return B(_19(0,B(A(_19H,[_19J])),_u));}))))),_1aA=function(_1aB,_){var _1aC=E(_19I),_1aD=jsFind(toJSStr(_1aC)),_1aE=_1aD,_1aF=E(_1aE);if(!_1aF[0]){return new F(function(){return _BI(_1aC);});}else{var _1aG=E(_1aF[1]),_1aH=E(_xg),_1aI=jsGetStyle(_1aG[1],toJSStr(_1aH)),_1aJ=_1aI;return !B(_cj(fromJSStr(_1aJ),_xf))?B(A(_wR,[_xO,_1aG,_1aH,_xf,_1aB,_])):[0,_cG,_1aB];}};if(!B(_v9(_19G,_19z))){var _1aK=E(E(_1au)[2]);return B(_xb(B(A(_19H,[_nu]))))>3*E(_1aK[8])[1]?[0,_cG,_1aK]:B(_1aA(_1aK,_));}else{return new F(function(){return _1aA(new T(function(){return E(E(_1au)[2]);}),_);});}}}else{var _1aL=B(A(_18W,[_1an,_xe,_])),_1aM=_1aL,_1aN=B(_O(_19I,_xS)),_1aO=jsFind(toJSStr(_1aN)),_1aP=_1aO,_1aQ=E(_1aP);if(!_1aQ[0]){return new F(function(){return _BI(_1aN);});}else{var _1aR=jsSet(E(_1aQ[1])[1],toJSStr(E(_xh)),toJSStr(B(_19o(new T(function(){return B(_19(0,B(A(_19H,[_19J])),_u));}))))),_1aS=function(_1aT,_){var _1aU=E(_19I),_1aV=jsFind(toJSStr(_1aU)),_1aW=_1aV,_1aX=E(_1aW);if(!_1aX[0]){return new F(function(){return _BI(_1aU);});}else{var _1aY=E(_1aX[1]),_1aZ=E(_xg),_1b0=jsGetStyle(_1aY[1],toJSStr(_1aZ)),_1b1=_1b0;return !B(_cj(fromJSStr(_1b1),_xf))?B(A(_wR,[_xO,_1aY,_1aZ,_xf,_1aT,_])):[0,_cG,_1aT];}};if(!B(_v9(_19G,_19z))){var _1b2=E(E(_1ai)[2]);return B(_xb(B(A(_19H,[_nu]))))>3*E(_1b2[8])[1]?[0,_cG,_1b2]:B(_1aS(_1b2,_));}else{return new F(function(){return _1aS(new T(function(){return E(E(_1ai)[2]);}),_);});}}}}};}),[_19D,_])),_1b3=_19E;return new F(function(){return A(new T(function(){return B(_19A(_19C[2]));}),[new T(function(){return E(E(_1b3)[2]);}),_]);});};};return new F(function(){return A(_19A,[_I6,_19y,_]);});},_1b4=function(_1b5,_1b6,_1b7){var _1b8=E(_1b5),_1b9=E(_1b7);if(!_1b9[0]){var _1ba=_1b9[2],_1bb=_1b9[3],_1bc=_1b9[4],_1bd=_1b9[5];switch(B(_3X(_1b8,_1ba))){case 0:return new F(function(){return _50(_1ba,_1bb,B(_1b4(_1b8,_1b6,_1bc)),_1bd);});break;case 1:return [0,_1b9[1],E(_1b8),new T(function(){return B(_PG(_1b6,_1bb));}),E(_1bc),E(_1bd)];default:return new F(function(){return _49(_1ba,_1bb,_1bc,B(_1b4(_1b8,_1b6,_1bd)));});}}else{return [0,1,E(_1b8),_1b6,E(_44),E(_44)];}},_1be=new T(function(){return [0,"click"];}),_1bf=new T(function(){return B(unCStr("\u300d\u3092\u8cfc\u5165\u3057\u307e\u3057\u305f"));}),_1bg=[0,12300],_1bh=function(_1bi,_1bj,_){var _1bk=E(_1bj);if(!_1bk[0]){return _cG;}else{var _1bl=_1bk[1],_1bm=jsFind(toJSStr(B(unAppCStr("item-",new T(function(){return B(_O(_1bl,_xT));}))))),_1bn=_1bm,_1bo=E(_1bn);if(!_1bo[0]){return _cG;}else{var _1bp=E(_1be)[1],_1bq=jsSetCB(E(_1bo[1])[1],_1bp,function(_1br,_1bs,_){var _1bt=E(_1bi)[1],_1bu=rMV(_1bt),_1bv=_1bu,_1bw=E(new T(function(){return B(_uT(_1bl,_I5));})),_1bx=new T(function(){var _1by=E(_1bv);return [0,_1by[1],_1by[2],_1by[3],_1by[4],_1by[5],_1by[6],new T(function(){return B(_1b4(_1bl,_9R,_1by[7]));}),_1by[8],_1by[9],_1by[10],_1by[11]];}),_1bz=new T(function(){return B(_uT(_1bl,E(_1bx)[7]));}),_1bA=B(A(_1bw[2],[_1bz,new T(function(){var _1bB=E(_1bx);return [0,new T(function(){return [0,E(_1bB[1])[1]-B(_xb(B(A(_1bw[1],[new T(function(){return [0,E(_1bz)[1]-1|0];})]))))];}),_1bB[2],_1bB[3],_1bB[4],_1bB[5],_1bB[6],_1bB[7],_1bB[8],_1bB[9],_1bB[10],_1bB[11]];}),_])),_1bC=_1bA,_1bD=B(A(_FG,[_Du,[1,_1bg,new T(function(){return B(_O(E(_1bw[3])[3],_1bf));})],new T(function(){return E(E(_1bC)[2]);}),_])),_1bE=_1bD,_1bF=new T(function(){return E(E(_1bE)[2]);}),_1bG=B(A(_Dt,[_1bF,_])),_1bH=_1bG,_1bI=B(_GY(_1bF,_)),_1bJ=_1bI,_1bK=B(_H0(new T(function(){return E(E(_1bJ)[2]);}),_)),_1bL=_1bK,_=wMV(_1bt,new T(function(){return E(E(_1bL)[2]);})),_1bM=rMV(_1bt),_1bN=_1bM,_1bO=E(_1bN),_1bP=jsLog(toJSStr(B(A(_ux,[0,_1bO[1],_1bO[2],_1bO[3],_1bO[4],_1bO[5],_1bO[6],_1bO[7],_1bO[8],_1bO[9],_1bO[10],_1bO[11],_u]))));return _cG;}),_1bQ=_1bq;return new F(function(){return (function(_1bR,_1bS,_){while(1){var _1bT=(function(_1bU,_1bV,_){var _1bW=E(_1bV);if(!_1bW[0]){return _cG;}else{var _1bX=_1bW[1],_1bY=jsFind(toJSStr(B(unAppCStr("item-",new T(function(){return B(_O(_1bX,_xT));}))))),_1bZ=_1bY,_1c0=E(_1bZ);if(!_1c0[0]){return _cG;}else{var _1c1=jsSetCB(E(_1c0[1])[1],_1bp,function(_1c2,_1c3,_){var _1c4=E(_1bU)[1],_1c5=rMV(_1c4),_1c6=_1c5,_1c7=E(new T(function(){return B(_uT(_1bX,_I5));})),_1c8=new T(function(){var _1c9=E(_1c6);return [0,_1c9[1],_1c9[2],_1c9[3],_1c9[4],_1c9[5],_1c9[6],new T(function(){return B(_1b4(_1bX,_9R,_1c9[7]));}),_1c9[8],_1c9[9],_1c9[10],_1c9[11]];}),_1ca=new T(function(){return B(_uT(_1bX,E(_1c8)[7]));}),_1cb=B(A(_1c7[2],[_1ca,new T(function(){var _1cc=E(_1c8);return [0,new T(function(){return [0,E(_1cc[1])[1]-B(_xb(B(A(_1c7[1],[new T(function(){return [0,E(_1ca)[1]-1|0];})]))))];}),_1cc[2],_1cc[3],_1cc[4],_1cc[5],_1cc[6],_1cc[7],_1cc[8],_1cc[9],_1cc[10],_1cc[11]];}),_])),_1cd=_1cb,_1ce=B(A(_FG,[_Du,[1,_1bg,new T(function(){return B(_O(E(_1c7[3])[3],_1bf));})],new T(function(){return E(E(_1cd)[2]);}),_])),_1cf=_1ce,_1cg=new T(function(){return E(E(_1cf)[2]);}),_1ch=B(A(_Dt,[_1cg,_])),_1ci=_1ch,_1cj=B(_GY(_1cg,_)),_1ck=_1cj,_1cl=B(_H0(new T(function(){return E(E(_1ck)[2]);}),_)),_1cm=_1cl,_=wMV(_1c4,new T(function(){return E(E(_1cm)[2]);})),_1cn=rMV(_1c4),_1co=_1cn,_1cp=E(_1co),_1cq=jsLog(toJSStr(B(A(_ux,[0,_1cp[1],_1cp[2],_1cp[3],_1cp[4],_1cp[5],_1cp[6],_1cp[7],_1cp[8],_1cp[9],_1cp[10],_1cp[11],_u]))));return _cG;}),_1cr=_1c1,_1cs=_1bU;_1bS=_1bW[2];_1bR=_1cs;return null;}}})(_1bR,_1bS,_);if(_1bT!=null){return _1bT;}}})(_1bi,_1bk[2],_);});}}},_1ct=function(_1cu){var _1cv=E(_1cu);if(!_1cv[0]){return [0,_u,_u];}else{var _1cw=E(_1cv[1]),_1cx=new T(function(){var _1cy=B(_1ct(_1cv[2]));return [0,_1cy[1],_1cy[2]];});return !B(_eR(_cr,_1cw[1],_IC))?[0,new T(function(){return E(E(_1cx)[1]);}),[1,_1cw,new T(function(){return E(E(_1cx)[2]);})]]:[0,[1,_1cw,new T(function(){return E(E(_1cx)[1]);})],new T(function(){return E(E(_1cx)[2]);})];}},_1cz=new T(function(){var _1cA=B(_1ct(_I6));return [0,_1cA[1],_1cA[2]];}),_1cB=new T(function(){return E(E(_1cz)[1]);}),_1cC=new T(function(){return E(E(_1cz)[2]);}),_1cD=function(_1cE){return _1cE>0;},_1cF=new T(function(){return B(_Dk("(function(x) {return x === null;})"));}),_1cG=new T(function(){return B(unCStr("No such value"));}),_1cH=[0,_1cG],_1cI=new T(function(){return B(unCStr("Invalid JSON!"));}),_1cJ=[0,_1cI],_1cK=new T(function(){return [0,"(function(k) {return localStorage.getItem(k);})"];}),_1cL=function(_1cM,_1cN,_){var _1cO=B(A(_Dk,[E(_1cK)[1],E(toJSStr(E(_1cN))),_])),_1cP=_1cO;return new T(function(){if(!B(_o9(function(_){var _=0,_1cQ=B(A(_1cF,[E(_1cP),_])),_1cR=_1cQ;return new T(function(){return B(_1cD(_1cR));});}))){var _1cS=String(_1cP),_1cT=_1cS,_1cU=jsParseJSON(_1cT),_1cV=_1cU,_1cW=E(_1cV),_1cX=_1cW[0]==0?E(_1cJ):B(A(_2L,[_1cM,_1cW[1]]));}else{var _1cX=E(_1cH);}return _1cX;});},_1cY=[0,10],_1cZ=[1,_1cY,_u],_1d0=function(_1d1,_1d2,_){var _1d3=jsWriteHandle(E(_1d1)[1],toJSStr(E(_1d2)));return _cG;},_1d4=function(_1d5,_1d6,_){var _1d7=E(_1d5),_1d8=jsWriteHandle(_1d7[1],toJSStr(E(_1d6)));return new F(function(){return _1d0(_1d7,_1cZ,_);});},_1d9=new T(function(){return B(unCStr("fa fa-plus-circle"));}),_1da=new T(function(){return B(unCStr(" loves"));}),_1db=new T(function(){return B(unCStr("btn btn-default btn-buy"));}),_1dc=new T(function(){return B(unCStr("item-list"));}),_1dd=new T(function(){return B(unCStr("count"));}),_1de=new T(function(){return B(unCStr("tip"));}),_1df=new T(function(){return B(unCStr("list-group-item tooltips"));}),_1dg=[0,105],_1dh=[1,_1dg,_u],_1di=function(_1dj,_1dk,_1dl,_){var _1dm=jsCreateElem(toJSStr(_1dh)),_1dn=_1dm,_1do=jsAppendChild(_1dn,E(_1dl)[1]),_1dp=[0,_1dn],_1dq=B(A(_1dj,[_1dk,_1dp,_])),_1dr=_1dq;return _1dp;},_1ds=new T(function(){return B(unCStr("li"));}),_1dt=function(_1du,_1dv,_1dw,_){var _1dx=jsCreateElem(toJSStr(E(_1ds))),_1dy=_1dx,_1dz=jsAppendChild(_1dy,E(_1dw)[1]),_1dA=[0,_1dy],_1dB=B(A(_1du,[_1dv,_1dA,_])),_1dC=_1dB;return _1dA;},_1dD=[0,48],_1dE=[1,_1dD,_u],_1dF=new T(function(){return B(unCStr("id"));}),_1dG=function(_1dH,_1dI,_1dJ,_1dK){var _1dL=new T(function(){return B(unAppCStr("item-",_1dH));});return function(_1dM,_){var _1dN=B(_1dt(_DY,function(_1dO,_){var _1dP=B(_DO(_DY,function(_1dQ,_){var _1dR=B(A(_wL,[_dk,_1dQ,_DE,_1dJ,_])),_1dS=_1dR;return _1dQ;},_1dO,_)),_1dT=_1dP,_1dU=B(A(_wF,[_dk,_1dT,_DM,_1de,_])),_1dV=_1dU,_1dW=B(_DO(_DY,function(_1dX,_){var _1dY=B(_DO(_DY,function(_1dZ,_){var _1e0=B(_1di(_Dw,_u,_1dZ,_)),_1e1=_1e0,_1e2=B(A(_wF,[_dk,_1e1,_DM,new T(function(){return B(unAppCStr("fa ",_1dI));}),_])),_1e3=_1e2,_1e4=B(_Dw(_EH,_1dZ,_)),_1e5=_1e4;return _1dZ;},_1dX,_)),_1e6=_1dY,_1e7=B(A(_wF,[_dk,_1e6,_1dF,new T(function(){return B(_O(_1dL,_xW));}),_])),_1e8=_1e7,_1e9=B(_DO(_Dw,_u,_1dX,_)),_1ea=_1e9,_1eb=B(A(_wF,[_dk,_1ea,_1dF,new T(function(){return B(_O(_1dL,_xV));}),_])),_1ec=_1eb;return _1dX;},_1dO,_)),_1ed=_1dW,_1ee=B(A(_wF,[_dk,_1ed,_DM,_1dd,_])),_1ef=_1ee,_1eg=B(_DO(_Dw,_1dK,_1dO,_)),_1eh=_1eg,_1ei=B(A(_wF,[_dk,_1eh,_1dF,new T(function(){return B(_O(_1dL,_xU));}),_])),_1ej=_1ei,_1ek=B(A(_wF,[_dk,_1eh,_DM,_1dc,_])),_1el=_1ek,_1em=B(_Ei(_DY,function(_1en,_){var _1eo=B(_1di(_Dw,_u,_1en,_)),_1ep=_1eo,_1eq=B(A(_wF,[_dk,_1ep,_DM,_1d9,_])),_1er=_1eq,_1es=B(_Dw(_EH,_1en,_)),_1et=_1es,_1eu=B(_DO(_Dw,_1dE,_1en,_)),_1ev=_1eu,_1ew=B(A(_wF,[_dk,_1ev,_1dF,new T(function(){return B(_O(_1dL,_xS));}),_])),_1ex=_1ew,_1ey=B(_Dw(_1da,_1en,_)),_1ez=_1ey;return _1en;},_1dO,_)),_1eA=_1em,_1eB=B(A(_wF,[_dk,_1eA,_Ed,_Ec,_])),_1eC=_1eB,_1eD=B(A(_wF,[_dk,_1eA,_1dF,new T(function(){return B(_O(_1dL,_xT));}),_])),_1eE=_1eD,_1eF=B(A(_wF,[_dk,_1eA,_DM,_1db,_])),_1eG=_1eF;return _1dO;},_1dM,_)),_1eH=_1dN,_1eI=B(A(_wF,[_dk,_1eH,_ED,_1dL,_])),_1eJ=_1eI,_1eK=B(A(_wF,[_dk,_1eH,_DM,_1df,_])),_1eL=_1eK;return _1eH;};},_1eM=[1,_sd,_u],_1eN=new T(function(){return B(unCStr("%.2f"));}),_1eO=function(_1eP,_){var _1eQ=jsEval(toJSStr(B(unAppCStr("document.title = ",[1,_sd,new T(function(){return B(_sf(B(_1q(_ve,B(_15a(_1eN,new T(function(){return B(_Jf([1,[4,new T(function(){return E(E(_1eP)[1]);})],_u],_u));}))))),_1eM));})])))),_1eR=_1eQ;return [0,_cG,_1eP];},_1eS=function(_1eT,_1eU,_1eV,_1eW,_1eX){var _1eY=E(_1eX);if(!_1eY[0]){var _1eZ=new T(function(){var _1f0=B(_1eS(_1eY[1],_1eY[2],_1eY[3],_1eY[4],_1eY[5]));return [0,_1f0[1],_1f0[2]];});return [0,new T(function(){return E(E(_1eZ)[1]);}),new T(function(){return B(_50(_1eU,_1eV,_1eW,E(_1eZ)[2]));})];}else{return [0,[0,_1eU,_1eV],_1eW];}},_1f1=function(_1f2,_1f3,_1f4,_1f5,_1f6){var _1f7=E(_1f5);if(!_1f7[0]){var _1f8=new T(function(){var _1f9=B(_1f1(_1f7[1],_1f7[2],_1f7[3],_1f7[4],_1f7[5]));return [0,_1f9[1],_1f9[2]];});return [0,new T(function(){return E(E(_1f8)[1]);}),new T(function(){return B(_49(_1f3,_1f4,E(_1f8)[2],_1f6));})];}else{return [0,[0,_1f3,_1f4],_1f6];}},_1fa=function(_1fb,_1fc){var _1fd=E(_1fb);if(!_1fd[0]){var _1fe=_1fd[1],_1ff=E(_1fc);if(!_1ff[0]){var _1fg=_1ff[1];if(_1fe<=_1fg){var _1fh=B(_1f1(_1fg,_1ff[2],_1ff[3],_1ff[4],_1ff[5])),_1fi=E(_1fh[1]);return new F(function(){return _50(_1fi[1],_1fi[2],_1fd,_1fh[2]);});}else{var _1fj=B(_1eS(_1fe,_1fd[2],_1fd[3],_1fd[4],_1fd[5])),_1fk=E(_1fj[1]);return new F(function(){return _49(_1fk[1],_1fk[2],_1fj[2],_1ff);});}}else{return E(_1fd);}}else{return E(_1fc);}},_1fl=function(_1fm,_1fn,_1fo,_1fp,_1fq,_1fr){var _1fs=E(_1fm);if(!_1fs[0]){var _1ft=_1fs[1],_1fu=_1fs[2],_1fv=_1fs[3],_1fw=_1fs[4],_1fx=_1fs[5];if((imul(3,_1ft)|0)>=_1fn){if((imul(3,_1fn)|0)>=_1ft){return new F(function(){return _1fa(_1fs,[0,_1fn,E(_1fo),_1fp,E(_1fq),E(_1fr)]);});}else{return new F(function(){return _49(_1fu,_1fv,_1fw,B(_1fl(_1fx,_1fn,_1fo,_1fp,_1fq,_1fr)));});}}else{return new F(function(){return _50(_1fo,_1fp,B(_1fy(_1ft,_1fu,_1fv,_1fw,_1fx,_1fq)),_1fr);});}}else{return [0,_1fn,E(_1fo),_1fp,E(_1fq),E(_1fr)];}},_1fy=function(_1fz,_1fA,_1fB,_1fC,_1fD,_1fE){var _1fF=E(_1fE);if(!_1fF[0]){var _1fG=_1fF[1],_1fH=_1fF[2],_1fI=_1fF[3],_1fJ=_1fF[4],_1fK=_1fF[5];if((imul(3,_1fz)|0)>=_1fG){if((imul(3,_1fG)|0)>=_1fz){return new F(function(){return _1fa([0,_1fz,E(_1fA),_1fB,E(_1fC),E(_1fD)],_1fF);});}else{return new F(function(){return _49(_1fA,_1fB,_1fC,B(_1fl(_1fD,_1fG,_1fH,_1fI,_1fJ,_1fK)));});}}else{return new F(function(){return _50(_1fH,_1fI,B(_1fy(_1fz,_1fA,_1fB,_1fC,_1fD,_1fJ)),_1fK);});}}else{return [0,_1fz,E(_1fA),_1fB,E(_1fC),E(_1fD)];}},_1fL=function(_1fM,_1fN){var _1fO=E(_1fM);if(!_1fO[0]){var _1fP=_1fO[1],_1fQ=_1fO[2],_1fR=_1fO[3],_1fS=_1fO[4],_1fT=_1fO[5],_1fU=E(_1fN);if(!_1fU[0]){var _1fV=_1fU[1],_1fW=_1fU[2],_1fX=_1fU[3],_1fY=_1fU[4],_1fZ=_1fU[5];if((imul(3,_1fP)|0)>=_1fV){if((imul(3,_1fV)|0)>=_1fP){return new F(function(){return _1fa(_1fO,_1fU);});}else{return new F(function(){return _49(_1fQ,_1fR,_1fS,B(_1fl(_1fT,_1fV,_1fW,_1fX,_1fY,_1fZ)));});}}else{return new F(function(){return _50(_1fW,_1fX,B(_1fy(_1fP,_1fQ,_1fR,_1fS,_1fT,_1fY)),_1fZ);});}}else{return E(_1fO);}}else{return E(_1fN);}},_1g0=function(_1g1,_1g2){var _1g3=E(_1g2);if(!_1g3[0]){var _1g4=_1g3[2],_1g5=_1g3[3],_1g6=_1g3[4],_1g7=_1g3[5];if(!B(A(_1g1,[_1g4,_1g5]))){return new F(function(){return _1fL(B(_1g0(_1g1,_1g6)),B(_1g0(_1g1,_1g7)));});}else{return new F(function(){return _6k(_1g4,_1g5,B(_1g0(_1g1,_1g6)),B(_1g0(_1g1,_1g7)));});}}else{return [1];}},_1g8=[1,_8Y,_u],_1g9=[1,_8V,_1g8],_1ga=function(_1gb,_1gc){return new F(function(){return _eR(_cr,_1gb,_1g9);});},_1gd=new T(function(){return B(_1g0(_1ga,_I5));}),_1ge=function(_1gf,_){var _1gg=B(A(_Dt,[_1gf,_])),_1gh=_1gg,_1gi=B(_GY(_1gf,_)),_1gj=_1gi,_1gk=B(_H0(new T(function(){return E(E(_1gj)[2]);}),_)),_1gl=_1gk;return new F(function(){return A(_18i,[new T(function(){return E(E(_1gl)[2]);}),_]);});},_1gm=function(_1gn,_1go){var _1gp=E(_1gn);if(!_1gp[0]){return [0];}else{var _1gq=_1gp[1];return _1go>1?[1,_1gq,new T(function(){return B(_1gm(_1gp[2],_1go-1|0));})]:[1,_1gq,_u];}},_1gr=new T(function(){return B(_19(0,_yB,_u));}),_1gs=new T(function(){return B(_19(0,_yZ,_u));}),_1gt=function(_1gu){return _1gu>1000?B(_19o(new T(function(){var _1gv=B(_yf(_1gu)),_1gw=_1gv[1],_1gx=_1gv[2];if(_1gx>=0){var _1gy=B(_19(0,B(_z0(_1gw,_1gx)),_u));}else{var _1gz= -_1gx;if(_1gz<=52){var _1gA=hs_uncheckedIShiftRA64(B(_yy(_1gw)),_1gz),_1gB=_1gA,_1gC=B(_19(0,B(_yi(_1gB)),_u));}else{var _1gC=!B(_Y(_1gw,_yB))?E(_1gr):E(_1gs);}var _1gD=_1gC,_1gE=_1gD,_1gy=_1gE;}var _1gF=_1gy,_1gG=_1gF;return _1gG;}))):B(_19o(new T(function(){return B(_1gm(B(_15a(_1eN,new T(function(){return B(_Jf([1,[4,[0,_1gu]],_u],_u));}))),5));})));},_1gH=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:328:7-12"));}),_1gI=function(_,_1gJ){var _1gK=jsFind(toJSStr(E(_6))),_1gL=_1gK,_1gM=E(_1gL);if(!_1gM[0]){return new F(function(){return _nr(_1gH,_);});}else{var _1gN=toJSStr(E(_xh)),_1gO=E(E(_1gJ)[2]),_1gP=jsSet(E(_1gM[1])[1],_1gN,toJSStr(B(_1gt(E(_1gO[2])[1])))),_1gQ=jsFind(toJSStr(E(_8))),_1gR=_1gQ,_1gS=E(_1gR);if(!_1gS[0]){return new F(function(){return _nr(_1gH,_);});}else{var _1gT=jsSet(E(_1gS[1])[1],_1gN,toJSStr(B(_1gt(E(_1gO[1])[1])))),_1gU=jsFind(toJSStr(E(_4))),_1gV=_1gU,_1gW=E(_1gV);if(!_1gW[0]){return new F(function(){return _nr(_1gH,_);});}else{var _1gX=jsSet(E(_1gW[1])[1],_1gN,toJSStr(B(_1gt(E(_1gO[3])[1]))));return [0,_cG,_1gO];}}}},_1gY=function(_1gZ){return E(E(_1gZ)[2]);},_1h0=function(_1h1,_1h2,_1h3){while(1){var _1h4=E(_1h3);if(!_1h4[0]){return true;}else{if(!B(A(_1gY,[_1h1,_1h2,_1h4[1]]))){return false;}else{_1h3=_1h4[2];continue;}}}},_1h5=function(_,_1h6){var _1h7=new T(function(){return E(E(_1h6)[2]);}),_1h8=function(_1h9){var _1ha=E(_1h9);return _1ha[0]==0?E(_xQ):function(_1hb,_){var _1hc=B(A(new T(function(){var _1hd=E(_1ha[1]),_1he=_1hd[1],_1hf=new T(function(){var _1hg=B(_wA(_1he,new T(function(){return E(E(_1h7)[7]);})));return _1hg[0]==0?E(_nu):E(_1hg[1]);});return function(_1hh,_){if(!E(new T(function(){if(!B(_1h0(_cr,_1he,_IC))){var _1hi=true;}else{var _1hi=E(E(_1hf)[1])==0?true:false;}return _1hi;}))){return [0,_cG,_1hh];}else{var _1hj=E(_1hh);if(E(new T(function(){return [0,B(_xb(B(A(E(_1hd[2])[1],[_1hf]))))];}))[1]>E(_1hj[1])[1]){return [0,_cG,_1hj];}else{var _1hk=E(new T(function(){return B(unAppCStr("item-",new T(function(){return B(_O(_1he,_xT));})));})),_1hl=jsFind(toJSStr(_1hk)),_1hm=_1hl,_1hn=E(_1hm);if(!_1hn[0]){return new F(function(){return _BI(_1hk);});}else{var _1ho=B(A(_18W,[_1hn[1],_xe,_])),_1hp=_1ho;return [0,_1hp,_1hj];}}}};}),[_1hb,_])),_1hq=_1hc;return new F(function(){return A(new T(function(){return B(_1h8(_1ha[2]));}),[new T(function(){return E(E(_1hq)[2]);}),_]);});};};return new F(function(){return A(_1h8,[_I6,_1h7,_]);});},_1hr=function(_1hs,_1ht,_){while(1){var _1hu=(function(_1hv,_1hw,_){var _1hx=E(_1hv);if(!_1hx[0]){return [0,_cG,_1hw];}else{var _1hy=_1hx[2],_1hz=E(_1hx[1]),_1hA=_1hz[1],_1hB=_1hz[2],_1hC=E(_1hw),_1hD=_1hC[6];if(!B(_v9(_1hA,_1hD))){var _1hE=B(A(_1hB,[_1hC,_])),_1hF=_1hE;_1hs=_1hy;_1ht=new T(function(){return E(E(_1hF)[2]);});return null;}else{if(!B(_uT(_1hA,_1hD))[0]){var _1hG=B(A(_1hB,[_1hC,_])),_1hH=_1hG;_1hs=_1hy;_1ht=new T(function(){return E(E(_1hH)[2]);});return null;}else{return new F(function(){return _1hI(_1hy,_1hC[1],_1hC[2],_1hC[3],_1hC[4],_1hC[5],_1hD,_1hC[7],_1hC[8],_1hC[9],_1hC[10],_1hC[11],_);});}}}})(_1hs,_1ht,_);if(_1hu!=null){return _1hu;}}},_1hI=function(_1hJ,_1hK,_1hL,_1hM,_1hN,_1hO,_1hP,_1hQ,_1hR,_1hS,_1hT,_1hU,_){while(1){var _1hV=(function(_1hW,_1hX,_1hY,_1hZ,_1i0,_1i1,_1i2,_1i3,_1i4,_1i5,_1i6,_1i7,_){var _1i8=E(_1hW);if(!_1i8[0]){return [0,_cG,[0,_1hX,_1hY,_1hZ,_1i0,_1i1,_1i2,_1i3,_1i4,_1i5,_1i6,_1i7]];}else{var _1i9=_1i8[2],_1ia=E(_1i8[1]),_1ib=_1ia[1],_1ic=_1ia[2];if(!B(_v9(_1ib,_1i2))){var _1id=B(A(_1ic,[[0,_1hX,_1hY,_1hZ,_1i0,_1i1,_1i2,_1i3,_1i4,_1i5,_1i6,_1i7],_])),_1ie=_1id;return new F(function(){return _1hr(_1i9,new T(function(){return E(E(_1ie)[2]);}),_);});}else{if(!B(_uT(_1ib,_1i2))[0]){var _1if=B(A(_1ic,[[0,_1hX,_1hY,_1hZ,_1i0,_1i1,_1i2,_1i3,_1i4,_1i5,_1i6,_1i7],_])),_1ig=_1if;return new F(function(){return _1hr(_1i9,new T(function(){return E(E(_1ig)[2]);}),_);});}else{_1hJ=_1i9;var _1ih=_1hX,_1ii=_1hY,_1ij=_1hZ,_1ik=_1i0,_1il=_1i1,_1im=_1i2,_1in=_1i3,_1io=_1i4,_1ip=_1i5,_1iq=_1i6,_1ir=_1i7;_1hK=_1ih;_1hL=_1ii;_1hM=_1ij;_1hN=_1ik;_1hO=_1il;_1hP=_1im;_1hQ=_1in;_1hR=_1io;_1hS=_1ip;_1hT=_1iq;_1hU=_1ir;return null;}}}})(_1hJ,_1hK,_1hL,_1hM,_1hN,_1hO,_1hP,_1hQ,_1hR,_1hS,_1hT,_1hU,_);if(_1hV!=null){return _1hV;}}},_1is=new T(function(){return B(unCStr("\u304a\u304b\u3048\u308a\u306a\u3055\u3044\uff01<br>(\u4f9d\u5b58\u5ea6\u30dc\u30fc\u30ca\u30b9 +"));}),_1it=[0,41],_1iu=[1,_1it,_u],_1iv=new T(function(){return B(unCStr("\u611b\u3061\u3083\u3093"));}),_1iw=function(_1ix){return new F(function(){return err(B(unAppCStr("docFocused: ",[1,_sd,new T(function(){return B(_sf(_1ix,_1eM));})])));});},_1iy=new T(function(){return B(unCStr("false"));}),_1iz=new T(function(){return B(unCStr("document.hasFocus()"));}),_1iA=function(_1iB,_1iC){while(1){var _1iD=E(_1iB);if(!_1iD[0]){var _1iE=_1iD[1],_1iF=E(_1iC);if(!_1iF[0]){var _1iG=_1iF[1],_1iH=subC(_1iE,_1iG);if(!E(_1iH[2])){return [0,_1iH[1]];}else{_1iB=[1,I_fromInt(_1iE)];_1iC=[1,I_fromInt(_1iG)];continue;}}else{_1iB=[1,I_fromInt(_1iE)];_1iC=_1iF;continue;}}else{var _1iI=E(_1iC);if(!_1iI[0]){_1iB=_1iD;_1iC=[1,I_fromInt(_1iI[1])];continue;}else{return [1,I_sub(_1iD[1],_1iI[1])];}}}},_1iJ=function(_1iK,_){var _1iL=E(_1iK),_1iM=_1iL[1],_1iN=_1iL[2],_1iO=_1iL[3],_1iP=_1iL[4],_1iQ=_1iL[5],_1iR=_1iL[6],_1iS=_1iL[7],_1iT=_1iL[8],_1iU=_1iL[9],_1iV=_1iL[10],_1iW=_1iL[11];if(!B(_v9(_8V,_1iS))){return new F(function(){return _1gI(_,[0,_cG,_1iL]);});}else{var _1iX=jsEval(toJSStr(E(_1iz))),_1iY=_1iX,_1iZ=B(_m7(_)),_1j0=_1iZ,_1j1=new T(function(){var _1j2=fromJSStr(_1iY);return !B(_c7(_1j2,_1iy))?!B(_c7(_1j2,_DK))?B(_1iw(_1j2)):true:false;}),_1j3=function(_,_1j4,_1j5,_1j6,_1j7,_1j8,_1j9,_1ja,_1jb,_1jc,_1jd,_1je,_1jf){var _1jg=B(_1hI(_Jb,_1j5,_1j6,_1j7,_1j8,_1j1,_1ja,_1jb,_1jc,_1jd,_1je,_1jf,_)),_1jh=_1jg,_1ji=E(E(_1jh)[2]),_1jj=E(_1ji[1]);return _1jj[1]<=E(_1ji[8])[1]?B(_1h5(_,[0,_cG,_1ji])):B(_1h5(_,[0,_cG,[0,_1jj,_1ji[2],_1ji[3],_1ji[4],_1ji[5],_1ji[6],_1ji[7],_1jj,_1ji[9],_1ji[10],_1ji[11]]]));};if(!E(_1j1)){var _1jk=E(_1iO)[1],_1jl=new T(function(){return [0,1.0e-2*E(_1iV)[1]];});if(_1jk<=0){var _1jm=B(_1j3(_,_cG,new T(function(){return [0,E(_1iM)[1]+E(_1iN)[1]/30];}),_1iN,new T(function(){var _1jn=_1jk-E(_1jl)[1];return _1jn>0?[0,_1jn]:E(_nt);}),_1iP,_1iQ,_1iR,_1iS,_1iT,_1iU,_1iV,_1iW)),_1jo=_1jm;return new F(function(){return _1gI(_,_1jo);});}else{var _1jp=B(_1j3(_,_cG,new T(function(){return [0,E(_1iM)[1]+E(_1iN)[1]/30];}),new T(function(){return [0,E(_1iN)[1]+E(_1jl)[1]];}),new T(function(){var _1jq=_1jk-E(_1jl)[1];return _1jq>0?[0,_1jq]:E(_nt);}),_1iP,_1iQ,_1iR,_1iS,_1iT,_1iU,_1iV,_1iW)),_1jr=_1jp;return new F(function(){return _1gI(_,_1jr);});}}else{var _1js=new T(function(){return [0,B(_xb(B(_1iA(_1j0,_1iP))))];});if(!E(_1iQ)){var _1jt=new T(function(){return [0,E(_1js)[1]/1000/50*E(_1iU)[1]];}),_1ju=B(A(_Gb,[_1iv,new T(function(){return B(_O(_1is,new T(function(){return B(_O(B(_1gt(E(_1jt)[1])),_1iu));})));}),[0,new T(function(){return [0,E(_1iM)[1]+E(_1iN)[1]/30];}),_1iN,new T(function(){return [0,E(_1iO)[1]+E(_1jt)[1]];}),_1iP,_r,_1iR,_1iS,_1iT,_1iU,_1iV,_1iW],_])),_1jv=_1ju,_1jw=B(_1hr(_Jb,new T(function(){var _1jx=E(E(_1jv)[2]);return [0,_1jx[1],new T(function(){return [0,E(_1jx[2])[1]+E(_1js)[1]/1000/100*E(_1iV)[1]];}),new T(function(){return [0,E(_1jx[3])[1]+E(_1js)[1]/1000/1000*E(_1iU)[1]];}),_1j0,_n,_1jx[6],_1jx[7],_1jx[8],_1jx[9],_1jx[10],_1jx[11]];}),_)),_1jy=_1jw,_1jz=E(E(_1jy)[2]),_1jA=E(_1jz[1]);if(_1jA[1]<=E(_1jz[8])[1]){var _1jB=B(_1h5(_,[0,_cG,_1jz])),_1jC=_1jB;return new F(function(){return _1gI(_,_1jC);});}else{var _1jD=B(_1h5(_,[0,_cG,[0,_1jA,_1jz[2],_1jz[3],_1jz[4],_1jz[5],_1jz[6],_1jz[7],_1jA,_1jz[9],_1jz[10],_1jz[11]]])),_1jE=_1jD;return new F(function(){return _1gI(_,_1jE);});}}else{var _1jF=B(_1j3(_,_cG,new T(function(){return [0,E(_1iM)[1]+E(_1iN)[1]/30];}),new T(function(){return [0,E(_1iN)[1]+E(_1js)[1]/1000/100*E(_1iV)[1]];}),new T(function(){return [0,E(_1iO)[1]+E(_1js)[1]/1000/1000*E(_1iU)[1]];}),_1j0,_n,_1iR,_1iS,_1iT,_1iU,_1iV,_1iW)),_1jG=_1jF;return new F(function(){return _1gI(_,_1jG);});}}}},_1jH=function(_){return _cG;},_1jI=function(_){var _=0,_1jJ=jsMkStdout(),_1jK=_1jJ;return [0,_1jK];},_1jL=new T(function(){return B(_o9(_1jI));}),_1jM=function(_){var _1jN=B(_nX(_)),_1jO=_1jN,_1jP=B(_1cL(_qs,_Cp,_)),_1jQ=_1jP,_1jR=nMV(new T(function(){var _1jS=E(_1jQ);return _1jS[0]==0?E(_1jO):E(_1jS[1]);})),_1jT=_1jR,_1jU=B(unCStr("list-group")),_1jV=jsFind(toJSStr(_1jU)),_1jW=_1jV,_1jX=E(_1jW);if(!_1jX[0]){return new F(function(){return _BI(_1jU);});}else{var _1jY=B((function(_1jZ,_){while(1){var _1k0=E(_1jZ);if(!_1k0[0]){return _cG;}else{var _1k1=E(_1k0[1]),_1k2=E(E(_1k1[2])[3]),_1k3=B(A(_1dG,[_1k1[1],_1k2[1],_1k2[2],_1k2[3],_1jX[1],_])),_1k4=_1k3;_1jZ=_1k0[2];continue;}}})(_1cB,_)),_1k5=_1jY,_1k6=B(unCStr("list-sp-group")),_1k7=jsFind(toJSStr(_1k6)),_1k8=_1k7,_1k9=E(_1k8);if(!_1k9[0]){return new F(function(){return _BI(_1k6);});}else{var _1ka=B((function(_1kb,_){while(1){var _1kc=E(_1kb);if(!_1kc[0]){return _cG;}else{var _1kd=E(_1kc[1]),_1ke=E(E(_1kd[2])[3]),_1kf=B(A(_1dG,[_1kd[1],_1ke[1],_1ke[2],_1ke[3],_1k9[1],_])),_1kg=_1kf;_1kb=_1kc[2];continue;}}})(_1cC,_)),_1kh=_1ka,_1ki=B(_1bh([0,_1jT],_18N,_)),_1kj=_1ki,_1kk=function(_1kl,_1km){while(1){var _1kn=(function(_1ko,_1kp){var _1kq=E(_1kp);if(!_1kq[0]){var _1kr=_1kq[2],_1ks=_1kq[5];_1kl=function(_){var _1kt=rMV(_1jT),_1ku=_1kt,_1kv=E(_1ku),_1kw=_1kv[7];if(!B(_v9(_1kr,_1kw))){var _=wMV(_1jT,_1kv);return new F(function(){return A(_1kk,[_1ko,_1ks,_]);});}else{var _1kx=B(_uT(_1kr,_1kw));if(_1kx[1]<=0){var _=wMV(_1jT,_1kv);return new F(function(){return A(_1kk,[_1ko,_1ks,_]);});}else{var _1ky=B(A(E(_1kq[3])[2],[_1kx,_1kv,_])),_1kz=_1ky,_=wMV(_1jT,new T(function(){return E(E(_1kz)[2]);}));return new F(function(){return A(_1kk,[_1ko,_1ks,_]);});}}};_1km=_1kq[4];return null;}else{return E(_1ko);}})(_1kl,_1km);if(_1kn!=null){return _1kn;}}},_1kA=B(A(_1kk,[_1jH,_1gd,_])),_1kB=_1kA,_1kC=rMV(_1jT),_1kD=_1kC,_1kE=B(_GY(_1kD,_)),_1kF=_1kE,_=wMV(_1jT,new T(function(){return E(E(_1kF)[2]);})),_1kG=B(_uY(33,_1jT,_1iJ,_)),_1kH=_1kG,_1kI=B(_1d4(_1jL,B(_uO(_1kH)),_)),_1kJ=_1kI,_1kK=B(_uY(1000,_1jT,_1eO,_)),_1kL=_1kK,_1kM=B(_1d4(_1jL,B(_uO(_1kL)),_)),_1kN=_1kM,_1kO=B(_uY(60000,_1jT,_1ge,_)),_1kP=_1kO;return new F(function(){return _1d4(_1jL,B(_uO(_1kP)),_);});}}},_1kQ=function(_){return new F(function(){return _1jM(_);});};
var hasteMain = function() {B(A(_1kQ, [0]));};window.onload = hasteMain;
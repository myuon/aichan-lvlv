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

var _0=new T(function(){return B(unCStr("achievements"));}),_1=new T(function(){return [0,toJSStr(E(_0))];}),_2=new T(function(){return B(unCStr("lastFocus"));}),_3=new T(function(){return [0,toJSStr(E(_2))];}),_4=new T(function(){return B(unCStr("depend"));}),_5=new T(function(){return [0,toJSStr(E(_4))];}),_6=new T(function(){return B(unCStr("lps"));}),_7=new T(function(){return [0,toJSStr(E(_6))];}),_8=new T(function(){return B(unCStr("loves"));}),_9=new T(function(){return [0,toJSStr(E(_8))];}),_a=new T(function(){return B(unCStr("lpsCoeff"));}),_b=new T(function(){return [0,toJSStr(E(_a))];}),_c=new T(function(){return B(unCStr("dependCoeff"));}),_d=new T(function(){return [0,toJSStr(E(_c))];}),_e=new T(function(){return B(unCStr("maxLoves"));}),_f=new T(function(){return [0,toJSStr(E(_e))];}),_g=new T(function(){return B(unCStr("items"));}),_h=new T(function(){return [0,toJSStr(E(_g))];}),_i=function(_j){return [0,toJSStr(E(_j))];},_k=function(_l){return [1,new T(function(){return B(_i(_l));})];},_m=new T(function(){return [0,"value"];}),_n=true,_o=[2,_n],_p=new T(function(){return [0,"hasValue"];}),_q=[0,_p,_o],_r=false,_s=[2,_r],_t=[0,_p,_s],_u=[0],_v=[1,_t,_u],_w=[4,_v],_x=function(_y,_z){while(1){var _A=(function(_B,_C){var _D=E(_C);switch(_D[0]){case 0:_y=new T(function(){return B(_x(_B,_D[4]));});_z=_D[3];return null;case 1:return [1,[3,[1,[0,[0,_D[1]]],[1,new T(function(){var _E=E(_D[2]);return _E[0]==0?E(_w):[4,[1,_q,[1,[0,_m,new T(function(){return B(_k(_E[1]));})],_u]]];}),_u]]],_B];default:return E(_B);}})(_y,_z);if(_A!=null){return _A;}}},_F=function(_G){return [0,new T(function(){return [0,E(_G)[1]];})];},_H=function(_I,_J){while(1){var _K=(function(_L,_M){var _N=E(_M);switch(_N[0]){case 0:_I=new T(function(){return B(_H(_L,_N[4]));});_J=_N[3];return null;case 1:return [1,[3,[1,[0,[0,_N[1]]],[1,new T(function(){return B(_F(_N[2]));}),_u]]],_L];default:return E(_L);}})(_I,_J);if(_K!=null){return _K;}}},_O=function(_P,_Q){var _R=E(_P);return _R[0]==0?E(_Q):[1,_R[1],new T(function(){return B(_O(_R[2],_Q));})];},_S=function(_T){while(1){var _U=E(_T);if(!_U[0]){_T=[1,I_fromInt(_U[1])];continue;}else{return new F(function(){return I_toString(_U[1]);});}}},_V=function(_W,_X){return new F(function(){return _O(fromJSStr(B(_S(_W))),_X);});},_Y=function(_Z,_10){var _11=E(_Z);if(!_11[0]){var _12=_11[1],_13=E(_10);return _13[0]==0?_12<_13[1]:I_compareInt(_13[1],_12)>0;}else{var _14=_11[1],_15=E(_10);return _15[0]==0?I_compareInt(_14,_15[1])<0:I_compare(_14,_15[1])<0;}},_16=[0,41],_17=[0,40],_18=[0,0],_19=function(_1a,_1b,_1c){return _1a<=6?B(_V(_1b,_1c)):!B(_Y(_1b,_18))?B(_V(_1b,_1c)):[1,_17,new T(function(){return B(_O(fromJSStr(B(_S(_1b))),[1,_16,_1c]));})];},_1d=function(_1e,_1f,_1g,_1h,_1i,_1j,_1k,_1l,_1m){return [1,[0,_9,[0,_1e]],[1,[0,_7,[0,_1f]],[1,[0,_5,[0,_1g]],[1,[0,_3,[1,new T(function(){return [0,toJSStr(B(_19(0,_1h,_u)))];})]],[1,[0,_1,[3,new T(function(){var _1n=E(_1i);if(!_1n[0]){var _1o=_1n[3],_1p=_1n[4],_1q=_1n[2]>=0?B(_x(new T(function(){return B(_x(_u,_1p));}),_1o)):B(_x(new T(function(){return B(_x(_u,_1o));}),_1p));}else{var _1q=B(_x(_u,_1n));}return _1q;})]],[1,[0,_h,[3,new T(function(){var _1r=E(_1j);if(!_1r[0]){var _1s=_1r[3],_1t=_1r[4],_1u=_1r[2]>=0?B(_H(new T(function(){return B(_H(_u,_1t));}),_1s)):B(_H(new T(function(){return B(_H(_u,_1s));}),_1t));}else{var _1u=B(_H(_u,_1r));}return _1u;})]],[1,[0,_f,[0,_1k]],[1,[0,_d,[0,_1l]],[1,[0,_b,[0,_1m]],_u]]]]]]]]];},_1v=function(_1w){var _1x=E(_1w);return [4,B(_1d(_1x[1],_1x[2],_1x[3],_1x[4],_1x[6],_1x[7],_1x[8],_1x[9],_1x[10]))];},_1y=function(_1z,_1A){var _1B=E(_1A);return _1B[0]==0?[0]:[1,new T(function(){return B(A(_1z,[_1B[1]]));}),new T(function(){return B(_1y(_1z,_1B[2]));})];},_1C=function(_1D){return [3,new T(function(){return B(_1y(_1v,_1D));})];},_1E=new T(function(){return B(unCStr("Tried to deserialize long string to a Char"));}),_1F=[0,_1E],_1G=new T(function(){return B(unCStr("Tried to deserialize a non-string to a Char"));}),_1H=[0,_1G],_1I=function(_1J){var _1K=E(_1J);if(_1K[0]==1){var _1L=fromJSStr(E(_1K[1])[1]);return _1L[0]==0?E(_1F):E(_1L[2])[0]==0?[1,_1L[1]]:E(_1F);}else{return E(_1H);}},_1M=new T(function(){return B(unCStr("Tried to deserialize a non-JSString to a JSString"));}),_1N=[0,_1M],_1O=function(_1P){return new F(function(){return fromJSStr(E(_1P)[1]);});},_1Q=function(_1R){var _1S=E(_1R);return _1S[0]==1?[1,new T(function(){return B(_1O(_1S[1]));})]:E(_1N);},_1T=function(_1U){return [1,new T(function(){return [0,toJSStr([1,_1U,_u])];})];},_1V=[0,_1T,_k,_1I,_1Q],_1W=function(_1X){return E(E(_1X)[2]);},_1Y=function(_1Z,_20){return [3,new T(function(){return B(_1y(new T(function(){return B(_1W(_1Z));}),_20));})];},_21=[1,_u],_22=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_23=[0,_22],_24=function(_25){return E(E(_25)[4]);},_26=function(_27,_28){var _29=E(_28);if(_29[0]==3){var _2a=function(_2b){var _2c=E(_2b);if(!_2c[0]){return E(_21);}else{var _2d=B(A(new T(function(){return B(_24(_27));}),[_2c[1]]));if(!_2d[0]){return [0,_2d[1]];}else{var _2e=B(_2a(_2c[2]));return _2e[0]==0?[0,_2e[1]]:[1,[1,_2d[1],_2e[1]]];}}};return new F(function(){return _2a(_29[1]);});}else{return E(_23);}},_2f=function(_2g){return [0,new T(function(){return B(_1W(_2g));}),function(_2h){return new F(function(){return _1Y(_2g,_2h);});},new T(function(){return B(_24(_2g));}),function(_2h){return new F(function(){return _26(_2g,_2h);});}];},_2i=new T(function(){return B(_2f(_1V));}),_2j=function(_2k){return E(E(_2k)[1]);},_2l=function(_2m,_2n){var _2o=E(_2n);return _2o[0]==0?E(_w):[4,[1,_q,[1,[0,_m,new T(function(){return B(A(_2j,[_2m,_2o[1]]));})],_u]]];},_2p=function(_2q,_2r){return [3,new T(function(){return B(_1y(function(_2h){return new F(function(){return _2l(_2q,_2h);});},_2r));})];},_2s=function(_2t,_2u){var _2v=strEq(E(_2t)[1],E(_2u)[1]),_2w=_2v;return E(_2w)==0?true:false;},_2x=function(_2y,_2z){var _2A=strEq(E(_2y)[1],E(_2z)[1]),_2B=_2A;return E(_2B)==0?false:true;},_2C=[0,_2x,_2s],_2D=[0],_2E=[1,_2D],_2F=new T(function(){return B(unCStr("Tried to deserialize a non-Bool to a Bool"));}),_2G=[0,_2F],_2H=new T(function(){return B(unCStr("Key not found"));}),_2I=[0,_2H],_2J=new T(function(){return B(unCStr("Tried to do lookup on non-object!"));}),_2K=[0,_2J],_2L=function(_2M){return E(E(_2M)[1]);},_2N=function(_2O,_2P,_2Q){while(1){var _2R=E(_2Q);if(!_2R[0]){return [0];}else{var _2S=E(_2R[1]);if(!B(A(_2L,[_2O,_2P,_2S[1]]))){_2Q=_2R[2];continue;}else{return [1,_2S[2]];}}}},_2T=function(_2U){return E(E(_2U)[3]);},_2V=function(_2W,_2X){var _2Y=E(_2X);if(_2Y[0]==4){var _2Z=_2Y[1],_30=B(_2N(_2C,_p,_2Z));if(!_30[0]){return E(_2I);}else{var _31=E(_30[1]);if(_31[0]==2){if(!E(_31[1])){return E(_2E);}else{var _32=B(_2N(_2C,_m,_2Z));if(!_32[0]){return E(_2I);}else{var _33=B(A(_2T,[_2W,_32[1]]));return _33[0]==0?[0,_33[1]]:[1,[1,_33[1]]];}}}else{return E(_2G);}}}else{return E(_2K);}},_34=[1,_u],_35=[0,_22],_36=function(_37,_38){var _39=E(_38);if(_39[0]==3){var _3a=function(_3b){var _3c=E(_3b);if(!_3c[0]){return E(_34);}else{var _3d=B(_2V(_37,_3c[1]));if(!_3d[0]){return [0,_3d[1]];}else{var _3e=B(_3a(_3c[2]));return _3e[0]==0?[0,_3e[1]]:[1,[1,_3d[1],_3e[1]]];}}};return new F(function(){return _3a(_39[1]);});}else{return E(_35);}},_3f=function(_3g){return [0,function(_2h){return new F(function(){return _2l(_3g,_2h);});},function(_2h){return new F(function(){return _2p(_3g,_2h);});},function(_2h){return new F(function(){return _2V(_3g,_2h);});},function(_2h){return new F(function(){return _36(_3g,_2h);});}];},_3h=new T(function(){return B(_3f(_2i));}),_3i=[1,_u],_3j=[0,_22],_3k=new T(function(){return B(unCStr("Tried to deserialize a non-array into a pair!"));}),_3l=[0,_3k],_3m=function(_3n,_3o,_3p){var _3q=E(_3p);if(_3q[0]==3){var _3r=E(_3q[1]);if(!_3r[0]){return E(_3l);}else{var _3s=E(_3r[2]);if(!_3s[0]){return E(_3l);}else{if(!E(_3s[2])[0]){var _3t=B(A(_2T,[_3n,_3r[1]]));if(!_3t[0]){return [0,_3t[1]];}else{var _3u=B(A(_2T,[_3o,_3s[1]]));return _3u[0]==0?[0,_3u[1]]:[1,[0,_3t[1],_3u[1]]];}}else{return E(_3l);}}}}else{return E(_3l);}},_3v=function(_3w,_3x,_3y){var _3z=E(_3y);if(_3z[0]==3){var _3A=function(_3B){var _3C=E(_3B);if(!_3C[0]){return E(_3i);}else{var _3D=B(_3m(_3w,_3x,_3C[1]));if(!_3D[0]){return [0,_3D[1]];}else{var _3E=B(_3A(_3C[2]));return _3E[0]==0?[0,_3E[1]]:[1,[1,_3D[1],_3E[1]]];}}};return new F(function(){return _3A(_3z[1]);});}else{return E(_3j);}},_3F=function(_3G){return [3,new T(function(){return B(_1y(_F,_3G));})];},_3H=new T(function(){return B(unCStr("The given Number can\'t be represented as an Int"));}),_3I=[0,_3H],_3J=new T(function(){return B(unCStr("Tried to deserialize a non-Number to an Int"));}),_3K=[0,_3J],_3L=function(_3M){var _3N=E(_3M);if(!_3N[0]){var _3O=E(_3N[1])[1],_3P=_3O&4294967295;return _3P!=_3O?E(_3I):[1,[0,_3P]];}else{return E(_3K);}},_3Q=[0,_22],_3R=[1,_u],_3S=[0,_3H],_3T=[0,_3J],_3U=function(_3V){var _3W=E(_3V);if(!_3W[0]){return E(_3R);}else{var _3X=E(_3W[1]);if(!_3X[0]){var _3Y=E(_3X[1])[1],_3Z=_3Y&4294967295;if(_3Z!=_3Y){return E(_3S);}else{var _40=B(_3U(_3W[2]));return _40[0]==0?[0,_40[1]]:[1,[1,[0,_3Z],_40[1]]];}}else{return E(_3T);}}},_41=function(_42){var _43=E(_42);return _43[0]==3?B(_3U(_43[1])):E(_3Q);},_44=[0,_F,_3F,_3L,_41],_45=[2],_46=function(_47,_48,_49){var _4a=E(_49);switch(_4a[0]){case 0:var _4b=_4a[1],_4c=_4a[2],_4d=_4a[3],_4e=_4a[4],_4f=_4c>>>0;if(((_47>>>0&((_4f-1>>>0^4294967295)>>>0^_4f)>>>0)>>>0&4294967295)==_4b){return (_47>>>0&_4f)>>>0==0?[0,_4b,_4c,E(B(_46(_47,_48,_4d))),E(_4e)]:[0,_4b,_4c,E(_4d),E(B(_46(_47,_48,_4e)))];}else{var _4g=(_47>>>0^_4b>>>0)>>>0,_4h=(_4g|_4g>>>1)>>>0,_4i=(_4h|_4h>>>2)>>>0,_4j=(_4i|_4i>>>4)>>>0,_4k=(_4j|_4j>>>8)>>>0,_4l=(_4k|_4k>>>16)>>>0,_4m=(_4l^_4l>>>1)>>>0&4294967295,_4n=_4m>>>0;return (_47>>>0&_4n)>>>0==0?[0,(_47>>>0&((_4n-1>>>0^4294967295)>>>0^_4n)>>>0)>>>0&4294967295,_4m,E([1,_47,_48]),E(_4a)]:[0,(_47>>>0&((_4n-1>>>0^4294967295)>>>0^_4n)>>>0)>>>0&4294967295,_4m,E(_4a),E([1,_47,_48])];}break;case 1:var _4o=_4a[1];if(_47!=_4o){var _4p=(_47>>>0^_4o>>>0)>>>0,_4q=(_4p|_4p>>>1)>>>0,_4r=(_4q|_4q>>>2)>>>0,_4s=(_4r|_4r>>>4)>>>0,_4t=(_4s|_4s>>>8)>>>0,_4u=(_4t|_4t>>>16)>>>0,_4v=(_4u^_4u>>>1)>>>0&4294967295,_4w=_4v>>>0;return (_47>>>0&_4w)>>>0==0?[0,(_47>>>0&((_4w-1>>>0^4294967295)>>>0^_4w)>>>0)>>>0&4294967295,_4v,E([1,_47,_48]),E(_4a)]:[0,(_47>>>0&((_4w-1>>>0^4294967295)>>>0^_4w)>>>0)>>>0&4294967295,_4v,E(_4a),E([1,_47,_48])];}else{return [1,_47,_48];}break;default:return [1,_47,_48];}},_4x=function(_4y,_4z){while(1){var _4A=E(_4z);if(!_4A[0]){return E(_4y);}else{var _4B=E(_4A[1]),_4C=B(_46(E(_4B[1])[1],_4B[2],_4y));_4z=_4A[2];_4y=_4C;continue;}}},_4D=new T(function(){return B(unCStr("Control.Exception.Base"));}),_4E=new T(function(){return B(unCStr("base"));}),_4F=new T(function(){return B(unCStr("PatternMatchFail"));}),_4G=new T(function(){var _4H=hs_wordToWord64(18445595),_4I=_4H,_4J=hs_wordToWord64(52003073),_4K=_4J;return [0,_4I,_4K,[0,_4I,_4K,_4E,_4D,_4F],_u];}),_4L=function(_4M){return E(_4G);},_4N=function(_4O){return E(E(_4O)[1]);},_4P=function(_4Q,_4R,_4S){var _4T=B(A(_4Q,[_])),_4U=B(A(_4R,[_])),_4V=hs_eqWord64(_4T[1],_4U[1]),_4W=_4V;if(!E(_4W)){return [0];}else{var _4X=hs_eqWord64(_4T[2],_4U[2]),_4Y=_4X;return E(_4Y)==0?[0]:[1,_4S];}},_4Z=function(_50){var _51=E(_50);return new F(function(){return _4P(B(_4N(_51[1])),_4L,_51[2]);});},_52=function(_53){return E(E(_53)[1]);},_54=function(_55,_56){return new F(function(){return _O(E(_55)[1],_56);});},_57=[0,44],_58=[0,93],_59=[0,91],_5a=function(_5b,_5c,_5d){var _5e=E(_5c);return _5e[0]==0?B(unAppCStr("[]",_5d)):[1,_59,new T(function(){return B(A(_5b,[_5e[1],new T(function(){var _5f=function(_5g){var _5h=E(_5g);return _5h[0]==0?E([1,_58,_5d]):[1,_57,new T(function(){return B(A(_5b,[_5h[1],new T(function(){return B(_5f(_5h[2]));})]));})];};return B(_5f(_5e[2]));})]));})];},_5i=function(_5j,_5k){return new F(function(){return _5a(_54,_5j,_5k);});},_5l=function(_5m,_5n,_5o){return new F(function(){return _O(E(_5n)[1],_5o);});},_5p=[0,_5l,_52,_5i],_5q=new T(function(){return [0,_4L,_5p,_5r,_4Z];}),_5r=function(_5s){return [0,_5q,_5s];},_5t=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_5u=function(_5v,_5w){return new F(function(){return die(new T(function(){return B(A(_5w,[_5v]));}));});},_5x=function(_5y,_5z){var _5A=E(_5z);if(!_5A[0]){return [0,_u,_u];}else{var _5B=_5A[1];if(!B(A(_5y,[_5B]))){return [0,_u,_5A];}else{var _5C=new T(function(){var _5D=B(_5x(_5y,_5A[2]));return [0,_5D[1],_5D[2]];});return [0,[1,_5B,new T(function(){return E(E(_5C)[1]);})],new T(function(){return E(E(_5C)[2]);})];}}},_5E=[0,32],_5F=[0,10],_5G=[1,_5F,_u],_5H=function(_5I){return E(E(_5I)[1])==124?false:true;},_5J=function(_5K,_5L){var _5M=B(_5x(_5H,B(unCStr(_5K)))),_5N=_5M[1],_5O=function(_5P,_5Q){return new F(function(){return _O(_5P,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_O(_5L,new T(function(){return B(_O(_5Q,_5G));})));})));}));});},_5R=E(_5M[2]);if(!_5R[0]){return new F(function(){return _5O(_5N,_u);});}else{return E(E(_5R[1])[1])==124?B(_5O(_5N,[1,_5E,_5R[2]])):B(_5O(_5N,_u));}},_5S=function(_5T){return new F(function(){return _5u([0,new T(function(){return B(_5J(_5T,_5t));})],_5r);});},_5U=new T(function(){return B(_5S("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_5V=function(_5W,_5X){while(1){var _5Y=(function(_5Z,_60){var _61=E(_5Z);switch(_61[0]){case 0:var _62=E(_60);if(!_62[0]){return [0];}else{_5W=B(A(_61[1],[_62[1]]));_5X=_62[2];return null;}break;case 1:var _63=B(A(_61[1],[_60])),_64=_60;_5W=_63;_5X=_64;return null;case 2:return [0];case 3:return [1,[0,_61[1],_60],new T(function(){return B(_5V(_61[2],_60));})];default:return E(_61[1]);}})(_5W,_5X);if(_5Y!=null){return _5Y;}}},_65=function(_66,_67){var _68=function(_69){var _6a=E(_67);if(_6a[0]==3){return [3,_6a[1],new T(function(){return B(_65(_66,_6a[2]));})];}else{var _6b=E(_66);if(_6b[0]==2){return E(_6a);}else{var _6c=E(_6a);if(_6c[0]==2){return E(_6b);}else{var _6d=function(_6e){var _6f=E(_6c);if(_6f[0]==4){return [1,function(_6g){return [4,new T(function(){return B(_O(B(_5V(_6b,_6g)),_6f[1]));})];}];}else{var _6h=E(_6b);if(_6h[0]==1){var _6i=_6h[1],_6j=E(_6f);return _6j[0]==0?[1,function(_6k){return new F(function(){return _65(B(A(_6i,[_6k])),_6j);});}]:[1,function(_6l){return new F(function(){return _65(B(A(_6i,[_6l])),new T(function(){return B(A(_6j[1],[_6l]));}));});}];}else{var _6m=E(_6f);return _6m[0]==0?E(_5U):[1,function(_6n){return new F(function(){return _65(_6h,new T(function(){return B(A(_6m[1],[_6n]));}));});}];}}},_6o=E(_6b);switch(_6o[0]){case 1:var _6p=E(_6c);if(_6p[0]==4){return [1,function(_6q){return [4,new T(function(){return B(_O(B(_5V(B(A(_6o[1],[_6q])),_6q)),_6p[1]));})];}];}else{return new F(function(){return _6d(_);});}break;case 4:var _6r=_6o[1],_6s=E(_6c);switch(_6s[0]){case 0:return [1,function(_6t){return [4,new T(function(){return B(_O(_6r,new T(function(){return B(_5V(_6s,_6t));})));})];}];case 1:return [1,function(_6u){return [4,new T(function(){return B(_O(_6r,new T(function(){return B(_5V(B(A(_6s[1],[_6u])),_6u));})));})];}];default:return [4,new T(function(){return B(_O(_6r,_6s[1]));})];}break;default:return new F(function(){return _6d(_);});}}}}},_6v=E(_66);switch(_6v[0]){case 0:var _6w=E(_67);if(!_6w[0]){return [0,function(_6x){return new F(function(){return _65(B(A(_6v[1],[_6x])),new T(function(){return B(A(_6w[1],[_6x]));}));});}];}else{return new F(function(){return _68(_);});}break;case 3:return [3,_6v[1],new T(function(){return B(_65(_6v[2],_67));})];default:return new F(function(){return _68(_);});}},_6y=[0,41],_6z=[1,_6y,_u],_6A=[0,40],_6B=[1,_6A,_u],_6C=function(_6D,_6E){while(1){var _6F=E(_6D);if(!_6F[0]){return E(_6E)[0]==0?true:false;}else{var _6G=E(_6E);if(!_6G[0]){return false;}else{if(E(_6F[1])[1]!=E(_6G[1])[1]){return false;}else{_6D=_6F[2];_6E=_6G[2];continue;}}}}},_6H=function(_6I,_6J){return E(_6I)[1]!=E(_6J)[1];},_6K=function(_6L,_6M){return E(_6L)[1]==E(_6M)[1];},_6N=[0,_6K,_6H],_6O=function(_6P,_6Q){while(1){var _6R=E(_6P);if(!_6R[0]){return E(_6Q)[0]==0?true:false;}else{var _6S=E(_6Q);if(!_6S[0]){return false;}else{if(E(_6R[1])[1]!=E(_6S[1])[1]){return false;}else{_6P=_6R[2];_6Q=_6S[2];continue;}}}}},_6T=function(_6U,_6V){return !B(_6O(_6U,_6V))?true:false;},_6W=[0,_6O,_6T],_6X=function(_6Y,_6Z){var _70=E(_6Y);switch(_70[0]){case 0:return [0,function(_71){return new F(function(){return _6X(B(A(_70[1],[_71])),_6Z);});}];case 1:return [1,function(_72){return new F(function(){return _6X(B(A(_70[1],[_72])),_6Z);});}];case 2:return [2];case 3:return new F(function(){return _65(B(A(_6Z,[_70[1]])),new T(function(){return B(_6X(_70[2],_6Z));}));});break;default:var _73=function(_74){var _75=E(_74);if(!_75[0]){return [0];}else{var _76=E(_75[1]);return new F(function(){return _O(B(_5V(B(A(_6Z,[_76[1]])),_76[2])),new T(function(){return B(_73(_75[2]));}));});}},_77=B(_73(_70[1]));return _77[0]==0?[2]:[4,_77];}},_78=[2],_79=function(_7a){return [3,_7a,_78];},_7b=0,_7c=function(_7d,_7e){var _7f=E(_7d);if(!_7f){return new F(function(){return A(_7e,[_7b]);});}else{return [0,function(_7g){return E(new T(function(){return B(_7c(_7f-1|0,_7e));}));}];}},_7h=function(_7i,_7j,_7k){return function(_7l){return new F(function(){return A(function(_7m,_7n,_7o){while(1){var _7p=(function(_7q,_7r,_7s){var _7t=E(_7q);switch(_7t[0]){case 0:var _7u=E(_7r);if(!_7u[0]){return E(_7j);}else{_7m=B(A(_7t[1],[_7u[1]]));_7n=_7u[2];var _7v=_7s+1|0;_7o=_7v;return null;}break;case 1:var _7w=B(A(_7t[1],[_7r])),_7x=_7r,_7v=_7s;_7m=_7w;_7n=_7x;_7o=_7v;return null;case 2:return E(_7j);case 3:return function(_7y){return new F(function(){return _7c(_7s,function(_7z){return E(new T(function(){return B(_6X(_7t,_7y));}));});});};default:return function(_7A){return new F(function(){return _6X(_7t,_7A);});};}})(_7m,_7n,_7o);if(_7p!=null){return _7p;}}},[new T(function(){return B(A(_7i,[_79]));}),_7l,0,_7k]);});};},_7B=function(_7C){return new F(function(){return A(_7C,[_u]);});},_7D=function(_7E,_7F){var _7G=function(_7H){var _7I=E(_7H);if(!_7I[0]){return E(_7B);}else{var _7J=_7I[1];return !B(A(_7E,[_7J]))?E(_7B):function(_7K){return [0,function(_7L){return E(new T(function(){return B(A(new T(function(){return B(_7G(_7I[2]));}),[function(_7M){return new F(function(){return A(_7K,[[1,_7J,_7M]]);});}]));}));}];};}};return function(_7N){return new F(function(){return A(_7G,[_7N,_7F]);});};},_7O=[6],_7P=function(_7Q){return E(_7Q);},_7R=new T(function(){return B(unCStr("valDig: Bad base"));}),_7S=new T(function(){return B(err(_7R));}),_7T=function(_7U,_7V){var _7W=function(_7X,_7Y){var _7Z=E(_7X);if(!_7Z[0]){return function(_80){return new F(function(){return A(_80,[new T(function(){return B(A(_7Y,[_u]));})]);});};}else{var _81=E(_7Z[1])[1],_82=function(_83){return function(_84){return [0,function(_85){return E(new T(function(){return B(A(new T(function(){return B(_7W(_7Z[2],function(_86){return new F(function(){return A(_7Y,[[1,_83,_86]]);});}));}),[_84]));}));}];};};switch(E(E(_7U)[1])){case 8:if(48>_81){return function(_87){return new F(function(){return A(_87,[new T(function(){return B(A(_7Y,[_u]));})]);});};}else{if(_81>55){return function(_88){return new F(function(){return A(_88,[new T(function(){return B(A(_7Y,[_u]));})]);});};}else{return new F(function(){return _82([0,_81-48|0]);});}}break;case 10:if(48>_81){return function(_89){return new F(function(){return A(_89,[new T(function(){return B(A(_7Y,[_u]));})]);});};}else{if(_81>57){return function(_8a){return new F(function(){return A(_8a,[new T(function(){return B(A(_7Y,[_u]));})]);});};}else{return new F(function(){return _82([0,_81-48|0]);});}}break;case 16:if(48>_81){if(97>_81){if(65>_81){return function(_8b){return new F(function(){return A(_8b,[new T(function(){return B(A(_7Y,[_u]));})]);});};}else{if(_81>70){return function(_8c){return new F(function(){return A(_8c,[new T(function(){return B(A(_7Y,[_u]));})]);});};}else{return new F(function(){return _82([0,(_81-65|0)+10|0]);});}}}else{if(_81>102){if(65>_81){return function(_8d){return new F(function(){return A(_8d,[new T(function(){return B(A(_7Y,[_u]));})]);});};}else{if(_81>70){return function(_8e){return new F(function(){return A(_8e,[new T(function(){return B(A(_7Y,[_u]));})]);});};}else{return new F(function(){return _82([0,(_81-65|0)+10|0]);});}}}else{return new F(function(){return _82([0,(_81-97|0)+10|0]);});}}}else{if(_81>57){if(97>_81){if(65>_81){return function(_8f){return new F(function(){return A(_8f,[new T(function(){return B(A(_7Y,[_u]));})]);});};}else{if(_81>70){return function(_8g){return new F(function(){return A(_8g,[new T(function(){return B(A(_7Y,[_u]));})]);});};}else{return new F(function(){return _82([0,(_81-65|0)+10|0]);});}}}else{if(_81>102){if(65>_81){return function(_8h){return new F(function(){return A(_8h,[new T(function(){return B(A(_7Y,[_u]));})]);});};}else{if(_81>70){return function(_8i){return new F(function(){return A(_8i,[new T(function(){return B(A(_7Y,[_u]));})]);});};}else{return new F(function(){return _82([0,(_81-65|0)+10|0]);});}}}else{return new F(function(){return _82([0,(_81-97|0)+10|0]);});}}}else{return new F(function(){return _82([0,_81-48|0]);});}}break;default:return E(_7S);}}};return function(_8j){return new F(function(){return A(_7W,[_8j,_7P,function(_8k){var _8l=E(_8k);return _8l[0]==0?[2]:B(A(_7V,[_8l]));}]);});};},_8m=[0,10],_8n=[0,1],_8o=[0,2147483647],_8p=function(_8q,_8r){while(1){var _8s=E(_8q);if(!_8s[0]){var _8t=_8s[1],_8u=E(_8r);if(!_8u[0]){var _8v=_8u[1],_8w=addC(_8t,_8v);if(!E(_8w[2])){return [0,_8w[1]];}else{_8q=[1,I_fromInt(_8t)];_8r=[1,I_fromInt(_8v)];continue;}}else{_8q=[1,I_fromInt(_8t)];_8r=_8u;continue;}}else{var _8x=E(_8r);if(!_8x[0]){_8q=_8s;_8r=[1,I_fromInt(_8x[1])];continue;}else{return [1,I_add(_8s[1],_8x[1])];}}}},_8y=new T(function(){return B(_8p(_8o,_8n));}),_8z=function(_8A){var _8B=E(_8A);if(!_8B[0]){var _8C=E(_8B[1]);return _8C==(-2147483648)?E(_8y):[0, -_8C];}else{return [1,I_negate(_8B[1])];}},_8D=[0,10],_8E=[0,0],_8F=function(_8G){return [0,_8G];},_8H=function(_8I,_8J){while(1){var _8K=E(_8I);if(!_8K[0]){var _8L=_8K[1],_8M=E(_8J);if(!_8M[0]){var _8N=_8M[1];if(!(imul(_8L,_8N)|0)){return [0,imul(_8L,_8N)|0];}else{_8I=[1,I_fromInt(_8L)];_8J=[1,I_fromInt(_8N)];continue;}}else{_8I=[1,I_fromInt(_8L)];_8J=_8M;continue;}}else{var _8O=E(_8J);if(!_8O[0]){_8I=_8K;_8J=[1,I_fromInt(_8O[1])];continue;}else{return [1,I_mul(_8K[1],_8O[1])];}}}},_8P=function(_8Q,_8R,_8S){while(1){var _8T=E(_8S);if(!_8T[0]){return E(_8R);}else{var _8U=B(_8p(B(_8H(_8R,_8Q)),B(_8F(E(_8T[1])[1]))));_8S=_8T[2];_8R=_8U;continue;}}},_8V=function(_8W){var _8X=new T(function(){return B(_65(B(_65([0,function(_8Y){return E(E(_8Y)[1])==45?[1,B(_7T(_8m,function(_8Z){return new F(function(){return A(_8W,[[1,new T(function(){return B(_8z(B(_8P(_8D,_8E,_8Z))));})]]);});}))]:[2];}],[0,function(_90){return E(E(_90)[1])==43?[1,B(_7T(_8m,function(_91){return new F(function(){return A(_8W,[[1,new T(function(){return B(_8P(_8D,_8E,_91));})]]);});}))]:[2];}])),new T(function(){return [1,B(_7T(_8m,function(_92){return new F(function(){return A(_8W,[[1,new T(function(){return B(_8P(_8D,_8E,_92));})]]);});}))];})));});return new F(function(){return _65([0,function(_93){return E(E(_93)[1])==101?E(_8X):[2];}],[0,function(_94){return E(E(_94)[1])==69?E(_8X):[2];}]);});},_95=function(_96){return new F(function(){return A(_96,[_2D]);});},_97=function(_98){return new F(function(){return A(_98,[_2D]);});},_99=function(_9a){return function(_9b){return E(E(_9b)[1])==46?[1,B(_7T(_8m,function(_9c){return new F(function(){return A(_9a,[[1,_9c]]);});}))]:[2];};},_9d=function(_9e){return [0,B(_99(_9e))];},_9f=function(_9g){return new F(function(){return _7T(_8m,function(_9h){return [1,B(_7h(_9d,_95,function(_9i){return [1,B(_7h(_8V,_97,function(_9j){return new F(function(){return A(_9g,[[5,[1,_9h,_9i,_9j]]]);});}))];}))];});});},_9k=function(_9l){return [1,B(_9f(_9l))];},_9m=function(_9n,_9o,_9p){while(1){var _9q=E(_9p);if(!_9q[0]){return false;}else{if(!B(A(_2L,[_9n,_9o,_9q[1]]))){_9p=_9q[2];continue;}else{return true;}}}},_9r=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_9s=function(_9t){return new F(function(){return _9m(_6N,_9t,_9r);});},_9u=[0,8],_9v=[0,16],_9w=function(_9x){var _9y=function(_9z){return new F(function(){return A(_9x,[[5,[0,_9u,_9z]]]);});},_9A=function(_9B){return new F(function(){return A(_9x,[[5,[0,_9v,_9B]]]);});};return function(_9C){return E(E(_9C)[1])==48?E([0,function(_9D){switch(E(E(_9D)[1])){case 79:return [1,B(_7T(_9u,_9y))];case 88:return [1,B(_7T(_9v,_9A))];case 111:return [1,B(_7T(_9u,_9y))];case 120:return [1,B(_7T(_9v,_9A))];default:return [2];}}]):[2];};},_9E=function(_9F){return [0,B(_9w(_9F))];},_9G=function(_9H){var _9I=new T(function(){return B(A(_9H,[_9u]));}),_9J=new T(function(){return B(A(_9H,[_9v]));});return function(_9K){switch(E(E(_9K)[1])){case 79:return E(_9I);case 88:return E(_9J);case 111:return E(_9I);case 120:return E(_9J);default:return [2];}};},_9L=function(_9M){return [0,B(_9G(_9M))];},_9N=[0,92],_9O=function(_9P){return new F(function(){return A(_9P,[_8m]);});},_9Q=function(_9R,_9S){var _9T=jsShowI(_9R),_9U=_9T;return new F(function(){return _O(fromJSStr(_9U),_9S);});},_9V=function(_9W,_9X,_9Y){if(_9X>=0){return new F(function(){return _9Q(_9X,_9Y);});}else{return _9W<=6?B(_9Q(_9X,_9Y)):[1,_17,new T(function(){var _9Z=jsShowI(_9X),_a0=_9Z;return B(_O(fromJSStr(_a0),[1,_16,_9Y]));})];}},_a1=function(_a2){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_9V(9,_a2,_u));}))));});},_a3=function(_a4){var _a5=E(_a4);return _a5[0]==0?E(_a5[1]):I_toInt(_a5[1]);},_a6=function(_a7,_a8){var _a9=E(_a7);if(!_a9[0]){var _aa=_a9[1],_ab=E(_a8);return _ab[0]==0?_aa<=_ab[1]:I_compareInt(_ab[1],_aa)>=0;}else{var _ac=_a9[1],_ad=E(_a8);return _ad[0]==0?I_compareInt(_ac,_ad[1])<=0:I_compare(_ac,_ad[1])<=0;}},_ae=function(_af){return [2];},_ag=function(_ah){var _ai=E(_ah);if(!_ai[0]){return E(_ae);}else{var _aj=_ai[1],_ak=E(_ai[2]);return _ak[0]==0?E(_aj):function(_al){return new F(function(){return _65(B(A(_aj,[_al])),new T(function(){return B(A(new T(function(){return B(_ag(_ak));}),[_al]));}));});};}},_am=function(_an){return [2];},_ao=function(_ap,_aq){var _ar=function(_as,_at){var _au=E(_as);if(!_au[0]){return function(_av){return new F(function(){return A(_av,[_ap]);});};}else{var _aw=E(_at);return _aw[0]==0?E(_am):E(_au[1])[1]!=E(_aw[1])[1]?E(_am):function(_ax){return [0,function(_ay){return E(new T(function(){return B(A(new T(function(){return B(_ar(_au[2],_aw[2]));}),[_ax]));}));}];};}};return function(_az){return new F(function(){return A(_ar,[_ap,_az,_aq]);});};},_aA=new T(function(){return B(unCStr("SOH"));}),_aB=[0,1],_aC=function(_aD){return [1,B(_ao(_aA,function(_aE){return E(new T(function(){return B(A(_aD,[_aB]));}));}))];},_aF=new T(function(){return B(unCStr("SO"));}),_aG=[0,14],_aH=function(_aI){return [1,B(_ao(_aF,function(_aJ){return E(new T(function(){return B(A(_aI,[_aG]));}));}))];},_aK=function(_aL){return [1,B(_7h(_aC,_aH,_aL))];},_aM=new T(function(){return B(unCStr("NUL"));}),_aN=[0,0],_aO=function(_aP){return [1,B(_ao(_aM,function(_aQ){return E(new T(function(){return B(A(_aP,[_aN]));}));}))];},_aR=new T(function(){return B(unCStr("STX"));}),_aS=[0,2],_aT=function(_aU){return [1,B(_ao(_aR,function(_aV){return E(new T(function(){return B(A(_aU,[_aS]));}));}))];},_aW=new T(function(){return B(unCStr("ETX"));}),_aX=[0,3],_aY=function(_aZ){return [1,B(_ao(_aW,function(_b0){return E(new T(function(){return B(A(_aZ,[_aX]));}));}))];},_b1=new T(function(){return B(unCStr("EOT"));}),_b2=[0,4],_b3=function(_b4){return [1,B(_ao(_b1,function(_b5){return E(new T(function(){return B(A(_b4,[_b2]));}));}))];},_b6=new T(function(){return B(unCStr("ENQ"));}),_b7=[0,5],_b8=function(_b9){return [1,B(_ao(_b6,function(_ba){return E(new T(function(){return B(A(_b9,[_b7]));}));}))];},_bb=new T(function(){return B(unCStr("ACK"));}),_bc=[0,6],_bd=function(_be){return [1,B(_ao(_bb,function(_bf){return E(new T(function(){return B(A(_be,[_bc]));}));}))];},_bg=new T(function(){return B(unCStr("BEL"));}),_bh=[0,7],_bi=function(_bj){return [1,B(_ao(_bg,function(_bk){return E(new T(function(){return B(A(_bj,[_bh]));}));}))];},_bl=new T(function(){return B(unCStr("BS"));}),_bm=[0,8],_bn=function(_bo){return [1,B(_ao(_bl,function(_bp){return E(new T(function(){return B(A(_bo,[_bm]));}));}))];},_bq=new T(function(){return B(unCStr("HT"));}),_br=[0,9],_bs=function(_bt){return [1,B(_ao(_bq,function(_bu){return E(new T(function(){return B(A(_bt,[_br]));}));}))];},_bv=new T(function(){return B(unCStr("LF"));}),_bw=[0,10],_bx=function(_by){return [1,B(_ao(_bv,function(_bz){return E(new T(function(){return B(A(_by,[_bw]));}));}))];},_bA=new T(function(){return B(unCStr("VT"));}),_bB=[0,11],_bC=function(_bD){return [1,B(_ao(_bA,function(_bE){return E(new T(function(){return B(A(_bD,[_bB]));}));}))];},_bF=new T(function(){return B(unCStr("FF"));}),_bG=[0,12],_bH=function(_bI){return [1,B(_ao(_bF,function(_bJ){return E(new T(function(){return B(A(_bI,[_bG]));}));}))];},_bK=new T(function(){return B(unCStr("CR"));}),_bL=[0,13],_bM=function(_bN){return [1,B(_ao(_bK,function(_bO){return E(new T(function(){return B(A(_bN,[_bL]));}));}))];},_bP=new T(function(){return B(unCStr("SI"));}),_bQ=[0,15],_bR=function(_bS){return [1,B(_ao(_bP,function(_bT){return E(new T(function(){return B(A(_bS,[_bQ]));}));}))];},_bU=new T(function(){return B(unCStr("DLE"));}),_bV=[0,16],_bW=function(_bX){return [1,B(_ao(_bU,function(_bY){return E(new T(function(){return B(A(_bX,[_bV]));}));}))];},_bZ=new T(function(){return B(unCStr("DC1"));}),_c0=[0,17],_c1=function(_c2){return [1,B(_ao(_bZ,function(_c3){return E(new T(function(){return B(A(_c2,[_c0]));}));}))];},_c4=new T(function(){return B(unCStr("DC2"));}),_c5=[0,18],_c6=function(_c7){return [1,B(_ao(_c4,function(_c8){return E(new T(function(){return B(A(_c7,[_c5]));}));}))];},_c9=new T(function(){return B(unCStr("DC3"));}),_ca=[0,19],_cb=function(_cc){return [1,B(_ao(_c9,function(_cd){return E(new T(function(){return B(A(_cc,[_ca]));}));}))];},_ce=new T(function(){return B(unCStr("DC4"));}),_cf=[0,20],_cg=function(_ch){return [1,B(_ao(_ce,function(_ci){return E(new T(function(){return B(A(_ch,[_cf]));}));}))];},_cj=new T(function(){return B(unCStr("NAK"));}),_ck=[0,21],_cl=function(_cm){return [1,B(_ao(_cj,function(_cn){return E(new T(function(){return B(A(_cm,[_ck]));}));}))];},_co=new T(function(){return B(unCStr("SYN"));}),_cp=[0,22],_cq=function(_cr){return [1,B(_ao(_co,function(_cs){return E(new T(function(){return B(A(_cr,[_cp]));}));}))];},_ct=new T(function(){return B(unCStr("ETB"));}),_cu=[0,23],_cv=function(_cw){return [1,B(_ao(_ct,function(_cx){return E(new T(function(){return B(A(_cw,[_cu]));}));}))];},_cy=new T(function(){return B(unCStr("CAN"));}),_cz=[0,24],_cA=function(_cB){return [1,B(_ao(_cy,function(_cC){return E(new T(function(){return B(A(_cB,[_cz]));}));}))];},_cD=new T(function(){return B(unCStr("EM"));}),_cE=[0,25],_cF=function(_cG){return [1,B(_ao(_cD,function(_cH){return E(new T(function(){return B(A(_cG,[_cE]));}));}))];},_cI=new T(function(){return B(unCStr("SUB"));}),_cJ=[0,26],_cK=function(_cL){return [1,B(_ao(_cI,function(_cM){return E(new T(function(){return B(A(_cL,[_cJ]));}));}))];},_cN=new T(function(){return B(unCStr("ESC"));}),_cO=[0,27],_cP=function(_cQ){return [1,B(_ao(_cN,function(_cR){return E(new T(function(){return B(A(_cQ,[_cO]));}));}))];},_cS=new T(function(){return B(unCStr("FS"));}),_cT=[0,28],_cU=function(_cV){return [1,B(_ao(_cS,function(_cW){return E(new T(function(){return B(A(_cV,[_cT]));}));}))];},_cX=new T(function(){return B(unCStr("GS"));}),_cY=[0,29],_cZ=function(_d0){return [1,B(_ao(_cX,function(_d1){return E(new T(function(){return B(A(_d0,[_cY]));}));}))];},_d2=new T(function(){return B(unCStr("RS"));}),_d3=[0,30],_d4=function(_d5){return [1,B(_ao(_d2,function(_d6){return E(new T(function(){return B(A(_d5,[_d3]));}));}))];},_d7=new T(function(){return B(unCStr("US"));}),_d8=[0,31],_d9=function(_da){return [1,B(_ao(_d7,function(_db){return E(new T(function(){return B(A(_da,[_d8]));}));}))];},_dc=new T(function(){return B(unCStr("SP"));}),_dd=[0,32],_de=function(_df){return [1,B(_ao(_dc,function(_dg){return E(new T(function(){return B(A(_df,[_dd]));}));}))];},_dh=new T(function(){return B(unCStr("DEL"));}),_di=[0,127],_dj=function(_dk){return [1,B(_ao(_dh,function(_dl){return E(new T(function(){return B(A(_dk,[_di]));}));}))];},_dm=[1,_dj,_u],_dn=[1,_de,_dm],_do=[1,_d9,_dn],_dp=[1,_d4,_do],_dq=[1,_cZ,_dp],_dr=[1,_cU,_dq],_ds=[1,_cP,_dr],_dt=[1,_cK,_ds],_du=[1,_cF,_dt],_dv=[1,_cA,_du],_dw=[1,_cv,_dv],_dx=[1,_cq,_dw],_dy=[1,_cl,_dx],_dz=[1,_cg,_dy],_dA=[1,_cb,_dz],_dB=[1,_c6,_dA],_dC=[1,_c1,_dB],_dD=[1,_bW,_dC],_dE=[1,_bR,_dD],_dF=[1,_bM,_dE],_dG=[1,_bH,_dF],_dH=[1,_bC,_dG],_dI=[1,_bx,_dH],_dJ=[1,_bs,_dI],_dK=[1,_bn,_dJ],_dL=[1,_bi,_dK],_dM=[1,_bd,_dL],_dN=[1,_b8,_dM],_dO=[1,_b3,_dN],_dP=[1,_aY,_dO],_dQ=[1,_aT,_dP],_dR=[1,_aO,_dQ],_dS=[1,_aK,_dR],_dT=new T(function(){return B(_ag(_dS));}),_dU=[0,1114111],_dV=[0,34],_dW=[0,39],_dX=function(_dY){var _dZ=new T(function(){return B(A(_dY,[_bh]));}),_e0=new T(function(){return B(A(_dY,[_bm]));}),_e1=new T(function(){return B(A(_dY,[_br]));}),_e2=new T(function(){return B(A(_dY,[_bw]));}),_e3=new T(function(){return B(A(_dY,[_bB]));}),_e4=new T(function(){return B(A(_dY,[_bG]));}),_e5=new T(function(){return B(A(_dY,[_bL]));});return new F(function(){return _65([0,function(_e6){switch(E(E(_e6)[1])){case 34:return E(new T(function(){return B(A(_dY,[_dV]));}));case 39:return E(new T(function(){return B(A(_dY,[_dW]));}));case 92:return E(new T(function(){return B(A(_dY,[_9N]));}));case 97:return E(_dZ);case 98:return E(_e0);case 102:return E(_e4);case 110:return E(_e2);case 114:return E(_e5);case 116:return E(_e1);case 118:return E(_e3);default:return [2];}}],new T(function(){return B(_65([1,B(_7h(_9L,_9O,function(_e7){return [1,B(_7T(_e7,function(_e8){var _e9=B(_8P(new T(function(){return B(_8F(E(_e7)[1]));}),_8E,_e8));return !B(_a6(_e9,_dU))?[2]:B(A(_dY,[new T(function(){var _ea=B(_a3(_e9));if(_ea>>>0>1114111){var _eb=B(_a1(_ea));}else{var _eb=[0,_ea];}var _ec=_eb,_ed=_ec,_ee=_ed;return _ee;})]));}))];}))],new T(function(){return B(_65([0,function(_ef){return E(E(_ef)[1])==94?E([0,function(_eg){switch(E(E(_eg)[1])){case 64:return E(new T(function(){return B(A(_dY,[_aN]));}));case 65:return E(new T(function(){return B(A(_dY,[_aB]));}));case 66:return E(new T(function(){return B(A(_dY,[_aS]));}));case 67:return E(new T(function(){return B(A(_dY,[_aX]));}));case 68:return E(new T(function(){return B(A(_dY,[_b2]));}));case 69:return E(new T(function(){return B(A(_dY,[_b7]));}));case 70:return E(new T(function(){return B(A(_dY,[_bc]));}));case 71:return E(_dZ);case 72:return E(_e0);case 73:return E(_e1);case 74:return E(_e2);case 75:return E(_e3);case 76:return E(_e4);case 77:return E(_e5);case 78:return E(new T(function(){return B(A(_dY,[_aG]));}));case 79:return E(new T(function(){return B(A(_dY,[_bQ]));}));case 80:return E(new T(function(){return B(A(_dY,[_bV]));}));case 81:return E(new T(function(){return B(A(_dY,[_c0]));}));case 82:return E(new T(function(){return B(A(_dY,[_c5]));}));case 83:return E(new T(function(){return B(A(_dY,[_ca]));}));case 84:return E(new T(function(){return B(A(_dY,[_cf]));}));case 85:return E(new T(function(){return B(A(_dY,[_ck]));}));case 86:return E(new T(function(){return B(A(_dY,[_cp]));}));case 87:return E(new T(function(){return B(A(_dY,[_cu]));}));case 88:return E(new T(function(){return B(A(_dY,[_cz]));}));case 89:return E(new T(function(){return B(A(_dY,[_cE]));}));case 90:return E(new T(function(){return B(A(_dY,[_cJ]));}));case 91:return E(new T(function(){return B(A(_dY,[_cO]));}));case 92:return E(new T(function(){return B(A(_dY,[_cT]));}));case 93:return E(new T(function(){return B(A(_dY,[_cY]));}));case 94:return E(new T(function(){return B(A(_dY,[_d3]));}));case 95:return E(new T(function(){return B(A(_dY,[_d8]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_dT,[_dY]));})));})));}));});},_eh=function(_ei){return new F(function(){return A(_ei,[_7b]);});},_ej=function(_ek){var _el=E(_ek);if(!_el[0]){return E(_eh);}else{var _em=_el[2],_en=E(E(_el[1])[1]);switch(_en){case 9:return function(_eo){return [0,function(_ep){return E(new T(function(){return B(A(new T(function(){return B(_ej(_em));}),[_eo]));}));}];};case 10:return function(_eq){return [0,function(_er){return E(new T(function(){return B(A(new T(function(){return B(_ej(_em));}),[_eq]));}));}];};case 11:return function(_es){return [0,function(_et){return E(new T(function(){return B(A(new T(function(){return B(_ej(_em));}),[_es]));}));}];};case 12:return function(_eu){return [0,function(_ev){return E(new T(function(){return B(A(new T(function(){return B(_ej(_em));}),[_eu]));}));}];};case 13:return function(_ew){return [0,function(_ex){return E(new T(function(){return B(A(new T(function(){return B(_ej(_em));}),[_ew]));}));}];};case 32:return function(_ey){return [0,function(_ez){return E(new T(function(){return B(A(new T(function(){return B(_ej(_em));}),[_ey]));}));}];};case 160:return function(_eA){return [0,function(_eB){return E(new T(function(){return B(A(new T(function(){return B(_ej(_em));}),[_eA]));}));}];};default:var _eC=u_iswspace(_en),_eD=_eC;return E(_eD)==0?E(_eh):function(_eE){return [0,function(_eF){return E(new T(function(){return B(A(new T(function(){return B(_ej(_em));}),[_eE]));}));}];};}}},_eG=function(_eH){var _eI=new T(function(){return B(_eG(_eH));}),_eJ=[1,function(_eK){return new F(function(){return A(_ej,[_eK,function(_eL){return E([0,function(_eM){return E(E(_eM)[1])==92?E(_eI):[2];}]);}]);});}];return new F(function(){return _65([0,function(_eN){return E(E(_eN)[1])==92?E([0,function(_eO){var _eP=E(E(_eO)[1]);switch(_eP){case 9:return E(_eJ);case 10:return E(_eJ);case 11:return E(_eJ);case 12:return E(_eJ);case 13:return E(_eJ);case 32:return E(_eJ);case 38:return E(_eI);case 160:return E(_eJ);default:var _eQ=u_iswspace(_eP),_eR=_eQ;return E(_eR)==0?[2]:E(_eJ);}}]):[2];}],[0,function(_eS){var _eT=E(_eS);return E(_eT[1])==92?E(new T(function(){return B(_dX(function(_eU){return new F(function(){return A(_eH,[[0,_eU,_n]]);});}));})):B(A(_eH,[[0,_eT,_r]]));}]);});},_eV=function(_eW,_eX){return new F(function(){return _eG(function(_eY){var _eZ=E(_eY),_f0=E(_eZ[1]);if(E(_f0[1])==34){if(!E(_eZ[2])){return E(new T(function(){return B(A(_eX,[[1,new T(function(){return B(A(_eW,[_u]));})]]));}));}else{return new F(function(){return _eV(function(_f1){return new F(function(){return A(_eW,[[1,_f0,_f1]]);});},_eX);});}}else{return new F(function(){return _eV(function(_f2){return new F(function(){return A(_eW,[[1,_f0,_f2]]);});},_eX);});}});});},_f3=new T(function(){return B(unCStr("_\'"));}),_f4=function(_f5){var _f6=u_iswalnum(_f5),_f7=_f6;return E(_f7)==0?B(_9m(_6N,[0,_f5],_f3)):true;},_f8=function(_f9){return new F(function(){return _f4(E(_f9)[1]);});},_fa=new T(function(){return B(unCStr(",;()[]{}`"));}),_fb=new T(function(){return B(unCStr(".."));}),_fc=new T(function(){return B(unCStr("::"));}),_fd=new T(function(){return B(unCStr("->"));}),_fe=[0,64],_ff=[1,_fe,_u],_fg=[0,126],_fh=[1,_fg,_u],_fi=new T(function(){return B(unCStr("=>"));}),_fj=[1,_fi,_u],_fk=[1,_fh,_fj],_fl=[1,_ff,_fk],_fm=[1,_fd,_fl],_fn=new T(function(){return B(unCStr("<-"));}),_fo=[1,_fn,_fm],_fp=[0,124],_fq=[1,_fp,_u],_fr=[1,_fq,_fo],_fs=[1,_9N,_u],_ft=[1,_fs,_fr],_fu=[0,61],_fv=[1,_fu,_u],_fw=[1,_fv,_ft],_fx=[1,_fc,_fw],_fy=[1,_fb,_fx],_fz=function(_fA){return new F(function(){return _65([1,function(_fB){return E(_fB)[0]==0?E(new T(function(){return B(A(_fA,[_7O]));})):[2];}],new T(function(){return B(_65([0,function(_fC){return E(E(_fC)[1])==39?E([0,function(_fD){var _fE=E(_fD);switch(E(_fE[1])){case 39:return [2];case 92:return E(new T(function(){return B(_dX(function(_fF){return [0,function(_fG){return E(E(_fG)[1])==39?E(new T(function(){return B(A(_fA,[[0,_fF]]));})):[2];}];}));}));default:return [0,function(_fH){return E(E(_fH)[1])==39?E(new T(function(){return B(A(_fA,[[0,_fE]]));})):[2];}];}}]):[2];}],new T(function(){return B(_65([0,function(_fI){return E(E(_fI)[1])==34?E(new T(function(){return B(_eV(_7P,_fA));})):[2];}],new T(function(){return B(_65([0,function(_fJ){return !B(_9m(_6N,_fJ,_fa))?[2]:B(A(_fA,[[2,[1,_fJ,_u]]]));}],new T(function(){return B(_65([0,function(_fK){return !B(_9m(_6N,_fK,_9r))?[2]:[1,B(_7D(_9s,function(_fL){var _fM=[1,_fK,_fL];return !B(_9m(_6W,_fM,_fy))?B(A(_fA,[[4,_fM]])):B(A(_fA,[[2,_fM]]));}))];}],new T(function(){return B(_65([0,function(_fN){var _fO=E(_fN),_fP=_fO[1],_fQ=u_iswalpha(_fP),_fR=_fQ;return E(_fR)==0?E(_fP)==95?[1,B(_7D(_f8,function(_fS){return new F(function(){return A(_fA,[[3,[1,_fO,_fS]]]);});}))]:[2]:[1,B(_7D(_f8,function(_fT){return new F(function(){return A(_fA,[[3,[1,_fO,_fT]]]);});}))];}],new T(function(){return [1,B(_7h(_9E,_9k,_fA))];})));})));})));})));})));}));});},_fU=[0,0],_fV=function(_fW,_fX){return function(_fY){return new F(function(){return A(_ej,[_fY,function(_fZ){return E(new T(function(){return B(_fz(function(_g0){var _g1=E(_g0);return _g1[0]==2?!B(_6C(_g1[1],_6B))?[2]:E(new T(function(){return B(A(_fW,[_fU,function(_g2){return [1,function(_g3){return new F(function(){return A(_ej,[_g3,function(_g4){return E(new T(function(){return B(_fz(function(_g5){var _g6=E(_g5);return _g6[0]==2?!B(_6C(_g6[1],_6z))?[2]:E(new T(function(){return B(A(_fX,[_g2]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_g7=function(_g8,_g9,_ga){var _gb=function(_gc,_gd){return new F(function(){return _65([1,function(_ge){return new F(function(){return A(_ej,[_ge,function(_gf){return E(new T(function(){return B(_fz(function(_gg){var _gh=E(_gg);if(_gh[0]==4){var _gi=E(_gh[1]);if(!_gi[0]){return new F(function(){return A(_g8,[_gh,_gc,_gd]);});}else{return E(E(_gi[1])[1])==45?E(_gi[2])[0]==0?E([1,function(_gj){return new F(function(){return A(_ej,[_gj,function(_gk){return E(new T(function(){return B(_fz(function(_gl){return new F(function(){return A(_g8,[_gl,_gc,function(_gm){return new F(function(){return A(_gd,[new T(function(){return B(_8z(_gm));})]);});}]);});}));}));}]);});}]):B(A(_g8,[_gh,_gc,_gd])):B(A(_g8,[_gh,_gc,_gd]));}}else{return new F(function(){return A(_g8,[_gh,_gc,_gd]);});}}));}));}]);});}],new T(function(){return [1,B(_fV(_gb,_gd))];}));});};return new F(function(){return _gb(_g9,_ga);});},_gn=function(_go,_gp){return [2];},_gq=function(_gr){var _gs=E(_gr);return _gs[0]==0?[1,new T(function(){return B(_8P(new T(function(){return B(_8F(E(_gs[1])[1]));}),_8E,_gs[2]));})]:E(_gs[2])[0]==0?E(_gs[3])[0]==0?[1,new T(function(){return B(_8P(_8D,_8E,_gs[1]));})]:[0]:[0];},_gt=function(_gu){var _gv=E(_gu);if(_gv[0]==5){var _gw=B(_gq(_gv[1]));return _gw[0]==0?E(_gn):function(_gx,_gy){return new F(function(){return A(_gy,[_gw[1]]);});};}else{return E(_gn);}},_gz=function(_gA){return [1,function(_gB){return new F(function(){return A(_ej,[_gB,function(_gC){return E([3,_gA,_78]);}]);});}];},_gD=new T(function(){return B(_g7(_gt,_fU,_gz));}),_gE=[0,_2J],_gF=[0,_2H],_gG=new T(function(){return B(unCStr("Tried to deserialize a non-Number to a Double"));}),_gH=[0,_gG],_gI=[0,_1M],_gJ=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_gK=new T(function(){return B(err(_gJ));}),_gL=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_gM=new T(function(){return B(err(_gL));}),_gN=new T(function(){return [0,"lpsCoeff"];}),_gO=new T(function(){return [0,"dependCoeff"];}),_gP=new T(function(){return [0,"maxLoves"];}),_gQ=new T(function(){return [0,"items"];}),_gR=new T(function(){return [0,"achievements"];}),_gS=new T(function(){return [0,"lastFocus"];}),_gT=new T(function(){return [0,"depend"];}),_gU=new T(function(){return [0,"lps"];}),_gV=new T(function(){return [0,"loves"];}),_gW=function(_gX){while(1){var _gY=(function(_gZ){var _h0=E(_gZ);if(!_h0[0]){return [0];}else{var _h1=_h0[2],_h2=E(_h0[1]);if(!E(_h2[2])[0]){return [1,_h2[1],new T(function(){return B(_gW(_h1));})];}else{_gX=_h1;return null;}}})(_gX);if(_gY!=null){return _gY;}}},_h3=function(_){var _h4=jsEval("Date.now()"),_h5=_h4;return new T(function(){var _h6=B(_gW(B(_5V(_gD,new T(function(){return fromJSStr(_h5);})))));return _h6[0]==0?B(err(_gJ)):E(_h6[2])[0]==0?E(_h6[1]):B(err(_gL));});},_h7=[0,0],_h8=[0,1],_h9=function(_){var _=0,_ha=B(_h3(_)),_hb=_ha;return [0,_h7,_h7,_h7,_hb,_r,_45,_45,_h7,_h8,_h8];},_hc=function(_hd){var _he=B(A(_hd,[_])),_hf=_he;return E(_hf);},_hg=new T(function(){return B(_hc(_h9));}),_hh=function(_hi){var _hj=E(_hi);if(_hj[0]==4){var _hk=_hj[1],_hl=B(_2N(_2C,_gV,_hk));if(!_hl[0]){return E(_gF);}else{var _hm=E(_hl[1]);if(!_hm[0]){var _hn=_hm[1],_ho=B(_2N(_2C,_gU,_hk));if(!_ho[0]){return E(_gF);}else{var _hp=E(_ho[1]);if(!_hp[0]){var _hq=_hp[1],_hr=B(_2N(_2C,_gT,_hk));if(!_hr[0]){return E(_gF);}else{var _hs=E(_hr[1]);if(!_hs[0]){var _ht=_hs[1],_hu=B(_2N(_2C,_gS,_hk));if(!_hu[0]){return E(_gF);}else{var _hv=E(_hu[1]);if(_hv[0]==1){var _hw=_hv[1],_hx=B(_2N(_2C,_gR,_hk));if(!_hx[0]){return E(_gF);}else{var _hy=B(_3v(_44,_3h,_hx[1]));if(!_hy[0]){return [0,_hy[1]];}else{var _hz=_hy[1],_hA=B(_2N(_2C,_gQ,_hk));if(!_hA[0]){return E(_gF);}else{var _hB=B(_3v(_44,_44,_hA[1]));if(!_hB[0]){return [0,_hB[1]];}else{var _hC=_hB[1],_hD=B(_2N(_2C,_gP,_hk));if(!_hD[0]){return E(_gF);}else{var _hE=E(_hD[1]);if(!_hE[0]){var _hF=_hE[1],_hG=function(_hH){var _hI=function(_hJ){return [1,new T(function(){var _hK=E(_hg),_hL=_hK[5],_hM=_hK[9],_hN=new T(function(){var _hO=B(_gW(B(_5V(_gD,new T(function(){return fromJSStr(E(_hw)[1]);})))));return _hO[0]==0?E(_gK):E(_hO[2])[0]==0?E(_hO[1]):E(_gM);}),_hP=E(_hJ);return _hP[0]==0?[0,_hn,_hq,_ht,_hN,_hL,new T(function(){return B(_4x(_45,_hz));}),new T(function(){return B(_4x(_45,_hC));}),_hF,_hM,_hK[10]]:[0,_hn,_hq,_ht,_hN,_hL,new T(function(){return B(_4x(_45,_hz));}),new T(function(){return B(_4x(_45,_hC));}),_hF,_hM,_hP[1]];})];},_hQ=B(_2N(_2C,_gN,_hk));if(!_hQ[0]){return new F(function(){return _hI(_2D);});}else{var _hR=E(_hQ[1]);return _hR[0]==0?B(_hI([1,_hR[1]])):B(_hI(_2D));}},_hS=B(_2N(_2C,_gO,_hk));if(!_hS[0]){return new F(function(){return _hG(_);});}else{var _hT=E(_hS[1]);if(!_hT[0]){var _hU=_hT[1],_hV=function(_hW){return [1,new T(function(){var _hX=E(_hg),_hY=_hX[5],_hZ=new T(function(){var _i0=B(_gW(B(_5V(_gD,new T(function(){return fromJSStr(E(_hw)[1]);})))));return _i0[0]==0?E(_gK):E(_i0[2])[0]==0?E(_i0[1]):E(_gM);}),_i1=E(_hW);return _i1[0]==0?[0,_hn,_hq,_ht,_hZ,_hY,new T(function(){return B(_4x(_45,_hz));}),new T(function(){return B(_4x(_45,_hC));}),_hF,_hU,_hX[10]]:[0,_hn,_hq,_ht,_hZ,_hY,new T(function(){return B(_4x(_45,_hz));}),new T(function(){return B(_4x(_45,_hC));}),_hF,_hU,_i1[1]];})];},_i2=B(_2N(_2C,_gN,_hk));if(!_i2[0]){return new F(function(){return _hV(_2D);});}else{var _i3=E(_i2[1]);return _i3[0]==0?B(_hV([1,_i3[1]])):B(_hV(_2D));}}else{return new F(function(){return _hG(_);});}}}else{return E(_gH);}}}}}}}else{return E(_gI);}}}else{return E(_gH);}}}else{return E(_gH);}}}else{return E(_gH);}}}else{return E(_gE);}},_i4=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_i5=[0,_i4],_i6=[1,_u],_i7=function(_i8){var _i9=E(_i8);if(!_i9[0]){return E(_i6);}else{var _ia=B(_hh(_i9[1]));if(!_ia[0]){return [0,_ia[1]];}else{var _ib=B(_i7(_i9[2]));return _ib[0]==0?[0,_ib[1]]:[1,[1,_ia[1],_ib[1]]];}}},_ic=function(_id){var _ie=E(_id);return _ie[0]==3?B(_i7(_ie[1])):E(_i5);},_if=[0,_1v,_1C,_hh,_ic],_ig=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_ih=new T(function(){return B(err(_ig));}),_ii=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_ij=new T(function(){return B(err(_ii));}),_ik=function(_il,_im){while(1){var _in=E(_il);if(!_in[0]){return E(_ij);}else{var _io=E(_im);if(!_io){return E(_in[1]);}else{_il=_in[2];_im=_io-1|0;continue;}}}},_ip=new T(function(){return B(unCStr("ACK"));}),_iq=new T(function(){return B(unCStr("BEL"));}),_ir=new T(function(){return B(unCStr("BS"));}),_is=new T(function(){return B(unCStr("SP"));}),_it=[1,_is,_u],_iu=new T(function(){return B(unCStr("US"));}),_iv=[1,_iu,_it],_iw=new T(function(){return B(unCStr("RS"));}),_ix=[1,_iw,_iv],_iy=new T(function(){return B(unCStr("GS"));}),_iz=[1,_iy,_ix],_iA=new T(function(){return B(unCStr("FS"));}),_iB=[1,_iA,_iz],_iC=new T(function(){return B(unCStr("ESC"));}),_iD=[1,_iC,_iB],_iE=new T(function(){return B(unCStr("SUB"));}),_iF=[1,_iE,_iD],_iG=new T(function(){return B(unCStr("EM"));}),_iH=[1,_iG,_iF],_iI=new T(function(){return B(unCStr("CAN"));}),_iJ=[1,_iI,_iH],_iK=new T(function(){return B(unCStr("ETB"));}),_iL=[1,_iK,_iJ],_iM=new T(function(){return B(unCStr("SYN"));}),_iN=[1,_iM,_iL],_iO=new T(function(){return B(unCStr("NAK"));}),_iP=[1,_iO,_iN],_iQ=new T(function(){return B(unCStr("DC4"));}),_iR=[1,_iQ,_iP],_iS=new T(function(){return B(unCStr("DC3"));}),_iT=[1,_iS,_iR],_iU=new T(function(){return B(unCStr("DC2"));}),_iV=[1,_iU,_iT],_iW=new T(function(){return B(unCStr("DC1"));}),_iX=[1,_iW,_iV],_iY=new T(function(){return B(unCStr("DLE"));}),_iZ=[1,_iY,_iX],_j0=new T(function(){return B(unCStr("SI"));}),_j1=[1,_j0,_iZ],_j2=new T(function(){return B(unCStr("SO"));}),_j3=[1,_j2,_j1],_j4=new T(function(){return B(unCStr("CR"));}),_j5=[1,_j4,_j3],_j6=new T(function(){return B(unCStr("FF"));}),_j7=[1,_j6,_j5],_j8=new T(function(){return B(unCStr("VT"));}),_j9=[1,_j8,_j7],_ja=new T(function(){return B(unCStr("LF"));}),_jb=[1,_ja,_j9],_jc=new T(function(){return B(unCStr("HT"));}),_jd=[1,_jc,_jb],_je=[1,_ir,_jd],_jf=[1,_iq,_je],_jg=[1,_ip,_jf],_jh=new T(function(){return B(unCStr("ENQ"));}),_ji=[1,_jh,_jg],_jj=new T(function(){return B(unCStr("EOT"));}),_jk=[1,_jj,_ji],_jl=new T(function(){return B(unCStr("ETX"));}),_jm=[1,_jl,_jk],_jn=new T(function(){return B(unCStr("STX"));}),_jo=[1,_jn,_jm],_jp=new T(function(){return B(unCStr("SOH"));}),_jq=[1,_jp,_jo],_jr=new T(function(){return B(unCStr("NUL"));}),_js=[1,_jr,_jq],_jt=[0,92],_ju=new T(function(){return B(unCStr("\\DEL"));}),_jv=new T(function(){return B(unCStr("\\a"));}),_jw=new T(function(){return B(unCStr("\\\\"));}),_jx=new T(function(){return B(unCStr("\\SO"));}),_jy=new T(function(){return B(unCStr("\\r"));}),_jz=new T(function(){return B(unCStr("\\f"));}),_jA=new T(function(){return B(unCStr("\\v"));}),_jB=new T(function(){return B(unCStr("\\n"));}),_jC=new T(function(){return B(unCStr("\\t"));}),_jD=new T(function(){return B(unCStr("\\b"));}),_jE=function(_jF,_jG){if(_jF<=127){var _jH=E(_jF);switch(_jH){case 92:return new F(function(){return _O(_jw,_jG);});break;case 127:return new F(function(){return _O(_ju,_jG);});break;default:if(_jH<32){var _jI=E(_jH);switch(_jI){case 7:return new F(function(){return _O(_jv,_jG);});break;case 8:return new F(function(){return _O(_jD,_jG);});break;case 9:return new F(function(){return _O(_jC,_jG);});break;case 10:return new F(function(){return _O(_jB,_jG);});break;case 11:return new F(function(){return _O(_jA,_jG);});break;case 12:return new F(function(){return _O(_jz,_jG);});break;case 13:return new F(function(){return _O(_jy,_jG);});break;case 14:return new F(function(){return _O(_jx,new T(function(){var _jJ=E(_jG);if(!_jJ[0]){var _jK=[0];}else{var _jK=E(E(_jJ[1])[1])==72?B(unAppCStr("\\&",_jJ)):E(_jJ);}return _jK;}));});break;default:return new F(function(){return _O([1,_jt,new T(function(){var _jL=_jI;return _jL>=0?B(_ik(_js,_jL)):E(_ih);})],_jG);});}}else{return [1,[0,_jH],_jG];}}}else{return [1,_jt,new T(function(){var _jM=jsShowI(_jF),_jN=_jM;return B(_O(fromJSStr(_jN),new T(function(){var _jO=E(_jG);if(!_jO[0]){var _jP=[0];}else{var _jQ=E(_jO[1])[1];if(_jQ<48){var _jR=E(_jO);}else{var _jR=_jQ>57?E(_jO):B(unAppCStr("\\&",_jO));}var _jS=_jR,_jT=_jS,_jP=_jT;}return _jP;})));})];}},_jU=[0,39],_jV=[1,_jU,_u],_jW=new T(function(){return B(unCStr("\'\\\'\'"));}),_jX=function(_jY){var _jZ=E(E(_jY)[1]);return _jZ==39?E(_jW):[1,_jU,new T(function(){return B(_jE(_jZ,_jV));})];},_k0=[0,34],_k1=new T(function(){return B(unCStr("\\\""));}),_k2=function(_k3,_k4){var _k5=E(_k3);if(!_k5[0]){return E(_k4);}else{var _k6=_k5[2],_k7=E(E(_k5[1])[1]);if(_k7==34){return new F(function(){return _O(_k1,new T(function(){return B(_k2(_k6,_k4));}));});}else{return new F(function(){return _jE(_k7,new T(function(){return B(_k2(_k6,_k4));}));});}}},_k8=function(_k9,_ka){return [1,_k0,new T(function(){return B(_k2(_k9,[1,_k0,_ka]));})];},_kb=function(_kc){return new F(function(){return _O(_jW,_kc);});},_kd=function(_ke,_kf){var _kg=E(E(_kf)[1]);return _kg==39?E(_kb):function(_kh){return [1,_jU,new T(function(){return B(_jE(_kg,[1,_jU,_kh]));})];};},_ki=[0,_kd,_jX,_k8],_kj=function(_kk){return E(E(_kk)[3]);},_kl=function(_km,_kn){return new F(function(){return A(_kj,[_km,_kn,_u]);});},_ko=function(_kp,_kq,_kr){return new F(function(){return _5a(new T(function(){return B(_kj(_kp));}),_kq,_kr);});},_ks=function(_kt){return [0,function(_ku){return E(new T(function(){return B(_kj(_kt));}));},function(_kc){return new F(function(){return _kl(_kt,_kc);});},function(_kv,_kc){return new F(function(){return _ko(_kt,_kv,_kc);});}];},_kw=new T(function(){return B(_ks(_ki));}),_kx=new T(function(){return B(unCStr("Just "));}),_ky=new T(function(){return B(unCStr("Nothing"));}),_kz=[0,11],_kA=function(_kB){return E(E(_kB)[1]);},_kC=function(_kD,_kE,_kF,_kG){var _kH=E(_kF);if(!_kH[0]){return new F(function(){return _O(_ky,_kG);});}else{var _kI=_kH[1];return E(_kE)[1]<=10?B(_O(_kx,new T(function(){return B(A(_kA,[_kD,_kz,_kI,_kG]));}))):[1,_17,new T(function(){return B(_O(_kx,new T(function(){return B(A(_kA,[_kD,_kz,_kI,[1,_16,_kG]]));})));})];}},_kJ=[0,0],_kK=function(_kL,_kM){return new F(function(){return _kC(_kL,_kJ,_kM,_u);});},_kN=function(_kO,_kP,_kQ){return new F(function(){return _5a(function(_kv,_kc){return new F(function(){return _kC(_kO,_kJ,_kv,_kc);});},_kP,_kQ);});},_kR=function(_kS){return [0,function(_kT,_kv,_kc){return new F(function(){return _kC(_kS,_kT,_kv,_kc);});},function(_kc){return new F(function(){return _kK(_kS,_kc);});},function(_kv,_kc){return new F(function(){return _kN(_kS,_kv,_kc);});}];},_kU=new T(function(){return B(_kR(_kw));}),_kV=function(_kW){var _kX=jsShow(E(_kW)[1]),_kY=_kX;return new F(function(){return fromJSStr(_kY);});},_kZ=function(_l0){return function(_7A){return new F(function(){return _O(new T(function(){return B(_kV(_l0));}),_7A);});};},_l1=function(_l2){return new F(function(){return _9V(0,E(_l2)[1],_u);});},_l3=function(_l4,_l5){return new F(function(){return _9V(0,E(_l4)[1],_l5);});},_l6=function(_l7,_l8){return new F(function(){return _5a(_l3,_l7,_l8);});},_l9=function(_la,_lb,_lc){return new F(function(){return _9V(E(_la)[1],E(_lb)[1],_lc);});},_ld=[0,_l9,_l1,_l6],_le=function(_lf,_lg,_lh){return new F(function(){return A(_lf,[[1,_57,new T(function(){return B(A(_lg,[_lh]));})]]);});},_li=new T(function(){return B(unCStr(": empty list"));}),_lj=new T(function(){return B(unCStr("Prelude."));}),_lk=function(_ll){return new F(function(){return err(B(_O(_lj,new T(function(){return B(_O(_ll,_li));}))));});},_lm=new T(function(){return B(unCStr("foldr1"));}),_ln=new T(function(){return B(_lk(_lm));}),_lo=function(_lp,_lq){var _lr=E(_lq);if(!_lr[0]){return E(_ln);}else{var _ls=_lr[1],_lt=E(_lr[2]);if(!_lt[0]){return E(_ls);}else{return new F(function(){return A(_lp,[_ls,new T(function(){return B(_lo(_lp,_lt));})]);});}}},_lu=function(_lv,_lw,_lx,_ly){return new F(function(){return _5a(function(_lz,_lA){var _lB=E(_lz);return [1,_17,new T(function(){return B(A(_lo,[_le,[1,new T(function(){return B(A(new T(function(){return B(_kA(_lv));}),[_kJ,_lB[1]]));}),[1,new T(function(){return B(A(new T(function(){return B(_kA(_lw));}),[_kJ,_lB[2]]));}),_u]],[1,_16,_lA]]));})];},_lx,_ly);});},_lC=new T(function(){return B(unCStr("fromList "));}),_lD=function(_lE,_lF){while(1){var _lG=(function(_lH,_lI){var _lJ=E(_lI);switch(_lJ[0]){case 0:_lE=new T(function(){return B(_lD(_lH,_lJ[4]));});_lF=_lJ[3];return null;case 1:return [1,[0,[0,_lJ[1]],_lJ[2]],_lH];default:return E(_lH);}})(_lE,_lF);if(_lG!=null){return _lG;}}},_lK=function(_lL){var _lM=E(_lL);if(!_lM[0]){var _lN=_lM[3],_lO=_lM[4];return _lM[2]>=0?B(_lD(new T(function(){return B(_lD(_u,_lO));}),_lN)):B(_lD(new T(function(){return B(_lD(_u,_lN));}),_lO));}else{return new F(function(){return _lD(_u,_lM);});}},_lP=function(_lQ,_lR,_lS){var _lT=new T(function(){return B(_lK(_lS));});return _lR<=10?function(_lU){return new F(function(){return _O(_lC,new T(function(){return B(_lu(_ld,_lQ,_lT,_lU));}));});}:function(_lV){return [1,_17,new T(function(){return B(_O(_lC,new T(function(){return B(_lu(_ld,_lQ,_lT,[1,_16,_lV]));})));})];};},_lW=[0,45],_lX=function(_lY,_lZ,_m0){var _m1=function(_m2){var _m3=new T(function(){return B(A(_lY,[[0, -_m0]]));});return E(_lZ)[1]<=6?function(_m4){return [1,_lW,new T(function(){return B(A(_m3,[_m4]));})];}:function(_m5){return [1,_17,[1,_lW,new T(function(){return B(A(_m3,[[1,_16,_m5]]));})]];};};if(_m0>=0){var _m6=isDoubleNegativeZero(_m0),_m7=_m6;return E(_m7)==0?B(A(_lY,[[0,_m0]])):B(_m1(_));}else{return new F(function(){return _m1(_);});}},_m8=[0,0],_m9=[0,125],_ma=new T(function(){return B(unCStr("_lpsCoeff = "));}),_mb=new T(function(){return B(unCStr(", "));}),_mc=new T(function(){return B(unCStr("_dependCoeff = "));}),_md=new T(function(){return B(unCStr("_maxLoves = "));}),_me=new T(function(){return B(unCStr("_items = "));}),_mf=new T(function(){return B(unCStr("_achieves = "));}),_mg=new T(function(){return B(unCStr("_hasFocus = "));}),_mh=new T(function(){return B(unCStr("_lastFocus = "));}),_mi=new T(function(){return B(unCStr("_depend = "));}),_mj=new T(function(){return B(unCStr("_lps = "));}),_mk=new T(function(){return B(unCStr("_loves = "));}),_ml=new T(function(){return B(unCStr("Aichan {"));}),_mm=new T(function(){return B(unCStr("True"));}),_mn=new T(function(){return B(unCStr("False"));}),_mo=function(_mp,_mq,_mr,_ms,_mt,_mu,_mv,_mw,_mx,_my,_mz){var _mA=function(_mB){return new F(function(){return _O(_ml,new T(function(){return B(_O(_mk,new T(function(){return B(A(new T(function(){return B(_lX(_kZ,_m8,E(_mq)[1]));}),[new T(function(){return B(_O(_mb,new T(function(){return B(_O(_mj,new T(function(){return B(A(new T(function(){return B(_lX(_kZ,_m8,E(_mr)[1]));}),[new T(function(){return B(_O(_mb,new T(function(){return B(_O(_mi,new T(function(){return B(A(new T(function(){return B(_lX(_kZ,_m8,E(_ms)[1]));}),[new T(function(){return B(_O(_mb,new T(function(){return B(_O(_mh,new T(function(){return B(_19(0,_mt,new T(function(){return B(_O(_mb,new T(function(){return B(_O(_mg,new T(function(){var _mC=new T(function(){return B(_O(_mb,new T(function(){return B(_O(_mf,new T(function(){return B(A(new T(function(){return B(_lP(_kU,0,_mv));}),[new T(function(){return B(_O(_mb,new T(function(){return B(_O(_me,new T(function(){return B(A(new T(function(){return B(_lP(_ld,0,_mw));}),[new T(function(){return B(_O(_mb,new T(function(){return B(_O(_md,new T(function(){return B(A(new T(function(){return B(_lX(_kZ,_m8,E(_mx)[1]));}),[new T(function(){return B(_O(_mb,new T(function(){return B(_O(_mc,new T(function(){return B(A(new T(function(){return B(_lX(_kZ,_m8,E(_my)[1]));}),[new T(function(){return B(_O(_mb,new T(function(){return B(_O(_ma,new T(function(){return B(A(new T(function(){return B(_lX(_kZ,_m8,E(_mz)[1]));}),[[1,_m9,_mB]]));})));})));})]));})));})));})]));})));})));})]));})));})));})]));})));})));});return !E(_mu)?B(_O(_mn,_mC)):B(_O(_mm,_mC));})));})));})));})));})));})]));})));})));})]));})));})));})]));})));}));});};return _mp<11?E(_mA):function(_mD){return [1,_17,new T(function(){return B(_mA([1,_16,_mD]));})];};},_mE=function(_mF){var _mG=E(_mF);return new F(function(){return A(_mo,[0,_mG[1],_mG[2],_mG[3],_mG[4],_mG[5],_mG[6],_mG[7],_mG[8],_mG[9],_mG[10],_u]);});},_mH=function(_mI,_mJ,_mK,_){var _mL=rMV(_mJ),_mM=_mL,_mN=B(A(_mK,[_mM,_])),_mO=_mN,_=wMV(_mJ,new T(function(){return E(E(_mO)[2]);})),_mP=jsSetTimeout(_mI,function(_){var _mQ=B(_mH(_mI,_mJ,_mK,_)),_mR=_mQ;return _7b;});return new F(function(){return rMV(_mJ);});},_mS=new T(function(){return B(unCStr(" is not an element of the map"));}),_mT=function(_mU){return new F(function(){return err(B(unAppCStr("IntMap.!: key ",new T(function(){return B(_O(B(_9V(0,_mU,_u)),_mS));}))));});},_mV=function(_mW,_mX){var _mY=new T(function(){return B(_mT(_mX));});return new F(function(){return (function(_mZ){while(1){var _n0=E(_mZ);switch(_n0[0]){case 0:var _n1=_n0[2]>>>0;if(((_mX>>>0&((_n1-1>>>0^4294967295)>>>0^_n1)>>>0)>>>0&4294967295)==_n0[1]){if(!((_mX>>>0&_n1)>>>0)){_mZ=_n0[3];continue;}else{_mZ=_n0[4];continue;}}else{return E(_mY);}break;case 1:return _mX!=_n0[1]?E(_mY):E(_n0[2]);default:return E(_mY);}}})(_mW);});},_n2=function(_n3,_n4){return new F(function(){return (function(_n5){while(1){var _n6=E(_n5);switch(_n6[0]){case 0:var _n7=_n6[2]>>>0;if(((_n3>>>0&((_n7-1>>>0^4294967295)>>>0^_n7)>>>0)>>>0&4294967295)==_n6[1]){if(!((_n3>>>0&_n7)>>>0)){_n5=_n6[3];continue;}else{_n5=_n6[4];continue;}}else{return false;}break;case 1:return _n3==_n6[1];default:return false;}}})(_n4);});},_n8=function(_n9){var _na=E(_n9);if(!_na[0]){return [0,_u,_u];}else{var _nb=E(_na[1]),_nc=new T(function(){var _nd=B(_n8(_na[2]));return [0,_nd[1],_nd[2]];});return E(_nb[1])[1]<=0?[0,new T(function(){return E(E(_nc)[1]);}),[1,_nb,new T(function(){return E(E(_nc)[2]);})]]:[0,[1,_nb,new T(function(){return E(E(_nc)[1]);})],new T(function(){return E(E(_nc)[2]);})];}},_ne=function(_nf,_ng){if(_nf<=_ng){var _nh=function(_ni){return [1,[0,_ni],new T(function(){if(_ni!=_ng){var _nj=B(_nh(_ni+1|0));}else{var _nj=[0];}var _nk=_nj;return _nk;})];};return new F(function(){return _nh(_nf);});}else{return [0];}},_nl=[0,23478],_nm=[1,_nl,_u],_nn=new T(function(){return B(unCStr("\u5bb6<br>\u597d\u611f\u5ea6 +12000"));}),_no=new T(function(){return B(unCStr("fa-home"));}),_np=[0,_no,_nn,_nm],_nq=function(_nr,_ns,_){return [0,_7b,new T(function(){var _nt=E(_ns);return [0,_nt[1],new T(function(){return [0,E(_nt[2])[1]+12000];}),_nt[3],_nt[4],_nt[5],_nt[6],_nt[7],_nt[8],_nt[9],_nt[10]];})];},_nu=new T(function(){return B(unCStr("Negative exponent"));}),_nv=new T(function(){return B(err(_nu));}),_nw=function(_nx,_ny,_nz){while(1){if(!(_ny%2)){var _nA=_nx*_nx,_nB=quot(_ny,2);_nx=_nA;_ny=_nB;continue;}else{var _nC=E(_ny);if(_nC==1){return _nx*_nz;}else{var _nA=_nx*_nx;_ny=quot(_nC-1|0,2);var _nD=_nx*_nz;_nx=_nA;_nz=_nD;continue;}}}},_nE=function(_nF,_nG){while(1){if(!(_nG%2)){var _nH=_nF*_nF,_nI=quot(_nG,2);_nF=_nH;_nG=_nI;continue;}else{var _nJ=E(_nG);if(_nJ==1){return E(_nF);}else{return new F(function(){return _nw(_nF*_nF,quot(_nJ-1|0,2),_nF);});}}}},_nK=function(_nL){var _nM=I_decodeDouble(_nL);return [0,[1,_nM[2]],_nM[1]];},_nN=function(_nO){var _nP=hs_intToInt64(2147483647),_nQ=_nP,_nR=hs_leInt64(_nO,_nQ),_nS=_nR;if(!E(_nS)){return [1,I_fromInt64(_nO)];}else{var _nT=hs_intToInt64(-2147483648),_nU=_nT,_nV=hs_geInt64(_nO,_nU),_nW=_nV;if(!E(_nW)){return [1,I_fromInt64(_nO)];}else{var _nX=hs_int64ToInt(_nO),_nY=_nX;return new F(function(){return _8F(_nY);});}}},_nZ=function(_o0){var _o1=hs_intToInt64(_o0),_o2=_o1;return E(_o2);},_o3=function(_o4){var _o5=E(_o4);return _o5[0]==0?B(_nZ(_o5[1])):I_toInt64(_o5[1]);},_o6=[0,0],_o7=new T(function(){return [0,0/0];}),_o8=new T(function(){return [0,-1/0];}),_o9=new T(function(){return [0,1/0];}),_oa=[0,0],_ob=function(_oc,_od){while(1){var _oe=E(_oc);if(!_oe[0]){_oc=[1,I_fromInt(_oe[1])];continue;}else{var _of=E(_od);if(!_of[0]){_oc=_oe;_od=[1,I_fromInt(_of[1])];continue;}else{return new F(function(){return I_fromRat(_oe[1],_of[1]);});}}}},_og=function(_oh,_oi){var _oj=E(_oh);if(!_oj[0]){var _ok=_oj[1],_ol=E(_oi);return _ol[0]==0?_ok==_ol[1]:I_compareInt(_ol[1],_ok)==0?true:false;}else{var _om=_oj[1],_on=E(_oi);return _on[0]==0?I_compareInt(_om,_on[1])==0?true:false:I_compare(_om,_on[1])==0?true:false;}},_oo=function(_op,_oq){return !B(_og(_oq,_oa))?[0,B(_ob(_op,_oq))]:!B(_og(_op,_oa))?!B(_Y(_op,_oa))?E(_o9):E(_o8):E(_o7);},_or=[0,27],_os=[0,20],_ot=new T(function(){return B(_oo(_or,_os));}),_ou=[0,-1],_ov=function(_ow,_ox){while(1){var _oy=E(_ow);if(!_oy[0]){_ow=[1,I_fromInt(_oy[1])];continue;}else{return [1,I_shiftLeft(_oy[1],_ox)];}}},_oz=function(_oA,_oB){if(_oB>=0){var _oC=function(_oD){var _oE=B(_nK(_oA*_oD)),_oF=_oE[1],_oG=_oE[2];if(_oG>=0){return new F(function(){return _ov(_oF,_oG);});}else{var _oH= -_oG;if(_oH<=52){var _oI=hs_uncheckedIShiftRA64(B(_o3(_oF)),_oH),_oJ=_oI;return new F(function(){return _nN(_oJ);});}else{return !B(_Y(_oF,_o6))?E(_o6):E(_ou);}}},_oK=E(_oB);if(!_oK){return new F(function(){return _oC(1);});}else{return new F(function(){return _oC(B(_nE(E(_ot)[1],_oK)));});}}else{return E(_nv);}},_oL=function(_oM){return new F(function(){return _oz(250000000,E(_oM)[1]);});},_oN=[0,_oL,_nq,_np],_oO=[0,36554],_oP=[1,_oO,_u],_oQ=new T(function(){return B(unCStr("\u8eca<br>\u597d\u611f\u5ea6 +5000"));}),_oR=new T(function(){return B(unCStr("fa-car"));}),_oS=[0,_oR,_oQ,_oP],_oT=function(_oU,_oV,_){return [0,_7b,new T(function(){var _oW=E(_oV);return [0,_oW[1],new T(function(){return [0,E(_oW[2])[1]+5000];}),_oW[3],_oW[4],_oW[5],_oW[6],_oW[7],_oW[8],_oW[9],_oW[10]];})];},_oX=function(_oY){return new F(function(){return _oz(10000000,E(_oY)[1]);});},_oZ=[0,_oX,_oT,_oS],_p0=new T(function(){return B(unCStr("\u65c5\u884c"));}),_p1=new T(function(){return B(unCStr("\u65c5\u884c<br>\u597d\u611f\u5ea6 +600"));}),_p2=new T(function(){return B(unCStr("fa-plane"));}),_p3=[0,_p2,_p1,_p0],_p4=function(_p5,_p6,_){return [0,_7b,new T(function(){var _p7=E(_p6);return [0,_p7[1],new T(function(){return [0,E(_p7[2])[1]+600];}),_p7[3],_p7[4],_p7[5],_p7[6],_p7[7],_p7[8],_p7[9],_p7[10]];})];},_p8=function(_p9){return new F(function(){return _oz(500000,E(_p9)[1]);});},_pa=[0,_p8,_p4,_p3],_pb=new T(function(){return B(unCStr("\u30d7\u30ec\u30bc\u30f3\u30c8"));}),_pc=new T(function(){return B(unCStr("\u30d7\u30ec\u30bc\u30f3\u30c8<br>\u597d\u611f\u5ea6 +100"));}),_pd=new T(function(){return B(unCStr("fa-gift"));}),_pe=[0,_pd,_pc,_pb],_pf=function(_pg,_ph,_){return [0,_7b,new T(function(){var _pi=E(_ph);return [0,_pi[1],new T(function(){return [0,E(_pi[2])[1]+100];}),_pi[3],_pi[4],_pi[5],_pi[6],_pi[7],_pi[8],_pi[9],_pi[10]];})];},_pj=function(_pk){return new F(function(){return _oz(20000,E(_pk)[1]);});},_pl=[0,_pj,_pf,_pe],_pm=new T(function(){return B(unCStr("\u55ab\u8336\u5e97"));}),_pn=new T(function(){return B(unCStr("\u55ab\u8336\u5e97<br>\u597d\u611f\u5ea6 +10"));}),_po=new T(function(){return B(unCStr("fa-coffee"));}),_pp=[0,_po,_pn,_pm],_pq=function(_pr,_ps,_){return [0,_7b,new T(function(){var _pt=E(_ps);return [0,_pt[1],new T(function(){return [0,E(_pt[2])[1]+10];}),_pt[3],_pt[4],_pt[5],_pt[6],_pt[7],_pt[8],_pt[9],_pt[10]];})];},_pu=function(_pv){return new F(function(){return _oz(1000,E(_pv)[1]);});},_pw=[0,_pu,_pq,_pp],_px=new T(function(){return B(unCStr("\u30e1\u30fc\u30eb"));}),_py=new T(function(){return B(unCStr("\u30e1\u30fc\u30eb<br>\u597d\u611f\u5ea6 +1.0"));}),_pz=new T(function(){return B(unCStr("fa-envelope"));}),_pA=[0,_pz,_py,_px],_pB=function(_pC,_pD,_){return [0,_7b,new T(function(){var _pE=E(_pD);return [0,_pE[1],new T(function(){return [0,E(_pE[2])[1]+1];}),_pE[3],_pE[4],_pE[5],_pE[6],_pE[7],_pE[8],_pE[9],_pE[10]];})];},_pF=function(_pG){return new F(function(){return _oz(50,E(_pG)[1]);});},_pH=[0,_pF,_pB,_pA],_pI=new T(function(){return B(unCStr("\u4f1a\u8a71"));}),_pJ=new T(function(){return B(unCStr("\u4f1a\u8a71<br>\u597d\u611f\u5ea6 +0.2"));}),_pK=new T(function(){return B(unCStr("fa-comments-o"));}),_pL=[0,_pK,_pJ,_pI],_pM=function(_pN,_pO,_){return [0,_7b,new T(function(){var _pP=E(_pO);return [0,_pP[1],new T(function(){return [0,E(_pP[2])[1]+0.2];}),_pP[3],_pP[4],_pP[5],_pP[6],_pP[7],_pP[8],_pP[9],_pP[10]];})];},_pQ=function(_pR){return new F(function(){return _oz(1,E(_pR)[1]);});},_pS=[0,_pQ,_pM,_pL],_pT=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u53f3\u76ee+"));}),_pU=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u53f3\u76ee<br>\u4f9d\u5b58\u5ea6\u30dc\u30fc\u30ca\u30b9\u304c100\u500d\u306b\u306a\u308a\u307e\u3059\u3002"));}),_pV=new T(function(){return B(unCStr("fa-eye-slash"));}),_pW=[0,_pV,_pU,_pT],_pX=[0,100],_pY=function(_pZ,_q0,_){return [0,_7b,new T(function(){var _q1=E(_q0);return [0,_q1[1],_q1[2],_q1[3],_q1[4],_q1[5],_q1[6],_q1[7],_q1[8],_pX,_q1[10]];})];},_q2=[0,10000000],_q3=function(_q4){return E(_q2);},_q5=[0,_q3,_pY,_pW],_q6=function(_q7,_q8,_){return [0,_7b,new T(function(){var _q9=E(_q8);return [0,_q9[1],_q9[2],_q9[3],_q9[4],_q9[5],_q9[6],_q9[7],_q9[8],_q9[9],_pX];})];},_qa=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u5de6\u76ee+"));}),_qb=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u5de6\u76ee<br>\u4f9d\u5b58\u5ea6\u304c\u597d\u611f\u5ea6\u306b\u5909\u308f\u308b\u901f\u3055\u304c100\u500d\u306b\u306a\u308a\u307e\u3059\u3002"));}),_qc=[0,_pV,_qb,_qa],_qd=[0,_q3,_q6,_qc],_qe=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u53f3\u76ee"));}),_qf=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u53f3\u76ee<br>\u4f9d\u5b58\u5ea6\u30dc\u30fc\u30ca\u30b9\u304c10\u500d\u306b\u306a\u308a\u307e\u3059\u3002"));}),_qg=new T(function(){return B(unCStr("fa-eye"));}),_qh=[0,_qg,_qf,_qe],_qi=[0,10],_qj=function(_qk,_ql,_){return [0,_7b,new T(function(){var _qm=E(_ql);return [0,_qm[1],_qm[2],_qm[3],_qm[4],_qm[5],_qm[6],_qm[7],_qm[8],_qi,_qm[10]];})];},_qn=[0,100000],_qo=function(_qp){return E(_qn);},_qq=[0,_qo,_qj,_qh],_qr=function(_qs,_qt,_){return [0,_7b,new T(function(){var _qu=E(_qt);return [0,_qu[1],_qu[2],_qu[3],_qu[4],_qu[5],_qu[6],_qu[7],_qu[8],_qu[9],_qi];})];},_qv=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u5de6\u76ee"));}),_qw=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u5de6\u76ee<br>\u4f9d\u5b58\u5ea6\u304c\u597d\u611f\u5ea6\u306b\u5909\u308f\u308b\u901f\u3055\u304c10\u500d\u306b\u306a\u308a\u307e\u3059\u3002"));}),_qx=[0,_qg,_qw,_qv],_qy=[0,_qo,_qr,_qx],_qz=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u30b7\u30e7\u30c3\u30d7"));}),_qA=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u30b7\u30e7\u30c3\u30d7<br>\u30a2\u30a4\u30c6\u30e0\u304c\u8cfc\u5165\u3067\u304d\u308b\u3088\u3046\u306b\u306a\u308a\u307e\u3059\u3002"));}),_qB=new T(function(){return B(unCStr("fa-shopping-cart"));}),_qC=[0,_qB,_qA,_qz],_qD=function(_qE,_qF,_qG,_qH){return new F(function(){return A(_qE,[function(_){var _qI=jsSetStyle(E(_qF)[1],toJSStr(E(_qG)),toJSStr(E(_qH)));return _7b;}]);});},_qJ=function(_qK,_){var _qL=B(_h3(_)),_qM=_qL;return [0,_7b,new T(function(){var _qN=E(_qK);return [0,_qN[1],_qN[2],_qN[3],_qM,_qN[5],_qN[6],_qN[7],_qN[8],_qN[9],_qN[10]];})];},_qO=new T(function(){return B(unCStr("block"));}),_qP=function(_qQ,_){return [0,_7b,_qQ];},_qR=function(_qS,_qT,_){var _qU=B(A(_qS,[_])),_qV=_qU;return new F(function(){return A(_qT,[_qV,_]);});},_qW=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_qX=new T(function(){return B(unCStr("base"));}),_qY=new T(function(){return B(unCStr("IOException"));}),_qZ=new T(function(){var _r0=hs_wordToWord64(4053623282),_r1=_r0,_r2=hs_wordToWord64(3693590983),_r3=_r2;return [0,_r1,_r3,[0,_r1,_r3,_qX,_qW,_qY],_u];}),_r4=function(_r5){return E(_qZ);},_r6=function(_r7){var _r8=E(_r7);return new F(function(){return _4P(B(_4N(_r8[1])),_r4,_r8[2]);});},_r9=new T(function(){return B(unCStr(": "));}),_ra=[0,41],_rb=new T(function(){return B(unCStr(" ("));}),_rc=new T(function(){return B(unCStr("already exists"));}),_rd=new T(function(){return B(unCStr("does not exist"));}),_re=new T(function(){return B(unCStr("protocol error"));}),_rf=new T(function(){return B(unCStr("failed"));}),_rg=new T(function(){return B(unCStr("invalid argument"));}),_rh=new T(function(){return B(unCStr("inappropriate type"));}),_ri=new T(function(){return B(unCStr("hardware fault"));}),_rj=new T(function(){return B(unCStr("unsupported operation"));}),_rk=new T(function(){return B(unCStr("timeout"));}),_rl=new T(function(){return B(unCStr("resource vanished"));}),_rm=new T(function(){return B(unCStr("interrupted"));}),_rn=new T(function(){return B(unCStr("resource busy"));}),_ro=new T(function(){return B(unCStr("resource exhausted"));}),_rp=new T(function(){return B(unCStr("end of file"));}),_rq=new T(function(){return B(unCStr("illegal operation"));}),_rr=new T(function(){return B(unCStr("permission denied"));}),_rs=new T(function(){return B(unCStr("user error"));}),_rt=new T(function(){return B(unCStr("unsatisified constraints"));}),_ru=new T(function(){return B(unCStr("system error"));}),_rv=function(_rw,_rx){switch(E(_rw)){case 0:return new F(function(){return _O(_rc,_rx);});break;case 1:return new F(function(){return _O(_rd,_rx);});break;case 2:return new F(function(){return _O(_rn,_rx);});break;case 3:return new F(function(){return _O(_ro,_rx);});break;case 4:return new F(function(){return _O(_rp,_rx);});break;case 5:return new F(function(){return _O(_rq,_rx);});break;case 6:return new F(function(){return _O(_rr,_rx);});break;case 7:return new F(function(){return _O(_rs,_rx);});break;case 8:return new F(function(){return _O(_rt,_rx);});break;case 9:return new F(function(){return _O(_ru,_rx);});break;case 10:return new F(function(){return _O(_re,_rx);});break;case 11:return new F(function(){return _O(_rf,_rx);});break;case 12:return new F(function(){return _O(_rg,_rx);});break;case 13:return new F(function(){return _O(_rh,_rx);});break;case 14:return new F(function(){return _O(_ri,_rx);});break;case 15:return new F(function(){return _O(_rj,_rx);});break;case 16:return new F(function(){return _O(_rk,_rx);});break;case 17:return new F(function(){return _O(_rl,_rx);});break;default:return new F(function(){return _O(_rm,_rx);});}},_ry=[0,125],_rz=new T(function(){return B(unCStr("{handle: "));}),_rA=function(_rB,_rC,_rD,_rE,_rF,_rG){var _rH=new T(function(){var _rI=new T(function(){return B(_rv(_rC,new T(function(){var _rJ=E(_rE);return _rJ[0]==0?E(_rG):B(_O(_rb,new T(function(){return B(_O(_rJ,[1,_ra,_rG]));})));})));}),_rK=E(_rD);return _rK[0]==0?E(_rI):B(_O(_rK,new T(function(){return B(_O(_r9,_rI));})));}),_rL=E(_rF);if(!_rL[0]){var _rM=E(_rB);if(!_rM[0]){return E(_rH);}else{var _rN=E(_rM[1]);return _rN[0]==0?B(_O(_rz,new T(function(){return B(_O(_rN[1],[1,_ry,new T(function(){return B(_O(_r9,_rH));})]));}))):B(_O(_rz,new T(function(){return B(_O(_rN[1],[1,_ry,new T(function(){return B(_O(_r9,_rH));})]));})));}}else{return new F(function(){return _O(_rL[1],new T(function(){return B(_O(_r9,_rH));}));});}},_rO=function(_rP){var _rQ=E(_rP);return new F(function(){return _rA(_rQ[1],_rQ[2],_rQ[3],_rQ[4],_rQ[6],_u);});},_rR=function(_rS,_rT){var _rU=E(_rS);return new F(function(){return _rA(_rU[1],_rU[2],_rU[3],_rU[4],_rU[6],_rT);});},_rV=function(_rW,_rX){return new F(function(){return _5a(_rR,_rW,_rX);});},_rY=function(_rZ,_s0,_s1){var _s2=E(_s0);return new F(function(){return _rA(_s2[1],_s2[2],_s2[3],_s2[4],_s2[6],_s1);});},_s3=[0,_rY,_rO,_rV],_s4=new T(function(){return [0,_r4,_s3,_s5,_r6];}),_s5=function(_s6){return [0,_s4,_s6];},_s7=7,_s8=function(_s9){return [0,_2D,_s7,_u,_s9,_2D,_2D];},_sa=function(_sb,_){return new F(function(){return die(new T(function(){return B(_s5(new T(function(){return B(_s8(_sb));})));}));});},_sc=function(_sd,_){return new F(function(){return _sa(_sd,_);});},_se=function(_sf,_){return _sf;},_sg=function(_sh,_si,_){var _sj=B(A(_sh,[_])),_sk=_sj;return new F(function(){return A(_si,[_]);});},_sl=[0,_qR,_sg,_se,_sc],_sm=[0,_sl,_7P],_sn=function(_so){return E(E(_so)[1]);},_sp=function(_sq){return E(E(_sq)[1]);},_sr=function(_ss){return E(E(_ss)[2]);},_st=function(_su){return E(E(_su)[3]);},_sv=function(_sw,_sx){var _sy=new T(function(){return B(_sn(_sw));});return function(_sz){return new F(function(){return A(new T(function(){return B(_sp(_sy));}),[new T(function(){return B(A(_sr,[_sw,_sx]));}),function(_sA){return new F(function(){return A(new T(function(){return B(_st(_sy));}),[[0,_sA,_sz]]);});}]);});};},_sB=function(_sC){return new F(function(){return _sv(_sm,_sC);});},_sD=new T(function(){return B(unCStr("display"));}),_sE=new T(function(){return B(unCStr("monitor"));}),_sF=new T(function(){return B(unCStr(" could be found!"));}),_sG=function(_sH){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_O(_sH,_sF));}))));});},_sI=function(_sJ){return function(_sK,_){var _sL=E(_sJ),_sM=jsFind(toJSStr(_sL)),_sN=_sM,_sO=E(_sN);if(!_sO[0]){return new F(function(){return _sG(_sL);});}else{var _sP=B(A(_qD,[_sB,_sO[1],_sD,_qO,_sK,_])),_sQ=_sP;return new F(function(){return A(new T(function(){return !B(_6C(_sJ,_sE))?E(_qP):E(_qJ);}),[new T(function(){return E(E(_sQ)[2]);}),_]);});}};},_sR=new T(function(){return B(unCStr("item-shop"));}),_sS=new T(function(){return B(_sI(_sR));}),_sT=function(_sU){return E(_sS);},_sV=[0,1],_sW=function(_sX){return E(_sV);},_sY=[0,_sW,_sT,_qC],_sZ=new T(function(){return B(unCStr("\u3055\u3041\u59cb\u3081\u3088\u3046"));}),_t0=new T(function(){return B(unCStr("\u3055\u3041\u59cb\u3081\u3088\u3046<br>\u30b2\u30fc\u30e0\u3092\u59cb\u3081\u307e\u3057\u3087\u3046\u3002\u53f3\u306e\u30dc\u30bf\u30f3\u304b\u3089\u3053\u306e\u30a2\u30a4\u30c6\u30e0\u3092\u8cfc\u5165\u3057\u3066\u304f\u3060\u3055\u3044\u3002"));}),_t1=new T(function(){return B(unCStr("fa-power-off"));}),_t2=[0,_t1,_t0,_sZ],_t3=new T(function(){return B(_sI(_sE));}),_t4=function(_t5){return E(_t3);},_t6=function(_t7){return E(_o6);},_t8=[0,_t6,_t4,_t2],_t9=new T(function(){return B(unCStr("\u30c7\u30fc\u30bf\u306e\u6d88\u53bb"));}),_ta=new T(function(){return B(unCStr("\u30c7\u30fc\u30bf\u306e\u6d88\u53bb<br>\u5168\u3066\u306e\u30c7\u30fc\u30bf\u304c\u6d88\u53bb\u3055\u308c\u307e\u3059\u3002\u3053\u306e\u64cd\u4f5c\u306f\u53d6\u308a\u6d88\u305b\u307e\u305b\u3093\u3002"));}),_tb=new T(function(){return B(unCStr("fa-trash"));}),_tc=[0,_tb,_ta,_t9],_td=[0,100],_te=function(_tf){return E(_td);},_tg=function(_th){return E(_th);},_ti=function(_tj,_tk){return new F(function(){return (function(_tl){while(1){var _tm=E(_tl);switch(_tm[0]){case 0:var _tn=_tm[2]>>>0;if(((_tj>>>0&((_tn-1>>>0^4294967295)>>>0^_tn)>>>0)>>>0&4294967295)==_tm[1]){if(!((_tj>>>0&_tn)>>>0)){_tl=_tm[3];continue;}else{_tl=_tm[4];continue;}}else{return [0];}break;case 1:return _tj!=_tm[1]?[0]:[1,_tm[2]];default:return [0];}}})(_tk);});},_to=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:336:3-8"));}),_tp=function(_tq,_tr,_ts,_tt){return new F(function(){return A(_tq,[function(_){var _tu=jsSetAttr(E(_tr)[1],toJSStr(E(_ts)),toJSStr(E(_tt)));return _7b;}]);});},_tv=function(_tw,_tx,_ty,_tz){return new F(function(){return A(_tw,[function(_){var _tA=jsSet(E(_tx)[1],toJSStr(E(_ty)),toJSStr(E(_tz)));return _7b;}]);});},_tB=new T(function(){return B(unCStr("button"));}),_tC=new T(function(){return B(unCStr("type"));}),_tD=new T(function(){return B(unCStr("alert"));}),_tE=new T(function(){return B(unCStr("data-dismiss"));}),_tF=new T(function(){return B(unCStr("close"));}),_tG=function(_tH,_tI,_){var _tJ=jsCreateTextNode(toJSStr(E(_tH))),_tK=_tJ,_tL=jsAppendChild(_tK,E(_tI)[1]);return [0,_tK];},_tM=new T(function(){return B(unCStr("aria-hidden"));}),_tN=new T(function(){return B(unCStr("true"));}),_tO=new T(function(){return B(unCStr("innerHTML"));}),_tP=new T(function(){return B(unCStr("&times;"));}),_tQ=function(_tR,_){var _tS=B(A(_tv,[_7P,_tR,_tO,_tP,_])),_tT=_tS;return _tR;},_tU=new T(function(){return B(unCStr("sr-only"));}),_tV=new T(function(){return B(unCStr("Close"));}),_tW=new T(function(){return B(unCStr("class"));}),_tX=new T(function(){return B(unCStr("span"));}),_tY=function(_tZ,_u0,_u1,_){var _u2=jsCreateElem(toJSStr(E(_tX))),_u3=_u2,_u4=jsAppendChild(_u3,E(_u1)[1]),_u5=[0,_u3],_u6=B(A(_tZ,[_u0,_u5,_])),_u7=_u6;return _u5;},_u8=function(_u9){return E(_u9);},_ua=function(_ub,_){var _uc=B(_tY(_u8,_tQ,_ub,_)),_ud=_uc,_ue=B(A(_tp,[_7P,_ud,_tM,_tN,_])),_uf=_ue,_ug=B(_tY(_tG,_tV,_ub,_)),_uh=_ug,_ui=B(A(_tp,[_7P,_uh,_tW,_tU,_])),_uj=_ui;return _ub;},_uk=new T(function(){return B(unCStr("role"));}),_ul=function(_um,_un,_){var _uo=jsGet(_um,toJSStr(E(_un))),_up=_uo;return new T(function(){return fromJSStr(_up);});},_uq=[0,32],_ur=[1,_uq,_u],_us=new T(function(){return B(unCStr("list-group-item"));}),_ut=function(_uu){return new F(function(){return _hc(function(_){var _=0;return new F(function(){return eval(_uu);});});});},_uv=new T(function(){return B(_ut("(function(e,c){var first = e.firstChild; e.insertBefore(c,first);})"));}),_uw=function(_ux){return function(_uy,_){var _uz=B(A(new T(function(){return B(A(_uv,[E(E(_ux)[1])]));}),[E(E(_uy)[1]),_])),_uA=_uz;return _7b;};},_uB=new T(function(){return B(unCStr(") "));}),_uC=new T(function(){return B(unCStr("li"));}),_uD=new T(function(){return B(unCStr("var d = new Date(); d.getHours() + \':\' + d.getMinutes() + \':\' + d.getSeconds()"));}),_uE=new T(function(){return B(unCStr("innerHTML"));}),_uF=[0,49],_uG=[1,_uF,_u],_uH=new T(function(){return B(unCStr("<br>"));}),_uI=new T(function(){return B(unCStr("log-group"));}),_uJ=function(_uK,_uL){while(1){var _uM=E(_uK);if(!_uM[0]){return E(_uL);}else{_uK=_uM[2];var _uN=_uL+1|0;_uL=_uN;continue;}}},_uO=function(_uP,_uQ){while(1){var _uR=E(_uP);if(!_uR){return E(_uQ);}else{var _uS=E(_uQ);if(!_uS[0]){return [0];}else{_uP=_uR-1|0;_uQ=_uS[2];continue;}}}},_uT=function(_uU,_uV,_uW){while(1){var _uX=E(_uV);if(!_uX[0]){return true;}else{var _uY=E(_uW);if(!_uY[0]){return false;}else{if(!B(A(_2L,[_uU,_uX[1],_uY[1]]))){return false;}else{_uV=_uX[2];_uW=_uY[2];continue;}}}}},_uZ=function(_v0,_v1,_v2,_v3){if(!B(_uT(_6N,_v0,[1,_v2,_v3]))){return [1,_v2,new T(function(){return B(_v4(_v0,_v1,_v3));})];}else{return new F(function(){return _O(_v1,new T(function(){var _v5=B(_uJ(_v0,0));if(_v5>=0){var _v6=B(_v4(_v0,_v1,B(_uO(_v5,[1,_v2,_v3]))));}else{var _v6=B(_uZ(_v0,_v1,_v2,_v3));}var _v7=_v6,_v8=_v7;return _v8;}));});}},_v4=function(_v9,_va,_vb){var _vc=E(_vb);if(!_vc[0]){return [0];}else{var _vd=_vc[1],_ve=_vc[2];if(!B(_uT(_6N,_v9,_vc))){return [1,_vd,new T(function(){return B(_v4(_v9,_va,_ve));})];}else{return new F(function(){return _O(_va,new T(function(){var _vf=B(_uJ(_v9,0));if(_vf>=0){var _vg=B(_v4(_v9,_va,B(_uO(_vf,_vc))));}else{var _vg=B(_uZ(_v9,_va,_vd,_ve));}var _vh=_vg,_vi=_vh;return _vi;}));});}}},_vj=new T(function(){return B(unCStr("strong"));}),_vk=function(_vl,_vm,_vn,_){var _vo=jsCreateElem(toJSStr(E(_vj))),_vp=_vo,_vq=jsAppendChild(_vp,E(_vn)[1]),_vr=[0,_vp],_vs=B(A(_vl,[_vm,_vr,_])),_vt=_vs;return _vr;},_vu=new T(function(){return B(unCStr("unread-badge"));}),_vv=function(_vw,_vx,_){var _vy=E(_uI),_vz=jsFind(toJSStr(_vy)),_vA=_vz,_vB=E(_vA);if(!_vB[0]){return new F(function(){return _sG(_vy);});}else{var _vC=jsEval(toJSStr(E(_uD))),_vD=_vC,_vE=jsCreateElem(toJSStr(E(_uC))),_vF=_vE,_vG=[0,_vF],_vH=B(A(_tp,[_7P,_vG,_tW,_us,_])),_vI=_vH,_vJ=B(_vk(_tG,new T(function(){return B(_O(_vw,new T(function(){return B(unAppCStr(" (",new T(function(){return B(_O(fromJSStr(_vD),_uB));})));})));}),_vG,_)),_vK=_vJ,_vL=B(_tY(_u8,function(_vM,_){var _vN=B(A(_tv,[_7P,_vM,_tO,new T(function(){return B(_v4(_uH,_ur,_vx));}),_])),_vO=_vN;return _vM;},_vG,_)),_vP=_vL,_vQ=B(A(_uw,[_vB[1],_vG,_])),_vR=_vQ,_vS=E(_vu),_vT=jsFind(toJSStr(_vS)),_vU=_vT,_vV=E(_vU);if(!_vV[0]){return new F(function(){return _sG(_vS);});}else{var _vW=E(_vV[1]),_vX=_vW[1],_vY=B(_ul(_vX,_uE,_)),_vZ=_vY;if(!B(_6C(_vZ,_u))){var _w0=B(_gW(B(_5V(_gD,_vZ))));if(!_w0[0]){return E(_gK);}else{if(!E(_w0[2])[0]){var _w1=jsSet(_vX,toJSStr(E(_uE)),toJSStr(B(_19(0,B(_8p(_sV,_w0[1])),_u))));return _7b;}else{return E(_gM);}}}else{var _w2=B(A(_tv,[_7P,_vW,_uE,_uG,_])),_w3=_w2;return _7b;}}}},_w4=new T(function(){return B(unCStr("alert alert-info fade in tip"));}),_w5=new T(function(){return B(unCStr("button"));}),_w6=function(_w7,_w8,_w9,_){var _wa=jsCreateElem(toJSStr(E(_w5))),_wb=_wa,_wc=jsAppendChild(_wb,E(_w9)[1]),_wd=[0,_wb],_we=B(A(_w7,[_w8,_wd,_])),_wf=_we;return _wd;},_wg=new T(function(){return B(unCStr("div"));}),_wh=function(_wi,_wj,_wk,_){var _wl=jsCreateElem(toJSStr(E(_wg))),_wm=_wl,_wn=jsAppendChild(_wm,E(_wk)[1]),_wo=[0,_wm],_wp=B(A(_wi,[_wj,_wo,_])),_wq=_wp;return _wo;},_wr=new T(function(){return B(unCStr("id"));}),_ws=new T(function(){return B(unCStr("\').alert(\'close\')"));}),_wt=new T(function(){return B(unCStr("alerts"));}),_wu=function(_wv,_ww,_){var _wx=B(_h3(_)),_wy=_wx,_wz=E(_wt),_wA=jsFind(toJSStr(_wz)),_wB=_wA,_wC=E(_wB);if(!_wC[0]){return new F(function(){return _sG(_wz);});}else{var _wD=_wC[1],_wE=B(A(_tv,[_7P,_wD,_tO,_u,_])),_wF=_wE,_wG=B(_wh(_u8,function(_wH,_){var _wI=B(_w6(_u8,_ua,_wH,_)),_wJ=_wI,_wK=B(A(_tp,[_7P,_wJ,_tC,_tB,_])),_wL=_wK,_wM=B(A(_tp,[_7P,_wJ,_tW,_tF,_])),_wN=_wM,_wO=B(A(_tp,[_7P,_wJ,_tE,_tD,_])),_wP=_wO,_wQ=B(_tY(_u8,function(_wR,_){var _wS=B(A(_tv,[_7P,_wR,_tO,_ww,_])),_wT=_wS;return _wR;},_wH,_)),_wU=_wQ;return _wH;},_wD,_)),_wV=_wG,_wW=E(_wV),_wX=jsSetAttr(_wW[1],toJSStr(E(_wr)),toJSStr(B(unAppCStr("alert-",new T(function(){return B(_19(0,_wy,_u));}))))),_wY=B(A(_tp,[_7P,_wW,_tW,_w4,_])),_wZ=_wY,_x0=B(A(_tp,[_7P,_wW,_uk,_tD,_])),_x1=_x0,_x2=jsSetTimeout(5000,function(_){var _x3=jsEval(toJSStr(B(unAppCStr("$(\'#alert-",new T(function(){return B(_O(B(_19(0,_wy,_u)),_ws));}))))),_x4=_x3;return _7b;});return new F(function(){return _vv(_wv,_ww,_);});}},_x5=new T(function(){return B(unCStr("Aichan"));}),_x6=new T(function(){return [0,toJSStr(_u)];}),_x7=[0,93],_x8=[1,_x7,_u],_x9=new T(function(){return [0,toJSStr(_x8)];}),_xa=[0,125],_xb=[1,_xa,_u],_xc=new T(function(){return [0,toJSStr(_xb)];}),_xd=[0,58],_xe=[1,_xd,_u],_xf=new T(function(){return [0,toJSStr(_xe)];}),_xg=[0,44],_xh=[1,_xg,_u],_xi=new T(function(){return [0,toJSStr(_xh)];}),_xj=new T(function(){return [0,"false"];}),_xk=function(_xl){var _xm=jsShow(E(_xl)[1]),_xn=_xm;return [0,_xn];},_xo=function(_xp){var _xq=jsStringify(E(_xp)[1]),_xr=_xq;return [0,_xr];},_xs=new T(function(){return [0,"null"];}),_xt=[0,91],_xu=[1,_xt,_u],_xv=new T(function(){return [0,toJSStr(_xu)];}),_xw=[0,123],_xx=[1,_xw,_u],_xy=new T(function(){return [0,toJSStr(_xx)];}),_xz=[0,34],_xA=[1,_xz,_u],_xB=new T(function(){return [0,toJSStr(_xA)];}),_xC=new T(function(){return [0,"true"];}),_xD=function(_xE,_xF){var _xG=E(_xF);switch(_xG[0]){case 0:return [0,new T(function(){return B(_xk(_xG[1]));}),_xE];case 1:return [0,new T(function(){return B(_xo(_xG[1]));}),_xE];case 2:return !E(_xG[1])?[0,_xj,_xE]:[0,_xC,_xE];case 3:var _xH=E(_xG[1]);return _xH[0]==0?[0,_xv,[1,_x9,_xE]]:[0,_xv,new T(function(){var _xI=B(_xD(new T(function(){var _xJ=function(_xK){var _xL=E(_xK);return _xL[0]==0?E([1,_x9,_xE]):[1,_xi,new T(function(){var _xM=B(_xD(new T(function(){return B(_xJ(_xL[2]));}),_xL[1]));return [1,_xM[1],_xM[2]];})];};return B(_xJ(_xH[2]));}),_xH[1]));return [1,_xI[1],_xI[2]];})];case 4:var _xN=E(_xG[1]);if(!_xN[0]){return [0,_xy,[1,_xc,_xE]];}else{var _xO=E(_xN[1]);return [0,_xy,[1,new T(function(){return B(_xo(_xO[1]));}),[1,_xf,new T(function(){var _xP=B(_xD(new T(function(){var _xQ=function(_xR){var _xS=E(_xR);if(!_xS[0]){return E([1,_xc,_xE]);}else{var _xT=E(_xS[1]);return [1,_xi,[1,_xB,[1,_xT[1],[1,_xB,[1,_xf,new T(function(){var _xU=B(_xD(new T(function(){return B(_xQ(_xS[2]));}),_xT[2]));return [1,_xU[1],_xU[2]];})]]]]];}};return B(_xQ(_xN[2]));}),_xO[2]));return [1,_xP[1],_xP[2]];})]]];}break;default:return [0,_xs,_xE];}},_xV=function(_xW){var _xX=jsCat(new T(function(){var _xY=B(_xD(_u,_xW));return [1,_xY[1],_xY[2]];}),E(_x6)[1]),_xZ=_xX;return E(_xZ);},_y0=new T(function(){return [0,"(function(k,v) {localStorage.setItem(k,v);})"];}),_y1=function(_y2,_y3){return function(_y4,_){var _y5=B(A(new T(function(){return B(A(_ut,[E(_y0)[1],E(toJSStr(E(_y3)))]));}),[E(B(_xV(B(A(new T(function(){return B(_2j(_y2));}),[_y4]))))),_])),_y6=_y5;return _7b;};},_y7=new T(function(){return B(_y1(_if,_x5));}),_y8=new T(function(){return B(unCStr("game"));}),_y9=new T(function(){return B(unCStr("\u5b9f\u7e3e\u7372\u5f97: "));}),_ya=function(_yb,_yc,_yd){return function(_ye,_){var _yf=B(_wu(_y8,new T(function(){return B(_O(_y9,_yc));}),_)),_yg=_yf,_yh=new T(function(){var _yi=E(_ye);return [0,_yi[1],_yi[2],_yi[3],_yi[4],_yi[5],new T(function(){return B(_46(E(_yd)[1],[1,_yb],_yi[6]));}),_yi[7],_yi[8],_yi[9],_yi[10]];}),_yj=B(A(_y7,[_yh,_])),_yk=_yj;return new F(function(){return _yl(_yh,_);});};},_ym=function(_yn){var _yo=E(_yn);return _yo[0]==0?_yo[1]:I_toNumber(_yo[1]);},_yp=new T(function(){return B(unCStr("\u611b\u60c5\u304c"));}),_yq=new T(function(){return B(unCStr("\u3092\u8d85\u3048\u308b"));}),_yr=function(_ys,_yt,_yu){return function(_yv,_){var _yw=E(_yv);return E(_yw[1])[1]<=E(new T(function(){return [0,B(_ym(_ys))];}))[1]?[0,_7b,_yw]:B(A(new T(function(){return B(_ya(new T(function(){return B(_O(_yp,new T(function(){return B(_O(B(_19(0,_ys,_u)),_yq));})));}),_yt,_yu));}),[_yw,_]));};},_yx=new T(function(){return B(unCStr("\u30a2\u30a4\u3068\u306e\u906d\u9047"));}),_yy=function(_yz){return new F(function(){return _yr(_td,_yx,_yz);});},_yA=new T(function(){return [0,_yx,_yy];}),_yB=[0,10000],_yC=new T(function(){return B(unCStr("\u611b\u3055\u308c\u6c17\u5206"));}),_yD=function(_yz){return new F(function(){return _yr(_yB,_yC,_yz);});},_yE=new T(function(){return [0,_yC,_yD];}),_yF=[0,1000000],_yG=new T(function(){return B(unCStr("\u611b\u30e9\u30d6\u30e6\u30fc"));}),_yH=function(_yz){return new F(function(){return _yr(_yF,_yG,_yz);});},_yI=new T(function(){return [0,_yG,_yH];}),_yJ=[0,100000000],_yK=new T(function(){return B(unCStr("\u611b\u3055\u3093\u306e\u611b\u304c\u91cd\u3044"));}),_yL=function(_yz){return new F(function(){return _yr(_yJ,_yK,_yz);});},_yM=new T(function(){return [0,_yK,_yL];}),_yN=[1,I_fromBits([1215752192,23])],_yO=new T(function(){return B(unCStr("\u611b\u3061\u3083\u3093\u30de\u30b9\u30bf\u30fc"));}),_yP=function(_yz){return new F(function(){return _yr(_yN,_yO,_yz);});},_yQ=new T(function(){return [0,_yO,_yP];}),_yR=new T(function(){return B(unCStr("\u597d\u611f\u5ea6\u304c"));}),_yS=function(_yT,_yU,_yV){return function(_yW,_){var _yX=E(_yW);return E(_yX[2])[1]<=E(new T(function(){return [0,B(_ym(_yT))];}))[1]?[0,_7b,_yX]:B(A(new T(function(){return B(_ya(new T(function(){return B(_O(_yR,new T(function(){return B(_O(B(_19(0,_yT,_u)),_yq));})));}),_yU,_yV));}),[_yX,_]));};},_yY=new T(function(){return B(unCStr("\u307e\u305a\u306f\u304a\u53cb\u9054\u304b\u3089"));}),_yZ=[0,10],_z0=function(_yz){return new F(function(){return _yS(_yZ,_yY,_yz);});},_z1=new T(function(){return [0,_yY,_z0];}),_z2=new T(function(){return B(unCStr("\u4e00\u7d1a\u30d5\u30e9\u30b0\u5efa\u7bc9\u58eb"));}),_z3=function(_yz){return new F(function(){return _yS(_td,_z2,_yz);});},_z4=new T(function(){return [0,_z2,_z3];}),_z5=[0,1000],_z6=new T(function(){return B(unCStr("\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_z7=function(_yz){return new F(function(){return _yS(_z5,_z6,_yz);});},_z8=new T(function(){return [0,_z6,_z7];}),_z9=new T(function(){return B(unCStr("\uff8a\uff72\uff8a\uff9f\uff70\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_za=function(_yz){return new F(function(){return _yS(_yB,_z9,_yz);});},_zb=new T(function(){return [0,_z9,_za];}),_zc=new T(function(){return B(unCStr("\u5168\u958b\u3089\u3076\u3071\u308f\u30fc"));}),_zd=function(_yz){return new F(function(){return _yS(_qn,_zc,_yz);});},_ze=new T(function(){return [0,_zc,_zd];}),_zf=new T(function(){return B(unCStr("\u4f9d\u5b58\u5ea6\u304c"));}),_zg=function(_zh,_zi,_zj){return function(_zk,_){var _zl=E(_zk);return E(_zl[3])[1]<=E(new T(function(){return [0,B(_ym(_zh))];}))[1]?[0,_7b,_zl]:B(A(new T(function(){return B(_ya(new T(function(){return B(_O(_zf,new T(function(){return B(_O(B(_19(0,_zh,_u)),_yq));})));}),_zi,_zj));}),[_zl,_]));};},_zm=new T(function(){return B(unCStr("\u4f9d\u5b58\u6ce8\u610f\u5831"));}),_zn=function(_yz){return new F(function(){return _zg(_td,_zm,_yz);});},_zo=new T(function(){return [0,_zm,_zn];}),_zp=new T(function(){return B(unCStr("\u4f9d\u5b58\u30c9\u30e9\u30c3\u30b0"));}),_zq=function(_yz){return new F(function(){return _zg(_yB,_zp,_yz);});},_zr=new T(function(){return [0,_zp,_zq];}),_zs=new T(function(){return B(unCStr("\u304a\u3057\u3083\u3079\u308a\u611b\u3061\u3083\u3093"));}),_zt=[0,1],_zu=[0,100],_zv=new T(function(){return B(unCStr("\u300d\u3092"));}),_zw=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u300c"));}),_zx=new T(function(){return B(unCStr("\u500b\u4ee5\u4e0a\u624b\u306b\u5165\u308c\u308b"));}),_zy=new T(function(){return B(_4x(_45,_zz));}),_zA=function(_zB,_zC,_zD,_zE){return function(_zF,_){var _zG=E(_zF),_zH=_zG[7],_zI=E(_zB)[1];return !B(_n2(_zI,_zH))?[0,_7b,_zG]:B(_mV(_zH,_zI))[1]<E(_zC)[1]?[0,_7b,_zG]:B(A(new T(function(){return B(_ya(new T(function(){return B(_O(_zw,new T(function(){return B(_O(E(B(_mV(_zy,E(_zB)[1]))[3])[3],new T(function(){return B(_O(_zv,new T(function(){return B(_O(B(_9V(0,E(_zC)[1],_u)),_zx));})));})));})));}),_zD,_zE));}),[_zG,_]));};},_zJ=function(_yz){return new F(function(){return _zA(_zt,_zu,_zs,_yz);});},_zK=new T(function(){return [0,_zs,_zJ];}),_zL=new T(function(){return B(unCStr("\u3042\u3001\u3046\u3093"));}),_zM=[0,200],_zN=function(_yz){return new F(function(){return _zA(_zt,_zM,_zL,_yz);});},_zO=new T(function(){return [0,_zL,_zN];}),_zP=new T(function(){return B(unCStr("\u55ab\u8336\u5e97\u306e\u30dd\u30a4\u30f3\u30c8\u30ab\u30fc\u30c9"));}),_zQ=[0,50],_zR=[0,3],_zS=function(_yz){return new F(function(){return _zA(_zR,_zQ,_zP,_yz);});},_zT=new T(function(){return [0,_zP,_zS];}),_zU=new T(function(){return B(unCStr("\u611b\u3068\u3044\u3046\u540d\u306e\u30d7\u30ec\u30bc\u30f3\u30c8"));}),_zV=[0,4],_zW=function(_yz){return new F(function(){return _zA(_zV,_zQ,_zU,_yz);});},_zX=new T(function(){return [0,_zU,_zW];}),_zY=new T(function(){return B(unCStr("\u30b3\u30f3\u30d7\u30ea\u30fc\u30c8"));}),_zZ=function(_A0,_A1){while(1){var _A2=E(_A1);if(!_A2[0]){return true;}else{if(!B(A(_A0,[_A2[1]]))){return false;}else{_A1=_A2[2];continue;}}}},_A3=new T(function(){return B(unCStr("\u5168\u3066\u306e\u901a\u5e38\u30a2\u30a4\u30c6\u30e0\u3092"));}),_A4=function(_A5,_A6){while(1){var _A7=(function(_A8,_A9){var _Aa=E(_A9);switch(_Aa[0]){case 0:_A5=new T(function(){return B(_A4(_A8,_Aa[4]));});_A6=_Aa[3];return null;case 1:return [1,[0,_Aa[1]],_A8];default:return E(_A8);}})(_A5,_A6);if(_A7!=null){return _A7;}}},_Ab=function(_Ac){var _Ad=E(_Ac);if(!_Ad[0]){var _Ae=_Ad[3],_Af=_Ad[4];return _Ad[2]>=0?B(_A4(new T(function(){return B(_A4(_u,_Af));}),_Ae)):B(_A4(new T(function(){return B(_A4(_u,_Ae));}),_Af));}else{return new F(function(){return _A4(_u,_Ad);});}},_Ag=function(_Ah,_Ai){while(1){var _Aj=E(_Ai);switch(_Aj[0]){case 0:var _Ak=_Aj[3],_Al=B(_Ag(_Ah,_Aj[4]));if(_Al[0]==2){_Ai=_Ak;continue;}else{var _Am=B(_Ag(_Ah,_Ak));return _Am[0]==2?E(_Al):[0,_Aj[1],_Aj[2],E(_Am),E(_Al)];}break;case 1:return !B(A(_Ah,[[0,_Aj[1]],_Aj[2]]))?[2]:E(_Aj);default:return [2];}}},_An=function(_Ao,_Ap){return E(_Ao)[1]>0;},_Aq=new T(function(){return B(_Ag(_An,_zy));}),_Ar=new T(function(){return B(_Ab(_Aq));}),_As=function(_At,_Au,_Av){return function(_Aw,_){var _Ax=new T(function(){return E(E(_Aw)[7]);});return !B(_zZ(function(_Ay){var _Az=E(_Ay)[1];return !B(_n2(_Az,_Ax))?false:B(_mV(_Ax,_Az))[1]>=E(_At)[1];},_Ar))?[0,_7b,_Aw]:B(A(new T(function(){return B(_ya(new T(function(){return B(_O(_A3,new T(function(){return B(_O(B(_9V(0,E(_At)[1],_u)),_zx));})));}),_Au,_Av));}),[_Aw,_]));};},_AA=function(_yz){return new F(function(){return _As(_zt,_zY,_yz);});},_AB=new T(function(){return [0,_zY,_AA];}),_AC=new T(function(){return B(unCStr("\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30f3\u30d7\u30ea\u30fc\u30c8"));}),_AD=[0,10],_AE=function(_yz){return new F(function(){return _As(_AD,_AC,_yz);});},_AF=new T(function(){return [0,_AC,_AE];}),_AG=new T(function(){var _AH=B(_ne(1,2147483647));return _AH[0]==0?[0]:[1,[0,_AH[1],_yA],new T(function(){var _AI=E(_AH[2]);return _AI[0]==0?[0]:[1,[0,_AI[1],_yE],new T(function(){var _AJ=E(_AI[2]);return _AJ[0]==0?[0]:[1,[0,_AJ[1],_yI],new T(function(){var _AK=E(_AJ[2]);return _AK[0]==0?[0]:[1,[0,_AK[1],_yM],new T(function(){var _AL=E(_AK[2]);return _AL[0]==0?[0]:[1,[0,_AL[1],_yQ],new T(function(){var _AM=E(_AL[2]);return _AM[0]==0?[0]:[1,[0,_AM[1],_z1],new T(function(){var _AN=E(_AM[2]);return _AN[0]==0?[0]:[1,[0,_AN[1],_z4],new T(function(){var _AO=E(_AN[2]);return _AO[0]==0?[0]:[1,[0,_AO[1],_z8],new T(function(){var _AP=E(_AO[2]);return _AP[0]==0?[0]:[1,[0,_AP[1],_zb],new T(function(){var _AQ=E(_AP[2]);return _AQ[0]==0?[0]:[1,[0,_AQ[1],_ze],new T(function(){var _AR=E(_AQ[2]);return _AR[0]==0?[0]:[1,[0,_AR[1],_zo],new T(function(){var _AS=E(_AR[2]);return _AS[0]==0?[0]:[1,[0,_AS[1],_zr],new T(function(){var _AT=E(_AS[2]);return _AT[0]==0?[0]:[1,[0,_AT[1],_zK],new T(function(){var _AU=E(_AT[2]);return _AU[0]==0?[0]:[1,[0,_AU[1],_zO],new T(function(){var _AV=E(_AU[2]);return _AV[0]==0?[0]:[1,[0,_AV[1],_zT],new T(function(){var _AW=E(_AV[2]);return _AW[0]==0?[0]:[1,[0,_AW[1],_zX],new T(function(){var _AX=E(_AW[2]);return _AX[0]==0?[0]:[1,[0,_AX[1],_AB],new T(function(){var _AY=E(_AX[2]);return _AY[0]==0?[0]:[1,[0,_AY[1],_AF],_u];})];})];})];})];})];})];})];})];})];})];})];})];})];})];})];})];})];}),_AZ=new T(function(){return B(unCStr("</tbody>"));}),_B0=new T(function(){return B(unCStr("<tr><td>%s</td><td>%s</td></tr>"));}),_B1=new T(function(){return B(unCStr("<thead><tr><th>\u5b9f\u7e3e\u540d</th><th>\u5185\u5bb9</th></tr></thead>"));}),_B2=function(_B3,_B4){while(1){var _B5=E(_B3);if(!_B5[0]){return E(_B4);}else{_B3=_B5[2];var _B6=[1,_B5[1],_B4];_B4=_B6;continue;}}},_B7=function(_B8){var _B9=E(_B8)[1];return [0,Math.log(_B9+(_B9+1)*Math.sqrt((_B9-1)/(_B9+1)))];},_Ba=function(_Bb){var _Bc=E(_Bb)[1];return [0,Math.log(_Bc+Math.sqrt(1+_Bc*_Bc))];},_Bd=function(_Be){var _Bf=E(_Be)[1];return [0,0.5*Math.log((1+_Bf)/(1-_Bf))];},_Bg=function(_Bh,_Bi){return [0,Math.log(E(_Bi)[1])/Math.log(E(_Bh)[1])];},_Bj=[0,3.141592653589793],_Bk=function(_Bl){var _Bm=E(_Bl);return new F(function(){return _oo(_Bm[1],_Bm[2]);});},_Bn=function(_Bo){return [0,1/E(_Bo)[1]];},_Bp=function(_Bq){var _Br=E(_Bq),_Bs=_Br[1];return _Bs<0?[0, -_Bs]:E(_Br);},_Bt=function(_Bu){return [0,B(_ym(_Bu))];},_Bv=[0,0],_Bw=[0,1],_Bx=[0,-1],_By=function(_Bz){var _BA=E(E(_Bz)[1]);return _BA==0?E(_Bv):_BA<=0?E(_Bx):E(_Bw);},_BB=function(_BC,_BD){return [0,E(_BC)[1]-E(_BD)[1]];},_BE=function(_BF){return [0, -E(_BF)[1]];},_BG=function(_BH,_BI){return [0,E(_BH)[1]+E(_BI)[1]];},_BJ=function(_BK,_BL){return [0,E(_BK)[1]*E(_BL)[1]];},_BM=[0,_BG,_BJ,_BB,_BE,_Bp,_By,_Bt],_BN=function(_BO,_BP){return [0,E(_BO)[1]/E(_BP)[1]];},_BQ=[0,_BM,_BN,_Bn,_Bk],_BR=function(_BS){return [0,Math.acos(E(_BS)[1])];},_BT=function(_BU){return [0,Math.asin(E(_BU)[1])];},_BV=function(_BW){return [0,Math.atan(E(_BW)[1])];},_BX=function(_BY){return [0,Math.cos(E(_BY)[1])];},_BZ=function(_C0){return [0,cosh(E(_C0)[1])];},_C1=function(_C2){return [0,Math.exp(E(_C2)[1])];},_C3=function(_C4){return [0,Math.log(E(_C4)[1])];},_C5=function(_C6,_C7){return [0,Math.pow(E(_C6)[1],E(_C7)[1])];},_C8=function(_C9){return [0,Math.sin(E(_C9)[1])];},_Ca=function(_Cb){return [0,sinh(E(_Cb)[1])];},_Cc=function(_Cd){return [0,Math.sqrt(E(_Cd)[1])];},_Ce=function(_Cf){return [0,Math.tan(E(_Cf)[1])];},_Cg=function(_Ch){return [0,tanh(E(_Ch)[1])];},_Ci=[0,_BQ,_Bj,_C1,_Cc,_C3,_C5,_Bg,_C8,_Ce,_BX,_BT,_BV,_BR,_Ca,_Cg,_BZ,_Ba,_Bd,_B7],_Cj=function(_Ck){var _Cl=E(_Ck)[1];return [0,Math.log(_Cl+(_Cl+1)*Math.sqrt((_Cl-1)/(_Cl+1)))];},_Cm=function(_Cn){var _Co=E(_Cn)[1];return [0,Math.log(_Co+Math.sqrt(1+_Co*_Co))];},_Cp=function(_Cq){var _Cr=E(_Cq)[1];return [0,0.5*Math.log((1+_Cr)/(1-_Cr))];},_Cs=function(_Ct,_Cu){return [0,Math.log(E(_Cu)[1])/Math.log(E(_Ct)[1])];},_Cv=[0,3.141592653589793],_Cw=new T(function(){return [0,0/0];}),_Cx=new T(function(){return [0,-1/0];}),_Cy=new T(function(){return [0,1/0];}),_Cz=function(_CA,_CB){return !B(_og(_CB,_oa))?[0,B(_ob(_CA,_CB))]:!B(_og(_CA,_oa))?!B(_Y(_CA,_oa))?E(_Cy):E(_Cx):E(_Cw);},_CC=function(_CD){var _CE=E(_CD);return new F(function(){return _Cz(_CE[1],_CE[2]);});},_CF=function(_CG){return [0,1/E(_CG)[1]];},_CH=function(_CI){var _CJ=E(_CI),_CK=_CJ[1];return _CK<0?[0, -_CK]:E(_CJ);},_CL=function(_CM){var _CN=E(_CM);return _CN[0]==0?_CN[1]:I_toNumber(_CN[1]);},_CO=function(_CP){return [0,B(_CL(_CP))];},_CQ=[0,0],_CR=[0,1],_CS=[0,-1],_CT=function(_CU){var _CV=E(E(_CU)[1]);return _CV==0?E(_CQ):_CV<=0?E(_CS):E(_CR);},_CW=function(_CX,_CY){return [0,E(_CX)[1]-E(_CY)[1]];},_CZ=function(_D0){return [0, -E(_D0)[1]];},_D1=function(_D2,_D3){return [0,E(_D2)[1]+E(_D3)[1]];},_D4=function(_D5,_D6){return [0,E(_D5)[1]*E(_D6)[1]];},_D7=[0,_D1,_D4,_CW,_CZ,_CH,_CT,_CO],_D8=function(_D9,_Da){return [0,E(_D9)[1]/E(_Da)[1]];},_Db=[0,_D7,_D8,_CF,_CC],_Dc=function(_Dd){return [0,Math.acos(E(_Dd)[1])];},_De=function(_Df){return [0,Math.asin(E(_Df)[1])];},_Dg=function(_Dh){return [0,Math.atan(E(_Dh)[1])];},_Di=function(_Dj){return [0,Math.cos(E(_Dj)[1])];},_Dk=function(_Dl){return [0,cosh(E(_Dl)[1])];},_Dm=function(_Dn){return [0,Math.exp(E(_Dn)[1])];},_Do=function(_Dp){return [0,Math.log(E(_Dp)[1])];},_Dq=function(_Dr,_Ds){return [0,Math.pow(E(_Dr)[1],E(_Ds)[1])];},_Dt=function(_Du){return [0,Math.sin(E(_Du)[1])];},_Dv=function(_Dw){return [0,sinh(E(_Dw)[1])];},_Dx=function(_Dy){return [0,Math.sqrt(E(_Dy)[1])];},_Dz=function(_DA){return [0,Math.tan(E(_DA)[1])];},_DB=function(_DC){return [0,tanh(E(_DC)[1])];},_DD=[0,_Db,_Cv,_Dm,_Dx,_Do,_Dq,_Cs,_Dt,_Dz,_Di,_De,_Dg,_Dc,_Dv,_DB,_Dk,_Cm,_Cp,_Cj],_DE=function(_DF){var _DG=B(_nK(E(_DF)[1]));return [0,_DG[1],[0,_DG[2]]];},_DH=[0,53],_DI=function(_DJ){return E(_DH);},_DK=[0,2],_DL=function(_DM){return E(_DK);},_DN=[0,1024],_DO=[0,-1021],_DP=[0,_DO,_DN],_DQ=function(_DR){return E(_DP);},_DS=function(_DT){var _DU=isDoubleInfinite(E(_DT)[1]),_DV=_DU;return E(_DV)==0?false:true;},_DW=function(_DX){var _DY=isDoubleNaN(E(_DX)[1]),_DZ=_DY;return E(_DZ)==0?false:true;},_E0=function(_E1){var _E2=isDoubleNegativeZero(E(_E1)[1]),_E3=_E2;return E(_E3)==0?false:true;},_E4=function(_E5){var _E6=decodeFloat(E(_E5)[1]);return [0,new T(function(){return B(_8F(_E6[1]));}),[0,_E6[2]]];},_E7=[0,24],_E8=function(_E9){return E(_E7);},_Ea=function(_Eb){return E(_DK);},_Ec=[0,128],_Ed=[0,-125],_Ee=[0,_Ed,_Ec],_Ef=function(_Eg){return E(_Ee);},_Eh=function(_Ei){var _Ej=isFloatInfinite(E(_Ei)[1]),_Ek=_Ej;return E(_Ek)==0?false:true;},_El=function(_Em){var _En=isFloatNaN(E(_Em)[1]),_Eo=_En;return E(_Eo)==0?false:true;},_Ep=function(_Eq){var _Er=isFloatNegativeZero(E(_Eq)[1]),_Es=_Er;return E(_Es)==0?false:true;},_Et=function(_Eu,_Ev){return E(_Eu)[1]!=E(_Ev)[1]?true:false;},_Ew=function(_Ex,_Ey){return E(_Ex)[1]==E(_Ey)[1];},_Ez=[0,_Ew,_Et],_EA=function(_EB,_EC){return E(_EB)[1]<E(_EC)[1];},_ED=function(_EE,_EF){return E(_EE)[1]<=E(_EF)[1];},_EG=function(_EH,_EI){return E(_EH)[1]>E(_EI)[1];},_EJ=function(_EK,_EL){return E(_EK)[1]>=E(_EL)[1];},_EM=function(_EN,_EO){var _EP=E(_EN)[1],_EQ=E(_EO)[1];return _EP>=_EQ?_EP!=_EQ?2:1:0;},_ER=function(_ES,_ET){var _EU=E(_ES),_EV=E(_ET);return _EU[1]>_EV[1]?E(_EU):E(_EV);},_EW=function(_EX,_EY){var _EZ=E(_EX),_F0=E(_EY);return _EZ[1]>_F0[1]?E(_F0):E(_EZ);},_F1=[0,_Ez,_EM,_EA,_EJ,_EG,_ED,_ER,_EW],_F2=[0,1],_F3=new T(function(){var _F4=newByteArr(256),_F5=_F4,_=_F5["v"]["i8"][0]=8,_=B((function(_F6,_F7,_F8,_){while(1){if(_F8>=256){if(_F6>=256){return E(_);}else{var _F9=imul(2,_F6)|0,_Fa=_F7+1|0,_Fb=_F6;_F6=_F9;_F7=_Fa;_F8=_Fb;continue;}}else{var _=_F5["v"]["i8"][_F8]=_F7,_Fb=_F8+_F6|0;_F8=_Fb;continue;}}})(2,0,1,_)),_Fc=_F5,_Fd=_Fc;return [0,_Fd];}),_Fe=function(_Ff,_Fg){while(1){var _Fh=(function(_Fi,_Fj){var _Fk=hs_int64ToInt(_Fi),_Fl=_Fk,_Fm=E(_F3)[1]["v"]["i8"][(255&_Fl>>>0)>>>0&4294967295];if(_Fj>_Fm){if(_Fm>=8){var _Fn=hs_uncheckedIShiftRA64(_Fi,8),_Fo=_Fn;_Ff=_Fo;var _Fp=_Fj-8|0;_Fg=_Fp;return null;}else{return [0,new T(function(){var _Fq=hs_uncheckedIShiftRA64(_Fi,_Fm),_Fr=_Fq;return B(_nN(_Fr));}),_Fj-_Fm|0];}}else{return [0,new T(function(){var _Fs=hs_uncheckedIShiftRA64(_Fi,_Fj),_Ft=_Fs;return B(_nN(_Ft));}),0];}})(_Ff,_Fg);if(_Fh!=null){return _Fh;}}},_Fu=function(_Fv){return I_toInt(_Fv)>>>0;},_Fw=function(_Fx){var _Fy=E(_Fx);return _Fy[0]==0?_Fy[1]>>>0:B(_Fu(_Fy[1]));},_Fz=function(_FA){var _FB=B(_nK(_FA)),_FC=_FB[1],_FD=_FB[2];if(_FD<0){var _FE=function(_FF){if(!_FF){return [0,E(_FC),B(_ov(_F2, -_FD))];}else{var _FG=B(_Fe(B(_o3(_FC)), -_FD));return [0,E(_FG[1]),B(_ov(_F2,_FG[2]))];}};return (B(_Fw(_FC))&1)>>>0==0?B(_FE(1)):B(_FE(0));}else{return [0,B(_ov(_FC,_FD)),_F2];}},_FH=function(_FI){var _FJ=B(_Fz(E(_FI)[1]));return [0,E(_FJ[1]),E(_FJ[2])];},_FK=[0,_BM,_F1,_FH],_FL=function(_FM){return E(E(_FM)[1]);},_FN=[0,1],_FO=function(_FP){return new F(function(){return _ne(E(_FP)[1],2147483647);});},_FQ=function(_FR,_FS,_FT){return _FT<=_FS?[1,[0,_FR],new T(function(){var _FU=_FS-_FR|0,_FV=function(_FW){return _FW>=(_FT-_FU|0)?[1,[0,_FW],new T(function(){return B(_FV(_FW+_FU|0));})]:[1,[0,_FW],_u];};return B(_FV(_FS));})]:_FT<=_FR?[1,[0,_FR],_u]:[0];},_FX=function(_FY,_FZ,_G0){return _G0>=_FZ?[1,[0,_FY],new T(function(){var _G1=_FZ-_FY|0,_G2=function(_G3){return _G3<=(_G0-_G1|0)?[1,[0,_G3],new T(function(){return B(_G2(_G3+_G1|0));})]:[1,[0,_G3],_u];};return B(_G2(_FZ));})]:_G0>=_FY?[1,[0,_FY],_u]:[0];},_G4=function(_G5,_G6){return _G6<_G5?B(_FQ(_G5,_G6,-2147483648)):B(_FX(_G5,_G6,2147483647));},_G7=function(_G8,_G9){return new F(function(){return _G4(E(_G8)[1],E(_G9)[1]);});},_Ga=function(_Gb,_Gc,_Gd){return _Gc<_Gb?B(_FQ(_Gb,_Gc,_Gd)):B(_FX(_Gb,_Gc,_Gd));},_Ge=function(_Gf,_Gg,_Gh){return new F(function(){return _Ga(E(_Gf)[1],E(_Gg)[1],E(_Gh)[1]);});},_Gi=function(_Gj,_Gk){return new F(function(){return _ne(E(_Gj)[1],E(_Gk)[1]);});},_Gl=function(_Gm){return E(_Gm);},_Gn=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_Go=new T(function(){return B(err(_Gn));}),_Gp=function(_Gq){var _Gr=E(E(_Gq)[1]);return _Gr==(-2147483648)?E(_Go):[0,_Gr-1|0];},_Gs=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_Gt=new T(function(){return B(err(_Gs));}),_Gu=function(_Gv){var _Gw=E(E(_Gv)[1]);return _Gw==2147483647?E(_Gt):[0,_Gw+1|0];},_Gx=[0,_Gu,_Gp,_Gl,_Gl,_FO,_G7,_Gi,_Ge],_Gy=function(_Gz,_GA){if(_Gz<=0){if(_Gz>=0){return new F(function(){return quot(_Gz,_GA);});}else{if(_GA<=0){return new F(function(){return quot(_Gz,_GA);});}else{return quot(_Gz+1|0,_GA)-1|0;}}}else{if(_GA>=0){if(_Gz>=0){return new F(function(){return quot(_Gz,_GA);});}else{if(_GA<=0){return new F(function(){return quot(_Gz,_GA);});}else{return quot(_Gz+1|0,_GA)-1|0;}}}else{return quot(_Gz-1|0,_GA)-1|0;}}},_GB=new T(function(){return B(unCStr("ArithException"));}),_GC=new T(function(){return B(unCStr("GHC.Exception"));}),_GD=new T(function(){return B(unCStr("base"));}),_GE=new T(function(){var _GF=hs_wordToWord64(4194982440),_GG=_GF,_GH=hs_wordToWord64(3110813675),_GI=_GH;return [0,_GG,_GI,[0,_GG,_GI,_GD,_GC,_GB],_u];}),_GJ=function(_GK){return E(_GE);},_GL=function(_GM){var _GN=E(_GM);return new F(function(){return _4P(B(_4N(_GN[1])),_GJ,_GN[2]);});},_GO=new T(function(){return B(unCStr("arithmetic underflow"));}),_GP=new T(function(){return B(unCStr("arithmetic overflow"));}),_GQ=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_GR=new T(function(){return B(unCStr("denormal"));}),_GS=new T(function(){return B(unCStr("divide by zero"));}),_GT=new T(function(){return B(unCStr("loss of precision"));}),_GU=function(_GV){switch(E(_GV)){case 0:return E(_GP);case 1:return E(_GO);case 2:return E(_GT);case 3:return E(_GS);case 4:return E(_GR);default:return E(_GQ);}},_GW=function(_GX){return new F(function(){return _O(_GO,_GX);});},_GY=function(_GX){return new F(function(){return _O(_GP,_GX);});},_GZ=function(_GX){return new F(function(){return _O(_GQ,_GX);});},_H0=function(_GX){return new F(function(){return _O(_GR,_GX);});},_H1=function(_GX){return new F(function(){return _O(_GS,_GX);});},_H2=function(_GX){return new F(function(){return _O(_GT,_GX);});},_H3=function(_H4){switch(E(_H4)){case 0:return E(_GY);case 1:return E(_GW);case 2:return E(_H2);case 3:return E(_H1);case 4:return E(_H0);default:return E(_GZ);}},_H5=function(_H6,_H7){return new F(function(){return _5a(_H3,_H6,_H7);});},_H8=function(_H9,_Ha){switch(E(_Ha)){case 0:return E(_GY);case 1:return E(_GW);case 2:return E(_H2);case 3:return E(_H1);case 4:return E(_H0);default:return E(_GZ);}},_Hb=[0,_H8,_GU,_H5],_Hc=new T(function(){return [0,_GJ,_Hb,_Hd,_GL];}),_Hd=function(_GX){return [0,_Hc,_GX];},_He=3,_Hf=new T(function(){return B(_Hd(_He));}),_Hg=new T(function(){return die(_Hf);}),_Hh=0,_Hi=new T(function(){return B(_Hd(_Hh));}),_Hj=new T(function(){return die(_Hi);}),_Hk=function(_Hl,_Hm){var _Hn=E(_Hm);switch(_Hn){case -1:var _Ho=E(_Hl);return _Ho==(-2147483648)?E(_Hj):B(_Gy(_Ho,-1));case 0:return E(_Hg);default:return new F(function(){return _Gy(_Hl,_Hn);});}},_Hp=function(_Hq,_Hr){return [0,B(_Hk(E(_Hq)[1],E(_Hr)[1]))];},_Hs=[0,0],_Ht=[0,_Hj,_Hs],_Hu=function(_Hv,_Hw){var _Hx=E(_Hv)[1],_Hy=E(E(_Hw)[1]);switch(_Hy){case -1:var _Hz=E(_Hx);if(_Hz==(-2147483648)){return E(_Ht);}else{if(_Hz<=0){if(_Hz>=0){var _HA=quotRemI(_Hz,-1);return [0,[0,_HA[1]],[0,_HA[2]]];}else{var _HB=quotRemI(_Hz,-1);return [0,[0,_HB[1]],[0,_HB[2]]];}}else{var _HC=quotRemI(_Hz-1|0,-1);return [0,[0,_HC[1]-1|0],[0,(_HC[2]+(-1)|0)+1|0]];}}break;case 0:return E(_Hg);default:if(_Hx<=0){if(_Hx>=0){var _HD=quotRemI(_Hx,_Hy);return [0,[0,_HD[1]],[0,_HD[2]]];}else{if(_Hy<=0){var _HE=quotRemI(_Hx,_Hy);return [0,[0,_HE[1]],[0,_HE[2]]];}else{var _HF=quotRemI(_Hx+1|0,_Hy);return [0,[0,_HF[1]-1|0],[0,(_HF[2]+_Hy|0)-1|0]];}}}else{if(_Hy>=0){if(_Hx>=0){var _HG=quotRemI(_Hx,_Hy);return [0,[0,_HG[1]],[0,_HG[2]]];}else{if(_Hy<=0){var _HH=quotRemI(_Hx,_Hy);return [0,[0,_HH[1]],[0,_HH[2]]];}else{var _HI=quotRemI(_Hx+1|0,_Hy);return [0,[0,_HI[1]-1|0],[0,(_HI[2]+_Hy|0)-1|0]];}}}else{var _HJ=quotRemI(_Hx-1|0,_Hy);return [0,[0,_HJ[1]-1|0],[0,(_HJ[2]+_Hy|0)+1|0]];}}}},_HK=function(_HL,_HM){var _HN=_HL%_HM;if(_HL<=0){if(_HL>=0){return E(_HN);}else{if(_HM<=0){return E(_HN);}else{var _HO=E(_HN);return _HO==0?0:_HO+_HM|0;}}}else{if(_HM>=0){if(_HL>=0){return E(_HN);}else{if(_HM<=0){return E(_HN);}else{var _HP=E(_HN);return _HP==0?0:_HP+_HM|0;}}}else{var _HQ=E(_HN);return _HQ==0?0:_HQ+_HM|0;}}},_HR=function(_HS,_HT){var _HU=E(E(_HT)[1]);switch(_HU){case -1:return E(_Hs);case 0:return E(_Hg);default:return [0,B(_HK(E(_HS)[1],_HU))];}},_HV=function(_HW,_HX){var _HY=E(_HW)[1],_HZ=E(E(_HX)[1]);switch(_HZ){case -1:var _I0=E(_HY);return _I0==(-2147483648)?E(_Hj):[0,quot(_I0,-1)];case 0:return E(_Hg);default:return [0,quot(_HY,_HZ)];}},_I1=function(_I2,_I3){var _I4=E(_I2)[1],_I5=E(E(_I3)[1]);switch(_I5){case -1:var _I6=E(_I4);if(_I6==(-2147483648)){return E(_Ht);}else{var _I7=quotRemI(_I6,-1);return [0,[0,_I7[1]],[0,_I7[2]]];}break;case 0:return E(_Hg);default:var _I8=quotRemI(_I4,_I5);return [0,[0,_I8[1]],[0,_I8[2]]];}},_I9=function(_Ia,_Ib){var _Ic=E(E(_Ib)[1]);switch(_Ic){case -1:return E(_Hs);case 0:return E(_Hg);default:return [0,E(_Ia)[1]%_Ic];}},_Id=function(_Ie){return new F(function(){return _8F(E(_Ie)[1]);});},_If=function(_Ig){return [0,E(B(_8F(E(_Ig)[1]))),E(_FN)];},_Ih=function(_Ii,_Ij){return [0,imul(E(_Ii)[1],E(_Ij)[1])|0];},_Ik=function(_Il,_Im){return [0,E(_Il)[1]+E(_Im)[1]|0];},_In=function(_Io,_Ip){return [0,E(_Io)[1]-E(_Ip)[1]|0];},_Iq=function(_Ir){var _Is=E(_Ir),_It=_Is[1];return _It<0?[0, -_It]:E(_Is);},_Iu=function(_Iv){return [0,B(_a3(_Iv))];},_Iw=function(_Ix){return [0, -E(_Ix)[1]];},_Iy=[0,-1],_Iz=[0,0],_IA=[0,1],_IB=function(_IC){var _ID=E(_IC)[1];return _ID>=0?E(_ID)==0?E(_Iz):E(_IA):E(_Iy);},_IE=[0,_Ik,_Ih,_In,_Iw,_Iq,_IB,_Iu],_IF=function(_IG,_IH){return E(_IG)[1]==E(_IH)[1];},_II=function(_IJ,_IK){return E(_IJ)[1]!=E(_IK)[1];},_IL=[0,_IF,_II],_IM=function(_IN,_IO){var _IP=E(_IN),_IQ=E(_IO);return _IP[1]>_IQ[1]?E(_IP):E(_IQ);},_IR=function(_IS,_IT){var _IU=E(_IS),_IV=E(_IT);return _IU[1]>_IV[1]?E(_IV):E(_IU);},_IW=function(_IX,_IY){return _IX>=_IY?_IX!=_IY?2:1:0;},_IZ=function(_J0,_J1){return new F(function(){return _IW(E(_J0)[1],E(_J1)[1]);});},_J2=function(_J3,_J4){return E(_J3)[1]>=E(_J4)[1];},_J5=function(_J6,_J7){return E(_J6)[1]>E(_J7)[1];},_J8=function(_J9,_Ja){return E(_J9)[1]<=E(_Ja)[1];},_Jb=function(_Jc,_Jd){return E(_Jc)[1]<E(_Jd)[1];},_Je=[0,_IL,_IZ,_Jb,_J2,_J5,_J8,_IM,_IR],_Jf=[0,_IE,_Je,_If],_Jg=[0,_Jf,_Gx,_HV,_I9,_Hp,_HR,_I1,_Hu,_Id],_Jh=function(_Ji){return E(E(_Ji)[1]);},_Jj=function(_Jk,_Jl,_Jm){while(1){if(!(_Jl%2)){var _Jn=B(_8H(_Jk,_Jk)),_Jo=quot(_Jl,2);_Jk=_Jn;_Jl=_Jo;continue;}else{var _Jp=E(_Jl);if(_Jp==1){return new F(function(){return _8H(_Jk,_Jm);});}else{var _Jn=B(_8H(_Jk,_Jk));_Jl=quot(_Jp-1|0,2);var _Jq=B(_8H(_Jk,_Jm));_Jk=_Jn;_Jm=_Jq;continue;}}}},_Jr=function(_Js,_Jt){while(1){if(!(_Jt%2)){var _Ju=B(_8H(_Js,_Js)),_Jv=quot(_Jt,2);_Js=_Ju;_Jt=_Jv;continue;}else{var _Jw=E(_Jt);if(_Jw==1){return E(_Js);}else{return new F(function(){return _Jj(B(_8H(_Js,_Js)),quot(_Jw-1|0,2),_Js);});}}}},_Jx=function(_Jy){return E(E(_Jy)[2]);},_Jz=function(_JA){return E(E(_JA)[1]);},_JB=function(_JC){return E(E(_JC)[2]);},_JD=[0,0],_JE=[0,2],_JF=function(_JG){return E(E(_JG)[7]);},_JH=function(_JI,_JJ,_JK,_JL,_JM){return new F(function(){return A(E(E(_JJ)[1])[1],[new T(function(){return B(A(_JL,[_JM,new T(function(){return B(A(_JF,[_JI,_JE]));})]));}),new T(function(){return B(A(_JF,[_JI,_JD]));})]);});},_JN=function(_JO){return E(E(_JO)[3]);},_JP=new T(function(){return B(unCStr("Negative exponent"));}),_JQ=new T(function(){return B(err(_JP));}),_JR=function(_JS,_JT,_JU,_JV){var _JW=B(_FL(_JT)),_JX=_JW[1],_JY=E(_JW[2]);if(!B(A(_JY[3],[_JV,new T(function(){return B(A(_JF,[_JX,_JD]));})]))){if(!B(A(E(_JY[1])[1],[_JV,new T(function(){return B(A(_JF,[_JX,_JD]));})]))){var _JZ=B(_FL(_JT)),_K0=_JZ[1],_K1=new T(function(){return B(_FL(_JT));}),_K2=new T(function(){return B(_Jh(_K1));});return new F(function(){return (function(_K3,_K4){while(1){var _K5=(function(_K6,_K7){var _K8=E(_JT),_K9=_K8[3],_Ka=E(_K8[1]);if(!B(_JH(_Ka[1],_Ka[2],_Ka[3],_K8[4],_K7))){return !B(A(E(E(_JZ[2])[1])[1],[_K7,new T(function(){return B(A(_JF,[_K0,_FN]));})]))?B((function(_Kb,_Kc,_Kd){while(1){var _Ke=(function(_Kf,_Kg,_Kh){var _Ki=E(_JT),_Kj=_Ki[3],_Kk=E(_Ki[1]);if(!B(_JH(_Kk[1],_Kk[2],_Kk[3],_Ki[4],_Kg))){if(!B(A(new T(function(){return B(_2L(new T(function(){return B(_Jz(new T(function(){return B(_JB(_K1));})));})));}),[_Kg,new T(function(){return B(A(_JF,[_K2,_FN]));})]))){_Kb=new T(function(){return B(A(new T(function(){return B(_Jx(_JS));}),[_Kf,_Kf]));});_Kc=new T(function(){return B(A(_Kj,[new T(function(){return B(A(new T(function(){return B(_JN(_K2));}),[_Kg,new T(function(){return B(A(_JF,[_K2,_FN]));})]));}),new T(function(){return B(A(_JF,[_K2,_JE]));})]));});_Kd=new T(function(){return B(A(new T(function(){return B(_Jx(_JS));}),[_Kf,_Kh]));});return null;}else{return new F(function(){return A(new T(function(){return B(_Jx(_JS));}),[_Kf,_Kh]);});}}else{_Kb=new T(function(){return B(A(new T(function(){return B(_Jx(_JS));}),[_Kf,_Kf]));});_Kc=new T(function(){return B(A(_Kj,[_Kg,new T(function(){return B(A(_JF,[_K2,_JE]));})]));});var _Kl=_Kh;_Kd=_Kl;return null;}})(_Kb,_Kc,_Kd);if(_Ke!=null){return _Ke;}}})(new T(function(){return B(A(new T(function(){return B(_Jx(_JS));}),[_K6,_K6]));}),new T(function(){return B(A(_K9,[new T(function(){return B(A(new T(function(){return B(_JN(_K0));}),[_K7,new T(function(){return B(A(_JF,[_K0,_FN]));})]));}),new T(function(){return B(A(_JF,[_K0,_JE]));})]));}),_K6)):E(_K6);}else{_K3=new T(function(){return B(A(new T(function(){return B(_Jx(_JS));}),[_K6,_K6]));});_K4=new T(function(){return B(A(_K9,[_K7,new T(function(){return B(A(_JF,[_K0,_JE]));})]));});return null;}})(_K3,_K4);if(_K5!=null){return _K5;}}})(_JU,_JV);});}else{return new F(function(){return A(_JF,[_JS,_FN]);});}}else{return E(_JQ);}},_Km=new T(function(){return B(err(_JP));}),_Kn=function(_Ko,_Kp){var _Kq=E(_Ko);return _Kq[0]==0?_Kq[1]*Math.pow(2,_Kp):I_toNumber(_Kq[1])*Math.pow(2,_Kp);},_Kr=function(_Ks,_Kt){while(1){var _Ku=E(_Ks);if(!_Ku[0]){var _Kv=E(_Ku[1]);if(_Kv==(-2147483648)){_Ks=[1,I_fromInt(-2147483648)];continue;}else{var _Kw=E(_Kt);if(!_Kw[0]){var _Kx=_Kw[1];return [0,[0,quot(_Kv,_Kx)],[0,_Kv%_Kx]];}else{_Ks=[1,I_fromInt(_Kv)];_Kt=_Kw;continue;}}}else{var _Ky=E(_Kt);if(!_Ky[0]){_Ks=_Ku;_Kt=[1,I_fromInt(_Ky[1])];continue;}else{var _Kz=I_quotRem(_Ku[1],_Ky[1]);return [0,[1,_Kz[1]],[1,_Kz[2]]];}}}},_KA=function(_KB,_KC){var _KD=B(_nK(_KC)),_KE=_KD[1],_KF=_KD[2],_KG=new T(function(){return B(_Jh(new T(function(){return B(_FL(_KB));})));});if(_KF<0){var _KH= -_KF;if(_KH>=0){var _KI=E(_KH),_KJ=_KI==0?E(_FN):B(_Jr(_DK,_KI));if(!B(_og(_KJ,_oa))){var _KK=B(_Kr(_KE,_KJ));return [0,new T(function(){return B(A(_JF,[_KG,_KK[1]]));}),new T(function(){return [0,B(_Kn(_KK[2],_KF))];})];}else{return E(_Hg);}}else{return E(_Km);}}else{return [0,new T(function(){return B(A(_Jx,[_KG,new T(function(){return B(A(_JF,[_KG,_KE]));}),new T(function(){return B(_JR(_KG,_Jg,new T(function(){return B(A(_JF,[_KG,_DK]));}),[0,_KF]));})]));}),_Bv];}},_KL=function(_KM,_KN){var _KO=B(_KA(_KM,E(_KN)[1])),_KP=_KO[1];if(E(_KO[2])[1]<=0){return E(_KP);}else{var _KQ=E(B(_FL(_KM))[1]);return new F(function(){return A(_KQ[1],[_KP,new T(function(){return B(A(_KQ[7],[_F2]));})]);});}},_KR=function(_KS,_KT){var _KU=B(_KA(_KS,E(_KT)[1])),_KV=_KU[1];if(E(_KU[2])[1]>=0){return E(_KV);}else{var _KW=E(B(_FL(_KS))[1]);return new F(function(){return A(_KW[3],[_KV,new T(function(){return B(A(_KW[7],[_F2]));})]);});}},_KX=function(_KY,_KZ){var _L0=B(_KA(_KY,E(_KZ)[1]));return [0,_L0[1],_L0[2]];},_L1=function(_L2,_L3){var _L4=B(_KA(_L2,_L3)),_L5=_L4[1],_L6=E(_L4[2])[1],_L7=new T(function(){var _L8=E(B(_FL(_L2))[1]),_L9=_L8[7];return _L6>=0?B(A(_L8[1],[_L5,new T(function(){return B(A(_L9,[_F2]));})])):B(A(_L8[3],[_L5,new T(function(){return B(A(_L9,[_F2]));})]));});if(_L6<0){var _La= -_L6-0.5;if(_La>=0){if(!E(_La)){var _Lb=E(_L2),_Lc=E(_Lb[1]);return !B(_JH(_Lc[1],_Lc[2],_Lc[3],_Lb[4],_L5))?E(_L7):E(_L5);}else{return E(_L7);}}else{return E(_L5);}}else{var _Ld=_L6-0.5;if(_Ld>=0){if(!E(_Ld)){var _Le=E(_L2),_Lf=E(_Le[1]);return !B(_JH(_Lf[1],_Lf[2],_Lf[3],_Le[4],_L5))?E(_L7):E(_L5);}else{return E(_L7);}}else{return E(_L5);}}},_Lg=function(_Lh,_Li){return new F(function(){return _L1(_Lh,E(_Li)[1]);});},_Lj=function(_Lk,_Ll){return E(B(_KA(_Lk,E(_Ll)[1]))[1]);},_Lm=[0,_FK,_BQ,_KX,_Lj,_Lg,_KL,_KR],_Ln=function(_Lo,_Lp){return E(_Lo)[1]!=E(_Lp)[1]?true:false;},_Lq=function(_Lr,_Ls){return E(_Lr)[1]==E(_Ls)[1];},_Lt=[0,_Lq,_Ln],_Lu=function(_Lv,_Lw){return E(_Lv)[1]<E(_Lw)[1];},_Lx=function(_Ly,_Lz){return E(_Ly)[1]<=E(_Lz)[1];},_LA=function(_LB,_LC){return E(_LB)[1]>E(_LC)[1];},_LD=function(_LE,_LF){return E(_LE)[1]>=E(_LF)[1];},_LG=function(_LH,_LI){var _LJ=E(_LH)[1],_LK=E(_LI)[1];return _LJ>=_LK?_LJ!=_LK?2:1:0;},_LL=function(_LM,_LN){var _LO=E(_LM),_LP=E(_LN);return _LO[1]>_LP[1]?E(_LO):E(_LP);},_LQ=function(_LR,_LS){var _LT=E(_LR),_LU=E(_LS);return _LT[1]>_LU[1]?E(_LU):E(_LT);},_LV=[0,_Lt,_LG,_Lu,_LD,_LA,_Lx,_LL,_LQ],_LW=function(_LX,_LY){while(1){var _LZ=(function(_M0,_M1){var _M2=E(_F3)[1]["v"]["i8"][(255&_M0>>>0)>>>0&4294967295];if(_M1>_M2){if(_M2>=8){var _M3=_M0>>8,_M4=_M1-8|0;_LX=_M3;_LY=_M4;return null;}else{return [0,new T(function(){return B(_8F(_M0>>_M2));}),_M1-_M2|0];}}else{return [0,new T(function(){return B(_8F(_M0>>_M1));}),0];}})(_LX,_LY);if(_LZ!=null){return _LZ;}}},_M5=function(_M6){var _M7=decodeFloat(_M6),_M8=_M7[1],_M9=_M7[2];if(_M9<0){var _Ma=function(_Mb){if(!_Mb){return [0,B(_8F(_M8)),B(_ov(_F2, -_M9))];}else{var _Mc=B(_LW(_M8, -_M9));return [0,E(_Mc[1]),B(_ov(_F2,_Mc[2]))];}};return (_M8>>>0&1)>>>0==0?B(_Ma(1)):B(_Ma(0));}else{return [0,B(_ov(B(_8F(_M8)),_M9)),_F2];}},_Md=function(_Me){var _Mf=B(_M5(E(_Me)[1]));return [0,E(_Mf[1]),E(_Mf[2])];},_Mg=[0,_D7,_LV,_Md],_Mh=[0,-1],_Mi=[0,1],_Mj=function(_Mk,_Ml){var _Mm=E(_Mk);return _Mm[0]==0?_Mm[1]*Math.pow(2,_Ml):I_toNumber(_Mm[1])*Math.pow(2,_Ml);},_Mn=[0,0],_Mo=function(_Mp,_Mq){var _Mr=decodeFloat(_Mq),_Ms=_Mr[1],_Mt=_Mr[2],_Mu=new T(function(){return B(_Jh(new T(function(){return B(_FL(_Mp));})));});if(_Mt<0){var _Mv=new T(function(){if(_Ms<0){var _Mw= -_Mt;if(_Mw<32){var _Mx=[0, -( -_Ms>>_Mw)];}else{var _Mx= -_Ms>=0?E(_Mn):E(_Mi);}var _My=_Mx,_Mz=_My,_MA=_Mz;}else{var _MB= -_Mt;if(_MB<32){var _MC=[0,_Ms>>_MB];}else{var _MC=_Ms>=0?E(_Mn):E(_Mh);}var _MD=_MC,_ME=_MD,_MA=_ME;}var _MF=_MA;return _MF;});return [0,new T(function(){return B(A(_JF,[_Mu,new T(function(){return B(_8F(E(_Mv)[1]));})]));}),new T(function(){var _MG= -_Mt;if(_MG<32){var _MH=[0,B(_Mj(B(_8F(_Ms-(E(_Mv)[1]<<_MG)|0)),_Mt))];}else{var _MH=[0,B(_Mj(B(_8F(_Ms)),_Mt))];}var _MI=_MH,_MJ=_MI,_MK=_MJ;return _MK;})];}else{return [0,new T(function(){return B(A(_Jx,[_Mu,new T(function(){return B(A(_JF,[_Mu,new T(function(){return B(_8F(_Ms));})]));}),new T(function(){return B(_JR(_Mu,_Jg,new T(function(){return B(A(_JF,[_Mu,_DK]));}),[0,_Mt]));})]));}),_CQ];}},_ML=function(_MM,_MN){var _MO=B(_Mo(_MM,E(_MN)[1])),_MP=_MO[1];if(E(_MO[2])[1]<=0){return E(_MP);}else{var _MQ=E(B(_FL(_MM))[1]);return new F(function(){return A(_MQ[1],[_MP,new T(function(){return B(A(_MQ[7],[_F2]));})]);});}},_MR=function(_MS,_MT){var _MU=B(_Mo(_MS,E(_MT)[1])),_MV=_MU[1];if(E(_MU[2])[1]>=0){return E(_MV);}else{var _MW=E(B(_FL(_MS))[1]);return new F(function(){return A(_MW[3],[_MV,new T(function(){return B(A(_MW[7],[_F2]));})]);});}},_MX=function(_MY,_MZ){var _N0=B(_Mo(_MY,E(_MZ)[1]));return [0,_N0[1],_N0[2]];},_N1=function(_N2,_N3){var _N4=B(_Mo(_N2,_N3)),_N5=_N4[1],_N6=E(_N4[2])[1],_N7=new T(function(){var _N8=E(B(_FL(_N2))[1]),_N9=_N8[7];return _N6>=0?B(A(_N8[1],[_N5,new T(function(){return B(A(_N9,[_F2]));})])):B(A(_N8[3],[_N5,new T(function(){return B(A(_N9,[_F2]));})]));});if(_N6<0){var _Na= -_N6-0.5;if(_Na>=0){if(!E(_Na)){var _Nb=E(_N2),_Nc=E(_Nb[1]);return !B(_JH(_Nc[1],_Nc[2],_Nc[3],_Nb[4],_N5))?E(_N7):E(_N5);}else{return E(_N7);}}else{return E(_N5);}}else{var _Nd=_N6-0.5;if(_Nd>=0){if(!E(_Nd)){var _Ne=E(_N2),_Nf=E(_Ne[1]);return !B(_JH(_Nf[1],_Nf[2],_Nf[3],_Ne[4],_N5))?E(_N7):E(_N5);}else{return E(_N7);}}else{return E(_N5);}}},_Ng=function(_Nh,_Ni){return new F(function(){return _N1(_Nh,E(_Ni)[1]);});},_Nj=function(_Nk,_Nl){return E(B(_Mo(_Nk,E(_Nl)[1]))[1]);},_Nm=[0,_Mg,_Db,_MX,_Nj,_Ng,_ML,_MR],_Nn=function(_No){return new F(function(){return err(B(unAppCStr("Char.intToDigit: not a digit ",new T(function(){if(_No>=0){var _Np=jsShowI(_No),_Nq=_Np,_Nr=fromJSStr(_Nq);}else{var _Ns=jsShowI(_No),_Nt=_Ns,_Nr=fromJSStr(_Nt);}var _Nu=_Nr;return _Nu;}))));});},_Nv=function(_Nw){var _Nx=function(_Ny){if(_Nw<10){return new F(function(){return _Nn(_Nw);});}else{if(_Nw>15){return new F(function(){return _Nn(_Nw);});}else{return (97+_Nw|0)-10|0;}}};if(_Nw<0){return new F(function(){return _Nx(_);});}else{if(_Nw>9){return new F(function(){return _Nx(_);});}else{return 48+_Nw|0;}}},_Nz=function(_NA){return [0,B(_Nv(E(_NA)[1]))];},_NB=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_NC=function(_ND){return new F(function(){return _5u([0,new T(function(){return B(_5J(_ND,_NB));})],_5r);});},_NE=new T(function(){return B(_NC("GHC/Float.lhs:619:11-64|d : ds\'"));}),_NF=function(_NG,_NH){if(E(_NG)[1]<=0){var _NI=B(_1y(_Nz,[1,_Mn,_NH]));return _NI[0]==0?E(_NE):[0,_NI[1],_NI[2]];}else{var _NJ=B(_1y(_Nz,_NH));return _NJ[0]==0?E(_NE):[0,_NJ[1],_NJ[2]];}},_NK=function(_NL){return E(E(_NL)[1]);},_NM=function(_NN){return E(E(_NN)[1]);},_NO=function(_NP){return E(E(_NP)[1]);},_NQ=[0,48],_NR=[1,_NQ,_u],_NS=[0,46],_NT=function(_NU,_NV,_NW){while(1){var _NX=(function(_NY,_NZ,_O0){var _O1=E(_NY);if(!_O1){var _O2=B(_B2(_NZ,_u));return _O2[0]==0?[1,_NQ,[1,_NS,new T(function(){var _O3=E(_O0);return _O3[0]==0?E(_NR):E(_O3);})]]:B(_O(_O2,[1,_NS,new T(function(){var _O4=E(_O0);return _O4[0]==0?E(_NR):E(_O4);})]));}else{var _O5=E(_O0);if(!_O5[0]){_NU=_O1-1|0;var _O6=[1,_NQ,_NZ];_NW=_u;_NV=_O6;return null;}else{_NU=_O1-1|0;var _O6=[1,_O5[1],_NZ];_NW=_O5[2];_NV=_O6;return null;}}})(_NU,_NV,_NW);if(_NX!=null){return _NX;}}},_O7=[0,0],_O8=new T(function(){return B(unCStr(" out of range "));}),_O9=new T(function(){return B(unCStr("}.index: Index "));}),_Oa=new T(function(){return B(unCStr("Ix{"));}),_Ob=[1,_16,_u],_Oc=[1,_16,_Ob],_Od=function(_Oe,_Of,_Og,_Oh,_Oi){return new F(function(){return err(B(_O(_Oa,new T(function(){return B(_O(_Oe,new T(function(){return B(_O(_O9,[1,_17,new T(function(){return B(A(_Oi,[_O7,_Of,[1,_16,new T(function(){return B(_O(_O8,[1,_17,[1,_17,new T(function(){return B(A(_lo,[_le,[1,new T(function(){return B(A(_Oi,[_kJ,_Og]));}),[1,new T(function(){return B(A(_Oi,[_kJ,_Oh]));}),_u]],_Oc]));})]]));})]]));})]));})));}))));});},_Oj=function(_Ok,_Ol,_Om,_On){var _Oo=E(_Om);return new F(function(){return _Od(_Ok,_Ol,_Oo[1],_Oo[2],E(_On)[1]);});},_Op=function(_Oq,_Or,_Os,_Ot){return new F(function(){return _Oj(_Ot,_Os,_Or,_Oq);});},_Ou=new T(function(){return B(unCStr("Int"));}),_Ov=function(_Ow,_Ox,_Oy){return new F(function(){return _Op(_ld,[0,_Ox,_Oy],_Ow,_Ou);});},_Oz=new T(function(){return B(unCStr("(Array.!): undefined array element"));}),_OA=new T(function(){return B(err(_Oz));}),_OB=[0,1100],_OC=[0,_Mn,_OB],_OD=function(_OE){return new F(function(){return _Op(_ld,_OC,[0,_OE],_Ou);});},_OF=function(_){var _OG=newArr(1101,_OA),_OH=_OG;return new F(function(){return (function(_OI,_){while(1){var _OJ=(function(_OK,_){if(0>_OK){return new F(function(){return _OD(_OK);});}else{if(_OK>1100){return new F(function(){return _OD(_OK);});}else{var _=_OH[_OK]=new T(function(){if(_OK>=0){var _OL=E(_OK),_OM=_OL==0?E(_FN):B(_Jr(_DK,_OL));}else{var _OM=E(_Km);}var _ON=_OM;return _ON;}),_OO=E(_OK);if(_OO==1100){var _OP=_OH,_OQ=_OP;return [0,E(_Mn),E(_OB),1101,_OQ];}else{_OI=_OO+1|0;return null;}}}})(_OI,_);if(_OJ!=null){return _OJ;}}})(0,_);});},_OR=function(_OS){var _OT=B(A(_OS,[_])),_OU=_OT;return E(_OU);},_OV=new T(function(){return B(_OR(_OF));}),_OW=[0,10],_OX=[0,324],_OY=[0,_Mn,_OX],_OZ=function(_P0){return new F(function(){return _Op(_ld,_OY,[0,_P0],_Ou);});},_P1=function(_){var _P2=newArr(325,_OA),_P3=_P2;return new F(function(){return (function(_P4,_){while(1){var _P5=(function(_P6,_){if(0>_P6){return new F(function(){return _OZ(_P6);});}else{if(_P6>324){return new F(function(){return _OZ(_P6);});}else{var _=_P3[_P6]=new T(function(){if(_P6>=0){var _P7=E(_P6),_P8=_P7==0?E(_FN):B(_Jr(_OW,_P7));}else{var _P8=E(_Km);}var _P9=_P8;return _P9;}),_Pa=E(_P6);if(_Pa==324){var _Pb=_P3,_Pc=_Pb;return [0,E(_Mn),E(_OX),325,_Pc];}else{_P4=_Pa+1|0;return null;}}}})(_P4,_);if(_P5!=null){return _P5;}}})(0,_);});},_Pd=new T(function(){return B(_OR(_P1));}),_Pe=function(_Pf,_Pg){var _Ph=[0,_Pg],_Pi=function(_Pj){if(!B(_og(_Pf,_OW))){if(_Pg>=0){var _Pk=E(_Pg);return _Pk==0?E(_FN):B(_Jr(_Pf,_Pk));}else{return E(_Km);}}else{if(_Pg>324){if(_Pg>=0){var _Pl=E(_Pg);return _Pl==0?E(_FN):B(_Jr(_Pf,_Pl));}else{return E(_Km);}}else{var _Pm=E(_Pd),_Pn=E(_Pm[1]),_Po=_Pn[1],_Pp=E(_Pm[2]);if(_Po>_Pg){return new F(function(){return _Ov(_Ph,_Pn,_Pp);});}else{if(_Pg>_Pp[1]){return new F(function(){return _Ov(_Ph,_Pn,_Pp);});}else{return E(_Pm[4][_Pg-_Po|0]);}}}}};if(!B(_og(_Pf,_DK))){return new F(function(){return _Pi(_);});}else{if(_Pg<0){return new F(function(){return _Pi(_);});}else{if(_Pg>1100){return new F(function(){return _Pi(_);});}else{var _Pq=E(_OV),_Pr=E(_Pq[1]),_Ps=_Pr[1],_Pt=E(_Pq[2]);if(_Ps>_Pg){return new F(function(){return _Ov(_Ph,_Pr,_Pt);});}else{if(_Pg>_Pt[1]){return new F(function(){return _Ov(_Ph,_Pr,_Pt);});}else{return E(_Pq[4][_Pg-_Ps|0]);}}}}}},_Pu=function(_Pv,_Pw){var _Px=E(_Pv);if(!_Px[0]){var _Py=_Px[1],_Pz=E(_Pw);return _Pz[0]==0?_Py>_Pz[1]:I_compareInt(_Pz[1],_Py)<0;}else{var _PA=_Px[1],_PB=E(_Pw);return _PB[0]==0?I_compareInt(_PA,_PB[1])>0:I_compare(_PA,_PB[1])>0;}},_PC=[1,_Mn,_u],_PD=function(_PE,_PF){while(1){var _PG=E(_PE);if(!_PG[0]){var _PH=E(_PG[1]);if(_PH==(-2147483648)){_PE=[1,I_fromInt(-2147483648)];continue;}else{var _PI=E(_PF);if(!_PI[0]){return [0,quot(_PH,_PI[1])];}else{_PE=[1,I_fromInt(_PH)];_PF=_PI;continue;}}}else{var _PJ=_PG[1],_PK=E(_PF);return _PK[0]==0?[0,I_toInt(I_quot(_PJ,I_fromInt(_PK[1])))]:[1,I_quot(_PJ,_PK[1])];}}},_PL=function(_PM,_PN,_PO,_PP,_PQ,_PR,_PS,_PT){if(!B(A(_PM,[_PT,new T(function(){return B(A(_JF,[B(_NM(B(_NK(_PN)))),_oa]));})]))){var _PU=new T(function(){return B(A(_PO,[_PT]));}),_PV=new T(function(){return B(A(_PP,[_PT]));}),_PW=new T(function(){return [0,E(B(A(_PQ,[_PT]))[1])[1]-E(_PV)[1]|0];}),_PX=new T(function(){return B(A(_PR,[_PT]));}),_PY=new T(function(){return E(E(_PX)[2]);}),_PZ=new T(function(){var _Q0=E(_PY),_Q1=_Q0[1],_Q2=E(_PW)[1]-_Q1|0;if(_Q2<=0){var _Q3=[0,new T(function(){return E(E(_PX)[1]);}),_Q0];}else{var _Q3=[0,new T(function(){var _Q4=B(_Pe(_PU,_Q2));if(!B(_og(_Q4,_oa))){var _Q5=B(_PD(E(_PX)[1],_Q4));}else{var _Q5=E(_Hg);}var _Q6=_Q5;return _Q6;}),[0,_Q1+_Q2|0]];}var _Q7=_Q3,_Q8=_Q7,_Q9=_Q8,_Qa=_Q9;return _Qa;}),_Qb=new T(function(){return E(E(_PZ)[2]);}),_Qc=new T(function(){return E(E(_PZ)[1]);}),_Qd=new T(function(){var _Qe=E(_Qb)[1];if(_Qe<0){if(_Qe<=E(_PW)[1]){var _Qf=[0,new T(function(){return B(_8H(_Qc,_DK));}),new T(function(){return B(_8H(B(_Pe(_PU, -_Qe)),_DK));}),_F2,_F2];}else{var _Qf=!B(_og(_Qc,B(_Pe(_PU,E(_PV)[1]-1|0))))?[0,new T(function(){return B(_8H(_Qc,_DK));}),new T(function(){return B(_8H(B(_Pe(_PU, -_Qe)),_DK));}),_F2,_F2]:[0,new T(function(){return B(_8H(B(_8H(_Qc,_PU)),_DK));}),new T(function(){return B(_8H(B(_Pe(_PU, -_Qe+1|0)),_DK));}),_PU,_F2];}var _Qg=_Qf,_Qh=_Qg,_Qi=_Qh;}else{var _Qj=new T(function(){return B(_Pe(_PU,_Qe));}),_Qi=!B(_og(_Qc,B(_Pe(_PU,E(_PV)[1]-1|0))))?[0,new T(function(){return B(_8H(B(_8H(_Qc,_Qj)),_DK));}),_DK,_Qj,_Qj]:[0,new T(function(){return B(_8H(B(_8H(B(_8H(_Qc,_Qj)),_PU)),_DK));}),new T(function(){return B(_8H(_DK,_PU));}),new T(function(){return B(_8H(_Qj,_PU));}),_Qj];}var _Qk=_Qi,_Ql=_Qk;return _Ql;}),_Qm=new T(function(){return E(E(_Qd)[2]);}),_Qn=new T(function(){return E(E(_Qd)[3]);}),_Qo=new T(function(){return E(E(_Qd)[1]);}),_Qp=new T(function(){var _Qq=new T(function(){return B(_8p(_Qo,_Qn));}),_Qr=function(_Qs){var _Qt=(Math.log(B(_CL(B(_8p(_Qc,_F2)))))+E(_Qb)[1]*Math.log(B(_CL(_PU))))/Math.log(B(_CL(_PS))),_Qu=_Qt&4294967295;return _Qu>=_Qt?E(_Qu):_Qu+1|0;},_Qv=function(_Qw){while(1){if(_Qw<0){if(!B(_a6(B(_8H(B(_Pe(_PS, -_Qw)),_Qq)),_Qm))){var _Qx=_Qw+1|0;_Qw=_Qx;continue;}else{return E(_Qw);}}else{if(!B(_a6(_Qq,B(_8H(B(_Pe(_PS,_Qw)),_Qm))))){var _Qx=_Qw+1|0;_Qw=_Qx;continue;}else{return E(_Qw);}}}};if(!B(_og(_PU,_DK))){var _Qy=[0,B(_Qv(B(_Qr(_))))];}else{if(!B(_og(_PS,_OW))){var _Qz=[0,B(_Qv(B(_Qr(_))))];}else{var _QA=(E(_PV)[1]-1|0)+E(_PY)[1]|0;if(_QA<0){var _QB=[0,B(_Qv(quot(imul(_QA,8651)|0,28738)))];}else{var _QB=[0,B(_Qv(quot(imul(_QA,8651)|0,28738)+1|0))];}var _QC=_QB,_QD=_QC,_QE=_QD,_QF=_QE,_QG=_QF,_Qz=_QG;}var _Qy=_Qz;}return _Qy;});return [0,new T(function(){var _QH=E(_Qp)[1],_QI=function(_QJ,_QK,_QL,_QM,_QN){while(1){var _QO=(function(_QP,_QQ,_QR,_QS,_QT){if(!B(_og(_QR,_oa))){var _QU=B(_Kr(B(_8H(_QQ,_PS)),_QR)),_QV=_QU[1],_QW=_QU[2],_QX=B(_8H(_QT,_PS)),_QY=B(_8H(_QS,_PS));if(!B(_Y(_QW,_QX))){if(!B(_Pu(B(_8p(_QW,_QY)),_QR))){var _QZ=[1,_QV,_QP];_QK=_QW;var _R0=_QR;_QM=_QY;_QN=_QX;_QJ=_QZ;_QL=_R0;return null;}else{return [1,new T(function(){return B(_8p(_QV,_F2));}),_QP];}}else{return !B(_Pu(B(_8p(_QW,_QY)),_QR))?[1,_QV,_QP]:!B(_Y(B(_8H(_QW,_DK)),_QR))?[1,new T(function(){return B(_8p(_QV,_F2));}),_QP]:[1,_QV,_QP];}}else{return E(_Hg);}})(_QJ,_QK,_QL,_QM,_QN);if(_QO!=null){return _QO;}}};if(_QH<0){var _R1=B(_Pe(_PS, -_QH)),_R2=B(_1y(_Iu,B(_B2(B(_QI(_u,B(_8H(_Qo,_R1)),_Qm,B(_8H(_Qn,_R1)),B(_8H(E(_Qd)[4],_R1)))),_u))));}else{var _R2=B(_1y(_Iu,B(_B2(B(_QI(_u,_Qo,B(_8H(_Qm,B(_Pe(_PS,_QH)))),_Qn,E(_Qd)[4])),_u))));}var _R3=_R2,_R4=_R3;return _R4;}),_Qp];}else{return [0,_PC,_Mn];}},_R5=function(_R6){return E(_R6)[1]%2==0?true:false;},_R7=new T(function(){return B(unCStr("roundTo: bad Value"));}),_R8=new T(function(){return B(err(_R7));}),_R9=function(_Ra){return E(E(_Ra)[1])==0?true:false;},_Rb=function(_Rc){return _Rc>1?[1,_Mn,new T(function(){return B(_Rb(_Rc-1|0));})]:E(_PC);},_Rd=function(_Re,_Rf,_Rg){var _Rh=function(_Ri,_Rj,_Rk){var _Rl=E(_Rk);if(!_Rl[0]){return [0,_Mn,new T(function(){var _Rm=E(_Ri)[1];return _Rm>0?B(_Rb(_Rm)):[0];})];}else{var _Rn=_Rl[1],_Ro=_Rl[2],_Rp=E(E(_Ri)[1]);if(!_Rp){var _Rq=E(_Rn)[1],_Rr=E(new T(function(){return [0,quot(E(_Re)[1],2)];}))[1];return _Rq!=_Rr?[0,new T(function(){return _Rq<_Rr?E(_Mn):E(_Mi);}),_u]:!E(_Rj)?[0,new T(function(){return _Rq<_Rr?E(_Mn):E(_Mi);}),_u]:!B(_zZ(_R9,_Ro))?[0,new T(function(){return _Rq<_Rr?E(_Mn):E(_Mi);}),_u]:[0,_Mn,_u];}else{var _Rs=B(_Rh([0,_Rp-1|0],new T(function(){return B(_R5(_Rn));}),_Ro)),_Rt=_Rs[2],_Ru=E(_Rs[1])[1]+E(_Rn)[1]|0;return _Ru!=E(_Re)[1]?[0,_Mn,[1,[0,_Ru],_Rt]]:[0,_Mi,[1,_Mn,_Rt]];}}},_Rv=B(_Rh(_Rf,_n,_Rg));switch(E(E(_Rv[1])[1])){case 0:return E(_Rv);case 1:return [0,_Mi,[1,_Mi,_Rv[2]]];default:return E(_R8);}},_Rw=function(_Rx,_Ry){var _Rz=E(_Rx);if(!_Rz){return [0,_u,_Ry];}else{var _RA=E(_Ry);if(!_RA[0]){return [0,_u,_u];}else{var _RB=new T(function(){var _RC=B(_Rw(_Rz-1|0,_RA[2]));return [0,_RC[1],_RC[2]];});return [0,[1,_RA[1],new T(function(){return E(E(_RB)[1]);})],new T(function(){return E(E(_RB)[2]);})];}}},_RD=function(_RE){return E(E(_RE)[3]);},_RF=0,_RG=1,_RH=[0,10],_RI=new T(function(){return B(unCStr("e0"));}),_RJ=function(_RK,_RL){var _RM=E(_RK);if(!_RM[0]){return E(_RI);}else{var _RN=_RM[1];return _RL>1?[1,_RN,new T(function(){return B(_RJ(_RM[2],_RL-1|0));})]:[1,_RN,_RI];}},_RO=function(_RP,_RQ){var _RR=E(_RQ);return _RR[0]==0?[0]:[1,_RP,new T(function(){return B(_RO(_RR[1],_RR[2]));})];},_RS=new T(function(){return B(unCStr("init"));}),_RT=new T(function(){return B(_lk(_RS));}),_RU=new T(function(){return B(_NC("GHC/Float.lhs:591:12-70|(d : ds\')"));}),_RV=[0,101],_RW=new T(function(){return B(unCStr("Infinity"));}),_RX=new T(function(){return B(unCStr("-Infinity"));}),_RY=new T(function(){return B(unCStr("NaN"));}),_RZ=new T(function(){return B(unCStr("formatRealFloat/doFmt/FFExponent: []"));}),_S0=new T(function(){return B(err(_RZ));}),_S1=new T(function(){return B(unCStr("0.0e0"));}),_S2=function(_S3){return E(E(_S3)[4]);},_S4=new T(function(){return [1,_NQ,_S4];}),_S5=function(_S6,_S7,_S8,_S9,_Sa,_Sb,_Sc,_Sd,_Se,_Sf,_Sg,_Sh){if(!B(A(_Sc,[_Sh]))){var _Si=new T(function(){return B(_NM(new T(function(){return B(_NK(_S7));})));});if(!B(A(_Sd,[_Sh]))){var _Sj=function(_Sk,_Sl,_Sm){while(1){var _Sn=(function(_So,_Sp,_Sq){switch(E(_So)){case 0:var _Sr=E(_Sg);if(!_Sr[0]){var _Ss=B(_1y(_Nz,_Sp));if(!_Ss[0]){return E(_S0);}else{var _St=_Ss[2],_Su=E(_Ss[1]),_Sv=function(_Sw){var _Sx=E(_St);return _Sx[0]==0?[1,_Su,new T(function(){return B(unAppCStr(".0e",new T(function(){return B(_9V(0,E(_Sq)[1]-1|0,_u));})));})]:[1,_Su,[1,_NS,new T(function(){return B(_O(_Sx,[1,_RV,new T(function(){return B(_9V(0,E(_Sq)[1]-1|0,_u));})]));})]];};return E(_Su[1])==48?E(_St)[0]==0?E(_S1):B(_Sv(_)):B(_Sv(_));}}else{var _Sy=new T(function(){var _Sz=E(_Sr[1]);return _Sz[1]>1?E(_Sz):E(_Mi);}),_SA=function(_SB){var _SC=new T(function(){var _SD=B(_Rd(_RH,new T(function(){return [0,E(_Sy)[1]+1|0];}),_Sp));return [0,_SD[1],_SD[2]];}),_SE=new T(function(){return E(E(_SC)[1]);}),_SF=new T(function(){if(E(_SE)[1]<=0){var _SG=B(_1y(_Nz,E(_SC)[2])),_SH=_SG[0]==0?E(_RU):[0,_SG[1],_SG[2]];}else{var _SI=E(E(_SC)[2]);if(!_SI[0]){var _SJ=E(_RT);}else{var _SK=B(_1y(_Nz,B(_RO(_SI[1],_SI[2])))),_SJ=_SK[0]==0?E(_RU):[0,_SK[1],_SK[2]];}var _SL=_SJ,_SH=_SL;}var _SM=_SH,_SN=_SM;return _SN;});return [1,new T(function(){return E(E(_SF)[1]);}),[1,_NS,new T(function(){return B(_O(E(_SF)[2],[1,_RV,new T(function(){return B(_9V(0,(E(_Sq)[1]-1|0)+E(_SE)[1]|0,_u));})]));})]];},_SO=E(_Sp);if(!_SO[0]){return new F(function(){return _SA(_);});}else{return E(E(_SO[1])[1])==0?E(_SO[2])[0]==0?[1,_NQ,[1,_NS,new T(function(){var _SP=E(_Sy)[1];return _SP>0?B(_RJ(_S4,_SP)):E(_RI);})]]:B(_SA(_)):B(_SA(_));}}break;case 1:var _SQ=E(_Sg);if(!_SQ[0]){var _SR=E(_Sq)[1];return _SR>0?B(_NT(_SR,_u,new T(function(){return B(_1y(_Nz,_Sp));}))):B(unAppCStr("0.",new T(function(){var _SS= -_SR;if(_SS>0){var _ST=function(_SU){return _SU>1?[1,_NQ,new T(function(){return B(_ST(_SU-1|0));})]:E([1,_NQ,new T(function(){return B(_1y(_Nz,_Sp));})]);},_SV=B(_ST(_SS));}else{var _SV=B(_1y(_Nz,_Sp));}var _SW=_SV,_SX=_SW;return _SX;})));}else{var _SY=_SQ[1],_SZ=E(_Sq),_T0=_SZ[1];if(_T0<0){var _T1=new T(function(){var _T2= -_T0;if(_T2>0){var _T3=function(_T4){return _T4>1?[1,_Mn,new T(function(){return B(_T3(_T4-1|0));})]:E([1,_Mn,_Sp]);},_T5=B(_Rd(_RH,new T(function(){var _T6=E(_SY);return _T6[1]>0?E(_T6):E(_Mn);}),B(_T3(_T2)))),_T7=B(_NF(_T5[1],_T5[2]));}else{var _T8=B(_Rd(_RH,new T(function(){var _T9=E(_SY);return _T9[1]>0?E(_T9):E(_Mn);}),_Sp)),_T7=B(_NF(_T8[1],_T8[2]));}var _Ta=_T7,_Tb=_Ta;return _Tb;});return [1,new T(function(){return E(E(_T1)[1]);}),new T(function(){var _Tc=E(E(_T1)[2]);return _Tc[0]==0?[0]:[1,_NS,_Tc];})];}else{var _Td=B(_Rd(_RH,new T(function(){var _Te=E(_SY)[1];if(_Te>0){var _Tf=[0,_Te+_T0|0];}else{var _Tf=E(_SZ);}var _Tg=_Tf,_Th=_Tg;return _Th;}),_Sp)),_Ti=_Td[2],_Tj=_T0+E(_Td[1])[1]|0;if(_Tj>=0){var _Tk=B(_Rw(_Tj,new T(function(){return B(_1y(_Nz,_Ti));}))),_Tl=_Tk[2],_Tm=E(_Tk[1]);return _Tm[0]==0?[1,_NQ,new T(function(){var _Tn=E(_Tl);return _Tn[0]==0?[0]:[1,_NS,_Tn];})]:B(_O(_Tm,new T(function(){var _To=E(_Tl);return _To[0]==0?[0]:[1,_NS,_To];})));}else{return [1,_NQ,new T(function(){var _Tp=B(_1y(_Nz,_Ti));return _Tp[0]==0?[0]:[1,_NS,_Tp];})];}}}break;default:var _Tq=E(_Sq),_Tr=_Tq[1];if(_Tr>=0){if(_Tr<=7){_Sk=_RG;var _Ts=_Sp;_Sm=_Tq;_Sl=_Ts;return null;}else{_Sk=_RF;var _Ts=_Sp;_Sm=_Tq;_Sl=_Ts;return null;}}else{_Sk=_RF;var _Ts=_Sp;_Sm=_Tq;_Sl=_Ts;return null;}}})(_Sk,_Sl,_Sm);if(_Sn!=null){return _Sn;}}},_Tt=function(_Tu){return [1,_lW,new T(function(){var _Tv=B(_PL(E(E(E(E(_S6)[1])[2])[1])[1],_S7,_S8,_S9,_Sa,_Sb,_OW,new T(function(){return B(A(_S2,[_Si,_Sh]));})));return B(_Sj(_Sf,_Tv[1],_Tv[2]));})];};if(!B(A(_RD,[B(_JB(B(_NO(_S6)))),_Sh,new T(function(){return B(A(_JF,[_Si,_oa]));})]))){if(!B(A(_Se,[_Sh]))){var _Tw=B(_PL(E(E(E(E(_S6)[1])[2])[1])[1],_S7,_S8,_S9,_Sa,_Sb,_OW,_Sh));return new F(function(){return _Sj(_Sf,_Tw[1],_Tw[2]);});}else{return new F(function(){return _Tt(_);});}}else{return new F(function(){return _Tt(_);});}}else{return !B(A(_RD,[B(_JB(B(_NO(_S6)))),_Sh,new T(function(){return B(A(_JF,[_Si,_oa]));})]))?E(_RW):E(_RX);}}else{return E(_RY);}},_Tx=function(_Ty){var _Tz=u_towlower(_Ty),_TA=_Tz;return _TA>>>0>1114111?B(_a1(_TA)):_TA;},_TB=function(_TC){return new F(function(){return err(B(unAppCStr("Printf.printf: ",_TC)));});},_TD=new T(function(){return B(unCStr("bad argument"));}),_TE=new T(function(){return B(_TB(_TD));}),_TF=new T(function(){return B(unCStr("Printf.dfmt: impossible"));}),_TG=new T(function(){return B(err(_TF));}),_TH=[0,45],_TI=[1,_TH,_u],_TJ=new T(function(){return B(err(_TF));}),_TK=function(_TL,_TM){var _TN=E(_TL);return _TN[0]==0?function(_7A){return new F(function(){return _O(new T(function(){var _TO=jsShow(E(_TM)[1]),_TP=_TO;return fromJSStr(_TP);}),_7A);});}:function(_7A){return new F(function(){return _O(new T(function(){var _TQ=E(E(_TN[1])[1]);if(!_TQ){var _TR=jsRound(E(_TM)[1]),_TS=_TR,_TT=B(_nK(_TS)),_TU=_TT[1],_TV=_TT[2];if(_TV>=0){var _TW=jsShow(B(_ym(B(_ov(_TU,_TV))))),_TX=_TW,_TY=fromJSStr(_TX);}else{var _TZ=hs_uncheckedIShiftRA64(B(_o3(_TU)), -_TV),_U0=_TZ,_U1=jsShow(B(_ym(B(_nN(_U0))))),_U2=_U1,_TY=fromJSStr(_U2);}var _U3=_TY,_U4=_U3,_U5=_U4,_U6=_U5;}else{if(_TQ>=0){var _U7=B(_nE(10,_TQ)),_U8=jsRound(E(_TM)[1]*_U7),_U9=_U8,_Ua=jsShow((_U9&4294967295)/_U7),_Ub=_Ua,_Uc=fromJSStr(_Ub);}else{var _Uc=E(_nv);}var _Ud=_Uc,_Ue=_Ud,_U6=_Ue;}var _Uf=_U6;return _Uf;}),_7A);});};},_Ug=function(_Uh,_Ui){var _Uj=E(_Uh);return _Uj[0]==0?function(_7A){return new F(function(){return _O(new T(function(){var _Uk=B(_M5(E(_Ui)[1])),_Ul=jsShow(B(_oo(_Uk[1],_Uk[2]))[1]),_Um=_Ul;return fromJSStr(_Um);}),_7A);});}:function(_7A){return new F(function(){return _O(new T(function(){var _Un=E(E(_Uj[1])[1]);if(!_Un){var _Uo=jsRound(E(_Ui)[1]),_Up=_Uo,_Uq=decodeFloat(_Up),_Ur=_Uq[1],_Us=_Uq[2];if(_Us>=0){var _Ut=jsShow(B(_ym(B(_ov(B(_8F(_Ur)),_Us))))),_Uu=_Ut,_Uv=fromJSStr(_Uu);}else{var _Uw=jsShow(_Ur>> -_Us),_Ux=_Uw,_Uv=fromJSStr(_Ux);}var _Uy=_Uv,_Uz=_Uy,_UA=_Uz,_UB=_UA;}else{var _UC=B(_M5(E(_Ui)[1]));if(_Un>=0){var _UD=B(_nE(10,_Un)),_UE=jsRound(B(_oo(_UC[1],_UC[2]))[1]*_UD),_UF=_UE,_UG=jsShow((_UF&4294967295)/_UD),_UH=_UG,_UI=fromJSStr(_UH);}else{var _UI=E(_nv);}var _UJ=_UI,_UK=_UJ,_UL=_UK,_UM=_UL,_UB=_UM;}var _UN=_UB;return _UN;}),_7A);});};},_UO=function(_UP){var _UQ=u_towupper(_UP),_UR=_UQ;return _UR>>>0>1114111?B(_a1(_UR)):_UR;},_US=function(_UT){return [0,B(_UO(E(_UT)[1]))];},_UU=function(_UV,_UW,_UX){var _UY=E(_UX);switch(_UY[0]){case 3:var _UZ=_UY[1],_V0=u_iswupper(_UV),_V1=_V0;switch(B(_Tx(_UV))){case 101:var _V2=B(_S5(_Nm,_DD,_Ea,_E8,_Ef,_E4,_El,_Eh,_Ep,_RF,new T(function(){var _V3=E(_UW);return _V3[1]>=0?[1,_V3]:[0];}),_UZ));break;case 102:var _V2=B(_S5(_Nm,_DD,_Ea,_E8,_Ef,_E4,_El,_Eh,_Ep,_RG,new T(function(){var _V4=E(_UW);return _V4[1]>=0?[1,_V4]:[0];}),_UZ));break;case 103:var _V5=E(_UW),_V2=_V5[1]>=0?B(A(_Ug,[[1,_V5],_UZ,_u])):B(A(_Ug,[_2D,_UZ,_u]));break;default:var _V2=E(_TJ);}var _V6=_V2,_V7=E(_V1);if(!_V7){var _V8=E(_V6);if(!_V8[0]){return [0,_u,_u];}else{var _V9=_V8[1],_Va=_V8[2],_Vb=E(_V9),_Vc=_Vb[1],_Vd=E(_Vc);return _Vd==45?[0,_TI,_Va]:[0,_u,_V8];}}else{var _Ve=B(_1y(_US,_V6));if(!_Ve[0]){return [0,_u,_u];}else{var _Vf=_Ve[1],_Vg=_Ve[2],_Vh=E(_Vf),_Vi=_Vh[1],_Vj=E(_Vi);return _Vj==45?[0,_TI,_Vg]:[0,_u,_Ve];}}break;case 4:var _Vk=_UY[1],_Vl=u_iswupper(_UV),_Vm=_Vl;switch(B(_Tx(_UV))){case 101:var _Vn=B(_S5(_Lm,_Ci,_DL,_DI,_DQ,_DE,_DW,_DS,_E0,_RF,new T(function(){var _Vo=E(_UW);return _Vo[1]>=0?[1,_Vo]:[0];}),_Vk));break;case 102:var _Vn=B(_S5(_Lm,_Ci,_DL,_DI,_DQ,_DE,_DW,_DS,_E0,_RG,new T(function(){var _Vp=E(_UW);return _Vp[1]>=0?[1,_Vp]:[0];}),_Vk));break;case 103:var _Vq=E(_UW),_Vn=_Vq[1]>=0?B(A(_TK,[[1,_Vq],_Vk,_u])):B(A(_TK,[_2D,_Vk,_u]));break;default:var _Vn=E(_TG);}var _Vr=_Vn,_Vs=E(_Vm);if(!_Vs){var _Vt=E(_Vr);if(!_Vt[0]){return [0,_u,_u];}else{var _Vu=_Vt[1],_Vv=_Vt[2],_Vw=E(_Vu),_Vx=_Vw[1],_Vy=E(_Vx);return _Vy==45?[0,_TI,_Vv]:[0,_u,_Vt];}}else{var _Vz=B(_1y(_US,_Vr));if(!_Vz[0]){return [0,_u,_u];}else{var _VA=_Vz[1],_VB=_Vz[2],_VC=E(_VA),_VD=_VC[1],_VE=E(_VD);return _VE==45?[0,_TI,_VB]:[0,_u,_Vz];}}break;default:return E(_TE);}},_VF=[0,0],_VG=function(_VH){return new F(function(){return _19(0,_VH,_u);});},_VI=[0,48],_VJ=function(_VK,_VL){var _VM=_VK-B(_uJ(_VL,0))|0;if(_VM>0){var _VN=function(_VO){return _VO>1?[1,_VI,new T(function(){return B(_VN(_VO-1|0));})]:E([1,_VI,_VL]);};return new F(function(){return _VN(_VM);});}else{return E(_VL);}},_VP=[0,0],_VQ=[0,-2147483648],_VR=function(_VS,_VT){while(1){var _VU=(function(_VV,_VW){var _VX=E(_VW);switch(_VX[0]){case 0:_VS=_VP;_VT=[2,_VQ,new T(function(){return B(_8F(E(_VX[1])[1]));})];return null;case 2:var _VY=_VX[2];return !B(_Y(_VY,_VF))?[0,_u,new T(function(){return B(_VJ(E(_VV)[1],B(_VG(_VY))));})]:[0,_TI,new T(function(){return B(_VJ(E(_VV)[1],B(_19(0,B(_8z(_VY)),_u))));})];default:return E(_TE);}})(_VS,_VT);if(_VU!=null){return _VU;}}},_VZ=[1,_jU,_u],_W0=function(_W1){return new F(function(){return err(B(unAppCStr("Char.digitToInt: not a digit ",new T(function(){var _W2=E(_W1);return _W2==39?E(_jW):[1,_jU,new T(function(){return B(_jE(_W2,_VZ));})];}))));});},_W3=function(_W4){var _W5=function(_W6){var _W7=function(_W8){if(_W4<65){return new F(function(){return _W0(_W4);});}else{if(_W4>70){return new F(function(){return _W0(_W4);});}else{return (_W4-65|0)+10|0;}}};if(_W4<97){return new F(function(){return _W7(_);});}else{if(_W4>102){return new F(function(){return _W7(_);});}else{return (_W4-97|0)+10|0;}}};if(_W4<48){return new F(function(){return _W5(_);});}else{if(_W4>57){return new F(function(){return _W5(_);});}else{return _W4-48|0;}}},_W9=function(_Wa,_Wb){while(1){var _Wc=(function(_Wd,_We){var _Wf=E(_We);if(!_Wf[0]){return [0,_Wd,_u];}else{var _Wg=E(_Wf[1])[1];if(_Wg<48){return [0,_Wd,_Wf];}else{if(_Wg>57){return [0,_Wd,_Wf];}else{_Wa=new T(function(){return [0,(imul(E(_Wd)[1],10)|0)+B(_W3(_Wg))|0];});_Wb=_Wf[2];return null;}}}})(_Wa,_Wb);if(_Wc!=null){return _Wc;}}},_Wh=new T(function(){return B(unCStr("argument list ended prematurely"));}),_Wi=new T(function(){return B(_TB(_Wh));}),_Wj=[0,-1],_Wk=function(_Wl){return [0,E(_Wl)[1]];},_Wm=function(_Wn){var _Wo=E(_Wn);switch(_Wo[0]){case 0:return new F(function(){return _Wk(_Wo[1]);});break;case 2:return new F(function(){return _Iu(_Wo[2]);});break;default:return E(_TE);}},_Wp=function(_Wq,_Wr,_Ws,_Wt,_Wu){while(1){var _Wv=(function(_Ww,_Wx,_Wy,_Wz,_WA){var _WB=E(_Wz);if(!_WB[0]){return [0,_VP,_Wj,_Ww,_Wx,_Wy,_u,_WA];}else{var _WC=_WB[2],_WD=E(E(_WB[1])[1]);switch(_WD){case 42:var _WE=new T(function(){var _WF=E(_WA);return _WF[0]==0?E(_Wi):[0,_WF[2],new T(function(){return B(_Wm(_WF[1]));})];}),_WG=new T(function(){var _WH=E(_WC);if(!_WH[0]){var _WI=[0,_Wj,_u,new T(function(){return E(E(_WE)[1]);})];}else{if(E(E(_WH[1])[1])==46){var _WJ=E(_WH[2]);if(!_WJ[0]){var _WK=B(_W9(_VP,_u)),_WL=[0,_WK[1],_WK[2],new T(function(){return E(E(_WE)[1]);})];}else{if(E(E(_WJ[1])[1])==42){var _WM=new T(function(){var _WN=E(E(_WE)[1]);return _WN[0]==0?E(_Wi):[0,_WN[2],new T(function(){return B(_Wm(_WN[1]));})];}),_WO=[0,new T(function(){return E(E(_WM)[2]);}),_WJ[2],new T(function(){return E(E(_WM)[1]);})];}else{var _WP=B(_W9(_VP,_WJ)),_WO=[0,_WP[1],_WP[2],new T(function(){return E(E(_WE)[1]);})];}var _WQ=_WO,_WL=_WQ;}var _WR=_WL;}else{var _WR=[0,_Wj,_WH,new T(function(){return E(E(_WE)[1]);})];}var _WS=_WR,_WI=_WS;}return _WI;});return [0,new T(function(){return E(E(_WE)[2]);}),new T(function(){return E(E(_WG)[1]);}),_Ww,_Wx,_Wy,new T(function(){return E(E(_WG)[2]);}),new T(function(){return E(E(_WG)[3]);})];case 43:var _WT=_Ww,_WU=_Wx;_Ws=_n;_Wt=_WC;var _WV=_WA;_Wq=_WT;_Wr=_WU;_Wu=_WV;return null;case 45:_Wq=_n;var _WU=_Wx,_WW=_Wy;_Wt=_WC;var _WV=_WA;_Wr=_WU;_Ws=_WW;_Wu=_WV;return null;case 46:var _WX=new T(function(){var _WY=E(_WC);if(!_WY[0]){var _WZ=B(_W9(_VP,_u)),_X0=[0,_WZ[1],_WZ[2],_WA];}else{if(E(E(_WY[1])[1])==42){var _X1=new T(function(){var _X2=E(_WA);return _X2[0]==0?E(_Wi):[0,_X2[2],new T(function(){return B(_Wm(_X2[1]));})];}),_X3=[0,new T(function(){return E(E(_X1)[2]);}),_WY[2],new T(function(){return E(E(_X1)[1]);})];}else{var _X4=B(_W9(_VP,_WY)),_X3=[0,_X4[1],_X4[2],_WA];}var _X5=_X3,_X0=_X5;}return _X0;});return [0,_VP,new T(function(){return E(E(_WX)[1]);}),_Ww,_Wx,_Wy,new T(function(){return E(E(_WX)[2]);}),new T(function(){return E(E(_WX)[3]);})];case 48:var _WT=_Ww;_Wr=_n;var _WW=_Wy;_Wt=_WC;var _WV=_WA;_Wq=_WT;_Ws=_WW;_Wu=_WV;return null;default:if(_WD<48){return [0,_VP,_Wj,_Ww,_Wx,_Wy,_WB,_WA];}else{if(_WD>57){return [0,_VP,_Wj,_Ww,_Wx,_Wy,_WB,_WA];}else{var _X6=new T(function(){var _X7=B(_W9(_VP,_WB));return [0,_X7[1],_X7[2]];}),_X8=new T(function(){var _X9=E(E(_X6)[2]);if(!_X9[0]){var _Xa=[0,_Wj,_u,_WA];}else{if(E(E(_X9[1])[1])==46){var _Xb=E(_X9[2]);if(!_Xb[0]){var _Xc=B(_W9(_VP,_u)),_Xd=[0,_Xc[1],_Xc[2],_WA];}else{if(E(E(_Xb[1])[1])==42){var _Xe=new T(function(){var _Xf=E(_WA);return _Xf[0]==0?E(_Wi):[0,_Xf[2],new T(function(){return B(_Wm(_Xf[1]));})];}),_Xg=[0,new T(function(){return E(E(_Xe)[2]);}),_Xb[2],new T(function(){return E(E(_Xe)[1]);})];}else{var _Xh=B(_W9(_VP,_Xb)),_Xg=[0,_Xh[1],_Xh[2],_WA];}var _Xi=_Xg,_Xd=_Xi;}var _Xj=_Xd;}else{var _Xj=[0,_Wj,_X9,_WA];}var _Xk=_Xj,_Xa=_Xk;}var _Xl=_Xa;return _Xl;});return [0,new T(function(){return E(E(_X6)[1]);}),new T(function(){return E(E(_X8)[1]);}),_Ww,_Wx,_Wy,new T(function(){return E(E(_X8)[2]);}),new T(function(){return E(E(_X8)[3]);})];}}}}})(_Wq,_Wr,_Ws,_Wt,_Wu);if(_Wv!=null){return _Wv;}}},_Xm=new T(function(){return B(unCStr("formatting string ended prematurely"));}),_Xn=new T(function(){return B(_TB(_Xm));}),_Xo=function(_Xp,_Xq){if(!B(_Y(_Xq,_Xp))){if(!B(_og(_Xp,_VF))){var _Xr=B(_Kr(_Xq,_Xp));return new F(function(){return _O(B(_Xo(_Xp,_Xr[1])),[1,new T(function(){return [0,B(_Nv(B(_a3(_Xr[2]))))];}),_u]);});}else{return E(_Hg);}}else{return [1,new T(function(){return [0,B(_Nv(B(_a3(_Xq))))];}),_u];}},_Xs=[0,2],_Xt=function(_Xu,_Xv,_Xw){var _Xx=E(_Xw);switch(_Xx[0]){case 0:return new F(function(){return _Xo(_Xu,B(_8F(E(_Xx[1])[1])));});break;case 2:var _Xy=_Xx[2],_Xz=E(_Xv)[1];if(!B(_Y(_Xy,_VF))){return new F(function(){return _VJ(_Xz,B(_Xo(_Xu,_Xy)));});}else{return new F(function(){return _VJ(_Xz,B(_Xo(_Xu,B(_8p(B(_8z(B(_8H(_Xs,_Xx[1])))),_Xy)))));});}break;default:return E(_TE);}},_XA=[0,37],_XB=[0,16],_XC=[0,10],_XD=[0,8],_XE=[0,43],_XF=[1,_XE,_u],_XG=[0,32],_XH=function(_XI){return new F(function(){return _TB(new T(function(){return B(unAppCStr("bad formatting char ",[1,_XI,_u]));}));});},_XJ=function(_XK,_XL){var _XM=E(_XK);if(!_XM){return [0];}else{var _XN=E(_XL);return _XN[0]==0?[0]:[1,_XN[1],new T(function(){return B(_XJ(_XM-1|0,_XN[2]));})];}},_XO=function(_XP,_XQ){var _XR=E(_XP);if(!_XR[0]){return E(_XQ)[0]==0?[0]:E(_Xn);}else{var _XS=_XR[2],_XT=E(_XR[1]);if(E(_XT[1])==37){var _XU=function(_XV){var _XW=E(_XQ);if(!_XW[0]){return E(_Wi);}else{var _XX=B(_Wp(_r,_r,_r,_XS,_XW)),_XY=_XX[2],_XZ=_XX[4],_Y0=E(_XX[6]);if(!_Y0[0]){return E(_Xn);}else{var _Y1=_Y0[2],_Y2=E(_XX[7]);if(!_Y2[0]){return E(_Wi);}else{var _Y3=_Y2[1],_Y4=_Y2[2],_Y5=E(_Y0[1]),_Y6=function(_Y7,_Y8){var _Y9=new T(function(){var _Ya=B(_uJ(_Y8,0)),_Yb=B(_uJ(_Y7,0)),_Yc=E(_XX[1])[1];if((_Ya+_Yb|0)>=_Yc){var _Yd=[0];}else{var _Ye=_Yc-(_Ya+_Yb|0)|0;if(_Ye>0){if(_Ye<0){var _Yf=[0];}else{var _Yg=new T(function(){return [1,new T(function(){return !E(_XZ)?E(_XG):E(_VI);}),_Yg];}),_Yf=B(_XJ(_Ye,_Yg));}var _Yh=_Yf,_Yi=_Yh;}else{var _Yi=[0];}var _Yj=_Yi,_Yk=_Yj,_Yl=_Yk,_Yd=_Yl;}var _Ym=_Yd,_Yn=_Ym,_Yo=_Yn,_Yp=_Yo,_Yq=_Yp;return _Yq;});return !E(_XX[3])?!E(_XZ)?B(_O(_Y9,new T(function(){return B(_O(_Y7,_Y8));}))):B(_O(_Y7,new T(function(){return B(_O(_Y9,_Y8));}))):B(_O(_Y7,new T(function(){return B(_O(_Y8,_Y9));})));},_Yr=function(_Ys,_Yt){var _Yu=E(_Ys);return _Yu[0]==0?!E(_XX[5])?B(_Y6(_u,_Yt)):B(_Y6(_XF,_Yt)):B(_Y6(_Yu,_Yt));};switch(E(_Y5[1])){case 69:var _Yv=B(_UU(69,_XY,_Y3));return new F(function(){return _O(B(_Yr(_Yv[1],_Yv[2])),new T(function(){return B(_XO(_Y1,_Y4));}));});break;case 71:var _Yw=B(_UU(71,_XY,_Y3));return new F(function(){return _O(B(_Yr(_Yw[1],_Yw[2])),new T(function(){return B(_XO(_Y1,_Y4));}));});break;case 88:return new F(function(){return _O(B(_Y6(_u,new T(function(){return B(_1y(_US,B(_Xt(_XB,_XY,_Y3))));}))),new T(function(){return B(_XO(_Y1,_Y4));}));});break;case 99:return new F(function(){return _O(B(_Y6(_u,[1,new T(function(){var _Yx=E(_Y3);switch(_Yx[0]){case 0:var _Yy=E(_Yx[1])[1];if(_Yy>>>0>1114111){var _Yz=B(_a1(_Yy));}else{var _Yz=[0,_Yy];}var _YA=_Yz,_YB=_YA,_YC=_YB,_YD=_YC,_YE=_YD;break;case 2:var _YF=B(_a3(_Yx[2]));if(_YF>>>0>1114111){var _YG=B(_a1(_YF));}else{var _YG=[0,_YF];}var _YH=_YG,_YI=_YH,_YJ=_YI,_YE=_YJ;break;default:var _YE=E(_TE);}return _YE;}),_u])),new T(function(){return B(_XO(_Y1,_Y4));}));});break;case 100:var _YK=B(_VR(_XY,_Y3));return new F(function(){return _O(B(_Yr(_YK[1],_YK[2])),new T(function(){return B(_XO(_Y1,_Y4));}));});break;case 101:var _YL=B(_UU(101,_XY,_Y3));return new F(function(){return _O(B(_Yr(_YL[1],_YL[2])),new T(function(){return B(_XO(_Y1,_Y4));}));});break;case 102:var _YM=B(_UU(102,_XY,_Y3));return new F(function(){return _O(B(_Yr(_YM[1],_YM[2])),new T(function(){return B(_XO(_Y1,_Y4));}));});break;case 103:var _YN=B(_UU(103,_XY,_Y3));return new F(function(){return _O(B(_Yr(_YN[1],_YN[2])),new T(function(){return B(_XO(_Y1,_Y4));}));});break;case 105:var _YO=B(_VR(_XY,_Y3));return new F(function(){return _O(B(_Yr(_YO[1],_YO[2])),new T(function(){return B(_XO(_Y1,_Y4));}));});break;case 111:return new F(function(){return _O(B(_Y6(_u,new T(function(){return B(_Xt(_XD,_XY,_Y3));}))),new T(function(){return B(_XO(_Y1,_Y4));}));});break;case 115:return new F(function(){return _O(B(_Y6(_u,new T(function(){var _YP=E(_Y3);if(_YP[0]==1){var _YQ=_YP[1],_YR=E(_XY)[1];if(_YR<0){var _YS=E(_YQ);}else{var _YS=_YR>0?B(_XJ(_YR,_YQ)):[0];}var _YT=_YS,_YU=_YT,_YV=_YU;}else{var _YV=E(_TE);}return _YV;}))),new T(function(){return B(_XO(_Y1,_Y4));}));});break;case 117:return new F(function(){return _O(B(_Y6(_u,new T(function(){return B(_Xt(_XC,_XY,_Y3));}))),new T(function(){return B(_XO(_Y1,_Y4));}));});break;case 120:return new F(function(){return _O(B(_Y6(_u,new T(function(){return B(_Xt(_XB,_XY,_Y3));}))),new T(function(){return B(_XO(_Y1,_Y4));}));});break;default:return new F(function(){return _XH(_Y5);});}}}}},_YW=E(_XS);if(!_YW[0]){return new F(function(){return _XU(_);});}else{if(E(E(_YW[1])[1])==37){return [1,_XA,new T(function(){return B(_XO(_YW[2],_XQ));})];}else{return new F(function(){return _XU(_);});}}}else{return [1,_XT,new T(function(){return B(_XO(_XS,_XQ));})];}}},_yl=function(_YX,_){var _YY=jsFind(toJSStr(E(_0))),_YZ=_YY,_Z0=E(_YZ);if(!_Z0[0]){return new F(function(){return _sc(_to,_);});}else{var _Z1=jsSet(E(_Z0[1])[1],toJSStr(E(_uE)),toJSStr(B(_O(_B1,new T(function(){return B(unAppCStr("<tbody>",new T(function(){var _Z2=function(_Z3){var _Z4=E(_Z3);if(!_Z4[0]){return [0];}else{var _Z5=E(_Z4[1]),_Z6=function(_Z7){var _Z8=E(_Z7);return _Z8[0]==0?E(new T(function(){return B(_Z2(_Z4[2]));})):[1,_Z8[1],new T(function(){return B(_Z6(_Z8[2]));})];};return new F(function(){return _Z6(B(_XO(_B0,new T(function(){return B(_B2([1,[1,new T(function(){return B(_1y(_tg,_Z5[2]));})],[1,[1,new T(function(){return B(_1y(_tg,_Z5[1]));})],_u]],_u));}))));});}};return B(_O(B(_Z2(B(_1y(function(_Z9){var _Za=E(_Z9),_Zb=E(_Za[2])[1],_Zc=B(_ti(E(_Za[1])[1],new T(function(){return E(E(_YX)[6]);})));if(!_Zc[0]){return [0,_Zb,_u];}else{var _Zd=E(_Zc[1]);return _Zd[0]==0?[0,_Zb,_u]:[0,_Zb,_Zd[1]];}},_AG)))),_AZ));})));})))));return [0,_7b,_YX];}},_Ze=new T(function(){return B(unCStr("none"));}),_Zf=function(_Zg,_Zh,_){var _Zi=B(_h3(_)),_Zj=_Zi,_Zk=[0,_h7,_h7,_h7,_Zj,_r,_45,_45,_h7,_h8,_h8],_Zl=B(A(_y7,[_Zk,_])),_Zm=_Zl,_Zn=B(_yl(_Zk,_)),_Zo=_Zn,_Zp=E(_sE),_Zq=jsFind(toJSStr(_Zp)),_Zr=_Zq,_Zs=E(_Zr);if(!_Zs[0]){return new F(function(){return _sG(_Zp);});}else{var _Zt=B(A(_qD,[_sB,_Zs[1],_sD,_Ze,new T(function(){return E(E(_Zo)[2]);}),_])),_Zu=_Zt,_Zv=E(_sR),_Zw=jsFind(toJSStr(_Zv)),_Zx=_Zw,_Zy=E(_Zx);return _Zy[0]==0?B(_sG(_Zv)):B(A(_qD,[_sB,_Zy[1],_sD,_Ze,new T(function(){return E(E(_Zu)[2]);}),_]));}},_Zz=new T(function(){return [0,_te,_Zf,_tc];}),_ZA=new T(function(){return B(unCStr("\u521d\u671f\u5316"));}),_ZB=new T(function(){return B(unCStr("\u521d\u671f\u5316<br>\u5b9f\u7e3e\u3092\u9664\u304f\u5168\u3066\u306e\u30c7\u30fc\u30bf\u304c\u521d\u671f\u5316\u3055\u308c\u307e\u3059"));}),_ZC=new T(function(){return B(unCStr("fa-history"));}),_ZD=[0,_ZC,_ZB,_ZA],_ZE=function(_ZF){return E(_yZ);},_ZG=function(_ZH,_ZI,_){var _ZJ=B(_h3(_)),_ZK=_ZJ,_ZL=[0,_h7,_h7,_h7,_ZK,_r,new T(function(){return E(E(_ZI)[6]);}),_45,_h7,_h8,_h8],_ZM=B(A(_y7,[_ZL,_])),_ZN=_ZM,_ZO=B(_yl(_ZL,_)),_ZP=_ZO,_ZQ=E(_sE),_ZR=jsFind(toJSStr(_ZQ)),_ZS=_ZR,_ZT=E(_ZS);if(!_ZT[0]){return new F(function(){return _sG(_ZQ);});}else{var _ZU=B(A(_qD,[_sB,_ZT[1],_sD,_Ze,new T(function(){return E(E(_ZP)[2]);}),_])),_ZV=_ZU,_ZW=E(_sR),_ZX=jsFind(toJSStr(_ZW)),_ZY=_ZX,_ZZ=E(_ZY);return _ZZ[0]==0?B(_sG(_ZW)):B(A(_qD,[_sB,_ZZ[1],_sD,_Ze,new T(function(){return E(E(_ZV)[2]);}),_]));}},_100=new T(function(){return [0,_ZE,_ZG,_ZD];}),_101=new T(function(){return B(_G4(-1,-2));}),_102=new T(function(){var _103=E(_101);return _103[0]==0?[0]:[1,[0,_103[1],_t8],new T(function(){var _104=E(_103[2]);return _104[0]==0?[0]:[1,[0,_104[1],_sY],new T(function(){var _105=E(_104[2]);return _105[0]==0?[0]:[1,[0,_105[1],_qq],new T(function(){var _106=E(_105[2]);return _106[0]==0?[0]:[1,[0,_106[1],_qy],new T(function(){var _107=E(_106[2]);return _107[0]==0?[0]:[1,[0,_107[1],_q5],new T(function(){var _108=E(_107[2]);return _108[0]==0?[0]:[1,[0,_108[1],_qd],new T(function(){var _109=E(_108[2]);return _109[0]==0?[0]:[1,[0,_109[1],_100],new T(function(){var _10a=E(_109[2]);return _10a[0]==0?[0]:[1,[0,_10a[1],_Zz],_u];})];})];})];})];})];})];})];}),_zz=new T(function(){var _10b=B(_ne(1,2147483647));return _10b[0]==0?E(_102):[1,[0,_10b[1],_pS],new T(function(){var _10c=E(_10b[2]);return _10c[0]==0?E(_102):[1,[0,_10c[1],_pH],new T(function(){var _10d=E(_10c[2]);return _10d[0]==0?E(_102):[1,[0,_10d[1],_pw],new T(function(){var _10e=E(_10d[2]);return _10e[0]==0?E(_102):[1,[0,_10e[1],_pl],new T(function(){var _10f=E(_10e[2]);return _10f[0]==0?E(_102):[1,[0,_10f[1],_pa],new T(function(){var _10g=E(_10f[2]);return _10g[0]==0?E(_102):[1,[0,_10g[1],_oZ],new T(function(){var _10h=E(_10g[2]);return _10h[0]==0?E(_102):[1,[0,_10h[1],_oN],_102];})];})];})];})];})];})];}),_10i=new T(function(){var _10j=B(_n8(_zz));return [0,_10j[1],_10j[2]];}),_10k=new T(function(){return E(E(_10i)[1]);}),_10l=function(_10m,_10n,_10o,_10p){var _10q=E(_10p);switch(_10q[0]){case 0:var _10r=_10q[1],_10s=_10q[2],_10t=_10q[3],_10u=_10q[4],_10v=_10s>>>0;if(((_10n>>>0&((_10v-1>>>0^4294967295)>>>0^_10v)>>>0)>>>0&4294967295)==_10r){return (_10n>>>0&_10v)>>>0==0?[0,_10r,_10s,E(B(_10l(_10m,_10n,_10o,_10t))),E(_10u)]:[0,_10r,_10s,E(_10t),E(B(_10l(_10m,_10n,_10o,_10u)))];}else{var _10w=(_10n>>>0^_10r>>>0)>>>0,_10x=(_10w|_10w>>>1)>>>0,_10y=(_10x|_10x>>>2)>>>0,_10z=(_10y|_10y>>>4)>>>0,_10A=(_10z|_10z>>>8)>>>0,_10B=(_10A|_10A>>>16)>>>0,_10C=(_10B^_10B>>>1)>>>0&4294967295,_10D=_10C>>>0;return (_10n>>>0&_10D)>>>0==0?[0,(_10n>>>0&((_10D-1>>>0^4294967295)>>>0^_10D)>>>0)>>>0&4294967295,_10C,E([1,_10n,_10o]),E(_10q)]:[0,(_10n>>>0&((_10D-1>>>0^4294967295)>>>0^_10D)>>>0)>>>0&4294967295,_10C,E(_10q),E([1,_10n,_10o])];}break;case 1:var _10E=_10q[1];if(_10n!=_10E){var _10F=(_10n>>>0^_10E>>>0)>>>0,_10G=(_10F|_10F>>>1)>>>0,_10H=(_10G|_10G>>>2)>>>0,_10I=(_10H|_10H>>>4)>>>0,_10J=(_10I|_10I>>>8)>>>0,_10K=(_10J|_10J>>>16)>>>0,_10L=(_10K^_10K>>>1)>>>0&4294967295,_10M=_10L>>>0;return (_10n>>>0&_10M)>>>0==0?[0,(_10n>>>0&((_10M-1>>>0^4294967295)>>>0^_10M)>>>0)>>>0&4294967295,_10L,E([1,_10n,_10o]),E(_10q)]:[0,(_10n>>>0&((_10M-1>>>0^4294967295)>>>0^_10M)>>>0)>>>0&4294967295,_10L,E(_10q),E([1,_10n,_10o])];}else{return [1,_10n,new T(function(){return B(A(_10m,[[0,_10n],_10o,_10q[2]]));})];}break;default:return [1,_10n,_10o];}},_10N=new T(function(){return [0,"click"];}),_10O=new T(function(){return B(unCStr("\u300d\u3092\u8cfc\u5165\u3057\u307e\u3057\u305f"));}),_10P=new T(function(){return B(_5S("main.hs:(440,1)-(457,24)|function btnEvents"));}),_10Q=[0,12300],_10R=function(_10S,_10T,_10U){return new F(function(){return _Ik(_10T,_10U);});},_10V=new T(function(){return B(unCStr("-btn"));}),_10W=function(_10X,_10Y,_){var _10Z=E(_10Y);if(!_10Z[0]){return E(_10P);}else{var _110=E(_10Z[1]),_111=_110[1],_112=function(_,_113){var _114=E(_113);if(!_114[0]){return _7b;}else{var _115=E(_10N)[1],_116=jsSetCB(E(_114[1])[1],_115,function(_117,_118,_){var _119=E(_10X)[1],_11a=rMV(_119),_11b=_11a,_11c=E(new T(function(){return B(_mV(_zy,_111));})),_11d=B(A(_11c[2],[_110,new T(function(){var _11e=E(_11b),_11f=new T(function(){return B(_10l(_10R,_111,_zt,_11e[7]));});return [0,new T(function(){return [0,E(_11e[1])[1]-B(_ym(B(A(_11c[1],[new T(function(){return [0,B(_mV(_11f,_111))[1]-1|0];})]))))];}),_11e[2],_11e[3],_11e[4],_11e[5],_11e[6],_11f,_11e[8],_11e[9],_11e[10]];}),_])),_11g=_11d,_11h=B(_vv(_y8,[1,_10Q,new T(function(){return B(_O(E(_11c[3])[3],_10O));})],_)),_11i=_11h,_11j=new T(function(){return E(E(_11g)[2]);}),_11k=B(A(_y7,[_11j,_])),_11l=_11k,_11m=B(_yl(_11j,_)),_11n=_11m,_=wMV(_119,new T(function(){return E(E(_11n)[2]);})),_11o=rMV(_119),_11p=_11o,_11q=E(_11p),_11r=jsLog(toJSStr(B(A(_mo,[0,_11q[1],_11q[2],_11q[3],_11q[4],_11q[5],_11q[6],_11q[7],_11q[8],_11q[9],_11q[10],_u]))));return _7b;}),_11s=_116,_11t=function(_11u,_11v,_){var _11w=E(_11v);if(!_11w[0]){return E(_10P);}else{var _11x=E(_11w[1]),_11y=_11x[1],_11z=function(_,_11A){var _11B=E(_11A);if(!_11B[0]){return _7b;}else{var _11C=jsSetCB(E(_11B[1])[1],_115,function(_11D,_11E,_){var _11F=E(_11u)[1],_11G=rMV(_11F),_11H=_11G,_11I=E(new T(function(){return B(_mV(_zy,_11y));})),_11J=B(A(_11I[2],[_11x,new T(function(){var _11K=E(_11H),_11L=new T(function(){return B(_10l(_10R,_11y,_zt,_11K[7]));});return [0,new T(function(){return [0,E(_11K[1])[1]-B(_ym(B(A(_11I[1],[new T(function(){return [0,B(_mV(_11L,_11y))[1]-1|0];})]))))];}),_11K[2],_11K[3],_11K[4],_11K[5],_11K[6],_11L,_11K[8],_11K[9],_11K[10]];}),_])),_11M=_11J,_11N=B(_vv(_y8,[1,_10Q,new T(function(){return B(_O(E(_11I[3])[3],_10O));})],_)),_11O=_11N,_11P=new T(function(){return E(E(_11M)[2]);}),_11Q=B(A(_y7,[_11P,_])),_11R=_11Q,_11S=B(_yl(_11P,_)),_11T=_11S,_=wMV(_11F,new T(function(){return E(E(_11T)[2]);})),_11U=rMV(_11F),_11V=_11U,_11W=E(_11V),_11X=jsLog(toJSStr(B(A(_mo,[0,_11W[1],_11W[2],_11W[3],_11W[4],_11W[5],_11W[6],_11W[7],_11W[8],_11W[9],_11W[10],_u]))));return _7b;}),_11Y=_11C;return new F(function(){return _11t(_11u,_11w[2],_);});}};if(_11y<=0){var _11Z=jsFind(toJSStr(B(unAppCStr("item-sp-",new T(function(){if(_11y<0){var _120=B(_O(B(_9V(0, -_11y,_u)),_10V));}else{var _120=B(_O(B(_9V(0,_11y,_u)),_10V));}var _121=_120;return _121;}))))),_122=_11Z;return new F(function(){return _11z(_,_122);});}else{var _123=jsFind(toJSStr(B(unAppCStr("item-",new T(function(){return B(_O(B(_9V(0,_11y,_u)),_10V));}))))),_124=_123;return new F(function(){return _11z(_,_124);});}}};return new F(function(){return _11t(_10X,_10Z[2],_);});}};if(_111<=0){var _125=jsFind(toJSStr(B(unAppCStr("item-sp-",new T(function(){if(_111<0){var _126=B(_O(B(_9V(0, -_111,_u)),_10V));}else{var _126=B(_O(B(_9V(0,_111,_u)),_10V));}var _127=_126;return _127;}))))),_128=_125;return new F(function(){return _112(_,_128);});}else{var _129=jsFind(toJSStr(B(unAppCStr("item-",new T(function(){return B(_O(B(_9V(0,_111,_u)),_10V));}))))),_12a=_129;return new F(function(){return _112(_,_12a);});}}},_12b=function(_){return _7b;},_12c=new T(function(){return E(E(_10i)[2]);}),_12d=function(_12e){return _12e>0;},_12f=new T(function(){return B(_ut("(function(x) {return x === null;})"));}),_12g=new T(function(){return B(unCStr("No such value"));}),_12h=[0,_12g],_12i=new T(function(){return B(unCStr("Invalid JSON!"));}),_12j=[0,_12i],_12k=new T(function(){return [0,"(function(k) {return localStorage.getItem(k);})"];}),_12l=function(_12m,_12n,_){var _12o=B(A(_ut,[E(_12k)[1],E(toJSStr(E(_12n))),_])),_12p=_12o;return new T(function(){if(!B(_hc(function(_){var _=0,_12q=B(A(_12f,[E(_12p),_])),_12r=_12q;return new T(function(){return B(_12d(_12r));});}))){var _12s=String(_12p),_12t=_12s,_12u=jsParseJSON(_12t),_12v=_12u,_12w=E(_12v),_12x=_12w[0]==0?E(_12j):B(A(_2T,[_12m,_12w[1]]));}else{var _12x=E(_12h);}return _12x;});},_12y=[0,10],_12z=[1,_12y,_u],_12A=function(_12B,_12C,_){var _12D=jsWriteHandle(E(_12B)[1],toJSStr(E(_12C)));return _7b;},_12E=function(_12F,_12G,_){var _12H=E(_12F),_12I=jsWriteHandle(_12H[1],toJSStr(E(_12G)));return new F(function(){return _12A(_12H,_12z,_);});},_12J=new T(function(){return B(unCStr("btn btn-default btn-buy"));}),_12K=new T(function(){return B(unCStr("item-list"));}),_12L=new T(function(){return B(unCStr("count"));}),_12M=new T(function(){return B(unCStr("tip"));}),_12N=new T(function(){return B(unCStr("list-group-item tooltips"));}),_12O=new T(function(){return B(unCStr("fa fa-plus-circle"));}),_12P=new T(function(){return B(unCStr(" loves"));}),_12Q=[0,105],_12R=[1,_12Q,_u],_12S=function(_12T,_12U,_12V,_){var _12W=jsCreateElem(toJSStr(_12R)),_12X=_12W,_12Y=jsAppendChild(_12X,E(_12V)[1]),_12Z=[0,_12X],_130=B(A(_12T,[_12U,_12Z,_])),_131=_130;return _12Z;},_132=new T(function(){return B(unCStr("li"));}),_133=function(_134,_135,_136,_){var _137=jsCreateElem(toJSStr(E(_132))),_138=_137,_139=jsAppendChild(_138,E(_136)[1]),_13a=[0,_138],_13b=B(A(_134,[_135,_13a,_])),_13c=_13b;return _13a;},_13d=[0,48],_13e=[1,_13d,_u],_13f=new T(function(){return B(unCStr("id"));}),_13g=new T(function(){return B(unCStr("-icon"));}),_13h=new T(function(){return B(unCStr("-num"));}),_13i=new T(function(){return B(unCStr("-box"));}),_13j=new T(function(){return B(unCStr("-cost"));}),_13k=function(_13l,_13m,_13n,_13o){var _13p=new T(function(){return B(unAppCStr("item-",new T(function(){var _13q=E(_13l)[1];return _13q<=0?B(unAppCStr("sp-",new T(function(){if(_13q<0){var _13r=B(_9V(0, -_13q,_u));}else{var _13r=B(_9V(0,_13q,_u));}var _13s=_13r;return _13s;}))):B(_9V(0,_13q,_u));})));});return function(_13t,_){var _13u=B(_133(_u8,function(_13v,_){var _13w=B(_tY(_u8,function(_13x,_){var _13y=B(A(_tv,[_7P,_13x,_tO,_13n,_])),_13z=_13y;return _13x;},_13v,_)),_13A=_13w,_13B=B(A(_tp,[_7P,_13A,_tW,_12M,_])),_13C=_13B,_13D=B(_tY(_u8,function(_13E,_){var _13F=B(_tY(_u8,function(_13G,_){var _13H=B(_12S(_tG,_u,_13G,_)),_13I=_13H,_13J=B(A(_tp,[_7P,_13I,_tW,new T(function(){return B(unAppCStr("fa ",_13m));}),_])),_13K=_13J,_13L=B(_tG(_ur,_13G,_)),_13M=_13L;return _13G;},_13E,_)),_13N=_13F,_13O=B(A(_tp,[_7P,_13N,_13f,new T(function(){return B(_O(_13p,_13g));}),_])),_13P=_13O,_13Q=B(_tY(_tG,_u,_13E,_)),_13R=_13Q,_13S=B(A(_tp,[_7P,_13R,_13f,new T(function(){return B(_O(_13p,_13h));}),_])),_13T=_13S;return _13E;},_13v,_)),_13U=_13D,_13V=B(A(_tp,[_7P,_13U,_tW,_12L,_])),_13W=_13V,_13X=B(_tY(_tG,_13o,_13v,_)),_13Y=_13X,_13Z=B(A(_tp,[_7P,_13Y,_13f,new T(function(){return B(_O(_13p,_13i));}),_])),_140=_13Z,_141=B(A(_tp,[_7P,_13Y,_tW,_12K,_])),_142=_141,_143=B(_w6(_u8,function(_144,_){var _145=B(_12S(_tG,_u,_144,_)),_146=_145,_147=B(A(_tp,[_7P,_146,_tW,_12O,_])),_148=_147,_149=B(_tG(_ur,_144,_)),_14a=_149,_14b=B(_tY(_tG,_13e,_144,_)),_14c=_14b,_14d=B(A(_tp,[_7P,_14c,_13f,new T(function(){return B(_O(_13p,_13j));}),_])),_14e=_14d,_14f=B(_tG(_12P,_144,_)),_14g=_14f;return _144;},_13v,_)),_14h=_143,_14i=B(A(_tp,[_7P,_14h,_tC,_tB,_])),_14j=_14i,_14k=B(A(_tp,[_7P,_14h,_13f,new T(function(){return B(_O(_13p,_10V));}),_])),_14l=_14k,_14m=B(A(_tp,[_7P,_14h,_tW,_12J,_])),_14n=_14m;return _13v;},_13t,_)),_14o=_13u,_14p=B(A(_tp,[_7P,_14o,_wr,_13p,_])),_14q=_14p,_14r=B(A(_tp,[_7P,_14o,_tW,_12N,_])),_14s=_14r;return _14o;};},_14t=new T(function(){return B(unCStr("auto"));}),_14u=new T(function(){return B(unCStr("\u30bb\u30fc\u30d6\u3057\u307e\u3057\u305f"));}),_14v=function(_14w,_){var _14x=B(A(_y7,[_14w,_])),_14y=_14x,_14z=B(_yl(_14w,_)),_14A=_14z,_14B=B(_wu(_14t,_14u,_)),_14C=_14B;return [0,_14C,new T(function(){return E(E(_14A)[2]);})];},_14D=new T(function(){return B(unCStr("%.2f"));}),_14E=[1,_k0,_u],_14F=function(_14G,_){var _14H=jsEval(toJSStr(B(unAppCStr("document.title = ",[1,_k0,new T(function(){return B(_k2(B(_1y(_tg,B(_XO(_14D,new T(function(){return B(_B2([1,[4,new T(function(){return E(E(_14G)[1]);})],_u],_u));}))))),_14E));})])))),_14I=_14H;return [0,_7b,_14G];},_14J=function(_14K,_14L,_14M){while(1){var _14N=(function(_14O,_14P,_14Q){var _14R=E(_14Q);if(!_14R[0]){return [0,_14O,_14P];}else{var _14S=_14R[1];_14K=new T(function(){var _14T=E(E(_14S)[1]);switch(_14T){case -1:var _14U=[0,0];break;case 0:var _14U=E(_Hg);break;default:var _14U=[0,B(_HK(E(_14O)[1],_14T))];}var _14V=_14U;return _14V;});var _14W=[1,new T(function(){return [0,B(_Hk(E(_14O)[1],E(_14S)[1]))];}),_14P];_14M=_14R[2];_14L=_14W;return null;}})(_14K,_14L,_14M);if(_14N!=null){return _14N;}}},_14X=function(_14Y,_14Z,_150,_151){return new F(function(){return _14J(new T(function(){var _152=E(E(_150)[1]);switch(_152){case -1:var _153=[0,0];break;case 0:var _153=E(_Hg);break;default:var _153=[0,B(_HK(E(_14Y)[1],_152))];}var _154=_153;return _154;}),[1,new T(function(){return [0,B(_Hk(E(_14Y)[1],E(_150)[1]))];}),_14Z],_151);});},_155=function(_156,_157){var _158=E(_156);if(!_158[0]){return [0];}else{var _159=_158[1];return _157>1?[1,_159,new T(function(){return B(_155(_158[2],_157-1|0));})]:[1,_159,_u];}},_15a=new T(function(){return B(_19(0,_o6,_u));}),_15b=new T(function(){return B(_19(0,_ou,_u));}),_15c=function(_15d,_15e){var _15f=E(_15e);if(!_15f[0]){return [0,_u,_u];}else{var _15g=_15f[1];if(!B(A(_15d,[_15g]))){var _15h=new T(function(){var _15i=B(_15c(_15d,_15f[2]));return [0,_15i[1],_15i[2]];});return [0,[1,_15g,new T(function(){return E(E(_15h)[1]);})],new T(function(){return E(E(_15h)[2]);})];}else{return [0,_u,_15f];}}},_15j=function(_15k,_15l){var _15m=function(_15n,_15o){return !B(_6C(_15o,_u))?[0,_15n,new T(function(){var _15p=B(_15j(_15k,_15o));return [1,_15p[1],_15p[2]];})]:[0,_15n,_u];};if(_15k>=0){var _15q=B(_Rw(_15k,_15l));return new F(function(){return _15m(_15q[1],_15q[2]);});}else{return new F(function(){return _15m(_u,_15l);});}},_15r=function(_15s){var _15t=E(_15s);if(!_15t[0]){return [0];}else{return new F(function(){return _O(_15t[1],new T(function(){return B(_15r(_15t[2]));}));});}},_15u=[0,44],_15v=[1,_15u,_u],_15w=function(_15x){return E(E(_15x)[1])==46?true:false;},_15y=function(_15z,_15A){var _15B=E(_15A);return _15B[0]==0?[0]:[1,_15z,[1,_15B[1],new T(function(){return B(_15y(_15z,_15B[2]));})]];},_15C=function(_15D){var _15E=new T(function(){var _15F=B(_15c(_15w,_15D));return [0,_15F[1],_15F[2]];}),_15G=B(_15j(3,new T(function(){return B(_B2(E(_15E)[1],_u));})));return new F(function(){return _O(B(_B2(B(_15r([1,_15G[1],new T(function(){return B(_15y(_15v,_15G[2]));})])),_u)),new T(function(){return E(E(_15E)[2]);}));});},_15H=function(_15I){return _15I>1000?B(_15C(new T(function(){var _15J=B(_nK(_15I)),_15K=_15J[1],_15L=_15J[2];if(_15L>=0){var _15M=B(_19(0,B(_ov(_15K,_15L)),_u));}else{var _15N= -_15L;if(_15N<=52){var _15O=hs_uncheckedIShiftRA64(B(_o3(_15K)),_15N),_15P=_15O,_15Q=B(_19(0,B(_nN(_15P)),_u));}else{var _15Q=!B(_Y(_15K,_o6))?E(_15a):E(_15b);}var _15R=_15Q,_15S=_15R,_15M=_15S;}var _15T=_15M,_15U=_15T;return _15U;}))):B(_15C(new T(function(){return B(_155(B(_XO(_14D,new T(function(){return B(_B2([1,[4,[0,_15I]],_u],_u));}))),5));})));},_15V=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:308:9-14"));}),_15W=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:295:7-12"));}),_15X=[1,_zt,_u],_15Y=[0,5],_15Z=[1,_15Y,_15X],_160=[1,_AD,_15Z],_161=[1,_zQ,_160],_162=[1,_zu,_161],_163=[0,500],_164=[1,_163,_162],_165=new T(function(){return B(_15r(_u));}),_166=new T(function(){return B(_1y(_tg,_165));}),_167=[0,-2147483648],_168=new T(function(){return B(unCStr("disabled"));}),_169=new T(function(){return B(unCStr("%s<span class=\"item-%d\">%s</span>"));}),_16a=new T(function(){return B(_ut("(function(e,c){e.removeAttribute(c);})"));}),_16b=function(_16c){return function(_16d,_){var _16e=B(A(new T(function(){return B(A(_16a,[E(E(_16c)[1])]));}),[E(toJSStr(E(_16d))),_])),_16f=_16e;return _7b;};},_16g=function(_16h,_16i){var _16j=E(_16h);if(!_16j[0]){return [0];}else{var _16k=E(_16i);return _16k[0]==0?[0]:[1,[0,_16j[1],_16k[1]],new T(function(){return B(_16g(_16j[2],_16k[2]));})];}},_16l=function(_,_16m){var _16n=jsFind(toJSStr(E(_6))),_16o=_16n,_16p=E(_16o);if(!_16p[0]){return new F(function(){return _sc(_15W,_);});}else{var _16q=E(_uE),_16r=toJSStr(_16q),_16s=E(E(_16m)[2]),_16t=_16s[7],_16u=jsSet(E(_16p[1])[1],_16r,toJSStr(B(_15H(E(_16s[2])[1])))),_16v=jsFind(toJSStr(E(_8))),_16w=_16v,_16x=E(_16w);if(!_16x[0]){return new F(function(){return _sc(_15W,_);});}else{var _16y=E(_16s[1])[1],_16z=jsSet(E(_16x[1])[1],_16r,toJSStr(B(_15H(_16y)))),_16A=jsFind(toJSStr(E(_4))),_16B=_16A,_16C=E(_16B);if(!_16C[0]){return new F(function(){return _sc(_15W,_);});}else{var _16D=jsSet(E(_16C[1])[1],_16r,toJSStr(B(_15H(E(_16s[3])[1])))),_16E=function(_16F){var _16G=E(_16F);return _16G[0]==0?E(_qP):function(_16H,_){var _16I=B(A(new T(function(){var _16J=E(_16G[1]),_16K=_16J[1],_16L=E(_16J[2])[1],_16M=new T(function(){var _16N=E(_16K)[1];return _16N<=0?B(unAppCStr("item-sp-",new T(function(){if(_16N<0){var _16O=B(_9V(0, -_16N,_u));}else{var _16O=B(_9V(0,_16N,_u));}var _16P=_16O;return _16P;}))):B(unAppCStr("item-",new T(function(){return B(_9V(0,_16N,_u));})));}),_16Q=new T(function(){var _16R=B(_ti(E(_16K)[1],_16t));return _16R[0]==0?E(_m8):E(_16R[1]);});return function(_16S,_){var _16T=B(A(new T(function(){if(E(_16K)[1]<=0){var _16U=E(_qP);}else{var _16U=function(_16V,_){var _16W=E(new T(function(){return B(_O(_16M,_13i));})),_16X=jsFind(toJSStr(_16W)),_16Y=_16X,_16Z=E(_16Y);if(!_16Z[0]){return new F(function(){return _sG(_16W);});}else{var _170=jsFind(toJSStr(E(new T(function(){return B(_O(_16M,_13g));})))),_171=_170,_172=E(_171);if(!_172[0]){return new F(function(){return _sc(_15V,_);});}else{var _173=jsGet(E(_172[1])[1],toJSStr(_16q)),_174=_173,_175=new T(function(){return fromJSStr(_174);}),_176=function(_177){return _177>1?[1,_175,new T(function(){return B(_176(_177-1|0));})]:E([1,_175,_u]);},_178=jsSet(E(_16Z[1])[1],_16r,toJSStr(B((function(_179,_17a){while(1){var _17b=(function(_17c,_17d){var _17e=E(_17d);if(!_17e[0]){return E(_17c);}else{var _17f=E(_17e[1]);_179=B(_1y(_tg,B(_XO(_169,new T(function(){return B(_B2([1,[1,new T(function(){var _17g=E(_17f[2])[1];if(_17g>0){var _17h=B(_1y(_tg,B(_15r(B(_176(_17g))))));}else{var _17h=E(_166);}var _17i=_17h,_17j=_17i;return _17j;})],[1,[2,_167,new T(function(){return B(_8F(E(_17f[1])[1]));})],[1,[1,new T(function(){return B(_1y(_tg,_17c));})],_u]]],_u));})))));_17a=_17e[2];return null;}})(_179,_17a);if(_17b!=null){return _17b;}}})(_u,new T(function(){return B(_16g(_164,new T(function(){return B(_B2(B(_14X(_16Q,_u,_163,_162))[2],_u));})));}))))),_17k=E(new T(function(){return B(_O(_16M,_13h));})),_17l=jsFind(toJSStr(_17k)),_17m=_17l,_17n=E(_17m);return _17n[0]==0?B(_sG(_17k)):B(A(_tv,[_sB,_17n[1],_16q,new T(function(){return B(_9V(0,E(_16Q)[1],_u));}),_16V,_]));}}};}var _17o=_16U,_17p=_17o;return _17p;}),[_16S,_])),_17q=_16T,_17r=E(new T(function(){return B(_O(_16M,_10V));})),_17s=jsFind(toJSStr(_17r)),_17t=_17s,_17u=E(_17t);if(!_17u[0]){return new F(function(){return _sG(_17r);});}else{var _17v=_17u[1];if(!E(new T(function(){var _17w=E(_16K)[1];if(_17w<=0){if(!B(_n2(_17w,_16t))){var _17x=B(_ym(B(A(_16L,[_16Q]))))<=_16y;}else{if(B(_mV(_16t,_17w))[1]>=1){var _17y=false;}else{var _17y=B(_ym(B(A(_16L,[_16Q]))))<=_16y;}var _17z=_17y,_17A=_17z,_17x=_17A;}var _17B=_17x;}else{var _17B=B(_ym(B(A(_16L,[_16Q]))))<=_16y;}var _17C=_17B,_17D=_17C;return _17D;}))){var _17E=B(A(_tp,[_sB,_17v,_168,_168,new T(function(){return E(E(_17q)[2]);}),_])),_17F=_17E,_17G=B(_O(_16M,_13j)),_17H=jsFind(toJSStr(_17G)),_17I=_17H,_17J=E(_17I);if(!_17J[0]){return new F(function(){return _sG(_17G);});}else{var _17K=jsSet(E(_17J[1])[1],toJSStr(_16q),toJSStr(B(_15C(new T(function(){return B(_19(0,B(A(_16L,[_16Q])),_u));}))))),_17L=function(_17M,_){var _17N=E(_16M),_17O=jsFind(toJSStr(_17N)),_17P=_17O,_17Q=E(_17P);if(!_17Q[0]){return new F(function(){return _sG(_17N);});}else{var _17R=E(_17Q[1]),_17S=E(_sD),_17T=jsGetStyle(_17R[1],toJSStr(_17S)),_17U=_17T;return !B(_6O(fromJSStr(_17U),_qO))?B(A(_qD,[_sB,_17R,_17S,_qO,_17M,_])):[0,_7b,_17M];}};if(!B(_n2(E(_16K)[1],_16t))){var _17V=E(E(_17F)[2]);return B(_ym(B(A(_16L,[_m8]))))>3*E(_17V[8])[1]?[0,_7b,_17V]:B(_17L(_17V,_));}else{return new F(function(){return _17L(new T(function(){return E(E(_17F)[2]);}),_);});}}}else{var _17W=B(A(_16b,[_17v,_168,_])),_17X=_17W,_17Y=B(_O(_16M,_13j)),_17Z=jsFind(toJSStr(_17Y)),_180=_17Z,_181=E(_180);if(!_181[0]){return new F(function(){return _sG(_17Y);});}else{var _182=jsSet(E(_181[1])[1],toJSStr(_16q),toJSStr(B(_15C(new T(function(){return B(_19(0,B(A(_16L,[_16Q])),_u));}))))),_183=function(_184,_){var _185=E(_16M),_186=jsFind(toJSStr(_185)),_187=_186,_188=E(_187);if(!_188[0]){return new F(function(){return _sG(_185);});}else{var _189=E(_188[1]),_18a=E(_sD),_18b=jsGetStyle(_189[1],toJSStr(_18a)),_18c=_18b;return !B(_6O(fromJSStr(_18c),_qO))?B(A(_qD,[_sB,_189,_18a,_qO,_184,_])):[0,_7b,_184];}};if(!B(_n2(E(_16K)[1],_16t))){var _18d=E(E(_17q)[2]);return B(_ym(B(A(_16L,[_m8]))))>3*E(_18d[8])[1]?[0,_7b,_18d]:B(_183(_18d,_));}else{return new F(function(){return _183(new T(function(){return E(E(_17q)[2]);}),_);});}}}}};}),[_16H,_])),_18e=_16I;return new F(function(){return A(new T(function(){return B(_16E(_16G[2]));}),[new T(function(){return E(E(_18e)[2]);}),_]);});};};return new F(function(){return A(_16E,[_zz,_16s,_]);});}}}},_18f=function(_18g){var _18h=E(_18g);return _18h[0]==0?E(_qP):function(_18i,_){var _18j=B(A(new T(function(){var _18k=E(_18h[1]),_18l=_18k[1],_18m=new T(function(){return B(A(E(_18k[2])[2],[_18l]));});return function(_18n,_){var _18o=E(_18n),_18p=_18o[6],_18q=E(_18l)[1];return !B(_n2(_18q,_18p))?B(A(_18m,[_18o,_])):B(_mV(_18p,_18q))[0]==0?B(A(_18m,[_18o,_])):[0,_7b,_18o];};}),[_18i,_])),_18r=_18j;return new F(function(){return A(new T(function(){return B(_18f(_18h[2]));}),[new T(function(){return E(E(_18r)[2]);}),_]);});};},_18s=new T(function(){return B(_18f(_AG));}),_18t=function(_18u,_18v){while(1){var _18w=E(_18u);if(!_18w[0]){return E(_18v)[0]==0?1:0;}else{var _18x=E(_18v);if(!_18x[0]){return 2;}else{var _18y=E(_18w[1])[1],_18z=E(_18x[1])[1];if(_18y!=_18z){return _18y>_18z?2:0;}else{_18u=_18w[2];_18v=_18x[2];continue;}}}}},_18A=new T(function(){return B(unCStr("Map.!: given key is not an element in the map"));}),_18B=new T(function(){return B(err(_18A));}),_18C=function(_18D,_18E){while(1){var _18F=E(_18D),_18G=E(_18E);if(!_18G[0]){switch(B(_18t(_18F,_18G[2]))){case 0:_18D=_18F;_18E=_18G[4];continue;case 1:return E(_18G[3]);default:_18D=_18F;_18E=_18G[5];continue;}}else{return E(_18B);}}},_18H=[1],_18I=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_18J=function(_18K){return new F(function(){return err(_18I);});},_18L=new T(function(){return B(_18J(_));}),_18M=function(_18N,_18O,_18P,_18Q){var _18R=E(_18P);if(!_18R[0]){var _18S=_18R[1],_18T=E(_18Q);if(!_18T[0]){var _18U=_18T[1],_18V=_18T[2],_18W=_18T[3];if(_18U<=(imul(3,_18S)|0)){return [0,(1+_18S|0)+_18U|0,E(E(_18N)),_18O,E(_18R),E(_18T)];}else{var _18X=E(_18T[4]);if(!_18X[0]){var _18Y=_18X[1],_18Z=_18X[2],_190=_18X[3],_191=_18X[4],_192=E(_18T[5]);if(!_192[0]){var _193=_192[1];if(_18Y>=(imul(2,_193)|0)){var _194=function(_195){var _196=E(_18N),_197=E(_18X[5]);return _197[0]==0?[0,(1+_18S|0)+_18U|0,E(_18Z),_190,E([0,(1+_18S|0)+_195|0,E(_196),_18O,E(_18R),E(_191)]),E([0,(1+_193|0)+_197[1]|0,E(_18V),_18W,E(_197),E(_192)])]:[0,(1+_18S|0)+_18U|0,E(_18Z),_190,E([0,(1+_18S|0)+_195|0,E(_196),_18O,E(_18R),E(_191)]),E([0,1+_193|0,E(_18V),_18W,E(_18H),E(_192)])];},_198=E(_191);return _198[0]==0?B(_194(_198[1])):B(_194(0));}else{return [0,(1+_18S|0)+_18U|0,E(_18V),_18W,E([0,(1+_18S|0)+_18Y|0,E(E(_18N)),_18O,E(_18R),E(_18X)]),E(_192)];}}else{return E(_18L);}}else{return E(_18L);}}}else{return [0,1+_18S|0,E(E(_18N)),_18O,E(_18R),E(_18H)];}}else{var _199=E(_18Q);if(!_199[0]){var _19a=_199[1],_19b=_199[2],_19c=_199[3],_19d=_199[5],_19e=E(_199[4]);if(!_19e[0]){var _19f=_19e[1],_19g=_19e[2],_19h=_19e[3],_19i=_19e[4],_19j=E(_19d);if(!_19j[0]){var _19k=_19j[1];if(_19f>=(imul(2,_19k)|0)){var _19l=function(_19m){var _19n=E(_18N),_19o=E(_19e[5]);return _19o[0]==0?[0,1+_19a|0,E(_19g),_19h,E([0,1+_19m|0,E(_19n),_18O,E(_18H),E(_19i)]),E([0,(1+_19k|0)+_19o[1]|0,E(_19b),_19c,E(_19o),E(_19j)])]:[0,1+_19a|0,E(_19g),_19h,E([0,1+_19m|0,E(_19n),_18O,E(_18H),E(_19i)]),E([0,1+_19k|0,E(_19b),_19c,E(_18H),E(_19j)])];},_19p=E(_19i);return _19p[0]==0?B(_19l(_19p[1])):B(_19l(0));}else{return [0,1+_19a|0,E(_19b),_19c,E([0,1+_19f|0,E(E(_18N)),_18O,E(_18H),E(_19e)]),E(_19j)];}}else{return [0,3,E(_19g),_19h,E([0,1,E(E(_18N)),_18O,E(_18H),E(_18H)]),E([0,1,E(_19b),_19c,E(_18H),E(_18H)])];}}else{var _19q=E(_19d);return _19q[0]==0?[0,3,E(_19b),_19c,E([0,1,E(E(_18N)),_18O,E(_18H),E(_18H)]),E(_19q)]:[0,2,E(E(_18N)),_18O,E(_18H),E(_199)];}}else{return [0,1,E(E(_18N)),_18O,E(_18H),E(_18H)];}}},_19r=function(_19s,_19t){return [0,1,E(E(_19s)),_19t,E(_18H),E(_18H)];},_19u=function(_19v,_19w,_19x){var _19y=E(_19x);if(!_19y[0]){return new F(function(){return _18M(_19y[2],_19y[3],_19y[4],B(_19u(_19v,_19w,_19y[5])));});}else{return new F(function(){return _19r(_19v,_19w);});}},_19z=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_19A=function(_19B){return new F(function(){return err(_19z);});},_19C=new T(function(){return B(_19A(_));}),_19D=function(_19E,_19F,_19G,_19H){var _19I=E(_19H);if(!_19I[0]){var _19J=_19I[1],_19K=E(_19G);if(!_19K[0]){var _19L=_19K[1],_19M=_19K[2],_19N=_19K[3];if(_19L<=(imul(3,_19J)|0)){return [0,(1+_19L|0)+_19J|0,E(E(_19E)),_19F,E(_19K),E(_19I)];}else{var _19O=E(_19K[4]);if(!_19O[0]){var _19P=_19O[1],_19Q=E(_19K[5]);if(!_19Q[0]){var _19R=_19Q[1],_19S=_19Q[2],_19T=_19Q[3],_19U=_19Q[4];if(_19R>=(imul(2,_19P)|0)){var _19V=function(_19W){var _19X=E(_19Q[5]);return _19X[0]==0?[0,(1+_19L|0)+_19J|0,E(_19S),_19T,E([0,(1+_19P|0)+_19W|0,E(_19M),_19N,E(_19O),E(_19U)]),E([0,(1+_19J|0)+_19X[1]|0,E(E(_19E)),_19F,E(_19X),E(_19I)])]:[0,(1+_19L|0)+_19J|0,E(_19S),_19T,E([0,(1+_19P|0)+_19W|0,E(_19M),_19N,E(_19O),E(_19U)]),E([0,1+_19J|0,E(E(_19E)),_19F,E(_18H),E(_19I)])];},_19Y=E(_19U);return _19Y[0]==0?B(_19V(_19Y[1])):B(_19V(0));}else{return [0,(1+_19L|0)+_19J|0,E(_19M),_19N,E(_19O),E([0,(1+_19J|0)+_19R|0,E(E(_19E)),_19F,E(_19Q),E(_19I)])];}}else{return E(_19C);}}else{return E(_19C);}}}else{return [0,1+_19J|0,E(E(_19E)),_19F,E(_18H),E(_19I)];}}else{var _19Z=E(_19G);if(!_19Z[0]){var _1a0=_19Z[1],_1a1=_19Z[2],_1a2=_19Z[3],_1a3=_19Z[5],_1a4=E(_19Z[4]);if(!_1a4[0]){var _1a5=_1a4[1],_1a6=E(_1a3);if(!_1a6[0]){var _1a7=_1a6[1],_1a8=_1a6[2],_1a9=_1a6[3],_1aa=_1a6[4];if(_1a7>=(imul(2,_1a5)|0)){var _1ab=function(_1ac){var _1ad=E(_1a6[5]);return _1ad[0]==0?[0,1+_1a0|0,E(_1a8),_1a9,E([0,(1+_1a5|0)+_1ac|0,E(_1a1),_1a2,E(_1a4),E(_1aa)]),E([0,1+_1ad[1]|0,E(E(_19E)),_19F,E(_1ad),E(_18H)])]:[0,1+_1a0|0,E(_1a8),_1a9,E([0,(1+_1a5|0)+_1ac|0,E(_1a1),_1a2,E(_1a4),E(_1aa)]),E([0,1,E(E(_19E)),_19F,E(_18H),E(_18H)])];},_1ae=E(_1aa);return _1ae[0]==0?B(_1ab(_1ae[1])):B(_1ab(0));}else{return [0,1+_1a0|0,E(_1a1),_1a2,E(_1a4),E([0,1+_1a7|0,E(E(_19E)),_19F,E(_1a6),E(_18H)])];}}else{return [0,3,E(_1a1),_1a2,E(_1a4),E([0,1,E(E(_19E)),_19F,E(_18H),E(_18H)])];}}else{var _1af=E(_1a3);return _1af[0]==0?[0,3,E(_1af[2]),_1af[3],E([0,1,E(_1a1),_1a2,E(_18H),E(_18H)]),E([0,1,E(E(_19E)),_19F,E(_18H),E(_18H)])]:[0,2,E(E(_19E)),_19F,E(_19Z),E(_18H)];}}else{return [0,1,E(E(_19E)),_19F,E(_18H),E(_18H)];}}},_1ag=function(_1ah,_1ai,_1aj){var _1ak=E(_1aj);if(!_1ak[0]){return new F(function(){return _19D(_1ak[2],_1ak[3],B(_1ag(_1ah,_1ai,_1ak[4])),_1ak[5]);});}else{return new F(function(){return _19r(_1ah,_1ai);});}},_1al=function(_1am,_1an,_1ao,_1ap,_1aq,_1ar,_1as){return new F(function(){return _19D(_1ap,_1aq,B(_1ag(_1am,_1an,_1ar)),_1as);});},_1at=function(_1au,_1av,_1aw,_1ax,_1ay,_1az,_1aA,_1aB){var _1aC=E(_1aw);if(!_1aC[0]){var _1aD=_1aC[1],_1aE=_1aC[2],_1aF=_1aC[3],_1aG=_1aC[4],_1aH=_1aC[5];if((imul(3,_1aD)|0)>=_1ax){if((imul(3,_1ax)|0)>=_1aD){return [0,(_1aD+_1ax|0)+1|0,E(E(_1au)),_1av,E(_1aC),E([0,_1ax,E(_1ay),_1az,E(_1aA),E(_1aB)])];}else{return new F(function(){return _18M(_1aE,_1aF,_1aG,B(_1at(_1au,_1av,_1aH,_1ax,_1ay,_1az,_1aA,_1aB)));});}}else{return new F(function(){return _19D(_1ay,_1az,B(_1aI(_1au,_1av,_1aD,_1aE,_1aF,_1aG,_1aH,_1aA)),_1aB);});}}else{return new F(function(){return _1al(_1au,_1av,_1ax,_1ay,_1az,_1aA,_1aB);});}},_1aI=function(_1aJ,_1aK,_1aL,_1aM,_1aN,_1aO,_1aP,_1aQ){var _1aR=E(_1aQ);if(!_1aR[0]){var _1aS=_1aR[1],_1aT=_1aR[2],_1aU=_1aR[3],_1aV=_1aR[4],_1aW=_1aR[5];if((imul(3,_1aL)|0)>=_1aS){if((imul(3,_1aS)|0)>=_1aL){return [0,(_1aL+_1aS|0)+1|0,E(E(_1aJ)),_1aK,E([0,_1aL,E(_1aM),_1aN,E(_1aO),E(_1aP)]),E(_1aR)];}else{return new F(function(){return _18M(_1aM,_1aN,_1aO,B(_1at(_1aJ,_1aK,_1aP,_1aS,_1aT,_1aU,_1aV,_1aW)));});}}else{return new F(function(){return _19D(_1aT,_1aU,B(_1aI(_1aJ,_1aK,_1aL,_1aM,_1aN,_1aO,_1aP,_1aV)),_1aW);});}}else{return new F(function(){return _19u(_1aJ,_1aK,[0,_1aL,E(_1aM),_1aN,E(_1aO),E(_1aP)]);});}},_1aX=function(_1aY,_1aZ,_1b0,_1b1){var _1b2=E(_1b0);if(!_1b2[0]){var _1b3=_1b2[1],_1b4=_1b2[2],_1b5=_1b2[3],_1b6=_1b2[4],_1b7=_1b2[5],_1b8=E(_1b1);if(!_1b8[0]){var _1b9=_1b8[1],_1ba=_1b8[2],_1bb=_1b8[3],_1bc=_1b8[4],_1bd=_1b8[5];if((imul(3,_1b3)|0)>=_1b9){if((imul(3,_1b9)|0)>=_1b3){return [0,(_1b3+_1b9|0)+1|0,E(E(_1aY)),_1aZ,E(_1b2),E(_1b8)];}else{return new F(function(){return _18M(_1b4,_1b5,_1b6,B(_1at(_1aY,_1aZ,_1b7,_1b9,_1ba,_1bb,_1bc,_1bd)));});}}else{return new F(function(){return _19D(_1ba,_1bb,B(_1aI(_1aY,_1aZ,_1b3,_1b4,_1b5,_1b6,_1b7,_1bc)),_1bd);});}}else{return new F(function(){return _19u(_1aY,_1aZ,_1b2);});}}else{return new F(function(){return _1ag(_1aY,_1aZ,_1b1);});}},_1be=function(_1bf,_1bg,_1bh,_1bi){var _1bj=E(_1bf);if(_1bj==1){var _1bk=E(_1bi);return _1bk[0]==0?[0,new T(function(){return [0,1,E(E(_1bg)),_1bh,E(_18H),E(_18H)];}),_u,_u]:B(_18t(_1bg,E(_1bk[1])[1]))==0?[0,new T(function(){return [0,1,E(E(_1bg)),_1bh,E(_18H),E(_18H)];}),_1bk,_u]:[0,new T(function(){return [0,1,E(E(_1bg)),_1bh,E(_18H),E(_18H)];}),_u,_1bk];}else{var _1bl=B(_1be(_1bj>>1,_1bg,_1bh,_1bi)),_1bm=_1bl[1],_1bn=_1bl[3],_1bo=E(_1bl[2]);if(!_1bo[0]){return [0,_1bm,_u,_1bn];}else{var _1bp=E(_1bo[1]),_1bq=_1bp[1],_1br=_1bp[2],_1bs=E(_1bo[2]);if(!_1bs[0]){return [0,new T(function(){return B(_19u(_1bq,_1br,_1bm));}),_u,_1bn];}else{var _1bt=E(_1bs[1]),_1bu=_1bt[1];if(!B(_18t(_1bq,_1bu))){var _1bv=B(_1be(_1bj>>1,_1bu,_1bt[2],_1bs[2]));return [0,new T(function(){return B(_1aX(_1bq,_1br,_1bm,_1bv[1]));}),_1bv[2],_1bv[3]];}else{return [0,_1bm,_u,_1bo];}}}}},_1bw=function(_1bx,_1by,_1bz){var _1bA=E(_1bx),_1bB=E(_1bz);if(!_1bB[0]){var _1bC=_1bB[2],_1bD=_1bB[3],_1bE=_1bB[4],_1bF=_1bB[5];switch(B(_18t(_1bA,_1bC))){case 0:return new F(function(){return _19D(_1bC,_1bD,B(_1bw(_1bA,_1by,_1bE)),_1bF);});break;case 1:return [0,_1bB[1],E(_1bA),_1by,E(_1bE),E(_1bF)];default:return new F(function(){return _18M(_1bC,_1bD,_1bE,B(_1bw(_1bA,_1by,_1bF)));});}}else{return [0,1,E(_1bA),_1by,E(_18H),E(_18H)];}},_1bG=function(_1bH,_1bI){while(1){var _1bJ=E(_1bI);if(!_1bJ[0]){return E(_1bH);}else{var _1bK=E(_1bJ[1]),_1bL=B(_1bw(_1bK[1],_1bK[2],_1bH));_1bI=_1bJ[2];_1bH=_1bL;continue;}}},_1bM=function(_1bN,_1bO,_1bP,_1bQ){return new F(function(){return _1bG(B(_1bw(_1bO,_1bP,_1bN)),_1bQ);});},_1bR=function(_1bS,_1bT,_1bU){var _1bV=E(_1bT);return new F(function(){return _1bG(B(_1bw(_1bV[1],_1bV[2],_1bS)),_1bU);});},_1bW=function(_1bX,_1bY,_1bZ){while(1){var _1c0=E(_1bZ);if(!_1c0[0]){return E(_1bY);}else{var _1c1=E(_1c0[1]),_1c2=_1c1[1],_1c3=_1c1[2],_1c4=E(_1c0[2]);if(!_1c4[0]){return new F(function(){return _19u(_1c2,_1c3,_1bY);});}else{var _1c5=E(_1c4[1]),_1c6=_1c5[1];if(!B(_18t(_1c2,_1c6))){var _1c7=B(_1be(_1bX,_1c6,_1c5[2],_1c4[2])),_1c8=_1c7[1],_1c9=E(_1c7[3]);if(!_1c9[0]){var _1ca=_1bX<<1,_1cb=B(_1aX(_1c2,_1c3,_1bY,_1c8));_1bZ=_1c7[2];_1bX=_1ca;_1bY=_1cb;continue;}else{return new F(function(){return _1bR(B(_1aX(_1c2,_1c3,_1bY,_1c8)),_1c9[1],_1c9[2]);});}}else{return new F(function(){return _1bM(_1bY,_1c2,_1c3,_1c4);});}}}}},_1cc=function(_1cd,_1ce,_1cf,_1cg,_1ch){var _1ci=E(_1ch);if(!_1ci[0]){return new F(function(){return _19u(_1cf,_1cg,_1ce);});}else{var _1cj=E(_1ci[1]),_1ck=_1cj[1];if(!B(_18t(_1cf,_1ck))){var _1cl=B(_1be(_1cd,_1ck,_1cj[2],_1ci[2])),_1cm=_1cl[1],_1cn=E(_1cl[3]);if(!_1cn[0]){return new F(function(){return _1bW(_1cd<<1,B(_1aX(_1cf,_1cg,_1ce,_1cm)),_1cl[2]);});}else{return new F(function(){return _1bR(B(_1aX(_1cf,_1cg,_1ce,_1cm)),_1cn[1],_1cn[2]);});}}else{return new F(function(){return _1bM(_1ce,_1cf,_1cg,_1ci);});}}},_1co=function(_1cp){var _1cq=E(_1cp);if(!_1cq[0]){return [1];}else{var _1cr=E(_1cq[1]),_1cs=_1cr[1],_1ct=_1cr[2],_1cu=E(_1cq[2]);if(!_1cu[0]){return [0,1,E(E(_1cs)),_1ct,E(_18H),E(_18H)];}else{var _1cv=_1cu[2],_1cw=E(_1cu[1]),_1cx=_1cw[1],_1cy=_1cw[2];if(!B(_18t(_1cs,_1cx))){return new F(function(){return _1cc(1,[0,1,E(E(_1cs)),_1ct,E(_18H),E(_18H)],_1cx,_1cy,_1cv);});}else{return new F(function(){return _1bM([0,1,E(E(_1cs)),_1ct,E(_18H),E(_18H)],_1cx,_1cy,_1cv);});}}}},_1cz=new T(function(){return B(unCStr("reset"));}),_1cA=new T(function(){return B(unCStr("resetAll"));}),_1cB=[1,_1cA,_u],_1cC=[1,_1cz,_1cB],_1cD=[1,_sR,_1cC],_1cE=[1,_sE,_1cD],_1cF=new T(function(){return B(_16g(_1cE,_101));}),_1cG=new T(function(){return B(_1co(_1cF));}),_1cH=new T(function(){return B(_18C(_sE,_1cG));}),_1cI=function(_1cJ){return new F(function(){return err(B(unAppCStr("docFocused: ",[1,_k0,new T(function(){return B(_k2(_1cJ,_14E));})])));});},_1cK=new T(function(){return B(unCStr("\u304a\u304b\u3048\u308a\u306a\u3055\u3044\uff01<br>(\u4f9d\u5b58\u5ea6\u30dc\u30fc\u30ca\u30b9 +"));}),_1cL=new T(function(){return B(unCStr("false"));}),_1cM=new T(function(){return B(unCStr("document.hasFocus()"));}),_1cN=[0,41],_1cO=[1,_1cN,_u],_1cP=new T(function(){return B(unCStr("\u611b\u3061\u3083\u3093"));}),_1cQ=function(_1cR,_1cS){while(1){var _1cT=E(_1cR);if(!_1cT[0]){var _1cU=_1cT[1],_1cV=E(_1cS);if(!_1cV[0]){var _1cW=_1cV[1],_1cX=subC(_1cU,_1cW);if(!E(_1cX[2])){return [0,_1cX[1]];}else{_1cR=[1,I_fromInt(_1cU)];_1cS=[1,I_fromInt(_1cW)];continue;}}else{_1cR=[1,I_fromInt(_1cU)];_1cS=_1cV;continue;}}else{var _1cY=E(_1cS);if(!_1cY[0]){_1cR=_1cT;_1cS=[1,I_fromInt(_1cY[1])];continue;}else{return [1,I_sub(_1cT[1],_1cY[1])];}}}},_1cZ=function(_1d0,_){var _1d1=E(_1d0),_1d2=_1d1[1],_1d3=_1d1[2],_1d4=_1d1[3],_1d5=_1d1[4],_1d6=_1d1[6],_1d7=_1d1[7],_1d8=_1d1[8],_1d9=_1d1[9],_1da=_1d1[10];if(!B(_n2(E(_1cH)[1],_1d7))){return new F(function(){return _16l(_,[0,_7b,_1d1]);});}else{var _1db=jsEval(toJSStr(E(_1cM))),_1dc=_1db,_1dd=B(_h3(_)),_1de=_1dd,_1df=fromJSStr(_1dc);if(!B(_6C(_1df,_1cL))){if(!B(_6C(_1df,_tN))){return new F(function(){return _1cI(_1df);});}else{var _1dg=new T(function(){return [0,B(_ym(B(_1cQ(_1de,_1d5))))];});if(!E(_1d1[5])){var _1dh=new T(function(){return [0,E(_1dg)[1]/1000/50*E(_1d9)[1]];}),_1di=B(_wu(_1cP,new T(function(){return B(_O(_1cK,new T(function(){return B(_O(B(_15H(E(_1dh)[1])),_1cO));})));}),_)),_1dj=_1di,_1dk=B(A(_18s,[[0,new T(function(){return [0,E(_1d2)[1]+E(_1d3)[1]/30];}),new T(function(){return [0,E(_1d3)[1]+E(_1dg)[1]/1000/100*E(_1da)[1]];}),new T(function(){return [0,E(_1d4)[1]+E(_1dh)[1]+E(_1dg)[1]/1000/1000*E(_1d9)[1]];}),_1de,_n,_1d6,_1d7,_1d8,_1d9,_1da],_])),_1dl=_1dk,_1dm=E(E(_1dl)[2]),_1dn=E(_1dm[1]);return _1dn[1]<=E(_1dm[8])[1]?B(_16l(_,[0,_7b,_1dm])):B(_16l(_,[0,_7b,[0,_1dn,_1dm[2],_1dm[3],_1dm[4],_1dm[5],_1dm[6],_1dm[7],_1dn,_1dm[9],_1dm[10]]]));}else{var _1do=B(A(_18s,[[0,new T(function(){return [0,E(_1d2)[1]+E(_1d3)[1]/30];}),new T(function(){return [0,E(_1d3)[1]+E(_1dg)[1]/1000/100*E(_1da)[1]];}),new T(function(){return [0,E(_1d4)[1]+E(_1dg)[1]/1000/1000*E(_1d9)[1]];}),_1de,_n,_1d6,_1d7,_1d8,_1d9,_1da],_])),_1dp=_1do,_1dq=E(E(_1dp)[2]),_1dr=E(_1dq[1]);return _1dr[1]<=E(_1dq[8])[1]?B(_16l(_,[0,_7b,_1dq])):B(_16l(_,[0,_7b,[0,_1dr,_1dq[2],_1dq[3],_1dq[4],_1dq[5],_1dq[6],_1dq[7],_1dr,_1dq[9],_1dq[10]]]));}}}else{var _1ds=E(_1d4)[1],_1dt=new T(function(){return [0,1.0e-2*E(_1da)[1]];});if(_1ds<=0){var _1du=B(A(_18s,[[0,new T(function(){return [0,E(_1d2)[1]+E(_1d3)[1]/30];}),_1d3,new T(function(){var _1dv=_1ds-E(_1dt)[1];return _1dv>0?[0,_1dv]:E(_h7);}),_1d5,_r,_1d6,_1d7,_1d8,_1d9,_1da],_])),_1dw=_1du,_1dx=E(E(_1dw)[2]),_1dy=E(_1dx[1]);return _1dy[1]<=E(_1dx[8])[1]?B(_16l(_,[0,_7b,_1dx])):B(_16l(_,[0,_7b,[0,_1dy,_1dx[2],_1dx[3],_1dx[4],_1dx[5],_1dx[6],_1dx[7],_1dy,_1dx[9],_1dx[10]]]));}else{var _1dz=B(A(_18s,[[0,new T(function(){return [0,E(_1d2)[1]+E(_1d3)[1]/30];}),new T(function(){return [0,E(_1d3)[1]+E(_1dt)[1]];}),new T(function(){var _1dA=_1ds-E(_1dt)[1];return _1dA>0?[0,_1dA]:E(_h7);}),_1d5,_r,_1d6,_1d7,_1d8,_1d9,_1da],_])),_1dB=_1dz,_1dC=E(E(_1dB)[2]),_1dD=E(_1dC[1]);return _1dD[1]<=E(_1dC[8])[1]?B(_16l(_,[0,_7b,_1dC])):B(_16l(_,[0,_7b,[0,_1dD,_1dC[2],_1dC[3],_1dC[4],_1dC[5],_1dC[6],_1dC[7],_1dD,_1dC[9],_1dC[10]]]));}}}},_1dE=function(_1dF){return new F(function(){return _18C(_1dF,_1cG);});},_1dG=new T(function(){return B(_1y(_1dE,_1cC));}),_1dH=function(_1dI){return E(E(_1dI)[2]);},_1dJ=function(_1dK,_1dL,_1dM){while(1){var _1dN=E(_1dM);if(!_1dN[0]){return true;}else{if(!B(A(_1dH,[_1dK,_1dL,_1dN[1]]))){return false;}else{_1dM=_1dN[2];continue;}}}},_1dO=function(_1dP,_1dQ){return new F(function(){return _1dJ(_IL,_1dP,_1dG);});},_1dR=new T(function(){return B(_ne(1,2147483647));}),_1dS=function(_){var _=0,_1dT=jsMkStdout(),_1dU=_1dT;return [0,_1dU];},_1dV=new T(function(){return B(_hc(_1dS));}),_1dW=function(_){var _1dX=B(_h3(_)),_1dY=_1dX,_1dZ=B(_12l(_if,_x5,_)),_1e0=_1dZ,_1e1=nMV(new T(function(){var _1e2=E(_1e0);return _1e2[0]==0?[0,_h7,_h7,_h7,_1dY,_r,_45,_45,_h7,_h8,_h8]:E(_1e2[1]);})),_1e3=_1e1,_1e4=B(unCStr("list-group")),_1e5=jsFind(toJSStr(_1e4)),_1e6=_1e5,_1e7=E(_1e6);if(!_1e7[0]){return new F(function(){return _sG(_1e4);});}else{var _1e8=B((function(_1e9,_){while(1){var _1ea=E(_1e9);if(!_1ea[0]){return _7b;}else{var _1eb=E(_1ea[1]),_1ec=E(E(_1eb[2])[3]),_1ed=B(A(_13k,[_1eb[1],_1ec[1],_1ec[2],_1ec[3],_1e7[1],_])),_1ee=_1ed;_1e9=_1ea[2];continue;}}})(_10k,_)),_1ef=_1e8,_1eg=B(unCStr("list-sp-group")),_1eh=jsFind(toJSStr(_1eg)),_1ei=_1eh,_1ej=E(_1ei);if(!_1ej[0]){return new F(function(){return _sG(_1eg);});}else{var _1ek=B((function(_1el,_){while(1){var _1em=E(_1el);if(!_1em[0]){return _7b;}else{var _1en=E(_1em[1]),_1eo=E(E(_1en[2])[3]),_1ep=B(A(_13k,[_1en[1],_1eo[1],_1eo[2],_1eo[3],_1ej[1],_])),_1eq=_1ep;_1el=_1em[2];continue;}}})(_12c,_)),_1er=_1ek,_1es=[0,_1e3],_1et=B(_10W(_1es,_1dR,_)),_1eu=_1et,_1ev=B(_10W(_1es,_101,_)),_1ew=_1ev,_1ex=function(_){var _1ey=B(_mH(33,_1e3,_1cZ,_)),_1ez=_1ey,_1eA=B(_12E(_1dV,B(_mE(_1ez)),_)),_1eB=_1eA,_1eC=B(_mH(1000,_1e3,_14F,_)),_1eD=_1eC,_1eE=B(_12E(_1dV,B(_mE(_1eD)),_)),_1eF=_1eE,_1eG=B(_mH(60000,_1e3,_14v,_)),_1eH=_1eG;return new F(function(){return _12E(_1dV,B(_mE(_1eH)),_);});},_1eI=function(_1eJ,_1eK,_){while(1){var _1eL=(function(_1eM,_1eN,_){var _1eO=E(_1eN);switch(_1eO[0]){case 0:_1eJ=function(_){return new F(function(){return _1eI(_1eM,_1eO[4],_);});};_1eK=_1eO[3];return null;case 1:var _1eP=_1eO[1],_1eQ=rMV(_1e3),_1eR=_1eQ,_1eS=E(_1eR),_1eT=_1eS[7];if(!B(_n2(_1eP,_1eT))){var _=wMV(_1e3,_1eS);return new F(function(){return A(_1eM,[_]);});}else{if(B(_mV(_1eT,_1eP))[1]<=0){var _=wMV(_1e3,_1eS);return new F(function(){return A(_1eM,[_]);});}else{var _1eU=B(A(E(_1eO[2])[2],[[0,_1eP],_1eS,_])),_1eV=_1eU,_=wMV(_1e3,new T(function(){return E(E(_1eV)[2]);}));return new F(function(){return A(_1eM,[_]);});}}break;default:return new F(function(){return A(_1eM,[_]);});}})(_1eJ,_1eK,_);if(_1eL!=null){return _1eL;}}},_1eW=B(_Ag(_1dO,_zy));if(!_1eW[0]){var _1eX=_1eW[3],_1eY=_1eW[4];if(_1eW[2]>=0){var _1eZ=B(_1eI(function(_){return new F(function(){return _1eI(_12b,_1eY,_);});},_1eX,_)),_1f0=_1eZ;return new F(function(){return _1ex(_);});}else{var _1f1=B(_1eI(function(_){return new F(function(){return _1eI(_12b,_1eX,_);});},_1eY,_)),_1f2=_1f1;return new F(function(){return _1ex(_);});}}else{var _1f3=B(_1eI(_12b,_1eW,_)),_1f4=_1f3;return new F(function(){return _1ex(_);});}}}},_1f5=function(_){return new F(function(){return _1dW(_);});};
var hasteMain = function() {B(A(_1f5, [0]));};window.onload = hasteMain;
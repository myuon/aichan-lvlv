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

var _0=new T(function(){return B(unCStr("items"));}),_1=new T(function(){return [0,toJSStr(E(_0))];}),_2=new T(function(){return B(unCStr("achievements"));}),_3=new T(function(){return [0,toJSStr(E(_2))];}),_4=new T(function(){return B(unCStr("lastFocus"));}),_5=new T(function(){return [0,toJSStr(E(_4))];}),_6=new T(function(){return B(unCStr("depend"));}),_7=new T(function(){return [0,toJSStr(E(_6))];}),_8=new T(function(){return B(unCStr("lps"));}),_9=new T(function(){return [0,toJSStr(E(_8))];}),_a=new T(function(){return B(unCStr("loves"));}),_b=new T(function(){return [0,toJSStr(E(_a))];}),_c=function(_d){return [0,toJSStr(E(_d))];},_e=function(_f){return [1,new T(function(){return B(_c(_f));})];},_g=new T(function(){return [0,"value"];}),_h=true,_i=[2,_h],_j=new T(function(){return [0,"hasValue"];}),_k=[0,_j,_i],_l=false,_m=[2,_l],_n=[0,_j,_m],_o=[0],_p=[1,_n,_o],_q=[4,_p],_r=function(_s,_t){while(1){var _u=(function(_v,_w){var _x=E(_w);switch(_x[0]){case 0:_s=new T(function(){return B(_r(_v,_x[4]));});_t=_x[3];return null;case 1:return [1,[3,[1,[0,[0,_x[1]]],[1,new T(function(){var _y=E(_x[2]);return _y[0]==0?E(_q):[4,[1,_k,[1,[0,_g,new T(function(){return B(_e(_y[1]));})],_o]]];}),_o]]],_v];default:return E(_v);}})(_s,_t);if(_u!=null){return _u;}}},_z=function(_A){return [0,new T(function(){return [0,E(_A)[1]];})];},_B=function(_C,_D){while(1){var _E=(function(_F,_G){var _H=E(_G);switch(_H[0]){case 0:_C=new T(function(){return B(_B(_F,_H[4]));});_D=_H[3];return null;case 1:return [1,[3,[1,[0,[0,_H[1]]],[1,new T(function(){return B(_z(_H[2]));}),_o]]],_F];default:return E(_F);}})(_C,_D);if(_E!=null){return _E;}}},_I=function(_J,_K){var _L=E(_J);return _L[0]==0?E(_K):[1,_L[1],new T(function(){return B(_I(_L[2],_K));})];},_M=function(_N){while(1){var _O=E(_N);if(!_O[0]){_N=[1,I_fromInt(_O[1])];continue;}else{return new F(function(){return I_toString(_O[1]);});}}},_P=function(_Q,_R){return new F(function(){return _I(fromJSStr(B(_M(_Q))),_R);});},_S=function(_T,_U){var _V=E(_T);if(!_V[0]){var _W=_V[1],_X=E(_U);return _X[0]==0?_W<_X[1]:I_compareInt(_X[1],_W)>0;}else{var _Y=_V[1],_Z=E(_U);return _Z[0]==0?I_compareInt(_Y,_Z[1])<0:I_compare(_Y,_Z[1])<0;}},_10=[0,41],_11=[0,40],_12=[0,0],_13=function(_14,_15,_16){return _14<=6?B(_P(_15,_16)):!B(_S(_15,_12))?B(_P(_15,_16)):[1,_11,new T(function(){return B(_I(fromJSStr(B(_M(_15))),[1,_10,_16]));})];},_17=function(_18,_19,_1a,_1b,_1c,_1d){return [1,[0,_b,[0,_18]],[1,[0,_9,[0,_19]],[1,[0,_7,[0,_1a]],[1,[0,_5,[1,new T(function(){return [0,toJSStr(B(_13(0,_1b,_o)))];})]],[1,[0,_3,[3,new T(function(){var _1e=E(_1c);if(!_1e[0]){var _1f=_1e[3],_1g=_1e[4],_1h=_1e[2]>=0?B(_r(new T(function(){return B(_r(_o,_1g));}),_1f)):B(_r(new T(function(){return B(_r(_o,_1f));}),_1g));}else{var _1h=B(_r(_o,_1e));}return _1h;})]],[1,[0,_1,[3,new T(function(){var _1i=E(_1d);if(!_1i[0]){var _1j=_1i[3],_1k=_1i[4],_1l=_1i[2]>=0?B(_B(new T(function(){return B(_B(_o,_1k));}),_1j)):B(_B(new T(function(){return B(_B(_o,_1j));}),_1k));}else{var _1l=B(_B(_o,_1i));}return _1l;})]],_o]]]]]];},_1m=function(_1n){var _1o=E(_1n);return [4,B(_17(_1o[1],_1o[2],_1o[3],_1o[4],_1o[7],_1o[8]))];},_1p=function(_1q,_1r){var _1s=E(_1r);return _1s[0]==0?[0]:[1,new T(function(){return B(A(_1q,[_1s[1]]));}),new T(function(){return B(_1p(_1q,_1s[2]));})];},_1t=function(_1u){return [3,new T(function(){return B(_1p(_1m,_1u));})];},_1v=function(_1w,_1x){var _1y=strEq(E(_1w)[1],E(_1x)[1]),_1z=_1y;return E(_1z)==0?true:false;},_1A=function(_1B,_1C){var _1D=strEq(E(_1B)[1],E(_1C)[1]),_1E=_1D;return E(_1E)==0?false:true;},_1F=[0,_1A,_1v],_1G=[1,_o],_1H=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_1I=[0,_1H],_1J=new T(function(){return B(unCStr("Tried to deserialize a non-array into a pair!"));}),_1K=[0,_1J],_1L=function(_1M){return E(E(_1M)[3]);},_1N=function(_1O,_1P,_1Q){var _1R=E(_1Q);if(_1R[0]==3){var _1S=E(_1R[1]);if(!_1S[0]){return E(_1K);}else{var _1T=E(_1S[2]);if(!_1T[0]){return E(_1K);}else{if(!E(_1T[2])[0]){var _1U=B(A(_1L,[_1O,_1S[1]]));if(!_1U[0]){return [0,_1U[1]];}else{var _1V=B(A(_1L,[_1P,_1T[1]]));return _1V[0]==0?[0,_1V[1]]:[1,[0,_1U[1],_1V[1]]];}}else{return E(_1K);}}}}else{return E(_1K);}},_1W=function(_1X,_1Y,_1Z){var _20=E(_1Z);if(_20[0]==3){var _21=function(_22){var _23=E(_22);if(!_23[0]){return E(_1G);}else{var _24=B(_1N(_1X,_1Y,_23[1]));if(!_24[0]){return [0,_24[1]];}else{var _25=B(_21(_23[2]));return _25[0]==0?[0,_25[1]]:[1,[1,_24[1],_25[1]]];}}};return new F(function(){return _21(_20[1]);});}else{return E(_1I);}},_26=new T(function(){return [0,"achievements"];}),_27=new T(function(){return B(unCStr("Tried to deserialize a non-JSString to a JSString"));}),_28=[0,_27],_29=new T(function(){return [0,"lastFocus"];}),_2a=new T(function(){return [0,"depend"];}),_2b=new T(function(){return [0,"lps"];}),_2c=new T(function(){return B(unCStr("Tried to deserialize a non-Number to a Double"));}),_2d=[0,_2c],_2e=new T(function(){return B(unCStr("Key not found"));}),_2f=[0,_2e],_2g=new T(function(){return [0,"loves"];}),_2h=new T(function(){return B(unCStr("Tried to do lookup on non-object!"));}),_2i=[0,_2h],_2j=[0,0],_2k=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_2l=new T(function(){return B(err(_2k));}),_2m=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_2n=new T(function(){return B(err(_2m));}),_2o=new T(function(){return B(unCStr("Control.Exception.Base"));}),_2p=new T(function(){return B(unCStr("base"));}),_2q=new T(function(){return B(unCStr("PatternMatchFail"));}),_2r=new T(function(){var _2s=hs_wordToWord64(18445595),_2t=_2s,_2u=hs_wordToWord64(52003073),_2v=_2u;return [0,_2t,_2v,[0,_2t,_2v,_2p,_2o,_2q],_o];}),_2w=function(_2x){return E(_2r);},_2y=function(_2z){return E(E(_2z)[1]);},_2A=function(_2B,_2C,_2D){var _2E=B(A(_2B,[_])),_2F=B(A(_2C,[_])),_2G=hs_eqWord64(_2E[1],_2F[1]),_2H=_2G;if(!E(_2H)){return [0];}else{var _2I=hs_eqWord64(_2E[2],_2F[2]),_2J=_2I;return E(_2J)==0?[0]:[1,_2D];}},_2K=function(_2L){var _2M=E(_2L);return new F(function(){return _2A(B(_2y(_2M[1])),_2w,_2M[2]);});},_2N=function(_2O){return E(E(_2O)[1]);},_2P=function(_2Q,_2R){return new F(function(){return _I(E(_2Q)[1],_2R);});},_2S=[0,44],_2T=[0,93],_2U=[0,91],_2V=function(_2W,_2X,_2Y){var _2Z=E(_2X);return _2Z[0]==0?B(unAppCStr("[]",_2Y)):[1,_2U,new T(function(){return B(A(_2W,[_2Z[1],new T(function(){var _30=function(_31){var _32=E(_31);return _32[0]==0?E([1,_2T,_2Y]):[1,_2S,new T(function(){return B(A(_2W,[_32[1],new T(function(){return B(_30(_32[2]));})]));})];};return B(_30(_2Z[2]));})]));})];},_33=function(_34,_35){return new F(function(){return _2V(_2P,_34,_35);});},_36=function(_37,_38,_39){return new F(function(){return _I(E(_38)[1],_39);});},_3a=[0,_36,_2N,_33],_3b=new T(function(){return [0,_2w,_3a,_3c,_2K];}),_3c=function(_3d){return [0,_3b,_3d];},_3e=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_3f=function(_3g,_3h){return new F(function(){return die(new T(function(){return B(A(_3h,[_3g]));}));});},_3i=function(_3j,_3k){var _3l=E(_3k);if(!_3l[0]){return [0,_o,_o];}else{var _3m=_3l[1];if(!B(A(_3j,[_3m]))){return [0,_o,_3l];}else{var _3n=new T(function(){var _3o=B(_3i(_3j,_3l[2]));return [0,_3o[1],_3o[2]];});return [0,[1,_3m,new T(function(){return E(E(_3n)[1]);})],new T(function(){return E(E(_3n)[2]);})];}}},_3p=[0,32],_3q=[0,10],_3r=[1,_3q,_o],_3s=function(_3t){return E(E(_3t)[1])==124?false:true;},_3u=function(_3v,_3w){var _3x=B(_3i(_3s,B(unCStr(_3v)))),_3y=_3x[1],_3z=function(_3A,_3B){return new F(function(){return _I(_3A,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_I(_3w,new T(function(){return B(_I(_3B,_3r));})));})));}));});},_3C=E(_3x[2]);if(!_3C[0]){return new F(function(){return _3z(_3y,_o);});}else{return E(E(_3C[1])[1])==124?B(_3z(_3y,[1,_3p,_3C[2]])):B(_3z(_3y,_o));}},_3D=function(_3E){return new F(function(){return _3f([0,new T(function(){return B(_3u(_3E,_3e));})],_3c);});},_3F=new T(function(){return B(_3D("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_3G=function(_3H,_3I){while(1){var _3J=(function(_3K,_3L){var _3M=E(_3K);switch(_3M[0]){case 0:var _3N=E(_3L);if(!_3N[0]){return [0];}else{_3H=B(A(_3M[1],[_3N[1]]));_3I=_3N[2];return null;}break;case 1:var _3O=B(A(_3M[1],[_3L])),_3P=_3L;_3H=_3O;_3I=_3P;return null;case 2:return [0];case 3:return [1,[0,_3M[1],_3L],new T(function(){return B(_3G(_3M[2],_3L));})];default:return E(_3M[1]);}})(_3H,_3I);if(_3J!=null){return _3J;}}},_3Q=function(_3R,_3S){var _3T=function(_3U){var _3V=E(_3S);if(_3V[0]==3){return [3,_3V[1],new T(function(){return B(_3Q(_3R,_3V[2]));})];}else{var _3W=E(_3R);if(_3W[0]==2){return E(_3V);}else{var _3X=E(_3V);if(_3X[0]==2){return E(_3W);}else{var _3Y=function(_3Z){var _40=E(_3X);if(_40[0]==4){return [1,function(_41){return [4,new T(function(){return B(_I(B(_3G(_3W,_41)),_40[1]));})];}];}else{var _42=E(_3W);if(_42[0]==1){var _43=_42[1],_44=E(_40);return _44[0]==0?[1,function(_45){return new F(function(){return _3Q(B(A(_43,[_45])),_44);});}]:[1,function(_46){return new F(function(){return _3Q(B(A(_43,[_46])),new T(function(){return B(A(_44[1],[_46]));}));});}];}else{var _47=E(_40);return _47[0]==0?E(_3F):[1,function(_48){return new F(function(){return _3Q(_42,new T(function(){return B(A(_47[1],[_48]));}));});}];}}},_49=E(_3W);switch(_49[0]){case 1:var _4a=E(_3X);if(_4a[0]==4){return [1,function(_4b){return [4,new T(function(){return B(_I(B(_3G(B(A(_49[1],[_4b])),_4b)),_4a[1]));})];}];}else{return new F(function(){return _3Y(_);});}break;case 4:var _4c=_49[1],_4d=E(_3X);switch(_4d[0]){case 0:return [1,function(_4e){return [4,new T(function(){return B(_I(_4c,new T(function(){return B(_3G(_4d,_4e));})));})];}];case 1:return [1,function(_4f){return [4,new T(function(){return B(_I(_4c,new T(function(){return B(_3G(B(A(_4d[1],[_4f])),_4f));})));})];}];default:return [4,new T(function(){return B(_I(_4c,_4d[1]));})];}break;default:return new F(function(){return _3Y(_);});}}}}},_4g=E(_3R);switch(_4g[0]){case 0:var _4h=E(_3S);if(!_4h[0]){return [0,function(_4i){return new F(function(){return _3Q(B(A(_4g[1],[_4i])),new T(function(){return B(A(_4h[1],[_4i]));}));});}];}else{return new F(function(){return _3T(_);});}break;case 3:return [3,_4g[1],new T(function(){return B(_3Q(_4g[2],_3S));})];default:return new F(function(){return _3T(_);});}},_4j=[0,41],_4k=[1,_4j,_o],_4l=[0,40],_4m=[1,_4l,_o],_4n=function(_4o,_4p){while(1){var _4q=E(_4o);if(!_4q[0]){return E(_4p)[0]==0?true:false;}else{var _4r=E(_4p);if(!_4r[0]){return false;}else{if(E(_4q[1])[1]!=E(_4r[1])[1]){return false;}else{_4o=_4q[2];_4p=_4r[2];continue;}}}}},_4s=function(_4t,_4u){return E(_4t)[1]!=E(_4u)[1];},_4v=function(_4w,_4x){return E(_4w)[1]==E(_4x)[1];},_4y=[0,_4v,_4s],_4z=function(_4A,_4B){while(1){var _4C=E(_4A);if(!_4C[0]){return E(_4B)[0]==0?true:false;}else{var _4D=E(_4B);if(!_4D[0]){return false;}else{if(E(_4C[1])[1]!=E(_4D[1])[1]){return false;}else{_4A=_4C[2];_4B=_4D[2];continue;}}}}},_4E=function(_4F,_4G){return !B(_4z(_4F,_4G))?true:false;},_4H=[0,_4z,_4E],_4I=function(_4J,_4K){var _4L=E(_4J);switch(_4L[0]){case 0:return [0,function(_4M){return new F(function(){return _4I(B(A(_4L[1],[_4M])),_4K);});}];case 1:return [1,function(_4N){return new F(function(){return _4I(B(A(_4L[1],[_4N])),_4K);});}];case 2:return [2];case 3:return new F(function(){return _3Q(B(A(_4K,[_4L[1]])),new T(function(){return B(_4I(_4L[2],_4K));}));});break;default:var _4O=function(_4P){var _4Q=E(_4P);if(!_4Q[0]){return [0];}else{var _4R=E(_4Q[1]);return new F(function(){return _I(B(_3G(B(A(_4K,[_4R[1]])),_4R[2])),new T(function(){return B(_4O(_4Q[2]));}));});}},_4S=B(_4O(_4L[1]));return _4S[0]==0?[2]:[4,_4S];}},_4T=[2],_4U=function(_4V){return [3,_4V,_4T];},_4W=0,_4X=function(_4Y,_4Z){var _50=E(_4Y);if(!_50){return new F(function(){return A(_4Z,[_4W]);});}else{return [0,function(_51){return E(new T(function(){return B(_4X(_50-1|0,_4Z));}));}];}},_52=function(_53,_54,_55){return function(_56){return new F(function(){return A(function(_57,_58,_59){while(1){var _5a=(function(_5b,_5c,_5d){var _5e=E(_5b);switch(_5e[0]){case 0:var _5f=E(_5c);if(!_5f[0]){return E(_54);}else{_57=B(A(_5e[1],[_5f[1]]));_58=_5f[2];var _5g=_5d+1|0;_59=_5g;return null;}break;case 1:var _5h=B(A(_5e[1],[_5c])),_5i=_5c,_5g=_5d;_57=_5h;_58=_5i;_59=_5g;return null;case 2:return E(_54);case 3:return function(_5j){return new F(function(){return _4X(_5d,function(_5k){return E(new T(function(){return B(_4I(_5e,_5j));}));});});};default:return function(_5l){return new F(function(){return _4I(_5e,_5l);});};}})(_57,_58,_59);if(_5a!=null){return _5a;}}},[new T(function(){return B(A(_53,[_4U]));}),_56,0,_55]);});};},_5m=function(_5n){return new F(function(){return A(_5n,[_o]);});},_5o=function(_5p,_5q){var _5r=function(_5s){var _5t=E(_5s);if(!_5t[0]){return E(_5m);}else{var _5u=_5t[1];return !B(A(_5p,[_5u]))?E(_5m):function(_5v){return [0,function(_5w){return E(new T(function(){return B(A(new T(function(){return B(_5r(_5t[2]));}),[function(_5x){return new F(function(){return A(_5v,[[1,_5u,_5x]]);});}]));}));}];};}};return function(_5y){return new F(function(){return A(_5r,[_5y,_5q]);});};},_5z=[6],_5A=function(_5B){return E(_5B);},_5C=new T(function(){return B(unCStr("valDig: Bad base"));}),_5D=new T(function(){return B(err(_5C));}),_5E=function(_5F,_5G){var _5H=function(_5I,_5J){var _5K=E(_5I);if(!_5K[0]){return function(_5L){return new F(function(){return A(_5L,[new T(function(){return B(A(_5J,[_o]));})]);});};}else{var _5M=E(_5K[1])[1],_5N=function(_5O){return function(_5P){return [0,function(_5Q){return E(new T(function(){return B(A(new T(function(){return B(_5H(_5K[2],function(_5R){return new F(function(){return A(_5J,[[1,_5O,_5R]]);});}));}),[_5P]));}));}];};};switch(E(E(_5F)[1])){case 8:if(48>_5M){return function(_5S){return new F(function(){return A(_5S,[new T(function(){return B(A(_5J,[_o]));})]);});};}else{if(_5M>55){return function(_5T){return new F(function(){return A(_5T,[new T(function(){return B(A(_5J,[_o]));})]);});};}else{return new F(function(){return _5N([0,_5M-48|0]);});}}break;case 10:if(48>_5M){return function(_5U){return new F(function(){return A(_5U,[new T(function(){return B(A(_5J,[_o]));})]);});};}else{if(_5M>57){return function(_5V){return new F(function(){return A(_5V,[new T(function(){return B(A(_5J,[_o]));})]);});};}else{return new F(function(){return _5N([0,_5M-48|0]);});}}break;case 16:if(48>_5M){if(97>_5M){if(65>_5M){return function(_5W){return new F(function(){return A(_5W,[new T(function(){return B(A(_5J,[_o]));})]);});};}else{if(_5M>70){return function(_5X){return new F(function(){return A(_5X,[new T(function(){return B(A(_5J,[_o]));})]);});};}else{return new F(function(){return _5N([0,(_5M-65|0)+10|0]);});}}}else{if(_5M>102){if(65>_5M){return function(_5Y){return new F(function(){return A(_5Y,[new T(function(){return B(A(_5J,[_o]));})]);});};}else{if(_5M>70){return function(_5Z){return new F(function(){return A(_5Z,[new T(function(){return B(A(_5J,[_o]));})]);});};}else{return new F(function(){return _5N([0,(_5M-65|0)+10|0]);});}}}else{return new F(function(){return _5N([0,(_5M-97|0)+10|0]);});}}}else{if(_5M>57){if(97>_5M){if(65>_5M){return function(_60){return new F(function(){return A(_60,[new T(function(){return B(A(_5J,[_o]));})]);});};}else{if(_5M>70){return function(_61){return new F(function(){return A(_61,[new T(function(){return B(A(_5J,[_o]));})]);});};}else{return new F(function(){return _5N([0,(_5M-65|0)+10|0]);});}}}else{if(_5M>102){if(65>_5M){return function(_62){return new F(function(){return A(_62,[new T(function(){return B(A(_5J,[_o]));})]);});};}else{if(_5M>70){return function(_63){return new F(function(){return A(_63,[new T(function(){return B(A(_5J,[_o]));})]);});};}else{return new F(function(){return _5N([0,(_5M-65|0)+10|0]);});}}}else{return new F(function(){return _5N([0,(_5M-97|0)+10|0]);});}}}else{return new F(function(){return _5N([0,_5M-48|0]);});}}break;default:return E(_5D);}}};return function(_64){return new F(function(){return A(_5H,[_64,_5A,function(_65){var _66=E(_65);return _66[0]==0?[2]:B(A(_5G,[_66]));}]);});};},_67=[0,10],_68=[0,1],_69=[0,2147483647],_6a=function(_6b,_6c){while(1){var _6d=E(_6b);if(!_6d[0]){var _6e=_6d[1],_6f=E(_6c);if(!_6f[0]){var _6g=_6f[1],_6h=addC(_6e,_6g);if(!E(_6h[2])){return [0,_6h[1]];}else{_6b=[1,I_fromInt(_6e)];_6c=[1,I_fromInt(_6g)];continue;}}else{_6b=[1,I_fromInt(_6e)];_6c=_6f;continue;}}else{var _6i=E(_6c);if(!_6i[0]){_6b=_6d;_6c=[1,I_fromInt(_6i[1])];continue;}else{return [1,I_add(_6d[1],_6i[1])];}}}},_6j=new T(function(){return B(_6a(_69,_68));}),_6k=function(_6l){var _6m=E(_6l);if(!_6m[0]){var _6n=E(_6m[1]);return _6n==(-2147483648)?E(_6j):[0, -_6n];}else{return [1,I_negate(_6m[1])];}},_6o=[0,10],_6p=[0,0],_6q=function(_6r){return [0,_6r];},_6s=function(_6t,_6u){while(1){var _6v=E(_6t);if(!_6v[0]){var _6w=_6v[1],_6x=E(_6u);if(!_6x[0]){var _6y=_6x[1];if(!(imul(_6w,_6y)|0)){return [0,imul(_6w,_6y)|0];}else{_6t=[1,I_fromInt(_6w)];_6u=[1,I_fromInt(_6y)];continue;}}else{_6t=[1,I_fromInt(_6w)];_6u=_6x;continue;}}else{var _6z=E(_6u);if(!_6z[0]){_6t=_6v;_6u=[1,I_fromInt(_6z[1])];continue;}else{return [1,I_mul(_6v[1],_6z[1])];}}}},_6A=function(_6B,_6C,_6D){while(1){var _6E=E(_6D);if(!_6E[0]){return E(_6C);}else{var _6F=B(_6a(B(_6s(_6C,_6B)),B(_6q(E(_6E[1])[1]))));_6D=_6E[2];_6C=_6F;continue;}}},_6G=function(_6H){var _6I=new T(function(){return B(_3Q(B(_3Q([0,function(_6J){return E(E(_6J)[1])==45?[1,B(_5E(_67,function(_6K){return new F(function(){return A(_6H,[[1,new T(function(){return B(_6k(B(_6A(_6o,_6p,_6K))));})]]);});}))]:[2];}],[0,function(_6L){return E(E(_6L)[1])==43?[1,B(_5E(_67,function(_6M){return new F(function(){return A(_6H,[[1,new T(function(){return B(_6A(_6o,_6p,_6M));})]]);});}))]:[2];}])),new T(function(){return [1,B(_5E(_67,function(_6N){return new F(function(){return A(_6H,[[1,new T(function(){return B(_6A(_6o,_6p,_6N));})]]);});}))];})));});return new F(function(){return _3Q([0,function(_6O){return E(E(_6O)[1])==101?E(_6I):[2];}],[0,function(_6P){return E(E(_6P)[1])==69?E(_6I):[2];}]);});},_6Q=[0],_6R=function(_6S){return new F(function(){return A(_6S,[_6Q]);});},_6T=function(_6U){return new F(function(){return A(_6U,[_6Q]);});},_6V=function(_6W){return function(_6X){return E(E(_6X)[1])==46?[1,B(_5E(_67,function(_6Y){return new F(function(){return A(_6W,[[1,_6Y]]);});}))]:[2];};},_6Z=function(_70){return [0,B(_6V(_70))];},_71=function(_72){return new F(function(){return _5E(_67,function(_73){return [1,B(_52(_6Z,_6R,function(_74){return [1,B(_52(_6G,_6T,function(_75){return new F(function(){return A(_72,[[5,[1,_73,_74,_75]]]);});}))];}))];});});},_76=function(_77){return [1,B(_71(_77))];},_78=function(_79){return E(E(_79)[1]);},_7a=function(_7b,_7c,_7d){while(1){var _7e=E(_7d);if(!_7e[0]){return false;}else{if(!B(A(_78,[_7b,_7c,_7e[1]]))){_7d=_7e[2];continue;}else{return true;}}}},_7f=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_7g=function(_7h){return new F(function(){return _7a(_4y,_7h,_7f);});},_7i=[0,8],_7j=[0,16],_7k=function(_7l){var _7m=function(_7n){return new F(function(){return A(_7l,[[5,[0,_7i,_7n]]]);});},_7o=function(_7p){return new F(function(){return A(_7l,[[5,[0,_7j,_7p]]]);});};return function(_7q){return E(E(_7q)[1])==48?E([0,function(_7r){switch(E(E(_7r)[1])){case 79:return [1,B(_5E(_7i,_7m))];case 88:return [1,B(_5E(_7j,_7o))];case 111:return [1,B(_5E(_7i,_7m))];case 120:return [1,B(_5E(_7j,_7o))];default:return [2];}}]):[2];};},_7s=function(_7t){return [0,B(_7k(_7t))];},_7u=function(_7v){var _7w=new T(function(){return B(A(_7v,[_7i]));}),_7x=new T(function(){return B(A(_7v,[_7j]));});return function(_7y){switch(E(E(_7y)[1])){case 79:return E(_7w);case 88:return E(_7x);case 111:return E(_7w);case 120:return E(_7x);default:return [2];}};},_7z=function(_7A){return [0,B(_7u(_7A))];},_7B=[0,92],_7C=function(_7D){return new F(function(){return A(_7D,[_67]);});},_7E=function(_7F,_7G){var _7H=jsShowI(_7F),_7I=_7H;return new F(function(){return _I(fromJSStr(_7I),_7G);});},_7J=function(_7K,_7L,_7M){if(_7L>=0){return new F(function(){return _7E(_7L,_7M);});}else{return _7K<=6?B(_7E(_7L,_7M)):[1,_11,new T(function(){var _7N=jsShowI(_7L),_7O=_7N;return B(_I(fromJSStr(_7O),[1,_10,_7M]));})];}},_7P=function(_7Q){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_7J(9,_7Q,_o));}))));});},_7R=function(_7S){var _7T=E(_7S);return _7T[0]==0?E(_7T[1]):I_toInt(_7T[1]);},_7U=function(_7V,_7W){var _7X=E(_7V);if(!_7X[0]){var _7Y=_7X[1],_7Z=E(_7W);return _7Z[0]==0?_7Y<=_7Z[1]:I_compareInt(_7Z[1],_7Y)>=0;}else{var _80=_7X[1],_81=E(_7W);return _81[0]==0?I_compareInt(_80,_81[1])<=0:I_compare(_80,_81[1])<=0;}},_82=function(_83){return [2];},_84=function(_85){var _86=E(_85);if(!_86[0]){return E(_82);}else{var _87=_86[1],_88=E(_86[2]);return _88[0]==0?E(_87):function(_89){return new F(function(){return _3Q(B(A(_87,[_89])),new T(function(){return B(A(new T(function(){return B(_84(_88));}),[_89]));}));});};}},_8a=function(_8b){return [2];},_8c=function(_8d,_8e){var _8f=function(_8g,_8h){var _8i=E(_8g);if(!_8i[0]){return function(_8j){return new F(function(){return A(_8j,[_8d]);});};}else{var _8k=E(_8h);return _8k[0]==0?E(_8a):E(_8i[1])[1]!=E(_8k[1])[1]?E(_8a):function(_8l){return [0,function(_8m){return E(new T(function(){return B(A(new T(function(){return B(_8f(_8i[2],_8k[2]));}),[_8l]));}));}];};}};return function(_8n){return new F(function(){return A(_8f,[_8d,_8n,_8e]);});};},_8o=new T(function(){return B(unCStr("SOH"));}),_8p=[0,1],_8q=function(_8r){return [1,B(_8c(_8o,function(_8s){return E(new T(function(){return B(A(_8r,[_8p]));}));}))];},_8t=new T(function(){return B(unCStr("SO"));}),_8u=[0,14],_8v=function(_8w){return [1,B(_8c(_8t,function(_8x){return E(new T(function(){return B(A(_8w,[_8u]));}));}))];},_8y=function(_8z){return [1,B(_52(_8q,_8v,_8z))];},_8A=new T(function(){return B(unCStr("NUL"));}),_8B=[0,0],_8C=function(_8D){return [1,B(_8c(_8A,function(_8E){return E(new T(function(){return B(A(_8D,[_8B]));}));}))];},_8F=new T(function(){return B(unCStr("STX"));}),_8G=[0,2],_8H=function(_8I){return [1,B(_8c(_8F,function(_8J){return E(new T(function(){return B(A(_8I,[_8G]));}));}))];},_8K=new T(function(){return B(unCStr("ETX"));}),_8L=[0,3],_8M=function(_8N){return [1,B(_8c(_8K,function(_8O){return E(new T(function(){return B(A(_8N,[_8L]));}));}))];},_8P=new T(function(){return B(unCStr("EOT"));}),_8Q=[0,4],_8R=function(_8S){return [1,B(_8c(_8P,function(_8T){return E(new T(function(){return B(A(_8S,[_8Q]));}));}))];},_8U=new T(function(){return B(unCStr("ENQ"));}),_8V=[0,5],_8W=function(_8X){return [1,B(_8c(_8U,function(_8Y){return E(new T(function(){return B(A(_8X,[_8V]));}));}))];},_8Z=new T(function(){return B(unCStr("ACK"));}),_90=[0,6],_91=function(_92){return [1,B(_8c(_8Z,function(_93){return E(new T(function(){return B(A(_92,[_90]));}));}))];},_94=new T(function(){return B(unCStr("BEL"));}),_95=[0,7],_96=function(_97){return [1,B(_8c(_94,function(_98){return E(new T(function(){return B(A(_97,[_95]));}));}))];},_99=new T(function(){return B(unCStr("BS"));}),_9a=[0,8],_9b=function(_9c){return [1,B(_8c(_99,function(_9d){return E(new T(function(){return B(A(_9c,[_9a]));}));}))];},_9e=new T(function(){return B(unCStr("HT"));}),_9f=[0,9],_9g=function(_9h){return [1,B(_8c(_9e,function(_9i){return E(new T(function(){return B(A(_9h,[_9f]));}));}))];},_9j=new T(function(){return B(unCStr("LF"));}),_9k=[0,10],_9l=function(_9m){return [1,B(_8c(_9j,function(_9n){return E(new T(function(){return B(A(_9m,[_9k]));}));}))];},_9o=new T(function(){return B(unCStr("VT"));}),_9p=[0,11],_9q=function(_9r){return [1,B(_8c(_9o,function(_9s){return E(new T(function(){return B(A(_9r,[_9p]));}));}))];},_9t=new T(function(){return B(unCStr("FF"));}),_9u=[0,12],_9v=function(_9w){return [1,B(_8c(_9t,function(_9x){return E(new T(function(){return B(A(_9w,[_9u]));}));}))];},_9y=new T(function(){return B(unCStr("CR"));}),_9z=[0,13],_9A=function(_9B){return [1,B(_8c(_9y,function(_9C){return E(new T(function(){return B(A(_9B,[_9z]));}));}))];},_9D=new T(function(){return B(unCStr("SI"));}),_9E=[0,15],_9F=function(_9G){return [1,B(_8c(_9D,function(_9H){return E(new T(function(){return B(A(_9G,[_9E]));}));}))];},_9I=new T(function(){return B(unCStr("DLE"));}),_9J=[0,16],_9K=function(_9L){return [1,B(_8c(_9I,function(_9M){return E(new T(function(){return B(A(_9L,[_9J]));}));}))];},_9N=new T(function(){return B(unCStr("DC1"));}),_9O=[0,17],_9P=function(_9Q){return [1,B(_8c(_9N,function(_9R){return E(new T(function(){return B(A(_9Q,[_9O]));}));}))];},_9S=new T(function(){return B(unCStr("DC2"));}),_9T=[0,18],_9U=function(_9V){return [1,B(_8c(_9S,function(_9W){return E(new T(function(){return B(A(_9V,[_9T]));}));}))];},_9X=new T(function(){return B(unCStr("DC3"));}),_9Y=[0,19],_9Z=function(_a0){return [1,B(_8c(_9X,function(_a1){return E(new T(function(){return B(A(_a0,[_9Y]));}));}))];},_a2=new T(function(){return B(unCStr("DC4"));}),_a3=[0,20],_a4=function(_a5){return [1,B(_8c(_a2,function(_a6){return E(new T(function(){return B(A(_a5,[_a3]));}));}))];},_a7=new T(function(){return B(unCStr("NAK"));}),_a8=[0,21],_a9=function(_aa){return [1,B(_8c(_a7,function(_ab){return E(new T(function(){return B(A(_aa,[_a8]));}));}))];},_ac=new T(function(){return B(unCStr("SYN"));}),_ad=[0,22],_ae=function(_af){return [1,B(_8c(_ac,function(_ag){return E(new T(function(){return B(A(_af,[_ad]));}));}))];},_ah=new T(function(){return B(unCStr("ETB"));}),_ai=[0,23],_aj=function(_ak){return [1,B(_8c(_ah,function(_al){return E(new T(function(){return B(A(_ak,[_ai]));}));}))];},_am=new T(function(){return B(unCStr("CAN"));}),_an=[0,24],_ao=function(_ap){return [1,B(_8c(_am,function(_aq){return E(new T(function(){return B(A(_ap,[_an]));}));}))];},_ar=new T(function(){return B(unCStr("EM"));}),_as=[0,25],_at=function(_au){return [1,B(_8c(_ar,function(_av){return E(new T(function(){return B(A(_au,[_as]));}));}))];},_aw=new T(function(){return B(unCStr("SUB"));}),_ax=[0,26],_ay=function(_az){return [1,B(_8c(_aw,function(_aA){return E(new T(function(){return B(A(_az,[_ax]));}));}))];},_aB=new T(function(){return B(unCStr("ESC"));}),_aC=[0,27],_aD=function(_aE){return [1,B(_8c(_aB,function(_aF){return E(new T(function(){return B(A(_aE,[_aC]));}));}))];},_aG=new T(function(){return B(unCStr("FS"));}),_aH=[0,28],_aI=function(_aJ){return [1,B(_8c(_aG,function(_aK){return E(new T(function(){return B(A(_aJ,[_aH]));}));}))];},_aL=new T(function(){return B(unCStr("GS"));}),_aM=[0,29],_aN=function(_aO){return [1,B(_8c(_aL,function(_aP){return E(new T(function(){return B(A(_aO,[_aM]));}));}))];},_aQ=new T(function(){return B(unCStr("RS"));}),_aR=[0,30],_aS=function(_aT){return [1,B(_8c(_aQ,function(_aU){return E(new T(function(){return B(A(_aT,[_aR]));}));}))];},_aV=new T(function(){return B(unCStr("US"));}),_aW=[0,31],_aX=function(_aY){return [1,B(_8c(_aV,function(_aZ){return E(new T(function(){return B(A(_aY,[_aW]));}));}))];},_b0=new T(function(){return B(unCStr("SP"));}),_b1=[0,32],_b2=function(_b3){return [1,B(_8c(_b0,function(_b4){return E(new T(function(){return B(A(_b3,[_b1]));}));}))];},_b5=new T(function(){return B(unCStr("DEL"));}),_b6=[0,127],_b7=function(_b8){return [1,B(_8c(_b5,function(_b9){return E(new T(function(){return B(A(_b8,[_b6]));}));}))];},_ba=[1,_b7,_o],_bb=[1,_b2,_ba],_bc=[1,_aX,_bb],_bd=[1,_aS,_bc],_be=[1,_aN,_bd],_bf=[1,_aI,_be],_bg=[1,_aD,_bf],_bh=[1,_ay,_bg],_bi=[1,_at,_bh],_bj=[1,_ao,_bi],_bk=[1,_aj,_bj],_bl=[1,_ae,_bk],_bm=[1,_a9,_bl],_bn=[1,_a4,_bm],_bo=[1,_9Z,_bn],_bp=[1,_9U,_bo],_bq=[1,_9P,_bp],_br=[1,_9K,_bq],_bs=[1,_9F,_br],_bt=[1,_9A,_bs],_bu=[1,_9v,_bt],_bv=[1,_9q,_bu],_bw=[1,_9l,_bv],_bx=[1,_9g,_bw],_by=[1,_9b,_bx],_bz=[1,_96,_by],_bA=[1,_91,_bz],_bB=[1,_8W,_bA],_bC=[1,_8R,_bB],_bD=[1,_8M,_bC],_bE=[1,_8H,_bD],_bF=[1,_8C,_bE],_bG=[1,_8y,_bF],_bH=new T(function(){return B(_84(_bG));}),_bI=[0,1114111],_bJ=[0,34],_bK=[0,39],_bL=function(_bM){var _bN=new T(function(){return B(A(_bM,[_95]));}),_bO=new T(function(){return B(A(_bM,[_9a]));}),_bP=new T(function(){return B(A(_bM,[_9f]));}),_bQ=new T(function(){return B(A(_bM,[_9k]));}),_bR=new T(function(){return B(A(_bM,[_9p]));}),_bS=new T(function(){return B(A(_bM,[_9u]));}),_bT=new T(function(){return B(A(_bM,[_9z]));});return new F(function(){return _3Q([0,function(_bU){switch(E(E(_bU)[1])){case 34:return E(new T(function(){return B(A(_bM,[_bJ]));}));case 39:return E(new T(function(){return B(A(_bM,[_bK]));}));case 92:return E(new T(function(){return B(A(_bM,[_7B]));}));case 97:return E(_bN);case 98:return E(_bO);case 102:return E(_bS);case 110:return E(_bQ);case 114:return E(_bT);case 116:return E(_bP);case 118:return E(_bR);default:return [2];}}],new T(function(){return B(_3Q([1,B(_52(_7z,_7C,function(_bV){return [1,B(_5E(_bV,function(_bW){var _bX=B(_6A(new T(function(){return B(_6q(E(_bV)[1]));}),_6p,_bW));return !B(_7U(_bX,_bI))?[2]:B(A(_bM,[new T(function(){var _bY=B(_7R(_bX));if(_bY>>>0>1114111){var _bZ=B(_7P(_bY));}else{var _bZ=[0,_bY];}var _c0=_bZ,_c1=_c0,_c2=_c1;return _c2;})]));}))];}))],new T(function(){return B(_3Q([0,function(_c3){return E(E(_c3)[1])==94?E([0,function(_c4){switch(E(E(_c4)[1])){case 64:return E(new T(function(){return B(A(_bM,[_8B]));}));case 65:return E(new T(function(){return B(A(_bM,[_8p]));}));case 66:return E(new T(function(){return B(A(_bM,[_8G]));}));case 67:return E(new T(function(){return B(A(_bM,[_8L]));}));case 68:return E(new T(function(){return B(A(_bM,[_8Q]));}));case 69:return E(new T(function(){return B(A(_bM,[_8V]));}));case 70:return E(new T(function(){return B(A(_bM,[_90]));}));case 71:return E(_bN);case 72:return E(_bO);case 73:return E(_bP);case 74:return E(_bQ);case 75:return E(_bR);case 76:return E(_bS);case 77:return E(_bT);case 78:return E(new T(function(){return B(A(_bM,[_8u]));}));case 79:return E(new T(function(){return B(A(_bM,[_9E]));}));case 80:return E(new T(function(){return B(A(_bM,[_9J]));}));case 81:return E(new T(function(){return B(A(_bM,[_9O]));}));case 82:return E(new T(function(){return B(A(_bM,[_9T]));}));case 83:return E(new T(function(){return B(A(_bM,[_9Y]));}));case 84:return E(new T(function(){return B(A(_bM,[_a3]));}));case 85:return E(new T(function(){return B(A(_bM,[_a8]));}));case 86:return E(new T(function(){return B(A(_bM,[_ad]));}));case 87:return E(new T(function(){return B(A(_bM,[_ai]));}));case 88:return E(new T(function(){return B(A(_bM,[_an]));}));case 89:return E(new T(function(){return B(A(_bM,[_as]));}));case 90:return E(new T(function(){return B(A(_bM,[_ax]));}));case 91:return E(new T(function(){return B(A(_bM,[_aC]));}));case 92:return E(new T(function(){return B(A(_bM,[_aH]));}));case 93:return E(new T(function(){return B(A(_bM,[_aM]));}));case 94:return E(new T(function(){return B(A(_bM,[_aR]));}));case 95:return E(new T(function(){return B(A(_bM,[_aW]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_bH,[_bM]));})));})));}));});},_c5=function(_c6){return new F(function(){return A(_c6,[_4W]);});},_c7=function(_c8){var _c9=E(_c8);if(!_c9[0]){return E(_c5);}else{var _ca=_c9[2],_cb=E(E(_c9[1])[1]);switch(_cb){case 9:return function(_cc){return [0,function(_cd){return E(new T(function(){return B(A(new T(function(){return B(_c7(_ca));}),[_cc]));}));}];};case 10:return function(_ce){return [0,function(_cf){return E(new T(function(){return B(A(new T(function(){return B(_c7(_ca));}),[_ce]));}));}];};case 11:return function(_cg){return [0,function(_ch){return E(new T(function(){return B(A(new T(function(){return B(_c7(_ca));}),[_cg]));}));}];};case 12:return function(_ci){return [0,function(_cj){return E(new T(function(){return B(A(new T(function(){return B(_c7(_ca));}),[_ci]));}));}];};case 13:return function(_ck){return [0,function(_cl){return E(new T(function(){return B(A(new T(function(){return B(_c7(_ca));}),[_ck]));}));}];};case 32:return function(_cm){return [0,function(_cn){return E(new T(function(){return B(A(new T(function(){return B(_c7(_ca));}),[_cm]));}));}];};case 160:return function(_co){return [0,function(_cp){return E(new T(function(){return B(A(new T(function(){return B(_c7(_ca));}),[_co]));}));}];};default:var _cq=u_iswspace(_cb),_cr=_cq;return E(_cr)==0?E(_c5):function(_cs){return [0,function(_ct){return E(new T(function(){return B(A(new T(function(){return B(_c7(_ca));}),[_cs]));}));}];};}}},_cu=function(_cv){var _cw=new T(function(){return B(_cu(_cv));}),_cx=[1,function(_cy){return new F(function(){return A(_c7,[_cy,function(_cz){return E([0,function(_cA){return E(E(_cA)[1])==92?E(_cw):[2];}]);}]);});}];return new F(function(){return _3Q([0,function(_cB){return E(E(_cB)[1])==92?E([0,function(_cC){var _cD=E(E(_cC)[1]);switch(_cD){case 9:return E(_cx);case 10:return E(_cx);case 11:return E(_cx);case 12:return E(_cx);case 13:return E(_cx);case 32:return E(_cx);case 38:return E(_cw);case 160:return E(_cx);default:var _cE=u_iswspace(_cD),_cF=_cE;return E(_cF)==0?[2]:E(_cx);}}]):[2];}],[0,function(_cG){var _cH=E(_cG);return E(_cH[1])==92?E(new T(function(){return B(_bL(function(_cI){return new F(function(){return A(_cv,[[0,_cI,_h]]);});}));})):B(A(_cv,[[0,_cH,_l]]));}]);});},_cJ=function(_cK,_cL){return new F(function(){return _cu(function(_cM){var _cN=E(_cM),_cO=E(_cN[1]);if(E(_cO[1])==34){if(!E(_cN[2])){return E(new T(function(){return B(A(_cL,[[1,new T(function(){return B(A(_cK,[_o]));})]]));}));}else{return new F(function(){return _cJ(function(_cP){return new F(function(){return A(_cK,[[1,_cO,_cP]]);});},_cL);});}}else{return new F(function(){return _cJ(function(_cQ){return new F(function(){return A(_cK,[[1,_cO,_cQ]]);});},_cL);});}});});},_cR=new T(function(){return B(unCStr("_\'"));}),_cS=function(_cT){var _cU=u_iswalnum(_cT),_cV=_cU;return E(_cV)==0?B(_7a(_4y,[0,_cT],_cR)):true;},_cW=function(_cX){return new F(function(){return _cS(E(_cX)[1]);});},_cY=new T(function(){return B(unCStr(",;()[]{}`"));}),_cZ=new T(function(){return B(unCStr(".."));}),_d0=new T(function(){return B(unCStr("::"));}),_d1=new T(function(){return B(unCStr("->"));}),_d2=[0,64],_d3=[1,_d2,_o],_d4=[0,126],_d5=[1,_d4,_o],_d6=new T(function(){return B(unCStr("=>"));}),_d7=[1,_d6,_o],_d8=[1,_d5,_d7],_d9=[1,_d3,_d8],_da=[1,_d1,_d9],_db=new T(function(){return B(unCStr("<-"));}),_dc=[1,_db,_da],_dd=[0,124],_de=[1,_dd,_o],_df=[1,_de,_dc],_dg=[1,_7B,_o],_dh=[1,_dg,_df],_di=[0,61],_dj=[1,_di,_o],_dk=[1,_dj,_dh],_dl=[1,_d0,_dk],_dm=[1,_cZ,_dl],_dn=function(_do){return new F(function(){return _3Q([1,function(_dp){return E(_dp)[0]==0?E(new T(function(){return B(A(_do,[_5z]));})):[2];}],new T(function(){return B(_3Q([0,function(_dq){return E(E(_dq)[1])==39?E([0,function(_dr){var _ds=E(_dr);switch(E(_ds[1])){case 39:return [2];case 92:return E(new T(function(){return B(_bL(function(_dt){return [0,function(_du){return E(E(_du)[1])==39?E(new T(function(){return B(A(_do,[[0,_dt]]));})):[2];}];}));}));default:return [0,function(_dv){return E(E(_dv)[1])==39?E(new T(function(){return B(A(_do,[[0,_ds]]));})):[2];}];}}]):[2];}],new T(function(){return B(_3Q([0,function(_dw){return E(E(_dw)[1])==34?E(new T(function(){return B(_cJ(_5A,_do));})):[2];}],new T(function(){return B(_3Q([0,function(_dx){return !B(_7a(_4y,_dx,_cY))?[2]:B(A(_do,[[2,[1,_dx,_o]]]));}],new T(function(){return B(_3Q([0,function(_dy){return !B(_7a(_4y,_dy,_7f))?[2]:[1,B(_5o(_7g,function(_dz){var _dA=[1,_dy,_dz];return !B(_7a(_4H,_dA,_dm))?B(A(_do,[[4,_dA]])):B(A(_do,[[2,_dA]]));}))];}],new T(function(){return B(_3Q([0,function(_dB){var _dC=E(_dB),_dD=_dC[1],_dE=u_iswalpha(_dD),_dF=_dE;return E(_dF)==0?E(_dD)==95?[1,B(_5o(_cW,function(_dG){return new F(function(){return A(_do,[[3,[1,_dC,_dG]]]);});}))]:[2]:[1,B(_5o(_cW,function(_dH){return new F(function(){return A(_do,[[3,[1,_dC,_dH]]]);});}))];}],new T(function(){return [1,B(_52(_7s,_76,_do))];})));})));})));})));})));}));});},_dI=[0,0],_dJ=function(_dK,_dL){return function(_dM){return new F(function(){return A(_c7,[_dM,function(_dN){return E(new T(function(){return B(_dn(function(_dO){var _dP=E(_dO);return _dP[0]==2?!B(_4n(_dP[1],_4m))?[2]:E(new T(function(){return B(A(_dK,[_dI,function(_dQ){return [1,function(_dR){return new F(function(){return A(_c7,[_dR,function(_dS){return E(new T(function(){return B(_dn(function(_dT){var _dU=E(_dT);return _dU[0]==2?!B(_4n(_dU[1],_4k))?[2]:E(new T(function(){return B(A(_dL,[_dQ]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_dV=function(_dW,_dX,_dY){var _dZ=function(_e0,_e1){return new F(function(){return _3Q([1,function(_e2){return new F(function(){return A(_c7,[_e2,function(_e3){return E(new T(function(){return B(_dn(function(_e4){var _e5=E(_e4);if(_e5[0]==4){var _e6=E(_e5[1]);if(!_e6[0]){return new F(function(){return A(_dW,[_e5,_e0,_e1]);});}else{return E(E(_e6[1])[1])==45?E(_e6[2])[0]==0?E([1,function(_e7){return new F(function(){return A(_c7,[_e7,function(_e8){return E(new T(function(){return B(_dn(function(_e9){return new F(function(){return A(_dW,[_e9,_e0,function(_ea){return new F(function(){return A(_e1,[new T(function(){return B(_6k(_ea));})]);});}]);});}));}));}]);});}]):B(A(_dW,[_e5,_e0,_e1])):B(A(_dW,[_e5,_e0,_e1]));}}else{return new F(function(){return A(_dW,[_e5,_e0,_e1]);});}}));}));}]);});}],new T(function(){return [1,B(_dJ(_dZ,_e1))];}));});};return new F(function(){return _dZ(_dX,_dY);});},_eb=function(_ec,_ed){return [2];},_ee=function(_ef){var _eg=E(_ef);return _eg[0]==0?[1,new T(function(){return B(_6A(new T(function(){return B(_6q(E(_eg[1])[1]));}),_6p,_eg[2]));})]:E(_eg[2])[0]==0?E(_eg[3])[0]==0?[1,new T(function(){return B(_6A(_6o,_6p,_eg[1]));})]:[0]:[0];},_eh=function(_ei){var _ej=E(_ei);if(_ej[0]==5){var _ek=B(_ee(_ej[1]));return _ek[0]==0?E(_eb):function(_el,_em){return new F(function(){return A(_em,[_ek[1]]);});};}else{return E(_eb);}},_en=function(_eo){return [1,function(_ep){return new F(function(){return A(_c7,[_ep,function(_eq){return E([3,_eo,_4T]);}]);});}];},_er=new T(function(){return B(_dV(_eh,_dI,_en));}),_es=new T(function(){return [0,"items"];}),_et=new T(function(){return B(unCStr("Tried to deserialize long string to a Char"));}),_eu=[0,_et],_ev=new T(function(){return B(unCStr("Tried to deserialize a non-string to a Char"));}),_ew=[0,_ev],_ex=function(_ey){var _ez=E(_ey);if(_ez[0]==1){var _eA=fromJSStr(E(_ez[1])[1]);return _eA[0]==0?E(_eu):E(_eA[2])[0]==0?[1,_eA[1]]:E(_eu);}else{return E(_ew);}},_eB=[0,_27],_eC=function(_eD){return new F(function(){return fromJSStr(E(_eD)[1]);});},_eE=function(_eF){var _eG=E(_eF);return _eG[0]==1?[1,new T(function(){return B(_eC(_eG[1]));})]:E(_eB);},_eH=function(_eI){return [1,new T(function(){return [0,toJSStr([1,_eI,_o])];})];},_eJ=[0,_eH,_e,_ex,_eE],_eK=function(_eL){return E(E(_eL)[2]);},_eM=function(_eN,_eO){return [3,new T(function(){return B(_1p(new T(function(){return B(_eK(_eN));}),_eO));})];},_eP=[1,_o],_eQ=[0,_1H],_eR=function(_eS){return E(E(_eS)[4]);},_eT=function(_eU,_eV){var _eW=E(_eV);if(_eW[0]==3){var _eX=function(_eY){var _eZ=E(_eY);if(!_eZ[0]){return E(_eP);}else{var _f0=B(A(new T(function(){return B(_eR(_eU));}),[_eZ[1]]));if(!_f0[0]){return [0,_f0[1]];}else{var _f1=B(_eX(_eZ[2]));return _f1[0]==0?[0,_f1[1]]:[1,[1,_f0[1],_f1[1]]];}}};return new F(function(){return _eX(_eW[1]);});}else{return E(_eQ);}},_f2=function(_f3){return [0,new T(function(){return B(_eK(_f3));}),function(_f4){return new F(function(){return _eM(_f3,_f4);});},new T(function(){return B(_eR(_f3));}),function(_f4){return new F(function(){return _eT(_f3,_f4);});}];},_f5=new T(function(){return B(_f2(_eJ));}),_f6=function(_f7){return E(E(_f7)[1]);},_f8=function(_f9,_fa){var _fb=E(_fa);return _fb[0]==0?E(_q):[4,[1,_k,[1,[0,_g,new T(function(){return B(A(_f6,[_f9,_fb[1]]));})],_o]]];},_fc=function(_fd,_fe){return [3,new T(function(){return B(_1p(function(_f4){return new F(function(){return _f8(_fd,_f4);});},_fe));})];},_ff=[1,_6Q],_fg=new T(function(){return B(unCStr("Tried to deserialize a non-Bool to a Bool"));}),_fh=[0,_fg],_fi=[0,_2e],_fj=[0,_2h],_fk=function(_fl,_fm,_fn){while(1){var _fo=E(_fn);if(!_fo[0]){return [0];}else{var _fp=E(_fo[1]);if(!B(A(_78,[_fl,_fm,_fp[1]]))){_fn=_fo[2];continue;}else{return [1,_fp[2]];}}}},_fq=function(_fr,_fs){var _ft=E(_fs);if(_ft[0]==4){var _fu=_ft[1],_fv=B(_fk(_1F,_j,_fu));if(!_fv[0]){return E(_fi);}else{var _fw=E(_fv[1]);if(_fw[0]==2){if(!E(_fw[1])){return E(_ff);}else{var _fx=B(_fk(_1F,_g,_fu));if(!_fx[0]){return E(_fi);}else{var _fy=B(A(_1L,[_fr,_fx[1]]));return _fy[0]==0?[0,_fy[1]]:[1,[1,_fy[1]]];}}}else{return E(_fh);}}}else{return E(_fj);}},_fz=[1,_o],_fA=[0,_1H],_fB=function(_fC,_fD){var _fE=E(_fD);if(_fE[0]==3){var _fF=function(_fG){var _fH=E(_fG);if(!_fH[0]){return E(_fz);}else{var _fI=B(_fq(_fC,_fH[1]));if(!_fI[0]){return [0,_fI[1]];}else{var _fJ=B(_fF(_fH[2]));return _fJ[0]==0?[0,_fJ[1]]:[1,[1,_fI[1],_fJ[1]]];}}};return new F(function(){return _fF(_fE[1]);});}else{return E(_fA);}},_fK=function(_fL){return [0,function(_f4){return new F(function(){return _f8(_fL,_f4);});},function(_f4){return new F(function(){return _fc(_fL,_f4);});},function(_f4){return new F(function(){return _fq(_fL,_f4);});},function(_f4){return new F(function(){return _fB(_fL,_f4);});}];},_fM=new T(function(){return B(_fK(_f5));}),_fN=function(_fO){return [3,new T(function(){return B(_1p(_z,_fO));})];},_fP=new T(function(){return B(unCStr("The given Number can\'t be represented as an Int"));}),_fQ=[0,_fP],_fR=new T(function(){return B(unCStr("Tried to deserialize a non-Number to an Int"));}),_fS=[0,_fR],_fT=function(_fU){var _fV=E(_fU);if(!_fV[0]){var _fW=E(_fV[1])[1],_fX=_fW&4294967295;return _fX!=_fW?E(_fQ):[1,[0,_fX]];}else{return E(_fS);}},_fY=[0,_1H],_fZ=[1,_o],_g0=[0,_fP],_g1=[0,_fR],_g2=function(_g3){var _g4=E(_g3);if(!_g4[0]){return E(_fZ);}else{var _g5=E(_g4[1]);if(!_g5[0]){var _g6=E(_g5[1])[1],_g7=_g6&4294967295;if(_g7!=_g6){return E(_g0);}else{var _g8=B(_g2(_g4[2]));return _g8[0]==0?[0,_g8[1]]:[1,[1,[0,_g7],_g8[1]]];}}else{return E(_g1);}}},_g9=function(_ga){var _gb=E(_ga);return _gb[0]==3?B(_g2(_gb[1])):E(_fY);},_gc=[0,_z,_fN,_fT,_g9],_gd=[2],_ge=function(_gf,_gg,_gh){var _gi=E(_gh);switch(_gi[0]){case 0:var _gj=_gi[1],_gk=_gi[2],_gl=_gi[3],_gm=_gi[4],_gn=_gk>>>0;if(((_gf>>>0&((_gn-1>>>0^4294967295)>>>0^_gn)>>>0)>>>0&4294967295)==_gj){return (_gf>>>0&_gn)>>>0==0?[0,_gj,_gk,E(B(_ge(_gf,_gg,_gl))),E(_gm)]:[0,_gj,_gk,E(_gl),E(B(_ge(_gf,_gg,_gm)))];}else{var _go=(_gf>>>0^_gj>>>0)>>>0,_gp=(_go|_go>>>1)>>>0,_gq=(_gp|_gp>>>2)>>>0,_gr=(_gq|_gq>>>4)>>>0,_gs=(_gr|_gr>>>8)>>>0,_gt=(_gs|_gs>>>16)>>>0,_gu=(_gt^_gt>>>1)>>>0&4294967295,_gv=_gu>>>0;return (_gf>>>0&_gv)>>>0==0?[0,(_gf>>>0&((_gv-1>>>0^4294967295)>>>0^_gv)>>>0)>>>0&4294967295,_gu,E([1,_gf,_gg]),E(_gi)]:[0,(_gf>>>0&((_gv-1>>>0^4294967295)>>>0^_gv)>>>0)>>>0&4294967295,_gu,E(_gi),E([1,_gf,_gg])];}break;case 1:var _gw=_gi[1];if(_gf!=_gw){var _gx=(_gf>>>0^_gw>>>0)>>>0,_gy=(_gx|_gx>>>1)>>>0,_gz=(_gy|_gy>>>2)>>>0,_gA=(_gz|_gz>>>4)>>>0,_gB=(_gA|_gA>>>8)>>>0,_gC=(_gB|_gB>>>16)>>>0,_gD=(_gC^_gC>>>1)>>>0&4294967295,_gE=_gD>>>0;return (_gf>>>0&_gE)>>>0==0?[0,(_gf>>>0&((_gE-1>>>0^4294967295)>>>0^_gE)>>>0)>>>0&4294967295,_gD,E([1,_gf,_gg]),E(_gi)]:[0,(_gf>>>0&((_gE-1>>>0^4294967295)>>>0^_gE)>>>0)>>>0&4294967295,_gD,E(_gi),E([1,_gf,_gg])];}else{return [1,_gf,_gg];}break;default:return [1,_gf,_gg];}},_gF=function(_gG,_gH){while(1){var _gI=E(_gH);if(!_gI[0]){return E(_gG);}else{var _gJ=E(_gI[1]),_gK=B(_ge(E(_gJ[1])[1],_gJ[2],_gG));_gH=_gI[2];_gG=_gK;continue;}}},_gL=function(_gM){return new F(function(){return _gF(_gd,_gM);});},_gN=function(_gO){while(1){var _gP=(function(_gQ){var _gR=E(_gQ);if(!_gR[0]){return [0];}else{var _gS=_gR[2],_gT=E(_gR[1]);if(!E(_gT[2])[0]){return [1,_gT[1],new T(function(){return B(_gN(_gS));})];}else{_gO=_gS;return null;}}})(_gO);if(_gP!=null){return _gP;}}},_gU=function(_gV){var _gW=E(_gV);if(_gW[0]==4){var _gX=_gW[1],_gY=B(_fk(_1F,_2g,_gX));if(!_gY[0]){return E(_2f);}else{var _gZ=E(_gY[1]);if(!_gZ[0]){var _h0=B(_fk(_1F,_2b,_gX));if(!_h0[0]){return E(_2f);}else{var _h1=E(_h0[1]);if(!_h1[0]){var _h2=B(_fk(_1F,_2a,_gX));if(!_h2[0]){return E(_2f);}else{var _h3=E(_h2[1]);if(!_h3[0]){var _h4=B(_fk(_1F,_29,_gX));if(!_h4[0]){return E(_2f);}else{var _h5=E(_h4[1]);if(_h5[0]==1){var _h6=B(_fk(_1F,_26,_gX));if(!_h6[0]){return E(_2f);}else{var _h7=B(_1W(_gc,_fM,_h6[1]));if(!_h7[0]){return [0,_h7[1]];}else{var _h8=B(_fk(_1F,_es,_gX));if(!_h8[0]){return E(_2f);}else{var _h9=B(_1W(_gc,_gc,_h8[1]));return _h9[0]==0?[0,_h9[1]]:[1,[0,_gZ[1],_h1[1],_h3[1],new T(function(){var _ha=B(_gN(B(_3G(_er,new T(function(){return fromJSStr(E(_h5[1])[1]);})))));return _ha[0]==0?E(_2n):E(_ha[2])[0]==0?E(_ha[1]):E(_2l);}),_2j,_l,new T(function(){return B(_gL(_h7[1]));}),new T(function(){return B(_gL(_h9[1]));})]];}}}}else{return E(_28);}}}else{return E(_2d);}}}else{return E(_2d);}}}else{return E(_2d);}}}else{return E(_2i);}},_hb=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_hc=[0,_hb],_hd=[1,_o],_he=function(_hf){var _hg=E(_hf);if(!_hg[0]){return E(_hd);}else{var _hh=B(_gU(_hg[1]));if(!_hh[0]){return [0,_hh[1]];}else{var _hi=B(_he(_hg[2]));return _hi[0]==0?[0,_hi[1]]:[1,[1,_hh[1],_hi[1]]];}}},_hj=function(_hk){var _hl=E(_hk);return _hl[0]==3?B(_he(_hl[1])):E(_hc);},_hm=[0,_1m,_1t,_gU,_hj],_hn=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_ho=new T(function(){return B(err(_hn));}),_hp=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_hq=new T(function(){return B(err(_hp));}),_hr=function(_hs,_ht){while(1){var _hu=E(_hs);if(!_hu[0]){return E(_hq);}else{var _hv=E(_ht);if(!_hv){return E(_hu[1]);}else{_hs=_hu[2];_ht=_hv-1|0;continue;}}}},_hw=new T(function(){return B(unCStr("ACK"));}),_hx=new T(function(){return B(unCStr("BEL"));}),_hy=new T(function(){return B(unCStr("BS"));}),_hz=new T(function(){return B(unCStr("SP"));}),_hA=[1,_hz,_o],_hB=new T(function(){return B(unCStr("US"));}),_hC=[1,_hB,_hA],_hD=new T(function(){return B(unCStr("RS"));}),_hE=[1,_hD,_hC],_hF=new T(function(){return B(unCStr("GS"));}),_hG=[1,_hF,_hE],_hH=new T(function(){return B(unCStr("FS"));}),_hI=[1,_hH,_hG],_hJ=new T(function(){return B(unCStr("ESC"));}),_hK=[1,_hJ,_hI],_hL=new T(function(){return B(unCStr("SUB"));}),_hM=[1,_hL,_hK],_hN=new T(function(){return B(unCStr("EM"));}),_hO=[1,_hN,_hM],_hP=new T(function(){return B(unCStr("CAN"));}),_hQ=[1,_hP,_hO],_hR=new T(function(){return B(unCStr("ETB"));}),_hS=[1,_hR,_hQ],_hT=new T(function(){return B(unCStr("SYN"));}),_hU=[1,_hT,_hS],_hV=new T(function(){return B(unCStr("NAK"));}),_hW=[1,_hV,_hU],_hX=new T(function(){return B(unCStr("DC4"));}),_hY=[1,_hX,_hW],_hZ=new T(function(){return B(unCStr("DC3"));}),_i0=[1,_hZ,_hY],_i1=new T(function(){return B(unCStr("DC2"));}),_i2=[1,_i1,_i0],_i3=new T(function(){return B(unCStr("DC1"));}),_i4=[1,_i3,_i2],_i5=new T(function(){return B(unCStr("DLE"));}),_i6=[1,_i5,_i4],_i7=new T(function(){return B(unCStr("SI"));}),_i8=[1,_i7,_i6],_i9=new T(function(){return B(unCStr("SO"));}),_ia=[1,_i9,_i8],_ib=new T(function(){return B(unCStr("CR"));}),_ic=[1,_ib,_ia],_id=new T(function(){return B(unCStr("FF"));}),_ie=[1,_id,_ic],_if=new T(function(){return B(unCStr("VT"));}),_ig=[1,_if,_ie],_ih=new T(function(){return B(unCStr("LF"));}),_ii=[1,_ih,_ig],_ij=new T(function(){return B(unCStr("HT"));}),_ik=[1,_ij,_ii],_il=[1,_hy,_ik],_im=[1,_hx,_il],_in=[1,_hw,_im],_io=new T(function(){return B(unCStr("ENQ"));}),_ip=[1,_io,_in],_iq=new T(function(){return B(unCStr("EOT"));}),_ir=[1,_iq,_ip],_is=new T(function(){return B(unCStr("ETX"));}),_it=[1,_is,_ir],_iu=new T(function(){return B(unCStr("STX"));}),_iv=[1,_iu,_it],_iw=new T(function(){return B(unCStr("SOH"));}),_ix=[1,_iw,_iv],_iy=new T(function(){return B(unCStr("NUL"));}),_iz=[1,_iy,_ix],_iA=[0,92],_iB=new T(function(){return B(unCStr("\\DEL"));}),_iC=new T(function(){return B(unCStr("\\a"));}),_iD=new T(function(){return B(unCStr("\\\\"));}),_iE=new T(function(){return B(unCStr("\\SO"));}),_iF=new T(function(){return B(unCStr("\\r"));}),_iG=new T(function(){return B(unCStr("\\f"));}),_iH=new T(function(){return B(unCStr("\\v"));}),_iI=new T(function(){return B(unCStr("\\n"));}),_iJ=new T(function(){return B(unCStr("\\t"));}),_iK=new T(function(){return B(unCStr("\\b"));}),_iL=function(_iM,_iN){if(_iM<=127){var _iO=E(_iM);switch(_iO){case 92:return new F(function(){return _I(_iD,_iN);});break;case 127:return new F(function(){return _I(_iB,_iN);});break;default:if(_iO<32){var _iP=E(_iO);switch(_iP){case 7:return new F(function(){return _I(_iC,_iN);});break;case 8:return new F(function(){return _I(_iK,_iN);});break;case 9:return new F(function(){return _I(_iJ,_iN);});break;case 10:return new F(function(){return _I(_iI,_iN);});break;case 11:return new F(function(){return _I(_iH,_iN);});break;case 12:return new F(function(){return _I(_iG,_iN);});break;case 13:return new F(function(){return _I(_iF,_iN);});break;case 14:return new F(function(){return _I(_iE,new T(function(){var _iQ=E(_iN);if(!_iQ[0]){var _iR=[0];}else{var _iR=E(E(_iQ[1])[1])==72?B(unAppCStr("\\&",_iQ)):E(_iQ);}return _iR;}));});break;default:return new F(function(){return _I([1,_iA,new T(function(){var _iS=_iP;return _iS>=0?B(_hr(_iz,_iS)):E(_ho);})],_iN);});}}else{return [1,[0,_iO],_iN];}}}else{return [1,_iA,new T(function(){var _iT=jsShowI(_iM),_iU=_iT;return B(_I(fromJSStr(_iU),new T(function(){var _iV=E(_iN);if(!_iV[0]){var _iW=[0];}else{var _iX=E(_iV[1])[1];if(_iX<48){var _iY=E(_iV);}else{var _iY=_iX>57?E(_iV):B(unAppCStr("\\&",_iV));}var _iZ=_iY,_j0=_iZ,_iW=_j0;}return _iW;})));})];}},_j1=[0,39],_j2=[1,_j1,_o],_j3=new T(function(){return B(unCStr("\'\\\'\'"));}),_j4=function(_j5){var _j6=E(E(_j5)[1]);return _j6==39?E(_j3):[1,_j1,new T(function(){return B(_iL(_j6,_j2));})];},_j7=[0,34],_j8=new T(function(){return B(unCStr("\\\""));}),_j9=function(_ja,_jb){var _jc=E(_ja);if(!_jc[0]){return E(_jb);}else{var _jd=_jc[2],_je=E(E(_jc[1])[1]);if(_je==34){return new F(function(){return _I(_j8,new T(function(){return B(_j9(_jd,_jb));}));});}else{return new F(function(){return _iL(_je,new T(function(){return B(_j9(_jd,_jb));}));});}}},_jf=function(_jg,_jh){return [1,_j7,new T(function(){return B(_j9(_jg,[1,_j7,_jh]));})];},_ji=function(_jj){return new F(function(){return _I(_j3,_jj);});},_jk=function(_jl,_jm){var _jn=E(E(_jm)[1]);return _jn==39?E(_ji):function(_jo){return [1,_j1,new T(function(){return B(_iL(_jn,[1,_j1,_jo]));})];};},_jp=[0,_jk,_j4,_jf],_jq=function(_jr){return E(E(_jr)[3]);},_js=function(_jt,_ju){return new F(function(){return A(_jq,[_jt,_ju,_o]);});},_jv=function(_jw,_jx,_jy){return new F(function(){return _2V(new T(function(){return B(_jq(_jw));}),_jx,_jy);});},_jz=function(_jA){return [0,function(_jB){return E(new T(function(){return B(_jq(_jA));}));},function(_jj){return new F(function(){return _js(_jA,_jj);});},function(_jC,_jj){return new F(function(){return _jv(_jA,_jC,_jj);});}];},_jD=new T(function(){return B(_jz(_jp));}),_jE=new T(function(){return B(unCStr("Just "));}),_jF=new T(function(){return B(unCStr("Nothing"));}),_jG=[0,11],_jH=function(_jI){return E(E(_jI)[1]);},_jJ=function(_jK,_jL,_jM,_jN){var _jO=E(_jM);if(!_jO[0]){return new F(function(){return _I(_jF,_jN);});}else{var _jP=_jO[1];return E(_jL)[1]<=10?B(_I(_jE,new T(function(){return B(A(_jH,[_jK,_jG,_jP,_jN]));}))):[1,_11,new T(function(){return B(_I(_jE,new T(function(){return B(A(_jH,[_jK,_jG,_jP,[1,_10,_jN]]));})));})];}},_jQ=[0,0],_jR=function(_jS,_jT){return new F(function(){return _jJ(_jS,_jQ,_jT,_o);});},_jU=function(_jV,_jW,_jX){return new F(function(){return _2V(function(_jC,_jj){return new F(function(){return _jJ(_jV,_jQ,_jC,_jj);});},_jW,_jX);});},_jY=function(_jZ){return [0,function(_k0,_jC,_jj){return new F(function(){return _jJ(_jZ,_k0,_jC,_jj);});},function(_jj){return new F(function(){return _jR(_jZ,_jj);});},function(_jC,_jj){return new F(function(){return _jU(_jZ,_jC,_jj);});}];},_k1=new T(function(){return B(_jY(_jD));}),_k2=function(_k3){var _k4=jsShow(E(_k3)[1]),_k5=_k4;return new F(function(){return fromJSStr(_k5);});},_k6=function(_k7){return function(_5l){return new F(function(){return _I(new T(function(){return B(_k2(_k7));}),_5l);});};},_k8=function(_k9){return new F(function(){return _7J(0,E(_k9)[1],_o);});},_ka=function(_kb,_kc){return new F(function(){return _7J(0,E(_kb)[1],_kc);});},_kd=function(_ke,_kf){return new F(function(){return _2V(_ka,_ke,_kf);});},_kg=function(_kh,_ki,_kj){return new F(function(){return _7J(E(_kh)[1],E(_ki)[1],_kj);});},_kk=[0,_kg,_k8,_kd],_kl=function(_km,_kn,_ko){return new F(function(){return A(_km,[[1,_2S,new T(function(){return B(A(_kn,[_ko]));})]]);});},_kp=new T(function(){return B(unCStr(": empty list"));}),_kq=new T(function(){return B(unCStr("Prelude."));}),_kr=function(_ks){return new F(function(){return err(B(_I(_kq,new T(function(){return B(_I(_ks,_kp));}))));});},_kt=new T(function(){return B(unCStr("foldr1"));}),_ku=new T(function(){return B(_kr(_kt));}),_kv=function(_kw,_kx){var _ky=E(_kx);if(!_ky[0]){return E(_ku);}else{var _kz=_ky[1],_kA=E(_ky[2]);if(!_kA[0]){return E(_kz);}else{return new F(function(){return A(_kw,[_kz,new T(function(){return B(_kv(_kw,_kA));})]);});}}},_kB=function(_kC,_kD,_kE,_kF){return new F(function(){return _2V(function(_kG,_kH){var _kI=E(_kG);return [1,_11,new T(function(){return B(A(_kv,[_kl,[1,new T(function(){return B(A(new T(function(){return B(_jH(_kC));}),[_jQ,_kI[1]]));}),[1,new T(function(){return B(A(new T(function(){return B(_jH(_kD));}),[_jQ,_kI[2]]));}),_o]],[1,_10,_kH]]));})];},_kE,_kF);});},_kJ=new T(function(){return B(unCStr("fromList "));}),_kK=function(_kL,_kM){while(1){var _kN=(function(_kO,_kP){var _kQ=E(_kP);switch(_kQ[0]){case 0:_kL=new T(function(){return B(_kK(_kO,_kQ[4]));});_kM=_kQ[3];return null;case 1:return [1,[0,[0,_kQ[1]],_kQ[2]],_kO];default:return E(_kO);}})(_kL,_kM);if(_kN!=null){return _kN;}}},_kR=function(_kS){var _kT=E(_kS);if(!_kT[0]){var _kU=_kT[3],_kV=_kT[4];return _kT[2]>=0?B(_kK(new T(function(){return B(_kK(_o,_kV));}),_kU)):B(_kK(new T(function(){return B(_kK(_o,_kU));}),_kV));}else{return new F(function(){return _kK(_o,_kT);});}},_kW=function(_kX,_kY,_kZ){var _l0=new T(function(){return B(_kR(_kZ));});return _kY<=10?function(_l1){return new F(function(){return _I(_kJ,new T(function(){return B(_kB(_kk,_kX,_l0,_l1));}));});}:function(_l2){return [1,_11,new T(function(){return B(_I(_kJ,new T(function(){return B(_kB(_kk,_kX,_l0,[1,_10,_l2]));})));})];};},_l3=[0,45],_l4=function(_l5,_l6,_l7){var _l8=function(_l9){var _la=new T(function(){return B(A(_l5,[[0, -_l7]]));});return E(_l6)[1]<=6?function(_lb){return [1,_l3,new T(function(){return B(A(_la,[_lb]));})];}:function(_lc){return [1,_11,[1,_l3,new T(function(){return B(A(_la,[[1,_10,_lc]]));})]];};};if(_l7>=0){var _ld=isDoubleNegativeZero(_l7),_le=_ld;return E(_le)==0?B(A(_l5,[[0,_l7]])):B(_l8(_));}else{return new F(function(){return _l8(_);});}},_lf=new T(function(){return B(unCStr("Aichan {"));}),_lg=new T(function(){return B(unCStr("_loves = "));}),_lh=new T(function(){return B(unCStr("_items = "));}),_li=[0,125],_lj=[0,0],_lk=new T(function(){return B(unCStr(", "));}),_ll=new T(function(){return B(unCStr("_lps = "));}),_lm=new T(function(){return B(unCStr("_depend = "));}),_ln=new T(function(){return B(unCStr("_lastFocus = "));}),_lo=new T(function(){return B(unCStr("_interval = "));}),_lp=new T(function(){return B(unCStr("_hasFocus = "));}),_lq=new T(function(){return B(unCStr("_achieves = "));}),_lr=new T(function(){return B(unCStr("True"));}),_ls=new T(function(){return B(unCStr("False"));}),_lt=function(_lu,_lv,_lw,_lx,_ly,_lz,_lA,_lB,_lC){var _lD=function(_lE){return new F(function(){return _I(_lg,new T(function(){return B(A(new T(function(){return B(_l4(_k6,_lj,E(_lv)[1]));}),[new T(function(){return B(_I(_lk,new T(function(){return B(_I(_ll,new T(function(){return B(A(new T(function(){return B(_l4(_k6,_lj,E(_lw)[1]));}),[new T(function(){return B(_I(_lk,new T(function(){return B(_I(_lm,new T(function(){return B(A(new T(function(){return B(_l4(_k6,_lj,E(_lx)[1]));}),[new T(function(){return B(_I(_lk,new T(function(){return B(_I(_ln,new T(function(){return B(_13(0,_ly,new T(function(){return B(_I(_lk,new T(function(){return B(_I(_lo,new T(function(){return B(_13(0,_lz,new T(function(){return B(_I(_lk,new T(function(){return B(_I(_lp,new T(function(){var _lF=new T(function(){return B(_I(_lk,new T(function(){return B(_I(_lq,new T(function(){return B(A(new T(function(){return B(_kW(_k1,0,_lB));}),[new T(function(){return B(_I(_lk,new T(function(){return B(_I(_lh,new T(function(){return B(A(new T(function(){return B(_kW(_kk,0,_lC));}),[[1,_li,_lE]]));})));})));})]));})));})));});return !E(_lA)?B(_I(_ls,_lF)):B(_I(_lr,_lF));})));})));})));})));})));})));})));})));})]));})));})));})]));})));})));})]));}));});};return _lu<11?function(_lG){return new F(function(){return _I(_lf,new T(function(){return B(_lD(_lG));}));});}:function(_lH){return [1,_11,new T(function(){return B(_I(_lf,new T(function(){return B(_lD([1,_10,_lH]));})));})];};},_lI=function(_lJ){var _lK=E(_lJ);return new F(function(){return A(_lt,[0,_lK[1],_lK[2],_lK[3],_lK[4],_lK[5],_lK[6],_lK[7],_lK[8],_o]);});},_lL=function(_lM,_lN,_lO,_){var _lP=rMV(_lN),_lQ=_lP,_lR=B(A(_lO,[_lQ,_])),_lS=_lR,_=wMV(_lN,new T(function(){return E(E(_lS)[2]);})),_lT=jsSetTimeout(_lM,function(_){var _lU=B(_lL(_lM,_lN,_lO,_)),_lV=_lU;return _4W;});return new F(function(){return rMV(_lN);});},_lW=new T(function(){return B(unCStr(" is not an element of the map"));}),_lX=function(_lY){return new F(function(){return err(B(unAppCStr("IntMap.!: key ",new T(function(){return B(_I(B(_7J(0,_lY,_o)),_lW));}))));});},_lZ=function(_m0,_m1){var _m2=new T(function(){return B(_lX(_m1));});return new F(function(){return (function(_m3){while(1){var _m4=E(_m3);switch(_m4[0]){case 0:var _m5=_m4[2]>>>0;if(((_m1>>>0&((_m5-1>>>0^4294967295)>>>0^_m5)>>>0)>>>0&4294967295)==_m4[1]){if(!((_m1>>>0&_m5)>>>0)){_m3=_m4[3];continue;}else{_m3=_m4[4];continue;}}else{return E(_m2);}break;case 1:return _m1!=_m4[1]?E(_m2):E(_m4[2]);default:return E(_m2);}}})(_m0);});},_m6=function(_m7,_m8){return new F(function(){return (function(_m9){while(1){var _ma=E(_m9);switch(_ma[0]){case 0:var _mb=_ma[2]>>>0;if(((_m7>>>0&((_mb-1>>>0^4294967295)>>>0^_mb)>>>0)>>>0&4294967295)==_ma[1]){if(!((_m7>>>0&_mb)>>>0)){_m9=_ma[3];continue;}else{_m9=_ma[4];continue;}}else{return false;}break;case 1:return _m7==_ma[1];default:return false;}}})(_m8);});},_mc=function(_){var _md=jsEval("Date.now()"),_me=_md;return new T(function(){var _mf=B(_gN(B(_3G(_er,new T(function(){return fromJSStr(_me);})))));return _mf[0]==0?B(err(_2m)):E(_mf[2])[0]==0?E(_mf[1]):B(err(_2k));});},_mg=function(_mh,_mi,_mj,_mk){var _ml=E(_mk);switch(_ml[0]){case 0:var _mm=_ml[1],_mn=_ml[2],_mo=_ml[3],_mp=_ml[4],_mq=_mn>>>0;if(((_mi>>>0&((_mq-1>>>0^4294967295)>>>0^_mq)>>>0)>>>0&4294967295)==_mm){return (_mi>>>0&_mq)>>>0==0?[0,_mm,_mn,E(B(_mg(_mh,_mi,_mj,_mo))),E(_mp)]:[0,_mm,_mn,E(_mo),E(B(_mg(_mh,_mi,_mj,_mp)))];}else{var _mr=(_mi>>>0^_mm>>>0)>>>0,_ms=(_mr|_mr>>>1)>>>0,_mt=(_ms|_ms>>>2)>>>0,_mu=(_mt|_mt>>>4)>>>0,_mv=(_mu|_mu>>>8)>>>0,_mw=(_mv|_mv>>>16)>>>0,_mx=(_mw^_mw>>>1)>>>0&4294967295,_my=_mx>>>0;return (_mi>>>0&_my)>>>0==0?[0,(_mi>>>0&((_my-1>>>0^4294967295)>>>0^_my)>>>0)>>>0&4294967295,_mx,E([1,_mi,_mj]),E(_ml)]:[0,(_mi>>>0&((_my-1>>>0^4294967295)>>>0^_my)>>>0)>>>0&4294967295,_mx,E(_ml),E([1,_mi,_mj])];}break;case 1:var _mz=_ml[1];if(_mi!=_mz){var _mA=(_mi>>>0^_mz>>>0)>>>0,_mB=(_mA|_mA>>>1)>>>0,_mC=(_mB|_mB>>>2)>>>0,_mD=(_mC|_mC>>>4)>>>0,_mE=(_mD|_mD>>>8)>>>0,_mF=(_mE|_mE>>>16)>>>0,_mG=(_mF^_mF>>>1)>>>0&4294967295,_mH=_mG>>>0;return (_mi>>>0&_mH)>>>0==0?[0,(_mi>>>0&((_mH-1>>>0^4294967295)>>>0^_mH)>>>0)>>>0&4294967295,_mG,E([1,_mi,_mj]),E(_ml)]:[0,(_mi>>>0&((_mH-1>>>0^4294967295)>>>0^_mH)>>>0)>>>0&4294967295,_mG,E(_ml),E([1,_mi,_mj])];}else{return [1,_mi,new T(function(){return B(A(_mh,[[0,_mi],_mj,_ml[2]]));})];}break;default:return [1,_mi,_mj];}},_mI=function(_mJ){return E(_mJ);},_mK=function(_mL,_mM){return new F(function(){return (function(_mN){while(1){var _mO=E(_mN);switch(_mO[0]){case 0:var _mP=_mO[2]>>>0;if(((_mL>>>0&((_mP-1>>>0^4294967295)>>>0^_mP)>>>0)>>>0&4294967295)==_mO[1]){if(!((_mL>>>0&_mP)>>>0)){_mN=_mO[3];continue;}else{_mN=_mO[4];continue;}}else{return [0];}break;case 1:return _mL!=_mO[1]?[0]:[1,_mO[2]];default:return [0];}}})(_mM);});},_mQ=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:248:3-8"));}),_mR=function(_mS,_mT){if(_mS<=_mT){var _mU=function(_mV){return [1,[0,_mV],new T(function(){if(_mV!=_mT){var _mW=B(_mU(_mV+1|0));}else{var _mW=[0];}var _mX=_mW;return _mX;})];};return new F(function(){return _mU(_mS);});}else{return [0];}},_mY=new T(function(){return B(unCStr("multiplier 1"));}),_mZ=[0,0],_n0=new T(function(){return B(unCStr("alerts"));}),_n1=new T(function(){return B(unCStr("innerHTML"));}),_n2=new T(function(){return B(unCStr("<div id=\"alert-%d\" class=\"alert alert-info fade in\" role=\"alert\">  <button type=\"button\" class=\"close\" data-dismiss=\"alert\"><span aria-hidden=\"true\">&times;</span><span class=\"sr-only\">Close</span></button>%s </div>"));}),_n3=new T(function(){return B(unCStr("\').alert(\'close\')"));}),_n4=function(_n5,_n6){while(1){var _n7=E(_n5);if(!_n7[0]){return E(_n6);}else{_n5=_n7[2];var _n8=[1,_n7[1],_n6];_n6=_n8;continue;}}},_n9=function(_na){var _nb=E(_na)[1];return [0,Math.log(_nb+(_nb+1)*Math.sqrt((_nb-1)/(_nb+1)))];},_nc=function(_nd){var _ne=E(_nd)[1];return [0,Math.log(_ne+Math.sqrt(1+_ne*_ne))];},_nf=function(_ng){var _nh=E(_ng)[1];return [0,0.5*Math.log((1+_nh)/(1-_nh))];},_ni=function(_nj,_nk){return [0,Math.log(E(_nk)[1])/Math.log(E(_nj)[1])];},_nl=[0,3.141592653589793],_nm=new T(function(){return [0,0/0];}),_nn=new T(function(){return [0,-1/0];}),_no=new T(function(){return [0,1/0];}),_np=[0,0],_nq=function(_nr,_ns){while(1){var _nt=E(_nr);if(!_nt[0]){_nr=[1,I_fromInt(_nt[1])];continue;}else{var _nu=E(_ns);if(!_nu[0]){_nr=_nt;_ns=[1,I_fromInt(_nu[1])];continue;}else{return new F(function(){return I_fromRat(_nt[1],_nu[1]);});}}}},_nv=function(_nw,_nx){var _ny=E(_nw);if(!_ny[0]){var _nz=_ny[1],_nA=E(_nx);return _nA[0]==0?_nz==_nA[1]:I_compareInt(_nA[1],_nz)==0?true:false;}else{var _nB=_ny[1],_nC=E(_nx);return _nC[0]==0?I_compareInt(_nB,_nC[1])==0?true:false:I_compare(_nB,_nC[1])==0?true:false;}},_nD=function(_nE,_nF){return !B(_nv(_nF,_np))?[0,B(_nq(_nE,_nF))]:!B(_nv(_nE,_np))?!B(_S(_nE,_np))?E(_no):E(_nn):E(_nm);},_nG=function(_nH){var _nI=E(_nH);return new F(function(){return _nD(_nI[1],_nI[2]);});},_nJ=function(_nK){return [0,1/E(_nK)[1]];},_nL=function(_nM){var _nN=E(_nM),_nO=_nN[1];return _nO<0?[0, -_nO]:E(_nN);},_nP=function(_nQ){var _nR=E(_nQ);return _nR[0]==0?_nR[1]:I_toNumber(_nR[1]);},_nS=function(_nT){return [0,B(_nP(_nT))];},_nU=[0,0],_nV=[0,1],_nW=[0,-1],_nX=function(_nY){var _nZ=E(E(_nY)[1]);return _nZ==0?E(_nU):_nZ<=0?E(_nW):E(_nV);},_o0=function(_o1,_o2){return [0,E(_o1)[1]-E(_o2)[1]];},_o3=function(_o4){return [0, -E(_o4)[1]];},_o5=function(_o6,_o7){return [0,E(_o6)[1]+E(_o7)[1]];},_o8=function(_o9,_oa){return [0,E(_o9)[1]*E(_oa)[1]];},_ob=[0,_o5,_o8,_o0,_o3,_nL,_nX,_nS],_oc=function(_od,_oe){return [0,E(_od)[1]/E(_oe)[1]];},_of=[0,_ob,_oc,_nJ,_nG],_og=function(_oh){return [0,Math.acos(E(_oh)[1])];},_oi=function(_oj){return [0,Math.asin(E(_oj)[1])];},_ok=function(_ol){return [0,Math.atan(E(_ol)[1])];},_om=function(_on){return [0,Math.cos(E(_on)[1])];},_oo=function(_op){return [0,cosh(E(_op)[1])];},_oq=function(_or){return [0,Math.exp(E(_or)[1])];},_os=function(_ot){return [0,Math.log(E(_ot)[1])];},_ou=function(_ov,_ow){return [0,Math.pow(E(_ov)[1],E(_ow)[1])];},_ox=function(_oy){return [0,Math.sin(E(_oy)[1])];},_oz=function(_oA){return [0,sinh(E(_oA)[1])];},_oB=function(_oC){return [0,Math.sqrt(E(_oC)[1])];},_oD=function(_oE){return [0,Math.tan(E(_oE)[1])];},_oF=function(_oG){return [0,tanh(E(_oG)[1])];},_oH=[0,_of,_nl,_oq,_oB,_os,_ou,_ni,_ox,_oD,_om,_oi,_ok,_og,_oz,_oF,_oo,_nc,_nf,_n9],_oI=function(_oJ){var _oK=E(_oJ)[1];return [0,Math.log(_oK+(_oK+1)*Math.sqrt((_oK-1)/(_oK+1)))];},_oL=function(_oM){var _oN=E(_oM)[1];return [0,Math.log(_oN+Math.sqrt(1+_oN*_oN))];},_oO=function(_oP){var _oQ=E(_oP)[1];return [0,0.5*Math.log((1+_oQ)/(1-_oQ))];},_oR=function(_oS,_oT){return [0,Math.log(E(_oT)[1])/Math.log(E(_oS)[1])];},_oU=[0,3.141592653589793],_oV=new T(function(){return [0,0/0];}),_oW=new T(function(){return [0,-1/0];}),_oX=new T(function(){return [0,1/0];}),_oY=function(_oZ,_p0){return !B(_nv(_p0,_np))?[0,B(_nq(_oZ,_p0))]:!B(_nv(_oZ,_np))?!B(_S(_oZ,_np))?E(_oX):E(_oW):E(_oV);},_p1=function(_p2){var _p3=E(_p2);return new F(function(){return _oY(_p3[1],_p3[2]);});},_p4=function(_p5){return [0,1/E(_p5)[1]];},_p6=function(_p7){var _p8=E(_p7),_p9=_p8[1];return _p9<0?[0, -_p9]:E(_p8);},_pa=function(_pb){var _pc=E(_pb);return _pc[0]==0?_pc[1]:I_toNumber(_pc[1]);},_pd=function(_pe){return [0,B(_pa(_pe))];},_pf=[0,0],_pg=[0,1],_ph=[0,-1],_pi=function(_pj){var _pk=E(E(_pj)[1]);return _pk==0?E(_pf):_pk<=0?E(_ph):E(_pg);},_pl=function(_pm,_pn){return [0,E(_pm)[1]-E(_pn)[1]];},_po=function(_pp){return [0, -E(_pp)[1]];},_pq=function(_pr,_ps){return [0,E(_pr)[1]+E(_ps)[1]];},_pt=function(_pu,_pv){return [0,E(_pu)[1]*E(_pv)[1]];},_pw=[0,_pq,_pt,_pl,_po,_p6,_pi,_pd],_px=function(_py,_pz){return [0,E(_py)[1]/E(_pz)[1]];},_pA=[0,_pw,_px,_p4,_p1],_pB=function(_pC){return [0,Math.acos(E(_pC)[1])];},_pD=function(_pE){return [0,Math.asin(E(_pE)[1])];},_pF=function(_pG){return [0,Math.atan(E(_pG)[1])];},_pH=function(_pI){return [0,Math.cos(E(_pI)[1])];},_pJ=function(_pK){return [0,cosh(E(_pK)[1])];},_pL=function(_pM){return [0,Math.exp(E(_pM)[1])];},_pN=function(_pO){return [0,Math.log(E(_pO)[1])];},_pP=function(_pQ,_pR){return [0,Math.pow(E(_pQ)[1],E(_pR)[1])];},_pS=function(_pT){return [0,Math.sin(E(_pT)[1])];},_pU=function(_pV){return [0,sinh(E(_pV)[1])];},_pW=function(_pX){return [0,Math.sqrt(E(_pX)[1])];},_pY=function(_pZ){return [0,Math.tan(E(_pZ)[1])];},_q0=function(_q1){return [0,tanh(E(_q1)[1])];},_q2=[0,_pA,_oU,_pL,_pW,_pN,_pP,_oR,_pS,_pY,_pH,_pD,_pF,_pB,_pU,_q0,_pJ,_oL,_oO,_oI],_q3=function(_q4){var _q5=I_decodeDouble(_q4);return [0,[1,_q5[2]],_q5[1]];},_q6=function(_q7){var _q8=B(_q3(E(_q7)[1]));return [0,_q8[1],[0,_q8[2]]];},_q9=[0,53],_qa=function(_qb){return E(_q9);},_qc=[0,2],_qd=function(_qe){return E(_qc);},_qf=[0,1024],_qg=[0,-1021],_qh=[0,_qg,_qf],_qi=function(_qj){return E(_qh);},_qk=function(_ql){var _qm=isDoubleInfinite(E(_ql)[1]),_qn=_qm;return E(_qn)==0?false:true;},_qo=function(_qp){var _qq=isDoubleNaN(E(_qp)[1]),_qr=_qq;return E(_qr)==0?false:true;},_qs=function(_qt){var _qu=isDoubleNegativeZero(E(_qt)[1]),_qv=_qu;return E(_qv)==0?false:true;},_qw=function(_qx){var _qy=decodeFloat(E(_qx)[1]);return [0,new T(function(){return B(_6q(_qy[1]));}),[0,_qy[2]]];},_qz=[0,24],_qA=function(_qB){return E(_qz);},_qC=function(_qD){return E(_qc);},_qE=[0,128],_qF=[0,-125],_qG=[0,_qF,_qE],_qH=function(_qI){return E(_qG);},_qJ=function(_qK){var _qL=isFloatInfinite(E(_qK)[1]),_qM=_qL;return E(_qM)==0?false:true;},_qN=function(_qO){var _qP=isFloatNaN(E(_qO)[1]),_qQ=_qP;return E(_qQ)==0?false:true;},_qR=function(_qS){var _qT=isFloatNegativeZero(E(_qS)[1]),_qU=_qT;return E(_qU)==0?false:true;},_qV=function(_qW,_qX){return E(_qW)[1]!=E(_qX)[1]?true:false;},_qY=function(_qZ,_r0){return E(_qZ)[1]==E(_r0)[1];},_r1=[0,_qY,_qV],_r2=function(_r3,_r4){return E(_r3)[1]<E(_r4)[1];},_r5=function(_r6,_r7){return E(_r6)[1]<=E(_r7)[1];},_r8=function(_r9,_ra){return E(_r9)[1]>E(_ra)[1];},_rb=function(_rc,_rd){return E(_rc)[1]>=E(_rd)[1];},_re=function(_rf,_rg){var _rh=E(_rf)[1],_ri=E(_rg)[1];return _rh>=_ri?_rh!=_ri?2:1:0;},_rj=function(_rk,_rl){var _rm=E(_rk),_rn=E(_rl);return _rm[1]>_rn[1]?E(_rm):E(_rn);},_ro=function(_rp,_rq){var _rr=E(_rp),_rs=E(_rq);return _rr[1]>_rs[1]?E(_rs):E(_rr);},_rt=[0,_r1,_re,_r2,_rb,_r8,_r5,_rj,_ro],_ru=[0,1],_rv=function(_rw){var _rx=hs_intToInt64(2147483647),_ry=_rx,_rz=hs_leInt64(_rw,_ry),_rA=_rz;if(!E(_rA)){return [1,I_fromInt64(_rw)];}else{var _rB=hs_intToInt64(-2147483648),_rC=_rB,_rD=hs_geInt64(_rw,_rC),_rE=_rD;if(!E(_rE)){return [1,I_fromInt64(_rw)];}else{var _rF=hs_int64ToInt(_rw),_rG=_rF;return new F(function(){return _6q(_rG);});}}},_rH=new T(function(){var _rI=newByteArr(256),_rJ=_rI,_=_rJ["v"]["i8"][0]=8,_=B((function(_rK,_rL,_rM,_){while(1){if(_rM>=256){if(_rK>=256){return E(_);}else{var _rN=imul(2,_rK)|0,_rO=_rL+1|0,_rP=_rK;_rK=_rN;_rL=_rO;_rM=_rP;continue;}}else{var _=_rJ["v"]["i8"][_rM]=_rL,_rP=_rM+_rK|0;_rM=_rP;continue;}}})(2,0,1,_)),_rQ=_rJ,_rR=_rQ;return [0,_rR];}),_rS=function(_rT,_rU){while(1){var _rV=(function(_rW,_rX){var _rY=hs_int64ToInt(_rW),_rZ=_rY,_s0=E(_rH)[1]["v"]["i8"][(255&_rZ>>>0)>>>0&4294967295];if(_rX>_s0){if(_s0>=8){var _s1=hs_uncheckedIShiftRA64(_rW,8),_s2=_s1;_rT=_s2;var _s3=_rX-8|0;_rU=_s3;return null;}else{return [0,new T(function(){var _s4=hs_uncheckedIShiftRA64(_rW,_s0),_s5=_s4;return B(_rv(_s5));}),_rX-_s0|0];}}else{return [0,new T(function(){var _s6=hs_uncheckedIShiftRA64(_rW,_rX),_s7=_s6;return B(_rv(_s7));}),0];}})(_rT,_rU);if(_rV!=null){return _rV;}}},_s8=function(_s9){var _sa=hs_intToInt64(_s9),_sb=_sa;return E(_sb);},_sc=function(_sd){var _se=E(_sd);return _se[0]==0?B(_s8(_se[1])):I_toInt64(_se[1]);},_sf=function(_sg){return I_toInt(_sg)>>>0;},_sh=function(_si){var _sj=E(_si);return _sj[0]==0?_sj[1]>>>0:B(_sf(_sj[1]));},_sk=function(_sl,_sm){while(1){var _sn=E(_sl);if(!_sn[0]){_sl=[1,I_fromInt(_sn[1])];continue;}else{return [1,I_shiftLeft(_sn[1],_sm)];}}},_so=function(_sp){var _sq=B(_q3(_sp)),_sr=_sq[1],_ss=_sq[2];if(_ss<0){var _st=function(_su){if(!_su){return [0,E(_sr),B(_sk(_ru, -_ss))];}else{var _sv=B(_rS(B(_sc(_sr)), -_ss));return [0,E(_sv[1]),B(_sk(_ru,_sv[2]))];}};return (B(_sh(_sr))&1)>>>0==0?B(_st(1)):B(_st(0));}else{return [0,B(_sk(_sr,_ss)),_ru];}},_sw=function(_sx){var _sy=B(_so(E(_sx)[1]));return [0,E(_sy[1]),E(_sy[2])];},_sz=[0,_ob,_rt,_sw],_sA=function(_sB){return E(E(_sB)[1]);},_sC=[0,1],_sD=function(_sE){return new F(function(){return _mR(E(_sE)[1],2147483647);});},_sF=function(_sG,_sH,_sI){return _sI<=_sH?[1,[0,_sG],new T(function(){var _sJ=_sH-_sG|0,_sK=function(_sL){return _sL>=(_sI-_sJ|0)?[1,[0,_sL],new T(function(){return B(_sK(_sL+_sJ|0));})]:[1,[0,_sL],_o];};return B(_sK(_sH));})]:_sI<=_sG?[1,[0,_sG],_o]:[0];},_sM=function(_sN,_sO,_sP){return _sP>=_sO?[1,[0,_sN],new T(function(){var _sQ=_sO-_sN|0,_sR=function(_sS){return _sS<=(_sP-_sQ|0)?[1,[0,_sS],new T(function(){return B(_sR(_sS+_sQ|0));})]:[1,[0,_sS],_o];};return B(_sR(_sO));})]:_sP>=_sN?[1,[0,_sN],_o]:[0];},_sT=function(_sU,_sV){return _sV<_sU?B(_sF(_sU,_sV,-2147483648)):B(_sM(_sU,_sV,2147483647));},_sW=function(_sX,_sY){return new F(function(){return _sT(E(_sX)[1],E(_sY)[1]);});},_sZ=function(_t0,_t1,_t2){return _t1<_t0?B(_sF(_t0,_t1,_t2)):B(_sM(_t0,_t1,_t2));},_t3=function(_t4,_t5,_t6){return new F(function(){return _sZ(E(_t4)[1],E(_t5)[1],E(_t6)[1]);});},_t7=function(_t8,_t9){return new F(function(){return _mR(E(_t8)[1],E(_t9)[1]);});},_ta=function(_tb){return E(_tb);},_tc=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_td=new T(function(){return B(err(_tc));}),_te=function(_tf){var _tg=E(E(_tf)[1]);return _tg==(-2147483648)?E(_td):[0,_tg-1|0];},_th=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_ti=new T(function(){return B(err(_th));}),_tj=function(_tk){var _tl=E(E(_tk)[1]);return _tl==2147483647?E(_ti):[0,_tl+1|0];},_tm=[0,_tj,_te,_ta,_ta,_sD,_sW,_t7,_t3],_tn=function(_to,_tp){if(_to<=0){if(_to>=0){return new F(function(){return quot(_to,_tp);});}else{if(_tp<=0){return new F(function(){return quot(_to,_tp);});}else{return quot(_to+1|0,_tp)-1|0;}}}else{if(_tp>=0){if(_to>=0){return new F(function(){return quot(_to,_tp);});}else{if(_tp<=0){return new F(function(){return quot(_to,_tp);});}else{return quot(_to+1|0,_tp)-1|0;}}}else{return quot(_to-1|0,_tp)-1|0;}}},_tq=new T(function(){return B(unCStr("ArithException"));}),_tr=new T(function(){return B(unCStr("GHC.Exception"));}),_ts=new T(function(){return B(unCStr("base"));}),_tt=new T(function(){var _tu=hs_wordToWord64(4194982440),_tv=_tu,_tw=hs_wordToWord64(3110813675),_tx=_tw;return [0,_tv,_tx,[0,_tv,_tx,_ts,_tr,_tq],_o];}),_ty=function(_tz){return E(_tt);},_tA=function(_tB){var _tC=E(_tB);return new F(function(){return _2A(B(_2y(_tC[1])),_ty,_tC[2]);});},_tD=new T(function(){return B(unCStr("arithmetic underflow"));}),_tE=new T(function(){return B(unCStr("arithmetic overflow"));}),_tF=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_tG=new T(function(){return B(unCStr("denormal"));}),_tH=new T(function(){return B(unCStr("divide by zero"));}),_tI=new T(function(){return B(unCStr("loss of precision"));}),_tJ=function(_tK){switch(E(_tK)){case 0:return E(_tE);case 1:return E(_tD);case 2:return E(_tI);case 3:return E(_tH);case 4:return E(_tG);default:return E(_tF);}},_tL=function(_tM){return new F(function(){return _I(_tD,_tM);});},_tN=function(_tM){return new F(function(){return _I(_tE,_tM);});},_tO=function(_tM){return new F(function(){return _I(_tF,_tM);});},_tP=function(_tM){return new F(function(){return _I(_tG,_tM);});},_tQ=function(_tM){return new F(function(){return _I(_tH,_tM);});},_tR=function(_tM){return new F(function(){return _I(_tI,_tM);});},_tS=function(_tT){switch(E(_tT)){case 0:return E(_tN);case 1:return E(_tL);case 2:return E(_tR);case 3:return E(_tQ);case 4:return E(_tP);default:return E(_tO);}},_tU=function(_tV,_tW){return new F(function(){return _2V(_tS,_tV,_tW);});},_tX=function(_tY,_tZ){switch(E(_tZ)){case 0:return E(_tN);case 1:return E(_tL);case 2:return E(_tR);case 3:return E(_tQ);case 4:return E(_tP);default:return E(_tO);}},_u0=[0,_tX,_tJ,_tU],_u1=new T(function(){return [0,_ty,_u0,_u2,_tA];}),_u2=function(_tM){return [0,_u1,_tM];},_u3=3,_u4=new T(function(){return B(_u2(_u3));}),_u5=new T(function(){return die(_u4);}),_u6=0,_u7=new T(function(){return B(_u2(_u6));}),_u8=new T(function(){return die(_u7);}),_u9=function(_ua,_ub){var _uc=E(_ub);switch(_uc){case -1:var _ud=E(_ua);return _ud==(-2147483648)?E(_u8):B(_tn(_ud,-1));case 0:return E(_u5);default:return new F(function(){return _tn(_ua,_uc);});}},_ue=function(_uf,_ug){return [0,B(_u9(E(_uf)[1],E(_ug)[1]))];},_uh=[0,0],_ui=[0,_u8,_uh],_uj=function(_uk,_ul){var _um=E(_uk)[1],_un=E(E(_ul)[1]);switch(_un){case -1:var _uo=E(_um);if(_uo==(-2147483648)){return E(_ui);}else{if(_uo<=0){if(_uo>=0){var _up=quotRemI(_uo,-1);return [0,[0,_up[1]],[0,_up[2]]];}else{var _uq=quotRemI(_uo,-1);return [0,[0,_uq[1]],[0,_uq[2]]];}}else{var _ur=quotRemI(_uo-1|0,-1);return [0,[0,_ur[1]-1|0],[0,(_ur[2]+(-1)|0)+1|0]];}}break;case 0:return E(_u5);default:if(_um<=0){if(_um>=0){var _us=quotRemI(_um,_un);return [0,[0,_us[1]],[0,_us[2]]];}else{if(_un<=0){var _ut=quotRemI(_um,_un);return [0,[0,_ut[1]],[0,_ut[2]]];}else{var _uu=quotRemI(_um+1|0,_un);return [0,[0,_uu[1]-1|0],[0,(_uu[2]+_un|0)-1|0]];}}}else{if(_un>=0){if(_um>=0){var _uv=quotRemI(_um,_un);return [0,[0,_uv[1]],[0,_uv[2]]];}else{if(_un<=0){var _uw=quotRemI(_um,_un);return [0,[0,_uw[1]],[0,_uw[2]]];}else{var _ux=quotRemI(_um+1|0,_un);return [0,[0,_ux[1]-1|0],[0,(_ux[2]+_un|0)-1|0]];}}}else{var _uy=quotRemI(_um-1|0,_un);return [0,[0,_uy[1]-1|0],[0,(_uy[2]+_un|0)+1|0]];}}}},_uz=function(_uA,_uB){var _uC=_uA%_uB;if(_uA<=0){if(_uA>=0){return E(_uC);}else{if(_uB<=0){return E(_uC);}else{var _uD=E(_uC);return _uD==0?0:_uD+_uB|0;}}}else{if(_uB>=0){if(_uA>=0){return E(_uC);}else{if(_uB<=0){return E(_uC);}else{var _uE=E(_uC);return _uE==0?0:_uE+_uB|0;}}}else{var _uF=E(_uC);return _uF==0?0:_uF+_uB|0;}}},_uG=function(_uH,_uI){var _uJ=E(E(_uI)[1]);switch(_uJ){case -1:return E(_uh);case 0:return E(_u5);default:return [0,B(_uz(E(_uH)[1],_uJ))];}},_uK=function(_uL,_uM){var _uN=E(_uL)[1],_uO=E(E(_uM)[1]);switch(_uO){case -1:var _uP=E(_uN);return _uP==(-2147483648)?E(_u8):[0,quot(_uP,-1)];case 0:return E(_u5);default:return [0,quot(_uN,_uO)];}},_uQ=function(_uR,_uS){var _uT=E(_uR)[1],_uU=E(E(_uS)[1]);switch(_uU){case -1:var _uV=E(_uT);if(_uV==(-2147483648)){return E(_ui);}else{var _uW=quotRemI(_uV,-1);return [0,[0,_uW[1]],[0,_uW[2]]];}break;case 0:return E(_u5);default:var _uX=quotRemI(_uT,_uU);return [0,[0,_uX[1]],[0,_uX[2]]];}},_uY=function(_uZ,_v0){var _v1=E(E(_v0)[1]);switch(_v1){case -1:return E(_uh);case 0:return E(_u5);default:return [0,E(_uZ)[1]%_v1];}},_v2=function(_v3){return new F(function(){return _6q(E(_v3)[1]);});},_v4=function(_v5){return [0,E(B(_6q(E(_v5)[1]))),E(_sC)];},_v6=function(_v7,_v8){return [0,imul(E(_v7)[1],E(_v8)[1])|0];},_v9=function(_va,_vb){return [0,E(_va)[1]+E(_vb)[1]|0];},_vc=function(_vd,_ve){return [0,E(_vd)[1]-E(_ve)[1]|0];},_vf=function(_vg){var _vh=E(_vg),_vi=_vh[1];return _vi<0?[0, -_vi]:E(_vh);},_vj=function(_vk){return [0,B(_7R(_vk))];},_vl=function(_vm){return [0, -E(_vm)[1]];},_vn=[0,-1],_vo=[0,0],_vp=[0,1],_vq=function(_vr){var _vs=E(_vr)[1];return _vs>=0?E(_vs)==0?E(_vo):E(_vp):E(_vn);},_vt=[0,_v9,_v6,_vc,_vl,_vf,_vq,_vj],_vu=function(_vv,_vw){return E(_vv)[1]==E(_vw)[1];},_vx=function(_vy,_vz){return E(_vy)[1]!=E(_vz)[1];},_vA=[0,_vu,_vx],_vB=function(_vC,_vD){var _vE=E(_vC),_vF=E(_vD);return _vE[1]>_vF[1]?E(_vE):E(_vF);},_vG=function(_vH,_vI){var _vJ=E(_vH),_vK=E(_vI);return _vJ[1]>_vK[1]?E(_vK):E(_vJ);},_vL=function(_vM,_vN){return _vM>=_vN?_vM!=_vN?2:1:0;},_vO=function(_vP,_vQ){return new F(function(){return _vL(E(_vP)[1],E(_vQ)[1]);});},_vR=function(_vS,_vT){return E(_vS)[1]>=E(_vT)[1];},_vU=function(_vV,_vW){return E(_vV)[1]>E(_vW)[1];},_vX=function(_vY,_vZ){return E(_vY)[1]<=E(_vZ)[1];},_w0=function(_w1,_w2){return E(_w1)[1]<E(_w2)[1];},_w3=[0,_vA,_vO,_w0,_vR,_vU,_vX,_vB,_vG],_w4=[0,_vt,_w3,_v4],_w5=[0,_w4,_tm,_uK,_uY,_ue,_uG,_uQ,_uj,_v2],_w6=function(_w7){return E(E(_w7)[1]);},_w8=function(_w9,_wa,_wb){while(1){if(!(_wa%2)){var _wc=B(_6s(_w9,_w9)),_wd=quot(_wa,2);_w9=_wc;_wa=_wd;continue;}else{var _we=E(_wa);if(_we==1){return new F(function(){return _6s(_w9,_wb);});}else{var _wc=B(_6s(_w9,_w9));_wa=quot(_we-1|0,2);var _wf=B(_6s(_w9,_wb));_w9=_wc;_wb=_wf;continue;}}}},_wg=function(_wh,_wi){while(1){if(!(_wi%2)){var _wj=B(_6s(_wh,_wh)),_wk=quot(_wi,2);_wh=_wj;_wi=_wk;continue;}else{var _wl=E(_wi);if(_wl==1){return E(_wh);}else{return new F(function(){return _w8(B(_6s(_wh,_wh)),quot(_wl-1|0,2),_wh);});}}}},_wm=function(_wn){return E(E(_wn)[2]);},_wo=function(_wp){return E(E(_wp)[1]);},_wq=function(_wr){return E(E(_wr)[2]);},_ws=[0,0],_wt=[0,2],_wu=function(_wv){return E(E(_wv)[7]);},_ww=function(_wx,_wy,_wz,_wA,_wB){return new F(function(){return A(E(E(_wy)[1])[1],[new T(function(){return B(A(_wA,[_wB,new T(function(){return B(A(_wu,[_wx,_wt]));})]));}),new T(function(){return B(A(_wu,[_wx,_ws]));})]);});},_wC=function(_wD){return E(E(_wD)[3]);},_wE=new T(function(){return B(unCStr("Negative exponent"));}),_wF=new T(function(){return B(err(_wE));}),_wG=function(_wH,_wI,_wJ,_wK){var _wL=B(_sA(_wI)),_wM=_wL[1],_wN=E(_wL[2]);if(!B(A(_wN[3],[_wK,new T(function(){return B(A(_wu,[_wM,_ws]));})]))){if(!B(A(E(_wN[1])[1],[_wK,new T(function(){return B(A(_wu,[_wM,_ws]));})]))){var _wO=B(_sA(_wI)),_wP=_wO[1],_wQ=new T(function(){return B(_sA(_wI));}),_wR=new T(function(){return B(_w6(_wQ));});return new F(function(){return (function(_wS,_wT){while(1){var _wU=(function(_wV,_wW){var _wX=E(_wI),_wY=_wX[3],_wZ=E(_wX[1]);if(!B(_ww(_wZ[1],_wZ[2],_wZ[3],_wX[4],_wW))){return !B(A(E(E(_wO[2])[1])[1],[_wW,new T(function(){return B(A(_wu,[_wP,_sC]));})]))?B((function(_x0,_x1,_x2){while(1){var _x3=(function(_x4,_x5,_x6){var _x7=E(_wI),_x8=_x7[3],_x9=E(_x7[1]);if(!B(_ww(_x9[1],_x9[2],_x9[3],_x7[4],_x5))){if(!B(A(new T(function(){return B(_78(new T(function(){return B(_wo(new T(function(){return B(_wq(_wQ));})));})));}),[_x5,new T(function(){return B(A(_wu,[_wR,_sC]));})]))){_x0=new T(function(){return B(A(new T(function(){return B(_wm(_wH));}),[_x4,_x4]));});_x1=new T(function(){return B(A(_x8,[new T(function(){return B(A(new T(function(){return B(_wC(_wR));}),[_x5,new T(function(){return B(A(_wu,[_wR,_sC]));})]));}),new T(function(){return B(A(_wu,[_wR,_wt]));})]));});_x2=new T(function(){return B(A(new T(function(){return B(_wm(_wH));}),[_x4,_x6]));});return null;}else{return new F(function(){return A(new T(function(){return B(_wm(_wH));}),[_x4,_x6]);});}}else{_x0=new T(function(){return B(A(new T(function(){return B(_wm(_wH));}),[_x4,_x4]));});_x1=new T(function(){return B(A(_x8,[_x5,new T(function(){return B(A(_wu,[_wR,_wt]));})]));});var _xa=_x6;_x2=_xa;return null;}})(_x0,_x1,_x2);if(_x3!=null){return _x3;}}})(new T(function(){return B(A(new T(function(){return B(_wm(_wH));}),[_wV,_wV]));}),new T(function(){return B(A(_wY,[new T(function(){return B(A(new T(function(){return B(_wC(_wP));}),[_wW,new T(function(){return B(A(_wu,[_wP,_sC]));})]));}),new T(function(){return B(A(_wu,[_wP,_wt]));})]));}),_wV)):E(_wV);}else{_wS=new T(function(){return B(A(new T(function(){return B(_wm(_wH));}),[_wV,_wV]));});_wT=new T(function(){return B(A(_wY,[_wW,new T(function(){return B(A(_wu,[_wP,_wt]));})]));});return null;}})(_wS,_wT);if(_wU!=null){return _wU;}}})(_wJ,_wK);});}else{return new F(function(){return A(_wu,[_wH,_sC]);});}}else{return E(_wF);}},_xb=new T(function(){return B(err(_wE));}),_xc=function(_xd,_xe){var _xf=E(_xd);return _xf[0]==0?_xf[1]*Math.pow(2,_xe):I_toNumber(_xf[1])*Math.pow(2,_xe);},_xg=function(_xh,_xi){while(1){var _xj=E(_xh);if(!_xj[0]){var _xk=E(_xj[1]);if(_xk==(-2147483648)){_xh=[1,I_fromInt(-2147483648)];continue;}else{var _xl=E(_xi);if(!_xl[0]){var _xm=_xl[1];return [0,[0,quot(_xk,_xm)],[0,_xk%_xm]];}else{_xh=[1,I_fromInt(_xk)];_xi=_xl;continue;}}}else{var _xn=E(_xi);if(!_xn[0]){_xh=_xj;_xi=[1,I_fromInt(_xn[1])];continue;}else{var _xo=I_quotRem(_xj[1],_xn[1]);return [0,[1,_xo[1]],[1,_xo[2]]];}}}},_xp=function(_xq,_xr){var _xs=B(_q3(_xr)),_xt=_xs[1],_xu=_xs[2],_xv=new T(function(){return B(_w6(new T(function(){return B(_sA(_xq));})));});if(_xu<0){var _xw= -_xu;if(_xw>=0){var _xx=E(_xw),_xy=_xx==0?E(_sC):B(_wg(_qc,_xx));if(!B(_nv(_xy,_np))){var _xz=B(_xg(_xt,_xy));return [0,new T(function(){return B(A(_wu,[_xv,_xz[1]]));}),new T(function(){return [0,B(_xc(_xz[2],_xu))];})];}else{return E(_u5);}}else{return E(_xb);}}else{return [0,new T(function(){return B(A(_wm,[_xv,new T(function(){return B(A(_wu,[_xv,_xt]));}),new T(function(){return B(_wG(_xv,_w5,new T(function(){return B(A(_wu,[_xv,_qc]));}),[0,_xu]));})]));}),_nU];}},_xA=function(_xB,_xC){var _xD=B(_xp(_xB,E(_xC)[1])),_xE=_xD[1];if(E(_xD[2])[1]<=0){return E(_xE);}else{var _xF=E(B(_sA(_xB))[1]);return new F(function(){return A(_xF[1],[_xE,new T(function(){return B(A(_xF[7],[_ru]));})]);});}},_xG=function(_xH,_xI){var _xJ=B(_xp(_xH,E(_xI)[1])),_xK=_xJ[1];if(E(_xJ[2])[1]>=0){return E(_xK);}else{var _xL=E(B(_sA(_xH))[1]);return new F(function(){return A(_xL[3],[_xK,new T(function(){return B(A(_xL[7],[_ru]));})]);});}},_xM=function(_xN,_xO){var _xP=B(_xp(_xN,E(_xO)[1]));return [0,_xP[1],_xP[2]];},_xQ=function(_xR,_xS){var _xT=B(_xp(_xR,_xS)),_xU=_xT[1],_xV=E(_xT[2])[1],_xW=new T(function(){var _xX=E(B(_sA(_xR))[1]),_xY=_xX[7];return _xV>=0?B(A(_xX[1],[_xU,new T(function(){return B(A(_xY,[_ru]));})])):B(A(_xX[3],[_xU,new T(function(){return B(A(_xY,[_ru]));})]));});if(_xV<0){var _xZ= -_xV-0.5;if(_xZ>=0){if(!E(_xZ)){var _y0=E(_xR),_y1=E(_y0[1]);return !B(_ww(_y1[1],_y1[2],_y1[3],_y0[4],_xU))?E(_xW):E(_xU);}else{return E(_xW);}}else{return E(_xU);}}else{var _y2=_xV-0.5;if(_y2>=0){if(!E(_y2)){var _y3=E(_xR),_y4=E(_y3[1]);return !B(_ww(_y4[1],_y4[2],_y4[3],_y3[4],_xU))?E(_xW):E(_xU);}else{return E(_xW);}}else{return E(_xU);}}},_y5=function(_y6,_y7){return new F(function(){return _xQ(_y6,E(_y7)[1]);});},_y8=function(_y9,_ya){return E(B(_xp(_y9,E(_ya)[1]))[1]);},_yb=[0,_sz,_of,_xM,_y8,_y5,_xA,_xG],_yc=function(_yd,_ye){return E(_yd)[1]!=E(_ye)[1]?true:false;},_yf=function(_yg,_yh){return E(_yg)[1]==E(_yh)[1];},_yi=[0,_yf,_yc],_yj=function(_yk,_yl){return E(_yk)[1]<E(_yl)[1];},_ym=function(_yn,_yo){return E(_yn)[1]<=E(_yo)[1];},_yp=function(_yq,_yr){return E(_yq)[1]>E(_yr)[1];},_ys=function(_yt,_yu){return E(_yt)[1]>=E(_yu)[1];},_yv=function(_yw,_yx){var _yy=E(_yw)[1],_yz=E(_yx)[1];return _yy>=_yz?_yy!=_yz?2:1:0;},_yA=function(_yB,_yC){var _yD=E(_yB),_yE=E(_yC);return _yD[1]>_yE[1]?E(_yD):E(_yE);},_yF=function(_yG,_yH){var _yI=E(_yG),_yJ=E(_yH);return _yI[1]>_yJ[1]?E(_yJ):E(_yI);},_yK=[0,_yi,_yv,_yj,_ys,_yp,_ym,_yA,_yF],_yL=function(_yM,_yN){while(1){var _yO=(function(_yP,_yQ){var _yR=E(_rH)[1]["v"]["i8"][(255&_yP>>>0)>>>0&4294967295];if(_yQ>_yR){if(_yR>=8){var _yS=_yP>>8,_yT=_yQ-8|0;_yM=_yS;_yN=_yT;return null;}else{return [0,new T(function(){return B(_6q(_yP>>_yR));}),_yQ-_yR|0];}}else{return [0,new T(function(){return B(_6q(_yP>>_yQ));}),0];}})(_yM,_yN);if(_yO!=null){return _yO;}}},_yU=function(_yV){var _yW=decodeFloat(_yV),_yX=_yW[1],_yY=_yW[2];if(_yY<0){var _yZ=function(_z0){if(!_z0){return [0,B(_6q(_yX)),B(_sk(_ru, -_yY))];}else{var _z1=B(_yL(_yX, -_yY));return [0,E(_z1[1]),B(_sk(_ru,_z1[2]))];}};return (_yX>>>0&1)>>>0==0?B(_yZ(1)):B(_yZ(0));}else{return [0,B(_sk(B(_6q(_yX)),_yY)),_ru];}},_z2=function(_z3){var _z4=B(_yU(E(_z3)[1]));return [0,E(_z4[1]),E(_z4[2])];},_z5=[0,_pw,_yK,_z2],_z6=[0,-1],_z7=[0,1],_z8=function(_z9,_za){var _zb=E(_z9);return _zb[0]==0?_zb[1]*Math.pow(2,_za):I_toNumber(_zb[1])*Math.pow(2,_za);},_zc=[0,0],_zd=function(_ze,_zf){var _zg=decodeFloat(_zf),_zh=_zg[1],_zi=_zg[2],_zj=new T(function(){return B(_w6(new T(function(){return B(_sA(_ze));})));});if(_zi<0){var _zk=new T(function(){if(_zh<0){var _zl= -_zi;if(_zl<32){var _zm=[0, -( -_zh>>_zl)];}else{var _zm= -_zh>=0?E(_zc):E(_z7);}var _zn=_zm,_zo=_zn,_zp=_zo;}else{var _zq= -_zi;if(_zq<32){var _zr=[0,_zh>>_zq];}else{var _zr=_zh>=0?E(_zc):E(_z6);}var _zs=_zr,_zt=_zs,_zp=_zt;}var _zu=_zp;return _zu;});return [0,new T(function(){return B(A(_wu,[_zj,new T(function(){return B(_6q(E(_zk)[1]));})]));}),new T(function(){var _zv= -_zi;if(_zv<32){var _zw=[0,B(_z8(B(_6q(_zh-(E(_zk)[1]<<_zv)|0)),_zi))];}else{var _zw=[0,B(_z8(B(_6q(_zh)),_zi))];}var _zx=_zw,_zy=_zx,_zz=_zy;return _zz;})];}else{return [0,new T(function(){return B(A(_wm,[_zj,new T(function(){return B(A(_wu,[_zj,new T(function(){return B(_6q(_zh));})]));}),new T(function(){return B(_wG(_zj,_w5,new T(function(){return B(A(_wu,[_zj,_qc]));}),[0,_zi]));})]));}),_pf];}},_zA=function(_zB,_zC){var _zD=B(_zd(_zB,E(_zC)[1])),_zE=_zD[1];if(E(_zD[2])[1]<=0){return E(_zE);}else{var _zF=E(B(_sA(_zB))[1]);return new F(function(){return A(_zF[1],[_zE,new T(function(){return B(A(_zF[7],[_ru]));})]);});}},_zG=function(_zH,_zI){var _zJ=B(_zd(_zH,E(_zI)[1])),_zK=_zJ[1];if(E(_zJ[2])[1]>=0){return E(_zK);}else{var _zL=E(B(_sA(_zH))[1]);return new F(function(){return A(_zL[3],[_zK,new T(function(){return B(A(_zL[7],[_ru]));})]);});}},_zM=function(_zN,_zO){var _zP=B(_zd(_zN,E(_zO)[1]));return [0,_zP[1],_zP[2]];},_zQ=function(_zR,_zS){var _zT=B(_zd(_zR,_zS)),_zU=_zT[1],_zV=E(_zT[2])[1],_zW=new T(function(){var _zX=E(B(_sA(_zR))[1]),_zY=_zX[7];return _zV>=0?B(A(_zX[1],[_zU,new T(function(){return B(A(_zY,[_ru]));})])):B(A(_zX[3],[_zU,new T(function(){return B(A(_zY,[_ru]));})]));});if(_zV<0){var _zZ= -_zV-0.5;if(_zZ>=0){if(!E(_zZ)){var _A0=E(_zR),_A1=E(_A0[1]);return !B(_ww(_A1[1],_A1[2],_A1[3],_A0[4],_zU))?E(_zW):E(_zU);}else{return E(_zW);}}else{return E(_zU);}}else{var _A2=_zV-0.5;if(_A2>=0){if(!E(_A2)){var _A3=E(_zR),_A4=E(_A3[1]);return !B(_ww(_A4[1],_A4[2],_A4[3],_A3[4],_zU))?E(_zW):E(_zU);}else{return E(_zW);}}else{return E(_zU);}}},_A5=function(_A6,_A7){return new F(function(){return _zQ(_A6,E(_A7)[1]);});},_A8=function(_A9,_Aa){return E(B(_zd(_A9,E(_Aa)[1]))[1]);},_Ab=[0,_z5,_pA,_zM,_A8,_A5,_zA,_zG],_Ac=function(_Ad){return new F(function(){return err(B(unAppCStr("Char.intToDigit: not a digit ",new T(function(){if(_Ad>=0){var _Ae=jsShowI(_Ad),_Af=_Ae,_Ag=fromJSStr(_Af);}else{var _Ah=jsShowI(_Ad),_Ai=_Ah,_Ag=fromJSStr(_Ai);}var _Aj=_Ag;return _Aj;}))));});},_Ak=function(_Al){var _Am=function(_An){if(_Al<10){return new F(function(){return _Ac(_Al);});}else{if(_Al>15){return new F(function(){return _Ac(_Al);});}else{return (97+_Al|0)-10|0;}}};if(_Al<0){return new F(function(){return _Am(_);});}else{if(_Al>9){return new F(function(){return _Am(_);});}else{return 48+_Al|0;}}},_Ao=function(_Ap){return [0,B(_Ak(E(_Ap)[1]))];},_Aq=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_Ar=function(_As){return new F(function(){return _3f([0,new T(function(){return B(_3u(_As,_Aq));})],_3c);});},_At=new T(function(){return B(_Ar("GHC/Float.lhs:619:11-64|d : ds\'"));}),_Au=function(_Av,_Aw){if(E(_Av)[1]<=0){var _Ax=B(_1p(_Ao,[1,_zc,_Aw]));return _Ax[0]==0?E(_At):[0,_Ax[1],_Ax[2]];}else{var _Ay=B(_1p(_Ao,_Aw));return _Ay[0]==0?E(_At):[0,_Ay[1],_Ay[2]];}},_Az=function(_AA){return E(E(_AA)[1]);},_AB=function(_AC){return E(E(_AC)[1]);},_AD=function(_AE){return E(E(_AE)[1]);},_AF=[0,48],_AG=[1,_AF,_o],_AH=[0,46],_AI=function(_AJ,_AK,_AL){while(1){var _AM=(function(_AN,_AO,_AP){var _AQ=E(_AN);if(!_AQ){var _AR=B(_n4(_AO,_o));return _AR[0]==0?[1,_AF,[1,_AH,new T(function(){var _AS=E(_AP);return _AS[0]==0?E(_AG):E(_AS);})]]:B(_I(_AR,[1,_AH,new T(function(){var _AT=E(_AP);return _AT[0]==0?E(_AG):E(_AT);})]));}else{var _AU=E(_AP);if(!_AU[0]){_AJ=_AQ-1|0;var _AV=[1,_AF,_AO];_AL=_o;_AK=_AV;return null;}else{_AJ=_AQ-1|0;var _AV=[1,_AU[1],_AO];_AL=_AU[2];_AK=_AV;return null;}}})(_AJ,_AK,_AL);if(_AM!=null){return _AM;}}},_AW=[0,0],_AX=new T(function(){return B(unCStr(" out of range "));}),_AY=new T(function(){return B(unCStr("}.index: Index "));}),_AZ=new T(function(){return B(unCStr("Ix{"));}),_B0=[1,_10,_o],_B1=[1,_10,_B0],_B2=function(_B3,_B4,_B5,_B6,_B7){return new F(function(){return err(B(_I(_AZ,new T(function(){return B(_I(_B3,new T(function(){return B(_I(_AY,[1,_11,new T(function(){return B(A(_B7,[_AW,_B4,[1,_10,new T(function(){return B(_I(_AX,[1,_11,[1,_11,new T(function(){return B(A(_kv,[_kl,[1,new T(function(){return B(A(_B7,[_jQ,_B5]));}),[1,new T(function(){return B(A(_B7,[_jQ,_B6]));}),_o]],_B1]));})]]));})]]));})]));})));}))));});},_B8=function(_B9,_Ba,_Bb,_Bc){var _Bd=E(_Bb);return new F(function(){return _B2(_B9,_Ba,_Bd[1],_Bd[2],E(_Bc)[1]);});},_Be=function(_Bf,_Bg,_Bh,_Bi){return new F(function(){return _B8(_Bi,_Bh,_Bg,_Bf);});},_Bj=new T(function(){return B(unCStr("Int"));}),_Bk=function(_Bl,_Bm,_Bn){return new F(function(){return _Be(_kk,[0,_Bm,_Bn],_Bl,_Bj);});},_Bo=new T(function(){return B(unCStr("(Array.!): undefined array element"));}),_Bp=new T(function(){return B(err(_Bo));}),_Bq=[0,1100],_Br=[0,_zc,_Bq],_Bs=function(_Bt){return new F(function(){return _Be(_kk,_Br,[0,_Bt],_Bj);});},_Bu=function(_){var _Bv=newArr(1101,_Bp),_Bw=_Bv;return new F(function(){return (function(_Bx,_){while(1){var _By=(function(_Bz,_){if(0>_Bz){return new F(function(){return _Bs(_Bz);});}else{if(_Bz>1100){return new F(function(){return _Bs(_Bz);});}else{var _=_Bw[_Bz]=new T(function(){if(_Bz>=0){var _BA=E(_Bz),_BB=_BA==0?E(_sC):B(_wg(_qc,_BA));}else{var _BB=E(_xb);}var _BC=_BB;return _BC;}),_BD=E(_Bz);if(_BD==1100){var _BE=_Bw,_BF=_BE;return [0,E(_zc),E(_Bq),1101,_BF];}else{_Bx=_BD+1|0;return null;}}}})(_Bx,_);if(_By!=null){return _By;}}})(0,_);});},_BG=function(_BH){var _BI=B(A(_BH,[_])),_BJ=_BI;return E(_BJ);},_BK=new T(function(){return B(_BG(_Bu));}),_BL=[0,10],_BM=[0,324],_BN=[0,_zc,_BM],_BO=function(_BP){return new F(function(){return _Be(_kk,_BN,[0,_BP],_Bj);});},_BQ=function(_){var _BR=newArr(325,_Bp),_BS=_BR;return new F(function(){return (function(_BT,_){while(1){var _BU=(function(_BV,_){if(0>_BV){return new F(function(){return _BO(_BV);});}else{if(_BV>324){return new F(function(){return _BO(_BV);});}else{var _=_BS[_BV]=new T(function(){if(_BV>=0){var _BW=E(_BV),_BX=_BW==0?E(_sC):B(_wg(_BL,_BW));}else{var _BX=E(_xb);}var _BY=_BX;return _BY;}),_BZ=E(_BV);if(_BZ==324){var _C0=_BS,_C1=_C0;return [0,E(_zc),E(_BM),325,_C1];}else{_BT=_BZ+1|0;return null;}}}})(_BT,_);if(_BU!=null){return _BU;}}})(0,_);});},_C2=new T(function(){return B(_BG(_BQ));}),_C3=function(_C4,_C5){var _C6=[0,_C5],_C7=function(_C8){if(!B(_nv(_C4,_BL))){if(_C5>=0){var _C9=E(_C5);return _C9==0?E(_sC):B(_wg(_C4,_C9));}else{return E(_xb);}}else{if(_C5>324){if(_C5>=0){var _Ca=E(_C5);return _Ca==0?E(_sC):B(_wg(_C4,_Ca));}else{return E(_xb);}}else{var _Cb=E(_C2),_Cc=E(_Cb[1]),_Cd=_Cc[1],_Ce=E(_Cb[2]);if(_Cd>_C5){return new F(function(){return _Bk(_C6,_Cc,_Ce);});}else{if(_C5>_Ce[1]){return new F(function(){return _Bk(_C6,_Cc,_Ce);});}else{return E(_Cb[4][_C5-_Cd|0]);}}}}};if(!B(_nv(_C4,_qc))){return new F(function(){return _C7(_);});}else{if(_C5<0){return new F(function(){return _C7(_);});}else{if(_C5>1100){return new F(function(){return _C7(_);});}else{var _Cf=E(_BK),_Cg=E(_Cf[1]),_Ch=_Cg[1],_Ci=E(_Cf[2]);if(_Ch>_C5){return new F(function(){return _Bk(_C6,_Cg,_Ci);});}else{if(_C5>_Ci[1]){return new F(function(){return _Bk(_C6,_Cg,_Ci);});}else{return E(_Cf[4][_C5-_Ch|0]);}}}}}},_Cj=function(_Ck,_Cl){var _Cm=E(_Ck);if(!_Cm[0]){var _Cn=_Cm[1],_Co=E(_Cl);return _Co[0]==0?_Cn>_Co[1]:I_compareInt(_Co[1],_Cn)<0;}else{var _Cp=_Cm[1],_Cq=E(_Cl);return _Cq[0]==0?I_compareInt(_Cp,_Cq[1])>0:I_compare(_Cp,_Cq[1])>0;}},_Cr=[1,_zc,_o],_Cs=function(_Ct,_Cu){while(1){var _Cv=E(_Ct);if(!_Cv[0]){var _Cw=E(_Cv[1]);if(_Cw==(-2147483648)){_Ct=[1,I_fromInt(-2147483648)];continue;}else{var _Cx=E(_Cu);if(!_Cx[0]){return [0,quot(_Cw,_Cx[1])];}else{_Ct=[1,I_fromInt(_Cw)];_Cu=_Cx;continue;}}}else{var _Cy=_Cv[1],_Cz=E(_Cu);return _Cz[0]==0?[0,I_toInt(I_quot(_Cy,I_fromInt(_Cz[1])))]:[1,I_quot(_Cy,_Cz[1])];}}},_CA=function(_CB,_CC,_CD,_CE,_CF,_CG,_CH,_CI){if(!B(A(_CB,[_CI,new T(function(){return B(A(_wu,[B(_AB(B(_Az(_CC)))),_np]));})]))){var _CJ=new T(function(){return B(A(_CD,[_CI]));}),_CK=new T(function(){return B(A(_CE,[_CI]));}),_CL=new T(function(){return [0,E(B(A(_CF,[_CI]))[1])[1]-E(_CK)[1]|0];}),_CM=new T(function(){return B(A(_CG,[_CI]));}),_CN=new T(function(){return E(E(_CM)[2]);}),_CO=new T(function(){var _CP=E(_CN),_CQ=_CP[1],_CR=E(_CL)[1]-_CQ|0;if(_CR<=0){var _CS=[0,new T(function(){return E(E(_CM)[1]);}),_CP];}else{var _CS=[0,new T(function(){var _CT=B(_C3(_CJ,_CR));if(!B(_nv(_CT,_np))){var _CU=B(_Cs(E(_CM)[1],_CT));}else{var _CU=E(_u5);}var _CV=_CU;return _CV;}),[0,_CQ+_CR|0]];}var _CW=_CS,_CX=_CW,_CY=_CX,_CZ=_CY;return _CZ;}),_D0=new T(function(){return E(E(_CO)[2]);}),_D1=new T(function(){return E(E(_CO)[1]);}),_D2=new T(function(){var _D3=E(_D0)[1];if(_D3<0){if(_D3<=E(_CL)[1]){var _D4=[0,new T(function(){return B(_6s(_D1,_qc));}),new T(function(){return B(_6s(B(_C3(_CJ, -_D3)),_qc));}),_ru,_ru];}else{var _D4=!B(_nv(_D1,B(_C3(_CJ,E(_CK)[1]-1|0))))?[0,new T(function(){return B(_6s(_D1,_qc));}),new T(function(){return B(_6s(B(_C3(_CJ, -_D3)),_qc));}),_ru,_ru]:[0,new T(function(){return B(_6s(B(_6s(_D1,_CJ)),_qc));}),new T(function(){return B(_6s(B(_C3(_CJ, -_D3+1|0)),_qc));}),_CJ,_ru];}var _D5=_D4,_D6=_D5,_D7=_D6;}else{var _D8=new T(function(){return B(_C3(_CJ,_D3));}),_D7=!B(_nv(_D1,B(_C3(_CJ,E(_CK)[1]-1|0))))?[0,new T(function(){return B(_6s(B(_6s(_D1,_D8)),_qc));}),_qc,_D8,_D8]:[0,new T(function(){return B(_6s(B(_6s(B(_6s(_D1,_D8)),_CJ)),_qc));}),new T(function(){return B(_6s(_qc,_CJ));}),new T(function(){return B(_6s(_D8,_CJ));}),_D8];}var _D9=_D7,_Da=_D9;return _Da;}),_Db=new T(function(){return E(E(_D2)[2]);}),_Dc=new T(function(){return E(E(_D2)[3]);}),_Dd=new T(function(){return E(E(_D2)[1]);}),_De=new T(function(){var _Df=new T(function(){return B(_6a(_Dd,_Dc));}),_Dg=function(_Dh){var _Di=(Math.log(B(_pa(B(_6a(_D1,_ru)))))+E(_D0)[1]*Math.log(B(_pa(_CJ))))/Math.log(B(_pa(_CH))),_Dj=_Di&4294967295;return _Dj>=_Di?E(_Dj):_Dj+1|0;},_Dk=function(_Dl){while(1){if(_Dl<0){if(!B(_7U(B(_6s(B(_C3(_CH, -_Dl)),_Df)),_Db))){var _Dm=_Dl+1|0;_Dl=_Dm;continue;}else{return E(_Dl);}}else{if(!B(_7U(_Df,B(_6s(B(_C3(_CH,_Dl)),_Db))))){var _Dm=_Dl+1|0;_Dl=_Dm;continue;}else{return E(_Dl);}}}};if(!B(_nv(_CJ,_qc))){var _Dn=[0,B(_Dk(B(_Dg(_))))];}else{if(!B(_nv(_CH,_BL))){var _Do=[0,B(_Dk(B(_Dg(_))))];}else{var _Dp=(E(_CK)[1]-1|0)+E(_CN)[1]|0;if(_Dp<0){var _Dq=[0,B(_Dk(quot(imul(_Dp,8651)|0,28738)))];}else{var _Dq=[0,B(_Dk(quot(imul(_Dp,8651)|0,28738)+1|0))];}var _Dr=_Dq,_Ds=_Dr,_Dt=_Ds,_Du=_Dt,_Dv=_Du,_Do=_Dv;}var _Dn=_Do;}return _Dn;});return [0,new T(function(){var _Dw=E(_De)[1],_Dx=function(_Dy,_Dz,_DA,_DB,_DC){while(1){var _DD=(function(_DE,_DF,_DG,_DH,_DI){if(!B(_nv(_DG,_np))){var _DJ=B(_xg(B(_6s(_DF,_CH)),_DG)),_DK=_DJ[1],_DL=_DJ[2],_DM=B(_6s(_DI,_CH)),_DN=B(_6s(_DH,_CH));if(!B(_S(_DL,_DM))){if(!B(_Cj(B(_6a(_DL,_DN)),_DG))){var _DO=[1,_DK,_DE];_Dz=_DL;var _DP=_DG;_DB=_DN;_DC=_DM;_Dy=_DO;_DA=_DP;return null;}else{return [1,new T(function(){return B(_6a(_DK,_ru));}),_DE];}}else{return !B(_Cj(B(_6a(_DL,_DN)),_DG))?[1,_DK,_DE]:!B(_S(B(_6s(_DL,_qc)),_DG))?[1,new T(function(){return B(_6a(_DK,_ru));}),_DE]:[1,_DK,_DE];}}else{return E(_u5);}})(_Dy,_Dz,_DA,_DB,_DC);if(_DD!=null){return _DD;}}};if(_Dw<0){var _DQ=B(_C3(_CH, -_Dw)),_DR=B(_1p(_vj,B(_n4(B(_Dx(_o,B(_6s(_Dd,_DQ)),_Db,B(_6s(_Dc,_DQ)),B(_6s(E(_D2)[4],_DQ)))),_o))));}else{var _DR=B(_1p(_vj,B(_n4(B(_Dx(_o,_Dd,B(_6s(_Db,B(_C3(_CH,_Dw)))),_Dc,E(_D2)[4])),_o))));}var _DS=_DR,_DT=_DS;return _DT;}),_De];}else{return [0,_Cr,_zc];}},_DU=function(_DV,_DW){while(1){var _DX=E(_DW);if(!_DX[0]){return true;}else{if(!B(A(_DV,[_DX[1]]))){return false;}else{_DW=_DX[2];continue;}}}},_DY=function(_DZ){return E(_DZ)[1]%2==0?true:false;},_E0=new T(function(){return B(unCStr("roundTo: bad Value"));}),_E1=new T(function(){return B(err(_E0));}),_E2=function(_E3){return E(E(_E3)[1])==0?true:false;},_E4=function(_E5){return _E5>1?[1,_zc,new T(function(){return B(_E4(_E5-1|0));})]:E(_Cr);},_E6=function(_E7,_E8,_E9){var _Ea=function(_Eb,_Ec,_Ed){var _Ee=E(_Ed);if(!_Ee[0]){return [0,_zc,new T(function(){var _Ef=E(_Eb)[1];return _Ef>0?B(_E4(_Ef)):[0];})];}else{var _Eg=_Ee[1],_Eh=_Ee[2],_Ei=E(E(_Eb)[1]);if(!_Ei){var _Ej=E(_Eg)[1],_Ek=E(new T(function(){return [0,quot(E(_E7)[1],2)];}))[1];return _Ej!=_Ek?[0,new T(function(){return _Ej<_Ek?E(_zc):E(_z7);}),_o]:!E(_Ec)?[0,new T(function(){return _Ej<_Ek?E(_zc):E(_z7);}),_o]:!B(_DU(_E2,_Eh))?[0,new T(function(){return _Ej<_Ek?E(_zc):E(_z7);}),_o]:[0,_zc,_o];}else{var _El=B(_Ea([0,_Ei-1|0],new T(function(){return B(_DY(_Eg));}),_Eh)),_Em=_El[2],_En=E(_El[1])[1]+E(_Eg)[1]|0;return _En!=E(_E7)[1]?[0,_zc,[1,[0,_En],_Em]]:[0,_z7,[1,_zc,_Em]];}}},_Eo=B(_Ea(_E8,_h,_E9));switch(E(E(_Eo[1])[1])){case 0:return E(_Eo);case 1:return [0,_z7,[1,_z7,_Eo[2]]];default:return E(_E1);}},_Ep=function(_Eq,_Er){var _Es=E(_Eq);if(!_Es){return [0,_o,_Er];}else{var _Et=E(_Er);if(!_Et[0]){return [0,_o,_o];}else{var _Eu=new T(function(){var _Ev=B(_Ep(_Es-1|0,_Et[2]));return [0,_Ev[1],_Ev[2]];});return [0,[1,_Et[1],new T(function(){return E(E(_Eu)[1]);})],new T(function(){return E(E(_Eu)[2]);})];}}},_Ew=function(_Ex){return E(E(_Ex)[3]);},_Ey=0,_Ez=1,_EA=[0,10],_EB=new T(function(){return B(unCStr("e0"));}),_EC=function(_ED,_EE){var _EF=E(_ED);if(!_EF[0]){return E(_EB);}else{var _EG=_EF[1];return _EE>1?[1,_EG,new T(function(){return B(_EC(_EF[2],_EE-1|0));})]:[1,_EG,_EB];}},_EH=function(_EI,_EJ){var _EK=E(_EJ);return _EK[0]==0?[0]:[1,_EI,new T(function(){return B(_EH(_EK[1],_EK[2]));})];},_EL=new T(function(){return B(unCStr("init"));}),_EM=new T(function(){return B(_kr(_EL));}),_EN=new T(function(){return B(_Ar("GHC/Float.lhs:591:12-70|(d : ds\')"));}),_EO=[0,101],_EP=new T(function(){return B(unCStr("Infinity"));}),_EQ=new T(function(){return B(unCStr("-Infinity"));}),_ER=new T(function(){return B(unCStr("NaN"));}),_ES=new T(function(){return B(unCStr("formatRealFloat/doFmt/FFExponent: []"));}),_ET=new T(function(){return B(err(_ES));}),_EU=new T(function(){return B(unCStr("0.0e0"));}),_EV=function(_EW){return E(E(_EW)[4]);},_EX=new T(function(){return [1,_AF,_EX];}),_EY=function(_EZ,_F0,_F1,_F2,_F3,_F4,_F5,_F6,_F7,_F8,_F9,_Fa){if(!B(A(_F5,[_Fa]))){var _Fb=new T(function(){return B(_AB(new T(function(){return B(_Az(_F0));})));});if(!B(A(_F6,[_Fa]))){var _Fc=function(_Fd,_Fe,_Ff){while(1){var _Fg=(function(_Fh,_Fi,_Fj){switch(E(_Fh)){case 0:var _Fk=E(_F9);if(!_Fk[0]){var _Fl=B(_1p(_Ao,_Fi));if(!_Fl[0]){return E(_ET);}else{var _Fm=_Fl[2],_Fn=E(_Fl[1]),_Fo=function(_Fp){var _Fq=E(_Fm);return _Fq[0]==0?[1,_Fn,new T(function(){return B(unAppCStr(".0e",new T(function(){return B(_7J(0,E(_Fj)[1]-1|0,_o));})));})]:[1,_Fn,[1,_AH,new T(function(){return B(_I(_Fq,[1,_EO,new T(function(){return B(_7J(0,E(_Fj)[1]-1|0,_o));})]));})]];};return E(_Fn[1])==48?E(_Fm)[0]==0?E(_EU):B(_Fo(_)):B(_Fo(_));}}else{var _Fr=new T(function(){var _Fs=E(_Fk[1]);return _Fs[1]>1?E(_Fs):E(_z7);}),_Ft=function(_Fu){var _Fv=new T(function(){var _Fw=B(_E6(_EA,new T(function(){return [0,E(_Fr)[1]+1|0];}),_Fi));return [0,_Fw[1],_Fw[2]];}),_Fx=new T(function(){return E(E(_Fv)[1]);}),_Fy=new T(function(){if(E(_Fx)[1]<=0){var _Fz=B(_1p(_Ao,E(_Fv)[2])),_FA=_Fz[0]==0?E(_EN):[0,_Fz[1],_Fz[2]];}else{var _FB=E(E(_Fv)[2]);if(!_FB[0]){var _FC=E(_EM);}else{var _FD=B(_1p(_Ao,B(_EH(_FB[1],_FB[2])))),_FC=_FD[0]==0?E(_EN):[0,_FD[1],_FD[2]];}var _FE=_FC,_FA=_FE;}var _FF=_FA,_FG=_FF;return _FG;});return [1,new T(function(){return E(E(_Fy)[1]);}),[1,_AH,new T(function(){return B(_I(E(_Fy)[2],[1,_EO,new T(function(){return B(_7J(0,(E(_Fj)[1]-1|0)+E(_Fx)[1]|0,_o));})]));})]];},_FH=E(_Fi);if(!_FH[0]){return new F(function(){return _Ft(_);});}else{return E(E(_FH[1])[1])==0?E(_FH[2])[0]==0?[1,_AF,[1,_AH,new T(function(){var _FI=E(_Fr)[1];return _FI>0?B(_EC(_EX,_FI)):E(_EB);})]]:B(_Ft(_)):B(_Ft(_));}}break;case 1:var _FJ=E(_F9);if(!_FJ[0]){var _FK=E(_Fj)[1];return _FK>0?B(_AI(_FK,_o,new T(function(){return B(_1p(_Ao,_Fi));}))):B(unAppCStr("0.",new T(function(){var _FL= -_FK;if(_FL>0){var _FM=function(_FN){return _FN>1?[1,_AF,new T(function(){return B(_FM(_FN-1|0));})]:E([1,_AF,new T(function(){return B(_1p(_Ao,_Fi));})]);},_FO=B(_FM(_FL));}else{var _FO=B(_1p(_Ao,_Fi));}var _FP=_FO,_FQ=_FP;return _FQ;})));}else{var _FR=_FJ[1],_FS=E(_Fj),_FT=_FS[1];if(_FT<0){var _FU=new T(function(){var _FV= -_FT;if(_FV>0){var _FW=function(_FX){return _FX>1?[1,_zc,new T(function(){return B(_FW(_FX-1|0));})]:E([1,_zc,_Fi]);},_FY=B(_E6(_EA,new T(function(){var _FZ=E(_FR);return _FZ[1]>0?E(_FZ):E(_zc);}),B(_FW(_FV)))),_G0=B(_Au(_FY[1],_FY[2]));}else{var _G1=B(_E6(_EA,new T(function(){var _G2=E(_FR);return _G2[1]>0?E(_G2):E(_zc);}),_Fi)),_G0=B(_Au(_G1[1],_G1[2]));}var _G3=_G0,_G4=_G3;return _G4;});return [1,new T(function(){return E(E(_FU)[1]);}),new T(function(){var _G5=E(E(_FU)[2]);return _G5[0]==0?[0]:[1,_AH,_G5];})];}else{var _G6=B(_E6(_EA,new T(function(){var _G7=E(_FR)[1];if(_G7>0){var _G8=[0,_G7+_FT|0];}else{var _G8=E(_FS);}var _G9=_G8,_Ga=_G9;return _Ga;}),_Fi)),_Gb=_G6[2],_Gc=_FT+E(_G6[1])[1]|0;if(_Gc>=0){var _Gd=B(_Ep(_Gc,new T(function(){return B(_1p(_Ao,_Gb));}))),_Ge=_Gd[2],_Gf=E(_Gd[1]);return _Gf[0]==0?[1,_AF,new T(function(){var _Gg=E(_Ge);return _Gg[0]==0?[0]:[1,_AH,_Gg];})]:B(_I(_Gf,new T(function(){var _Gh=E(_Ge);return _Gh[0]==0?[0]:[1,_AH,_Gh];})));}else{return [1,_AF,new T(function(){var _Gi=B(_1p(_Ao,_Gb));return _Gi[0]==0?[0]:[1,_AH,_Gi];})];}}}break;default:var _Gj=E(_Fj),_Gk=_Gj[1];if(_Gk>=0){if(_Gk<=7){_Fd=_Ez;var _Gl=_Fi;_Ff=_Gj;_Fe=_Gl;return null;}else{_Fd=_Ey;var _Gl=_Fi;_Ff=_Gj;_Fe=_Gl;return null;}}else{_Fd=_Ey;var _Gl=_Fi;_Ff=_Gj;_Fe=_Gl;return null;}}})(_Fd,_Fe,_Ff);if(_Fg!=null){return _Fg;}}},_Gm=function(_Gn){return [1,_l3,new T(function(){var _Go=B(_CA(E(E(E(E(_EZ)[1])[2])[1])[1],_F0,_F1,_F2,_F3,_F4,_BL,new T(function(){return B(A(_EV,[_Fb,_Fa]));})));return B(_Fc(_F8,_Go[1],_Go[2]));})];};if(!B(A(_Ew,[B(_wq(B(_AD(_EZ)))),_Fa,new T(function(){return B(A(_wu,[_Fb,_np]));})]))){if(!B(A(_F7,[_Fa]))){var _Gp=B(_CA(E(E(E(E(_EZ)[1])[2])[1])[1],_F0,_F1,_F2,_F3,_F4,_BL,_Fa));return new F(function(){return _Fc(_F8,_Gp[1],_Gp[2]);});}else{return new F(function(){return _Gm(_);});}}else{return new F(function(){return _Gm(_);});}}else{return !B(A(_Ew,[B(_wq(B(_AD(_EZ)))),_Fa,new T(function(){return B(A(_wu,[_Fb,_np]));})]))?E(_EP):E(_EQ);}}else{return E(_ER);}},_Gq=function(_Gr){var _Gs=u_towlower(_Gr),_Gt=_Gs;return _Gt>>>0>1114111?B(_7P(_Gt)):_Gt;},_Gu=function(_Gv){return new F(function(){return err(B(unAppCStr("Printf.printf: ",_Gv)));});},_Gw=new T(function(){return B(unCStr("bad argument"));}),_Gx=new T(function(){return B(_Gu(_Gw));}),_Gy=new T(function(){return B(unCStr("Printf.dfmt: impossible"));}),_Gz=new T(function(){return B(err(_Gy));}),_GA=[0,45],_GB=[1,_GA,_o],_GC=new T(function(){return B(err(_Gy));}),_GD=new T(function(){return B(unCStr("Negative exponent"));}),_GE=new T(function(){return B(err(_GD));}),_GF=function(_GG,_GH,_GI){while(1){if(!(_GH%2)){var _GJ=_GG*_GG,_GK=quot(_GH,2);_GG=_GJ;_GH=_GK;continue;}else{var _GL=E(_GH);if(_GL==1){return _GG*_GI;}else{var _GJ=_GG*_GG;_GH=quot(_GL-1|0,2);var _GM=_GG*_GI;_GG=_GJ;_GI=_GM;continue;}}}},_GN=function(_GO,_GP){while(1){if(!(_GP%2)){var _GQ=_GO*_GO,_GR=quot(_GP,2);_GO=_GQ;_GP=_GR;continue;}else{var _GS=E(_GP);if(_GS==1){return E(_GO);}else{return new F(function(){return _GF(_GO*_GO,quot(_GS-1|0,2),_GO);});}}}},_GT=function(_GU,_GV){var _GW=E(_GU);return _GW[0]==0?function(_5l){return new F(function(){return _I(new T(function(){var _GX=jsShow(E(_GV)[1]),_GY=_GX;return fromJSStr(_GY);}),_5l);});}:function(_5l){return new F(function(){return _I(new T(function(){var _GZ=E(E(_GW[1])[1]);if(!_GZ){var _H0=jsRound(E(_GV)[1]),_H1=_H0,_H2=B(_q3(_H1)),_H3=_H2[1],_H4=_H2[2];if(_H4>=0){var _H5=jsShow(B(_nP(B(_sk(_H3,_H4))))),_H6=_H5,_H7=fromJSStr(_H6);}else{var _H8=hs_uncheckedIShiftRA64(B(_sc(_H3)), -_H4),_H9=_H8,_Ha=jsShow(B(_nP(B(_rv(_H9))))),_Hb=_Ha,_H7=fromJSStr(_Hb);}var _Hc=_H7,_Hd=_Hc,_He=_Hd,_Hf=_He;}else{if(_GZ>=0){var _Hg=B(_GN(10,_GZ)),_Hh=jsRound(E(_GV)[1]*_Hg),_Hi=_Hh,_Hj=jsShow((_Hi&4294967295)/_Hg),_Hk=_Hj,_Hl=fromJSStr(_Hk);}else{var _Hl=E(_GE);}var _Hm=_Hl,_Hn=_Hm,_Hf=_Hn;}var _Ho=_Hf;return _Ho;}),_5l);});};},_Hp=function(_Hq,_Hr){var _Hs=E(_Hq);return _Hs[0]==0?function(_5l){return new F(function(){return _I(new T(function(){var _Ht=B(_yU(E(_Hr)[1])),_Hu=jsShow(B(_nD(_Ht[1],_Ht[2]))[1]),_Hv=_Hu;return fromJSStr(_Hv);}),_5l);});}:function(_5l){return new F(function(){return _I(new T(function(){var _Hw=E(E(_Hs[1])[1]);if(!_Hw){var _Hx=jsRound(E(_Hr)[1]),_Hy=_Hx,_Hz=decodeFloat(_Hy),_HA=_Hz[1],_HB=_Hz[2];if(_HB>=0){var _HC=jsShow(B(_nP(B(_sk(B(_6q(_HA)),_HB))))),_HD=_HC,_HE=fromJSStr(_HD);}else{var _HF=jsShow(_HA>> -_HB),_HG=_HF,_HE=fromJSStr(_HG);}var _HH=_HE,_HI=_HH,_HJ=_HI,_HK=_HJ;}else{var _HL=B(_yU(E(_Hr)[1]));if(_Hw>=0){var _HM=B(_GN(10,_Hw)),_HN=jsRound(B(_nD(_HL[1],_HL[2]))[1]*_HM),_HO=_HN,_HP=jsShow((_HO&4294967295)/_HM),_HQ=_HP,_HR=fromJSStr(_HQ);}else{var _HR=E(_GE);}var _HS=_HR,_HT=_HS,_HU=_HT,_HV=_HU,_HK=_HV;}var _HW=_HK;return _HW;}),_5l);});};},_HX=function(_HY){var _HZ=u_towupper(_HY),_I0=_HZ;return _I0>>>0>1114111?B(_7P(_I0)):_I0;},_I1=function(_I2){return [0,B(_HX(E(_I2)[1]))];},_I3=function(_I4,_I5,_I6){var _I7=E(_I6);switch(_I7[0]){case 3:var _I8=_I7[1],_I9=u_iswupper(_I4),_Ia=_I9;switch(B(_Gq(_I4))){case 101:var _Ib=B(_EY(_Ab,_q2,_qC,_qA,_qH,_qw,_qN,_qJ,_qR,_Ey,new T(function(){var _Ic=E(_I5);return _Ic[1]>=0?[1,_Ic]:[0];}),_I8));break;case 102:var _Ib=B(_EY(_Ab,_q2,_qC,_qA,_qH,_qw,_qN,_qJ,_qR,_Ez,new T(function(){var _Id=E(_I5);return _Id[1]>=0?[1,_Id]:[0];}),_I8));break;case 103:var _Ie=E(_I5),_Ib=_Ie[1]>=0?B(A(_Hp,[[1,_Ie],_I8,_o])):B(A(_Hp,[_6Q,_I8,_o]));break;default:var _Ib=E(_GC);}var _If=_Ib,_Ig=E(_Ia);if(!_Ig){var _Ih=E(_If);if(!_Ih[0]){return [0,_o,_o];}else{var _Ii=_Ih[1],_Ij=_Ih[2],_Ik=E(_Ii),_Il=_Ik[1],_Im=E(_Il);return _Im==45?[0,_GB,_Ij]:[0,_o,_Ih];}}else{var _In=B(_1p(_I1,_If));if(!_In[0]){return [0,_o,_o];}else{var _Io=_In[1],_Ip=_In[2],_Iq=E(_Io),_Ir=_Iq[1],_Is=E(_Ir);return _Is==45?[0,_GB,_Ip]:[0,_o,_In];}}break;case 4:var _It=_I7[1],_Iu=u_iswupper(_I4),_Iv=_Iu;switch(B(_Gq(_I4))){case 101:var _Iw=B(_EY(_yb,_oH,_qd,_qa,_qi,_q6,_qo,_qk,_qs,_Ey,new T(function(){var _Ix=E(_I5);return _Ix[1]>=0?[1,_Ix]:[0];}),_It));break;case 102:var _Iw=B(_EY(_yb,_oH,_qd,_qa,_qi,_q6,_qo,_qk,_qs,_Ez,new T(function(){var _Iy=E(_I5);return _Iy[1]>=0?[1,_Iy]:[0];}),_It));break;case 103:var _Iz=E(_I5),_Iw=_Iz[1]>=0?B(A(_GT,[[1,_Iz],_It,_o])):B(A(_GT,[_6Q,_It,_o]));break;default:var _Iw=E(_Gz);}var _IA=_Iw,_IB=E(_Iv);if(!_IB){var _IC=E(_IA);if(!_IC[0]){return [0,_o,_o];}else{var _ID=_IC[1],_IE=_IC[2],_IF=E(_ID),_IG=_IF[1],_IH=E(_IG);return _IH==45?[0,_GB,_IE]:[0,_o,_IC];}}else{var _II=B(_1p(_I1,_IA));if(!_II[0]){return [0,_o,_o];}else{var _IJ=_II[1],_IK=_II[2],_IL=E(_IJ),_IM=_IL[1],_IN=E(_IM);return _IN==45?[0,_GB,_IK]:[0,_o,_II];}}break;default:return E(_Gx);}},_IO=function(_IP){return new F(function(){return _13(0,_IP,_o);});},_IQ=function(_IR,_IS){while(1){var _IT=E(_IR);if(!_IT[0]){return E(_IS);}else{_IR=_IT[2];var _IU=_IS+1|0;_IS=_IU;continue;}}},_IV=[0,48],_IW=function(_IX,_IY){var _IZ=_IX-B(_IQ(_IY,0))|0;if(_IZ>0){var _J0=function(_J1){return _J1>1?[1,_IV,new T(function(){return B(_J0(_J1-1|0));})]:E([1,_IV,_IY]);};return new F(function(){return _J0(_IZ);});}else{return E(_IY);}},_J2=[0,0],_J3=[0,-2147483648],_J4=function(_J5,_J6){while(1){var _J7=(function(_J8,_J9){var _Ja=E(_J9);switch(_Ja[0]){case 0:_J5=_J2;_J6=[2,_J3,new T(function(){return B(_6q(E(_Ja[1])[1]));})];return null;case 2:var _Jb=_Ja[2];return !B(_S(_Jb,_mZ))?[0,_o,new T(function(){return B(_IW(E(_J8)[1],B(_IO(_Jb))));})]:[0,_GB,new T(function(){return B(_IW(E(_J8)[1],B(_13(0,B(_6k(_Jb)),_o))));})];default:return E(_Gx);}})(_J5,_J6);if(_J7!=null){return _J7;}}},_Jc=[1,_j1,_o],_Jd=function(_Je){return new F(function(){return err(B(unAppCStr("Char.digitToInt: not a digit ",new T(function(){var _Jf=E(_Je);return _Jf==39?E(_j3):[1,_j1,new T(function(){return B(_iL(_Jf,_Jc));})];}))));});},_Jg=function(_Jh){var _Ji=function(_Jj){var _Jk=function(_Jl){if(_Jh<65){return new F(function(){return _Jd(_Jh);});}else{if(_Jh>70){return new F(function(){return _Jd(_Jh);});}else{return (_Jh-65|0)+10|0;}}};if(_Jh<97){return new F(function(){return _Jk(_);});}else{if(_Jh>102){return new F(function(){return _Jk(_);});}else{return (_Jh-97|0)+10|0;}}};if(_Jh<48){return new F(function(){return _Ji(_);});}else{if(_Jh>57){return new F(function(){return _Ji(_);});}else{return _Jh-48|0;}}},_Jm=function(_Jn,_Jo){while(1){var _Jp=(function(_Jq,_Jr){var _Js=E(_Jr);if(!_Js[0]){return [0,_Jq,_o];}else{var _Jt=E(_Js[1])[1];if(_Jt<48){return [0,_Jq,_Js];}else{if(_Jt>57){return [0,_Jq,_Js];}else{_Jn=new T(function(){return [0,(imul(E(_Jq)[1],10)|0)+B(_Jg(_Jt))|0];});_Jo=_Js[2];return null;}}}})(_Jn,_Jo);if(_Jp!=null){return _Jp;}}},_Ju=new T(function(){return B(unCStr("argument list ended prematurely"));}),_Jv=new T(function(){return B(_Gu(_Ju));}),_Jw=[0,-1],_Jx=function(_Jy){return [0,E(_Jy)[1]];},_Jz=function(_JA){var _JB=E(_JA);switch(_JB[0]){case 0:return new F(function(){return _Jx(_JB[1]);});break;case 2:return new F(function(){return _vj(_JB[2]);});break;default:return E(_Gx);}},_JC=function(_JD,_JE,_JF,_JG,_JH){while(1){var _JI=(function(_JJ,_JK,_JL,_JM,_JN){var _JO=E(_JM);if(!_JO[0]){return [0,_J2,_Jw,_JJ,_JK,_JL,_o,_JN];}else{var _JP=_JO[2],_JQ=E(E(_JO[1])[1]);switch(_JQ){case 42:var _JR=new T(function(){var _JS=E(_JN);return _JS[0]==0?E(_Jv):[0,_JS[2],new T(function(){return B(_Jz(_JS[1]));})];}),_JT=new T(function(){var _JU=E(_JP);if(!_JU[0]){var _JV=[0,_Jw,_o,new T(function(){return E(E(_JR)[1]);})];}else{if(E(E(_JU[1])[1])==46){var _JW=E(_JU[2]);if(!_JW[0]){var _JX=B(_Jm(_J2,_o)),_JY=[0,_JX[1],_JX[2],new T(function(){return E(E(_JR)[1]);})];}else{if(E(E(_JW[1])[1])==42){var _JZ=new T(function(){var _K0=E(E(_JR)[1]);return _K0[0]==0?E(_Jv):[0,_K0[2],new T(function(){return B(_Jz(_K0[1]));})];}),_K1=[0,new T(function(){return E(E(_JZ)[2]);}),_JW[2],new T(function(){return E(E(_JZ)[1]);})];}else{var _K2=B(_Jm(_J2,_JW)),_K1=[0,_K2[1],_K2[2],new T(function(){return E(E(_JR)[1]);})];}var _K3=_K1,_JY=_K3;}var _K4=_JY;}else{var _K4=[0,_Jw,_JU,new T(function(){return E(E(_JR)[1]);})];}var _K5=_K4,_JV=_K5;}return _JV;});return [0,new T(function(){return E(E(_JR)[2]);}),new T(function(){return E(E(_JT)[1]);}),_JJ,_JK,_JL,new T(function(){return E(E(_JT)[2]);}),new T(function(){return E(E(_JT)[3]);})];case 43:var _K6=_JJ,_K7=_JK;_JF=_h;_JG=_JP;var _K8=_JN;_JD=_K6;_JE=_K7;_JH=_K8;return null;case 45:_JD=_h;var _K7=_JK,_K9=_JL;_JG=_JP;var _K8=_JN;_JE=_K7;_JF=_K9;_JH=_K8;return null;case 46:var _Ka=new T(function(){var _Kb=E(_JP);if(!_Kb[0]){var _Kc=B(_Jm(_J2,_o)),_Kd=[0,_Kc[1],_Kc[2],_JN];}else{if(E(E(_Kb[1])[1])==42){var _Ke=new T(function(){var _Kf=E(_JN);return _Kf[0]==0?E(_Jv):[0,_Kf[2],new T(function(){return B(_Jz(_Kf[1]));})];}),_Kg=[0,new T(function(){return E(E(_Ke)[2]);}),_Kb[2],new T(function(){return E(E(_Ke)[1]);})];}else{var _Kh=B(_Jm(_J2,_Kb)),_Kg=[0,_Kh[1],_Kh[2],_JN];}var _Ki=_Kg,_Kd=_Ki;}return _Kd;});return [0,_J2,new T(function(){return E(E(_Ka)[1]);}),_JJ,_JK,_JL,new T(function(){return E(E(_Ka)[2]);}),new T(function(){return E(E(_Ka)[3]);})];case 48:var _K6=_JJ;_JE=_h;var _K9=_JL;_JG=_JP;var _K8=_JN;_JD=_K6;_JF=_K9;_JH=_K8;return null;default:if(_JQ<48){return [0,_J2,_Jw,_JJ,_JK,_JL,_JO,_JN];}else{if(_JQ>57){return [0,_J2,_Jw,_JJ,_JK,_JL,_JO,_JN];}else{var _Kj=new T(function(){var _Kk=B(_Jm(_J2,_JO));return [0,_Kk[1],_Kk[2]];}),_Kl=new T(function(){var _Km=E(E(_Kj)[2]);if(!_Km[0]){var _Kn=[0,_Jw,_o,_JN];}else{if(E(E(_Km[1])[1])==46){var _Ko=E(_Km[2]);if(!_Ko[0]){var _Kp=B(_Jm(_J2,_o)),_Kq=[0,_Kp[1],_Kp[2],_JN];}else{if(E(E(_Ko[1])[1])==42){var _Kr=new T(function(){var _Ks=E(_JN);return _Ks[0]==0?E(_Jv):[0,_Ks[2],new T(function(){return B(_Jz(_Ks[1]));})];}),_Kt=[0,new T(function(){return E(E(_Kr)[2]);}),_Ko[2],new T(function(){return E(E(_Kr)[1]);})];}else{var _Ku=B(_Jm(_J2,_Ko)),_Kt=[0,_Ku[1],_Ku[2],_JN];}var _Kv=_Kt,_Kq=_Kv;}var _Kw=_Kq;}else{var _Kw=[0,_Jw,_Km,_JN];}var _Kx=_Kw,_Kn=_Kx;}var _Ky=_Kn;return _Ky;});return [0,new T(function(){return E(E(_Kj)[1]);}),new T(function(){return E(E(_Kl)[1]);}),_JJ,_JK,_JL,new T(function(){return E(E(_Kl)[2]);}),new T(function(){return E(E(_Kl)[3]);})];}}}}})(_JD,_JE,_JF,_JG,_JH);if(_JI!=null){return _JI;}}},_Kz=new T(function(){return B(unCStr("formatting string ended prematurely"));}),_KA=new T(function(){return B(_Gu(_Kz));}),_KB=function(_KC,_KD){if(!B(_S(_KD,_KC))){if(!B(_nv(_KC,_mZ))){var _KE=B(_xg(_KD,_KC));return new F(function(){return _I(B(_KB(_KC,_KE[1])),[1,new T(function(){return [0,B(_Ak(B(_7R(_KE[2]))))];}),_o]);});}else{return E(_u5);}}else{return [1,new T(function(){return [0,B(_Ak(B(_7R(_KD))))];}),_o];}},_KF=[0,2],_KG=function(_KH,_KI,_KJ){var _KK=E(_KJ);switch(_KK[0]){case 0:return new F(function(){return _KB(_KH,B(_6q(E(_KK[1])[1])));});break;case 2:var _KL=_KK[2],_KM=E(_KI)[1];if(!B(_S(_KL,_mZ))){return new F(function(){return _IW(_KM,B(_KB(_KH,_KL)));});}else{return new F(function(){return _IW(_KM,B(_KB(_KH,B(_6a(B(_6k(B(_6s(_KF,_KK[1])))),_KL)))));});}break;default:return E(_Gx);}},_KN=[0,37],_KO=[0,16],_KP=[0,10],_KQ=[0,8],_KR=[0,43],_KS=[1,_KR,_o],_KT=[0,32],_KU=function(_KV){return new F(function(){return _Gu(new T(function(){return B(unAppCStr("bad formatting char ",[1,_KV,_o]));}));});},_KW=function(_KX,_KY){var _KZ=E(_KX);if(!_KZ){return [0];}else{var _L0=E(_KY);return _L0[0]==0?[0]:[1,_L0[1],new T(function(){return B(_KW(_KZ-1|0,_L0[2]));})];}},_L1=function(_L2,_L3){var _L4=E(_L2);if(!_L4[0]){return E(_L3)[0]==0?[0]:E(_KA);}else{var _L5=_L4[2],_L6=E(_L4[1]);if(E(_L6[1])==37){var _L7=function(_L8){var _L9=E(_L3);if(!_L9[0]){return E(_Jv);}else{var _La=B(_JC(_l,_l,_l,_L5,_L9)),_Lb=_La[2],_Lc=_La[4],_Ld=E(_La[6]);if(!_Ld[0]){return E(_KA);}else{var _Le=_Ld[2],_Lf=E(_La[7]);if(!_Lf[0]){return E(_Jv);}else{var _Lg=_Lf[1],_Lh=_Lf[2],_Li=E(_Ld[1]),_Lj=function(_Lk,_Ll){var _Lm=new T(function(){var _Ln=B(_IQ(_Ll,0)),_Lo=B(_IQ(_Lk,0)),_Lp=E(_La[1])[1];if((_Ln+_Lo|0)>=_Lp){var _Lq=[0];}else{var _Lr=_Lp-(_Ln+_Lo|0)|0;if(_Lr>0){if(_Lr<0){var _Ls=[0];}else{var _Lt=new T(function(){return [1,new T(function(){return !E(_Lc)?E(_KT):E(_IV);}),_Lt];}),_Ls=B(_KW(_Lr,_Lt));}var _Lu=_Ls,_Lv=_Lu;}else{var _Lv=[0];}var _Lw=_Lv,_Lx=_Lw,_Ly=_Lx,_Lq=_Ly;}var _Lz=_Lq,_LA=_Lz,_LB=_LA,_LC=_LB,_LD=_LC;return _LD;});return !E(_La[3])?!E(_Lc)?B(_I(_Lm,new T(function(){return B(_I(_Lk,_Ll));}))):B(_I(_Lk,new T(function(){return B(_I(_Lm,_Ll));}))):B(_I(_Lk,new T(function(){return B(_I(_Ll,_Lm));})));},_LE=function(_LF,_LG){var _LH=E(_LF);return _LH[0]==0?!E(_La[5])?B(_Lj(_o,_LG)):B(_Lj(_KS,_LG)):B(_Lj(_LH,_LG));};switch(E(_Li[1])){case 69:var _LI=B(_I3(69,_Lb,_Lg));return new F(function(){return _I(B(_LE(_LI[1],_LI[2])),new T(function(){return B(_L1(_Le,_Lh));}));});break;case 71:var _LJ=B(_I3(71,_Lb,_Lg));return new F(function(){return _I(B(_LE(_LJ[1],_LJ[2])),new T(function(){return B(_L1(_Le,_Lh));}));});break;case 88:return new F(function(){return _I(B(_Lj(_o,new T(function(){return B(_1p(_I1,B(_KG(_KO,_Lb,_Lg))));}))),new T(function(){return B(_L1(_Le,_Lh));}));});break;case 99:return new F(function(){return _I(B(_Lj(_o,[1,new T(function(){var _LK=E(_Lg);switch(_LK[0]){case 0:var _LL=E(_LK[1])[1];if(_LL>>>0>1114111){var _LM=B(_7P(_LL));}else{var _LM=[0,_LL];}var _LN=_LM,_LO=_LN,_LP=_LO,_LQ=_LP,_LR=_LQ;break;case 2:var _LS=B(_7R(_LK[2]));if(_LS>>>0>1114111){var _LT=B(_7P(_LS));}else{var _LT=[0,_LS];}var _LU=_LT,_LV=_LU,_LW=_LV,_LR=_LW;break;default:var _LR=E(_Gx);}return _LR;}),_o])),new T(function(){return B(_L1(_Le,_Lh));}));});break;case 100:var _LX=B(_J4(_Lb,_Lg));return new F(function(){return _I(B(_LE(_LX[1],_LX[2])),new T(function(){return B(_L1(_Le,_Lh));}));});break;case 101:var _LY=B(_I3(101,_Lb,_Lg));return new F(function(){return _I(B(_LE(_LY[1],_LY[2])),new T(function(){return B(_L1(_Le,_Lh));}));});break;case 102:var _LZ=B(_I3(102,_Lb,_Lg));return new F(function(){return _I(B(_LE(_LZ[1],_LZ[2])),new T(function(){return B(_L1(_Le,_Lh));}));});break;case 103:var _M0=B(_I3(103,_Lb,_Lg));return new F(function(){return _I(B(_LE(_M0[1],_M0[2])),new T(function(){return B(_L1(_Le,_Lh));}));});break;case 105:var _M1=B(_J4(_Lb,_Lg));return new F(function(){return _I(B(_LE(_M1[1],_M1[2])),new T(function(){return B(_L1(_Le,_Lh));}));});break;case 111:return new F(function(){return _I(B(_Lj(_o,new T(function(){return B(_KG(_KQ,_Lb,_Lg));}))),new T(function(){return B(_L1(_Le,_Lh));}));});break;case 115:return new F(function(){return _I(B(_Lj(_o,new T(function(){var _M2=E(_Lg);if(_M2[0]==1){var _M3=_M2[1],_M4=E(_Lb)[1];if(_M4<0){var _M5=E(_M3);}else{var _M5=_M4>0?B(_KW(_M4,_M3)):[0];}var _M6=_M5,_M7=_M6,_M8=_M7;}else{var _M8=E(_Gx);}return _M8;}))),new T(function(){return B(_L1(_Le,_Lh));}));});break;case 117:return new F(function(){return _I(B(_Lj(_o,new T(function(){return B(_KG(_KP,_Lb,_Lg));}))),new T(function(){return B(_L1(_Le,_Lh));}));});break;case 120:return new F(function(){return _I(B(_Lj(_o,new T(function(){return B(_KG(_KO,_Lb,_Lg));}))),new T(function(){return B(_L1(_Le,_Lh));}));});break;default:return new F(function(){return _KU(_Li);});}}}}},_M9=E(_L5);if(!_M9[0]){return new F(function(){return _L7(_);});}else{if(E(E(_M9[1])[1])==37){return [1,_KN,new T(function(){return B(_L1(_M9[2],_L3));})];}else{return new F(function(){return _L7(_);});}}}else{return [1,_L6,new T(function(){return B(_L1(_L5,_L3));})];}}},_Ma=new T(function(){return B(unCStr(" could be found!"));}),_Mb=function(_Mc){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_I(_Mc,_Ma));}))));});},_Md=function(_Me,_){var _Mf=E(_n0),_Mg=jsFind(toJSStr(_Mf)),_Mh=_Mg,_Mi=E(_Mh);if(!_Mi[0]){return new F(function(){return _Mb(_Mf);});}else{var _Mj=B(_mc(_)),_Mk=_Mj,_Ml=jsSet(E(_Mi[1])[1],toJSStr(E(_n1)),toJSStr(B(_1p(_mI,B(_L1(_n2,new T(function(){return B(_n4([1,[1,new T(function(){return B(_1p(_mI,_Me));})],[1,[2,_mZ,_Mk],_o]],_o));}))))))),_Mm=jsSetTimeout(5000,function(_){var _Mn=jsEval(toJSStr(B(unAppCStr("$(\'#alert-",new T(function(){return B(_I(B(_13(0,_Mk,_o)),_n3));}))))),_Mo=_Mn;return _4W;});return _4W;}},_Mp=new T(function(){return B(unCStr("\u3092\u8d85\u3048\u308b"));}),_Mq=new T(function(){return B(unCStr("\u5b9f\u7e3e\u7372\u5f97\uff1a "));}),_Mr=new T(function(){return B(unCStr("\u4f9d\u5b58\u5ea6\u304c"));}),_Ms=function(_Mt,_Mu,_Mv){return function(_Mw,_){var _Mx=E(_Mw),_My=E(_Mx[3]);if(_My[1]<=E(new T(function(){return [0,B(_nP(_Mt))];}))[1]){return [0,_4W,_Mx];}else{var _Mz=B(_Md(new T(function(){return B(_I(_Mq,_Mu));}),_)),_MA=_Mz;return new F(function(){return _MB([0,_Mx[1],_Mx[2],_My,_Mx[4],_Mx[5],_Mx[6],new T(function(){return B(_ge(E(_Mv)[1],[1,new T(function(){return B(_I(_Mr,new T(function(){return B(_I(B(_13(0,_Mt,_o)),_Mp));})));})],_Mx[7]));}),_Mx[8]],_);});}};},_MC=[0,1],_MD=function(_ME){return new F(function(){return _Ms(_MC,_mY,_ME);});},_MF=new T(function(){return [0,_mY,_MD];}),_MG=new T(function(){return B(unCStr("multiplier 10"));}),_MH=[0,10],_MI=function(_ME){return new F(function(){return _Ms(_MH,_MG,_ME);});},_MJ=new T(function(){return [0,_MG,_MI];}),_MK=new T(function(){return B(unCStr("\u4f9d\u5b58\u6ce8\u610f\u5831"));}),_ML=[0,100],_MM=function(_ME){return new F(function(){return _Ms(_ML,_MK,_ME);});},_MN=new T(function(){return [0,_MK,_MM];}),_MO=new T(function(){return B(unCStr("\u4f9d\u5b58\u30c9\u30e9\u30c3\u30b0"));}),_MP=[0,1000],_MQ=function(_ME){return new F(function(){return _Ms(_MP,_MO,_ME);});},_MR=new T(function(){return [0,_MO,_MQ];}),_MS=new T(function(){return B(unCStr("\u307e\u305a\u306f\u304a\u53cb\u9054\u304b\u3089"));}),_MT=new T(function(){return B(unCStr("\u597d\u611f\u5ea6\u304c"));}),_MU=function(_MV,_MW,_MX){return function(_MY,_){var _MZ=E(_MY),_N0=E(_MZ[2]);if(_N0[1]<=E(new T(function(){return [0,B(_nP(_MV))];}))[1]){return [0,_4W,_MZ];}else{var _N1=B(_Md(new T(function(){return B(_I(_Mq,_MW));}),_)),_N2=_N1;return new F(function(){return _MB([0,_MZ[1],_N0,_MZ[3],_MZ[4],_MZ[5],_MZ[6],new T(function(){return B(_ge(E(_MX)[1],[1,new T(function(){return B(_I(_MT,new T(function(){return B(_I(B(_13(0,_MV,_o)),_Mp));})));})],_MZ[7]));}),_MZ[8]],_);});}};},_N3=function(_ME){return new F(function(){return _MU(_MC,_MS,_ME);});},_N4=new T(function(){return [0,_MS,_N3];}),_N5=new T(function(){return B(unCStr("\u4e8c\u7d1a\u30d5\u30e9\u30b0\u5efa\u7bc9\u58eb"));}),_N6=[0,5],_N7=function(_ME){return new F(function(){return _MU(_N6,_N5,_ME);});},_N8=new T(function(){return [0,_N5,_N7];}),_N9=new T(function(){return B(unCStr("\u4e00\u7d1a\u30d5\u30e9\u30b0\u5efa\u7bc9\u58eb"));}),_Na=function(_ME){return new F(function(){return _MU(_MH,_N9,_ME);});},_Nb=new T(function(){return [0,_N9,_Na];}),_Nc=new T(function(){return B(unCStr("\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_Nd=[0,50],_Ne=function(_ME){return new F(function(){return _MU(_Nd,_Nc,_ME);});},_Nf=new T(function(){return [0,_Nc,_Ne];}),_Ng=new T(function(){return B(unCStr("\uff8a\uff72\uff8a\uff9f\uff70\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_Nh=function(_ME){return new F(function(){return _MU(_ML,_Ng,_ME);});},_Ni=new T(function(){return [0,_Ng,_Nh];}),_Nj=new T(function(){return B(unCStr("\u5168\u958b\u3089\u3076\u3071\u308f\u30fc"));}),_Nk=[0,250],_Nl=function(_ME){return new F(function(){return _MU(_Nk,_Nj,_ME);});},_Nm=new T(function(){return [0,_Nj,_Nl];}),_Nn=new T(function(){return B(unCStr("\u30a2\u30a4\u3068\u306e\u906d\u9047"));}),_No=new T(function(){return B(unCStr("\u611b\u60c5\u304c"));}),_Np=function(_Nq,_Nr,_Ns){return function(_Nt,_){var _Nu=E(_Nt),_Nv=E(_Nu[1]);if(_Nv[1]<=E(new T(function(){return [0,B(_nP(_Nq))];}))[1]){return [0,_4W,_Nu];}else{var _Nw=B(_Md(new T(function(){return B(_I(_Mq,_Nr));}),_)),_Nx=_Nw;return new F(function(){return _MB([0,_Nv,_Nu[2],_Nu[3],_Nu[4],_Nu[5],_Nu[6],new T(function(){return B(_ge(E(_Ns)[1],[1,new T(function(){return B(_I(_No,new T(function(){return B(_I(B(_13(0,_Nq,_o)),_Mp));})));})],_Nu[7]));}),_Nu[8]],_);});}};},_Ny=function(_ME){return new F(function(){return _Np(_MC,_Nn,_ME);});},_Nz=new T(function(){return [0,_Nn,_Ny];}),_NA=new T(function(){return B(unCStr("\u611b\u3055\u308c\u6c17\u5206"));}),_NB=function(_ME){return new F(function(){return _Np(_ML,_NA,_ME);});},_NC=new T(function(){return [0,_NA,_NB];}),_ND=new T(function(){return B(unCStr("\u611b\u30e9\u30d6\u30e6\u30fc"));}),_NE=[0,10000],_NF=function(_ME){return new F(function(){return _Np(_NE,_ND,_ME);});},_NG=new T(function(){return [0,_ND,_NF];}),_NH=new T(function(){return B(unCStr("\u611b\u3055\u3093\u306e\u611b\u304c\u91cd\u3044"));}),_NI=[0,10000000],_NJ=function(_ME){return new F(function(){return _Np(_NI,_NH,_ME);});},_NK=new T(function(){return [0,_NH,_NJ];}),_NL=new T(function(){var _NM=B(_mR(1,2147483647));return _NM[0]==0?[0]:[1,[0,_NM[1],_Nz],new T(function(){var _NN=E(_NM[2]);return _NN[0]==0?[0]:[1,[0,_NN[1],_NC],new T(function(){var _NO=E(_NN[2]);return _NO[0]==0?[0]:[1,[0,_NO[1],_NG],new T(function(){var _NP=E(_NO[2]);return _NP[0]==0?[0]:[1,[0,_NP[1],_NK],new T(function(){var _NQ=E(_NP[2]);return _NQ[0]==0?[0]:[1,[0,_NQ[1],_N4],new T(function(){var _NR=E(_NQ[2]);return _NR[0]==0?[0]:[1,[0,_NR[1],_N8],new T(function(){var _NS=E(_NR[2]);return _NS[0]==0?[0]:[1,[0,_NS[1],_Nb],new T(function(){var _NT=E(_NS[2]);return _NT[0]==0?[0]:[1,[0,_NT[1],_Nf],new T(function(){var _NU=E(_NT[2]);return _NU[0]==0?[0]:[1,[0,_NU[1],_Ni],new T(function(){var _NV=E(_NU[2]);return _NV[0]==0?[0]:[1,[0,_NV[1],_Nm],new T(function(){var _NW=E(_NV[2]);return _NW[0]==0?[0]:[1,[0,_NW[1],_MF],new T(function(){var _NX=E(_NW[2]);return _NX[0]==0?[0]:[1,[0,_NX[1],_MJ],new T(function(){var _NY=E(_NX[2]);return _NY[0]==0?[0]:[1,[0,_NY[1],_MN],new T(function(){var _NZ=E(_NY[2]);return _NZ[0]==0?[0]:[1,[0,_NZ[1],_MR],_o];})];})];})];})];})];})];})];})];})];})];})];})];})];}),_O0=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_O1=new T(function(){return B(unCStr("base"));}),_O2=new T(function(){return B(unCStr("IOException"));}),_O3=new T(function(){var _O4=hs_wordToWord64(4053623282),_O5=_O4,_O6=hs_wordToWord64(3693590983),_O7=_O6;return [0,_O5,_O7,[0,_O5,_O7,_O1,_O0,_O2],_o];}),_O8=function(_O9){return E(_O3);},_Oa=function(_Ob){var _Oc=E(_Ob);return new F(function(){return _2A(B(_2y(_Oc[1])),_O8,_Oc[2]);});},_Od=new T(function(){return B(unCStr(": "));}),_Oe=[0,41],_Of=new T(function(){return B(unCStr(" ("));}),_Og=new T(function(){return B(unCStr("already exists"));}),_Oh=new T(function(){return B(unCStr("does not exist"));}),_Oi=new T(function(){return B(unCStr("protocol error"));}),_Oj=new T(function(){return B(unCStr("failed"));}),_Ok=new T(function(){return B(unCStr("invalid argument"));}),_Ol=new T(function(){return B(unCStr("inappropriate type"));}),_Om=new T(function(){return B(unCStr("hardware fault"));}),_On=new T(function(){return B(unCStr("unsupported operation"));}),_Oo=new T(function(){return B(unCStr("timeout"));}),_Op=new T(function(){return B(unCStr("resource vanished"));}),_Oq=new T(function(){return B(unCStr("interrupted"));}),_Or=new T(function(){return B(unCStr("resource busy"));}),_Os=new T(function(){return B(unCStr("resource exhausted"));}),_Ot=new T(function(){return B(unCStr("end of file"));}),_Ou=new T(function(){return B(unCStr("illegal operation"));}),_Ov=new T(function(){return B(unCStr("permission denied"));}),_Ow=new T(function(){return B(unCStr("user error"));}),_Ox=new T(function(){return B(unCStr("unsatisified constraints"));}),_Oy=new T(function(){return B(unCStr("system error"));}),_Oz=function(_OA,_OB){switch(E(_OA)){case 0:return new F(function(){return _I(_Og,_OB);});break;case 1:return new F(function(){return _I(_Oh,_OB);});break;case 2:return new F(function(){return _I(_Or,_OB);});break;case 3:return new F(function(){return _I(_Os,_OB);});break;case 4:return new F(function(){return _I(_Ot,_OB);});break;case 5:return new F(function(){return _I(_Ou,_OB);});break;case 6:return new F(function(){return _I(_Ov,_OB);});break;case 7:return new F(function(){return _I(_Ow,_OB);});break;case 8:return new F(function(){return _I(_Ox,_OB);});break;case 9:return new F(function(){return _I(_Oy,_OB);});break;case 10:return new F(function(){return _I(_Oi,_OB);});break;case 11:return new F(function(){return _I(_Oj,_OB);});break;case 12:return new F(function(){return _I(_Ok,_OB);});break;case 13:return new F(function(){return _I(_Ol,_OB);});break;case 14:return new F(function(){return _I(_Om,_OB);});break;case 15:return new F(function(){return _I(_On,_OB);});break;case 16:return new F(function(){return _I(_Oo,_OB);});break;case 17:return new F(function(){return _I(_Op,_OB);});break;default:return new F(function(){return _I(_Oq,_OB);});}},_OC=[0,125],_OD=new T(function(){return B(unCStr("{handle: "));}),_OE=function(_OF,_OG,_OH,_OI,_OJ,_OK){var _OL=new T(function(){var _OM=new T(function(){return B(_Oz(_OG,new T(function(){var _ON=E(_OI);return _ON[0]==0?E(_OK):B(_I(_Of,new T(function(){return B(_I(_ON,[1,_Oe,_OK]));})));})));}),_OO=E(_OH);return _OO[0]==0?E(_OM):B(_I(_OO,new T(function(){return B(_I(_Od,_OM));})));}),_OP=E(_OJ);if(!_OP[0]){var _OQ=E(_OF);if(!_OQ[0]){return E(_OL);}else{var _OR=E(_OQ[1]);return _OR[0]==0?B(_I(_OD,new T(function(){return B(_I(_OR[1],[1,_OC,new T(function(){return B(_I(_Od,_OL));})]));}))):B(_I(_OD,new T(function(){return B(_I(_OR[1],[1,_OC,new T(function(){return B(_I(_Od,_OL));})]));})));}}else{return new F(function(){return _I(_OP[1],new T(function(){return B(_I(_Od,_OL));}));});}},_OS=function(_OT){var _OU=E(_OT);return new F(function(){return _OE(_OU[1],_OU[2],_OU[3],_OU[4],_OU[6],_o);});},_OV=function(_OW,_OX){var _OY=E(_OW);return new F(function(){return _OE(_OY[1],_OY[2],_OY[3],_OY[4],_OY[6],_OX);});},_OZ=function(_P0,_P1){return new F(function(){return _2V(_OV,_P0,_P1);});},_P2=function(_P3,_P4,_P5){var _P6=E(_P4);return new F(function(){return _OE(_P6[1],_P6[2],_P6[3],_P6[4],_P6[6],_P5);});},_P7=[0,_P2,_OS,_OZ],_P8=new T(function(){return [0,_O8,_P7,_P9,_Oa];}),_P9=function(_Pa){return [0,_P8,_Pa];},_Pb=7,_Pc=function(_Pd){return [0,_6Q,_Pb,_o,_Pd,_6Q,_6Q];},_Pe=function(_Pf,_){return new F(function(){return die(new T(function(){return B(_P9(new T(function(){return B(_Pc(_Pf));})));}));});},_Pg=function(_Ph,_){return new F(function(){return _Pe(_Ph,_);});},_Pi=new T(function(){return B(unCStr("<tr><td>%s</td><td>%s</td></tr>"));}),_Pj=new T(function(){return B(unCStr("<thead><tr><th>\u5b9f\u7e3e\u540d</th><th>\u5185\u5bb9</th></tr></thead>"));}),_Pk=new T(function(){return B(unCStr("</tbody>"));}),_MB=function(_Pl,_){var _Pm=jsFind(toJSStr(E(_2))),_Pn=_Pm,_Po=E(_Pn);if(!_Po[0]){return new F(function(){return _Pg(_mQ,_);});}else{var _Pp=jsSet(E(_Po[1])[1],toJSStr(E(_n1)),toJSStr(B(_I(_Pj,new T(function(){return B(unAppCStr("<tbody>",new T(function(){var _Pq=function(_Pr){var _Ps=E(_Pr);if(!_Ps[0]){return [0];}else{var _Pt=E(_Ps[1]),_Pu=function(_Pv){var _Pw=E(_Pv);return _Pw[0]==0?E(new T(function(){return B(_Pq(_Ps[2]));})):[1,_Pw[1],new T(function(){return B(_Pu(_Pw[2]));})];};return new F(function(){return _Pu(B(_L1(_Pi,new T(function(){return B(_n4([1,[1,new T(function(){return B(_1p(_mI,_Pt[2]));})],[1,[1,new T(function(){return B(_1p(_mI,_Pt[1]));})],_o]],_o));}))));});}};return B(_I(B(_Pq(B(_1p(function(_Px){var _Py=E(_Px),_Pz=E(_Py[2])[1],_PA=B(_mK(E(_Py[1])[1],new T(function(){return E(E(_Pl)[7]);})));if(!_PA[0]){return [0,_Pz,_o];}else{var _PB=E(_PA[1]);return _PB[0]==0?[0,_Pz,_o]:[0,_Pz,_PB[1]];}},_NL)))),_Pk));})));})))));return [0,_4W,_Pl];}},_PC=new T(function(){return [0,"click"];}),_PD=new T(function(){return B(unCStr("-btn"));}),_PE=[0,1],_PF=new T(function(){return B(_3D("main.hs:(336,1)-(352,24)|function btnEvents"));}),_PG=function(_PH,_PI,_PJ){return new F(function(){return _v9(_PI,_PJ);});},_PK=function(_PL){return E(_ML);},_PM=[0,0],_PN=function(_PO,_PP,_PQ,_PR,_PS,_){var _PT=jsFind(toJSStr(E(_2))),_PU=_PT,_PV=E(_PU);if(!_PV[0]){return new F(function(){return _Pg(_mQ,_);});}else{var _PW=jsSet(E(_PV[1])[1],toJSStr(E(_n1)),toJSStr(B(_I(_Pj,new T(function(){return B(unAppCStr("<tbody>",new T(function(){var _PX=function(_PY){var _PZ=E(_PY);if(!_PZ[0]){return [0];}else{var _Q0=E(_PZ[1]),_Q1=function(_Q2){var _Q3=E(_Q2);return _Q3[0]==0?E(new T(function(){return B(_PX(_PZ[2]));})):[1,_Q3[1],new T(function(){return B(_Q1(_Q3[2]));})];};return new F(function(){return _Q1(B(_L1(_Pi,new T(function(){return B(_n4([1,[1,new T(function(){return B(_1p(_mI,_Q0[2]));})],[1,[1,new T(function(){return B(_1p(_mI,_Q0[1]));})],_o]],_o));}))));});}};return B(_I(B(_PX(B(_1p(function(_Q4){var _Q5=E(_Q4),_Q6=E(_Q5[2])[1],_Q7=B(_mK(E(_Q5[1])[1],_PR));if(!_Q7[0]){return [0,_Q6,_o];}else{var _Q8=E(_Q7[1]);return _Q8[0]==0?[0,_Q6,_o]:[0,_Q6,_Q8[1]];}},_NL)))),_Pk));})));})))));return [0,_4W,[0,_PM,_PM,_PM,_PO,_PP,_PQ,_PR,_PS]];}},_Q9=function(_Qa,_Qb,_){var _Qc=B(_mc(_)),_Qd=_Qc;return new F(function(){return _PN(_Qd,_2j,_l,_gd,_gd,_);});},_Qe=new T(function(){return B(unCStr("fa-trash"));}),_Qf=new T(function(){return B(unCStr("\u30c7\u30fc\u30bf\u306e\u6d88\u53bb<br>\u5168\u3066\u306e\u30c7\u30fc\u30bf\u304c\u6d88\u53bb\u3055\u308c\u307e\u3059\u3002\u3053\u306e\u64cd\u4f5c\u306f\u53d6\u308a\u6d88\u305b\u307e\u305b\u3093\u3002"));}),_Qg=new T(function(){return B(unCStr("\u30c7\u30fc\u30bf\u306e\u6d88\u53bb"));}),_Qh=[0,_Qe,_Qf,_Qg],_Qi=[0,_PK,_Q9,_Qh],_Qj=function(_Qk,_Ql,_Qm,_Qn){return new F(function(){return A(_Qk,[function(_){var _Qo=jsSetStyle(E(_Ql)[1],toJSStr(E(_Qm)),toJSStr(E(_Qn)));return _4W;}]);});},_Qp=new T(function(){return B(unCStr("monitor"));}),_Qq=new T(function(){return B(unCStr("display"));}),_Qr=new T(function(){return B(unCStr("block"));}),_Qs=function(_Qt,_Qu,_){var _Qv=B(A(_Qt,[_])),_Qw=_Qv;return new F(function(){return A(_Qu,[_Qw,_]);});},_Qx=function(_Qy,_){return _Qy;},_Qz=function(_QA,_QB,_){var _QC=B(A(_QA,[_])),_QD=_QC;return new F(function(){return A(_QB,[_]);});},_QE=[0,_Qs,_Qz,_Qx,_Pg],_QF=[0,_QE,_5A],_QG=function(_QH){return E(E(_QH)[1]);},_QI=function(_QJ){return E(E(_QJ)[1]);},_QK=function(_QL){return E(E(_QL)[2]);},_QM=function(_QN){return E(E(_QN)[3]);},_QO=function(_QP,_QQ){var _QR=new T(function(){return B(_QG(_QP));});return function(_QS){return new F(function(){return A(new T(function(){return B(_QI(_QR));}),[new T(function(){return B(A(_QK,[_QP,_QQ]));}),function(_QT){return new F(function(){return A(new T(function(){return B(_QM(_QR));}),[[0,_QT,_QS]]);});}]);});};},_QU=function(_QV){return new F(function(){return _QO(_QF,_QV);});},_QW=function(_QX,_QY,_){var _QZ=E(_Qp),_R0=jsFind(toJSStr(_QZ)),_R1=_R0,_R2=E(_R1);return _R2[0]==0?B(_Mb(_QZ)):B(A(_Qj,[_QU,_R2[1],_Qq,_Qr,_QY,_]));},_R3=function(_R4){return E(_2j);},_R5=new T(function(){return B(unCStr("fa-power-off"));}),_R6=new T(function(){return B(unCStr("\u3055\u3041\u59cb\u3081\u3088\u3046<br>\u30b2\u30fc\u30e0\u3092\u59cb\u3081\u307e\u3057\u3087\u3046\u3002\u53f3\u306e\u30dc\u30bf\u30f3\u304b\u3089\u3053\u306e\u30a2\u30a4\u30c6\u30e0\u3092\u8cfc\u5165\u3057\u3066\u304f\u3060\u3055\u3044\u3002"));}),_R7=new T(function(){return B(unCStr("\u3055\u3041\u59cb\u3081\u3088\u3046"));}),_R8=[0,_R5,_R6,_R7],_R9=[0,_R3,_QW,_R8],_Ra=new T(function(){return B(unCStr("item-shop"));}),_Rb=function(_Rc,_Rd,_){var _Re=E(_Ra),_Rf=jsFind(toJSStr(_Re)),_Rg=_Rf,_Rh=E(_Rg);return _Rh[0]==0?B(_Mb(_Re)):B(A(_Qj,[_QU,_Rh[1],_Qq,_Qr,_Rd,_]));},_Ri=function(_Rj){return E(_MC);},_Rk=new T(function(){return B(unCStr("fa-shopping-cart"));}),_Rl=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u30b7\u30e7\u30c3\u30d7<br>\u30a2\u30a4\u30c6\u30e0\u304c\u8cfc\u5165\u3067\u304d\u308b\u3088\u3046\u306b\u306a\u308a\u307e\u3059\u3002"));}),_Rm=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u30b7\u30e7\u30c3\u30d7"));}),_Rn=[0,_Rk,_Rl,_Rm],_Ro=[0,_Ri,_Rb,_Rn],_Rp=function(_Rq){return E(_MH);},_Rr=function(_Rs,_Rt,_){var _Ru=B(_mc(_)),_Rv=_Ru;return new F(function(){return _PN(_Rv,_2j,_l,new T(function(){return E(E(_Rt)[7]);}),_gd,_);});},_Rw=new T(function(){return B(unCStr("fa-history"));}),_Rx=new T(function(){return B(unCStr("\u521d\u671f\u5316<br>\u5b9f\u7e3e\u3092\u9664\u304f\u5168\u3066\u306e\u30c7\u30fc\u30bf\u304c\u521d\u671f\u5316\u3055\u308c\u307e\u3059"));}),_Ry=new T(function(){return B(unCStr("\u521d\u671f\u5316"));}),_Rz=[0,_Rw,_Rx,_Ry],_RA=[0,_Rp,_Rr,_Rz],_RB=new T(function(){var _RC=B(_sT(-1,-2));return _RC[0]==0?[0]:[1,[0,_RC[1],_R9],new T(function(){var _RD=E(_RC[2]);return _RD[0]==0?[0]:[1,[0,_RD[1],_Ro],new T(function(){var _RE=E(_RD[2]);return _RE[0]==0?[0]:[1,[0,_RE[1],_RA],new T(function(){var _RF=E(_RE[2]);return _RF[0]==0?[0]:[1,[0,_RF[1],_Qi],_o];})];})];})];}),_RG=function(_RH,_RI){var _RJ=E(_RI);return _RJ[0]==0?E(_RB):[1,[0,[0,_RH],_RJ[1]],new T(function(){var _RK=E(_RH);if(_RK==2147483647){var _RL=E(_RB);}else{var _RL=B(_RG(_RK+1|0,_RJ[2]));}return _RL;})];},_RM=function(_RN,_RO,_RP){return [1,[0,[0,_RN],_RO],new T(function(){var _RQ=E(_RN);if(_RQ==2147483647){var _RR=E(_RB);}else{var _RR=B(_RG(_RQ+1|0,_RP));}return _RR;})];},_RS=[0,3],_RT=[0,2],_RU=new T(function(){return B(_nD(_RS,_RT));}),_RV=[0,-1],_RW=function(_RX,_RY){if(_RY>=0){var _RZ=function(_S0){var _S1=B(_q3(_RX*_S0)),_S2=_S1[1],_S3=_S1[2];if(_S3>=0){return new F(function(){return _sk(_S2,_S3);});}else{var _S4= -_S3;if(_S4<=52){var _S5=hs_uncheckedIShiftRA64(B(_sc(_S2)),_S4),_S6=_S5;return new F(function(){return _rv(_S6);});}else{return !B(_S(_S2,_2j))?E(_2j):E(_RV);}}},_S7=E(_RY);if(!_S7){return new F(function(){return _RZ(1);});}else{return new F(function(){return _RZ(B(_GN(E(_RU)[1],_S7)));});}}else{return E(_GE);}},_S8=function(_S9){return new F(function(){return _RW(1,E(_S9)[1]);});},_Sa=function(_Sb,_Sc,_){return [0,_4W,new T(function(){var _Sd=E(_Sc);return [0,_Sd[1],new T(function(){return [0,E(_Sd[2])[1]+0.1];}),_Sd[3],_Sd[4],_Sd[5],_Sd[6],_Sd[7],_Sd[8]];})];},_Se=new T(function(){return B(unCStr("fa-comments-o"));}),_Sf=new T(function(){return B(unCStr("\u4f1a\u8a71<br>\u597d\u611f\u5ea6 +0.1"));}),_Sg=[0,_Se,_Sf,_o],_Sh=[0,_S8,_Sa,_Sg],_Si=function(_Sj){return new F(function(){return _RW(100,E(_Sj)[1]);});},_Sk=function(_Sl,_Sm,_){return [0,_4W,new T(function(){var _Sn=E(_Sm);return [0,_Sn[1],new T(function(){return [0,E(_Sn[2])[1]+1];}),_Sn[3],_Sn[4],_Sn[5],_Sn[6],_Sn[7],_Sn[8]];})];},_So=new T(function(){return B(unCStr("fa-envelope"));}),_Sp=new T(function(){return B(unCStr("\u30e1\u30fc\u30eb<br>\u597d\u611f\u5ea6 +1.0"));}),_Sq=[0,_So,_Sp,_o],_Sr=[0,_Si,_Sk,_Sq],_Ss=function(_St){return new F(function(){return _RW(1000,E(_St)[1]);});},_Su=function(_Sv,_Sw,_){return [0,_4W,new T(function(){var _Sx=E(_Sw);return [0,_Sx[1],new T(function(){return [0,E(_Sx[2])[1]+5];}),_Sx[3],_Sx[4],_Sx[5],_Sx[6],_Sx[7],_Sx[8]];})];},_Sy=new T(function(){return B(unCStr("fa-coffee"));}),_Sz=new T(function(){return B(unCStr("\u55ab\u8336\u5e97<br>\u597d\u611f\u5ea6 +5.0"));}),_SA=[0,_Sy,_Sz,_o],_SB=[0,_Ss,_Su,_SA],_SC=function(_SD){return new F(function(){return _RW(20000,E(_SD)[1]);});},_SE=function(_SF,_SG,_){return [0,_4W,new T(function(){var _SH=E(_SG);return [0,_SH[1],new T(function(){return [0,E(_SH[2])[1]+15];}),_SH[3],_SH[4],_SH[5],_SH[6],_SH[7],_SH[8]];})];},_SI=new T(function(){return B(unCStr("fa-plane"));}),_SJ=new T(function(){return B(unCStr("\u65c5\u884c<br>\u597d\u611f\u5ea6 +15.0"));}),_SK=[0,_SI,_SJ,_o],_SL=[0,_SC,_SE,_SK],_SM=function(_SN){return new F(function(){return _RW(200000,E(_SN)[1]);});},_SO=function(_SP,_SQ,_){return [0,_4W,new T(function(){var _SR=E(_SQ);return [0,_SR[1],new T(function(){return [0,E(_SR[2])[1]+50];}),_SR[3],_SR[4],_SR[5],_SR[6],_SR[7],_SR[8]];})];},_SS=new T(function(){return B(unCStr("fa-car"));}),_ST=new T(function(){return B(unCStr("\u8eca<br>\u597d\u611f\u5ea6 +50.0"));}),_SU=[0,_SS,_ST,_o],_SV=[0,_SM,_SO,_SU],_SW=function(_SX){return new F(function(){return _RW(5000000,E(_SX)[1]);});},_SY=function(_SZ,_T0,_){return [0,_4W,new T(function(){var _T1=E(_T0);return [0,_T1[1],new T(function(){return [0,E(_T1[2])[1]+100];}),_T1[3],_T1[4],_T1[5],_T1[6],_T1[7],_T1[8]];})];},_T2=new T(function(){return B(unCStr("fa-home"));}),_T3=new T(function(){return B(unCStr("\u5bb6<br>\u597d\u611f\u5ea6 +100.0"));}),_T4=[0,_T2,_T3,_o],_T5=[0,_SW,_SY,_T4],_T6=[1,_T5,_o],_T7=[1,_SV,_T6],_T8=[1,_SL,_T7],_T9=[1,_SB,_T8],_Ta=[1,_Sr,_T9],_Tb=new T(function(){return B(_RM(1,_Sh,_Ta));}),_Tc=new T(function(){return B(_gF(_gd,_Tb));}),_Td=function(_Te,_Tf,_){var _Tg=E(_Tf);if(!_Tg[0]){return E(_PF);}else{var _Th=E(_Tg[1]),_Ti=_Th[1],_Tj=function(_,_Tk){var _Tl=E(_Tk);if(!_Tl[0]){return _4W;}else{var _Tm=E(_PC)[1],_Tn=jsSetCB(E(_Tl[1])[1],_Tm,function(_To,_Tp,_){var _Tq=E(_Te)[1],_Tr=rMV(_Tq),_Ts=_Tr,_Tt=E(new T(function(){return B(_lZ(_Tc,_Ti));})),_Tu=B(A(_Tt[2],[_Th,new T(function(){var _Tv=E(_Ts),_Tw=new T(function(){return B(_mg(_PG,_Ti,_PE,_Tv[8]));});return [0,new T(function(){return [0,E(_Tv[1])[1]-B(_nP(B(A(_Tt[1],[new T(function(){return [0,B(_lZ(_Tw,_Ti))[1]-1|0];})]))))];}),_Tv[2],_Tv[3],_Tv[4],_Tv[5],_Tv[6],_Tv[7],_Tw];}),_])),_Tx=_Tu,_Ty=B(_MB(new T(function(){return E(E(_Tx)[2]);}),_)),_Tz=_Ty,_=wMV(_Tq,new T(function(){return E(E(_Tz)[2]);})),_TA=rMV(_Tq),_TB=_TA,_TC=E(_TB),_TD=jsLog(toJSStr(B(A(_lt,[0,_TC[1],_TC[2],_TC[3],_TC[4],_TC[5],_TC[6],_TC[7],_TC[8],_o]))));return _4W;}),_TE=_Tn,_TF=function(_TG,_TH,_){var _TI=E(_TH);if(!_TI[0]){return E(_PF);}else{var _TJ=E(_TI[1]),_TK=_TJ[1],_TL=function(_,_TM){var _TN=E(_TM);if(!_TN[0]){return _4W;}else{var _TO=jsSetCB(E(_TN[1])[1],_Tm,function(_TP,_TQ,_){var _TR=E(_TG)[1],_TS=rMV(_TR),_TT=_TS,_TU=E(new T(function(){return B(_lZ(_Tc,_TK));})),_TV=B(A(_TU[2],[_TJ,new T(function(){var _TW=E(_TT),_TX=new T(function(){return B(_mg(_PG,_TK,_PE,_TW[8]));});return [0,new T(function(){return [0,E(_TW[1])[1]-B(_nP(B(A(_TU[1],[new T(function(){return [0,B(_lZ(_TX,_TK))[1]-1|0];})]))))];}),_TW[2],_TW[3],_TW[4],_TW[5],_TW[6],_TW[7],_TX];}),_])),_TY=_TV,_TZ=B(_MB(new T(function(){return E(E(_TY)[2]);}),_)),_U0=_TZ,_=wMV(_TR,new T(function(){return E(E(_U0)[2]);})),_U1=rMV(_TR),_U2=_U1,_U3=E(_U2),_U4=jsLog(toJSStr(B(A(_lt,[0,_U3[1],_U3[2],_U3[3],_U3[4],_U3[5],_U3[6],_U3[7],_U3[8],_o]))));return _4W;}),_U5=_TO;return new F(function(){return _TF(_TG,_TI[2],_);});}};if(_TK<=0){var _U6=jsFind(toJSStr(B(unAppCStr("item-sp-",new T(function(){if(_TK<0){var _U7=B(_I(B(_7J(0, -_TK,_o)),_PD));}else{var _U7=B(_I(B(_7J(0,_TK,_o)),_PD));}var _U8=_U7;return _U8;}))))),_U9=_U6;return new F(function(){return _TL(_,_U9);});}else{var _Ua=jsFind(toJSStr(B(unAppCStr("item-",new T(function(){return B(_I(B(_7J(0,_TK,_o)),_PD));}))))),_Ub=_Ua;return new F(function(){return _TL(_,_Ub);});}}};return new F(function(){return _TF(_Te,_Tg[2],_);});}};if(_Ti<=0){var _Uc=jsFind(toJSStr(B(unAppCStr("item-sp-",new T(function(){if(_Ti<0){var _Ud=B(_I(B(_7J(0, -_Ti,_o)),_PD));}else{var _Ud=B(_I(B(_7J(0,_Ti,_o)),_PD));}var _Ue=_Ud;return _Ue;}))))),_Uf=_Uc;return new F(function(){return _Tj(_,_Uf);});}else{var _Ug=jsFind(toJSStr(B(unAppCStr("item-",new T(function(){return B(_I(B(_7J(0,_Ti,_o)),_PD));}))))),_Uh=_Ug;return new F(function(){return _Tj(_,_Uh);});}}},_Ui=function(_Uj){var _Uk=E(_Uj);if(!_Uk[0]){return [0,_o,_o];}else{var _Ul=E(_Uk[1]),_Um=new T(function(){var _Un=B(_Ui(_Uk[2]));return [0,_Un[1],_Un[2]];});return E(_Ul[1])[1]<=0?[0,new T(function(){return E(E(_Um)[1]);}),[1,_Ul,new T(function(){return E(E(_Um)[2]);})]]:[0,[1,_Ul,new T(function(){return E(E(_Um)[1]);})],new T(function(){return E(E(_Um)[2]);})];}},_Uo=new T(function(){var _Up=B(_Ui(_Tb));return [0,_Up[1],_Up[2]];}),_Uq=new T(function(){return E(E(_Uo)[1]);}),_Ur=new T(function(){return E(E(_Uo)[2]);}),_Us=function(_Ut){return _Ut>0;},_Uu=function(_Uv){var _Uw=B(A(_Uv,[_])),_Ux=_Uw;return E(_Ux);},_Uy=function(_Uz){return new F(function(){return _Uu(function(_){var _=0;return new F(function(){return eval(_Uz);});});});},_UA=new T(function(){return B(_Uy("(function(x) {return x === null;})"));}),_UB=new T(function(){return B(unCStr("No such value"));}),_UC=[0,_UB],_UD=new T(function(){return B(unCStr("Invalid JSON!"));}),_UE=[0,_UD],_UF=new T(function(){return [0,"(function(k) {return localStorage.getItem(k);})"];}),_UG=function(_UH,_UI,_){var _UJ=B(A(_Uy,[E(_UF)[1],E(toJSStr(E(_UI))),_])),_UK=_UJ;return new T(function(){if(!B(_Uu(function(_){var _=0,_UL=B(A(_UA,[E(_UK),_])),_UM=_UL;return new T(function(){return B(_Us(_UM));});}))){var _UN=String(_UK),_UO=_UN,_UP=jsParseJSON(_UO),_UQ=_UP,_UR=E(_UQ),_US=_UR[0]==0?E(_UE):B(A(_1L,[_UH,_UR[1]]));}else{var _US=E(_UC);}return _US;});},_UT=[0,10],_UU=[1,_UT,_o],_UV=function(_UW,_UX,_){var _UY=jsWriteHandle(E(_UW)[1],toJSStr(E(_UX)));return _4W;},_UZ=function(_V0,_V1,_){var _V2=E(_V0),_V3=jsWriteHandle(_V2[1],toJSStr(E(_V1)));return new F(function(){return _UV(_V2,_UU,_);});},_V4=function(_V5,_V6,_){var _V7=jsCreateTextNode(toJSStr(E(_V5))),_V8=_V7,_V9=jsAppendChild(_V8,E(_V6)[1]);return [0,_V8];},_Va=function(_Vb,_Vc,_Vd,_Ve){return new F(function(){return A(_Vb,[function(_){var _Vf=jsSetAttr(E(_Vc)[1],toJSStr(E(_Vd)),toJSStr(E(_Ve)));return _4W;}]);});},_Vg=function(_Vh,_Vi,_Vj,_Vk){return new F(function(){return A(_Vh,[function(_){var _Vl=jsSet(E(_Vi)[1],toJSStr(E(_Vj)),toJSStr(E(_Vk)));return _4W;}]);});},_Vm=new T(function(){return B(unCStr("count"));}),_Vn=[0,32],_Vo=[1,_Vn,_o],_Vp=new T(function(){return B(unCStr("item-list"));}),_Vq=new T(function(){return B(unCStr("type"));}),_Vr=new T(function(){return B(unCStr("button"));}),_Vs=new T(function(){return B(unCStr("btn btn-default btn-buy"));}),_Vt=new T(function(){return B(unCStr("fa fa-plus-circle"));}),_Vu=new T(function(){return B(unCStr(" loves"));}),_Vv=new T(function(){return B(unCStr("class"));}),_Vw=new T(function(){return B(unCStr("list-group-item tooltips"));}),_Vx=new T(function(){return B(unCStr("tip"));}),_Vy=new T(function(){return B(unCStr("button"));}),_Vz=function(_VA,_VB,_VC,_){var _VD=jsCreateElem(toJSStr(E(_Vy))),_VE=_VD,_VF=jsAppendChild(_VE,E(_VC)[1]),_VG=[0,_VE],_VH=B(A(_VA,[_VB,_VG,_])),_VI=_VH;return _VG;},_VJ=[0,105],_VK=[1,_VJ,_o],_VL=function(_VM,_VN,_VO,_){var _VP=jsCreateElem(toJSStr(_VK)),_VQ=_VP,_VR=jsAppendChild(_VQ,E(_VO)[1]),_VS=[0,_VQ],_VT=B(A(_VM,[_VN,_VS,_])),_VU=_VT;return _VS;},_VV=new T(function(){return B(unCStr("li"));}),_VW=function(_VX,_VY,_VZ,_){var _W0=jsCreateElem(toJSStr(E(_VV))),_W1=_W0,_W2=jsAppendChild(_W1,E(_VZ)[1]),_W3=[0,_W1],_W4=B(A(_VX,[_VY,_W3,_])),_W5=_W4;return _W3;},_W6=new T(function(){return B(unCStr("id"));}),_W7=[0,48],_W8=[1,_W7,_o],_W9=new T(function(){return B(unCStr("-icon"));}),_Wa=new T(function(){return B(unCStr("-num"));}),_Wb=new T(function(){return B(unCStr("-box"));}),_Wc=new T(function(){return B(unCStr("-cost"));}),_Wd=new T(function(){return B(unCStr("innerHTML"));}),_We=new T(function(){return B(unCStr("span"));}),_Wf=function(_Wg,_Wh,_Wi,_){var _Wj=jsCreateElem(toJSStr(E(_We))),_Wk=_Wj,_Wl=jsAppendChild(_Wk,E(_Wi)[1]),_Wm=[0,_Wk],_Wn=B(A(_Wg,[_Wh,_Wm,_])),_Wo=_Wn;return _Wm;},_Wp=function(_Wq){return E(_Wq);},_Wr=function(_Ws,_Wt,_Wu,_Wv){var _Ww=new T(function(){return B(unAppCStr("item-",new T(function(){var _Wx=E(_Ws)[1];return _Wx<=0?B(unAppCStr("sp-",new T(function(){if(_Wx<0){var _Wy=B(_7J(0, -_Wx,_o));}else{var _Wy=B(_7J(0,_Wx,_o));}var _Wz=_Wy;return _Wz;}))):B(_7J(0,_Wx,_o));})));});return function(_WA,_){var _WB=B(_VW(_Wp,function(_WC,_){var _WD=B(_Wf(_Wp,function(_WE,_){var _WF=B(A(_Vg,[_5A,_WE,_Wd,_Wu,_])),_WG=_WF;return _WE;},_WC,_)),_WH=_WD,_WI=B(A(_Va,[_5A,_WH,_Vv,_Vx,_])),_WJ=_WI,_WK=B(_Wf(_Wp,function(_WL,_){var _WM=B(_Wf(_Wp,function(_WN,_){var _WO=B(_VL(_V4,_o,_WN,_)),_WP=_WO,_WQ=B(A(_Va,[_5A,_WP,_Vv,new T(function(){return B(unAppCStr("fa ",_Wt));}),_])),_WR=_WQ,_WS=B(_V4(_Vo,_WN,_)),_WT=_WS;return _WN;},_WL,_)),_WU=_WM,_WV=B(A(_Va,[_5A,_WU,_W6,new T(function(){return B(_I(_Ww,_W9));}),_])),_WW=_WV,_WX=B(_Wf(_V4,_o,_WL,_)),_WY=_WX,_WZ=B(A(_Va,[_5A,_WY,_W6,new T(function(){return B(_I(_Ww,_Wa));}),_])),_X0=_WZ;return _WL;},_WC,_)),_X1=_WK,_X2=B(A(_Va,[_5A,_X1,_Vv,_Vm,_])),_X3=_X2,_X4=B(_Wf(_V4,_Wv,_WC,_)),_X5=_X4,_X6=B(A(_Va,[_5A,_X5,_W6,new T(function(){return B(_I(_Ww,_Wb));}),_])),_X7=_X6,_X8=B(A(_Va,[_5A,_X5,_Vv,_Vp,_])),_X9=_X8,_Xa=B(_Vz(_Wp,function(_Xb,_){var _Xc=B(_VL(_V4,_o,_Xb,_)),_Xd=_Xc,_Xe=B(A(_Va,[_5A,_Xd,_Vv,_Vt,_])),_Xf=_Xe,_Xg=B(_V4(_Vo,_Xb,_)),_Xh=_Xg,_Xi=B(_Wf(_V4,_W8,_Xb,_)),_Xj=_Xi,_Xk=B(A(_Va,[_5A,_Xj,_W6,new T(function(){return B(_I(_Ww,_Wc));}),_])),_Xl=_Xk,_Xm=B(_V4(_Vu,_Xb,_)),_Xn=_Xm;return _Xb;},_WC,_)),_Xo=_Xa,_Xp=B(A(_Va,[_5A,_Xo,_Vq,_Vr,_])),_Xq=_Xp,_Xr=B(A(_Va,[_5A,_Xo,_W6,new T(function(){return B(_I(_Ww,_PD));}),_])),_Xs=_Xr,_Xt=B(A(_Va,[_5A,_Xo,_Vv,_Vs,_])),_Xu=_Xt;return _WC;},_WA,_)),_Xv=_WB,_Xw=B(A(_Va,[_5A,_Xv,_Vv,_Vw,_])),_Xx=_Xw;return _Xv;};},_Xy=new T(function(){return B(unCStr("Aichan"));}),_Xz=new T(function(){return [0,toJSStr(_o)];}),_XA=[0,93],_XB=[1,_XA,_o],_XC=new T(function(){return [0,toJSStr(_XB)];}),_XD=[0,125],_XE=[1,_XD,_o],_XF=new T(function(){return [0,toJSStr(_XE)];}),_XG=[0,58],_XH=[1,_XG,_o],_XI=new T(function(){return [0,toJSStr(_XH)];}),_XJ=[0,44],_XK=[1,_XJ,_o],_XL=new T(function(){return [0,toJSStr(_XK)];}),_XM=new T(function(){return [0,"false"];}),_XN=function(_XO){var _XP=jsShow(E(_XO)[1]),_XQ=_XP;return [0,_XQ];},_XR=function(_XS){var _XT=jsStringify(E(_XS)[1]),_XU=_XT;return [0,_XU];},_XV=new T(function(){return [0,"null"];}),_XW=[0,91],_XX=[1,_XW,_o],_XY=new T(function(){return [0,toJSStr(_XX)];}),_XZ=[0,123],_Y0=[1,_XZ,_o],_Y1=new T(function(){return [0,toJSStr(_Y0)];}),_Y2=[0,34],_Y3=[1,_Y2,_o],_Y4=new T(function(){return [0,toJSStr(_Y3)];}),_Y5=new T(function(){return [0,"true"];}),_Y6=function(_Y7,_Y8){var _Y9=E(_Y8);switch(_Y9[0]){case 0:return [0,new T(function(){return B(_XN(_Y9[1]));}),_Y7];case 1:return [0,new T(function(){return B(_XR(_Y9[1]));}),_Y7];case 2:return !E(_Y9[1])?[0,_XM,_Y7]:[0,_Y5,_Y7];case 3:var _Ya=E(_Y9[1]);return _Ya[0]==0?[0,_XY,[1,_XC,_Y7]]:[0,_XY,new T(function(){var _Yb=B(_Y6(new T(function(){var _Yc=function(_Yd){var _Ye=E(_Yd);return _Ye[0]==0?E([1,_XC,_Y7]):[1,_XL,new T(function(){var _Yf=B(_Y6(new T(function(){return B(_Yc(_Ye[2]));}),_Ye[1]));return [1,_Yf[1],_Yf[2]];})];};return B(_Yc(_Ya[2]));}),_Ya[1]));return [1,_Yb[1],_Yb[2]];})];case 4:var _Yg=E(_Y9[1]);if(!_Yg[0]){return [0,_Y1,[1,_XF,_Y7]];}else{var _Yh=E(_Yg[1]);return [0,_Y1,[1,new T(function(){return B(_XR(_Yh[1]));}),[1,_XI,new T(function(){var _Yi=B(_Y6(new T(function(){var _Yj=function(_Yk){var _Yl=E(_Yk);if(!_Yl[0]){return E([1,_XF,_Y7]);}else{var _Ym=E(_Yl[1]);return [1,_XL,[1,_Y4,[1,_Ym[1],[1,_Y4,[1,_XI,new T(function(){var _Yn=B(_Y6(new T(function(){return B(_Yj(_Yl[2]));}),_Ym[2]));return [1,_Yn[1],_Yn[2]];})]]]]];}};return B(_Yj(_Yg[2]));}),_Yh[2]));return [1,_Yi[1],_Yi[2]];})]]];}break;default:return [0,_XV,_Y7];}},_Yo=function(_Yp){var _Yq=jsCat(new T(function(){var _Yr=B(_Y6(_o,_Yp));return [1,_Yr[1],_Yr[2]];}),E(_Xz)[1]),_Ys=_Yq;return E(_Ys);},_Yt=new T(function(){return [0,"(function(k,v) {localStorage.setItem(k,v);})"];}),_Yu=function(_Yv,_Yw){return function(_Yx,_){var _Yy=B(A(new T(function(){return B(A(_Uy,[E(_Yt)[1],E(toJSStr(E(_Yw)))]));}),[E(B(_Yo(B(A(new T(function(){return B(_f6(_Yv));}),[_Yx]))))),_])),_Yz=_Yy;return _4W;};},_YA=new T(function(){return B(_Yu(_hm,_Xy));}),_YB=function(_YC,_){var _YD=B(A(_YA,[_YC,_])),_YE=_YD;return new F(function(){return _MB(_YC,_);});},_YF=new T(function(){return B(unCStr("%.2f"));}),_YG=[1,_j7,_o],_YH=function(_YI,_){var _YJ=jsEval(toJSStr(B(unAppCStr("document.title = ",[1,_j7,new T(function(){return B(_j9(B(_1p(_mI,B(_L1(_YF,new T(function(){return B(_n4([1,[4,new T(function(){return E(E(_YI)[1]);})],_o],_o));}))))),_YG));})])))),_YK=_YJ;return [0,_4W,_YI];},_YL=function(_YM,_YN,_YO){while(1){var _YP=(function(_YQ,_YR,_YS){var _YT=E(_YS);if(!_YT[0]){return [0,_YQ,_YR];}else{var _YU=_YT[1];_YM=new T(function(){var _YV=E(E(_YU)[1]);switch(_YV){case -1:var _YW=[0,0];break;case 0:var _YW=E(_u5);break;default:var _YW=[0,B(_uz(E(_YQ)[1],_YV))];}var _YX=_YW;return _YX;});var _YY=[1,new T(function(){return [0,B(_u9(E(_YQ)[1],E(_YU)[1]))];}),_YR];_YO=_YT[2];_YN=_YY;return null;}})(_YM,_YN,_YO);if(_YP!=null){return _YP;}}},_YZ=function(_Z0,_Z1,_Z2,_Z3){return new F(function(){return _YL(new T(function(){var _Z4=E(E(_Z2)[1]);switch(_Z4){case -1:var _Z5=[0,0];break;case 0:var _Z5=E(_u5);break;default:var _Z5=[0,B(_uz(E(_Z0)[1],_Z4))];}var _Z6=_Z5;return _Z6;}),[1,new T(function(){return [0,B(_u9(E(_Z0)[1],E(_Z2)[1]))];}),_Z1],_Z3);});},_Z7=function(_Z8,_Z9){var _Za=E(_Z8);if(!_Za[0]){return [0];}else{var _Zb=_Za[1];return _Z9>1?[1,_Zb,new T(function(){return B(_Z7(_Za[2],_Z9-1|0));})]:[1,_Zb,_o];}},_Zc=new T(function(){return B(_13(0,_2j,_o));}),_Zd=new T(function(){return B(_13(0,_RV,_o));}),_Ze=function(_Zf,_Zg){var _Zh=E(_Zg);if(!_Zh[0]){return [0,_o,_o];}else{var _Zi=_Zh[1];if(!B(A(_Zf,[_Zi]))){var _Zj=new T(function(){var _Zk=B(_Ze(_Zf,_Zh[2]));return [0,_Zk[1],_Zk[2]];});return [0,[1,_Zi,new T(function(){return E(E(_Zj)[1]);})],new T(function(){return E(E(_Zj)[2]);})];}else{return [0,_o,_Zh];}}},_Zl=function(_Zm,_Zn){var _Zo=function(_Zp,_Zq){return !B(_4n(_Zq,_o))?[0,_Zp,new T(function(){var _Zr=B(_Zl(_Zm,_Zq));return [1,_Zr[1],_Zr[2]];})]:[0,_Zp,_o];};if(_Zm>=0){var _Zs=B(_Ep(_Zm,_Zn));return new F(function(){return _Zo(_Zs[1],_Zs[2]);});}else{return new F(function(){return _Zo(_o,_Zn);});}},_Zt=function(_Zu){var _Zv=E(_Zu);if(!_Zv[0]){return [0];}else{return new F(function(){return _I(_Zv[1],new T(function(){return B(_Zt(_Zv[2]));}));});}},_Zw=function(_Zx){return E(E(_Zx)[1])==46?true:false;},_Zy=[0,44],_Zz=[1,_Zy,_o],_ZA=function(_ZB,_ZC){var _ZD=E(_ZC);return _ZD[0]==0?[0]:[1,_ZB,[1,_ZD[1],new T(function(){return B(_ZA(_ZB,_ZD[2]));})]];},_ZE=function(_ZF){var _ZG=new T(function(){var _ZH=B(_Ze(_Zw,_ZF));return [0,_ZH[1],_ZH[2]];}),_ZI=B(_Zl(3,new T(function(){return B(_n4(E(_ZG)[1],_o));})));return new F(function(){return _I(B(_n4(B(_Zt([1,_ZI[1],new T(function(){return B(_ZA(_Zz,_ZI[2]));})])),_o)),new T(function(){return E(E(_ZG)[2]);}));});},_ZJ=function(_ZK){return _ZK>1000?B(_ZE(new T(function(){var _ZL=B(_q3(_ZK)),_ZM=_ZL[1],_ZN=_ZL[2];if(_ZN>=0){var _ZO=B(_13(0,B(_sk(_ZM,_ZN)),_o));}else{var _ZP= -_ZN;if(_ZP<=52){var _ZQ=hs_uncheckedIShiftRA64(B(_sc(_ZM)),_ZP),_ZR=_ZQ,_ZS=B(_13(0,B(_rv(_ZR)),_o));}else{var _ZS=!B(_S(_ZM,_2j))?E(_Zc):E(_Zd);}var _ZT=_ZS,_ZU=_ZT,_ZO=_ZU;}var _ZV=_ZO,_ZW=_ZV;return _ZW;}))):B(_ZE(new T(function(){return B(_Z7(B(_L1(_YF,new T(function(){return B(_n4([1,[4,[0,_ZK]],_o],_o));}))),5));})));},_ZX=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:213:7-12"));}),_ZY=function(_ZZ,_){return [0,_4W,_ZZ];},_100=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:226:9-14"));}),_101=[0,500],_102=[0,100],_103=[0,50],_104=[0,10],_105=[0,5],_106=[1,_PE,_o],_107=[1,_105,_106],_108=[1,_104,_107],_109=[1,_103,_108],_10a=[1,_102,_109],_10b=function(_10c){var _10d=E(_10c);return _10d[0]==0?E(_ZY):function(_10e,_){var _10f=B(A(new T(function(){var _10g=E(_10d[1]),_10h=_10g[1],_10i=new T(function(){return B(A(E(_10g[2])[2],[_10h]));});return function(_10j,_){var _10k=E(_10j),_10l=_10k[7],_10m=E(_10h)[1];return !B(_m6(_10m,_10l))?B(A(_10i,[_10k,_])):B(_lZ(_10l,_10m))[0]==0?B(A(_10i,[_10k,_])):[0,_4W,_10k];};}),[_10e,_])),_10n=_10f;return new F(function(){return A(new T(function(){return B(_10b(_10d[2]));}),[new T(function(){return E(E(_10n)[2]);}),_]);});};},_10o=new T(function(){return B(_10b(_NL));}),_10p=[1,_101,_10a],_10q=new T(function(){return B(unCStr("%s<span class=\"item-%d\">%s</span>"));}),_10r=new T(function(){return B(unCStr("disabled"));}),_10s=function(_10t){return new F(function(){return err(B(unAppCStr("docFocused: ",[1,_j7,new T(function(){return B(_j9(_10t,_YG));})])));});},_10u=new T(function(){return B(unCStr("true"));}),_10v=new T(function(){return B(unCStr("false"));}),_10w=new T(function(){return B(_Zt(_o));}),_10x=new T(function(){return B(_1p(_mI,_10w));}),_10y=[0,-2147483648],_10z=new T(function(){return B(unCStr("\u518d\u4f1a\u30dc\u30fc\u30ca\u30b9<br>\u4f9d\u5b58\u5ea6 +"));}),_10A=new T(function(){return B(unCStr("document.hasFocus()"));}),_10B=function(_10C,_10D){while(1){var _10E=E(_10C);if(!_10E[0]){var _10F=_10E[1],_10G=E(_10D);if(!_10G[0]){var _10H=_10G[1],_10I=subC(_10F,_10H);if(!E(_10I[2])){return [0,_10I[1]];}else{_10C=[1,I_fromInt(_10F)];_10D=[1,I_fromInt(_10H)];continue;}}else{_10C=[1,I_fromInt(_10F)];_10D=_10G;continue;}}else{var _10J=E(_10D);if(!_10J[0]){_10C=_10E;_10D=[1,I_fromInt(_10J[1])];continue;}else{return [1,I_sub(_10E[1],_10J[1])];}}}},_10K=new T(function(){return B(_Uy("(function(e,c){e.removeAttribute(c);})"));}),_10L=function(_10M){return function(_10N,_){var _10O=B(A(new T(function(){return B(A(_10K,[E(E(_10M)[1])]));}),[E(toJSStr(E(_10N))),_])),_10P=_10O;return _4W;};},_10Q=function(_10R,_10S){var _10T=E(_10R);if(!_10T[0]){return [0];}else{var _10U=E(_10S);return _10U[0]==0?[0]:[1,[0,_10T[1],_10U[1]],new T(function(){return B(_10Q(_10T[2],_10U[2]));})];}},_10V=function(_10W,_){var _10X=jsEval(toJSStr(E(_10A))),_10Y=_10X,_10Z=B(_mc(_)),_110=_10Z,_111=new T(function(){var _112=fromJSStr(_10Y);return !B(_4n(_112,_10v))?!B(_4n(_112,_10u))?B(_10s(_112)):true:false;}),_113=function(_,_114,_115,_116,_117,_118,_119,_11a,_11b,_11c){var _11d=B(A(_10o,[[0,_115,_116,_117,_118,_119,_111,_11b,_11c],_])),_11e=_11d,_11f=jsFind(toJSStr(E(_8))),_11g=_11f,_11h=E(_11g);if(!_11h[0]){return new F(function(){return _Pg(_ZX,_);});}else{var _11i=E(_n1),_11j=toJSStr(_11i),_11k=E(E(_11e)[2]),_11l=_11k[8],_11m=jsSet(E(_11h[1])[1],_11j,toJSStr(B(_ZJ(E(_11k[2])[1])))),_11n=jsFind(toJSStr(E(_a))),_11o=_11n,_11p=E(_11o);if(!_11p[0]){return new F(function(){return _Pg(_ZX,_);});}else{var _11q=E(_11k[1])[1],_11r=jsSet(E(_11p[1])[1],_11j,toJSStr(B(_ZJ(_11q)))),_11s=jsFind(toJSStr(E(_6))),_11t=_11s,_11u=E(_11t);if(!_11u[0]){return new F(function(){return _Pg(_ZX,_);});}else{var _11v=jsSet(E(_11u[1])[1],_11j,toJSStr(B(_ZJ(E(_11k[3])[1])))),_11w=function(_11x){var _11y=E(_11x);return _11y[0]==0?E(_ZY):function(_11z,_){var _11A=B(A(new T(function(){var _11B=E(_11y[1]),_11C=_11B[1],_11D=E(_11B[2])[1],_11E=new T(function(){var _11F=E(_11C)[1];return _11F<=0?B(unAppCStr("item-sp-",new T(function(){if(_11F<0){var _11G=B(_7J(0, -_11F,_o));}else{var _11G=B(_7J(0,_11F,_o));}var _11H=_11G;return _11H;}))):B(unAppCStr("item-",new T(function(){return B(_7J(0,_11F,_o));})));}),_11I=new T(function(){var _11J=B(_mK(E(_11C)[1],_11l));return _11J[0]==0?E(_lj):E(_11J[1]);});return function(_11K,_){var _11L=B(A(new T(function(){if(E(_11C)[1]<=0){var _11M=E(_ZY);}else{var _11M=function(_11N,_){var _11O=E(new T(function(){return B(_I(_11E,_Wb));})),_11P=jsFind(toJSStr(_11O)),_11Q=_11P,_11R=E(_11Q);if(!_11R[0]){return new F(function(){return _Mb(_11O);});}else{var _11S=jsFind(toJSStr(E(new T(function(){return B(_I(_11E,_W9));})))),_11T=_11S,_11U=E(_11T);if(!_11U[0]){return new F(function(){return _Pg(_100,_);});}else{var _11V=jsGet(E(_11U[1])[1],toJSStr(_11i)),_11W=_11V,_11X=new T(function(){return fromJSStr(_11W);}),_11Y=function(_11Z){return _11Z>1?[1,_11X,new T(function(){return B(_11Y(_11Z-1|0));})]:E([1,_11X,_o]);},_120=jsSet(E(_11R[1])[1],_11j,toJSStr(B((function(_121,_122){while(1){var _123=(function(_124,_125){var _126=E(_125);if(!_126[0]){return E(_124);}else{var _127=E(_126[1]);_121=B(_1p(_mI,B(_L1(_10q,new T(function(){return B(_n4([1,[1,new T(function(){var _128=E(_127[2])[1];if(_128>0){var _129=B(_1p(_mI,B(_Zt(B(_11Y(_128))))));}else{var _129=E(_10x);}var _12a=_129,_12b=_12a;return _12b;})],[1,[2,_10y,new T(function(){return B(_6q(E(_127[1])[1]));})],[1,[1,new T(function(){return B(_1p(_mI,_124));})],_o]]],_o));})))));_122=_126[2];return null;}})(_121,_122);if(_123!=null){return _123;}}})(_o,new T(function(){return B(_10Q(_10p,new T(function(){return B(_n4(B(_YZ(_11I,_o,_101,_10a))[2],_o));})));}))))),_12c=E(new T(function(){return B(_I(_11E,_Wa));})),_12d=jsFind(toJSStr(_12c)),_12e=_12d,_12f=E(_12e);return _12f[0]==0?B(_Mb(_12c)):B(A(_Vg,[_QU,_12f[1],_11i,new T(function(){return B(_7J(0,E(_11I)[1],_o));}),_11N,_]));}}};}var _12g=_11M,_12h=_12g;return _12h;}),[_11K,_])),_12i=_11L,_12j=E(new T(function(){return B(_I(_11E,_PD));})),_12k=jsFind(toJSStr(_12j)),_12l=_12k,_12m=E(_12l);if(!_12m[0]){return new F(function(){return _Mb(_12j);});}else{var _12n=_12m[1];if(!E(new T(function(){var _12o=E(_11C)[1];if(_12o<=0){if(!B(_m6(_12o,_11l))){var _12p=B(_nP(B(A(_11D,[_11I]))))<=_11q;}else{if(B(_lZ(_11l,_12o))[1]>=1){var _12q=false;}else{var _12q=B(_nP(B(A(_11D,[_11I]))))<=_11q;}var _12r=_12q,_12s=_12r,_12p=_12s;}var _12t=_12p;}else{var _12t=B(_nP(B(A(_11D,[_11I]))))<=_11q;}var _12u=_12t,_12v=_12u;return _12v;}))){var _12w=B(A(_Va,[_QU,_12n,_10r,_10r,new T(function(){return E(E(_12i)[2]);}),_])),_12x=_12w,_12y=B(_I(_11E,_Wc)),_12z=jsFind(toJSStr(_12y)),_12A=_12z,_12B=E(_12A);if(!_12B[0]){return new F(function(){return _Mb(_12y);});}else{var _12C=jsSet(E(_12B[1])[1],toJSStr(_11i),toJSStr(B(_ZE(new T(function(){return B(_13(0,B(A(_11D,[_11I])),_o));})))));return [0,_4W,new T(function(){return E(E(_12x)[2]);})];}}else{var _12D=B(A(_10L,[_12n,_10r,_])),_12E=_12D,_12F=B(_I(_11E,_Wc)),_12G=jsFind(toJSStr(_12F)),_12H=_12G,_12I=E(_12H);if(!_12I[0]){return new F(function(){return _Mb(_12F);});}else{var _12J=jsSet(E(_12I[1])[1],toJSStr(_11i),toJSStr(B(_ZE(new T(function(){return B(_13(0,B(A(_11D,[_11I])),_o));})))));return [0,_4W,new T(function(){return E(E(_12i)[2]);})];}}}};}),[_11z,_])),_12K=_11A;return new F(function(){return A(new T(function(){return B(_11w(_11y[2]));}),[new T(function(){return E(E(_12K)[2]);}),_]);});};};return new F(function(){return A(_11w,[_Tb,_11k,_]);});}}}},_12L=new T(function(){return [0,B(_nP(B(_10B(_110,E(_10W)[4]))))];});if(!E(_111)){var _12M=E(_10W),_12N=_12M[1],_12O=_12M[2],_12P=_12M[4],_12Q=_12M[5],_12R=_12M[6],_12S=_12M[7],_12T=_12M[8],_12U=E(_12M[3])[1],_12V=new T(function(){return [0,E(_12L)[1]/1000/60/60/10];});return _12U<=0?B(_113(_,_4W,new T(function(){return [0,E(_12N)[1]+E(_12O)[1]/30];}),_12O,new T(function(){var _12W=_12U-E(_12V)[1];return _12W>0?[0,_12W]:E(_PM);}),_12P,_12Q,_12R,_12S,_12T)):B(_113(_,_4W,new T(function(){return [0,E(_12N)[1]+E(_12O)[1]/30];}),new T(function(){return [0,E(_12O)[1]+E(_12V)[1]];}),new T(function(){var _12X=_12U-E(_12V)[1];return _12X>0?[0,_12X]:E(_PM);}),_12P,_12Q,_12R,_12S,_12T));}else{var _12Y=E(_10W),_12Z=_12Y[1],_130=_12Y[2],_131=_12Y[3],_132=_12Y[4],_133=_12Y[5],_134=_12Y[7],_135=_12Y[8],_136=new T(function(){return [0,E(_12L)[1]/1000/60/60/120];});if(!E(_12Y[6])){var _137=new T(function(){return [0,E(_12L)[1]/1000/60/10];}),_138=B(_Md(new T(function(){return B(_I(_10z,new T(function(){return B(_ZJ(E(_137)[1]));})));}),_)),_139=_138;return new F(function(){return _113(_,_4W,new T(function(){return [0,E(_12Z)[1]+E(_130)[1]/30];}),new T(function(){return [0,E(_130)[1]+E(_136)[1]];}),new T(function(){var _13a=E(_131)[1]+E(_12L)[1]/1000/60/60/100+E(_137)[1]-E(_136)[1];return _13a>0?[0,_13a]:E(_PM);}),_132,_133,_l,_134,_135);});}else{return new F(function(){return _113(_,_4W,new T(function(){return [0,E(_12Z)[1]+E(_130)[1]/30];}),new T(function(){return [0,E(_130)[1]+E(_136)[1]];}),new T(function(){var _13b=E(_131)[1]+E(_12L)[1]/1000/60/60/100-E(_136)[1];return _13b>0?[0,_13b]:E(_PM);}),_132,_133,_h,_134,_135);});}}},_13c=[0,-2],_13d=[0,-1],_13e=new T(function(){return B(_sT(-1,-2));}),_13f=new T(function(){return B(_mR(1,2147483647));}),_13g=function(_){var _=0,_13h=jsMkStdout(),_13i=_13h;return [0,_13i];},_13j=new T(function(){return B(_Uu(_13g));}),_13k=function(_){var _13l=B(_mc(_)),_13m=_13l,_13n=B(_UG(_hm,_Xy,_)),_13o=_13n,_13p=nMV(new T(function(){var _13q=E(_13o);return _13q[0]==0?[0,_PM,_PM,_PM,_13m,_2j,_l,_gd,_gd]:E(_13q[1]);})),_13r=_13p,_13s=B(unCStr("list-group")),_13t=jsFind(toJSStr(_13s)),_13u=_13t,_13v=E(_13u);if(!_13v[0]){return new F(function(){return _Mb(_13s);});}else{var _13w=B((function(_13x,_){while(1){var _13y=E(_13x);if(!_13y[0]){return _4W;}else{var _13z=E(_13y[1]),_13A=E(E(_13z[2])[3]),_13B=B(A(_Wr,[_13z[1],_13A[1],_13A[2],_13A[3],_13v[1],_])),_13C=_13B;_13x=_13y[2];continue;}}})(_Uq,_)),_13D=_13w,_13E=B(unCStr("list-sp-group")),_13F=jsFind(toJSStr(_13E)),_13G=_13F,_13H=E(_13G);if(!_13H[0]){return new F(function(){return _Mb(_13E);});}else{var _13I=B((function(_13J,_){while(1){var _13K=E(_13J);if(!_13K[0]){return _4W;}else{var _13L=E(_13K[1]),_13M=E(E(_13L[2])[3]),_13N=B(A(_Wr,[_13L[1],_13M[1],_13M[2],_13M[3],_13H[1],_])),_13O=_13N;_13J=_13K[2];continue;}}})(_Ur,_)),_13P=_13I,_13Q=[0,_13r],_13R=B(_Td(_13Q,_13f,_)),_13S=_13R,_13T=B(_Td(_13Q,_13e,_)),_13U=_13T,_13V=rMV(_13r),_13W=_13V,_13X=E(_13W),_13Y=_13X[8],_13Z=function(_){var _140=rMV(_13r),_141=_140,_142=E(_141),_143=_142[8],_144=function(_){var _145=B(_lL(33,_13r,_10V,_)),_146=_145,_147=B(_UZ(_13j,B(_lI(_146)),_)),_148=_147,_149=B(_lL(1000,_13r,_YH,_)),_14a=_149,_14b=B(_UZ(_13j,B(_lI(_14a)),_)),_14c=_14b,_14d=B(_lL(60000,_13r,_YB,_)),_14e=_14d;return new F(function(){return _UZ(_13j,B(_lI(_14e)),_);});};if(!B(_m6(-2,_143))){var _=wMV(_13r,_142);return new F(function(){return _144(_);});}else{if(B(_lZ(_143,-2))[1]<=0){var _=wMV(_13r,_142);return new F(function(){return _144(_);});}else{var _14f=B(A(B(_lZ(_Tc,-2))[2],[_13c,_142,_])),_14g=_14f,_=wMV(_13r,new T(function(){return E(E(_14g)[2]);}));return new F(function(){return _144(_);});}}};if(!B(_m6(-1,_13Y))){var _=wMV(_13r,_13X);return new F(function(){return _13Z(_);});}else{if(B(_lZ(_13Y,-1))[1]<=0){var _=wMV(_13r,_13X);return new F(function(){return _13Z(_);});}else{var _14h=B(A(B(_lZ(_Tc,-1))[2],[_13d,_13X,_])),_14i=_14h,_=wMV(_13r,new T(function(){return E(E(_14i)[2]);}));return new F(function(){return _13Z(_);});}}}}},_14j=function(_){return new F(function(){return _13k(_);});};
var hasteMain = function() {B(A(_14j, [0]));};window.onload = hasteMain;
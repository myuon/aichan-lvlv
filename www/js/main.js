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

var _0=new T(function(){return [0,"achievements"];}),_1=new T(function(){return [0,"lastFocus"];}),_2=new T(function(){return [0,"depend"];}),_3=new T(function(){return [0,"lps"];}),_4=new T(function(){return [0,"loves"];}),_5=new T(function(){return [0,"items"];}),_6=function(_7){return [0,toJSStr(E(_7))];},_8=function(_9){return [1,new T(function(){return B(_6(_9));})];},_a=new T(function(){return [0,"value"];}),_b=true,_c=[2,_b],_d=new T(function(){return [0,"hasValue"];}),_e=[0,_d,_c],_f=false,_g=[2,_f],_h=[0,_d,_g],_i=[0],_j=[1,_h,_i],_k=[4,_j],_l=function(_m,_n){while(1){var _o=(function(_p,_q){var _r=E(_q);switch(_r[0]){case 0:_m=new T(function(){return B(_l(_p,_r[4]));});_n=_r[3];return null;case 1:return [1,[3,[1,[0,[0,_r[1]]],[1,new T(function(){var _s=E(_r[2]);return _s[0]==0?E(_k):[4,[1,_e,[1,[0,_a,new T(function(){return B(_8(_s[1]));})],_i]]];}),_i]]],_p];default:return E(_p);}})(_m,_n);if(_o!=null){return _o;}}},_t=function(_u){return [0,new T(function(){return [0,E(_u)[1]];})];},_v=function(_w,_x){while(1){var _y=(function(_z,_A){var _B=E(_A);switch(_B[0]){case 0:_w=new T(function(){return B(_v(_z,_B[4]));});_x=_B[3];return null;case 1:return [1,[3,[1,[0,[0,_B[1]]],[1,new T(function(){return B(_t(_B[2]));}),_i]]],_z];default:return E(_z);}})(_w,_x);if(_y!=null){return _y;}}},_C=function(_D,_E){var _F=E(_D);return _F[0]==0?E(_E):[1,_F[1],new T(function(){return B(_C(_F[2],_E));})];},_G=function(_H){while(1){var _I=E(_H);if(!_I[0]){_H=[1,I_fromInt(_I[1])];continue;}else{return new F(function(){return I_toString(_I[1]);});}}},_J=function(_K,_L){return new F(function(){return _C(fromJSStr(B(_G(_K))),_L);});},_M=function(_N,_O){var _P=E(_N);if(!_P[0]){var _Q=_P[1],_R=E(_O);return _R[0]==0?_Q<_R[1]:I_compareInt(_R[1],_Q)>0;}else{var _S=_P[1],_T=E(_O);return _T[0]==0?I_compareInt(_S,_T[1])<0:I_compare(_S,_T[1])<0;}},_U=[0,41],_V=[0,40],_W=[0,0],_X=function(_Y,_Z,_10){return _Y<=6?B(_J(_Z,_10)):!B(_M(_Z,_W))?B(_J(_Z,_10)):[1,_V,new T(function(){return B(_C(fromJSStr(B(_G(_Z))),[1,_U,_10]));})];},_11=function(_12,_13,_14,_15,_16,_17){return [1,[0,_4,[0,_12]],[1,[0,_3,[0,_13]],[1,[0,_2,[0,_14]],[1,[0,_1,[1,new T(function(){return [0,toJSStr(B(_X(0,_15,_i)))];})]],[1,[0,_0,[3,new T(function(){var _18=E(_16);if(!_18[0]){var _19=_18[3],_1a=_18[4],_1b=_18[2]>=0?B(_l(new T(function(){return B(_l(_i,_1a));}),_19)):B(_l(new T(function(){return B(_l(_i,_19));}),_1a));}else{var _1b=B(_l(_i,_18));}return _1b;})]],[1,[0,_5,[3,new T(function(){var _1c=E(_17);if(!_1c[0]){var _1d=_1c[3],_1e=_1c[4],_1f=_1c[2]>=0?B(_v(new T(function(){return B(_v(_i,_1e));}),_1d)):B(_v(new T(function(){return B(_v(_i,_1d));}),_1e));}else{var _1f=B(_v(_i,_1c));}return _1f;})]],_i]]]]]];},_1g=function(_1h){var _1i=E(_1h);return [4,B(_11(_1i[1],_1i[2],_1i[3],_1i[4],_1i[7],_1i[8]))];},_1j=function(_1k,_1l){var _1m=E(_1l);return _1m[0]==0?[0]:[1,new T(function(){return B(A(_1k,[_1m[1]]));}),new T(function(){return B(_1j(_1k,_1m[2]));})];},_1n=function(_1o){return [3,new T(function(){return B(_1j(_1g,_1o));})];},_1p=function(_1q,_1r){var _1s=strEq(E(_1q)[1],E(_1r)[1]),_1t=_1s;return E(_1t)==0?true:false;},_1u=function(_1v,_1w){var _1x=strEq(E(_1v)[1],E(_1w)[1]),_1y=_1x;return E(_1y)==0?false:true;},_1z=[0,_1u,_1p],_1A=[1,_i],_1B=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_1C=[0,_1B],_1D=new T(function(){return B(unCStr("Tried to deserialize a non-array into a pair!"));}),_1E=[0,_1D],_1F=function(_1G){return E(E(_1G)[3]);},_1H=function(_1I,_1J,_1K){var _1L=E(_1K);if(_1L[0]==3){var _1M=E(_1L[1]);if(!_1M[0]){return E(_1E);}else{var _1N=E(_1M[2]);if(!_1N[0]){return E(_1E);}else{if(!E(_1N[2])[0]){var _1O=B(A(_1F,[_1I,_1M[1]]));if(!_1O[0]){return [0,_1O[1]];}else{var _1P=B(A(_1F,[_1J,_1N[1]]));return _1P[0]==0?[0,_1P[1]]:[1,[0,_1O[1],_1P[1]]];}}else{return E(_1E);}}}}else{return E(_1E);}},_1Q=function(_1R,_1S,_1T){var _1U=E(_1T);if(_1U[0]==3){var _1V=function(_1W){var _1X=E(_1W);if(!_1X[0]){return E(_1A);}else{var _1Y=B(_1H(_1R,_1S,_1X[1]));if(!_1Y[0]){return [0,_1Y[1]];}else{var _1Z=B(_1V(_1X[2]));return _1Z[0]==0?[0,_1Z[1]]:[1,[1,_1Y[1],_1Z[1]]];}}};return new F(function(){return _1V(_1U[1]);});}else{return E(_1C);}},_20=new T(function(){return B(unCStr("Tried to deserialize a non-JSString to a JSString"));}),_21=[0,_20],_22=new T(function(){return B(unCStr("Tried to deserialize a non-Number to a Double"));}),_23=[0,_22],_24=new T(function(){return B(unCStr("Key not found"));}),_25=[0,_24],_26=new T(function(){return B(unCStr("Tried to do lookup on non-object!"));}),_27=[0,_26],_28=[0,0],_29=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_2a=new T(function(){return B(err(_29));}),_2b=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_2c=new T(function(){return B(err(_2b));}),_2d=new T(function(){return B(unCStr("Control.Exception.Base"));}),_2e=new T(function(){return B(unCStr("base"));}),_2f=new T(function(){return B(unCStr("PatternMatchFail"));}),_2g=new T(function(){var _2h=hs_wordToWord64(18445595),_2i=_2h,_2j=hs_wordToWord64(52003073),_2k=_2j;return [0,_2i,_2k,[0,_2i,_2k,_2e,_2d,_2f],_i];}),_2l=function(_2m){return E(_2g);},_2n=function(_2o){return E(E(_2o)[1]);},_2p=function(_2q,_2r,_2s){var _2t=B(A(_2q,[_])),_2u=B(A(_2r,[_])),_2v=hs_eqWord64(_2t[1],_2u[1]),_2w=_2v;if(!E(_2w)){return [0];}else{var _2x=hs_eqWord64(_2t[2],_2u[2]),_2y=_2x;return E(_2y)==0?[0]:[1,_2s];}},_2z=function(_2A){var _2B=E(_2A);return new F(function(){return _2p(B(_2n(_2B[1])),_2l,_2B[2]);});},_2C=function(_2D){return E(E(_2D)[1]);},_2E=function(_2F,_2G){return new F(function(){return _C(E(_2F)[1],_2G);});},_2H=[0,44],_2I=[0,93],_2J=[0,91],_2K=function(_2L,_2M,_2N){var _2O=E(_2M);return _2O[0]==0?B(unAppCStr("[]",_2N)):[1,_2J,new T(function(){return B(A(_2L,[_2O[1],new T(function(){var _2P=function(_2Q){var _2R=E(_2Q);return _2R[0]==0?E([1,_2I,_2N]):[1,_2H,new T(function(){return B(A(_2L,[_2R[1],new T(function(){return B(_2P(_2R[2]));})]));})];};return B(_2P(_2O[2]));})]));})];},_2S=function(_2T,_2U){return new F(function(){return _2K(_2E,_2T,_2U);});},_2V=function(_2W,_2X,_2Y){return new F(function(){return _C(E(_2X)[1],_2Y);});},_2Z=[0,_2V,_2C,_2S],_30=new T(function(){return [0,_2l,_2Z,_31,_2z];}),_31=function(_32){return [0,_30,_32];},_33=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_34=function(_35,_36){return new F(function(){return die(new T(function(){return B(A(_36,[_35]));}));});},_37=function(_38,_39){var _3a=E(_39);if(!_3a[0]){return [0,_i,_i];}else{var _3b=_3a[1];if(!B(A(_38,[_3b]))){return [0,_i,_3a];}else{var _3c=new T(function(){var _3d=B(_37(_38,_3a[2]));return [0,_3d[1],_3d[2]];});return [0,[1,_3b,new T(function(){return E(E(_3c)[1]);})],new T(function(){return E(E(_3c)[2]);})];}}},_3e=[0,32],_3f=[0,10],_3g=[1,_3f,_i],_3h=function(_3i){return E(E(_3i)[1])==124?false:true;},_3j=function(_3k,_3l){var _3m=B(_37(_3h,B(unCStr(_3k)))),_3n=_3m[1],_3o=function(_3p,_3q){return new F(function(){return _C(_3p,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_C(_3l,new T(function(){return B(_C(_3q,_3g));})));})));}));});},_3r=E(_3m[2]);if(!_3r[0]){return new F(function(){return _3o(_3n,_i);});}else{return E(E(_3r[1])[1])==124?B(_3o(_3n,[1,_3e,_3r[2]])):B(_3o(_3n,_i));}},_3s=function(_3t){return new F(function(){return _34([0,new T(function(){return B(_3j(_3t,_33));})],_31);});},_3u=new T(function(){return B(_3s("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_3v=function(_3w,_3x){while(1){var _3y=(function(_3z,_3A){var _3B=E(_3z);switch(_3B[0]){case 0:var _3C=E(_3A);if(!_3C[0]){return [0];}else{_3w=B(A(_3B[1],[_3C[1]]));_3x=_3C[2];return null;}break;case 1:var _3D=B(A(_3B[1],[_3A])),_3E=_3A;_3w=_3D;_3x=_3E;return null;case 2:return [0];case 3:return [1,[0,_3B[1],_3A],new T(function(){return B(_3v(_3B[2],_3A));})];default:return E(_3B[1]);}})(_3w,_3x);if(_3y!=null){return _3y;}}},_3F=function(_3G,_3H){var _3I=function(_3J){var _3K=E(_3H);if(_3K[0]==3){return [3,_3K[1],new T(function(){return B(_3F(_3G,_3K[2]));})];}else{var _3L=E(_3G);if(_3L[0]==2){return E(_3K);}else{var _3M=E(_3K);if(_3M[0]==2){return E(_3L);}else{var _3N=function(_3O){var _3P=E(_3M);if(_3P[0]==4){return [1,function(_3Q){return [4,new T(function(){return B(_C(B(_3v(_3L,_3Q)),_3P[1]));})];}];}else{var _3R=E(_3L);if(_3R[0]==1){var _3S=_3R[1],_3T=E(_3P);return _3T[0]==0?[1,function(_3U){return new F(function(){return _3F(B(A(_3S,[_3U])),_3T);});}]:[1,function(_3V){return new F(function(){return _3F(B(A(_3S,[_3V])),new T(function(){return B(A(_3T[1],[_3V]));}));});}];}else{var _3W=E(_3P);return _3W[0]==0?E(_3u):[1,function(_3X){return new F(function(){return _3F(_3R,new T(function(){return B(A(_3W[1],[_3X]));}));});}];}}},_3Y=E(_3L);switch(_3Y[0]){case 1:var _3Z=E(_3M);if(_3Z[0]==4){return [1,function(_40){return [4,new T(function(){return B(_C(B(_3v(B(A(_3Y[1],[_40])),_40)),_3Z[1]));})];}];}else{return new F(function(){return _3N(_);});}break;case 4:var _41=_3Y[1],_42=E(_3M);switch(_42[0]){case 0:return [1,function(_43){return [4,new T(function(){return B(_C(_41,new T(function(){return B(_3v(_42,_43));})));})];}];case 1:return [1,function(_44){return [4,new T(function(){return B(_C(_41,new T(function(){return B(_3v(B(A(_42[1],[_44])),_44));})));})];}];default:return [4,new T(function(){return B(_C(_41,_42[1]));})];}break;default:return new F(function(){return _3N(_);});}}}}},_45=E(_3G);switch(_45[0]){case 0:var _46=E(_3H);if(!_46[0]){return [0,function(_47){return new F(function(){return _3F(B(A(_45[1],[_47])),new T(function(){return B(A(_46[1],[_47]));}));});}];}else{return new F(function(){return _3I(_);});}break;case 3:return [3,_45[1],new T(function(){return B(_3F(_45[2],_3H));})];default:return new F(function(){return _3I(_);});}},_48=[0,41],_49=[1,_48,_i],_4a=[0,40],_4b=[1,_4a,_i],_4c=function(_4d,_4e){while(1){var _4f=E(_4d);if(!_4f[0]){return E(_4e)[0]==0?true:false;}else{var _4g=E(_4e);if(!_4g[0]){return false;}else{if(E(_4f[1])[1]!=E(_4g[1])[1]){return false;}else{_4d=_4f[2];_4e=_4g[2];continue;}}}}},_4h=function(_4i,_4j){return E(_4i)[1]!=E(_4j)[1];},_4k=function(_4l,_4m){return E(_4l)[1]==E(_4m)[1];},_4n=[0,_4k,_4h],_4o=function(_4p,_4q){while(1){var _4r=E(_4p);if(!_4r[0]){return E(_4q)[0]==0?true:false;}else{var _4s=E(_4q);if(!_4s[0]){return false;}else{if(E(_4r[1])[1]!=E(_4s[1])[1]){return false;}else{_4p=_4r[2];_4q=_4s[2];continue;}}}}},_4t=function(_4u,_4v){return !B(_4o(_4u,_4v))?true:false;},_4w=[0,_4o,_4t],_4x=function(_4y,_4z){var _4A=E(_4y);switch(_4A[0]){case 0:return [0,function(_4B){return new F(function(){return _4x(B(A(_4A[1],[_4B])),_4z);});}];case 1:return [1,function(_4C){return new F(function(){return _4x(B(A(_4A[1],[_4C])),_4z);});}];case 2:return [2];case 3:return new F(function(){return _3F(B(A(_4z,[_4A[1]])),new T(function(){return B(_4x(_4A[2],_4z));}));});break;default:var _4D=function(_4E){var _4F=E(_4E);if(!_4F[0]){return [0];}else{var _4G=E(_4F[1]);return new F(function(){return _C(B(_3v(B(A(_4z,[_4G[1]])),_4G[2])),new T(function(){return B(_4D(_4F[2]));}));});}},_4H=B(_4D(_4A[1]));return _4H[0]==0?[2]:[4,_4H];}},_4I=[2],_4J=function(_4K){return [3,_4K,_4I];},_4L=0,_4M=function(_4N,_4O){var _4P=E(_4N);if(!_4P){return new F(function(){return A(_4O,[_4L]);});}else{return [0,function(_4Q){return E(new T(function(){return B(_4M(_4P-1|0,_4O));}));}];}},_4R=function(_4S,_4T,_4U){return function(_4V){return new F(function(){return A(function(_4W,_4X,_4Y){while(1){var _4Z=(function(_50,_51,_52){var _53=E(_50);switch(_53[0]){case 0:var _54=E(_51);if(!_54[0]){return E(_4T);}else{_4W=B(A(_53[1],[_54[1]]));_4X=_54[2];var _55=_52+1|0;_4Y=_55;return null;}break;case 1:var _56=B(A(_53[1],[_51])),_57=_51,_55=_52;_4W=_56;_4X=_57;_4Y=_55;return null;case 2:return E(_4T);case 3:return function(_58){return new F(function(){return _4M(_52,function(_59){return E(new T(function(){return B(_4x(_53,_58));}));});});};default:return function(_5a){return new F(function(){return _4x(_53,_5a);});};}})(_4W,_4X,_4Y);if(_4Z!=null){return _4Z;}}},[new T(function(){return B(A(_4S,[_4J]));}),_4V,0,_4U]);});};},_5b=function(_5c){return new F(function(){return A(_5c,[_i]);});},_5d=function(_5e,_5f){var _5g=function(_5h){var _5i=E(_5h);if(!_5i[0]){return E(_5b);}else{var _5j=_5i[1];return !B(A(_5e,[_5j]))?E(_5b):function(_5k){return [0,function(_5l){return E(new T(function(){return B(A(new T(function(){return B(_5g(_5i[2]));}),[function(_5m){return new F(function(){return A(_5k,[[1,_5j,_5m]]);});}]));}));}];};}};return function(_5n){return new F(function(){return A(_5g,[_5n,_5f]);});};},_5o=[6],_5p=function(_5q){return E(_5q);},_5r=new T(function(){return B(unCStr("valDig: Bad base"));}),_5s=new T(function(){return B(err(_5r));}),_5t=function(_5u,_5v){var _5w=function(_5x,_5y){var _5z=E(_5x);if(!_5z[0]){return function(_5A){return new F(function(){return A(_5A,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{var _5B=E(_5z[1])[1],_5C=function(_5D){return function(_5E){return [0,function(_5F){return E(new T(function(){return B(A(new T(function(){return B(_5w(_5z[2],function(_5G){return new F(function(){return A(_5y,[[1,_5D,_5G]]);});}));}),[_5E]));}));}];};};switch(E(E(_5u)[1])){case 8:if(48>_5B){return function(_5H){return new F(function(){return A(_5H,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{if(_5B>55){return function(_5I){return new F(function(){return A(_5I,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{return new F(function(){return _5C([0,_5B-48|0]);});}}break;case 10:if(48>_5B){return function(_5J){return new F(function(){return A(_5J,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{if(_5B>57){return function(_5K){return new F(function(){return A(_5K,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{return new F(function(){return _5C([0,_5B-48|0]);});}}break;case 16:if(48>_5B){if(97>_5B){if(65>_5B){return function(_5L){return new F(function(){return A(_5L,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{if(_5B>70){return function(_5M){return new F(function(){return A(_5M,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{return new F(function(){return _5C([0,(_5B-65|0)+10|0]);});}}}else{if(_5B>102){if(65>_5B){return function(_5N){return new F(function(){return A(_5N,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{if(_5B>70){return function(_5O){return new F(function(){return A(_5O,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{return new F(function(){return _5C([0,(_5B-65|0)+10|0]);});}}}else{return new F(function(){return _5C([0,(_5B-97|0)+10|0]);});}}}else{if(_5B>57){if(97>_5B){if(65>_5B){return function(_5P){return new F(function(){return A(_5P,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{if(_5B>70){return function(_5Q){return new F(function(){return A(_5Q,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{return new F(function(){return _5C([0,(_5B-65|0)+10|0]);});}}}else{if(_5B>102){if(65>_5B){return function(_5R){return new F(function(){return A(_5R,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{if(_5B>70){return function(_5S){return new F(function(){return A(_5S,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{return new F(function(){return _5C([0,(_5B-65|0)+10|0]);});}}}else{return new F(function(){return _5C([0,(_5B-97|0)+10|0]);});}}}else{return new F(function(){return _5C([0,_5B-48|0]);});}}break;default:return E(_5s);}}};return function(_5T){return new F(function(){return A(_5w,[_5T,_5p,function(_5U){var _5V=E(_5U);return _5V[0]==0?[2]:B(A(_5v,[_5V]));}]);});};},_5W=[0,10],_5X=[0,1],_5Y=[0,2147483647],_5Z=function(_60,_61){while(1){var _62=E(_60);if(!_62[0]){var _63=_62[1],_64=E(_61);if(!_64[0]){var _65=_64[1],_66=addC(_63,_65);if(!E(_66[2])){return [0,_66[1]];}else{_60=[1,I_fromInt(_63)];_61=[1,I_fromInt(_65)];continue;}}else{_60=[1,I_fromInt(_63)];_61=_64;continue;}}else{var _67=E(_61);if(!_67[0]){_60=_62;_61=[1,I_fromInt(_67[1])];continue;}else{return [1,I_add(_62[1],_67[1])];}}}},_68=new T(function(){return B(_5Z(_5Y,_5X));}),_69=function(_6a){var _6b=E(_6a);if(!_6b[0]){var _6c=E(_6b[1]);return _6c==(-2147483648)?E(_68):[0, -_6c];}else{return [1,I_negate(_6b[1])];}},_6d=[0,10],_6e=[0,0],_6f=function(_6g){return [0,_6g];},_6h=function(_6i,_6j){while(1){var _6k=E(_6i);if(!_6k[0]){var _6l=_6k[1],_6m=E(_6j);if(!_6m[0]){var _6n=_6m[1];if(!(imul(_6l,_6n)|0)){return [0,imul(_6l,_6n)|0];}else{_6i=[1,I_fromInt(_6l)];_6j=[1,I_fromInt(_6n)];continue;}}else{_6i=[1,I_fromInt(_6l)];_6j=_6m;continue;}}else{var _6o=E(_6j);if(!_6o[0]){_6i=_6k;_6j=[1,I_fromInt(_6o[1])];continue;}else{return [1,I_mul(_6k[1],_6o[1])];}}}},_6p=function(_6q,_6r,_6s){while(1){var _6t=E(_6s);if(!_6t[0]){return E(_6r);}else{var _6u=B(_5Z(B(_6h(_6r,_6q)),B(_6f(E(_6t[1])[1]))));_6s=_6t[2];_6r=_6u;continue;}}},_6v=function(_6w){var _6x=new T(function(){return B(_3F(B(_3F([0,function(_6y){return E(E(_6y)[1])==45?[1,B(_5t(_5W,function(_6z){return new F(function(){return A(_6w,[[1,new T(function(){return B(_69(B(_6p(_6d,_6e,_6z))));})]]);});}))]:[2];}],[0,function(_6A){return E(E(_6A)[1])==43?[1,B(_5t(_5W,function(_6B){return new F(function(){return A(_6w,[[1,new T(function(){return B(_6p(_6d,_6e,_6B));})]]);});}))]:[2];}])),new T(function(){return [1,B(_5t(_5W,function(_6C){return new F(function(){return A(_6w,[[1,new T(function(){return B(_6p(_6d,_6e,_6C));})]]);});}))];})));});return new F(function(){return _3F([0,function(_6D){return E(E(_6D)[1])==101?E(_6x):[2];}],[0,function(_6E){return E(E(_6E)[1])==69?E(_6x):[2];}]);});},_6F=[0],_6G=function(_6H){return new F(function(){return A(_6H,[_6F]);});},_6I=function(_6J){return new F(function(){return A(_6J,[_6F]);});},_6K=function(_6L){return function(_6M){return E(E(_6M)[1])==46?[1,B(_5t(_5W,function(_6N){return new F(function(){return A(_6L,[[1,_6N]]);});}))]:[2];};},_6O=function(_6P){return [0,B(_6K(_6P))];},_6Q=function(_6R){return new F(function(){return _5t(_5W,function(_6S){return [1,B(_4R(_6O,_6G,function(_6T){return [1,B(_4R(_6v,_6I,function(_6U){return new F(function(){return A(_6R,[[5,[1,_6S,_6T,_6U]]]);});}))];}))];});});},_6V=function(_6W){return [1,B(_6Q(_6W))];},_6X=function(_6Y){return E(E(_6Y)[1]);},_6Z=function(_70,_71,_72){while(1){var _73=E(_72);if(!_73[0]){return false;}else{if(!B(A(_6X,[_70,_71,_73[1]]))){_72=_73[2];continue;}else{return true;}}}},_74=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_75=function(_76){return new F(function(){return _6Z(_4n,_76,_74);});},_77=[0,8],_78=[0,16],_79=function(_7a){var _7b=function(_7c){return new F(function(){return A(_7a,[[5,[0,_77,_7c]]]);});},_7d=function(_7e){return new F(function(){return A(_7a,[[5,[0,_78,_7e]]]);});};return function(_7f){return E(E(_7f)[1])==48?E([0,function(_7g){switch(E(E(_7g)[1])){case 79:return [1,B(_5t(_77,_7b))];case 88:return [1,B(_5t(_78,_7d))];case 111:return [1,B(_5t(_77,_7b))];case 120:return [1,B(_5t(_78,_7d))];default:return [2];}}]):[2];};},_7h=function(_7i){return [0,B(_79(_7i))];},_7j=function(_7k){var _7l=new T(function(){return B(A(_7k,[_77]));}),_7m=new T(function(){return B(A(_7k,[_78]));});return function(_7n){switch(E(E(_7n)[1])){case 79:return E(_7l);case 88:return E(_7m);case 111:return E(_7l);case 120:return E(_7m);default:return [2];}};},_7o=function(_7p){return [0,B(_7j(_7p))];},_7q=[0,92],_7r=function(_7s){return new F(function(){return A(_7s,[_5W]);});},_7t=function(_7u,_7v){var _7w=jsShowI(_7u),_7x=_7w;return new F(function(){return _C(fromJSStr(_7x),_7v);});},_7y=function(_7z,_7A,_7B){if(_7A>=0){return new F(function(){return _7t(_7A,_7B);});}else{return _7z<=6?B(_7t(_7A,_7B)):[1,_V,new T(function(){var _7C=jsShowI(_7A),_7D=_7C;return B(_C(fromJSStr(_7D),[1,_U,_7B]));})];}},_7E=function(_7F){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_7y(9,_7F,_i));}))));});},_7G=function(_7H){var _7I=E(_7H);return _7I[0]==0?E(_7I[1]):I_toInt(_7I[1]);},_7J=function(_7K,_7L){var _7M=E(_7K);if(!_7M[0]){var _7N=_7M[1],_7O=E(_7L);return _7O[0]==0?_7N<=_7O[1]:I_compareInt(_7O[1],_7N)>=0;}else{var _7P=_7M[1],_7Q=E(_7L);return _7Q[0]==0?I_compareInt(_7P,_7Q[1])<=0:I_compare(_7P,_7Q[1])<=0;}},_7R=function(_7S){return [2];},_7T=function(_7U){var _7V=E(_7U);if(!_7V[0]){return E(_7R);}else{var _7W=_7V[1],_7X=E(_7V[2]);return _7X[0]==0?E(_7W):function(_7Y){return new F(function(){return _3F(B(A(_7W,[_7Y])),new T(function(){return B(A(new T(function(){return B(_7T(_7X));}),[_7Y]));}));});};}},_7Z=function(_80){return [2];},_81=function(_82,_83){var _84=function(_85,_86){var _87=E(_85);if(!_87[0]){return function(_88){return new F(function(){return A(_88,[_82]);});};}else{var _89=E(_86);return _89[0]==0?E(_7Z):E(_87[1])[1]!=E(_89[1])[1]?E(_7Z):function(_8a){return [0,function(_8b){return E(new T(function(){return B(A(new T(function(){return B(_84(_87[2],_89[2]));}),[_8a]));}));}];};}};return function(_8c){return new F(function(){return A(_84,[_82,_8c,_83]);});};},_8d=new T(function(){return B(unCStr("SOH"));}),_8e=[0,1],_8f=function(_8g){return [1,B(_81(_8d,function(_8h){return E(new T(function(){return B(A(_8g,[_8e]));}));}))];},_8i=new T(function(){return B(unCStr("SO"));}),_8j=[0,14],_8k=function(_8l){return [1,B(_81(_8i,function(_8m){return E(new T(function(){return B(A(_8l,[_8j]));}));}))];},_8n=function(_8o){return [1,B(_4R(_8f,_8k,_8o))];},_8p=new T(function(){return B(unCStr("NUL"));}),_8q=[0,0],_8r=function(_8s){return [1,B(_81(_8p,function(_8t){return E(new T(function(){return B(A(_8s,[_8q]));}));}))];},_8u=new T(function(){return B(unCStr("STX"));}),_8v=[0,2],_8w=function(_8x){return [1,B(_81(_8u,function(_8y){return E(new T(function(){return B(A(_8x,[_8v]));}));}))];},_8z=new T(function(){return B(unCStr("ETX"));}),_8A=[0,3],_8B=function(_8C){return [1,B(_81(_8z,function(_8D){return E(new T(function(){return B(A(_8C,[_8A]));}));}))];},_8E=new T(function(){return B(unCStr("EOT"));}),_8F=[0,4],_8G=function(_8H){return [1,B(_81(_8E,function(_8I){return E(new T(function(){return B(A(_8H,[_8F]));}));}))];},_8J=new T(function(){return B(unCStr("ENQ"));}),_8K=[0,5],_8L=function(_8M){return [1,B(_81(_8J,function(_8N){return E(new T(function(){return B(A(_8M,[_8K]));}));}))];},_8O=new T(function(){return B(unCStr("ACK"));}),_8P=[0,6],_8Q=function(_8R){return [1,B(_81(_8O,function(_8S){return E(new T(function(){return B(A(_8R,[_8P]));}));}))];},_8T=new T(function(){return B(unCStr("BEL"));}),_8U=[0,7],_8V=function(_8W){return [1,B(_81(_8T,function(_8X){return E(new T(function(){return B(A(_8W,[_8U]));}));}))];},_8Y=new T(function(){return B(unCStr("BS"));}),_8Z=[0,8],_90=function(_91){return [1,B(_81(_8Y,function(_92){return E(new T(function(){return B(A(_91,[_8Z]));}));}))];},_93=new T(function(){return B(unCStr("HT"));}),_94=[0,9],_95=function(_96){return [1,B(_81(_93,function(_97){return E(new T(function(){return B(A(_96,[_94]));}));}))];},_98=new T(function(){return B(unCStr("LF"));}),_99=[0,10],_9a=function(_9b){return [1,B(_81(_98,function(_9c){return E(new T(function(){return B(A(_9b,[_99]));}));}))];},_9d=new T(function(){return B(unCStr("VT"));}),_9e=[0,11],_9f=function(_9g){return [1,B(_81(_9d,function(_9h){return E(new T(function(){return B(A(_9g,[_9e]));}));}))];},_9i=new T(function(){return B(unCStr("FF"));}),_9j=[0,12],_9k=function(_9l){return [1,B(_81(_9i,function(_9m){return E(new T(function(){return B(A(_9l,[_9j]));}));}))];},_9n=new T(function(){return B(unCStr("CR"));}),_9o=[0,13],_9p=function(_9q){return [1,B(_81(_9n,function(_9r){return E(new T(function(){return B(A(_9q,[_9o]));}));}))];},_9s=new T(function(){return B(unCStr("SI"));}),_9t=[0,15],_9u=function(_9v){return [1,B(_81(_9s,function(_9w){return E(new T(function(){return B(A(_9v,[_9t]));}));}))];},_9x=new T(function(){return B(unCStr("DLE"));}),_9y=[0,16],_9z=function(_9A){return [1,B(_81(_9x,function(_9B){return E(new T(function(){return B(A(_9A,[_9y]));}));}))];},_9C=new T(function(){return B(unCStr("DC1"));}),_9D=[0,17],_9E=function(_9F){return [1,B(_81(_9C,function(_9G){return E(new T(function(){return B(A(_9F,[_9D]));}));}))];},_9H=new T(function(){return B(unCStr("DC2"));}),_9I=[0,18],_9J=function(_9K){return [1,B(_81(_9H,function(_9L){return E(new T(function(){return B(A(_9K,[_9I]));}));}))];},_9M=new T(function(){return B(unCStr("DC3"));}),_9N=[0,19],_9O=function(_9P){return [1,B(_81(_9M,function(_9Q){return E(new T(function(){return B(A(_9P,[_9N]));}));}))];},_9R=new T(function(){return B(unCStr("DC4"));}),_9S=[0,20],_9T=function(_9U){return [1,B(_81(_9R,function(_9V){return E(new T(function(){return B(A(_9U,[_9S]));}));}))];},_9W=new T(function(){return B(unCStr("NAK"));}),_9X=[0,21],_9Y=function(_9Z){return [1,B(_81(_9W,function(_a0){return E(new T(function(){return B(A(_9Z,[_9X]));}));}))];},_a1=new T(function(){return B(unCStr("SYN"));}),_a2=[0,22],_a3=function(_a4){return [1,B(_81(_a1,function(_a5){return E(new T(function(){return B(A(_a4,[_a2]));}));}))];},_a6=new T(function(){return B(unCStr("ETB"));}),_a7=[0,23],_a8=function(_a9){return [1,B(_81(_a6,function(_aa){return E(new T(function(){return B(A(_a9,[_a7]));}));}))];},_ab=new T(function(){return B(unCStr("CAN"));}),_ac=[0,24],_ad=function(_ae){return [1,B(_81(_ab,function(_af){return E(new T(function(){return B(A(_ae,[_ac]));}));}))];},_ag=new T(function(){return B(unCStr("EM"));}),_ah=[0,25],_ai=function(_aj){return [1,B(_81(_ag,function(_ak){return E(new T(function(){return B(A(_aj,[_ah]));}));}))];},_al=new T(function(){return B(unCStr("SUB"));}),_am=[0,26],_an=function(_ao){return [1,B(_81(_al,function(_ap){return E(new T(function(){return B(A(_ao,[_am]));}));}))];},_aq=new T(function(){return B(unCStr("ESC"));}),_ar=[0,27],_as=function(_at){return [1,B(_81(_aq,function(_au){return E(new T(function(){return B(A(_at,[_ar]));}));}))];},_av=new T(function(){return B(unCStr("FS"));}),_aw=[0,28],_ax=function(_ay){return [1,B(_81(_av,function(_az){return E(new T(function(){return B(A(_ay,[_aw]));}));}))];},_aA=new T(function(){return B(unCStr("GS"));}),_aB=[0,29],_aC=function(_aD){return [1,B(_81(_aA,function(_aE){return E(new T(function(){return B(A(_aD,[_aB]));}));}))];},_aF=new T(function(){return B(unCStr("RS"));}),_aG=[0,30],_aH=function(_aI){return [1,B(_81(_aF,function(_aJ){return E(new T(function(){return B(A(_aI,[_aG]));}));}))];},_aK=new T(function(){return B(unCStr("US"));}),_aL=[0,31],_aM=function(_aN){return [1,B(_81(_aK,function(_aO){return E(new T(function(){return B(A(_aN,[_aL]));}));}))];},_aP=new T(function(){return B(unCStr("SP"));}),_aQ=[0,32],_aR=function(_aS){return [1,B(_81(_aP,function(_aT){return E(new T(function(){return B(A(_aS,[_aQ]));}));}))];},_aU=new T(function(){return B(unCStr("DEL"));}),_aV=[0,127],_aW=function(_aX){return [1,B(_81(_aU,function(_aY){return E(new T(function(){return B(A(_aX,[_aV]));}));}))];},_aZ=[1,_aW,_i],_b0=[1,_aR,_aZ],_b1=[1,_aM,_b0],_b2=[1,_aH,_b1],_b3=[1,_aC,_b2],_b4=[1,_ax,_b3],_b5=[1,_as,_b4],_b6=[1,_an,_b5],_b7=[1,_ai,_b6],_b8=[1,_ad,_b7],_b9=[1,_a8,_b8],_ba=[1,_a3,_b9],_bb=[1,_9Y,_ba],_bc=[1,_9T,_bb],_bd=[1,_9O,_bc],_be=[1,_9J,_bd],_bf=[1,_9E,_be],_bg=[1,_9z,_bf],_bh=[1,_9u,_bg],_bi=[1,_9p,_bh],_bj=[1,_9k,_bi],_bk=[1,_9f,_bj],_bl=[1,_9a,_bk],_bm=[1,_95,_bl],_bn=[1,_90,_bm],_bo=[1,_8V,_bn],_bp=[1,_8Q,_bo],_bq=[1,_8L,_bp],_br=[1,_8G,_bq],_bs=[1,_8B,_br],_bt=[1,_8w,_bs],_bu=[1,_8r,_bt],_bv=[1,_8n,_bu],_bw=new T(function(){return B(_7T(_bv));}),_bx=[0,1114111],_by=[0,34],_bz=[0,39],_bA=function(_bB){var _bC=new T(function(){return B(A(_bB,[_8U]));}),_bD=new T(function(){return B(A(_bB,[_8Z]));}),_bE=new T(function(){return B(A(_bB,[_94]));}),_bF=new T(function(){return B(A(_bB,[_99]));}),_bG=new T(function(){return B(A(_bB,[_9e]));}),_bH=new T(function(){return B(A(_bB,[_9j]));}),_bI=new T(function(){return B(A(_bB,[_9o]));});return new F(function(){return _3F([0,function(_bJ){switch(E(E(_bJ)[1])){case 34:return E(new T(function(){return B(A(_bB,[_by]));}));case 39:return E(new T(function(){return B(A(_bB,[_bz]));}));case 92:return E(new T(function(){return B(A(_bB,[_7q]));}));case 97:return E(_bC);case 98:return E(_bD);case 102:return E(_bH);case 110:return E(_bF);case 114:return E(_bI);case 116:return E(_bE);case 118:return E(_bG);default:return [2];}}],new T(function(){return B(_3F([1,B(_4R(_7o,_7r,function(_bK){return [1,B(_5t(_bK,function(_bL){var _bM=B(_6p(new T(function(){return B(_6f(E(_bK)[1]));}),_6e,_bL));return !B(_7J(_bM,_bx))?[2]:B(A(_bB,[new T(function(){var _bN=B(_7G(_bM));if(_bN>>>0>1114111){var _bO=B(_7E(_bN));}else{var _bO=[0,_bN];}var _bP=_bO,_bQ=_bP,_bR=_bQ;return _bR;})]));}))];}))],new T(function(){return B(_3F([0,function(_bS){return E(E(_bS)[1])==94?E([0,function(_bT){switch(E(E(_bT)[1])){case 64:return E(new T(function(){return B(A(_bB,[_8q]));}));case 65:return E(new T(function(){return B(A(_bB,[_8e]));}));case 66:return E(new T(function(){return B(A(_bB,[_8v]));}));case 67:return E(new T(function(){return B(A(_bB,[_8A]));}));case 68:return E(new T(function(){return B(A(_bB,[_8F]));}));case 69:return E(new T(function(){return B(A(_bB,[_8K]));}));case 70:return E(new T(function(){return B(A(_bB,[_8P]));}));case 71:return E(_bC);case 72:return E(_bD);case 73:return E(_bE);case 74:return E(_bF);case 75:return E(_bG);case 76:return E(_bH);case 77:return E(_bI);case 78:return E(new T(function(){return B(A(_bB,[_8j]));}));case 79:return E(new T(function(){return B(A(_bB,[_9t]));}));case 80:return E(new T(function(){return B(A(_bB,[_9y]));}));case 81:return E(new T(function(){return B(A(_bB,[_9D]));}));case 82:return E(new T(function(){return B(A(_bB,[_9I]));}));case 83:return E(new T(function(){return B(A(_bB,[_9N]));}));case 84:return E(new T(function(){return B(A(_bB,[_9S]));}));case 85:return E(new T(function(){return B(A(_bB,[_9X]));}));case 86:return E(new T(function(){return B(A(_bB,[_a2]));}));case 87:return E(new T(function(){return B(A(_bB,[_a7]));}));case 88:return E(new T(function(){return B(A(_bB,[_ac]));}));case 89:return E(new T(function(){return B(A(_bB,[_ah]));}));case 90:return E(new T(function(){return B(A(_bB,[_am]));}));case 91:return E(new T(function(){return B(A(_bB,[_ar]));}));case 92:return E(new T(function(){return B(A(_bB,[_aw]));}));case 93:return E(new T(function(){return B(A(_bB,[_aB]));}));case 94:return E(new T(function(){return B(A(_bB,[_aG]));}));case 95:return E(new T(function(){return B(A(_bB,[_aL]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_bw,[_bB]));})));})));}));});},_bU=function(_bV){return new F(function(){return A(_bV,[_4L]);});},_bW=function(_bX){var _bY=E(_bX);if(!_bY[0]){return E(_bU);}else{var _bZ=_bY[2],_c0=E(E(_bY[1])[1]);switch(_c0){case 9:return function(_c1){return [0,function(_c2){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_c1]));}));}];};case 10:return function(_c3){return [0,function(_c4){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_c3]));}));}];};case 11:return function(_c5){return [0,function(_c6){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_c5]));}));}];};case 12:return function(_c7){return [0,function(_c8){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_c7]));}));}];};case 13:return function(_c9){return [0,function(_ca){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_c9]));}));}];};case 32:return function(_cb){return [0,function(_cc){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_cb]));}));}];};case 160:return function(_cd){return [0,function(_ce){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_cd]));}));}];};default:var _cf=u_iswspace(_c0),_cg=_cf;return E(_cg)==0?E(_bU):function(_ch){return [0,function(_ci){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_ch]));}));}];};}}},_cj=function(_ck){var _cl=new T(function(){return B(_cj(_ck));}),_cm=[1,function(_cn){return new F(function(){return A(_bW,[_cn,function(_co){return E([0,function(_cp){return E(E(_cp)[1])==92?E(_cl):[2];}]);}]);});}];return new F(function(){return _3F([0,function(_cq){return E(E(_cq)[1])==92?E([0,function(_cr){var _cs=E(E(_cr)[1]);switch(_cs){case 9:return E(_cm);case 10:return E(_cm);case 11:return E(_cm);case 12:return E(_cm);case 13:return E(_cm);case 32:return E(_cm);case 38:return E(_cl);case 160:return E(_cm);default:var _ct=u_iswspace(_cs),_cu=_ct;return E(_cu)==0?[2]:E(_cm);}}]):[2];}],[0,function(_cv){var _cw=E(_cv);return E(_cw[1])==92?E(new T(function(){return B(_bA(function(_cx){return new F(function(){return A(_ck,[[0,_cx,_b]]);});}));})):B(A(_ck,[[0,_cw,_f]]));}]);});},_cy=function(_cz,_cA){return new F(function(){return _cj(function(_cB){var _cC=E(_cB),_cD=E(_cC[1]);if(E(_cD[1])==34){if(!E(_cC[2])){return E(new T(function(){return B(A(_cA,[[1,new T(function(){return B(A(_cz,[_i]));})]]));}));}else{return new F(function(){return _cy(function(_cE){return new F(function(){return A(_cz,[[1,_cD,_cE]]);});},_cA);});}}else{return new F(function(){return _cy(function(_cF){return new F(function(){return A(_cz,[[1,_cD,_cF]]);});},_cA);});}});});},_cG=new T(function(){return B(unCStr("_\'"));}),_cH=function(_cI){var _cJ=u_iswalnum(_cI),_cK=_cJ;return E(_cK)==0?B(_6Z(_4n,[0,_cI],_cG)):true;},_cL=function(_cM){return new F(function(){return _cH(E(_cM)[1]);});},_cN=new T(function(){return B(unCStr(",;()[]{}`"));}),_cO=new T(function(){return B(unCStr(".."));}),_cP=new T(function(){return B(unCStr("::"));}),_cQ=new T(function(){return B(unCStr("->"));}),_cR=[0,64],_cS=[1,_cR,_i],_cT=[0,126],_cU=[1,_cT,_i],_cV=new T(function(){return B(unCStr("=>"));}),_cW=[1,_cV,_i],_cX=[1,_cU,_cW],_cY=[1,_cS,_cX],_cZ=[1,_cQ,_cY],_d0=new T(function(){return B(unCStr("<-"));}),_d1=[1,_d0,_cZ],_d2=[0,124],_d3=[1,_d2,_i],_d4=[1,_d3,_d1],_d5=[1,_7q,_i],_d6=[1,_d5,_d4],_d7=[0,61],_d8=[1,_d7,_i],_d9=[1,_d8,_d6],_da=[1,_cP,_d9],_db=[1,_cO,_da],_dc=function(_dd){return new F(function(){return _3F([1,function(_de){return E(_de)[0]==0?E(new T(function(){return B(A(_dd,[_5o]));})):[2];}],new T(function(){return B(_3F([0,function(_df){return E(E(_df)[1])==39?E([0,function(_dg){var _dh=E(_dg);switch(E(_dh[1])){case 39:return [2];case 92:return E(new T(function(){return B(_bA(function(_di){return [0,function(_dj){return E(E(_dj)[1])==39?E(new T(function(){return B(A(_dd,[[0,_di]]));})):[2];}];}));}));default:return [0,function(_dk){return E(E(_dk)[1])==39?E(new T(function(){return B(A(_dd,[[0,_dh]]));})):[2];}];}}]):[2];}],new T(function(){return B(_3F([0,function(_dl){return E(E(_dl)[1])==34?E(new T(function(){return B(_cy(_5p,_dd));})):[2];}],new T(function(){return B(_3F([0,function(_dm){return !B(_6Z(_4n,_dm,_cN))?[2]:B(A(_dd,[[2,[1,_dm,_i]]]));}],new T(function(){return B(_3F([0,function(_dn){return !B(_6Z(_4n,_dn,_74))?[2]:[1,B(_5d(_75,function(_do){var _dp=[1,_dn,_do];return !B(_6Z(_4w,_dp,_db))?B(A(_dd,[[4,_dp]])):B(A(_dd,[[2,_dp]]));}))];}],new T(function(){return B(_3F([0,function(_dq){var _dr=E(_dq),_ds=_dr[1],_dt=u_iswalpha(_ds),_du=_dt;return E(_du)==0?E(_ds)==95?[1,B(_5d(_cL,function(_dv){return new F(function(){return A(_dd,[[3,[1,_dr,_dv]]]);});}))]:[2]:[1,B(_5d(_cL,function(_dw){return new F(function(){return A(_dd,[[3,[1,_dr,_dw]]]);});}))];}],new T(function(){return [1,B(_4R(_7h,_6V,_dd))];})));})));})));})));})));}));});},_dx=[0,0],_dy=function(_dz,_dA){return function(_dB){return new F(function(){return A(_bW,[_dB,function(_dC){return E(new T(function(){return B(_dc(function(_dD){var _dE=E(_dD);return _dE[0]==2?!B(_4c(_dE[1],_4b))?[2]:E(new T(function(){return B(A(_dz,[_dx,function(_dF){return [1,function(_dG){return new F(function(){return A(_bW,[_dG,function(_dH){return E(new T(function(){return B(_dc(function(_dI){var _dJ=E(_dI);return _dJ[0]==2?!B(_4c(_dJ[1],_49))?[2]:E(new T(function(){return B(A(_dA,[_dF]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_dK=function(_dL,_dM,_dN){var _dO=function(_dP,_dQ){return new F(function(){return _3F([1,function(_dR){return new F(function(){return A(_bW,[_dR,function(_dS){return E(new T(function(){return B(_dc(function(_dT){var _dU=E(_dT);if(_dU[0]==4){var _dV=E(_dU[1]);if(!_dV[0]){return new F(function(){return A(_dL,[_dU,_dP,_dQ]);});}else{return E(E(_dV[1])[1])==45?E(_dV[2])[0]==0?E([1,function(_dW){return new F(function(){return A(_bW,[_dW,function(_dX){return E(new T(function(){return B(_dc(function(_dY){return new F(function(){return A(_dL,[_dY,_dP,function(_dZ){return new F(function(){return A(_dQ,[new T(function(){return B(_69(_dZ));})]);});}]);});}));}));}]);});}]):B(A(_dL,[_dU,_dP,_dQ])):B(A(_dL,[_dU,_dP,_dQ]));}}else{return new F(function(){return A(_dL,[_dU,_dP,_dQ]);});}}));}));}]);});}],new T(function(){return [1,B(_dy(_dO,_dQ))];}));});};return new F(function(){return _dO(_dM,_dN);});},_e0=function(_e1,_e2){return [2];},_e3=function(_e4){var _e5=E(_e4);return _e5[0]==0?[1,new T(function(){return B(_6p(new T(function(){return B(_6f(E(_e5[1])[1]));}),_6e,_e5[2]));})]:E(_e5[2])[0]==0?E(_e5[3])[0]==0?[1,new T(function(){return B(_6p(_6d,_6e,_e5[1]));})]:[0]:[0];},_e6=function(_e7){var _e8=E(_e7);if(_e8[0]==5){var _e9=B(_e3(_e8[1]));return _e9[0]==0?E(_e0):function(_ea,_eb){return new F(function(){return A(_eb,[_e9[1]]);});};}else{return E(_e0);}},_ec=function(_ed){return [1,function(_ee){return new F(function(){return A(_bW,[_ee,function(_ef){return E([3,_ed,_4I]);}]);});}];},_eg=new T(function(){return B(_dK(_e6,_dx,_ec));}),_eh=new T(function(){return B(unCStr("Tried to deserialize long string to a Char"));}),_ei=[0,_eh],_ej=new T(function(){return B(unCStr("Tried to deserialize a non-string to a Char"));}),_ek=[0,_ej],_el=function(_em){var _en=E(_em);if(_en[0]==1){var _eo=fromJSStr(E(_en[1])[1]);return _eo[0]==0?E(_ei):E(_eo[2])[0]==0?[1,_eo[1]]:E(_ei);}else{return E(_ek);}},_ep=[0,_20],_eq=function(_er){return new F(function(){return fromJSStr(E(_er)[1]);});},_es=function(_et){var _eu=E(_et);return _eu[0]==1?[1,new T(function(){return B(_eq(_eu[1]));})]:E(_ep);},_ev=function(_ew){return [1,new T(function(){return [0,toJSStr([1,_ew,_i])];})];},_ex=[0,_ev,_8,_el,_es],_ey=function(_ez){return E(E(_ez)[2]);},_eA=function(_eB,_eC){return [3,new T(function(){return B(_1j(new T(function(){return B(_ey(_eB));}),_eC));})];},_eD=[1,_i],_eE=[0,_1B],_eF=function(_eG){return E(E(_eG)[4]);},_eH=function(_eI,_eJ){var _eK=E(_eJ);if(_eK[0]==3){var _eL=function(_eM){var _eN=E(_eM);if(!_eN[0]){return E(_eD);}else{var _eO=B(A(new T(function(){return B(_eF(_eI));}),[_eN[1]]));if(!_eO[0]){return [0,_eO[1]];}else{var _eP=B(_eL(_eN[2]));return _eP[0]==0?[0,_eP[1]]:[1,[1,_eO[1],_eP[1]]];}}};return new F(function(){return _eL(_eK[1]);});}else{return E(_eE);}},_eQ=function(_eR){return [0,new T(function(){return B(_ey(_eR));}),function(_eS){return new F(function(){return _eA(_eR,_eS);});},new T(function(){return B(_eF(_eR));}),function(_eS){return new F(function(){return _eH(_eR,_eS);});}];},_eT=new T(function(){return B(_eQ(_ex));}),_eU=function(_eV){return E(E(_eV)[1]);},_eW=function(_eX,_eY){var _eZ=E(_eY);return _eZ[0]==0?E(_k):[4,[1,_e,[1,[0,_a,new T(function(){return B(A(_eU,[_eX,_eZ[1]]));})],_i]]];},_f0=function(_f1,_f2){return [3,new T(function(){return B(_1j(function(_eS){return new F(function(){return _eW(_f1,_eS);});},_f2));})];},_f3=[1,_6F],_f4=new T(function(){return B(unCStr("Tried to deserialize a non-Bool to a Bool"));}),_f5=[0,_f4],_f6=[0,_24],_f7=[0,_26],_f8=function(_f9,_fa,_fb){while(1){var _fc=E(_fb);if(!_fc[0]){return [0];}else{var _fd=E(_fc[1]);if(!B(A(_6X,[_f9,_fa,_fd[1]]))){_fb=_fc[2];continue;}else{return [1,_fd[2]];}}}},_fe=function(_ff,_fg){var _fh=E(_fg);if(_fh[0]==4){var _fi=_fh[1],_fj=B(_f8(_1z,_d,_fi));if(!_fj[0]){return E(_f6);}else{var _fk=E(_fj[1]);if(_fk[0]==2){if(!E(_fk[1])){return E(_f3);}else{var _fl=B(_f8(_1z,_a,_fi));if(!_fl[0]){return E(_f6);}else{var _fm=B(A(_1F,[_ff,_fl[1]]));return _fm[0]==0?[0,_fm[1]]:[1,[1,_fm[1]]];}}}else{return E(_f5);}}}else{return E(_f7);}},_fn=[1,_i],_fo=[0,_1B],_fp=function(_fq,_fr){var _fs=E(_fr);if(_fs[0]==3){var _ft=function(_fu){var _fv=E(_fu);if(!_fv[0]){return E(_fn);}else{var _fw=B(_fe(_fq,_fv[1]));if(!_fw[0]){return [0,_fw[1]];}else{var _fx=B(_ft(_fv[2]));return _fx[0]==0?[0,_fx[1]]:[1,[1,_fw[1],_fx[1]]];}}};return new F(function(){return _ft(_fs[1]);});}else{return E(_fo);}},_fy=function(_fz){return [0,function(_eS){return new F(function(){return _eW(_fz,_eS);});},function(_eS){return new F(function(){return _f0(_fz,_eS);});},function(_eS){return new F(function(){return _fe(_fz,_eS);});},function(_eS){return new F(function(){return _fp(_fz,_eS);});}];},_fA=new T(function(){return B(_fy(_eT));}),_fB=function(_fC){return [3,new T(function(){return B(_1j(_t,_fC));})];},_fD=new T(function(){return B(unCStr("The given Number can\'t be represented as an Int"));}),_fE=[0,_fD],_fF=new T(function(){return B(unCStr("Tried to deserialize a non-Number to an Int"));}),_fG=[0,_fF],_fH=function(_fI){var _fJ=E(_fI);if(!_fJ[0]){var _fK=E(_fJ[1])[1],_fL=_fK&4294967295;return _fL!=_fK?E(_fE):[1,[0,_fL]];}else{return E(_fG);}},_fM=[0,_1B],_fN=[1,_i],_fO=[0,_fD],_fP=[0,_fF],_fQ=function(_fR){var _fS=E(_fR);if(!_fS[0]){return E(_fN);}else{var _fT=E(_fS[1]);if(!_fT[0]){var _fU=E(_fT[1])[1],_fV=_fU&4294967295;if(_fV!=_fU){return E(_fO);}else{var _fW=B(_fQ(_fS[2]));return _fW[0]==0?[0,_fW[1]]:[1,[1,[0,_fV],_fW[1]]];}}else{return E(_fP);}}},_fX=function(_fY){var _fZ=E(_fY);return _fZ[0]==3?B(_fQ(_fZ[1])):E(_fM);},_g0=[0,_t,_fB,_fH,_fX],_g1=[2],_g2=function(_g3,_g4,_g5){var _g6=E(_g5);switch(_g6[0]){case 0:var _g7=_g6[1],_g8=_g6[2],_g9=_g6[3],_ga=_g6[4],_gb=_g8>>>0;if(((_g3>>>0&((_gb-1>>>0^4294967295)>>>0^_gb)>>>0)>>>0&4294967295)==_g7){return (_g3>>>0&_gb)>>>0==0?[0,_g7,_g8,E(B(_g2(_g3,_g4,_g9))),E(_ga)]:[0,_g7,_g8,E(_g9),E(B(_g2(_g3,_g4,_ga)))];}else{var _gc=(_g3>>>0^_g7>>>0)>>>0,_gd=(_gc|_gc>>>1)>>>0,_ge=(_gd|_gd>>>2)>>>0,_gf=(_ge|_ge>>>4)>>>0,_gg=(_gf|_gf>>>8)>>>0,_gh=(_gg|_gg>>>16)>>>0,_gi=(_gh^_gh>>>1)>>>0&4294967295,_gj=_gi>>>0;return (_g3>>>0&_gj)>>>0==0?[0,(_g3>>>0&((_gj-1>>>0^4294967295)>>>0^_gj)>>>0)>>>0&4294967295,_gi,E([1,_g3,_g4]),E(_g6)]:[0,(_g3>>>0&((_gj-1>>>0^4294967295)>>>0^_gj)>>>0)>>>0&4294967295,_gi,E(_g6),E([1,_g3,_g4])];}break;case 1:var _gk=_g6[1];if(_g3!=_gk){var _gl=(_g3>>>0^_gk>>>0)>>>0,_gm=(_gl|_gl>>>1)>>>0,_gn=(_gm|_gm>>>2)>>>0,_go=(_gn|_gn>>>4)>>>0,_gp=(_go|_go>>>8)>>>0,_gq=(_gp|_gp>>>16)>>>0,_gr=(_gq^_gq>>>1)>>>0&4294967295,_gs=_gr>>>0;return (_g3>>>0&_gs)>>>0==0?[0,(_g3>>>0&((_gs-1>>>0^4294967295)>>>0^_gs)>>>0)>>>0&4294967295,_gr,E([1,_g3,_g4]),E(_g6)]:[0,(_g3>>>0&((_gs-1>>>0^4294967295)>>>0^_gs)>>>0)>>>0&4294967295,_gr,E(_g6),E([1,_g3,_g4])];}else{return [1,_g3,_g4];}break;default:return [1,_g3,_g4];}},_gt=function(_gu,_gv){while(1){var _gw=E(_gv);if(!_gw[0]){return E(_gu);}else{var _gx=E(_gw[1]),_gy=B(_g2(E(_gx[1])[1],_gx[2],_gu));_gv=_gw[2];_gu=_gy;continue;}}},_gz=function(_gA){return new F(function(){return _gt(_g1,_gA);});},_gB=function(_gC){while(1){var _gD=(function(_gE){var _gF=E(_gE);if(!_gF[0]){return [0];}else{var _gG=_gF[2],_gH=E(_gF[1]);if(!E(_gH[2])[0]){return [1,_gH[1],new T(function(){return B(_gB(_gG));})];}else{_gC=_gG;return null;}}})(_gC);if(_gD!=null){return _gD;}}},_gI=function(_gJ){var _gK=E(_gJ);if(_gK[0]==4){var _gL=_gK[1],_gM=B(_f8(_1z,_4,_gL));if(!_gM[0]){return E(_25);}else{var _gN=E(_gM[1]);if(!_gN[0]){var _gO=B(_f8(_1z,_3,_gL));if(!_gO[0]){return E(_25);}else{var _gP=E(_gO[1]);if(!_gP[0]){var _gQ=B(_f8(_1z,_2,_gL));if(!_gQ[0]){return E(_25);}else{var _gR=E(_gQ[1]);if(!_gR[0]){var _gS=B(_f8(_1z,_1,_gL));if(!_gS[0]){return E(_25);}else{var _gT=E(_gS[1]);if(_gT[0]==1){var _gU=B(_f8(_1z,_0,_gL));if(!_gU[0]){return E(_25);}else{var _gV=B(_1Q(_g0,_fA,_gU[1]));if(!_gV[0]){return [0,_gV[1]];}else{var _gW=B(_f8(_1z,_5,_gL));if(!_gW[0]){return E(_25);}else{var _gX=B(_1Q(_g0,_g0,_gW[1]));return _gX[0]==0?[0,_gX[1]]:[1,[0,_gN[1],_gP[1],_gR[1],new T(function(){var _gY=B(_gB(B(_3v(_eg,new T(function(){return fromJSStr(E(_gT[1])[1]);})))));return _gY[0]==0?E(_2c):E(_gY[2])[0]==0?E(_gY[1]):E(_2a);}),_28,_f,new T(function(){return B(_gz(_gV[1]));}),new T(function(){return B(_gz(_gX[1]));})]];}}}}else{return E(_21);}}}else{return E(_23);}}}else{return E(_23);}}}else{return E(_23);}}}else{return E(_27);}},_gZ=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_h0=[0,_gZ],_h1=[1,_i],_h2=function(_h3){var _h4=E(_h3);if(!_h4[0]){return E(_h1);}else{var _h5=B(_gI(_h4[1]));if(!_h5[0]){return [0,_h5[1]];}else{var _h6=B(_h2(_h4[2]));return _h6[0]==0?[0,_h6[1]]:[1,[1,_h5[1],_h6[1]]];}}},_h7=function(_h8){var _h9=E(_h8);return _h9[0]==3?B(_h2(_h9[1])):E(_h0);},_ha=[0,_1g,_1n,_gI,_h7],_hb=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_hc=new T(function(){return B(err(_hb));}),_hd=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_he=new T(function(){return B(err(_hd));}),_hf=function(_hg,_hh){while(1){var _hi=E(_hg);if(!_hi[0]){return E(_he);}else{var _hj=E(_hh);if(!_hj){return E(_hi[1]);}else{_hg=_hi[2];_hh=_hj-1|0;continue;}}}},_hk=new T(function(){return B(unCStr("ACK"));}),_hl=new T(function(){return B(unCStr("BEL"));}),_hm=new T(function(){return B(unCStr("BS"));}),_hn=new T(function(){return B(unCStr("SP"));}),_ho=[1,_hn,_i],_hp=new T(function(){return B(unCStr("US"));}),_hq=[1,_hp,_ho],_hr=new T(function(){return B(unCStr("RS"));}),_hs=[1,_hr,_hq],_ht=new T(function(){return B(unCStr("GS"));}),_hu=[1,_ht,_hs],_hv=new T(function(){return B(unCStr("FS"));}),_hw=[1,_hv,_hu],_hx=new T(function(){return B(unCStr("ESC"));}),_hy=[1,_hx,_hw],_hz=new T(function(){return B(unCStr("SUB"));}),_hA=[1,_hz,_hy],_hB=new T(function(){return B(unCStr("EM"));}),_hC=[1,_hB,_hA],_hD=new T(function(){return B(unCStr("CAN"));}),_hE=[1,_hD,_hC],_hF=new T(function(){return B(unCStr("ETB"));}),_hG=[1,_hF,_hE],_hH=new T(function(){return B(unCStr("SYN"));}),_hI=[1,_hH,_hG],_hJ=new T(function(){return B(unCStr("NAK"));}),_hK=[1,_hJ,_hI],_hL=new T(function(){return B(unCStr("DC4"));}),_hM=[1,_hL,_hK],_hN=new T(function(){return B(unCStr("DC3"));}),_hO=[1,_hN,_hM],_hP=new T(function(){return B(unCStr("DC2"));}),_hQ=[1,_hP,_hO],_hR=new T(function(){return B(unCStr("DC1"));}),_hS=[1,_hR,_hQ],_hT=new T(function(){return B(unCStr("DLE"));}),_hU=[1,_hT,_hS],_hV=new T(function(){return B(unCStr("SI"));}),_hW=[1,_hV,_hU],_hX=new T(function(){return B(unCStr("SO"));}),_hY=[1,_hX,_hW],_hZ=new T(function(){return B(unCStr("CR"));}),_i0=[1,_hZ,_hY],_i1=new T(function(){return B(unCStr("FF"));}),_i2=[1,_i1,_i0],_i3=new T(function(){return B(unCStr("VT"));}),_i4=[1,_i3,_i2],_i5=new T(function(){return B(unCStr("LF"));}),_i6=[1,_i5,_i4],_i7=new T(function(){return B(unCStr("HT"));}),_i8=[1,_i7,_i6],_i9=[1,_hm,_i8],_ia=[1,_hl,_i9],_ib=[1,_hk,_ia],_ic=new T(function(){return B(unCStr("ENQ"));}),_id=[1,_ic,_ib],_ie=new T(function(){return B(unCStr("EOT"));}),_if=[1,_ie,_id],_ig=new T(function(){return B(unCStr("ETX"));}),_ih=[1,_ig,_if],_ii=new T(function(){return B(unCStr("STX"));}),_ij=[1,_ii,_ih],_ik=new T(function(){return B(unCStr("SOH"));}),_il=[1,_ik,_ij],_im=new T(function(){return B(unCStr("NUL"));}),_in=[1,_im,_il],_io=[0,92],_ip=new T(function(){return B(unCStr("\\DEL"));}),_iq=new T(function(){return B(unCStr("\\a"));}),_ir=new T(function(){return B(unCStr("\\\\"));}),_is=new T(function(){return B(unCStr("\\SO"));}),_it=new T(function(){return B(unCStr("\\r"));}),_iu=new T(function(){return B(unCStr("\\f"));}),_iv=new T(function(){return B(unCStr("\\v"));}),_iw=new T(function(){return B(unCStr("\\n"));}),_ix=new T(function(){return B(unCStr("\\t"));}),_iy=new T(function(){return B(unCStr("\\b"));}),_iz=function(_iA,_iB){if(_iA<=127){var _iC=E(_iA);switch(_iC){case 92:return new F(function(){return _C(_ir,_iB);});break;case 127:return new F(function(){return _C(_ip,_iB);});break;default:if(_iC<32){var _iD=E(_iC);switch(_iD){case 7:return new F(function(){return _C(_iq,_iB);});break;case 8:return new F(function(){return _C(_iy,_iB);});break;case 9:return new F(function(){return _C(_ix,_iB);});break;case 10:return new F(function(){return _C(_iw,_iB);});break;case 11:return new F(function(){return _C(_iv,_iB);});break;case 12:return new F(function(){return _C(_iu,_iB);});break;case 13:return new F(function(){return _C(_it,_iB);});break;case 14:return new F(function(){return _C(_is,new T(function(){var _iE=E(_iB);if(!_iE[0]){var _iF=[0];}else{var _iF=E(E(_iE[1])[1])==72?B(unAppCStr("\\&",_iE)):E(_iE);}return _iF;}));});break;default:return new F(function(){return _C([1,_io,new T(function(){var _iG=_iD;return _iG>=0?B(_hf(_in,_iG)):E(_hc);})],_iB);});}}else{return [1,[0,_iC],_iB];}}}else{return [1,_io,new T(function(){var _iH=jsShowI(_iA),_iI=_iH;return B(_C(fromJSStr(_iI),new T(function(){var _iJ=E(_iB);if(!_iJ[0]){var _iK=[0];}else{var _iL=E(_iJ[1])[1];if(_iL<48){var _iM=E(_iJ);}else{var _iM=_iL>57?E(_iJ):B(unAppCStr("\\&",_iJ));}var _iN=_iM,_iO=_iN,_iK=_iO;}return _iK;})));})];}},_iP=[0,39],_iQ=[1,_iP,_i],_iR=new T(function(){return B(unCStr("\'\\\'\'"));}),_iS=function(_iT){var _iU=E(E(_iT)[1]);return _iU==39?E(_iR):[1,_iP,new T(function(){return B(_iz(_iU,_iQ));})];},_iV=[0,34],_iW=new T(function(){return B(unCStr("\\\""));}),_iX=function(_iY,_iZ){var _j0=E(_iY);if(!_j0[0]){return E(_iZ);}else{var _j1=_j0[2],_j2=E(E(_j0[1])[1]);if(_j2==34){return new F(function(){return _C(_iW,new T(function(){return B(_iX(_j1,_iZ));}));});}else{return new F(function(){return _iz(_j2,new T(function(){return B(_iX(_j1,_iZ));}));});}}},_j3=function(_j4,_j5){return [1,_iV,new T(function(){return B(_iX(_j4,[1,_iV,_j5]));})];},_j6=function(_j7){return new F(function(){return _C(_iR,_j7);});},_j8=function(_j9,_ja){var _jb=E(E(_ja)[1]);return _jb==39?E(_j6):function(_jc){return [1,_iP,new T(function(){return B(_iz(_jb,[1,_iP,_jc]));})];};},_jd=[0,_j8,_iS,_j3],_je=function(_jf){return E(E(_jf)[3]);},_jg=function(_jh,_ji){return new F(function(){return A(_je,[_jh,_ji,_i]);});},_jj=function(_jk,_jl,_jm){return new F(function(){return _2K(new T(function(){return B(_je(_jk));}),_jl,_jm);});},_jn=function(_jo){return [0,function(_jp){return E(new T(function(){return B(_je(_jo));}));},function(_j7){return new F(function(){return _jg(_jo,_j7);});},function(_jq,_j7){return new F(function(){return _jj(_jo,_jq,_j7);});}];},_jr=new T(function(){return B(_jn(_jd));}),_js=new T(function(){return B(unCStr("Just "));}),_jt=new T(function(){return B(unCStr("Nothing"));}),_ju=[0,11],_jv=function(_jw){return E(E(_jw)[1]);},_jx=function(_jy,_jz,_jA,_jB){var _jC=E(_jA);if(!_jC[0]){return new F(function(){return _C(_jt,_jB);});}else{var _jD=_jC[1];return E(_jz)[1]<=10?B(_C(_js,new T(function(){return B(A(_jv,[_jy,_ju,_jD,_jB]));}))):[1,_V,new T(function(){return B(_C(_js,new T(function(){return B(A(_jv,[_jy,_ju,_jD,[1,_U,_jB]]));})));})];}},_jE=[0,0],_jF=function(_jG,_jH){return new F(function(){return _jx(_jG,_jE,_jH,_i);});},_jI=function(_jJ,_jK,_jL){return new F(function(){return _2K(function(_jq,_j7){return new F(function(){return _jx(_jJ,_jE,_jq,_j7);});},_jK,_jL);});},_jM=function(_jN){return [0,function(_jO,_jq,_j7){return new F(function(){return _jx(_jN,_jO,_jq,_j7);});},function(_j7){return new F(function(){return _jF(_jN,_j7);});},function(_jq,_j7){return new F(function(){return _jI(_jN,_jq,_j7);});}];},_jP=new T(function(){return B(_jM(_jr));}),_jQ=function(_jR){var _jS=jsShow(E(_jR)[1]),_jT=_jS;return new F(function(){return fromJSStr(_jT);});},_jU=function(_jV){return function(_5a){return new F(function(){return _C(new T(function(){return B(_jQ(_jV));}),_5a);});};},_jW=function(_jX){return new F(function(){return _7y(0,E(_jX)[1],_i);});},_jY=function(_jZ,_k0){return new F(function(){return _7y(0,E(_jZ)[1],_k0);});},_k1=function(_k2,_k3){return new F(function(){return _2K(_jY,_k2,_k3);});},_k4=function(_k5,_k6,_k7){return new F(function(){return _7y(E(_k5)[1],E(_k6)[1],_k7);});},_k8=[0,_k4,_jW,_k1],_k9=function(_ka,_kb,_kc){return new F(function(){return A(_ka,[[1,_2H,new T(function(){return B(A(_kb,[_kc]));})]]);});},_kd=new T(function(){return B(unCStr(": empty list"));}),_ke=new T(function(){return B(unCStr("Prelude."));}),_kf=function(_kg){return new F(function(){return err(B(_C(_ke,new T(function(){return B(_C(_kg,_kd));}))));});},_kh=new T(function(){return B(unCStr("foldr1"));}),_ki=new T(function(){return B(_kf(_kh));}),_kj=function(_kk,_kl){var _km=E(_kl);if(!_km[0]){return E(_ki);}else{var _kn=_km[1],_ko=E(_km[2]);if(!_ko[0]){return E(_kn);}else{return new F(function(){return A(_kk,[_kn,new T(function(){return B(_kj(_kk,_ko));})]);});}}},_kp=function(_kq,_kr,_ks,_kt){return new F(function(){return _2K(function(_ku,_kv){var _kw=E(_ku);return [1,_V,new T(function(){return B(A(_kj,[_k9,[1,new T(function(){return B(A(new T(function(){return B(_jv(_kq));}),[_jE,_kw[1]]));}),[1,new T(function(){return B(A(new T(function(){return B(_jv(_kr));}),[_jE,_kw[2]]));}),_i]],[1,_U,_kv]]));})];},_ks,_kt);});},_kx=new T(function(){return B(unCStr("fromList "));}),_ky=function(_kz,_kA){while(1){var _kB=(function(_kC,_kD){var _kE=E(_kD);switch(_kE[0]){case 0:_kz=new T(function(){return B(_ky(_kC,_kE[4]));});_kA=_kE[3];return null;case 1:return [1,[0,[0,_kE[1]],_kE[2]],_kC];default:return E(_kC);}})(_kz,_kA);if(_kB!=null){return _kB;}}},_kF=function(_kG){var _kH=E(_kG);if(!_kH[0]){var _kI=_kH[3],_kJ=_kH[4];return _kH[2]>=0?B(_ky(new T(function(){return B(_ky(_i,_kJ));}),_kI)):B(_ky(new T(function(){return B(_ky(_i,_kI));}),_kJ));}else{return new F(function(){return _ky(_i,_kH);});}},_kK=function(_kL,_kM,_kN){var _kO=new T(function(){return B(_kF(_kN));});return _kM<=10?function(_kP){return new F(function(){return _C(_kx,new T(function(){return B(_kp(_k8,_kL,_kO,_kP));}));});}:function(_kQ){return [1,_V,new T(function(){return B(_C(_kx,new T(function(){return B(_kp(_k8,_kL,_kO,[1,_U,_kQ]));})));})];};},_kR=[0,45],_kS=function(_kT,_kU,_kV){var _kW=function(_kX){var _kY=new T(function(){return B(A(_kT,[[0, -_kV]]));});return E(_kU)[1]<=6?function(_kZ){return [1,_kR,new T(function(){return B(A(_kY,[_kZ]));})];}:function(_l0){return [1,_V,[1,_kR,new T(function(){return B(A(_kY,[[1,_U,_l0]]));})]];};};if(_kV>=0){var _l1=isDoubleNegativeZero(_kV),_l2=_l1;return E(_l2)==0?B(A(_kT,[[0,_kV]])):B(_kW(_));}else{return new F(function(){return _kW(_);});}},_l3=new T(function(){return B(unCStr("Aichan {"));}),_l4=new T(function(){return B(unCStr("_loves = "));}),_l5=new T(function(){return B(unCStr("_items = "));}),_l6=[0,125],_l7=[0,0],_l8=new T(function(){return B(unCStr(", "));}),_l9=new T(function(){return B(unCStr("_lps = "));}),_la=new T(function(){return B(unCStr("_depend = "));}),_lb=new T(function(){return B(unCStr("_lastFocus = "));}),_lc=new T(function(){return B(unCStr("_interval = "));}),_ld=new T(function(){return B(unCStr("_hasFocus = "));}),_le=new T(function(){return B(unCStr("_achieves = "));}),_lf=new T(function(){return B(unCStr("True"));}),_lg=new T(function(){return B(unCStr("False"));}),_lh=function(_li,_lj,_lk,_ll,_lm,_ln,_lo,_lp,_lq){var _lr=function(_ls){return new F(function(){return _C(_l4,new T(function(){return B(A(new T(function(){return B(_kS(_jU,_l7,E(_lj)[1]));}),[new T(function(){return B(_C(_l8,new T(function(){return B(_C(_l9,new T(function(){return B(A(new T(function(){return B(_kS(_jU,_l7,E(_lk)[1]));}),[new T(function(){return B(_C(_l8,new T(function(){return B(_C(_la,new T(function(){return B(A(new T(function(){return B(_kS(_jU,_l7,E(_ll)[1]));}),[new T(function(){return B(_C(_l8,new T(function(){return B(_C(_lb,new T(function(){return B(_X(0,_lm,new T(function(){return B(_C(_l8,new T(function(){return B(_C(_lc,new T(function(){return B(_X(0,_ln,new T(function(){return B(_C(_l8,new T(function(){return B(_C(_ld,new T(function(){var _lt=new T(function(){return B(_C(_l8,new T(function(){return B(_C(_le,new T(function(){return B(A(new T(function(){return B(_kK(_jP,0,_lp));}),[new T(function(){return B(_C(_l8,new T(function(){return B(_C(_l5,new T(function(){return B(A(new T(function(){return B(_kK(_k8,0,_lq));}),[[1,_l6,_ls]]));})));})));})]));})));})));});return !E(_lo)?B(_C(_lg,_lt)):B(_C(_lf,_lt));})));})));})));})));})));})));})));})));})]));})));})));})]));})));})));})]));}));});};return _li<11?function(_lu){return new F(function(){return _C(_l3,new T(function(){return B(_lr(_lu));}));});}:function(_lv){return [1,_V,new T(function(){return B(_C(_l3,new T(function(){return B(_lr([1,_U,_lv]));})));})];};},_lw=function(_lx){var _ly=E(_lx);return new F(function(){return A(_lh,[0,_ly[1],_ly[2],_ly[3],_ly[4],_ly[5],_ly[6],_ly[7],_ly[8],_i]);});},_lz=function(_lA){return _lA>0;},_lB=function(_lC){var _lD=B(A(_lC,[_])),_lE=_lD;return E(_lE);},_lF=function(_lG){return new F(function(){return _lB(function(_){var _=0;return new F(function(){return eval(_lG);});});});},_lH=new T(function(){return B(_lF("(function(x) {return x === null;})"));}),_lI=new T(function(){return B(unCStr("No such value"));}),_lJ=[0,_lI],_lK=new T(function(){return B(unCStr("Invalid JSON!"));}),_lL=[0,_lK],_lM=new T(function(){return [0,"(function(k) {return localStorage.getItem(k);})"];}),_lN=function(_lO,_lP,_){var _lQ=B(A(_lF,[E(_lM)[1],E(toJSStr(E(_lP))),_])),_lR=_lQ;return new T(function(){if(!B(_lB(function(_){var _=0,_lS=B(A(_lH,[E(_lR),_])),_lT=_lS;return new T(function(){return B(_lz(_lT));});}))){var _lU=String(_lR),_lV=_lU,_lW=jsParseJSON(_lV),_lX=_lW,_lY=E(_lX),_lZ=_lY[0]==0?E(_lL):B(A(_1F,[_lO,_lY[1]]));}else{var _lZ=E(_lJ);}return _lZ;});},_m0=[0,10],_m1=[1,_m0,_i],_m2=function(_m3,_m4,_){var _m5=jsWriteHandle(E(_m3)[1],toJSStr(E(_m4)));return _4L;},_m6=function(_m7,_m8,_){var _m9=E(_m7),_ma=jsWriteHandle(_m9[1],toJSStr(E(_m8)));return new F(function(){return _m2(_m9,_m1,_);});},_mb=function(_mc){var _md=I_decodeDouble(_mc);return [0,[1,_md[2]],_md[1]];},_me=function(_mf,_mg){var _mh=E(_mf);if(!_mh[0]){return [0];}else{var _mi=_mh[1];return _mg>1?[1,_mi,new T(function(){return B(_me(_mh[2],_mg-1|0));})]:[1,_mi,_i];}},_mj=function(_mk){var _ml=hs_intToInt64(2147483647),_mm=_ml,_mn=hs_leInt64(_mk,_mm),_mo=_mn;if(!E(_mo)){return [1,I_fromInt64(_mk)];}else{var _mp=hs_intToInt64(-2147483648),_mq=_mp,_mr=hs_geInt64(_mk,_mq),_ms=_mr;if(!E(_ms)){return [1,I_fromInt64(_mk)];}else{var _mt=hs_int64ToInt(_mk),_mu=_mt;return new F(function(){return _6f(_mu);});}}},_mv=function(_mw){var _mx=hs_intToInt64(_mw),_my=_mx;return E(_my);},_mz=function(_mA){var _mB=E(_mA);return _mB[0]==0?B(_mv(_mB[1])):I_toInt64(_mB[1]);},_mC=new T(function(){return B(_X(0,_28,_i));}),_mD=[0,-1],_mE=new T(function(){return B(_X(0,_mD,_i));}),_mF=new T(function(){return B(unCStr("%.2f"));}),_mG=function(_mH,_mI){while(1){var _mJ=E(_mH);if(!_mJ[0]){return E(_mI);}else{_mH=_mJ[2];var _mK=[1,_mJ[1],_mI];_mI=_mK;continue;}}},_mL=function(_mM,_mN){var _mO=E(_mN);if(!_mO[0]){return [0,_i,_i];}else{var _mP=_mO[1];if(!B(A(_mM,[_mP]))){var _mQ=new T(function(){var _mR=B(_mL(_mM,_mO[2]));return [0,_mR[1],_mR[2]];});return [0,[1,_mP,new T(function(){return E(E(_mQ)[1]);})],new T(function(){return E(E(_mQ)[2]);})];}else{return [0,_i,_mO];}}},_mS=function(_mT,_mU){var _mV=E(_mT);if(!_mV){return [0,_i,_mU];}else{var _mW=E(_mU);if(!_mW[0]){return [0,_i,_i];}else{var _mX=new T(function(){var _mY=B(_mS(_mV-1|0,_mW[2]));return [0,_mY[1],_mY[2]];});return [0,[1,_mW[1],new T(function(){return E(E(_mX)[1]);})],new T(function(){return E(E(_mX)[2]);})];}}},_mZ=function(_n0,_n1){var _n2=function(_n3,_n4){return !B(_4c(_n4,_i))?[0,_n3,new T(function(){var _n5=B(_mZ(_n0,_n4));return [1,_n5[1],_n5[2]];})]:[0,_n3,_i];};if(_n0>=0){var _n6=B(_mS(_n0,_n1));return new F(function(){return _n2(_n6[1],_n6[2]);});}else{return new F(function(){return _n2(_i,_n1);});}},_n7=function(_n8){var _n9=E(_n8);if(!_n9[0]){return [0];}else{return new F(function(){return _C(_n9[1],new T(function(){return B(_n7(_n9[2]));}));});}},_na=function(_nb){return E(E(_nb)[1])==46?true:false;},_nc=[0,44],_nd=[1,_nc,_i],_ne=function(_nf,_ng){var _nh=E(_ng);return _nh[0]==0?[0]:[1,_nf,[1,_nh[1],new T(function(){return B(_ne(_nf,_nh[2]));})]];},_ni=function(_nj){var _nk=new T(function(){var _nl=B(_mL(_na,_nj));return [0,_nl[1],_nl[2]];}),_nm=B(_mZ(3,new T(function(){return B(_mG(E(_nk)[1],_i));})));return new F(function(){return _C(B(_mG(B(_n7([1,_nm[1],new T(function(){return B(_ne(_nd,_nm[2]));})])),_i)),new T(function(){return E(E(_nk)[2]);}));});},_nn=function(_no,_np){while(1){var _nq=E(_no);if(!_nq[0]){_no=[1,I_fromInt(_nq[1])];continue;}else{return [1,I_shiftLeft(_nq[1],_np)];}}},_nr=function(_ns){var _nt=E(_ns)[1];return [0,Math.log(_nt+(_nt+1)*Math.sqrt((_nt-1)/(_nt+1)))];},_nu=function(_nv){var _nw=E(_nv)[1];return [0,Math.log(_nw+Math.sqrt(1+_nw*_nw))];},_nx=function(_ny){var _nz=E(_ny)[1];return [0,0.5*Math.log((1+_nz)/(1-_nz))];},_nA=function(_nB,_nC){return [0,Math.log(E(_nC)[1])/Math.log(E(_nB)[1])];},_nD=[0,3.141592653589793],_nE=new T(function(){return [0,0/0];}),_nF=new T(function(){return [0,-1/0];}),_nG=new T(function(){return [0,1/0];}),_nH=[0,0],_nI=function(_nJ,_nK){while(1){var _nL=E(_nJ);if(!_nL[0]){_nJ=[1,I_fromInt(_nL[1])];continue;}else{var _nM=E(_nK);if(!_nM[0]){_nJ=_nL;_nK=[1,I_fromInt(_nM[1])];continue;}else{return new F(function(){return I_fromRat(_nL[1],_nM[1]);});}}}},_nN=function(_nO,_nP){var _nQ=E(_nO);if(!_nQ[0]){var _nR=_nQ[1],_nS=E(_nP);return _nS[0]==0?_nR==_nS[1]:I_compareInt(_nS[1],_nR)==0?true:false;}else{var _nT=_nQ[1],_nU=E(_nP);return _nU[0]==0?I_compareInt(_nT,_nU[1])==0?true:false:I_compare(_nT,_nU[1])==0?true:false;}},_nV=function(_nW,_nX){return !B(_nN(_nX,_nH))?[0,B(_nI(_nW,_nX))]:!B(_nN(_nW,_nH))?!B(_M(_nW,_nH))?E(_nG):E(_nF):E(_nE);},_nY=function(_nZ){var _o0=E(_nZ);return new F(function(){return _nV(_o0[1],_o0[2]);});},_o1=function(_o2){return [0,1/E(_o2)[1]];},_o3=function(_o4){var _o5=E(_o4),_o6=_o5[1];return _o6<0?[0, -_o6]:E(_o5);},_o7=function(_o8){var _o9=E(_o8);return _o9[0]==0?_o9[1]:I_toNumber(_o9[1]);},_oa=function(_ob){return [0,B(_o7(_ob))];},_oc=[0,0],_od=[0,1],_oe=[0,-1],_of=function(_og){var _oh=E(E(_og)[1]);return _oh==0?E(_oc):_oh<=0?E(_oe):E(_od);},_oi=function(_oj,_ok){return [0,E(_oj)[1]-E(_ok)[1]];},_ol=function(_om){return [0, -E(_om)[1]];},_on=function(_oo,_op){return [0,E(_oo)[1]+E(_op)[1]];},_oq=function(_or,_os){return [0,E(_or)[1]*E(_os)[1]];},_ot=[0,_on,_oq,_oi,_ol,_o3,_of,_oa],_ou=function(_ov,_ow){return [0,E(_ov)[1]/E(_ow)[1]];},_ox=[0,_ot,_ou,_o1,_nY],_oy=function(_oz){return [0,Math.acos(E(_oz)[1])];},_oA=function(_oB){return [0,Math.asin(E(_oB)[1])];},_oC=function(_oD){return [0,Math.atan(E(_oD)[1])];},_oE=function(_oF){return [0,Math.cos(E(_oF)[1])];},_oG=function(_oH){return [0,cosh(E(_oH)[1])];},_oI=function(_oJ){return [0,Math.exp(E(_oJ)[1])];},_oK=function(_oL){return [0,Math.log(E(_oL)[1])];},_oM=function(_oN,_oO){return [0,Math.pow(E(_oN)[1],E(_oO)[1])];},_oP=function(_oQ){return [0,Math.sin(E(_oQ)[1])];},_oR=function(_oS){return [0,sinh(E(_oS)[1])];},_oT=function(_oU){return [0,Math.sqrt(E(_oU)[1])];},_oV=function(_oW){return [0,Math.tan(E(_oW)[1])];},_oX=function(_oY){return [0,tanh(E(_oY)[1])];},_oZ=[0,_ox,_nD,_oI,_oT,_oK,_oM,_nA,_oP,_oV,_oE,_oA,_oC,_oy,_oR,_oX,_oG,_nu,_nx,_nr],_p0=function(_p1){var _p2=E(_p1)[1];return [0,Math.log(_p2+(_p2+1)*Math.sqrt((_p2-1)/(_p2+1)))];},_p3=function(_p4){var _p5=E(_p4)[1];return [0,Math.log(_p5+Math.sqrt(1+_p5*_p5))];},_p6=function(_p7){var _p8=E(_p7)[1];return [0,0.5*Math.log((1+_p8)/(1-_p8))];},_p9=function(_pa,_pb){return [0,Math.log(E(_pb)[1])/Math.log(E(_pa)[1])];},_pc=[0,3.141592653589793],_pd=new T(function(){return [0,0/0];}),_pe=new T(function(){return [0,-1/0];}),_pf=new T(function(){return [0,1/0];}),_pg=function(_ph,_pi){return !B(_nN(_pi,_nH))?[0,B(_nI(_ph,_pi))]:!B(_nN(_ph,_nH))?!B(_M(_ph,_nH))?E(_pf):E(_pe):E(_pd);},_pj=function(_pk){var _pl=E(_pk);return new F(function(){return _pg(_pl[1],_pl[2]);});},_pm=function(_pn){return [0,1/E(_pn)[1]];},_po=function(_pp){var _pq=E(_pp),_pr=_pq[1];return _pr<0?[0, -_pr]:E(_pq);},_ps=function(_pt){var _pu=E(_pt);return _pu[0]==0?_pu[1]:I_toNumber(_pu[1]);},_pv=function(_pw){return [0,B(_ps(_pw))];},_px=[0,0],_py=[0,1],_pz=[0,-1],_pA=function(_pB){var _pC=E(E(_pB)[1]);return _pC==0?E(_px):_pC<=0?E(_pz):E(_py);},_pD=function(_pE,_pF){return [0,E(_pE)[1]-E(_pF)[1]];},_pG=function(_pH){return [0, -E(_pH)[1]];},_pI=function(_pJ,_pK){return [0,E(_pJ)[1]+E(_pK)[1]];},_pL=function(_pM,_pN){return [0,E(_pM)[1]*E(_pN)[1]];},_pO=[0,_pI,_pL,_pD,_pG,_po,_pA,_pv],_pP=function(_pQ,_pR){return [0,E(_pQ)[1]/E(_pR)[1]];},_pS=[0,_pO,_pP,_pm,_pj],_pT=function(_pU){return [0,Math.acos(E(_pU)[1])];},_pV=function(_pW){return [0,Math.asin(E(_pW)[1])];},_pX=function(_pY){return [0,Math.atan(E(_pY)[1])];},_pZ=function(_q0){return [0,Math.cos(E(_q0)[1])];},_q1=function(_q2){return [0,cosh(E(_q2)[1])];},_q3=function(_q4){return [0,Math.exp(E(_q4)[1])];},_q5=function(_q6){return [0,Math.log(E(_q6)[1])];},_q7=function(_q8,_q9){return [0,Math.pow(E(_q8)[1],E(_q9)[1])];},_qa=function(_qb){return [0,Math.sin(E(_qb)[1])];},_qc=function(_qd){return [0,sinh(E(_qd)[1])];},_qe=function(_qf){return [0,Math.sqrt(E(_qf)[1])];},_qg=function(_qh){return [0,Math.tan(E(_qh)[1])];},_qi=function(_qj){return [0,tanh(E(_qj)[1])];},_qk=[0,_pS,_pc,_q3,_qe,_q5,_q7,_p9,_qa,_qg,_pZ,_pV,_pX,_pT,_qc,_qi,_q1,_p3,_p6,_p0],_ql=function(_qm){var _qn=B(_mb(E(_qm)[1]));return [0,_qn[1],[0,_qn[2]]];},_qo=[0,53],_qp=function(_qq){return E(_qo);},_qr=[0,2],_qs=function(_qt){return E(_qr);},_qu=[0,1024],_qv=[0,-1021],_qw=[0,_qv,_qu],_qx=function(_qy){return E(_qw);},_qz=function(_qA){var _qB=isDoubleInfinite(E(_qA)[1]),_qC=_qB;return E(_qC)==0?false:true;},_qD=function(_qE){var _qF=isDoubleNaN(E(_qE)[1]),_qG=_qF;return E(_qG)==0?false:true;},_qH=function(_qI){var _qJ=isDoubleNegativeZero(E(_qI)[1]),_qK=_qJ;return E(_qK)==0?false:true;},_qL=function(_qM){var _qN=decodeFloat(E(_qM)[1]);return [0,new T(function(){return B(_6f(_qN[1]));}),[0,_qN[2]]];},_qO=[0,24],_qP=function(_qQ){return E(_qO);},_qR=function(_qS){return E(_qr);},_qT=[0,128],_qU=[0,-125],_qV=[0,_qU,_qT],_qW=function(_qX){return E(_qV);},_qY=function(_qZ){var _r0=isFloatInfinite(E(_qZ)[1]),_r1=_r0;return E(_r1)==0?false:true;},_r2=function(_r3){var _r4=isFloatNaN(E(_r3)[1]),_r5=_r4;return E(_r5)==0?false:true;},_r6=function(_r7){var _r8=isFloatNegativeZero(E(_r7)[1]),_r9=_r8;return E(_r9)==0?false:true;},_ra=function(_rb,_rc){return E(_rb)[1]!=E(_rc)[1]?true:false;},_rd=function(_re,_rf){return E(_re)[1]==E(_rf)[1];},_rg=[0,_rd,_ra],_rh=function(_ri,_rj){return E(_ri)[1]<E(_rj)[1];},_rk=function(_rl,_rm){return E(_rl)[1]<=E(_rm)[1];},_rn=function(_ro,_rp){return E(_ro)[1]>E(_rp)[1];},_rq=function(_rr,_rs){return E(_rr)[1]>=E(_rs)[1];},_rt=function(_ru,_rv){var _rw=E(_ru)[1],_rx=E(_rv)[1];return _rw>=_rx?_rw!=_rx?2:1:0;},_ry=function(_rz,_rA){var _rB=E(_rz),_rC=E(_rA);return _rB[1]>_rC[1]?E(_rB):E(_rC);},_rD=function(_rE,_rF){var _rG=E(_rE),_rH=E(_rF);return _rG[1]>_rH[1]?E(_rH):E(_rG);},_rI=[0,_rg,_rt,_rh,_rq,_rn,_rk,_ry,_rD],_rJ=[0,1],_rK=new T(function(){var _rL=newByteArr(256),_rM=_rL,_=_rM["v"]["i8"][0]=8,_=B((function(_rN,_rO,_rP,_){while(1){if(_rP>=256){if(_rN>=256){return E(_);}else{var _rQ=imul(2,_rN)|0,_rR=_rO+1|0,_rS=_rN;_rN=_rQ;_rO=_rR;_rP=_rS;continue;}}else{var _=_rM["v"]["i8"][_rP]=_rO,_rS=_rP+_rN|0;_rP=_rS;continue;}}})(2,0,1,_)),_rT=_rM,_rU=_rT;return [0,_rU];}),_rV=function(_rW,_rX){while(1){var _rY=(function(_rZ,_s0){var _s1=hs_int64ToInt(_rZ),_s2=_s1,_s3=E(_rK)[1]["v"]["i8"][(255&_s2>>>0)>>>0&4294967295];if(_s0>_s3){if(_s3>=8){var _s4=hs_uncheckedIShiftRA64(_rZ,8),_s5=_s4;_rW=_s5;var _s6=_s0-8|0;_rX=_s6;return null;}else{return [0,new T(function(){var _s7=hs_uncheckedIShiftRA64(_rZ,_s3),_s8=_s7;return B(_mj(_s8));}),_s0-_s3|0];}}else{return [0,new T(function(){var _s9=hs_uncheckedIShiftRA64(_rZ,_s0),_sa=_s9;return B(_mj(_sa));}),0];}})(_rW,_rX);if(_rY!=null){return _rY;}}},_sb=function(_sc){return I_toInt(_sc)>>>0;},_sd=function(_se){var _sf=E(_se);return _sf[0]==0?_sf[1]>>>0:B(_sb(_sf[1]));},_sg=function(_sh){var _si=B(_mb(_sh)),_sj=_si[1],_sk=_si[2];if(_sk<0){var _sl=function(_sm){if(!_sm){return [0,E(_sj),B(_nn(_rJ, -_sk))];}else{var _sn=B(_rV(B(_mz(_sj)), -_sk));return [0,E(_sn[1]),B(_nn(_rJ,_sn[2]))];}};return (B(_sd(_sj))&1)>>>0==0?B(_sl(1)):B(_sl(0));}else{return [0,B(_nn(_sj,_sk)),_rJ];}},_so=function(_sp){var _sq=B(_sg(E(_sp)[1]));return [0,E(_sq[1]),E(_sq[2])];},_sr=[0,_ot,_rI,_so],_ss=function(_st){return E(E(_st)[1]);},_su=[0,1],_sv=function(_sw,_sx){if(_sw<=_sx){var _sy=function(_sz){return [1,[0,_sz],new T(function(){if(_sz!=_sx){var _sA=B(_sy(_sz+1|0));}else{var _sA=[0];}var _sB=_sA;return _sB;})];};return new F(function(){return _sy(_sw);});}else{return [0];}},_sC=function(_sD){return new F(function(){return _sv(E(_sD)[1],2147483647);});},_sE=function(_sF,_sG,_sH){return _sH<=_sG?[1,[0,_sF],new T(function(){var _sI=_sG-_sF|0,_sJ=function(_sK){return _sK>=(_sH-_sI|0)?[1,[0,_sK],new T(function(){return B(_sJ(_sK+_sI|0));})]:[1,[0,_sK],_i];};return B(_sJ(_sG));})]:_sH<=_sF?[1,[0,_sF],_i]:[0];},_sL=function(_sM,_sN,_sO){return _sO>=_sN?[1,[0,_sM],new T(function(){var _sP=_sN-_sM|0,_sQ=function(_sR){return _sR<=(_sO-_sP|0)?[1,[0,_sR],new T(function(){return B(_sQ(_sR+_sP|0));})]:[1,[0,_sR],_i];};return B(_sQ(_sN));})]:_sO>=_sM?[1,[0,_sM],_i]:[0];},_sS=function(_sT,_sU){return _sU<_sT?B(_sE(_sT,_sU,-2147483648)):B(_sL(_sT,_sU,2147483647));},_sV=function(_sW,_sX){return new F(function(){return _sS(E(_sW)[1],E(_sX)[1]);});},_sY=function(_sZ,_t0,_t1){return _t0<_sZ?B(_sE(_sZ,_t0,_t1)):B(_sL(_sZ,_t0,_t1));},_t2=function(_t3,_t4,_t5){return new F(function(){return _sY(E(_t3)[1],E(_t4)[1],E(_t5)[1]);});},_t6=function(_t7,_t8){return new F(function(){return _sv(E(_t7)[1],E(_t8)[1]);});},_t9=function(_ta){return E(_ta);},_tb=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_tc=new T(function(){return B(err(_tb));}),_td=function(_te){var _tf=E(E(_te)[1]);return _tf==(-2147483648)?E(_tc):[0,_tf-1|0];},_tg=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_th=new T(function(){return B(err(_tg));}),_ti=function(_tj){var _tk=E(E(_tj)[1]);return _tk==2147483647?E(_th):[0,_tk+1|0];},_tl=[0,_ti,_td,_t9,_t9,_sC,_sV,_t6,_t2],_tm=function(_tn,_to){if(_tn<=0){if(_tn>=0){return new F(function(){return quot(_tn,_to);});}else{if(_to<=0){return new F(function(){return quot(_tn,_to);});}else{return quot(_tn+1|0,_to)-1|0;}}}else{if(_to>=0){if(_tn>=0){return new F(function(){return quot(_tn,_to);});}else{if(_to<=0){return new F(function(){return quot(_tn,_to);});}else{return quot(_tn+1|0,_to)-1|0;}}}else{return quot(_tn-1|0,_to)-1|0;}}},_tp=new T(function(){return B(unCStr("ArithException"));}),_tq=new T(function(){return B(unCStr("GHC.Exception"));}),_tr=new T(function(){return B(unCStr("base"));}),_ts=new T(function(){var _tt=hs_wordToWord64(4194982440),_tu=_tt,_tv=hs_wordToWord64(3110813675),_tw=_tv;return [0,_tu,_tw,[0,_tu,_tw,_tr,_tq,_tp],_i];}),_tx=function(_ty){return E(_ts);},_tz=function(_tA){var _tB=E(_tA);return new F(function(){return _2p(B(_2n(_tB[1])),_tx,_tB[2]);});},_tC=new T(function(){return B(unCStr("arithmetic underflow"));}),_tD=new T(function(){return B(unCStr("arithmetic overflow"));}),_tE=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_tF=new T(function(){return B(unCStr("denormal"));}),_tG=new T(function(){return B(unCStr("divide by zero"));}),_tH=new T(function(){return B(unCStr("loss of precision"));}),_tI=function(_tJ){switch(E(_tJ)){case 0:return E(_tD);case 1:return E(_tC);case 2:return E(_tH);case 3:return E(_tG);case 4:return E(_tF);default:return E(_tE);}},_tK=function(_tL){return new F(function(){return _C(_tC,_tL);});},_tM=function(_tL){return new F(function(){return _C(_tD,_tL);});},_tN=function(_tL){return new F(function(){return _C(_tE,_tL);});},_tO=function(_tL){return new F(function(){return _C(_tF,_tL);});},_tP=function(_tL){return new F(function(){return _C(_tG,_tL);});},_tQ=function(_tL){return new F(function(){return _C(_tH,_tL);});},_tR=function(_tS){switch(E(_tS)){case 0:return E(_tM);case 1:return E(_tK);case 2:return E(_tQ);case 3:return E(_tP);case 4:return E(_tO);default:return E(_tN);}},_tT=function(_tU,_tV){return new F(function(){return _2K(_tR,_tU,_tV);});},_tW=function(_tX,_tY){switch(E(_tY)){case 0:return E(_tM);case 1:return E(_tK);case 2:return E(_tQ);case 3:return E(_tP);case 4:return E(_tO);default:return E(_tN);}},_tZ=[0,_tW,_tI,_tT],_u0=new T(function(){return [0,_tx,_tZ,_u1,_tz];}),_u1=function(_tL){return [0,_u0,_tL];},_u2=3,_u3=new T(function(){return B(_u1(_u2));}),_u4=new T(function(){return die(_u3);}),_u5=0,_u6=new T(function(){return B(_u1(_u5));}),_u7=new T(function(){return die(_u6);}),_u8=function(_u9,_ua){var _ub=E(_ua);switch(_ub){case -1:var _uc=E(_u9);return _uc==(-2147483648)?E(_u7):B(_tm(_uc,-1));case 0:return E(_u4);default:return new F(function(){return _tm(_u9,_ub);});}},_ud=function(_ue,_uf){return [0,B(_u8(E(_ue)[1],E(_uf)[1]))];},_ug=[0,0],_uh=[0,_u7,_ug],_ui=function(_uj,_uk){var _ul=E(_uj)[1],_um=E(E(_uk)[1]);switch(_um){case -1:var _un=E(_ul);if(_un==(-2147483648)){return E(_uh);}else{if(_un<=0){if(_un>=0){var _uo=quotRemI(_un,-1);return [0,[0,_uo[1]],[0,_uo[2]]];}else{var _up=quotRemI(_un,-1);return [0,[0,_up[1]],[0,_up[2]]];}}else{var _uq=quotRemI(_un-1|0,-1);return [0,[0,_uq[1]-1|0],[0,(_uq[2]+(-1)|0)+1|0]];}}break;case 0:return E(_u4);default:if(_ul<=0){if(_ul>=0){var _ur=quotRemI(_ul,_um);return [0,[0,_ur[1]],[0,_ur[2]]];}else{if(_um<=0){var _us=quotRemI(_ul,_um);return [0,[0,_us[1]],[0,_us[2]]];}else{var _ut=quotRemI(_ul+1|0,_um);return [0,[0,_ut[1]-1|0],[0,(_ut[2]+_um|0)-1|0]];}}}else{if(_um>=0){if(_ul>=0){var _uu=quotRemI(_ul,_um);return [0,[0,_uu[1]],[0,_uu[2]]];}else{if(_um<=0){var _uv=quotRemI(_ul,_um);return [0,[0,_uv[1]],[0,_uv[2]]];}else{var _uw=quotRemI(_ul+1|0,_um);return [0,[0,_uw[1]-1|0],[0,(_uw[2]+_um|0)-1|0]];}}}else{var _ux=quotRemI(_ul-1|0,_um);return [0,[0,_ux[1]-1|0],[0,(_ux[2]+_um|0)+1|0]];}}}},_uy=function(_uz,_uA){var _uB=_uz%_uA;if(_uz<=0){if(_uz>=0){return E(_uB);}else{if(_uA<=0){return E(_uB);}else{var _uC=E(_uB);return _uC==0?0:_uC+_uA|0;}}}else{if(_uA>=0){if(_uz>=0){return E(_uB);}else{if(_uA<=0){return E(_uB);}else{var _uD=E(_uB);return _uD==0?0:_uD+_uA|0;}}}else{var _uE=E(_uB);return _uE==0?0:_uE+_uA|0;}}},_uF=function(_uG,_uH){var _uI=E(E(_uH)[1]);switch(_uI){case -1:return E(_ug);case 0:return E(_u4);default:return [0,B(_uy(E(_uG)[1],_uI))];}},_uJ=function(_uK,_uL){var _uM=E(_uK)[1],_uN=E(E(_uL)[1]);switch(_uN){case -1:var _uO=E(_uM);return _uO==(-2147483648)?E(_u7):[0,quot(_uO,-1)];case 0:return E(_u4);default:return [0,quot(_uM,_uN)];}},_uP=function(_uQ,_uR){var _uS=E(_uQ)[1],_uT=E(E(_uR)[1]);switch(_uT){case -1:var _uU=E(_uS);if(_uU==(-2147483648)){return E(_uh);}else{var _uV=quotRemI(_uU,-1);return [0,[0,_uV[1]],[0,_uV[2]]];}break;case 0:return E(_u4);default:var _uW=quotRemI(_uS,_uT);return [0,[0,_uW[1]],[0,_uW[2]]];}},_uX=function(_uY,_uZ){var _v0=E(E(_uZ)[1]);switch(_v0){case -1:return E(_ug);case 0:return E(_u4);default:return [0,E(_uY)[1]%_v0];}},_v1=function(_v2){return new F(function(){return _6f(E(_v2)[1]);});},_v3=function(_v4){return [0,E(B(_6f(E(_v4)[1]))),E(_su)];},_v5=function(_v6,_v7){return [0,imul(E(_v6)[1],E(_v7)[1])|0];},_v8=function(_v9,_va){return [0,E(_v9)[1]+E(_va)[1]|0];},_vb=function(_vc,_vd){return [0,E(_vc)[1]-E(_vd)[1]|0];},_ve=function(_vf){var _vg=E(_vf),_vh=_vg[1];return _vh<0?[0, -_vh]:E(_vg);},_vi=function(_vj){return [0,B(_7G(_vj))];},_vk=function(_vl){return [0, -E(_vl)[1]];},_vm=[0,-1],_vn=[0,0],_vo=[0,1],_vp=function(_vq){var _vr=E(_vq)[1];return _vr>=0?E(_vr)==0?E(_vn):E(_vo):E(_vm);},_vs=[0,_v8,_v5,_vb,_vk,_ve,_vp,_vi],_vt=function(_vu,_vv){return E(_vu)[1]==E(_vv)[1];},_vw=function(_vx,_vy){return E(_vx)[1]!=E(_vy)[1];},_vz=[0,_vt,_vw],_vA=function(_vB,_vC){var _vD=E(_vB),_vE=E(_vC);return _vD[1]>_vE[1]?E(_vD):E(_vE);},_vF=function(_vG,_vH){var _vI=E(_vG),_vJ=E(_vH);return _vI[1]>_vJ[1]?E(_vJ):E(_vI);},_vK=function(_vL,_vM){return _vL>=_vM?_vL!=_vM?2:1:0;},_vN=function(_vO,_vP){return new F(function(){return _vK(E(_vO)[1],E(_vP)[1]);});},_vQ=function(_vR,_vS){return E(_vR)[1]>=E(_vS)[1];},_vT=function(_vU,_vV){return E(_vU)[1]>E(_vV)[1];},_vW=function(_vX,_vY){return E(_vX)[1]<=E(_vY)[1];},_vZ=function(_w0,_w1){return E(_w0)[1]<E(_w1)[1];},_w2=[0,_vz,_vN,_vZ,_vQ,_vT,_vW,_vA,_vF],_w3=[0,_vs,_w2,_v3],_w4=[0,_w3,_tl,_uJ,_uX,_ud,_uF,_uP,_ui,_v1],_w5=function(_w6){return E(E(_w6)[1]);},_w7=function(_w8,_w9,_wa){while(1){if(!(_w9%2)){var _wb=B(_6h(_w8,_w8)),_wc=quot(_w9,2);_w8=_wb;_w9=_wc;continue;}else{var _wd=E(_w9);if(_wd==1){return new F(function(){return _6h(_w8,_wa);});}else{var _wb=B(_6h(_w8,_w8));_w9=quot(_wd-1|0,2);var _we=B(_6h(_w8,_wa));_w8=_wb;_wa=_we;continue;}}}},_wf=function(_wg,_wh){while(1){if(!(_wh%2)){var _wi=B(_6h(_wg,_wg)),_wj=quot(_wh,2);_wg=_wi;_wh=_wj;continue;}else{var _wk=E(_wh);if(_wk==1){return E(_wg);}else{return new F(function(){return _w7(B(_6h(_wg,_wg)),quot(_wk-1|0,2),_wg);});}}}},_wl=function(_wm){return E(E(_wm)[2]);},_wn=function(_wo){return E(E(_wo)[1]);},_wp=function(_wq){return E(E(_wq)[2]);},_wr=[0,0],_ws=[0,2],_wt=function(_wu){return E(E(_wu)[7]);},_wv=function(_ww,_wx,_wy,_wz,_wA){return new F(function(){return A(E(E(_wx)[1])[1],[new T(function(){return B(A(_wz,[_wA,new T(function(){return B(A(_wt,[_ww,_ws]));})]));}),new T(function(){return B(A(_wt,[_ww,_wr]));})]);});},_wB=function(_wC){return E(E(_wC)[3]);},_wD=new T(function(){return B(unCStr("Negative exponent"));}),_wE=new T(function(){return B(err(_wD));}),_wF=function(_wG,_wH,_wI,_wJ){var _wK=B(_ss(_wH)),_wL=_wK[1],_wM=E(_wK[2]);if(!B(A(_wM[3],[_wJ,new T(function(){return B(A(_wt,[_wL,_wr]));})]))){if(!B(A(E(_wM[1])[1],[_wJ,new T(function(){return B(A(_wt,[_wL,_wr]));})]))){var _wN=B(_ss(_wH)),_wO=_wN[1],_wP=new T(function(){return B(_ss(_wH));}),_wQ=new T(function(){return B(_w5(_wP));});return new F(function(){return (function(_wR,_wS){while(1){var _wT=(function(_wU,_wV){var _wW=E(_wH),_wX=_wW[3],_wY=E(_wW[1]);if(!B(_wv(_wY[1],_wY[2],_wY[3],_wW[4],_wV))){return !B(A(E(E(_wN[2])[1])[1],[_wV,new T(function(){return B(A(_wt,[_wO,_su]));})]))?B((function(_wZ,_x0,_x1){while(1){var _x2=(function(_x3,_x4,_x5){var _x6=E(_wH),_x7=_x6[3],_x8=E(_x6[1]);if(!B(_wv(_x8[1],_x8[2],_x8[3],_x6[4],_x4))){if(!B(A(new T(function(){return B(_6X(new T(function(){return B(_wn(new T(function(){return B(_wp(_wP));})));})));}),[_x4,new T(function(){return B(A(_wt,[_wQ,_su]));})]))){_wZ=new T(function(){return B(A(new T(function(){return B(_wl(_wG));}),[_x3,_x3]));});_x0=new T(function(){return B(A(_x7,[new T(function(){return B(A(new T(function(){return B(_wB(_wQ));}),[_x4,new T(function(){return B(A(_wt,[_wQ,_su]));})]));}),new T(function(){return B(A(_wt,[_wQ,_ws]));})]));});_x1=new T(function(){return B(A(new T(function(){return B(_wl(_wG));}),[_x3,_x5]));});return null;}else{return new F(function(){return A(new T(function(){return B(_wl(_wG));}),[_x3,_x5]);});}}else{_wZ=new T(function(){return B(A(new T(function(){return B(_wl(_wG));}),[_x3,_x3]));});_x0=new T(function(){return B(A(_x7,[_x4,new T(function(){return B(A(_wt,[_wQ,_ws]));})]));});var _x9=_x5;_x1=_x9;return null;}})(_wZ,_x0,_x1);if(_x2!=null){return _x2;}}})(new T(function(){return B(A(new T(function(){return B(_wl(_wG));}),[_wU,_wU]));}),new T(function(){return B(A(_wX,[new T(function(){return B(A(new T(function(){return B(_wB(_wO));}),[_wV,new T(function(){return B(A(_wt,[_wO,_su]));})]));}),new T(function(){return B(A(_wt,[_wO,_ws]));})]));}),_wU)):E(_wU);}else{_wR=new T(function(){return B(A(new T(function(){return B(_wl(_wG));}),[_wU,_wU]));});_wS=new T(function(){return B(A(_wX,[_wV,new T(function(){return B(A(_wt,[_wO,_ws]));})]));});return null;}})(_wR,_wS);if(_wT!=null){return _wT;}}})(_wI,_wJ);});}else{return new F(function(){return A(_wt,[_wG,_su]);});}}else{return E(_wE);}},_xa=new T(function(){return B(err(_wD));}),_xb=function(_xc,_xd){var _xe=E(_xc);return _xe[0]==0?_xe[1]*Math.pow(2,_xd):I_toNumber(_xe[1])*Math.pow(2,_xd);},_xf=function(_xg,_xh){while(1){var _xi=E(_xg);if(!_xi[0]){var _xj=E(_xi[1]);if(_xj==(-2147483648)){_xg=[1,I_fromInt(-2147483648)];continue;}else{var _xk=E(_xh);if(!_xk[0]){var _xl=_xk[1];return [0,[0,quot(_xj,_xl)],[0,_xj%_xl]];}else{_xg=[1,I_fromInt(_xj)];_xh=_xk;continue;}}}else{var _xm=E(_xh);if(!_xm[0]){_xg=_xi;_xh=[1,I_fromInt(_xm[1])];continue;}else{var _xn=I_quotRem(_xi[1],_xm[1]);return [0,[1,_xn[1]],[1,_xn[2]]];}}}},_xo=function(_xp,_xq){var _xr=B(_mb(_xq)),_xs=_xr[1],_xt=_xr[2],_xu=new T(function(){return B(_w5(new T(function(){return B(_ss(_xp));})));});if(_xt<0){var _xv= -_xt;if(_xv>=0){var _xw=E(_xv),_xx=_xw==0?E(_su):B(_wf(_qr,_xw));if(!B(_nN(_xx,_nH))){var _xy=B(_xf(_xs,_xx));return [0,new T(function(){return B(A(_wt,[_xu,_xy[1]]));}),new T(function(){return [0,B(_xb(_xy[2],_xt))];})];}else{return E(_u4);}}else{return E(_xa);}}else{return [0,new T(function(){return B(A(_wl,[_xu,new T(function(){return B(A(_wt,[_xu,_xs]));}),new T(function(){return B(_wF(_xu,_w4,new T(function(){return B(A(_wt,[_xu,_qr]));}),[0,_xt]));})]));}),_oc];}},_xz=function(_xA,_xB){var _xC=B(_xo(_xA,E(_xB)[1])),_xD=_xC[1];if(E(_xC[2])[1]<=0){return E(_xD);}else{var _xE=E(B(_ss(_xA))[1]);return new F(function(){return A(_xE[1],[_xD,new T(function(){return B(A(_xE[7],[_rJ]));})]);});}},_xF=function(_xG,_xH){var _xI=B(_xo(_xG,E(_xH)[1])),_xJ=_xI[1];if(E(_xI[2])[1]>=0){return E(_xJ);}else{var _xK=E(B(_ss(_xG))[1]);return new F(function(){return A(_xK[3],[_xJ,new T(function(){return B(A(_xK[7],[_rJ]));})]);});}},_xL=function(_xM,_xN){var _xO=B(_xo(_xM,E(_xN)[1]));return [0,_xO[1],_xO[2]];},_xP=function(_xQ,_xR){var _xS=B(_xo(_xQ,_xR)),_xT=_xS[1],_xU=E(_xS[2])[1],_xV=new T(function(){var _xW=E(B(_ss(_xQ))[1]),_xX=_xW[7];return _xU>=0?B(A(_xW[1],[_xT,new T(function(){return B(A(_xX,[_rJ]));})])):B(A(_xW[3],[_xT,new T(function(){return B(A(_xX,[_rJ]));})]));});if(_xU<0){var _xY= -_xU-0.5;if(_xY>=0){if(!E(_xY)){var _xZ=E(_xQ),_y0=E(_xZ[1]);return !B(_wv(_y0[1],_y0[2],_y0[3],_xZ[4],_xT))?E(_xV):E(_xT);}else{return E(_xV);}}else{return E(_xT);}}else{var _y1=_xU-0.5;if(_y1>=0){if(!E(_y1)){var _y2=E(_xQ),_y3=E(_y2[1]);return !B(_wv(_y3[1],_y3[2],_y3[3],_y2[4],_xT))?E(_xV):E(_xT);}else{return E(_xV);}}else{return E(_xT);}}},_y4=function(_y5,_y6){return new F(function(){return _xP(_y5,E(_y6)[1]);});},_y7=function(_y8,_y9){return E(B(_xo(_y8,E(_y9)[1]))[1]);},_ya=[0,_sr,_ox,_xL,_y7,_y4,_xz,_xF],_yb=function(_yc,_yd){return E(_yc)[1]!=E(_yd)[1]?true:false;},_ye=function(_yf,_yg){return E(_yf)[1]==E(_yg)[1];},_yh=[0,_ye,_yb],_yi=function(_yj,_yk){return E(_yj)[1]<E(_yk)[1];},_yl=function(_ym,_yn){return E(_ym)[1]<=E(_yn)[1];},_yo=function(_yp,_yq){return E(_yp)[1]>E(_yq)[1];},_yr=function(_ys,_yt){return E(_ys)[1]>=E(_yt)[1];},_yu=function(_yv,_yw){var _yx=E(_yv)[1],_yy=E(_yw)[1];return _yx>=_yy?_yx!=_yy?2:1:0;},_yz=function(_yA,_yB){var _yC=E(_yA),_yD=E(_yB);return _yC[1]>_yD[1]?E(_yC):E(_yD);},_yE=function(_yF,_yG){var _yH=E(_yF),_yI=E(_yG);return _yH[1]>_yI[1]?E(_yI):E(_yH);},_yJ=[0,_yh,_yu,_yi,_yr,_yo,_yl,_yz,_yE],_yK=function(_yL,_yM){while(1){var _yN=(function(_yO,_yP){var _yQ=E(_rK)[1]["v"]["i8"][(255&_yO>>>0)>>>0&4294967295];if(_yP>_yQ){if(_yQ>=8){var _yR=_yO>>8,_yS=_yP-8|0;_yL=_yR;_yM=_yS;return null;}else{return [0,new T(function(){return B(_6f(_yO>>_yQ));}),_yP-_yQ|0];}}else{return [0,new T(function(){return B(_6f(_yO>>_yP));}),0];}})(_yL,_yM);if(_yN!=null){return _yN;}}},_yT=function(_yU){var _yV=decodeFloat(_yU),_yW=_yV[1],_yX=_yV[2];if(_yX<0){var _yY=function(_yZ){if(!_yZ){return [0,B(_6f(_yW)),B(_nn(_rJ, -_yX))];}else{var _z0=B(_yK(_yW, -_yX));return [0,E(_z0[1]),B(_nn(_rJ,_z0[2]))];}};return (_yW>>>0&1)>>>0==0?B(_yY(1)):B(_yY(0));}else{return [0,B(_nn(B(_6f(_yW)),_yX)),_rJ];}},_z1=function(_z2){var _z3=B(_yT(E(_z2)[1]));return [0,E(_z3[1]),E(_z3[2])];},_z4=[0,_pO,_yJ,_z1],_z5=[0,-1],_z6=[0,1],_z7=function(_z8,_z9){var _za=E(_z8);return _za[0]==0?_za[1]*Math.pow(2,_z9):I_toNumber(_za[1])*Math.pow(2,_z9);},_zb=[0,0],_zc=function(_zd,_ze){var _zf=decodeFloat(_ze),_zg=_zf[1],_zh=_zf[2],_zi=new T(function(){return B(_w5(new T(function(){return B(_ss(_zd));})));});if(_zh<0){var _zj=new T(function(){if(_zg<0){var _zk= -_zh;if(_zk<32){var _zl=[0, -( -_zg>>_zk)];}else{var _zl= -_zg>=0?E(_zb):E(_z6);}var _zm=_zl,_zn=_zm,_zo=_zn;}else{var _zp= -_zh;if(_zp<32){var _zq=[0,_zg>>_zp];}else{var _zq=_zg>=0?E(_zb):E(_z5);}var _zr=_zq,_zs=_zr,_zo=_zs;}var _zt=_zo;return _zt;});return [0,new T(function(){return B(A(_wt,[_zi,new T(function(){return B(_6f(E(_zj)[1]));})]));}),new T(function(){var _zu= -_zh;if(_zu<32){var _zv=[0,B(_z7(B(_6f(_zg-(E(_zj)[1]<<_zu)|0)),_zh))];}else{var _zv=[0,B(_z7(B(_6f(_zg)),_zh))];}var _zw=_zv,_zx=_zw,_zy=_zx;return _zy;})];}else{return [0,new T(function(){return B(A(_wl,[_zi,new T(function(){return B(A(_wt,[_zi,new T(function(){return B(_6f(_zg));})]));}),new T(function(){return B(_wF(_zi,_w4,new T(function(){return B(A(_wt,[_zi,_qr]));}),[0,_zh]));})]));}),_px];}},_zz=function(_zA,_zB){var _zC=B(_zc(_zA,E(_zB)[1])),_zD=_zC[1];if(E(_zC[2])[1]<=0){return E(_zD);}else{var _zE=E(B(_ss(_zA))[1]);return new F(function(){return A(_zE[1],[_zD,new T(function(){return B(A(_zE[7],[_rJ]));})]);});}},_zF=function(_zG,_zH){var _zI=B(_zc(_zG,E(_zH)[1])),_zJ=_zI[1];if(E(_zI[2])[1]>=0){return E(_zJ);}else{var _zK=E(B(_ss(_zG))[1]);return new F(function(){return A(_zK[3],[_zJ,new T(function(){return B(A(_zK[7],[_rJ]));})]);});}},_zL=function(_zM,_zN){var _zO=B(_zc(_zM,E(_zN)[1]));return [0,_zO[1],_zO[2]];},_zP=function(_zQ,_zR){var _zS=B(_zc(_zQ,_zR)),_zT=_zS[1],_zU=E(_zS[2])[1],_zV=new T(function(){var _zW=E(B(_ss(_zQ))[1]),_zX=_zW[7];return _zU>=0?B(A(_zW[1],[_zT,new T(function(){return B(A(_zX,[_rJ]));})])):B(A(_zW[3],[_zT,new T(function(){return B(A(_zX,[_rJ]));})]));});if(_zU<0){var _zY= -_zU-0.5;if(_zY>=0){if(!E(_zY)){var _zZ=E(_zQ),_A0=E(_zZ[1]);return !B(_wv(_A0[1],_A0[2],_A0[3],_zZ[4],_zT))?E(_zV):E(_zT);}else{return E(_zV);}}else{return E(_zT);}}else{var _A1=_zU-0.5;if(_A1>=0){if(!E(_A1)){var _A2=E(_zQ),_A3=E(_A2[1]);return !B(_wv(_A3[1],_A3[2],_A3[3],_A2[4],_zT))?E(_zV):E(_zT);}else{return E(_zV);}}else{return E(_zT);}}},_A4=function(_A5,_A6){return new F(function(){return _zP(_A5,E(_A6)[1]);});},_A7=function(_A8,_A9){return E(B(_zc(_A8,E(_A9)[1]))[1]);},_Aa=[0,_z4,_pS,_zL,_A7,_A4,_zz,_zF],_Ab=function(_Ac){return new F(function(){return err(B(unAppCStr("Char.intToDigit: not a digit ",new T(function(){if(_Ac>=0){var _Ad=jsShowI(_Ac),_Ae=_Ad,_Af=fromJSStr(_Ae);}else{var _Ag=jsShowI(_Ac),_Ah=_Ag,_Af=fromJSStr(_Ah);}var _Ai=_Af;return _Ai;}))));});},_Aj=function(_Ak){var _Al=function(_Am){if(_Ak<10){return new F(function(){return _Ab(_Ak);});}else{if(_Ak>15){return new F(function(){return _Ab(_Ak);});}else{return (97+_Ak|0)-10|0;}}};if(_Ak<0){return new F(function(){return _Al(_);});}else{if(_Ak>9){return new F(function(){return _Al(_);});}else{return 48+_Ak|0;}}},_An=function(_Ao){return [0,B(_Aj(E(_Ao)[1]))];},_Ap=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_Aq=function(_Ar){return new F(function(){return _34([0,new T(function(){return B(_3j(_Ar,_Ap));})],_31);});},_As=new T(function(){return B(_Aq("GHC/Float.lhs:619:11-64|d : ds\'"));}),_At=function(_Au,_Av){if(E(_Au)[1]<=0){var _Aw=B(_1j(_An,[1,_zb,_Av]));return _Aw[0]==0?E(_As):[0,_Aw[1],_Aw[2]];}else{var _Ax=B(_1j(_An,_Av));return _Ax[0]==0?E(_As):[0,_Ax[1],_Ax[2]];}},_Ay=function(_Az){return E(E(_Az)[1]);},_AA=function(_AB){return E(E(_AB)[1]);},_AC=function(_AD){return E(E(_AD)[1]);},_AE=[0,48],_AF=[1,_AE,_i],_AG=[0,46],_AH=function(_AI,_AJ,_AK){while(1){var _AL=(function(_AM,_AN,_AO){var _AP=E(_AM);if(!_AP){var _AQ=B(_mG(_AN,_i));return _AQ[0]==0?[1,_AE,[1,_AG,new T(function(){var _AR=E(_AO);return _AR[0]==0?E(_AF):E(_AR);})]]:B(_C(_AQ,[1,_AG,new T(function(){var _AS=E(_AO);return _AS[0]==0?E(_AF):E(_AS);})]));}else{var _AT=E(_AO);if(!_AT[0]){_AI=_AP-1|0;var _AU=[1,_AE,_AN];_AK=_i;_AJ=_AU;return null;}else{_AI=_AP-1|0;var _AU=[1,_AT[1],_AN];_AK=_AT[2];_AJ=_AU;return null;}}})(_AI,_AJ,_AK);if(_AL!=null){return _AL;}}},_AV=[0,0],_AW=new T(function(){return B(unCStr(" out of range "));}),_AX=new T(function(){return B(unCStr("}.index: Index "));}),_AY=new T(function(){return B(unCStr("Ix{"));}),_AZ=[1,_U,_i],_B0=[1,_U,_AZ],_B1=function(_B2,_B3,_B4,_B5,_B6){return new F(function(){return err(B(_C(_AY,new T(function(){return B(_C(_B2,new T(function(){return B(_C(_AX,[1,_V,new T(function(){return B(A(_B6,[_AV,_B3,[1,_U,new T(function(){return B(_C(_AW,[1,_V,[1,_V,new T(function(){return B(A(_kj,[_k9,[1,new T(function(){return B(A(_B6,[_jE,_B4]));}),[1,new T(function(){return B(A(_B6,[_jE,_B5]));}),_i]],_B0]));})]]));})]]));})]));})));}))));});},_B7=function(_B8,_B9,_Ba,_Bb){var _Bc=E(_Ba);return new F(function(){return _B1(_B8,_B9,_Bc[1],_Bc[2],E(_Bb)[1]);});},_Bd=function(_Be,_Bf,_Bg,_Bh){return new F(function(){return _B7(_Bh,_Bg,_Bf,_Be);});},_Bi=new T(function(){return B(unCStr("Int"));}),_Bj=function(_Bk,_Bl,_Bm){return new F(function(){return _Bd(_k8,[0,_Bl,_Bm],_Bk,_Bi);});},_Bn=new T(function(){return B(unCStr("(Array.!): undefined array element"));}),_Bo=new T(function(){return B(err(_Bn));}),_Bp=[0,1100],_Bq=[0,_zb,_Bp],_Br=function(_Bs){return new F(function(){return _Bd(_k8,_Bq,[0,_Bs],_Bi);});},_Bt=function(_){var _Bu=newArr(1101,_Bo),_Bv=_Bu;return new F(function(){return (function(_Bw,_){while(1){var _Bx=(function(_By,_){if(0>_By){return new F(function(){return _Br(_By);});}else{if(_By>1100){return new F(function(){return _Br(_By);});}else{var _=_Bv[_By]=new T(function(){if(_By>=0){var _Bz=E(_By),_BA=_Bz==0?E(_su):B(_wf(_qr,_Bz));}else{var _BA=E(_xa);}var _BB=_BA;return _BB;}),_BC=E(_By);if(_BC==1100){var _BD=_Bv,_BE=_BD;return [0,E(_zb),E(_Bp),1101,_BE];}else{_Bw=_BC+1|0;return null;}}}})(_Bw,_);if(_Bx!=null){return _Bx;}}})(0,_);});},_BF=function(_BG){var _BH=B(A(_BG,[_])),_BI=_BH;return E(_BI);},_BJ=new T(function(){return B(_BF(_Bt));}),_BK=[0,10],_BL=[0,324],_BM=[0,_zb,_BL],_BN=function(_BO){return new F(function(){return _Bd(_k8,_BM,[0,_BO],_Bi);});},_BP=function(_){var _BQ=newArr(325,_Bo),_BR=_BQ;return new F(function(){return (function(_BS,_){while(1){var _BT=(function(_BU,_){if(0>_BU){return new F(function(){return _BN(_BU);});}else{if(_BU>324){return new F(function(){return _BN(_BU);});}else{var _=_BR[_BU]=new T(function(){if(_BU>=0){var _BV=E(_BU),_BW=_BV==0?E(_su):B(_wf(_BK,_BV));}else{var _BW=E(_xa);}var _BX=_BW;return _BX;}),_BY=E(_BU);if(_BY==324){var _BZ=_BR,_C0=_BZ;return [0,E(_zb),E(_BL),325,_C0];}else{_BS=_BY+1|0;return null;}}}})(_BS,_);if(_BT!=null){return _BT;}}})(0,_);});},_C1=new T(function(){return B(_BF(_BP));}),_C2=function(_C3,_C4){var _C5=[0,_C4],_C6=function(_C7){if(!B(_nN(_C3,_BK))){if(_C4>=0){var _C8=E(_C4);return _C8==0?E(_su):B(_wf(_C3,_C8));}else{return E(_xa);}}else{if(_C4>324){if(_C4>=0){var _C9=E(_C4);return _C9==0?E(_su):B(_wf(_C3,_C9));}else{return E(_xa);}}else{var _Ca=E(_C1),_Cb=E(_Ca[1]),_Cc=_Cb[1],_Cd=E(_Ca[2]);if(_Cc>_C4){return new F(function(){return _Bj(_C5,_Cb,_Cd);});}else{if(_C4>_Cd[1]){return new F(function(){return _Bj(_C5,_Cb,_Cd);});}else{return E(_Ca[4][_C4-_Cc|0]);}}}}};if(!B(_nN(_C3,_qr))){return new F(function(){return _C6(_);});}else{if(_C4<0){return new F(function(){return _C6(_);});}else{if(_C4>1100){return new F(function(){return _C6(_);});}else{var _Ce=E(_BJ),_Cf=E(_Ce[1]),_Cg=_Cf[1],_Ch=E(_Ce[2]);if(_Cg>_C4){return new F(function(){return _Bj(_C5,_Cf,_Ch);});}else{if(_C4>_Ch[1]){return new F(function(){return _Bj(_C5,_Cf,_Ch);});}else{return E(_Ce[4][_C4-_Cg|0]);}}}}}},_Ci=function(_Cj,_Ck){var _Cl=E(_Cj);if(!_Cl[0]){var _Cm=_Cl[1],_Cn=E(_Ck);return _Cn[0]==0?_Cm>_Cn[1]:I_compareInt(_Cn[1],_Cm)<0;}else{var _Co=_Cl[1],_Cp=E(_Ck);return _Cp[0]==0?I_compareInt(_Co,_Cp[1])>0:I_compare(_Co,_Cp[1])>0;}},_Cq=[1,_zb,_i],_Cr=function(_Cs,_Ct){while(1){var _Cu=E(_Cs);if(!_Cu[0]){var _Cv=E(_Cu[1]);if(_Cv==(-2147483648)){_Cs=[1,I_fromInt(-2147483648)];continue;}else{var _Cw=E(_Ct);if(!_Cw[0]){return [0,quot(_Cv,_Cw[1])];}else{_Cs=[1,I_fromInt(_Cv)];_Ct=_Cw;continue;}}}else{var _Cx=_Cu[1],_Cy=E(_Ct);return _Cy[0]==0?[0,I_toInt(I_quot(_Cx,I_fromInt(_Cy[1])))]:[1,I_quot(_Cx,_Cy[1])];}}},_Cz=function(_CA,_CB,_CC,_CD,_CE,_CF,_CG,_CH){if(!B(A(_CA,[_CH,new T(function(){return B(A(_wt,[B(_AA(B(_Ay(_CB)))),_nH]));})]))){var _CI=new T(function(){return B(A(_CC,[_CH]));}),_CJ=new T(function(){return B(A(_CD,[_CH]));}),_CK=new T(function(){return [0,E(B(A(_CE,[_CH]))[1])[1]-E(_CJ)[1]|0];}),_CL=new T(function(){return B(A(_CF,[_CH]));}),_CM=new T(function(){return E(E(_CL)[2]);}),_CN=new T(function(){var _CO=E(_CM),_CP=_CO[1],_CQ=E(_CK)[1]-_CP|0;if(_CQ<=0){var _CR=[0,new T(function(){return E(E(_CL)[1]);}),_CO];}else{var _CR=[0,new T(function(){var _CS=B(_C2(_CI,_CQ));if(!B(_nN(_CS,_nH))){var _CT=B(_Cr(E(_CL)[1],_CS));}else{var _CT=E(_u4);}var _CU=_CT;return _CU;}),[0,_CP+_CQ|0]];}var _CV=_CR,_CW=_CV,_CX=_CW,_CY=_CX;return _CY;}),_CZ=new T(function(){return E(E(_CN)[2]);}),_D0=new T(function(){return E(E(_CN)[1]);}),_D1=new T(function(){var _D2=E(_CZ)[1];if(_D2<0){if(_D2<=E(_CK)[1]){var _D3=[0,new T(function(){return B(_6h(_D0,_qr));}),new T(function(){return B(_6h(B(_C2(_CI, -_D2)),_qr));}),_rJ,_rJ];}else{var _D3=!B(_nN(_D0,B(_C2(_CI,E(_CJ)[1]-1|0))))?[0,new T(function(){return B(_6h(_D0,_qr));}),new T(function(){return B(_6h(B(_C2(_CI, -_D2)),_qr));}),_rJ,_rJ]:[0,new T(function(){return B(_6h(B(_6h(_D0,_CI)),_qr));}),new T(function(){return B(_6h(B(_C2(_CI, -_D2+1|0)),_qr));}),_CI,_rJ];}var _D4=_D3,_D5=_D4,_D6=_D5;}else{var _D7=new T(function(){return B(_C2(_CI,_D2));}),_D6=!B(_nN(_D0,B(_C2(_CI,E(_CJ)[1]-1|0))))?[0,new T(function(){return B(_6h(B(_6h(_D0,_D7)),_qr));}),_qr,_D7,_D7]:[0,new T(function(){return B(_6h(B(_6h(B(_6h(_D0,_D7)),_CI)),_qr));}),new T(function(){return B(_6h(_qr,_CI));}),new T(function(){return B(_6h(_D7,_CI));}),_D7];}var _D8=_D6,_D9=_D8;return _D9;}),_Da=new T(function(){return E(E(_D1)[2]);}),_Db=new T(function(){return E(E(_D1)[3]);}),_Dc=new T(function(){return E(E(_D1)[1]);}),_Dd=new T(function(){var _De=new T(function(){return B(_5Z(_Dc,_Db));}),_Df=function(_Dg){var _Dh=(Math.log(B(_ps(B(_5Z(_D0,_rJ)))))+E(_CZ)[1]*Math.log(B(_ps(_CI))))/Math.log(B(_ps(_CG))),_Di=_Dh&4294967295;return _Di>=_Dh?E(_Di):_Di+1|0;},_Dj=function(_Dk){while(1){if(_Dk<0){if(!B(_7J(B(_6h(B(_C2(_CG, -_Dk)),_De)),_Da))){var _Dl=_Dk+1|0;_Dk=_Dl;continue;}else{return E(_Dk);}}else{if(!B(_7J(_De,B(_6h(B(_C2(_CG,_Dk)),_Da))))){var _Dl=_Dk+1|0;_Dk=_Dl;continue;}else{return E(_Dk);}}}};if(!B(_nN(_CI,_qr))){var _Dm=[0,B(_Dj(B(_Df(_))))];}else{if(!B(_nN(_CG,_BK))){var _Dn=[0,B(_Dj(B(_Df(_))))];}else{var _Do=(E(_CJ)[1]-1|0)+E(_CM)[1]|0;if(_Do<0){var _Dp=[0,B(_Dj(quot(imul(_Do,8651)|0,28738)))];}else{var _Dp=[0,B(_Dj(quot(imul(_Do,8651)|0,28738)+1|0))];}var _Dq=_Dp,_Dr=_Dq,_Ds=_Dr,_Dt=_Ds,_Du=_Dt,_Dn=_Du;}var _Dm=_Dn;}return _Dm;});return [0,new T(function(){var _Dv=E(_Dd)[1],_Dw=function(_Dx,_Dy,_Dz,_DA,_DB){while(1){var _DC=(function(_DD,_DE,_DF,_DG,_DH){if(!B(_nN(_DF,_nH))){var _DI=B(_xf(B(_6h(_DE,_CG)),_DF)),_DJ=_DI[1],_DK=_DI[2],_DL=B(_6h(_DH,_CG)),_DM=B(_6h(_DG,_CG));if(!B(_M(_DK,_DL))){if(!B(_Ci(B(_5Z(_DK,_DM)),_DF))){var _DN=[1,_DJ,_DD];_Dy=_DK;var _DO=_DF;_DA=_DM;_DB=_DL;_Dx=_DN;_Dz=_DO;return null;}else{return [1,new T(function(){return B(_5Z(_DJ,_rJ));}),_DD];}}else{return !B(_Ci(B(_5Z(_DK,_DM)),_DF))?[1,_DJ,_DD]:!B(_M(B(_6h(_DK,_qr)),_DF))?[1,new T(function(){return B(_5Z(_DJ,_rJ));}),_DD]:[1,_DJ,_DD];}}else{return E(_u4);}})(_Dx,_Dy,_Dz,_DA,_DB);if(_DC!=null){return _DC;}}};if(_Dv<0){var _DP=B(_C2(_CG, -_Dv)),_DQ=B(_1j(_vi,B(_mG(B(_Dw(_i,B(_6h(_Dc,_DP)),_Da,B(_6h(_Db,_DP)),B(_6h(E(_D1)[4],_DP)))),_i))));}else{var _DQ=B(_1j(_vi,B(_mG(B(_Dw(_i,_Dc,B(_6h(_Da,B(_C2(_CG,_Dv)))),_Db,E(_D1)[4])),_i))));}var _DR=_DQ,_DS=_DR;return _DS;}),_Dd];}else{return [0,_Cq,_zb];}},_DT=function(_DU,_DV){while(1){var _DW=E(_DV);if(!_DW[0]){return true;}else{if(!B(A(_DU,[_DW[1]]))){return false;}else{_DV=_DW[2];continue;}}}},_DX=function(_DY){return E(_DY)[1]%2==0?true:false;},_DZ=new T(function(){return B(unCStr("roundTo: bad Value"));}),_E0=new T(function(){return B(err(_DZ));}),_E1=function(_E2){return E(E(_E2)[1])==0?true:false;},_E3=function(_E4){return _E4>1?[1,_zb,new T(function(){return B(_E3(_E4-1|0));})]:E(_Cq);},_E5=function(_E6,_E7,_E8){var _E9=function(_Ea,_Eb,_Ec){var _Ed=E(_Ec);if(!_Ed[0]){return [0,_zb,new T(function(){var _Ee=E(_Ea)[1];return _Ee>0?B(_E3(_Ee)):[0];})];}else{var _Ef=_Ed[1],_Eg=_Ed[2],_Eh=E(E(_Ea)[1]);if(!_Eh){var _Ei=E(_Ef)[1],_Ej=E(new T(function(){return [0,quot(E(_E6)[1],2)];}))[1];return _Ei!=_Ej?[0,new T(function(){return _Ei<_Ej?E(_zb):E(_z6);}),_i]:!E(_Eb)?[0,new T(function(){return _Ei<_Ej?E(_zb):E(_z6);}),_i]:!B(_DT(_E1,_Eg))?[0,new T(function(){return _Ei<_Ej?E(_zb):E(_z6);}),_i]:[0,_zb,_i];}else{var _Ek=B(_E9([0,_Eh-1|0],new T(function(){return B(_DX(_Ef));}),_Eg)),_El=_Ek[2],_Em=E(_Ek[1])[1]+E(_Ef)[1]|0;return _Em!=E(_E6)[1]?[0,_zb,[1,[0,_Em],_El]]:[0,_z6,[1,_zb,_El]];}}},_En=B(_E9(_E7,_b,_E8));switch(E(E(_En[1])[1])){case 0:return E(_En);case 1:return [0,_z6,[1,_z6,_En[2]]];default:return E(_E0);}},_Eo=function(_Ep){return E(E(_Ep)[3]);},_Eq=0,_Er=1,_Es=[0,10],_Et=new T(function(){return B(unCStr("e0"));}),_Eu=function(_Ev,_Ew){var _Ex=E(_Ev);if(!_Ex[0]){return E(_Et);}else{var _Ey=_Ex[1];return _Ew>1?[1,_Ey,new T(function(){return B(_Eu(_Ex[2],_Ew-1|0));})]:[1,_Ey,_Et];}},_Ez=function(_EA,_EB){var _EC=E(_EB);return _EC[0]==0?[0]:[1,_EA,new T(function(){return B(_Ez(_EC[1],_EC[2]));})];},_ED=new T(function(){return B(unCStr("init"));}),_EE=new T(function(){return B(_kf(_ED));}),_EF=new T(function(){return B(_Aq("GHC/Float.lhs:591:12-70|(d : ds\')"));}),_EG=[0,101],_EH=new T(function(){return B(unCStr("Infinity"));}),_EI=new T(function(){return B(unCStr("-Infinity"));}),_EJ=new T(function(){return B(unCStr("NaN"));}),_EK=new T(function(){return B(unCStr("formatRealFloat/doFmt/FFExponent: []"));}),_EL=new T(function(){return B(err(_EK));}),_EM=new T(function(){return B(unCStr("0.0e0"));}),_EN=function(_EO){return E(E(_EO)[4]);},_EP=new T(function(){return [1,_AE,_EP];}),_EQ=function(_ER,_ES,_ET,_EU,_EV,_EW,_EX,_EY,_EZ,_F0,_F1,_F2){if(!B(A(_EX,[_F2]))){var _F3=new T(function(){return B(_AA(new T(function(){return B(_Ay(_ES));})));});if(!B(A(_EY,[_F2]))){var _F4=function(_F5,_F6,_F7){while(1){var _F8=(function(_F9,_Fa,_Fb){switch(E(_F9)){case 0:var _Fc=E(_F1);if(!_Fc[0]){var _Fd=B(_1j(_An,_Fa));if(!_Fd[0]){return E(_EL);}else{var _Fe=_Fd[2],_Ff=E(_Fd[1]),_Fg=function(_Fh){var _Fi=E(_Fe);return _Fi[0]==0?[1,_Ff,new T(function(){return B(unAppCStr(".0e",new T(function(){return B(_7y(0,E(_Fb)[1]-1|0,_i));})));})]:[1,_Ff,[1,_AG,new T(function(){return B(_C(_Fi,[1,_EG,new T(function(){return B(_7y(0,E(_Fb)[1]-1|0,_i));})]));})]];};return E(_Ff[1])==48?E(_Fe)[0]==0?E(_EM):B(_Fg(_)):B(_Fg(_));}}else{var _Fj=new T(function(){var _Fk=E(_Fc[1]);return _Fk[1]>1?E(_Fk):E(_z6);}),_Fl=function(_Fm){var _Fn=new T(function(){var _Fo=B(_E5(_Es,new T(function(){return [0,E(_Fj)[1]+1|0];}),_Fa));return [0,_Fo[1],_Fo[2]];}),_Fp=new T(function(){return E(E(_Fn)[1]);}),_Fq=new T(function(){if(E(_Fp)[1]<=0){var _Fr=B(_1j(_An,E(_Fn)[2])),_Fs=_Fr[0]==0?E(_EF):[0,_Fr[1],_Fr[2]];}else{var _Ft=E(E(_Fn)[2]);if(!_Ft[0]){var _Fu=E(_EE);}else{var _Fv=B(_1j(_An,B(_Ez(_Ft[1],_Ft[2])))),_Fu=_Fv[0]==0?E(_EF):[0,_Fv[1],_Fv[2]];}var _Fw=_Fu,_Fs=_Fw;}var _Fx=_Fs,_Fy=_Fx;return _Fy;});return [1,new T(function(){return E(E(_Fq)[1]);}),[1,_AG,new T(function(){return B(_C(E(_Fq)[2],[1,_EG,new T(function(){return B(_7y(0,(E(_Fb)[1]-1|0)+E(_Fp)[1]|0,_i));})]));})]];},_Fz=E(_Fa);if(!_Fz[0]){return new F(function(){return _Fl(_);});}else{return E(E(_Fz[1])[1])==0?E(_Fz[2])[0]==0?[1,_AE,[1,_AG,new T(function(){var _FA=E(_Fj)[1];return _FA>0?B(_Eu(_EP,_FA)):E(_Et);})]]:B(_Fl(_)):B(_Fl(_));}}break;case 1:var _FB=E(_F1);if(!_FB[0]){var _FC=E(_Fb)[1];return _FC>0?B(_AH(_FC,_i,new T(function(){return B(_1j(_An,_Fa));}))):B(unAppCStr("0.",new T(function(){var _FD= -_FC;if(_FD>0){var _FE=function(_FF){return _FF>1?[1,_AE,new T(function(){return B(_FE(_FF-1|0));})]:E([1,_AE,new T(function(){return B(_1j(_An,_Fa));})]);},_FG=B(_FE(_FD));}else{var _FG=B(_1j(_An,_Fa));}var _FH=_FG,_FI=_FH;return _FI;})));}else{var _FJ=_FB[1],_FK=E(_Fb),_FL=_FK[1];if(_FL<0){var _FM=new T(function(){var _FN= -_FL;if(_FN>0){var _FO=function(_FP){return _FP>1?[1,_zb,new T(function(){return B(_FO(_FP-1|0));})]:E([1,_zb,_Fa]);},_FQ=B(_E5(_Es,new T(function(){var _FR=E(_FJ);return _FR[1]>0?E(_FR):E(_zb);}),B(_FO(_FN)))),_FS=B(_At(_FQ[1],_FQ[2]));}else{var _FT=B(_E5(_Es,new T(function(){var _FU=E(_FJ);return _FU[1]>0?E(_FU):E(_zb);}),_Fa)),_FS=B(_At(_FT[1],_FT[2]));}var _FV=_FS,_FW=_FV;return _FW;});return [1,new T(function(){return E(E(_FM)[1]);}),new T(function(){var _FX=E(E(_FM)[2]);return _FX[0]==0?[0]:[1,_AG,_FX];})];}else{var _FY=B(_E5(_Es,new T(function(){var _FZ=E(_FJ)[1];if(_FZ>0){var _G0=[0,_FZ+_FL|0];}else{var _G0=E(_FK);}var _G1=_G0,_G2=_G1;return _G2;}),_Fa)),_G3=_FY[2],_G4=_FL+E(_FY[1])[1]|0;if(_G4>=0){var _G5=B(_mS(_G4,new T(function(){return B(_1j(_An,_G3));}))),_G6=_G5[2],_G7=E(_G5[1]);return _G7[0]==0?[1,_AE,new T(function(){var _G8=E(_G6);return _G8[0]==0?[0]:[1,_AG,_G8];})]:B(_C(_G7,new T(function(){var _G9=E(_G6);return _G9[0]==0?[0]:[1,_AG,_G9];})));}else{return [1,_AE,new T(function(){var _Ga=B(_1j(_An,_G3));return _Ga[0]==0?[0]:[1,_AG,_Ga];})];}}}break;default:var _Gb=E(_Fb),_Gc=_Gb[1];if(_Gc>=0){if(_Gc<=7){_F5=_Er;var _Gd=_Fa;_F7=_Gb;_F6=_Gd;return null;}else{_F5=_Eq;var _Gd=_Fa;_F7=_Gb;_F6=_Gd;return null;}}else{_F5=_Eq;var _Gd=_Fa;_F7=_Gb;_F6=_Gd;return null;}}})(_F5,_F6,_F7);if(_F8!=null){return _F8;}}},_Ge=function(_Gf){return [1,_kR,new T(function(){var _Gg=B(_Cz(E(E(E(E(_ER)[1])[2])[1])[1],_ES,_ET,_EU,_EV,_EW,_BK,new T(function(){return B(A(_EN,[_F3,_F2]));})));return B(_F4(_F0,_Gg[1],_Gg[2]));})];};if(!B(A(_Eo,[B(_wp(B(_AC(_ER)))),_F2,new T(function(){return B(A(_wt,[_F3,_nH]));})]))){if(!B(A(_EZ,[_F2]))){var _Gh=B(_Cz(E(E(E(E(_ER)[1])[2])[1])[1],_ES,_ET,_EU,_EV,_EW,_BK,_F2));return new F(function(){return _F4(_F0,_Gh[1],_Gh[2]);});}else{return new F(function(){return _Ge(_);});}}else{return new F(function(){return _Ge(_);});}}else{return !B(A(_Eo,[B(_wp(B(_AC(_ER)))),_F2,new T(function(){return B(A(_wt,[_F3,_nH]));})]))?E(_EH):E(_EI);}}else{return E(_EJ);}},_Gi=function(_Gj){var _Gk=u_towlower(_Gj),_Gl=_Gk;return _Gl>>>0>1114111?B(_7E(_Gl)):_Gl;},_Gm=function(_Gn){return new F(function(){return err(B(unAppCStr("Printf.printf: ",_Gn)));});},_Go=new T(function(){return B(unCStr("bad argument"));}),_Gp=new T(function(){return B(_Gm(_Go));}),_Gq=new T(function(){return B(unCStr("Printf.dfmt: impossible"));}),_Gr=new T(function(){return B(err(_Gq));}),_Gs=[0,45],_Gt=[1,_Gs,_i],_Gu=new T(function(){return B(err(_Gq));}),_Gv=new T(function(){return B(unCStr("Negative exponent"));}),_Gw=new T(function(){return B(err(_Gv));}),_Gx=function(_Gy,_Gz,_GA){while(1){if(!(_Gz%2)){var _GB=_Gy*_Gy,_GC=quot(_Gz,2);_Gy=_GB;_Gz=_GC;continue;}else{var _GD=E(_Gz);if(_GD==1){return _Gy*_GA;}else{var _GB=_Gy*_Gy;_Gz=quot(_GD-1|0,2);var _GE=_Gy*_GA;_Gy=_GB;_GA=_GE;continue;}}}},_GF=function(_GG,_GH){while(1){if(!(_GH%2)){var _GI=_GG*_GG,_GJ=quot(_GH,2);_GG=_GI;_GH=_GJ;continue;}else{var _GK=E(_GH);if(_GK==1){return E(_GG);}else{return new F(function(){return _Gx(_GG*_GG,quot(_GK-1|0,2),_GG);});}}}},_GL=function(_GM,_GN){var _GO=E(_GM);return _GO[0]==0?function(_5a){return new F(function(){return _C(new T(function(){var _GP=jsShow(E(_GN)[1]),_GQ=_GP;return fromJSStr(_GQ);}),_5a);});}:function(_5a){return new F(function(){return _C(new T(function(){var _GR=E(E(_GO[1])[1]);if(!_GR){var _GS=jsRound(E(_GN)[1]),_GT=_GS,_GU=B(_mb(_GT)),_GV=_GU[1],_GW=_GU[2];if(_GW>=0){var _GX=jsShow(B(_o7(B(_nn(_GV,_GW))))),_GY=_GX,_GZ=fromJSStr(_GY);}else{var _H0=hs_uncheckedIShiftRA64(B(_mz(_GV)), -_GW),_H1=_H0,_H2=jsShow(B(_o7(B(_mj(_H1))))),_H3=_H2,_GZ=fromJSStr(_H3);}var _H4=_GZ,_H5=_H4,_H6=_H5,_H7=_H6;}else{if(_GR>=0){var _H8=B(_GF(10,_GR)),_H9=jsRound(E(_GN)[1]*_H8),_Ha=_H9,_Hb=jsShow((_Ha&4294967295)/_H8),_Hc=_Hb,_Hd=fromJSStr(_Hc);}else{var _Hd=E(_Gw);}var _He=_Hd,_Hf=_He,_H7=_Hf;}var _Hg=_H7;return _Hg;}),_5a);});};},_Hh=function(_Hi,_Hj){var _Hk=E(_Hi);return _Hk[0]==0?function(_5a){return new F(function(){return _C(new T(function(){var _Hl=B(_yT(E(_Hj)[1])),_Hm=jsShow(B(_nV(_Hl[1],_Hl[2]))[1]),_Hn=_Hm;return fromJSStr(_Hn);}),_5a);});}:function(_5a){return new F(function(){return _C(new T(function(){var _Ho=E(E(_Hk[1])[1]);if(!_Ho){var _Hp=jsRound(E(_Hj)[1]),_Hq=_Hp,_Hr=decodeFloat(_Hq),_Hs=_Hr[1],_Ht=_Hr[2];if(_Ht>=0){var _Hu=jsShow(B(_o7(B(_nn(B(_6f(_Hs)),_Ht))))),_Hv=_Hu,_Hw=fromJSStr(_Hv);}else{var _Hx=jsShow(_Hs>> -_Ht),_Hy=_Hx,_Hw=fromJSStr(_Hy);}var _Hz=_Hw,_HA=_Hz,_HB=_HA,_HC=_HB;}else{var _HD=B(_yT(E(_Hj)[1]));if(_Ho>=0){var _HE=B(_GF(10,_Ho)),_HF=jsRound(B(_nV(_HD[1],_HD[2]))[1]*_HE),_HG=_HF,_HH=jsShow((_HG&4294967295)/_HE),_HI=_HH,_HJ=fromJSStr(_HI);}else{var _HJ=E(_Gw);}var _HK=_HJ,_HL=_HK,_HM=_HL,_HN=_HM,_HC=_HN;}var _HO=_HC;return _HO;}),_5a);});};},_HP=function(_HQ){var _HR=u_towupper(_HQ),_HS=_HR;return _HS>>>0>1114111?B(_7E(_HS)):_HS;},_HT=function(_HU){return [0,B(_HP(E(_HU)[1]))];},_HV=function(_HW,_HX,_HY){var _HZ=E(_HY);switch(_HZ[0]){case 3:var _I0=_HZ[1],_I1=u_iswupper(_HW),_I2=_I1;switch(B(_Gi(_HW))){case 101:var _I3=B(_EQ(_Aa,_qk,_qR,_qP,_qW,_qL,_r2,_qY,_r6,_Eq,new T(function(){var _I4=E(_HX);return _I4[1]>=0?[1,_I4]:[0];}),_I0));break;case 102:var _I3=B(_EQ(_Aa,_qk,_qR,_qP,_qW,_qL,_r2,_qY,_r6,_Er,new T(function(){var _I5=E(_HX);return _I5[1]>=0?[1,_I5]:[0];}),_I0));break;case 103:var _I6=E(_HX),_I3=_I6[1]>=0?B(A(_Hh,[[1,_I6],_I0,_i])):B(A(_Hh,[_6F,_I0,_i]));break;default:var _I3=E(_Gu);}var _I7=_I3,_I8=E(_I2);if(!_I8){var _I9=E(_I7);if(!_I9[0]){return [0,_i,_i];}else{var _Ia=_I9[1],_Ib=_I9[2],_Ic=E(_Ia),_Id=_Ic[1],_Ie=E(_Id);return _Ie==45?[0,_Gt,_Ib]:[0,_i,_I9];}}else{var _If=B(_1j(_HT,_I7));if(!_If[0]){return [0,_i,_i];}else{var _Ig=_If[1],_Ih=_If[2],_Ii=E(_Ig),_Ij=_Ii[1],_Ik=E(_Ij);return _Ik==45?[0,_Gt,_Ih]:[0,_i,_If];}}break;case 4:var _Il=_HZ[1],_Im=u_iswupper(_HW),_In=_Im;switch(B(_Gi(_HW))){case 101:var _Io=B(_EQ(_ya,_oZ,_qs,_qp,_qx,_ql,_qD,_qz,_qH,_Eq,new T(function(){var _Ip=E(_HX);return _Ip[1]>=0?[1,_Ip]:[0];}),_Il));break;case 102:var _Io=B(_EQ(_ya,_oZ,_qs,_qp,_qx,_ql,_qD,_qz,_qH,_Er,new T(function(){var _Iq=E(_HX);return _Iq[1]>=0?[1,_Iq]:[0];}),_Il));break;case 103:var _Ir=E(_HX),_Io=_Ir[1]>=0?B(A(_GL,[[1,_Ir],_Il,_i])):B(A(_GL,[_6F,_Il,_i]));break;default:var _Io=E(_Gr);}var _Is=_Io,_It=E(_In);if(!_It){var _Iu=E(_Is);if(!_Iu[0]){return [0,_i,_i];}else{var _Iv=_Iu[1],_Iw=_Iu[2],_Ix=E(_Iv),_Iy=_Ix[1],_Iz=E(_Iy);return _Iz==45?[0,_Gt,_Iw]:[0,_i,_Iu];}}else{var _IA=B(_1j(_HT,_Is));if(!_IA[0]){return [0,_i,_i];}else{var _IB=_IA[1],_IC=_IA[2],_ID=E(_IB),_IE=_ID[1],_IF=E(_IE);return _IF==45?[0,_Gt,_IC]:[0,_i,_IA];}}break;default:return E(_Gp);}},_IG=[0,0],_IH=function(_II){return new F(function(){return _X(0,_II,_i);});},_IJ=function(_IK,_IL){while(1){var _IM=E(_IK);if(!_IM[0]){return E(_IL);}else{_IK=_IM[2];var _IN=_IL+1|0;_IL=_IN;continue;}}},_IO=[0,48],_IP=function(_IQ,_IR){var _IS=_IQ-B(_IJ(_IR,0))|0;if(_IS>0){var _IT=function(_IU){return _IU>1?[1,_IO,new T(function(){return B(_IT(_IU-1|0));})]:E([1,_IO,_IR]);};return new F(function(){return _IT(_IS);});}else{return E(_IR);}},_IV=[0,0],_IW=[0,-2147483648],_IX=function(_IY,_IZ){while(1){var _J0=(function(_J1,_J2){var _J3=E(_J2);switch(_J3[0]){case 0:_IY=_IV;_IZ=[2,_IW,new T(function(){return B(_6f(E(_J3[1])[1]));})];return null;case 2:var _J4=_J3[2];return !B(_M(_J4,_IG))?[0,_i,new T(function(){return B(_IP(E(_J1)[1],B(_IH(_J4))));})]:[0,_Gt,new T(function(){return B(_IP(E(_J1)[1],B(_X(0,B(_69(_J4)),_i))));})];default:return E(_Gp);}})(_IY,_IZ);if(_J0!=null){return _J0;}}},_J5=[1,_iP,_i],_J6=function(_J7){return new F(function(){return err(B(unAppCStr("Char.digitToInt: not a digit ",new T(function(){var _J8=E(_J7);return _J8==39?E(_iR):[1,_iP,new T(function(){return B(_iz(_J8,_J5));})];}))));});},_J9=function(_Ja){var _Jb=function(_Jc){var _Jd=function(_Je){if(_Ja<65){return new F(function(){return _J6(_Ja);});}else{if(_Ja>70){return new F(function(){return _J6(_Ja);});}else{return (_Ja-65|0)+10|0;}}};if(_Ja<97){return new F(function(){return _Jd(_);});}else{if(_Ja>102){return new F(function(){return _Jd(_);});}else{return (_Ja-97|0)+10|0;}}};if(_Ja<48){return new F(function(){return _Jb(_);});}else{if(_Ja>57){return new F(function(){return _Jb(_);});}else{return _Ja-48|0;}}},_Jf=function(_Jg,_Jh){while(1){var _Ji=(function(_Jj,_Jk){var _Jl=E(_Jk);if(!_Jl[0]){return [0,_Jj,_i];}else{var _Jm=E(_Jl[1])[1];if(_Jm<48){return [0,_Jj,_Jl];}else{if(_Jm>57){return [0,_Jj,_Jl];}else{_Jg=new T(function(){return [0,(imul(E(_Jj)[1],10)|0)+B(_J9(_Jm))|0];});_Jh=_Jl[2];return null;}}}})(_Jg,_Jh);if(_Ji!=null){return _Ji;}}},_Jn=new T(function(){return B(unCStr("argument list ended prematurely"));}),_Jo=new T(function(){return B(_Gm(_Jn));}),_Jp=[0,-1],_Jq=function(_Jr){return [0,E(_Jr)[1]];},_Js=function(_Jt){var _Ju=E(_Jt);switch(_Ju[0]){case 0:return new F(function(){return _Jq(_Ju[1]);});break;case 2:return new F(function(){return _vi(_Ju[2]);});break;default:return E(_Gp);}},_Jv=function(_Jw,_Jx,_Jy,_Jz,_JA){while(1){var _JB=(function(_JC,_JD,_JE,_JF,_JG){var _JH=E(_JF);if(!_JH[0]){return [0,_IV,_Jp,_JC,_JD,_JE,_i,_JG];}else{var _JI=_JH[2],_JJ=E(E(_JH[1])[1]);switch(_JJ){case 42:var _JK=new T(function(){var _JL=E(_JG);return _JL[0]==0?E(_Jo):[0,_JL[2],new T(function(){return B(_Js(_JL[1]));})];}),_JM=new T(function(){var _JN=E(_JI);if(!_JN[0]){var _JO=[0,_Jp,_i,new T(function(){return E(E(_JK)[1]);})];}else{if(E(E(_JN[1])[1])==46){var _JP=E(_JN[2]);if(!_JP[0]){var _JQ=B(_Jf(_IV,_i)),_JR=[0,_JQ[1],_JQ[2],new T(function(){return E(E(_JK)[1]);})];}else{if(E(E(_JP[1])[1])==42){var _JS=new T(function(){var _JT=E(E(_JK)[1]);return _JT[0]==0?E(_Jo):[0,_JT[2],new T(function(){return B(_Js(_JT[1]));})];}),_JU=[0,new T(function(){return E(E(_JS)[2]);}),_JP[2],new T(function(){return E(E(_JS)[1]);})];}else{var _JV=B(_Jf(_IV,_JP)),_JU=[0,_JV[1],_JV[2],new T(function(){return E(E(_JK)[1]);})];}var _JW=_JU,_JR=_JW;}var _JX=_JR;}else{var _JX=[0,_Jp,_JN,new T(function(){return E(E(_JK)[1]);})];}var _JY=_JX,_JO=_JY;}return _JO;});return [0,new T(function(){return E(E(_JK)[2]);}),new T(function(){return E(E(_JM)[1]);}),_JC,_JD,_JE,new T(function(){return E(E(_JM)[2]);}),new T(function(){return E(E(_JM)[3]);})];case 43:var _JZ=_JC,_K0=_JD;_Jy=_b;_Jz=_JI;var _K1=_JG;_Jw=_JZ;_Jx=_K0;_JA=_K1;return null;case 45:_Jw=_b;var _K0=_JD,_K2=_JE;_Jz=_JI;var _K1=_JG;_Jx=_K0;_Jy=_K2;_JA=_K1;return null;case 46:var _K3=new T(function(){var _K4=E(_JI);if(!_K4[0]){var _K5=B(_Jf(_IV,_i)),_K6=[0,_K5[1],_K5[2],_JG];}else{if(E(E(_K4[1])[1])==42){var _K7=new T(function(){var _K8=E(_JG);return _K8[0]==0?E(_Jo):[0,_K8[2],new T(function(){return B(_Js(_K8[1]));})];}),_K9=[0,new T(function(){return E(E(_K7)[2]);}),_K4[2],new T(function(){return E(E(_K7)[1]);})];}else{var _Ka=B(_Jf(_IV,_K4)),_K9=[0,_Ka[1],_Ka[2],_JG];}var _Kb=_K9,_K6=_Kb;}return _K6;});return [0,_IV,new T(function(){return E(E(_K3)[1]);}),_JC,_JD,_JE,new T(function(){return E(E(_K3)[2]);}),new T(function(){return E(E(_K3)[3]);})];case 48:var _JZ=_JC;_Jx=_b;var _K2=_JE;_Jz=_JI;var _K1=_JG;_Jw=_JZ;_Jy=_K2;_JA=_K1;return null;default:if(_JJ<48){return [0,_IV,_Jp,_JC,_JD,_JE,_JH,_JG];}else{if(_JJ>57){return [0,_IV,_Jp,_JC,_JD,_JE,_JH,_JG];}else{var _Kc=new T(function(){var _Kd=B(_Jf(_IV,_JH));return [0,_Kd[1],_Kd[2]];}),_Ke=new T(function(){var _Kf=E(E(_Kc)[2]);if(!_Kf[0]){var _Kg=[0,_Jp,_i,_JG];}else{if(E(E(_Kf[1])[1])==46){var _Kh=E(_Kf[2]);if(!_Kh[0]){var _Ki=B(_Jf(_IV,_i)),_Kj=[0,_Ki[1],_Ki[2],_JG];}else{if(E(E(_Kh[1])[1])==42){var _Kk=new T(function(){var _Kl=E(_JG);return _Kl[0]==0?E(_Jo):[0,_Kl[2],new T(function(){return B(_Js(_Kl[1]));})];}),_Km=[0,new T(function(){return E(E(_Kk)[2]);}),_Kh[2],new T(function(){return E(E(_Kk)[1]);})];}else{var _Kn=B(_Jf(_IV,_Kh)),_Km=[0,_Kn[1],_Kn[2],_JG];}var _Ko=_Km,_Kj=_Ko;}var _Kp=_Kj;}else{var _Kp=[0,_Jp,_Kf,_JG];}var _Kq=_Kp,_Kg=_Kq;}var _Kr=_Kg;return _Kr;});return [0,new T(function(){return E(E(_Kc)[1]);}),new T(function(){return E(E(_Ke)[1]);}),_JC,_JD,_JE,new T(function(){return E(E(_Ke)[2]);}),new T(function(){return E(E(_Ke)[3]);})];}}}}})(_Jw,_Jx,_Jy,_Jz,_JA);if(_JB!=null){return _JB;}}},_Ks=new T(function(){return B(unCStr("formatting string ended prematurely"));}),_Kt=new T(function(){return B(_Gm(_Ks));}),_Ku=function(_Kv,_Kw){if(!B(_M(_Kw,_Kv))){if(!B(_nN(_Kv,_IG))){var _Kx=B(_xf(_Kw,_Kv));return new F(function(){return _C(B(_Ku(_Kv,_Kx[1])),[1,new T(function(){return [0,B(_Aj(B(_7G(_Kx[2]))))];}),_i]);});}else{return E(_u4);}}else{return [1,new T(function(){return [0,B(_Aj(B(_7G(_Kw))))];}),_i];}},_Ky=[0,2],_Kz=function(_KA,_KB,_KC){var _KD=E(_KC);switch(_KD[0]){case 0:return new F(function(){return _Ku(_KA,B(_6f(E(_KD[1])[1])));});break;case 2:var _KE=_KD[2],_KF=E(_KB)[1];if(!B(_M(_KE,_IG))){return new F(function(){return _IP(_KF,B(_Ku(_KA,_KE)));});}else{return new F(function(){return _IP(_KF,B(_Ku(_KA,B(_5Z(B(_69(B(_6h(_Ky,_KD[1])))),_KE)))));});}break;default:return E(_Gp);}},_KG=[0,37],_KH=[0,16],_KI=[0,10],_KJ=[0,8],_KK=[0,43],_KL=[1,_KK,_i],_KM=[0,32],_KN=function(_KO){return new F(function(){return _Gm(new T(function(){return B(unAppCStr("bad formatting char ",[1,_KO,_i]));}));});},_KP=function(_KQ,_KR){var _KS=E(_KQ);if(!_KS){return [0];}else{var _KT=E(_KR);return _KT[0]==0?[0]:[1,_KT[1],new T(function(){return B(_KP(_KS-1|0,_KT[2]));})];}},_KU=function(_KV,_KW){var _KX=E(_KV);if(!_KX[0]){return E(_KW)[0]==0?[0]:E(_Kt);}else{var _KY=_KX[2],_KZ=E(_KX[1]);if(E(_KZ[1])==37){var _L0=function(_L1){var _L2=E(_KW);if(!_L2[0]){return E(_Jo);}else{var _L3=B(_Jv(_f,_f,_f,_KY,_L2)),_L4=_L3[2],_L5=_L3[4],_L6=E(_L3[6]);if(!_L6[0]){return E(_Kt);}else{var _L7=_L6[2],_L8=E(_L3[7]);if(!_L8[0]){return E(_Jo);}else{var _L9=_L8[1],_La=_L8[2],_Lb=E(_L6[1]),_Lc=function(_Ld,_Le){var _Lf=new T(function(){var _Lg=B(_IJ(_Le,0)),_Lh=B(_IJ(_Ld,0)),_Li=E(_L3[1])[1];if((_Lg+_Lh|0)>=_Li){var _Lj=[0];}else{var _Lk=_Li-(_Lg+_Lh|0)|0;if(_Lk>0){if(_Lk<0){var _Ll=[0];}else{var _Lm=new T(function(){return [1,new T(function(){return !E(_L5)?E(_KM):E(_IO);}),_Lm];}),_Ll=B(_KP(_Lk,_Lm));}var _Ln=_Ll,_Lo=_Ln;}else{var _Lo=[0];}var _Lp=_Lo,_Lq=_Lp,_Lr=_Lq,_Lj=_Lr;}var _Ls=_Lj,_Lt=_Ls,_Lu=_Lt,_Lv=_Lu,_Lw=_Lv;return _Lw;});return !E(_L3[3])?!E(_L5)?B(_C(_Lf,new T(function(){return B(_C(_Ld,_Le));}))):B(_C(_Ld,new T(function(){return B(_C(_Lf,_Le));}))):B(_C(_Ld,new T(function(){return B(_C(_Le,_Lf));})));},_Lx=function(_Ly,_Lz){var _LA=E(_Ly);return _LA[0]==0?!E(_L3[5])?B(_Lc(_i,_Lz)):B(_Lc(_KL,_Lz)):B(_Lc(_LA,_Lz));};switch(E(_Lb[1])){case 69:var _LB=B(_HV(69,_L4,_L9));return new F(function(){return _C(B(_Lx(_LB[1],_LB[2])),new T(function(){return B(_KU(_L7,_La));}));});break;case 71:var _LC=B(_HV(71,_L4,_L9));return new F(function(){return _C(B(_Lx(_LC[1],_LC[2])),new T(function(){return B(_KU(_L7,_La));}));});break;case 88:return new F(function(){return _C(B(_Lc(_i,new T(function(){return B(_1j(_HT,B(_Kz(_KH,_L4,_L9))));}))),new T(function(){return B(_KU(_L7,_La));}));});break;case 99:return new F(function(){return _C(B(_Lc(_i,[1,new T(function(){var _LD=E(_L9);switch(_LD[0]){case 0:var _LE=E(_LD[1])[1];if(_LE>>>0>1114111){var _LF=B(_7E(_LE));}else{var _LF=[0,_LE];}var _LG=_LF,_LH=_LG,_LI=_LH,_LJ=_LI,_LK=_LJ;break;case 2:var _LL=B(_7G(_LD[2]));if(_LL>>>0>1114111){var _LM=B(_7E(_LL));}else{var _LM=[0,_LL];}var _LN=_LM,_LO=_LN,_LP=_LO,_LK=_LP;break;default:var _LK=E(_Gp);}return _LK;}),_i])),new T(function(){return B(_KU(_L7,_La));}));});break;case 100:var _LQ=B(_IX(_L4,_L9));return new F(function(){return _C(B(_Lx(_LQ[1],_LQ[2])),new T(function(){return B(_KU(_L7,_La));}));});break;case 101:var _LR=B(_HV(101,_L4,_L9));return new F(function(){return _C(B(_Lx(_LR[1],_LR[2])),new T(function(){return B(_KU(_L7,_La));}));});break;case 102:var _LS=B(_HV(102,_L4,_L9));return new F(function(){return _C(B(_Lx(_LS[1],_LS[2])),new T(function(){return B(_KU(_L7,_La));}));});break;case 103:var _LT=B(_HV(103,_L4,_L9));return new F(function(){return _C(B(_Lx(_LT[1],_LT[2])),new T(function(){return B(_KU(_L7,_La));}));});break;case 105:var _LU=B(_IX(_L4,_L9));return new F(function(){return _C(B(_Lx(_LU[1],_LU[2])),new T(function(){return B(_KU(_L7,_La));}));});break;case 111:return new F(function(){return _C(B(_Lc(_i,new T(function(){return B(_Kz(_KJ,_L4,_L9));}))),new T(function(){return B(_KU(_L7,_La));}));});break;case 115:return new F(function(){return _C(B(_Lc(_i,new T(function(){var _LV=E(_L9);if(_LV[0]==1){var _LW=_LV[1],_LX=E(_L4)[1];if(_LX<0){var _LY=E(_LW);}else{var _LY=_LX>0?B(_KP(_LX,_LW)):[0];}var _LZ=_LY,_M0=_LZ,_M1=_M0;}else{var _M1=E(_Gp);}return _M1;}))),new T(function(){return B(_KU(_L7,_La));}));});break;case 117:return new F(function(){return _C(B(_Lc(_i,new T(function(){return B(_Kz(_KI,_L4,_L9));}))),new T(function(){return B(_KU(_L7,_La));}));});break;case 120:return new F(function(){return _C(B(_Lc(_i,new T(function(){return B(_Kz(_KH,_L4,_L9));}))),new T(function(){return B(_KU(_L7,_La));}));});break;default:return new F(function(){return _KN(_Lb);});}}}}},_M2=E(_KY);if(!_M2[0]){return new F(function(){return _L0(_);});}else{if(E(E(_M2[1])[1])==37){return [1,_KG,new T(function(){return B(_KU(_M2[2],_KW));})];}else{return new F(function(){return _L0(_);});}}}else{return [1,_KZ,new T(function(){return B(_KU(_KY,_KW));})];}}},_M3=function(_M4){return _M4>1000?B(_ni(new T(function(){var _M5=B(_mb(_M4)),_M6=_M5[1],_M7=_M5[2];if(_M7>=0){var _M8=B(_X(0,B(_nn(_M6,_M7)),_i));}else{var _M9= -_M7;if(_M9<=52){var _Ma=hs_uncheckedIShiftRA64(B(_mz(_M6)),_M9),_Mb=_Ma,_Mc=B(_X(0,B(_mj(_Mb)),_i));}else{var _Mc=!B(_M(_M6,_28))?E(_mC):E(_mE);}var _Md=_Mc,_Me=_Md,_M8=_Me;}var _Mf=_M8,_Mg=_Mf;return _Mg;}))):B(_ni(new T(function(){return B(_me(B(_KU(_mF,new T(function(){return B(_mG([1,[4,[0,_M4]],_i],_i));}))),5));})));},_Mh=function(_Mi,_Mj){return new F(function(){return (function(_Mk){while(1){var _Ml=E(_Mk);switch(_Ml[0]){case 0:var _Mm=_Ml[2]>>>0;if(((_Mi>>>0&((_Mm-1>>>0^4294967295)>>>0^_Mm)>>>0)>>>0&4294967295)==_Ml[1]){if(!((_Mi>>>0&_Mm)>>>0)){_Mk=_Ml[3];continue;}else{_Mk=_Ml[4];continue;}}else{return [0];}break;case 1:return _Mi!=_Ml[1]?[0]:[1,_Ml[2]];default:return [0];}}})(_Mj);});},_Mn=function(_Mo,_Mp,_Mq,_Mr){return new F(function(){return A(_Mo,[function(_){var _Ms=jsSetAttr(E(_Mp)[1],toJSStr(E(_Mq)),toJSStr(E(_Mr)));return _4L;}]);});},_Mt=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:181:7-12"));}),_Mu=new T(function(){return B(unCStr("lps"));}),_Mv=new T(function(){return B(unCStr("loves"));}),_Mw=new T(function(){return B(unCStr("depend"));}),_Mx=function(_My,_){return [0,_4L,_My];},_Mz=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:194:9-14"));}),_MA=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_MB=new T(function(){return B(unCStr("base"));}),_MC=new T(function(){return B(unCStr("IOException"));}),_MD=new T(function(){var _ME=hs_wordToWord64(4053623282),_MF=_ME,_MG=hs_wordToWord64(3693590983),_MH=_MG;return [0,_MF,_MH,[0,_MF,_MH,_MB,_MA,_MC],_i];}),_MI=function(_MJ){return E(_MD);},_MK=function(_ML){var _MM=E(_ML);return new F(function(){return _2p(B(_2n(_MM[1])),_MI,_MM[2]);});},_MN=new T(function(){return B(unCStr(": "));}),_MO=[0,41],_MP=new T(function(){return B(unCStr(" ("));}),_MQ=new T(function(){return B(unCStr("already exists"));}),_MR=new T(function(){return B(unCStr("does not exist"));}),_MS=new T(function(){return B(unCStr("protocol error"));}),_MT=new T(function(){return B(unCStr("failed"));}),_MU=new T(function(){return B(unCStr("invalid argument"));}),_MV=new T(function(){return B(unCStr("inappropriate type"));}),_MW=new T(function(){return B(unCStr("hardware fault"));}),_MX=new T(function(){return B(unCStr("unsupported operation"));}),_MY=new T(function(){return B(unCStr("timeout"));}),_MZ=new T(function(){return B(unCStr("resource vanished"));}),_N0=new T(function(){return B(unCStr("interrupted"));}),_N1=new T(function(){return B(unCStr("resource busy"));}),_N2=new T(function(){return B(unCStr("resource exhausted"));}),_N3=new T(function(){return B(unCStr("end of file"));}),_N4=new T(function(){return B(unCStr("illegal operation"));}),_N5=new T(function(){return B(unCStr("permission denied"));}),_N6=new T(function(){return B(unCStr("user error"));}),_N7=new T(function(){return B(unCStr("unsatisified constraints"));}),_N8=new T(function(){return B(unCStr("system error"));}),_N9=function(_Na,_Nb){switch(E(_Na)){case 0:return new F(function(){return _C(_MQ,_Nb);});break;case 1:return new F(function(){return _C(_MR,_Nb);});break;case 2:return new F(function(){return _C(_N1,_Nb);});break;case 3:return new F(function(){return _C(_N2,_Nb);});break;case 4:return new F(function(){return _C(_N3,_Nb);});break;case 5:return new F(function(){return _C(_N4,_Nb);});break;case 6:return new F(function(){return _C(_N5,_Nb);});break;case 7:return new F(function(){return _C(_N6,_Nb);});break;case 8:return new F(function(){return _C(_N7,_Nb);});break;case 9:return new F(function(){return _C(_N8,_Nb);});break;case 10:return new F(function(){return _C(_MS,_Nb);});break;case 11:return new F(function(){return _C(_MT,_Nb);});break;case 12:return new F(function(){return _C(_MU,_Nb);});break;case 13:return new F(function(){return _C(_MV,_Nb);});break;case 14:return new F(function(){return _C(_MW,_Nb);});break;case 15:return new F(function(){return _C(_MX,_Nb);});break;case 16:return new F(function(){return _C(_MY,_Nb);});break;case 17:return new F(function(){return _C(_MZ,_Nb);});break;default:return new F(function(){return _C(_N0,_Nb);});}},_Nc=[0,125],_Nd=new T(function(){return B(unCStr("{handle: "));}),_Ne=function(_Nf,_Ng,_Nh,_Ni,_Nj,_Nk){var _Nl=new T(function(){var _Nm=new T(function(){return B(_N9(_Ng,new T(function(){var _Nn=E(_Ni);return _Nn[0]==0?E(_Nk):B(_C(_MP,new T(function(){return B(_C(_Nn,[1,_MO,_Nk]));})));})));}),_No=E(_Nh);return _No[0]==0?E(_Nm):B(_C(_No,new T(function(){return B(_C(_MN,_Nm));})));}),_Np=E(_Nj);if(!_Np[0]){var _Nq=E(_Nf);if(!_Nq[0]){return E(_Nl);}else{var _Nr=E(_Nq[1]);return _Nr[0]==0?B(_C(_Nd,new T(function(){return B(_C(_Nr[1],[1,_Nc,new T(function(){return B(_C(_MN,_Nl));})]));}))):B(_C(_Nd,new T(function(){return B(_C(_Nr[1],[1,_Nc,new T(function(){return B(_C(_MN,_Nl));})]));})));}}else{return new F(function(){return _C(_Np[1],new T(function(){return B(_C(_MN,_Nl));}));});}},_Ns=function(_Nt){var _Nu=E(_Nt);return new F(function(){return _Ne(_Nu[1],_Nu[2],_Nu[3],_Nu[4],_Nu[6],_i);});},_Nv=function(_Nw,_Nx){var _Ny=E(_Nw);return new F(function(){return _Ne(_Ny[1],_Ny[2],_Ny[3],_Ny[4],_Ny[6],_Nx);});},_Nz=function(_NA,_NB){return new F(function(){return _2K(_Nv,_NA,_NB);});},_NC=function(_ND,_NE,_NF){var _NG=E(_NE);return new F(function(){return _Ne(_NG[1],_NG[2],_NG[3],_NG[4],_NG[6],_NF);});},_NH=[0,_NC,_Ns,_Nz],_NI=new T(function(){return [0,_MI,_NH,_NJ,_MK];}),_NJ=function(_NK){return [0,_NI,_NK];},_NL=7,_NM=function(_NN){return [0,_6F,_NL,_i,_NN,_6F,_6F];},_NO=function(_NP,_){return new F(function(){return die(new T(function(){return B(_NJ(new T(function(){return B(_NM(_NP));})));}));});},_NQ=function(_NR,_){return new F(function(){return _NO(_NR,_);});},_NS=new T(function(){return B(unCStr("disabled"));}),_NT=new T(function(){return B(unCStr("-btn"));}),_NU=new T(function(){return B(unCStr("-box"));}),_NV=new T(function(){return B(unCStr("-num"));}),_NW=function(_NX,_NY,_){var _NZ=B(A(_NX,[_])),_O0=_NZ;return new F(function(){return A(_NY,[_O0,_]);});},_O1=function(_O2,_){return _O2;},_O3=function(_O4,_O5,_){var _O6=B(A(_O4,[_])),_O7=_O6;return new F(function(){return A(_O5,[_]);});},_O8=[0,_NW,_O3,_O1,_NQ],_O9=[0,_O8,_5p],_Oa=function(_Ob){return E(E(_Ob)[1]);},_Oc=function(_Od){return E(E(_Od)[1]);},_Oe=function(_Of){return E(E(_Of)[2]);},_Og=function(_Oh){return E(E(_Oh)[3]);},_Oi=function(_Oj,_Ok){var _Ol=new T(function(){return B(_Oa(_Oj));});return function(_Om){return new F(function(){return A(new T(function(){return B(_Oc(_Ol));}),[new T(function(){return B(A(_Oe,[_Oj,_Ok]));}),function(_On){return new F(function(){return A(new T(function(){return B(_Og(_Ol));}),[[0,_On,_Om]]);});}]);});};},_Oo=function(_Op){return new F(function(){return _Oi(_O9,_Op);});},_Oq=new T(function(){return B(_n7(_i));}),_Or=new T(function(){return B(unCStr("-icon"));}),_Os=new T(function(){return B(unCStr("-cost"));}),_Ot=new T(function(){return B(unCStr("innerHTML"));}),_Ou=new T(function(){return B(_lF("(function(e,c){e.removeAttribute(c);})"));}),_Ov=function(_Ow){return function(_Ox,_){var _Oy=B(A(new T(function(){return B(A(_Ou,[E(E(_Ow)[1])]));}),[E(toJSStr(E(_Ox))),_])),_Oz=_Oy;return _4L;};},_OA=[0,10],_OB=function(_OC){return E(_OA);},_OD=function(_){var _OE=jsEval("Date.now()"),_OF=_OE;return new T(function(){var _OG=B(_gB(B(_3v(_eg,new T(function(){return fromJSStr(_OF);})))));return _OG[0]==0?B(err(_2b)):E(_OG[2])[0]==0?E(_OG[1]):B(err(_29));});},_OH=function(_OI){return E(_OI);},_OJ=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:212:3-8"));}),_OK=new T(function(){return B(unCStr("multiplier 1"));}),_OL=new T(function(){return B(unCStr("alerts"));}),_OM=new T(function(){return B(unCStr("<div id=\"alert-%d\" class=\"alert alert-info fade in\" role=\"alert\">  <button type=\"button\" class=\"close\" data-dismiss=\"alert\"><span aria-hidden=\"true\">&times;</span><span class=\"sr-only\">Close</span></button>%s </div>"));}),_ON=new T(function(){return B(unCStr("\').alert(\'close\')"));}),_OO=new T(function(){return B(unCStr(" could be found!"));}),_OP=function(_OQ){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_C(_OQ,_OO));}))));});},_OR=function(_OS,_){var _OT=E(_OL),_OU=jsFind(toJSStr(_OT)),_OV=_OU,_OW=E(_OV);if(!_OW[0]){return new F(function(){return _OP(_OT);});}else{var _OX=B(_OD(_)),_OY=_OX,_OZ=jsSet(E(_OW[1])[1],toJSStr(E(_Ot)),toJSStr(B(_1j(_OH,B(_KU(_OM,new T(function(){return B(_mG([1,[1,new T(function(){return B(_1j(_OH,_OS));})],[1,[2,_IG,_OY],_i]],_i));}))))))),_P0=jsSetTimeout(5000,function(_){var _P1=jsEval(toJSStr(B(unAppCStr("$(\'#alert-",new T(function(){return B(_C(B(_X(0,_OY,_i)),_ON));}))))),_P2=_P1;return _4L;});return _4L;}},_P3=new T(function(){return B(unCStr("\u3092\u8d85\u3048\u308b"));}),_P4=new T(function(){return B(unCStr("\u5b9f\u7e3e\u7372\u5f97\uff1a "));}),_P5=new T(function(){return B(unCStr("\u4f9d\u5b58\u5ea6\u304c"));}),_P6=new T(function(){return B(unCStr("<tr><td>%s</td><td>%s</td></tr>"));}),_P7=new T(function(){return B(unCStr("<thead><tr><th>\u5b9f\u7e3e\u540d</th><th>\u5185\u5bb9</th></tr></thead>"));}),_P8=new T(function(){return B(unCStr("</tbody>"));}),_P9=new T(function(){return B(unCStr("achievements"));}),_Pa=function(_Pb,_){var _Pc=jsFind(toJSStr(E(_P9))),_Pd=_Pc,_Pe=E(_Pd);if(!_Pe[0]){return new F(function(){return _NQ(_OJ,_);});}else{var _Pf=jsSet(E(_Pe[1])[1],toJSStr(E(_Ot)),toJSStr(B(_C(_P7,new T(function(){return B(unAppCStr("<tbody>",new T(function(){var _Pg=function(_Ph){var _Pi=E(_Ph);if(!_Pi[0]){return [0];}else{var _Pj=E(_Pi[1]),_Pk=function(_Pl){var _Pm=E(_Pl);return _Pm[0]==0?E(new T(function(){return B(_Pg(_Pi[2]));})):[1,_Pm[1],new T(function(){return B(_Pk(_Pm[2]));})];};return new F(function(){return _Pk(B(_KU(_P6,new T(function(){return B(_mG([1,[1,new T(function(){return B(_1j(_OH,_Pj[2]));})],[1,[1,new T(function(){return B(_1j(_OH,_Pj[1]));})],_i]],_i));}))));});}};return B(_C(B(_Pg(B(_1j(function(_Pn){var _Po=E(_Pn),_Pp=E(_Po[2])[1],_Pq=B(_Mh(E(_Po[1])[1],new T(function(){return E(E(_Pb)[7]);})));if(!_Pq[0]){return [0,_Pp,_i];}else{var _Pr=E(_Pq[1]);return _Pr[0]==0?[0,_Pp,_i]:[0,_Pp,_Pr[1]];}},_Ps)))),_P8));})));})))));return [0,_4L,_Pb];}},_Pt=function(_Pu,_Pv,_Pw){return function(_Px,_){var _Py=E(_Px),_Pz=E(_Py[3]);if(_Pz[1]<=E(new T(function(){return [0,B(_o7(_Pu))];}))[1]){return [0,_4L,_Py];}else{var _PA=B(_OR(new T(function(){return B(_C(_P4,_Pv));}),_)),_PB=_PA;return new F(function(){return _Pa([0,_Py[1],_Py[2],_Pz,_Py[4],_Py[5],_Py[6],new T(function(){return B(_g2(E(_Pw)[1],[1,new T(function(){return B(_C(_P5,new T(function(){return B(_C(B(_X(0,_Pu,_i)),_P3));})));})],_Py[7]));}),_Py[8]],_);});}};},_PC=[0,1],_PD=function(_PE){return new F(function(){return _Pt(_PC,_OK,_PE);});},_PF=new T(function(){return [0,_OK,_PD];}),_PG=new T(function(){return B(unCStr("multiplier 10"));}),_PH=function(_PE){return new F(function(){return _Pt(_OA,_PG,_PE);});},_PI=new T(function(){return [0,_PG,_PH];}),_PJ=new T(function(){return B(unCStr("\u4f9d\u5b58\u6ce8\u610f\u5831"));}),_PK=[0,100],_PL=function(_PE){return new F(function(){return _Pt(_PK,_PJ,_PE);});},_PM=new T(function(){return [0,_PJ,_PL];}),_PN=new T(function(){return B(unCStr("\u4f9d\u5b58\u30c9\u30e9\u30c3\u30b0"));}),_PO=[0,1000],_PP=function(_PE){return new F(function(){return _Pt(_PO,_PN,_PE);});},_PQ=new T(function(){return [0,_PN,_PP];}),_PR=new T(function(){return B(unCStr("\u307e\u305a\u306f\u304a\u53cb\u9054\u304b\u3089"));}),_PS=new T(function(){return B(unCStr("\u597d\u611f\u5ea6\u304c"));}),_PT=function(_PU,_PV,_PW){return function(_PX,_){var _PY=E(_PX),_PZ=E(_PY[2]);if(_PZ[1]<=E(new T(function(){return [0,B(_o7(_PU))];}))[1]){return [0,_4L,_PY];}else{var _Q0=B(_OR(new T(function(){return B(_C(_P4,_PV));}),_)),_Q1=_Q0;return new F(function(){return _Pa([0,_PY[1],_PZ,_PY[3],_PY[4],_PY[5],_PY[6],new T(function(){return B(_g2(E(_PW)[1],[1,new T(function(){return B(_C(_PS,new T(function(){return B(_C(B(_X(0,_PU,_i)),_P3));})));})],_PY[7]));}),_PY[8]],_);});}};},_Q2=function(_PE){return new F(function(){return _PT(_PC,_PR,_PE);});},_Q3=new T(function(){return [0,_PR,_Q2];}),_Q4=new T(function(){return B(unCStr("\u4e8c\u7d1a\u30d5\u30e9\u30b0\u5efa\u7bc9\u58eb"));}),_Q5=[0,5],_Q6=function(_PE){return new F(function(){return _PT(_Q5,_Q4,_PE);});},_Q7=new T(function(){return [0,_Q4,_Q6];}),_Q8=new T(function(){return B(unCStr("\u4e00\u7d1a\u30d5\u30e9\u30b0\u5efa\u7bc9\u58eb"));}),_Q9=function(_PE){return new F(function(){return _PT(_OA,_Q8,_PE);});},_Qa=new T(function(){return [0,_Q8,_Q9];}),_Qb=new T(function(){return B(unCStr("\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_Qc=[0,50],_Qd=function(_PE){return new F(function(){return _PT(_Qc,_Qb,_PE);});},_Qe=new T(function(){return [0,_Qb,_Qd];}),_Qf=new T(function(){return B(unCStr("\uff8a\uff72\uff8a\uff9f\uff70\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_Qg=function(_PE){return new F(function(){return _PT(_PK,_Qf,_PE);});},_Qh=new T(function(){return [0,_Qf,_Qg];}),_Qi=new T(function(){return B(unCStr("\u5168\u958b\u3089\u3076\u3071\u308f\u30fc"));}),_Qj=[0,250],_Qk=function(_PE){return new F(function(){return _PT(_Qj,_Qi,_PE);});},_Ql=new T(function(){return [0,_Qi,_Qk];}),_Qm=new T(function(){return B(unCStr("\u30a2\u30a4\u3068\u306e\u906d\u9047"));}),_Qn=new T(function(){return B(unCStr("\u611b\u60c5\u304c"));}),_Qo=function(_Qp,_Qq,_Qr){return function(_Qs,_){var _Qt=E(_Qs),_Qu=E(_Qt[1]);if(_Qu[1]<=E(new T(function(){return [0,B(_o7(_Qp))];}))[1]){return [0,_4L,_Qt];}else{var _Qv=B(_OR(new T(function(){return B(_C(_P4,_Qq));}),_)),_Qw=_Qv;return new F(function(){return _Pa([0,_Qu,_Qt[2],_Qt[3],_Qt[4],_Qt[5],_Qt[6],new T(function(){return B(_g2(E(_Qr)[1],[1,new T(function(){return B(_C(_Qn,new T(function(){return B(_C(B(_X(0,_Qp,_i)),_P3));})));})],_Qt[7]));}),_Qt[8]],_);});}};},_Qx=function(_PE){return new F(function(){return _Qo(_PC,_Qm,_PE);});},_Qy=new T(function(){return [0,_Qm,_Qx];}),_Qz=new T(function(){return B(unCStr("\u611b\u3055\u308c\u6c17\u5206"));}),_QA=function(_PE){return new F(function(){return _Qo(_PK,_Qz,_PE);});},_QB=new T(function(){return [0,_Qz,_QA];}),_QC=new T(function(){return B(unCStr("\u611b\u30e9\u30d6\u30e6\u30fc"));}),_QD=[0,10000],_QE=function(_PE){return new F(function(){return _Qo(_QD,_QC,_PE);});},_QF=new T(function(){return [0,_QC,_QE];}),_QG=new T(function(){return B(unCStr("\u611b\u3055\u3093\u306e\u611b\u304c\u91cd\u3044"));}),_QH=[0,10000000],_QI=function(_PE){return new F(function(){return _Qo(_QH,_QG,_PE);});},_QJ=new T(function(){return [0,_QG,_QI];}),_Ps=new T(function(){var _QK=B(_sv(1,2147483647));return _QK[0]==0?[0]:[1,[0,_QK[1],_Qy],new T(function(){var _QL=E(_QK[2]);return _QL[0]==0?[0]:[1,[0,_QL[1],_QB],new T(function(){var _QM=E(_QL[2]);return _QM[0]==0?[0]:[1,[0,_QM[1],_QF],new T(function(){var _QN=E(_QM[2]);return _QN[0]==0?[0]:[1,[0,_QN[1],_QJ],new T(function(){var _QO=E(_QN[2]);return _QO[0]==0?[0]:[1,[0,_QO[1],_Q3],new T(function(){var _QP=E(_QO[2]);return _QP[0]==0?[0]:[1,[0,_QP[1],_Q7],new T(function(){var _QQ=E(_QP[2]);return _QQ[0]==0?[0]:[1,[0,_QQ[1],_Qa],new T(function(){var _QR=E(_QQ[2]);return _QR[0]==0?[0]:[1,[0,_QR[1],_Qe],new T(function(){var _QS=E(_QR[2]);return _QS[0]==0?[0]:[1,[0,_QS[1],_Qh],new T(function(){var _QT=E(_QS[2]);return _QT[0]==0?[0]:[1,[0,_QT[1],_Ql],new T(function(){var _QU=E(_QT[2]);return _QU[0]==0?[0]:[1,[0,_QU[1],_PF],new T(function(){var _QV=E(_QU[2]);return _QV[0]==0?[0]:[1,[0,_QV[1],_PI],new T(function(){var _QW=E(_QV[2]);return _QW[0]==0?[0]:[1,[0,_QW[1],_PM],new T(function(){var _QX=E(_QW[2]);return _QX[0]==0?[0]:[1,[0,_QX[1],_PQ],_i];})];})];})];})];})];})];})];})];})];})];})];})];})];}),_QY=[0,0],_QZ=function(_R0,_R1,_R2,_R3,_R4,_){var _R5=jsFind(toJSStr(E(_P9))),_R6=_R5,_R7=E(_R6);if(!_R7[0]){return new F(function(){return _NQ(_OJ,_);});}else{var _R8=jsSet(E(_R7[1])[1],toJSStr(E(_Ot)),toJSStr(B(_C(_P7,new T(function(){return B(unAppCStr("<tbody>",new T(function(){var _R9=function(_Ra){var _Rb=E(_Ra);if(!_Rb[0]){return [0];}else{var _Rc=E(_Rb[1]),_Rd=function(_Re){var _Rf=E(_Re);return _Rf[0]==0?E(new T(function(){return B(_R9(_Rb[2]));})):[1,_Rf[1],new T(function(){return B(_Rd(_Rf[2]));})];};return new F(function(){return _Rd(B(_KU(_P6,new T(function(){return B(_mG([1,[1,new T(function(){return B(_1j(_OH,_Rc[2]));})],[1,[1,new T(function(){return B(_1j(_OH,_Rc[1]));})],_i]],_i));}))));});}};return B(_C(B(_R9(B(_1j(function(_Rg){var _Rh=E(_Rg),_Ri=E(_Rh[2])[1],_Rj=B(_Mh(E(_Rh[1])[1],_R3));if(!_Rj[0]){return [0,_Ri,_i];}else{var _Rk=E(_Rj[1]);return _Rk[0]==0?[0,_Ri,_i]:[0,_Ri,_Rk[1]];}},_Ps)))),_P8));})));})))));return [0,_4L,[0,_QY,_QY,_QY,_R0,_R1,_R2,_R3,_R4]];}},_Rl=function(_Rm,_){var _Rn=B(_OD(_)),_Ro=_Rn;return new F(function(){return _QZ(_Ro,_28,_f,new T(function(){return E(E(_Rm)[7]);}),_g1,_);});},_Rp=[0,_OB,_Rl],_Rq=new T(function(){var _Rr=B(_sS(-1,-2));return _Rr[0]==0?[0]:[1,[0,_Rr[1],_Rp],_i];}),_Rs=function(_Rt,_Ru){var _Rv=E(_Ru);return _Rv[0]==0?E(_Rq):[1,[0,[0,_Rt],_Rv[1]],new T(function(){var _Rw=E(_Rt);if(_Rw==2147483647){var _Rx=E(_Rq);}else{var _Rx=B(_Rs(_Rw+1|0,_Rv[2]));}return _Rx;})];},_Ry=function(_Rz,_RA,_RB){return [1,[0,[0,_Rz],_RA],new T(function(){var _RC=E(_Rz);if(_RC==2147483647){var _RD=E(_Rq);}else{var _RD=B(_Rs(_RC+1|0,_RB));}return _RD;})];},_RE=[0,3],_RF=[0,2],_RG=new T(function(){return B(_nV(_RE,_RF));}),_RH=function(_RI,_RJ){if(_RJ>=0){var _RK=function(_RL){var _RM=B(_mb(_RI*_RL)),_RN=_RM[1],_RO=_RM[2];if(_RO>=0){return new F(function(){return _nn(_RN,_RO);});}else{var _RP= -_RO;if(_RP<=52){var _RQ=hs_uncheckedIShiftRA64(B(_mz(_RN)),_RP),_RR=_RQ;return new F(function(){return _mj(_RR);});}else{return !B(_M(_RN,_28))?E(_28):E(_mD);}}},_RS=E(_RJ);if(!_RS){return new F(function(){return _RK(1);});}else{return new F(function(){return _RK(B(_GF(E(_RG)[1],_RS)));});}}else{return E(_Gw);}},_RT=function(_RU){return new F(function(){return _RH(200000,E(_RU)[1]);});},_RV=function(_RW,_){return [0,_4L,new T(function(){var _RX=E(_RW);return [0,_RX[1],new T(function(){return [0,E(_RX[2])[1]+50];}),_RX[3],_RX[4],_RX[5],_RX[6],_RX[7],_RX[8]];})];},_RY=[0,_RT,_RV],_RZ=function(_S0){return new F(function(){return _RH(5000000,E(_S0)[1]);});},_S1=function(_S2,_){return [0,_4L,new T(function(){var _S3=E(_S2);return [0,_S3[1],new T(function(){return [0,E(_S3[2])[1]+100];}),_S3[3],_S3[4],_S3[5],_S3[6],_S3[7],_S3[8]];})];},_S4=[0,_RZ,_S1],_S5=[1,_S4,_i],_S6=[1,_RY,_S5],_S7=function(_S8){return new F(function(){return _RH(20000,E(_S8)[1]);});},_S9=function(_Sa,_){return [0,_4L,new T(function(){var _Sb=E(_Sa);return [0,_Sb[1],new T(function(){return [0,E(_Sb[2])[1]+15];}),_Sb[3],_Sb[4],_Sb[5],_Sb[6],_Sb[7],_Sb[8]];})];},_Sc=[0,_S7,_S9],_Sd=[1,_Sc,_S6],_Se=function(_Sf){return new F(function(){return _RH(1000,E(_Sf)[1]);});},_Sg=function(_Sh,_){return [0,_4L,new T(function(){var _Si=E(_Sh);return [0,_Si[1],new T(function(){return [0,E(_Si[2])[1]+5];}),_Si[3],_Si[4],_Si[5],_Si[6],_Si[7],_Si[8]];})];},_Sj=[0,_Se,_Sg],_Sk=[1,_Sj,_Sd],_Sl=function(_Sm){return new F(function(){return _RH(100,E(_Sm)[1]);});},_Sn=function(_So,_){return [0,_4L,new T(function(){var _Sp=E(_So);return [0,_Sp[1],new T(function(){return [0,E(_Sp[2])[1]+1];}),_Sp[3],_Sp[4],_Sp[5],_Sp[6],_Sp[7],_Sp[8]];})];},_Sq=[0,_Sl,_Sn],_Sr=[1,_Sq,_Sk],_Ss=function(_St){return new F(function(){return _RH(1,E(_St)[1]);});},_Su=function(_Sv,_){return [0,_4L,new T(function(){var _Sw=E(_Sv);return [0,_Sw[1],new T(function(){return [0,E(_Sw[2])[1]+0.1];}),_Sw[3],_Sw[4],_Sw[5],_Sw[6],_Sw[7],_Sw[8]];})];},_Sx=[0,_Ss,_Su],_Sy=new T(function(){return B(_Ry(1,_Sx,_Sr));}),_Sz=function(_,_SA){var _SB=jsFind(toJSStr(E(_Mu))),_SC=_SB,_SD=E(_SC);if(!_SD[0]){return new F(function(){return _NQ(_Mt,_);});}else{var _SE=E(_Ot),_SF=toJSStr(_SE),_SG=E(E(_SA)[2]),_SH=jsSet(E(_SD[1])[1],_SF,toJSStr(B(_M3(E(_SG[2])[1])))),_SI=jsFind(toJSStr(E(_Mv))),_SJ=_SI,_SK=E(_SJ);if(!_SK[0]){return new F(function(){return _NQ(_Mt,_);});}else{var _SL=E(_SG[1])[1],_SM=jsSet(E(_SK[1])[1],_SF,toJSStr(B(_M3(_SL)))),_SN=jsFind(toJSStr(E(_Mw))),_SO=_SN,_SP=E(_SO);if(!_SP[0]){return new F(function(){return _NQ(_Mt,_);});}else{var _SQ=jsSet(E(_SP[1])[1],_SF,toJSStr(B(_M3(E(_SG[3])[1])))),_SR=function(_SS){var _ST=E(_SS);return _ST[0]==0?E(_Mx):function(_SU,_){var _SV=B(A(new T(function(){var _SW=E(_ST[1]),_SX=_SW[1],_SY=E(_SW[2])[1],_SZ=new T(function(){var _T0=E(_SX)[1];return _T0<=0?B(unAppCStr("item-sp-",new T(function(){if(_T0<0){var _T1=B(_7y(0, -_T0,_i));}else{var _T1=B(_7y(0,_T0,_i));}var _T2=_T1;return _T2;}))):B(unAppCStr("item-",new T(function(){return B(_7y(0,_T0,_i));})));}),_T3=new T(function(){var _T4=B(_Mh(E(_SX)[1],_SG[8]));return _T4[0]==0?E(_l7):E(_T4[1]);});return function(_T5,_){var _T6=B(A(new T(function(){if(E(_SX)[1]<=0){var _T7=E(_Mx);}else{var _T8=function(_,_T9,_Ta){var _Tb=B(_C(_SZ,_NV)),_Tc=jsFind(toJSStr(_Tb)),_Td=_Tc,_Te=E(_Td);if(!_Te[0]){return new F(function(){return _OP(_Tb);});}else{var _Tf=jsSet(E(_Te[1])[1],toJSStr(_SE),toJSStr(B(_7y(0,E(_T3)[1],_i))));return [0,_4L,_Ta];}},_T7=function(_Tg,_){var _Th=E(new T(function(){return B(_C(_SZ,_NU));})),_Ti=jsFind(toJSStr(_Th)),_Tj=_Ti,_Tk=E(_Tj);if(!_Tk[0]){return new F(function(){return _OP(_Th);});}else{var _Tl=jsFind(toJSStr(E(new T(function(){return B(_C(_SZ,_Or));})))),_Tm=_Tl,_Tn=E(_Tm);if(!_Tn[0]){return new F(function(){return _NQ(_Mz,_);});}else{var _To=jsGet(E(_Tn[1])[1],toJSStr(_SE)),_Tp=_To,_Tq=E(_Tk[1])[1],_Tr=E(_T3)[1];if(_Tr>0){var _Ts=new T(function(){return fromJSStr(_Tp);}),_Tt=function(_Tu){return _Tu>1?[1,_Ts,new T(function(){return B(_Tt(_Tu-1|0));})]:E([1,_Ts,_i]);},_Tv=jsSet(_Tq,_SF,toJSStr(B(_n7(B(_Tt(_Tr))))));return new F(function(){return _T8(_,_4L,_Tg);});}else{var _Tw=jsSet(_Tq,_SF,toJSStr(E(_Oq)));return new F(function(){return _T8(_,_4L,_Tg);});}}}};}var _Tx=_T7,_Ty=_Tx;return _Ty;}),[_T5,_])),_Tz=_T6,_TA=E(new T(function(){return B(_C(_SZ,_NT));})),_TB=jsFind(toJSStr(_TA)),_TC=_TB,_TD=E(_TC);if(!_TD[0]){return new F(function(){return _OP(_TA);});}else{var _TE=_TD[1];if(!E(new T(function(){return B(_o7(B(A(_SY,[_T3]))))<=_SL;}))){var _TF=B(A(_Mn,[_Oo,_TE,_NS,_NS,new T(function(){return E(E(_Tz)[2]);}),_])),_TG=_TF,_TH=B(_C(_SZ,_Os)),_TI=jsFind(toJSStr(_TH)),_TJ=_TI,_TK=E(_TJ);if(!_TK[0]){return new F(function(){return _OP(_TH);});}else{var _TL=jsSet(E(_TK[1])[1],toJSStr(_SE),toJSStr(B(_ni(new T(function(){return B(_X(0,B(A(_SY,[_T3])),_i));})))));return [0,_4L,new T(function(){return E(E(_TG)[2]);})];}}else{var _TM=B(A(_Ov,[_TE,_NS,_])),_TN=_TM,_TO=B(_C(_SZ,_Os)),_TP=jsFind(toJSStr(_TO)),_TQ=_TP,_TR=E(_TQ);if(!_TR[0]){return new F(function(){return _OP(_TO);});}else{var _TS=jsSet(E(_TR[1])[1],toJSStr(_SE),toJSStr(B(_ni(new T(function(){return B(_X(0,B(A(_SY,[_T3])),_i));})))));return [0,_4L,new T(function(){return E(E(_Tz)[2]);})];}}}};}),[_SU,_])),_TT=_SV;return new F(function(){return A(new T(function(){return B(_SR(_ST[2]));}),[new T(function(){return E(E(_TT)[2]);}),_]);});};};return new F(function(){return A(_SR,[_Sy,_SG,_]);});}}}},_TU=new T(function(){return B(unCStr(" is not an element of the map"));}),_TV=function(_TW){return new F(function(){return err(B(unAppCStr("IntMap.!: key ",new T(function(){return B(_C(B(_7y(0,_TW,_i)),_TU));}))));});},_TX=function(_TY,_TZ){var _U0=new T(function(){return B(_TV(_TZ));});return new F(function(){return (function(_U1){while(1){var _U2=E(_U1);switch(_U2[0]){case 0:var _U3=_U2[2]>>>0;if(((_TZ>>>0&((_U3-1>>>0^4294967295)>>>0^_U3)>>>0)>>>0&4294967295)==_U2[1]){if(!((_TZ>>>0&_U3)>>>0)){_U1=_U2[3];continue;}else{_U1=_U2[4];continue;}}else{return E(_U0);}break;case 1:return _TZ!=_U2[1]?E(_U0):E(_U2[2]);default:return E(_U0);}}})(_TY);});},_U4=function(_U5,_U6){return new F(function(){return (function(_U7){while(1){var _U8=E(_U7);switch(_U8[0]){case 0:var _U9=_U8[2]>>>0;if(((_U5>>>0&((_U9-1>>>0^4294967295)>>>0^_U9)>>>0)>>>0&4294967295)==_U8[1]){if(!((_U5>>>0&_U9)>>>0)){_U7=_U8[3];continue;}else{_U7=_U8[4];continue;}}else{return false;}break;case 1:return _U5==_U8[1];default:return false;}}})(_U6);});},_Ua=function(_Ub){var _Uc=E(_Ub);return _Uc[0]==0?E(_Mx):function(_Ud,_){var _Ue=B(A(new T(function(){var _Uf=E(_Uc[1]),_Ug=_Uf[1],_Uh=new T(function(){return B(A(E(_Uf[2])[2],[_Ug]));});return function(_Ui,_){var _Uj=E(_Ui),_Uk=_Uj[7],_Ul=E(_Ug)[1];return !B(_U4(_Ul,_Uk))?B(A(_Uh,[_Uj,_])):B(_TX(_Uk,_Ul))[0]==0?B(A(_Uh,[_Uj,_])):[0,_4L,_Uj];};}),[_Ud,_])),_Um=_Ue;return new F(function(){return A(new T(function(){return B(_Ua(_Uc[2]));}),[new T(function(){return E(E(_Um)[2]);}),_]);});};},_Un=new T(function(){return B(_Ua(_Ps));}),_Uo=new T(function(){return B(unCStr("\u653e\u7f6e\u671f\u9593 +"));}),_Up=function(_Uq){return new F(function(){return err(B(unAppCStr("docFocused: ",new T(function(){return fromJSStr(_Uq);}))));});},_Ur=new T(function(){return B(unCStr("false"));}),_Us=new T(function(){return B(unCStr("true"));}),_Ut=new T(function(){return B(unCStr("document.hasFocus()"));}),_Uu=function(_Uv,_){var _Uw=jsEval(toJSStr(E(_Ut))),_Ux=_Uw,_Uy=strEq(_Ux,toJSStr(E(_Us))),_Uz=_Uy;if(!E(_Uz)){var _UA=strEq(_Ux,toJSStr(E(_Ur))),_UB=_UA;if(!E(_UB)){return new F(function(){return _Up(_Ux);});}else{var _UC=B(A(_Un,[new T(function(){var _UD=E(_Uv),_UE=_UD[2],_UF=_UD[5],_UG=new T(function(){return [0,B(_o7(_UF))/1000/60/120];});return [0,new T(function(){return [0,E(_UD[1])[1]+E(_UE)[1]/30];}),new T(function(){return [0,E(_UE)[1]+E(_UG)[1]];}),new T(function(){var _UH=E(_UD[3])[1]-E(_UG)[1];return _UH>0?[0,_UH]:E(_QY);}),_UD[4],_UF,_f,_UD[7],_UD[8]];}),_])),_UI=_UC;return new F(function(){return _Sz(_,_UI);});}}else{var _UJ=E(_Uv),_UK=_UJ[1],_UL=_UJ[2],_UM=_UJ[3],_UN=_UJ[4],_UO=_UJ[5],_UP=_UJ[7],_UQ=_UJ[8];if(!E(_UJ[6])){var _UR=B(_OR(new T(function(){return B(_C(_Uo,new T(function(){return B(_X(0,_UO,_i));})));}),_)),_US=_UR,_UT=B(A(_Un,[[0,new T(function(){return [0,E(_UK)[1]+E(_UL)[1]/30];}),_UL,new T(function(){return [0,E(_UM)[1]+B(_o7(_UO))/1000/60/15];}),_UN,_UO,_b,_UP,_UQ],_])),_UU=_UT;return new F(function(){return _Sz(_,_UU);});}else{var _UV=B(A(_Un,[[0,new T(function(){return [0,E(_UK)[1]+E(_UL)[1]/30];}),_UL,new T(function(){return [0,E(_UM)[1]+B(_o7(_UO))/1000/60/15];}),_UN,_UO,_b,_UP,_UQ],_])),_UW=_UV;return new F(function(){return _Sz(_,_UW);});}}},_UX=new T(function(){return B(_sS(-1,-2));}),_UY=new T(function(){return B(_sv(1,2147483647));}),_UZ=function(_V0,_V1,_V2,_V3){var _V4=E(_V3);switch(_V4[0]){case 0:var _V5=_V4[1],_V6=_V4[2],_V7=_V4[3],_V8=_V4[4],_V9=_V6>>>0;if(((_V1>>>0&((_V9-1>>>0^4294967295)>>>0^_V9)>>>0)>>>0&4294967295)==_V5){return (_V1>>>0&_V9)>>>0==0?[0,_V5,_V6,E(B(_UZ(_V0,_V1,_V2,_V7))),E(_V8)]:[0,_V5,_V6,E(_V7),E(B(_UZ(_V0,_V1,_V2,_V8)))];}else{var _Va=(_V1>>>0^_V5>>>0)>>>0,_Vb=(_Va|_Va>>>1)>>>0,_Vc=(_Vb|_Vb>>>2)>>>0,_Vd=(_Vc|_Vc>>>4)>>>0,_Ve=(_Vd|_Vd>>>8)>>>0,_Vf=(_Ve|_Ve>>>16)>>>0,_Vg=(_Vf^_Vf>>>1)>>>0&4294967295,_Vh=_Vg>>>0;return (_V1>>>0&_Vh)>>>0==0?[0,(_V1>>>0&((_Vh-1>>>0^4294967295)>>>0^_Vh)>>>0)>>>0&4294967295,_Vg,E([1,_V1,_V2]),E(_V4)]:[0,(_V1>>>0&((_Vh-1>>>0^4294967295)>>>0^_Vh)>>>0)>>>0&4294967295,_Vg,E(_V4),E([1,_V1,_V2])];}break;case 1:var _Vi=_V4[1];if(_V1!=_Vi){var _Vj=(_V1>>>0^_Vi>>>0)>>>0,_Vk=(_Vj|_Vj>>>1)>>>0,_Vl=(_Vk|_Vk>>>2)>>>0,_Vm=(_Vl|_Vl>>>4)>>>0,_Vn=(_Vm|_Vm>>>8)>>>0,_Vo=(_Vn|_Vn>>>16)>>>0,_Vp=(_Vo^_Vo>>>1)>>>0&4294967295,_Vq=_Vp>>>0;return (_V1>>>0&_Vq)>>>0==0?[0,(_V1>>>0&((_Vq-1>>>0^4294967295)>>>0^_Vq)>>>0)>>>0&4294967295,_Vp,E([1,_V1,_V2]),E(_V4)]:[0,(_V1>>>0&((_Vq-1>>>0^4294967295)>>>0^_Vq)>>>0)>>>0&4294967295,_Vp,E(_V4),E([1,_V1,_V2])];}else{return [1,_V1,new T(function(){return B(A(_V0,[[0,_V1],_V2,_V4[2]]));})];}break;default:return [1,_V1,_V2];}},_Vr=new T(function(){return [0,"click"];}),_Vs=[0,1],_Vt=new T(function(){return B(_3s("main.hs:(286,1)-(303,24)|function btnEvents"));}),_Vu=function(_Vv,_Vw,_Vx){return new F(function(){return _v8(_Vw,_Vx);});},_Vy=new T(function(){return B(_gt(_g1,_Sy));}),_Vz=function(_VA,_VB,_){var _VC=E(_VB);if(!_VC[0]){return E(_Vt);}else{var _VD=E(_VC[1])[1],_VE=function(_,_VF){var _VG=E(_VF);if(!_VG[0]){return _4L;}else{var _VH=E(_Vr)[1],_VI=jsSetCB(E(_VG[1])[1],_VH,function(_VJ,_VK,_){var _VL=E(_VA)[1],_VM=rMV(_VL),_VN=_VM,_VO=E(new T(function(){return B(_TX(_Vy,_VD));})),_VP=B(A(_VO[2],[new T(function(){var _VQ=E(_VN),_VR=new T(function(){return B(_UZ(_Vu,_VD,_Vs,_VQ[8]));});return [0,new T(function(){return [0,E(_VQ[1])[1]-B(_o7(B(A(_VO[1],[new T(function(){return [0,B(_TX(_VR,_VD))[1]-1|0];})]))))];}),_VQ[2],_VQ[3],_VQ[4],_VQ[5],_VQ[6],_VQ[7],_VR];}),_])),_VS=_VP,_VT=B(_Pa(new T(function(){return E(E(_VS)[2]);}),_)),_VU=_VT,_=wMV(_VL,new T(function(){return E(E(_VU)[2]);})),_VV=rMV(_VL),_VW=_VV,_VX=E(_VW),_VY=jsLog(toJSStr(B(A(_lh,[0,_VX[1],_VX[2],_VX[3],_VX[4],_VX[5],_VX[6],_VX[7],_VX[8],_i]))));return _4L;}),_VZ=_VI,_W0=function(_W1,_W2,_){var _W3=E(_W2);if(!_W3[0]){return E(_Vt);}else{var _W4=E(_W3[1])[1],_W5=function(_,_W6){var _W7=E(_W6);if(!_W7[0]){return _4L;}else{var _W8=jsSetCB(E(_W7[1])[1],_VH,function(_W9,_Wa,_){var _Wb=E(_W1)[1],_Wc=rMV(_Wb),_Wd=_Wc,_We=E(new T(function(){return B(_TX(_Vy,_W4));})),_Wf=B(A(_We[2],[new T(function(){var _Wg=E(_Wd),_Wh=new T(function(){return B(_UZ(_Vu,_W4,_Vs,_Wg[8]));});return [0,new T(function(){return [0,E(_Wg[1])[1]-B(_o7(B(A(_We[1],[new T(function(){return [0,B(_TX(_Wh,_W4))[1]-1|0];})]))))];}),_Wg[2],_Wg[3],_Wg[4],_Wg[5],_Wg[6],_Wg[7],_Wh];}),_])),_Wi=_Wf,_Wj=B(_Pa(new T(function(){return E(E(_Wi)[2]);}),_)),_Wk=_Wj,_=wMV(_Wb,new T(function(){return E(E(_Wk)[2]);})),_Wl=rMV(_Wb),_Wm=_Wl,_Wn=E(_Wm),_Wo=jsLog(toJSStr(B(A(_lh,[0,_Wn[1],_Wn[2],_Wn[3],_Wn[4],_Wn[5],_Wn[6],_Wn[7],_Wn[8],_i]))));return _4L;}),_Wp=_W8;return new F(function(){return _W0(_W1,_W3[2],_);});}};if(_W4<=0){var _Wq=jsFind(toJSStr(B(unAppCStr("item-sp-",new T(function(){if(_W4<0){var _Wr=B(_C(B(_7y(0, -_W4,_i)),_NT));}else{var _Wr=B(_C(B(_7y(0,_W4,_i)),_NT));}var _Ws=_Wr;return _Ws;}))))),_Wt=_Wq;return new F(function(){return _W5(_,_Wt);});}else{var _Wu=jsFind(toJSStr(B(unAppCStr("item-",new T(function(){return B(_C(B(_7y(0,_W4,_i)),_NT));}))))),_Wv=_Wu;return new F(function(){return _W5(_,_Wv);});}}};return new F(function(){return _W0(_VA,_VC[2],_);});}};if(_VD<=0){var _Ww=jsFind(toJSStr(B(unAppCStr("item-sp-",new T(function(){if(_VD<0){var _Wx=B(_C(B(_7y(0, -_VD,_i)),_NT));}else{var _Wx=B(_C(B(_7y(0,_VD,_i)),_NT));}var _Wy=_Wx;return _Wy;}))))),_Wz=_Ww;return new F(function(){return _VE(_,_Wz);});}else{var _WA=jsFind(toJSStr(B(unAppCStr("item-",new T(function(){return B(_C(B(_7y(0,_VD,_i)),_NT));}))))),_WB=_WA;return new F(function(){return _VE(_,_WB);});}}},_WC=new T(function(){return B(unCStr("Aichan"));}),_WD=new T(function(){return [0,toJSStr(_i)];}),_WE=[0,93],_WF=[1,_WE,_i],_WG=new T(function(){return [0,toJSStr(_WF)];}),_WH=[0,125],_WI=[1,_WH,_i],_WJ=new T(function(){return [0,toJSStr(_WI)];}),_WK=[0,58],_WL=[1,_WK,_i],_WM=new T(function(){return [0,toJSStr(_WL)];}),_WN=[0,44],_WO=[1,_WN,_i],_WP=new T(function(){return [0,toJSStr(_WO)];}),_WQ=new T(function(){return [0,"false"];}),_WR=function(_WS){var _WT=jsShow(E(_WS)[1]),_WU=_WT;return [0,_WU];},_WV=function(_WW){var _WX=jsStringify(E(_WW)[1]),_WY=_WX;return [0,_WY];},_WZ=new T(function(){return [0,"null"];}),_X0=[0,91],_X1=[1,_X0,_i],_X2=new T(function(){return [0,toJSStr(_X1)];}),_X3=[0,123],_X4=[1,_X3,_i],_X5=new T(function(){return [0,toJSStr(_X4)];}),_X6=[0,34],_X7=[1,_X6,_i],_X8=new T(function(){return [0,toJSStr(_X7)];}),_X9=new T(function(){return [0,"true"];}),_Xa=function(_Xb,_Xc){var _Xd=E(_Xc);switch(_Xd[0]){case 0:return [0,new T(function(){return B(_WR(_Xd[1]));}),_Xb];case 1:return [0,new T(function(){return B(_WV(_Xd[1]));}),_Xb];case 2:return !E(_Xd[1])?[0,_WQ,_Xb]:[0,_X9,_Xb];case 3:var _Xe=E(_Xd[1]);return _Xe[0]==0?[0,_X2,[1,_WG,_Xb]]:[0,_X2,new T(function(){var _Xf=B(_Xa(new T(function(){var _Xg=function(_Xh){var _Xi=E(_Xh);return _Xi[0]==0?E([1,_WG,_Xb]):[1,_WP,new T(function(){var _Xj=B(_Xa(new T(function(){return B(_Xg(_Xi[2]));}),_Xi[1]));return [1,_Xj[1],_Xj[2]];})];};return B(_Xg(_Xe[2]));}),_Xe[1]));return [1,_Xf[1],_Xf[2]];})];case 4:var _Xk=E(_Xd[1]);if(!_Xk[0]){return [0,_X5,[1,_WJ,_Xb]];}else{var _Xl=E(_Xk[1]);return [0,_X5,[1,new T(function(){return B(_WV(_Xl[1]));}),[1,_WM,new T(function(){var _Xm=B(_Xa(new T(function(){var _Xn=function(_Xo){var _Xp=E(_Xo);if(!_Xp[0]){return E([1,_WJ,_Xb]);}else{var _Xq=E(_Xp[1]);return [1,_WP,[1,_X8,[1,_Xq[1],[1,_X8,[1,_WM,new T(function(){var _Xr=B(_Xa(new T(function(){return B(_Xn(_Xp[2]));}),_Xq[2]));return [1,_Xr[1],_Xr[2]];})]]]]];}};return B(_Xn(_Xk[2]));}),_Xl[2]));return [1,_Xm[1],_Xm[2]];})]]];}break;default:return [0,_WZ,_Xb];}},_Xs=function(_Xt){var _Xu=jsCat(new T(function(){var _Xv=B(_Xa(_i,_Xt));return [1,_Xv[1],_Xv[2]];}),E(_WD)[1]),_Xw=_Xu;return E(_Xw);},_Xx=new T(function(){return [0,"(function(k,v) {localStorage.setItem(k,v);})"];}),_Xy=function(_Xz,_XA){return function(_XB,_){var _XC=B(A(new T(function(){return B(A(_lF,[E(_Xx)[1],E(toJSStr(E(_XA)))]));}),[E(B(_Xs(B(A(new T(function(){return B(_eU(_Xz));}),[_XB]))))),_])),_XD=_XC;return _4L;};},_XE=new T(function(){return B(_Xy(_ha,_WC));}),_XF=function(_XG,_){var _XH=B(A(_XE,[_XG,_])),_XI=_XH;return new F(function(){return _Pa(_XG,_);});},_XJ=[1,_iV,_i],_XK=function(_XL,_XM){while(1){var _XN=E(_XL);if(!_XN[0]){var _XO=_XN[1],_XP=E(_XM);if(!_XP[0]){var _XQ=_XP[1],_XR=subC(_XO,_XQ);if(!E(_XR[2])){return [0,_XR[1]];}else{_XL=[1,I_fromInt(_XO)];_XM=[1,I_fromInt(_XQ)];continue;}}else{_XL=[1,I_fromInt(_XO)];_XM=_XP;continue;}}else{var _XS=E(_XM);if(!_XS[0]){_XL=_XN;_XM=[1,I_fromInt(_XS[1])];continue;}else{return [1,I_sub(_XN[1],_XS[1])];}}}},_XT=function(_XU,_){var _XV=jsEval(toJSStr(B(unAppCStr("document.title = ",[1,_iV,new T(function(){return B(_iX(B(_1j(_OH,B(_KU(_mF,new T(function(){return B(_mG([1,[4,new T(function(){return E(E(_XU)[1]);})],_i],_i));}))))),_XJ));})])))),_XW=_XV,_XX=E(_XU);if(!E(_XX[6])){return [0,_4L,_XX];}else{var _XY=B(_OD(_)),_XZ=_XY;return [0,_4L,[0,_XX[1],_XX[2],_XX[3],_XZ,new T(function(){return B(_XK(_XZ,_XX[4]));}),_b,_XX[7],_XX[8]]];}},_Y0=function(_Y1,_Y2,_Y3,_){var _Y4=rMV(_Y2),_Y5=_Y4,_Y6=B(A(_Y3,[_Y5,_])),_Y7=_Y6,_=wMV(_Y2,new T(function(){return E(E(_Y7)[2]);})),_Y8=jsSetTimeout(_Y1,function(_){var _Y9=B(_Y0(_Y1,_Y2,_Y3,_)),_Ya=_Y9;return _4L;});return new F(function(){return rMV(_Y2);});},_Yb=function(_){var _=0,_Yc=jsMkStdout(),_Yd=_Yc;return [0,_Yd];},_Ye=new T(function(){return B(_lB(_Yb));}),_Yf=function(_){var _Yg=B(_OD(_)),_Yh=_Yg,_Yi=B(_lN(_ha,_WC,_)),_Yj=_Yi,_Yk=nMV(new T(function(){var _Yl=E(_Yj);return _Yl[0]==0?[0,_QY,_QY,_QY,_Yh,_28,_f,_g1,_g1]:E(_Yl[1]);})),_Ym=_Yk,_Yn=B(_Y0(33,_Ym,_Uu,_)),_Yo=_Yn,_Yp=B(_m6(_Ye,B(_lw(_Yo)),_)),_Yq=_Yp,_Yr=B(_Y0(1000,_Ym,_XT,_)),_Ys=_Yr,_Yt=B(_m6(_Ye,B(_lw(_Ys)),_)),_Yu=_Yt,_Yv=B(_Y0(60000,_Ym,_XF,_)),_Yw=_Yv,_Yx=B(_m6(_Ye,B(_lw(_Yw)),_)),_Yy=_Yx,_Yz=[0,_Ym],_YA=B(_Vz(_Yz,_UY,_)),_YB=_YA;return new F(function(){return _Vz(_Yz,_UX,_);});},_YC=function(_){return new F(function(){return _Yf(_);});};
var hasteMain = function() {B(A(_YC, [0]));};window.onload = hasteMain;
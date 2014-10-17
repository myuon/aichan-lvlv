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
    this.f = f;
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

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
        if(t.f != __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            t.x = f();
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f = __blackhole;
        f = fun();
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
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
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
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
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

function jsQuerySelector(elem, query) {
  if (!elem || typeof elem.querySelector !== 'function') {
    return [0];
  }

  var e = elem.querySelector(query);
  return e ? [1, [0, e]] : [0];
}

function jsQuerySelectorAll(elem, query) {
  var els = [0],
      len, nl, i;

  if (!elem || typeof elem.querySelectorAll !== 'function') {
    return els;
  }

  nl = elem.querySelectorAll(query);
  len = nl.length;

  for (i=len-1; i >= 0; --i) {
    els = [1, [0, nl[i]], els];
  }

  return els;
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

var _0=[8,_],_1=new T(function(){return B(unCStr("achievements"));}),_2=new T(function(){return [0,toJSStr(E(_1))];}),_3=new T(function(){return B(unCStr("lastFocus"));}),_4=new T(function(){return [0,toJSStr(E(_3))];}),_5=new T(function(){return B(unCStr("depend"));}),_6=new T(function(){return [0,toJSStr(E(_5))];}),_7=new T(function(){return B(unCStr("lps"));}),_8=new T(function(){return [0,toJSStr(E(_7))];}),_9=new T(function(){return B(unCStr("loves"));}),_a=new T(function(){return [0,toJSStr(E(_9))];}),_b=new T(function(){return B(unCStr("lpsCoeff"));}),_c=new T(function(){return [0,toJSStr(E(_b))];}),_d=new T(function(){return B(unCStr("dependCoeff"));}),_e=new T(function(){return [0,toJSStr(E(_d))];}),_f=new T(function(){return B(unCStr("maxLoves"));}),_g=new T(function(){return [0,toJSStr(E(_f))];}),_h=new T(function(){return B(unCStr("itemsWithMap"));}),_i=new T(function(){return [0,toJSStr(E(_h))];}),_j=function(_k){return [0,toJSStr(E(_k))];},_l=function(_m){return [1,new T(function(){return B(_j(_m));})];},_n=new T(function(){return [0,"value"];}),_o=true,_p=[2,_o],_q=new T(function(){return [0,"hasValue"];}),_r=[0,_q,_p],_s=false,_t=[2,_s],_u=[0,_q,_t],_v=[0],_w=[1,_u,_v],_x=[4,_w],_y=function(_z,_A){while(1){var _B=(function(_C,_D){var _E=E(_D);if(!_E[0]){_z=[1,[3,[1,[1,new T(function(){return [0,toJSStr(_E[2])];})],[1,new T(function(){var _F=E(_E[3]);return _F[0]==0?E(_x):[4,[1,_r,[1,[0,_n,new T(function(){return B(_l(_F[1]));})],_v]]];}),_v]]],new T(function(){return B(_y(_C,_E[5]));})];_A=_E[4];return null;}else{return E(_C);}})(_z,_A);if(_B!=null){return _B;}}},_G=function(_H){return [0,new T(function(){return [0,E(_H)[1]];})];},_I=function(_J,_K){while(1){var _L=(function(_M,_N){var _O=E(_N);if(!_O[0]){_J=[1,[3,[1,[1,new T(function(){return [0,toJSStr(_O[2])];})],[1,new T(function(){return B(_G(_O[3]));}),_v]]],new T(function(){return B(_I(_M,_O[5]));})];_K=_O[4];return null;}else{return E(_M);}})(_J,_K);if(_L!=null){return _L;}}},_P=function(_Q,_R){var _S=E(_Q);return _S[0]==0?E(_R):[1,_S[1],new T(function(){return B(_P(_S[2],_R));})];},_T=function(_U){while(1){var _V=E(_U);if(!_V[0]){_U=[1,I_fromInt(_V[1])];continue;}else{return new F(function(){return I_toString(_V[1]);});}}},_W=function(_X,_Y){return new F(function(){return _P(fromJSStr(B(_T(_X))),_Y);});},_Z=function(_10,_11){var _12=E(_10);if(!_12[0]){var _13=_12[1],_14=E(_11);return _14[0]==0?_13<_14[1]:I_compareInt(_14[1],_13)>0;}else{var _15=_12[1],_16=E(_11);return _16[0]==0?I_compareInt(_15,_16[1])<0:I_compare(_15,_16[1])<0;}},_17=[0,41],_18=[0,40],_19=[0,0],_1a=function(_1b,_1c,_1d){return _1b<=6?B(_W(_1c,_1d)):!B(_Z(_1c,_19))?B(_W(_1c,_1d)):[1,_18,new T(function(){return B(_P(fromJSStr(B(_T(_1c))),[1,_17,_1d]));})];},_1e=function(_1f,_1g,_1h,_1i,_1j,_1k,_1l,_1m,_1n){return [1,[0,_a,[0,_1f]],[1,[0,_8,[0,_1g]],[1,[0,_6,[0,_1h]],[1,[0,_4,[1,new T(function(){return [0,toJSStr(B(_1a(0,_1i,_v)))];})]],[1,[0,_2,[3,new T(function(){return B(_y(_v,_1j));})]],[1,[0,_i,[3,new T(function(){return B(_I(_v,_1k));})]],[1,[0,_g,[0,_1l]],[1,[0,_e,[0,_1m]],[1,[0,_c,[0,_1n]],_v]]]]]]]]];},_1o=function(_1p){var _1q=E(_1p);return [4,B(_1e(_1q[1],_1q[2],_1q[3],_1q[4],_1q[6],_1q[7],_1q[8],_1q[9],_1q[10]))];},_1r=function(_1s,_1t){var _1u=E(_1t);return _1u[0]==0?[0]:[1,new T(function(){return B(A(_1s,[_1u[1]]));}),new T(function(){return B(_1r(_1s,_1u[2]));})];},_1v=function(_1w){return [3,new T(function(){return B(_1r(_1o,_1w));})];},_1x=new T(function(){return B(unCStr("Tried to deserialize long string to a Char"));}),_1y=[0,_1x],_1z=new T(function(){return B(unCStr("Tried to deserialize a non-string to a Char"));}),_1A=[0,_1z],_1B=function(_1C){var _1D=E(_1C);if(_1D[0]==1){var _1E=fromJSStr(E(_1D[1])[1]);return _1E[0]==0?E(_1y):E(_1E[2])[0]==0?[1,_1E[1]]:E(_1y);}else{return E(_1A);}},_1F=new T(function(){return B(unCStr("Tried to deserialize a non-JSString to a JSString"));}),_1G=[0,_1F],_1H=function(_1I){return new F(function(){return fromJSStr(E(_1I)[1]);});},_1J=function(_1K){var _1L=E(_1K);return _1L[0]==1?[1,new T(function(){return B(_1H(_1L[1]));})]:E(_1G);},_1M=function(_1N){return [1,new T(function(){return [0,toJSStr([1,_1N,_v])];})];},_1O=[0,_1M,_l,_1B,_1J],_1P=function(_1Q){return E(E(_1Q)[2]);},_1R=function(_1S,_1T){return [3,new T(function(){return B(_1r(new T(function(){return B(_1P(_1S));}),_1T));})];},_1U=[1,_v],_1V=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_1W=[0,_1V],_1X=function(_1Y){return E(E(_1Y)[4]);},_1Z=function(_20,_21){var _22=E(_21);if(_22[0]==3){var _23=function(_24){var _25=E(_24);if(!_25[0]){return E(_1U);}else{var _26=B(A(new T(function(){return B(_1X(_20));}),[_25[1]]));if(!_26[0]){return [0,_26[1]];}else{var _27=B(_23(_25[2]));return _27[0]==0?[0,_27[1]]:[1,[1,_26[1],_27[1]]];}}};return new F(function(){return _23(_22[1]);});}else{return E(_1W);}},_28=function(_29){return [0,new T(function(){return B(_1P(_29));}),function(_2a){return new F(function(){return _1R(_29,_2a);});},new T(function(){return B(_1X(_29));}),function(_2a){return new F(function(){return _1Z(_29,_2a);});}];},_2b=new T(function(){return B(_28(_1O));}),_2c=function(_2d){return E(E(_2d)[1]);},_2e=function(_2f,_2g){var _2h=E(_2g);return _2h[0]==0?E(_x):[4,[1,_r,[1,[0,_n,new T(function(){return B(A(_2c,[_2f,_2h[1]]));})],_v]]];},_2i=function(_2j,_2k){return [3,new T(function(){return B(_1r(function(_2a){return new F(function(){return _2e(_2j,_2a);});},_2k));})];},_2l=function(_2m,_2n){var _2o=strEq(E(_2m)[1],E(_2n)[1]),_2p=_2o;return E(_2p)==0?true:false;},_2q=function(_2r,_2s){var _2t=strEq(E(_2r)[1],E(_2s)[1]),_2u=_2t;return E(_2u)==0?false:true;},_2v=[0,_2q,_2l],_2w=[0],_2x=[1,_2w],_2y=new T(function(){return B(unCStr("Tried to deserialize a non-Bool to a Bool"));}),_2z=[0,_2y],_2A=new T(function(){return B(unCStr("Key not found"));}),_2B=[0,_2A],_2C=new T(function(){return B(unCStr("Tried to do lookup on non-object!"));}),_2D=[0,_2C],_2E=function(_2F){return E(E(_2F)[1]);},_2G=function(_2H,_2I,_2J){while(1){var _2K=E(_2J);if(!_2K[0]){return [0];}else{var _2L=E(_2K[1]);if(!B(A(_2E,[_2H,_2I,_2L[1]]))){_2J=_2K[2];continue;}else{return [1,_2L[2]];}}}},_2M=function(_2N){return E(E(_2N)[3]);},_2O=function(_2P,_2Q){var _2R=E(_2Q);if(_2R[0]==4){var _2S=_2R[1],_2T=B(_2G(_2v,_q,_2S));if(!_2T[0]){return E(_2B);}else{var _2U=E(_2T[1]);if(_2U[0]==2){if(!E(_2U[1])){return E(_2x);}else{var _2V=B(_2G(_2v,_n,_2S));if(!_2V[0]){return E(_2B);}else{var _2W=B(A(_2M,[_2P,_2V[1]]));return _2W[0]==0?[0,_2W[1]]:[1,[1,_2W[1]]];}}}else{return E(_2z);}}}else{return E(_2D);}},_2X=[1,_v],_2Y=[0,_1V],_2Z=function(_30,_31){var _32=E(_31);if(_32[0]==3){var _33=function(_34){var _35=E(_34);if(!_35[0]){return E(_2X);}else{var _36=B(_2O(_30,_35[1]));if(!_36[0]){return [0,_36[1]];}else{var _37=B(_33(_35[2]));return _37[0]==0?[0,_37[1]]:[1,[1,_36[1],_37[1]]];}}};return new F(function(){return _33(_32[1]);});}else{return E(_2Y);}},_38=function(_39){return [0,function(_2a){return new F(function(){return _2e(_39,_2a);});},function(_2a){return new F(function(){return _2i(_39,_2a);});},function(_2a){return new F(function(){return _2O(_39,_2a);});},function(_2a){return new F(function(){return _2Z(_39,_2a);});}];},_3a=new T(function(){return B(_38(_2b));}),_3b=[1,_v],_3c=[0,_1V],_3d=new T(function(){return B(unCStr("Tried to deserialize a non-array into a pair!"));}),_3e=[0,_3d],_3f=function(_3g,_3h,_3i){var _3j=E(_3i);if(_3j[0]==3){var _3k=E(_3j[1]);if(!_3k[0]){return E(_3e);}else{var _3l=E(_3k[2]);if(!_3l[0]){return E(_3e);}else{if(!E(_3l[2])[0]){var _3m=B(A(_2M,[_3g,_3k[1]]));if(!_3m[0]){return [0,_3m[1]];}else{var _3n=B(A(_2M,[_3h,_3l[1]]));return _3n[0]==0?[0,_3n[1]]:[1,[0,_3m[1],_3n[1]]];}}else{return E(_3e);}}}}else{return E(_3e);}},_3o=function(_3p,_3q,_3r){var _3s=E(_3r);if(_3s[0]==3){var _3t=function(_3u){var _3v=E(_3u);if(!_3v[0]){return E(_3b);}else{var _3w=B(_3f(_3p,_3q,_3v[1]));if(!_3w[0]){return [0,_3w[1]];}else{var _3x=B(_3t(_3v[2]));return _3x[0]==0?[0,_3x[1]]:[1,[1,_3w[1],_3x[1]]];}}};return new F(function(){return _3t(_3s[1]);});}else{return E(_3c);}},_3y=function(_3z){return [3,new T(function(){return B(_1r(_G,_3z));})];},_3A=new T(function(){return B(unCStr("The given Number can\'t be represented as an Int"));}),_3B=[0,_3A],_3C=new T(function(){return B(unCStr("Tried to deserialize a non-Number to an Int"));}),_3D=[0,_3C],_3E=function(_3F){var _3G=E(_3F);if(!_3G[0]){var _3H=E(_3G[1])[1],_3I=_3H&4294967295;return _3I!=_3H?E(_3B):[1,[0,_3I]];}else{return E(_3D);}},_3J=[0,_1V],_3K=[1,_v],_3L=[0,_3A],_3M=[0,_3C],_3N=function(_3O){var _3P=E(_3O);if(!_3P[0]){return E(_3K);}else{var _3Q=E(_3P[1]);if(!_3Q[0]){var _3R=E(_3Q[1])[1],_3S=_3R&4294967295;if(_3S!=_3R){return E(_3L);}else{var _3T=B(_3N(_3P[2]));return _3T[0]==0?[0,_3T[1]]:[1,[1,[0,_3S],_3T[1]]];}}else{return E(_3M);}}},_3U=function(_3V){var _3W=E(_3V);return _3W[0]==3?B(_3N(_3W[1])):E(_3J);},_3X=[0,_G,_3y,_3E,_3U],_3Y=function(_3Z,_40){while(1){var _41=E(_3Z);if(!_41[0]){return E(_40)[0]==0?1:0;}else{var _42=E(_40);if(!_42[0]){return 2;}else{var _43=E(_41[1])[1],_44=E(_42[1])[1];if(_43!=_44){return _43>_44?2:0;}else{_3Z=_41[2];_40=_42[2];continue;}}}}},_45=[1],_46=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_47=function(_48){return new F(function(){return err(_46);});},_49=new T(function(){return B(_47(_));}),_4a=function(_4b,_4c,_4d,_4e){var _4f=E(_4d);if(!_4f[0]){var _4g=_4f[1],_4h=E(_4e);if(!_4h[0]){var _4i=_4h[1],_4j=_4h[2],_4k=_4h[3];if(_4i<=(imul(3,_4g)|0)){return [0,(1+_4g|0)+_4i|0,E(E(_4b)),_4c,E(_4f),E(_4h)];}else{var _4l=E(_4h[4]);if(!_4l[0]){var _4m=_4l[1],_4n=_4l[2],_4o=_4l[3],_4p=_4l[4],_4q=E(_4h[5]);if(!_4q[0]){var _4r=_4q[1];if(_4m>=(imul(2,_4r)|0)){var _4s=function(_4t){var _4u=E(_4b),_4v=E(_4l[5]);return _4v[0]==0?[0,(1+_4g|0)+_4i|0,E(_4n),_4o,E([0,(1+_4g|0)+_4t|0,E(_4u),_4c,E(_4f),E(_4p)]),E([0,(1+_4r|0)+_4v[1]|0,E(_4j),_4k,E(_4v),E(_4q)])]:[0,(1+_4g|0)+_4i|0,E(_4n),_4o,E([0,(1+_4g|0)+_4t|0,E(_4u),_4c,E(_4f),E(_4p)]),E([0,1+_4r|0,E(_4j),_4k,E(_45),E(_4q)])];},_4w=E(_4p);return _4w[0]==0?B(_4s(_4w[1])):B(_4s(0));}else{return [0,(1+_4g|0)+_4i|0,E(_4j),_4k,E([0,(1+_4g|0)+_4m|0,E(E(_4b)),_4c,E(_4f),E(_4l)]),E(_4q)];}}else{return E(_49);}}else{return E(_49);}}}else{return [0,1+_4g|0,E(E(_4b)),_4c,E(_4f),E(_45)];}}else{var _4x=E(_4e);if(!_4x[0]){var _4y=_4x[1],_4z=_4x[2],_4A=_4x[3],_4B=_4x[5],_4C=E(_4x[4]);if(!_4C[0]){var _4D=_4C[1],_4E=_4C[2],_4F=_4C[3],_4G=_4C[4],_4H=E(_4B);if(!_4H[0]){var _4I=_4H[1];if(_4D>=(imul(2,_4I)|0)){var _4J=function(_4K){var _4L=E(_4b),_4M=E(_4C[5]);return _4M[0]==0?[0,1+_4y|0,E(_4E),_4F,E([0,1+_4K|0,E(_4L),_4c,E(_45),E(_4G)]),E([0,(1+_4I|0)+_4M[1]|0,E(_4z),_4A,E(_4M),E(_4H)])]:[0,1+_4y|0,E(_4E),_4F,E([0,1+_4K|0,E(_4L),_4c,E(_45),E(_4G)]),E([0,1+_4I|0,E(_4z),_4A,E(_45),E(_4H)])];},_4N=E(_4G);return _4N[0]==0?B(_4J(_4N[1])):B(_4J(0));}else{return [0,1+_4y|0,E(_4z),_4A,E([0,1+_4D|0,E(E(_4b)),_4c,E(_45),E(_4C)]),E(_4H)];}}else{return [0,3,E(_4E),_4F,E([0,1,E(E(_4b)),_4c,E(_45),E(_45)]),E([0,1,E(_4z),_4A,E(_45),E(_45)])];}}else{var _4O=E(_4B);return _4O[0]==0?[0,3,E(_4z),_4A,E([0,1,E(E(_4b)),_4c,E(_45),E(_45)]),E(_4O)]:[0,2,E(E(_4b)),_4c,E(_45),E(_4x)];}}else{return [0,1,E(E(_4b)),_4c,E(_45),E(_45)];}}},_4P=function(_4Q,_4R){return [0,1,E(E(_4Q)),_4R,E(_45),E(_45)];},_4S=function(_4T,_4U,_4V){var _4W=E(_4V);if(!_4W[0]){return new F(function(){return _4a(_4W[2],_4W[3],_4W[4],B(_4S(_4T,_4U,_4W[5])));});}else{return new F(function(){return _4P(_4T,_4U);});}},_4X=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_4Y=function(_4Z){return new F(function(){return err(_4X);});},_50=new T(function(){return B(_4Y(_));}),_51=function(_52,_53,_54,_55){var _56=E(_55);if(!_56[0]){var _57=_56[1],_58=E(_54);if(!_58[0]){var _59=_58[1],_5a=_58[2],_5b=_58[3];if(_59<=(imul(3,_57)|0)){return [0,(1+_59|0)+_57|0,E(E(_52)),_53,E(_58),E(_56)];}else{var _5c=E(_58[4]);if(!_5c[0]){var _5d=_5c[1],_5e=E(_58[5]);if(!_5e[0]){var _5f=_5e[1],_5g=_5e[2],_5h=_5e[3],_5i=_5e[4];if(_5f>=(imul(2,_5d)|0)){var _5j=function(_5k){var _5l=E(_5e[5]);return _5l[0]==0?[0,(1+_59|0)+_57|0,E(_5g),_5h,E([0,(1+_5d|0)+_5k|0,E(_5a),_5b,E(_5c),E(_5i)]),E([0,(1+_57|0)+_5l[1]|0,E(E(_52)),_53,E(_5l),E(_56)])]:[0,(1+_59|0)+_57|0,E(_5g),_5h,E([0,(1+_5d|0)+_5k|0,E(_5a),_5b,E(_5c),E(_5i)]),E([0,1+_57|0,E(E(_52)),_53,E(_45),E(_56)])];},_5m=E(_5i);return _5m[0]==0?B(_5j(_5m[1])):B(_5j(0));}else{return [0,(1+_59|0)+_57|0,E(_5a),_5b,E(_5c),E([0,(1+_57|0)+_5f|0,E(E(_52)),_53,E(_5e),E(_56)])];}}else{return E(_50);}}else{return E(_50);}}}else{return [0,1+_57|0,E(E(_52)),_53,E(_45),E(_56)];}}else{var _5n=E(_54);if(!_5n[0]){var _5o=_5n[1],_5p=_5n[2],_5q=_5n[3],_5r=_5n[5],_5s=E(_5n[4]);if(!_5s[0]){var _5t=_5s[1],_5u=E(_5r);if(!_5u[0]){var _5v=_5u[1],_5w=_5u[2],_5x=_5u[3],_5y=_5u[4];if(_5v>=(imul(2,_5t)|0)){var _5z=function(_5A){var _5B=E(_5u[5]);return _5B[0]==0?[0,1+_5o|0,E(_5w),_5x,E([0,(1+_5t|0)+_5A|0,E(_5p),_5q,E(_5s),E(_5y)]),E([0,1+_5B[1]|0,E(E(_52)),_53,E(_5B),E(_45)])]:[0,1+_5o|0,E(_5w),_5x,E([0,(1+_5t|0)+_5A|0,E(_5p),_5q,E(_5s),E(_5y)]),E([0,1,E(E(_52)),_53,E(_45),E(_45)])];},_5C=E(_5y);return _5C[0]==0?B(_5z(_5C[1])):B(_5z(0));}else{return [0,1+_5o|0,E(_5p),_5q,E(_5s),E([0,1+_5v|0,E(E(_52)),_53,E(_5u),E(_45)])];}}else{return [0,3,E(_5p),_5q,E(_5s),E([0,1,E(E(_52)),_53,E(_45),E(_45)])];}}else{var _5D=E(_5r);return _5D[0]==0?[0,3,E(_5D[2]),_5D[3],E([0,1,E(_5p),_5q,E(_45),E(_45)]),E([0,1,E(E(_52)),_53,E(_45),E(_45)])]:[0,2,E(E(_52)),_53,E(_5n),E(_45)];}}else{return [0,1,E(E(_52)),_53,E(_45),E(_45)];}}},_5E=function(_5F,_5G,_5H){var _5I=E(_5H);if(!_5I[0]){return new F(function(){return _51(_5I[2],_5I[3],B(_5E(_5F,_5G,_5I[4])),_5I[5]);});}else{return new F(function(){return _4P(_5F,_5G);});}},_5J=function(_5K,_5L,_5M,_5N,_5O,_5P,_5Q){return new F(function(){return _51(_5N,_5O,B(_5E(_5K,_5L,_5P)),_5Q);});},_5R=function(_5S,_5T,_5U,_5V,_5W,_5X,_5Y,_5Z){var _60=E(_5U);if(!_60[0]){var _61=_60[1],_62=_60[2],_63=_60[3],_64=_60[4],_65=_60[5];if((imul(3,_61)|0)>=_5V){if((imul(3,_5V)|0)>=_61){return [0,(_61+_5V|0)+1|0,E(E(_5S)),_5T,E(_60),E([0,_5V,E(_5W),_5X,E(_5Y),E(_5Z)])];}else{return new F(function(){return _4a(_62,_63,_64,B(_5R(_5S,_5T,_65,_5V,_5W,_5X,_5Y,_5Z)));});}}else{return new F(function(){return _51(_5W,_5X,B(_66(_5S,_5T,_61,_62,_63,_64,_65,_5Y)),_5Z);});}}else{return new F(function(){return _5J(_5S,_5T,_5V,_5W,_5X,_5Y,_5Z);});}},_66=function(_67,_68,_69,_6a,_6b,_6c,_6d,_6e){var _6f=E(_6e);if(!_6f[0]){var _6g=_6f[1],_6h=_6f[2],_6i=_6f[3],_6j=_6f[4],_6k=_6f[5];if((imul(3,_69)|0)>=_6g){if((imul(3,_6g)|0)>=_69){return [0,(_69+_6g|0)+1|0,E(E(_67)),_68,E([0,_69,E(_6a),_6b,E(_6c),E(_6d)]),E(_6f)];}else{return new F(function(){return _4a(_6a,_6b,_6c,B(_5R(_67,_68,_6d,_6g,_6h,_6i,_6j,_6k)));});}}else{return new F(function(){return _51(_6h,_6i,B(_66(_67,_68,_69,_6a,_6b,_6c,_6d,_6j)),_6k);});}}else{return new F(function(){return _4S(_67,_68,[0,_69,E(_6a),_6b,E(_6c),E(_6d)]);});}},_6l=function(_6m,_6n,_6o,_6p){var _6q=E(_6o);if(!_6q[0]){var _6r=_6q[1],_6s=_6q[2],_6t=_6q[3],_6u=_6q[4],_6v=_6q[5],_6w=E(_6p);if(!_6w[0]){var _6x=_6w[1],_6y=_6w[2],_6z=_6w[3],_6A=_6w[4],_6B=_6w[5];if((imul(3,_6r)|0)>=_6x){if((imul(3,_6x)|0)>=_6r){return [0,(_6r+_6x|0)+1|0,E(E(_6m)),_6n,E(_6q),E(_6w)];}else{return new F(function(){return _4a(_6s,_6t,_6u,B(_5R(_6m,_6n,_6v,_6x,_6y,_6z,_6A,_6B)));});}}else{return new F(function(){return _51(_6y,_6z,B(_66(_6m,_6n,_6r,_6s,_6t,_6u,_6v,_6A)),_6B);});}}else{return new F(function(){return _4S(_6m,_6n,_6q);});}}else{return new F(function(){return _5E(_6m,_6n,_6p);});}},_6C=function(_6D,_6E,_6F,_6G){var _6H=E(_6D);if(_6H==1){var _6I=E(_6G);return _6I[0]==0?[0,new T(function(){return [0,1,E(E(_6E)),_6F,E(_45),E(_45)];}),_v,_v]:B(_3Y(_6E,E(_6I[1])[1]))==0?[0,new T(function(){return [0,1,E(E(_6E)),_6F,E(_45),E(_45)];}),_6I,_v]:[0,new T(function(){return [0,1,E(E(_6E)),_6F,E(_45),E(_45)];}),_v,_6I];}else{var _6J=B(_6C(_6H>>1,_6E,_6F,_6G)),_6K=_6J[1],_6L=_6J[3],_6M=E(_6J[2]);if(!_6M[0]){return [0,_6K,_v,_6L];}else{var _6N=E(_6M[1]),_6O=_6N[1],_6P=_6N[2],_6Q=E(_6M[2]);if(!_6Q[0]){return [0,new T(function(){return B(_4S(_6O,_6P,_6K));}),_v,_6L];}else{var _6R=E(_6Q[1]),_6S=_6R[1];if(!B(_3Y(_6O,_6S))){var _6T=B(_6C(_6H>>1,_6S,_6R[2],_6Q[2]));return [0,new T(function(){return B(_6l(_6O,_6P,_6K,_6T[1]));}),_6T[2],_6T[3]];}else{return [0,_6K,_v,_6M];}}}}},_6U=function(_6V,_6W,_6X){var _6Y=E(_6V),_6Z=E(_6X);if(!_6Z[0]){var _70=_6Z[2],_71=_6Z[3],_72=_6Z[4],_73=_6Z[5];switch(B(_3Y(_6Y,_70))){case 0:return new F(function(){return _51(_70,_71,B(_6U(_6Y,_6W,_72)),_73);});break;case 1:return [0,_6Z[1],E(_6Y),_6W,E(_72),E(_73)];default:return new F(function(){return _4a(_70,_71,_72,B(_6U(_6Y,_6W,_73)));});}}else{return [0,1,E(_6Y),_6W,E(_45),E(_45)];}},_74=function(_75,_76){while(1){var _77=E(_76);if(!_77[0]){return E(_75);}else{var _78=E(_77[1]),_79=B(_6U(_78[1],_78[2],_75));_76=_77[2];_75=_79;continue;}}},_7a=function(_7b,_7c,_7d,_7e){return new F(function(){return _74(B(_6U(_7c,_7d,_7b)),_7e);});},_7f=function(_7g,_7h,_7i){var _7j=E(_7h);return new F(function(){return _74(B(_6U(_7j[1],_7j[2],_7g)),_7i);});},_7k=function(_7l,_7m,_7n){while(1){var _7o=E(_7n);if(!_7o[0]){return E(_7m);}else{var _7p=E(_7o[1]),_7q=_7p[1],_7r=_7p[2],_7s=E(_7o[2]);if(!_7s[0]){return new F(function(){return _4S(_7q,_7r,_7m);});}else{var _7t=E(_7s[1]),_7u=_7t[1];if(!B(_3Y(_7q,_7u))){var _7v=B(_6C(_7l,_7u,_7t[2],_7s[2])),_7w=_7v[1],_7x=E(_7v[3]);if(!_7x[0]){var _7y=_7l<<1,_7z=B(_6l(_7q,_7r,_7m,_7w));_7n=_7v[2];_7l=_7y;_7m=_7z;continue;}else{return new F(function(){return _7f(B(_6l(_7q,_7r,_7m,_7w)),_7x[1],_7x[2]);});}}else{return new F(function(){return _7a(_7m,_7q,_7r,_7s);});}}}}},_7A=function(_7B,_7C,_7D,_7E,_7F){var _7G=E(_7F);if(!_7G[0]){return new F(function(){return _4S(_7D,_7E,_7C);});}else{var _7H=E(_7G[1]),_7I=_7H[1];if(!B(_3Y(_7D,_7I))){var _7J=B(_6C(_7B,_7I,_7H[2],_7G[2])),_7K=_7J[1],_7L=E(_7J[3]);if(!_7L[0]){return new F(function(){return _7k(_7B<<1,B(_6l(_7D,_7E,_7C,_7K)),_7J[2]);});}else{return new F(function(){return _7f(B(_6l(_7D,_7E,_7C,_7K)),_7L[1],_7L[2]);});}}else{return new F(function(){return _7a(_7C,_7D,_7E,_7G);});}}},_7M=function(_7N){var _7O=E(_7N);if(!_7O[0]){return [1];}else{var _7P=E(_7O[1]),_7Q=_7P[1],_7R=_7P[2],_7S=E(_7O[2]);if(!_7S[0]){return [0,1,E(E(_7Q)),_7R,E(_45),E(_45)];}else{var _7T=_7S[2],_7U=E(_7S[1]),_7V=_7U[1],_7W=_7U[2];if(!B(_3Y(_7Q,_7V))){return new F(function(){return _7A(1,[0,1,E(E(_7Q)),_7R,E(_45),E(_45)],_7V,_7W,_7T);});}else{return new F(function(){return _7a([0,1,E(E(_7Q)),_7R,E(_45),E(_45)],_7V,_7W,_7T);});}}}},_7X=function(_7Y,_7Z){var _80=jsShowI(_7Y),_81=_80;return new F(function(){return _P(fromJSStr(_81),_7Z);});},_82=function(_83,_84,_85){if(_84>=0){return new F(function(){return _7X(_84,_85);});}else{return _83<=6?B(_7X(_84,_85)):[1,_18,new T(function(){var _86=jsShowI(_84),_87=_86;return B(_P(fromJSStr(_87),[1,_17,_85]));})];}},_88=new T(function(){return B(unCStr(" is not an element of the map"));}),_89=function(_8a){return new F(function(){return err(B(unAppCStr("IntMap.!: key ",new T(function(){return B(_P(B(_82(0,_8a,_v)),_88));}))));});},_8b=function(_8c,_8d){var _8e=new T(function(){return B(_89(_8d));});return new F(function(){return (function(_8f){while(1){var _8g=E(_8f);switch(_8g[0]){case 0:var _8h=_8g[2]>>>0;if(((_8d>>>0&((_8h-1>>>0^4294967295)>>>0^_8h)>>>0)>>>0&4294967295)==_8g[1]){if(!((_8d>>>0&_8h)>>>0)){_8f=_8g[3];continue;}else{_8f=_8g[4];continue;}}else{return E(_8e);}break;case 1:return _8d!=_8g[1]?E(_8e):E(_8g[2]);default:return E(_8e);}}})(_8c);});},_8i=function(_8j,_8k){return new F(function(){return _8b(_8j,E(_8k)[1]);});},_8l=[2],_8m=function(_8n,_8o,_8p){var _8q=E(_8p);switch(_8q[0]){case 0:var _8r=_8q[1],_8s=_8q[2],_8t=_8q[3],_8u=_8q[4],_8v=_8s>>>0;if(((_8n>>>0&((_8v-1>>>0^4294967295)>>>0^_8v)>>>0)>>>0&4294967295)==_8r){return (_8n>>>0&_8v)>>>0==0?[0,_8r,_8s,E(B(_8m(_8n,_8o,_8t))),E(_8u)]:[0,_8r,_8s,E(_8t),E(B(_8m(_8n,_8o,_8u)))];}else{var _8w=(_8n>>>0^_8r>>>0)>>>0,_8x=(_8w|_8w>>>1)>>>0,_8y=(_8x|_8x>>>2)>>>0,_8z=(_8y|_8y>>>4)>>>0,_8A=(_8z|_8z>>>8)>>>0,_8B=(_8A|_8A>>>16)>>>0,_8C=(_8B^_8B>>>1)>>>0&4294967295,_8D=_8C>>>0;return (_8n>>>0&_8D)>>>0==0?[0,(_8n>>>0&((_8D-1>>>0^4294967295)>>>0^_8D)>>>0)>>>0&4294967295,_8C,E([1,_8n,_8o]),E(_8q)]:[0,(_8n>>>0&((_8D-1>>>0^4294967295)>>>0^_8D)>>>0)>>>0&4294967295,_8C,E(_8q),E([1,_8n,_8o])];}break;case 1:var _8E=_8q[1];if(_8n!=_8E){var _8F=(_8n>>>0^_8E>>>0)>>>0,_8G=(_8F|_8F>>>1)>>>0,_8H=(_8G|_8G>>>2)>>>0,_8I=(_8H|_8H>>>4)>>>0,_8J=(_8I|_8I>>>8)>>>0,_8K=(_8J|_8J>>>16)>>>0,_8L=(_8K^_8K>>>1)>>>0&4294967295,_8M=_8L>>>0;return (_8n>>>0&_8M)>>>0==0?[0,(_8n>>>0&((_8M-1>>>0^4294967295)>>>0^_8M)>>>0)>>>0&4294967295,_8L,E([1,_8n,_8o]),E(_8q)]:[0,(_8n>>>0&((_8M-1>>>0^4294967295)>>>0^_8M)>>>0)>>>0&4294967295,_8L,E(_8q),E([1,_8n,_8o])];}else{return [1,_8n,_8o];}break;default:return [1,_8n,_8o];}},_8N=function(_8O,_8P){while(1){var _8Q=E(_8P);if(!_8Q[0]){return E(_8O);}else{var _8R=E(_8Q[1]),_8S=B(_8m(E(_8R[1])[1],_8R[2],_8O));_8P=_8Q[2];_8O=_8S;continue;}}},_8T=[0,6],_8U=new T(function(){return B(unCStr("car"));}),_8V=[0,_8T,_8U],_8W=new T(function(){return B(unCStr("lefteye"));}),_8X=[0,9],_8Y=[0,_8X,_8W],_8Z=new T(function(){return B(unCStr("monitor"));}),_90=[0,-1],_91=[0,_90,_8Z],_92=new T(function(){return B(unCStr("itemshop"));}),_93=[0,-2],_94=[0,_93,_92],_95=[0,-3],_96=new T(function(){return B(unCStr("righteye"));}),_97=[0,_95,_96],_98=[0,-4],_99=[0,_98,_8W],_9a=new T(function(){return B(unCStr("restart"));}),_9b=[0,-8],_9c=[0,_9b,_9a],_9d=[1,_9c,_v],_9e=new T(function(){return B(unCStr("reset"));}),_9f=[0,-7],_9g=[0,_9f,_9e],_9h=[1,_9g,_9d],_9i=[0,-6],_9j=[0,_9i,_8W],_9k=[1,_9j,_9h],_9l=[0,-5],_9m=[0,_9l,_96],_9n=[1,_9m,_9k],_9o=[1,_99,_9n],_9p=[1,_97,_9o],_9q=[1,_94,_9p],_9r=[1,_91,_9q],_9s=[1,_8Y,_9r],_9t=[0,8],_9u=[0,_9t,_96],_9v=[1,_9u,_9s],_9w=new T(function(){return B(unCStr("house"));}),_9x=[0,7],_9y=[0,_9x,_9w],_9z=[1,_9y,_9v],_9A=[1,_8V,_9z],_9B=new T(function(){return B(unCStr("trip"));}),_9C=[0,5],_9D=[0,_9C,_9B],_9E=[1,_9D,_9A],_9F=new T(function(){return B(unCStr("gift"));}),_9G=[0,4],_9H=[0,_9G,_9F],_9I=[1,_9H,_9E],_9J=new T(function(){return B(unCStr("coffee"));}),_9K=[0,3],_9L=[0,_9K,_9J],_9M=[1,_9L,_9I],_9N=new T(function(){return B(unCStr("mail"));}),_9O=[0,2],_9P=[0,_9O,_9N],_9Q=[1,_9P,_9M],_9R=new T(function(){return B(unCStr("chat"));}),_9S=[0,1],_9T=[0,_9S,_9R],_9U=[1,_9T,_9Q],_9V=new T(function(){return B(_8N(_8l,_9U));}),_9W=function(_9X){var _9Y=E(_9X);return [0,new T(function(){return B(_8i(_9V,_9Y[1]));}),_9Y[2]];},_9Z=new T(function(){return [0,"items"];}),_a0=new T(function(){return [0,"lpsCoeff"];}),_a1=new T(function(){return [0,"dependCoeff"];}),_a2=new T(function(){return [0,"maxLoves"];}),_a3=new T(function(){return [0,"itemsWithMap"];}),_a4=new T(function(){return [0,"achievements"];}),_a5=new T(function(){return [0,"lastFocus"];}),_a6=new T(function(){return [0,"depend"];}),_a7=new T(function(){return [0,"lps"];}),_a8=new T(function(){return [0,"loves"];}),_a9=new T(function(){return B(unCStr("Control.Exception.Base"));}),_aa=new T(function(){return B(unCStr("base"));}),_ab=new T(function(){return B(unCStr("PatternMatchFail"));}),_ac=new T(function(){var _ad=hs_wordToWord64(18445595),_ae=_ad,_af=hs_wordToWord64(52003073),_ag=_af;return [0,_ae,_ag,[0,_ae,_ag,_aa,_a9,_ab],_v];}),_ah=function(_ai){return E(_ac);},_aj=function(_ak){return E(E(_ak)[1]);},_al=function(_am,_an,_ao){var _ap=B(A(_am,[_])),_aq=B(A(_an,[_])),_ar=hs_eqWord64(_ap[1],_aq[1]),_as=_ar;if(!E(_as)){return [0];}else{var _at=hs_eqWord64(_ap[2],_aq[2]),_au=_at;return E(_au)==0?[0]:[1,_ao];}},_av=function(_aw){var _ax=E(_aw);return new F(function(){return _al(B(_aj(_ax[1])),_ah,_ax[2]);});},_ay=function(_az){return E(E(_az)[1]);},_aA=function(_aB,_aC){return new F(function(){return _P(E(_aB)[1],_aC);});},_aD=[0,44],_aE=[0,93],_aF=[0,91],_aG=function(_aH,_aI,_aJ){var _aK=E(_aI);return _aK[0]==0?B(unAppCStr("[]",_aJ)):[1,_aF,new T(function(){return B(A(_aH,[_aK[1],new T(function(){var _aL=function(_aM){var _aN=E(_aM);return _aN[0]==0?E([1,_aE,_aJ]):[1,_aD,new T(function(){return B(A(_aH,[_aN[1],new T(function(){return B(_aL(_aN[2]));})]));})];};return B(_aL(_aK[2]));})]));})];},_aO=function(_aP,_aQ){return new F(function(){return _aG(_aA,_aP,_aQ);});},_aR=function(_aS,_aT,_aU){return new F(function(){return _P(E(_aT)[1],_aU);});},_aV=[0,_aR,_ay,_aO],_aW=new T(function(){return [0,_ah,_aV,_aX,_av];}),_aX=function(_aY){return [0,_aW,_aY];},_aZ=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_b0=function(_b1,_b2){return new F(function(){return die(new T(function(){return B(A(_b2,[_b1]));}));});},_b3=function(_b4,_b5){var _b6=E(_b5);if(!_b6[0]){return [0,_v,_v];}else{var _b7=_b6[1];if(!B(A(_b4,[_b7]))){return [0,_v,_b6];}else{var _b8=new T(function(){var _b9=B(_b3(_b4,_b6[2]));return [0,_b9[1],_b9[2]];});return [0,[1,_b7,new T(function(){return E(E(_b8)[1]);})],new T(function(){return E(E(_b8)[2]);})];}}},_ba=[0,32],_bb=[0,10],_bc=[1,_bb,_v],_bd=function(_be){return E(E(_be)[1])==124?false:true;},_bf=function(_bg,_bh){var _bi=B(_b3(_bd,B(unCStr(_bg)))),_bj=_bi[1],_bk=function(_bl,_bm){return new F(function(){return _P(_bl,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_P(_bh,new T(function(){return B(_P(_bm,_bc));})));})));}));});},_bn=E(_bi[2]);if(!_bn[0]){return new F(function(){return _bk(_bj,_v);});}else{return E(E(_bn[1])[1])==124?B(_bk(_bj,[1,_ba,_bn[2]])):B(_bk(_bj,_v));}},_bo=function(_bp){return new F(function(){return _b0([0,new T(function(){return B(_bf(_bp,_aZ));})],_aX);});},_bq=new T(function(){return B(_bo("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_br=function(_bs,_bt){while(1){var _bu=(function(_bv,_bw){var _bx=E(_bv);switch(_bx[0]){case 0:var _by=E(_bw);if(!_by[0]){return [0];}else{_bs=B(A(_bx[1],[_by[1]]));_bt=_by[2];return null;}break;case 1:var _bz=B(A(_bx[1],[_bw])),_bA=_bw;_bs=_bz;_bt=_bA;return null;case 2:return [0];case 3:return [1,[0,_bx[1],_bw],new T(function(){return B(_br(_bx[2],_bw));})];default:return E(_bx[1]);}})(_bs,_bt);if(_bu!=null){return _bu;}}},_bB=function(_bC,_bD){var _bE=function(_bF){var _bG=E(_bD);if(_bG[0]==3){return [3,_bG[1],new T(function(){return B(_bB(_bC,_bG[2]));})];}else{var _bH=E(_bC);if(_bH[0]==2){return E(_bG);}else{var _bI=E(_bG);if(_bI[0]==2){return E(_bH);}else{var _bJ=function(_bK){var _bL=E(_bI);if(_bL[0]==4){return [1,function(_bM){return [4,new T(function(){return B(_P(B(_br(_bH,_bM)),_bL[1]));})];}];}else{var _bN=E(_bH);if(_bN[0]==1){var _bO=_bN[1],_bP=E(_bL);return _bP[0]==0?[1,function(_bQ){return new F(function(){return _bB(B(A(_bO,[_bQ])),_bP);});}]:[1,function(_bR){return new F(function(){return _bB(B(A(_bO,[_bR])),new T(function(){return B(A(_bP[1],[_bR]));}));});}];}else{var _bS=E(_bL);return _bS[0]==0?E(_bq):[1,function(_bT){return new F(function(){return _bB(_bN,new T(function(){return B(A(_bS[1],[_bT]));}));});}];}}},_bU=E(_bH);switch(_bU[0]){case 1:var _bV=E(_bI);if(_bV[0]==4){return [1,function(_bW){return [4,new T(function(){return B(_P(B(_br(B(A(_bU[1],[_bW])),_bW)),_bV[1]));})];}];}else{return new F(function(){return _bJ(_);});}break;case 4:var _bX=_bU[1],_bY=E(_bI);switch(_bY[0]){case 0:return [1,function(_bZ){return [4,new T(function(){return B(_P(_bX,new T(function(){return B(_br(_bY,_bZ));})));})];}];case 1:return [1,function(_c0){return [4,new T(function(){return B(_P(_bX,new T(function(){return B(_br(B(A(_bY[1],[_c0])),_c0));})));})];}];default:return [4,new T(function(){return B(_P(_bX,_bY[1]));})];}break;default:return new F(function(){return _bJ(_);});}}}}},_c1=E(_bC);switch(_c1[0]){case 0:var _c2=E(_bD);if(!_c2[0]){return [0,function(_c3){return new F(function(){return _bB(B(A(_c1[1],[_c3])),new T(function(){return B(A(_c2[1],[_c3]));}));});}];}else{return new F(function(){return _bE(_);});}break;case 3:return [3,_c1[1],new T(function(){return B(_bB(_c1[2],_bD));})];default:return new F(function(){return _bE(_);});}},_c4=[0,41],_c5=[1,_c4,_v],_c6=[0,40],_c7=[1,_c6,_v],_c8=function(_c9,_ca){while(1){var _cb=E(_c9);if(!_cb[0]){return E(_ca)[0]==0?true:false;}else{var _cc=E(_ca);if(!_cc[0]){return false;}else{if(E(_cb[1])[1]!=E(_cc[1])[1]){return false;}else{_c9=_cb[2];_ca=_cc[2];continue;}}}}},_cd=function(_ce,_cf){return E(_ce)[1]!=E(_cf)[1];},_cg=function(_ch,_ci){return E(_ch)[1]==E(_ci)[1];},_cj=[0,_cg,_cd],_ck=function(_cl,_cm){while(1){var _cn=E(_cl);if(!_cn[0]){return E(_cm)[0]==0?true:false;}else{var _co=E(_cm);if(!_co[0]){return false;}else{if(E(_cn[1])[1]!=E(_co[1])[1]){return false;}else{_cl=_cn[2];_cm=_co[2];continue;}}}}},_cp=function(_cq,_cr){return !B(_ck(_cq,_cr))?true:false;},_cs=[0,_ck,_cp],_ct=function(_cu,_cv){var _cw=E(_cu);switch(_cw[0]){case 0:return [0,function(_cx){return new F(function(){return _ct(B(A(_cw[1],[_cx])),_cv);});}];case 1:return [1,function(_cy){return new F(function(){return _ct(B(A(_cw[1],[_cy])),_cv);});}];case 2:return [2];case 3:return new F(function(){return _bB(B(A(_cv,[_cw[1]])),new T(function(){return B(_ct(_cw[2],_cv));}));});break;default:var _cz=function(_cA){var _cB=E(_cA);if(!_cB[0]){return [0];}else{var _cC=E(_cB[1]);return new F(function(){return _P(B(_br(B(A(_cv,[_cC[1]])),_cC[2])),new T(function(){return B(_cz(_cB[2]));}));});}},_cD=B(_cz(_cw[1]));return _cD[0]==0?[2]:[4,_cD];}},_cE=[2],_cF=function(_cG){return [3,_cG,_cE];},_cH=0,_cI=function(_cJ,_cK){var _cL=E(_cJ);if(!_cL){return new F(function(){return A(_cK,[_cH]);});}else{return [0,function(_cM){return E(new T(function(){return B(_cI(_cL-1|0,_cK));}));}];}},_cN=function(_cO,_cP,_cQ){return function(_cR){return new F(function(){return A(function(_cS,_cT,_cU){while(1){var _cV=(function(_cW,_cX,_cY){var _cZ=E(_cW);switch(_cZ[0]){case 0:var _d0=E(_cX);if(!_d0[0]){return E(_cP);}else{_cS=B(A(_cZ[1],[_d0[1]]));_cT=_d0[2];var _d1=_cY+1|0;_cU=_d1;return null;}break;case 1:var _d2=B(A(_cZ[1],[_cX])),_d3=_cX,_d1=_cY;_cS=_d2;_cT=_d3;_cU=_d1;return null;case 2:return E(_cP);case 3:return function(_d4){return new F(function(){return _cI(_cY,function(_d5){return E(new T(function(){return B(_ct(_cZ,_d4));}));});});};default:return function(_d6){return new F(function(){return _ct(_cZ,_d6);});};}})(_cS,_cT,_cU);if(_cV!=null){return _cV;}}},[new T(function(){return B(A(_cO,[_cF]));}),_cR,0,_cQ]);});};},_d7=function(_d8){return new F(function(){return A(_d8,[_v]);});},_d9=function(_da,_db){var _dc=function(_dd){var _de=E(_dd);if(!_de[0]){return E(_d7);}else{var _df=_de[1];return !B(A(_da,[_df]))?E(_d7):function(_dg){return [0,function(_dh){return E(new T(function(){return B(A(new T(function(){return B(_dc(_de[2]));}),[function(_di){return new F(function(){return A(_dg,[[1,_df,_di]]);});}]));}));}];};}};return function(_dj){return new F(function(){return A(_dc,[_dj,_db]);});};},_dk=[6],_dl=function(_dm){return E(_dm);},_dn=new T(function(){return B(unCStr("valDig: Bad base"));}),_do=new T(function(){return B(err(_dn));}),_dp=function(_dq,_dr){var _ds=function(_dt,_du){var _dv=E(_dt);if(!_dv[0]){return function(_dw){return new F(function(){return A(_dw,[new T(function(){return B(A(_du,[_v]));})]);});};}else{var _dx=E(_dv[1])[1],_dy=function(_dz){return function(_dA){return [0,function(_dB){return E(new T(function(){return B(A(new T(function(){return B(_ds(_dv[2],function(_dC){return new F(function(){return A(_du,[[1,_dz,_dC]]);});}));}),[_dA]));}));}];};};switch(E(E(_dq)[1])){case 8:if(48>_dx){return function(_dD){return new F(function(){return A(_dD,[new T(function(){return B(A(_du,[_v]));})]);});};}else{if(_dx>55){return function(_dE){return new F(function(){return A(_dE,[new T(function(){return B(A(_du,[_v]));})]);});};}else{return new F(function(){return _dy([0,_dx-48|0]);});}}break;case 10:if(48>_dx){return function(_dF){return new F(function(){return A(_dF,[new T(function(){return B(A(_du,[_v]));})]);});};}else{if(_dx>57){return function(_dG){return new F(function(){return A(_dG,[new T(function(){return B(A(_du,[_v]));})]);});};}else{return new F(function(){return _dy([0,_dx-48|0]);});}}break;case 16:if(48>_dx){if(97>_dx){if(65>_dx){return function(_dH){return new F(function(){return A(_dH,[new T(function(){return B(A(_du,[_v]));})]);});};}else{if(_dx>70){return function(_dI){return new F(function(){return A(_dI,[new T(function(){return B(A(_du,[_v]));})]);});};}else{return new F(function(){return _dy([0,(_dx-65|0)+10|0]);});}}}else{if(_dx>102){if(65>_dx){return function(_dJ){return new F(function(){return A(_dJ,[new T(function(){return B(A(_du,[_v]));})]);});};}else{if(_dx>70){return function(_dK){return new F(function(){return A(_dK,[new T(function(){return B(A(_du,[_v]));})]);});};}else{return new F(function(){return _dy([0,(_dx-65|0)+10|0]);});}}}else{return new F(function(){return _dy([0,(_dx-97|0)+10|0]);});}}}else{if(_dx>57){if(97>_dx){if(65>_dx){return function(_dL){return new F(function(){return A(_dL,[new T(function(){return B(A(_du,[_v]));})]);});};}else{if(_dx>70){return function(_dM){return new F(function(){return A(_dM,[new T(function(){return B(A(_du,[_v]));})]);});};}else{return new F(function(){return _dy([0,(_dx-65|0)+10|0]);});}}}else{if(_dx>102){if(65>_dx){return function(_dN){return new F(function(){return A(_dN,[new T(function(){return B(A(_du,[_v]));})]);});};}else{if(_dx>70){return function(_dO){return new F(function(){return A(_dO,[new T(function(){return B(A(_du,[_v]));})]);});};}else{return new F(function(){return _dy([0,(_dx-65|0)+10|0]);});}}}else{return new F(function(){return _dy([0,(_dx-97|0)+10|0]);});}}}else{return new F(function(){return _dy([0,_dx-48|0]);});}}break;default:return E(_do);}}};return function(_dP){return new F(function(){return A(_ds,[_dP,_dl,function(_dQ){var _dR=E(_dQ);return _dR[0]==0?[2]:B(A(_dr,[_dR]));}]);});};},_dS=[0,10],_dT=[0,1],_dU=[0,2147483647],_dV=function(_dW,_dX){while(1){var _dY=E(_dW);if(!_dY[0]){var _dZ=_dY[1],_e0=E(_dX);if(!_e0[0]){var _e1=_e0[1],_e2=addC(_dZ,_e1);if(!E(_e2[2])){return [0,_e2[1]];}else{_dW=[1,I_fromInt(_dZ)];_dX=[1,I_fromInt(_e1)];continue;}}else{_dW=[1,I_fromInt(_dZ)];_dX=_e0;continue;}}else{var _e3=E(_dX);if(!_e3[0]){_dW=_dY;_dX=[1,I_fromInt(_e3[1])];continue;}else{return [1,I_add(_dY[1],_e3[1])];}}}},_e4=new T(function(){return B(_dV(_dU,_dT));}),_e5=function(_e6){var _e7=E(_e6);if(!_e7[0]){var _e8=E(_e7[1]);return _e8==(-2147483648)?E(_e4):[0, -_e8];}else{return [1,I_negate(_e7[1])];}},_e9=[0,10],_ea=[0,0],_eb=function(_ec){return [0,_ec];},_ed=function(_ee,_ef){while(1){var _eg=E(_ee);if(!_eg[0]){var _eh=_eg[1],_ei=E(_ef);if(!_ei[0]){var _ej=_ei[1];if(!(imul(_eh,_ej)|0)){return [0,imul(_eh,_ej)|0];}else{_ee=[1,I_fromInt(_eh)];_ef=[1,I_fromInt(_ej)];continue;}}else{_ee=[1,I_fromInt(_eh)];_ef=_ei;continue;}}else{var _ek=E(_ef);if(!_ek[0]){_ee=_eg;_ef=[1,I_fromInt(_ek[1])];continue;}else{return [1,I_mul(_eg[1],_ek[1])];}}}},_el=function(_em,_en,_eo){while(1){var _ep=E(_eo);if(!_ep[0]){return E(_en);}else{var _eq=B(_dV(B(_ed(_en,_em)),B(_eb(E(_ep[1])[1]))));_eo=_ep[2];_en=_eq;continue;}}},_er=function(_es){var _et=new T(function(){return B(_bB(B(_bB([0,function(_eu){return E(E(_eu)[1])==45?[1,B(_dp(_dS,function(_ev){return new F(function(){return A(_es,[[1,new T(function(){return B(_e5(B(_el(_e9,_ea,_ev))));})]]);});}))]:[2];}],[0,function(_ew){return E(E(_ew)[1])==43?[1,B(_dp(_dS,function(_ex){return new F(function(){return A(_es,[[1,new T(function(){return B(_el(_e9,_ea,_ex));})]]);});}))]:[2];}])),new T(function(){return [1,B(_dp(_dS,function(_ey){return new F(function(){return A(_es,[[1,new T(function(){return B(_el(_e9,_ea,_ey));})]]);});}))];})));});return new F(function(){return _bB([0,function(_ez){return E(E(_ez)[1])==101?E(_et):[2];}],[0,function(_eA){return E(E(_eA)[1])==69?E(_et):[2];}]);});},_eB=function(_eC){return new F(function(){return A(_eC,[_2w]);});},_eD=function(_eE){return new F(function(){return A(_eE,[_2w]);});},_eF=function(_eG){return function(_eH){return E(E(_eH)[1])==46?[1,B(_dp(_dS,function(_eI){return new F(function(){return A(_eG,[[1,_eI]]);});}))]:[2];};},_eJ=function(_eK){return [0,B(_eF(_eK))];},_eL=function(_eM){return new F(function(){return _dp(_dS,function(_eN){return [1,B(_cN(_eJ,_eB,function(_eO){return [1,B(_cN(_er,_eD,function(_eP){return new F(function(){return A(_eM,[[5,[1,_eN,_eO,_eP]]]);});}))];}))];});});},_eQ=function(_eR){return [1,B(_eL(_eR))];},_eS=function(_eT,_eU,_eV){while(1){var _eW=E(_eV);if(!_eW[0]){return false;}else{if(!B(A(_2E,[_eT,_eU,_eW[1]]))){_eV=_eW[2];continue;}else{return true;}}}},_eX=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_eY=function(_eZ){return new F(function(){return _eS(_cj,_eZ,_eX);});},_f0=[0,8],_f1=[0,16],_f2=function(_f3){var _f4=function(_f5){return new F(function(){return A(_f3,[[5,[0,_f0,_f5]]]);});},_f6=function(_f7){return new F(function(){return A(_f3,[[5,[0,_f1,_f7]]]);});};return function(_f8){return E(E(_f8)[1])==48?E([0,function(_f9){switch(E(E(_f9)[1])){case 79:return [1,B(_dp(_f0,_f4))];case 88:return [1,B(_dp(_f1,_f6))];case 111:return [1,B(_dp(_f0,_f4))];case 120:return [1,B(_dp(_f1,_f6))];default:return [2];}}]):[2];};},_fa=function(_fb){return [0,B(_f2(_fb))];},_fc=function(_fd){var _fe=new T(function(){return B(A(_fd,[_f0]));}),_ff=new T(function(){return B(A(_fd,[_f1]));});return function(_fg){switch(E(E(_fg)[1])){case 79:return E(_fe);case 88:return E(_ff);case 111:return E(_fe);case 120:return E(_ff);default:return [2];}};},_fh=function(_fi){return [0,B(_fc(_fi))];},_fj=[0,92],_fk=function(_fl){return new F(function(){return A(_fl,[_dS]);});},_fm=function(_fn){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_82(9,_fn,_v));}))));});},_fo=function(_fp){var _fq=E(_fp);return _fq[0]==0?E(_fq[1]):I_toInt(_fq[1]);},_fr=function(_fs,_ft){var _fu=E(_fs);if(!_fu[0]){var _fv=_fu[1],_fw=E(_ft);return _fw[0]==0?_fv<=_fw[1]:I_compareInt(_fw[1],_fv)>=0;}else{var _fx=_fu[1],_fy=E(_ft);return _fy[0]==0?I_compareInt(_fx,_fy[1])<=0:I_compare(_fx,_fy[1])<=0;}},_fz=function(_fA){return [2];},_fB=function(_fC){var _fD=E(_fC);if(!_fD[0]){return E(_fz);}else{var _fE=_fD[1],_fF=E(_fD[2]);return _fF[0]==0?E(_fE):function(_fG){return new F(function(){return _bB(B(A(_fE,[_fG])),new T(function(){return B(A(new T(function(){return B(_fB(_fF));}),[_fG]));}));});};}},_fH=function(_fI){return [2];},_fJ=function(_fK,_fL){var _fM=function(_fN,_fO){var _fP=E(_fN);if(!_fP[0]){return function(_fQ){return new F(function(){return A(_fQ,[_fK]);});};}else{var _fR=E(_fO);return _fR[0]==0?E(_fH):E(_fP[1])[1]!=E(_fR[1])[1]?E(_fH):function(_fS){return [0,function(_fT){return E(new T(function(){return B(A(new T(function(){return B(_fM(_fP[2],_fR[2]));}),[_fS]));}));}];};}};return function(_fU){return new F(function(){return A(_fM,[_fK,_fU,_fL]);});};},_fV=new T(function(){return B(unCStr("SOH"));}),_fW=[0,1],_fX=function(_fY){return [1,B(_fJ(_fV,function(_fZ){return E(new T(function(){return B(A(_fY,[_fW]));}));}))];},_g0=new T(function(){return B(unCStr("SO"));}),_g1=[0,14],_g2=function(_g3){return [1,B(_fJ(_g0,function(_g4){return E(new T(function(){return B(A(_g3,[_g1]));}));}))];},_g5=function(_g6){return [1,B(_cN(_fX,_g2,_g6))];},_g7=new T(function(){return B(unCStr("NUL"));}),_g8=[0,0],_g9=function(_ga){return [1,B(_fJ(_g7,function(_gb){return E(new T(function(){return B(A(_ga,[_g8]));}));}))];},_gc=new T(function(){return B(unCStr("STX"));}),_gd=[0,2],_ge=function(_gf){return [1,B(_fJ(_gc,function(_gg){return E(new T(function(){return B(A(_gf,[_gd]));}));}))];},_gh=new T(function(){return B(unCStr("ETX"));}),_gi=[0,3],_gj=function(_gk){return [1,B(_fJ(_gh,function(_gl){return E(new T(function(){return B(A(_gk,[_gi]));}));}))];},_gm=new T(function(){return B(unCStr("EOT"));}),_gn=[0,4],_go=function(_gp){return [1,B(_fJ(_gm,function(_gq){return E(new T(function(){return B(A(_gp,[_gn]));}));}))];},_gr=new T(function(){return B(unCStr("ENQ"));}),_gs=[0,5],_gt=function(_gu){return [1,B(_fJ(_gr,function(_gv){return E(new T(function(){return B(A(_gu,[_gs]));}));}))];},_gw=new T(function(){return B(unCStr("ACK"));}),_gx=[0,6],_gy=function(_gz){return [1,B(_fJ(_gw,function(_gA){return E(new T(function(){return B(A(_gz,[_gx]));}));}))];},_gB=new T(function(){return B(unCStr("BEL"));}),_gC=[0,7],_gD=function(_gE){return [1,B(_fJ(_gB,function(_gF){return E(new T(function(){return B(A(_gE,[_gC]));}));}))];},_gG=new T(function(){return B(unCStr("BS"));}),_gH=[0,8],_gI=function(_gJ){return [1,B(_fJ(_gG,function(_gK){return E(new T(function(){return B(A(_gJ,[_gH]));}));}))];},_gL=new T(function(){return B(unCStr("HT"));}),_gM=[0,9],_gN=function(_gO){return [1,B(_fJ(_gL,function(_gP){return E(new T(function(){return B(A(_gO,[_gM]));}));}))];},_gQ=new T(function(){return B(unCStr("LF"));}),_gR=[0,10],_gS=function(_gT){return [1,B(_fJ(_gQ,function(_gU){return E(new T(function(){return B(A(_gT,[_gR]));}));}))];},_gV=new T(function(){return B(unCStr("VT"));}),_gW=[0,11],_gX=function(_gY){return [1,B(_fJ(_gV,function(_gZ){return E(new T(function(){return B(A(_gY,[_gW]));}));}))];},_h0=new T(function(){return B(unCStr("FF"));}),_h1=[0,12],_h2=function(_h3){return [1,B(_fJ(_h0,function(_h4){return E(new T(function(){return B(A(_h3,[_h1]));}));}))];},_h5=new T(function(){return B(unCStr("CR"));}),_h6=[0,13],_h7=function(_h8){return [1,B(_fJ(_h5,function(_h9){return E(new T(function(){return B(A(_h8,[_h6]));}));}))];},_ha=new T(function(){return B(unCStr("SI"));}),_hb=[0,15],_hc=function(_hd){return [1,B(_fJ(_ha,function(_he){return E(new T(function(){return B(A(_hd,[_hb]));}));}))];},_hf=new T(function(){return B(unCStr("DLE"));}),_hg=[0,16],_hh=function(_hi){return [1,B(_fJ(_hf,function(_hj){return E(new T(function(){return B(A(_hi,[_hg]));}));}))];},_hk=new T(function(){return B(unCStr("DC1"));}),_hl=[0,17],_hm=function(_hn){return [1,B(_fJ(_hk,function(_ho){return E(new T(function(){return B(A(_hn,[_hl]));}));}))];},_hp=new T(function(){return B(unCStr("DC2"));}),_hq=[0,18],_hr=function(_hs){return [1,B(_fJ(_hp,function(_ht){return E(new T(function(){return B(A(_hs,[_hq]));}));}))];},_hu=new T(function(){return B(unCStr("DC3"));}),_hv=[0,19],_hw=function(_hx){return [1,B(_fJ(_hu,function(_hy){return E(new T(function(){return B(A(_hx,[_hv]));}));}))];},_hz=new T(function(){return B(unCStr("DC4"));}),_hA=[0,20],_hB=function(_hC){return [1,B(_fJ(_hz,function(_hD){return E(new T(function(){return B(A(_hC,[_hA]));}));}))];},_hE=new T(function(){return B(unCStr("NAK"));}),_hF=[0,21],_hG=function(_hH){return [1,B(_fJ(_hE,function(_hI){return E(new T(function(){return B(A(_hH,[_hF]));}));}))];},_hJ=new T(function(){return B(unCStr("SYN"));}),_hK=[0,22],_hL=function(_hM){return [1,B(_fJ(_hJ,function(_hN){return E(new T(function(){return B(A(_hM,[_hK]));}));}))];},_hO=new T(function(){return B(unCStr("ETB"));}),_hP=[0,23],_hQ=function(_hR){return [1,B(_fJ(_hO,function(_hS){return E(new T(function(){return B(A(_hR,[_hP]));}));}))];},_hT=new T(function(){return B(unCStr("CAN"));}),_hU=[0,24],_hV=function(_hW){return [1,B(_fJ(_hT,function(_hX){return E(new T(function(){return B(A(_hW,[_hU]));}));}))];},_hY=new T(function(){return B(unCStr("EM"));}),_hZ=[0,25],_i0=function(_i1){return [1,B(_fJ(_hY,function(_i2){return E(new T(function(){return B(A(_i1,[_hZ]));}));}))];},_i3=new T(function(){return B(unCStr("SUB"));}),_i4=[0,26],_i5=function(_i6){return [1,B(_fJ(_i3,function(_i7){return E(new T(function(){return B(A(_i6,[_i4]));}));}))];},_i8=new T(function(){return B(unCStr("ESC"));}),_i9=[0,27],_ia=function(_ib){return [1,B(_fJ(_i8,function(_ic){return E(new T(function(){return B(A(_ib,[_i9]));}));}))];},_id=new T(function(){return B(unCStr("FS"));}),_ie=[0,28],_if=function(_ig){return [1,B(_fJ(_id,function(_ih){return E(new T(function(){return B(A(_ig,[_ie]));}));}))];},_ii=new T(function(){return B(unCStr("GS"));}),_ij=[0,29],_ik=function(_il){return [1,B(_fJ(_ii,function(_im){return E(new T(function(){return B(A(_il,[_ij]));}));}))];},_in=new T(function(){return B(unCStr("RS"));}),_io=[0,30],_ip=function(_iq){return [1,B(_fJ(_in,function(_ir){return E(new T(function(){return B(A(_iq,[_io]));}));}))];},_is=new T(function(){return B(unCStr("US"));}),_it=[0,31],_iu=function(_iv){return [1,B(_fJ(_is,function(_iw){return E(new T(function(){return B(A(_iv,[_it]));}));}))];},_ix=new T(function(){return B(unCStr("SP"));}),_iy=[0,32],_iz=function(_iA){return [1,B(_fJ(_ix,function(_iB){return E(new T(function(){return B(A(_iA,[_iy]));}));}))];},_iC=new T(function(){return B(unCStr("DEL"));}),_iD=[0,127],_iE=function(_iF){return [1,B(_fJ(_iC,function(_iG){return E(new T(function(){return B(A(_iF,[_iD]));}));}))];},_iH=[1,_iE,_v],_iI=[1,_iz,_iH],_iJ=[1,_iu,_iI],_iK=[1,_ip,_iJ],_iL=[1,_ik,_iK],_iM=[1,_if,_iL],_iN=[1,_ia,_iM],_iO=[1,_i5,_iN],_iP=[1,_i0,_iO],_iQ=[1,_hV,_iP],_iR=[1,_hQ,_iQ],_iS=[1,_hL,_iR],_iT=[1,_hG,_iS],_iU=[1,_hB,_iT],_iV=[1,_hw,_iU],_iW=[1,_hr,_iV],_iX=[1,_hm,_iW],_iY=[1,_hh,_iX],_iZ=[1,_hc,_iY],_j0=[1,_h7,_iZ],_j1=[1,_h2,_j0],_j2=[1,_gX,_j1],_j3=[1,_gS,_j2],_j4=[1,_gN,_j3],_j5=[1,_gI,_j4],_j6=[1,_gD,_j5],_j7=[1,_gy,_j6],_j8=[1,_gt,_j7],_j9=[1,_go,_j8],_ja=[1,_gj,_j9],_jb=[1,_ge,_ja],_jc=[1,_g9,_jb],_jd=[1,_g5,_jc],_je=new T(function(){return B(_fB(_jd));}),_jf=[0,1114111],_jg=[0,34],_jh=[0,39],_ji=function(_jj){var _jk=new T(function(){return B(A(_jj,[_gC]));}),_jl=new T(function(){return B(A(_jj,[_gH]));}),_jm=new T(function(){return B(A(_jj,[_gM]));}),_jn=new T(function(){return B(A(_jj,[_gR]));}),_jo=new T(function(){return B(A(_jj,[_gW]));}),_jp=new T(function(){return B(A(_jj,[_h1]));}),_jq=new T(function(){return B(A(_jj,[_h6]));});return new F(function(){return _bB([0,function(_jr){switch(E(E(_jr)[1])){case 34:return E(new T(function(){return B(A(_jj,[_jg]));}));case 39:return E(new T(function(){return B(A(_jj,[_jh]));}));case 92:return E(new T(function(){return B(A(_jj,[_fj]));}));case 97:return E(_jk);case 98:return E(_jl);case 102:return E(_jp);case 110:return E(_jn);case 114:return E(_jq);case 116:return E(_jm);case 118:return E(_jo);default:return [2];}}],new T(function(){return B(_bB([1,B(_cN(_fh,_fk,function(_js){return [1,B(_dp(_js,function(_jt){var _ju=B(_el(new T(function(){return B(_eb(E(_js)[1]));}),_ea,_jt));return !B(_fr(_ju,_jf))?[2]:B(A(_jj,[new T(function(){var _jv=B(_fo(_ju));if(_jv>>>0>1114111){var _jw=B(_fm(_jv));}else{var _jw=[0,_jv];}var _jx=_jw,_jy=_jx,_jz=_jy;return _jz;})]));}))];}))],new T(function(){return B(_bB([0,function(_jA){return E(E(_jA)[1])==94?E([0,function(_jB){switch(E(E(_jB)[1])){case 64:return E(new T(function(){return B(A(_jj,[_g8]));}));case 65:return E(new T(function(){return B(A(_jj,[_fW]));}));case 66:return E(new T(function(){return B(A(_jj,[_gd]));}));case 67:return E(new T(function(){return B(A(_jj,[_gi]));}));case 68:return E(new T(function(){return B(A(_jj,[_gn]));}));case 69:return E(new T(function(){return B(A(_jj,[_gs]));}));case 70:return E(new T(function(){return B(A(_jj,[_gx]));}));case 71:return E(_jk);case 72:return E(_jl);case 73:return E(_jm);case 74:return E(_jn);case 75:return E(_jo);case 76:return E(_jp);case 77:return E(_jq);case 78:return E(new T(function(){return B(A(_jj,[_g1]));}));case 79:return E(new T(function(){return B(A(_jj,[_hb]));}));case 80:return E(new T(function(){return B(A(_jj,[_hg]));}));case 81:return E(new T(function(){return B(A(_jj,[_hl]));}));case 82:return E(new T(function(){return B(A(_jj,[_hq]));}));case 83:return E(new T(function(){return B(A(_jj,[_hv]));}));case 84:return E(new T(function(){return B(A(_jj,[_hA]));}));case 85:return E(new T(function(){return B(A(_jj,[_hF]));}));case 86:return E(new T(function(){return B(A(_jj,[_hK]));}));case 87:return E(new T(function(){return B(A(_jj,[_hP]));}));case 88:return E(new T(function(){return B(A(_jj,[_hU]));}));case 89:return E(new T(function(){return B(A(_jj,[_hZ]));}));case 90:return E(new T(function(){return B(A(_jj,[_i4]));}));case 91:return E(new T(function(){return B(A(_jj,[_i9]));}));case 92:return E(new T(function(){return B(A(_jj,[_ie]));}));case 93:return E(new T(function(){return B(A(_jj,[_ij]));}));case 94:return E(new T(function(){return B(A(_jj,[_io]));}));case 95:return E(new T(function(){return B(A(_jj,[_it]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_je,[_jj]));})));})));}));});},_jC=function(_jD){return new F(function(){return A(_jD,[_cH]);});},_jE=function(_jF){var _jG=E(_jF);if(!_jG[0]){return E(_jC);}else{var _jH=_jG[2],_jI=E(E(_jG[1])[1]);switch(_jI){case 9:return function(_jJ){return [0,function(_jK){return E(new T(function(){return B(A(new T(function(){return B(_jE(_jH));}),[_jJ]));}));}];};case 10:return function(_jL){return [0,function(_jM){return E(new T(function(){return B(A(new T(function(){return B(_jE(_jH));}),[_jL]));}));}];};case 11:return function(_jN){return [0,function(_jO){return E(new T(function(){return B(A(new T(function(){return B(_jE(_jH));}),[_jN]));}));}];};case 12:return function(_jP){return [0,function(_jQ){return E(new T(function(){return B(A(new T(function(){return B(_jE(_jH));}),[_jP]));}));}];};case 13:return function(_jR){return [0,function(_jS){return E(new T(function(){return B(A(new T(function(){return B(_jE(_jH));}),[_jR]));}));}];};case 32:return function(_jT){return [0,function(_jU){return E(new T(function(){return B(A(new T(function(){return B(_jE(_jH));}),[_jT]));}));}];};case 160:return function(_jV){return [0,function(_jW){return E(new T(function(){return B(A(new T(function(){return B(_jE(_jH));}),[_jV]));}));}];};default:var _jX=u_iswspace(_jI),_jY=_jX;return E(_jY)==0?E(_jC):function(_jZ){return [0,function(_k0){return E(new T(function(){return B(A(new T(function(){return B(_jE(_jH));}),[_jZ]));}));}];};}}},_k1=function(_k2){var _k3=new T(function(){return B(_k1(_k2));}),_k4=[1,function(_k5){return new F(function(){return A(_jE,[_k5,function(_k6){return E([0,function(_k7){return E(E(_k7)[1])==92?E(_k3):[2];}]);}]);});}];return new F(function(){return _bB([0,function(_k8){return E(E(_k8)[1])==92?E([0,function(_k9){var _ka=E(E(_k9)[1]);switch(_ka){case 9:return E(_k4);case 10:return E(_k4);case 11:return E(_k4);case 12:return E(_k4);case 13:return E(_k4);case 32:return E(_k4);case 38:return E(_k3);case 160:return E(_k4);default:var _kb=u_iswspace(_ka),_kc=_kb;return E(_kc)==0?[2]:E(_k4);}}]):[2];}],[0,function(_kd){var _ke=E(_kd);return E(_ke[1])==92?E(new T(function(){return B(_ji(function(_kf){return new F(function(){return A(_k2,[[0,_kf,_o]]);});}));})):B(A(_k2,[[0,_ke,_s]]));}]);});},_kg=function(_kh,_ki){return new F(function(){return _k1(function(_kj){var _kk=E(_kj),_kl=E(_kk[1]);if(E(_kl[1])==34){if(!E(_kk[2])){return E(new T(function(){return B(A(_ki,[[1,new T(function(){return B(A(_kh,[_v]));})]]));}));}else{return new F(function(){return _kg(function(_km){return new F(function(){return A(_kh,[[1,_kl,_km]]);});},_ki);});}}else{return new F(function(){return _kg(function(_kn){return new F(function(){return A(_kh,[[1,_kl,_kn]]);});},_ki);});}});});},_ko=new T(function(){return B(unCStr("_\'"));}),_kp=function(_kq){var _kr=u_iswalnum(_kq),_ks=_kr;return E(_ks)==0?B(_eS(_cj,[0,_kq],_ko)):true;},_kt=function(_ku){return new F(function(){return _kp(E(_ku)[1]);});},_kv=new T(function(){return B(unCStr(",;()[]{}`"));}),_kw=new T(function(){return B(unCStr(".."));}),_kx=new T(function(){return B(unCStr("::"));}),_ky=new T(function(){return B(unCStr("->"));}),_kz=[0,64],_kA=[1,_kz,_v],_kB=[0,126],_kC=[1,_kB,_v],_kD=new T(function(){return B(unCStr("=>"));}),_kE=[1,_kD,_v],_kF=[1,_kC,_kE],_kG=[1,_kA,_kF],_kH=[1,_ky,_kG],_kI=new T(function(){return B(unCStr("<-"));}),_kJ=[1,_kI,_kH],_kK=[0,124],_kL=[1,_kK,_v],_kM=[1,_kL,_kJ],_kN=[1,_fj,_v],_kO=[1,_kN,_kM],_kP=[0,61],_kQ=[1,_kP,_v],_kR=[1,_kQ,_kO],_kS=[1,_kx,_kR],_kT=[1,_kw,_kS],_kU=function(_kV){return new F(function(){return _bB([1,function(_kW){return E(_kW)[0]==0?E(new T(function(){return B(A(_kV,[_dk]));})):[2];}],new T(function(){return B(_bB([0,function(_kX){return E(E(_kX)[1])==39?E([0,function(_kY){var _kZ=E(_kY);switch(E(_kZ[1])){case 39:return [2];case 92:return E(new T(function(){return B(_ji(function(_l0){return [0,function(_l1){return E(E(_l1)[1])==39?E(new T(function(){return B(A(_kV,[[0,_l0]]));})):[2];}];}));}));default:return [0,function(_l2){return E(E(_l2)[1])==39?E(new T(function(){return B(A(_kV,[[0,_kZ]]));})):[2];}];}}]):[2];}],new T(function(){return B(_bB([0,function(_l3){return E(E(_l3)[1])==34?E(new T(function(){return B(_kg(_dl,_kV));})):[2];}],new T(function(){return B(_bB([0,function(_l4){return !B(_eS(_cj,_l4,_kv))?[2]:B(A(_kV,[[2,[1,_l4,_v]]]));}],new T(function(){return B(_bB([0,function(_l5){return !B(_eS(_cj,_l5,_eX))?[2]:[1,B(_d9(_eY,function(_l6){var _l7=[1,_l5,_l6];return !B(_eS(_cs,_l7,_kT))?B(A(_kV,[[4,_l7]])):B(A(_kV,[[2,_l7]]));}))];}],new T(function(){return B(_bB([0,function(_l8){var _l9=E(_l8),_la=_l9[1],_lb=u_iswalpha(_la),_lc=_lb;return E(_lc)==0?E(_la)==95?[1,B(_d9(_kt,function(_ld){return new F(function(){return A(_kV,[[3,[1,_l9,_ld]]]);});}))]:[2]:[1,B(_d9(_kt,function(_le){return new F(function(){return A(_kV,[[3,[1,_l9,_le]]]);});}))];}],new T(function(){return [1,B(_cN(_fa,_eQ,_kV))];})));})));})));})));})));}));});},_lf=[0,0],_lg=function(_lh,_li){return function(_lj){return new F(function(){return A(_jE,[_lj,function(_lk){return E(new T(function(){return B(_kU(function(_ll){var _lm=E(_ll);return _lm[0]==2?!B(_c8(_lm[1],_c7))?[2]:E(new T(function(){return B(A(_lh,[_lf,function(_ln){return [1,function(_lo){return new F(function(){return A(_jE,[_lo,function(_lp){return E(new T(function(){return B(_kU(function(_lq){var _lr=E(_lq);return _lr[0]==2?!B(_c8(_lr[1],_c5))?[2]:E(new T(function(){return B(A(_li,[_ln]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_ls=function(_lt,_lu,_lv){var _lw=function(_lx,_ly){return new F(function(){return _bB([1,function(_lz){return new F(function(){return A(_jE,[_lz,function(_lA){return E(new T(function(){return B(_kU(function(_lB){var _lC=E(_lB);if(_lC[0]==4){var _lD=E(_lC[1]);if(!_lD[0]){return new F(function(){return A(_lt,[_lC,_lx,_ly]);});}else{return E(E(_lD[1])[1])==45?E(_lD[2])[0]==0?E([1,function(_lE){return new F(function(){return A(_jE,[_lE,function(_lF){return E(new T(function(){return B(_kU(function(_lG){return new F(function(){return A(_lt,[_lG,_lx,function(_lH){return new F(function(){return A(_ly,[new T(function(){return B(_e5(_lH));})]);});}]);});}));}));}]);});}]):B(A(_lt,[_lC,_lx,_ly])):B(A(_lt,[_lC,_lx,_ly]));}}else{return new F(function(){return A(_lt,[_lC,_lx,_ly]);});}}));}));}]);});}],new T(function(){return [1,B(_lg(_lw,_ly))];}));});};return new F(function(){return _lw(_lu,_lv);});},_lI=function(_lJ,_lK){return [2];},_lL=function(_lM){var _lN=E(_lM);return _lN[0]==0?[1,new T(function(){return B(_el(new T(function(){return B(_eb(E(_lN[1])[1]));}),_ea,_lN[2]));})]:E(_lN[2])[0]==0?E(_lN[3])[0]==0?[1,new T(function(){return B(_el(_e9,_ea,_lN[1]));})]:[0]:[0];},_lO=function(_lP){var _lQ=E(_lP);if(_lQ[0]==5){var _lR=B(_lL(_lQ[1]));return _lR[0]==0?E(_lI):function(_lS,_lT){return new F(function(){return A(_lT,[_lR[1]]);});};}else{return E(_lI);}},_lU=function(_lV){return [1,function(_lW){return new F(function(){return A(_jE,[_lW,function(_lX){return E([3,_lV,_cE]);}]);});}];},_lY=new T(function(){return B(_ls(_lO,_lf,_lU));}),_lZ=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_m0=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_m1=function(_m2){while(1){var _m3=(function(_m4){var _m5=E(_m4);if(!_m5[0]){return [0];}else{var _m6=_m5[2],_m7=E(_m5[1]);if(!E(_m7[2])[0]){return [1,_m7[1],new T(function(){return B(_m1(_m6));})];}else{_m2=_m6;return null;}}})(_m2);if(_m3!=null){return _m3;}}},_m8=function(_){var _m9=jsEval("Date.now()"),_ma=_m9;return new T(function(){var _mb=B(_m1(B(_br(_lY,new T(function(){return fromJSStr(_ma);})))));return _mb[0]==0?B(err(_m0)):E(_mb[2])[0]==0?E(_mb[1]):B(err(_lZ));});},_mc=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_md=new T(function(){return B(unCStr("base"));}),_me=new T(function(){return B(unCStr("IOException"));}),_mf=new T(function(){var _mg=hs_wordToWord64(4053623282),_mh=_mg,_mi=hs_wordToWord64(3693590983),_mj=_mi;return [0,_mh,_mj,[0,_mh,_mj,_md,_mc,_me],_v];}),_mk=function(_ml){return E(_mf);},_mm=function(_mn){var _mo=E(_mn);return new F(function(){return _al(B(_aj(_mo[1])),_mk,_mo[2]);});},_mp=new T(function(){return B(unCStr(": "));}),_mq=[0,41],_mr=new T(function(){return B(unCStr(" ("));}),_ms=new T(function(){return B(unCStr("already exists"));}),_mt=new T(function(){return B(unCStr("does not exist"));}),_mu=new T(function(){return B(unCStr("protocol error"));}),_mv=new T(function(){return B(unCStr("failed"));}),_mw=new T(function(){return B(unCStr("invalid argument"));}),_mx=new T(function(){return B(unCStr("inappropriate type"));}),_my=new T(function(){return B(unCStr("hardware fault"));}),_mz=new T(function(){return B(unCStr("unsupported operation"));}),_mA=new T(function(){return B(unCStr("timeout"));}),_mB=new T(function(){return B(unCStr("resource vanished"));}),_mC=new T(function(){return B(unCStr("interrupted"));}),_mD=new T(function(){return B(unCStr("resource busy"));}),_mE=new T(function(){return B(unCStr("resource exhausted"));}),_mF=new T(function(){return B(unCStr("end of file"));}),_mG=new T(function(){return B(unCStr("illegal operation"));}),_mH=new T(function(){return B(unCStr("permission denied"));}),_mI=new T(function(){return B(unCStr("user error"));}),_mJ=new T(function(){return B(unCStr("unsatisified constraints"));}),_mK=new T(function(){return B(unCStr("system error"));}),_mL=function(_mM,_mN){switch(E(_mM)){case 0:return new F(function(){return _P(_ms,_mN);});break;case 1:return new F(function(){return _P(_mt,_mN);});break;case 2:return new F(function(){return _P(_mD,_mN);});break;case 3:return new F(function(){return _P(_mE,_mN);});break;case 4:return new F(function(){return _P(_mF,_mN);});break;case 5:return new F(function(){return _P(_mG,_mN);});break;case 6:return new F(function(){return _P(_mH,_mN);});break;case 7:return new F(function(){return _P(_mI,_mN);});break;case 8:return new F(function(){return _P(_mJ,_mN);});break;case 9:return new F(function(){return _P(_mK,_mN);});break;case 10:return new F(function(){return _P(_mu,_mN);});break;case 11:return new F(function(){return _P(_mv,_mN);});break;case 12:return new F(function(){return _P(_mw,_mN);});break;case 13:return new F(function(){return _P(_mx,_mN);});break;case 14:return new F(function(){return _P(_my,_mN);});break;case 15:return new F(function(){return _P(_mz,_mN);});break;case 16:return new F(function(){return _P(_mA,_mN);});break;case 17:return new F(function(){return _P(_mB,_mN);});break;default:return new F(function(){return _P(_mC,_mN);});}},_mO=[0,125],_mP=new T(function(){return B(unCStr("{handle: "));}),_mQ=function(_mR,_mS,_mT,_mU,_mV,_mW){var _mX=new T(function(){var _mY=new T(function(){return B(_mL(_mS,new T(function(){var _mZ=E(_mU);return _mZ[0]==0?E(_mW):B(_P(_mr,new T(function(){return B(_P(_mZ,[1,_mq,_mW]));})));})));}),_n0=E(_mT);return _n0[0]==0?E(_mY):B(_P(_n0,new T(function(){return B(_P(_mp,_mY));})));}),_n1=E(_mV);if(!_n1[0]){var _n2=E(_mR);if(!_n2[0]){return E(_mX);}else{var _n3=E(_n2[1]);return _n3[0]==0?B(_P(_mP,new T(function(){return B(_P(_n3[1],[1,_mO,new T(function(){return B(_P(_mp,_mX));})]));}))):B(_P(_mP,new T(function(){return B(_P(_n3[1],[1,_mO,new T(function(){return B(_P(_mp,_mX));})]));})));}}else{return new F(function(){return _P(_n1[1],new T(function(){return B(_P(_mp,_mX));}));});}},_n4=function(_n5){var _n6=E(_n5);return new F(function(){return _mQ(_n6[1],_n6[2],_n6[3],_n6[4],_n6[6],_v);});},_n7=function(_n8,_n9){var _na=E(_n8);return new F(function(){return _mQ(_na[1],_na[2],_na[3],_na[4],_na[6],_n9);});},_nb=function(_nc,_nd){return new F(function(){return _aG(_n7,_nc,_nd);});},_ne=function(_nf,_ng,_nh){var _ni=E(_ng);return new F(function(){return _mQ(_ni[1],_ni[2],_ni[3],_ni[4],_ni[6],_nh);});},_nj=[0,_ne,_n4,_nb],_nk=new T(function(){return [0,_mk,_nj,_nl,_mm];}),_nl=function(_nm){return [0,_nk,_nm];},_nn=7,_no=function(_np){return [0,_2w,_nn,_v,_np,_2w,_2w];},_nq=function(_nr,_){return new F(function(){return die(new T(function(){return B(_nl(new T(function(){return B(_no(_nr));})));}));});},_ns=function(_nt,_){return new F(function(){return _nq(_nt,_);});},_nu=[0,0],_nv=[0,0],_nw=function(_nx,_ny,_nz){var _nA=function(_nB,_nC){return new F(function(){return _bB([1,function(_nD){return new F(function(){return A(_jE,[_nD,function(_nE){return E(new T(function(){return B(_kU(function(_nF){var _nG=E(_nF);if(_nG[0]==4){var _nH=E(_nG[1]);if(!_nH[0]){return new F(function(){return A(_nx,[_nG,_nB,_nC]);});}else{return E(E(_nH[1])[1])==45?E(_nH[2])[0]==0?E([1,function(_nI){return new F(function(){return A(_jE,[_nI,function(_nJ){return E(new T(function(){return B(_kU(function(_nK){return new F(function(){return A(_nx,[_nK,_nB,function(_nL){return new F(function(){return A(_nC,[new T(function(){return [0, -E(_nL)[1]];})]);});}]);});}));}));}]);});}]):B(A(_nx,[_nG,_nB,_nC])):B(A(_nx,[_nG,_nB,_nC]));}}else{return new F(function(){return A(_nx,[_nG,_nB,_nC]);});}}));}));}]);});}],new T(function(){return [1,B(_lg(_nA,_nC))];}));});};return new F(function(){return _nA(_ny,_nz);});},_nM=function(_nN,_nO){return [2];},_nP=function(_nQ){var _nR=E(_nQ);if(_nR[0]==5){var _nS=B(_lL(_nR[1]));return _nS[0]==0?E(_nM):function(_nT,_nU){return new F(function(){return A(_nU,[new T(function(){return [0,B(_fo(_nS[1]))];})]);});};}else{return E(_nM);}},_nV=new T(function(){return B(_nw(_nP,_lf,_lU));}),_nW=[0,1],_nX=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:99:3-10"));}),_nY=function(_){var _nZ=B(_m8(_)),_o0=_nZ,_o1=jsFind("unread-badge"),_o2=_o1,_o3=E(_o2);if(!_o3[0]){return new F(function(){return _ns(_nX,_);});}else{var _o4=jsGet(E(_o3[1])[1],"innerHTML"),_o5=_o4;return [0,_nu,_nu,_nu,_o0,_s,_45,_45,_nu,_nW,_nW,new T(function(){var _o6=new T(function(){return fromJSStr(_o5);});if(!B(_c8(_o6,_v))){var _o7=B(_m1(B(_br(_nV,_o6)))),_o8=_o7[0]==0?B(err(_m0)):E(_o7[2])[0]==0?E(_o7[1]):B(err(_lZ));}else{var _o8=E(_nv);}return _o8;})];}},_o9=function(_){var _=0;return new F(function(){return _nY(_);});},_oa=function(_ob){var _oc=B(A(_ob,[_])),_od=_oc;return E(_od);},_oe=new T(function(){return B(_oa(_o9));}),_of=new T(function(){return B(err(_lZ));}),_og=new T(function(){return B(err(_m0));}),_oh=[0,_1F],_oi=new T(function(){return B(unCStr("Tried to deserialize a non-Number to a Double"));}),_oj=[0,_oi],_ok=[0,_2A],_ol=[0,_2C],_om=function(_on){var _oo=E(_on);if(_oo[0]==4){var _op=_oo[1],_oq=B(_2G(_2v,_a8,_op));if(!_oq[0]){return E(_ok);}else{var _or=E(_oq[1]);if(!_or[0]){var _os=_or[1],_ot=B(_2G(_2v,_a7,_op));if(!_ot[0]){return E(_ok);}else{var _ou=E(_ot[1]);if(!_ou[0]){var _ov=_ou[1],_ow=B(_2G(_2v,_a6,_op));if(!_ow[0]){return E(_ok);}else{var _ox=E(_ow[1]);if(!_ox[0]){var _oy=_ox[1],_oz=B(_2G(_2v,_a5,_op));if(!_oz[0]){return E(_ok);}else{var _oA=E(_oz[1]);if(_oA[0]==1){var _oB=_oA[1],_oC=function(_oD){var _oE=function(_oF){var _oG=B(_2G(_2v,_a2,_op));if(!_oG[0]){return E(_ok);}else{var _oH=E(_oG[1]);if(!_oH[0]){var _oI=_oH[1],_oJ=function(_oK){var _oL=function(_oM){var _oN=function(_oO){return [1,new T(function(){var _oP=E(_oe),_oQ=_oP[5],_oR=_oP[6],_oS=_oP[7],_oT=_oP[9],_oU=_oP[10],_oV=_oP[11],_oW=new T(function(){var _oX=B(_m1(B(_br(_lY,new T(function(){return fromJSStr(E(_oB)[1]);})))));return _oX[0]==0?E(_og):E(_oX[2])[0]==0?E(_oX[1]):E(_of);}),_oY=E(_oO);if(!_oY[0]){var _oZ=E(_oF);if(!_oZ[0]){var _p0=E(_oM);if(!_p0[0]){var _p1=E(_oK),_p2=_p1[0]==0?[0,_os,_ov,_oy,_oW,_oQ,_oR,_oS,_oI,_oT,_oU,_oV]:[0,_os,_ov,_oy,_oW,_oQ,_oR,_oS,_oI,_p1[1],_oU,_oV];}else{var _p3=_p0[1],_p4=E(_oK),_p2=_p4[0]==0?[0,_os,_ov,_oy,_oW,_oQ,_oR,_oS,_oI,_oT,_p3,_oV]:[0,_os,_ov,_oy,_oW,_oQ,_oR,_oS,_oI,_p4[1],_p3,_oV];}var _p5=_p2;}else{var _p6=_oZ[1],_p7=E(_oM);if(!_p7[0]){var _p8=E(_oK),_p9=_p8[0]==0?[0,_os,_ov,_oy,_oW,_oQ,_oR,_p6,_oI,_oT,_oU,_oV]:[0,_os,_ov,_oy,_oW,_oQ,_oR,_p6,_oI,_p8[1],_oU,_oV];}else{var _pa=_p7[1],_pb=E(_oK),_p9=_pb[0]==0?[0,_os,_ov,_oy,_oW,_oQ,_oR,_p6,_oI,_oT,_pa,_oV]:[0,_os,_ov,_oy,_oW,_oQ,_oR,_p6,_oI,_pb[1],_pa,_oV];}var _p5=_p9;}var _pc=_p5;}else{var _pd=_oY[1],_pe=E(_oM);if(!_pe[0]){var _pf=E(_oK),_pg=_pf[0]==0?[0,_os,_ov,_oy,_oW,_oQ,_oR,new T(function(){return B(_7M(B(_1r(_9W,_pd))));}),_oI,_oT,_oU,_oV]:[0,_os,_ov,_oy,_oW,_oQ,_oR,new T(function(){return B(_7M(B(_1r(_9W,_pd))));}),_oI,_pf[1],_oU,_oV];}else{var _ph=_pe[1],_pi=E(_oK),_pg=_pi[0]==0?[0,_os,_ov,_oy,_oW,_oQ,_oR,new T(function(){return B(_7M(B(_1r(_9W,_pd))));}),_oI,_oT,_ph,_oV]:[0,_os,_ov,_oy,_oW,_oQ,_oR,new T(function(){return B(_7M(B(_1r(_9W,_pd))));}),_oI,_pi[1],_ph,_oV];}var _pc=_pg;}var _pj=_pc;return _pj;})];},_pk=B(_2G(_2v,_9Z,_op));if(!_pk[0]){return new F(function(){return _oN(_2w);});}else{var _pl=B(_3o(_3X,_3X,_pk[1]));return _pl[0]==0?B(_oN(_2w)):B(_oN([1,_pl[1]]));}},_pm=B(_2G(_2v,_a0,_op));if(!_pm[0]){return new F(function(){return _oL(_2w);});}else{var _pn=E(_pm[1]);return _pn[0]==0?B(_oL([1,_pn[1]])):B(_oL(_2w));}},_po=B(_2G(_2v,_a1,_op));if(!_po[0]){return new F(function(){return _oJ(_2w);});}else{var _pp=E(_po[1]);return _pp[0]==0?B(_oJ([1,_pp[1]])):B(_oJ(_2w));}}else{return E(_oj);}}},_pq=B(_2G(_2v,_a3,_op));if(!_pq[0]){return new F(function(){return _oE(_2w);});}else{var _pr=B(_3o(_2b,_3X,_pq[1]));return _pr[0]==0?B(_oE(_2w)):B(_oE([1,new T(function(){return B(_7M(_pr[1]));})]));}},_ps=B(_2G(_2v,_a4,_op));if(!_ps[0]){return new F(function(){return _oC(_);});}else{var _pt=B(_3o(_2b,_3a,_ps[1]));if(!_pt[0]){return new F(function(){return _oC(_);});}else{var _pu=_pt[1],_pv=function(_pw){var _px=B(_2G(_2v,_a2,_op));if(!_px[0]){return E(_ok);}else{var _py=E(_px[1]);if(!_py[0]){var _pz=_py[1],_pA=function(_pB){var _pC=function(_pD){var _pE=function(_pF){return [1,new T(function(){var _pG=E(_oe),_pH=_pG[5],_pI=_pG[7],_pJ=_pG[9],_pK=_pG[10],_pL=_pG[11],_pM=new T(function(){var _pN=B(_m1(B(_br(_lY,new T(function(){return fromJSStr(E(_oB)[1]);})))));return _pN[0]==0?E(_og):E(_pN[2])[0]==0?E(_pN[1]):E(_of);}),_pO=E(_pF);if(!_pO[0]){var _pP=E(_pw);if(!_pP[0]){var _pQ=E(_pD);if(!_pQ[0]){var _pR=E(_pB),_pS=_pR[0]==0?[0,_os,_ov,_oy,_pM,_pH,new T(function(){return B(_7M(_pu));}),_pI,_pz,_pJ,_pK,_pL]:[0,_os,_ov,_oy,_pM,_pH,new T(function(){return B(_7M(_pu));}),_pI,_pz,_pR[1],_pK,_pL];}else{var _pT=_pQ[1],_pU=E(_pB),_pS=_pU[0]==0?[0,_os,_ov,_oy,_pM,_pH,new T(function(){return B(_7M(_pu));}),_pI,_pz,_pJ,_pT,_pL]:[0,_os,_ov,_oy,_pM,_pH,new T(function(){return B(_7M(_pu));}),_pI,_pz,_pU[1],_pT,_pL];}var _pV=_pS;}else{var _pW=_pP[1],_pX=E(_pD);if(!_pX[0]){var _pY=E(_pB),_pZ=_pY[0]==0?[0,_os,_ov,_oy,_pM,_pH,new T(function(){return B(_7M(_pu));}),_pW,_pz,_pJ,_pK,_pL]:[0,_os,_ov,_oy,_pM,_pH,new T(function(){return B(_7M(_pu));}),_pW,_pz,_pY[1],_pK,_pL];}else{var _q0=_pX[1],_q1=E(_pB),_pZ=_q1[0]==0?[0,_os,_ov,_oy,_pM,_pH,new T(function(){return B(_7M(_pu));}),_pW,_pz,_pJ,_q0,_pL]:[0,_os,_ov,_oy,_pM,_pH,new T(function(){return B(_7M(_pu));}),_pW,_pz,_q1[1],_q0,_pL];}var _pV=_pZ;}var _q2=_pV;}else{var _q3=_pO[1],_q4=E(_pD);if(!_q4[0]){var _q5=E(_pB),_q6=_q5[0]==0?[0,_os,_ov,_oy,_pM,_pH,new T(function(){return B(_7M(_pu));}),new T(function(){return B(_7M(B(_1r(_9W,_q3))));}),_pz,_pJ,_pK,_pL]:[0,_os,_ov,_oy,_pM,_pH,new T(function(){return B(_7M(_pu));}),new T(function(){return B(_7M(B(_1r(_9W,_q3))));}),_pz,_q5[1],_pK,_pL];}else{var _q7=_q4[1],_q8=E(_pB),_q6=_q8[0]==0?[0,_os,_ov,_oy,_pM,_pH,new T(function(){return B(_7M(_pu));}),new T(function(){return B(_7M(B(_1r(_9W,_q3))));}),_pz,_pJ,_q7,_pL]:[0,_os,_ov,_oy,_pM,_pH,new T(function(){return B(_7M(_pu));}),new T(function(){return B(_7M(B(_1r(_9W,_q3))));}),_pz,_q8[1],_q7,_pL];}var _q2=_q6;}var _q9=_q2;return _q9;})];},_qa=B(_2G(_2v,_9Z,_op));if(!_qa[0]){return new F(function(){return _pE(_2w);});}else{var _qb=B(_3o(_3X,_3X,_qa[1]));return _qb[0]==0?B(_pE(_2w)):B(_pE([1,_qb[1]]));}},_qc=B(_2G(_2v,_a0,_op));if(!_qc[0]){return new F(function(){return _pC(_2w);});}else{var _qd=E(_qc[1]);return _qd[0]==0?B(_pC([1,_qd[1]])):B(_pC(_2w));}},_qe=B(_2G(_2v,_a1,_op));if(!_qe[0]){return new F(function(){return _pA(_2w);});}else{var _qf=E(_qe[1]);return _qf[0]==0?B(_pA([1,_qf[1]])):B(_pA(_2w));}}else{return E(_oj);}}},_qg=B(_2G(_2v,_a3,_op));if(!_qg[0]){return new F(function(){return _pv(_2w);});}else{var _qh=B(_3o(_2b,_3X,_qg[1]));return _qh[0]==0?B(_pv(_2w)):B(_pv([1,new T(function(){return B(_7M(_qh[1]));})]));}}}}else{return E(_oh);}}}else{return E(_oj);}}}else{return E(_oj);}}}else{return E(_oj);}}}else{return E(_ol);}},_qi=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_qj=[0,_qi],_qk=[1,_v],_ql=function(_qm){var _qn=E(_qm);if(!_qn[0]){return E(_qk);}else{var _qo=B(_om(_qn[1]));if(!_qo[0]){return [0,_qo[1]];}else{var _qp=B(_ql(_qn[2]));return _qp[0]==0?[0,_qp[1]]:[1,[1,_qo[1],_qp[1]]];}}},_qq=function(_qr){var _qs=E(_qr);return _qs[0]==3?B(_ql(_qs[1])):E(_qj);},_qt=[0,_1o,_1v,_om,_qq],_qu=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_qv=new T(function(){return B(err(_qu));}),_qw=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_qx=new T(function(){return B(err(_qw));}),_qy=function(_qz,_qA){while(1){var _qB=E(_qz);if(!_qB[0]){return E(_qx);}else{var _qC=E(_qA);if(!_qC){return E(_qB[1]);}else{_qz=_qB[2];_qA=_qC-1|0;continue;}}}},_qD=new T(function(){return B(unCStr("ACK"));}),_qE=new T(function(){return B(unCStr("BEL"));}),_qF=new T(function(){return B(unCStr("BS"));}),_qG=new T(function(){return B(unCStr("SP"));}),_qH=[1,_qG,_v],_qI=new T(function(){return B(unCStr("US"));}),_qJ=[1,_qI,_qH],_qK=new T(function(){return B(unCStr("RS"));}),_qL=[1,_qK,_qJ],_qM=new T(function(){return B(unCStr("GS"));}),_qN=[1,_qM,_qL],_qO=new T(function(){return B(unCStr("FS"));}),_qP=[1,_qO,_qN],_qQ=new T(function(){return B(unCStr("ESC"));}),_qR=[1,_qQ,_qP],_qS=new T(function(){return B(unCStr("SUB"));}),_qT=[1,_qS,_qR],_qU=new T(function(){return B(unCStr("EM"));}),_qV=[1,_qU,_qT],_qW=new T(function(){return B(unCStr("CAN"));}),_qX=[1,_qW,_qV],_qY=new T(function(){return B(unCStr("ETB"));}),_qZ=[1,_qY,_qX],_r0=new T(function(){return B(unCStr("SYN"));}),_r1=[1,_r0,_qZ],_r2=new T(function(){return B(unCStr("NAK"));}),_r3=[1,_r2,_r1],_r4=new T(function(){return B(unCStr("DC4"));}),_r5=[1,_r4,_r3],_r6=new T(function(){return B(unCStr("DC3"));}),_r7=[1,_r6,_r5],_r8=new T(function(){return B(unCStr("DC2"));}),_r9=[1,_r8,_r7],_ra=new T(function(){return B(unCStr("DC1"));}),_rb=[1,_ra,_r9],_rc=new T(function(){return B(unCStr("DLE"));}),_rd=[1,_rc,_rb],_re=new T(function(){return B(unCStr("SI"));}),_rf=[1,_re,_rd],_rg=new T(function(){return B(unCStr("SO"));}),_rh=[1,_rg,_rf],_ri=new T(function(){return B(unCStr("CR"));}),_rj=[1,_ri,_rh],_rk=new T(function(){return B(unCStr("FF"));}),_rl=[1,_rk,_rj],_rm=new T(function(){return B(unCStr("VT"));}),_rn=[1,_rm,_rl],_ro=new T(function(){return B(unCStr("LF"));}),_rp=[1,_ro,_rn],_rq=new T(function(){return B(unCStr("HT"));}),_rr=[1,_rq,_rp],_rs=[1,_qF,_rr],_rt=[1,_qE,_rs],_ru=[1,_qD,_rt],_rv=new T(function(){return B(unCStr("ENQ"));}),_rw=[1,_rv,_ru],_rx=new T(function(){return B(unCStr("EOT"));}),_ry=[1,_rx,_rw],_rz=new T(function(){return B(unCStr("ETX"));}),_rA=[1,_rz,_ry],_rB=new T(function(){return B(unCStr("STX"));}),_rC=[1,_rB,_rA],_rD=new T(function(){return B(unCStr("SOH"));}),_rE=[1,_rD,_rC],_rF=new T(function(){return B(unCStr("NUL"));}),_rG=[1,_rF,_rE],_rH=[0,92],_rI=new T(function(){return B(unCStr("\\DEL"));}),_rJ=new T(function(){return B(unCStr("\\a"));}),_rK=new T(function(){return B(unCStr("\\\\"));}),_rL=new T(function(){return B(unCStr("\\SO"));}),_rM=new T(function(){return B(unCStr("\\r"));}),_rN=new T(function(){return B(unCStr("\\f"));}),_rO=new T(function(){return B(unCStr("\\v"));}),_rP=new T(function(){return B(unCStr("\\n"));}),_rQ=new T(function(){return B(unCStr("\\t"));}),_rR=new T(function(){return B(unCStr("\\b"));}),_rS=function(_rT,_rU){if(_rT<=127){var _rV=E(_rT);switch(_rV){case 92:return new F(function(){return _P(_rK,_rU);});break;case 127:return new F(function(){return _P(_rI,_rU);});break;default:if(_rV<32){var _rW=E(_rV);switch(_rW){case 7:return new F(function(){return _P(_rJ,_rU);});break;case 8:return new F(function(){return _P(_rR,_rU);});break;case 9:return new F(function(){return _P(_rQ,_rU);});break;case 10:return new F(function(){return _P(_rP,_rU);});break;case 11:return new F(function(){return _P(_rO,_rU);});break;case 12:return new F(function(){return _P(_rN,_rU);});break;case 13:return new F(function(){return _P(_rM,_rU);});break;case 14:return new F(function(){return _P(_rL,new T(function(){var _rX=E(_rU);if(!_rX[0]){var _rY=[0];}else{var _rY=E(E(_rX[1])[1])==72?B(unAppCStr("\\&",_rX)):E(_rX);}return _rY;}));});break;default:return new F(function(){return _P([1,_rH,new T(function(){var _rZ=_rW;return _rZ>=0?B(_qy(_rG,_rZ)):E(_qv);})],_rU);});}}else{return [1,[0,_rV],_rU];}}}else{return [1,_rH,new T(function(){var _s0=jsShowI(_rT),_s1=_s0;return B(_P(fromJSStr(_s1),new T(function(){var _s2=E(_rU);if(!_s2[0]){var _s3=[0];}else{var _s4=E(_s2[1])[1];if(_s4<48){var _s5=E(_s2);}else{var _s5=_s4>57?E(_s2):B(unAppCStr("\\&",_s2));}var _s6=_s5,_s7=_s6,_s3=_s7;}return _s3;})));})];}},_s8=[0,39],_s9=[1,_s8,_v],_sa=new T(function(){return B(unCStr("\'\\\'\'"));}),_sb=function(_sc){var _sd=E(E(_sc)[1]);return _sd==39?E(_sa):[1,_s8,new T(function(){return B(_rS(_sd,_s9));})];},_se=[0,34],_sf=new T(function(){return B(unCStr("\\\""));}),_sg=function(_sh,_si){var _sj=E(_sh);if(!_sj[0]){return E(_si);}else{var _sk=_sj[2],_sl=E(E(_sj[1])[1]);if(_sl==34){return new F(function(){return _P(_sf,new T(function(){return B(_sg(_sk,_si));}));});}else{return new F(function(){return _rS(_sl,new T(function(){return B(_sg(_sk,_si));}));});}}},_sm=function(_sn,_so){return [1,_se,new T(function(){return B(_sg(_sn,[1,_se,_so]));})];},_sp=function(_sq){return new F(function(){return _P(_sa,_sq);});},_sr=function(_ss,_st){var _su=E(E(_st)[1]);return _su==39?E(_sp):function(_sv){return [1,_s8,new T(function(){return B(_rS(_su,[1,_s8,_sv]));})];};},_sw=[0,_sr,_sb,_sm],_sx=function(_sy){return E(E(_sy)[3]);},_sz=function(_sA,_sB){return new F(function(){return A(_sx,[_sA,_sB,_v]);});},_sC=function(_sD,_sE,_sF){return new F(function(){return _aG(new T(function(){return B(_sx(_sD));}),_sE,_sF);});},_sG=function(_sH){return [0,function(_sI){return E(new T(function(){return B(_sx(_sH));}));},function(_sq){return new F(function(){return _sz(_sH,_sq);});},function(_sJ,_sq){return new F(function(){return _sC(_sH,_sJ,_sq);});}];},_sK=new T(function(){return B(_sG(_sw));}),_sL=new T(function(){return B(unCStr("Just "));}),_sM=new T(function(){return B(unCStr("Nothing"));}),_sN=[0,11],_sO=function(_sP){return E(E(_sP)[1]);},_sQ=function(_sR,_sS,_sT,_sU){var _sV=E(_sT);if(!_sV[0]){return new F(function(){return _P(_sM,_sU);});}else{var _sW=_sV[1];return E(_sS)[1]<=10?B(_P(_sL,new T(function(){return B(A(_sO,[_sR,_sN,_sW,_sU]));}))):[1,_18,new T(function(){return B(_P(_sL,new T(function(){return B(A(_sO,[_sR,_sN,_sW,[1,_17,_sU]]));})));})];}},_sX=[0,0],_sY=function(_sZ,_t0){return new F(function(){return _sQ(_sZ,_sX,_t0,_v);});},_t1=function(_t2,_t3,_t4){return new F(function(){return _aG(function(_sJ,_sq){return new F(function(){return _sQ(_t2,_sX,_sJ,_sq);});},_t3,_t4);});},_t5=function(_t6){return [0,function(_t7,_sJ,_sq){return new F(function(){return _sQ(_t6,_t7,_sJ,_sq);});},function(_sq){return new F(function(){return _sY(_t6,_sq);});},function(_sJ,_sq){return new F(function(){return _t1(_t6,_sJ,_sq);});}];},_t8=new T(function(){return B(_t5(_sK));}),_t9=function(_ta){var _tb=jsShow(E(_ta)[1]),_tc=_tb;return new F(function(){return fromJSStr(_tc);});},_td=function(_te){return function(_d6){return new F(function(){return _P(new T(function(){return B(_t9(_te));}),_d6);});};},_tf=function(_tg){return new F(function(){return _82(0,E(_tg)[1],_v);});},_th=function(_ti,_tj){return new F(function(){return _82(0,E(_ti)[1],_tj);});},_tk=function(_tl,_tm){return new F(function(){return _aG(_th,_tl,_tm);});},_tn=function(_to,_tp,_tq){return new F(function(){return _82(E(_to)[1],E(_tp)[1],_tq);});},_tr=[0,_tn,_tf,_tk],_ts=function(_tt,_tu,_tv){return new F(function(){return A(_tt,[[1,_aD,new T(function(){return B(A(_tu,[_tv]));})]]);});},_tw=new T(function(){return B(unCStr(": empty list"));}),_tx=new T(function(){return B(unCStr("Prelude."));}),_ty=function(_tz){return new F(function(){return err(B(_P(_tx,new T(function(){return B(_P(_tz,_tw));}))));});},_tA=new T(function(){return B(unCStr("foldr1"));}),_tB=new T(function(){return B(_ty(_tA));}),_tC=function(_tD,_tE){var _tF=E(_tE);if(!_tF[0]){return E(_tB);}else{var _tG=_tF[1],_tH=E(_tF[2]);if(!_tH[0]){return E(_tG);}else{return new F(function(){return A(_tD,[_tG,new T(function(){return B(_tC(_tD,_tH));})]);});}}},_tI=function(_tJ,_tK,_tL,_tM){return new F(function(){return _aG(function(_tN,_tO){var _tP=E(_tN);return [1,_18,new T(function(){return B(A(_tC,[_ts,[1,new T(function(){return B(A(new T(function(){return B(_sO(_tJ));}),[_sX,_tP[1]]));}),[1,new T(function(){return B(A(new T(function(){return B(_sO(_tK));}),[_sX,_tP[2]]));}),_v]],[1,_17,_tO]]));})];},_tL,_tM);});},_tQ=new T(function(){return B(unCStr("fromList "));}),_tR=function(_tS,_tT){while(1){var _tU=(function(_tV,_tW){var _tX=E(_tW);if(!_tX[0]){_tS=[1,[0,_tX[2],_tX[3]],new T(function(){return B(_tR(_tV,_tX[5]));})];_tT=_tX[4];return null;}else{return E(_tV);}})(_tS,_tT);if(_tU!=null){return _tU;}}},_tY=function(_tZ,_u0,_u1,_u2){var _u3=new T(function(){return B(_tR(_v,_u2));});return _u1<=10?function(_u4){return new F(function(){return _P(_tQ,new T(function(){return B(_tI(_tZ,_u0,_u3,_u4));}));});}:function(_u5){return [1,_18,new T(function(){return B(_P(_tQ,new T(function(){return B(_tI(_tZ,_u0,_u3,[1,_17,_u5]));})));})];};},_u6=[0,45],_u7=function(_u8,_u9,_ua){var _ub=function(_uc){var _ud=new T(function(){return B(A(_u8,[[0, -_ua]]));});return E(_u9)[1]<=6?function(_ue){return [1,_u6,new T(function(){return B(A(_ud,[_ue]));})];}:function(_uf){return [1,_18,[1,_u6,new T(function(){return B(A(_ud,[[1,_17,_uf]]));})]];};};if(_ua>=0){var _ug=isDoubleNegativeZero(_ua),_uh=_ug;return E(_uh)==0?B(A(_u8,[[0,_ua]])):B(_ub(_));}else{return new F(function(){return _ub(_);});}},_ui=[0,125],_uj=new T(function(){return B(unCStr("_logUnread = "));}),_uk=new T(function(){return B(unCStr(", "));}),_ul=new T(function(){return B(unCStr("_lpsCoeff = "));}),_um=new T(function(){return B(unCStr("_dependCoeff = "));}),_un=new T(function(){return B(unCStr("_maxLoves = "));}),_uo=new T(function(){return B(unCStr("_items = "));}),_up=new T(function(){return B(unCStr("_achieves = "));}),_uq=new T(function(){return B(unCStr("_hasFocus = "));}),_ur=new T(function(){return B(unCStr("_lastFocus = "));}),_us=new T(function(){return B(unCStr("_depend = "));}),_ut=new T(function(){return B(unCStr("_lps = "));}),_uu=new T(function(){return B(unCStr("_loves = "));}),_uv=new T(function(){return B(unCStr("Aichan {"));}),_uw=new T(function(){return B(unCStr("True"));}),_ux=new T(function(){return B(unCStr("False"));}),_uy=function(_uz,_uA,_uB,_uC,_uD,_uE,_uF,_uG,_uH,_uI,_uJ,_uK){var _uL=function(_uM){return new F(function(){return _P(_uv,new T(function(){return B(_P(_uu,new T(function(){return B(A(new T(function(){return B(_u7(_td,_nv,E(_uA)[1]));}),[new T(function(){return B(_P(_uk,new T(function(){return B(_P(_ut,new T(function(){return B(A(new T(function(){return B(_u7(_td,_nv,E(_uB)[1]));}),[new T(function(){return B(_P(_uk,new T(function(){return B(_P(_us,new T(function(){return B(A(new T(function(){return B(_u7(_td,_nv,E(_uC)[1]));}),[new T(function(){return B(_P(_uk,new T(function(){return B(_P(_ur,new T(function(){return B(_1a(0,_uD,new T(function(){return B(_P(_uk,new T(function(){return B(_P(_uq,new T(function(){var _uN=new T(function(){return B(_P(_uk,new T(function(){return B(_P(_up,new T(function(){return B(A(new T(function(){return B(_tY(_sK,_t8,0,_uF));}),[new T(function(){return B(_P(_uk,new T(function(){return B(_P(_uo,new T(function(){return B(A(new T(function(){return B(_tY(_sK,_tr,0,_uG));}),[new T(function(){return B(_P(_uk,new T(function(){return B(_P(_un,new T(function(){return B(A(new T(function(){return B(_u7(_td,_nv,E(_uH)[1]));}),[new T(function(){return B(_P(_uk,new T(function(){return B(_P(_um,new T(function(){return B(A(new T(function(){return B(_u7(_td,_nv,E(_uI)[1]));}),[new T(function(){return B(_P(_uk,new T(function(){return B(_P(_ul,new T(function(){return B(A(new T(function(){return B(_u7(_td,_nv,E(_uJ)[1]));}),[new T(function(){return B(_P(_uk,new T(function(){return B(_P(_uj,new T(function(){return B(_82(0,E(_uK)[1],[1,_ui,_uM]));})));})));})]));})));})));})]));})));})));})]));})));})));})]));})));})));})]));})));})));});return !E(_uE)?B(_P(_ux,_uN)):B(_P(_uw,_uN));})));})));})));})));})));})]));})));})));})]));})));})));})]));})));}));});};return _uz<11?E(_uL):function(_uO){return [1,_18,new T(function(){return B(_uL([1,_17,_uO]));})];};},_uP=function(_uQ){var _uR=E(_uQ);return new F(function(){return A(_uy,[0,_uR[1],_uR[2],_uR[3],_uR[4],_uR[5],_uR[6],_uR[7],_uR[8],_uR[9],_uR[10],_uR[11],_v]);});},_uS=new T(function(){return B(unCStr("Map.!: given key is not an element in the map"));}),_uT=new T(function(){return B(err(_uS));}),_uU=function(_uV,_uW){while(1){var _uX=E(_uV),_uY=E(_uW);if(!_uY[0]){switch(B(_3Y(_uX,_uY[2]))){case 0:_uV=_uX;_uW=_uY[4];continue;case 1:return E(_uY[3]);default:_uV=_uX;_uW=_uY[5];continue;}}else{return E(_uT);}}},_uZ=function(_v0,_v1,_v2,_){var _v3=rMV(_v1),_v4=_v3,_v5=B(A(_v2,[_v4,_])),_v6=_v5,_=wMV(_v1,new T(function(){return E(E(_v6)[2]);})),_v7=jsSetTimeout(_v0,function(_){var _v8=B(_uZ(_v0,_v1,_v2,_)),_v9=_v8;return _cH;});return new F(function(){return rMV(_v1);});},_va=function(_vb,_vc){while(1){var _vd=E(_vb),_ve=E(_vc);if(!_ve[0]){switch(B(_3Y(_vd,_ve[2]))){case 0:_vb=_vd;_vc=_ve[4];continue;case 1:return true;default:_vb=_vd;_vc=_ve[5];continue;}}else{return false;}}},_vf=function(_vg,_vh,_){var _vi=jsCreateTextNode(toJSStr(E(_vg))),_vj=_vi,_vk=jsAppendChild(_vj,E(_vh)[1]);return [0,_vj];},_vl=function(_vm,_vn,_vo,_vp){return new F(function(){return A(_vm,[function(_){var _vq=jsSetAttr(E(_vn)[1],toJSStr(E(_vo)),toJSStr(E(_vp)));return _cH;}]);});},_vr=function(_vs,_vt,_vu,_vv){return new F(function(){return A(_vs,[function(_){var _vw=jsSet(E(_vt)[1],toJSStr(E(_vu)),toJSStr(E(_vv)));return _cH;}]);});},_vx=new T(function(){return B(unCStr("fa fa-plus-circle"));}),_vy=new T(function(){return B(unCStr(" loves"));}),_vz=[0,32],_vA=[1,_vz,_v],_vB=new T(function(){return B(unCStr("btn btn-default btn-buy"));}),_vC=new T(function(){return B(unCStr("class"));}),_vD=new T(function(){return B(unCStr("button"));}),_vE=new T(function(){return B(unCStr("type"));}),_vF=new T(function(){return B(unCStr("item-list"));}),_vG=new T(function(){return B(unCStr("count"));}),_vH=new T(function(){return B(unCStr("tip"));}),_vI=new T(function(){return B(unCStr("list-group-item tooltips"));}),_vJ=new T(function(){return B(unCStr("button"));}),_vK=function(_vL,_vM,_vN,_){var _vO=jsCreateElem(toJSStr(E(_vJ))),_vP=_vO,_vQ=jsAppendChild(_vP,E(_vN)[1]),_vR=[0,_vP],_vS=B(A(_vL,[_vM,_vR,_])),_vT=_vS;return _vR;},_vU=[0,105],_vV=[1,_vU,_v],_vW=function(_vX,_vY,_vZ,_){var _w0=jsCreateElem(toJSStr(_vV)),_w1=_w0,_w2=jsAppendChild(_w1,E(_vZ)[1]),_w3=[0,_w1],_w4=B(A(_vX,[_vY,_w3,_])),_w5=_w4;return _w3;},_w6=new T(function(){return B(unCStr("id"));}),_w7=new T(function(){return B(unCStr("li"));}),_w8=function(_w9,_wa,_wb,_){var _wc=jsCreateElem(toJSStr(E(_w7))),_wd=_wc,_we=jsAppendChild(_wd,E(_wb)[1]),_wf=[0,_wd],_wg=B(A(_w9,[_wa,_wf,_])),_wh=_wg;return _wf;},_wi=[0,48],_wj=[1,_wi,_v],_wk=new T(function(){return B(unCStr("id"));}),_wl=new T(function(){return B(unCStr("-cost"));}),_wm=new T(function(){return B(unCStr("-btn"));}),_wn=new T(function(){return B(unCStr("-box"));}),_wo=new T(function(){return B(unCStr("-num"));}),_wp=new T(function(){return B(unCStr("-icon"));}),_wq=new T(function(){return B(unCStr("innerHTML"));}),_wr=new T(function(){return B(unCStr("span"));}),_ws=function(_wt,_wu,_wv,_){var _ww=jsCreateElem(toJSStr(E(_wr))),_wx=_ww,_wy=jsAppendChild(_wx,E(_wv)[1]),_wz=[0,_wx],_wA=B(A(_wt,[_wu,_wz,_])),_wB=_wA;return _wz;},_wC=function(_wD){return E(_wD);},_wE=function(_wF,_wG,_wH,_wI){var _wJ=new T(function(){return B(unAppCStr("item-",_wF));});return function(_wK,_){var _wL=B(_w8(_wC,function(_wM,_){var _wN=B(_ws(_wC,function(_wO,_){var _wP=B(A(_vr,[_dl,_wO,_wq,_wH,_])),_wQ=_wP;return _wO;},_wM,_)),_wR=_wN,_wS=B(A(_vl,[_dl,_wR,_vC,_vH,_])),_wT=_wS,_wU=B(_ws(_wC,function(_wV,_){var _wW=B(_ws(_wC,function(_wX,_){var _wY=B(_vW(_vf,_v,_wX,_)),_wZ=_wY,_x0=B(A(_vl,[_dl,_wZ,_vC,new T(function(){return B(unAppCStr("fa ",_wG));}),_])),_x1=_x0,_x2=B(_vf(_vA,_wX,_)),_x3=_x2;return _wX;},_wV,_)),_x4=_wW,_x5=B(A(_vl,[_dl,_x4,_wk,new T(function(){return B(_P(_wJ,_wp));}),_])),_x6=_x5,_x7=B(_ws(_vf,_v,_wV,_)),_x8=_x7,_x9=B(A(_vl,[_dl,_x8,_wk,new T(function(){return B(_P(_wJ,_wo));}),_])),_xa=_x9;return _wV;},_wM,_)),_xb=_wU,_xc=B(A(_vl,[_dl,_xb,_vC,_vG,_])),_xd=_xc,_xe=B(_ws(_vf,_wI,_wM,_)),_xf=_xe,_xg=B(A(_vl,[_dl,_xf,_wk,new T(function(){return B(_P(_wJ,_wn));}),_])),_xh=_xg,_xi=B(A(_vl,[_dl,_xf,_vC,_vF,_])),_xj=_xi,_xk=B(_vK(_wC,function(_xl,_){var _xm=B(_vW(_vf,_v,_xl,_)),_xn=_xm,_xo=B(A(_vl,[_dl,_xn,_vC,_vx,_])),_xp=_xo,_xq=B(_vf(_vA,_xl,_)),_xr=_xq,_xs=B(_ws(_vf,_wj,_xl,_)),_xt=_xs,_xu=B(A(_vl,[_dl,_xt,_wk,new T(function(){return B(_P(_wJ,_wl));}),_])),_xv=_xu,_xw=B(_vf(_vy,_xl,_)),_xx=_xw;return _xl;},_wM,_)),_xy=_xk,_xz=B(A(_vl,[_dl,_xy,_vE,_vD,_])),_xA=_xz,_xB=B(A(_vl,[_dl,_xy,_wk,new T(function(){return B(_P(_wJ,_wm));}),_])),_xC=_xB,_xD=B(A(_vl,[_dl,_xy,_vC,_vB,_])),_xE=_xD;return _wM;},_wK,_)),_xF=_wL,_xG=B(A(_vl,[_dl,_xF,_w6,_wJ,_])),_xH=_xG,_xI=B(A(_vl,[_dl,_xF,_vC,_vI,_])),_xJ=_xI;return _xF;};},_xK=function(_xL,_xM,_xN,_xO){return new F(function(){return A(_xL,[function(_){var _xP=jsSetStyle(E(_xM)[1],toJSStr(E(_xN)),toJSStr(E(_xO)));return _cH;}]);});},_xQ=function(_xR){return E(_xR);},_xS=function(_xT,_xU){if(_xT<=0){if(_xT>=0){return new F(function(){return quot(_xT,_xU);});}else{if(_xU<=0){return new F(function(){return quot(_xT,_xU);});}else{return quot(_xT+1|0,_xU)-1|0;}}}else{if(_xU>=0){if(_xT>=0){return new F(function(){return quot(_xT,_xU);});}else{if(_xU<=0){return new F(function(){return quot(_xT,_xU);});}else{return quot(_xT+1|0,_xU)-1|0;}}}else{return quot(_xT-1|0,_xU)-1|0;}}},_xV=new T(function(){return B(unCStr("ArithException"));}),_xW=new T(function(){return B(unCStr("GHC.Exception"));}),_xX=new T(function(){return B(unCStr("base"));}),_xY=new T(function(){var _xZ=hs_wordToWord64(4194982440),_y0=_xZ,_y1=hs_wordToWord64(3110813675),_y2=_y1;return [0,_y0,_y2,[0,_y0,_y2,_xX,_xW,_xV],_v];}),_y3=function(_y4){return E(_xY);},_y5=function(_y6){var _y7=E(_y6);return new F(function(){return _al(B(_aj(_y7[1])),_y3,_y7[2]);});},_y8=new T(function(){return B(unCStr("arithmetic underflow"));}),_y9=new T(function(){return B(unCStr("arithmetic overflow"));}),_ya=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_yb=new T(function(){return B(unCStr("denormal"));}),_yc=new T(function(){return B(unCStr("divide by zero"));}),_yd=new T(function(){return B(unCStr("loss of precision"));}),_ye=function(_yf){switch(E(_yf)){case 0:return E(_y9);case 1:return E(_y8);case 2:return E(_yd);case 3:return E(_yc);case 4:return E(_yb);default:return E(_ya);}},_yg=function(_yh){return new F(function(){return _P(_y8,_yh);});},_yi=function(_yh){return new F(function(){return _P(_y9,_yh);});},_yj=function(_yh){return new F(function(){return _P(_ya,_yh);});},_yk=function(_yh){return new F(function(){return _P(_yb,_yh);});},_yl=function(_yh){return new F(function(){return _P(_yc,_yh);});},_ym=function(_yh){return new F(function(){return _P(_yd,_yh);});},_yn=function(_yo){switch(E(_yo)){case 0:return E(_yi);case 1:return E(_yg);case 2:return E(_ym);case 3:return E(_yl);case 4:return E(_yk);default:return E(_yj);}},_yp=function(_yq,_yr){return new F(function(){return _aG(_yn,_yq,_yr);});},_ys=function(_yt,_yu){switch(E(_yu)){case 0:return E(_yi);case 1:return E(_yg);case 2:return E(_ym);case 3:return E(_yl);case 4:return E(_yk);default:return E(_yj);}},_yv=[0,_ys,_ye,_yp],_yw=new T(function(){return [0,_y3,_yv,_yx,_y5];}),_yx=function(_yh){return [0,_yw,_yh];},_yy=3,_yz=new T(function(){return B(_yx(_yy));}),_yA=new T(function(){return die(_yz);}),_yB=0,_yC=new T(function(){return B(_yx(_yB));}),_yD=new T(function(){return die(_yC);}),_yE=function(_yF,_yG){var _yH=E(_yG);switch(_yH){case -1:var _yI=E(_yF);return _yI==(-2147483648)?E(_yD):B(_xS(_yI,-1));case 0:return E(_yA);default:return new F(function(){return _xS(_yF,_yH);});}},_yJ=function(_yK,_yL){var _yM=_yK%_yL;if(_yK<=0){if(_yK>=0){return E(_yM);}else{if(_yL<=0){return E(_yM);}else{var _yN=E(_yM);return _yN==0?0:_yN+_yL|0;}}}else{if(_yL>=0){if(_yK>=0){return E(_yM);}else{if(_yL<=0){return E(_yM);}else{var _yO=E(_yM);return _yO==0?0:_yO+_yL|0;}}}else{var _yP=E(_yM);return _yP==0?0:_yP+_yL|0;}}},_yQ=function(_yR,_yS,_yT){while(1){var _yU=(function(_yV,_yW,_yX){var _yY=E(_yX);if(!_yY[0]){return [0,_yV,_yW];}else{var _yZ=_yY[1];_yR=new T(function(){var _z0=E(E(_yZ)[1]);switch(_z0){case -1:var _z1=[0,0];break;case 0:var _z1=E(_yA);break;default:var _z1=[0,B(_yJ(E(_yV)[1],_z0))];}var _z2=_z1;return _z2;});var _z3=[1,new T(function(){return [0,B(_yE(E(_yV)[1],E(_yZ)[1]))];}),_yW];_yT=_yY[2];_yS=_z3;return null;}})(_yR,_yS,_yT);if(_yU!=null){return _yU;}}},_z4=function(_z5,_z6,_z7,_z8){return new F(function(){return _yQ(new T(function(){var _z9=E(E(_z7)[1]);switch(_z9){case -1:var _za=[0,0];break;case 0:var _za=E(_yA);break;default:var _za=[0,B(_yJ(E(_z5)[1],_z9))];}var _zb=_za;return _zb;}),[1,new T(function(){return [0,B(_yE(E(_z5)[1],E(_z7)[1]))];}),_z6],_z8);});},_zc=function(_zd,_ze){while(1){var _zf=E(_zd),_zg=E(_ze);if(!_zg[0]){switch(B(_3Y(_zf,_zg[2]))){case 0:_zd=_zf;_ze=_zg[4];continue;case 1:return [1,_zg[3]];default:_zd=_zf;_ze=_zg[5];continue;}}else{return [0];}}},_zh=[0,500],_zi=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:333:9-14"));}),_zj=[1,_9S,_v],_zk=[1,_9C,_zj],_zl=[0,10],_zm=[1,_zl,_zk],_zn=[0,50],_zo=[1,_zn,_zm],_zp=[0,100],_zq=[1,_zp,_zo],_zr=function(_zs){var _zt=E(_zs);if(!_zt[0]){return [0];}else{return new F(function(){return _P(_zt[1],new T(function(){return B(_zr(_zt[2]));}));});}},_zu=[1,_zh,_zq],_zv=function(_zw){var _zx=E(_zw);return _zx[0]==0?_zx[1]:I_toNumber(_zx[1]);},_zy=new T(function(){return B(unCStr("block"));}),_zz=new T(function(){return B(unCStr("display"));}),_zA=new T(function(){return B(unCStr("disabled"));}),_zB=new T(function(){return B(unCStr("innerHTML"));}),_zC=new T(function(){return B(unCStr("%s<span class=\"item-%d\">%s</span>"));}),_zD=[0,-2147483648],_zE=new T(function(){return B(_zr(_v));}),_zF=new T(function(){return B(_1r(_xQ,_zE));}),_zG=function(_zH,_zI,_){var _zJ=B(A(_zH,[_])),_zK=_zJ;return new F(function(){return A(_zI,[_zK,_]);});},_zL=function(_zM,_){return _zM;},_zN=function(_zO,_zP,_){var _zQ=B(A(_zO,[_])),_zR=_zQ;return new F(function(){return A(_zP,[_]);});},_zS=[0,_zG,_zN,_zL,_ns],_zT=[0,_zS,_dl],_zU=function(_zV){return E(E(_zV)[1]);},_zW=function(_zX){return E(E(_zX)[1]);},_zY=function(_zZ){return E(E(_zZ)[2]);},_A0=function(_A1){return E(E(_A1)[3]);},_A2=function(_A3,_A4){var _A5=new T(function(){return B(_zU(_A3));});return function(_A6){return new F(function(){return A(new T(function(){return B(_zW(_A5));}),[new T(function(){return B(A(_zY,[_A3,_A4]));}),function(_A7){return new F(function(){return A(new T(function(){return B(_A0(_A5));}),[[0,_A7,_A6]]);});}]);});};},_A8=function(_A9){return new F(function(){return _A2(_zT,_A9);});},_Aa=function(_Ab,_){return [0,_cH,_Ab];},_Ac=function(_Ad){return E(E(_Ad)[1]);},_Ae=new T(function(){return B(unCStr("Negative exponent"));}),_Af=new T(function(){return B(err(_Ae));}),_Ag=function(_Ah,_Ai,_Aj){while(1){if(!(_Ai%2)){var _Ak=_Ah*_Ah,_Al=quot(_Ai,2);_Ah=_Ak;_Ai=_Al;continue;}else{var _Am=E(_Ai);if(_Am==1){return _Ah*_Aj;}else{var _Ak=_Ah*_Ah;_Ai=quot(_Am-1|0,2);var _An=_Ah*_Aj;_Ah=_Ak;_Aj=_An;continue;}}}},_Ao=function(_Ap,_Aq){while(1){if(!(_Aq%2)){var _Ar=_Ap*_Ap,_As=quot(_Aq,2);_Ap=_Ar;_Aq=_As;continue;}else{var _At=E(_Aq);if(_At==1){return E(_Ap);}else{return new F(function(){return _Ag(_Ap*_Ap,quot(_At-1|0,2),_Ap);});}}}},_Au=function(_Av){var _Aw=I_decodeDouble(_Av);return [0,[1,_Aw[2]],_Aw[1]];},_Ax=function(_Ay){var _Az=hs_intToInt64(2147483647),_AA=_Az,_AB=hs_leInt64(_Ay,_AA),_AC=_AB;if(!E(_AC)){return [1,I_fromInt64(_Ay)];}else{var _AD=hs_intToInt64(-2147483648),_AE=_AD,_AF=hs_geInt64(_Ay,_AE),_AG=_AF;if(!E(_AG)){return [1,I_fromInt64(_Ay)];}else{var _AH=hs_int64ToInt(_Ay),_AI=_AH;return new F(function(){return _eb(_AI);});}}},_AJ=function(_AK){var _AL=hs_intToInt64(_AK),_AM=_AL;return E(_AM);},_AN=function(_AO){var _AP=E(_AO);return _AP[0]==0?B(_AJ(_AP[1])):I_toInt64(_AP[1]);},_AQ=[0,0],_AR=new T(function(){return [0,0/0];}),_AS=new T(function(){return [0,-1/0];}),_AT=new T(function(){return [0,1/0];}),_AU=[0,0],_AV=function(_AW,_AX){while(1){var _AY=E(_AW);if(!_AY[0]){_AW=[1,I_fromInt(_AY[1])];continue;}else{var _AZ=E(_AX);if(!_AZ[0]){_AW=_AY;_AX=[1,I_fromInt(_AZ[1])];continue;}else{return new F(function(){return I_fromRat(_AY[1],_AZ[1]);});}}}},_B0=function(_B1,_B2){var _B3=E(_B1);if(!_B3[0]){var _B4=_B3[1],_B5=E(_B2);return _B5[0]==0?_B4==_B5[1]:I_compareInt(_B5[1],_B4)==0?true:false;}else{var _B6=_B3[1],_B7=E(_B2);return _B7[0]==0?I_compareInt(_B6,_B7[1])==0?true:false:I_compare(_B6,_B7[1])==0?true:false;}},_B8=function(_B9,_Ba){return !B(_B0(_Ba,_AU))?[0,B(_AV(_B9,_Ba))]:!B(_B0(_B9,_AU))?!B(_Z(_B9,_AU))?E(_AT):E(_AS):E(_AR);},_Bb=[0,5],_Bc=[0,4],_Bd=new T(function(){return B(_B8(_Bb,_Bc));}),_Be=[0,-1],_Bf=function(_Bg,_Bh){while(1){var _Bi=E(_Bg);if(!_Bi[0]){_Bg=[1,I_fromInt(_Bi[1])];continue;}else{return [1,I_shiftLeft(_Bi[1],_Bh)];}}},_Bj=function(_Bk,_Bl){if(_Bl>=0){var _Bm=function(_Bn){var _Bo=B(_Au(_Bk*_Bn)),_Bp=_Bo[1],_Bq=_Bo[2];if(_Bq>=0){return new F(function(){return _Bf(_Bp,_Bq);});}else{var _Br= -_Bq;if(_Br<=52){var _Bs=hs_uncheckedIShiftRA64(B(_AN(_Bp)),_Br),_Bt=_Bs;return new F(function(){return _Ax(_Bt);});}else{return !B(_Z(_Bp,_AQ))?E(_AQ):E(_Be);}}},_Bu=E(_Bl);if(!_Bu){return new F(function(){return _Bm(1);});}else{return new F(function(){return _Bm(B(_Ao(E(_Bd)[1],_Bu)));});}}else{return E(_Af);}},_Bv=function(_Bw){return new F(function(){return _Bj(1,E(_Bw)[1]);});},_Bx=new T(function(){return B(unCStr("\u4f1a\u8a71"));}),_By=new T(function(){return B(unCStr("\u4f1a\u8a71<br>\u597d\u611f\u5ea6 +0.2"));}),_Bz=new T(function(){return B(unCStr("fa-comments-o"));}),_BA=[0,_Bz,_By,_Bx],_BB=function(_BC,_BD,_){return [0,_cH,new T(function(){var _BE=E(_BD);return [0,_BE[1],new T(function(){return [0,E(_BE[2])[1]+0.2];}),_BE[3],_BE[4],_BE[5],_BE[6],_BE[7],_BE[8],_BE[9],_BE[10],_BE[11]];})];},_BF=[0,_Bv,_BB,_BA],_BG=[0,_9R,_BF],_BH=function(_BI){return new F(function(){return _Bj(50,E(_BI)[1]);});},_BJ=new T(function(){return B(unCStr("\u30e1\u30fc\u30eb"));}),_BK=new T(function(){return B(unCStr("\u30e1\u30fc\u30eb<br>\u597d\u611f\u5ea6 +1.0"));}),_BL=new T(function(){return B(unCStr("fa-envelope"));}),_BM=[0,_BL,_BK,_BJ],_BN=function(_BO,_BP,_){return [0,_cH,new T(function(){var _BQ=E(_BP);return [0,_BQ[1],new T(function(){return [0,E(_BQ[2])[1]+1];}),_BQ[3],_BQ[4],_BQ[5],_BQ[6],_BQ[7],_BQ[8],_BQ[9],_BQ[10],_BQ[11]];})];},_BR=[0,_BH,_BN,_BM],_BS=[0,_9N,_BR],_BT=function(_BU){return new F(function(){return _Bj(1000,E(_BU)[1]);});},_BV=new T(function(){return B(unCStr("\u55ab\u8336\u5e97"));}),_BW=new T(function(){return B(unCStr("\u55ab\u8336\u5e97<br>\u597d\u611f\u5ea6 +10"));}),_BX=new T(function(){return B(unCStr("fa-coffee"));}),_BY=[0,_BX,_BW,_BV],_BZ=function(_C0,_C1,_){return [0,_cH,new T(function(){var _C2=E(_C1);return [0,_C2[1],new T(function(){return [0,E(_C2[2])[1]+10];}),_C2[3],_C2[4],_C2[5],_C2[6],_C2[7],_C2[8],_C2[9],_C2[10],_C2[11]];})];},_C3=[0,_BT,_BZ,_BY],_C4=[0,_9J,_C3],_C5=function(_C6){return new F(function(){return _Bj(20000,E(_C6)[1]);});},_C7=new T(function(){return B(unCStr("\u30d7\u30ec\u30bc\u30f3\u30c8"));}),_C8=new T(function(){return B(unCStr("\u30d7\u30ec\u30bc\u30f3\u30c8<br>\u597d\u611f\u5ea6 +100"));}),_C9=new T(function(){return B(unCStr("fa-gift"));}),_Ca=[0,_C9,_C8,_C7],_Cb=function(_Cc,_Cd,_){return [0,_cH,new T(function(){var _Ce=E(_Cd);return [0,_Ce[1],new T(function(){return [0,E(_Ce[2])[1]+100];}),_Ce[3],_Ce[4],_Ce[5],_Ce[6],_Ce[7],_Ce[8],_Ce[9],_Ce[10],_Ce[11]];})];},_Cf=[0,_C5,_Cb,_Ca],_Cg=[0,_9F,_Cf],_Ch=function(_Ci){return new F(function(){return _Bj(500000,E(_Ci)[1]);});},_Cj=new T(function(){return B(unCStr("\u65c5\u884c"));}),_Ck=new T(function(){return B(unCStr("\u65c5\u884c<br>\u597d\u611f\u5ea6 +1000"));}),_Cl=new T(function(){return B(unCStr("fa-plane"));}),_Cm=[0,_Cl,_Ck,_Cj],_Cn=function(_Co,_Cp,_){return [0,_cH,new T(function(){var _Cq=E(_Cp);return [0,_Cq[1],new T(function(){return [0,E(_Cq[2])[1]+1000];}),_Cq[3],_Cq[4],_Cq[5],_Cq[6],_Cq[7],_Cq[8],_Cq[9],_Cq[10],_Cq[11]];})];},_Cr=[0,_Ch,_Cn,_Cm],_Cs=[0,_9B,_Cr],_Ct=function(_Cu){return new F(function(){return _Bj(10000000,E(_Cu)[1]);});},_Cv=[0,36554],_Cw=[1,_Cv,_v],_Cx=new T(function(){return B(unCStr("\u8eca<br>\u597d\u611f\u5ea6 +15000"));}),_Cy=new T(function(){return B(unCStr("fa-car"));}),_Cz=[0,_Cy,_Cx,_Cw],_CA=function(_CB,_CC,_){return [0,_cH,new T(function(){var _CD=E(_CC);return [0,_CD[1],new T(function(){return [0,E(_CD[2])[1]+15000];}),_CD[3],_CD[4],_CD[5],_CD[6],_CD[7],_CD[8],_CD[9],_CD[10],_CD[11]];})];},_CE=[0,_Ct,_CA,_Cz],_CF=[0,_8U,_CE],_CG=function(_CH){return new F(function(){return _Bj(250000000,E(_CH)[1]);});},_CI=[0,23478],_CJ=[1,_CI,_v],_CK=new T(function(){return B(unCStr("\u5bb6<br>\u597d\u611f\u5ea6 +200000"));}),_CL=new T(function(){return B(unCStr("fa-home"));}),_CM=[0,_CL,_CK,_CJ],_CN=function(_CO,_CP,_){return [0,_cH,new T(function(){var _CQ=E(_CP);return [0,_CQ[1],new T(function(){return [0,E(_CQ[2])[1]+200000];}),_CQ[3],_CQ[4],_CQ[5],_CQ[6],_CQ[7],_CQ[8],_CQ[9],_CQ[10],_CQ[11]];})];},_CR=[0,_CG,_CN,_CM],_CS=[0,_9w,_CR],_CT=function(_CU){var _CV=B(_Au(100000*_CU)),_CW=_CV[1],_CX=_CV[2];if(_CX>=0){return new F(function(){return _Bf(_CW,_CX);});}else{var _CY= -_CX;if(_CY<=52){var _CZ=hs_uncheckedIShiftRA64(B(_AN(_CW)),_CY),_D0=_CZ;return new F(function(){return _Ax(_D0);});}else{return !B(_Z(_CW,_AQ))?E(_AQ):E(_Be);}}},_D1=new T(function(){return B(_CT(1));}),_D2=function(_D3){if(_D3>=0){var _D4=E(_D3);if(!_D4){return E(_D1);}else{return new F(function(){return _CT(B(_Ao(3,_D4)));});}}else{return E(_Af);}},_D5=function(_D6){return new F(function(){return _D2(E(_D6)[1]);});},_D7=new T(function(){return B(unCStr("fa-eye"));}),_D8=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u53f3\u76ee"));}),_D9=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u53f3\u76ee<br>\u4f9d\u5b58\u5ea6\u30dc\u30fc\u30ca\u30b9\u304c\u5897\u3048\u307e\u3059\u3002"));}),_Da=[0,_D7,_D9,_D8],_Db=[0,1],_Dc=function(_Dd){return function(_De,_){return [0,_cH,new T(function(){var _Df=E(_De);return [0,_Df[1],_Df[2],_Df[3],_Df[4],_Df[5],_Df[6],_Df[7],_Df[8],new T(function(){return [0,E(_Df[9])[1]+E(new T(function(){var _Dg=E(_Dd)[1];if(_Dg>=0){var _Dh=E(_Dg);if(!_Dh){var _Di=E(_Db);}else{var _Di=[0,B(_Ao(2,_Dh))];}var _Dj=_Di;}else{var _Dj=E(_Af);}var _Dk=_Dj,_Dl=_Dk;return _Dl;}))[1]];}),_Df[10],_Df[11]];})];};},_Dm=[0,_D5,_Dc,_Da],_Dn=[0,_96,_Dm],_Do=function(_Dp){var _Dq=B(_Au(100000*_Dp)),_Dr=_Dq[1],_Ds=_Dq[2];if(_Ds>=0){return new F(function(){return _Bf(_Dr,_Ds);});}else{var _Dt= -_Ds;if(_Dt<=52){var _Du=hs_uncheckedIShiftRA64(B(_AN(_Dr)),_Dt),_Dv=_Du;return new F(function(){return _Ax(_Dv);});}else{return !B(_Z(_Dr,_AQ))?E(_AQ):E(_Be);}}},_Dw=new T(function(){return B(_Do(1));}),_Dx=function(_Dy){if(_Dy>=0){var _Dz=E(_Dy);if(!_Dz){return E(_Dw);}else{return new F(function(){return _Do(B(_Ao(3,_Dz)));});}}else{return E(_Af);}},_DA=function(_DB){return new F(function(){return _Dx(E(_DB)[1]);});},_DC=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u5de6\u76ee"));}),_DD=new T(function(){return B(unCStr("\u30a2\u30a4\u3061\u3083\u3093\u306e\u5de6\u76ee<br>\u4f9d\u5b58\u5ea6\u304c\u597d\u611f\u5ea6\u306b\u5909\u308f\u308b\u901f\u3055\u304c\u901f\u304f\u306a\u308a\u307e\u3059\u3002"));}),_DE=[0,_D7,_DD,_DC],_DF=function(_DG){return function(_DH,_){return [0,_cH,new T(function(){var _DI=E(_DH);return [0,_DI[1],_DI[2],_DI[3],_DI[4],_DI[5],_DI[6],_DI[7],_DI[8],_DI[9],new T(function(){return [0,E(_DI[10])[1]+E(new T(function(){var _DJ=E(_DG)[1];if(_DJ>=0){var _DK=E(_DJ);if(!_DK){var _DL=E(_Db);}else{var _DL=[0,B(_Ao(2,_DK))];}var _DM=_DL;}else{var _DM=E(_Af);}var _DN=_DM,_DO=_DN;return _DO;}))[1]];}),_DI[11]];})];};},_DP=[0,_DA,_DF,_DE],_DQ=[0,_8W,_DP],_DR=function(_DS){return E(_AQ);},_DT=function(_DU,_){var _DV=B(_m8(_)),_DW=_DV;return [0,_cH,new T(function(){var _DX=E(_DU);return [0,_DX[1],_DX[2],_DX[3],_DW,_DX[5],_DX[6],_DX[7],_DX[8],_DX[9],_DX[10],_DX[11]];})];},_DY=new T(function(){return B(unCStr("none"));}),_DZ=new T(function(){return B(unCStr("item-%s"));}),_E0=function(_E1,_E2){while(1){var _E3=E(_E1);if(!_E3[0]){return E(_E2);}else{_E1=_E3[2];var _E4=[1,_E3[1],_E2];_E2=_E4;continue;}}},_E5=function(_E6){var _E7=E(_E6)[1];return [0,Math.log(_E7+(_E7+1)*Math.sqrt((_E7-1)/(_E7+1)))];},_E8=function(_E9){var _Ea=E(_E9)[1];return [0,Math.log(_Ea+Math.sqrt(1+_Ea*_Ea))];},_Eb=function(_Ec){var _Ed=E(_Ec)[1];return [0,0.5*Math.log((1+_Ed)/(1-_Ed))];},_Ee=function(_Ef,_Eg){return [0,Math.log(E(_Eg)[1])/Math.log(E(_Ef)[1])];},_Eh=[0,3.141592653589793],_Ei=function(_Ej){var _Ek=E(_Ej);return new F(function(){return _B8(_Ek[1],_Ek[2]);});},_El=function(_Em){return [0,1/E(_Em)[1]];},_En=function(_Eo){var _Ep=E(_Eo),_Eq=_Ep[1];return _Eq<0?[0, -_Eq]:E(_Ep);},_Er=function(_Es){return [0,B(_zv(_Es))];},_Et=[0,0],_Eu=[0,1],_Ev=[0,-1],_Ew=function(_Ex){var _Ey=E(E(_Ex)[1]);return _Ey==0?E(_Et):_Ey<=0?E(_Ev):E(_Eu);},_Ez=function(_EA,_EB){return [0,E(_EA)[1]-E(_EB)[1]];},_EC=function(_ED){return [0, -E(_ED)[1]];},_EE=function(_EF,_EG){return [0,E(_EF)[1]+E(_EG)[1]];},_EH=function(_EI,_EJ){return [0,E(_EI)[1]*E(_EJ)[1]];},_EK=[0,_EE,_EH,_Ez,_EC,_En,_Ew,_Er],_EL=function(_EM,_EN){return [0,E(_EM)[1]/E(_EN)[1]];},_EO=[0,_EK,_EL,_El,_Ei],_EP=function(_EQ){return [0,Math.acos(E(_EQ)[1])];},_ER=function(_ES){return [0,Math.asin(E(_ES)[1])];},_ET=function(_EU){return [0,Math.atan(E(_EU)[1])];},_EV=function(_EW){return [0,Math.cos(E(_EW)[1])];},_EX=function(_EY){return [0,cosh(E(_EY)[1])];},_EZ=function(_F0){return [0,Math.exp(E(_F0)[1])];},_F1=function(_F2){return [0,Math.log(E(_F2)[1])];},_F3=function(_F4,_F5){return [0,Math.pow(E(_F4)[1],E(_F5)[1])];},_F6=function(_F7){return [0,Math.sin(E(_F7)[1])];},_F8=function(_F9){return [0,sinh(E(_F9)[1])];},_Fa=function(_Fb){return [0,Math.sqrt(E(_Fb)[1])];},_Fc=function(_Fd){return [0,Math.tan(E(_Fd)[1])];},_Fe=function(_Ff){return [0,tanh(E(_Ff)[1])];},_Fg=[0,_EO,_Eh,_EZ,_Fa,_F1,_F3,_Ee,_F6,_Fc,_EV,_ER,_ET,_EP,_F8,_Fe,_EX,_E8,_Eb,_E5],_Fh=function(_Fi){var _Fj=E(_Fi)[1];return [0,Math.log(_Fj+(_Fj+1)*Math.sqrt((_Fj-1)/(_Fj+1)))];},_Fk=function(_Fl){var _Fm=E(_Fl)[1];return [0,Math.log(_Fm+Math.sqrt(1+_Fm*_Fm))];},_Fn=function(_Fo){var _Fp=E(_Fo)[1];return [0,0.5*Math.log((1+_Fp)/(1-_Fp))];},_Fq=function(_Fr,_Fs){return [0,Math.log(E(_Fs)[1])/Math.log(E(_Fr)[1])];},_Ft=[0,3.141592653589793],_Fu=new T(function(){return [0,0/0];}),_Fv=new T(function(){return [0,-1/0];}),_Fw=new T(function(){return [0,1/0];}),_Fx=function(_Fy,_Fz){return !B(_B0(_Fz,_AU))?[0,B(_AV(_Fy,_Fz))]:!B(_B0(_Fy,_AU))?!B(_Z(_Fy,_AU))?E(_Fw):E(_Fv):E(_Fu);},_FA=function(_FB){var _FC=E(_FB);return new F(function(){return _Fx(_FC[1],_FC[2]);});},_FD=function(_FE){return [0,1/E(_FE)[1]];},_FF=function(_FG){var _FH=E(_FG),_FI=_FH[1];return _FI<0?[0, -_FI]:E(_FH);},_FJ=function(_FK){var _FL=E(_FK);return _FL[0]==0?_FL[1]:I_toNumber(_FL[1]);},_FM=function(_FN){return [0,B(_FJ(_FN))];},_FO=[0,0],_FP=[0,1],_FQ=[0,-1],_FR=function(_FS){var _FT=E(E(_FS)[1]);return _FT==0?E(_FO):_FT<=0?E(_FQ):E(_FP);},_FU=function(_FV,_FW){return [0,E(_FV)[1]-E(_FW)[1]];},_FX=function(_FY){return [0, -E(_FY)[1]];},_FZ=function(_G0,_G1){return [0,E(_G0)[1]+E(_G1)[1]];},_G2=function(_G3,_G4){return [0,E(_G3)[1]*E(_G4)[1]];},_G5=[0,_FZ,_G2,_FU,_FX,_FF,_FR,_FM],_G6=function(_G7,_G8){return [0,E(_G7)[1]/E(_G8)[1]];},_G9=[0,_G5,_G6,_FD,_FA],_Ga=function(_Gb){return [0,Math.acos(E(_Gb)[1])];},_Gc=function(_Gd){return [0,Math.asin(E(_Gd)[1])];},_Ge=function(_Gf){return [0,Math.atan(E(_Gf)[1])];},_Gg=function(_Gh){return [0,Math.cos(E(_Gh)[1])];},_Gi=function(_Gj){return [0,cosh(E(_Gj)[1])];},_Gk=function(_Gl){return [0,Math.exp(E(_Gl)[1])];},_Gm=function(_Gn){return [0,Math.log(E(_Gn)[1])];},_Go=function(_Gp,_Gq){return [0,Math.pow(E(_Gp)[1],E(_Gq)[1])];},_Gr=function(_Gs){return [0,Math.sin(E(_Gs)[1])];},_Gt=function(_Gu){return [0,sinh(E(_Gu)[1])];},_Gv=function(_Gw){return [0,Math.sqrt(E(_Gw)[1])];},_Gx=function(_Gy){return [0,Math.tan(E(_Gy)[1])];},_Gz=function(_GA){return [0,tanh(E(_GA)[1])];},_GB=[0,_G9,_Ft,_Gk,_Gv,_Gm,_Go,_Fq,_Gr,_Gx,_Gg,_Gc,_Ge,_Ga,_Gt,_Gz,_Gi,_Fk,_Fn,_Fh],_GC=function(_GD){var _GE=B(_Au(E(_GD)[1]));return [0,_GE[1],[0,_GE[2]]];},_GF=[0,53],_GG=function(_GH){return E(_GF);},_GI=[0,2],_GJ=function(_GK){return E(_GI);},_GL=[0,1024],_GM=[0,-1021],_GN=[0,_GM,_GL],_GO=function(_GP){return E(_GN);},_GQ=function(_GR){var _GS=isDoubleInfinite(E(_GR)[1]),_GT=_GS;return E(_GT)==0?false:true;},_GU=function(_GV){var _GW=isDoubleNaN(E(_GV)[1]),_GX=_GW;return E(_GX)==0?false:true;},_GY=function(_GZ){var _H0=isDoubleNegativeZero(E(_GZ)[1]),_H1=_H0;return E(_H1)==0?false:true;},_H2=function(_H3){var _H4=decodeFloat(E(_H3)[1]);return [0,new T(function(){return B(_eb(_H4[1]));}),[0,_H4[2]]];},_H5=[0,24],_H6=function(_H7){return E(_H5);},_H8=function(_H9){return E(_GI);},_Ha=[0,128],_Hb=[0,-125],_Hc=[0,_Hb,_Ha],_Hd=function(_He){return E(_Hc);},_Hf=function(_Hg){var _Hh=isFloatInfinite(E(_Hg)[1]),_Hi=_Hh;return E(_Hi)==0?false:true;},_Hj=function(_Hk){var _Hl=isFloatNaN(E(_Hk)[1]),_Hm=_Hl;return E(_Hm)==0?false:true;},_Hn=function(_Ho){var _Hp=isFloatNegativeZero(E(_Ho)[1]),_Hq=_Hp;return E(_Hq)==0?false:true;},_Hr=function(_Hs,_Ht){return E(_Hs)[1]!=E(_Ht)[1]?true:false;},_Hu=function(_Hv,_Hw){return E(_Hv)[1]==E(_Hw)[1];},_Hx=[0,_Hu,_Hr],_Hy=function(_Hz,_HA){return E(_Hz)[1]<E(_HA)[1];},_HB=function(_HC,_HD){return E(_HC)[1]<=E(_HD)[1];},_HE=function(_HF,_HG){return E(_HF)[1]>E(_HG)[1];},_HH=function(_HI,_HJ){return E(_HI)[1]>=E(_HJ)[1];},_HK=function(_HL,_HM){var _HN=E(_HL)[1],_HO=E(_HM)[1];return _HN>=_HO?_HN!=_HO?2:1:0;},_HP=function(_HQ,_HR){var _HS=E(_HQ),_HT=E(_HR);return _HS[1]>_HT[1]?E(_HS):E(_HT);},_HU=function(_HV,_HW){var _HX=E(_HV),_HY=E(_HW);return _HX[1]>_HY[1]?E(_HY):E(_HX);},_HZ=[0,_Hx,_HK,_Hy,_HH,_HE,_HB,_HP,_HU],_I0=[0,1],_I1=new T(function(){var _I2=newByteArr(256),_I3=_I2,_=_I3["v"]["i8"][0]=8,_=B((function(_I4,_I5,_I6,_){while(1){if(_I6>=256){if(_I4>=256){return E(_);}else{var _I7=imul(2,_I4)|0,_I8=_I5+1|0,_I9=_I4;_I4=_I7;_I5=_I8;_I6=_I9;continue;}}else{var _=_I3["v"]["i8"][_I6]=_I5,_I9=_I6+_I4|0;_I6=_I9;continue;}}})(2,0,1,_)),_Ia=_I3,_Ib=_Ia;return [0,_Ib];}),_Ic=function(_Id,_Ie){while(1){var _If=(function(_Ig,_Ih){var _Ii=hs_int64ToInt(_Ig),_Ij=_Ii,_Ik=E(_I1)[1]["v"]["i8"][(255&_Ij>>>0)>>>0&4294967295];if(_Ih>_Ik){if(_Ik>=8){var _Il=hs_uncheckedIShiftRA64(_Ig,8),_Im=_Il;_Id=_Im;var _In=_Ih-8|0;_Ie=_In;return null;}else{return [0,new T(function(){var _Io=hs_uncheckedIShiftRA64(_Ig,_Ik),_Ip=_Io;return B(_Ax(_Ip));}),_Ih-_Ik|0];}}else{return [0,new T(function(){var _Iq=hs_uncheckedIShiftRA64(_Ig,_Ih),_Ir=_Iq;return B(_Ax(_Ir));}),0];}})(_Id,_Ie);if(_If!=null){return _If;}}},_Is=function(_It){return I_toInt(_It)>>>0;},_Iu=function(_Iv){var _Iw=E(_Iv);return _Iw[0]==0?_Iw[1]>>>0:B(_Is(_Iw[1]));},_Ix=function(_Iy){var _Iz=B(_Au(_Iy)),_IA=_Iz[1],_IB=_Iz[2];if(_IB<0){var _IC=function(_ID){if(!_ID){return [0,E(_IA),B(_Bf(_I0, -_IB))];}else{var _IE=B(_Ic(B(_AN(_IA)), -_IB));return [0,E(_IE[1]),B(_Bf(_I0,_IE[2]))];}};return (B(_Iu(_IA))&1)>>>0==0?B(_IC(1)):B(_IC(0));}else{return [0,B(_Bf(_IA,_IB)),_I0];}},_IF=function(_IG){var _IH=B(_Ix(E(_IG)[1]));return [0,E(_IH[1]),E(_IH[2])];},_II=[0,_EK,_HZ,_IF],_IJ=function(_IK){return E(E(_IK)[1]);},_IL=[0,1],_IM=function(_IN,_IO){if(_IN<=_IO){var _IP=function(_IQ){return [1,[0,_IQ],new T(function(){if(_IQ!=_IO){var _IR=B(_IP(_IQ+1|0));}else{var _IR=[0];}var _IS=_IR;return _IS;})];};return new F(function(){return _IP(_IN);});}else{return [0];}},_IT=function(_IU){return new F(function(){return _IM(E(_IU)[1],2147483647);});},_IV=function(_IW,_IX,_IY){return _IY<=_IX?[1,[0,_IW],new T(function(){var _IZ=_IX-_IW|0,_J0=function(_J1){return _J1>=(_IY-_IZ|0)?[1,[0,_J1],new T(function(){return B(_J0(_J1+_IZ|0));})]:[1,[0,_J1],_v];};return B(_J0(_IX));})]:_IY<=_IW?[1,[0,_IW],_v]:[0];},_J2=function(_J3,_J4,_J5){return _J5>=_J4?[1,[0,_J3],new T(function(){var _J6=_J4-_J3|0,_J7=function(_J8){return _J8<=(_J5-_J6|0)?[1,[0,_J8],new T(function(){return B(_J7(_J8+_J6|0));})]:[1,[0,_J8],_v];};return B(_J7(_J4));})]:_J5>=_J3?[1,[0,_J3],_v]:[0];},_J9=function(_Ja,_Jb){return _Jb<_Ja?B(_IV(_Ja,_Jb,-2147483648)):B(_J2(_Ja,_Jb,2147483647));},_Jc=function(_Jd,_Je){return new F(function(){return _J9(E(_Jd)[1],E(_Je)[1]);});},_Jf=function(_Jg,_Jh,_Ji){return _Jh<_Jg?B(_IV(_Jg,_Jh,_Ji)):B(_J2(_Jg,_Jh,_Ji));},_Jj=function(_Jk,_Jl,_Jm){return new F(function(){return _Jf(E(_Jk)[1],E(_Jl)[1],E(_Jm)[1]);});},_Jn=function(_Jo,_Jp){return new F(function(){return _IM(E(_Jo)[1],E(_Jp)[1]);});},_Jq=function(_Jr){return E(_Jr);},_Js=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_Jt=new T(function(){return B(err(_Js));}),_Ju=function(_Jv){var _Jw=E(E(_Jv)[1]);return _Jw==(-2147483648)?E(_Jt):[0,_Jw-1|0];},_Jx=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_Jy=new T(function(){return B(err(_Jx));}),_Jz=function(_JA){var _JB=E(E(_JA)[1]);return _JB==2147483647?E(_Jy):[0,_JB+1|0];},_JC=[0,_Jz,_Ju,_Jq,_Jq,_IT,_Jc,_Jn,_Jj],_JD=function(_JE,_JF){return [0,B(_yE(E(_JE)[1],E(_JF)[1]))];},_JG=[0,0],_JH=[0,_yD,_JG],_JI=function(_JJ,_JK){var _JL=E(_JJ)[1],_JM=E(E(_JK)[1]);switch(_JM){case -1:var _JN=E(_JL);if(_JN==(-2147483648)){return E(_JH);}else{if(_JN<=0){if(_JN>=0){var _JO=quotRemI(_JN,-1);return [0,[0,_JO[1]],[0,_JO[2]]];}else{var _JP=quotRemI(_JN,-1);return [0,[0,_JP[1]],[0,_JP[2]]];}}else{var _JQ=quotRemI(_JN-1|0,-1);return [0,[0,_JQ[1]-1|0],[0,(_JQ[2]+(-1)|0)+1|0]];}}break;case 0:return E(_yA);default:if(_JL<=0){if(_JL>=0){var _JR=quotRemI(_JL,_JM);return [0,[0,_JR[1]],[0,_JR[2]]];}else{if(_JM<=0){var _JS=quotRemI(_JL,_JM);return [0,[0,_JS[1]],[0,_JS[2]]];}else{var _JT=quotRemI(_JL+1|0,_JM);return [0,[0,_JT[1]-1|0],[0,(_JT[2]+_JM|0)-1|0]];}}}else{if(_JM>=0){if(_JL>=0){var _JU=quotRemI(_JL,_JM);return [0,[0,_JU[1]],[0,_JU[2]]];}else{if(_JM<=0){var _JV=quotRemI(_JL,_JM);return [0,[0,_JV[1]],[0,_JV[2]]];}else{var _JW=quotRemI(_JL+1|0,_JM);return [0,[0,_JW[1]-1|0],[0,(_JW[2]+_JM|0)-1|0]];}}}else{var _JX=quotRemI(_JL-1|0,_JM);return [0,[0,_JX[1]-1|0],[0,(_JX[2]+_JM|0)+1|0]];}}}},_JY=function(_JZ,_K0){var _K1=E(E(_K0)[1]);switch(_K1){case -1:return E(_JG);case 0:return E(_yA);default:return [0,B(_yJ(E(_JZ)[1],_K1))];}},_K2=function(_K3,_K4){var _K5=E(_K3)[1],_K6=E(E(_K4)[1]);switch(_K6){case -1:var _K7=E(_K5);return _K7==(-2147483648)?E(_yD):[0,quot(_K7,-1)];case 0:return E(_yA);default:return [0,quot(_K5,_K6)];}},_K8=function(_K9,_Ka){var _Kb=E(_K9)[1],_Kc=E(E(_Ka)[1]);switch(_Kc){case -1:var _Kd=E(_Kb);if(_Kd==(-2147483648)){return E(_JH);}else{var _Ke=quotRemI(_Kd,-1);return [0,[0,_Ke[1]],[0,_Ke[2]]];}break;case 0:return E(_yA);default:var _Kf=quotRemI(_Kb,_Kc);return [0,[0,_Kf[1]],[0,_Kf[2]]];}},_Kg=function(_Kh,_Ki){var _Kj=E(E(_Ki)[1]);switch(_Kj){case -1:return E(_JG);case 0:return E(_yA);default:return [0,E(_Kh)[1]%_Kj];}},_Kk=function(_Kl){return new F(function(){return _eb(E(_Kl)[1]);});},_Km=function(_Kn){return [0,E(B(_eb(E(_Kn)[1]))),E(_IL)];},_Ko=function(_Kp,_Kq){return [0,imul(E(_Kp)[1],E(_Kq)[1])|0];},_Kr=function(_Ks,_Kt){return [0,E(_Ks)[1]+E(_Kt)[1]|0];},_Ku=function(_Kv,_Kw){return [0,E(_Kv)[1]-E(_Kw)[1]|0];},_Kx=function(_Ky){var _Kz=E(_Ky),_KA=_Kz[1];return _KA<0?[0, -_KA]:E(_Kz);},_KB=function(_KC){return [0,B(_fo(_KC))];},_KD=function(_KE){return [0, -E(_KE)[1]];},_KF=[0,-1],_KG=[0,0],_KH=[0,1],_KI=function(_KJ){var _KK=E(_KJ)[1];return _KK>=0?E(_KK)==0?E(_KG):E(_KH):E(_KF);},_KL=[0,_Kr,_Ko,_Ku,_KD,_Kx,_KI,_KB],_KM=function(_KN,_KO){return E(_KN)[1]==E(_KO)[1];},_KP=function(_KQ,_KR){return E(_KQ)[1]!=E(_KR)[1];},_KS=[0,_KM,_KP],_KT=function(_KU,_KV){var _KW=E(_KU),_KX=E(_KV);return _KW[1]>_KX[1]?E(_KW):E(_KX);},_KY=function(_KZ,_L0){var _L1=E(_KZ),_L2=E(_L0);return _L1[1]>_L2[1]?E(_L2):E(_L1);},_L3=function(_L4,_L5){return _L4>=_L5?_L4!=_L5?2:1:0;},_L6=function(_L7,_L8){return new F(function(){return _L3(E(_L7)[1],E(_L8)[1]);});},_L9=function(_La,_Lb){return E(_La)[1]>=E(_Lb)[1];},_Lc=function(_Ld,_Le){return E(_Ld)[1]>E(_Le)[1];},_Lf=function(_Lg,_Lh){return E(_Lg)[1]<=E(_Lh)[1];},_Li=function(_Lj,_Lk){return E(_Lj)[1]<E(_Lk)[1];},_Ll=[0,_KS,_L6,_Li,_L9,_Lc,_Lf,_KT,_KY],_Lm=[0,_KL,_Ll,_Km],_Ln=[0,_Lm,_JC,_K2,_Kg,_JD,_JY,_K8,_JI,_Kk],_Lo=function(_Lp){return E(E(_Lp)[1]);},_Lq=function(_Lr,_Ls,_Lt){while(1){if(!(_Ls%2)){var _Lu=B(_ed(_Lr,_Lr)),_Lv=quot(_Ls,2);_Lr=_Lu;_Ls=_Lv;continue;}else{var _Lw=E(_Ls);if(_Lw==1){return new F(function(){return _ed(_Lr,_Lt);});}else{var _Lu=B(_ed(_Lr,_Lr));_Ls=quot(_Lw-1|0,2);var _Lx=B(_ed(_Lr,_Lt));_Lr=_Lu;_Lt=_Lx;continue;}}}},_Ly=function(_Lz,_LA){while(1){if(!(_LA%2)){var _LB=B(_ed(_Lz,_Lz)),_LC=quot(_LA,2);_Lz=_LB;_LA=_LC;continue;}else{var _LD=E(_LA);if(_LD==1){return E(_Lz);}else{return new F(function(){return _Lq(B(_ed(_Lz,_Lz)),quot(_LD-1|0,2),_Lz);});}}}},_LE=function(_LF){return E(E(_LF)[2]);},_LG=function(_LH){return E(E(_LH)[1]);},_LI=function(_LJ){return E(E(_LJ)[2]);},_LK=[0,0],_LL=[0,2],_LM=function(_LN){return E(E(_LN)[7]);},_LO=function(_LP,_LQ,_LR,_LS,_LT){return new F(function(){return A(E(E(_LQ)[1])[1],[new T(function(){return B(A(_LS,[_LT,new T(function(){return B(A(_LM,[_LP,_LL]));})]));}),new T(function(){return B(A(_LM,[_LP,_LK]));})]);});},_LU=function(_LV){return E(E(_LV)[3]);},_LW=new T(function(){return B(unCStr("Negative exponent"));}),_LX=new T(function(){return B(err(_LW));}),_LY=function(_LZ,_M0,_M1,_M2){var _M3=B(_IJ(_M0)),_M4=_M3[1],_M5=E(_M3[2]);if(!B(A(_M5[3],[_M2,new T(function(){return B(A(_LM,[_M4,_LK]));})]))){if(!B(A(E(_M5[1])[1],[_M2,new T(function(){return B(A(_LM,[_M4,_LK]));})]))){var _M6=B(_IJ(_M0)),_M7=_M6[1],_M8=new T(function(){return B(_IJ(_M0));}),_M9=new T(function(){return B(_Lo(_M8));});return new F(function(){return (function(_Ma,_Mb){while(1){var _Mc=(function(_Md,_Me){var _Mf=E(_M0),_Mg=_Mf[3],_Mh=E(_Mf[1]);if(!B(_LO(_Mh[1],_Mh[2],_Mh[3],_Mf[4],_Me))){return !B(A(E(E(_M6[2])[1])[1],[_Me,new T(function(){return B(A(_LM,[_M7,_IL]));})]))?B((function(_Mi,_Mj,_Mk){while(1){var _Ml=(function(_Mm,_Mn,_Mo){var _Mp=E(_M0),_Mq=_Mp[3],_Mr=E(_Mp[1]);if(!B(_LO(_Mr[1],_Mr[2],_Mr[3],_Mp[4],_Mn))){if(!B(A(new T(function(){return B(_2E(new T(function(){return B(_LG(new T(function(){return B(_LI(_M8));})));})));}),[_Mn,new T(function(){return B(A(_LM,[_M9,_IL]));})]))){_Mi=new T(function(){return B(A(new T(function(){return B(_LE(_LZ));}),[_Mm,_Mm]));});_Mj=new T(function(){return B(A(_Mq,[new T(function(){return B(A(new T(function(){return B(_LU(_M9));}),[_Mn,new T(function(){return B(A(_LM,[_M9,_IL]));})]));}),new T(function(){return B(A(_LM,[_M9,_LL]));})]));});_Mk=new T(function(){return B(A(new T(function(){return B(_LE(_LZ));}),[_Mm,_Mo]));});return null;}else{return new F(function(){return A(new T(function(){return B(_LE(_LZ));}),[_Mm,_Mo]);});}}else{_Mi=new T(function(){return B(A(new T(function(){return B(_LE(_LZ));}),[_Mm,_Mm]));});_Mj=new T(function(){return B(A(_Mq,[_Mn,new T(function(){return B(A(_LM,[_M9,_LL]));})]));});var _Ms=_Mo;_Mk=_Ms;return null;}})(_Mi,_Mj,_Mk);if(_Ml!=null){return _Ml;}}})(new T(function(){return B(A(new T(function(){return B(_LE(_LZ));}),[_Md,_Md]));}),new T(function(){return B(A(_Mg,[new T(function(){return B(A(new T(function(){return B(_LU(_M7));}),[_Me,new T(function(){return B(A(_LM,[_M7,_IL]));})]));}),new T(function(){return B(A(_LM,[_M7,_LL]));})]));}),_Md)):E(_Md);}else{_Ma=new T(function(){return B(A(new T(function(){return B(_LE(_LZ));}),[_Md,_Md]));});_Mb=new T(function(){return B(A(_Mg,[_Me,new T(function(){return B(A(_LM,[_M7,_LL]));})]));});return null;}})(_Ma,_Mb);if(_Mc!=null){return _Mc;}}})(_M1,_M2);});}else{return new F(function(){return A(_LM,[_LZ,_IL]);});}}else{return E(_LX);}},_Mt=new T(function(){return B(err(_LW));}),_Mu=function(_Mv,_Mw){var _Mx=E(_Mv);return _Mx[0]==0?_Mx[1]*Math.pow(2,_Mw):I_toNumber(_Mx[1])*Math.pow(2,_Mw);},_My=function(_Mz,_MA){while(1){var _MB=E(_Mz);if(!_MB[0]){var _MC=E(_MB[1]);if(_MC==(-2147483648)){_Mz=[1,I_fromInt(-2147483648)];continue;}else{var _MD=E(_MA);if(!_MD[0]){var _ME=_MD[1];return [0,[0,quot(_MC,_ME)],[0,_MC%_ME]];}else{_Mz=[1,I_fromInt(_MC)];_MA=_MD;continue;}}}else{var _MF=E(_MA);if(!_MF[0]){_Mz=_MB;_MA=[1,I_fromInt(_MF[1])];continue;}else{var _MG=I_quotRem(_MB[1],_MF[1]);return [0,[1,_MG[1]],[1,_MG[2]]];}}}},_MH=function(_MI,_MJ){var _MK=B(_Au(_MJ)),_ML=_MK[1],_MM=_MK[2],_MN=new T(function(){return B(_Lo(new T(function(){return B(_IJ(_MI));})));});if(_MM<0){var _MO= -_MM;if(_MO>=0){var _MP=E(_MO),_MQ=_MP==0?E(_IL):B(_Ly(_GI,_MP));if(!B(_B0(_MQ,_AU))){var _MR=B(_My(_ML,_MQ));return [0,new T(function(){return B(A(_LM,[_MN,_MR[1]]));}),new T(function(){return [0,B(_Mu(_MR[2],_MM))];})];}else{return E(_yA);}}else{return E(_Mt);}}else{return [0,new T(function(){return B(A(_LE,[_MN,new T(function(){return B(A(_LM,[_MN,_ML]));}),new T(function(){return B(_LY(_MN,_Ln,new T(function(){return B(A(_LM,[_MN,_GI]));}),[0,_MM]));})]));}),_Et];}},_MS=function(_MT,_MU){var _MV=B(_MH(_MT,E(_MU)[1])),_MW=_MV[1];if(E(_MV[2])[1]<=0){return E(_MW);}else{var _MX=E(B(_IJ(_MT))[1]);return new F(function(){return A(_MX[1],[_MW,new T(function(){return B(A(_MX[7],[_I0]));})]);});}},_MY=function(_MZ,_N0){var _N1=B(_MH(_MZ,E(_N0)[1])),_N2=_N1[1];if(E(_N1[2])[1]>=0){return E(_N2);}else{var _N3=E(B(_IJ(_MZ))[1]);return new F(function(){return A(_N3[3],[_N2,new T(function(){return B(A(_N3[7],[_I0]));})]);});}},_N4=function(_N5,_N6){var _N7=B(_MH(_N5,E(_N6)[1]));return [0,_N7[1],_N7[2]];},_N8=function(_N9,_Na){var _Nb=B(_MH(_N9,_Na)),_Nc=_Nb[1],_Nd=E(_Nb[2])[1],_Ne=new T(function(){var _Nf=E(B(_IJ(_N9))[1]),_Ng=_Nf[7];return _Nd>=0?B(A(_Nf[1],[_Nc,new T(function(){return B(A(_Ng,[_I0]));})])):B(A(_Nf[3],[_Nc,new T(function(){return B(A(_Ng,[_I0]));})]));});if(_Nd<0){var _Nh= -_Nd-0.5;if(_Nh>=0){if(!E(_Nh)){var _Ni=E(_N9),_Nj=E(_Ni[1]);return !B(_LO(_Nj[1],_Nj[2],_Nj[3],_Ni[4],_Nc))?E(_Ne):E(_Nc);}else{return E(_Ne);}}else{return E(_Nc);}}else{var _Nk=_Nd-0.5;if(_Nk>=0){if(!E(_Nk)){var _Nl=E(_N9),_Nm=E(_Nl[1]);return !B(_LO(_Nm[1],_Nm[2],_Nm[3],_Nl[4],_Nc))?E(_Ne):E(_Nc);}else{return E(_Ne);}}else{return E(_Nc);}}},_Nn=function(_No,_Np){return new F(function(){return _N8(_No,E(_Np)[1]);});},_Nq=function(_Nr,_Ns){return E(B(_MH(_Nr,E(_Ns)[1]))[1]);},_Nt=[0,_II,_EO,_N4,_Nq,_Nn,_MS,_MY],_Nu=function(_Nv,_Nw){return E(_Nv)[1]!=E(_Nw)[1]?true:false;},_Nx=function(_Ny,_Nz){return E(_Ny)[1]==E(_Nz)[1];},_NA=[0,_Nx,_Nu],_NB=function(_NC,_ND){return E(_NC)[1]<E(_ND)[1];},_NE=function(_NF,_NG){return E(_NF)[1]<=E(_NG)[1];},_NH=function(_NI,_NJ){return E(_NI)[1]>E(_NJ)[1];},_NK=function(_NL,_NM){return E(_NL)[1]>=E(_NM)[1];},_NN=function(_NO,_NP){var _NQ=E(_NO)[1],_NR=E(_NP)[1];return _NQ>=_NR?_NQ!=_NR?2:1:0;},_NS=function(_NT,_NU){var _NV=E(_NT),_NW=E(_NU);return _NV[1]>_NW[1]?E(_NV):E(_NW);},_NX=function(_NY,_NZ){var _O0=E(_NY),_O1=E(_NZ);return _O0[1]>_O1[1]?E(_O1):E(_O0);},_O2=[0,_NA,_NN,_NB,_NK,_NH,_NE,_NS,_NX],_O3=function(_O4,_O5){while(1){var _O6=(function(_O7,_O8){var _O9=E(_I1)[1]["v"]["i8"][(255&_O7>>>0)>>>0&4294967295];if(_O8>_O9){if(_O9>=8){var _Oa=_O7>>8,_Ob=_O8-8|0;_O4=_Oa;_O5=_Ob;return null;}else{return [0,new T(function(){return B(_eb(_O7>>_O9));}),_O8-_O9|0];}}else{return [0,new T(function(){return B(_eb(_O7>>_O8));}),0];}})(_O4,_O5);if(_O6!=null){return _O6;}}},_Oc=function(_Od){var _Oe=decodeFloat(_Od),_Of=_Oe[1],_Og=_Oe[2];if(_Og<0){var _Oh=function(_Oi){if(!_Oi){return [0,B(_eb(_Of)),B(_Bf(_I0, -_Og))];}else{var _Oj=B(_O3(_Of, -_Og));return [0,E(_Oj[1]),B(_Bf(_I0,_Oj[2]))];}};return (_Of>>>0&1)>>>0==0?B(_Oh(1)):B(_Oh(0));}else{return [0,B(_Bf(B(_eb(_Of)),_Og)),_I0];}},_Ok=function(_Ol){var _Om=B(_Oc(E(_Ol)[1]));return [0,E(_Om[1]),E(_Om[2])];},_On=[0,_G5,_O2,_Ok],_Oo=[0,-1],_Op=[0,1],_Oq=function(_Or,_Os){var _Ot=E(_Or);return _Ot[0]==0?_Ot[1]*Math.pow(2,_Os):I_toNumber(_Ot[1])*Math.pow(2,_Os);},_Ou=[0,0],_Ov=function(_Ow,_Ox){var _Oy=decodeFloat(_Ox),_Oz=_Oy[1],_OA=_Oy[2],_OB=new T(function(){return B(_Lo(new T(function(){return B(_IJ(_Ow));})));});if(_OA<0){var _OC=new T(function(){if(_Oz<0){var _OD= -_OA;if(_OD<32){var _OE=[0, -( -_Oz>>_OD)];}else{var _OE= -_Oz>=0?E(_Ou):E(_Op);}var _OF=_OE,_OG=_OF,_OH=_OG;}else{var _OI= -_OA;if(_OI<32){var _OJ=[0,_Oz>>_OI];}else{var _OJ=_Oz>=0?E(_Ou):E(_Oo);}var _OK=_OJ,_OL=_OK,_OH=_OL;}var _OM=_OH;return _OM;});return [0,new T(function(){return B(A(_LM,[_OB,new T(function(){return B(_eb(E(_OC)[1]));})]));}),new T(function(){var _ON= -_OA;if(_ON<32){var _OO=[0,B(_Oq(B(_eb(_Oz-(E(_OC)[1]<<_ON)|0)),_OA))];}else{var _OO=[0,B(_Oq(B(_eb(_Oz)),_OA))];}var _OP=_OO,_OQ=_OP,_OR=_OQ;return _OR;})];}else{return [0,new T(function(){return B(A(_LE,[_OB,new T(function(){return B(A(_LM,[_OB,new T(function(){return B(_eb(_Oz));})]));}),new T(function(){return B(_LY(_OB,_Ln,new T(function(){return B(A(_LM,[_OB,_GI]));}),[0,_OA]));})]));}),_FO];}},_OS=function(_OT,_OU){var _OV=B(_Ov(_OT,E(_OU)[1])),_OW=_OV[1];if(E(_OV[2])[1]<=0){return E(_OW);}else{var _OX=E(B(_IJ(_OT))[1]);return new F(function(){return A(_OX[1],[_OW,new T(function(){return B(A(_OX[7],[_I0]));})]);});}},_OY=function(_OZ,_P0){var _P1=B(_Ov(_OZ,E(_P0)[1])),_P2=_P1[1];if(E(_P1[2])[1]>=0){return E(_P2);}else{var _P3=E(B(_IJ(_OZ))[1]);return new F(function(){return A(_P3[3],[_P2,new T(function(){return B(A(_P3[7],[_I0]));})]);});}},_P4=function(_P5,_P6){var _P7=B(_Ov(_P5,E(_P6)[1]));return [0,_P7[1],_P7[2]];},_P8=function(_P9,_Pa){var _Pb=B(_Ov(_P9,_Pa)),_Pc=_Pb[1],_Pd=E(_Pb[2])[1],_Pe=new T(function(){var _Pf=E(B(_IJ(_P9))[1]),_Pg=_Pf[7];return _Pd>=0?B(A(_Pf[1],[_Pc,new T(function(){return B(A(_Pg,[_I0]));})])):B(A(_Pf[3],[_Pc,new T(function(){return B(A(_Pg,[_I0]));})]));});if(_Pd<0){var _Ph= -_Pd-0.5;if(_Ph>=0){if(!E(_Ph)){var _Pi=E(_P9),_Pj=E(_Pi[1]);return !B(_LO(_Pj[1],_Pj[2],_Pj[3],_Pi[4],_Pc))?E(_Pe):E(_Pc);}else{return E(_Pe);}}else{return E(_Pc);}}else{var _Pk=_Pd-0.5;if(_Pk>=0){if(!E(_Pk)){var _Pl=E(_P9),_Pm=E(_Pl[1]);return !B(_LO(_Pm[1],_Pm[2],_Pm[3],_Pl[4],_Pc))?E(_Pe):E(_Pc);}else{return E(_Pe);}}else{return E(_Pc);}}},_Pn=function(_Po,_Pp){return new F(function(){return _P8(_Po,E(_Pp)[1]);});},_Pq=function(_Pr,_Ps){return E(B(_Ov(_Pr,E(_Ps)[1]))[1]);},_Pt=[0,_On,_G9,_P4,_Pq,_Pn,_OS,_OY],_Pu=function(_Pv){return new F(function(){return err(B(unAppCStr("Char.intToDigit: not a digit ",new T(function(){if(_Pv>=0){var _Pw=jsShowI(_Pv),_Px=_Pw,_Py=fromJSStr(_Px);}else{var _Pz=jsShowI(_Pv),_PA=_Pz,_Py=fromJSStr(_PA);}var _PB=_Py;return _PB;}))));});},_PC=function(_PD){var _PE=function(_PF){if(_PD<10){return new F(function(){return _Pu(_PD);});}else{if(_PD>15){return new F(function(){return _Pu(_PD);});}else{return (97+_PD|0)-10|0;}}};if(_PD<0){return new F(function(){return _PE(_);});}else{if(_PD>9){return new F(function(){return _PE(_);});}else{return 48+_PD|0;}}},_PG=function(_PH){return [0,B(_PC(E(_PH)[1]))];},_PI=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_PJ=function(_PK){return new F(function(){return _b0([0,new T(function(){return B(_bf(_PK,_PI));})],_aX);});},_PL=new T(function(){return B(_PJ("GHC/Float.lhs:619:11-64|d : ds\'"));}),_PM=function(_PN,_PO){if(E(_PN)[1]<=0){var _PP=B(_1r(_PG,[1,_Ou,_PO]));return _PP[0]==0?E(_PL):[0,_PP[1],_PP[2]];}else{var _PQ=B(_1r(_PG,_PO));return _PQ[0]==0?E(_PL):[0,_PQ[1],_PQ[2]];}},_PR=function(_PS){return E(E(_PS)[1]);},_PT=function(_PU){return E(E(_PU)[1]);},_PV=function(_PW){return E(E(_PW)[1]);},_PX=[0,48],_PY=[1,_PX,_v],_PZ=[0,46],_Q0=function(_Q1,_Q2,_Q3){while(1){var _Q4=(function(_Q5,_Q6,_Q7){var _Q8=E(_Q5);if(!_Q8){var _Q9=B(_E0(_Q6,_v));return _Q9[0]==0?[1,_PX,[1,_PZ,new T(function(){var _Qa=E(_Q7);return _Qa[0]==0?E(_PY):E(_Qa);})]]:B(_P(_Q9,[1,_PZ,new T(function(){var _Qb=E(_Q7);return _Qb[0]==0?E(_PY):E(_Qb);})]));}else{var _Qc=E(_Q7);if(!_Qc[0]){_Q1=_Q8-1|0;var _Qd=[1,_PX,_Q6];_Q3=_v;_Q2=_Qd;return null;}else{_Q1=_Q8-1|0;var _Qd=[1,_Qc[1],_Q6];_Q3=_Qc[2];_Q2=_Qd;return null;}}})(_Q1,_Q2,_Q3);if(_Q4!=null){return _Q4;}}},_Qe=[0,0],_Qf=new T(function(){return B(unCStr(" out of range "));}),_Qg=new T(function(){return B(unCStr("}.index: Index "));}),_Qh=new T(function(){return B(unCStr("Ix{"));}),_Qi=[1,_17,_v],_Qj=[1,_17,_Qi],_Qk=function(_Ql,_Qm,_Qn,_Qo,_Qp){return new F(function(){return err(B(_P(_Qh,new T(function(){return B(_P(_Ql,new T(function(){return B(_P(_Qg,[1,_18,new T(function(){return B(A(_Qp,[_Qe,_Qm,[1,_17,new T(function(){return B(_P(_Qf,[1,_18,[1,_18,new T(function(){return B(A(_tC,[_ts,[1,new T(function(){return B(A(_Qp,[_sX,_Qn]));}),[1,new T(function(){return B(A(_Qp,[_sX,_Qo]));}),_v]],_Qj]));})]]));})]]));})]));})));}))));});},_Qq=function(_Qr,_Qs,_Qt,_Qu){var _Qv=E(_Qt);return new F(function(){return _Qk(_Qr,_Qs,_Qv[1],_Qv[2],E(_Qu)[1]);});},_Qw=function(_Qx,_Qy,_Qz,_QA){return new F(function(){return _Qq(_QA,_Qz,_Qy,_Qx);});},_QB=new T(function(){return B(unCStr("Int"));}),_QC=function(_QD,_QE,_QF){return new F(function(){return _Qw(_tr,[0,_QE,_QF],_QD,_QB);});},_QG=new T(function(){return B(unCStr("(Array.!): undefined array element"));}),_QH=new T(function(){return B(err(_QG));}),_QI=[0,1100],_QJ=[0,_Ou,_QI],_QK=function(_QL){return new F(function(){return _Qw(_tr,_QJ,[0,_QL],_QB);});},_QM=function(_){var _QN=newArr(1101,_QH),_QO=_QN;return new F(function(){return (function(_QP,_){while(1){var _QQ=(function(_QR,_){if(0>_QR){return new F(function(){return _QK(_QR);});}else{if(_QR>1100){return new F(function(){return _QK(_QR);});}else{var _=_QO[_QR]=new T(function(){if(_QR>=0){var _QS=E(_QR),_QT=_QS==0?E(_IL):B(_Ly(_GI,_QS));}else{var _QT=E(_Mt);}var _QU=_QT;return _QU;}),_QV=E(_QR);if(_QV==1100){var _QW=_QO,_QX=_QW;return [0,E(_Ou),E(_QI),1101,_QX];}else{_QP=_QV+1|0;return null;}}}})(_QP,_);if(_QQ!=null){return _QQ;}}})(0,_);});},_QY=function(_QZ){var _R0=B(A(_QZ,[_])),_R1=_R0;return E(_R1);},_R2=new T(function(){return B(_QY(_QM));}),_R3=[0,10],_R4=[0,324],_R5=[0,_Ou,_R4],_R6=function(_R7){return new F(function(){return _Qw(_tr,_R5,[0,_R7],_QB);});},_R8=function(_){var _R9=newArr(325,_QH),_Ra=_R9;return new F(function(){return (function(_Rb,_){while(1){var _Rc=(function(_Rd,_){if(0>_Rd){return new F(function(){return _R6(_Rd);});}else{if(_Rd>324){return new F(function(){return _R6(_Rd);});}else{var _=_Ra[_Rd]=new T(function(){if(_Rd>=0){var _Re=E(_Rd),_Rf=_Re==0?E(_IL):B(_Ly(_R3,_Re));}else{var _Rf=E(_Mt);}var _Rg=_Rf;return _Rg;}),_Rh=E(_Rd);if(_Rh==324){var _Ri=_Ra,_Rj=_Ri;return [0,E(_Ou),E(_R4),325,_Rj];}else{_Rb=_Rh+1|0;return null;}}}})(_Rb,_);if(_Rc!=null){return _Rc;}}})(0,_);});},_Rk=new T(function(){return B(_QY(_R8));}),_Rl=function(_Rm,_Rn){var _Ro=[0,_Rn],_Rp=function(_Rq){if(!B(_B0(_Rm,_R3))){if(_Rn>=0){var _Rr=E(_Rn);return _Rr==0?E(_IL):B(_Ly(_Rm,_Rr));}else{return E(_Mt);}}else{if(_Rn>324){if(_Rn>=0){var _Rs=E(_Rn);return _Rs==0?E(_IL):B(_Ly(_Rm,_Rs));}else{return E(_Mt);}}else{var _Rt=E(_Rk),_Ru=E(_Rt[1]),_Rv=_Ru[1],_Rw=E(_Rt[2]);if(_Rv>_Rn){return new F(function(){return _QC(_Ro,_Ru,_Rw);});}else{if(_Rn>_Rw[1]){return new F(function(){return _QC(_Ro,_Ru,_Rw);});}else{return E(_Rt[4][_Rn-_Rv|0]);}}}}};if(!B(_B0(_Rm,_GI))){return new F(function(){return _Rp(_);});}else{if(_Rn<0){return new F(function(){return _Rp(_);});}else{if(_Rn>1100){return new F(function(){return _Rp(_);});}else{var _Rx=E(_R2),_Ry=E(_Rx[1]),_Rz=_Ry[1],_RA=E(_Rx[2]);if(_Rz>_Rn){return new F(function(){return _QC(_Ro,_Ry,_RA);});}else{if(_Rn>_RA[1]){return new F(function(){return _QC(_Ro,_Ry,_RA);});}else{return E(_Rx[4][_Rn-_Rz|0]);}}}}}},_RB=function(_RC,_RD){var _RE=E(_RC);if(!_RE[0]){var _RF=_RE[1],_RG=E(_RD);return _RG[0]==0?_RF>_RG[1]:I_compareInt(_RG[1],_RF)<0;}else{var _RH=_RE[1],_RI=E(_RD);return _RI[0]==0?I_compareInt(_RH,_RI[1])>0:I_compare(_RH,_RI[1])>0;}},_RJ=[1,_Ou,_v],_RK=function(_RL,_RM){while(1){var _RN=E(_RL);if(!_RN[0]){var _RO=E(_RN[1]);if(_RO==(-2147483648)){_RL=[1,I_fromInt(-2147483648)];continue;}else{var _RP=E(_RM);if(!_RP[0]){return [0,quot(_RO,_RP[1])];}else{_RL=[1,I_fromInt(_RO)];_RM=_RP;continue;}}}else{var _RQ=_RN[1],_RR=E(_RM);return _RR[0]==0?[0,I_toInt(I_quot(_RQ,I_fromInt(_RR[1])))]:[1,I_quot(_RQ,_RR[1])];}}},_RS=function(_RT,_RU,_RV,_RW,_RX,_RY,_RZ,_S0){if(!B(A(_RT,[_S0,new T(function(){return B(A(_LM,[B(_PT(B(_PR(_RU)))),_AU]));})]))){var _S1=new T(function(){return B(A(_RV,[_S0]));}),_S2=new T(function(){return B(A(_RW,[_S0]));}),_S3=new T(function(){return [0,E(B(A(_RX,[_S0]))[1])[1]-E(_S2)[1]|0];}),_S4=new T(function(){return B(A(_RY,[_S0]));}),_S5=new T(function(){return E(E(_S4)[2]);}),_S6=new T(function(){var _S7=E(_S5),_S8=_S7[1],_S9=E(_S3)[1]-_S8|0;if(_S9<=0){var _Sa=[0,new T(function(){return E(E(_S4)[1]);}),_S7];}else{var _Sa=[0,new T(function(){var _Sb=B(_Rl(_S1,_S9));if(!B(_B0(_Sb,_AU))){var _Sc=B(_RK(E(_S4)[1],_Sb));}else{var _Sc=E(_yA);}var _Sd=_Sc;return _Sd;}),[0,_S8+_S9|0]];}var _Se=_Sa,_Sf=_Se,_Sg=_Sf,_Sh=_Sg;return _Sh;}),_Si=new T(function(){return E(E(_S6)[2]);}),_Sj=new T(function(){return E(E(_S6)[1]);}),_Sk=new T(function(){var _Sl=E(_Si)[1];if(_Sl<0){if(_Sl<=E(_S3)[1]){var _Sm=[0,new T(function(){return B(_ed(_Sj,_GI));}),new T(function(){return B(_ed(B(_Rl(_S1, -_Sl)),_GI));}),_I0,_I0];}else{var _Sm=!B(_B0(_Sj,B(_Rl(_S1,E(_S2)[1]-1|0))))?[0,new T(function(){return B(_ed(_Sj,_GI));}),new T(function(){return B(_ed(B(_Rl(_S1, -_Sl)),_GI));}),_I0,_I0]:[0,new T(function(){return B(_ed(B(_ed(_Sj,_S1)),_GI));}),new T(function(){return B(_ed(B(_Rl(_S1, -_Sl+1|0)),_GI));}),_S1,_I0];}var _Sn=_Sm,_So=_Sn,_Sp=_So;}else{var _Sq=new T(function(){return B(_Rl(_S1,_Sl));}),_Sp=!B(_B0(_Sj,B(_Rl(_S1,E(_S2)[1]-1|0))))?[0,new T(function(){return B(_ed(B(_ed(_Sj,_Sq)),_GI));}),_GI,_Sq,_Sq]:[0,new T(function(){return B(_ed(B(_ed(B(_ed(_Sj,_Sq)),_S1)),_GI));}),new T(function(){return B(_ed(_GI,_S1));}),new T(function(){return B(_ed(_Sq,_S1));}),_Sq];}var _Sr=_Sp,_Ss=_Sr;return _Ss;}),_St=new T(function(){return E(E(_Sk)[2]);}),_Su=new T(function(){return E(E(_Sk)[3]);}),_Sv=new T(function(){return E(E(_Sk)[1]);}),_Sw=new T(function(){var _Sx=new T(function(){return B(_dV(_Sv,_Su));}),_Sy=function(_Sz){var _SA=(Math.log(B(_FJ(B(_dV(_Sj,_I0)))))+E(_Si)[1]*Math.log(B(_FJ(_S1))))/Math.log(B(_FJ(_RZ))),_SB=_SA&4294967295;return _SB>=_SA?E(_SB):_SB+1|0;},_SC=function(_SD){while(1){if(_SD<0){if(!B(_fr(B(_ed(B(_Rl(_RZ, -_SD)),_Sx)),_St))){var _SE=_SD+1|0;_SD=_SE;continue;}else{return E(_SD);}}else{if(!B(_fr(_Sx,B(_ed(B(_Rl(_RZ,_SD)),_St))))){var _SE=_SD+1|0;_SD=_SE;continue;}else{return E(_SD);}}}};if(!B(_B0(_S1,_GI))){var _SF=[0,B(_SC(B(_Sy(_))))];}else{if(!B(_B0(_RZ,_R3))){var _SG=[0,B(_SC(B(_Sy(_))))];}else{var _SH=(E(_S2)[1]-1|0)+E(_S5)[1]|0;if(_SH<0){var _SI=[0,B(_SC(quot(imul(_SH,8651)|0,28738)))];}else{var _SI=[0,B(_SC(quot(imul(_SH,8651)|0,28738)+1|0))];}var _SJ=_SI,_SK=_SJ,_SL=_SK,_SM=_SL,_SN=_SM,_SG=_SN;}var _SF=_SG;}return _SF;});return [0,new T(function(){var _SO=E(_Sw)[1],_SP=function(_SQ,_SR,_SS,_ST,_SU){while(1){var _SV=(function(_SW,_SX,_SY,_SZ,_T0){if(!B(_B0(_SY,_AU))){var _T1=B(_My(B(_ed(_SX,_RZ)),_SY)),_T2=_T1[1],_T3=_T1[2],_T4=B(_ed(_T0,_RZ)),_T5=B(_ed(_SZ,_RZ));if(!B(_Z(_T3,_T4))){if(!B(_RB(B(_dV(_T3,_T5)),_SY))){var _T6=[1,_T2,_SW];_SR=_T3;var _T7=_SY;_ST=_T5;_SU=_T4;_SQ=_T6;_SS=_T7;return null;}else{return [1,new T(function(){return B(_dV(_T2,_I0));}),_SW];}}else{return !B(_RB(B(_dV(_T3,_T5)),_SY))?[1,_T2,_SW]:!B(_Z(B(_ed(_T3,_GI)),_SY))?[1,new T(function(){return B(_dV(_T2,_I0));}),_SW]:[1,_T2,_SW];}}else{return E(_yA);}})(_SQ,_SR,_SS,_ST,_SU);if(_SV!=null){return _SV;}}};if(_SO<0){var _T8=B(_Rl(_RZ, -_SO)),_T9=B(_1r(_KB,B(_E0(B(_SP(_v,B(_ed(_Sv,_T8)),_St,B(_ed(_Su,_T8)),B(_ed(E(_Sk)[4],_T8)))),_v))));}else{var _T9=B(_1r(_KB,B(_E0(B(_SP(_v,_Sv,B(_ed(_St,B(_Rl(_RZ,_SO)))),_Su,E(_Sk)[4])),_v))));}var _Ta=_T9,_Tb=_Ta;return _Tb;}),_Sw];}else{return [0,_RJ,_Ou];}},_Tc=function(_Td,_Te){while(1){var _Tf=E(_Te);if(!_Tf[0]){return true;}else{if(!B(A(_Td,[_Tf[1]]))){return false;}else{_Te=_Tf[2];continue;}}}},_Tg=function(_Th){return E(_Th)[1]%2==0?true:false;},_Ti=new T(function(){return B(unCStr("roundTo: bad Value"));}),_Tj=new T(function(){return B(err(_Ti));}),_Tk=function(_Tl){return E(E(_Tl)[1])==0?true:false;},_Tm=function(_Tn){return _Tn>1?[1,_Ou,new T(function(){return B(_Tm(_Tn-1|0));})]:E(_RJ);},_To=function(_Tp,_Tq,_Tr){var _Ts=function(_Tt,_Tu,_Tv){var _Tw=E(_Tv);if(!_Tw[0]){return [0,_Ou,new T(function(){var _Tx=E(_Tt)[1];return _Tx>0?B(_Tm(_Tx)):[0];})];}else{var _Ty=_Tw[1],_Tz=_Tw[2],_TA=E(E(_Tt)[1]);if(!_TA){var _TB=E(_Ty)[1],_TC=E(new T(function(){return [0,quot(E(_Tp)[1],2)];}))[1];return _TB!=_TC?[0,new T(function(){return _TB<_TC?E(_Ou):E(_Op);}),_v]:!E(_Tu)?[0,new T(function(){return _TB<_TC?E(_Ou):E(_Op);}),_v]:!B(_Tc(_Tk,_Tz))?[0,new T(function(){return _TB<_TC?E(_Ou):E(_Op);}),_v]:[0,_Ou,_v];}else{var _TD=B(_Ts([0,_TA-1|0],new T(function(){return B(_Tg(_Ty));}),_Tz)),_TE=_TD[2],_TF=E(_TD[1])[1]+E(_Ty)[1]|0;return _TF!=E(_Tp)[1]?[0,_Ou,[1,[0,_TF],_TE]]:[0,_Op,[1,_Ou,_TE]];}}},_TG=B(_Ts(_Tq,_o,_Tr));switch(E(E(_TG[1])[1])){case 0:return E(_TG);case 1:return [0,_Op,[1,_Op,_TG[2]]];default:return E(_Tj);}},_TH=function(_TI,_TJ){var _TK=E(_TI);if(!_TK){return [0,_v,_TJ];}else{var _TL=E(_TJ);if(!_TL[0]){return [0,_v,_v];}else{var _TM=new T(function(){var _TN=B(_TH(_TK-1|0,_TL[2]));return [0,_TN[1],_TN[2]];});return [0,[1,_TL[1],new T(function(){return E(E(_TM)[1]);})],new T(function(){return E(E(_TM)[2]);})];}}},_TO=function(_TP){return E(E(_TP)[3]);},_TQ=0,_TR=1,_TS=[0,10],_TT=new T(function(){return B(unCStr("e0"));}),_TU=function(_TV,_TW){var _TX=E(_TV);if(!_TX[0]){return E(_TT);}else{var _TY=_TX[1];return _TW>1?[1,_TY,new T(function(){return B(_TU(_TX[2],_TW-1|0));})]:[1,_TY,_TT];}},_TZ=function(_U0,_U1){var _U2=E(_U1);return _U2[0]==0?[0]:[1,_U0,new T(function(){return B(_TZ(_U2[1],_U2[2]));})];},_U3=new T(function(){return B(unCStr("init"));}),_U4=new T(function(){return B(_ty(_U3));}),_U5=new T(function(){return B(_PJ("GHC/Float.lhs:591:12-70|(d : ds\')"));}),_U6=[0,101],_U7=new T(function(){return B(unCStr("Infinity"));}),_U8=new T(function(){return B(unCStr("-Infinity"));}),_U9=new T(function(){return B(unCStr("NaN"));}),_Ua=new T(function(){return B(unCStr("formatRealFloat/doFmt/FFExponent: []"));}),_Ub=new T(function(){return B(err(_Ua));}),_Uc=new T(function(){return B(unCStr("0.0e0"));}),_Ud=function(_Ue){return E(E(_Ue)[4]);},_Uf=new T(function(){return [1,_PX,_Uf];}),_Ug=function(_Uh,_Ui,_Uj,_Uk,_Ul,_Um,_Un,_Uo,_Up,_Uq,_Ur,_Us){if(!B(A(_Un,[_Us]))){var _Ut=new T(function(){return B(_PT(new T(function(){return B(_PR(_Ui));})));});if(!B(A(_Uo,[_Us]))){var _Uu=function(_Uv,_Uw,_Ux){while(1){var _Uy=(function(_Uz,_UA,_UB){switch(E(_Uz)){case 0:var _UC=E(_Ur);if(!_UC[0]){var _UD=B(_1r(_PG,_UA));if(!_UD[0]){return E(_Ub);}else{var _UE=_UD[2],_UF=E(_UD[1]),_UG=function(_UH){var _UI=E(_UE);return _UI[0]==0?[1,_UF,new T(function(){return B(unAppCStr(".0e",new T(function(){return B(_82(0,E(_UB)[1]-1|0,_v));})));})]:[1,_UF,[1,_PZ,new T(function(){return B(_P(_UI,[1,_U6,new T(function(){return B(_82(0,E(_UB)[1]-1|0,_v));})]));})]];};return E(_UF[1])==48?E(_UE)[0]==0?E(_Uc):B(_UG(_)):B(_UG(_));}}else{var _UJ=new T(function(){var _UK=E(_UC[1]);return _UK[1]>1?E(_UK):E(_Op);}),_UL=function(_UM){var _UN=new T(function(){var _UO=B(_To(_TS,new T(function(){return [0,E(_UJ)[1]+1|0];}),_UA));return [0,_UO[1],_UO[2]];}),_UP=new T(function(){return E(E(_UN)[1]);}),_UQ=new T(function(){if(E(_UP)[1]<=0){var _UR=B(_1r(_PG,E(_UN)[2])),_US=_UR[0]==0?E(_U5):[0,_UR[1],_UR[2]];}else{var _UT=E(E(_UN)[2]);if(!_UT[0]){var _UU=E(_U4);}else{var _UV=B(_1r(_PG,B(_TZ(_UT[1],_UT[2])))),_UU=_UV[0]==0?E(_U5):[0,_UV[1],_UV[2]];}var _UW=_UU,_US=_UW;}var _UX=_US,_UY=_UX;return _UY;});return [1,new T(function(){return E(E(_UQ)[1]);}),[1,_PZ,new T(function(){return B(_P(E(_UQ)[2],[1,_U6,new T(function(){return B(_82(0,(E(_UB)[1]-1|0)+E(_UP)[1]|0,_v));})]));})]];},_UZ=E(_UA);if(!_UZ[0]){return new F(function(){return _UL(_);});}else{return E(E(_UZ[1])[1])==0?E(_UZ[2])[0]==0?[1,_PX,[1,_PZ,new T(function(){var _V0=E(_UJ)[1];return _V0>0?B(_TU(_Uf,_V0)):E(_TT);})]]:B(_UL(_)):B(_UL(_));}}break;case 1:var _V1=E(_Ur);if(!_V1[0]){var _V2=E(_UB)[1];return _V2>0?B(_Q0(_V2,_v,new T(function(){return B(_1r(_PG,_UA));}))):B(unAppCStr("0.",new T(function(){var _V3= -_V2;if(_V3>0){var _V4=function(_V5){return _V5>1?[1,_PX,new T(function(){return B(_V4(_V5-1|0));})]:E([1,_PX,new T(function(){return B(_1r(_PG,_UA));})]);},_V6=B(_V4(_V3));}else{var _V6=B(_1r(_PG,_UA));}var _V7=_V6,_V8=_V7;return _V8;})));}else{var _V9=_V1[1],_Va=E(_UB),_Vb=_Va[1];if(_Vb<0){var _Vc=new T(function(){var _Vd= -_Vb;if(_Vd>0){var _Ve=function(_Vf){return _Vf>1?[1,_Ou,new T(function(){return B(_Ve(_Vf-1|0));})]:E([1,_Ou,_UA]);},_Vg=B(_To(_TS,new T(function(){var _Vh=E(_V9);return _Vh[1]>0?E(_Vh):E(_Ou);}),B(_Ve(_Vd)))),_Vi=B(_PM(_Vg[1],_Vg[2]));}else{var _Vj=B(_To(_TS,new T(function(){var _Vk=E(_V9);return _Vk[1]>0?E(_Vk):E(_Ou);}),_UA)),_Vi=B(_PM(_Vj[1],_Vj[2]));}var _Vl=_Vi,_Vm=_Vl;return _Vm;});return [1,new T(function(){return E(E(_Vc)[1]);}),new T(function(){var _Vn=E(E(_Vc)[2]);return _Vn[0]==0?[0]:[1,_PZ,_Vn];})];}else{var _Vo=B(_To(_TS,new T(function(){var _Vp=E(_V9)[1];if(_Vp>0){var _Vq=[0,_Vp+_Vb|0];}else{var _Vq=E(_Va);}var _Vr=_Vq,_Vs=_Vr;return _Vs;}),_UA)),_Vt=_Vo[2],_Vu=_Vb+E(_Vo[1])[1]|0;if(_Vu>=0){var _Vv=B(_TH(_Vu,new T(function(){return B(_1r(_PG,_Vt));}))),_Vw=_Vv[2],_Vx=E(_Vv[1]);return _Vx[0]==0?[1,_PX,new T(function(){var _Vy=E(_Vw);return _Vy[0]==0?[0]:[1,_PZ,_Vy];})]:B(_P(_Vx,new T(function(){var _Vz=E(_Vw);return _Vz[0]==0?[0]:[1,_PZ,_Vz];})));}else{return [1,_PX,new T(function(){var _VA=B(_1r(_PG,_Vt));return _VA[0]==0?[0]:[1,_PZ,_VA];})];}}}break;default:var _VB=E(_UB),_VC=_VB[1];if(_VC>=0){if(_VC<=7){_Uv=_TR;var _VD=_UA;_Ux=_VB;_Uw=_VD;return null;}else{_Uv=_TQ;var _VD=_UA;_Ux=_VB;_Uw=_VD;return null;}}else{_Uv=_TQ;var _VD=_UA;_Ux=_VB;_Uw=_VD;return null;}}})(_Uv,_Uw,_Ux);if(_Uy!=null){return _Uy;}}},_VE=function(_VF){return [1,_u6,new T(function(){var _VG=B(_RS(E(E(E(E(_Uh)[1])[2])[1])[1],_Ui,_Uj,_Uk,_Ul,_Um,_R3,new T(function(){return B(A(_Ud,[_Ut,_Us]));})));return B(_Uu(_Uq,_VG[1],_VG[2]));})];};if(!B(A(_TO,[B(_LI(B(_PV(_Uh)))),_Us,new T(function(){return B(A(_LM,[_Ut,_AU]));})]))){if(!B(A(_Up,[_Us]))){var _VH=B(_RS(E(E(E(E(_Uh)[1])[2])[1])[1],_Ui,_Uj,_Uk,_Ul,_Um,_R3,_Us));return new F(function(){return _Uu(_Uq,_VH[1],_VH[2]);});}else{return new F(function(){return _VE(_);});}}else{return new F(function(){return _VE(_);});}}else{return !B(A(_TO,[B(_LI(B(_PV(_Uh)))),_Us,new T(function(){return B(A(_LM,[_Ut,_AU]));})]))?E(_U7):E(_U8);}}else{return E(_U9);}},_VI=function(_VJ){var _VK=u_towlower(_VJ),_VL=_VK;return _VL>>>0>1114111?B(_fm(_VL)):_VL;},_VM=function(_VN){return new F(function(){return err(B(unAppCStr("Printf.printf: ",_VN)));});},_VO=new T(function(){return B(unCStr("bad argument"));}),_VP=new T(function(){return B(_VM(_VO));}),_VQ=new T(function(){return B(unCStr("Printf.dfmt: impossible"));}),_VR=new T(function(){return B(err(_VQ));}),_VS=[0,45],_VT=[1,_VS,_v],_VU=new T(function(){return B(err(_VQ));}),_VV=function(_VW,_VX){var _VY=E(_VW);return _VY[0]==0?function(_d6){return new F(function(){return _P(new T(function(){var _VZ=B(_Oc(E(_VX)[1])),_W0=jsShow(B(_B8(_VZ[1],_VZ[2]))[1]),_W1=_W0;return fromJSStr(_W1);}),_d6);});}:function(_d6){return new F(function(){return _P(new T(function(){var _W2=E(E(_VY[1])[1]);if(!_W2){var _W3=jsRound(E(_VX)[1]),_W4=_W3,_W5=jsShow(_W4),_W6=_W5,_W7=fromJSStr(_W6);}else{var _W8=B(_Oc(E(_VX)[1]));if(_W2>=0){var _W9=B(_Ao(10,_W2)),_Wa=jsRound(B(_B8(_W8[1],_W8[2]))[1]*_W9),_Wb=_Wa,_Wc=jsShow(_Wb/_W9),_Wd=_Wc,_We=fromJSStr(_Wd);}else{var _We=E(_Af);}var _Wf=_We,_Wg=_Wf,_Wh=_Wg,_Wi=_Wh,_W7=_Wi;}var _Wj=_W7;return _Wj;}),_d6);});};},_Wk=function(_Wl){var _Wm=u_towupper(_Wl),_Wn=_Wm;return _Wn>>>0>1114111?B(_fm(_Wn)):_Wn;},_Wo=function(_Wp){return [0,B(_Wk(E(_Wp)[1]))];},_Wq=function(_Wr,_Ws,_Wt){var _Wu=E(_Wt);switch(_Wu[0]){case 3:var _Wv=_Wu[1],_Ww=u_iswupper(_Wr),_Wx=_Ww;switch(B(_VI(_Wr))){case 101:var _Wy=B(_Ug(_Pt,_GB,_H8,_H6,_Hd,_H2,_Hj,_Hf,_Hn,_TQ,new T(function(){var _Wz=E(_Ws);return _Wz[1]>=0?[1,_Wz]:[0];}),_Wv));break;case 102:var _Wy=B(_Ug(_Pt,_GB,_H8,_H6,_Hd,_H2,_Hj,_Hf,_Hn,_TR,new T(function(){var _WA=E(_Ws);return _WA[1]>=0?[1,_WA]:[0];}),_Wv));break;case 103:var _WB=E(_Ws),_Wy=_WB[1]>=0?B(A(_VV,[[1,_WB],_Wv,_v])):B(A(_VV,[_2w,_Wv,_v]));break;default:var _Wy=E(_VU);}var _WC=_Wy,_WD=E(_Wx);if(!_WD){var _WE=E(_WC);if(!_WE[0]){return [0,_v,_v];}else{var _WF=_WE[1],_WG=_WE[2],_WH=E(_WF),_WI=_WH[1],_WJ=E(_WI);return _WJ==45?[0,_VT,_WG]:[0,_v,_WE];}}else{var _WK=B(_1r(_Wo,_WC));if(!_WK[0]){return [0,_v,_v];}else{var _WL=_WK[1],_WM=_WK[2],_WN=E(_WL),_WO=_WN[1],_WP=E(_WO);return _WP==45?[0,_VT,_WM]:[0,_v,_WK];}}break;case 4:var _WQ=_Wu[1],_WR=u_iswupper(_Wr),_WS=_WR;switch(B(_VI(_Wr))){case 101:var _WT=B(_Ug(_Nt,_Fg,_GJ,_GG,_GO,_GC,_GU,_GQ,_GY,_TQ,new T(function(){var _WU=E(_Ws);return _WU[1]>=0?[1,_WU]:[0];}),_WQ));break;case 102:var _WT=B(_Ug(_Nt,_Fg,_GJ,_GG,_GO,_GC,_GU,_GQ,_GY,_TR,new T(function(){var _WV=E(_Ws);return _WV[1]>=0?[1,_WV]:[0];}),_WQ));break;case 103:var _WW=E(_Ws)[1];if(_WW>=0){var _WX=E(_WW);if(!_WX){var _WY=jsRound(E(_WQ)[1]),_WZ=_WY,_X0=jsShow(_WZ),_X1=_X0,_X2=fromJSStr(_X1);}else{var _X3=B(_Ao(10,_WX)),_X4=jsRound(E(_WQ)[1]*_X3),_X5=_X4,_X6=jsShow(_X5/_X3),_X7=_X6,_X2=fromJSStr(_X7);}var _X8=_X2;}else{var _X9=jsShow(E(_WQ)[1]),_Xa=_X9,_X8=fromJSStr(_Xa);}var _Xb=_X8,_Xc=_Xb,_WT=_Xc;break;default:var _WT=E(_VR);}var _Xd=_WT,_Xe=E(_WS);if(!_Xe){var _Xf=E(_Xd);if(!_Xf[0]){return [0,_v,_v];}else{var _Xg=_Xf[1],_Xh=_Xf[2],_Xi=E(_Xg),_Xj=_Xi[1],_Xk=E(_Xj);return _Xk==45?[0,_VT,_Xh]:[0,_v,_Xf];}}else{var _Xl=B(_1r(_Wo,_Xd));if(!_Xl[0]){return [0,_v,_v];}else{var _Xm=_Xl[1],_Xn=_Xl[2],_Xo=E(_Xm),_Xp=_Xo[1],_Xq=E(_Xp);return _Xq==45?[0,_VT,_Xn]:[0,_v,_Xl];}}break;default:return E(_VP);}},_Xr=[0,0],_Xs=function(_Xt){return new F(function(){return _1a(0,_Xt,_v);});},_Xu=function(_Xv,_Xw){while(1){var _Xx=E(_Xv);if(!_Xx[0]){return E(_Xw);}else{_Xv=_Xx[2];var _Xy=_Xw+1|0;_Xw=_Xy;continue;}}},_Xz=[0,48],_XA=function(_XB,_XC){var _XD=_XB-B(_Xu(_XC,0))|0;if(_XD>0){var _XE=function(_XF){return _XF>1?[1,_Xz,new T(function(){return B(_XE(_XF-1|0));})]:E([1,_Xz,_XC]);};return new F(function(){return _XE(_XD);});}else{return E(_XC);}},_XG=[0,0],_XH=[0,-2147483648],_XI=function(_XJ,_XK){while(1){var _XL=(function(_XM,_XN){var _XO=E(_XN);switch(_XO[0]){case 0:_XJ=_XG;_XK=[2,_XH,new T(function(){return B(_eb(E(_XO[1])[1]));})];return null;case 2:var _XP=_XO[2];return !B(_Z(_XP,_Xr))?[0,_v,new T(function(){return B(_XA(E(_XM)[1],B(_Xs(_XP))));})]:[0,_VT,new T(function(){return B(_XA(E(_XM)[1],B(_1a(0,B(_e5(_XP)),_v))));})];default:return E(_VP);}})(_XJ,_XK);if(_XL!=null){return _XL;}}},_XQ=[1,_s8,_v],_XR=function(_XS){return new F(function(){return err(B(unAppCStr("Char.digitToInt: not a digit ",new T(function(){var _XT=E(_XS);return _XT==39?E(_sa):[1,_s8,new T(function(){return B(_rS(_XT,_XQ));})];}))));});},_XU=function(_XV){var _XW=function(_XX){var _XY=function(_XZ){if(_XV<65){return new F(function(){return _XR(_XV);});}else{if(_XV>70){return new F(function(){return _XR(_XV);});}else{return (_XV-65|0)+10|0;}}};if(_XV<97){return new F(function(){return _XY(_);});}else{if(_XV>102){return new F(function(){return _XY(_);});}else{return (_XV-97|0)+10|0;}}};if(_XV<48){return new F(function(){return _XW(_);});}else{if(_XV>57){return new F(function(){return _XW(_);});}else{return _XV-48|0;}}},_Y0=function(_Y1,_Y2){while(1){var _Y3=(function(_Y4,_Y5){var _Y6=E(_Y5);if(!_Y6[0]){return [0,_Y4,_v];}else{var _Y7=E(_Y6[1])[1];if(_Y7<48){return [0,_Y4,_Y6];}else{if(_Y7>57){return [0,_Y4,_Y6];}else{_Y1=new T(function(){return [0,(imul(E(_Y4)[1],10)|0)+B(_XU(_Y7))|0];});_Y2=_Y6[2];return null;}}}})(_Y1,_Y2);if(_Y3!=null){return _Y3;}}},_Y8=new T(function(){return B(unCStr("argument list ended prematurely"));}),_Y9=new T(function(){return B(_VM(_Y8));}),_Ya=[0,-1],_Yb=function(_Yc){return [0,E(_Yc)[1]];},_Yd=function(_Ye){var _Yf=E(_Ye);switch(_Yf[0]){case 0:return new F(function(){return _Yb(_Yf[1]);});break;case 2:return new F(function(){return _KB(_Yf[2]);});break;default:return E(_VP);}},_Yg=function(_Yh,_Yi,_Yj,_Yk,_Yl){while(1){var _Ym=(function(_Yn,_Yo,_Yp,_Yq,_Yr){var _Ys=E(_Yq);if(!_Ys[0]){return [0,_XG,_Ya,_Yn,_Yo,_Yp,_v,_Yr];}else{var _Yt=_Ys[2],_Yu=E(E(_Ys[1])[1]);switch(_Yu){case 42:var _Yv=new T(function(){var _Yw=E(_Yr);return _Yw[0]==0?E(_Y9):[0,_Yw[2],new T(function(){return B(_Yd(_Yw[1]));})];}),_Yx=new T(function(){var _Yy=E(_Yt);if(!_Yy[0]){var _Yz=[0,_Ya,_v,new T(function(){return E(E(_Yv)[1]);})];}else{if(E(E(_Yy[1])[1])==46){var _YA=E(_Yy[2]);if(!_YA[0]){var _YB=B(_Y0(_XG,_v)),_YC=[0,_YB[1],_YB[2],new T(function(){return E(E(_Yv)[1]);})];}else{if(E(E(_YA[1])[1])==42){var _YD=new T(function(){var _YE=E(E(_Yv)[1]);return _YE[0]==0?E(_Y9):[0,_YE[2],new T(function(){return B(_Yd(_YE[1]));})];}),_YF=[0,new T(function(){return E(E(_YD)[2]);}),_YA[2],new T(function(){return E(E(_YD)[1]);})];}else{var _YG=B(_Y0(_XG,_YA)),_YF=[0,_YG[1],_YG[2],new T(function(){return E(E(_Yv)[1]);})];}var _YH=_YF,_YC=_YH;}var _YI=_YC;}else{var _YI=[0,_Ya,_Yy,new T(function(){return E(E(_Yv)[1]);})];}var _YJ=_YI,_Yz=_YJ;}return _Yz;});return [0,new T(function(){return E(E(_Yv)[2]);}),new T(function(){return E(E(_Yx)[1]);}),_Yn,_Yo,_Yp,new T(function(){return E(E(_Yx)[2]);}),new T(function(){return E(E(_Yx)[3]);})];case 43:var _YK=_Yn,_YL=_Yo;_Yj=_o;_Yk=_Yt;var _YM=_Yr;_Yh=_YK;_Yi=_YL;_Yl=_YM;return null;case 45:_Yh=_o;var _YL=_Yo,_YN=_Yp;_Yk=_Yt;var _YM=_Yr;_Yi=_YL;_Yj=_YN;_Yl=_YM;return null;case 46:var _YO=new T(function(){var _YP=E(_Yt);if(!_YP[0]){var _YQ=B(_Y0(_XG,_v)),_YR=[0,_YQ[1],_YQ[2],_Yr];}else{if(E(E(_YP[1])[1])==42){var _YS=new T(function(){var _YT=E(_Yr);return _YT[0]==0?E(_Y9):[0,_YT[2],new T(function(){return B(_Yd(_YT[1]));})];}),_YU=[0,new T(function(){return E(E(_YS)[2]);}),_YP[2],new T(function(){return E(E(_YS)[1]);})];}else{var _YV=B(_Y0(_XG,_YP)),_YU=[0,_YV[1],_YV[2],_Yr];}var _YW=_YU,_YR=_YW;}return _YR;});return [0,_XG,new T(function(){return E(E(_YO)[1]);}),_Yn,_Yo,_Yp,new T(function(){return E(E(_YO)[2]);}),new T(function(){return E(E(_YO)[3]);})];case 48:var _YK=_Yn;_Yi=_o;var _YN=_Yp;_Yk=_Yt;var _YM=_Yr;_Yh=_YK;_Yj=_YN;_Yl=_YM;return null;default:if(_Yu<48){return [0,_XG,_Ya,_Yn,_Yo,_Yp,_Ys,_Yr];}else{if(_Yu>57){return [0,_XG,_Ya,_Yn,_Yo,_Yp,_Ys,_Yr];}else{var _YX=new T(function(){var _YY=B(_Y0(_XG,_Ys));return [0,_YY[1],_YY[2]];}),_YZ=new T(function(){var _Z0=E(E(_YX)[2]);if(!_Z0[0]){var _Z1=[0,_Ya,_v,_Yr];}else{if(E(E(_Z0[1])[1])==46){var _Z2=E(_Z0[2]);if(!_Z2[0]){var _Z3=B(_Y0(_XG,_v)),_Z4=[0,_Z3[1],_Z3[2],_Yr];}else{if(E(E(_Z2[1])[1])==42){var _Z5=new T(function(){var _Z6=E(_Yr);return _Z6[0]==0?E(_Y9):[0,_Z6[2],new T(function(){return B(_Yd(_Z6[1]));})];}),_Z7=[0,new T(function(){return E(E(_Z5)[2]);}),_Z2[2],new T(function(){return E(E(_Z5)[1]);})];}else{var _Z8=B(_Y0(_XG,_Z2)),_Z7=[0,_Z8[1],_Z8[2],_Yr];}var _Z9=_Z7,_Z4=_Z9;}var _Za=_Z4;}else{var _Za=[0,_Ya,_Z0,_Yr];}var _Zb=_Za,_Z1=_Zb;}var _Zc=_Z1;return _Zc;});return [0,new T(function(){return E(E(_YX)[1]);}),new T(function(){return E(E(_YZ)[1]);}),_Yn,_Yo,_Yp,new T(function(){return E(E(_YZ)[2]);}),new T(function(){return E(E(_YZ)[3]);})];}}}}})(_Yh,_Yi,_Yj,_Yk,_Yl);if(_Ym!=null){return _Ym;}}},_Zd=new T(function(){return B(unCStr("formatting string ended prematurely"));}),_Ze=new T(function(){return B(_VM(_Zd));}),_Zf=function(_Zg,_Zh){if(!B(_Z(_Zh,_Zg))){if(!B(_B0(_Zg,_Xr))){var _Zi=B(_My(_Zh,_Zg));return new F(function(){return _P(B(_Zf(_Zg,_Zi[1])),[1,new T(function(){return [0,B(_PC(B(_fo(_Zi[2]))))];}),_v]);});}else{return E(_yA);}}else{return [1,new T(function(){return [0,B(_PC(B(_fo(_Zh))))];}),_v];}},_Zj=[0,2],_Zk=function(_Zl,_Zm,_Zn){var _Zo=E(_Zn);switch(_Zo[0]){case 0:return new F(function(){return _Zf(_Zl,B(_eb(E(_Zo[1])[1])));});break;case 2:var _Zp=_Zo[2],_Zq=E(_Zm)[1];if(!B(_Z(_Zp,_Xr))){return new F(function(){return _XA(_Zq,B(_Zf(_Zl,_Zp)));});}else{return new F(function(){return _XA(_Zq,B(_Zf(_Zl,B(_dV(B(_e5(B(_ed(_Zj,_Zo[1])))),_Zp)))));});}break;default:return E(_VP);}},_Zr=[0,37],_Zs=[0,16],_Zt=[0,10],_Zu=[0,8],_Zv=[0,43],_Zw=[1,_Zv,_v],_Zx=[0,32],_Zy=function(_Zz){return new F(function(){return _VM(new T(function(){return B(unAppCStr("bad formatting char ",[1,_Zz,_v]));}));});},_ZA=function(_ZB,_ZC){var _ZD=E(_ZB);if(!_ZD){return [0];}else{var _ZE=E(_ZC);return _ZE[0]==0?[0]:[1,_ZE[1],new T(function(){return B(_ZA(_ZD-1|0,_ZE[2]));})];}},_ZF=function(_ZG,_ZH){var _ZI=E(_ZG);if(!_ZI[0]){return E(_ZH)[0]==0?[0]:E(_Ze);}else{var _ZJ=_ZI[2],_ZK=E(_ZI[1]);if(E(_ZK[1])==37){var _ZL=function(_ZM){var _ZN=E(_ZH);if(!_ZN[0]){return E(_Y9);}else{var _ZO=B(_Yg(_s,_s,_s,_ZJ,_ZN)),_ZP=_ZO[2],_ZQ=_ZO[4],_ZR=E(_ZO[6]);if(!_ZR[0]){return E(_Ze);}else{var _ZS=_ZR[2],_ZT=E(_ZO[7]);if(!_ZT[0]){return E(_Y9);}else{var _ZU=_ZT[1],_ZV=_ZT[2],_ZW=E(_ZR[1]),_ZX=function(_ZY,_ZZ){var _100=new T(function(){var _101=B(_Xu(_ZZ,0)),_102=B(_Xu(_ZY,0)),_103=E(_ZO[1])[1];if((_101+_102|0)>=_103){var _104=[0];}else{var _105=_103-(_101+_102|0)|0;if(_105>0){if(_105<0){var _106=[0];}else{var _107=new T(function(){return [1,new T(function(){return !E(_ZQ)?E(_Zx):E(_Xz);}),_107];}),_106=B(_ZA(_105,_107));}var _108=_106,_109=_108;}else{var _109=[0];}var _10a=_109,_10b=_10a,_10c=_10b,_104=_10c;}var _10d=_104,_10e=_10d,_10f=_10e,_10g=_10f,_10h=_10g;return _10h;});return !E(_ZO[3])?!E(_ZQ)?B(_P(_100,new T(function(){return B(_P(_ZY,_ZZ));}))):B(_P(_ZY,new T(function(){return B(_P(_100,_ZZ));}))):B(_P(_ZY,new T(function(){return B(_P(_ZZ,_100));})));},_10i=function(_10j,_10k){var _10l=E(_10j);return _10l[0]==0?!E(_ZO[5])?B(_ZX(_v,_10k)):B(_ZX(_Zw,_10k)):B(_ZX(_10l,_10k));};switch(E(_ZW[1])){case 69:var _10m=B(_Wq(69,_ZP,_ZU));return new F(function(){return _P(B(_10i(_10m[1],_10m[2])),new T(function(){return B(_ZF(_ZS,_ZV));}));});break;case 71:var _10n=B(_Wq(71,_ZP,_ZU));return new F(function(){return _P(B(_10i(_10n[1],_10n[2])),new T(function(){return B(_ZF(_ZS,_ZV));}));});break;case 88:return new F(function(){return _P(B(_ZX(_v,new T(function(){return B(_1r(_Wo,B(_Zk(_Zs,_ZP,_ZU))));}))),new T(function(){return B(_ZF(_ZS,_ZV));}));});break;case 99:return new F(function(){return _P(B(_ZX(_v,[1,new T(function(){var _10o=E(_ZU);switch(_10o[0]){case 0:var _10p=E(_10o[1])[1];if(_10p>>>0>1114111){var _10q=B(_fm(_10p));}else{var _10q=[0,_10p];}var _10r=_10q,_10s=_10r,_10t=_10s,_10u=_10t,_10v=_10u;break;case 2:var _10w=B(_fo(_10o[2]));if(_10w>>>0>1114111){var _10x=B(_fm(_10w));}else{var _10x=[0,_10w];}var _10y=_10x,_10z=_10y,_10A=_10z,_10v=_10A;break;default:var _10v=E(_VP);}return _10v;}),_v])),new T(function(){return B(_ZF(_ZS,_ZV));}));});break;case 100:var _10B=B(_XI(_ZP,_ZU));return new F(function(){return _P(B(_10i(_10B[1],_10B[2])),new T(function(){return B(_ZF(_ZS,_ZV));}));});break;case 101:var _10C=B(_Wq(101,_ZP,_ZU));return new F(function(){return _P(B(_10i(_10C[1],_10C[2])),new T(function(){return B(_ZF(_ZS,_ZV));}));});break;case 102:var _10D=B(_Wq(102,_ZP,_ZU));return new F(function(){return _P(B(_10i(_10D[1],_10D[2])),new T(function(){return B(_ZF(_ZS,_ZV));}));});break;case 103:var _10E=B(_Wq(103,_ZP,_ZU));return new F(function(){return _P(B(_10i(_10E[1],_10E[2])),new T(function(){return B(_ZF(_ZS,_ZV));}));});break;case 105:var _10F=B(_XI(_ZP,_ZU));return new F(function(){return _P(B(_10i(_10F[1],_10F[2])),new T(function(){return B(_ZF(_ZS,_ZV));}));});break;case 111:return new F(function(){return _P(B(_ZX(_v,new T(function(){return B(_Zk(_Zu,_ZP,_ZU));}))),new T(function(){return B(_ZF(_ZS,_ZV));}));});break;case 115:return new F(function(){return _P(B(_ZX(_v,new T(function(){var _10G=E(_ZU);if(_10G[0]==1){var _10H=_10G[1],_10I=E(_ZP)[1];if(_10I<0){var _10J=E(_10H);}else{var _10J=_10I>0?B(_ZA(_10I,_10H)):[0];}var _10K=_10J,_10L=_10K,_10M=_10L;}else{var _10M=E(_VP);}return _10M;}))),new T(function(){return B(_ZF(_ZS,_ZV));}));});break;case 117:return new F(function(){return _P(B(_ZX(_v,new T(function(){return B(_Zk(_Zt,_ZP,_ZU));}))),new T(function(){return B(_ZF(_ZS,_ZV));}));});break;case 120:return new F(function(){return _P(B(_ZX(_v,new T(function(){return B(_Zk(_Zs,_ZP,_ZU));}))),new T(function(){return B(_ZF(_ZS,_ZV));}));});break;default:return new F(function(){return _Zy(_ZW);});}}}}},_10N=E(_ZJ);if(!_10N[0]){return new F(function(){return _ZL(_);});}else{if(E(E(_10N[1])[1])==37){return [1,_Zr,new T(function(){return B(_ZF(_10N[2],_ZH));})];}else{return new F(function(){return _ZL(_);});}}}else{return [1,_ZK,new T(function(){return B(_ZF(_ZJ,_ZH));})];}}},_10O=new T(function(){return B(unCStr(" could be found!"));}),_10P=function(_10Q){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_P(_10Q,_10O));}))));});},_10R=function(_10S){return function(_10T,_){var _10U=E(_10S),_10V=jsFind(toJSStr(_10U)),_10W=_10V,_10X=E(_10W);if(!_10X[0]){return new F(function(){return _10P(_10U);});}else{var _10Y=B(A(_xK,[_A8,_10X[1],_zz,_zy,_10T,_])),_10Z=_10Y,_110=E(new T(function(){return B(_1r(_xQ,B(_ZF(_DZ,new T(function(){return B(_E0([1,[1,new T(function(){return B(_1r(_xQ,_10S));})],_v],_v));})))));})),_111=jsFind(toJSStr(_110)),_112=_111,_113=E(_112);if(!_113[0]){return new F(function(){return _10P(_110);});}else{var _114=B(A(_xK,[_A8,_113[1],_zz,_DY,new T(function(){return E(E(_10Z)[2]);}),_])),_115=_114;return new F(function(){return A(new T(function(){return !B(_c8(_10S,_8Z))?E(_Aa):E(_DT);}),[new T(function(){return E(E(_115)[2]);}),_]);});}}};},_116=new T(function(){return B(_10R(_8Z));}),_117=function(_118){return E(_116);},_119=new T(function(){return B(unCStr("\u3055\u3041\u59cb\u3081\u3088\u3046"));}),_11a=new T(function(){return B(unCStr("\u3055\u3041\u59cb\u3081\u3088\u3046<br>\u30b2\u30fc\u30e0\u3092\u59cb\u3081\u307e\u3057\u3087\u3046\u3002\u53f3\u306e\u30dc\u30bf\u30f3\u304b\u3089\u3053\u306e\u30a2\u30a4\u30c6\u30e0\u3092\u8cfc\u5165\u3057\u3066\u304f\u3060\u3055\u3044\u3002"));}),_11b=new T(function(){return B(unCStr("fa-power-off"));}),_11c=[0,_11b,_11a,_119],_11d=[0,_DR,_117,_11c],_11e=[0,_8Z,_11d],_11f=[0,1],_11g=function(_11h){return E(_11f);},_11i=new T(function(){return B(_10R(_92));}),_11j=function(_11k){return E(_11i);},_11l=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u30b7\u30e7\u30c3\u30d7"));}),_11m=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u30b7\u30e7\u30c3\u30d7<br>\u30a2\u30a4\u30c6\u30e0\u304c\u8cfc\u5165\u3067\u304d\u308b\u3088\u3046\u306b\u306a\u308a\u307e\u3059\u3002"));}),_11n=new T(function(){return B(unCStr("fa-shopping-cart"));}),_11o=[0,_11n,_11m,_11l],_11p=[0,_11g,_11j,_11o],_11q=[0,_92,_11p],_11r=[0,10],_11s=function(_11t){return E(_11r);},_11u=new T(function(){return B(unCStr("fa-history"));}),_11v=new T(function(){return B(unCStr("\u521d\u671f\u5316"));}),_11w=new T(function(){return B(unCStr("\u521d\u671f\u5316<br>\u5b9f\u7e3e\u3092\u9664\u304f\u5168\u3066\u306e\u30c7\u30fc\u30bf\u3092\u30ea\u30bb\u30c3\u30c8\u3057\u307e\u3059\u3002<br>\u4eca\u3042\u308b\u4f9d\u5b58\u5ea6\u306f\u597d\u611f\u5ea6\u4fc2\u6570\u306b\u3001\u611b\u60c5\u306f\u4f9d\u5b58\u5ea6\u4fc2\u6570\u306b\u52a0\u308f\u308a\u307e\u3059\u3002"));}),_11x=[0,_11u,_11w,_11v],_11y=new T(function(){return B(unCStr("td"));}),_11z=function(_11A,_11B,_11C,_){var _11D=jsCreateElem(toJSStr(E(_11y))),_11E=_11D,_11F=jsAppendChild(_11E,E(_11C)[1]),_11G=[0,_11E],_11H=B(A(_11A,[_11B,_11G,_])),_11I=_11H;return _11G;},_11J=new T(function(){return B(unCStr("tr"));}),_11K=function(_11L,_11M,_11N,_){var _11O=jsCreateElem(toJSStr(E(_11J))),_11P=_11O,_11Q=jsAppendChild(_11P,E(_11N)[1]),_11R=[0,_11P],_11S=B(A(_11L,[_11M,_11R,_])),_11T=_11S;return _11R;},_11U=function(_11V,_11W,_11X,_){return new F(function(){return _11K(_wC,function(_11Y,_){var _11Z=B(_11z(_vf,_11V,_11Y,_)),_120=_11Z,_121=B(_11z(_vf,_11W,_11Y,_)),_122=_121;return _11Y;},_11X,_);});},_123=function(_124,_125){var _126=E(_124);if(!_126[0]){return [0];}else{var _127=_126[1];return _125>1?[1,_127,new T(function(){return B(_123(_126[2],_125-1|0));})]:[1,_127,_v];}},_128=new T(function(){return B(_1a(0,_AQ,_v));}),_129=new T(function(){return B(unCStr("%.2f"));}),_12a=new T(function(){return B(_1a(0,_Be,_v));}),_12b=function(_12c,_12d){var _12e=E(_12d);if(!_12e[0]){return [0,_v,_v];}else{var _12f=_12e[1];if(!B(A(_12c,[_12f]))){var _12g=new T(function(){var _12h=B(_12b(_12c,_12e[2]));return [0,_12h[1],_12h[2]];});return [0,[1,_12f,new T(function(){return E(E(_12g)[1]);})],new T(function(){return E(E(_12g)[2]);})];}else{return [0,_v,_12e];}}},_12i=function(_12j,_12k){var _12l=function(_12m,_12n){return !B(_c8(_12n,_v))?[0,_12m,new T(function(){var _12o=B(_12i(_12j,_12n));return [1,_12o[1],_12o[2]];})]:[0,_12m,_v];};if(_12j>=0){var _12p=B(_TH(_12j,_12k));return new F(function(){return _12l(_12p[1],_12p[2]);});}else{return new F(function(){return _12l(_v,_12k);});}},_12q=[0,44],_12r=[1,_12q,_v],_12s=function(_12t){return E(E(_12t)[1])==46?true:false;},_12u=function(_12v,_12w){var _12x=E(_12w);return _12x[0]==0?[0]:[1,_12v,[1,_12x[1],new T(function(){return B(_12u(_12v,_12x[2]));})]];},_12y=function(_12z){var _12A=new T(function(){var _12B=B(_12b(_12s,_12z));return [0,_12B[1],_12B[2]];}),_12C=B(_12i(3,new T(function(){return B(_E0(E(_12A)[1],_v));})));return new F(function(){return _P(B(_E0(B(_zr([1,_12C[1],new T(function(){return B(_12u(_12r,_12C[2]));})])),_v)),new T(function(){return E(E(_12A)[2]);}));});},_12D=function(_12E){return _12E>1000?B(_12y(new T(function(){var _12F=B(_Au(_12E)),_12G=_12F[1],_12H=_12F[2];if(_12H>=0){var _12I=B(_1a(0,B(_Bf(_12G,_12H)),_v));}else{var _12J= -_12H;if(_12J<=52){var _12K=hs_uncheckedIShiftRA64(B(_AN(_12G)),_12J),_12L=_12K,_12M=B(_1a(0,B(_Ax(_12L)),_v));}else{var _12M=!B(_Z(_12G,_AQ))?E(_128):E(_12a);}var _12N=_12M,_12O=_12N,_12I=_12O;}var _12P=_12I,_12Q=_12P;return _12Q;}))):B(_12y(new T(function(){return B(_123(B(_ZF(_129,new T(function(){return B(_E0([1,[4,[0,_12E]],_v],_v));}))),5));})));},_12R=new T(function(){return B(unCStr("\u5185\u5bb9"));}),_12S=new T(function(){return B(unCStr("\u5b9f\u7e3e\u540d"));}),_12T=new T(function(){return B(unCStr("th"));}),_12U=function(_12V,_12W,_12X,_){var _12Y=jsCreateElem(toJSStr(E(_12T))),_12Z=_12Y,_130=jsAppendChild(_12Z,E(_12X)[1]),_131=[0,_12Z],_132=B(A(_12V,[_12W,_131,_])),_133=_132;return _131;},_134=function(_135,_){var _136=B(_12U(_vf,_12S,_135,_)),_137=_136,_138=B(_12U(_vf,_12R,_135,_)),_139=_138;return _135;},_13a=function(_13b,_){return new F(function(){return _11K(_wC,_134,_13b,_);});},_13c=new T(function(){return B(unCStr("\u9805\u76ee\u540d"));}),_13d=function(_13e,_){var _13f=B(_12U(_vf,_13c,_13e,_)),_13g=_13f,_13h=B(_12U(_vf,_12R,_13e,_)),_13i=_13h;return _13e;},_13j=function(_13b,_){return new F(function(){return _11K(_wC,_13d,_13b,_);});},_13k=new T(function(){return B(unCStr("Aichan"));}),_13l=new T(function(){return [0,toJSStr(_v)];}),_13m=[0,93],_13n=[1,_13m,_v],_13o=new T(function(){return [0,toJSStr(_13n)];}),_13p=[0,125],_13q=[1,_13p,_v],_13r=new T(function(){return [0,toJSStr(_13q)];}),_13s=[0,58],_13t=[1,_13s,_v],_13u=new T(function(){return [0,toJSStr(_13t)];}),_13v=[0,44],_13w=[1,_13v,_v],_13x=new T(function(){return [0,toJSStr(_13w)];}),_13y=new T(function(){return [0,"false"];}),_13z=function(_13A){var _13B=jsShow(E(_13A)[1]),_13C=_13B;return [0,_13C];},_13D=function(_13E){var _13F=jsStringify(E(_13E)[1]),_13G=_13F;return [0,_13G];},_13H=new T(function(){return [0,"null"];}),_13I=[0,91],_13J=[1,_13I,_v],_13K=new T(function(){return [0,toJSStr(_13J)];}),_13L=[0,123],_13M=[1,_13L,_v],_13N=new T(function(){return [0,toJSStr(_13M)];}),_13O=[0,34],_13P=[1,_13O,_v],_13Q=new T(function(){return [0,toJSStr(_13P)];}),_13R=new T(function(){return [0,"true"];}),_13S=function(_13T,_13U){var _13V=E(_13U);switch(_13V[0]){case 0:return [0,new T(function(){return B(_13z(_13V[1]));}),_13T];case 1:return [0,new T(function(){return B(_13D(_13V[1]));}),_13T];case 2:return !E(_13V[1])?[0,_13y,_13T]:[0,_13R,_13T];case 3:var _13W=E(_13V[1]);return _13W[0]==0?[0,_13K,[1,_13o,_13T]]:[0,_13K,new T(function(){var _13X=B(_13S(new T(function(){var _13Y=function(_13Z){var _140=E(_13Z);return _140[0]==0?E([1,_13o,_13T]):[1,_13x,new T(function(){var _141=B(_13S(new T(function(){return B(_13Y(_140[2]));}),_140[1]));return [1,_141[1],_141[2]];})];};return B(_13Y(_13W[2]));}),_13W[1]));return [1,_13X[1],_13X[2]];})];case 4:var _142=E(_13V[1]);if(!_142[0]){return [0,_13N,[1,_13r,_13T]];}else{var _143=E(_142[1]);return [0,_13N,[1,new T(function(){return B(_13D(_143[1]));}),[1,_13u,new T(function(){var _144=B(_13S(new T(function(){var _145=function(_146){var _147=E(_146);if(!_147[0]){return E([1,_13r,_13T]);}else{var _148=E(_147[1]);return [1,_13x,[1,_13Q,[1,_148[1],[1,_13Q,[1,_13u,new T(function(){var _149=B(_13S(new T(function(){return B(_145(_147[2]));}),_148[2]));return [1,_149[1],_149[2]];})]]]]];}};return B(_145(_142[2]));}),_143[2]));return [1,_144[1],_144[2]];})]]];}break;default:return [0,_13H,_13T];}},_14a=function(_14b){var _14c=jsCat(new T(function(){var _14d=B(_13S(_v,_14b));return [1,_14d[1],_14d[2]];}),E(_13l)[1]),_14e=_14c;return E(_14e);},_14f=function(_14g){return new F(function(){return _oa(function(_){var _=0;return new F(function(){return eval(_14g);});});});},_14h=new T(function(){return [0,"(function(k,v) {localStorage.setItem(k,v);})"];}),_14i=function(_14j,_14k){return function(_14l,_){var _14m=B(A(new T(function(){return B(A(_14f,[E(_14h)[1],E(toJSStr(E(_14k)))]));}),[E(B(_14a(B(A(new T(function(){return B(_2c(_14j));}),[_14l]))))),_])),_14n=_14m;return _cH;};},_14o=new T(function(){return B(_14i(_qt,_13k));}),_14p=new T(function(){return B(unCStr("game"));}),_14q=new T(function(){return B(unCStr("\u5b9f\u7e3e\u7372\u5f97: "));}),_14r=new T(function(){return B(unCStr("Close"));}),_14s=new T(function(){return B(unCStr("&times;"));}),_14t=function(_14u,_){var _14v=B(A(_vr,[_dl,_14u,_wq,_14s,_])),_14w=_14v;return _14u;},_14x=new T(function(){return B(unCStr("sr-only"));}),_14y=new T(function(){return B(unCStr("true"));}),_14z=new T(function(){return B(unCStr("aria-hidden"));}),_14A=function(_14B,_){var _14C=B(_ws(_wC,_14t,_14B,_)),_14D=_14C,_14E=B(A(_vl,[_dl,_14D,_14z,_14y,_])),_14F=_14E,_14G=B(_ws(_vf,_14r,_14B,_)),_14H=_14G,_14I=B(A(_vl,[_dl,_14H,_vC,_14x,_])),_14J=_14I;return _14B;},_14K=new T(function(){return B(unCStr("close"));}),_14L=new T(function(){return B(unCStr("data-dismiss"));}),_14M=new T(function(){return B(unCStr("alert"));}),_14N=new T(function(){return B(unCStr("role"));}),_14O=new T(function(){return B(unCStr("alert alert-info fade in tip"));}),_14P=new T(function(){return B(unCStr("div"));}),_14Q=function(_14R,_14S,_14T,_){var _14U=jsCreateElem(toJSStr(E(_14P))),_14V=_14U,_14W=jsAppendChild(_14V,E(_14T)[1]),_14X=[0,_14V],_14Y=B(A(_14R,[_14S,_14X,_])),_14Z=_14Y;return _14X;},_150=new T(function(){return B(unCStr("alerts"));}),_151=new T(function(){return B(unCStr("\').alert(\'close\')"));}),_152=new T(function(){return B(unCStr("list-group-item"));}),_153=new T(function(){return B(_14f("(function(e,c){var first = e.firstChild; e.insertBefore(c,first);})"));}),_154=function(_155){return function(_156,_){var _157=B(A(new T(function(){return B(A(_153,[E(E(_155)[1])]));}),[E(E(_156)[1]),_])),_158=_157;return _cH;};},_159=new T(function(){return B(unCStr("unread-badge"));}),_15a=new T(function(){return B(unCStr("<br>"));}),_15b=new T(function(){return B(unCStr("log-group"));}),_15c=new T(function(){return B(unCStr(") "));}),_15d=new T(function(){return B(unCStr("li"));}),_15e=new T(function(){return B(unCStr("var d = new Date(); d.getHours() + \':\' + d.getMinutes() + \':\' + d.getSeconds()"));}),_15f=function(_15g,_15h){while(1){var _15i=E(_15g);if(!_15i){return E(_15h);}else{var _15j=E(_15h);if(!_15j[0]){return [0];}else{_15g=_15i-1|0;_15h=_15j[2];continue;}}}},_15k=function(_15l,_15m,_15n){while(1){var _15o=E(_15m);if(!_15o[0]){return true;}else{var _15p=E(_15n);if(!_15p[0]){return false;}else{if(!B(A(_2E,[_15l,_15o[1],_15p[1]]))){return false;}else{_15m=_15o[2];_15n=_15p[2];continue;}}}}},_15q=function(_15r,_15s,_15t,_15u){if(!B(_15k(_cj,_15r,[1,_15t,_15u]))){return [1,_15t,new T(function(){return B(_15v(_15r,_15s,_15u));})];}else{return new F(function(){return _P(_15s,new T(function(){var _15w=B(_Xu(_15r,0));if(_15w>=0){var _15x=B(_15v(_15r,_15s,B(_15f(_15w,[1,_15t,_15u]))));}else{var _15x=B(_15q(_15r,_15s,_15t,_15u));}var _15y=_15x,_15z=_15y;return _15z;}));});}},_15v=function(_15A,_15B,_15C){var _15D=E(_15C);if(!_15D[0]){return [0];}else{var _15E=_15D[1],_15F=_15D[2];if(!B(_15k(_cj,_15A,_15D))){return [1,_15E,new T(function(){return B(_15v(_15A,_15B,_15F));})];}else{return new F(function(){return _P(_15B,new T(function(){var _15G=B(_Xu(_15A,0));if(_15G>=0){var _15H=B(_15v(_15A,_15B,B(_15f(_15G,_15D))));}else{var _15H=B(_15q(_15A,_15B,_15E,_15F));}var _15I=_15H,_15J=_15I;return _15J;}));});}}},_15K=new T(function(){return B(unCStr("strong"));}),_15L=function(_15M,_15N,_15O,_){var _15P=jsCreateElem(toJSStr(E(_15K))),_15Q=_15P,_15R=jsAppendChild(_15Q,E(_15O)[1]),_15S=[0,_15Q],_15T=B(A(_15M,[_15N,_15S,_])),_15U=_15T;return _15S;},_15V=function(_15W,_15X){return function(_15Y,_){var _15Z=E(_15b),_160=jsFind(toJSStr(_15Z)),_161=_160,_162=E(_161);if(!_162[0]){return new F(function(){return _10P(_15Z);});}else{var _163=jsEval(toJSStr(E(_15e))),_164=_163,_165=jsCreateElem(toJSStr(E(_15d))),_166=_165,_167=[0,_166],_168=B(A(_vl,[_dl,_167,_vC,_152,_])),_169=_168,_16a=B(_15L(_vf,new T(function(){return B(_P(_15W,new T(function(){return B(unAppCStr(" (",new T(function(){return B(_P(fromJSStr(_164),_15c));})));})));}),_167,_)),_16b=_16a,_16c=B(_ws(_wC,function(_16d,_){var _16e=B(A(_vr,[_dl,_16d,_wq,new T(function(){return B(_15v(_15a,_vA,_15X));}),_])),_16f=_16e;return _16d;},_167,_)),_16g=_16c,_16h=B(A(_154,[_162[1],_167,_])),_16i=_16h,_16j=E(_159),_16k=jsFind(toJSStr(_16j)),_16l=_16k,_16m=E(_16l);if(!_16m[0]){return new F(function(){return _10P(_16j);});}else{var _16n=E(_15Y),_16o=E(_16n[11])[1]+1|0,_16p=jsSet(E(_16m[1])[1],toJSStr(E(_zB)),toJSStr(B(_82(0,_16o,_v))));return [0,_cH,[0,_16n[1],_16n[2],_16n[3],_16n[4],_16n[5],_16n[6],_16n[7],_16n[8],_16n[9],_16n[10],[0,_16o]]];}}};},_16q=function(_16r,_16s){return function(_16t,_){var _16u=B(_m8(_)),_16v=_16u,_16w=E(_150),_16x=jsFind(toJSStr(_16w)),_16y=_16x,_16z=E(_16y);if(!_16z[0]){return new F(function(){return _10P(_16w);});}else{var _16A=_16z[1],_16B=B(A(_vr,[_dl,_16A,_wq,_v,_])),_16C=_16B,_16D=B(_14Q(_wC,function(_16E,_){var _16F=B(_vK(_wC,_14A,_16E,_)),_16G=_16F,_16H=B(A(_vl,[_dl,_16G,_vE,_vD,_])),_16I=_16H,_16J=B(A(_vl,[_dl,_16G,_vC,_14K,_])),_16K=_16J,_16L=B(A(_vl,[_dl,_16G,_14L,_14M,_])),_16M=_16L,_16N=B(_ws(_wC,function(_16O,_){var _16P=B(A(_vr,[_dl,_16O,_wq,_16s,_])),_16Q=_16P;return _16O;},_16E,_)),_16R=_16N;return _16E;},_16A,_)),_16S=_16D,_16T=E(_16S),_16U=jsSetAttr(_16T[1],toJSStr(E(_w6)),toJSStr(B(unAppCStr("alert-",new T(function(){return B(_1a(0,_16v,_v));}))))),_16V=B(A(_vl,[_dl,_16T,_vC,_14O,_])),_16W=_16V,_16X=B(A(_vl,[_dl,_16T,_14N,_14M,_])),_16Y=_16X,_16Z=jsSetTimeout(5000,function(_){var _170=jsEval(toJSStr(B(unAppCStr("$(\'#alert-",new T(function(){return B(_P(B(_1a(0,_16v,_v)),_151));}))))),_171=_170;return _cH;});return new F(function(){return A(new T(function(){return B(_15V(_16r,_16s));}),[_16t,_]);});}};},_172=function(_173,_174){return function(_175,_){var _176=B(A(new T(function(){return B(_16q(_14p,new T(function(){return B(_P(_14q,_174));})));}),[new T(function(){var _177=E(_175);return [0,_177[1],_177[2],_177[3],_177[4],_177[5],new T(function(){return B(_6U(_174,[1,_173],_177[6]));}),_177[7],_177[8],_177[9],_177[10],_177[11]];}),_])),_178=_176,_179=new T(function(){return E(E(_178)[2]);}),_17a=B(A(_14o,[_179,_])),_17b=_17a,_17c=B(_17d(_179,_)),_17e=_17c;return new F(function(){return _17f(new T(function(){return E(E(_17e)[2]);}),_);});};},_17g=new T(function(){return B(unCStr("\u3092\u8d85\u3048\u308b"));}),_17h=new T(function(){return B(unCStr("\u611b\u60c5\u304c"));}),_17i=function(_17j,_17k){return function(_17l,_){var _17m=E(_17l);return E(_17m[1])[1]<=E(new T(function(){return [0,B(_zv(_17j))];}))[1]?[0,_cH,_17m]:B(A(new T(function(){return B(_172(new T(function(){return B(_P(_17h,new T(function(){return B(_P(B(_1a(0,_17j,_v)),_17g));})));}),_17k));}),[_17m,_]));};},_17n=new T(function(){return B(unCStr("\u30a2\u30a4\u3068\u306e\u906d\u9047"));}),_17o=[0,100],_17p=new T(function(){return B(_17i(_17o,_17n));}),_17q=new T(function(){return [0,_17n,_17p];}),_17r=new T(function(){return B(unCStr("\u611b\u3055\u308c\u6c17\u5206"));}),_17s=[0,10000],_17t=new T(function(){return B(_17i(_17s,_17r));}),_17u=new T(function(){return [0,_17r,_17t];}),_17v=new T(function(){return B(unCStr("\u611b\u30e9\u30d6\u30e6\u30fc"));}),_17w=[0,1000000],_17x=new T(function(){return B(_17i(_17w,_17v));}),_17y=new T(function(){return [0,_17v,_17x];}),_17z=new T(function(){return B(unCStr("\u611b\u3055\u3093\u306e\u611b\u304c\u91cd\u3044"));}),_17A=[0,100000000],_17B=new T(function(){return B(_17i(_17A,_17z));}),_17C=new T(function(){return [0,_17z,_17B];}),_17D=[1,I_fromBits([1215752192,23])],_17E=new T(function(){return B(unCStr("\u611b\u3061\u3083\u3093\u30de\u30b9\u30bf\u30fc"));}),_17F=new T(function(){return B(_17i(_17D,_17E));}),_17G=new T(function(){return [0,_17E,_17F];}),_17H=new T(function(){return B(unCStr("\u597d\u611f\u5ea6\u304c"));}),_17I=function(_17J,_17K){return function(_17L,_){var _17M=E(_17L);return E(_17M[2])[1]<=E(new T(function(){return [0,B(_zv(_17J))];}))[1]?[0,_cH,_17M]:B(A(new T(function(){return B(_172(new T(function(){return B(_P(_17H,new T(function(){return B(_P(B(_1a(0,_17J,_v)),_17g));})));}),_17K));}),[_17M,_]));};},_17N=new T(function(){return B(unCStr("\u307e\u305a\u306f\u304a\u53cb\u9054\u304b\u3089"));}),_17O=new T(function(){return B(_17I(_11r,_17N));}),_17P=new T(function(){return [0,_17N,_17O];}),_17Q=new T(function(){return B(unCStr("\u4e00\u7d1a\u30d5\u30e9\u30b0\u5efa\u7bc9\u58eb"));}),_17R=new T(function(){return B(_17I(_17o,_17Q));}),_17S=new T(function(){return [0,_17Q,_17R];}),_17T=[0,1000],_17U=new T(function(){return B(unCStr("\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_17V=new T(function(){return B(_17I(_17T,_17U));}),_17W=new T(function(){return [0,_17U,_17V];}),_17X=new T(function(){return B(unCStr("\uff8a\uff72\uff8a\uff9f\uff70\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_17Y=new T(function(){return B(_17I(_17s,_17X));}),_17Z=new T(function(){return [0,_17X,_17Y];}),_180=[0,100000],_181=new T(function(){return B(unCStr("\u5168\u958b\u3089\u3076\u3071\u308f\u30fc"));}),_182=new T(function(){return B(_17I(_180,_181));}),_183=new T(function(){return [0,_181,_182];}),_184=new T(function(){return B(unCStr("\u4f9d\u5b58\u5ea6\u304c"));}),_185=function(_186,_187){return function(_188,_){var _189=E(_188);return E(_189[3])[1]<=E(new T(function(){return [0,B(_zv(_186))];}))[1]?[0,_cH,_189]:B(A(new T(function(){return B(_172(new T(function(){return B(_P(_184,new T(function(){return B(_P(B(_1a(0,_186,_v)),_17g));})));}),_187));}),[_189,_]));};},_18a=new T(function(){return B(unCStr("\u4f9d\u5b58\u6ce8\u610f\u5831"));}),_18b=new T(function(){return B(_185(_17o,_18a));}),_18c=new T(function(){return [0,_18a,_18b];}),_18d=new T(function(){return B(unCStr("\u4f9d\u5b58\u30c9\u30e9\u30c3\u30b0"));}),_18e=new T(function(){return B(_185(_17s,_18d));}),_18f=new T(function(){return [0,_18d,_18e];}),_18g=new T(function(){return B(unCStr("\u500b\u4ee5\u4e0a\u624b\u306b\u5165\u308c\u308b"));}),_18h=new T(function(){return B(unCStr("\u300d\u3092"));}),_18i=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u300c"));}),_18j=new T(function(){return B(_7M(_18k));}),_18l=function(_18m,_18n,_18o){return function(_18p,_){var _18q=E(_18p),_18r=_18q[7];return !B(_va(_18m,_18r))?[0,_cH,_18q]:B(_uU(_18m,_18r))[1]<E(_18n)[1]?[0,_cH,_18q]:B(A(new T(function(){return B(_172(new T(function(){return B(_P(_18i,new T(function(){return B(_P(E(B(_uU(_18m,_18j))[3])[3],new T(function(){return B(_P(_18h,new T(function(){return B(_P(B(_82(0,E(_18n)[1],_v)),_18g));})));})));})));}),_18o));}),[_18q,_]));};},_18s=new T(function(){return B(unCStr("\u304a\u3057\u3083\u3079\u308a\u611b\u3061\u3083\u3093"));}),_18t=new T(function(){return B(_18l(_9R,_zp,_18s));}),_18u=new T(function(){return [0,_18s,_18t];}),_18v=new T(function(){return B(unCStr("\u3042\u3001\u3046\u3093"));}),_18w=[0,200],_18x=new T(function(){return B(_18l(_9R,_18w,_18v));}),_18y=new T(function(){return [0,_18v,_18x];}),_18z=new T(function(){return B(unCStr("\u6587\u901a\u76f8\u624b"));}),_18A=new T(function(){return B(_18l(_9N,_zp,_18z));}),_18B=new T(function(){return [0,_18z,_18A];}),_18C=new T(function(){return B(unCStr("\u55ab\u8336\u5e97\u306e\u30dd\u30a4\u30f3\u30c8\u30ab\u30fc\u30c9"));}),_18D=new T(function(){return B(_18l(_9J,_zp,_18C));}),_18E=new T(function(){return [0,_18C,_18D];}),_18F=new T(function(){return B(unCStr("\u611b\u3068\u3044\u3046\u540d\u306e\u30d7\u30ec\u30bc\u30f3\u30c8"));}),_18G=new T(function(){return B(_18l(_9F,_zn,_18F));}),_18H=new T(function(){return [0,_18F,_18G];}),_18I=new T(function(){return B(unCStr("\u30de\u30a4\u30eb\u304c\u305f\u304f\u3055\u3093"));}),_18J=new T(function(){return B(_18l(_9B,_zn,_18I));}),_18K=new T(function(){return [0,_18I,_18J];}),_18L=new T(function(){return B(unCStr("\u8eca\u30b3\u30ec\u30af\u30bf\u30fc"));}),_18M=new T(function(){return B(_18l(_8U,_zn,_18L));}),_18N=new T(function(){return [0,_18L,_18M];}),_18O=new T(function(){return B(unCStr("\u5225\u8358\u30de\u30cb\u30a2"));}),_18P=new T(function(){return B(_18l(_9w,_zn,_18O));}),_18Q=new T(function(){return [0,_18O,_18P];}),_18R=new T(function(){return B(unCStr("\u5168\u3066\u306e\u901a\u5e38\u30a2\u30a4\u30c6\u30e0\u3092"));}),_18S=function(_18T,_18U){return function(_18V,_){var _18W=new T(function(){return E(E(_18V)[7]);});return !B(_Tc(function(_18X){return !B(_va(_18X,_18W))?false:B(_uU(_18X,_18W))[1]>=E(_18T)[1];},_18Y))?[0,_cH,_18V]:B(A(new T(function(){return B(_172(new T(function(){return B(_P(_18R,new T(function(){return B(_P(B(_82(0,E(_18T)[1],_v)),_18g));})));}),_18U));}),[_18V,_]));};},_18Z=new T(function(){return B(unCStr("\u30b3\u30f3\u30d7\u30ea\u30fc\u30c8"));}),_190=new T(function(){return B(_18S(_9S,_18Z));}),_191=new T(function(){return [0,_18Z,_190];}),_192=new T(function(){return B(unCStr("\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30f3\u30d7\u30ea\u30fc\u30c8"));}),_193=new T(function(){return B(_18S(_zl,_192));}),_194=new T(function(){return [0,_192,_193];}),_195=new T(function(){return B(unCStr("\u9577\u3044\u9577\u3044\u30ed\u30b0"));}),_196=new T(function(){return B(unCStr("\u30ed\u30b0\u306e\u884c\u6570\u304c"));}),_197=new T(function(){return B(_82(0,100,_v));}),_198=new T(function(){return B(_P(_197,_17g));}),_199=new T(function(){return B(_P(_196,_198));}),_19a=new T(function(){return B(_172(_199,_195));}),_19b=function(_19c,_){var _19d=E(_19c);return E(_19d[11])[1]<100?[0,_cH,_19d]:B(A(_19a,[_19d,_]));},_19e=new T(function(){return [0,_195,_19b];}),_19f=new T(function(){return B(unCStr("\u653e\u7f6e\u30d7\u30ec\u30a4"));}),_19g=new T(function(){return B(unCStr("\u901a\u5e38\u30a2\u30a4\u30c6\u30e0\u3092\u8cb7\u3063\u3066\u3044\u306a\u3044\u72b6\u614b\u3067\u611b\u60c5\u304c"));}),_19h=new T(function(){return B(_1a(0,_17w,_v));}),_19i=new T(function(){return B(_P(_19h,_17g));}),_19j=new T(function(){return B(_P(_19g,_19i));}),_19k=new T(function(){return B(_172(_19j,_19f));}),_19l=function(_19m,_19n,_19o,_19p,_19q,_19r,_19s,_19t,_19u,_19v,_19w,_){return _19m<=1000000?[0,_cH,[0,[0,_19m],_19n,_19o,_19p,_19q,_19r,_19s,_19t,_19u,_19v,_19w]]:!B(_Tc(function(_19x){return !B(_va(_19x,_19s))?true:E(B(_uU(_19x,_19s))[1])==0?true:false;},_18Y))?[0,_cH,[0,[0,_19m],_19n,_19o,_19p,_19q,_19r,_19s,_19t,_19u,_19v,_19w]]:B(A(_19k,[[0,[0,_19m],_19n,_19o,_19p,_19q,_19r,_19s,_19t,_19u,_19v,_19w],_]));},_19y=function(_19z,_){var _19A=E(_19z);return new F(function(){return _19l(E(_19A[1])[1],_19A[2],_19A[3],_19A[4],_19A[5],_19A[6],_19A[7],_19A[8],_19A[9],_19A[10],_19A[11],_);});},_19B=new T(function(){return [0,_19f,_19y];}),_19C=new T(function(){return [1,_19B,_v];}),_19D=new T(function(){return [1,_19e,_19C];}),_19E=new T(function(){return [1,_194,_19D];}),_19F=new T(function(){return [1,_191,_19E];}),_19G=new T(function(){return [1,_18Q,_19F];}),_19H=new T(function(){return [1,_18N,_19G];}),_19I=new T(function(){return [1,_18K,_19H];}),_19J=new T(function(){return [1,_18H,_19I];}),_19K=new T(function(){return [1,_18E,_19J];}),_19L=new T(function(){return [1,_18B,_19K];}),_19M=new T(function(){return [1,_18y,_19L];}),_19N=new T(function(){return [1,_18u,_19M];}),_19O=new T(function(){return [1,_18f,_19N];}),_19P=new T(function(){return [1,_18c,_19O];}),_19Q=new T(function(){return [1,_183,_19P];}),_19R=new T(function(){return [1,_17Z,_19Q];}),_19S=new T(function(){return [1,_17W,_19R];}),_19T=new T(function(){return [1,_17S,_19S];}),_19U=new T(function(){return [1,_17P,_19T];}),_19V=new T(function(){return [1,_17G,_19U];}),_19W=new T(function(){return [1,_17C,_19V];}),_19X=new T(function(){return [1,_17y,_19W];}),_19Y=new T(function(){return [1,_17u,_19X];}),_19Z=new T(function(){return [1,_17q,_19Y];}),_1a0=new T(function(){return B(unCStr("stats"));}),_1a1=new T(function(){return B(unCStr("%s/%s"));}),_1a2=new T(function(){return B(unCStr("\u5b9f\u7e3e"));}),_1a3=new T(function(){return B(unCStr("\u4f9d\u5b58\u5ea6\u4fc2\u6570"));}),_1a4=new T(function(){return B(unCStr("\u597d\u611f\u5ea6\u4fc2\u6570"));}),_1a5=new T(function(){return B(unCStr("\u4f9d\u5b58\u5ea6"));}),_1a6=new T(function(){return B(unCStr("\u597d\u611f\u5ea6(\u611b\u60c5/s)"));}),_1a7=new T(function(){return B(unCStr("\u611b\u60c5"));}),_1a8=new T(function(){return B(_82(0,B(_Xu(_19Z,0)),_v));}),_1a9=new T(function(){return B(_1r(_xQ,_1a8));}),_1aa=new T(function(){return [1,_1a9];}),_1ab=new T(function(){return B(_82(0,0,_v));}),_1ac=new T(function(){return B(_1r(_xQ,_1ab));}),_1ad=new T(function(){return B(unCStr("tbody"));}),_1ae=function(_1af,_1ag,_1ah,_){var _1ai=jsCreateElem(toJSStr(E(_1ad))),_1aj=_1ai,_1ak=jsAppendChild(_1aj,E(_1ah)[1]),_1al=[0,_1aj],_1am=B(A(_1af,[_1ag,_1al,_])),_1an=_1am;return _1al;},_1ao=new T(function(){return B(unCStr("thead"));}),_1ap=function(_1aq,_1ar,_1as,_){var _1at=jsCreateElem(toJSStr(E(_1ao))),_1au=_1at,_1av=jsAppendChild(_1au,E(_1as)[1]),_1aw=[0,_1au],_1ax=B(A(_1aq,[_1ar,_1aw,_])),_1ay=_1ax;return _1aw;},_17f=function(_1az,_){var _1aA=E(_1a0),_1aB=jsFind(toJSStr(_1aA)),_1aC=_1aB,_1aD=E(_1aC);if(!_1aD[0]){return new F(function(){return _10P(_1aA);});}else{var _1aE=_1aD[1],_1aF=B(A(_vr,[_dl,_1aE,_wq,_v,_])),_1aG=_1aF,_1aH=B(_1ap(_wC,_13j,_1aE,_)),_1aI=_1aH,_1aJ=B(_1ae(_wC,function(_1aK,_){var _1aL=B(_11U(_1a7,new T(function(){return B(_12D(E(E(_1az)[1])[1]));}),_1aK,_)),_1aM=_1aL,_1aN=B(_11U(_1a6,new T(function(){return B(_12D(E(E(_1az)[2])[1]));}),_1aK,_)),_1aO=_1aN,_1aP=B(_11U(_1a5,new T(function(){return B(_12D(E(E(_1az)[3])[1]));}),_1aK,_)),_1aQ=_1aP,_1aR=B(_11U(_1a4,new T(function(){return B(_12D(E(E(_1az)[10])[1]));}),_1aK,_)),_1aS=_1aR,_1aT=B(_11U(_1a3,new T(function(){return B(_12D(E(E(_1az)[9])[1]));}),_1aK,_)),_1aU=_1aT,_1aV=B(_11U(_1a2,new T(function(){return B(_1r(_xQ,B(_ZF(_1a1,new T(function(){return B(_E0([1,_1aa,[1,[1,new T(function(){var _1aW=E(E(_1az)[6]);if(!_1aW[0]){var _1aX=B(_1r(_xQ,B(_82(0,_1aW[1],_v))));}else{var _1aX=E(_1ac);}var _1aY=_1aX;return _1aY;})],_v]],_v));})))));}),_1aK,_)),_1aZ=_1aV;return _1aK;},_1aE,_)),_1b0=_1aJ,_1b1=E(_1),_1b2=jsFind(toJSStr(_1b1)),_1b3=_1b2,_1b4=E(_1b3);if(!_1b4[0]){return new F(function(){return _10P(_1b1);});}else{var _1b5=_1b4[1],_1b6=B(A(_vr,[_dl,_1b5,_wq,_v,_])),_1b7=_1b6,_1b8=B(_1ap(_wC,_13a,_1b5,_)),_1b9=_1b8,_1ba=B(_1ae(_wC,new T(function(){var _1bb=function(_1bc){var _1bd=E(_1bc);return _1bd[0]==0?function(_1be,_){return _1be;}:function(_1bf,_){var _1bg=E(new T(function(){var _1bh=E(_1bd[1])[1],_1bi=B(_zc(_1bh,new T(function(){return E(E(_1az)[6]);})));if(!_1bi[0]){var _1bj=[0,_1bh,_v];}else{var _1bk=E(_1bi[1]),_1bj=_1bk[0]==0?[0,_1bh,_v]:[0,_1bh,_1bk[1]];}var _1bl=_1bj;return _1bl;})),_1bm=B(_11K(_wC,function(_1bn,_){var _1bo=B(_11z(_vf,_1bg[1],_1bn,_)),_1bp=_1bo,_1bq=B(_11z(_vf,_1bg[2],_1bn,_)),_1br=_1bq;return _1bn;},_1bf,_)),_1bs=_1bm,_1bt=B(A(new T(function(){return B(_1bb(_1bd[2]));}),[_1bf,_])),_1bu=_1bt;return _1bf;};};return B(_1bb(_19Z));}),_1b5,_)),_1bv=_1ba;return [0,_cH,_1az];}}},_1bw=function(_1bx,_){var _1by=B(_nY(_)),_1bz=_1by,_1bA=new T(function(){var _1bB=E(_1bz);return [0,_1bB[1],_1bB[2],_1bB[3],_1bB[4],_1bB[5],new T(function(){return E(E(_1bx)[6]);}),_1bB[7],_1bB[8],new T(function(){var _1bC=E(_1bx),_1bD=E(_1bC[1])[1];return [0,E(_1bC[9])[1]+Math.sqrt(_1bD)/10000*Math.log(_1bD)];}),new T(function(){var _1bE=E(_1bx),_1bF=E(_1bE[3])[1];return [0,E(_1bE[10])[1]+Math.sqrt(_1bF)/100*Math.log(_1bF)];}),_1bB[11]];}),_1bG=B(A(_14o,[_1bA,_])),_1bH=_1bG,_1bI=B(_17d(_1bA,_)),_1bJ=_1bI,_1bK=B(_17f(new T(function(){return E(E(_1bJ)[2]);}),_)),_1bL=_1bK,_1bM=E(_8Z),_1bN=jsFind(toJSStr(_1bM)),_1bO=_1bN,_1bP=E(_1bO);if(!_1bP[0]){return new F(function(){return _10P(_1bM);});}else{var _1bQ=B(A(_xK,[_A8,_1bP[1],_zz,_DY,new T(function(){return E(E(_1bL)[2]);}),_])),_1bR=_1bQ,_1bS=E(_92),_1bT=jsFind(toJSStr(_1bS)),_1bU=_1bT,_1bV=E(_1bU);return _1bV[0]==0?B(_10P(_1bS)):B(A(_xK,[_A8,_1bV[1],_zz,_DY,new T(function(){return E(E(_1bR)[2]);}),_]));}},_1bW=function(_1bX,_1bY,_){return new F(function(){return _1bw(_1bY,_);});},_1bZ=new T(function(){return [0,_11s,_1bW,_11x];}),_1c0=new T(function(){return [0,_9e,_1bZ];}),_1c1=new T(function(){return [1,_1c0,_v];}),_1c2=new T(function(){return B(unCStr("save"));}),_1c3=new T(function(){return B(unCStr("\u30bb\u30fc\u30d6\u3057\u307e\u3057\u305f"));}),_1c4=new T(function(){return B(unCStr("auto"));}),_1c5=new T(function(){return B(_16q(_1c4,_1c3));}),_1c6=function(_1c7,_){var _1c8=new T(function(){var _1c9=E(_1c7);return [0,_1c9[1],_1c9[2],_1c9[3],_1c9[4],_1c9[5],_1c9[6],new T(function(){return B(_6U(_1c2,_nv,_1c9[7]));}),_1c9[8],_1c9[9],_1c9[10],_1c9[11]];}),_1ca=B(A(_14o,[_1c8,_])),_1cb=_1ca,_1cc=B(_17d(_1c8,_)),_1cd=_1cc,_1ce=B(_17f(new T(function(){return E(E(_1cd)[2]);}),_)),_1cf=_1ce;return new F(function(){return A(_1c5,[new T(function(){return E(E(_1cf)[2]);}),_]);});},_1cg=function(_1ch,_1ci,_){return new F(function(){return _1c6(_1ci,_);});},_1cj=new T(function(){return B(unCStr("\u30bb\u30fc\u30d6"));}),_1ck=new T(function(){return B(unCStr("\u30bb\u30fc\u30d6<br>\u30bb\u30fc\u30d6\u3057\u307e\u3059\u3002"));}),_1cl=new T(function(){return B(unCStr("fa-save"));}),_1cm=[0,_1cl,_1ck,_1cj],_1cn=new T(function(){return [0,_DR,_1cg,_1cm];}),_1co=new T(function(){return [0,_1c2,_1cn];}),_1cp=new T(function(){return [1,_1co,_1c1];}),_1cq=new T(function(){return [1,_11q,_1cp];}),_1cr=new T(function(){return [1,_11e,_1cq];}),_1cs=new T(function(){return [1,_DQ,_1cr];}),_1ct=new T(function(){return [1,_Dn,_1cs];}),_1cu=new T(function(){return [1,_CS,_1ct];}),_1cv=new T(function(){return [1,_CF,_1cu];}),_1cw=new T(function(){return [1,_Cs,_1cv];}),_1cx=new T(function(){return [1,_Cg,_1cw];}),_1cy=new T(function(){return [1,_C4,_1cx];}),_1cz=new T(function(){return [1,_BS,_1cy];}),_18k=new T(function(){return [1,_BG,_1cz];}),_1cA=new T(function(){return B(_1r(_Ac,_18k));}),_1cB=function(_1cC){return new F(function(){return _cp(_1cC,_8Z);});},_1cD=function(_1cE,_1cF){var _1cG=E(_1cF);if(!_1cG[0]){return [0];}else{var _1cH=_1cG[1];return !B(A(_1cE,[_1cH]))?[0]:[1,_1cH,new T(function(){return B(_1cD(_1cE,_1cG[2]));})];}},_18Y=new T(function(){return B(_1cD(_1cB,_1cA));}),_1cI=new T(function(){return B(_14f("(function(e,c){e.removeAttribute(c);})"));}),_1cJ=function(_1cK){return function(_1cL,_){var _1cM=B(A(new T(function(){return B(A(_1cI,[E(E(_1cK)[1])]));}),[E(toJSStr(E(_1cL))),_])),_1cN=_1cM;return _cH;};},_1cO=function(_1cP,_1cQ){var _1cR=E(_1cP);if(!_1cR[0]){return [0];}else{var _1cS=E(_1cQ);return _1cS[0]==0?[0]:[1,[0,_1cR[1],_1cS[1]],new T(function(){return B(_1cO(_1cR[2],_1cS[2]));})];}},_17d=function(_1cT,_){var _1cU=new T(function(){return E(E(_1cT)[7]);}),_1cV=function(_1cW){var _1cX=E(_1cW);return _1cX[0]==0?E(_Aa):function(_1cY,_){var _1cZ=B(A(new T(function(){var _1d0=E(_1cX[1]),_1d1=_1d0[1],_1d2=E(_1d0[2])[1],_1d3=new T(function(){return B(unAppCStr("item-",_1d1));}),_1d4=new T(function(){var _1d5=B(_zc(_1d1,_1cU));return _1d5[0]==0?E(_nv):E(_1d5[1]);});return function(_1d6,_){var _1d7=B(A(new T(function(){if(!B(_eS(_cs,_1d1,_18Y))){var _1d8=E(_Aa);}else{var _1d8=function(_1d9,_){var _1da=E(new T(function(){return B(_P(_1d3,_wn));})),_1db=jsFind(toJSStr(_1da)),_1dc=_1db,_1dd=E(_1dc);if(!_1dd[0]){return new F(function(){return _10P(_1da);});}else{var _1de=jsFind(toJSStr(E(new T(function(){return B(_P(_1d3,_wp));})))),_1df=_1de,_1dg=E(_1df);if(!_1dg[0]){return new F(function(){return _ns(_zi,_);});}else{var _1dh=E(_zB),_1di=jsGet(E(_1dg[1])[1],toJSStr(_1dh)),_1dj=_1di,_1dk=new T(function(){return fromJSStr(_1dj);}),_1dl=function(_1dm){return _1dm>1?[1,_1dk,new T(function(){return B(_1dl(_1dm-1|0));})]:E([1,_1dk,_v]);},_1dn=jsSet(E(_1dd[1])[1],toJSStr(_1dh),toJSStr(B((function(_1do,_1dp){while(1){var _1dq=(function(_1dr,_1ds){var _1dt=E(_1ds);if(!_1dt[0]){return E(_1dr);}else{var _1du=E(_1dt[1]);_1do=B(_1r(_xQ,B(_ZF(_zC,new T(function(){return B(_E0([1,[1,new T(function(){var _1dv=E(_1du[2])[1];if(_1dv>0){var _1dw=B(_1r(_xQ,B(_zr(B(_1dl(_1dv))))));}else{var _1dw=E(_zF);}var _1dx=_1dw,_1dy=_1dx;return _1dy;})],[1,[2,_zD,new T(function(){return B(_eb(E(_1du[1])[1]));})],[1,[1,new T(function(){return B(_1r(_xQ,_1dr));})],_v]]],_v));})))));_1dp=_1dt[2];return null;}})(_1do,_1dp);if(_1dq!=null){return _1dq;}}})(_v,new T(function(){return B(_1cO(_zu,new T(function(){return B(_E0(B(_z4(_1d4,_v,_zh,_zq))[2],_v));})));}))))),_1dz=E(new T(function(){return B(_P(_1d3,_wo));})),_1dA=jsFind(toJSStr(_1dz)),_1dB=_1dA,_1dC=E(_1dB);return _1dC[0]==0?B(_10P(_1dz)):B(A(_vr,[_A8,_1dC[1],_1dh,new T(function(){return B(_82(0,E(_1d4)[1],_v));}),_1d9,_]));}}};}return _1d8;}),[_1d6,_])),_1dD=_1d7,_1dE=E(new T(function(){return B(_P(_1d3,_wm));})),_1dF=jsFind(toJSStr(_1dE)),_1dG=_1dF,_1dH=E(_1dG);if(!_1dH[0]){return new F(function(){return _10P(_1dE);});}else{var _1dI=_1dH[1];if(!E(new T(function(){if(!B(_eS(_cs,_1d1,_18Y))){if(!B(_va(_1d1,_1cU))){var _1dJ=B(_zv(B(A(_1d2,[_1d4]))))<=E(E(_1cT)[1])[1];}else{if(B(_uU(_1d1,_1cU))[1]>=1){var _1dK=false;}else{var _1dK=B(_zv(B(A(_1d2,[_1d4]))))<=E(E(_1cT)[1])[1];}var _1dL=_1dK,_1dM=_1dL,_1dJ=_1dM;}var _1dN=_1dJ;}else{var _1dN=B(_zv(B(A(_1d2,[_1d4]))))<=E(E(_1cT)[1])[1];}return _1dN;}))){var _1dO=B(A(_vl,[_A8,_1dI,_zA,_zA,new T(function(){return E(E(_1dD)[2]);}),_])),_1dP=_1dO,_1dQ=B(_P(_1d3,_wl)),_1dR=jsFind(toJSStr(_1dQ)),_1dS=_1dR,_1dT=E(_1dS);if(!_1dT[0]){return new F(function(){return _10P(_1dQ);});}else{var _1dU=jsSet(E(_1dT[1])[1],toJSStr(E(_zB)),toJSStr(B(_12y(new T(function(){return B(_1a(0,B(A(_1d2,[_1d4])),_v));})))));if(!B(_eS(_cs,_1d1,_18Y))){return [0,_cH,new T(function(){return E(E(_1dP)[2]);})];}else{var _1dV=function(_1dW,_){var _1dX=E(_1d3),_1dY=jsFind(toJSStr(_1dX)),_1dZ=_1dY,_1e0=E(_1dZ);if(!_1e0[0]){return new F(function(){return _10P(_1dX);});}else{var _1e1=E(_1e0[1]),_1e2=E(_zz),_1e3=jsGetStyle(_1e1[1],toJSStr(_1e2)),_1e4=_1e3;return !B(_ck(fromJSStr(_1e4),_zy))?B(A(_xK,[_A8,_1e1,_1e2,_zy,_1dW,_])):[0,_cH,_1dW];}};if(!B(_va(_1d1,_1cU))){var _1e5=E(E(_1dP)[2]);return B(_zv(B(A(_1d2,[_nv]))))>3*E(_1e5[8])[1]?[0,_cH,_1e5]:B(_1dV(_1e5,_));}else{return new F(function(){return _1dV(new T(function(){return E(E(_1dP)[2]);}),_);});}}}}else{var _1e6=B(A(_1cJ,[_1dI,_zA,_])),_1e7=_1e6,_1e8=B(_P(_1d3,_wl)),_1e9=jsFind(toJSStr(_1e8)),_1ea=_1e9,_1eb=E(_1ea);if(!_1eb[0]){return new F(function(){return _10P(_1e8);});}else{var _1ec=jsSet(E(_1eb[1])[1],toJSStr(E(_zB)),toJSStr(B(_12y(new T(function(){return B(_1a(0,B(A(_1d2,[_1d4])),_v));})))));if(!B(_eS(_cs,_1d1,_18Y))){return [0,_cH,new T(function(){return E(E(_1dD)[2]);})];}else{var _1ed=function(_1ee,_){var _1ef=E(_1d3),_1eg=jsFind(toJSStr(_1ef)),_1eh=_1eg,_1ei=E(_1eh);if(!_1ei[0]){return new F(function(){return _10P(_1ef);});}else{var _1ej=E(_1ei[1]),_1ek=E(_zz),_1el=jsGetStyle(_1ej[1],toJSStr(_1ek)),_1em=_1el;return !B(_ck(fromJSStr(_1em),_zy))?B(A(_xK,[_A8,_1ej,_1ek,_zy,_1ee,_])):[0,_cH,_1ee];}};if(!B(_va(_1d1,_1cU))){var _1en=E(E(_1dD)[2]);return B(_zv(B(A(_1d2,[_nv]))))>3*E(_1en[8])[1]?[0,_cH,_1en]:B(_1ed(_1en,_));}else{return new F(function(){return _1ed(new T(function(){return E(E(_1dD)[2]);}),_);});}}}}}};}),[_1cY,_])),_1eo=_1cZ;return new F(function(){return A(new T(function(){return B(_1cV(_1cX[2]));}),[new T(function(){return E(E(_1eo)[2]);}),_]);});};};return new F(function(){return A(_1cV,[_18k,_1cT,_]);});},_1ep=function(_1eq,_1er,_1es){var _1et=E(_1eq),_1eu=E(_1es);if(!_1eu[0]){var _1ev=_1eu[2],_1ew=_1eu[3],_1ex=_1eu[4],_1ey=_1eu[5];switch(B(_3Y(_1et,_1ev))){case 0:return new F(function(){return _51(_1ev,_1ew,B(_1ep(_1et,_1er,_1ex)),_1ey);});break;case 1:return [0,_1eu[1],E(_1et),new T(function(){return B(_Kr(_1er,_1ew));}),E(_1ex),E(_1ey)];default:return new F(function(){return _4a(_1ev,_1ew,_1ex,B(_1ep(_1et,_1er,_1ey)));});}}else{return [0,1,E(_1et),_1er,E(_45),E(_45)];}},_1ez=new T(function(){return "window.confirm(\"\u3053\u306e\u64cd\u4f5c\u306f\u53d6\u308a\u6d88\u305b\u307e\u305b\u3093\u3002\u3088\u308d\u3057\u3044\u3067\u3059\u304b\uff1f\");";}),_1eA=new T(function(){return [0,"click"];}),_1eB=[0,10],_1eC=[1,_1eB,_v],_1eD=function(_1eE,_1eF,_){var _1eG=jsWriteHandle(E(_1eE)[1],toJSStr(E(_1eF)));return _cH;},_1eH=function(_1eI,_1eJ,_){var _1eK=E(_1eI),_1eL=jsWriteHandle(_1eK[1],toJSStr(E(_1eJ)));return new F(function(){return _1eD(_1eK,_1eC,_);});},_1eM=new T(function(){return B(unCStr("\u300d\u3092\u8cfc\u5165\u3057\u307e\u3057\u305f"));}),_1eN=new T(function(){return B(unCStr("resetAll"));}),_1eO=[1,_1eN,_v],_1eP=[1,_9e,_1eO],_1eQ=[0,12300],_1eR=function(_){var _=0,_1eS=jsMkStdout(),_1eT=_1eS;return [0,_1eT];},_1eU=new T(function(){return B(_oa(_1eR));}),_1eV=function(_1eW,_1eX,_){var _1eY=E(_1eX);if(!_1eY[0]){return _cH;}else{var _1eZ=_1eY[1],_1f0=function(_){var _1f1=E(_1eW)[1],_1f2=rMV(_1f1),_1f3=_1f2,_1f4=B(_uU(_1eZ,_18j)),_1f5=new T(function(){var _1f6=E(_1f3);return [0,_1f6[1],_1f6[2],_1f6[3],_1f6[4],_1f6[5],_1f6[6],new T(function(){return B(_1ep(_1eZ,_9S,_1f6[7]));}),_1f6[8],_1f6[9],_1f6[10],_1f6[11]];}),_1f7=new T(function(){return B(_uU(_1eZ,E(_1f5)[7]));}),_1f8=B(A(_1f4[2],[_1f7,new T(function(){var _1f9=E(_1f5);return [0,new T(function(){return [0,E(_1f9[1])[1]-B(_zv(B(A(_1f4[1],[new T(function(){return [0,E(_1f7)[1]-1|0];})]))))];}),_1f9[2],_1f9[3],_1f9[4],_1f9[5],_1f9[6],_1f9[7],_1f9[8],_1f9[9],_1f9[10],_1f9[11]];}),_])),_1fa=_1f8,_1fb=B(A(_15V,[_14p,[1,_1eQ,new T(function(){return B(_P(E(_1f4[3])[3],_1eM));})],new T(function(){return E(E(_1fa)[2]);}),_])),_1fc=_1fb,_1fd=new T(function(){return E(E(_1fc)[2]);}),_1fe=B(A(_14o,[_1fd,_])),_1ff=_1fe,_1fg=B(_17d(_1fd,_)),_1fh=_1fg,_1fi=B(_17f(new T(function(){return E(E(_1fh)[2]);}),_)),_1fj=_1fi,_=wMV(_1f1,new T(function(){return E(E(_1fj)[2]);})),_1fk=rMV(_1f1),_1fl=_1fk,_1fm=E(_1fl),_1fn=jsLog(toJSStr(B(A(_uy,[0,_1fm[1],_1fm[2],_1fm[3],_1fm[4],_1fm[5],_1fm[6],_1fm[7],_1fm[8],_1fm[9],_1fm[10],_1fm[11],_v]))));return _cH;},_1fo=jsFind(toJSStr(B(unAppCStr("item-",new T(function(){return B(_P(_1eZ,_wm));}))))),_1fp=_1fo,_1fq=E(_1fp);if(!_1fq[0]){return _cH;}else{var _1fr=E(_1eA)[1],_1fs=jsSetCB(E(_1fq[1])[1],_1fr,function(_1ft,_1fu,_){if(!E(new T(function(){return B(_eS(_cs,_1eZ,_1eP));}))){return new F(function(){return _1f0(_);});}else{var _1fv=jsEval(_1ez),_1fw=_1fv,_1fx=B(_1eH(_1eU,fromJSStr(_1fw),_)),_1fy=_1fx;return new F(function(){return _1f0(_);});}}),_1fz=_1fs;return new F(function(){return (function(_1fA,_1fB,_){while(1){var _1fC=(function(_1fD,_1fE,_){var _1fF=E(_1fE);if(!_1fF[0]){return _cH;}else{var _1fG=_1fF[1],_1fH=function(_){var _1fI=E(_1fD)[1],_1fJ=rMV(_1fI),_1fK=_1fJ,_1fL=B(_uU(_1fG,_18j)),_1fM=new T(function(){var _1fN=E(_1fK);return [0,_1fN[1],_1fN[2],_1fN[3],_1fN[4],_1fN[5],_1fN[6],new T(function(){return B(_1ep(_1fG,_9S,_1fN[7]));}),_1fN[8],_1fN[9],_1fN[10],_1fN[11]];}),_1fO=new T(function(){return B(_uU(_1fG,E(_1fM)[7]));}),_1fP=B(A(_1fL[2],[_1fO,new T(function(){var _1fQ=E(_1fM);return [0,new T(function(){return [0,E(_1fQ[1])[1]-B(_zv(B(A(_1fL[1],[new T(function(){return [0,E(_1fO)[1]-1|0];})]))))];}),_1fQ[2],_1fQ[3],_1fQ[4],_1fQ[5],_1fQ[6],_1fQ[7],_1fQ[8],_1fQ[9],_1fQ[10],_1fQ[11]];}),_])),_1fR=_1fP,_1fS=B(A(_15V,[_14p,[1,_1eQ,new T(function(){return B(_P(E(_1fL[3])[3],_1eM));})],new T(function(){return E(E(_1fR)[2]);}),_])),_1fT=_1fS,_1fU=new T(function(){return E(E(_1fT)[2]);}),_1fV=B(A(_14o,[_1fU,_])),_1fW=_1fV,_1fX=B(_17d(_1fU,_)),_1fY=_1fX,_1fZ=B(_17f(new T(function(){return E(E(_1fY)[2]);}),_)),_1g0=_1fZ,_=wMV(_1fI,new T(function(){return E(E(_1g0)[2]);})),_1g1=rMV(_1fI),_1g2=_1g1,_1g3=E(_1g2),_1g4=jsLog(toJSStr(B(A(_uy,[0,_1g3[1],_1g3[2],_1g3[3],_1g3[4],_1g3[5],_1g3[6],_1g3[7],_1g3[8],_1g3[9],_1g3[10],_1g3[11],_v]))));return _cH;},_1g5=jsFind(toJSStr(B(unAppCStr("item-",new T(function(){return B(_P(_1fG,_wm));}))))),_1g6=_1g5,_1g7=E(_1g6);if(!_1g7[0]){return _cH;}else{var _1g8=jsSetCB(E(_1g7[1])[1],_1fr,function(_1g9,_1ga,_){if(!E(new T(function(){return B(_eS(_cs,_1fG,_1eP));}))){return new F(function(){return _1fH(_);});}else{var _1gb=jsEval(_1ez),_1gc=_1gb,_1gd=B(_1eH(_1eU,fromJSStr(_1gc),_)),_1ge=_1gd;return new F(function(){return _1fH(_);});}}),_1gf=_1g8,_1gg=_1fD;_1fB=_1fF[2];_1fA=_1gg;return null;}}})(_1fA,_1fB,_);if(_1fC!=null){return _1fC;}}})(_1eW,_1eY[2],_);});}}},_1gh=function(_1gi){var _1gj=E(_1gi);if(!_1gj[0]){return [0,_v,_v];}else{var _1gk=E(_1gj[1]),_1gl=new T(function(){var _1gm=B(_1gh(_1gj[2]));return [0,_1gm[1],_1gm[2]];});return !B(_eS(_cs,_1gk[1],_18Y))?[0,new T(function(){return E(E(_1gl)[1]);}),[1,_1gk,new T(function(){return E(E(_1gl)[2]);})]]:[0,[1,_1gk,new T(function(){return E(E(_1gl)[1]);})],new T(function(){return E(E(_1gl)[2]);})];}},_1gn=new T(function(){var _1go=B(_1gh(_18k));return [0,_1go[1],_1go[2]];}),_1gp=new T(function(){return E(E(_1gn)[1]);}),_1gq=new T(function(){return B(unCStr("keydown"));}),_1gr=new T(function(){return B(unCStr("mousemove"));}),_1gs=new T(function(){return B(unCStr("blur"));}),_1gt=new T(function(){return B(unCStr("focus"));}),_1gu=new T(function(){return B(unCStr("change"));}),_1gv=new T(function(){return B(unCStr("unload"));}),_1gw=new T(function(){return B(unCStr("load"));}),_1gx=new T(function(){return B(unCStr("keyup"));}),_1gy=new T(function(){return B(unCStr("keypress"));}),_1gz=new T(function(){return B(unCStr("mouseup"));}),_1gA=new T(function(){return B(unCStr("mousedown"));}),_1gB=new T(function(){return B(unCStr("dblclick"));}),_1gC=new T(function(){return B(unCStr("click"));}),_1gD=new T(function(){return B(unCStr("mouseout"));}),_1gE=new T(function(){return B(unCStr("mouseover"));}),_1gF=function(_1gG){switch(E(_1gG)[0]){case 0:return E(_1gw);case 1:return E(_1gv);case 2:return E(_1gu);case 3:return E(_1gt);case 4:return E(_1gs);case 5:return E(_1gr);case 6:return E(_1gE);case 7:return E(_1gD);case 8:return E(_1gC);case 9:return E(_1gB);case 10:return E(_1gA);case 11:return E(_1gz);case 12:return E(_1gy);case 13:return E(_1gx);default:return E(_1gq);}},_1gH=new T(function(){return B(unCStr("true"));}),_1gI=new T(function(){return [0,"keydown"];}),_1gJ=new T(function(){return [0,"mousemove"];}),_1gK=new T(function(){return [0,"blur"];}),_1gL=new T(function(){return [0,"focus"];}),_1gM=new T(function(){return [0,"change"];}),_1gN=new T(function(){return [0,"unload"];}),_1gO=new T(function(){return [0,"load"];}),_1gP=new T(function(){return [0,"keyup"];}),_1gQ=new T(function(){return [0,"keypress"];}),_1gR=new T(function(){return [0,"mouseup"];}),_1gS=new T(function(){return [0,"mousedown"];}),_1gT=new T(function(){return [0,"dblclick"];}),_1gU=new T(function(){return [0,"mouseout"];}),_1gV=new T(function(){return [0,"mouseover"];}),_1gW=function(_1gX){switch(E(_1gX)[0]){case 0:return E(_1gO);case 1:return E(_1gN);case 2:return E(_1gM);case 3:return E(_1gL);case 4:return E(_1gK);case 5:return E(_1gJ);case 6:return E(_1gV);case 7:return E(_1gU);case 8:return E(_1eA);case 9:return E(_1gT);case 10:return E(_1gS);case 11:return E(_1gR);case 12:return E(_1gQ);case 13:return E(_1gP);default:return E(_1gI);}},_1gY=function(_1gZ,_1h0,_1h1,_1h2,_){var _1h3=B(A(_1gZ,[_1h2,_])),_1h4=_1h3,_1h5=E(_1h4),_1h6=_1h5[1],_1h7=B(_1gF(_1h0)),_1h8=jsGetAttr(_1h6,toJSStr(_1h7)),_1h9=_1h8;if(!B(_c8(fromJSStr(_1h9),_1gH))){var _1ha=E(_1h1),_1hb=jsSetCB(_1h6,B(_1gW(_1h0))[1],_1h1),_1hc=_1hb,_1hd=B(A(_vl,[_dl,_1h5,_1h7,_1gH,_])),_1he=_1hd;return _1h5;}else{return _1h5;}},_1hf=new T(function(){return E(E(_1gn)[2]);}),_1hg=function(_1hh){return _1hh>0;},_1hi=new T(function(){return B(_14f("(function(x) {return x === null;})"));}),_1hj=new T(function(){return B(unCStr("No such value"));}),_1hk=[0,_1hj],_1hl=new T(function(){return B(unCStr("Invalid JSON!"));}),_1hm=[0,_1hl],_1hn=new T(function(){return [0,"(function(k) {return localStorage.getItem(k);})"];}),_1ho=function(_1hp,_1hq,_){var _1hr=B(A(_14f,[E(_1hn)[1],E(toJSStr(E(_1hq))),_])),_1hs=_1hr;return new T(function(){if(!B(_oa(function(_){var _=0,_1ht=B(A(_1hi,[E(_1hs),_])),_1hu=_1ht;return new T(function(){return B(_1hg(_1hu));});}))){var _1hv=String(_1hs),_1hw=_1hv,_1hx=jsParseJSON(_1hw),_1hy=_1hx,_1hz=E(_1hy),_1hA=_1hz[0]==0?E(_1hm):B(A(_2M,[_1hp,_1hz[1]]));}else{var _1hA=E(_1hk);}return _1hA;});},_1hB=[1,_se,_v],_1hC=function(_1hD,_){var _1hE=jsEval(toJSStr(B(unAppCStr("document.title = ",[1,_se,new T(function(){return B(_sg(B(_12D(E(E(_1hD)[1])[1])),_1hB));})])))),_1hF=_1hE;return [0,_cH,_1hD];},_1hG=function(_1hH,_1hI,_1hJ,_1hK,_1hL){var _1hM=E(_1hL);if(!_1hM[0]){var _1hN=new T(function(){var _1hO=B(_1hG(_1hM[1],_1hM[2],_1hM[3],_1hM[4],_1hM[5]));return [0,_1hO[1],_1hO[2]];});return [0,new T(function(){return E(E(_1hN)[1]);}),new T(function(){return B(_51(_1hI,_1hJ,_1hK,E(_1hN)[2]));})];}else{return [0,[0,_1hI,_1hJ],_1hK];}},_1hP=function(_1hQ,_1hR,_1hS,_1hT,_1hU){var _1hV=E(_1hT);if(!_1hV[0]){var _1hW=new T(function(){var _1hX=B(_1hP(_1hV[1],_1hV[2],_1hV[3],_1hV[4],_1hV[5]));return [0,_1hX[1],_1hX[2]];});return [0,new T(function(){return E(E(_1hW)[1]);}),new T(function(){return B(_4a(_1hR,_1hS,E(_1hW)[2],_1hU));})];}else{return [0,[0,_1hR,_1hS],_1hU];}},_1hY=function(_1hZ,_1i0){var _1i1=E(_1hZ);if(!_1i1[0]){var _1i2=_1i1[1],_1i3=E(_1i0);if(!_1i3[0]){var _1i4=_1i3[1];if(_1i2<=_1i4){var _1i5=B(_1hP(_1i4,_1i3[2],_1i3[3],_1i3[4],_1i3[5])),_1i6=E(_1i5[1]);return new F(function(){return _51(_1i6[1],_1i6[2],_1i1,_1i5[2]);});}else{var _1i7=B(_1hG(_1i2,_1i1[2],_1i1[3],_1i1[4],_1i1[5])),_1i8=E(_1i7[1]);return new F(function(){return _4a(_1i8[1],_1i8[2],_1i7[2],_1i3);});}}else{return E(_1i1);}}else{return E(_1i0);}},_1i9=function(_1ia,_1ib,_1ic,_1id,_1ie,_1if){var _1ig=E(_1ia);if(!_1ig[0]){var _1ih=_1ig[1],_1ii=_1ig[2],_1ij=_1ig[3],_1ik=_1ig[4],_1il=_1ig[5];if((imul(3,_1ih)|0)>=_1ib){if((imul(3,_1ib)|0)>=_1ih){return new F(function(){return _1hY(_1ig,[0,_1ib,E(_1ic),_1id,E(_1ie),E(_1if)]);});}else{return new F(function(){return _4a(_1ii,_1ij,_1ik,B(_1i9(_1il,_1ib,_1ic,_1id,_1ie,_1if)));});}}else{return new F(function(){return _51(_1ic,_1id,B(_1im(_1ih,_1ii,_1ij,_1ik,_1il,_1ie)),_1if);});}}else{return [0,_1ib,E(_1ic),_1id,E(_1ie),E(_1if)];}},_1im=function(_1in,_1io,_1ip,_1iq,_1ir,_1is){var _1it=E(_1is);if(!_1it[0]){var _1iu=_1it[1],_1iv=_1it[2],_1iw=_1it[3],_1ix=_1it[4],_1iy=_1it[5];if((imul(3,_1in)|0)>=_1iu){if((imul(3,_1iu)|0)>=_1in){return new F(function(){return _1hY([0,_1in,E(_1io),_1ip,E(_1iq),E(_1ir)],_1it);});}else{return new F(function(){return _4a(_1io,_1ip,_1iq,B(_1i9(_1ir,_1iu,_1iv,_1iw,_1ix,_1iy)));});}}else{return new F(function(){return _51(_1iv,_1iw,B(_1im(_1in,_1io,_1ip,_1iq,_1ir,_1ix)),_1iy);});}}else{return [0,_1in,E(_1io),_1ip,E(_1iq),E(_1ir)];}},_1iz=function(_1iA,_1iB){var _1iC=E(_1iA);if(!_1iC[0]){var _1iD=_1iC[1],_1iE=_1iC[2],_1iF=_1iC[3],_1iG=_1iC[4],_1iH=_1iC[5],_1iI=E(_1iB);if(!_1iI[0]){var _1iJ=_1iI[1],_1iK=_1iI[2],_1iL=_1iI[3],_1iM=_1iI[4],_1iN=_1iI[5];if((imul(3,_1iD)|0)>=_1iJ){if((imul(3,_1iJ)|0)>=_1iD){return new F(function(){return _1hY(_1iC,_1iI);});}else{return new F(function(){return _4a(_1iE,_1iF,_1iG,B(_1i9(_1iH,_1iJ,_1iK,_1iL,_1iM,_1iN)));});}}else{return new F(function(){return _51(_1iK,_1iL,B(_1im(_1iD,_1iE,_1iF,_1iG,_1iH,_1iM)),_1iN);});}}else{return E(_1iC);}}else{return E(_1iB);}},_1iO=function(_1iP,_1iQ){var _1iR=E(_1iQ);if(!_1iR[0]){var _1iS=_1iR[2],_1iT=_1iR[3],_1iU=_1iR[4],_1iV=_1iR[5];if(!B(A(_1iP,[_1iS,_1iT]))){return new F(function(){return _1iz(B(_1iO(_1iP,_1iU)),B(_1iO(_1iP,_1iV)));});}else{return new F(function(){return _6l(_1iS,_1iT,B(_1iO(_1iP,_1iU)),B(_1iO(_1iP,_1iV)));});}}else{return [1];}},_1iW=[1,_92,_v],_1iX=[1,_8Z,_1iW],_1iY=function(_1iZ,_1j0){return new F(function(){return _eS(_cs,_1iZ,_1iX);});},_1j1=new T(function(){return B(_1iO(_1iY,_18j));}),_1j2=function(_1j3,_){var _1j4=B(A(_14o,[_1j3,_])),_1j5=_1j4,_1j6=B(_17d(_1j3,_)),_1j7=_1j6,_1j8=B(_17f(new T(function(){return E(E(_1j7)[2]);}),_)),_1j9=_1j8;return new F(function(){return A(_1c5,[new T(function(){return E(E(_1j9)[2]);}),_]);});},_1ja=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:320:7-12"));}),_1jb=function(_,_1jc){var _1jd=jsFind(toJSStr(E(_7))),_1je=_1jd,_1jf=E(_1je);if(!_1jf[0]){return new F(function(){return _ns(_1ja,_);});}else{var _1jg=toJSStr(E(_zB)),_1jh=E(E(_1jc)[2]),_1ji=jsSet(E(_1jf[1])[1],_1jg,toJSStr(B(_12D(E(_1jh[2])[1])))),_1jj=jsFind(toJSStr(E(_9))),_1jk=_1jj,_1jl=E(_1jk);if(!_1jl[0]){return new F(function(){return _ns(_1ja,_);});}else{var _1jm=jsSet(E(_1jl[1])[1],_1jg,toJSStr(B(_12D(E(_1jh[1])[1])))),_1jn=jsFind(toJSStr(E(_5))),_1jo=_1jn,_1jp=E(_1jo);if(!_1jp[0]){return new F(function(){return _ns(_1ja,_);});}else{var _1jq=jsSet(E(_1jp[1])[1],_1jg,toJSStr(B(_12D(E(_1jh[3])[1]))));return [0,_cH,_1jh];}}}},_1jr=function(_1js){return E(E(_1js)[2]);},_1jt=function(_1ju,_1jv,_1jw){while(1){var _1jx=E(_1jw);if(!_1jx[0]){return true;}else{if(!B(A(_1jr,[_1ju,_1jv,_1jx[1]]))){return false;}else{_1jw=_1jx[2];continue;}}}},_1jy=function(_,_1jz){var _1jA=new T(function(){return E(E(_1jz)[2]);}),_1jB=function(_1jC){var _1jD=E(_1jC);return _1jD[0]==0?E(_Aa):function(_1jE,_){var _1jF=B(A(new T(function(){var _1jG=E(_1jD[1]),_1jH=_1jG[1],_1jI=new T(function(){var _1jJ=B(_zc(_1jH,new T(function(){return E(E(_1jA)[7]);})));return _1jJ[0]==0?E(_nv):E(_1jJ[1]);});return function(_1jK,_){if(!E(new T(function(){if(!B(_1jt(_cs,_1jH,_18Y))){var _1jL=true;}else{var _1jL=E(E(_1jI)[1])==0?true:false;}return _1jL;}))){return [0,_cH,_1jK];}else{var _1jM=E(_1jK);if(E(new T(function(){return [0,B(_zv(B(A(E(_1jG[2])[1],[_1jI]))))];}))[1]>E(_1jM[1])[1]){return [0,_cH,_1jM];}else{var _1jN=E(new T(function(){return B(unAppCStr("item-",new T(function(){return B(_P(_1jH,_wm));})));})),_1jO=jsFind(toJSStr(_1jN)),_1jP=_1jO,_1jQ=E(_1jP);if(!_1jQ[0]){return new F(function(){return _10P(_1jN);});}else{var _1jR=B(A(_1cJ,[_1jQ[1],_zA,_])),_1jS=_1jR;return [0,_1jS,_1jM];}}}};}),[_1jE,_])),_1jT=_1jF;return new F(function(){return A(new T(function(){return B(_1jB(_1jD[2]));}),[new T(function(){return E(E(_1jT)[2]);}),_]);});};};return new F(function(){return A(_1jB,[_18k,_1jA,_]);});},_1jU=function(_1jV,_1jW,_){while(1){var _1jX=(function(_1jY,_1jZ,_){var _1k0=E(_1jY);if(!_1k0[0]){return [0,_cH,_1jZ];}else{var _1k1=_1k0[2],_1k2=E(_1k0[1]),_1k3=_1k2[1],_1k4=_1k2[2],_1k5=E(_1jZ),_1k6=_1k5[6];if(!B(_va(_1k3,_1k6))){var _1k7=B(A(_1k4,[_1k5,_])),_1k8=_1k7;_1jV=_1k1;_1jW=new T(function(){return E(E(_1k8)[2]);});return null;}else{if(!B(_uU(_1k3,_1k6))[0]){var _1k9=B(A(_1k4,[_1k5,_])),_1ka=_1k9;_1jV=_1k1;_1jW=new T(function(){return E(E(_1ka)[2]);});return null;}else{return new F(function(){return _1kb(_1k1,_1k5[1],_1k5[2],_1k5[3],_1k5[4],_1k5[5],_1k6,_1k5[7],_1k5[8],_1k5[9],_1k5[10],_1k5[11],_);});}}}})(_1jV,_1jW,_);if(_1jX!=null){return _1jX;}}},_1kb=function(_1kc,_1kd,_1ke,_1kf,_1kg,_1kh,_1ki,_1kj,_1kk,_1kl,_1km,_1kn,_){while(1){var _1ko=(function(_1kp,_1kq,_1kr,_1ks,_1kt,_1ku,_1kv,_1kw,_1kx,_1ky,_1kz,_1kA,_){var _1kB=E(_1kp);if(!_1kB[0]){return [0,_cH,[0,_1kq,_1kr,_1ks,_1kt,_1ku,_1kv,_1kw,_1kx,_1ky,_1kz,_1kA]];}else{var _1kC=_1kB[2],_1kD=E(_1kB[1]),_1kE=_1kD[1],_1kF=_1kD[2];if(!B(_va(_1kE,_1kv))){var _1kG=B(A(_1kF,[[0,_1kq,_1kr,_1ks,_1kt,_1ku,_1kv,_1kw,_1kx,_1ky,_1kz,_1kA],_])),_1kH=_1kG;return new F(function(){return _1jU(_1kC,new T(function(){return E(E(_1kH)[2]);}),_);});}else{if(!B(_uU(_1kE,_1kv))[0]){var _1kI=B(A(_1kF,[[0,_1kq,_1kr,_1ks,_1kt,_1ku,_1kv,_1kw,_1kx,_1ky,_1kz,_1kA],_])),_1kJ=_1kI;return new F(function(){return _1jU(_1kC,new T(function(){return E(E(_1kJ)[2]);}),_);});}else{_1kc=_1kC;var _1kK=_1kq,_1kL=_1kr,_1kM=_1ks,_1kN=_1kt,_1kO=_1ku,_1kP=_1kv,_1kQ=_1kw,_1kR=_1kx,_1kS=_1ky,_1kT=_1kz,_1kU=_1kA;_1kd=_1kK;_1ke=_1kL;_1kf=_1kM;_1kg=_1kN;_1kh=_1kO;_1ki=_1kP;_1kj=_1kQ;_1kk=_1kR;_1kl=_1kS;_1km=_1kT;_1kn=_1kU;return null;}}}})(_1kc,_1kd,_1ke,_1kf,_1kg,_1kh,_1ki,_1kj,_1kk,_1kl,_1km,_1kn,_);if(_1ko!=null){return _1ko;}}},_1kV=new T(function(){return B(unCStr("\u304a\u304b\u3048\u308a\u306a\u3055\u3044\uff01<br>(\u4f9d\u5b58\u5ea6\u30dc\u30fc\u30ca\u30b9 +"));}),_1kW=[0,41],_1kX=[1,_1kW,_v],_1kY=new T(function(){return B(unCStr("\u611b\u3061\u3083\u3093"));}),_1kZ=function(_1l0){return new F(function(){return err(B(unAppCStr("docFocused: ",[1,_se,new T(function(){return B(_sg(_1l0,_1hB));})])));});},_1l1=new T(function(){return B(unCStr("false"));}),_1l2=new T(function(){return B(unCStr("document.hasFocus()"));}),_1l3=function(_1l4,_1l5){while(1){var _1l6=E(_1l4);if(!_1l6[0]){var _1l7=_1l6[1],_1l8=E(_1l5);if(!_1l8[0]){var _1l9=_1l8[1],_1la=subC(_1l7,_1l9);if(!E(_1la[2])){return [0,_1la[1]];}else{_1l4=[1,I_fromInt(_1l7)];_1l5=[1,I_fromInt(_1l9)];continue;}}else{_1l4=[1,I_fromInt(_1l7)];_1l5=_1l8;continue;}}else{var _1lb=E(_1l5);if(!_1lb[0]){_1l4=_1l6;_1l5=[1,I_fromInt(_1lb[1])];continue;}else{return [1,I_sub(_1l6[1],_1lb[1])];}}}},_1lc=function(_1ld,_){var _1le=E(_1ld),_1lf=_1le[1],_1lg=_1le[2],_1lh=_1le[3],_1li=_1le[4],_1lj=_1le[5],_1lk=_1le[6],_1ll=_1le[7],_1lm=_1le[8],_1ln=_1le[9],_1lo=_1le[10],_1lp=_1le[11];if(!B(_va(_8Z,_1ll))){return new F(function(){return _1jb(_,[0,_cH,_1le]);});}else{var _1lq=jsEval(toJSStr(E(_1l2))),_1lr=_1lq,_1ls=B(_m8(_)),_1lt=_1ls,_1lu=new T(function(){var _1lv=fromJSStr(_1lr);return !B(_c8(_1lv,_1l1))?!B(_c8(_1lv,_14y))?B(_1kZ(_1lv)):true:false;}),_1lw=function(_,_1lx,_1ly,_1lz,_1lA,_1lB,_1lC,_1lD,_1lE,_1lF,_1lG,_1lH,_1lI){var _1lJ=B(_1kb(_19Z,_1ly,_1lz,_1lA,_1lB,_1lu,_1lD,_1lE,_1lF,_1lG,_1lH,_1lI,_)),_1lK=_1lJ,_1lL=E(E(_1lK)[2]),_1lM=E(_1lL[1]);return _1lM[1]<=E(_1lL[8])[1]?B(_1jy(_,[0,_cH,_1lL])):B(_1jy(_,[0,_cH,[0,_1lM,_1lL[2],_1lL[3],_1lL[4],_1lL[5],_1lL[6],_1lL[7],_1lM,_1lL[9],_1lL[10],_1lL[11]]]));};if(!E(_1lu)){var _1lN=E(_1lh)[1],_1lO=new T(function(){return [0,1.0e-2*E(_1lo)[1]];});if(_1lN<=0){var _1lP=B(_1lw(_,_cH,new T(function(){return [0,E(_1lf)[1]+E(_1lg)[1]/30];}),_1lg,new T(function(){var _1lQ=_1lN-E(_1lO)[1];return _1lQ>0?[0,_1lQ]:E(_nu);}),_1li,_1lj,_1lk,_1ll,_1lm,_1ln,_1lo,_1lp)),_1lR=_1lP;return new F(function(){return _1jb(_,_1lR);});}else{var _1lS=B(_1lw(_,_cH,new T(function(){return [0,E(_1lf)[1]+E(_1lg)[1]/30];}),new T(function(){return [0,E(_1lg)[1]+E(_1lO)[1]];}),new T(function(){var _1lT=_1lN-E(_1lO)[1];return _1lT>0?[0,_1lT]:E(_nu);}),_1li,_1lj,_1lk,_1ll,_1lm,_1ln,_1lo,_1lp)),_1lU=_1lS;return new F(function(){return _1jb(_,_1lU);});}}else{var _1lV=new T(function(){return [0,B(_zv(B(_1l3(_1lt,_1li))))];});if(!E(_1lj)){var _1lW=new T(function(){return [0,E(_1lV)[1]/1000/50*E(_1ln)[1]];}),_1lX=B(A(_16q,[_1kY,new T(function(){return B(_P(_1kV,new T(function(){return B(_P(B(_12D(E(_1lW)[1])),_1kX));})));}),[0,new T(function(){return [0,E(_1lf)[1]+E(_1lg)[1]/30];}),_1lg,new T(function(){return [0,E(_1lh)[1]+E(_1lW)[1]];}),_1li,_s,_1lk,_1ll,_1lm,_1ln,_1lo,_1lp],_])),_1lY=_1lX,_1lZ=B(_1jU(_19Z,new T(function(){var _1m0=E(E(_1lY)[2]);return [0,_1m0[1],new T(function(){return [0,E(_1m0[2])[1]+E(_1lV)[1]/1000/100*E(_1lo)[1]];}),new T(function(){return [0,E(_1m0[3])[1]+E(_1lV)[1]/1000/1000*E(_1ln)[1]];}),_1lt,_o,_1m0[6],_1m0[7],_1m0[8],_1m0[9],_1m0[10],_1m0[11]];}),_)),_1m1=_1lZ,_1m2=E(E(_1m1)[2]),_1m3=E(_1m2[1]);if(_1m3[1]<=E(_1m2[8])[1]){var _1m4=B(_1jy(_,[0,_cH,_1m2])),_1m5=_1m4;return new F(function(){return _1jb(_,_1m5);});}else{var _1m6=B(_1jy(_,[0,_cH,[0,_1m3,_1m2[2],_1m2[3],_1m2[4],_1m2[5],_1m2[6],_1m2[7],_1m3,_1m2[9],_1m2[10],_1m2[11]]])),_1m7=_1m6;return new F(function(){return _1jb(_,_1m7);});}}else{var _1m8=B(_1lw(_,_cH,new T(function(){return [0,E(_1lf)[1]+E(_1lg)[1]/30];}),new T(function(){return [0,E(_1lg)[1]+E(_1lV)[1]/1000/100*E(_1lo)[1]];}),new T(function(){return [0,E(_1lh)[1]+E(_1lV)[1]/1000/1000*E(_1ln)[1]];}),_1lt,_o,_1lk,_1ll,_1lm,_1ln,_1lo,_1lp)),_1m9=_1m8;return new F(function(){return _1jb(_,_1m9);});}}}},_1ma=function(_){return _cH;},_1mb=new T(function(){return B(unCStr("item-itemshop"));}),_1mc=new T(function(){return B(unCStr("item-monitor"));}),_1md=function(_){var _1me=B(_nY(_)),_1mf=_1me,_1mg=B(_1ho(_qt,_13k,_)),_1mh=_1mg,_1mi=nMV(new T(function(){var _1mj=E(_1mh);return _1mj[0]==0?E(_1mf):E(_1mj[1]);})),_1mk=_1mi,_1ml=B(unCStr("list-group")),_1mm=jsFind(toJSStr(_1ml)),_1mn=_1mm,_1mo=E(_1mn);if(!_1mo[0]){return new F(function(){return _10P(_1ml);});}else{var _1mp=B((function(_1mq,_){while(1){var _1mr=E(_1mq);if(!_1mr[0]){return _cH;}else{var _1ms=E(_1mr[1]),_1mt=E(E(_1ms[2])[3]),_1mu=B(A(_wE,[_1ms[1],_1mt[1],_1mt[2],_1mt[3],_1mo[1],_])),_1mv=_1mu;_1mq=_1mr[2];continue;}}})(_1gp,_)),_1mw=_1mp,_1mx=B(unCStr("list-sp-group")),_1my=jsFind(toJSStr(_1mx)),_1mz=_1my,_1mA=E(_1mz);if(!_1mA[0]){return new F(function(){return _10P(_1mx);});}else{var _1mB=B((function(_1mC,_){while(1){var _1mD=E(_1mC);if(!_1mD[0]){return _cH;}else{var _1mE=E(_1mD[1]),_1mF=E(E(_1mE[2])[3]),_1mG=B(A(_wE,[_1mE[1],_1mF[1],_1mF[2],_1mF[3],_1mA[1],_])),_1mH=_1mG;_1mC=_1mD[2];continue;}}})(_1hf,_)),_1mI=_1mB,_1mJ=B(_1eV([0,_1mk],_1cA,_)),_1mK=_1mJ,_1mL=B(unCStr("data-erase")),_1mM=jsFind(toJSStr(_1mL)),_1mN=_1mM,_1mO=E(_1mN);if(!_1mO[0]){return new F(function(){return _10P(_1mL);});}else{var _1mP=B(_1gY(_zL,_0,function(_1mQ,_1mR,_){var _1mS=rMV(_1mk),_1mT=_1mS,_1mU=B(_nY(_)),_1mV=_1mU,_1mW=B(A(_14o,[_1mV,_])),_1mX=_1mW,_1mY=B(_17d(_1mV,_)),_1mZ=_1mY,_1n0=B(_17f(new T(function(){return E(E(_1mZ)[2]);}),_)),_1n1=_1n0,_1n2=E(_8Z),_1n3=jsFind(toJSStr(_1n2)),_1n4=_1n3,_1n5=E(_1n4);if(!_1n5[0]){return new F(function(){return _10P(_1n2);});}else{var _1n6=B(A(_xK,[_A8,_1n5[1],_zz,_DY,new T(function(){return E(E(_1n1)[2]);}),_])),_1n7=_1n6,_1n8=E(_92),_1n9=jsFind(toJSStr(_1n8)),_1na=_1n9,_1nb=E(_1na);if(!_1nb[0]){return new F(function(){return _10P(_1n8);});}else{var _1nc=B(A(_xK,[_A8,_1nb[1],_zz,_DY,new T(function(){return E(E(_1n7)[2]);}),_])),_1nd=_1nc,_1ne=E(_1mc),_1nf=jsFind(toJSStr(_1ne)),_1ng=_1nf,_1nh=E(_1ng);if(!_1nh[0]){return new F(function(){return _10P(_1ne);});}else{var _1ni=B(A(_xK,[_A8,_1nh[1],_zz,_zy,new T(function(){return E(E(_1nd)[2]);}),_])),_1nj=_1ni,_1nk=E(_1mb),_1nl=jsFind(toJSStr(_1nk)),_1nm=_1nl,_1nn=E(_1nm);if(!_1nn[0]){return new F(function(){return _10P(_1nk);});}else{var _1no=B(A(_xK,[_A8,_1nn[1],_zz,_zy,new T(function(){return E(E(_1nj)[2]);}),_])),_1np=_1no,_=wMV(_1mk,new T(function(){return E(E(_1np)[2]);}));return _cH;}}}}},_1mO[1],_)),_1nq=_1mP,_1nr=function(_1ns,_1nt){while(1){var _1nu=(function(_1nv,_1nw){var _1nx=E(_1nw);if(!_1nx[0]){var _1ny=_1nx[2],_1nz=_1nx[5];_1ns=function(_){var _1nA=rMV(_1mk),_1nB=_1nA,_1nC=E(_1nB),_1nD=_1nC[7];if(!B(_va(_1ny,_1nD))){var _=wMV(_1mk,_1nC);return new F(function(){return A(_1nr,[_1nv,_1nz,_]);});}else{var _1nE=B(_uU(_1ny,_1nD));if(_1nE[1]<=0){var _=wMV(_1mk,_1nC);return new F(function(){return A(_1nr,[_1nv,_1nz,_]);});}else{var _1nF=B(A(E(_1nx[3])[2],[_1nE,_1nC,_])),_1nG=_1nF,_=wMV(_1mk,new T(function(){return E(E(_1nG)[2]);}));return new F(function(){return A(_1nr,[_1nv,_1nz,_]);});}}};_1nt=_1nx[4];return null;}else{return E(_1nv);}})(_1ns,_1nt);if(_1nu!=null){return _1nu;}}},_1nH=B(A(_1nr,[_1ma,_1j1,_])),_1nI=_1nH,_1nJ=rMV(_1mk),_1nK=_1nJ,_1nL=B(_17d(_1nK,_)),_1nM=_1nL,_=wMV(_1mk,new T(function(){return E(E(_1nM)[2]);})),_1nN=B(_uZ(33,_1mk,_1lc,_)),_1nO=_1nN,_1nP=B(_1eH(_1eU,B(_uP(_1nO)),_)),_1nQ=_1nP,_1nR=B(_uZ(1000,_1mk,_1hC,_)),_1nS=_1nR,_1nT=B(_1eH(_1eU,B(_uP(_1nS)),_)),_1nU=_1nT,_1nV=B(_uZ(60000,_1mk,_1j2,_)),_1nW=_1nV;return new F(function(){return _1eH(_1eU,B(_uP(_1nW)),_);});}}}},_1nX=function(_){return new F(function(){return _1md(_);});};
var hasteMain = function() {B(A(_1nX, [0]));};window.onload = hasteMain;
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

var _0=new T(function(){return [0,"achievements"];}),_1=new T(function(){return [0,"lastFocus"];}),_2=new T(function(){return [0,"depend"];}),_3=new T(function(){return [0,"lps"];}),_4=new T(function(){return [0,"loves"];}),_5=new T(function(){return [0,"items"];}),_6=function(_7){return [0,toJSStr(E(_7))];},_8=function(_9){return [1,new T(function(){return B(_6(_9));})];},_a=new T(function(){return [0,"value"];}),_b=true,_c=[2,_b],_d=new T(function(){return [0,"hasValue"];}),_e=[0,_d,_c],_f=false,_g=[2,_f],_h=[0,_d,_g],_i=[0],_j=[1,_h,_i],_k=[4,_j],_l=function(_m,_n){while(1){var _o=(function(_p,_q){var _r=E(_q);switch(_r[0]){case 0:_m=new T(function(){return B(_l(_p,_r[4]));});_n=_r[3];return null;case 1:return [1,[3,[1,[0,[0,_r[1]]],[1,new T(function(){var _s=E(_r[2]);return _s[0]==0?E(_k):[4,[1,_e,[1,[0,_a,new T(function(){return B(_8(_s[1]));})],_i]]];}),_i]]],_p];default:return E(_p);}})(_m,_n);if(_o!=null){return _o;}}},_t=function(_u){return [0,new T(function(){return [0,E(_u)[1]];})];},_v=function(_w,_x){while(1){var _y=(function(_z,_A){var _B=E(_A);switch(_B[0]){case 0:_w=new T(function(){return B(_v(_z,_B[4]));});_x=_B[3];return null;case 1:return [1,[3,[1,[0,[0,_B[1]]],[1,new T(function(){return B(_t(_B[2]));}),_i]]],_z];default:return E(_z);}})(_w,_x);if(_y!=null){return _y;}}},_C=function(_D,_E){var _F=E(_D);return _F[0]==0?E(_E):[1,_F[1],new T(function(){return B(_C(_F[2],_E));})];},_G=function(_H){while(1){var _I=E(_H);if(!_I[0]){_H=[1,I_fromInt(_I[1])];continue;}else{return new F(function(){return I_toString(_I[1]);});}}},_J=function(_K,_L){return new F(function(){return _C(fromJSStr(B(_G(_K))),_L);});},_M=function(_N,_O){var _P=E(_N);if(!_P[0]){var _Q=_P[1],_R=E(_O);return _R[0]==0?_Q<_R[1]:I_compareInt(_R[1],_Q)>0;}else{var _S=_P[1],_T=E(_O);return _T[0]==0?I_compareInt(_S,_T[1])<0:I_compare(_S,_T[1])<0;}},_U=[0,41],_V=[0,40],_W=[0,0],_X=function(_Y,_Z,_10){return _Y<=6?B(_J(_Z,_10)):!B(_M(_Z,_W))?B(_J(_Z,_10)):[1,_V,new T(function(){return B(_C(fromJSStr(B(_G(_Z))),[1,_U,_10]));})];},_11=function(_12,_13,_14,_15,_16,_17){return [1,[0,_4,[0,_12]],[1,[0,_3,[0,_13]],[1,[0,_2,[0,_14]],[1,[0,_1,[1,new T(function(){return [0,toJSStr(B(_X(0,_15,_i)))];})]],[1,[0,_0,[3,new T(function(){var _18=E(_16);if(!_18[0]){var _19=_18[3],_1a=_18[4],_1b=_18[2]>=0?B(_l(new T(function(){return B(_l(_i,_1a));}),_19)):B(_l(new T(function(){return B(_l(_i,_19));}),_1a));}else{var _1b=B(_l(_i,_18));}return _1b;})]],[1,[0,_5,[3,new T(function(){var _1c=E(_17);if(!_1c[0]){var _1d=_1c[3],_1e=_1c[4],_1f=_1c[2]>=0?B(_v(new T(function(){return B(_v(_i,_1e));}),_1d)):B(_v(new T(function(){return B(_v(_i,_1d));}),_1e));}else{var _1f=B(_v(_i,_1c));}return _1f;})]],_i]]]]]];},_1g=function(_1h){var _1i=E(_1h);return [4,B(_11(_1i[1],_1i[2],_1i[3],_1i[4],_1i[7],_1i[8]))];},_1j=function(_1k,_1l){var _1m=E(_1l);return _1m[0]==0?[0]:[1,new T(function(){return B(A(_1k,[_1m[1]]));}),new T(function(){return B(_1j(_1k,_1m[2]));})];},_1n=function(_1o){return [3,new T(function(){return B(_1j(_1g,_1o));})];},_1p=function(_1q,_1r){var _1s=strEq(E(_1q)[1],E(_1r)[1]),_1t=_1s;return E(_1t)==0?true:false;},_1u=function(_1v,_1w){var _1x=strEq(E(_1v)[1],E(_1w)[1]),_1y=_1x;return E(_1y)==0?false:true;},_1z=[0,_1u,_1p],_1A=[1,_i],_1B=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_1C=[0,_1B],_1D=new T(function(){return B(unCStr("Tried to deserialize a non-array into a pair!"));}),_1E=[0,_1D],_1F=function(_1G){return E(E(_1G)[3]);},_1H=function(_1I,_1J,_1K){var _1L=E(_1K);if(_1L[0]==3){var _1M=E(_1L[1]);if(!_1M[0]){return E(_1E);}else{var _1N=E(_1M[2]);if(!_1N[0]){return E(_1E);}else{if(!E(_1N[2])[0]){var _1O=B(A(_1F,[_1I,_1M[1]]));if(!_1O[0]){return [0,_1O[1]];}else{var _1P=B(A(_1F,[_1J,_1N[1]]));return _1P[0]==0?[0,_1P[1]]:[1,[0,_1O[1],_1P[1]]];}}else{return E(_1E);}}}}else{return E(_1E);}},_1Q=function(_1R,_1S,_1T){var _1U=E(_1T);if(_1U[0]==3){var _1V=function(_1W){var _1X=E(_1W);if(!_1X[0]){return E(_1A);}else{var _1Y=B(_1H(_1R,_1S,_1X[1]));if(!_1Y[0]){return [0,_1Y[1]];}else{var _1Z=B(_1V(_1X[2]));return _1Z[0]==0?[0,_1Z[1]]:[1,[1,_1Y[1],_1Z[1]]];}}};return new F(function(){return _1V(_1U[1]);});}else{return E(_1C);}},_20=new T(function(){return B(unCStr("Tried to deserialize a non-JSString to a JSString"));}),_21=[0,_20],_22=new T(function(){return B(unCStr("Tried to deserialize a non-Number to a Double"));}),_23=[0,_22],_24=new T(function(){return B(unCStr("Key not found"));}),_25=[0,_24],_26=new T(function(){return B(unCStr("Tried to do lookup on non-object!"));}),_27=[0,_26],_28=[0,0],_29=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_2a=new T(function(){return B(err(_29));}),_2b=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_2c=new T(function(){return B(err(_2b));}),_2d=new T(function(){return B(unCStr("Control.Exception.Base"));}),_2e=new T(function(){return B(unCStr("base"));}),_2f=new T(function(){return B(unCStr("PatternMatchFail"));}),_2g=new T(function(){var _2h=hs_wordToWord64(18445595),_2i=_2h,_2j=hs_wordToWord64(52003073),_2k=_2j;return [0,_2i,_2k,[0,_2i,_2k,_2e,_2d,_2f],_i];}),_2l=function(_2m){return E(_2g);},_2n=function(_2o){return E(E(_2o)[1]);},_2p=function(_2q,_2r,_2s){var _2t=B(A(_2q,[_])),_2u=B(A(_2r,[_])),_2v=hs_eqWord64(_2t[1],_2u[1]),_2w=_2v;if(!E(_2w)){return [0];}else{var _2x=hs_eqWord64(_2t[2],_2u[2]),_2y=_2x;return E(_2y)==0?[0]:[1,_2s];}},_2z=function(_2A){var _2B=E(_2A);return new F(function(){return _2p(B(_2n(_2B[1])),_2l,_2B[2]);});},_2C=function(_2D){return E(E(_2D)[1]);},_2E=function(_2F,_2G){return new F(function(){return _C(E(_2F)[1],_2G);});},_2H=[0,44],_2I=[0,93],_2J=[0,91],_2K=function(_2L,_2M,_2N){var _2O=E(_2M);return _2O[0]==0?B(unAppCStr("[]",_2N)):[1,_2J,new T(function(){return B(A(_2L,[_2O[1],new T(function(){var _2P=function(_2Q){var _2R=E(_2Q);return _2R[0]==0?E([1,_2I,_2N]):[1,_2H,new T(function(){return B(A(_2L,[_2R[1],new T(function(){return B(_2P(_2R[2]));})]));})];};return B(_2P(_2O[2]));})]));})];},_2S=function(_2T,_2U){return new F(function(){return _2K(_2E,_2T,_2U);});},_2V=function(_2W,_2X,_2Y){return new F(function(){return _C(E(_2X)[1],_2Y);});},_2Z=[0,_2V,_2C,_2S],_30=new T(function(){return [0,_2l,_2Z,_31,_2z];}),_31=function(_32){return [0,_30,_32];},_33=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_34=function(_35,_36){return new F(function(){return die(new T(function(){return B(A(_36,[_35]));}));});},_37=function(_38,_39){var _3a=E(_39);if(!_3a[0]){return [0,_i,_i];}else{var _3b=_3a[1];if(!B(A(_38,[_3b]))){return [0,_i,_3a];}else{var _3c=new T(function(){var _3d=B(_37(_38,_3a[2]));return [0,_3d[1],_3d[2]];});return [0,[1,_3b,new T(function(){return E(E(_3c)[1]);})],new T(function(){return E(E(_3c)[2]);})];}}},_3e=[0,32],_3f=[0,10],_3g=[1,_3f,_i],_3h=function(_3i){return E(E(_3i)[1])==124?false:true;},_3j=function(_3k,_3l){var _3m=B(_37(_3h,B(unCStr(_3k)))),_3n=_3m[1],_3o=function(_3p,_3q){return new F(function(){return _C(_3p,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_C(_3l,new T(function(){return B(_C(_3q,_3g));})));})));}));});},_3r=E(_3m[2]);if(!_3r[0]){return new F(function(){return _3o(_3n,_i);});}else{return E(E(_3r[1])[1])==124?B(_3o(_3n,[1,_3e,_3r[2]])):B(_3o(_3n,_i));}},_3s=function(_3t){return new F(function(){return _34([0,new T(function(){return B(_3j(_3t,_33));})],_31);});},_3u=new T(function(){return B(_3s("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_3v=function(_3w,_3x){while(1){var _3y=(function(_3z,_3A){var _3B=E(_3z);switch(_3B[0]){case 0:var _3C=E(_3A);if(!_3C[0]){return [0];}else{_3w=B(A(_3B[1],[_3C[1]]));_3x=_3C[2];return null;}break;case 1:var _3D=B(A(_3B[1],[_3A])),_3E=_3A;_3w=_3D;_3x=_3E;return null;case 2:return [0];case 3:return [1,[0,_3B[1],_3A],new T(function(){return B(_3v(_3B[2],_3A));})];default:return E(_3B[1]);}})(_3w,_3x);if(_3y!=null){return _3y;}}},_3F=function(_3G,_3H){var _3I=function(_3J){var _3K=E(_3H);if(_3K[0]==3){return [3,_3K[1],new T(function(){return B(_3F(_3G,_3K[2]));})];}else{var _3L=E(_3G);if(_3L[0]==2){return E(_3K);}else{var _3M=E(_3K);if(_3M[0]==2){return E(_3L);}else{var _3N=function(_3O){var _3P=E(_3M);if(_3P[0]==4){return [1,function(_3Q){return [4,new T(function(){return B(_C(B(_3v(_3L,_3Q)),_3P[1]));})];}];}else{var _3R=E(_3L);if(_3R[0]==1){var _3S=_3R[1],_3T=E(_3P);return _3T[0]==0?[1,function(_3U){return new F(function(){return _3F(B(A(_3S,[_3U])),_3T);});}]:[1,function(_3V){return new F(function(){return _3F(B(A(_3S,[_3V])),new T(function(){return B(A(_3T[1],[_3V]));}));});}];}else{var _3W=E(_3P);return _3W[0]==0?E(_3u):[1,function(_3X){return new F(function(){return _3F(_3R,new T(function(){return B(A(_3W[1],[_3X]));}));});}];}}},_3Y=E(_3L);switch(_3Y[0]){case 1:var _3Z=E(_3M);if(_3Z[0]==4){return [1,function(_40){return [4,new T(function(){return B(_C(B(_3v(B(A(_3Y[1],[_40])),_40)),_3Z[1]));})];}];}else{return new F(function(){return _3N(_);});}break;case 4:var _41=_3Y[1],_42=E(_3M);switch(_42[0]){case 0:return [1,function(_43){return [4,new T(function(){return B(_C(_41,new T(function(){return B(_3v(_42,_43));})));})];}];case 1:return [1,function(_44){return [4,new T(function(){return B(_C(_41,new T(function(){return B(_3v(B(A(_42[1],[_44])),_44));})));})];}];default:return [4,new T(function(){return B(_C(_41,_42[1]));})];}break;default:return new F(function(){return _3N(_);});}}}}},_45=E(_3G);switch(_45[0]){case 0:var _46=E(_3H);if(!_46[0]){return [0,function(_47){return new F(function(){return _3F(B(A(_45[1],[_47])),new T(function(){return B(A(_46[1],[_47]));}));});}];}else{return new F(function(){return _3I(_);});}break;case 3:return [3,_45[1],new T(function(){return B(_3F(_45[2],_3H));})];default:return new F(function(){return _3I(_);});}},_48=[0,41],_49=[1,_48,_i],_4a=[0,40],_4b=[1,_4a,_i],_4c=function(_4d,_4e){while(1){var _4f=E(_4d);if(!_4f[0]){return E(_4e)[0]==0?true:false;}else{var _4g=E(_4e);if(!_4g[0]){return false;}else{if(E(_4f[1])[1]!=E(_4g[1])[1]){return false;}else{_4d=_4f[2];_4e=_4g[2];continue;}}}}},_4h=function(_4i,_4j){return E(_4i)[1]!=E(_4j)[1];},_4k=function(_4l,_4m){return E(_4l)[1]==E(_4m)[1];},_4n=[0,_4k,_4h],_4o=function(_4p,_4q){while(1){var _4r=E(_4p);if(!_4r[0]){return E(_4q)[0]==0?true:false;}else{var _4s=E(_4q);if(!_4s[0]){return false;}else{if(E(_4r[1])[1]!=E(_4s[1])[1]){return false;}else{_4p=_4r[2];_4q=_4s[2];continue;}}}}},_4t=function(_4u,_4v){return !B(_4o(_4u,_4v))?true:false;},_4w=[0,_4o,_4t],_4x=function(_4y,_4z){var _4A=E(_4y);switch(_4A[0]){case 0:return [0,function(_4B){return new F(function(){return _4x(B(A(_4A[1],[_4B])),_4z);});}];case 1:return [1,function(_4C){return new F(function(){return _4x(B(A(_4A[1],[_4C])),_4z);});}];case 2:return [2];case 3:return new F(function(){return _3F(B(A(_4z,[_4A[1]])),new T(function(){return B(_4x(_4A[2],_4z));}));});break;default:var _4D=function(_4E){var _4F=E(_4E);if(!_4F[0]){return [0];}else{var _4G=E(_4F[1]);return new F(function(){return _C(B(_3v(B(A(_4z,[_4G[1]])),_4G[2])),new T(function(){return B(_4D(_4F[2]));}));});}},_4H=B(_4D(_4A[1]));return _4H[0]==0?[2]:[4,_4H];}},_4I=[2],_4J=function(_4K){return [3,_4K,_4I];},_4L=0,_4M=function(_4N,_4O){var _4P=E(_4N);if(!_4P){return new F(function(){return A(_4O,[_4L]);});}else{return [0,function(_4Q){return E(new T(function(){return B(_4M(_4P-1|0,_4O));}));}];}},_4R=function(_4S,_4T,_4U){return function(_4V){return new F(function(){return A(function(_4W,_4X,_4Y){while(1){var _4Z=(function(_50,_51,_52){var _53=E(_50);switch(_53[0]){case 0:var _54=E(_51);if(!_54[0]){return E(_4T);}else{_4W=B(A(_53[1],[_54[1]]));_4X=_54[2];var _55=_52+1|0;_4Y=_55;return null;}break;case 1:var _56=B(A(_53[1],[_51])),_57=_51,_55=_52;_4W=_56;_4X=_57;_4Y=_55;return null;case 2:return E(_4T);case 3:return function(_58){return new F(function(){return _4M(_52,function(_59){return E(new T(function(){return B(_4x(_53,_58));}));});});};default:return function(_5a){return new F(function(){return _4x(_53,_5a);});};}})(_4W,_4X,_4Y);if(_4Z!=null){return _4Z;}}},[new T(function(){return B(A(_4S,[_4J]));}),_4V,0,_4U]);});};},_5b=function(_5c){return new F(function(){return A(_5c,[_i]);});},_5d=function(_5e,_5f){var _5g=function(_5h){var _5i=E(_5h);if(!_5i[0]){return E(_5b);}else{var _5j=_5i[1];return !B(A(_5e,[_5j]))?E(_5b):function(_5k){return [0,function(_5l){return E(new T(function(){return B(A(new T(function(){return B(_5g(_5i[2]));}),[function(_5m){return new F(function(){return A(_5k,[[1,_5j,_5m]]);});}]));}));}];};}};return function(_5n){return new F(function(){return A(_5g,[_5n,_5f]);});};},_5o=[6],_5p=function(_5q){return E(_5q);},_5r=new T(function(){return B(unCStr("valDig: Bad base"));}),_5s=new T(function(){return B(err(_5r));}),_5t=function(_5u,_5v){var _5w=function(_5x,_5y){var _5z=E(_5x);if(!_5z[0]){return function(_5A){return new F(function(){return A(_5A,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{var _5B=E(_5z[1])[1],_5C=function(_5D){return function(_5E){return [0,function(_5F){return E(new T(function(){return B(A(new T(function(){return B(_5w(_5z[2],function(_5G){return new F(function(){return A(_5y,[[1,_5D,_5G]]);});}));}),[_5E]));}));}];};};switch(E(E(_5u)[1])){case 8:if(48>_5B){return function(_5H){return new F(function(){return A(_5H,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{if(_5B>55){return function(_5I){return new F(function(){return A(_5I,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{return new F(function(){return _5C([0,_5B-48|0]);});}}break;case 10:if(48>_5B){return function(_5J){return new F(function(){return A(_5J,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{if(_5B>57){return function(_5K){return new F(function(){return A(_5K,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{return new F(function(){return _5C([0,_5B-48|0]);});}}break;case 16:if(48>_5B){if(97>_5B){if(65>_5B){return function(_5L){return new F(function(){return A(_5L,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{if(_5B>70){return function(_5M){return new F(function(){return A(_5M,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{return new F(function(){return _5C([0,(_5B-65|0)+10|0]);});}}}else{if(_5B>102){if(65>_5B){return function(_5N){return new F(function(){return A(_5N,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{if(_5B>70){return function(_5O){return new F(function(){return A(_5O,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{return new F(function(){return _5C([0,(_5B-65|0)+10|0]);});}}}else{return new F(function(){return _5C([0,(_5B-97|0)+10|0]);});}}}else{if(_5B>57){if(97>_5B){if(65>_5B){return function(_5P){return new F(function(){return A(_5P,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{if(_5B>70){return function(_5Q){return new F(function(){return A(_5Q,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{return new F(function(){return _5C([0,(_5B-65|0)+10|0]);});}}}else{if(_5B>102){if(65>_5B){return function(_5R){return new F(function(){return A(_5R,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{if(_5B>70){return function(_5S){return new F(function(){return A(_5S,[new T(function(){return B(A(_5y,[_i]));})]);});};}else{return new F(function(){return _5C([0,(_5B-65|0)+10|0]);});}}}else{return new F(function(){return _5C([0,(_5B-97|0)+10|0]);});}}}else{return new F(function(){return _5C([0,_5B-48|0]);});}}break;default:return E(_5s);}}};return function(_5T){return new F(function(){return A(_5w,[_5T,_5p,function(_5U){var _5V=E(_5U);return _5V[0]==0?[2]:B(A(_5v,[_5V]));}]);});};},_5W=[0,10],_5X=[0,1],_5Y=[0,2147483647],_5Z=function(_60,_61){while(1){var _62=E(_60);if(!_62[0]){var _63=_62[1],_64=E(_61);if(!_64[0]){var _65=_64[1],_66=addC(_63,_65);if(!E(_66[2])){return [0,_66[1]];}else{_60=[1,I_fromInt(_63)];_61=[1,I_fromInt(_65)];continue;}}else{_60=[1,I_fromInt(_63)];_61=_64;continue;}}else{var _67=E(_61);if(!_67[0]){_60=_62;_61=[1,I_fromInt(_67[1])];continue;}else{return [1,I_add(_62[1],_67[1])];}}}},_68=new T(function(){return B(_5Z(_5Y,_5X));}),_69=function(_6a){var _6b=E(_6a);if(!_6b[0]){var _6c=E(_6b[1]);return _6c==(-2147483648)?E(_68):[0, -_6c];}else{return [1,I_negate(_6b[1])];}},_6d=[0,10],_6e=[0,0],_6f=function(_6g){return [0,_6g];},_6h=function(_6i,_6j){while(1){var _6k=E(_6i);if(!_6k[0]){var _6l=_6k[1],_6m=E(_6j);if(!_6m[0]){var _6n=_6m[1];if(!(imul(_6l,_6n)|0)){return [0,imul(_6l,_6n)|0];}else{_6i=[1,I_fromInt(_6l)];_6j=[1,I_fromInt(_6n)];continue;}}else{_6i=[1,I_fromInt(_6l)];_6j=_6m;continue;}}else{var _6o=E(_6j);if(!_6o[0]){_6i=_6k;_6j=[1,I_fromInt(_6o[1])];continue;}else{return [1,I_mul(_6k[1],_6o[1])];}}}},_6p=function(_6q,_6r,_6s){while(1){var _6t=E(_6s);if(!_6t[0]){return E(_6r);}else{var _6u=B(_5Z(B(_6h(_6r,_6q)),B(_6f(E(_6t[1])[1]))));_6s=_6t[2];_6r=_6u;continue;}}},_6v=function(_6w){var _6x=new T(function(){return B(_3F(B(_3F([0,function(_6y){return E(E(_6y)[1])==45?[1,B(_5t(_5W,function(_6z){return new F(function(){return A(_6w,[[1,new T(function(){return B(_69(B(_6p(_6d,_6e,_6z))));})]]);});}))]:[2];}],[0,function(_6A){return E(E(_6A)[1])==43?[1,B(_5t(_5W,function(_6B){return new F(function(){return A(_6w,[[1,new T(function(){return B(_6p(_6d,_6e,_6B));})]]);});}))]:[2];}])),new T(function(){return [1,B(_5t(_5W,function(_6C){return new F(function(){return A(_6w,[[1,new T(function(){return B(_6p(_6d,_6e,_6C));})]]);});}))];})));});return new F(function(){return _3F([0,function(_6D){return E(E(_6D)[1])==101?E(_6x):[2];}],[0,function(_6E){return E(E(_6E)[1])==69?E(_6x):[2];}]);});},_6F=[0],_6G=function(_6H){return new F(function(){return A(_6H,[_6F]);});},_6I=function(_6J){return new F(function(){return A(_6J,[_6F]);});},_6K=function(_6L){return function(_6M){return E(E(_6M)[1])==46?[1,B(_5t(_5W,function(_6N){return new F(function(){return A(_6L,[[1,_6N]]);});}))]:[2];};},_6O=function(_6P){return [0,B(_6K(_6P))];},_6Q=function(_6R){return new F(function(){return _5t(_5W,function(_6S){return [1,B(_4R(_6O,_6G,function(_6T){return [1,B(_4R(_6v,_6I,function(_6U){return new F(function(){return A(_6R,[[5,[1,_6S,_6T,_6U]]]);});}))];}))];});});},_6V=function(_6W){return [1,B(_6Q(_6W))];},_6X=function(_6Y){return E(E(_6Y)[1]);},_6Z=function(_70,_71,_72){while(1){var _73=E(_72);if(!_73[0]){return false;}else{if(!B(A(_6X,[_70,_71,_73[1]]))){_72=_73[2];continue;}else{return true;}}}},_74=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_75=function(_76){return new F(function(){return _6Z(_4n,_76,_74);});},_77=[0,8],_78=[0,16],_79=function(_7a){var _7b=function(_7c){return new F(function(){return A(_7a,[[5,[0,_77,_7c]]]);});},_7d=function(_7e){return new F(function(){return A(_7a,[[5,[0,_78,_7e]]]);});};return function(_7f){return E(E(_7f)[1])==48?E([0,function(_7g){switch(E(E(_7g)[1])){case 79:return [1,B(_5t(_77,_7b))];case 88:return [1,B(_5t(_78,_7d))];case 111:return [1,B(_5t(_77,_7b))];case 120:return [1,B(_5t(_78,_7d))];default:return [2];}}]):[2];};},_7h=function(_7i){return [0,B(_79(_7i))];},_7j=function(_7k){var _7l=new T(function(){return B(A(_7k,[_77]));}),_7m=new T(function(){return B(A(_7k,[_78]));});return function(_7n){switch(E(E(_7n)[1])){case 79:return E(_7l);case 88:return E(_7m);case 111:return E(_7l);case 120:return E(_7m);default:return [2];}};},_7o=function(_7p){return [0,B(_7j(_7p))];},_7q=[0,92],_7r=function(_7s){return new F(function(){return A(_7s,[_5W]);});},_7t=function(_7u,_7v){var _7w=jsShowI(_7u),_7x=_7w;return new F(function(){return _C(fromJSStr(_7x),_7v);});},_7y=function(_7z,_7A,_7B){if(_7A>=0){return new F(function(){return _7t(_7A,_7B);});}else{return _7z<=6?B(_7t(_7A,_7B)):[1,_V,new T(function(){var _7C=jsShowI(_7A),_7D=_7C;return B(_C(fromJSStr(_7D),[1,_U,_7B]));})];}},_7E=function(_7F){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_7y(9,_7F,_i));}))));});},_7G=function(_7H){var _7I=E(_7H);return _7I[0]==0?E(_7I[1]):I_toInt(_7I[1]);},_7J=function(_7K,_7L){var _7M=E(_7K);if(!_7M[0]){var _7N=_7M[1],_7O=E(_7L);return _7O[0]==0?_7N<=_7O[1]:I_compareInt(_7O[1],_7N)>=0;}else{var _7P=_7M[1],_7Q=E(_7L);return _7Q[0]==0?I_compareInt(_7P,_7Q[1])<=0:I_compare(_7P,_7Q[1])<=0;}},_7R=function(_7S){return [2];},_7T=function(_7U){var _7V=E(_7U);if(!_7V[0]){return E(_7R);}else{var _7W=_7V[1],_7X=E(_7V[2]);return _7X[0]==0?E(_7W):function(_7Y){return new F(function(){return _3F(B(A(_7W,[_7Y])),new T(function(){return B(A(new T(function(){return B(_7T(_7X));}),[_7Y]));}));});};}},_7Z=function(_80){return [2];},_81=function(_82,_83){var _84=function(_85,_86){var _87=E(_85);if(!_87[0]){return function(_88){return new F(function(){return A(_88,[_82]);});};}else{var _89=E(_86);return _89[0]==0?E(_7Z):E(_87[1])[1]!=E(_89[1])[1]?E(_7Z):function(_8a){return [0,function(_8b){return E(new T(function(){return B(A(new T(function(){return B(_84(_87[2],_89[2]));}),[_8a]));}));}];};}};return function(_8c){return new F(function(){return A(_84,[_82,_8c,_83]);});};},_8d=new T(function(){return B(unCStr("SOH"));}),_8e=[0,1],_8f=function(_8g){return [1,B(_81(_8d,function(_8h){return E(new T(function(){return B(A(_8g,[_8e]));}));}))];},_8i=new T(function(){return B(unCStr("SO"));}),_8j=[0,14],_8k=function(_8l){return [1,B(_81(_8i,function(_8m){return E(new T(function(){return B(A(_8l,[_8j]));}));}))];},_8n=function(_8o){return [1,B(_4R(_8f,_8k,_8o))];},_8p=new T(function(){return B(unCStr("NUL"));}),_8q=[0,0],_8r=function(_8s){return [1,B(_81(_8p,function(_8t){return E(new T(function(){return B(A(_8s,[_8q]));}));}))];},_8u=new T(function(){return B(unCStr("STX"));}),_8v=[0,2],_8w=function(_8x){return [1,B(_81(_8u,function(_8y){return E(new T(function(){return B(A(_8x,[_8v]));}));}))];},_8z=new T(function(){return B(unCStr("ETX"));}),_8A=[0,3],_8B=function(_8C){return [1,B(_81(_8z,function(_8D){return E(new T(function(){return B(A(_8C,[_8A]));}));}))];},_8E=new T(function(){return B(unCStr("EOT"));}),_8F=[0,4],_8G=function(_8H){return [1,B(_81(_8E,function(_8I){return E(new T(function(){return B(A(_8H,[_8F]));}));}))];},_8J=new T(function(){return B(unCStr("ENQ"));}),_8K=[0,5],_8L=function(_8M){return [1,B(_81(_8J,function(_8N){return E(new T(function(){return B(A(_8M,[_8K]));}));}))];},_8O=new T(function(){return B(unCStr("ACK"));}),_8P=[0,6],_8Q=function(_8R){return [1,B(_81(_8O,function(_8S){return E(new T(function(){return B(A(_8R,[_8P]));}));}))];},_8T=new T(function(){return B(unCStr("BEL"));}),_8U=[0,7],_8V=function(_8W){return [1,B(_81(_8T,function(_8X){return E(new T(function(){return B(A(_8W,[_8U]));}));}))];},_8Y=new T(function(){return B(unCStr("BS"));}),_8Z=[0,8],_90=function(_91){return [1,B(_81(_8Y,function(_92){return E(new T(function(){return B(A(_91,[_8Z]));}));}))];},_93=new T(function(){return B(unCStr("HT"));}),_94=[0,9],_95=function(_96){return [1,B(_81(_93,function(_97){return E(new T(function(){return B(A(_96,[_94]));}));}))];},_98=new T(function(){return B(unCStr("LF"));}),_99=[0,10],_9a=function(_9b){return [1,B(_81(_98,function(_9c){return E(new T(function(){return B(A(_9b,[_99]));}));}))];},_9d=new T(function(){return B(unCStr("VT"));}),_9e=[0,11],_9f=function(_9g){return [1,B(_81(_9d,function(_9h){return E(new T(function(){return B(A(_9g,[_9e]));}));}))];},_9i=new T(function(){return B(unCStr("FF"));}),_9j=[0,12],_9k=function(_9l){return [1,B(_81(_9i,function(_9m){return E(new T(function(){return B(A(_9l,[_9j]));}));}))];},_9n=new T(function(){return B(unCStr("CR"));}),_9o=[0,13],_9p=function(_9q){return [1,B(_81(_9n,function(_9r){return E(new T(function(){return B(A(_9q,[_9o]));}));}))];},_9s=new T(function(){return B(unCStr("SI"));}),_9t=[0,15],_9u=function(_9v){return [1,B(_81(_9s,function(_9w){return E(new T(function(){return B(A(_9v,[_9t]));}));}))];},_9x=new T(function(){return B(unCStr("DLE"));}),_9y=[0,16],_9z=function(_9A){return [1,B(_81(_9x,function(_9B){return E(new T(function(){return B(A(_9A,[_9y]));}));}))];},_9C=new T(function(){return B(unCStr("DC1"));}),_9D=[0,17],_9E=function(_9F){return [1,B(_81(_9C,function(_9G){return E(new T(function(){return B(A(_9F,[_9D]));}));}))];},_9H=new T(function(){return B(unCStr("DC2"));}),_9I=[0,18],_9J=function(_9K){return [1,B(_81(_9H,function(_9L){return E(new T(function(){return B(A(_9K,[_9I]));}));}))];},_9M=new T(function(){return B(unCStr("DC3"));}),_9N=[0,19],_9O=function(_9P){return [1,B(_81(_9M,function(_9Q){return E(new T(function(){return B(A(_9P,[_9N]));}));}))];},_9R=new T(function(){return B(unCStr("DC4"));}),_9S=[0,20],_9T=function(_9U){return [1,B(_81(_9R,function(_9V){return E(new T(function(){return B(A(_9U,[_9S]));}));}))];},_9W=new T(function(){return B(unCStr("NAK"));}),_9X=[0,21],_9Y=function(_9Z){return [1,B(_81(_9W,function(_a0){return E(new T(function(){return B(A(_9Z,[_9X]));}));}))];},_a1=new T(function(){return B(unCStr("SYN"));}),_a2=[0,22],_a3=function(_a4){return [1,B(_81(_a1,function(_a5){return E(new T(function(){return B(A(_a4,[_a2]));}));}))];},_a6=new T(function(){return B(unCStr("ETB"));}),_a7=[0,23],_a8=function(_a9){return [1,B(_81(_a6,function(_aa){return E(new T(function(){return B(A(_a9,[_a7]));}));}))];},_ab=new T(function(){return B(unCStr("CAN"));}),_ac=[0,24],_ad=function(_ae){return [1,B(_81(_ab,function(_af){return E(new T(function(){return B(A(_ae,[_ac]));}));}))];},_ag=new T(function(){return B(unCStr("EM"));}),_ah=[0,25],_ai=function(_aj){return [1,B(_81(_ag,function(_ak){return E(new T(function(){return B(A(_aj,[_ah]));}));}))];},_al=new T(function(){return B(unCStr("SUB"));}),_am=[0,26],_an=function(_ao){return [1,B(_81(_al,function(_ap){return E(new T(function(){return B(A(_ao,[_am]));}));}))];},_aq=new T(function(){return B(unCStr("ESC"));}),_ar=[0,27],_as=function(_at){return [1,B(_81(_aq,function(_au){return E(new T(function(){return B(A(_at,[_ar]));}));}))];},_av=new T(function(){return B(unCStr("FS"));}),_aw=[0,28],_ax=function(_ay){return [1,B(_81(_av,function(_az){return E(new T(function(){return B(A(_ay,[_aw]));}));}))];},_aA=new T(function(){return B(unCStr("GS"));}),_aB=[0,29],_aC=function(_aD){return [1,B(_81(_aA,function(_aE){return E(new T(function(){return B(A(_aD,[_aB]));}));}))];},_aF=new T(function(){return B(unCStr("RS"));}),_aG=[0,30],_aH=function(_aI){return [1,B(_81(_aF,function(_aJ){return E(new T(function(){return B(A(_aI,[_aG]));}));}))];},_aK=new T(function(){return B(unCStr("US"));}),_aL=[0,31],_aM=function(_aN){return [1,B(_81(_aK,function(_aO){return E(new T(function(){return B(A(_aN,[_aL]));}));}))];},_aP=new T(function(){return B(unCStr("SP"));}),_aQ=[0,32],_aR=function(_aS){return [1,B(_81(_aP,function(_aT){return E(new T(function(){return B(A(_aS,[_aQ]));}));}))];},_aU=new T(function(){return B(unCStr("DEL"));}),_aV=[0,127],_aW=function(_aX){return [1,B(_81(_aU,function(_aY){return E(new T(function(){return B(A(_aX,[_aV]));}));}))];},_aZ=[1,_aW,_i],_b0=[1,_aR,_aZ],_b1=[1,_aM,_b0],_b2=[1,_aH,_b1],_b3=[1,_aC,_b2],_b4=[1,_ax,_b3],_b5=[1,_as,_b4],_b6=[1,_an,_b5],_b7=[1,_ai,_b6],_b8=[1,_ad,_b7],_b9=[1,_a8,_b8],_ba=[1,_a3,_b9],_bb=[1,_9Y,_ba],_bc=[1,_9T,_bb],_bd=[1,_9O,_bc],_be=[1,_9J,_bd],_bf=[1,_9E,_be],_bg=[1,_9z,_bf],_bh=[1,_9u,_bg],_bi=[1,_9p,_bh],_bj=[1,_9k,_bi],_bk=[1,_9f,_bj],_bl=[1,_9a,_bk],_bm=[1,_95,_bl],_bn=[1,_90,_bm],_bo=[1,_8V,_bn],_bp=[1,_8Q,_bo],_bq=[1,_8L,_bp],_br=[1,_8G,_bq],_bs=[1,_8B,_br],_bt=[1,_8w,_bs],_bu=[1,_8r,_bt],_bv=[1,_8n,_bu],_bw=new T(function(){return B(_7T(_bv));}),_bx=[0,1114111],_by=[0,34],_bz=[0,39],_bA=function(_bB){var _bC=new T(function(){return B(A(_bB,[_8U]));}),_bD=new T(function(){return B(A(_bB,[_8Z]));}),_bE=new T(function(){return B(A(_bB,[_94]));}),_bF=new T(function(){return B(A(_bB,[_99]));}),_bG=new T(function(){return B(A(_bB,[_9e]));}),_bH=new T(function(){return B(A(_bB,[_9j]));}),_bI=new T(function(){return B(A(_bB,[_9o]));});return new F(function(){return _3F([0,function(_bJ){switch(E(E(_bJ)[1])){case 34:return E(new T(function(){return B(A(_bB,[_by]));}));case 39:return E(new T(function(){return B(A(_bB,[_bz]));}));case 92:return E(new T(function(){return B(A(_bB,[_7q]));}));case 97:return E(_bC);case 98:return E(_bD);case 102:return E(_bH);case 110:return E(_bF);case 114:return E(_bI);case 116:return E(_bE);case 118:return E(_bG);default:return [2];}}],new T(function(){return B(_3F([1,B(_4R(_7o,_7r,function(_bK){return [1,B(_5t(_bK,function(_bL){var _bM=B(_6p(new T(function(){return B(_6f(E(_bK)[1]));}),_6e,_bL));return !B(_7J(_bM,_bx))?[2]:B(A(_bB,[new T(function(){var _bN=B(_7G(_bM));if(_bN>>>0>1114111){var _bO=B(_7E(_bN));}else{var _bO=[0,_bN];}var _bP=_bO,_bQ=_bP,_bR=_bQ;return _bR;})]));}))];}))],new T(function(){return B(_3F([0,function(_bS){return E(E(_bS)[1])==94?E([0,function(_bT){switch(E(E(_bT)[1])){case 64:return E(new T(function(){return B(A(_bB,[_8q]));}));case 65:return E(new T(function(){return B(A(_bB,[_8e]));}));case 66:return E(new T(function(){return B(A(_bB,[_8v]));}));case 67:return E(new T(function(){return B(A(_bB,[_8A]));}));case 68:return E(new T(function(){return B(A(_bB,[_8F]));}));case 69:return E(new T(function(){return B(A(_bB,[_8K]));}));case 70:return E(new T(function(){return B(A(_bB,[_8P]));}));case 71:return E(_bC);case 72:return E(_bD);case 73:return E(_bE);case 74:return E(_bF);case 75:return E(_bG);case 76:return E(_bH);case 77:return E(_bI);case 78:return E(new T(function(){return B(A(_bB,[_8j]));}));case 79:return E(new T(function(){return B(A(_bB,[_9t]));}));case 80:return E(new T(function(){return B(A(_bB,[_9y]));}));case 81:return E(new T(function(){return B(A(_bB,[_9D]));}));case 82:return E(new T(function(){return B(A(_bB,[_9I]));}));case 83:return E(new T(function(){return B(A(_bB,[_9N]));}));case 84:return E(new T(function(){return B(A(_bB,[_9S]));}));case 85:return E(new T(function(){return B(A(_bB,[_9X]));}));case 86:return E(new T(function(){return B(A(_bB,[_a2]));}));case 87:return E(new T(function(){return B(A(_bB,[_a7]));}));case 88:return E(new T(function(){return B(A(_bB,[_ac]));}));case 89:return E(new T(function(){return B(A(_bB,[_ah]));}));case 90:return E(new T(function(){return B(A(_bB,[_am]));}));case 91:return E(new T(function(){return B(A(_bB,[_ar]));}));case 92:return E(new T(function(){return B(A(_bB,[_aw]));}));case 93:return E(new T(function(){return B(A(_bB,[_aB]));}));case 94:return E(new T(function(){return B(A(_bB,[_aG]));}));case 95:return E(new T(function(){return B(A(_bB,[_aL]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_bw,[_bB]));})));})));}));});},_bU=function(_bV){return new F(function(){return A(_bV,[_4L]);});},_bW=function(_bX){var _bY=E(_bX);if(!_bY[0]){return E(_bU);}else{var _bZ=_bY[2],_c0=E(E(_bY[1])[1]);switch(_c0){case 9:return function(_c1){return [0,function(_c2){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_c1]));}));}];};case 10:return function(_c3){return [0,function(_c4){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_c3]));}));}];};case 11:return function(_c5){return [0,function(_c6){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_c5]));}));}];};case 12:return function(_c7){return [0,function(_c8){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_c7]));}));}];};case 13:return function(_c9){return [0,function(_ca){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_c9]));}));}];};case 32:return function(_cb){return [0,function(_cc){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_cb]));}));}];};case 160:return function(_cd){return [0,function(_ce){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_cd]));}));}];};default:var _cf=u_iswspace(_c0),_cg=_cf;return E(_cg)==0?E(_bU):function(_ch){return [0,function(_ci){return E(new T(function(){return B(A(new T(function(){return B(_bW(_bZ));}),[_ch]));}));}];};}}},_cj=function(_ck){var _cl=new T(function(){return B(_cj(_ck));}),_cm=[1,function(_cn){return new F(function(){return A(_bW,[_cn,function(_co){return E([0,function(_cp){return E(E(_cp)[1])==92?E(_cl):[2];}]);}]);});}];return new F(function(){return _3F([0,function(_cq){return E(E(_cq)[1])==92?E([0,function(_cr){var _cs=E(E(_cr)[1]);switch(_cs){case 9:return E(_cm);case 10:return E(_cm);case 11:return E(_cm);case 12:return E(_cm);case 13:return E(_cm);case 32:return E(_cm);case 38:return E(_cl);case 160:return E(_cm);default:var _ct=u_iswspace(_cs),_cu=_ct;return E(_cu)==0?[2]:E(_cm);}}]):[2];}],[0,function(_cv){var _cw=E(_cv);return E(_cw[1])==92?E(new T(function(){return B(_bA(function(_cx){return new F(function(){return A(_ck,[[0,_cx,_b]]);});}));})):B(A(_ck,[[0,_cw,_f]]));}]);});},_cy=function(_cz,_cA){return new F(function(){return _cj(function(_cB){var _cC=E(_cB),_cD=E(_cC[1]);if(E(_cD[1])==34){if(!E(_cC[2])){return E(new T(function(){return B(A(_cA,[[1,new T(function(){return B(A(_cz,[_i]));})]]));}));}else{return new F(function(){return _cy(function(_cE){return new F(function(){return A(_cz,[[1,_cD,_cE]]);});},_cA);});}}else{return new F(function(){return _cy(function(_cF){return new F(function(){return A(_cz,[[1,_cD,_cF]]);});},_cA);});}});});},_cG=new T(function(){return B(unCStr("_\'"));}),_cH=function(_cI){var _cJ=u_iswalnum(_cI),_cK=_cJ;return E(_cK)==0?B(_6Z(_4n,[0,_cI],_cG)):true;},_cL=function(_cM){return new F(function(){return _cH(E(_cM)[1]);});},_cN=new T(function(){return B(unCStr(",;()[]{}`"));}),_cO=new T(function(){return B(unCStr(".."));}),_cP=new T(function(){return B(unCStr("::"));}),_cQ=new T(function(){return B(unCStr("->"));}),_cR=[0,64],_cS=[1,_cR,_i],_cT=[0,126],_cU=[1,_cT,_i],_cV=new T(function(){return B(unCStr("=>"));}),_cW=[1,_cV,_i],_cX=[1,_cU,_cW],_cY=[1,_cS,_cX],_cZ=[1,_cQ,_cY],_d0=new T(function(){return B(unCStr("<-"));}),_d1=[1,_d0,_cZ],_d2=[0,124],_d3=[1,_d2,_i],_d4=[1,_d3,_d1],_d5=[1,_7q,_i],_d6=[1,_d5,_d4],_d7=[0,61],_d8=[1,_d7,_i],_d9=[1,_d8,_d6],_da=[1,_cP,_d9],_db=[1,_cO,_da],_dc=function(_dd){return new F(function(){return _3F([1,function(_de){return E(_de)[0]==0?E(new T(function(){return B(A(_dd,[_5o]));})):[2];}],new T(function(){return B(_3F([0,function(_df){return E(E(_df)[1])==39?E([0,function(_dg){var _dh=E(_dg);switch(E(_dh[1])){case 39:return [2];case 92:return E(new T(function(){return B(_bA(function(_di){return [0,function(_dj){return E(E(_dj)[1])==39?E(new T(function(){return B(A(_dd,[[0,_di]]));})):[2];}];}));}));default:return [0,function(_dk){return E(E(_dk)[1])==39?E(new T(function(){return B(A(_dd,[[0,_dh]]));})):[2];}];}}]):[2];}],new T(function(){return B(_3F([0,function(_dl){return E(E(_dl)[1])==34?E(new T(function(){return B(_cy(_5p,_dd));})):[2];}],new T(function(){return B(_3F([0,function(_dm){return !B(_6Z(_4n,_dm,_cN))?[2]:B(A(_dd,[[2,[1,_dm,_i]]]));}],new T(function(){return B(_3F([0,function(_dn){return !B(_6Z(_4n,_dn,_74))?[2]:[1,B(_5d(_75,function(_do){var _dp=[1,_dn,_do];return !B(_6Z(_4w,_dp,_db))?B(A(_dd,[[4,_dp]])):B(A(_dd,[[2,_dp]]));}))];}],new T(function(){return B(_3F([0,function(_dq){var _dr=E(_dq),_ds=_dr[1],_dt=u_iswalpha(_ds),_du=_dt;return E(_du)==0?E(_ds)==95?[1,B(_5d(_cL,function(_dv){return new F(function(){return A(_dd,[[3,[1,_dr,_dv]]]);});}))]:[2]:[1,B(_5d(_cL,function(_dw){return new F(function(){return A(_dd,[[3,[1,_dr,_dw]]]);});}))];}],new T(function(){return [1,B(_4R(_7h,_6V,_dd))];})));})));})));})));})));}));});},_dx=[0,0],_dy=function(_dz,_dA){return function(_dB){return new F(function(){return A(_bW,[_dB,function(_dC){return E(new T(function(){return B(_dc(function(_dD){var _dE=E(_dD);return _dE[0]==2?!B(_4c(_dE[1],_4b))?[2]:E(new T(function(){return B(A(_dz,[_dx,function(_dF){return [1,function(_dG){return new F(function(){return A(_bW,[_dG,function(_dH){return E(new T(function(){return B(_dc(function(_dI){var _dJ=E(_dI);return _dJ[0]==2?!B(_4c(_dJ[1],_49))?[2]:E(new T(function(){return B(A(_dA,[_dF]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_dK=function(_dL,_dM,_dN){var _dO=function(_dP,_dQ){return new F(function(){return _3F([1,function(_dR){return new F(function(){return A(_bW,[_dR,function(_dS){return E(new T(function(){return B(_dc(function(_dT){var _dU=E(_dT);if(_dU[0]==4){var _dV=E(_dU[1]);if(!_dV[0]){return new F(function(){return A(_dL,[_dU,_dP,_dQ]);});}else{return E(E(_dV[1])[1])==45?E(_dV[2])[0]==0?E([1,function(_dW){return new F(function(){return A(_bW,[_dW,function(_dX){return E(new T(function(){return B(_dc(function(_dY){return new F(function(){return A(_dL,[_dY,_dP,function(_dZ){return new F(function(){return A(_dQ,[new T(function(){return B(_69(_dZ));})]);});}]);});}));}));}]);});}]):B(A(_dL,[_dU,_dP,_dQ])):B(A(_dL,[_dU,_dP,_dQ]));}}else{return new F(function(){return A(_dL,[_dU,_dP,_dQ]);});}}));}));}]);});}],new T(function(){return [1,B(_dy(_dO,_dQ))];}));});};return new F(function(){return _dO(_dM,_dN);});},_e0=function(_e1,_e2){return [2];},_e3=function(_e4){var _e5=E(_e4);return _e5[0]==0?[1,new T(function(){return B(_6p(new T(function(){return B(_6f(E(_e5[1])[1]));}),_6e,_e5[2]));})]:E(_e5[2])[0]==0?E(_e5[3])[0]==0?[1,new T(function(){return B(_6p(_6d,_6e,_e5[1]));})]:[0]:[0];},_e6=function(_e7){var _e8=E(_e7);if(_e8[0]==5){var _e9=B(_e3(_e8[1]));return _e9[0]==0?E(_e0):function(_ea,_eb){return new F(function(){return A(_eb,[_e9[1]]);});};}else{return E(_e0);}},_ec=function(_ed){return [1,function(_ee){return new F(function(){return A(_bW,[_ee,function(_ef){return E([3,_ed,_4I]);}]);});}];},_eg=new T(function(){return B(_dK(_e6,_dx,_ec));}),_eh=new T(function(){return B(unCStr("Tried to deserialize long string to a Char"));}),_ei=[0,_eh],_ej=new T(function(){return B(unCStr("Tried to deserialize a non-string to a Char"));}),_ek=[0,_ej],_el=function(_em){var _en=E(_em);if(_en[0]==1){var _eo=fromJSStr(E(_en[1])[1]);return _eo[0]==0?E(_ei):E(_eo[2])[0]==0?[1,_eo[1]]:E(_ei);}else{return E(_ek);}},_ep=[0,_20],_eq=function(_er){return new F(function(){return fromJSStr(E(_er)[1]);});},_es=function(_et){var _eu=E(_et);return _eu[0]==1?[1,new T(function(){return B(_eq(_eu[1]));})]:E(_ep);},_ev=function(_ew){return [1,new T(function(){return [0,toJSStr([1,_ew,_i])];})];},_ex=[0,_ev,_8,_el,_es],_ey=function(_ez){return E(E(_ez)[2]);},_eA=function(_eB,_eC){return [3,new T(function(){return B(_1j(new T(function(){return B(_ey(_eB));}),_eC));})];},_eD=[1,_i],_eE=[0,_1B],_eF=function(_eG){return E(E(_eG)[4]);},_eH=function(_eI,_eJ){var _eK=E(_eJ);if(_eK[0]==3){var _eL=function(_eM){var _eN=E(_eM);if(!_eN[0]){return E(_eD);}else{var _eO=B(A(new T(function(){return B(_eF(_eI));}),[_eN[1]]));if(!_eO[0]){return [0,_eO[1]];}else{var _eP=B(_eL(_eN[2]));return _eP[0]==0?[0,_eP[1]]:[1,[1,_eO[1],_eP[1]]];}}};return new F(function(){return _eL(_eK[1]);});}else{return E(_eE);}},_eQ=function(_eR){return [0,new T(function(){return B(_ey(_eR));}),function(_eS){return new F(function(){return _eA(_eR,_eS);});},new T(function(){return B(_eF(_eR));}),function(_eS){return new F(function(){return _eH(_eR,_eS);});}];},_eT=new T(function(){return B(_eQ(_ex));}),_eU=function(_eV){return E(E(_eV)[1]);},_eW=function(_eX,_eY){var _eZ=E(_eY);return _eZ[0]==0?E(_k):[4,[1,_e,[1,[0,_a,new T(function(){return B(A(_eU,[_eX,_eZ[1]]));})],_i]]];},_f0=function(_f1,_f2){return [3,new T(function(){return B(_1j(function(_eS){return new F(function(){return _eW(_f1,_eS);});},_f2));})];},_f3=[1,_6F],_f4=new T(function(){return B(unCStr("Tried to deserialize a non-Bool to a Bool"));}),_f5=[0,_f4],_f6=[0,_24],_f7=[0,_26],_f8=function(_f9,_fa,_fb){while(1){var _fc=E(_fb);if(!_fc[0]){return [0];}else{var _fd=E(_fc[1]);if(!B(A(_6X,[_f9,_fa,_fd[1]]))){_fb=_fc[2];continue;}else{return [1,_fd[2]];}}}},_fe=function(_ff,_fg){var _fh=E(_fg);if(_fh[0]==4){var _fi=_fh[1],_fj=B(_f8(_1z,_d,_fi));if(!_fj[0]){return E(_f6);}else{var _fk=E(_fj[1]);if(_fk[0]==2){if(!E(_fk[1])){return E(_f3);}else{var _fl=B(_f8(_1z,_a,_fi));if(!_fl[0]){return E(_f6);}else{var _fm=B(A(_1F,[_ff,_fl[1]]));return _fm[0]==0?[0,_fm[1]]:[1,[1,_fm[1]]];}}}else{return E(_f5);}}}else{return E(_f7);}},_fn=[1,_i],_fo=[0,_1B],_fp=function(_fq,_fr){var _fs=E(_fr);if(_fs[0]==3){var _ft=function(_fu){var _fv=E(_fu);if(!_fv[0]){return E(_fn);}else{var _fw=B(_fe(_fq,_fv[1]));if(!_fw[0]){return [0,_fw[1]];}else{var _fx=B(_ft(_fv[2]));return _fx[0]==0?[0,_fx[1]]:[1,[1,_fw[1],_fx[1]]];}}};return new F(function(){return _ft(_fs[1]);});}else{return E(_fo);}},_fy=function(_fz){return [0,function(_eS){return new F(function(){return _eW(_fz,_eS);});},function(_eS){return new F(function(){return _f0(_fz,_eS);});},function(_eS){return new F(function(){return _fe(_fz,_eS);});},function(_eS){return new F(function(){return _fp(_fz,_eS);});}];},_fA=new T(function(){return B(_fy(_eT));}),_fB=function(_fC){return [3,new T(function(){return B(_1j(_t,_fC));})];},_fD=new T(function(){return B(unCStr("The given Number can\'t be represented as an Int"));}),_fE=[0,_fD],_fF=new T(function(){return B(unCStr("Tried to deserialize a non-Number to an Int"));}),_fG=[0,_fF],_fH=function(_fI){var _fJ=E(_fI);if(!_fJ[0]){var _fK=E(_fJ[1])[1],_fL=_fK&4294967295;return _fL!=_fK?E(_fE):[1,[0,_fL]];}else{return E(_fG);}},_fM=[0,_1B],_fN=[1,_i],_fO=[0,_fD],_fP=[0,_fF],_fQ=function(_fR){var _fS=E(_fR);if(!_fS[0]){return E(_fN);}else{var _fT=E(_fS[1]);if(!_fT[0]){var _fU=E(_fT[1])[1],_fV=_fU&4294967295;if(_fV!=_fU){return E(_fO);}else{var _fW=B(_fQ(_fS[2]));return _fW[0]==0?[0,_fW[1]]:[1,[1,[0,_fV],_fW[1]]];}}else{return E(_fP);}}},_fX=function(_fY){var _fZ=E(_fY);return _fZ[0]==3?B(_fQ(_fZ[1])):E(_fM);},_g0=[0,_t,_fB,_fH,_fX],_g1=[2],_g2=function(_g3,_g4,_g5){var _g6=E(_g5);switch(_g6[0]){case 0:var _g7=_g6[1],_g8=_g6[2],_g9=_g6[3],_ga=_g6[4],_gb=_g8>>>0;if(((_g3>>>0&((_gb-1>>>0^4294967295)>>>0^_gb)>>>0)>>>0&4294967295)==_g7){return (_g3>>>0&_gb)>>>0==0?[0,_g7,_g8,E(B(_g2(_g3,_g4,_g9))),E(_ga)]:[0,_g7,_g8,E(_g9),E(B(_g2(_g3,_g4,_ga)))];}else{var _gc=(_g3>>>0^_g7>>>0)>>>0,_gd=(_gc|_gc>>>1)>>>0,_ge=(_gd|_gd>>>2)>>>0,_gf=(_ge|_ge>>>4)>>>0,_gg=(_gf|_gf>>>8)>>>0,_gh=(_gg|_gg>>>16)>>>0,_gi=(_gh^_gh>>>1)>>>0&4294967295,_gj=_gi>>>0;return (_g3>>>0&_gj)>>>0==0?[0,(_g3>>>0&((_gj-1>>>0^4294967295)>>>0^_gj)>>>0)>>>0&4294967295,_gi,E([1,_g3,_g4]),E(_g6)]:[0,(_g3>>>0&((_gj-1>>>0^4294967295)>>>0^_gj)>>>0)>>>0&4294967295,_gi,E(_g6),E([1,_g3,_g4])];}break;case 1:var _gk=_g6[1];if(_g3!=_gk){var _gl=(_g3>>>0^_gk>>>0)>>>0,_gm=(_gl|_gl>>>1)>>>0,_gn=(_gm|_gm>>>2)>>>0,_go=(_gn|_gn>>>4)>>>0,_gp=(_go|_go>>>8)>>>0,_gq=(_gp|_gp>>>16)>>>0,_gr=(_gq^_gq>>>1)>>>0&4294967295,_gs=_gr>>>0;return (_g3>>>0&_gs)>>>0==0?[0,(_g3>>>0&((_gs-1>>>0^4294967295)>>>0^_gs)>>>0)>>>0&4294967295,_gr,E([1,_g3,_g4]),E(_g6)]:[0,(_g3>>>0&((_gs-1>>>0^4294967295)>>>0^_gs)>>>0)>>>0&4294967295,_gr,E(_g6),E([1,_g3,_g4])];}else{return [1,_g3,_g4];}break;default:return [1,_g3,_g4];}},_gt=function(_gu,_gv){while(1){var _gw=E(_gv);if(!_gw[0]){return E(_gu);}else{var _gx=E(_gw[1]),_gy=B(_g2(E(_gx[1])[1],_gx[2],_gu));_gv=_gw[2];_gu=_gy;continue;}}},_gz=function(_gA){return new F(function(){return _gt(_g1,_gA);});},_gB=function(_gC){while(1){var _gD=(function(_gE){var _gF=E(_gE);if(!_gF[0]){return [0];}else{var _gG=_gF[2],_gH=E(_gF[1]);if(!E(_gH[2])[0]){return [1,_gH[1],new T(function(){return B(_gB(_gG));})];}else{_gC=_gG;return null;}}})(_gC);if(_gD!=null){return _gD;}}},_gI=function(_gJ){var _gK=E(_gJ);if(_gK[0]==4){var _gL=_gK[1],_gM=B(_f8(_1z,_4,_gL));if(!_gM[0]){return E(_25);}else{var _gN=E(_gM[1]);if(!_gN[0]){var _gO=B(_f8(_1z,_3,_gL));if(!_gO[0]){return E(_25);}else{var _gP=E(_gO[1]);if(!_gP[0]){var _gQ=B(_f8(_1z,_2,_gL));if(!_gQ[0]){return E(_25);}else{var _gR=E(_gQ[1]);if(!_gR[0]){var _gS=B(_f8(_1z,_1,_gL));if(!_gS[0]){return E(_25);}else{var _gT=E(_gS[1]);if(_gT[0]==1){var _gU=B(_f8(_1z,_0,_gL));if(!_gU[0]){return E(_25);}else{var _gV=B(_1Q(_g0,_fA,_gU[1]));if(!_gV[0]){return [0,_gV[1]];}else{var _gW=B(_f8(_1z,_5,_gL));if(!_gW[0]){return E(_25);}else{var _gX=B(_1Q(_g0,_g0,_gW[1]));return _gX[0]==0?[0,_gX[1]]:[1,[0,_gN[1],_gP[1],_gR[1],new T(function(){var _gY=B(_gB(B(_3v(_eg,new T(function(){return fromJSStr(E(_gT[1])[1]);})))));return _gY[0]==0?E(_2c):E(_gY[2])[0]==0?E(_gY[1]):E(_2a);}),_28,_f,new T(function(){return B(_gz(_gV[1]));}),new T(function(){return B(_gz(_gX[1]));})]];}}}}else{return E(_21);}}}else{return E(_23);}}}else{return E(_23);}}}else{return E(_23);}}}else{return E(_27);}},_gZ=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_h0=[0,_gZ],_h1=[1,_i],_h2=function(_h3){var _h4=E(_h3);if(!_h4[0]){return E(_h1);}else{var _h5=B(_gI(_h4[1]));if(!_h5[0]){return [0,_h5[1]];}else{var _h6=B(_h2(_h4[2]));return _h6[0]==0?[0,_h6[1]]:[1,[1,_h5[1],_h6[1]]];}}},_h7=function(_h8){var _h9=E(_h8);return _h9[0]==3?B(_h2(_h9[1])):E(_h0);},_ha=[0,_1g,_1n,_gI,_h7],_hb=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_hc=new T(function(){return B(err(_hb));}),_hd=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_he=new T(function(){return B(err(_hd));}),_hf=function(_hg,_hh){while(1){var _hi=E(_hg);if(!_hi[0]){return E(_he);}else{var _hj=E(_hh);if(!_hj){return E(_hi[1]);}else{_hg=_hi[2];_hh=_hj-1|0;continue;}}}},_hk=new T(function(){return B(unCStr("ACK"));}),_hl=new T(function(){return B(unCStr("BEL"));}),_hm=new T(function(){return B(unCStr("BS"));}),_hn=new T(function(){return B(unCStr("SP"));}),_ho=[1,_hn,_i],_hp=new T(function(){return B(unCStr("US"));}),_hq=[1,_hp,_ho],_hr=new T(function(){return B(unCStr("RS"));}),_hs=[1,_hr,_hq],_ht=new T(function(){return B(unCStr("GS"));}),_hu=[1,_ht,_hs],_hv=new T(function(){return B(unCStr("FS"));}),_hw=[1,_hv,_hu],_hx=new T(function(){return B(unCStr("ESC"));}),_hy=[1,_hx,_hw],_hz=new T(function(){return B(unCStr("SUB"));}),_hA=[1,_hz,_hy],_hB=new T(function(){return B(unCStr("EM"));}),_hC=[1,_hB,_hA],_hD=new T(function(){return B(unCStr("CAN"));}),_hE=[1,_hD,_hC],_hF=new T(function(){return B(unCStr("ETB"));}),_hG=[1,_hF,_hE],_hH=new T(function(){return B(unCStr("SYN"));}),_hI=[1,_hH,_hG],_hJ=new T(function(){return B(unCStr("NAK"));}),_hK=[1,_hJ,_hI],_hL=new T(function(){return B(unCStr("DC4"));}),_hM=[1,_hL,_hK],_hN=new T(function(){return B(unCStr("DC3"));}),_hO=[1,_hN,_hM],_hP=new T(function(){return B(unCStr("DC2"));}),_hQ=[1,_hP,_hO],_hR=new T(function(){return B(unCStr("DC1"));}),_hS=[1,_hR,_hQ],_hT=new T(function(){return B(unCStr("DLE"));}),_hU=[1,_hT,_hS],_hV=new T(function(){return B(unCStr("SI"));}),_hW=[1,_hV,_hU],_hX=new T(function(){return B(unCStr("SO"));}),_hY=[1,_hX,_hW],_hZ=new T(function(){return B(unCStr("CR"));}),_i0=[1,_hZ,_hY],_i1=new T(function(){return B(unCStr("FF"));}),_i2=[1,_i1,_i0],_i3=new T(function(){return B(unCStr("VT"));}),_i4=[1,_i3,_i2],_i5=new T(function(){return B(unCStr("LF"));}),_i6=[1,_i5,_i4],_i7=new T(function(){return B(unCStr("HT"));}),_i8=[1,_i7,_i6],_i9=[1,_hm,_i8],_ia=[1,_hl,_i9],_ib=[1,_hk,_ia],_ic=new T(function(){return B(unCStr("ENQ"));}),_id=[1,_ic,_ib],_ie=new T(function(){return B(unCStr("EOT"));}),_if=[1,_ie,_id],_ig=new T(function(){return B(unCStr("ETX"));}),_ih=[1,_ig,_if],_ii=new T(function(){return B(unCStr("STX"));}),_ij=[1,_ii,_ih],_ik=new T(function(){return B(unCStr("SOH"));}),_il=[1,_ik,_ij],_im=new T(function(){return B(unCStr("NUL"));}),_in=[1,_im,_il],_io=[0,92],_ip=new T(function(){return B(unCStr("\\DEL"));}),_iq=new T(function(){return B(unCStr("\\a"));}),_ir=new T(function(){return B(unCStr("\\\\"));}),_is=new T(function(){return B(unCStr("\\SO"));}),_it=new T(function(){return B(unCStr("\\r"));}),_iu=new T(function(){return B(unCStr("\\f"));}),_iv=new T(function(){return B(unCStr("\\v"));}),_iw=new T(function(){return B(unCStr("\\n"));}),_ix=new T(function(){return B(unCStr("\\t"));}),_iy=new T(function(){return B(unCStr("\\b"));}),_iz=function(_iA,_iB){if(_iA<=127){var _iC=E(_iA);switch(_iC){case 92:return new F(function(){return _C(_ir,_iB);});break;case 127:return new F(function(){return _C(_ip,_iB);});break;default:if(_iC<32){var _iD=E(_iC);switch(_iD){case 7:return new F(function(){return _C(_iq,_iB);});break;case 8:return new F(function(){return _C(_iy,_iB);});break;case 9:return new F(function(){return _C(_ix,_iB);});break;case 10:return new F(function(){return _C(_iw,_iB);});break;case 11:return new F(function(){return _C(_iv,_iB);});break;case 12:return new F(function(){return _C(_iu,_iB);});break;case 13:return new F(function(){return _C(_it,_iB);});break;case 14:return new F(function(){return _C(_is,new T(function(){var _iE=E(_iB);if(!_iE[0]){var _iF=[0];}else{var _iF=E(E(_iE[1])[1])==72?B(unAppCStr("\\&",_iE)):E(_iE);}return _iF;}));});break;default:return new F(function(){return _C([1,_io,new T(function(){var _iG=_iD;return _iG>=0?B(_hf(_in,_iG)):E(_hc);})],_iB);});}}else{return [1,[0,_iC],_iB];}}}else{return [1,_io,new T(function(){var _iH=jsShowI(_iA),_iI=_iH;return B(_C(fromJSStr(_iI),new T(function(){var _iJ=E(_iB);if(!_iJ[0]){var _iK=[0];}else{var _iL=E(_iJ[1])[1];if(_iL<48){var _iM=E(_iJ);}else{var _iM=_iL>57?E(_iJ):B(unAppCStr("\\&",_iJ));}var _iN=_iM,_iO=_iN,_iK=_iO;}return _iK;})));})];}},_iP=[0,39],_iQ=[1,_iP,_i],_iR=new T(function(){return B(unCStr("\'\\\'\'"));}),_iS=function(_iT){var _iU=E(E(_iT)[1]);return _iU==39?E(_iR):[1,_iP,new T(function(){return B(_iz(_iU,_iQ));})];},_iV=[0,34],_iW=new T(function(){return B(unCStr("\\\""));}),_iX=function(_iY,_iZ){var _j0=E(_iY);if(!_j0[0]){return E(_iZ);}else{var _j1=_j0[2],_j2=E(E(_j0[1])[1]);if(_j2==34){return new F(function(){return _C(_iW,new T(function(){return B(_iX(_j1,_iZ));}));});}else{return new F(function(){return _iz(_j2,new T(function(){return B(_iX(_j1,_iZ));}));});}}},_j3=function(_j4,_j5){return [1,_iV,new T(function(){return B(_iX(_j4,[1,_iV,_j5]));})];},_j6=function(_j7){return new F(function(){return _C(_iR,_j7);});},_j8=function(_j9,_ja){var _jb=E(E(_ja)[1]);return _jb==39?E(_j6):function(_jc){return [1,_iP,new T(function(){return B(_iz(_jb,[1,_iP,_jc]));})];};},_jd=[0,_j8,_iS,_j3],_je=function(_jf){return E(E(_jf)[3]);},_jg=function(_jh,_ji){return new F(function(){return A(_je,[_jh,_ji,_i]);});},_jj=function(_jk,_jl,_jm){return new F(function(){return _2K(new T(function(){return B(_je(_jk));}),_jl,_jm);});},_jn=function(_jo){return [0,function(_jp){return E(new T(function(){return B(_je(_jo));}));},function(_j7){return new F(function(){return _jg(_jo,_j7);});},function(_jq,_j7){return new F(function(){return _jj(_jo,_jq,_j7);});}];},_jr=new T(function(){return B(_jn(_jd));}),_js=new T(function(){return B(unCStr("Just "));}),_jt=new T(function(){return B(unCStr("Nothing"));}),_ju=[0,11],_jv=function(_jw){return E(E(_jw)[1]);},_jx=function(_jy,_jz,_jA,_jB){var _jC=E(_jA);if(!_jC[0]){return new F(function(){return _C(_jt,_jB);});}else{var _jD=_jC[1];return E(_jz)[1]<=10?B(_C(_js,new T(function(){return B(A(_jv,[_jy,_ju,_jD,_jB]));}))):[1,_V,new T(function(){return B(_C(_js,new T(function(){return B(A(_jv,[_jy,_ju,_jD,[1,_U,_jB]]));})));})];}},_jE=[0,0],_jF=function(_jG,_jH){return new F(function(){return _jx(_jG,_jE,_jH,_i);});},_jI=function(_jJ,_jK,_jL){return new F(function(){return _2K(function(_jq,_j7){return new F(function(){return _jx(_jJ,_jE,_jq,_j7);});},_jK,_jL);});},_jM=function(_jN){return [0,function(_jO,_jq,_j7){return new F(function(){return _jx(_jN,_jO,_jq,_j7);});},function(_j7){return new F(function(){return _jF(_jN,_j7);});},function(_jq,_j7){return new F(function(){return _jI(_jN,_jq,_j7);});}];},_jP=new T(function(){return B(_jM(_jr));}),_jQ=function(_jR){var _jS=jsShow(E(_jR)[1]),_jT=_jS;return new F(function(){return fromJSStr(_jT);});},_jU=function(_jV){return function(_5a){return new F(function(){return _C(new T(function(){return B(_jQ(_jV));}),_5a);});};},_jW=function(_jX){return new F(function(){return _7y(0,E(_jX)[1],_i);});},_jY=function(_jZ,_k0){return new F(function(){return _7y(0,E(_jZ)[1],_k0);});},_k1=function(_k2,_k3){return new F(function(){return _2K(_jY,_k2,_k3);});},_k4=function(_k5,_k6,_k7){return new F(function(){return _7y(E(_k5)[1],E(_k6)[1],_k7);});},_k8=[0,_k4,_jW,_k1],_k9=function(_ka,_kb,_kc){return new F(function(){return A(_ka,[[1,_2H,new T(function(){return B(A(_kb,[_kc]));})]]);});},_kd=new T(function(){return B(unCStr(": empty list"));}),_ke=new T(function(){return B(unCStr("Prelude."));}),_kf=function(_kg){return new F(function(){return err(B(_C(_ke,new T(function(){return B(_C(_kg,_kd));}))));});},_kh=new T(function(){return B(unCStr("foldr1"));}),_ki=new T(function(){return B(_kf(_kh));}),_kj=function(_kk,_kl){var _km=E(_kl);if(!_km[0]){return E(_ki);}else{var _kn=_km[1],_ko=E(_km[2]);if(!_ko[0]){return E(_kn);}else{return new F(function(){return A(_kk,[_kn,new T(function(){return B(_kj(_kk,_ko));})]);});}}},_kp=function(_kq,_kr,_ks,_kt){return new F(function(){return _2K(function(_ku,_kv){var _kw=E(_ku);return [1,_V,new T(function(){return B(A(_kj,[_k9,[1,new T(function(){return B(A(new T(function(){return B(_jv(_kq));}),[_jE,_kw[1]]));}),[1,new T(function(){return B(A(new T(function(){return B(_jv(_kr));}),[_jE,_kw[2]]));}),_i]],[1,_U,_kv]]));})];},_ks,_kt);});},_kx=new T(function(){return B(unCStr("fromList "));}),_ky=function(_kz,_kA){while(1){var _kB=(function(_kC,_kD){var _kE=E(_kD);switch(_kE[0]){case 0:_kz=new T(function(){return B(_ky(_kC,_kE[4]));});_kA=_kE[3];return null;case 1:return [1,[0,[0,_kE[1]],_kE[2]],_kC];default:return E(_kC);}})(_kz,_kA);if(_kB!=null){return _kB;}}},_kF=function(_kG){var _kH=E(_kG);if(!_kH[0]){var _kI=_kH[3],_kJ=_kH[4];return _kH[2]>=0?B(_ky(new T(function(){return B(_ky(_i,_kJ));}),_kI)):B(_ky(new T(function(){return B(_ky(_i,_kI));}),_kJ));}else{return new F(function(){return _ky(_i,_kH);});}},_kK=function(_kL,_kM,_kN){var _kO=new T(function(){return B(_kF(_kN));});return _kM<=10?function(_kP){return new F(function(){return _C(_kx,new T(function(){return B(_kp(_k8,_kL,_kO,_kP));}));});}:function(_kQ){return [1,_V,new T(function(){return B(_C(_kx,new T(function(){return B(_kp(_k8,_kL,_kO,[1,_U,_kQ]));})));})];};},_kR=[0,45],_kS=function(_kT,_kU,_kV){var _kW=function(_kX){var _kY=new T(function(){return B(A(_kT,[[0, -_kV]]));});return E(_kU)[1]<=6?function(_kZ){return [1,_kR,new T(function(){return B(A(_kY,[_kZ]));})];}:function(_l0){return [1,_V,[1,_kR,new T(function(){return B(A(_kY,[[1,_U,_l0]]));})]];};};if(_kV>=0){var _l1=isDoubleNegativeZero(_kV),_l2=_l1;return E(_l2)==0?B(A(_kT,[[0,_kV]])):B(_kW(_));}else{return new F(function(){return _kW(_);});}},_l3=new T(function(){return B(unCStr("Aichan {"));}),_l4=new T(function(){return B(unCStr("_loves = "));}),_l5=new T(function(){return B(unCStr("_items = "));}),_l6=[0,125],_l7=[0,0],_l8=new T(function(){return B(unCStr(", "));}),_l9=new T(function(){return B(unCStr("_lps = "));}),_la=new T(function(){return B(unCStr("_depend = "));}),_lb=new T(function(){return B(unCStr("_lastFocus = "));}),_lc=new T(function(){return B(unCStr("_interval = "));}),_ld=new T(function(){return B(unCStr("_hasFocus = "));}),_le=new T(function(){return B(unCStr("_achieves = "));}),_lf=new T(function(){return B(unCStr("True"));}),_lg=new T(function(){return B(unCStr("False"));}),_lh=function(_li,_lj,_lk,_ll,_lm,_ln,_lo,_lp,_lq){var _lr=function(_ls){return new F(function(){return _C(_l4,new T(function(){return B(A(new T(function(){return B(_kS(_jU,_l7,E(_lj)[1]));}),[new T(function(){return B(_C(_l8,new T(function(){return B(_C(_l9,new T(function(){return B(A(new T(function(){return B(_kS(_jU,_l7,E(_lk)[1]));}),[new T(function(){return B(_C(_l8,new T(function(){return B(_C(_la,new T(function(){return B(A(new T(function(){return B(_kS(_jU,_l7,E(_ll)[1]));}),[new T(function(){return B(_C(_l8,new T(function(){return B(_C(_lb,new T(function(){return B(_X(0,_lm,new T(function(){return B(_C(_l8,new T(function(){return B(_C(_lc,new T(function(){return B(_X(0,_ln,new T(function(){return B(_C(_l8,new T(function(){return B(_C(_ld,new T(function(){var _lt=new T(function(){return B(_C(_l8,new T(function(){return B(_C(_le,new T(function(){return B(A(new T(function(){return B(_kK(_jP,0,_lp));}),[new T(function(){return B(_C(_l8,new T(function(){return B(_C(_l5,new T(function(){return B(A(new T(function(){return B(_kK(_k8,0,_lq));}),[[1,_l6,_ls]]));})));})));})]));})));})));});return !E(_lo)?B(_C(_lg,_lt)):B(_C(_lf,_lt));})));})));})));})));})));})));})));})));})]));})));})));})]));})));})));})]));}));});};return _li<11?function(_lu){return new F(function(){return _C(_l3,new T(function(){return B(_lr(_lu));}));});}:function(_lv){return [1,_V,new T(function(){return B(_C(_l3,new T(function(){return B(_lr([1,_U,_lv]));})));})];};},_lw=function(_lx){var _ly=E(_lx);return new F(function(){return A(_lh,[0,_ly[1],_ly[2],_ly[3],_ly[4],_ly[5],_ly[6],_ly[7],_ly[8],_i]);});},_lz=function(_lA){return _lA>0;},_lB=function(_lC){var _lD=B(A(_lC,[_])),_lE=_lD;return E(_lE);},_lF=function(_lG){return new F(function(){return _lB(function(_){var _=0;return new F(function(){return eval(_lG);});});});},_lH=new T(function(){return B(_lF("(function(x) {return x === null;})"));}),_lI=new T(function(){return B(unCStr("No such value"));}),_lJ=[0,_lI],_lK=new T(function(){return B(unCStr("Invalid JSON!"));}),_lL=[0,_lK],_lM=new T(function(){return [0,"(function(k) {return localStorage.getItem(k);})"];}),_lN=function(_lO,_lP,_){var _lQ=B(A(_lF,[E(_lM)[1],E(toJSStr(E(_lP))),_])),_lR=_lQ;return new T(function(){if(!B(_lB(function(_){var _=0,_lS=B(A(_lH,[E(_lR),_])),_lT=_lS;return new T(function(){return B(_lz(_lT));});}))){var _lU=String(_lR),_lV=_lU,_lW=jsParseJSON(_lV),_lX=_lW,_lY=E(_lX),_lZ=_lY[0]==0?E(_lL):B(A(_1F,[_lO,_lY[1]]));}else{var _lZ=E(_lJ);}return _lZ;});},_m0=[0,10],_m1=[1,_m0,_i],_m2=function(_m3,_m4,_){var _m5=jsWriteHandle(E(_m3)[1],toJSStr(E(_m4)));return _4L;},_m6=function(_m7,_m8,_){var _m9=E(_m7),_ma=jsWriteHandle(_m9[1],toJSStr(E(_m8)));return new F(function(){return _m2(_m9,_m1,_);});},_mb=function(_mc){return E(_mc);},_md=function(_me){var _mf=I_decodeDouble(_me);return [0,[1,_mf[2]],_mf[1]];},_mg=function(_mh,_mi){var _mj=E(_mh);if(!_mj[0]){return [0];}else{var _mk=_mj[1];return _mi>1?[1,_mk,new T(function(){return B(_mg(_mj[2],_mi-1|0));})]:[1,_mk,_i];}},_ml=function(_mm){var _mn=hs_intToInt64(2147483647),_mo=_mn,_mp=hs_leInt64(_mm,_mo),_mq=_mp;if(!E(_mq)){return [1,I_fromInt64(_mm)];}else{var _mr=hs_intToInt64(-2147483648),_ms=_mr,_mt=hs_geInt64(_mm,_ms),_mu=_mt;if(!E(_mu)){return [1,I_fromInt64(_mm)];}else{var _mv=hs_int64ToInt(_mm),_mw=_mv;return new F(function(){return _6f(_mw);});}}},_mx=function(_my){var _mz=hs_intToInt64(_my),_mA=_mz;return E(_mA);},_mB=function(_mC){var _mD=E(_mC);return _mD[0]==0?B(_mx(_mD[1])):I_toInt64(_mD[1]);},_mE=new T(function(){return B(_X(0,_28,_i));}),_mF=[0,-1],_mG=new T(function(){return B(_X(0,_mF,_i));}),_mH=new T(function(){return B(unCStr("%.2f"));}),_mI=function(_mJ,_mK){while(1){var _mL=E(_mJ);if(!_mL[0]){return E(_mK);}else{_mJ=_mL[2];var _mM=[1,_mL[1],_mK];_mK=_mM;continue;}}},_mN=function(_mO,_mP){var _mQ=E(_mP);if(!_mQ[0]){return [0,_i,_i];}else{var _mR=_mQ[1];if(!B(A(_mO,[_mR]))){var _mS=new T(function(){var _mT=B(_mN(_mO,_mQ[2]));return [0,_mT[1],_mT[2]];});return [0,[1,_mR,new T(function(){return E(E(_mS)[1]);})],new T(function(){return E(E(_mS)[2]);})];}else{return [0,_i,_mQ];}}},_mU=function(_mV,_mW){var _mX=E(_mV);if(!_mX){return [0,_i,_mW];}else{var _mY=E(_mW);if(!_mY[0]){return [0,_i,_i];}else{var _mZ=new T(function(){var _n0=B(_mU(_mX-1|0,_mY[2]));return [0,_n0[1],_n0[2]];});return [0,[1,_mY[1],new T(function(){return E(E(_mZ)[1]);})],new T(function(){return E(E(_mZ)[2]);})];}}},_n1=function(_n2,_n3){var _n4=function(_n5,_n6){return !B(_4c(_n6,_i))?[0,_n5,new T(function(){var _n7=B(_n1(_n2,_n6));return [1,_n7[1],_n7[2]];})]:[0,_n5,_i];};if(_n2>=0){var _n8=B(_mU(_n2,_n3));return new F(function(){return _n4(_n8[1],_n8[2]);});}else{return new F(function(){return _n4(_i,_n3);});}},_n9=function(_na){var _nb=E(_na);if(!_nb[0]){return [0];}else{return new F(function(){return _C(_nb[1],new T(function(){return B(_n9(_nb[2]));}));});}},_nc=function(_nd){return E(E(_nd)[1])==46?true:false;},_ne=[0,44],_nf=[1,_ne,_i],_ng=function(_nh,_ni){var _nj=E(_ni);return _nj[0]==0?[0]:[1,_nh,[1,_nj[1],new T(function(){return B(_ng(_nh,_nj[2]));})]];},_nk=function(_nl){var _nm=new T(function(){var _nn=B(_mN(_nc,_nl));return [0,_nn[1],_nn[2]];}),_no=B(_n1(3,new T(function(){return B(_mI(E(_nm)[1],_i));})));return new F(function(){return _C(B(_mI(B(_n9([1,_no[1],new T(function(){return B(_ng(_nf,_no[2]));})])),_i)),new T(function(){return E(E(_nm)[2]);}));});},_np=function(_nq,_nr){while(1){var _ns=E(_nq);if(!_ns[0]){_nq=[1,I_fromInt(_ns[1])];continue;}else{return [1,I_shiftLeft(_ns[1],_nr)];}}},_nt=function(_nu){var _nv=E(_nu)[1];return [0,Math.log(_nv+(_nv+1)*Math.sqrt((_nv-1)/(_nv+1)))];},_nw=function(_nx){var _ny=E(_nx)[1];return [0,Math.log(_ny+Math.sqrt(1+_ny*_ny))];},_nz=function(_nA){var _nB=E(_nA)[1];return [0,0.5*Math.log((1+_nB)/(1-_nB))];},_nC=function(_nD,_nE){return [0,Math.log(E(_nE)[1])/Math.log(E(_nD)[1])];},_nF=[0,3.141592653589793],_nG=new T(function(){return [0,0/0];}),_nH=new T(function(){return [0,-1/0];}),_nI=new T(function(){return [0,1/0];}),_nJ=[0,0],_nK=function(_nL,_nM){while(1){var _nN=E(_nL);if(!_nN[0]){_nL=[1,I_fromInt(_nN[1])];continue;}else{var _nO=E(_nM);if(!_nO[0]){_nL=_nN;_nM=[1,I_fromInt(_nO[1])];continue;}else{return new F(function(){return I_fromRat(_nN[1],_nO[1]);});}}}},_nP=function(_nQ,_nR){var _nS=E(_nQ);if(!_nS[0]){var _nT=_nS[1],_nU=E(_nR);return _nU[0]==0?_nT==_nU[1]:I_compareInt(_nU[1],_nT)==0?true:false;}else{var _nV=_nS[1],_nW=E(_nR);return _nW[0]==0?I_compareInt(_nV,_nW[1])==0?true:false:I_compare(_nV,_nW[1])==0?true:false;}},_nX=function(_nY,_nZ){return !B(_nP(_nZ,_nJ))?[0,B(_nK(_nY,_nZ))]:!B(_nP(_nY,_nJ))?!B(_M(_nY,_nJ))?E(_nI):E(_nH):E(_nG);},_o0=function(_o1){var _o2=E(_o1);return new F(function(){return _nX(_o2[1],_o2[2]);});},_o3=function(_o4){return [0,1/E(_o4)[1]];},_o5=function(_o6){var _o7=E(_o6),_o8=_o7[1];return _o8<0?[0, -_o8]:E(_o7);},_o9=function(_oa){var _ob=E(_oa);return _ob[0]==0?_ob[1]:I_toNumber(_ob[1]);},_oc=function(_od){return [0,B(_o9(_od))];},_oe=[0,0],_of=[0,1],_og=[0,-1],_oh=function(_oi){var _oj=E(E(_oi)[1]);return _oj==0?E(_oe):_oj<=0?E(_og):E(_of);},_ok=function(_ol,_om){return [0,E(_ol)[1]-E(_om)[1]];},_on=function(_oo){return [0, -E(_oo)[1]];},_op=function(_oq,_or){return [0,E(_oq)[1]+E(_or)[1]];},_os=function(_ot,_ou){return [0,E(_ot)[1]*E(_ou)[1]];},_ov=[0,_op,_os,_ok,_on,_o5,_oh,_oc],_ow=function(_ox,_oy){return [0,E(_ox)[1]/E(_oy)[1]];},_oz=[0,_ov,_ow,_o3,_o0],_oA=function(_oB){return [0,Math.acos(E(_oB)[1])];},_oC=function(_oD){return [0,Math.asin(E(_oD)[1])];},_oE=function(_oF){return [0,Math.atan(E(_oF)[1])];},_oG=function(_oH){return [0,Math.cos(E(_oH)[1])];},_oI=function(_oJ){return [0,cosh(E(_oJ)[1])];},_oK=function(_oL){return [0,Math.exp(E(_oL)[1])];},_oM=function(_oN){return [0,Math.log(E(_oN)[1])];},_oO=function(_oP,_oQ){return [0,Math.pow(E(_oP)[1],E(_oQ)[1])];},_oR=function(_oS){return [0,Math.sin(E(_oS)[1])];},_oT=function(_oU){return [0,sinh(E(_oU)[1])];},_oV=function(_oW){return [0,Math.sqrt(E(_oW)[1])];},_oX=function(_oY){return [0,Math.tan(E(_oY)[1])];},_oZ=function(_p0){return [0,tanh(E(_p0)[1])];},_p1=[0,_oz,_nF,_oK,_oV,_oM,_oO,_nC,_oR,_oX,_oG,_oC,_oE,_oA,_oT,_oZ,_oI,_nw,_nz,_nt],_p2=function(_p3){var _p4=E(_p3)[1];return [0,Math.log(_p4+(_p4+1)*Math.sqrt((_p4-1)/(_p4+1)))];},_p5=function(_p6){var _p7=E(_p6)[1];return [0,Math.log(_p7+Math.sqrt(1+_p7*_p7))];},_p8=function(_p9){var _pa=E(_p9)[1];return [0,0.5*Math.log((1+_pa)/(1-_pa))];},_pb=function(_pc,_pd){return [0,Math.log(E(_pd)[1])/Math.log(E(_pc)[1])];},_pe=[0,3.141592653589793],_pf=new T(function(){return [0,0/0];}),_pg=new T(function(){return [0,-1/0];}),_ph=new T(function(){return [0,1/0];}),_pi=function(_pj,_pk){return !B(_nP(_pk,_nJ))?[0,B(_nK(_pj,_pk))]:!B(_nP(_pj,_nJ))?!B(_M(_pj,_nJ))?E(_ph):E(_pg):E(_pf);},_pl=function(_pm){var _pn=E(_pm);return new F(function(){return _pi(_pn[1],_pn[2]);});},_po=function(_pp){return [0,1/E(_pp)[1]];},_pq=function(_pr){var _ps=E(_pr),_pt=_ps[1];return _pt<0?[0, -_pt]:E(_ps);},_pu=function(_pv){var _pw=E(_pv);return _pw[0]==0?_pw[1]:I_toNumber(_pw[1]);},_px=function(_py){return [0,B(_pu(_py))];},_pz=[0,0],_pA=[0,1],_pB=[0,-1],_pC=function(_pD){var _pE=E(E(_pD)[1]);return _pE==0?E(_pz):_pE<=0?E(_pB):E(_pA);},_pF=function(_pG,_pH){return [0,E(_pG)[1]-E(_pH)[1]];},_pI=function(_pJ){return [0, -E(_pJ)[1]];},_pK=function(_pL,_pM){return [0,E(_pL)[1]+E(_pM)[1]];},_pN=function(_pO,_pP){return [0,E(_pO)[1]*E(_pP)[1]];},_pQ=[0,_pK,_pN,_pF,_pI,_pq,_pC,_px],_pR=function(_pS,_pT){return [0,E(_pS)[1]/E(_pT)[1]];},_pU=[0,_pQ,_pR,_po,_pl],_pV=function(_pW){return [0,Math.acos(E(_pW)[1])];},_pX=function(_pY){return [0,Math.asin(E(_pY)[1])];},_pZ=function(_q0){return [0,Math.atan(E(_q0)[1])];},_q1=function(_q2){return [0,Math.cos(E(_q2)[1])];},_q3=function(_q4){return [0,cosh(E(_q4)[1])];},_q5=function(_q6){return [0,Math.exp(E(_q6)[1])];},_q7=function(_q8){return [0,Math.log(E(_q8)[1])];},_q9=function(_qa,_qb){return [0,Math.pow(E(_qa)[1],E(_qb)[1])];},_qc=function(_qd){return [0,Math.sin(E(_qd)[1])];},_qe=function(_qf){return [0,sinh(E(_qf)[1])];},_qg=function(_qh){return [0,Math.sqrt(E(_qh)[1])];},_qi=function(_qj){return [0,Math.tan(E(_qj)[1])];},_qk=function(_ql){return [0,tanh(E(_ql)[1])];},_qm=[0,_pU,_pe,_q5,_qg,_q7,_q9,_pb,_qc,_qi,_q1,_pX,_pZ,_pV,_qe,_qk,_q3,_p5,_p8,_p2],_qn=function(_qo){var _qp=B(_md(E(_qo)[1]));return [0,_qp[1],[0,_qp[2]]];},_qq=[0,53],_qr=function(_qs){return E(_qq);},_qt=[0,2],_qu=function(_qv){return E(_qt);},_qw=[0,1024],_qx=[0,-1021],_qy=[0,_qx,_qw],_qz=function(_qA){return E(_qy);},_qB=function(_qC){var _qD=isDoubleInfinite(E(_qC)[1]),_qE=_qD;return E(_qE)==0?false:true;},_qF=function(_qG){var _qH=isDoubleNaN(E(_qG)[1]),_qI=_qH;return E(_qI)==0?false:true;},_qJ=function(_qK){var _qL=isDoubleNegativeZero(E(_qK)[1]),_qM=_qL;return E(_qM)==0?false:true;},_qN=function(_qO){var _qP=decodeFloat(E(_qO)[1]);return [0,new T(function(){return B(_6f(_qP[1]));}),[0,_qP[2]]];},_qQ=[0,24],_qR=function(_qS){return E(_qQ);},_qT=function(_qU){return E(_qt);},_qV=[0,128],_qW=[0,-125],_qX=[0,_qW,_qV],_qY=function(_qZ){return E(_qX);},_r0=function(_r1){var _r2=isFloatInfinite(E(_r1)[1]),_r3=_r2;return E(_r3)==0?false:true;},_r4=function(_r5){var _r6=isFloatNaN(E(_r5)[1]),_r7=_r6;return E(_r7)==0?false:true;},_r8=function(_r9){var _ra=isFloatNegativeZero(E(_r9)[1]),_rb=_ra;return E(_rb)==0?false:true;},_rc=function(_rd,_re){return E(_rd)[1]!=E(_re)[1]?true:false;},_rf=function(_rg,_rh){return E(_rg)[1]==E(_rh)[1];},_ri=[0,_rf,_rc],_rj=function(_rk,_rl){return E(_rk)[1]<E(_rl)[1];},_rm=function(_rn,_ro){return E(_rn)[1]<=E(_ro)[1];},_rp=function(_rq,_rr){return E(_rq)[1]>E(_rr)[1];},_rs=function(_rt,_ru){return E(_rt)[1]>=E(_ru)[1];},_rv=function(_rw,_rx){var _ry=E(_rw)[1],_rz=E(_rx)[1];return _ry>=_rz?_ry!=_rz?2:1:0;},_rA=function(_rB,_rC){var _rD=E(_rB),_rE=E(_rC);return _rD[1]>_rE[1]?E(_rD):E(_rE);},_rF=function(_rG,_rH){var _rI=E(_rG),_rJ=E(_rH);return _rI[1]>_rJ[1]?E(_rJ):E(_rI);},_rK=[0,_ri,_rv,_rj,_rs,_rp,_rm,_rA,_rF],_rL=[0,1],_rM=new T(function(){var _rN=newByteArr(256),_rO=_rN,_=_rO["v"]["i8"][0]=8,_=B((function(_rP,_rQ,_rR,_){while(1){if(_rR>=256){if(_rP>=256){return E(_);}else{var _rS=imul(2,_rP)|0,_rT=_rQ+1|0,_rU=_rP;_rP=_rS;_rQ=_rT;_rR=_rU;continue;}}else{var _=_rO["v"]["i8"][_rR]=_rQ,_rU=_rR+_rP|0;_rR=_rU;continue;}}})(2,0,1,_)),_rV=_rO,_rW=_rV;return [0,_rW];}),_rX=function(_rY,_rZ){while(1){var _s0=(function(_s1,_s2){var _s3=hs_int64ToInt(_s1),_s4=_s3,_s5=E(_rM)[1]["v"]["i8"][(255&_s4>>>0)>>>0&4294967295];if(_s2>_s5){if(_s5>=8){var _s6=hs_uncheckedIShiftRA64(_s1,8),_s7=_s6;_rY=_s7;var _s8=_s2-8|0;_rZ=_s8;return null;}else{return [0,new T(function(){var _s9=hs_uncheckedIShiftRA64(_s1,_s5),_sa=_s9;return B(_ml(_sa));}),_s2-_s5|0];}}else{return [0,new T(function(){var _sb=hs_uncheckedIShiftRA64(_s1,_s2),_sc=_sb;return B(_ml(_sc));}),0];}})(_rY,_rZ);if(_s0!=null){return _s0;}}},_sd=function(_se){return I_toInt(_se)>>>0;},_sf=function(_sg){var _sh=E(_sg);return _sh[0]==0?_sh[1]>>>0:B(_sd(_sh[1]));},_si=function(_sj){var _sk=B(_md(_sj)),_sl=_sk[1],_sm=_sk[2];if(_sm<0){var _sn=function(_so){if(!_so){return [0,E(_sl),B(_np(_rL, -_sm))];}else{var _sp=B(_rX(B(_mB(_sl)), -_sm));return [0,E(_sp[1]),B(_np(_rL,_sp[2]))];}};return (B(_sf(_sl))&1)>>>0==0?B(_sn(1)):B(_sn(0));}else{return [0,B(_np(_sl,_sm)),_rL];}},_sq=function(_sr){var _ss=B(_si(E(_sr)[1]));return [0,E(_ss[1]),E(_ss[2])];},_st=[0,_ov,_rK,_sq],_su=function(_sv){return E(E(_sv)[1]);},_sw=[0,1],_sx=function(_sy,_sz){if(_sy<=_sz){var _sA=function(_sB){return [1,[0,_sB],new T(function(){if(_sB!=_sz){var _sC=B(_sA(_sB+1|0));}else{var _sC=[0];}var _sD=_sC;return _sD;})];};return new F(function(){return _sA(_sy);});}else{return [0];}},_sE=function(_sF){return new F(function(){return _sx(E(_sF)[1],2147483647);});},_sG=function(_sH,_sI,_sJ){return _sJ<=_sI?[1,[0,_sH],new T(function(){var _sK=_sI-_sH|0,_sL=function(_sM){return _sM>=(_sJ-_sK|0)?[1,[0,_sM],new T(function(){return B(_sL(_sM+_sK|0));})]:[1,[0,_sM],_i];};return B(_sL(_sI));})]:_sJ<=_sH?[1,[0,_sH],_i]:[0];},_sN=function(_sO,_sP,_sQ){return _sQ>=_sP?[1,[0,_sO],new T(function(){var _sR=_sP-_sO|0,_sS=function(_sT){return _sT<=(_sQ-_sR|0)?[1,[0,_sT],new T(function(){return B(_sS(_sT+_sR|0));})]:[1,[0,_sT],_i];};return B(_sS(_sP));})]:_sQ>=_sO?[1,[0,_sO],_i]:[0];},_sU=function(_sV,_sW){return _sW<_sV?B(_sG(_sV,_sW,-2147483648)):B(_sN(_sV,_sW,2147483647));},_sX=function(_sY,_sZ){return new F(function(){return _sU(E(_sY)[1],E(_sZ)[1]);});},_t0=function(_t1,_t2,_t3){return _t2<_t1?B(_sG(_t1,_t2,_t3)):B(_sN(_t1,_t2,_t3));},_t4=function(_t5,_t6,_t7){return new F(function(){return _t0(E(_t5)[1],E(_t6)[1],E(_t7)[1]);});},_t8=function(_t9,_ta){return new F(function(){return _sx(E(_t9)[1],E(_ta)[1]);});},_tb=function(_tc){return E(_tc);},_td=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_te=new T(function(){return B(err(_td));}),_tf=function(_tg){var _th=E(E(_tg)[1]);return _th==(-2147483648)?E(_te):[0,_th-1|0];},_ti=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_tj=new T(function(){return B(err(_ti));}),_tk=function(_tl){var _tm=E(E(_tl)[1]);return _tm==2147483647?E(_tj):[0,_tm+1|0];},_tn=[0,_tk,_tf,_tb,_tb,_sE,_sX,_t8,_t4],_to=function(_tp,_tq){if(_tp<=0){if(_tp>=0){return new F(function(){return quot(_tp,_tq);});}else{if(_tq<=0){return new F(function(){return quot(_tp,_tq);});}else{return quot(_tp+1|0,_tq)-1|0;}}}else{if(_tq>=0){if(_tp>=0){return new F(function(){return quot(_tp,_tq);});}else{if(_tq<=0){return new F(function(){return quot(_tp,_tq);});}else{return quot(_tp+1|0,_tq)-1|0;}}}else{return quot(_tp-1|0,_tq)-1|0;}}},_tr=new T(function(){return B(unCStr("ArithException"));}),_ts=new T(function(){return B(unCStr("GHC.Exception"));}),_tt=new T(function(){return B(unCStr("base"));}),_tu=new T(function(){var _tv=hs_wordToWord64(4194982440),_tw=_tv,_tx=hs_wordToWord64(3110813675),_ty=_tx;return [0,_tw,_ty,[0,_tw,_ty,_tt,_ts,_tr],_i];}),_tz=function(_tA){return E(_tu);},_tB=function(_tC){var _tD=E(_tC);return new F(function(){return _2p(B(_2n(_tD[1])),_tz,_tD[2]);});},_tE=new T(function(){return B(unCStr("arithmetic underflow"));}),_tF=new T(function(){return B(unCStr("arithmetic overflow"));}),_tG=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_tH=new T(function(){return B(unCStr("denormal"));}),_tI=new T(function(){return B(unCStr("divide by zero"));}),_tJ=new T(function(){return B(unCStr("loss of precision"));}),_tK=function(_tL){switch(E(_tL)){case 0:return E(_tF);case 1:return E(_tE);case 2:return E(_tJ);case 3:return E(_tI);case 4:return E(_tH);default:return E(_tG);}},_tM=function(_tN){return new F(function(){return _C(_tE,_tN);});},_tO=function(_tN){return new F(function(){return _C(_tF,_tN);});},_tP=function(_tN){return new F(function(){return _C(_tG,_tN);});},_tQ=function(_tN){return new F(function(){return _C(_tH,_tN);});},_tR=function(_tN){return new F(function(){return _C(_tI,_tN);});},_tS=function(_tN){return new F(function(){return _C(_tJ,_tN);});},_tT=function(_tU){switch(E(_tU)){case 0:return E(_tO);case 1:return E(_tM);case 2:return E(_tS);case 3:return E(_tR);case 4:return E(_tQ);default:return E(_tP);}},_tV=function(_tW,_tX){return new F(function(){return _2K(_tT,_tW,_tX);});},_tY=function(_tZ,_u0){switch(E(_u0)){case 0:return E(_tO);case 1:return E(_tM);case 2:return E(_tS);case 3:return E(_tR);case 4:return E(_tQ);default:return E(_tP);}},_u1=[0,_tY,_tK,_tV],_u2=new T(function(){return [0,_tz,_u1,_u3,_tB];}),_u3=function(_tN){return [0,_u2,_tN];},_u4=3,_u5=new T(function(){return B(_u3(_u4));}),_u6=new T(function(){return die(_u5);}),_u7=0,_u8=new T(function(){return B(_u3(_u7));}),_u9=new T(function(){return die(_u8);}),_ua=function(_ub,_uc){var _ud=E(_uc);switch(_ud){case -1:var _ue=E(_ub);return _ue==(-2147483648)?E(_u9):B(_to(_ue,-1));case 0:return E(_u6);default:return new F(function(){return _to(_ub,_ud);});}},_uf=function(_ug,_uh){return [0,B(_ua(E(_ug)[1],E(_uh)[1]))];},_ui=[0,0],_uj=[0,_u9,_ui],_uk=function(_ul,_um){var _un=E(_ul)[1],_uo=E(E(_um)[1]);switch(_uo){case -1:var _up=E(_un);if(_up==(-2147483648)){return E(_uj);}else{if(_up<=0){if(_up>=0){var _uq=quotRemI(_up,-1);return [0,[0,_uq[1]],[0,_uq[2]]];}else{var _ur=quotRemI(_up,-1);return [0,[0,_ur[1]],[0,_ur[2]]];}}else{var _us=quotRemI(_up-1|0,-1);return [0,[0,_us[1]-1|0],[0,(_us[2]+(-1)|0)+1|0]];}}break;case 0:return E(_u6);default:if(_un<=0){if(_un>=0){var _ut=quotRemI(_un,_uo);return [0,[0,_ut[1]],[0,_ut[2]]];}else{if(_uo<=0){var _uu=quotRemI(_un,_uo);return [0,[0,_uu[1]],[0,_uu[2]]];}else{var _uv=quotRemI(_un+1|0,_uo);return [0,[0,_uv[1]-1|0],[0,(_uv[2]+_uo|0)-1|0]];}}}else{if(_uo>=0){if(_un>=0){var _uw=quotRemI(_un,_uo);return [0,[0,_uw[1]],[0,_uw[2]]];}else{if(_uo<=0){var _ux=quotRemI(_un,_uo);return [0,[0,_ux[1]],[0,_ux[2]]];}else{var _uy=quotRemI(_un+1|0,_uo);return [0,[0,_uy[1]-1|0],[0,(_uy[2]+_uo|0)-1|0]];}}}else{var _uz=quotRemI(_un-1|0,_uo);return [0,[0,_uz[1]-1|0],[0,(_uz[2]+_uo|0)+1|0]];}}}},_uA=function(_uB,_uC){var _uD=_uB%_uC;if(_uB<=0){if(_uB>=0){return E(_uD);}else{if(_uC<=0){return E(_uD);}else{var _uE=E(_uD);return _uE==0?0:_uE+_uC|0;}}}else{if(_uC>=0){if(_uB>=0){return E(_uD);}else{if(_uC<=0){return E(_uD);}else{var _uF=E(_uD);return _uF==0?0:_uF+_uC|0;}}}else{var _uG=E(_uD);return _uG==0?0:_uG+_uC|0;}}},_uH=function(_uI,_uJ){var _uK=E(E(_uJ)[1]);switch(_uK){case -1:return E(_ui);case 0:return E(_u6);default:return [0,B(_uA(E(_uI)[1],_uK))];}},_uL=function(_uM,_uN){var _uO=E(_uM)[1],_uP=E(E(_uN)[1]);switch(_uP){case -1:var _uQ=E(_uO);return _uQ==(-2147483648)?E(_u9):[0,quot(_uQ,-1)];case 0:return E(_u6);default:return [0,quot(_uO,_uP)];}},_uR=function(_uS,_uT){var _uU=E(_uS)[1],_uV=E(E(_uT)[1]);switch(_uV){case -1:var _uW=E(_uU);if(_uW==(-2147483648)){return E(_uj);}else{var _uX=quotRemI(_uW,-1);return [0,[0,_uX[1]],[0,_uX[2]]];}break;case 0:return E(_u6);default:var _uY=quotRemI(_uU,_uV);return [0,[0,_uY[1]],[0,_uY[2]]];}},_uZ=function(_v0,_v1){var _v2=E(E(_v1)[1]);switch(_v2){case -1:return E(_ui);case 0:return E(_u6);default:return [0,E(_v0)[1]%_v2];}},_v3=function(_v4){return new F(function(){return _6f(E(_v4)[1]);});},_v5=function(_v6){return [0,E(B(_6f(E(_v6)[1]))),E(_sw)];},_v7=function(_v8,_v9){return [0,imul(E(_v8)[1],E(_v9)[1])|0];},_va=function(_vb,_vc){return [0,E(_vb)[1]+E(_vc)[1]|0];},_vd=function(_ve,_vf){return [0,E(_ve)[1]-E(_vf)[1]|0];},_vg=function(_vh){var _vi=E(_vh),_vj=_vi[1];return _vj<0?[0, -_vj]:E(_vi);},_vk=function(_vl){return [0,B(_7G(_vl))];},_vm=function(_vn){return [0, -E(_vn)[1]];},_vo=[0,-1],_vp=[0,0],_vq=[0,1],_vr=function(_vs){var _vt=E(_vs)[1];return _vt>=0?E(_vt)==0?E(_vp):E(_vq):E(_vo);},_vu=[0,_va,_v7,_vd,_vm,_vg,_vr,_vk],_vv=function(_vw,_vx){return E(_vw)[1]==E(_vx)[1];},_vy=function(_vz,_vA){return E(_vz)[1]!=E(_vA)[1];},_vB=[0,_vv,_vy],_vC=function(_vD,_vE){var _vF=E(_vD),_vG=E(_vE);return _vF[1]>_vG[1]?E(_vF):E(_vG);},_vH=function(_vI,_vJ){var _vK=E(_vI),_vL=E(_vJ);return _vK[1]>_vL[1]?E(_vL):E(_vK);},_vM=function(_vN,_vO){return _vN>=_vO?_vN!=_vO?2:1:0;},_vP=function(_vQ,_vR){return new F(function(){return _vM(E(_vQ)[1],E(_vR)[1]);});},_vS=function(_vT,_vU){return E(_vT)[1]>=E(_vU)[1];},_vV=function(_vW,_vX){return E(_vW)[1]>E(_vX)[1];},_vY=function(_vZ,_w0){return E(_vZ)[1]<=E(_w0)[1];},_w1=function(_w2,_w3){return E(_w2)[1]<E(_w3)[1];},_w4=[0,_vB,_vP,_w1,_vS,_vV,_vY,_vC,_vH],_w5=[0,_vu,_w4,_v5],_w6=[0,_w5,_tn,_uL,_uZ,_uf,_uH,_uR,_uk,_v3],_w7=function(_w8){return E(E(_w8)[1]);},_w9=function(_wa,_wb,_wc){while(1){if(!(_wb%2)){var _wd=B(_6h(_wa,_wa)),_we=quot(_wb,2);_wa=_wd;_wb=_we;continue;}else{var _wf=E(_wb);if(_wf==1){return new F(function(){return _6h(_wa,_wc);});}else{var _wd=B(_6h(_wa,_wa));_wb=quot(_wf-1|0,2);var _wg=B(_6h(_wa,_wc));_wa=_wd;_wc=_wg;continue;}}}},_wh=function(_wi,_wj){while(1){if(!(_wj%2)){var _wk=B(_6h(_wi,_wi)),_wl=quot(_wj,2);_wi=_wk;_wj=_wl;continue;}else{var _wm=E(_wj);if(_wm==1){return E(_wi);}else{return new F(function(){return _w9(B(_6h(_wi,_wi)),quot(_wm-1|0,2),_wi);});}}}},_wn=function(_wo){return E(E(_wo)[2]);},_wp=function(_wq){return E(E(_wq)[1]);},_wr=function(_ws){return E(E(_ws)[2]);},_wt=[0,0],_wu=[0,2],_wv=function(_ww){return E(E(_ww)[7]);},_wx=function(_wy,_wz,_wA,_wB,_wC){return new F(function(){return A(E(E(_wz)[1])[1],[new T(function(){return B(A(_wB,[_wC,new T(function(){return B(A(_wv,[_wy,_wu]));})]));}),new T(function(){return B(A(_wv,[_wy,_wt]));})]);});},_wD=function(_wE){return E(E(_wE)[3]);},_wF=new T(function(){return B(unCStr("Negative exponent"));}),_wG=new T(function(){return B(err(_wF));}),_wH=function(_wI,_wJ,_wK,_wL){var _wM=B(_su(_wJ)),_wN=_wM[1],_wO=E(_wM[2]);if(!B(A(_wO[3],[_wL,new T(function(){return B(A(_wv,[_wN,_wt]));})]))){if(!B(A(E(_wO[1])[1],[_wL,new T(function(){return B(A(_wv,[_wN,_wt]));})]))){var _wP=B(_su(_wJ)),_wQ=_wP[1],_wR=new T(function(){return B(_su(_wJ));}),_wS=new T(function(){return B(_w7(_wR));});return new F(function(){return (function(_wT,_wU){while(1){var _wV=(function(_wW,_wX){var _wY=E(_wJ),_wZ=_wY[3],_x0=E(_wY[1]);if(!B(_wx(_x0[1],_x0[2],_x0[3],_wY[4],_wX))){return !B(A(E(E(_wP[2])[1])[1],[_wX,new T(function(){return B(A(_wv,[_wQ,_sw]));})]))?B((function(_x1,_x2,_x3){while(1){var _x4=(function(_x5,_x6,_x7){var _x8=E(_wJ),_x9=_x8[3],_xa=E(_x8[1]);if(!B(_wx(_xa[1],_xa[2],_xa[3],_x8[4],_x6))){if(!B(A(new T(function(){return B(_6X(new T(function(){return B(_wp(new T(function(){return B(_wr(_wR));})));})));}),[_x6,new T(function(){return B(A(_wv,[_wS,_sw]));})]))){_x1=new T(function(){return B(A(new T(function(){return B(_wn(_wI));}),[_x5,_x5]));});_x2=new T(function(){return B(A(_x9,[new T(function(){return B(A(new T(function(){return B(_wD(_wS));}),[_x6,new T(function(){return B(A(_wv,[_wS,_sw]));})]));}),new T(function(){return B(A(_wv,[_wS,_wu]));})]));});_x3=new T(function(){return B(A(new T(function(){return B(_wn(_wI));}),[_x5,_x7]));});return null;}else{return new F(function(){return A(new T(function(){return B(_wn(_wI));}),[_x5,_x7]);});}}else{_x1=new T(function(){return B(A(new T(function(){return B(_wn(_wI));}),[_x5,_x5]));});_x2=new T(function(){return B(A(_x9,[_x6,new T(function(){return B(A(_wv,[_wS,_wu]));})]));});var _xb=_x7;_x3=_xb;return null;}})(_x1,_x2,_x3);if(_x4!=null){return _x4;}}})(new T(function(){return B(A(new T(function(){return B(_wn(_wI));}),[_wW,_wW]));}),new T(function(){return B(A(_wZ,[new T(function(){return B(A(new T(function(){return B(_wD(_wQ));}),[_wX,new T(function(){return B(A(_wv,[_wQ,_sw]));})]));}),new T(function(){return B(A(_wv,[_wQ,_wu]));})]));}),_wW)):E(_wW);}else{_wT=new T(function(){return B(A(new T(function(){return B(_wn(_wI));}),[_wW,_wW]));});_wU=new T(function(){return B(A(_wZ,[_wX,new T(function(){return B(A(_wv,[_wQ,_wu]));})]));});return null;}})(_wT,_wU);if(_wV!=null){return _wV;}}})(_wK,_wL);});}else{return new F(function(){return A(_wv,[_wI,_sw]);});}}else{return E(_wG);}},_xc=new T(function(){return B(err(_wF));}),_xd=function(_xe,_xf){var _xg=E(_xe);return _xg[0]==0?_xg[1]*Math.pow(2,_xf):I_toNumber(_xg[1])*Math.pow(2,_xf);},_xh=function(_xi,_xj){while(1){var _xk=E(_xi);if(!_xk[0]){var _xl=E(_xk[1]);if(_xl==(-2147483648)){_xi=[1,I_fromInt(-2147483648)];continue;}else{var _xm=E(_xj);if(!_xm[0]){var _xn=_xm[1];return [0,[0,quot(_xl,_xn)],[0,_xl%_xn]];}else{_xi=[1,I_fromInt(_xl)];_xj=_xm;continue;}}}else{var _xo=E(_xj);if(!_xo[0]){_xi=_xk;_xj=[1,I_fromInt(_xo[1])];continue;}else{var _xp=I_quotRem(_xk[1],_xo[1]);return [0,[1,_xp[1]],[1,_xp[2]]];}}}},_xq=function(_xr,_xs){var _xt=B(_md(_xs)),_xu=_xt[1],_xv=_xt[2],_xw=new T(function(){return B(_w7(new T(function(){return B(_su(_xr));})));});if(_xv<0){var _xx= -_xv;if(_xx>=0){var _xy=E(_xx),_xz=_xy==0?E(_sw):B(_wh(_qt,_xy));if(!B(_nP(_xz,_nJ))){var _xA=B(_xh(_xu,_xz));return [0,new T(function(){return B(A(_wv,[_xw,_xA[1]]));}),new T(function(){return [0,B(_xd(_xA[2],_xv))];})];}else{return E(_u6);}}else{return E(_xc);}}else{return [0,new T(function(){return B(A(_wn,[_xw,new T(function(){return B(A(_wv,[_xw,_xu]));}),new T(function(){return B(_wH(_xw,_w6,new T(function(){return B(A(_wv,[_xw,_qt]));}),[0,_xv]));})]));}),_oe];}},_xB=function(_xC,_xD){var _xE=B(_xq(_xC,E(_xD)[1])),_xF=_xE[1];if(E(_xE[2])[1]<=0){return E(_xF);}else{var _xG=E(B(_su(_xC))[1]);return new F(function(){return A(_xG[1],[_xF,new T(function(){return B(A(_xG[7],[_rL]));})]);});}},_xH=function(_xI,_xJ){var _xK=B(_xq(_xI,E(_xJ)[1])),_xL=_xK[1];if(E(_xK[2])[1]>=0){return E(_xL);}else{var _xM=E(B(_su(_xI))[1]);return new F(function(){return A(_xM[3],[_xL,new T(function(){return B(A(_xM[7],[_rL]));})]);});}},_xN=function(_xO,_xP){var _xQ=B(_xq(_xO,E(_xP)[1]));return [0,_xQ[1],_xQ[2]];},_xR=function(_xS,_xT){var _xU=B(_xq(_xS,_xT)),_xV=_xU[1],_xW=E(_xU[2])[1],_xX=new T(function(){var _xY=E(B(_su(_xS))[1]),_xZ=_xY[7];return _xW>=0?B(A(_xY[1],[_xV,new T(function(){return B(A(_xZ,[_rL]));})])):B(A(_xY[3],[_xV,new T(function(){return B(A(_xZ,[_rL]));})]));});if(_xW<0){var _y0= -_xW-0.5;if(_y0>=0){if(!E(_y0)){var _y1=E(_xS),_y2=E(_y1[1]);return !B(_wx(_y2[1],_y2[2],_y2[3],_y1[4],_xV))?E(_xX):E(_xV);}else{return E(_xX);}}else{return E(_xV);}}else{var _y3=_xW-0.5;if(_y3>=0){if(!E(_y3)){var _y4=E(_xS),_y5=E(_y4[1]);return !B(_wx(_y5[1],_y5[2],_y5[3],_y4[4],_xV))?E(_xX):E(_xV);}else{return E(_xX);}}else{return E(_xV);}}},_y6=function(_y7,_y8){return new F(function(){return _xR(_y7,E(_y8)[1]);});},_y9=function(_ya,_yb){return E(B(_xq(_ya,E(_yb)[1]))[1]);},_yc=[0,_st,_oz,_xN,_y9,_y6,_xB,_xH],_yd=function(_ye,_yf){return E(_ye)[1]!=E(_yf)[1]?true:false;},_yg=function(_yh,_yi){return E(_yh)[1]==E(_yi)[1];},_yj=[0,_yg,_yd],_yk=function(_yl,_ym){return E(_yl)[1]<E(_ym)[1];},_yn=function(_yo,_yp){return E(_yo)[1]<=E(_yp)[1];},_yq=function(_yr,_ys){return E(_yr)[1]>E(_ys)[1];},_yt=function(_yu,_yv){return E(_yu)[1]>=E(_yv)[1];},_yw=function(_yx,_yy){var _yz=E(_yx)[1],_yA=E(_yy)[1];return _yz>=_yA?_yz!=_yA?2:1:0;},_yB=function(_yC,_yD){var _yE=E(_yC),_yF=E(_yD);return _yE[1]>_yF[1]?E(_yE):E(_yF);},_yG=function(_yH,_yI){var _yJ=E(_yH),_yK=E(_yI);return _yJ[1]>_yK[1]?E(_yK):E(_yJ);},_yL=[0,_yj,_yw,_yk,_yt,_yq,_yn,_yB,_yG],_yM=function(_yN,_yO){while(1){var _yP=(function(_yQ,_yR){var _yS=E(_rM)[1]["v"]["i8"][(255&_yQ>>>0)>>>0&4294967295];if(_yR>_yS){if(_yS>=8){var _yT=_yQ>>8,_yU=_yR-8|0;_yN=_yT;_yO=_yU;return null;}else{return [0,new T(function(){return B(_6f(_yQ>>_yS));}),_yR-_yS|0];}}else{return [0,new T(function(){return B(_6f(_yQ>>_yR));}),0];}})(_yN,_yO);if(_yP!=null){return _yP;}}},_yV=function(_yW){var _yX=decodeFloat(_yW),_yY=_yX[1],_yZ=_yX[2];if(_yZ<0){var _z0=function(_z1){if(!_z1){return [0,B(_6f(_yY)),B(_np(_rL, -_yZ))];}else{var _z2=B(_yM(_yY, -_yZ));return [0,E(_z2[1]),B(_np(_rL,_z2[2]))];}};return (_yY>>>0&1)>>>0==0?B(_z0(1)):B(_z0(0));}else{return [0,B(_np(B(_6f(_yY)),_yZ)),_rL];}},_z3=function(_z4){var _z5=B(_yV(E(_z4)[1]));return [0,E(_z5[1]),E(_z5[2])];},_z6=[0,_pQ,_yL,_z3],_z7=[0,-1],_z8=[0,1],_z9=function(_za,_zb){var _zc=E(_za);return _zc[0]==0?_zc[1]*Math.pow(2,_zb):I_toNumber(_zc[1])*Math.pow(2,_zb);},_zd=[0,0],_ze=function(_zf,_zg){var _zh=decodeFloat(_zg),_zi=_zh[1],_zj=_zh[2],_zk=new T(function(){return B(_w7(new T(function(){return B(_su(_zf));})));});if(_zj<0){var _zl=new T(function(){if(_zi<0){var _zm= -_zj;if(_zm<32){var _zn=[0, -( -_zi>>_zm)];}else{var _zn= -_zi>=0?E(_zd):E(_z8);}var _zo=_zn,_zp=_zo,_zq=_zp;}else{var _zr= -_zj;if(_zr<32){var _zs=[0,_zi>>_zr];}else{var _zs=_zi>=0?E(_zd):E(_z7);}var _zt=_zs,_zu=_zt,_zq=_zu;}var _zv=_zq;return _zv;});return [0,new T(function(){return B(A(_wv,[_zk,new T(function(){return B(_6f(E(_zl)[1]));})]));}),new T(function(){var _zw= -_zj;if(_zw<32){var _zx=[0,B(_z9(B(_6f(_zi-(E(_zl)[1]<<_zw)|0)),_zj))];}else{var _zx=[0,B(_z9(B(_6f(_zi)),_zj))];}var _zy=_zx,_zz=_zy,_zA=_zz;return _zA;})];}else{return [0,new T(function(){return B(A(_wn,[_zk,new T(function(){return B(A(_wv,[_zk,new T(function(){return B(_6f(_zi));})]));}),new T(function(){return B(_wH(_zk,_w6,new T(function(){return B(A(_wv,[_zk,_qt]));}),[0,_zj]));})]));}),_pz];}},_zB=function(_zC,_zD){var _zE=B(_ze(_zC,E(_zD)[1])),_zF=_zE[1];if(E(_zE[2])[1]<=0){return E(_zF);}else{var _zG=E(B(_su(_zC))[1]);return new F(function(){return A(_zG[1],[_zF,new T(function(){return B(A(_zG[7],[_rL]));})]);});}},_zH=function(_zI,_zJ){var _zK=B(_ze(_zI,E(_zJ)[1])),_zL=_zK[1];if(E(_zK[2])[1]>=0){return E(_zL);}else{var _zM=E(B(_su(_zI))[1]);return new F(function(){return A(_zM[3],[_zL,new T(function(){return B(A(_zM[7],[_rL]));})]);});}},_zN=function(_zO,_zP){var _zQ=B(_ze(_zO,E(_zP)[1]));return [0,_zQ[1],_zQ[2]];},_zR=function(_zS,_zT){var _zU=B(_ze(_zS,_zT)),_zV=_zU[1],_zW=E(_zU[2])[1],_zX=new T(function(){var _zY=E(B(_su(_zS))[1]),_zZ=_zY[7];return _zW>=0?B(A(_zY[1],[_zV,new T(function(){return B(A(_zZ,[_rL]));})])):B(A(_zY[3],[_zV,new T(function(){return B(A(_zZ,[_rL]));})]));});if(_zW<0){var _A0= -_zW-0.5;if(_A0>=0){if(!E(_A0)){var _A1=E(_zS),_A2=E(_A1[1]);return !B(_wx(_A2[1],_A2[2],_A2[3],_A1[4],_zV))?E(_zX):E(_zV);}else{return E(_zX);}}else{return E(_zV);}}else{var _A3=_zW-0.5;if(_A3>=0){if(!E(_A3)){var _A4=E(_zS),_A5=E(_A4[1]);return !B(_wx(_A5[1],_A5[2],_A5[3],_A4[4],_zV))?E(_zX):E(_zV);}else{return E(_zX);}}else{return E(_zV);}}},_A6=function(_A7,_A8){return new F(function(){return _zR(_A7,E(_A8)[1]);});},_A9=function(_Aa,_Ab){return E(B(_ze(_Aa,E(_Ab)[1]))[1]);},_Ac=[0,_z6,_pU,_zN,_A9,_A6,_zB,_zH],_Ad=function(_Ae){return new F(function(){return err(B(unAppCStr("Char.intToDigit: not a digit ",new T(function(){if(_Ae>=0){var _Af=jsShowI(_Ae),_Ag=_Af,_Ah=fromJSStr(_Ag);}else{var _Ai=jsShowI(_Ae),_Aj=_Ai,_Ah=fromJSStr(_Aj);}var _Ak=_Ah;return _Ak;}))));});},_Al=function(_Am){var _An=function(_Ao){if(_Am<10){return new F(function(){return _Ad(_Am);});}else{if(_Am>15){return new F(function(){return _Ad(_Am);});}else{return (97+_Am|0)-10|0;}}};if(_Am<0){return new F(function(){return _An(_);});}else{if(_Am>9){return new F(function(){return _An(_);});}else{return 48+_Am|0;}}},_Ap=function(_Aq){return [0,B(_Al(E(_Aq)[1]))];},_Ar=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_As=function(_At){return new F(function(){return _34([0,new T(function(){return B(_3j(_At,_Ar));})],_31);});},_Au=new T(function(){return B(_As("GHC/Float.lhs:619:11-64|d : ds\'"));}),_Av=function(_Aw,_Ax){if(E(_Aw)[1]<=0){var _Ay=B(_1j(_Ap,[1,_zd,_Ax]));return _Ay[0]==0?E(_Au):[0,_Ay[1],_Ay[2]];}else{var _Az=B(_1j(_Ap,_Ax));return _Az[0]==0?E(_Au):[0,_Az[1],_Az[2]];}},_AA=function(_AB){return E(E(_AB)[1]);},_AC=function(_AD){return E(E(_AD)[1]);},_AE=function(_AF){return E(E(_AF)[1]);},_AG=[0,48],_AH=[1,_AG,_i],_AI=[0,46],_AJ=function(_AK,_AL,_AM){while(1){var _AN=(function(_AO,_AP,_AQ){var _AR=E(_AO);if(!_AR){var _AS=B(_mI(_AP,_i));return _AS[0]==0?[1,_AG,[1,_AI,new T(function(){var _AT=E(_AQ);return _AT[0]==0?E(_AH):E(_AT);})]]:B(_C(_AS,[1,_AI,new T(function(){var _AU=E(_AQ);return _AU[0]==0?E(_AH):E(_AU);})]));}else{var _AV=E(_AQ);if(!_AV[0]){_AK=_AR-1|0;var _AW=[1,_AG,_AP];_AM=_i;_AL=_AW;return null;}else{_AK=_AR-1|0;var _AW=[1,_AV[1],_AP];_AM=_AV[2];_AL=_AW;return null;}}})(_AK,_AL,_AM);if(_AN!=null){return _AN;}}},_AX=[0,0],_AY=new T(function(){return B(unCStr(" out of range "));}),_AZ=new T(function(){return B(unCStr("}.index: Index "));}),_B0=new T(function(){return B(unCStr("Ix{"));}),_B1=[1,_U,_i],_B2=[1,_U,_B1],_B3=function(_B4,_B5,_B6,_B7,_B8){return new F(function(){return err(B(_C(_B0,new T(function(){return B(_C(_B4,new T(function(){return B(_C(_AZ,[1,_V,new T(function(){return B(A(_B8,[_AX,_B5,[1,_U,new T(function(){return B(_C(_AY,[1,_V,[1,_V,new T(function(){return B(A(_kj,[_k9,[1,new T(function(){return B(A(_B8,[_jE,_B6]));}),[1,new T(function(){return B(A(_B8,[_jE,_B7]));}),_i]],_B2]));})]]));})]]));})]));})));}))));});},_B9=function(_Ba,_Bb,_Bc,_Bd){var _Be=E(_Bc);return new F(function(){return _B3(_Ba,_Bb,_Be[1],_Be[2],E(_Bd)[1]);});},_Bf=function(_Bg,_Bh,_Bi,_Bj){return new F(function(){return _B9(_Bj,_Bi,_Bh,_Bg);});},_Bk=new T(function(){return B(unCStr("Int"));}),_Bl=function(_Bm,_Bn,_Bo){return new F(function(){return _Bf(_k8,[0,_Bn,_Bo],_Bm,_Bk);});},_Bp=new T(function(){return B(unCStr("(Array.!): undefined array element"));}),_Bq=new T(function(){return B(err(_Bp));}),_Br=[0,1100],_Bs=[0,_zd,_Br],_Bt=function(_Bu){return new F(function(){return _Bf(_k8,_Bs,[0,_Bu],_Bk);});},_Bv=function(_){var _Bw=newArr(1101,_Bq),_Bx=_Bw;return new F(function(){return (function(_By,_){while(1){var _Bz=(function(_BA,_){if(0>_BA){return new F(function(){return _Bt(_BA);});}else{if(_BA>1100){return new F(function(){return _Bt(_BA);});}else{var _=_Bx[_BA]=new T(function(){if(_BA>=0){var _BB=E(_BA),_BC=_BB==0?E(_sw):B(_wh(_qt,_BB));}else{var _BC=E(_xc);}var _BD=_BC;return _BD;}),_BE=E(_BA);if(_BE==1100){var _BF=_Bx,_BG=_BF;return [0,E(_zd),E(_Br),1101,_BG];}else{_By=_BE+1|0;return null;}}}})(_By,_);if(_Bz!=null){return _Bz;}}})(0,_);});},_BH=function(_BI){var _BJ=B(A(_BI,[_])),_BK=_BJ;return E(_BK);},_BL=new T(function(){return B(_BH(_Bv));}),_BM=[0,10],_BN=[0,324],_BO=[0,_zd,_BN],_BP=function(_BQ){return new F(function(){return _Bf(_k8,_BO,[0,_BQ],_Bk);});},_BR=function(_){var _BS=newArr(325,_Bq),_BT=_BS;return new F(function(){return (function(_BU,_){while(1){var _BV=(function(_BW,_){if(0>_BW){return new F(function(){return _BP(_BW);});}else{if(_BW>324){return new F(function(){return _BP(_BW);});}else{var _=_BT[_BW]=new T(function(){if(_BW>=0){var _BX=E(_BW),_BY=_BX==0?E(_sw):B(_wh(_BM,_BX));}else{var _BY=E(_xc);}var _BZ=_BY;return _BZ;}),_C0=E(_BW);if(_C0==324){var _C1=_BT,_C2=_C1;return [0,E(_zd),E(_BN),325,_C2];}else{_BU=_C0+1|0;return null;}}}})(_BU,_);if(_BV!=null){return _BV;}}})(0,_);});},_C3=new T(function(){return B(_BH(_BR));}),_C4=function(_C5,_C6){var _C7=[0,_C6],_C8=function(_C9){if(!B(_nP(_C5,_BM))){if(_C6>=0){var _Ca=E(_C6);return _Ca==0?E(_sw):B(_wh(_C5,_Ca));}else{return E(_xc);}}else{if(_C6>324){if(_C6>=0){var _Cb=E(_C6);return _Cb==0?E(_sw):B(_wh(_C5,_Cb));}else{return E(_xc);}}else{var _Cc=E(_C3),_Cd=E(_Cc[1]),_Ce=_Cd[1],_Cf=E(_Cc[2]);if(_Ce>_C6){return new F(function(){return _Bl(_C7,_Cd,_Cf);});}else{if(_C6>_Cf[1]){return new F(function(){return _Bl(_C7,_Cd,_Cf);});}else{return E(_Cc[4][_C6-_Ce|0]);}}}}};if(!B(_nP(_C5,_qt))){return new F(function(){return _C8(_);});}else{if(_C6<0){return new F(function(){return _C8(_);});}else{if(_C6>1100){return new F(function(){return _C8(_);});}else{var _Cg=E(_BL),_Ch=E(_Cg[1]),_Ci=_Ch[1],_Cj=E(_Cg[2]);if(_Ci>_C6){return new F(function(){return _Bl(_C7,_Ch,_Cj);});}else{if(_C6>_Cj[1]){return new F(function(){return _Bl(_C7,_Ch,_Cj);});}else{return E(_Cg[4][_C6-_Ci|0]);}}}}}},_Ck=function(_Cl,_Cm){var _Cn=E(_Cl);if(!_Cn[0]){var _Co=_Cn[1],_Cp=E(_Cm);return _Cp[0]==0?_Co>_Cp[1]:I_compareInt(_Cp[1],_Co)<0;}else{var _Cq=_Cn[1],_Cr=E(_Cm);return _Cr[0]==0?I_compareInt(_Cq,_Cr[1])>0:I_compare(_Cq,_Cr[1])>0;}},_Cs=[1,_zd,_i],_Ct=function(_Cu,_Cv){while(1){var _Cw=E(_Cu);if(!_Cw[0]){var _Cx=E(_Cw[1]);if(_Cx==(-2147483648)){_Cu=[1,I_fromInt(-2147483648)];continue;}else{var _Cy=E(_Cv);if(!_Cy[0]){return [0,quot(_Cx,_Cy[1])];}else{_Cu=[1,I_fromInt(_Cx)];_Cv=_Cy;continue;}}}else{var _Cz=_Cw[1],_CA=E(_Cv);return _CA[0]==0?[0,I_toInt(I_quot(_Cz,I_fromInt(_CA[1])))]:[1,I_quot(_Cz,_CA[1])];}}},_CB=function(_CC,_CD,_CE,_CF,_CG,_CH,_CI,_CJ){if(!B(A(_CC,[_CJ,new T(function(){return B(A(_wv,[B(_AC(B(_AA(_CD)))),_nJ]));})]))){var _CK=new T(function(){return B(A(_CE,[_CJ]));}),_CL=new T(function(){return B(A(_CF,[_CJ]));}),_CM=new T(function(){return [0,E(B(A(_CG,[_CJ]))[1])[1]-E(_CL)[1]|0];}),_CN=new T(function(){return B(A(_CH,[_CJ]));}),_CO=new T(function(){return E(E(_CN)[2]);}),_CP=new T(function(){var _CQ=E(_CO),_CR=_CQ[1],_CS=E(_CM)[1]-_CR|0;if(_CS<=0){var _CT=[0,new T(function(){return E(E(_CN)[1]);}),_CQ];}else{var _CT=[0,new T(function(){var _CU=B(_C4(_CK,_CS));if(!B(_nP(_CU,_nJ))){var _CV=B(_Ct(E(_CN)[1],_CU));}else{var _CV=E(_u6);}var _CW=_CV;return _CW;}),[0,_CR+_CS|0]];}var _CX=_CT,_CY=_CX,_CZ=_CY,_D0=_CZ;return _D0;}),_D1=new T(function(){return E(E(_CP)[2]);}),_D2=new T(function(){return E(E(_CP)[1]);}),_D3=new T(function(){var _D4=E(_D1)[1];if(_D4<0){if(_D4<=E(_CM)[1]){var _D5=[0,new T(function(){return B(_6h(_D2,_qt));}),new T(function(){return B(_6h(B(_C4(_CK, -_D4)),_qt));}),_rL,_rL];}else{var _D5=!B(_nP(_D2,B(_C4(_CK,E(_CL)[1]-1|0))))?[0,new T(function(){return B(_6h(_D2,_qt));}),new T(function(){return B(_6h(B(_C4(_CK, -_D4)),_qt));}),_rL,_rL]:[0,new T(function(){return B(_6h(B(_6h(_D2,_CK)),_qt));}),new T(function(){return B(_6h(B(_C4(_CK, -_D4+1|0)),_qt));}),_CK,_rL];}var _D6=_D5,_D7=_D6,_D8=_D7;}else{var _D9=new T(function(){return B(_C4(_CK,_D4));}),_D8=!B(_nP(_D2,B(_C4(_CK,E(_CL)[1]-1|0))))?[0,new T(function(){return B(_6h(B(_6h(_D2,_D9)),_qt));}),_qt,_D9,_D9]:[0,new T(function(){return B(_6h(B(_6h(B(_6h(_D2,_D9)),_CK)),_qt));}),new T(function(){return B(_6h(_qt,_CK));}),new T(function(){return B(_6h(_D9,_CK));}),_D9];}var _Da=_D8,_Db=_Da;return _Db;}),_Dc=new T(function(){return E(E(_D3)[2]);}),_Dd=new T(function(){return E(E(_D3)[3]);}),_De=new T(function(){return E(E(_D3)[1]);}),_Df=new T(function(){var _Dg=new T(function(){return B(_5Z(_De,_Dd));}),_Dh=function(_Di){var _Dj=(Math.log(B(_pu(B(_5Z(_D2,_rL)))))+E(_D1)[1]*Math.log(B(_pu(_CK))))/Math.log(B(_pu(_CI))),_Dk=_Dj&4294967295;return _Dk>=_Dj?E(_Dk):_Dk+1|0;},_Dl=function(_Dm){while(1){if(_Dm<0){if(!B(_7J(B(_6h(B(_C4(_CI, -_Dm)),_Dg)),_Dc))){var _Dn=_Dm+1|0;_Dm=_Dn;continue;}else{return E(_Dm);}}else{if(!B(_7J(_Dg,B(_6h(B(_C4(_CI,_Dm)),_Dc))))){var _Dn=_Dm+1|0;_Dm=_Dn;continue;}else{return E(_Dm);}}}};if(!B(_nP(_CK,_qt))){var _Do=[0,B(_Dl(B(_Dh(_))))];}else{if(!B(_nP(_CI,_BM))){var _Dp=[0,B(_Dl(B(_Dh(_))))];}else{var _Dq=(E(_CL)[1]-1|0)+E(_CO)[1]|0;if(_Dq<0){var _Dr=[0,B(_Dl(quot(imul(_Dq,8651)|0,28738)))];}else{var _Dr=[0,B(_Dl(quot(imul(_Dq,8651)|0,28738)+1|0))];}var _Ds=_Dr,_Dt=_Ds,_Du=_Dt,_Dv=_Du,_Dw=_Dv,_Dp=_Dw;}var _Do=_Dp;}return _Do;});return [0,new T(function(){var _Dx=E(_Df)[1],_Dy=function(_Dz,_DA,_DB,_DC,_DD){while(1){var _DE=(function(_DF,_DG,_DH,_DI,_DJ){if(!B(_nP(_DH,_nJ))){var _DK=B(_xh(B(_6h(_DG,_CI)),_DH)),_DL=_DK[1],_DM=_DK[2],_DN=B(_6h(_DJ,_CI)),_DO=B(_6h(_DI,_CI));if(!B(_M(_DM,_DN))){if(!B(_Ck(B(_5Z(_DM,_DO)),_DH))){var _DP=[1,_DL,_DF];_DA=_DM;var _DQ=_DH;_DC=_DO;_DD=_DN;_Dz=_DP;_DB=_DQ;return null;}else{return [1,new T(function(){return B(_5Z(_DL,_rL));}),_DF];}}else{return !B(_Ck(B(_5Z(_DM,_DO)),_DH))?[1,_DL,_DF]:!B(_M(B(_6h(_DM,_qt)),_DH))?[1,new T(function(){return B(_5Z(_DL,_rL));}),_DF]:[1,_DL,_DF];}}else{return E(_u6);}})(_Dz,_DA,_DB,_DC,_DD);if(_DE!=null){return _DE;}}};if(_Dx<0){var _DR=B(_C4(_CI, -_Dx)),_DS=B(_1j(_vk,B(_mI(B(_Dy(_i,B(_6h(_De,_DR)),_Dc,B(_6h(_Dd,_DR)),B(_6h(E(_D3)[4],_DR)))),_i))));}else{var _DS=B(_1j(_vk,B(_mI(B(_Dy(_i,_De,B(_6h(_Dc,B(_C4(_CI,_Dx)))),_Dd,E(_D3)[4])),_i))));}var _DT=_DS,_DU=_DT;return _DU;}),_Df];}else{return [0,_Cs,_zd];}},_DV=function(_DW,_DX){while(1){var _DY=E(_DX);if(!_DY[0]){return true;}else{if(!B(A(_DW,[_DY[1]]))){return false;}else{_DX=_DY[2];continue;}}}},_DZ=function(_E0){return E(_E0)[1]%2==0?true:false;},_E1=new T(function(){return B(unCStr("roundTo: bad Value"));}),_E2=new T(function(){return B(err(_E1));}),_E3=function(_E4){return E(E(_E4)[1])==0?true:false;},_E5=function(_E6){return _E6>1?[1,_zd,new T(function(){return B(_E5(_E6-1|0));})]:E(_Cs);},_E7=function(_E8,_E9,_Ea){var _Eb=function(_Ec,_Ed,_Ee){var _Ef=E(_Ee);if(!_Ef[0]){return [0,_zd,new T(function(){var _Eg=E(_Ec)[1];return _Eg>0?B(_E5(_Eg)):[0];})];}else{var _Eh=_Ef[1],_Ei=_Ef[2],_Ej=E(E(_Ec)[1]);if(!_Ej){var _Ek=E(_Eh)[1],_El=E(new T(function(){return [0,quot(E(_E8)[1],2)];}))[1];return _Ek!=_El?[0,new T(function(){return _Ek<_El?E(_zd):E(_z8);}),_i]:!E(_Ed)?[0,new T(function(){return _Ek<_El?E(_zd):E(_z8);}),_i]:!B(_DV(_E3,_Ei))?[0,new T(function(){return _Ek<_El?E(_zd):E(_z8);}),_i]:[0,_zd,_i];}else{var _Em=B(_Eb([0,_Ej-1|0],new T(function(){return B(_DZ(_Eh));}),_Ei)),_En=_Em[2],_Eo=E(_Em[1])[1]+E(_Eh)[1]|0;return _Eo!=E(_E8)[1]?[0,_zd,[1,[0,_Eo],_En]]:[0,_z8,[1,_zd,_En]];}}},_Ep=B(_Eb(_E9,_b,_Ea));switch(E(E(_Ep[1])[1])){case 0:return E(_Ep);case 1:return [0,_z8,[1,_z8,_Ep[2]]];default:return E(_E2);}},_Eq=function(_Er){return E(E(_Er)[3]);},_Es=0,_Et=1,_Eu=[0,10],_Ev=new T(function(){return B(unCStr("e0"));}),_Ew=function(_Ex,_Ey){var _Ez=E(_Ex);if(!_Ez[0]){return E(_Ev);}else{var _EA=_Ez[1];return _Ey>1?[1,_EA,new T(function(){return B(_Ew(_Ez[2],_Ey-1|0));})]:[1,_EA,_Ev];}},_EB=function(_EC,_ED){var _EE=E(_ED);return _EE[0]==0?[0]:[1,_EC,new T(function(){return B(_EB(_EE[1],_EE[2]));})];},_EF=new T(function(){return B(unCStr("init"));}),_EG=new T(function(){return B(_kf(_EF));}),_EH=new T(function(){return B(_As("GHC/Float.lhs:591:12-70|(d : ds\')"));}),_EI=[0,101],_EJ=new T(function(){return B(unCStr("Infinity"));}),_EK=new T(function(){return B(unCStr("-Infinity"));}),_EL=new T(function(){return B(unCStr("NaN"));}),_EM=new T(function(){return B(unCStr("formatRealFloat/doFmt/FFExponent: []"));}),_EN=new T(function(){return B(err(_EM));}),_EO=new T(function(){return B(unCStr("0.0e0"));}),_EP=function(_EQ){return E(E(_EQ)[4]);},_ER=new T(function(){return [1,_AG,_ER];}),_ES=function(_ET,_EU,_EV,_EW,_EX,_EY,_EZ,_F0,_F1,_F2,_F3,_F4){if(!B(A(_EZ,[_F4]))){var _F5=new T(function(){return B(_AC(new T(function(){return B(_AA(_EU));})));});if(!B(A(_F0,[_F4]))){var _F6=function(_F7,_F8,_F9){while(1){var _Fa=(function(_Fb,_Fc,_Fd){switch(E(_Fb)){case 0:var _Fe=E(_F3);if(!_Fe[0]){var _Ff=B(_1j(_Ap,_Fc));if(!_Ff[0]){return E(_EN);}else{var _Fg=_Ff[2],_Fh=E(_Ff[1]),_Fi=function(_Fj){var _Fk=E(_Fg);return _Fk[0]==0?[1,_Fh,new T(function(){return B(unAppCStr(".0e",new T(function(){return B(_7y(0,E(_Fd)[1]-1|0,_i));})));})]:[1,_Fh,[1,_AI,new T(function(){return B(_C(_Fk,[1,_EI,new T(function(){return B(_7y(0,E(_Fd)[1]-1|0,_i));})]));})]];};return E(_Fh[1])==48?E(_Fg)[0]==0?E(_EO):B(_Fi(_)):B(_Fi(_));}}else{var _Fl=new T(function(){var _Fm=E(_Fe[1]);return _Fm[1]>1?E(_Fm):E(_z8);}),_Fn=function(_Fo){var _Fp=new T(function(){var _Fq=B(_E7(_Eu,new T(function(){return [0,E(_Fl)[1]+1|0];}),_Fc));return [0,_Fq[1],_Fq[2]];}),_Fr=new T(function(){return E(E(_Fp)[1]);}),_Fs=new T(function(){if(E(_Fr)[1]<=0){var _Ft=B(_1j(_Ap,E(_Fp)[2])),_Fu=_Ft[0]==0?E(_EH):[0,_Ft[1],_Ft[2]];}else{var _Fv=E(E(_Fp)[2]);if(!_Fv[0]){var _Fw=E(_EG);}else{var _Fx=B(_1j(_Ap,B(_EB(_Fv[1],_Fv[2])))),_Fw=_Fx[0]==0?E(_EH):[0,_Fx[1],_Fx[2]];}var _Fy=_Fw,_Fu=_Fy;}var _Fz=_Fu,_FA=_Fz;return _FA;});return [1,new T(function(){return E(E(_Fs)[1]);}),[1,_AI,new T(function(){return B(_C(E(_Fs)[2],[1,_EI,new T(function(){return B(_7y(0,(E(_Fd)[1]-1|0)+E(_Fr)[1]|0,_i));})]));})]];},_FB=E(_Fc);if(!_FB[0]){return new F(function(){return _Fn(_);});}else{return E(E(_FB[1])[1])==0?E(_FB[2])[0]==0?[1,_AG,[1,_AI,new T(function(){var _FC=E(_Fl)[1];return _FC>0?B(_Ew(_ER,_FC)):E(_Ev);})]]:B(_Fn(_)):B(_Fn(_));}}break;case 1:var _FD=E(_F3);if(!_FD[0]){var _FE=E(_Fd)[1];return _FE>0?B(_AJ(_FE,_i,new T(function(){return B(_1j(_Ap,_Fc));}))):B(unAppCStr("0.",new T(function(){var _FF= -_FE;if(_FF>0){var _FG=function(_FH){return _FH>1?[1,_AG,new T(function(){return B(_FG(_FH-1|0));})]:E([1,_AG,new T(function(){return B(_1j(_Ap,_Fc));})]);},_FI=B(_FG(_FF));}else{var _FI=B(_1j(_Ap,_Fc));}var _FJ=_FI,_FK=_FJ;return _FK;})));}else{var _FL=_FD[1],_FM=E(_Fd),_FN=_FM[1];if(_FN<0){var _FO=new T(function(){var _FP= -_FN;if(_FP>0){var _FQ=function(_FR){return _FR>1?[1,_zd,new T(function(){return B(_FQ(_FR-1|0));})]:E([1,_zd,_Fc]);},_FS=B(_E7(_Eu,new T(function(){var _FT=E(_FL);return _FT[1]>0?E(_FT):E(_zd);}),B(_FQ(_FP)))),_FU=B(_Av(_FS[1],_FS[2]));}else{var _FV=B(_E7(_Eu,new T(function(){var _FW=E(_FL);return _FW[1]>0?E(_FW):E(_zd);}),_Fc)),_FU=B(_Av(_FV[1],_FV[2]));}var _FX=_FU,_FY=_FX;return _FY;});return [1,new T(function(){return E(E(_FO)[1]);}),new T(function(){var _FZ=E(E(_FO)[2]);return _FZ[0]==0?[0]:[1,_AI,_FZ];})];}else{var _G0=B(_E7(_Eu,new T(function(){var _G1=E(_FL)[1];if(_G1>0){var _G2=[0,_G1+_FN|0];}else{var _G2=E(_FM);}var _G3=_G2,_G4=_G3;return _G4;}),_Fc)),_G5=_G0[2],_G6=_FN+E(_G0[1])[1]|0;if(_G6>=0){var _G7=B(_mU(_G6,new T(function(){return B(_1j(_Ap,_G5));}))),_G8=_G7[2],_G9=E(_G7[1]);return _G9[0]==0?[1,_AG,new T(function(){var _Ga=E(_G8);return _Ga[0]==0?[0]:[1,_AI,_Ga];})]:B(_C(_G9,new T(function(){var _Gb=E(_G8);return _Gb[0]==0?[0]:[1,_AI,_Gb];})));}else{return [1,_AG,new T(function(){var _Gc=B(_1j(_Ap,_G5));return _Gc[0]==0?[0]:[1,_AI,_Gc];})];}}}break;default:var _Gd=E(_Fd),_Ge=_Gd[1];if(_Ge>=0){if(_Ge<=7){_F7=_Et;var _Gf=_Fc;_F9=_Gd;_F8=_Gf;return null;}else{_F7=_Es;var _Gf=_Fc;_F9=_Gd;_F8=_Gf;return null;}}else{_F7=_Es;var _Gf=_Fc;_F9=_Gd;_F8=_Gf;return null;}}})(_F7,_F8,_F9);if(_Fa!=null){return _Fa;}}},_Gg=function(_Gh){return [1,_kR,new T(function(){var _Gi=B(_CB(E(E(E(E(_ET)[1])[2])[1])[1],_EU,_EV,_EW,_EX,_EY,_BM,new T(function(){return B(A(_EP,[_F5,_F4]));})));return B(_F6(_F2,_Gi[1],_Gi[2]));})];};if(!B(A(_Eq,[B(_wr(B(_AE(_ET)))),_F4,new T(function(){return B(A(_wv,[_F5,_nJ]));})]))){if(!B(A(_F1,[_F4]))){var _Gj=B(_CB(E(E(E(E(_ET)[1])[2])[1])[1],_EU,_EV,_EW,_EX,_EY,_BM,_F4));return new F(function(){return _F6(_F2,_Gj[1],_Gj[2]);});}else{return new F(function(){return _Gg(_);});}}else{return new F(function(){return _Gg(_);});}}else{return !B(A(_Eq,[B(_wr(B(_AE(_ET)))),_F4,new T(function(){return B(A(_wv,[_F5,_nJ]));})]))?E(_EJ):E(_EK);}}else{return E(_EL);}},_Gk=function(_Gl){var _Gm=u_towlower(_Gl),_Gn=_Gm;return _Gn>>>0>1114111?B(_7E(_Gn)):_Gn;},_Go=function(_Gp){return new F(function(){return err(B(unAppCStr("Printf.printf: ",_Gp)));});},_Gq=new T(function(){return B(unCStr("bad argument"));}),_Gr=new T(function(){return B(_Go(_Gq));}),_Gs=new T(function(){return B(unCStr("Printf.dfmt: impossible"));}),_Gt=new T(function(){return B(err(_Gs));}),_Gu=[0,45],_Gv=[1,_Gu,_i],_Gw=new T(function(){return B(err(_Gs));}),_Gx=new T(function(){return B(unCStr("Negative exponent"));}),_Gy=new T(function(){return B(err(_Gx));}),_Gz=function(_GA,_GB,_GC){while(1){if(!(_GB%2)){var _GD=_GA*_GA,_GE=quot(_GB,2);_GA=_GD;_GB=_GE;continue;}else{var _GF=E(_GB);if(_GF==1){return _GA*_GC;}else{var _GD=_GA*_GA;_GB=quot(_GF-1|0,2);var _GG=_GA*_GC;_GA=_GD;_GC=_GG;continue;}}}},_GH=function(_GI,_GJ){while(1){if(!(_GJ%2)){var _GK=_GI*_GI,_GL=quot(_GJ,2);_GI=_GK;_GJ=_GL;continue;}else{var _GM=E(_GJ);if(_GM==1){return E(_GI);}else{return new F(function(){return _Gz(_GI*_GI,quot(_GM-1|0,2),_GI);});}}}},_GN=function(_GO,_GP){var _GQ=E(_GO);return _GQ[0]==0?function(_5a){return new F(function(){return _C(new T(function(){var _GR=jsShow(E(_GP)[1]),_GS=_GR;return fromJSStr(_GS);}),_5a);});}:function(_5a){return new F(function(){return _C(new T(function(){var _GT=E(E(_GQ[1])[1]);if(!_GT){var _GU=jsRound(E(_GP)[1]),_GV=_GU,_GW=B(_md(_GV)),_GX=_GW[1],_GY=_GW[2];if(_GY>=0){var _GZ=jsShow(B(_o9(B(_np(_GX,_GY))))),_H0=_GZ,_H1=fromJSStr(_H0);}else{var _H2=hs_uncheckedIShiftRA64(B(_mB(_GX)), -_GY),_H3=_H2,_H4=jsShow(B(_o9(B(_ml(_H3))))),_H5=_H4,_H1=fromJSStr(_H5);}var _H6=_H1,_H7=_H6,_H8=_H7,_H9=_H8;}else{if(_GT>=0){var _Ha=B(_GH(10,_GT)),_Hb=jsRound(E(_GP)[1]*_Ha),_Hc=_Hb,_Hd=jsShow((_Hc&4294967295)/_Ha),_He=_Hd,_Hf=fromJSStr(_He);}else{var _Hf=E(_Gy);}var _Hg=_Hf,_Hh=_Hg,_H9=_Hh;}var _Hi=_H9;return _Hi;}),_5a);});};},_Hj=function(_Hk,_Hl){var _Hm=E(_Hk);return _Hm[0]==0?function(_5a){return new F(function(){return _C(new T(function(){var _Hn=B(_yV(E(_Hl)[1])),_Ho=jsShow(B(_nX(_Hn[1],_Hn[2]))[1]),_Hp=_Ho;return fromJSStr(_Hp);}),_5a);});}:function(_5a){return new F(function(){return _C(new T(function(){var _Hq=E(E(_Hm[1])[1]);if(!_Hq){var _Hr=jsRound(E(_Hl)[1]),_Hs=_Hr,_Ht=decodeFloat(_Hs),_Hu=_Ht[1],_Hv=_Ht[2];if(_Hv>=0){var _Hw=jsShow(B(_o9(B(_np(B(_6f(_Hu)),_Hv))))),_Hx=_Hw,_Hy=fromJSStr(_Hx);}else{var _Hz=jsShow(_Hu>> -_Hv),_HA=_Hz,_Hy=fromJSStr(_HA);}var _HB=_Hy,_HC=_HB,_HD=_HC,_HE=_HD;}else{var _HF=B(_yV(E(_Hl)[1]));if(_Hq>=0){var _HG=B(_GH(10,_Hq)),_HH=jsRound(B(_nX(_HF[1],_HF[2]))[1]*_HG),_HI=_HH,_HJ=jsShow((_HI&4294967295)/_HG),_HK=_HJ,_HL=fromJSStr(_HK);}else{var _HL=E(_Gy);}var _HM=_HL,_HN=_HM,_HO=_HN,_HP=_HO,_HE=_HP;}var _HQ=_HE;return _HQ;}),_5a);});};},_HR=function(_HS){var _HT=u_towupper(_HS),_HU=_HT;return _HU>>>0>1114111?B(_7E(_HU)):_HU;},_HV=function(_HW){return [0,B(_HR(E(_HW)[1]))];},_HX=function(_HY,_HZ,_I0){var _I1=E(_I0);switch(_I1[0]){case 3:var _I2=_I1[1],_I3=u_iswupper(_HY),_I4=_I3;switch(B(_Gk(_HY))){case 101:var _I5=B(_ES(_Ac,_qm,_qT,_qR,_qY,_qN,_r4,_r0,_r8,_Es,new T(function(){var _I6=E(_HZ);return _I6[1]>=0?[1,_I6]:[0];}),_I2));break;case 102:var _I5=B(_ES(_Ac,_qm,_qT,_qR,_qY,_qN,_r4,_r0,_r8,_Et,new T(function(){var _I7=E(_HZ);return _I7[1]>=0?[1,_I7]:[0];}),_I2));break;case 103:var _I8=E(_HZ),_I5=_I8[1]>=0?B(A(_Hj,[[1,_I8],_I2,_i])):B(A(_Hj,[_6F,_I2,_i]));break;default:var _I5=E(_Gw);}var _I9=_I5,_Ia=E(_I4);if(!_Ia){var _Ib=E(_I9);if(!_Ib[0]){return [0,_i,_i];}else{var _Ic=_Ib[1],_Id=_Ib[2],_Ie=E(_Ic),_If=_Ie[1],_Ig=E(_If);return _Ig==45?[0,_Gv,_Id]:[0,_i,_Ib];}}else{var _Ih=B(_1j(_HV,_I9));if(!_Ih[0]){return [0,_i,_i];}else{var _Ii=_Ih[1],_Ij=_Ih[2],_Ik=E(_Ii),_Il=_Ik[1],_Im=E(_Il);return _Im==45?[0,_Gv,_Ij]:[0,_i,_Ih];}}break;case 4:var _In=_I1[1],_Io=u_iswupper(_HY),_Ip=_Io;switch(B(_Gk(_HY))){case 101:var _Iq=B(_ES(_yc,_p1,_qu,_qr,_qz,_qn,_qF,_qB,_qJ,_Es,new T(function(){var _Ir=E(_HZ);return _Ir[1]>=0?[1,_Ir]:[0];}),_In));break;case 102:var _Iq=B(_ES(_yc,_p1,_qu,_qr,_qz,_qn,_qF,_qB,_qJ,_Et,new T(function(){var _Is=E(_HZ);return _Is[1]>=0?[1,_Is]:[0];}),_In));break;case 103:var _It=E(_HZ),_Iq=_It[1]>=0?B(A(_GN,[[1,_It],_In,_i])):B(A(_GN,[_6F,_In,_i]));break;default:var _Iq=E(_Gt);}var _Iu=_Iq,_Iv=E(_Ip);if(!_Iv){var _Iw=E(_Iu);if(!_Iw[0]){return [0,_i,_i];}else{var _Ix=_Iw[1],_Iy=_Iw[2],_Iz=E(_Ix),_IA=_Iz[1],_IB=E(_IA);return _IB==45?[0,_Gv,_Iy]:[0,_i,_Iw];}}else{var _IC=B(_1j(_HV,_Iu));if(!_IC[0]){return [0,_i,_i];}else{var _ID=_IC[1],_IE=_IC[2],_IF=E(_ID),_IG=_IF[1],_IH=E(_IG);return _IH==45?[0,_Gv,_IE]:[0,_i,_IC];}}break;default:return E(_Gr);}},_II=[0,0],_IJ=function(_IK){return new F(function(){return _X(0,_IK,_i);});},_IL=function(_IM,_IN){while(1){var _IO=E(_IM);if(!_IO[0]){return E(_IN);}else{_IM=_IO[2];var _IP=_IN+1|0;_IN=_IP;continue;}}},_IQ=[0,48],_IR=function(_IS,_IT){var _IU=_IS-B(_IL(_IT,0))|0;if(_IU>0){var _IV=function(_IW){return _IW>1?[1,_IQ,new T(function(){return B(_IV(_IW-1|0));})]:E([1,_IQ,_IT]);};return new F(function(){return _IV(_IU);});}else{return E(_IT);}},_IX=[0,0],_IY=[0,-2147483648],_IZ=function(_J0,_J1){while(1){var _J2=(function(_J3,_J4){var _J5=E(_J4);switch(_J5[0]){case 0:_J0=_IX;_J1=[2,_IY,new T(function(){return B(_6f(E(_J5[1])[1]));})];return null;case 2:var _J6=_J5[2];return !B(_M(_J6,_II))?[0,_i,new T(function(){return B(_IR(E(_J3)[1],B(_IJ(_J6))));})]:[0,_Gv,new T(function(){return B(_IR(E(_J3)[1],B(_X(0,B(_69(_J6)),_i))));})];default:return E(_Gr);}})(_J0,_J1);if(_J2!=null){return _J2;}}},_J7=[1,_iP,_i],_J8=function(_J9){return new F(function(){return err(B(unAppCStr("Char.digitToInt: not a digit ",new T(function(){var _Ja=E(_J9);return _Ja==39?E(_iR):[1,_iP,new T(function(){return B(_iz(_Ja,_J7));})];}))));});},_Jb=function(_Jc){var _Jd=function(_Je){var _Jf=function(_Jg){if(_Jc<65){return new F(function(){return _J8(_Jc);});}else{if(_Jc>70){return new F(function(){return _J8(_Jc);});}else{return (_Jc-65|0)+10|0;}}};if(_Jc<97){return new F(function(){return _Jf(_);});}else{if(_Jc>102){return new F(function(){return _Jf(_);});}else{return (_Jc-97|0)+10|0;}}};if(_Jc<48){return new F(function(){return _Jd(_);});}else{if(_Jc>57){return new F(function(){return _Jd(_);});}else{return _Jc-48|0;}}},_Jh=function(_Ji,_Jj){while(1){var _Jk=(function(_Jl,_Jm){var _Jn=E(_Jm);if(!_Jn[0]){return [0,_Jl,_i];}else{var _Jo=E(_Jn[1])[1];if(_Jo<48){return [0,_Jl,_Jn];}else{if(_Jo>57){return [0,_Jl,_Jn];}else{_Ji=new T(function(){return [0,(imul(E(_Jl)[1],10)|0)+B(_Jb(_Jo))|0];});_Jj=_Jn[2];return null;}}}})(_Ji,_Jj);if(_Jk!=null){return _Jk;}}},_Jp=new T(function(){return B(unCStr("argument list ended prematurely"));}),_Jq=new T(function(){return B(_Go(_Jp));}),_Jr=[0,-1],_Js=function(_Jt){return [0,E(_Jt)[1]];},_Ju=function(_Jv){var _Jw=E(_Jv);switch(_Jw[0]){case 0:return new F(function(){return _Js(_Jw[1]);});break;case 2:return new F(function(){return _vk(_Jw[2]);});break;default:return E(_Gr);}},_Jx=function(_Jy,_Jz,_JA,_JB,_JC){while(1){var _JD=(function(_JE,_JF,_JG,_JH,_JI){var _JJ=E(_JH);if(!_JJ[0]){return [0,_IX,_Jr,_JE,_JF,_JG,_i,_JI];}else{var _JK=_JJ[2],_JL=E(E(_JJ[1])[1]);switch(_JL){case 42:var _JM=new T(function(){var _JN=E(_JI);return _JN[0]==0?E(_Jq):[0,_JN[2],new T(function(){return B(_Ju(_JN[1]));})];}),_JO=new T(function(){var _JP=E(_JK);if(!_JP[0]){var _JQ=[0,_Jr,_i,new T(function(){return E(E(_JM)[1]);})];}else{if(E(E(_JP[1])[1])==46){var _JR=E(_JP[2]);if(!_JR[0]){var _JS=B(_Jh(_IX,_i)),_JT=[0,_JS[1],_JS[2],new T(function(){return E(E(_JM)[1]);})];}else{if(E(E(_JR[1])[1])==42){var _JU=new T(function(){var _JV=E(E(_JM)[1]);return _JV[0]==0?E(_Jq):[0,_JV[2],new T(function(){return B(_Ju(_JV[1]));})];}),_JW=[0,new T(function(){return E(E(_JU)[2]);}),_JR[2],new T(function(){return E(E(_JU)[1]);})];}else{var _JX=B(_Jh(_IX,_JR)),_JW=[0,_JX[1],_JX[2],new T(function(){return E(E(_JM)[1]);})];}var _JY=_JW,_JT=_JY;}var _JZ=_JT;}else{var _JZ=[0,_Jr,_JP,new T(function(){return E(E(_JM)[1]);})];}var _K0=_JZ,_JQ=_K0;}return _JQ;});return [0,new T(function(){return E(E(_JM)[2]);}),new T(function(){return E(E(_JO)[1]);}),_JE,_JF,_JG,new T(function(){return E(E(_JO)[2]);}),new T(function(){return E(E(_JO)[3]);})];case 43:var _K1=_JE,_K2=_JF;_JA=_b;_JB=_JK;var _K3=_JI;_Jy=_K1;_Jz=_K2;_JC=_K3;return null;case 45:_Jy=_b;var _K2=_JF,_K4=_JG;_JB=_JK;var _K3=_JI;_Jz=_K2;_JA=_K4;_JC=_K3;return null;case 46:var _K5=new T(function(){var _K6=E(_JK);if(!_K6[0]){var _K7=B(_Jh(_IX,_i)),_K8=[0,_K7[1],_K7[2],_JI];}else{if(E(E(_K6[1])[1])==42){var _K9=new T(function(){var _Ka=E(_JI);return _Ka[0]==0?E(_Jq):[0,_Ka[2],new T(function(){return B(_Ju(_Ka[1]));})];}),_Kb=[0,new T(function(){return E(E(_K9)[2]);}),_K6[2],new T(function(){return E(E(_K9)[1]);})];}else{var _Kc=B(_Jh(_IX,_K6)),_Kb=[0,_Kc[1],_Kc[2],_JI];}var _Kd=_Kb,_K8=_Kd;}return _K8;});return [0,_IX,new T(function(){return E(E(_K5)[1]);}),_JE,_JF,_JG,new T(function(){return E(E(_K5)[2]);}),new T(function(){return E(E(_K5)[3]);})];case 48:var _K1=_JE;_Jz=_b;var _K4=_JG;_JB=_JK;var _K3=_JI;_Jy=_K1;_JA=_K4;_JC=_K3;return null;default:if(_JL<48){return [0,_IX,_Jr,_JE,_JF,_JG,_JJ,_JI];}else{if(_JL>57){return [0,_IX,_Jr,_JE,_JF,_JG,_JJ,_JI];}else{var _Ke=new T(function(){var _Kf=B(_Jh(_IX,_JJ));return [0,_Kf[1],_Kf[2]];}),_Kg=new T(function(){var _Kh=E(E(_Ke)[2]);if(!_Kh[0]){var _Ki=[0,_Jr,_i,_JI];}else{if(E(E(_Kh[1])[1])==46){var _Kj=E(_Kh[2]);if(!_Kj[0]){var _Kk=B(_Jh(_IX,_i)),_Kl=[0,_Kk[1],_Kk[2],_JI];}else{if(E(E(_Kj[1])[1])==42){var _Km=new T(function(){var _Kn=E(_JI);return _Kn[0]==0?E(_Jq):[0,_Kn[2],new T(function(){return B(_Ju(_Kn[1]));})];}),_Ko=[0,new T(function(){return E(E(_Km)[2]);}),_Kj[2],new T(function(){return E(E(_Km)[1]);})];}else{var _Kp=B(_Jh(_IX,_Kj)),_Ko=[0,_Kp[1],_Kp[2],_JI];}var _Kq=_Ko,_Kl=_Kq;}var _Kr=_Kl;}else{var _Kr=[0,_Jr,_Kh,_JI];}var _Ks=_Kr,_Ki=_Ks;}var _Kt=_Ki;return _Kt;});return [0,new T(function(){return E(E(_Ke)[1]);}),new T(function(){return E(E(_Kg)[1]);}),_JE,_JF,_JG,new T(function(){return E(E(_Kg)[2]);}),new T(function(){return E(E(_Kg)[3]);})];}}}}})(_Jy,_Jz,_JA,_JB,_JC);if(_JD!=null){return _JD;}}},_Ku=new T(function(){return B(unCStr("formatting string ended prematurely"));}),_Kv=new T(function(){return B(_Go(_Ku));}),_Kw=function(_Kx,_Ky){if(!B(_M(_Ky,_Kx))){if(!B(_nP(_Kx,_II))){var _Kz=B(_xh(_Ky,_Kx));return new F(function(){return _C(B(_Kw(_Kx,_Kz[1])),[1,new T(function(){return [0,B(_Al(B(_7G(_Kz[2]))))];}),_i]);});}else{return E(_u6);}}else{return [1,new T(function(){return [0,B(_Al(B(_7G(_Ky))))];}),_i];}},_KA=[0,2],_KB=function(_KC,_KD,_KE){var _KF=E(_KE);switch(_KF[0]){case 0:return new F(function(){return _Kw(_KC,B(_6f(E(_KF[1])[1])));});break;case 2:var _KG=_KF[2],_KH=E(_KD)[1];if(!B(_M(_KG,_II))){return new F(function(){return _IR(_KH,B(_Kw(_KC,_KG)));});}else{return new F(function(){return _IR(_KH,B(_Kw(_KC,B(_5Z(B(_69(B(_6h(_KA,_KF[1])))),_KG)))));});}break;default:return E(_Gr);}},_KI=[0,37],_KJ=[0,16],_KK=[0,10],_KL=[0,8],_KM=[0,43],_KN=[1,_KM,_i],_KO=[0,32],_KP=function(_KQ){return new F(function(){return _Go(new T(function(){return B(unAppCStr("bad formatting char ",[1,_KQ,_i]));}));});},_KR=function(_KS,_KT){var _KU=E(_KS);if(!_KU){return [0];}else{var _KV=E(_KT);return _KV[0]==0?[0]:[1,_KV[1],new T(function(){return B(_KR(_KU-1|0,_KV[2]));})];}},_KW=function(_KX,_KY){var _KZ=E(_KX);if(!_KZ[0]){return E(_KY)[0]==0?[0]:E(_Kv);}else{var _L0=_KZ[2],_L1=E(_KZ[1]);if(E(_L1[1])==37){var _L2=function(_L3){var _L4=E(_KY);if(!_L4[0]){return E(_Jq);}else{var _L5=B(_Jx(_f,_f,_f,_L0,_L4)),_L6=_L5[2],_L7=_L5[4],_L8=E(_L5[6]);if(!_L8[0]){return E(_Kv);}else{var _L9=_L8[2],_La=E(_L5[7]);if(!_La[0]){return E(_Jq);}else{var _Lb=_La[1],_Lc=_La[2],_Ld=E(_L8[1]),_Le=function(_Lf,_Lg){var _Lh=new T(function(){var _Li=B(_IL(_Lg,0)),_Lj=B(_IL(_Lf,0)),_Lk=E(_L5[1])[1];if((_Li+_Lj|0)>=_Lk){var _Ll=[0];}else{var _Lm=_Lk-(_Li+_Lj|0)|0;if(_Lm>0){if(_Lm<0){var _Ln=[0];}else{var _Lo=new T(function(){return [1,new T(function(){return !E(_L7)?E(_KO):E(_IQ);}),_Lo];}),_Ln=B(_KR(_Lm,_Lo));}var _Lp=_Ln,_Lq=_Lp;}else{var _Lq=[0];}var _Lr=_Lq,_Ls=_Lr,_Lt=_Ls,_Ll=_Lt;}var _Lu=_Ll,_Lv=_Lu,_Lw=_Lv,_Lx=_Lw,_Ly=_Lx;return _Ly;});return !E(_L5[3])?!E(_L7)?B(_C(_Lh,new T(function(){return B(_C(_Lf,_Lg));}))):B(_C(_Lf,new T(function(){return B(_C(_Lh,_Lg));}))):B(_C(_Lf,new T(function(){return B(_C(_Lg,_Lh));})));},_Lz=function(_LA,_LB){var _LC=E(_LA);return _LC[0]==0?!E(_L5[5])?B(_Le(_i,_LB)):B(_Le(_KN,_LB)):B(_Le(_LC,_LB));};switch(E(_Ld[1])){case 69:var _LD=B(_HX(69,_L6,_Lb));return new F(function(){return _C(B(_Lz(_LD[1],_LD[2])),new T(function(){return B(_KW(_L9,_Lc));}));});break;case 71:var _LE=B(_HX(71,_L6,_Lb));return new F(function(){return _C(B(_Lz(_LE[1],_LE[2])),new T(function(){return B(_KW(_L9,_Lc));}));});break;case 88:return new F(function(){return _C(B(_Le(_i,new T(function(){return B(_1j(_HV,B(_KB(_KJ,_L6,_Lb))));}))),new T(function(){return B(_KW(_L9,_Lc));}));});break;case 99:return new F(function(){return _C(B(_Le(_i,[1,new T(function(){var _LF=E(_Lb);switch(_LF[0]){case 0:var _LG=E(_LF[1])[1];if(_LG>>>0>1114111){var _LH=B(_7E(_LG));}else{var _LH=[0,_LG];}var _LI=_LH,_LJ=_LI,_LK=_LJ,_LL=_LK,_LM=_LL;break;case 2:var _LN=B(_7G(_LF[2]));if(_LN>>>0>1114111){var _LO=B(_7E(_LN));}else{var _LO=[0,_LN];}var _LP=_LO,_LQ=_LP,_LR=_LQ,_LM=_LR;break;default:var _LM=E(_Gr);}return _LM;}),_i])),new T(function(){return B(_KW(_L9,_Lc));}));});break;case 100:var _LS=B(_IZ(_L6,_Lb));return new F(function(){return _C(B(_Lz(_LS[1],_LS[2])),new T(function(){return B(_KW(_L9,_Lc));}));});break;case 101:var _LT=B(_HX(101,_L6,_Lb));return new F(function(){return _C(B(_Lz(_LT[1],_LT[2])),new T(function(){return B(_KW(_L9,_Lc));}));});break;case 102:var _LU=B(_HX(102,_L6,_Lb));return new F(function(){return _C(B(_Lz(_LU[1],_LU[2])),new T(function(){return B(_KW(_L9,_Lc));}));});break;case 103:var _LV=B(_HX(103,_L6,_Lb));return new F(function(){return _C(B(_Lz(_LV[1],_LV[2])),new T(function(){return B(_KW(_L9,_Lc));}));});break;case 105:var _LW=B(_IZ(_L6,_Lb));return new F(function(){return _C(B(_Lz(_LW[1],_LW[2])),new T(function(){return B(_KW(_L9,_Lc));}));});break;case 111:return new F(function(){return _C(B(_Le(_i,new T(function(){return B(_KB(_KL,_L6,_Lb));}))),new T(function(){return B(_KW(_L9,_Lc));}));});break;case 115:return new F(function(){return _C(B(_Le(_i,new T(function(){var _LX=E(_Lb);if(_LX[0]==1){var _LY=_LX[1],_LZ=E(_L6)[1];if(_LZ<0){var _M0=E(_LY);}else{var _M0=_LZ>0?B(_KR(_LZ,_LY)):[0];}var _M1=_M0,_M2=_M1,_M3=_M2;}else{var _M3=E(_Gr);}return _M3;}))),new T(function(){return B(_KW(_L9,_Lc));}));});break;case 117:return new F(function(){return _C(B(_Le(_i,new T(function(){return B(_KB(_KK,_L6,_Lb));}))),new T(function(){return B(_KW(_L9,_Lc));}));});break;case 120:return new F(function(){return _C(B(_Le(_i,new T(function(){return B(_KB(_KJ,_L6,_Lb));}))),new T(function(){return B(_KW(_L9,_Lc));}));});break;default:return new F(function(){return _KP(_Ld);});}}}}},_M4=E(_L0);if(!_M4[0]){return new F(function(){return _L2(_);});}else{if(E(E(_M4[1])[1])==37){return [1,_KI,new T(function(){return B(_KW(_M4[2],_KY));})];}else{return new F(function(){return _L2(_);});}}}else{return [1,_L1,new T(function(){return B(_KW(_L0,_KY));})];}}},_M5=function(_M6){return _M6>1000?B(_nk(new T(function(){var _M7=B(_md(_M6)),_M8=_M7[1],_M9=_M7[2];if(_M9>=0){var _Ma=B(_X(0,B(_np(_M8,_M9)),_i));}else{var _Mb= -_M9;if(_Mb<=52){var _Mc=hs_uncheckedIShiftRA64(B(_mB(_M8)),_Mb),_Md=_Mc,_Me=B(_X(0,B(_ml(_Md)),_i));}else{var _Me=!B(_M(_M8,_28))?E(_mE):E(_mG);}var _Mf=_Me,_Mg=_Mf,_Ma=_Mg;}var _Mh=_Ma,_Mi=_Mh;return _Mi;}))):B(_nk(new T(function(){return B(_mg(B(_KW(_mH,new T(function(){return B(_mI([1,[4,[0,_M6]],_i],_i));}))),5));})));},_Mj=function(_Mk,_Ml){return new F(function(){return (function(_Mm){while(1){var _Mn=E(_Mm);switch(_Mn[0]){case 0:var _Mo=_Mn[2]>>>0;if(((_Mk>>>0&((_Mo-1>>>0^4294967295)>>>0^_Mo)>>>0)>>>0&4294967295)==_Mn[1]){if(!((_Mk>>>0&_Mo)>>>0)){_Mm=_Mn[3];continue;}else{_Mm=_Mn[4];continue;}}else{return [0];}break;case 1:return _Mk!=_Mn[1]?[0]:[1,_Mn[2]];default:return [0];}}})(_Ml);});},_Mp=function(_Mq,_Mr,_Ms,_Mt){return new F(function(){return A(_Mq,[function(_){var _Mu=jsSet(E(_Mr)[1],toJSStr(E(_Ms)),toJSStr(E(_Mt)));return _4L;}]);});},_Mv=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:163:7-12"));}),_Mw=new T(function(){return B(unCStr("lps"));}),_Mx=new T(function(){return B(unCStr("loves"));}),_My=new T(function(){return B(unCStr("depend"));}),_Mz=new T(function(){return B(unCStr("interval"));}),_MA=function(_MB,_){return [0,_4L,_MB];},_MC=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_MD=new T(function(){return B(unCStr("base"));}),_ME=new T(function(){return B(unCStr("IOException"));}),_MF=new T(function(){var _MG=hs_wordToWord64(4053623282),_MH=_MG,_MI=hs_wordToWord64(3693590983),_MJ=_MI;return [0,_MH,_MJ,[0,_MH,_MJ,_MD,_MC,_ME],_i];}),_MK=function(_ML){return E(_MF);},_MM=function(_MN){var _MO=E(_MN);return new F(function(){return _2p(B(_2n(_MO[1])),_MK,_MO[2]);});},_MP=new T(function(){return B(unCStr(": "));}),_MQ=[0,41],_MR=new T(function(){return B(unCStr(" ("));}),_MS=new T(function(){return B(unCStr("already exists"));}),_MT=new T(function(){return B(unCStr("does not exist"));}),_MU=new T(function(){return B(unCStr("protocol error"));}),_MV=new T(function(){return B(unCStr("failed"));}),_MW=new T(function(){return B(unCStr("invalid argument"));}),_MX=new T(function(){return B(unCStr("inappropriate type"));}),_MY=new T(function(){return B(unCStr("hardware fault"));}),_MZ=new T(function(){return B(unCStr("unsupported operation"));}),_N0=new T(function(){return B(unCStr("timeout"));}),_N1=new T(function(){return B(unCStr("resource vanished"));}),_N2=new T(function(){return B(unCStr("interrupted"));}),_N3=new T(function(){return B(unCStr("resource busy"));}),_N4=new T(function(){return B(unCStr("resource exhausted"));}),_N5=new T(function(){return B(unCStr("end of file"));}),_N6=new T(function(){return B(unCStr("illegal operation"));}),_N7=new T(function(){return B(unCStr("permission denied"));}),_N8=new T(function(){return B(unCStr("user error"));}),_N9=new T(function(){return B(unCStr("unsatisified constraints"));}),_Na=new T(function(){return B(unCStr("system error"));}),_Nb=function(_Nc,_Nd){switch(E(_Nc)){case 0:return new F(function(){return _C(_MS,_Nd);});break;case 1:return new F(function(){return _C(_MT,_Nd);});break;case 2:return new F(function(){return _C(_N3,_Nd);});break;case 3:return new F(function(){return _C(_N4,_Nd);});break;case 4:return new F(function(){return _C(_N5,_Nd);});break;case 5:return new F(function(){return _C(_N6,_Nd);});break;case 6:return new F(function(){return _C(_N7,_Nd);});break;case 7:return new F(function(){return _C(_N8,_Nd);});break;case 8:return new F(function(){return _C(_N9,_Nd);});break;case 9:return new F(function(){return _C(_Na,_Nd);});break;case 10:return new F(function(){return _C(_MU,_Nd);});break;case 11:return new F(function(){return _C(_MV,_Nd);});break;case 12:return new F(function(){return _C(_MW,_Nd);});break;case 13:return new F(function(){return _C(_MX,_Nd);});break;case 14:return new F(function(){return _C(_MY,_Nd);});break;case 15:return new F(function(){return _C(_MZ,_Nd);});break;case 16:return new F(function(){return _C(_N0,_Nd);});break;case 17:return new F(function(){return _C(_N1,_Nd);});break;default:return new F(function(){return _C(_N2,_Nd);});}},_Ne=[0,125],_Nf=new T(function(){return B(unCStr("{handle: "));}),_Ng=function(_Nh,_Ni,_Nj,_Nk,_Nl,_Nm){var _Nn=new T(function(){var _No=new T(function(){return B(_Nb(_Ni,new T(function(){var _Np=E(_Nk);return _Np[0]==0?E(_Nm):B(_C(_MR,new T(function(){return B(_C(_Np,[1,_MQ,_Nm]));})));})));}),_Nq=E(_Nj);return _Nq[0]==0?E(_No):B(_C(_Nq,new T(function(){return B(_C(_MP,_No));})));}),_Nr=E(_Nl);if(!_Nr[0]){var _Ns=E(_Nh);if(!_Ns[0]){return E(_Nn);}else{var _Nt=E(_Ns[1]);return _Nt[0]==0?B(_C(_Nf,new T(function(){return B(_C(_Nt[1],[1,_Ne,new T(function(){return B(_C(_MP,_Nn));})]));}))):B(_C(_Nf,new T(function(){return B(_C(_Nt[1],[1,_Ne,new T(function(){return B(_C(_MP,_Nn));})]));})));}}else{return new F(function(){return _C(_Nr[1],new T(function(){return B(_C(_MP,_Nn));}));});}},_Nu=function(_Nv){var _Nw=E(_Nv);return new F(function(){return _Ng(_Nw[1],_Nw[2],_Nw[3],_Nw[4],_Nw[6],_i);});},_Nx=function(_Ny,_Nz){var _NA=E(_Ny);return new F(function(){return _Ng(_NA[1],_NA[2],_NA[3],_NA[4],_NA[6],_Nz);});},_NB=function(_NC,_ND){return new F(function(){return _2K(_Nx,_NC,_ND);});},_NE=function(_NF,_NG,_NH){var _NI=E(_NG);return new F(function(){return _Ng(_NI[1],_NI[2],_NI[3],_NI[4],_NI[6],_NH);});},_NJ=[0,_NE,_Nu,_NB],_NK=new T(function(){return [0,_MK,_NJ,_NL,_MM];}),_NL=function(_NM){return [0,_NK,_NM];},_NN=7,_NO=function(_NP){return [0,_6F,_NN,_i,_NP,_6F,_6F];},_NQ=function(_NR,_){return new F(function(){return die(new T(function(){return B(_NL(new T(function(){return B(_NO(_NR));})));}));});},_NS=function(_NT,_){return new F(function(){return _NQ(_NT,_);});},_NU=new T(function(){return B(unCStr("innerHTML"));}),_NV=new T(function(){return B(unCStr("main"));}),_NW=new T(function(){return B(unCStr(" <div class=\"panel panel-default item-box\">  <div class=\"panel-heading\">    <i class=\"fa fa-check\"></i> %s    <button type=\"button\" id=\"item-%d\" class=\"btn btn-sm btn-default btn-buy\"><i class=\"fa fa-plus-circle\"></i> %s loves</button>  </div>  <div class=\"panel-body item-box\">    <div class=\"count col-md-3\"><i class=\"fa fa-coffee\"></i> %s</div>    <div class=\"item-list col-md-offset-3\">      %s    </div>  </div>  </div>"));}),_NX=new T(function(){return B(_n9(_i));}),_NY=new T(function(){return B(_1j(_mb,_NX));}),_NZ=[0,-2147483648],_O0=function(_O1,_O2,_){var _O3=B(A(_O1,[_])),_O4=_O3;return new F(function(){return A(_O2,[_O4,_]);});},_O5=function(_O6,_){return _O6;},_O7=function(_O8,_O9,_){var _Oa=B(A(_O8,[_])),_Ob=_Oa;return new F(function(){return A(_O9,[_]);});},_Oc=[0,_O0,_O7,_O5,_NS],_Od=[0,_Oc,_5p],_Oe=function(_Of){return E(E(_Of)[1]);},_Og=function(_Oh){return E(E(_Oh)[1]);},_Oi=function(_Oj){return E(E(_Oj)[2]);},_Ok=function(_Ol){return E(E(_Ol)[3]);},_Om=function(_On,_Oo){var _Op=new T(function(){return B(_Oe(_On));});return function(_Oq){return new F(function(){return A(new T(function(){return B(_Og(_Op));}),[new T(function(){return B(A(_Oi,[_On,_Oo]));}),function(_Or){return new F(function(){return A(new T(function(){return B(_Ok(_Op));}),[[0,_Or,_Oq]]);});}]);});};},_Os=function(_Ot){return new F(function(){return _Om(_Od,_Ot);});},_Ou=function(_Ov,_Ow){var _Ox=E(_Ow);return _Ox[0]==0?[0]:[1,[0,[0,_Ov],_Ox[1]],new T(function(){var _Oy=E(_Ov);if(_Oy==2147483647){var _Oz=[0];}else{var _Oz=B(_Ou(_Oy+1|0,_Ox[2]));}return _Oz;})];},_OA=function(_OB,_OC,_OD){return [1,[0,[0,_OB],_OC],new T(function(){var _OE=E(_OB);if(_OE==2147483647){var _OF=[0];}else{var _OF=B(_Ou(_OE+1|0,_OD));}return _OF;})];},_OG=function(_OH,_){return [0,_4L,new T(function(){var _OI=E(_OH);return [0,_OI[1],new T(function(){return [0,E(_OI[2])[1]+0.5];}),_OI[3],_OI[4],_OI[5],_OI[6],_OI[7],_OI[8]];})];},_OJ=[0,10],_OK=new T(function(){return B(unCStr("\u55ab\u8336\u5e97"));}),_OL=[0,_OJ,_OK,_OG],_OM=new T(function(){return B(unCStr("chocolate"));}),_ON=[0,_OJ,_OM,_OG],_OO=[1,_ON,_i],_OP=new T(function(){return B(_OA(1,_OL,_OO));}),_OQ=function(_OR,_OS){var _OT=E(_OS);if(!_OT[0]){return [0];}else{var _OU=_OT[1];return !B(A(_OR,[_OU]))?[0]:[1,_OU,new T(function(){return B(_OQ(_OR,_OT[2]));})];}},_OV=new T(function(){return B(unCStr(" could be found!"));}),_OW=function(_OX){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_C(_OX,_OV));}))));});},_OY=new T(function(){return B(unCStr("<i class=\"fa fa-coffee\"></i>"));}),_OZ=[1,_OY,_i],_P0=function(_P1){return _P1>1?[1,_OY,new T(function(){return B(_P0(_P1-1|0));})]:E(_OZ);},_P2=function(_,_P3){var _P4=jsFind(toJSStr(E(_Mw))),_P5=_P4,_P6=E(_P5);if(!_P6[0]){return new F(function(){return _NS(_Mv,_);});}else{var _P7=E(_NU),_P8=toJSStr(_P7),_P9=E(E(_P3)[2]),_Pa=jsSet(E(_P6[1])[1],_P8,toJSStr(B(_M5(E(_P9[2])[1])))),_Pb=jsFind(toJSStr(E(_Mx))),_Pc=_Pb,_Pd=E(_Pc);if(!_Pd[0]){return new F(function(){return _NS(_Mv,_);});}else{var _Pe=E(_P9[1])[1],_Pf=jsSet(E(_Pd[1])[1],_P8,toJSStr(B(_M5(_Pe)))),_Pg=jsFind(toJSStr(E(_My))),_Ph=_Pg,_Pi=E(_Ph);if(!_Pi[0]){return new F(function(){return _NS(_Mv,_);});}else{var _Pj=jsSet(E(_Pi[1])[1],_P8,toJSStr(B(_M5(E(_P9[3])[1])))),_Pk=jsFind(toJSStr(E(_Mz))),_Pl=_Pk,_Pm=E(_Pl);if(!_Pm[0]){return new F(function(){return _NS(_Mv,_);});}else{var _Pn=jsSet(E(_Pm[1])[1],_P8,toJSStr(B(_nk(new T(function(){return B(_X(0,_P9[5],_i));}))))),_Po=E(_NV),_Pp=jsFind(toJSStr(_Po)),_Pq=_Pp,_Pr=E(_Pq);if(!_Pr[0]){return new F(function(){return _OW(_Po);});}else{var _Ps=_Pr[1],_Pt=B(A(_Mp,[_Os,_Ps,_P7,_i,_P9,_])),_Pu=_Pt,_Pv=function(_Pw){var _Px=E(_Pw);return _Px[0]==0?E(_MA):function(_Py,_){var _Pz=B(A(new T(function(){var _PA=E(_Px[1]),_PB=_PA[1],_PC=E(_PA[2]);return function(_PD,_){var _PE=E(_Ps)[1],_PF=jsGet(_PE,toJSStr(_P7)),_PG=_PF,_PH=jsSet(_PE,_P8,toJSStr(B(_C(fromJSStr(_PG),new T(function(){return B(_1j(_mb,B(_KW(_NW,new T(function(){var _PI=new T(function(){var _PJ=B(_Mj(E(_PB)[1],_P9[8]));return _PJ[0]==0?E(_l7):E(_PJ[1]);});return B(_mI([1,[1,new T(function(){var _PK=E(_PI)[1];if(_PK>0){var _PL=B(_1j(_mb,B(_n9(B(_P0(_PK))))));}else{var _PL=E(_NY);}var _PM=_PL,_PN=_PM;return _PN;})],[1,[1,new T(function(){return B(_1j(_mb,B(_nk(new T(function(){return B(_7y(0,E(_PI)[1],_i));})))));})],[1,[1,new T(function(){return B(_1j(_mb,B(_nk(new T(function(){return B(_7y(0,E(_PC[1])[1],_i));})))));})],[1,[2,_NZ,new T(function(){return B(_6f(E(_PB)[1]));})],[1,[1,new T(function(){return B(_1j(_mb,_PC[2]));})],_i]]]]],_i));})))));})))));return [0,_4L,_PD];};}),[_Py,_])),_PO=_Pz;return new F(function(){return A(new T(function(){return B(_Pv(_Px[2]));}),[new T(function(){return E(E(_PO)[2]);}),_]);});};};return new F(function(){return A(_Pv,[B(_OQ(function(_PP){return E(E(E(_PP)[2])[1])[1]<=_Pe;},_OP)),new T(function(){return E(E(_Pu)[2]);}),_]);});}}}}}},_PQ=function(_PR,_PS,_){var _PT=jsGet(_PR,toJSStr(E(_PS))),_PU=_PT;return new T(function(){return fromJSStr(_PU);});},_PV=new T(function(){return B(unCStr("alerts"));}),_PW=new T(function(){return B(unCStr("<div id=\"alert-%d\" class=\"alert alert-info fade in\" role=\"alert\">  <button type=\"button\" class=\"close\" data-dismiss=\"alert\"><span aria-hidden=\"true\">&times;</span><span class=\"sr-only\">Close</span></button>%s</div>%s"));}),_PX=new T(function(){return B(unCStr("\').alert(\'close\')"));}),_PY=new T(function(){return B(unCStr("\').animate({  top: \"50px\"  })"));}),_PZ=function(_){var _Q0=jsEval("Date.now()"),_Q1=_Q0;return new T(function(){var _Q2=B(_gB(B(_3v(_eg,new T(function(){return fromJSStr(_Q1);})))));return _Q2[0]==0?B(err(_2b)):E(_Q2[2])[0]==0?E(_Q2[1]):B(err(_29));});},_Q3=function(_Q4,_){var _Q5=E(_PV),_Q6=jsFind(toJSStr(_Q5)),_Q7=_Q6,_Q8=E(_Q7);if(!_Q8[0]){return new F(function(){return _OW(_Q5);});}else{var _Q9=E(_Q8[1])[1],_Qa=B(_PQ(_Q9,_NU,_)),_Qb=_Qa,_Qc=B(_PZ(_)),_Qd=_Qc,_Qe=jsSet(_Q9,toJSStr(E(_NU)),toJSStr(B(_1j(_mb,B(_KW(_PW,new T(function(){return B(_mI([1,[1,new T(function(){return B(_1j(_mb,_Qb));})],[1,[1,new T(function(){return B(_1j(_mb,_Q4));})],[1,[2,_II,_Qd],_i]]],_i));}))))))),_Qf=jsEval(toJSStr(B(unAppCStr("$(\'#alert-",new T(function(){return B(_C(B(_X(0,_Qd,_i)),_PY));}))))),_Qg=_Qf,_Qh=jsSetTimeout(5000,function(_){var _Qi=jsEval(toJSStr(B(unAppCStr("$(\'#alert-",new T(function(){return B(_C(B(_X(0,_Qd,_i)),_PX));}))))),_Qj=_Qi;return _4L;});return _4L;}},_Qk=new T(function(){return B(unCStr("multiplier 1"));}),_Ql=new T(function(){return B(unCStr("\u3092\u8d85\u3048\u308b"));}),_Qm=new T(function(){return B(unCStr("\u5b9f\u7e3e\u7372\u5f97\uff1a "));}),_Qn=new T(function(){return B(unCStr("\u4f9d\u5b58\u5ea6\u304c"));}),_Qo=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:201:3-8"));}),_Qp=new T(function(){return B(unCStr("<tr><td>%s</td><td>%s</td></tr>"));}),_Qq=new T(function(){return B(unCStr("<thead><tr><th>\u5b9f\u7e3e\u540d</th><th>\u5185\u5bb9</th></tr></thead>"));}),_Qr=new T(function(){return B(unCStr("</tbody>"));}),_Qs=new T(function(){return B(unCStr("achievements"));}),_Qt=function(_Qu,_){var _Qv=jsFind(toJSStr(E(_Qs))),_Qw=_Qv,_Qx=E(_Qw);if(!_Qx[0]){return new F(function(){return _NS(_Qo,_);});}else{var _Qy=jsSet(E(_Qx[1])[1],toJSStr(E(_NU)),toJSStr(B(_C(_Qq,new T(function(){return B(unAppCStr("<tbody>",new T(function(){var _Qz=function(_QA){var _QB=E(_QA);if(!_QB[0]){return [0];}else{var _QC=E(_QB[1]),_QD=function(_QE){var _QF=E(_QE);return _QF[0]==0?E(new T(function(){return B(_Qz(_QB[2]));})):[1,_QF[1],new T(function(){return B(_QD(_QF[2]));})];};return new F(function(){return _QD(B(_KW(_Qp,new T(function(){return B(_mI([1,[1,new T(function(){return B(_1j(_mb,_QC[2]));})],[1,[1,new T(function(){return B(_1j(_mb,_QC[1]));})],_i]],_i));}))));});}};return B(_C(B(_Qz(B(_1j(function(_QG){var _QH=E(_QG),_QI=E(_QH[2])[1],_QJ=B(_Mj(E(_QH[1])[1],new T(function(){return E(E(_Qu)[7]);})));if(!_QJ[0]){return [0,_QI,_i];}else{var _QK=E(_QJ[1]);return _QK[0]==0?[0,_QI,_i]:[0,_QI,_QK[1]];}},_QL)))),_Qr));})));})))));return [0,_4L,_Qu];}},_QM=function(_QN,_QO,_QP){return function(_QQ,_){var _QR=E(_QQ),_QS=E(_QR[3]);if(_QS[1]<=E(new T(function(){return [0,B(_o9(_QN))];}))[1]){return [0,_4L,_QR];}else{var _QT=B(_Q3(new T(function(){return B(_C(_Qm,_QO));}),_)),_QU=_QT;return new F(function(){return _Qt([0,_QR[1],_QR[2],_QS,_QR[4],_QR[5],_QR[6],new T(function(){return B(_g2(E(_QP)[1],[1,new T(function(){return B(_C(_Qn,new T(function(){return B(_C(B(_X(0,_QN,_i)),_Ql));})));})],_QR[7]));}),_QR[8]],_);});}};},_QV=[0,1],_QW=function(_QX){return new F(function(){return _QM(_QV,_Qk,_QX);});},_QY=new T(function(){return [0,_Qk,_QW];}),_QZ=new T(function(){return B(unCStr("multiplier 10"));}),_R0=[0,10],_R1=function(_QX){return new F(function(){return _QM(_R0,_QZ,_QX);});},_R2=new T(function(){return [0,_QZ,_R1];}),_R3=new T(function(){return B(unCStr("\u4f9d\u5b58\u6ce8\u610f\u5831"));}),_R4=[0,100],_R5=function(_QX){return new F(function(){return _QM(_R4,_R3,_QX);});},_R6=new T(function(){return [0,_R3,_R5];}),_R7=new T(function(){return B(unCStr("\u4f9d\u5b58\u30c9\u30e9\u30c3\u30b0"));}),_R8=[0,1000],_R9=function(_QX){return new F(function(){return _QM(_R8,_R7,_QX);});},_Ra=new T(function(){return [0,_R7,_R9];}),_Rb=new T(function(){return B(unCStr("\u307e\u305a\u306f\u304a\u53cb\u9054\u304b\u3089"));}),_Rc=new T(function(){return B(unCStr("\u597d\u611f\u5ea6\u304c"));}),_Rd=function(_Re,_Rf,_Rg){return function(_Rh,_){var _Ri=E(_Rh),_Rj=E(_Ri[2]);if(_Rj[1]<=E(new T(function(){return [0,B(_o9(_Re))];}))[1]){return [0,_4L,_Ri];}else{var _Rk=B(_Q3(new T(function(){return B(_C(_Qm,_Rf));}),_)),_Rl=_Rk;return new F(function(){return _Qt([0,_Ri[1],_Rj,_Ri[3],_Ri[4],_Ri[5],_Ri[6],new T(function(){return B(_g2(E(_Rg)[1],[1,new T(function(){return B(_C(_Rc,new T(function(){return B(_C(B(_X(0,_Re,_i)),_Ql));})));})],_Ri[7]));}),_Ri[8]],_);});}};},_Rm=function(_QX){return new F(function(){return _Rd(_QV,_Rb,_QX);});},_Rn=new T(function(){return [0,_Rb,_Rm];}),_Ro=new T(function(){return B(unCStr("\u4e8c\u7d1a\u30d5\u30e9\u30b0\u5efa\u7bc9\u58eb"));}),_Rp=[0,5],_Rq=function(_QX){return new F(function(){return _Rd(_Rp,_Ro,_QX);});},_Rr=new T(function(){return [0,_Ro,_Rq];}),_Rs=new T(function(){return B(unCStr("\u4e00\u7d1a\u30d5\u30e9\u30b0\u5efa\u7bc9\u58eb"));}),_Rt=function(_QX){return new F(function(){return _Rd(_R0,_Rs,_QX);});},_Ru=new T(function(){return [0,_Rs,_Rt];}),_Rv=new T(function(){return B(unCStr("\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_Rw=[0,50],_Rx=function(_QX){return new F(function(){return _Rd(_Rw,_Rv,_QX);});},_Ry=new T(function(){return [0,_Rv,_Rx];}),_Rz=new T(function(){return B(unCStr("\uff8a\uff72\uff8a\uff9f\uff70\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_RA=function(_QX){return new F(function(){return _Rd(_R4,_Rz,_QX);});},_RB=new T(function(){return [0,_Rz,_RA];}),_RC=new T(function(){return B(unCStr("\u5168\u958b\u3089\u3076\u3071\u308f\u30fc"));}),_RD=[0,250],_RE=function(_QX){return new F(function(){return _Rd(_RD,_RC,_QX);});},_RF=new T(function(){return [0,_RC,_RE];}),_RG=new T(function(){return B(unCStr("\u30a2\u30a4\u3068\u306e\u906d\u9047"));}),_RH=new T(function(){return B(unCStr("\u611b\u60c5\u304c"));}),_RI=function(_RJ,_RK,_RL){return function(_RM,_){var _RN=E(_RM),_RO=E(_RN[1]);if(_RO[1]<=E(new T(function(){return [0,B(_o9(_RJ))];}))[1]){return [0,_4L,_RN];}else{var _RP=B(_Q3(new T(function(){return B(_C(_Qm,_RK));}),_)),_RQ=_RP;return new F(function(){return _Qt([0,_RO,_RN[2],_RN[3],_RN[4],_RN[5],_RN[6],new T(function(){return B(_g2(E(_RL)[1],[1,new T(function(){return B(_C(_RH,new T(function(){return B(_C(B(_X(0,_RJ,_i)),_Ql));})));})],_RN[7]));}),_RN[8]],_);});}};},_RR=function(_QX){return new F(function(){return _RI(_QV,_RG,_QX);});},_RS=new T(function(){return [0,_RG,_RR];}),_RT=new T(function(){return B(unCStr("\u611b\u3055\u308c\u6c17\u5206"));}),_RU=function(_QX){return new F(function(){return _RI(_R4,_RT,_QX);});},_RV=new T(function(){return [0,_RT,_RU];}),_RW=new T(function(){return B(unCStr("\u611b\u30e9\u30d6\u30e6\u30fc"));}),_RX=[0,10000],_RY=function(_QX){return new F(function(){return _RI(_RX,_RW,_QX);});},_RZ=new T(function(){return [0,_RW,_RY];}),_S0=new T(function(){return B(unCStr("\u611b\u3055\u3093\u306e\u611b\u304c\u91cd\u3044"));}),_S1=[0,10000000],_S2=function(_QX){return new F(function(){return _RI(_S1,_S0,_QX);});},_S3=new T(function(){return [0,_S0,_S2];}),_QL=new T(function(){var _S4=B(_sx(1,2147483647));return _S4[0]==0?[0]:[1,[0,_S4[1],_RS],new T(function(){var _S5=E(_S4[2]);return _S5[0]==0?[0]:[1,[0,_S5[1],_RV],new T(function(){var _S6=E(_S5[2]);return _S6[0]==0?[0]:[1,[0,_S6[1],_RZ],new T(function(){var _S7=E(_S6[2]);return _S7[0]==0?[0]:[1,[0,_S7[1],_S3],new T(function(){var _S8=E(_S7[2]);return _S8[0]==0?[0]:[1,[0,_S8[1],_Rn],new T(function(){var _S9=E(_S8[2]);return _S9[0]==0?[0]:[1,[0,_S9[1],_Rr],new T(function(){var _Sa=E(_S9[2]);return _Sa[0]==0?[0]:[1,[0,_Sa[1],_Ru],new T(function(){var _Sb=E(_Sa[2]);return _Sb[0]==0?[0]:[1,[0,_Sb[1],_Ry],new T(function(){var _Sc=E(_Sb[2]);return _Sc[0]==0?[0]:[1,[0,_Sc[1],_RB],new T(function(){var _Sd=E(_Sc[2]);return _Sd[0]==0?[0]:[1,[0,_Sd[1],_RF],new T(function(){var _Se=E(_Sd[2]);return _Se[0]==0?[0]:[1,[0,_Se[1],_QY],new T(function(){var _Sf=E(_Se[2]);return _Sf[0]==0?[0]:[1,[0,_Sf[1],_R2],new T(function(){var _Sg=E(_Sf[2]);return _Sg[0]==0?[0]:[1,[0,_Sg[1],_R6],new T(function(){var _Sh=E(_Sg[2]);return _Sh[0]==0?[0]:[1,[0,_Sh[1],_Ra],_i];})];})];})];})];})];})];})];})];})];})];})];})];})];}),_Si=new T(function(){return B(unCStr(" is not an element of the map"));}),_Sj=function(_Sk){return new F(function(){return err(B(unAppCStr("IntMap.!: key ",new T(function(){return B(_C(B(_7y(0,_Sk,_i)),_Si));}))));});},_Sl=function(_Sm,_Sn){var _So=new T(function(){return B(_Sj(_Sn));});return new F(function(){return (function(_Sp){while(1){var _Sq=E(_Sp);switch(_Sq[0]){case 0:var _Sr=_Sq[2]>>>0;if(((_Sn>>>0&((_Sr-1>>>0^4294967295)>>>0^_Sr)>>>0)>>>0&4294967295)==_Sq[1]){if(!((_Sn>>>0&_Sr)>>>0)){_Sp=_Sq[3];continue;}else{_Sp=_Sq[4];continue;}}else{return E(_So);}break;case 1:return _Sn!=_Sq[1]?E(_So):E(_Sq[2]);default:return E(_So);}}})(_Sm);});},_Ss=function(_St,_Su){return new F(function(){return (function(_Sv){while(1){var _Sw=E(_Sv);switch(_Sw[0]){case 0:var _Sx=_Sw[2]>>>0;if(((_St>>>0&((_Sx-1>>>0^4294967295)>>>0^_Sx)>>>0)>>>0&4294967295)==_Sw[1]){if(!((_St>>>0&_Sx)>>>0)){_Sv=_Sw[3];continue;}else{_Sv=_Sw[4];continue;}}else{return false;}break;case 1:return _St==_Sw[1];default:return false;}}})(_Su);});},_Sy=function(_Sz){var _SA=E(_Sz);return _SA[0]==0?E(_MA):function(_SB,_){var _SC=B(A(new T(function(){var _SD=E(_SA[1]),_SE=_SD[1],_SF=new T(function(){return B(A(E(_SD[2])[2],[_SE]));});return function(_SG,_){var _SH=E(_SG),_SI=_SH[7],_SJ=E(_SE)[1];return !B(_Ss(_SJ,_SI))?B(A(_SF,[_SH,_])):B(_Sl(_SI,_SJ))[0]==0?B(A(_SF,[_SH,_])):[0,_4L,_SH];};}),[_SB,_])),_SK=_SC;return new F(function(){return A(new T(function(){return B(_Sy(_SA[2]));}),[new T(function(){return E(E(_SK)[2]);}),_]);});};},_SL=new T(function(){return B(_Sy(_QL));}),_SM=new T(function(){return B(unCStr("false"));}),_SN=new T(function(){return B(unCStr("true"));}),_SO=new T(function(){return B(unCStr("document.hasFocus()"));}),_SP=[0,1],_SQ=new T(function(){return B(unCStr("\u653e\u7f6e\u671f\u9593 +"));}),_SR=function(_SS){return new F(function(){return err(B(unAppCStr("docFocused: ",new T(function(){return fromJSStr(_SS);}))));});},_ST=function(_SU,_){var _SV=jsEval(toJSStr(E(_SO))),_SW=_SV,_SX=strEq(_SW,toJSStr(E(_SN))),_SY=_SX;if(!E(_SY)){var _SZ=strEq(_SW,toJSStr(E(_SM))),_T0=_SZ;if(!E(_T0)){return new F(function(){return _SR(_SW);});}else{var _T1=B(A(_SL,[new T(function(){var _T2=E(_SU),_T3=_T2[2],_T4=_T2[3],_T5=_T2[5],_T6=new T(function(){return [0,B(_o9(_T5))/1000/60/120];});return [0,new T(function(){return [0,E(_T2[1])[1]+E(_T3)[1]*E(_T4)[1]/30];}),new T(function(){return [0,E(_T3)[1]+E(_T6)[1]];}),new T(function(){var _T7=E(_T4)[1]-E(_T6)[1];return _T7>1?[0,_T7]:E(_SP);}),_T2[4],_T5,_f,_T2[7],_T2[8]];}),_])),_T8=_T1;return new F(function(){return _P2(_,_T8);});}}else{var _T9=E(_SU),_Ta=_T9[1],_Tb=_T9[2],_Tc=_T9[3],_Td=_T9[4],_Te=_T9[5],_Tf=_T9[7],_Tg=_T9[8];if(!E(_T9[6])){var _Th=B(_Q3(new T(function(){return B(_C(_SQ,new T(function(){return B(_X(0,_Te,_i));})));}),_)),_Ti=_Th,_Tj=B(A(_SL,[[0,new T(function(){return [0,E(_Ta)[1]+E(_Tb)[1]*E(_Tc)[1]/30];}),_Tb,new T(function(){return [0,E(_Tc)[1]+B(_o9(_Te))/1000/60/15];}),_Td,_Te,_b,_Tf,_Tg],_])),_Tk=_Tj;return new F(function(){return _P2(_,_Tk);});}else{var _Tl=B(A(_SL,[[0,new T(function(){return [0,E(_Ta)[1]+E(_Tb)[1]*E(_Tc)[1]/30];}),_Tb,new T(function(){return [0,E(_Tc)[1]+B(_o9(_Te))/1000/60/15];}),_Td,_Te,_b,_Tf,_Tg],_])),_Tm=_Tl;return new F(function(){return _P2(_,_Tm);});}}},_Tn=[0,0],_To=new T(function(){return B(unCStr("Aichan"));}),_Tp=new T(function(){return [0,toJSStr(_i)];}),_Tq=[0,93],_Tr=[1,_Tq,_i],_Ts=new T(function(){return [0,toJSStr(_Tr)];}),_Tt=[0,125],_Tu=[1,_Tt,_i],_Tv=new T(function(){return [0,toJSStr(_Tu)];}),_Tw=[0,58],_Tx=[1,_Tw,_i],_Ty=new T(function(){return [0,toJSStr(_Tx)];}),_Tz=[0,44],_TA=[1,_Tz,_i],_TB=new T(function(){return [0,toJSStr(_TA)];}),_TC=new T(function(){return [0,"false"];}),_TD=function(_TE){var _TF=jsShow(E(_TE)[1]),_TG=_TF;return [0,_TG];},_TH=function(_TI){var _TJ=jsStringify(E(_TI)[1]),_TK=_TJ;return [0,_TK];},_TL=new T(function(){return [0,"null"];}),_TM=[0,91],_TN=[1,_TM,_i],_TO=new T(function(){return [0,toJSStr(_TN)];}),_TP=[0,123],_TQ=[1,_TP,_i],_TR=new T(function(){return [0,toJSStr(_TQ)];}),_TS=[0,34],_TT=[1,_TS,_i],_TU=new T(function(){return [0,toJSStr(_TT)];}),_TV=new T(function(){return [0,"true"];}),_TW=function(_TX,_TY){var _TZ=E(_TY);switch(_TZ[0]){case 0:return [0,new T(function(){return B(_TD(_TZ[1]));}),_TX];case 1:return [0,new T(function(){return B(_TH(_TZ[1]));}),_TX];case 2:return !E(_TZ[1])?[0,_TC,_TX]:[0,_TV,_TX];case 3:var _U0=E(_TZ[1]);return _U0[0]==0?[0,_TO,[1,_Ts,_TX]]:[0,_TO,new T(function(){var _U1=B(_TW(new T(function(){var _U2=function(_U3){var _U4=E(_U3);return _U4[0]==0?E([1,_Ts,_TX]):[1,_TB,new T(function(){var _U5=B(_TW(new T(function(){return B(_U2(_U4[2]));}),_U4[1]));return [1,_U5[1],_U5[2]];})];};return B(_U2(_U0[2]));}),_U0[1]));return [1,_U1[1],_U1[2]];})];case 4:var _U6=E(_TZ[1]);if(!_U6[0]){return [0,_TR,[1,_Tv,_TX]];}else{var _U7=E(_U6[1]);return [0,_TR,[1,new T(function(){return B(_TH(_U7[1]));}),[1,_Ty,new T(function(){var _U8=B(_TW(new T(function(){var _U9=function(_Ua){var _Ub=E(_Ua);if(!_Ub[0]){return E([1,_Tv,_TX]);}else{var _Uc=E(_Ub[1]);return [1,_TB,[1,_TU,[1,_Uc[1],[1,_TU,[1,_Ty,new T(function(){var _Ud=B(_TW(new T(function(){return B(_U9(_Ub[2]));}),_Uc[2]));return [1,_Ud[1],_Ud[2]];})]]]]];}};return B(_U9(_U6[2]));}),_U7[2]));return [1,_U8[1],_U8[2]];})]]];}break;default:return [0,_TL,_TX];}},_Ue=function(_Uf){var _Ug=jsCat(new T(function(){var _Uh=B(_TW(_i,_Uf));return [1,_Uh[1],_Uh[2]];}),E(_Tp)[1]),_Ui=_Ug;return E(_Ui);},_Uj=new T(function(){return [0,"(function(k,v) {localStorage.setItem(k,v);})"];}),_Uk=function(_Ul,_Um){return function(_Un,_){var _Uo=B(A(new T(function(){return B(A(_lF,[E(_Uj)[1],E(toJSStr(E(_Um)))]));}),[E(B(_Ue(B(A(new T(function(){return B(_eU(_Ul));}),[_Un]))))),_])),_Up=_Uo;return _4L;};},_Uq=new T(function(){return B(_Uk(_ha,_To));}),_Ur=function(_Us,_){var _Ut=B(A(_Uq,[_Us,_])),_Uu=_Ut;return new F(function(){return _Qt(_Us,_);});},_Uv=function(_Uw,_Ux){while(1){var _Uy=E(_Uw);if(!_Uy[0]){var _Uz=E(_Uy[1]);if(_Uz==(-2147483648)){_Uw=[1,I_fromInt(-2147483648)];continue;}else{var _UA=E(_Ux);if(!_UA[0]){return [0,B(_to(_Uz,_UA[1]))];}else{_Uw=[1,I_fromInt(_Uz)];_Ux=_UA;continue;}}}else{var _UB=_Uy[1],_UC=E(_Ux);return _UC[0]==0?[0,I_toInt(I_div(_UB,I_fromInt(_UC[1])))]:[1,I_div(_UB,_UC[1])];}}},_UD=[0,3],_UE=[0,2],_UF=new T(function(){return B(_nP(_UD,_28));}),_UG=[1,_iV,_i],_UH=function(_UI,_UJ){while(1){var _UK=E(_UI);if(!_UK[0]){var _UL=_UK[1],_UM=E(_UJ);if(!_UM[0]){var _UN=_UM[1],_UO=subC(_UL,_UN);if(!E(_UO[2])){return [0,_UO[1]];}else{_UI=[1,I_fromInt(_UL)];_UJ=[1,I_fromInt(_UN)];continue;}}else{_UI=[1,I_fromInt(_UL)];_UJ=_UM;continue;}}else{var _UP=E(_UJ);if(!_UP[0]){_UI=_UK;_UJ=[1,I_fromInt(_UP[1])];continue;}else{return [1,I_sub(_UK[1],_UP[1])];}}}},_UQ=function(_UR,_){var _US=jsEval(toJSStr(B(unAppCStr("document.title = ",[1,_iV,new T(function(){return B(_iX(B(_1j(_mb,B(_KW(_mH,new T(function(){return B(_mI([1,[4,new T(function(){return E(E(_UR)[1]);})],_i],_i));}))))),_UG));})])))),_UT=_US,_UU=E(_UR);if(!E(_UU[6])){return [0,_4L,_UU];}else{var _UV=B(_PZ(_)),_UW=_UV;return [0,_4L,[0,_UU[1],_UU[2],_UU[3],_UW,new T(function(){if(!E(_UF)){var _UX=B(_Uv(B(_5Z(B(_UH(_UW,_UU[4])),B(_6h(_UU[5],_UE)))),_UD));}else{var _UX=E(_u6);}return _UX;}),_b,_UU[7],_UU[8]]];}},_UY=function(_UZ,_V0,_V1,_){var _V2=rMV(_V0),_V3=_V2,_V4=B(A(_V1,[_V3,_])),_V5=_V4,_=wMV(_V0,new T(function(){return E(E(_V5)[2]);})),_V6=jsSetTimeout(_UZ,function(_){var _V7=B(_UY(_UZ,_V0,_V1,_)),_V8=_V7;return _4L;});return new F(function(){return rMV(_V0);});},_V9=function(_){var _=0,_Va=jsMkStdout(),_Vb=_Va;return [0,_Vb];},_Vc=new T(function(){return B(_lB(_V9));}),_Vd=function(_){var _Ve=B(_PZ(_)),_Vf=_Ve,_Vg=B(_lN(_ha,_To,_)),_Vh=_Vg,_Vi=nMV(new T(function(){var _Vj=E(_Vh);return _Vj[0]==0?[0,_Tn,_Tn,_Tn,_Vf,_28,_f,_g1,_g1]:E(_Vj[1]);})),_Vk=_Vi,_Vl=B(_UY(33,_Vk,_ST,_)),_Vm=_Vl,_Vn=B(_m6(_Vc,B(_lw(_Vm)),_)),_Vo=_Vn,_Vp=B(_UY(1000,_Vk,_UQ,_)),_Vq=_Vp,_Vr=B(_m6(_Vc,B(_lw(_Vq)),_)),_Vs=_Vr,_Vt=B(_UY(60000,_Vk,_Ur,_)),_Vu=_Vt;return new F(function(){return _m6(_Vc,B(_lw(_Vu)),_);});},_Vv=function(_){return new F(function(){return _Vd(_);});};
var hasteMain = function() {B(A(_Vv, [0]));};window.onload = hasteMain;
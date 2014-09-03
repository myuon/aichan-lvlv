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

var _0=new T(function(){return B(unCStr("depend"));}),_1=new T(function(){return [0,toJSStr(E(_0))];}),_2=new T(function(){return B(unCStr("lps"));}),_3=new T(function(){return [0,toJSStr(E(_2))];}),_4=new T(function(){return B(unCStr("loves"));}),_5=new T(function(){return [0,toJSStr(E(_4))];}),_6=new T(function(){return B(unCStr("maxLoves"));}),_7=new T(function(){return [0,toJSStr(E(_6))];}),_8=new T(function(){return B(unCStr("items"));}),_9=new T(function(){return [0,toJSStr(E(_8))];}),_a=new T(function(){return B(unCStr("achievements"));}),_b=new T(function(){return [0,toJSStr(E(_a))];}),_c=new T(function(){return B(unCStr("lastFocus"));}),_d=new T(function(){return [0,toJSStr(E(_c))];}),_e=function(_f){return [0,toJSStr(E(_f))];},_g=function(_h){return [1,new T(function(){return B(_e(_h));})];},_i=new T(function(){return [0,"value"];}),_j=true,_k=[2,_j],_l=new T(function(){return [0,"hasValue"];}),_m=[0,_l,_k],_n=false,_o=[2,_n],_p=[0,_l,_o],_q=[0],_r=[1,_p,_q],_s=[4,_r],_t=function(_u,_v){while(1){var _w=(function(_x,_y){var _z=E(_y);switch(_z[0]){case 0:_u=new T(function(){return B(_t(_x,_z[4]));});_v=_z[3];return null;case 1:return [1,[3,[1,[0,[0,_z[1]]],[1,new T(function(){var _A=E(_z[2]);return _A[0]==0?E(_s):[4,[1,_m,[1,[0,_i,new T(function(){return B(_g(_A[1]));})],_q]]];}),_q]]],_x];default:return E(_x);}})(_u,_v);if(_w!=null){return _w;}}},_B=function(_C){return [0,new T(function(){return [0,E(_C)[1]];})];},_D=function(_E,_F){while(1){var _G=(function(_H,_I){var _J=E(_I);switch(_J[0]){case 0:_E=new T(function(){return B(_D(_H,_J[4]));});_F=_J[3];return null;case 1:return [1,[3,[1,[0,[0,_J[1]]],[1,new T(function(){return B(_B(_J[2]));}),_q]]],_H];default:return E(_H);}})(_E,_F);if(_G!=null){return _G;}}},_K=function(_L,_M){var _N=E(_L);return _N[0]==0?E(_M):[1,_N[1],new T(function(){return B(_K(_N[2],_M));})];},_O=function(_P){while(1){var _Q=E(_P);if(!_Q[0]){_P=[1,I_fromInt(_Q[1])];continue;}else{return new F(function(){return I_toString(_Q[1]);});}}},_R=function(_S,_T){return new F(function(){return _K(fromJSStr(B(_O(_S))),_T);});},_U=function(_V,_W){var _X=E(_V);if(!_X[0]){var _Y=_X[1],_Z=E(_W);return _Z[0]==0?_Y<_Z[1]:I_compareInt(_Z[1],_Y)>0;}else{var _10=_X[1],_11=E(_W);return _11[0]==0?I_compareInt(_10,_11[1])<0:I_compare(_10,_11[1])<0;}},_12=[0,41],_13=[0,40],_14=[0,0],_15=function(_16,_17,_18){return _16<=6?B(_R(_17,_18)):!B(_U(_17,_14))?B(_R(_17,_18)):[1,_13,new T(function(){return B(_K(fromJSStr(B(_O(_17))),[1,_12,_18]));})];},_19=function(_1a,_1b,_1c,_1d,_1e,_1f,_1g){return [1,[0,_5,[0,_1a]],[1,[0,_3,[0,_1b]],[1,[0,_1,[0,_1c]],[1,[0,_d,[1,new T(function(){return [0,toJSStr(B(_15(0,_1d,_q)))];})]],[1,[0,_b,[3,new T(function(){var _1h=E(_1e);if(!_1h[0]){var _1i=_1h[3],_1j=_1h[4],_1k=_1h[2]>=0?B(_t(new T(function(){return B(_t(_q,_1j));}),_1i)):B(_t(new T(function(){return B(_t(_q,_1i));}),_1j));}else{var _1k=B(_t(_q,_1h));}return _1k;})]],[1,[0,_9,[3,new T(function(){var _1l=E(_1f);if(!_1l[0]){var _1m=_1l[3],_1n=_1l[4],_1o=_1l[2]>=0?B(_D(new T(function(){return B(_D(_q,_1n));}),_1m)):B(_D(new T(function(){return B(_D(_q,_1m));}),_1n));}else{var _1o=B(_D(_q,_1l));}return _1o;})]],[1,[0,_7,[0,_1g]],_q]]]]]]];},_1p=function(_1q){var _1r=E(_1q);return [4,B(_19(_1r[1],_1r[2],_1r[3],_1r[4],_1r[6],_1r[7],_1r[8]))];},_1s=function(_1t,_1u){var _1v=E(_1u);return _1v[0]==0?[0]:[1,new T(function(){return B(A(_1t,[_1v[1]]));}),new T(function(){return B(_1s(_1t,_1v[2]));})];},_1w=function(_1x){return [3,new T(function(){return B(_1s(_1p,_1x));})];},_1y=new T(function(){return B(unCStr("Tried to deserialize long string to a Char"));}),_1z=[0,_1y],_1A=new T(function(){return B(unCStr("Tried to deserialize a non-string to a Char"));}),_1B=[0,_1A],_1C=function(_1D){var _1E=E(_1D);if(_1E[0]==1){var _1F=fromJSStr(E(_1E[1])[1]);return _1F[0]==0?E(_1z):E(_1F[2])[0]==0?[1,_1F[1]]:E(_1z);}else{return E(_1B);}},_1G=new T(function(){return B(unCStr("Tried to deserialize a non-JSString to a JSString"));}),_1H=[0,_1G],_1I=function(_1J){return new F(function(){return fromJSStr(E(_1J)[1]);});},_1K=function(_1L){var _1M=E(_1L);return _1M[0]==1?[1,new T(function(){return B(_1I(_1M[1]));})]:E(_1H);},_1N=function(_1O){return [1,new T(function(){return [0,toJSStr([1,_1O,_q])];})];},_1P=[0,_1N,_g,_1C,_1K],_1Q=function(_1R){return E(E(_1R)[2]);},_1S=function(_1T,_1U){return [3,new T(function(){return B(_1s(new T(function(){return B(_1Q(_1T));}),_1U));})];},_1V=[1,_q],_1W=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_1X=[0,_1W],_1Y=function(_1Z){return E(E(_1Z)[4]);},_20=function(_21,_22){var _23=E(_22);if(_23[0]==3){var _24=function(_25){var _26=E(_25);if(!_26[0]){return E(_1V);}else{var _27=B(A(new T(function(){return B(_1Y(_21));}),[_26[1]]));if(!_27[0]){return [0,_27[1]];}else{var _28=B(_24(_26[2]));return _28[0]==0?[0,_28[1]]:[1,[1,_27[1],_28[1]]];}}};return new F(function(){return _24(_23[1]);});}else{return E(_1X);}},_29=function(_2a){return [0,new T(function(){return B(_1Q(_2a));}),function(_2b){return new F(function(){return _1S(_2a,_2b);});},new T(function(){return B(_1Y(_2a));}),function(_2b){return new F(function(){return _20(_2a,_2b);});}];},_2c=new T(function(){return B(_29(_1P));}),_2d=function(_2e){return E(E(_2e)[1]);},_2f=function(_2g,_2h){var _2i=E(_2h);return _2i[0]==0?E(_s):[4,[1,_m,[1,[0,_i,new T(function(){return B(A(_2d,[_2g,_2i[1]]));})],_q]]];},_2j=function(_2k,_2l){return [3,new T(function(){return B(_1s(function(_2b){return new F(function(){return _2f(_2k,_2b);});},_2l));})];},_2m=function(_2n,_2o){var _2p=strEq(E(_2n)[1],E(_2o)[1]),_2q=_2p;return E(_2q)==0?true:false;},_2r=function(_2s,_2t){var _2u=strEq(E(_2s)[1],E(_2t)[1]),_2v=_2u;return E(_2v)==0?false:true;},_2w=[0,_2r,_2m],_2x=[0],_2y=[1,_2x],_2z=new T(function(){return B(unCStr("Tried to deserialize a non-Bool to a Bool"));}),_2A=[0,_2z],_2B=new T(function(){return B(unCStr("Key not found"));}),_2C=[0,_2B],_2D=new T(function(){return B(unCStr("Tried to do lookup on non-object!"));}),_2E=[0,_2D],_2F=function(_2G){return E(E(_2G)[1]);},_2H=function(_2I,_2J,_2K){while(1){var _2L=E(_2K);if(!_2L[0]){return [0];}else{var _2M=E(_2L[1]);if(!B(A(_2F,[_2I,_2J,_2M[1]]))){_2K=_2L[2];continue;}else{return [1,_2M[2]];}}}},_2N=function(_2O){return E(E(_2O)[3]);},_2P=function(_2Q,_2R){var _2S=E(_2R);if(_2S[0]==4){var _2T=_2S[1],_2U=B(_2H(_2w,_l,_2T));if(!_2U[0]){return E(_2C);}else{var _2V=E(_2U[1]);if(_2V[0]==2){if(!E(_2V[1])){return E(_2y);}else{var _2W=B(_2H(_2w,_i,_2T));if(!_2W[0]){return E(_2C);}else{var _2X=B(A(_2N,[_2Q,_2W[1]]));return _2X[0]==0?[0,_2X[1]]:[1,[1,_2X[1]]];}}}else{return E(_2A);}}}else{return E(_2E);}},_2Y=[1,_q],_2Z=[0,_1W],_30=function(_31,_32){var _33=E(_32);if(_33[0]==3){var _34=function(_35){var _36=E(_35);if(!_36[0]){return E(_2Y);}else{var _37=B(_2P(_31,_36[1]));if(!_37[0]){return [0,_37[1]];}else{var _38=B(_34(_36[2]));return _38[0]==0?[0,_38[1]]:[1,[1,_37[1],_38[1]]];}}};return new F(function(){return _34(_33[1]);});}else{return E(_2Z);}},_39=function(_3a){return [0,function(_2b){return new F(function(){return _2f(_3a,_2b);});},function(_2b){return new F(function(){return _2j(_3a,_2b);});},function(_2b){return new F(function(){return _2P(_3a,_2b);});},function(_2b){return new F(function(){return _30(_3a,_2b);});}];},_3b=new T(function(){return B(_39(_2c));}),_3c=[1,_q],_3d=[0,_1W],_3e=new T(function(){return B(unCStr("Tried to deserialize a non-array into a pair!"));}),_3f=[0,_3e],_3g=function(_3h,_3i,_3j){var _3k=E(_3j);if(_3k[0]==3){var _3l=E(_3k[1]);if(!_3l[0]){return E(_3f);}else{var _3m=E(_3l[2]);if(!_3m[0]){return E(_3f);}else{if(!E(_3m[2])[0]){var _3n=B(A(_2N,[_3h,_3l[1]]));if(!_3n[0]){return [0,_3n[1]];}else{var _3o=B(A(_2N,[_3i,_3m[1]]));return _3o[0]==0?[0,_3o[1]]:[1,[0,_3n[1],_3o[1]]];}}else{return E(_3f);}}}}else{return E(_3f);}},_3p=function(_3q,_3r,_3s){var _3t=E(_3s);if(_3t[0]==3){var _3u=function(_3v){var _3w=E(_3v);if(!_3w[0]){return E(_3c);}else{var _3x=B(_3g(_3q,_3r,_3w[1]));if(!_3x[0]){return [0,_3x[1]];}else{var _3y=B(_3u(_3w[2]));return _3y[0]==0?[0,_3y[1]]:[1,[1,_3x[1],_3y[1]]];}}};return new F(function(){return _3u(_3t[1]);});}else{return E(_3d);}},_3z=function(_3A){return [3,new T(function(){return B(_1s(_B,_3A));})];},_3B=new T(function(){return B(unCStr("The given Number can\'t be represented as an Int"));}),_3C=[0,_3B],_3D=new T(function(){return B(unCStr("Tried to deserialize a non-Number to an Int"));}),_3E=[0,_3D],_3F=function(_3G){var _3H=E(_3G);if(!_3H[0]){var _3I=E(_3H[1])[1],_3J=_3I&4294967295;return _3J!=_3I?E(_3C):[1,[0,_3J]];}else{return E(_3E);}},_3K=[0,_1W],_3L=[1,_q],_3M=[0,_3B],_3N=[0,_3D],_3O=function(_3P){var _3Q=E(_3P);if(!_3Q[0]){return E(_3L);}else{var _3R=E(_3Q[1]);if(!_3R[0]){var _3S=E(_3R[1])[1],_3T=_3S&4294967295;if(_3T!=_3S){return E(_3M);}else{var _3U=B(_3O(_3Q[2]));return _3U[0]==0?[0,_3U[1]]:[1,[1,[0,_3T],_3U[1]]];}}else{return E(_3N);}}},_3V=function(_3W){var _3X=E(_3W);return _3X[0]==3?B(_3O(_3X[1])):E(_3K);},_3Y=[0,_B,_3z,_3F,_3V],_3Z=[2],_40=function(_41,_42,_43){var _44=E(_43);switch(_44[0]){case 0:var _45=_44[1],_46=_44[2],_47=_44[3],_48=_44[4],_49=_46>>>0;if(((_41>>>0&((_49-1>>>0^4294967295)>>>0^_49)>>>0)>>>0&4294967295)==_45){return (_41>>>0&_49)>>>0==0?[0,_45,_46,E(B(_40(_41,_42,_47))),E(_48)]:[0,_45,_46,E(_47),E(B(_40(_41,_42,_48)))];}else{var _4a=(_41>>>0^_45>>>0)>>>0,_4b=(_4a|_4a>>>1)>>>0,_4c=(_4b|_4b>>>2)>>>0,_4d=(_4c|_4c>>>4)>>>0,_4e=(_4d|_4d>>>8)>>>0,_4f=(_4e|_4e>>>16)>>>0,_4g=(_4f^_4f>>>1)>>>0&4294967295,_4h=_4g>>>0;return (_41>>>0&_4h)>>>0==0?[0,(_41>>>0&((_4h-1>>>0^4294967295)>>>0^_4h)>>>0)>>>0&4294967295,_4g,E([1,_41,_42]),E(_44)]:[0,(_41>>>0&((_4h-1>>>0^4294967295)>>>0^_4h)>>>0)>>>0&4294967295,_4g,E(_44),E([1,_41,_42])];}break;case 1:var _4i=_44[1];if(_41!=_4i){var _4j=(_41>>>0^_4i>>>0)>>>0,_4k=(_4j|_4j>>>1)>>>0,_4l=(_4k|_4k>>>2)>>>0,_4m=(_4l|_4l>>>4)>>>0,_4n=(_4m|_4m>>>8)>>>0,_4o=(_4n|_4n>>>16)>>>0,_4p=(_4o^_4o>>>1)>>>0&4294967295,_4q=_4p>>>0;return (_41>>>0&_4q)>>>0==0?[0,(_41>>>0&((_4q-1>>>0^4294967295)>>>0^_4q)>>>0)>>>0&4294967295,_4p,E([1,_41,_42]),E(_44)]:[0,(_41>>>0&((_4q-1>>>0^4294967295)>>>0^_4q)>>>0)>>>0&4294967295,_4p,E(_44),E([1,_41,_42])];}else{return [1,_41,_42];}break;default:return [1,_41,_42];}},_4r=function(_4s,_4t){while(1){var _4u=E(_4t);if(!_4u[0]){return E(_4s);}else{var _4v=E(_4u[1]),_4w=B(_40(E(_4v[1])[1],_4v[2],_4s));_4t=_4u[2];_4s=_4w;continue;}}},_4x=function(_4y){return new F(function(){return _4r(_3Z,_4y);});},_4z=new T(function(){return B(unCStr("Control.Exception.Base"));}),_4A=new T(function(){return B(unCStr("base"));}),_4B=new T(function(){return B(unCStr("PatternMatchFail"));}),_4C=new T(function(){var _4D=hs_wordToWord64(18445595),_4E=_4D,_4F=hs_wordToWord64(52003073),_4G=_4F;return [0,_4E,_4G,[0,_4E,_4G,_4A,_4z,_4B],_q];}),_4H=function(_4I){return E(_4C);},_4J=function(_4K){return E(E(_4K)[1]);},_4L=function(_4M,_4N,_4O){var _4P=B(A(_4M,[_])),_4Q=B(A(_4N,[_])),_4R=hs_eqWord64(_4P[1],_4Q[1]),_4S=_4R;if(!E(_4S)){return [0];}else{var _4T=hs_eqWord64(_4P[2],_4Q[2]),_4U=_4T;return E(_4U)==0?[0]:[1,_4O];}},_4V=function(_4W){var _4X=E(_4W);return new F(function(){return _4L(B(_4J(_4X[1])),_4H,_4X[2]);});},_4Y=function(_4Z){return E(E(_4Z)[1]);},_50=function(_51,_52){return new F(function(){return _K(E(_51)[1],_52);});},_53=[0,44],_54=[0,93],_55=[0,91],_56=function(_57,_58,_59){var _5a=E(_58);return _5a[0]==0?B(unAppCStr("[]",_59)):[1,_55,new T(function(){return B(A(_57,[_5a[1],new T(function(){var _5b=function(_5c){var _5d=E(_5c);return _5d[0]==0?E([1,_54,_59]):[1,_53,new T(function(){return B(A(_57,[_5d[1],new T(function(){return B(_5b(_5d[2]));})]));})];};return B(_5b(_5a[2]));})]));})];},_5e=function(_5f,_5g){return new F(function(){return _56(_50,_5f,_5g);});},_5h=function(_5i,_5j,_5k){return new F(function(){return _K(E(_5j)[1],_5k);});},_5l=[0,_5h,_4Y,_5e],_5m=new T(function(){return [0,_4H,_5l,_5n,_4V];}),_5n=function(_5o){return [0,_5m,_5o];},_5p=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_5q=function(_5r,_5s){return new F(function(){return die(new T(function(){return B(A(_5s,[_5r]));}));});},_5t=function(_5u,_5v){var _5w=E(_5v);if(!_5w[0]){return [0,_q,_q];}else{var _5x=_5w[1];if(!B(A(_5u,[_5x]))){return [0,_q,_5w];}else{var _5y=new T(function(){var _5z=B(_5t(_5u,_5w[2]));return [0,_5z[1],_5z[2]];});return [0,[1,_5x,new T(function(){return E(E(_5y)[1]);})],new T(function(){return E(E(_5y)[2]);})];}}},_5A=[0,32],_5B=[0,10],_5C=[1,_5B,_q],_5D=function(_5E){return E(E(_5E)[1])==124?false:true;},_5F=function(_5G,_5H){var _5I=B(_5t(_5D,B(unCStr(_5G)))),_5J=_5I[1],_5K=function(_5L,_5M){return new F(function(){return _K(_5L,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_K(_5H,new T(function(){return B(_K(_5M,_5C));})));})));}));});},_5N=E(_5I[2]);if(!_5N[0]){return new F(function(){return _5K(_5J,_q);});}else{return E(E(_5N[1])[1])==124?B(_5K(_5J,[1,_5A,_5N[2]])):B(_5K(_5J,_q));}},_5O=function(_5P){return new F(function(){return _5q([0,new T(function(){return B(_5F(_5P,_5p));})],_5n);});},_5Q=new T(function(){return B(_5O("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_5R=function(_5S,_5T){while(1){var _5U=(function(_5V,_5W){var _5X=E(_5V);switch(_5X[0]){case 0:var _5Y=E(_5W);if(!_5Y[0]){return [0];}else{_5S=B(A(_5X[1],[_5Y[1]]));_5T=_5Y[2];return null;}break;case 1:var _5Z=B(A(_5X[1],[_5W])),_60=_5W;_5S=_5Z;_5T=_60;return null;case 2:return [0];case 3:return [1,[0,_5X[1],_5W],new T(function(){return B(_5R(_5X[2],_5W));})];default:return E(_5X[1]);}})(_5S,_5T);if(_5U!=null){return _5U;}}},_61=function(_62,_63){var _64=function(_65){var _66=E(_63);if(_66[0]==3){return [3,_66[1],new T(function(){return B(_61(_62,_66[2]));})];}else{var _67=E(_62);if(_67[0]==2){return E(_66);}else{var _68=E(_66);if(_68[0]==2){return E(_67);}else{var _69=function(_6a){var _6b=E(_68);if(_6b[0]==4){return [1,function(_6c){return [4,new T(function(){return B(_K(B(_5R(_67,_6c)),_6b[1]));})];}];}else{var _6d=E(_67);if(_6d[0]==1){var _6e=_6d[1],_6f=E(_6b);return _6f[0]==0?[1,function(_6g){return new F(function(){return _61(B(A(_6e,[_6g])),_6f);});}]:[1,function(_6h){return new F(function(){return _61(B(A(_6e,[_6h])),new T(function(){return B(A(_6f[1],[_6h]));}));});}];}else{var _6i=E(_6b);return _6i[0]==0?E(_5Q):[1,function(_6j){return new F(function(){return _61(_6d,new T(function(){return B(A(_6i[1],[_6j]));}));});}];}}},_6k=E(_67);switch(_6k[0]){case 1:var _6l=E(_68);if(_6l[0]==4){return [1,function(_6m){return [4,new T(function(){return B(_K(B(_5R(B(A(_6k[1],[_6m])),_6m)),_6l[1]));})];}];}else{return new F(function(){return _69(_);});}break;case 4:var _6n=_6k[1],_6o=E(_68);switch(_6o[0]){case 0:return [1,function(_6p){return [4,new T(function(){return B(_K(_6n,new T(function(){return B(_5R(_6o,_6p));})));})];}];case 1:return [1,function(_6q){return [4,new T(function(){return B(_K(_6n,new T(function(){return B(_5R(B(A(_6o[1],[_6q])),_6q));})));})];}];default:return [4,new T(function(){return B(_K(_6n,_6o[1]));})];}break;default:return new F(function(){return _69(_);});}}}}},_6r=E(_62);switch(_6r[0]){case 0:var _6s=E(_63);if(!_6s[0]){return [0,function(_6t){return new F(function(){return _61(B(A(_6r[1],[_6t])),new T(function(){return B(A(_6s[1],[_6t]));}));});}];}else{return new F(function(){return _64(_);});}break;case 3:return [3,_6r[1],new T(function(){return B(_61(_6r[2],_63));})];default:return new F(function(){return _64(_);});}},_6u=[0,41],_6v=[1,_6u,_q],_6w=[0,40],_6x=[1,_6w,_q],_6y=function(_6z,_6A){while(1){var _6B=E(_6z);if(!_6B[0]){return E(_6A)[0]==0?true:false;}else{var _6C=E(_6A);if(!_6C[0]){return false;}else{if(E(_6B[1])[1]!=E(_6C[1])[1]){return false;}else{_6z=_6B[2];_6A=_6C[2];continue;}}}}},_6D=function(_6E,_6F){return E(_6E)[1]!=E(_6F)[1];},_6G=function(_6H,_6I){return E(_6H)[1]==E(_6I)[1];},_6J=[0,_6G,_6D],_6K=function(_6L,_6M){while(1){var _6N=E(_6L);if(!_6N[0]){return E(_6M)[0]==0?true:false;}else{var _6O=E(_6M);if(!_6O[0]){return false;}else{if(E(_6N[1])[1]!=E(_6O[1])[1]){return false;}else{_6L=_6N[2];_6M=_6O[2];continue;}}}}},_6P=function(_6Q,_6R){return !B(_6K(_6Q,_6R))?true:false;},_6S=[0,_6K,_6P],_6T=function(_6U,_6V){var _6W=E(_6U);switch(_6W[0]){case 0:return [0,function(_6X){return new F(function(){return _6T(B(A(_6W[1],[_6X])),_6V);});}];case 1:return [1,function(_6Y){return new F(function(){return _6T(B(A(_6W[1],[_6Y])),_6V);});}];case 2:return [2];case 3:return new F(function(){return _61(B(A(_6V,[_6W[1]])),new T(function(){return B(_6T(_6W[2],_6V));}));});break;default:var _6Z=function(_70){var _71=E(_70);if(!_71[0]){return [0];}else{var _72=E(_71[1]);return new F(function(){return _K(B(_5R(B(A(_6V,[_72[1]])),_72[2])),new T(function(){return B(_6Z(_71[2]));}));});}},_73=B(_6Z(_6W[1]));return _73[0]==0?[2]:[4,_73];}},_74=[2],_75=function(_76){return [3,_76,_74];},_77=0,_78=function(_79,_7a){var _7b=E(_79);if(!_7b){return new F(function(){return A(_7a,[_77]);});}else{return [0,function(_7c){return E(new T(function(){return B(_78(_7b-1|0,_7a));}));}];}},_7d=function(_7e,_7f,_7g){return function(_7h){return new F(function(){return A(function(_7i,_7j,_7k){while(1){var _7l=(function(_7m,_7n,_7o){var _7p=E(_7m);switch(_7p[0]){case 0:var _7q=E(_7n);if(!_7q[0]){return E(_7f);}else{_7i=B(A(_7p[1],[_7q[1]]));_7j=_7q[2];var _7r=_7o+1|0;_7k=_7r;return null;}break;case 1:var _7s=B(A(_7p[1],[_7n])),_7t=_7n,_7r=_7o;_7i=_7s;_7j=_7t;_7k=_7r;return null;case 2:return E(_7f);case 3:return function(_7u){return new F(function(){return _78(_7o,function(_7v){return E(new T(function(){return B(_6T(_7p,_7u));}));});});};default:return function(_7w){return new F(function(){return _6T(_7p,_7w);});};}})(_7i,_7j,_7k);if(_7l!=null){return _7l;}}},[new T(function(){return B(A(_7e,[_75]));}),_7h,0,_7g]);});};},_7x=function(_7y){return new F(function(){return A(_7y,[_q]);});},_7z=function(_7A,_7B){var _7C=function(_7D){var _7E=E(_7D);if(!_7E[0]){return E(_7x);}else{var _7F=_7E[1];return !B(A(_7A,[_7F]))?E(_7x):function(_7G){return [0,function(_7H){return E(new T(function(){return B(A(new T(function(){return B(_7C(_7E[2]));}),[function(_7I){return new F(function(){return A(_7G,[[1,_7F,_7I]]);});}]));}));}];};}};return function(_7J){return new F(function(){return A(_7C,[_7J,_7B]);});};},_7K=[6],_7L=function(_7M){return E(_7M);},_7N=new T(function(){return B(unCStr("valDig: Bad base"));}),_7O=new T(function(){return B(err(_7N));}),_7P=function(_7Q,_7R){var _7S=function(_7T,_7U){var _7V=E(_7T);if(!_7V[0]){return function(_7W){return new F(function(){return A(_7W,[new T(function(){return B(A(_7U,[_q]));})]);});};}else{var _7X=E(_7V[1])[1],_7Y=function(_7Z){return function(_80){return [0,function(_81){return E(new T(function(){return B(A(new T(function(){return B(_7S(_7V[2],function(_82){return new F(function(){return A(_7U,[[1,_7Z,_82]]);});}));}),[_80]));}));}];};};switch(E(E(_7Q)[1])){case 8:if(48>_7X){return function(_83){return new F(function(){return A(_83,[new T(function(){return B(A(_7U,[_q]));})]);});};}else{if(_7X>55){return function(_84){return new F(function(){return A(_84,[new T(function(){return B(A(_7U,[_q]));})]);});};}else{return new F(function(){return _7Y([0,_7X-48|0]);});}}break;case 10:if(48>_7X){return function(_85){return new F(function(){return A(_85,[new T(function(){return B(A(_7U,[_q]));})]);});};}else{if(_7X>57){return function(_86){return new F(function(){return A(_86,[new T(function(){return B(A(_7U,[_q]));})]);});};}else{return new F(function(){return _7Y([0,_7X-48|0]);});}}break;case 16:if(48>_7X){if(97>_7X){if(65>_7X){return function(_87){return new F(function(){return A(_87,[new T(function(){return B(A(_7U,[_q]));})]);});};}else{if(_7X>70){return function(_88){return new F(function(){return A(_88,[new T(function(){return B(A(_7U,[_q]));})]);});};}else{return new F(function(){return _7Y([0,(_7X-65|0)+10|0]);});}}}else{if(_7X>102){if(65>_7X){return function(_89){return new F(function(){return A(_89,[new T(function(){return B(A(_7U,[_q]));})]);});};}else{if(_7X>70){return function(_8a){return new F(function(){return A(_8a,[new T(function(){return B(A(_7U,[_q]));})]);});};}else{return new F(function(){return _7Y([0,(_7X-65|0)+10|0]);});}}}else{return new F(function(){return _7Y([0,(_7X-97|0)+10|0]);});}}}else{if(_7X>57){if(97>_7X){if(65>_7X){return function(_8b){return new F(function(){return A(_8b,[new T(function(){return B(A(_7U,[_q]));})]);});};}else{if(_7X>70){return function(_8c){return new F(function(){return A(_8c,[new T(function(){return B(A(_7U,[_q]));})]);});};}else{return new F(function(){return _7Y([0,(_7X-65|0)+10|0]);});}}}else{if(_7X>102){if(65>_7X){return function(_8d){return new F(function(){return A(_8d,[new T(function(){return B(A(_7U,[_q]));})]);});};}else{if(_7X>70){return function(_8e){return new F(function(){return A(_8e,[new T(function(){return B(A(_7U,[_q]));})]);});};}else{return new F(function(){return _7Y([0,(_7X-65|0)+10|0]);});}}}else{return new F(function(){return _7Y([0,(_7X-97|0)+10|0]);});}}}else{return new F(function(){return _7Y([0,_7X-48|0]);});}}break;default:return E(_7O);}}};return function(_8f){return new F(function(){return A(_7S,[_8f,_7L,function(_8g){var _8h=E(_8g);return _8h[0]==0?[2]:B(A(_7R,[_8h]));}]);});};},_8i=[0,10],_8j=[0,1],_8k=[0,2147483647],_8l=function(_8m,_8n){while(1){var _8o=E(_8m);if(!_8o[0]){var _8p=_8o[1],_8q=E(_8n);if(!_8q[0]){var _8r=_8q[1],_8s=addC(_8p,_8r);if(!E(_8s[2])){return [0,_8s[1]];}else{_8m=[1,I_fromInt(_8p)];_8n=[1,I_fromInt(_8r)];continue;}}else{_8m=[1,I_fromInt(_8p)];_8n=_8q;continue;}}else{var _8t=E(_8n);if(!_8t[0]){_8m=_8o;_8n=[1,I_fromInt(_8t[1])];continue;}else{return [1,I_add(_8o[1],_8t[1])];}}}},_8u=new T(function(){return B(_8l(_8k,_8j));}),_8v=function(_8w){var _8x=E(_8w);if(!_8x[0]){var _8y=E(_8x[1]);return _8y==(-2147483648)?E(_8u):[0, -_8y];}else{return [1,I_negate(_8x[1])];}},_8z=[0,10],_8A=[0,0],_8B=function(_8C){return [0,_8C];},_8D=function(_8E,_8F){while(1){var _8G=E(_8E);if(!_8G[0]){var _8H=_8G[1],_8I=E(_8F);if(!_8I[0]){var _8J=_8I[1];if(!(imul(_8H,_8J)|0)){return [0,imul(_8H,_8J)|0];}else{_8E=[1,I_fromInt(_8H)];_8F=[1,I_fromInt(_8J)];continue;}}else{_8E=[1,I_fromInt(_8H)];_8F=_8I;continue;}}else{var _8K=E(_8F);if(!_8K[0]){_8E=_8G;_8F=[1,I_fromInt(_8K[1])];continue;}else{return [1,I_mul(_8G[1],_8K[1])];}}}},_8L=function(_8M,_8N,_8O){while(1){var _8P=E(_8O);if(!_8P[0]){return E(_8N);}else{var _8Q=B(_8l(B(_8D(_8N,_8M)),B(_8B(E(_8P[1])[1]))));_8O=_8P[2];_8N=_8Q;continue;}}},_8R=function(_8S){var _8T=new T(function(){return B(_61(B(_61([0,function(_8U){return E(E(_8U)[1])==45?[1,B(_7P(_8i,function(_8V){return new F(function(){return A(_8S,[[1,new T(function(){return B(_8v(B(_8L(_8z,_8A,_8V))));})]]);});}))]:[2];}],[0,function(_8W){return E(E(_8W)[1])==43?[1,B(_7P(_8i,function(_8X){return new F(function(){return A(_8S,[[1,new T(function(){return B(_8L(_8z,_8A,_8X));})]]);});}))]:[2];}])),new T(function(){return [1,B(_7P(_8i,function(_8Y){return new F(function(){return A(_8S,[[1,new T(function(){return B(_8L(_8z,_8A,_8Y));})]]);});}))];})));});return new F(function(){return _61([0,function(_8Z){return E(E(_8Z)[1])==101?E(_8T):[2];}],[0,function(_90){return E(E(_90)[1])==69?E(_8T):[2];}]);});},_91=function(_92){return new F(function(){return A(_92,[_2x]);});},_93=function(_94){return new F(function(){return A(_94,[_2x]);});},_95=function(_96){return function(_97){return E(E(_97)[1])==46?[1,B(_7P(_8i,function(_98){return new F(function(){return A(_96,[[1,_98]]);});}))]:[2];};},_99=function(_9a){return [0,B(_95(_9a))];},_9b=function(_9c){return new F(function(){return _7P(_8i,function(_9d){return [1,B(_7d(_99,_91,function(_9e){return [1,B(_7d(_8R,_93,function(_9f){return new F(function(){return A(_9c,[[5,[1,_9d,_9e,_9f]]]);});}))];}))];});});},_9g=function(_9h){return [1,B(_9b(_9h))];},_9i=function(_9j,_9k,_9l){while(1){var _9m=E(_9l);if(!_9m[0]){return false;}else{if(!B(A(_2F,[_9j,_9k,_9m[1]]))){_9l=_9m[2];continue;}else{return true;}}}},_9n=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_9o=function(_9p){return new F(function(){return _9i(_6J,_9p,_9n);});},_9q=[0,8],_9r=[0,16],_9s=function(_9t){var _9u=function(_9v){return new F(function(){return A(_9t,[[5,[0,_9q,_9v]]]);});},_9w=function(_9x){return new F(function(){return A(_9t,[[5,[0,_9r,_9x]]]);});};return function(_9y){return E(E(_9y)[1])==48?E([0,function(_9z){switch(E(E(_9z)[1])){case 79:return [1,B(_7P(_9q,_9u))];case 88:return [1,B(_7P(_9r,_9w))];case 111:return [1,B(_7P(_9q,_9u))];case 120:return [1,B(_7P(_9r,_9w))];default:return [2];}}]):[2];};},_9A=function(_9B){return [0,B(_9s(_9B))];},_9C=function(_9D){var _9E=new T(function(){return B(A(_9D,[_9q]));}),_9F=new T(function(){return B(A(_9D,[_9r]));});return function(_9G){switch(E(E(_9G)[1])){case 79:return E(_9E);case 88:return E(_9F);case 111:return E(_9E);case 120:return E(_9F);default:return [2];}};},_9H=function(_9I){return [0,B(_9C(_9I))];},_9J=[0,92],_9K=function(_9L){return new F(function(){return A(_9L,[_8i]);});},_9M=function(_9N,_9O){var _9P=jsShowI(_9N),_9Q=_9P;return new F(function(){return _K(fromJSStr(_9Q),_9O);});},_9R=function(_9S,_9T,_9U){if(_9T>=0){return new F(function(){return _9M(_9T,_9U);});}else{return _9S<=6?B(_9M(_9T,_9U)):[1,_13,new T(function(){var _9V=jsShowI(_9T),_9W=_9V;return B(_K(fromJSStr(_9W),[1,_12,_9U]));})];}},_9X=function(_9Y){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_9R(9,_9Y,_q));}))));});},_9Z=function(_a0){var _a1=E(_a0);return _a1[0]==0?E(_a1[1]):I_toInt(_a1[1]);},_a2=function(_a3,_a4){var _a5=E(_a3);if(!_a5[0]){var _a6=_a5[1],_a7=E(_a4);return _a7[0]==0?_a6<=_a7[1]:I_compareInt(_a7[1],_a6)>=0;}else{var _a8=_a5[1],_a9=E(_a4);return _a9[0]==0?I_compareInt(_a8,_a9[1])<=0:I_compare(_a8,_a9[1])<=0;}},_aa=function(_ab){return [2];},_ac=function(_ad){var _ae=E(_ad);if(!_ae[0]){return E(_aa);}else{var _af=_ae[1],_ag=E(_ae[2]);return _ag[0]==0?E(_af):function(_ah){return new F(function(){return _61(B(A(_af,[_ah])),new T(function(){return B(A(new T(function(){return B(_ac(_ag));}),[_ah]));}));});};}},_ai=function(_aj){return [2];},_ak=function(_al,_am){var _an=function(_ao,_ap){var _aq=E(_ao);if(!_aq[0]){return function(_ar){return new F(function(){return A(_ar,[_al]);});};}else{var _as=E(_ap);return _as[0]==0?E(_ai):E(_aq[1])[1]!=E(_as[1])[1]?E(_ai):function(_at){return [0,function(_au){return E(new T(function(){return B(A(new T(function(){return B(_an(_aq[2],_as[2]));}),[_at]));}));}];};}};return function(_av){return new F(function(){return A(_an,[_al,_av,_am]);});};},_aw=new T(function(){return B(unCStr("SOH"));}),_ax=[0,1],_ay=function(_az){return [1,B(_ak(_aw,function(_aA){return E(new T(function(){return B(A(_az,[_ax]));}));}))];},_aB=new T(function(){return B(unCStr("SO"));}),_aC=[0,14],_aD=function(_aE){return [1,B(_ak(_aB,function(_aF){return E(new T(function(){return B(A(_aE,[_aC]));}));}))];},_aG=function(_aH){return [1,B(_7d(_ay,_aD,_aH))];},_aI=new T(function(){return B(unCStr("NUL"));}),_aJ=[0,0],_aK=function(_aL){return [1,B(_ak(_aI,function(_aM){return E(new T(function(){return B(A(_aL,[_aJ]));}));}))];},_aN=new T(function(){return B(unCStr("STX"));}),_aO=[0,2],_aP=function(_aQ){return [1,B(_ak(_aN,function(_aR){return E(new T(function(){return B(A(_aQ,[_aO]));}));}))];},_aS=new T(function(){return B(unCStr("ETX"));}),_aT=[0,3],_aU=function(_aV){return [1,B(_ak(_aS,function(_aW){return E(new T(function(){return B(A(_aV,[_aT]));}));}))];},_aX=new T(function(){return B(unCStr("EOT"));}),_aY=[0,4],_aZ=function(_b0){return [1,B(_ak(_aX,function(_b1){return E(new T(function(){return B(A(_b0,[_aY]));}));}))];},_b2=new T(function(){return B(unCStr("ENQ"));}),_b3=[0,5],_b4=function(_b5){return [1,B(_ak(_b2,function(_b6){return E(new T(function(){return B(A(_b5,[_b3]));}));}))];},_b7=new T(function(){return B(unCStr("ACK"));}),_b8=[0,6],_b9=function(_ba){return [1,B(_ak(_b7,function(_bb){return E(new T(function(){return B(A(_ba,[_b8]));}));}))];},_bc=new T(function(){return B(unCStr("BEL"));}),_bd=[0,7],_be=function(_bf){return [1,B(_ak(_bc,function(_bg){return E(new T(function(){return B(A(_bf,[_bd]));}));}))];},_bh=new T(function(){return B(unCStr("BS"));}),_bi=[0,8],_bj=function(_bk){return [1,B(_ak(_bh,function(_bl){return E(new T(function(){return B(A(_bk,[_bi]));}));}))];},_bm=new T(function(){return B(unCStr("HT"));}),_bn=[0,9],_bo=function(_bp){return [1,B(_ak(_bm,function(_bq){return E(new T(function(){return B(A(_bp,[_bn]));}));}))];},_br=new T(function(){return B(unCStr("LF"));}),_bs=[0,10],_bt=function(_bu){return [1,B(_ak(_br,function(_bv){return E(new T(function(){return B(A(_bu,[_bs]));}));}))];},_bw=new T(function(){return B(unCStr("VT"));}),_bx=[0,11],_by=function(_bz){return [1,B(_ak(_bw,function(_bA){return E(new T(function(){return B(A(_bz,[_bx]));}));}))];},_bB=new T(function(){return B(unCStr("FF"));}),_bC=[0,12],_bD=function(_bE){return [1,B(_ak(_bB,function(_bF){return E(new T(function(){return B(A(_bE,[_bC]));}));}))];},_bG=new T(function(){return B(unCStr("CR"));}),_bH=[0,13],_bI=function(_bJ){return [1,B(_ak(_bG,function(_bK){return E(new T(function(){return B(A(_bJ,[_bH]));}));}))];},_bL=new T(function(){return B(unCStr("SI"));}),_bM=[0,15],_bN=function(_bO){return [1,B(_ak(_bL,function(_bP){return E(new T(function(){return B(A(_bO,[_bM]));}));}))];},_bQ=new T(function(){return B(unCStr("DLE"));}),_bR=[0,16],_bS=function(_bT){return [1,B(_ak(_bQ,function(_bU){return E(new T(function(){return B(A(_bT,[_bR]));}));}))];},_bV=new T(function(){return B(unCStr("DC1"));}),_bW=[0,17],_bX=function(_bY){return [1,B(_ak(_bV,function(_bZ){return E(new T(function(){return B(A(_bY,[_bW]));}));}))];},_c0=new T(function(){return B(unCStr("DC2"));}),_c1=[0,18],_c2=function(_c3){return [1,B(_ak(_c0,function(_c4){return E(new T(function(){return B(A(_c3,[_c1]));}));}))];},_c5=new T(function(){return B(unCStr("DC3"));}),_c6=[0,19],_c7=function(_c8){return [1,B(_ak(_c5,function(_c9){return E(new T(function(){return B(A(_c8,[_c6]));}));}))];},_ca=new T(function(){return B(unCStr("DC4"));}),_cb=[0,20],_cc=function(_cd){return [1,B(_ak(_ca,function(_ce){return E(new T(function(){return B(A(_cd,[_cb]));}));}))];},_cf=new T(function(){return B(unCStr("NAK"));}),_cg=[0,21],_ch=function(_ci){return [1,B(_ak(_cf,function(_cj){return E(new T(function(){return B(A(_ci,[_cg]));}));}))];},_ck=new T(function(){return B(unCStr("SYN"));}),_cl=[0,22],_cm=function(_cn){return [1,B(_ak(_ck,function(_co){return E(new T(function(){return B(A(_cn,[_cl]));}));}))];},_cp=new T(function(){return B(unCStr("ETB"));}),_cq=[0,23],_cr=function(_cs){return [1,B(_ak(_cp,function(_ct){return E(new T(function(){return B(A(_cs,[_cq]));}));}))];},_cu=new T(function(){return B(unCStr("CAN"));}),_cv=[0,24],_cw=function(_cx){return [1,B(_ak(_cu,function(_cy){return E(new T(function(){return B(A(_cx,[_cv]));}));}))];},_cz=new T(function(){return B(unCStr("EM"));}),_cA=[0,25],_cB=function(_cC){return [1,B(_ak(_cz,function(_cD){return E(new T(function(){return B(A(_cC,[_cA]));}));}))];},_cE=new T(function(){return B(unCStr("SUB"));}),_cF=[0,26],_cG=function(_cH){return [1,B(_ak(_cE,function(_cI){return E(new T(function(){return B(A(_cH,[_cF]));}));}))];},_cJ=new T(function(){return B(unCStr("ESC"));}),_cK=[0,27],_cL=function(_cM){return [1,B(_ak(_cJ,function(_cN){return E(new T(function(){return B(A(_cM,[_cK]));}));}))];},_cO=new T(function(){return B(unCStr("FS"));}),_cP=[0,28],_cQ=function(_cR){return [1,B(_ak(_cO,function(_cS){return E(new T(function(){return B(A(_cR,[_cP]));}));}))];},_cT=new T(function(){return B(unCStr("GS"));}),_cU=[0,29],_cV=function(_cW){return [1,B(_ak(_cT,function(_cX){return E(new T(function(){return B(A(_cW,[_cU]));}));}))];},_cY=new T(function(){return B(unCStr("RS"));}),_cZ=[0,30],_d0=function(_d1){return [1,B(_ak(_cY,function(_d2){return E(new T(function(){return B(A(_d1,[_cZ]));}));}))];},_d3=new T(function(){return B(unCStr("US"));}),_d4=[0,31],_d5=function(_d6){return [1,B(_ak(_d3,function(_d7){return E(new T(function(){return B(A(_d6,[_d4]));}));}))];},_d8=new T(function(){return B(unCStr("SP"));}),_d9=[0,32],_da=function(_db){return [1,B(_ak(_d8,function(_dc){return E(new T(function(){return B(A(_db,[_d9]));}));}))];},_dd=new T(function(){return B(unCStr("DEL"));}),_de=[0,127],_df=function(_dg){return [1,B(_ak(_dd,function(_dh){return E(new T(function(){return B(A(_dg,[_de]));}));}))];},_di=[1,_df,_q],_dj=[1,_da,_di],_dk=[1,_d5,_dj],_dl=[1,_d0,_dk],_dm=[1,_cV,_dl],_dn=[1,_cQ,_dm],_do=[1,_cL,_dn],_dp=[1,_cG,_do],_dq=[1,_cB,_dp],_dr=[1,_cw,_dq],_ds=[1,_cr,_dr],_dt=[1,_cm,_ds],_du=[1,_ch,_dt],_dv=[1,_cc,_du],_dw=[1,_c7,_dv],_dx=[1,_c2,_dw],_dy=[1,_bX,_dx],_dz=[1,_bS,_dy],_dA=[1,_bN,_dz],_dB=[1,_bI,_dA],_dC=[1,_bD,_dB],_dD=[1,_by,_dC],_dE=[1,_bt,_dD],_dF=[1,_bo,_dE],_dG=[1,_bj,_dF],_dH=[1,_be,_dG],_dI=[1,_b9,_dH],_dJ=[1,_b4,_dI],_dK=[1,_aZ,_dJ],_dL=[1,_aU,_dK],_dM=[1,_aP,_dL],_dN=[1,_aK,_dM],_dO=[1,_aG,_dN],_dP=new T(function(){return B(_ac(_dO));}),_dQ=[0,1114111],_dR=[0,34],_dS=[0,39],_dT=function(_dU){var _dV=new T(function(){return B(A(_dU,[_bd]));}),_dW=new T(function(){return B(A(_dU,[_bi]));}),_dX=new T(function(){return B(A(_dU,[_bn]));}),_dY=new T(function(){return B(A(_dU,[_bs]));}),_dZ=new T(function(){return B(A(_dU,[_bx]));}),_e0=new T(function(){return B(A(_dU,[_bC]));}),_e1=new T(function(){return B(A(_dU,[_bH]));});return new F(function(){return _61([0,function(_e2){switch(E(E(_e2)[1])){case 34:return E(new T(function(){return B(A(_dU,[_dR]));}));case 39:return E(new T(function(){return B(A(_dU,[_dS]));}));case 92:return E(new T(function(){return B(A(_dU,[_9J]));}));case 97:return E(_dV);case 98:return E(_dW);case 102:return E(_e0);case 110:return E(_dY);case 114:return E(_e1);case 116:return E(_dX);case 118:return E(_dZ);default:return [2];}}],new T(function(){return B(_61([1,B(_7d(_9H,_9K,function(_e3){return [1,B(_7P(_e3,function(_e4){var _e5=B(_8L(new T(function(){return B(_8B(E(_e3)[1]));}),_8A,_e4));return !B(_a2(_e5,_dQ))?[2]:B(A(_dU,[new T(function(){var _e6=B(_9Z(_e5));if(_e6>>>0>1114111){var _e7=B(_9X(_e6));}else{var _e7=[0,_e6];}var _e8=_e7,_e9=_e8,_ea=_e9;return _ea;})]));}))];}))],new T(function(){return B(_61([0,function(_eb){return E(E(_eb)[1])==94?E([0,function(_ec){switch(E(E(_ec)[1])){case 64:return E(new T(function(){return B(A(_dU,[_aJ]));}));case 65:return E(new T(function(){return B(A(_dU,[_ax]));}));case 66:return E(new T(function(){return B(A(_dU,[_aO]));}));case 67:return E(new T(function(){return B(A(_dU,[_aT]));}));case 68:return E(new T(function(){return B(A(_dU,[_aY]));}));case 69:return E(new T(function(){return B(A(_dU,[_b3]));}));case 70:return E(new T(function(){return B(A(_dU,[_b8]));}));case 71:return E(_dV);case 72:return E(_dW);case 73:return E(_dX);case 74:return E(_dY);case 75:return E(_dZ);case 76:return E(_e0);case 77:return E(_e1);case 78:return E(new T(function(){return B(A(_dU,[_aC]));}));case 79:return E(new T(function(){return B(A(_dU,[_bM]));}));case 80:return E(new T(function(){return B(A(_dU,[_bR]));}));case 81:return E(new T(function(){return B(A(_dU,[_bW]));}));case 82:return E(new T(function(){return B(A(_dU,[_c1]));}));case 83:return E(new T(function(){return B(A(_dU,[_c6]));}));case 84:return E(new T(function(){return B(A(_dU,[_cb]));}));case 85:return E(new T(function(){return B(A(_dU,[_cg]));}));case 86:return E(new T(function(){return B(A(_dU,[_cl]));}));case 87:return E(new T(function(){return B(A(_dU,[_cq]));}));case 88:return E(new T(function(){return B(A(_dU,[_cv]));}));case 89:return E(new T(function(){return B(A(_dU,[_cA]));}));case 90:return E(new T(function(){return B(A(_dU,[_cF]));}));case 91:return E(new T(function(){return B(A(_dU,[_cK]));}));case 92:return E(new T(function(){return B(A(_dU,[_cP]));}));case 93:return E(new T(function(){return B(A(_dU,[_cU]));}));case 94:return E(new T(function(){return B(A(_dU,[_cZ]));}));case 95:return E(new T(function(){return B(A(_dU,[_d4]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_dP,[_dU]));})));})));}));});},_ed=function(_ee){return new F(function(){return A(_ee,[_77]);});},_ef=function(_eg){var _eh=E(_eg);if(!_eh[0]){return E(_ed);}else{var _ei=_eh[2],_ej=E(E(_eh[1])[1]);switch(_ej){case 9:return function(_ek){return [0,function(_el){return E(new T(function(){return B(A(new T(function(){return B(_ef(_ei));}),[_ek]));}));}];};case 10:return function(_em){return [0,function(_en){return E(new T(function(){return B(A(new T(function(){return B(_ef(_ei));}),[_em]));}));}];};case 11:return function(_eo){return [0,function(_ep){return E(new T(function(){return B(A(new T(function(){return B(_ef(_ei));}),[_eo]));}));}];};case 12:return function(_eq){return [0,function(_er){return E(new T(function(){return B(A(new T(function(){return B(_ef(_ei));}),[_eq]));}));}];};case 13:return function(_es){return [0,function(_et){return E(new T(function(){return B(A(new T(function(){return B(_ef(_ei));}),[_es]));}));}];};case 32:return function(_eu){return [0,function(_ev){return E(new T(function(){return B(A(new T(function(){return B(_ef(_ei));}),[_eu]));}));}];};case 160:return function(_ew){return [0,function(_ex){return E(new T(function(){return B(A(new T(function(){return B(_ef(_ei));}),[_ew]));}));}];};default:var _ey=u_iswspace(_ej),_ez=_ey;return E(_ez)==0?E(_ed):function(_eA){return [0,function(_eB){return E(new T(function(){return B(A(new T(function(){return B(_ef(_ei));}),[_eA]));}));}];};}}},_eC=function(_eD){var _eE=new T(function(){return B(_eC(_eD));}),_eF=[1,function(_eG){return new F(function(){return A(_ef,[_eG,function(_eH){return E([0,function(_eI){return E(E(_eI)[1])==92?E(_eE):[2];}]);}]);});}];return new F(function(){return _61([0,function(_eJ){return E(E(_eJ)[1])==92?E([0,function(_eK){var _eL=E(E(_eK)[1]);switch(_eL){case 9:return E(_eF);case 10:return E(_eF);case 11:return E(_eF);case 12:return E(_eF);case 13:return E(_eF);case 32:return E(_eF);case 38:return E(_eE);case 160:return E(_eF);default:var _eM=u_iswspace(_eL),_eN=_eM;return E(_eN)==0?[2]:E(_eF);}}]):[2];}],[0,function(_eO){var _eP=E(_eO);return E(_eP[1])==92?E(new T(function(){return B(_dT(function(_eQ){return new F(function(){return A(_eD,[[0,_eQ,_j]]);});}));})):B(A(_eD,[[0,_eP,_n]]));}]);});},_eR=function(_eS,_eT){return new F(function(){return _eC(function(_eU){var _eV=E(_eU),_eW=E(_eV[1]);if(E(_eW[1])==34){if(!E(_eV[2])){return E(new T(function(){return B(A(_eT,[[1,new T(function(){return B(A(_eS,[_q]));})]]));}));}else{return new F(function(){return _eR(function(_eX){return new F(function(){return A(_eS,[[1,_eW,_eX]]);});},_eT);});}}else{return new F(function(){return _eR(function(_eY){return new F(function(){return A(_eS,[[1,_eW,_eY]]);});},_eT);});}});});},_eZ=new T(function(){return B(unCStr("_\'"));}),_f0=function(_f1){var _f2=u_iswalnum(_f1),_f3=_f2;return E(_f3)==0?B(_9i(_6J,[0,_f1],_eZ)):true;},_f4=function(_f5){return new F(function(){return _f0(E(_f5)[1]);});},_f6=new T(function(){return B(unCStr(",;()[]{}`"));}),_f7=new T(function(){return B(unCStr(".."));}),_f8=new T(function(){return B(unCStr("::"));}),_f9=new T(function(){return B(unCStr("->"));}),_fa=[0,64],_fb=[1,_fa,_q],_fc=[0,126],_fd=[1,_fc,_q],_fe=new T(function(){return B(unCStr("=>"));}),_ff=[1,_fe,_q],_fg=[1,_fd,_ff],_fh=[1,_fb,_fg],_fi=[1,_f9,_fh],_fj=new T(function(){return B(unCStr("<-"));}),_fk=[1,_fj,_fi],_fl=[0,124],_fm=[1,_fl,_q],_fn=[1,_fm,_fk],_fo=[1,_9J,_q],_fp=[1,_fo,_fn],_fq=[0,61],_fr=[1,_fq,_q],_fs=[1,_fr,_fp],_ft=[1,_f8,_fs],_fu=[1,_f7,_ft],_fv=function(_fw){return new F(function(){return _61([1,function(_fx){return E(_fx)[0]==0?E(new T(function(){return B(A(_fw,[_7K]));})):[2];}],new T(function(){return B(_61([0,function(_fy){return E(E(_fy)[1])==39?E([0,function(_fz){var _fA=E(_fz);switch(E(_fA[1])){case 39:return [2];case 92:return E(new T(function(){return B(_dT(function(_fB){return [0,function(_fC){return E(E(_fC)[1])==39?E(new T(function(){return B(A(_fw,[[0,_fB]]));})):[2];}];}));}));default:return [0,function(_fD){return E(E(_fD)[1])==39?E(new T(function(){return B(A(_fw,[[0,_fA]]));})):[2];}];}}]):[2];}],new T(function(){return B(_61([0,function(_fE){return E(E(_fE)[1])==34?E(new T(function(){return B(_eR(_7L,_fw));})):[2];}],new T(function(){return B(_61([0,function(_fF){return !B(_9i(_6J,_fF,_f6))?[2]:B(A(_fw,[[2,[1,_fF,_q]]]));}],new T(function(){return B(_61([0,function(_fG){return !B(_9i(_6J,_fG,_9n))?[2]:[1,B(_7z(_9o,function(_fH){var _fI=[1,_fG,_fH];return !B(_9i(_6S,_fI,_fu))?B(A(_fw,[[4,_fI]])):B(A(_fw,[[2,_fI]]));}))];}],new T(function(){return B(_61([0,function(_fJ){var _fK=E(_fJ),_fL=_fK[1],_fM=u_iswalpha(_fL),_fN=_fM;return E(_fN)==0?E(_fL)==95?[1,B(_7z(_f4,function(_fO){return new F(function(){return A(_fw,[[3,[1,_fK,_fO]]]);});}))]:[2]:[1,B(_7z(_f4,function(_fP){return new F(function(){return A(_fw,[[3,[1,_fK,_fP]]]);});}))];}],new T(function(){return [1,B(_7d(_9A,_9g,_fw))];})));})));})));})));})));}));});},_fQ=[0,0],_fR=function(_fS,_fT){return function(_fU){return new F(function(){return A(_ef,[_fU,function(_fV){return E(new T(function(){return B(_fv(function(_fW){var _fX=E(_fW);return _fX[0]==2?!B(_6y(_fX[1],_6x))?[2]:E(new T(function(){return B(A(_fS,[_fQ,function(_fY){return [1,function(_fZ){return new F(function(){return A(_ef,[_fZ,function(_g0){return E(new T(function(){return B(_fv(function(_g1){var _g2=E(_g1);return _g2[0]==2?!B(_6y(_g2[1],_6v))?[2]:E(new T(function(){return B(A(_fT,[_fY]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_g3=function(_g4,_g5,_g6){var _g7=function(_g8,_g9){return new F(function(){return _61([1,function(_ga){return new F(function(){return A(_ef,[_ga,function(_gb){return E(new T(function(){return B(_fv(function(_gc){var _gd=E(_gc);if(_gd[0]==4){var _ge=E(_gd[1]);if(!_ge[0]){return new F(function(){return A(_g4,[_gd,_g8,_g9]);});}else{return E(E(_ge[1])[1])==45?E(_ge[2])[0]==0?E([1,function(_gf){return new F(function(){return A(_ef,[_gf,function(_gg){return E(new T(function(){return B(_fv(function(_gh){return new F(function(){return A(_g4,[_gh,_g8,function(_gi){return new F(function(){return A(_g9,[new T(function(){return B(_8v(_gi));})]);});}]);});}));}));}]);});}]):B(A(_g4,[_gd,_g8,_g9])):B(A(_g4,[_gd,_g8,_g9]));}}else{return new F(function(){return A(_g4,[_gd,_g8,_g9]);});}}));}));}]);});}],new T(function(){return [1,B(_fR(_g7,_g9))];}));});};return new F(function(){return _g7(_g5,_g6);});},_gj=function(_gk,_gl){return [2];},_gm=function(_gn){var _go=E(_gn);return _go[0]==0?[1,new T(function(){return B(_8L(new T(function(){return B(_8B(E(_go[1])[1]));}),_8A,_go[2]));})]:E(_go[2])[0]==0?E(_go[3])[0]==0?[1,new T(function(){return B(_8L(_8z,_8A,_go[1]));})]:[0]:[0];},_gp=function(_gq){var _gr=E(_gq);if(_gr[0]==5){var _gs=B(_gm(_gr[1]));return _gs[0]==0?E(_gj):function(_gt,_gu){return new F(function(){return A(_gu,[_gs[1]]);});};}else{return E(_gj);}},_gv=function(_gw){return [1,function(_gx){return new F(function(){return A(_ef,[_gx,function(_gy){return E([3,_gw,_74]);}]);});}];},_gz=new T(function(){return B(_g3(_gp,_fQ,_gv));}),_gA=new T(function(){return [0,"maxLoves"];}),_gB=new T(function(){return [0,"items"];}),_gC=new T(function(){return [0,"achievements"];}),_gD=new T(function(){return [0,"lastFocus"];}),_gE=new T(function(){return [0,"depend"];}),_gF=new T(function(){return [0,"lps"];}),_gG=new T(function(){return [0,"loves"];}),_gH=[0,_2D],_gI=[0,_2B],_gJ=new T(function(){return B(unCStr("Tried to deserialize a non-Number to a Double"));}),_gK=[0,_gJ],_gL=[0,_1G],_gM=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_gN=new T(function(){return B(err(_gM));}),_gO=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_gP=new T(function(){return B(err(_gO));}),_gQ=function(_gR){while(1){var _gS=(function(_gT){var _gU=E(_gT);if(!_gU[0]){return [0];}else{var _gV=_gU[2],_gW=E(_gU[1]);if(!E(_gW[2])[0]){return [1,_gW[1],new T(function(){return B(_gQ(_gV));})];}else{_gR=_gV;return null;}}})(_gR);if(_gS!=null){return _gS;}}},_gX=function(_gY){var _gZ=E(_gY);if(_gZ[0]==4){var _h0=_gZ[1],_h1=B(_2H(_2w,_gG,_h0));if(!_h1[0]){return E(_gI);}else{var _h2=E(_h1[1]);if(!_h2[0]){var _h3=B(_2H(_2w,_gF,_h0));if(!_h3[0]){return E(_gI);}else{var _h4=E(_h3[1]);if(!_h4[0]){var _h5=B(_2H(_2w,_gE,_h0));if(!_h5[0]){return E(_gI);}else{var _h6=E(_h5[1]);if(!_h6[0]){var _h7=B(_2H(_2w,_gD,_h0));if(!_h7[0]){return E(_gI);}else{var _h8=E(_h7[1]);if(_h8[0]==1){var _h9=B(_2H(_2w,_gC,_h0));if(!_h9[0]){return E(_gI);}else{var _ha=B(_3p(_3Y,_3b,_h9[1]));if(!_ha[0]){return [0,_ha[1]];}else{var _hb=B(_2H(_2w,_gB,_h0));if(!_hb[0]){return E(_gI);}else{var _hc=B(_3p(_3Y,_3Y,_hb[1]));if(!_hc[0]){return [0,_hc[1]];}else{var _hd=B(_2H(_2w,_gA,_h0));if(!_hd[0]){return E(_gI);}else{var _he=E(_hd[1]);return _he[0]==0?[1,[0,_h2[1],_h4[1],_h6[1],new T(function(){var _hf=B(_gQ(B(_5R(_gz,new T(function(){return fromJSStr(E(_h8[1])[1]);})))));return _hf[0]==0?E(_gN):E(_hf[2])[0]==0?E(_hf[1]):E(_gP);}),_n,new T(function(){return B(_4x(_ha[1]));}),new T(function(){return B(_4x(_hc[1]));}),_he[1]]]:E(_gK);}}}}}}else{return E(_gL);}}}else{return E(_gK);}}}else{return E(_gK);}}}else{return E(_gK);}}}else{return E(_gH);}},_hg=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_hh=[0,_hg],_hi=[1,_q],_hj=function(_hk){var _hl=E(_hk);if(!_hl[0]){return E(_hi);}else{var _hm=B(_gX(_hl[1]));if(!_hm[0]){return [0,_hm[1]];}else{var _hn=B(_hj(_hl[2]));return _hn[0]==0?[0,_hn[1]]:[1,[1,_hm[1],_hn[1]]];}}},_ho=function(_hp){var _hq=E(_hp);return _hq[0]==3?B(_hj(_hq[1])):E(_hh);},_hr=[0,_1p,_1w,_gX,_ho],_hs=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_ht=new T(function(){return B(err(_hs));}),_hu=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_hv=new T(function(){return B(err(_hu));}),_hw=function(_hx,_hy){while(1){var _hz=E(_hx);if(!_hz[0]){return E(_hv);}else{var _hA=E(_hy);if(!_hA){return E(_hz[1]);}else{_hx=_hz[2];_hy=_hA-1|0;continue;}}}},_hB=new T(function(){return B(unCStr("ACK"));}),_hC=new T(function(){return B(unCStr("BEL"));}),_hD=new T(function(){return B(unCStr("BS"));}),_hE=new T(function(){return B(unCStr("SP"));}),_hF=[1,_hE,_q],_hG=new T(function(){return B(unCStr("US"));}),_hH=[1,_hG,_hF],_hI=new T(function(){return B(unCStr("RS"));}),_hJ=[1,_hI,_hH],_hK=new T(function(){return B(unCStr("GS"));}),_hL=[1,_hK,_hJ],_hM=new T(function(){return B(unCStr("FS"));}),_hN=[1,_hM,_hL],_hO=new T(function(){return B(unCStr("ESC"));}),_hP=[1,_hO,_hN],_hQ=new T(function(){return B(unCStr("SUB"));}),_hR=[1,_hQ,_hP],_hS=new T(function(){return B(unCStr("EM"));}),_hT=[1,_hS,_hR],_hU=new T(function(){return B(unCStr("CAN"));}),_hV=[1,_hU,_hT],_hW=new T(function(){return B(unCStr("ETB"));}),_hX=[1,_hW,_hV],_hY=new T(function(){return B(unCStr("SYN"));}),_hZ=[1,_hY,_hX],_i0=new T(function(){return B(unCStr("NAK"));}),_i1=[1,_i0,_hZ],_i2=new T(function(){return B(unCStr("DC4"));}),_i3=[1,_i2,_i1],_i4=new T(function(){return B(unCStr("DC3"));}),_i5=[1,_i4,_i3],_i6=new T(function(){return B(unCStr("DC2"));}),_i7=[1,_i6,_i5],_i8=new T(function(){return B(unCStr("DC1"));}),_i9=[1,_i8,_i7],_ia=new T(function(){return B(unCStr("DLE"));}),_ib=[1,_ia,_i9],_ic=new T(function(){return B(unCStr("SI"));}),_id=[1,_ic,_ib],_ie=new T(function(){return B(unCStr("SO"));}),_if=[1,_ie,_id],_ig=new T(function(){return B(unCStr("CR"));}),_ih=[1,_ig,_if],_ii=new T(function(){return B(unCStr("FF"));}),_ij=[1,_ii,_ih],_ik=new T(function(){return B(unCStr("VT"));}),_il=[1,_ik,_ij],_im=new T(function(){return B(unCStr("LF"));}),_in=[1,_im,_il],_io=new T(function(){return B(unCStr("HT"));}),_ip=[1,_io,_in],_iq=[1,_hD,_ip],_ir=[1,_hC,_iq],_is=[1,_hB,_ir],_it=new T(function(){return B(unCStr("ENQ"));}),_iu=[1,_it,_is],_iv=new T(function(){return B(unCStr("EOT"));}),_iw=[1,_iv,_iu],_ix=new T(function(){return B(unCStr("ETX"));}),_iy=[1,_ix,_iw],_iz=new T(function(){return B(unCStr("STX"));}),_iA=[1,_iz,_iy],_iB=new T(function(){return B(unCStr("SOH"));}),_iC=[1,_iB,_iA],_iD=new T(function(){return B(unCStr("NUL"));}),_iE=[1,_iD,_iC],_iF=[0,92],_iG=new T(function(){return B(unCStr("\\DEL"));}),_iH=new T(function(){return B(unCStr("\\a"));}),_iI=new T(function(){return B(unCStr("\\\\"));}),_iJ=new T(function(){return B(unCStr("\\SO"));}),_iK=new T(function(){return B(unCStr("\\r"));}),_iL=new T(function(){return B(unCStr("\\f"));}),_iM=new T(function(){return B(unCStr("\\v"));}),_iN=new T(function(){return B(unCStr("\\n"));}),_iO=new T(function(){return B(unCStr("\\t"));}),_iP=new T(function(){return B(unCStr("\\b"));}),_iQ=function(_iR,_iS){if(_iR<=127){var _iT=E(_iR);switch(_iT){case 92:return new F(function(){return _K(_iI,_iS);});break;case 127:return new F(function(){return _K(_iG,_iS);});break;default:if(_iT<32){var _iU=E(_iT);switch(_iU){case 7:return new F(function(){return _K(_iH,_iS);});break;case 8:return new F(function(){return _K(_iP,_iS);});break;case 9:return new F(function(){return _K(_iO,_iS);});break;case 10:return new F(function(){return _K(_iN,_iS);});break;case 11:return new F(function(){return _K(_iM,_iS);});break;case 12:return new F(function(){return _K(_iL,_iS);});break;case 13:return new F(function(){return _K(_iK,_iS);});break;case 14:return new F(function(){return _K(_iJ,new T(function(){var _iV=E(_iS);if(!_iV[0]){var _iW=[0];}else{var _iW=E(E(_iV[1])[1])==72?B(unAppCStr("\\&",_iV)):E(_iV);}return _iW;}));});break;default:return new F(function(){return _K([1,_iF,new T(function(){var _iX=_iU;return _iX>=0?B(_hw(_iE,_iX)):E(_ht);})],_iS);});}}else{return [1,[0,_iT],_iS];}}}else{return [1,_iF,new T(function(){var _iY=jsShowI(_iR),_iZ=_iY;return B(_K(fromJSStr(_iZ),new T(function(){var _j0=E(_iS);if(!_j0[0]){var _j1=[0];}else{var _j2=E(_j0[1])[1];if(_j2<48){var _j3=E(_j0);}else{var _j3=_j2>57?E(_j0):B(unAppCStr("\\&",_j0));}var _j4=_j3,_j5=_j4,_j1=_j5;}return _j1;})));})];}},_j6=[0,39],_j7=[1,_j6,_q],_j8=new T(function(){return B(unCStr("\'\\\'\'"));}),_j9=function(_ja){var _jb=E(E(_ja)[1]);return _jb==39?E(_j8):[1,_j6,new T(function(){return B(_iQ(_jb,_j7));})];},_jc=[0,34],_jd=new T(function(){return B(unCStr("\\\""));}),_je=function(_jf,_jg){var _jh=E(_jf);if(!_jh[0]){return E(_jg);}else{var _ji=_jh[2],_jj=E(E(_jh[1])[1]);if(_jj==34){return new F(function(){return _K(_jd,new T(function(){return B(_je(_ji,_jg));}));});}else{return new F(function(){return _iQ(_jj,new T(function(){return B(_je(_ji,_jg));}));});}}},_jk=function(_jl,_jm){return [1,_jc,new T(function(){return B(_je(_jl,[1,_jc,_jm]));})];},_jn=function(_jo){return new F(function(){return _K(_j8,_jo);});},_jp=function(_jq,_jr){var _js=E(E(_jr)[1]);return _js==39?E(_jn):function(_jt){return [1,_j6,new T(function(){return B(_iQ(_js,[1,_j6,_jt]));})];};},_ju=[0,_jp,_j9,_jk],_jv=function(_jw){return E(E(_jw)[3]);},_jx=function(_jy,_jz){return new F(function(){return A(_jv,[_jy,_jz,_q]);});},_jA=function(_jB,_jC,_jD){return new F(function(){return _56(new T(function(){return B(_jv(_jB));}),_jC,_jD);});},_jE=function(_jF){return [0,function(_jG){return E(new T(function(){return B(_jv(_jF));}));},function(_jo){return new F(function(){return _jx(_jF,_jo);});},function(_jH,_jo){return new F(function(){return _jA(_jF,_jH,_jo);});}];},_jI=new T(function(){return B(_jE(_ju));}),_jJ=new T(function(){return B(unCStr("Just "));}),_jK=new T(function(){return B(unCStr("Nothing"));}),_jL=[0,11],_jM=function(_jN){return E(E(_jN)[1]);},_jO=function(_jP,_jQ,_jR,_jS){var _jT=E(_jR);if(!_jT[0]){return new F(function(){return _K(_jK,_jS);});}else{var _jU=_jT[1];return E(_jQ)[1]<=10?B(_K(_jJ,new T(function(){return B(A(_jM,[_jP,_jL,_jU,_jS]));}))):[1,_13,new T(function(){return B(_K(_jJ,new T(function(){return B(A(_jM,[_jP,_jL,_jU,[1,_12,_jS]]));})));})];}},_jV=[0,0],_jW=function(_jX,_jY){return new F(function(){return _jO(_jX,_jV,_jY,_q);});},_jZ=function(_k0,_k1,_k2){return new F(function(){return _56(function(_jH,_jo){return new F(function(){return _jO(_k0,_jV,_jH,_jo);});},_k1,_k2);});},_k3=function(_k4){return [0,function(_k5,_jH,_jo){return new F(function(){return _jO(_k4,_k5,_jH,_jo);});},function(_jo){return new F(function(){return _jW(_k4,_jo);});},function(_jH,_jo){return new F(function(){return _jZ(_k4,_jH,_jo);});}];},_k6=new T(function(){return B(_k3(_jI));}),_k7=function(_k8){var _k9=jsShow(E(_k8)[1]),_ka=_k9;return new F(function(){return fromJSStr(_ka);});},_kb=function(_kc){return function(_7w){return new F(function(){return _K(new T(function(){return B(_k7(_kc));}),_7w);});};},_kd=function(_ke){return new F(function(){return _9R(0,E(_ke)[1],_q);});},_kf=function(_kg,_kh){return new F(function(){return _9R(0,E(_kg)[1],_kh);});},_ki=function(_kj,_kk){return new F(function(){return _56(_kf,_kj,_kk);});},_kl=function(_km,_kn,_ko){return new F(function(){return _9R(E(_km)[1],E(_kn)[1],_ko);});},_kp=[0,_kl,_kd,_ki],_kq=function(_kr,_ks,_kt){return new F(function(){return A(_kr,[[1,_53,new T(function(){return B(A(_ks,[_kt]));})]]);});},_ku=new T(function(){return B(unCStr(": empty list"));}),_kv=new T(function(){return B(unCStr("Prelude."));}),_kw=function(_kx){return new F(function(){return err(B(_K(_kv,new T(function(){return B(_K(_kx,_ku));}))));});},_ky=new T(function(){return B(unCStr("foldr1"));}),_kz=new T(function(){return B(_kw(_ky));}),_kA=function(_kB,_kC){var _kD=E(_kC);if(!_kD[0]){return E(_kz);}else{var _kE=_kD[1],_kF=E(_kD[2]);if(!_kF[0]){return E(_kE);}else{return new F(function(){return A(_kB,[_kE,new T(function(){return B(_kA(_kB,_kF));})]);});}}},_kG=function(_kH,_kI,_kJ,_kK){return new F(function(){return _56(function(_kL,_kM){var _kN=E(_kL);return [1,_13,new T(function(){return B(A(_kA,[_kq,[1,new T(function(){return B(A(new T(function(){return B(_jM(_kH));}),[_jV,_kN[1]]));}),[1,new T(function(){return B(A(new T(function(){return B(_jM(_kI));}),[_jV,_kN[2]]));}),_q]],[1,_12,_kM]]));})];},_kJ,_kK);});},_kO=new T(function(){return B(unCStr("fromList "));}),_kP=function(_kQ,_kR){while(1){var _kS=(function(_kT,_kU){var _kV=E(_kU);switch(_kV[0]){case 0:_kQ=new T(function(){return B(_kP(_kT,_kV[4]));});_kR=_kV[3];return null;case 1:return [1,[0,[0,_kV[1]],_kV[2]],_kT];default:return E(_kT);}})(_kQ,_kR);if(_kS!=null){return _kS;}}},_kW=function(_kX){var _kY=E(_kX);if(!_kY[0]){var _kZ=_kY[3],_l0=_kY[4];return _kY[2]>=0?B(_kP(new T(function(){return B(_kP(_q,_l0));}),_kZ)):B(_kP(new T(function(){return B(_kP(_q,_kZ));}),_l0));}else{return new F(function(){return _kP(_q,_kY);});}},_l1=function(_l2,_l3,_l4){var _l5=new T(function(){return B(_kW(_l4));});return _l3<=10?function(_l6){return new F(function(){return _K(_kO,new T(function(){return B(_kG(_kp,_l2,_l5,_l6));}));});}:function(_l7){return [1,_13,new T(function(){return B(_K(_kO,new T(function(){return B(_kG(_kp,_l2,_l5,[1,_12,_l7]));})));})];};},_l8=[0,45],_l9=function(_la,_lb,_lc){var _ld=function(_le){var _lf=new T(function(){return B(A(_la,[[0, -_lc]]));});return E(_lb)[1]<=6?function(_lg){return [1,_l8,new T(function(){return B(A(_lf,[_lg]));})];}:function(_lh){return [1,_13,[1,_l8,new T(function(){return B(A(_lf,[[1,_12,_lh]]));})]];};};if(_lc>=0){var _li=isDoubleNegativeZero(_lc),_lj=_li;return E(_lj)==0?B(A(_la,[[0,_lc]])):B(_ld(_));}else{return new F(function(){return _ld(_);});}},_lk=[0,0],_ll=[0,125],_lm=new T(function(){return B(unCStr("_maxLoves = "));}),_ln=new T(function(){return B(unCStr(", "));}),_lo=new T(function(){return B(unCStr("_items = "));}),_lp=new T(function(){return B(unCStr("_achieves = "));}),_lq=new T(function(){return B(unCStr("_hasFocus = "));}),_lr=new T(function(){return B(unCStr("_lastFocus = "));}),_ls=new T(function(){return B(unCStr("_depend = "));}),_lt=new T(function(){return B(unCStr("_lps = "));}),_lu=new T(function(){return B(unCStr("_loves = "));}),_lv=new T(function(){return B(unCStr("Aichan {"));}),_lw=new T(function(){return B(unCStr("True"));}),_lx=new T(function(){return B(unCStr("False"));}),_ly=function(_lz,_lA,_lB,_lC,_lD,_lE,_lF,_lG,_lH){var _lI=function(_lJ){return new F(function(){return _K(_lv,new T(function(){return B(_K(_lu,new T(function(){return B(A(new T(function(){return B(_l9(_kb,_lk,E(_lA)[1]));}),[new T(function(){return B(_K(_ln,new T(function(){return B(_K(_lt,new T(function(){return B(A(new T(function(){return B(_l9(_kb,_lk,E(_lB)[1]));}),[new T(function(){return B(_K(_ln,new T(function(){return B(_K(_ls,new T(function(){return B(A(new T(function(){return B(_l9(_kb,_lk,E(_lC)[1]));}),[new T(function(){return B(_K(_ln,new T(function(){return B(_K(_lr,new T(function(){return B(_15(0,_lD,new T(function(){return B(_K(_ln,new T(function(){return B(_K(_lq,new T(function(){var _lK=new T(function(){return B(_K(_ln,new T(function(){return B(_K(_lp,new T(function(){return B(A(new T(function(){return B(_l1(_k6,0,_lF));}),[new T(function(){return B(_K(_ln,new T(function(){return B(_K(_lo,new T(function(){return B(A(new T(function(){return B(_l1(_kp,0,_lG));}),[new T(function(){return B(_K(_ln,new T(function(){return B(_K(_lm,new T(function(){return B(A(new T(function(){return B(_l9(_kb,_lk,E(_lH)[1]));}),[[1,_ll,_lJ]]));})));})));})]));})));})));})]));})));})));});return !E(_lE)?B(_K(_lx,_lK)):B(_K(_lw,_lK));})));})));})));})));})));})]));})));})));})]));})));})));})]));})));}));});};return _lz<11?E(_lI):function(_lL){return [1,_13,new T(function(){return B(_lI([1,_12,_lL]));})];};},_lM=function(_lN){var _lO=E(_lN);return new F(function(){return A(_ly,[0,_lO[1],_lO[2],_lO[3],_lO[4],_lO[5],_lO[6],_lO[7],_lO[8],_q]);});},_lP=function(_lQ,_lR,_lS,_){var _lT=rMV(_lR),_lU=_lT,_lV=B(A(_lS,[_lU,_])),_lW=_lV,_=wMV(_lR,new T(function(){return E(E(_lW)[2]);})),_lX=jsSetTimeout(_lQ,function(_){var _lY=B(_lP(_lQ,_lR,_lS,_)),_lZ=_lY;return _77;});return new F(function(){return rMV(_lR);});},_m0=new T(function(){return B(unCStr(" is not an element of the map"));}),_m1=function(_m2){return new F(function(){return err(B(unAppCStr("IntMap.!: key ",new T(function(){return B(_K(B(_9R(0,_m2,_q)),_m0));}))));});},_m3=function(_m4,_m5){var _m6=new T(function(){return B(_m1(_m5));});return new F(function(){return (function(_m7){while(1){var _m8=E(_m7);switch(_m8[0]){case 0:var _m9=_m8[2]>>>0;if(((_m5>>>0&((_m9-1>>>0^4294967295)>>>0^_m9)>>>0)>>>0&4294967295)==_m8[1]){if(!((_m5>>>0&_m9)>>>0)){_m7=_m8[3];continue;}else{_m7=_m8[4];continue;}}else{return E(_m6);}break;case 1:return _m5!=_m8[1]?E(_m6):E(_m8[2]);default:return E(_m6);}}})(_m4);});},_ma=function(_mb,_mc){return new F(function(){return (function(_md){while(1){var _me=E(_md);switch(_me[0]){case 0:var _mf=_me[2]>>>0;if(((_mb>>>0&((_mf-1>>>0^4294967295)>>>0^_mf)>>>0)>>>0&4294967295)==_me[1]){if(!((_mb>>>0&_mf)>>>0)){_md=_me[3];continue;}else{_md=_me[4];continue;}}else{return false;}break;case 1:return _mb==_me[1];default:return false;}}})(_mc);});},_mg=function(_){var _mh=jsEval("Date.now()"),_mi=_mh;return new T(function(){var _mj=B(_gQ(B(_5R(_gz,new T(function(){return fromJSStr(_mi);})))));return _mj[0]==0?B(err(_gM)):E(_mj[2])[0]==0?E(_mj[1]):B(err(_gO));});},_mk=function(_ml,_mm,_mn,_mo){var _mp=E(_mo);switch(_mp[0]){case 0:var _mq=_mp[1],_mr=_mp[2],_ms=_mp[3],_mt=_mp[4],_mu=_mr>>>0;if(((_mm>>>0&((_mu-1>>>0^4294967295)>>>0^_mu)>>>0)>>>0&4294967295)==_mq){return (_mm>>>0&_mu)>>>0==0?[0,_mq,_mr,E(B(_mk(_ml,_mm,_mn,_ms))),E(_mt)]:[0,_mq,_mr,E(_ms),E(B(_mk(_ml,_mm,_mn,_mt)))];}else{var _mv=(_mm>>>0^_mq>>>0)>>>0,_mw=(_mv|_mv>>>1)>>>0,_mx=(_mw|_mw>>>2)>>>0,_my=(_mx|_mx>>>4)>>>0,_mz=(_my|_my>>>8)>>>0,_mA=(_mz|_mz>>>16)>>>0,_mB=(_mA^_mA>>>1)>>>0&4294967295,_mC=_mB>>>0;return (_mm>>>0&_mC)>>>0==0?[0,(_mm>>>0&((_mC-1>>>0^4294967295)>>>0^_mC)>>>0)>>>0&4294967295,_mB,E([1,_mm,_mn]),E(_mp)]:[0,(_mm>>>0&((_mC-1>>>0^4294967295)>>>0^_mC)>>>0)>>>0&4294967295,_mB,E(_mp),E([1,_mm,_mn])];}break;case 1:var _mD=_mp[1];if(_mm!=_mD){var _mE=(_mm>>>0^_mD>>>0)>>>0,_mF=(_mE|_mE>>>1)>>>0,_mG=(_mF|_mF>>>2)>>>0,_mH=(_mG|_mG>>>4)>>>0,_mI=(_mH|_mH>>>8)>>>0,_mJ=(_mI|_mI>>>16)>>>0,_mK=(_mJ^_mJ>>>1)>>>0&4294967295,_mL=_mK>>>0;return (_mm>>>0&_mL)>>>0==0?[0,(_mm>>>0&((_mL-1>>>0^4294967295)>>>0^_mL)>>>0)>>>0&4294967295,_mK,E([1,_mm,_mn]),E(_mp)]:[0,(_mm>>>0&((_mL-1>>>0^4294967295)>>>0^_mL)>>>0)>>>0&4294967295,_mK,E(_mp),E([1,_mm,_mn])];}else{return [1,_mm,new T(function(){return B(A(_ml,[[0,_mm],_mn,_mp[2]]));})];}break;default:return [1,_mm,_mn];}},_mM=[0,1],_mN=function(_mO){return E(_mO);},_mP=function(_mQ,_mR){return new F(function(){return (function(_mS){while(1){var _mT=E(_mS);switch(_mT[0]){case 0:var _mU=_mT[2]>>>0;if(((_mQ>>>0&((_mU-1>>>0^4294967295)>>>0^_mU)>>>0)>>>0&4294967295)==_mT[1]){if(!((_mQ>>>0&_mU)>>>0)){_mS=_mT[3];continue;}else{_mS=_mT[4];continue;}}else{return [0];}break;case 1:return _mQ!=_mT[1]?[0]:[1,_mT[2]];default:return [0];}}})(_mR);});},_mV=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:269:3-8"));}),_mW=function(_mX,_mY){if(_mX<=_mY){var _mZ=function(_n0){return [1,[0,_n0],new T(function(){if(_n0!=_mY){var _n1=B(_mZ(_n0+1|0));}else{var _n1=[0];}var _n2=_n1;return _n2;})];};return new F(function(){return _mZ(_mX);});}else{return [0];}},_n3=[0,0],_n4=new T(function(){return B(unCStr("innerHTML"));}),_n5=new T(function(){return B(unCStr("<div id=\"alert-%d\" class=\"alert alert-info fade in\" role=\"alert\">  <button type=\"button\" class=\"close\" data-dismiss=\"alert\"><span aria-hidden=\"true\">&times;</span><span class=\"sr-only\">Close</span></button>%s </div>"));}),_n6=new T(function(){return B(unCStr("alerts"));}),_n7=new T(function(){return B(unCStr("\').alert(\'close\')"));}),_n8=function(_n9,_na){while(1){var _nb=E(_n9);if(!_nb[0]){return E(_na);}else{_n9=_nb[2];var _nc=[1,_nb[1],_na];_na=_nc;continue;}}},_nd=function(_ne){var _nf=E(_ne)[1];return [0,Math.log(_nf+(_nf+1)*Math.sqrt((_nf-1)/(_nf+1)))];},_ng=function(_nh){var _ni=E(_nh)[1];return [0,Math.log(_ni+Math.sqrt(1+_ni*_ni))];},_nj=function(_nk){var _nl=E(_nk)[1];return [0,0.5*Math.log((1+_nl)/(1-_nl))];},_nm=function(_nn,_no){return [0,Math.log(E(_no)[1])/Math.log(E(_nn)[1])];},_np=[0,3.141592653589793],_nq=new T(function(){return [0,0/0];}),_nr=new T(function(){return [0,-1/0];}),_ns=new T(function(){return [0,1/0];}),_nt=[0,0],_nu=function(_nv,_nw){while(1){var _nx=E(_nv);if(!_nx[0]){_nv=[1,I_fromInt(_nx[1])];continue;}else{var _ny=E(_nw);if(!_ny[0]){_nv=_nx;_nw=[1,I_fromInt(_ny[1])];continue;}else{return new F(function(){return I_fromRat(_nx[1],_ny[1]);});}}}},_nz=function(_nA,_nB){var _nC=E(_nA);if(!_nC[0]){var _nD=_nC[1],_nE=E(_nB);return _nE[0]==0?_nD==_nE[1]:I_compareInt(_nE[1],_nD)==0?true:false;}else{var _nF=_nC[1],_nG=E(_nB);return _nG[0]==0?I_compareInt(_nF,_nG[1])==0?true:false:I_compare(_nF,_nG[1])==0?true:false;}},_nH=function(_nI,_nJ){return !B(_nz(_nJ,_nt))?[0,B(_nu(_nI,_nJ))]:!B(_nz(_nI,_nt))?!B(_U(_nI,_nt))?E(_ns):E(_nr):E(_nq);},_nK=function(_nL){var _nM=E(_nL);return new F(function(){return _nH(_nM[1],_nM[2]);});},_nN=function(_nO){return [0,1/E(_nO)[1]];},_nP=function(_nQ){var _nR=E(_nQ),_nS=_nR[1];return _nS<0?[0, -_nS]:E(_nR);},_nT=function(_nU){var _nV=E(_nU);return _nV[0]==0?_nV[1]:I_toNumber(_nV[1]);},_nW=function(_nX){return [0,B(_nT(_nX))];},_nY=[0,0],_nZ=[0,1],_o0=[0,-1],_o1=function(_o2){var _o3=E(E(_o2)[1]);return _o3==0?E(_nY):_o3<=0?E(_o0):E(_nZ);},_o4=function(_o5,_o6){return [0,E(_o5)[1]-E(_o6)[1]];},_o7=function(_o8){return [0, -E(_o8)[1]];},_o9=function(_oa,_ob){return [0,E(_oa)[1]+E(_ob)[1]];},_oc=function(_od,_oe){return [0,E(_od)[1]*E(_oe)[1]];},_of=[0,_o9,_oc,_o4,_o7,_nP,_o1,_nW],_og=function(_oh,_oi){return [0,E(_oh)[1]/E(_oi)[1]];},_oj=[0,_of,_og,_nN,_nK],_ok=function(_ol){return [0,Math.acos(E(_ol)[1])];},_om=function(_on){return [0,Math.asin(E(_on)[1])];},_oo=function(_op){return [0,Math.atan(E(_op)[1])];},_oq=function(_or){return [0,Math.cos(E(_or)[1])];},_os=function(_ot){return [0,cosh(E(_ot)[1])];},_ou=function(_ov){return [0,Math.exp(E(_ov)[1])];},_ow=function(_ox){return [0,Math.log(E(_ox)[1])];},_oy=function(_oz,_oA){return [0,Math.pow(E(_oz)[1],E(_oA)[1])];},_oB=function(_oC){return [0,Math.sin(E(_oC)[1])];},_oD=function(_oE){return [0,sinh(E(_oE)[1])];},_oF=function(_oG){return [0,Math.sqrt(E(_oG)[1])];},_oH=function(_oI){return [0,Math.tan(E(_oI)[1])];},_oJ=function(_oK){return [0,tanh(E(_oK)[1])];},_oL=[0,_oj,_np,_ou,_oF,_ow,_oy,_nm,_oB,_oH,_oq,_om,_oo,_ok,_oD,_oJ,_os,_ng,_nj,_nd],_oM=function(_oN){var _oO=E(_oN)[1];return [0,Math.log(_oO+(_oO+1)*Math.sqrt((_oO-1)/(_oO+1)))];},_oP=function(_oQ){var _oR=E(_oQ)[1];return [0,Math.log(_oR+Math.sqrt(1+_oR*_oR))];},_oS=function(_oT){var _oU=E(_oT)[1];return [0,0.5*Math.log((1+_oU)/(1-_oU))];},_oV=function(_oW,_oX){return [0,Math.log(E(_oX)[1])/Math.log(E(_oW)[1])];},_oY=[0,3.141592653589793],_oZ=new T(function(){return [0,0/0];}),_p0=new T(function(){return [0,-1/0];}),_p1=new T(function(){return [0,1/0];}),_p2=function(_p3,_p4){return !B(_nz(_p4,_nt))?[0,B(_nu(_p3,_p4))]:!B(_nz(_p3,_nt))?!B(_U(_p3,_nt))?E(_p1):E(_p0):E(_oZ);},_p5=function(_p6){var _p7=E(_p6);return new F(function(){return _p2(_p7[1],_p7[2]);});},_p8=function(_p9){return [0,1/E(_p9)[1]];},_pa=function(_pb){var _pc=E(_pb),_pd=_pc[1];return _pd<0?[0, -_pd]:E(_pc);},_pe=function(_pf){var _pg=E(_pf);return _pg[0]==0?_pg[1]:I_toNumber(_pg[1]);},_ph=function(_pi){return [0,B(_pe(_pi))];},_pj=[0,0],_pk=[0,1],_pl=[0,-1],_pm=function(_pn){var _po=E(E(_pn)[1]);return _po==0?E(_pj):_po<=0?E(_pl):E(_pk);},_pp=function(_pq,_pr){return [0,E(_pq)[1]-E(_pr)[1]];},_ps=function(_pt){return [0, -E(_pt)[1]];},_pu=function(_pv,_pw){return [0,E(_pv)[1]+E(_pw)[1]];},_px=function(_py,_pz){return [0,E(_py)[1]*E(_pz)[1]];},_pA=[0,_pu,_px,_pp,_ps,_pa,_pm,_ph],_pB=function(_pC,_pD){return [0,E(_pC)[1]/E(_pD)[1]];},_pE=[0,_pA,_pB,_p8,_p5],_pF=function(_pG){return [0,Math.acos(E(_pG)[1])];},_pH=function(_pI){return [0,Math.asin(E(_pI)[1])];},_pJ=function(_pK){return [0,Math.atan(E(_pK)[1])];},_pL=function(_pM){return [0,Math.cos(E(_pM)[1])];},_pN=function(_pO){return [0,cosh(E(_pO)[1])];},_pP=function(_pQ){return [0,Math.exp(E(_pQ)[1])];},_pR=function(_pS){return [0,Math.log(E(_pS)[1])];},_pT=function(_pU,_pV){return [0,Math.pow(E(_pU)[1],E(_pV)[1])];},_pW=function(_pX){return [0,Math.sin(E(_pX)[1])];},_pY=function(_pZ){return [0,sinh(E(_pZ)[1])];},_q0=function(_q1){return [0,Math.sqrt(E(_q1)[1])];},_q2=function(_q3){return [0,Math.tan(E(_q3)[1])];},_q4=function(_q5){return [0,tanh(E(_q5)[1])];},_q6=[0,_pE,_oY,_pP,_q0,_pR,_pT,_oV,_pW,_q2,_pL,_pH,_pJ,_pF,_pY,_q4,_pN,_oP,_oS,_oM],_q7=function(_q8){var _q9=I_decodeDouble(_q8);return [0,[1,_q9[2]],_q9[1]];},_qa=function(_qb){var _qc=B(_q7(E(_qb)[1]));return [0,_qc[1],[0,_qc[2]]];},_qd=[0,53],_qe=function(_qf){return E(_qd);},_qg=[0,2],_qh=function(_qi){return E(_qg);},_qj=[0,1024],_qk=[0,-1021],_ql=[0,_qk,_qj],_qm=function(_qn){return E(_ql);},_qo=function(_qp){var _qq=isDoubleInfinite(E(_qp)[1]),_qr=_qq;return E(_qr)==0?false:true;},_qs=function(_qt){var _qu=isDoubleNaN(E(_qt)[1]),_qv=_qu;return E(_qv)==0?false:true;},_qw=function(_qx){var _qy=isDoubleNegativeZero(E(_qx)[1]),_qz=_qy;return E(_qz)==0?false:true;},_qA=function(_qB){var _qC=decodeFloat(E(_qB)[1]);return [0,new T(function(){return B(_8B(_qC[1]));}),[0,_qC[2]]];},_qD=[0,24],_qE=function(_qF){return E(_qD);},_qG=function(_qH){return E(_qg);},_qI=[0,128],_qJ=[0,-125],_qK=[0,_qJ,_qI],_qL=function(_qM){return E(_qK);},_qN=function(_qO){var _qP=isFloatInfinite(E(_qO)[1]),_qQ=_qP;return E(_qQ)==0?false:true;},_qR=function(_qS){var _qT=isFloatNaN(E(_qS)[1]),_qU=_qT;return E(_qU)==0?false:true;},_qV=function(_qW){var _qX=isFloatNegativeZero(E(_qW)[1]),_qY=_qX;return E(_qY)==0?false:true;},_qZ=function(_r0,_r1){return E(_r0)[1]!=E(_r1)[1]?true:false;},_r2=function(_r3,_r4){return E(_r3)[1]==E(_r4)[1];},_r5=[0,_r2,_qZ],_r6=function(_r7,_r8){return E(_r7)[1]<E(_r8)[1];},_r9=function(_ra,_rb){return E(_ra)[1]<=E(_rb)[1];},_rc=function(_rd,_re){return E(_rd)[1]>E(_re)[1];},_rf=function(_rg,_rh){return E(_rg)[1]>=E(_rh)[1];},_ri=function(_rj,_rk){var _rl=E(_rj)[1],_rm=E(_rk)[1];return _rl>=_rm?_rl!=_rm?2:1:0;},_rn=function(_ro,_rp){var _rq=E(_ro),_rr=E(_rp);return _rq[1]>_rr[1]?E(_rq):E(_rr);},_rs=function(_rt,_ru){var _rv=E(_rt),_rw=E(_ru);return _rv[1]>_rw[1]?E(_rw):E(_rv);},_rx=[0,_r5,_ri,_r6,_rf,_rc,_r9,_rn,_rs],_ry=[0,1],_rz=function(_rA){var _rB=hs_intToInt64(2147483647),_rC=_rB,_rD=hs_leInt64(_rA,_rC),_rE=_rD;if(!E(_rE)){return [1,I_fromInt64(_rA)];}else{var _rF=hs_intToInt64(-2147483648),_rG=_rF,_rH=hs_geInt64(_rA,_rG),_rI=_rH;if(!E(_rI)){return [1,I_fromInt64(_rA)];}else{var _rJ=hs_int64ToInt(_rA),_rK=_rJ;return new F(function(){return _8B(_rK);});}}},_rL=new T(function(){var _rM=newByteArr(256),_rN=_rM,_=_rN["v"]["i8"][0]=8,_=B((function(_rO,_rP,_rQ,_){while(1){if(_rQ>=256){if(_rO>=256){return E(_);}else{var _rR=imul(2,_rO)|0,_rS=_rP+1|0,_rT=_rO;_rO=_rR;_rP=_rS;_rQ=_rT;continue;}}else{var _=_rN["v"]["i8"][_rQ]=_rP,_rT=_rQ+_rO|0;_rQ=_rT;continue;}}})(2,0,1,_)),_rU=_rN,_rV=_rU;return [0,_rV];}),_rW=function(_rX,_rY){while(1){var _rZ=(function(_s0,_s1){var _s2=hs_int64ToInt(_s0),_s3=_s2,_s4=E(_rL)[1]["v"]["i8"][(255&_s3>>>0)>>>0&4294967295];if(_s1>_s4){if(_s4>=8){var _s5=hs_uncheckedIShiftRA64(_s0,8),_s6=_s5;_rX=_s6;var _s7=_s1-8|0;_rY=_s7;return null;}else{return [0,new T(function(){var _s8=hs_uncheckedIShiftRA64(_s0,_s4),_s9=_s8;return B(_rz(_s9));}),_s1-_s4|0];}}else{return [0,new T(function(){var _sa=hs_uncheckedIShiftRA64(_s0,_s1),_sb=_sa;return B(_rz(_sb));}),0];}})(_rX,_rY);if(_rZ!=null){return _rZ;}}},_sc=function(_sd){var _se=hs_intToInt64(_sd),_sf=_se;return E(_sf);},_sg=function(_sh){var _si=E(_sh);return _si[0]==0?B(_sc(_si[1])):I_toInt64(_si[1]);},_sj=function(_sk){return I_toInt(_sk)>>>0;},_sl=function(_sm){var _sn=E(_sm);return _sn[0]==0?_sn[1]>>>0:B(_sj(_sn[1]));},_so=function(_sp,_sq){while(1){var _sr=E(_sp);if(!_sr[0]){_sp=[1,I_fromInt(_sr[1])];continue;}else{return [1,I_shiftLeft(_sr[1],_sq)];}}},_ss=function(_st){var _su=B(_q7(_st)),_sv=_su[1],_sw=_su[2];if(_sw<0){var _sx=function(_sy){if(!_sy){return [0,E(_sv),B(_so(_ry, -_sw))];}else{var _sz=B(_rW(B(_sg(_sv)), -_sw));return [0,E(_sz[1]),B(_so(_ry,_sz[2]))];}};return (B(_sl(_sv))&1)>>>0==0?B(_sx(1)):B(_sx(0));}else{return [0,B(_so(_sv,_sw)),_ry];}},_sA=function(_sB){var _sC=B(_ss(E(_sB)[1]));return [0,E(_sC[1]),E(_sC[2])];},_sD=[0,_of,_rx,_sA],_sE=function(_sF){return E(E(_sF)[1]);},_sG=[0,1],_sH=function(_sI){return new F(function(){return _mW(E(_sI)[1],2147483647);});},_sJ=function(_sK,_sL,_sM){return _sM<=_sL?[1,[0,_sK],new T(function(){var _sN=_sL-_sK|0,_sO=function(_sP){return _sP>=(_sM-_sN|0)?[1,[0,_sP],new T(function(){return B(_sO(_sP+_sN|0));})]:[1,[0,_sP],_q];};return B(_sO(_sL));})]:_sM<=_sK?[1,[0,_sK],_q]:[0];},_sQ=function(_sR,_sS,_sT){return _sT>=_sS?[1,[0,_sR],new T(function(){var _sU=_sS-_sR|0,_sV=function(_sW){return _sW<=(_sT-_sU|0)?[1,[0,_sW],new T(function(){return B(_sV(_sW+_sU|0));})]:[1,[0,_sW],_q];};return B(_sV(_sS));})]:_sT>=_sR?[1,[0,_sR],_q]:[0];},_sX=function(_sY,_sZ){return _sZ<_sY?B(_sJ(_sY,_sZ,-2147483648)):B(_sQ(_sY,_sZ,2147483647));},_t0=function(_t1,_t2){return new F(function(){return _sX(E(_t1)[1],E(_t2)[1]);});},_t3=function(_t4,_t5,_t6){return _t5<_t4?B(_sJ(_t4,_t5,_t6)):B(_sQ(_t4,_t5,_t6));},_t7=function(_t8,_t9,_ta){return new F(function(){return _t3(E(_t8)[1],E(_t9)[1],E(_ta)[1]);});},_tb=function(_tc,_td){return new F(function(){return _mW(E(_tc)[1],E(_td)[1]);});},_te=function(_tf){return E(_tf);},_tg=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_th=new T(function(){return B(err(_tg));}),_ti=function(_tj){var _tk=E(E(_tj)[1]);return _tk==(-2147483648)?E(_th):[0,_tk-1|0];},_tl=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_tm=new T(function(){return B(err(_tl));}),_tn=function(_to){var _tp=E(E(_to)[1]);return _tp==2147483647?E(_tm):[0,_tp+1|0];},_tq=[0,_tn,_ti,_te,_te,_sH,_t0,_tb,_t7],_tr=function(_ts,_tt){if(_ts<=0){if(_ts>=0){return new F(function(){return quot(_ts,_tt);});}else{if(_tt<=0){return new F(function(){return quot(_ts,_tt);});}else{return quot(_ts+1|0,_tt)-1|0;}}}else{if(_tt>=0){if(_ts>=0){return new F(function(){return quot(_ts,_tt);});}else{if(_tt<=0){return new F(function(){return quot(_ts,_tt);});}else{return quot(_ts+1|0,_tt)-1|0;}}}else{return quot(_ts-1|0,_tt)-1|0;}}},_tu=new T(function(){return B(unCStr("ArithException"));}),_tv=new T(function(){return B(unCStr("GHC.Exception"));}),_tw=new T(function(){return B(unCStr("base"));}),_tx=new T(function(){var _ty=hs_wordToWord64(4194982440),_tz=_ty,_tA=hs_wordToWord64(3110813675),_tB=_tA;return [0,_tz,_tB,[0,_tz,_tB,_tw,_tv,_tu],_q];}),_tC=function(_tD){return E(_tx);},_tE=function(_tF){var _tG=E(_tF);return new F(function(){return _4L(B(_4J(_tG[1])),_tC,_tG[2]);});},_tH=new T(function(){return B(unCStr("arithmetic underflow"));}),_tI=new T(function(){return B(unCStr("arithmetic overflow"));}),_tJ=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_tK=new T(function(){return B(unCStr("denormal"));}),_tL=new T(function(){return B(unCStr("divide by zero"));}),_tM=new T(function(){return B(unCStr("loss of precision"));}),_tN=function(_tO){switch(E(_tO)){case 0:return E(_tI);case 1:return E(_tH);case 2:return E(_tM);case 3:return E(_tL);case 4:return E(_tK);default:return E(_tJ);}},_tP=function(_tQ){return new F(function(){return _K(_tH,_tQ);});},_tR=function(_tQ){return new F(function(){return _K(_tI,_tQ);});},_tS=function(_tQ){return new F(function(){return _K(_tJ,_tQ);});},_tT=function(_tQ){return new F(function(){return _K(_tK,_tQ);});},_tU=function(_tQ){return new F(function(){return _K(_tL,_tQ);});},_tV=function(_tQ){return new F(function(){return _K(_tM,_tQ);});},_tW=function(_tX){switch(E(_tX)){case 0:return E(_tR);case 1:return E(_tP);case 2:return E(_tV);case 3:return E(_tU);case 4:return E(_tT);default:return E(_tS);}},_tY=function(_tZ,_u0){return new F(function(){return _56(_tW,_tZ,_u0);});},_u1=function(_u2,_u3){switch(E(_u3)){case 0:return E(_tR);case 1:return E(_tP);case 2:return E(_tV);case 3:return E(_tU);case 4:return E(_tT);default:return E(_tS);}},_u4=[0,_u1,_tN,_tY],_u5=new T(function(){return [0,_tC,_u4,_u6,_tE];}),_u6=function(_tQ){return [0,_u5,_tQ];},_u7=3,_u8=new T(function(){return B(_u6(_u7));}),_u9=new T(function(){return die(_u8);}),_ua=0,_ub=new T(function(){return B(_u6(_ua));}),_uc=new T(function(){return die(_ub);}),_ud=function(_ue,_uf){var _ug=E(_uf);switch(_ug){case -1:var _uh=E(_ue);return _uh==(-2147483648)?E(_uc):B(_tr(_uh,-1));case 0:return E(_u9);default:return new F(function(){return _tr(_ue,_ug);});}},_ui=function(_uj,_uk){return [0,B(_ud(E(_uj)[1],E(_uk)[1]))];},_ul=[0,0],_um=[0,_uc,_ul],_un=function(_uo,_up){var _uq=E(_uo)[1],_ur=E(E(_up)[1]);switch(_ur){case -1:var _us=E(_uq);if(_us==(-2147483648)){return E(_um);}else{if(_us<=0){if(_us>=0){var _ut=quotRemI(_us,-1);return [0,[0,_ut[1]],[0,_ut[2]]];}else{var _uu=quotRemI(_us,-1);return [0,[0,_uu[1]],[0,_uu[2]]];}}else{var _uv=quotRemI(_us-1|0,-1);return [0,[0,_uv[1]-1|0],[0,(_uv[2]+(-1)|0)+1|0]];}}break;case 0:return E(_u9);default:if(_uq<=0){if(_uq>=0){var _uw=quotRemI(_uq,_ur);return [0,[0,_uw[1]],[0,_uw[2]]];}else{if(_ur<=0){var _ux=quotRemI(_uq,_ur);return [0,[0,_ux[1]],[0,_ux[2]]];}else{var _uy=quotRemI(_uq+1|0,_ur);return [0,[0,_uy[1]-1|0],[0,(_uy[2]+_ur|0)-1|0]];}}}else{if(_ur>=0){if(_uq>=0){var _uz=quotRemI(_uq,_ur);return [0,[0,_uz[1]],[0,_uz[2]]];}else{if(_ur<=0){var _uA=quotRemI(_uq,_ur);return [0,[0,_uA[1]],[0,_uA[2]]];}else{var _uB=quotRemI(_uq+1|0,_ur);return [0,[0,_uB[1]-1|0],[0,(_uB[2]+_ur|0)-1|0]];}}}else{var _uC=quotRemI(_uq-1|0,_ur);return [0,[0,_uC[1]-1|0],[0,(_uC[2]+_ur|0)+1|0]];}}}},_uD=function(_uE,_uF){var _uG=_uE%_uF;if(_uE<=0){if(_uE>=0){return E(_uG);}else{if(_uF<=0){return E(_uG);}else{var _uH=E(_uG);return _uH==0?0:_uH+_uF|0;}}}else{if(_uF>=0){if(_uE>=0){return E(_uG);}else{if(_uF<=0){return E(_uG);}else{var _uI=E(_uG);return _uI==0?0:_uI+_uF|0;}}}else{var _uJ=E(_uG);return _uJ==0?0:_uJ+_uF|0;}}},_uK=function(_uL,_uM){var _uN=E(E(_uM)[1]);switch(_uN){case -1:return E(_ul);case 0:return E(_u9);default:return [0,B(_uD(E(_uL)[1],_uN))];}},_uO=function(_uP,_uQ){var _uR=E(_uP)[1],_uS=E(E(_uQ)[1]);switch(_uS){case -1:var _uT=E(_uR);return _uT==(-2147483648)?E(_uc):[0,quot(_uT,-1)];case 0:return E(_u9);default:return [0,quot(_uR,_uS)];}},_uU=function(_uV,_uW){var _uX=E(_uV)[1],_uY=E(E(_uW)[1]);switch(_uY){case -1:var _uZ=E(_uX);if(_uZ==(-2147483648)){return E(_um);}else{var _v0=quotRemI(_uZ,-1);return [0,[0,_v0[1]],[0,_v0[2]]];}break;case 0:return E(_u9);default:var _v1=quotRemI(_uX,_uY);return [0,[0,_v1[1]],[0,_v1[2]]];}},_v2=function(_v3,_v4){var _v5=E(E(_v4)[1]);switch(_v5){case -1:return E(_ul);case 0:return E(_u9);default:return [0,E(_v3)[1]%_v5];}},_v6=function(_v7){return new F(function(){return _8B(E(_v7)[1]);});},_v8=function(_v9){return [0,E(B(_8B(E(_v9)[1]))),E(_sG)];},_va=function(_vb,_vc){return [0,imul(E(_vb)[1],E(_vc)[1])|0];},_vd=function(_ve,_vf){return [0,E(_ve)[1]+E(_vf)[1]|0];},_vg=function(_vh,_vi){return [0,E(_vh)[1]-E(_vi)[1]|0];},_vj=function(_vk){var _vl=E(_vk),_vm=_vl[1];return _vm<0?[0, -_vm]:E(_vl);},_vn=function(_vo){return [0,B(_9Z(_vo))];},_vp=function(_vq){return [0, -E(_vq)[1]];},_vr=[0,-1],_vs=[0,0],_vt=[0,1],_vu=function(_vv){var _vw=E(_vv)[1];return _vw>=0?E(_vw)==0?E(_vs):E(_vt):E(_vr);},_vx=[0,_vd,_va,_vg,_vp,_vj,_vu,_vn],_vy=function(_vz,_vA){return E(_vz)[1]==E(_vA)[1];},_vB=function(_vC,_vD){return E(_vC)[1]!=E(_vD)[1];},_vE=[0,_vy,_vB],_vF=function(_vG,_vH){var _vI=E(_vG),_vJ=E(_vH);return _vI[1]>_vJ[1]?E(_vI):E(_vJ);},_vK=function(_vL,_vM){var _vN=E(_vL),_vO=E(_vM);return _vN[1]>_vO[1]?E(_vO):E(_vN);},_vP=function(_vQ,_vR){return _vQ>=_vR?_vQ!=_vR?2:1:0;},_vS=function(_vT,_vU){return new F(function(){return _vP(E(_vT)[1],E(_vU)[1]);});},_vV=function(_vW,_vX){return E(_vW)[1]>=E(_vX)[1];},_vY=function(_vZ,_w0){return E(_vZ)[1]>E(_w0)[1];},_w1=function(_w2,_w3){return E(_w2)[1]<=E(_w3)[1];},_w4=function(_w5,_w6){return E(_w5)[1]<E(_w6)[1];},_w7=[0,_vE,_vS,_w4,_vV,_vY,_w1,_vF,_vK],_w8=[0,_vx,_w7,_v8],_w9=[0,_w8,_tq,_uO,_v2,_ui,_uK,_uU,_un,_v6],_wa=function(_wb){return E(E(_wb)[1]);},_wc=function(_wd,_we,_wf){while(1){if(!(_we%2)){var _wg=B(_8D(_wd,_wd)),_wh=quot(_we,2);_wd=_wg;_we=_wh;continue;}else{var _wi=E(_we);if(_wi==1){return new F(function(){return _8D(_wd,_wf);});}else{var _wg=B(_8D(_wd,_wd));_we=quot(_wi-1|0,2);var _wj=B(_8D(_wd,_wf));_wd=_wg;_wf=_wj;continue;}}}},_wk=function(_wl,_wm){while(1){if(!(_wm%2)){var _wn=B(_8D(_wl,_wl)),_wo=quot(_wm,2);_wl=_wn;_wm=_wo;continue;}else{var _wp=E(_wm);if(_wp==1){return E(_wl);}else{return new F(function(){return _wc(B(_8D(_wl,_wl)),quot(_wp-1|0,2),_wl);});}}}},_wq=function(_wr){return E(E(_wr)[2]);},_ws=function(_wt){return E(E(_wt)[1]);},_wu=function(_wv){return E(E(_wv)[2]);},_ww=[0,0],_wx=[0,2],_wy=function(_wz){return E(E(_wz)[7]);},_wA=function(_wB,_wC,_wD,_wE,_wF){return new F(function(){return A(E(E(_wC)[1])[1],[new T(function(){return B(A(_wE,[_wF,new T(function(){return B(A(_wy,[_wB,_wx]));})]));}),new T(function(){return B(A(_wy,[_wB,_ww]));})]);});},_wG=function(_wH){return E(E(_wH)[3]);},_wI=new T(function(){return B(unCStr("Negative exponent"));}),_wJ=new T(function(){return B(err(_wI));}),_wK=function(_wL,_wM,_wN,_wO){var _wP=B(_sE(_wM)),_wQ=_wP[1],_wR=E(_wP[2]);if(!B(A(_wR[3],[_wO,new T(function(){return B(A(_wy,[_wQ,_ww]));})]))){if(!B(A(E(_wR[1])[1],[_wO,new T(function(){return B(A(_wy,[_wQ,_ww]));})]))){var _wS=B(_sE(_wM)),_wT=_wS[1],_wU=new T(function(){return B(_sE(_wM));}),_wV=new T(function(){return B(_wa(_wU));});return new F(function(){return (function(_wW,_wX){while(1){var _wY=(function(_wZ,_x0){var _x1=E(_wM),_x2=_x1[3],_x3=E(_x1[1]);if(!B(_wA(_x3[1],_x3[2],_x3[3],_x1[4],_x0))){return !B(A(E(E(_wS[2])[1])[1],[_x0,new T(function(){return B(A(_wy,[_wT,_sG]));})]))?B((function(_x4,_x5,_x6){while(1){var _x7=(function(_x8,_x9,_xa){var _xb=E(_wM),_xc=_xb[3],_xd=E(_xb[1]);if(!B(_wA(_xd[1],_xd[2],_xd[3],_xb[4],_x9))){if(!B(A(new T(function(){return B(_2F(new T(function(){return B(_ws(new T(function(){return B(_wu(_wU));})));})));}),[_x9,new T(function(){return B(A(_wy,[_wV,_sG]));})]))){_x4=new T(function(){return B(A(new T(function(){return B(_wq(_wL));}),[_x8,_x8]));});_x5=new T(function(){return B(A(_xc,[new T(function(){return B(A(new T(function(){return B(_wG(_wV));}),[_x9,new T(function(){return B(A(_wy,[_wV,_sG]));})]));}),new T(function(){return B(A(_wy,[_wV,_wx]));})]));});_x6=new T(function(){return B(A(new T(function(){return B(_wq(_wL));}),[_x8,_xa]));});return null;}else{return new F(function(){return A(new T(function(){return B(_wq(_wL));}),[_x8,_xa]);});}}else{_x4=new T(function(){return B(A(new T(function(){return B(_wq(_wL));}),[_x8,_x8]));});_x5=new T(function(){return B(A(_xc,[_x9,new T(function(){return B(A(_wy,[_wV,_wx]));})]));});var _xe=_xa;_x6=_xe;return null;}})(_x4,_x5,_x6);if(_x7!=null){return _x7;}}})(new T(function(){return B(A(new T(function(){return B(_wq(_wL));}),[_wZ,_wZ]));}),new T(function(){return B(A(_x2,[new T(function(){return B(A(new T(function(){return B(_wG(_wT));}),[_x0,new T(function(){return B(A(_wy,[_wT,_sG]));})]));}),new T(function(){return B(A(_wy,[_wT,_wx]));})]));}),_wZ)):E(_wZ);}else{_wW=new T(function(){return B(A(new T(function(){return B(_wq(_wL));}),[_wZ,_wZ]));});_wX=new T(function(){return B(A(_x2,[_x0,new T(function(){return B(A(_wy,[_wT,_wx]));})]));});return null;}})(_wW,_wX);if(_wY!=null){return _wY;}}})(_wN,_wO);});}else{return new F(function(){return A(_wy,[_wL,_sG]);});}}else{return E(_wJ);}},_xf=new T(function(){return B(err(_wI));}),_xg=function(_xh,_xi){var _xj=E(_xh);return _xj[0]==0?_xj[1]*Math.pow(2,_xi):I_toNumber(_xj[1])*Math.pow(2,_xi);},_xk=function(_xl,_xm){while(1){var _xn=E(_xl);if(!_xn[0]){var _xo=E(_xn[1]);if(_xo==(-2147483648)){_xl=[1,I_fromInt(-2147483648)];continue;}else{var _xp=E(_xm);if(!_xp[0]){var _xq=_xp[1];return [0,[0,quot(_xo,_xq)],[0,_xo%_xq]];}else{_xl=[1,I_fromInt(_xo)];_xm=_xp;continue;}}}else{var _xr=E(_xm);if(!_xr[0]){_xl=_xn;_xm=[1,I_fromInt(_xr[1])];continue;}else{var _xs=I_quotRem(_xn[1],_xr[1]);return [0,[1,_xs[1]],[1,_xs[2]]];}}}},_xt=function(_xu,_xv){var _xw=B(_q7(_xv)),_xx=_xw[1],_xy=_xw[2],_xz=new T(function(){return B(_wa(new T(function(){return B(_sE(_xu));})));});if(_xy<0){var _xA= -_xy;if(_xA>=0){var _xB=E(_xA),_xC=_xB==0?E(_sG):B(_wk(_qg,_xB));if(!B(_nz(_xC,_nt))){var _xD=B(_xk(_xx,_xC));return [0,new T(function(){return B(A(_wy,[_xz,_xD[1]]));}),new T(function(){return [0,B(_xg(_xD[2],_xy))];})];}else{return E(_u9);}}else{return E(_xf);}}else{return [0,new T(function(){return B(A(_wq,[_xz,new T(function(){return B(A(_wy,[_xz,_xx]));}),new T(function(){return B(_wK(_xz,_w9,new T(function(){return B(A(_wy,[_xz,_qg]));}),[0,_xy]));})]));}),_nY];}},_xE=function(_xF,_xG){var _xH=B(_xt(_xF,E(_xG)[1])),_xI=_xH[1];if(E(_xH[2])[1]<=0){return E(_xI);}else{var _xJ=E(B(_sE(_xF))[1]);return new F(function(){return A(_xJ[1],[_xI,new T(function(){return B(A(_xJ[7],[_ry]));})]);});}},_xK=function(_xL,_xM){var _xN=B(_xt(_xL,E(_xM)[1])),_xO=_xN[1];if(E(_xN[2])[1]>=0){return E(_xO);}else{var _xP=E(B(_sE(_xL))[1]);return new F(function(){return A(_xP[3],[_xO,new T(function(){return B(A(_xP[7],[_ry]));})]);});}},_xQ=function(_xR,_xS){var _xT=B(_xt(_xR,E(_xS)[1]));return [0,_xT[1],_xT[2]];},_xU=function(_xV,_xW){var _xX=B(_xt(_xV,_xW)),_xY=_xX[1],_xZ=E(_xX[2])[1],_y0=new T(function(){var _y1=E(B(_sE(_xV))[1]),_y2=_y1[7];return _xZ>=0?B(A(_y1[1],[_xY,new T(function(){return B(A(_y2,[_ry]));})])):B(A(_y1[3],[_xY,new T(function(){return B(A(_y2,[_ry]));})]));});if(_xZ<0){var _y3= -_xZ-0.5;if(_y3>=0){if(!E(_y3)){var _y4=E(_xV),_y5=E(_y4[1]);return !B(_wA(_y5[1],_y5[2],_y5[3],_y4[4],_xY))?E(_y0):E(_xY);}else{return E(_y0);}}else{return E(_xY);}}else{var _y6=_xZ-0.5;if(_y6>=0){if(!E(_y6)){var _y7=E(_xV),_y8=E(_y7[1]);return !B(_wA(_y8[1],_y8[2],_y8[3],_y7[4],_xY))?E(_y0):E(_xY);}else{return E(_y0);}}else{return E(_xY);}}},_y9=function(_ya,_yb){return new F(function(){return _xU(_ya,E(_yb)[1]);});},_yc=function(_yd,_ye){return E(B(_xt(_yd,E(_ye)[1]))[1]);},_yf=[0,_sD,_oj,_xQ,_yc,_y9,_xE,_xK],_yg=function(_yh,_yi){return E(_yh)[1]!=E(_yi)[1]?true:false;},_yj=function(_yk,_yl){return E(_yk)[1]==E(_yl)[1];},_ym=[0,_yj,_yg],_yn=function(_yo,_yp){return E(_yo)[1]<E(_yp)[1];},_yq=function(_yr,_ys){return E(_yr)[1]<=E(_ys)[1];},_yt=function(_yu,_yv){return E(_yu)[1]>E(_yv)[1];},_yw=function(_yx,_yy){return E(_yx)[1]>=E(_yy)[1];},_yz=function(_yA,_yB){var _yC=E(_yA)[1],_yD=E(_yB)[1];return _yC>=_yD?_yC!=_yD?2:1:0;},_yE=function(_yF,_yG){var _yH=E(_yF),_yI=E(_yG);return _yH[1]>_yI[1]?E(_yH):E(_yI);},_yJ=function(_yK,_yL){var _yM=E(_yK),_yN=E(_yL);return _yM[1]>_yN[1]?E(_yN):E(_yM);},_yO=[0,_ym,_yz,_yn,_yw,_yt,_yq,_yE,_yJ],_yP=function(_yQ,_yR){while(1){var _yS=(function(_yT,_yU){var _yV=E(_rL)[1]["v"]["i8"][(255&_yT>>>0)>>>0&4294967295];if(_yU>_yV){if(_yV>=8){var _yW=_yT>>8,_yX=_yU-8|0;_yQ=_yW;_yR=_yX;return null;}else{return [0,new T(function(){return B(_8B(_yT>>_yV));}),_yU-_yV|0];}}else{return [0,new T(function(){return B(_8B(_yT>>_yU));}),0];}})(_yQ,_yR);if(_yS!=null){return _yS;}}},_yY=function(_yZ){var _z0=decodeFloat(_yZ),_z1=_z0[1],_z2=_z0[2];if(_z2<0){var _z3=function(_z4){if(!_z4){return [0,B(_8B(_z1)),B(_so(_ry, -_z2))];}else{var _z5=B(_yP(_z1, -_z2));return [0,E(_z5[1]),B(_so(_ry,_z5[2]))];}};return (_z1>>>0&1)>>>0==0?B(_z3(1)):B(_z3(0));}else{return [0,B(_so(B(_8B(_z1)),_z2)),_ry];}},_z6=function(_z7){var _z8=B(_yY(E(_z7)[1]));return [0,E(_z8[1]),E(_z8[2])];},_z9=[0,_pA,_yO,_z6],_za=[0,-1],_zb=[0,1],_zc=function(_zd,_ze){var _zf=E(_zd);return _zf[0]==0?_zf[1]*Math.pow(2,_ze):I_toNumber(_zf[1])*Math.pow(2,_ze);},_zg=[0,0],_zh=function(_zi,_zj){var _zk=decodeFloat(_zj),_zl=_zk[1],_zm=_zk[2],_zn=new T(function(){return B(_wa(new T(function(){return B(_sE(_zi));})));});if(_zm<0){var _zo=new T(function(){if(_zl<0){var _zp= -_zm;if(_zp<32){var _zq=[0, -( -_zl>>_zp)];}else{var _zq= -_zl>=0?E(_zg):E(_zb);}var _zr=_zq,_zs=_zr,_zt=_zs;}else{var _zu= -_zm;if(_zu<32){var _zv=[0,_zl>>_zu];}else{var _zv=_zl>=0?E(_zg):E(_za);}var _zw=_zv,_zx=_zw,_zt=_zx;}var _zy=_zt;return _zy;});return [0,new T(function(){return B(A(_wy,[_zn,new T(function(){return B(_8B(E(_zo)[1]));})]));}),new T(function(){var _zz= -_zm;if(_zz<32){var _zA=[0,B(_zc(B(_8B(_zl-(E(_zo)[1]<<_zz)|0)),_zm))];}else{var _zA=[0,B(_zc(B(_8B(_zl)),_zm))];}var _zB=_zA,_zC=_zB,_zD=_zC;return _zD;})];}else{return [0,new T(function(){return B(A(_wq,[_zn,new T(function(){return B(A(_wy,[_zn,new T(function(){return B(_8B(_zl));})]));}),new T(function(){return B(_wK(_zn,_w9,new T(function(){return B(A(_wy,[_zn,_qg]));}),[0,_zm]));})]));}),_pj];}},_zE=function(_zF,_zG){var _zH=B(_zh(_zF,E(_zG)[1])),_zI=_zH[1];if(E(_zH[2])[1]<=0){return E(_zI);}else{var _zJ=E(B(_sE(_zF))[1]);return new F(function(){return A(_zJ[1],[_zI,new T(function(){return B(A(_zJ[7],[_ry]));})]);});}},_zK=function(_zL,_zM){var _zN=B(_zh(_zL,E(_zM)[1])),_zO=_zN[1];if(E(_zN[2])[1]>=0){return E(_zO);}else{var _zP=E(B(_sE(_zL))[1]);return new F(function(){return A(_zP[3],[_zO,new T(function(){return B(A(_zP[7],[_ry]));})]);});}},_zQ=function(_zR,_zS){var _zT=B(_zh(_zR,E(_zS)[1]));return [0,_zT[1],_zT[2]];},_zU=function(_zV,_zW){var _zX=B(_zh(_zV,_zW)),_zY=_zX[1],_zZ=E(_zX[2])[1],_A0=new T(function(){var _A1=E(B(_sE(_zV))[1]),_A2=_A1[7];return _zZ>=0?B(A(_A1[1],[_zY,new T(function(){return B(A(_A2,[_ry]));})])):B(A(_A1[3],[_zY,new T(function(){return B(A(_A2,[_ry]));})]));});if(_zZ<0){var _A3= -_zZ-0.5;if(_A3>=0){if(!E(_A3)){var _A4=E(_zV),_A5=E(_A4[1]);return !B(_wA(_A5[1],_A5[2],_A5[3],_A4[4],_zY))?E(_A0):E(_zY);}else{return E(_A0);}}else{return E(_zY);}}else{var _A6=_zZ-0.5;if(_A6>=0){if(!E(_A6)){var _A7=E(_zV),_A8=E(_A7[1]);return !B(_wA(_A8[1],_A8[2],_A8[3],_A7[4],_zY))?E(_A0):E(_zY);}else{return E(_A0);}}else{return E(_zY);}}},_A9=function(_Aa,_Ab){return new F(function(){return _zU(_Aa,E(_Ab)[1]);});},_Ac=function(_Ad,_Ae){return E(B(_zh(_Ad,E(_Ae)[1]))[1]);},_Af=[0,_z9,_pE,_zQ,_Ac,_A9,_zE,_zK],_Ag=function(_Ah){return new F(function(){return err(B(unAppCStr("Char.intToDigit: not a digit ",new T(function(){if(_Ah>=0){var _Ai=jsShowI(_Ah),_Aj=_Ai,_Ak=fromJSStr(_Aj);}else{var _Al=jsShowI(_Ah),_Am=_Al,_Ak=fromJSStr(_Am);}var _An=_Ak;return _An;}))));});},_Ao=function(_Ap){var _Aq=function(_Ar){if(_Ap<10){return new F(function(){return _Ag(_Ap);});}else{if(_Ap>15){return new F(function(){return _Ag(_Ap);});}else{return (97+_Ap|0)-10|0;}}};if(_Ap<0){return new F(function(){return _Aq(_);});}else{if(_Ap>9){return new F(function(){return _Aq(_);});}else{return 48+_Ap|0;}}},_As=function(_At){return [0,B(_Ao(E(_At)[1]))];},_Au=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_Av=function(_Aw){return new F(function(){return _5q([0,new T(function(){return B(_5F(_Aw,_Au));})],_5n);});},_Ax=new T(function(){return B(_Av("GHC/Float.lhs:619:11-64|d : ds\'"));}),_Ay=function(_Az,_AA){if(E(_Az)[1]<=0){var _AB=B(_1s(_As,[1,_zg,_AA]));return _AB[0]==0?E(_Ax):[0,_AB[1],_AB[2]];}else{var _AC=B(_1s(_As,_AA));return _AC[0]==0?E(_Ax):[0,_AC[1],_AC[2]];}},_AD=function(_AE){return E(E(_AE)[1]);},_AF=function(_AG){return E(E(_AG)[1]);},_AH=function(_AI){return E(E(_AI)[1]);},_AJ=[0,48],_AK=[1,_AJ,_q],_AL=[0,46],_AM=function(_AN,_AO,_AP){while(1){var _AQ=(function(_AR,_AS,_AT){var _AU=E(_AR);if(!_AU){var _AV=B(_n8(_AS,_q));return _AV[0]==0?[1,_AJ,[1,_AL,new T(function(){var _AW=E(_AT);return _AW[0]==0?E(_AK):E(_AW);})]]:B(_K(_AV,[1,_AL,new T(function(){var _AX=E(_AT);return _AX[0]==0?E(_AK):E(_AX);})]));}else{var _AY=E(_AT);if(!_AY[0]){_AN=_AU-1|0;var _AZ=[1,_AJ,_AS];_AP=_q;_AO=_AZ;return null;}else{_AN=_AU-1|0;var _AZ=[1,_AY[1],_AS];_AP=_AY[2];_AO=_AZ;return null;}}})(_AN,_AO,_AP);if(_AQ!=null){return _AQ;}}},_B0=[0,0],_B1=new T(function(){return B(unCStr(" out of range "));}),_B2=new T(function(){return B(unCStr("}.index: Index "));}),_B3=new T(function(){return B(unCStr("Ix{"));}),_B4=[1,_12,_q],_B5=[1,_12,_B4],_B6=function(_B7,_B8,_B9,_Ba,_Bb){return new F(function(){return err(B(_K(_B3,new T(function(){return B(_K(_B7,new T(function(){return B(_K(_B2,[1,_13,new T(function(){return B(A(_Bb,[_B0,_B8,[1,_12,new T(function(){return B(_K(_B1,[1,_13,[1,_13,new T(function(){return B(A(_kA,[_kq,[1,new T(function(){return B(A(_Bb,[_jV,_B9]));}),[1,new T(function(){return B(A(_Bb,[_jV,_Ba]));}),_q]],_B5]));})]]));})]]));})]));})));}))));});},_Bc=function(_Bd,_Be,_Bf,_Bg){var _Bh=E(_Bf);return new F(function(){return _B6(_Bd,_Be,_Bh[1],_Bh[2],E(_Bg)[1]);});},_Bi=function(_Bj,_Bk,_Bl,_Bm){return new F(function(){return _Bc(_Bm,_Bl,_Bk,_Bj);});},_Bn=new T(function(){return B(unCStr("Int"));}),_Bo=function(_Bp,_Bq,_Br){return new F(function(){return _Bi(_kp,[0,_Bq,_Br],_Bp,_Bn);});},_Bs=new T(function(){return B(unCStr("(Array.!): undefined array element"));}),_Bt=new T(function(){return B(err(_Bs));}),_Bu=[0,1100],_Bv=[0,_zg,_Bu],_Bw=function(_Bx){return new F(function(){return _Bi(_kp,_Bv,[0,_Bx],_Bn);});},_By=function(_){var _Bz=newArr(1101,_Bt),_BA=_Bz;return new F(function(){return (function(_BB,_){while(1){var _BC=(function(_BD,_){if(0>_BD){return new F(function(){return _Bw(_BD);});}else{if(_BD>1100){return new F(function(){return _Bw(_BD);});}else{var _=_BA[_BD]=new T(function(){if(_BD>=0){var _BE=E(_BD),_BF=_BE==0?E(_sG):B(_wk(_qg,_BE));}else{var _BF=E(_xf);}var _BG=_BF;return _BG;}),_BH=E(_BD);if(_BH==1100){var _BI=_BA,_BJ=_BI;return [0,E(_zg),E(_Bu),1101,_BJ];}else{_BB=_BH+1|0;return null;}}}})(_BB,_);if(_BC!=null){return _BC;}}})(0,_);});},_BK=function(_BL){var _BM=B(A(_BL,[_])),_BN=_BM;return E(_BN);},_BO=new T(function(){return B(_BK(_By));}),_BP=[0,10],_BQ=[0,324],_BR=[0,_zg,_BQ],_BS=function(_BT){return new F(function(){return _Bi(_kp,_BR,[0,_BT],_Bn);});},_BU=function(_){var _BV=newArr(325,_Bt),_BW=_BV;return new F(function(){return (function(_BX,_){while(1){var _BY=(function(_BZ,_){if(0>_BZ){return new F(function(){return _BS(_BZ);});}else{if(_BZ>324){return new F(function(){return _BS(_BZ);});}else{var _=_BW[_BZ]=new T(function(){if(_BZ>=0){var _C0=E(_BZ),_C1=_C0==0?E(_sG):B(_wk(_BP,_C0));}else{var _C1=E(_xf);}var _C2=_C1;return _C2;}),_C3=E(_BZ);if(_C3==324){var _C4=_BW,_C5=_C4;return [0,E(_zg),E(_BQ),325,_C5];}else{_BX=_C3+1|0;return null;}}}})(_BX,_);if(_BY!=null){return _BY;}}})(0,_);});},_C6=new T(function(){return B(_BK(_BU));}),_C7=function(_C8,_C9){var _Ca=[0,_C9],_Cb=function(_Cc){if(!B(_nz(_C8,_BP))){if(_C9>=0){var _Cd=E(_C9);return _Cd==0?E(_sG):B(_wk(_C8,_Cd));}else{return E(_xf);}}else{if(_C9>324){if(_C9>=0){var _Ce=E(_C9);return _Ce==0?E(_sG):B(_wk(_C8,_Ce));}else{return E(_xf);}}else{var _Cf=E(_C6),_Cg=E(_Cf[1]),_Ch=_Cg[1],_Ci=E(_Cf[2]);if(_Ch>_C9){return new F(function(){return _Bo(_Ca,_Cg,_Ci);});}else{if(_C9>_Ci[1]){return new F(function(){return _Bo(_Ca,_Cg,_Ci);});}else{return E(_Cf[4][_C9-_Ch|0]);}}}}};if(!B(_nz(_C8,_qg))){return new F(function(){return _Cb(_);});}else{if(_C9<0){return new F(function(){return _Cb(_);});}else{if(_C9>1100){return new F(function(){return _Cb(_);});}else{var _Cj=E(_BO),_Ck=E(_Cj[1]),_Cl=_Ck[1],_Cm=E(_Cj[2]);if(_Cl>_C9){return new F(function(){return _Bo(_Ca,_Ck,_Cm);});}else{if(_C9>_Cm[1]){return new F(function(){return _Bo(_Ca,_Ck,_Cm);});}else{return E(_Cj[4][_C9-_Cl|0]);}}}}}},_Cn=function(_Co,_Cp){var _Cq=E(_Co);if(!_Cq[0]){var _Cr=_Cq[1],_Cs=E(_Cp);return _Cs[0]==0?_Cr>_Cs[1]:I_compareInt(_Cs[1],_Cr)<0;}else{var _Ct=_Cq[1],_Cu=E(_Cp);return _Cu[0]==0?I_compareInt(_Ct,_Cu[1])>0:I_compare(_Ct,_Cu[1])>0;}},_Cv=[1,_zg,_q],_Cw=function(_Cx,_Cy){while(1){var _Cz=E(_Cx);if(!_Cz[0]){var _CA=E(_Cz[1]);if(_CA==(-2147483648)){_Cx=[1,I_fromInt(-2147483648)];continue;}else{var _CB=E(_Cy);if(!_CB[0]){return [0,quot(_CA,_CB[1])];}else{_Cx=[1,I_fromInt(_CA)];_Cy=_CB;continue;}}}else{var _CC=_Cz[1],_CD=E(_Cy);return _CD[0]==0?[0,I_toInt(I_quot(_CC,I_fromInt(_CD[1])))]:[1,I_quot(_CC,_CD[1])];}}},_CE=function(_CF,_CG,_CH,_CI,_CJ,_CK,_CL,_CM){if(!B(A(_CF,[_CM,new T(function(){return B(A(_wy,[B(_AF(B(_AD(_CG)))),_nt]));})]))){var _CN=new T(function(){return B(A(_CH,[_CM]));}),_CO=new T(function(){return B(A(_CI,[_CM]));}),_CP=new T(function(){return [0,E(B(A(_CJ,[_CM]))[1])[1]-E(_CO)[1]|0];}),_CQ=new T(function(){return B(A(_CK,[_CM]));}),_CR=new T(function(){return E(E(_CQ)[2]);}),_CS=new T(function(){var _CT=E(_CR),_CU=_CT[1],_CV=E(_CP)[1]-_CU|0;if(_CV<=0){var _CW=[0,new T(function(){return E(E(_CQ)[1]);}),_CT];}else{var _CW=[0,new T(function(){var _CX=B(_C7(_CN,_CV));if(!B(_nz(_CX,_nt))){var _CY=B(_Cw(E(_CQ)[1],_CX));}else{var _CY=E(_u9);}var _CZ=_CY;return _CZ;}),[0,_CU+_CV|0]];}var _D0=_CW,_D1=_D0,_D2=_D1,_D3=_D2;return _D3;}),_D4=new T(function(){return E(E(_CS)[2]);}),_D5=new T(function(){return E(E(_CS)[1]);}),_D6=new T(function(){var _D7=E(_D4)[1];if(_D7<0){if(_D7<=E(_CP)[1]){var _D8=[0,new T(function(){return B(_8D(_D5,_qg));}),new T(function(){return B(_8D(B(_C7(_CN, -_D7)),_qg));}),_ry,_ry];}else{var _D8=!B(_nz(_D5,B(_C7(_CN,E(_CO)[1]-1|0))))?[0,new T(function(){return B(_8D(_D5,_qg));}),new T(function(){return B(_8D(B(_C7(_CN, -_D7)),_qg));}),_ry,_ry]:[0,new T(function(){return B(_8D(B(_8D(_D5,_CN)),_qg));}),new T(function(){return B(_8D(B(_C7(_CN, -_D7+1|0)),_qg));}),_CN,_ry];}var _D9=_D8,_Da=_D9,_Db=_Da;}else{var _Dc=new T(function(){return B(_C7(_CN,_D7));}),_Db=!B(_nz(_D5,B(_C7(_CN,E(_CO)[1]-1|0))))?[0,new T(function(){return B(_8D(B(_8D(_D5,_Dc)),_qg));}),_qg,_Dc,_Dc]:[0,new T(function(){return B(_8D(B(_8D(B(_8D(_D5,_Dc)),_CN)),_qg));}),new T(function(){return B(_8D(_qg,_CN));}),new T(function(){return B(_8D(_Dc,_CN));}),_Dc];}var _Dd=_Db,_De=_Dd;return _De;}),_Df=new T(function(){return E(E(_D6)[2]);}),_Dg=new T(function(){return E(E(_D6)[3]);}),_Dh=new T(function(){return E(E(_D6)[1]);}),_Di=new T(function(){var _Dj=new T(function(){return B(_8l(_Dh,_Dg));}),_Dk=function(_Dl){var _Dm=(Math.log(B(_pe(B(_8l(_D5,_ry)))))+E(_D4)[1]*Math.log(B(_pe(_CN))))/Math.log(B(_pe(_CL))),_Dn=_Dm&4294967295;return _Dn>=_Dm?E(_Dn):_Dn+1|0;},_Do=function(_Dp){while(1){if(_Dp<0){if(!B(_a2(B(_8D(B(_C7(_CL, -_Dp)),_Dj)),_Df))){var _Dq=_Dp+1|0;_Dp=_Dq;continue;}else{return E(_Dp);}}else{if(!B(_a2(_Dj,B(_8D(B(_C7(_CL,_Dp)),_Df))))){var _Dq=_Dp+1|0;_Dp=_Dq;continue;}else{return E(_Dp);}}}};if(!B(_nz(_CN,_qg))){var _Dr=[0,B(_Do(B(_Dk(_))))];}else{if(!B(_nz(_CL,_BP))){var _Ds=[0,B(_Do(B(_Dk(_))))];}else{var _Dt=(E(_CO)[1]-1|0)+E(_CR)[1]|0;if(_Dt<0){var _Du=[0,B(_Do(quot(imul(_Dt,8651)|0,28738)))];}else{var _Du=[0,B(_Do(quot(imul(_Dt,8651)|0,28738)+1|0))];}var _Dv=_Du,_Dw=_Dv,_Dx=_Dw,_Dy=_Dx,_Dz=_Dy,_Ds=_Dz;}var _Dr=_Ds;}return _Dr;});return [0,new T(function(){var _DA=E(_Di)[1],_DB=function(_DC,_DD,_DE,_DF,_DG){while(1){var _DH=(function(_DI,_DJ,_DK,_DL,_DM){if(!B(_nz(_DK,_nt))){var _DN=B(_xk(B(_8D(_DJ,_CL)),_DK)),_DO=_DN[1],_DP=_DN[2],_DQ=B(_8D(_DM,_CL)),_DR=B(_8D(_DL,_CL));if(!B(_U(_DP,_DQ))){if(!B(_Cn(B(_8l(_DP,_DR)),_DK))){var _DS=[1,_DO,_DI];_DD=_DP;var _DT=_DK;_DF=_DR;_DG=_DQ;_DC=_DS;_DE=_DT;return null;}else{return [1,new T(function(){return B(_8l(_DO,_ry));}),_DI];}}else{return !B(_Cn(B(_8l(_DP,_DR)),_DK))?[1,_DO,_DI]:!B(_U(B(_8D(_DP,_qg)),_DK))?[1,new T(function(){return B(_8l(_DO,_ry));}),_DI]:[1,_DO,_DI];}}else{return E(_u9);}})(_DC,_DD,_DE,_DF,_DG);if(_DH!=null){return _DH;}}};if(_DA<0){var _DU=B(_C7(_CL, -_DA)),_DV=B(_1s(_vn,B(_n8(B(_DB(_q,B(_8D(_Dh,_DU)),_Df,B(_8D(_Dg,_DU)),B(_8D(E(_D6)[4],_DU)))),_q))));}else{var _DV=B(_1s(_vn,B(_n8(B(_DB(_q,_Dh,B(_8D(_Df,B(_C7(_CL,_DA)))),_Dg,E(_D6)[4])),_q))));}var _DW=_DV,_DX=_DW;return _DX;}),_Di];}else{return [0,_Cv,_zg];}},_DY=function(_DZ,_E0){while(1){var _E1=E(_E0);if(!_E1[0]){return true;}else{if(!B(A(_DZ,[_E1[1]]))){return false;}else{_E0=_E1[2];continue;}}}},_E2=function(_E3){return E(_E3)[1]%2==0?true:false;},_E4=new T(function(){return B(unCStr("roundTo: bad Value"));}),_E5=new T(function(){return B(err(_E4));}),_E6=function(_E7){return E(E(_E7)[1])==0?true:false;},_E8=function(_E9){return _E9>1?[1,_zg,new T(function(){return B(_E8(_E9-1|0));})]:E(_Cv);},_Ea=function(_Eb,_Ec,_Ed){var _Ee=function(_Ef,_Eg,_Eh){var _Ei=E(_Eh);if(!_Ei[0]){return [0,_zg,new T(function(){var _Ej=E(_Ef)[1];return _Ej>0?B(_E8(_Ej)):[0];})];}else{var _Ek=_Ei[1],_El=_Ei[2],_Em=E(E(_Ef)[1]);if(!_Em){var _En=E(_Ek)[1],_Eo=E(new T(function(){return [0,quot(E(_Eb)[1],2)];}))[1];return _En!=_Eo?[0,new T(function(){return _En<_Eo?E(_zg):E(_zb);}),_q]:!E(_Eg)?[0,new T(function(){return _En<_Eo?E(_zg):E(_zb);}),_q]:!B(_DY(_E6,_El))?[0,new T(function(){return _En<_Eo?E(_zg):E(_zb);}),_q]:[0,_zg,_q];}else{var _Ep=B(_Ee([0,_Em-1|0],new T(function(){return B(_E2(_Ek));}),_El)),_Eq=_Ep[2],_Er=E(_Ep[1])[1]+E(_Ek)[1]|0;return _Er!=E(_Eb)[1]?[0,_zg,[1,[0,_Er],_Eq]]:[0,_zb,[1,_zg,_Eq]];}}},_Es=B(_Ee(_Ec,_j,_Ed));switch(E(E(_Es[1])[1])){case 0:return E(_Es);case 1:return [0,_zb,[1,_zb,_Es[2]]];default:return E(_E5);}},_Et=function(_Eu,_Ev){var _Ew=E(_Eu);if(!_Ew){return [0,_q,_Ev];}else{var _Ex=E(_Ev);if(!_Ex[0]){return [0,_q,_q];}else{var _Ey=new T(function(){var _Ez=B(_Et(_Ew-1|0,_Ex[2]));return [0,_Ez[1],_Ez[2]];});return [0,[1,_Ex[1],new T(function(){return E(E(_Ey)[1]);})],new T(function(){return E(E(_Ey)[2]);})];}}},_EA=function(_EB){return E(E(_EB)[3]);},_EC=0,_ED=1,_EE=[0,10],_EF=new T(function(){return B(unCStr("e0"));}),_EG=function(_EH,_EI){var _EJ=E(_EH);if(!_EJ[0]){return E(_EF);}else{var _EK=_EJ[1];return _EI>1?[1,_EK,new T(function(){return B(_EG(_EJ[2],_EI-1|0));})]:[1,_EK,_EF];}},_EL=function(_EM,_EN){var _EO=E(_EN);return _EO[0]==0?[0]:[1,_EM,new T(function(){return B(_EL(_EO[1],_EO[2]));})];},_EP=new T(function(){return B(unCStr("init"));}),_EQ=new T(function(){return B(_kw(_EP));}),_ER=new T(function(){return B(_Av("GHC/Float.lhs:591:12-70|(d : ds\')"));}),_ES=[0,101],_ET=new T(function(){return B(unCStr("Infinity"));}),_EU=new T(function(){return B(unCStr("-Infinity"));}),_EV=new T(function(){return B(unCStr("NaN"));}),_EW=new T(function(){return B(unCStr("formatRealFloat/doFmt/FFExponent: []"));}),_EX=new T(function(){return B(err(_EW));}),_EY=new T(function(){return B(unCStr("0.0e0"));}),_EZ=function(_F0){return E(E(_F0)[4]);},_F1=new T(function(){return [1,_AJ,_F1];}),_F2=function(_F3,_F4,_F5,_F6,_F7,_F8,_F9,_Fa,_Fb,_Fc,_Fd,_Fe){if(!B(A(_F9,[_Fe]))){var _Ff=new T(function(){return B(_AF(new T(function(){return B(_AD(_F4));})));});if(!B(A(_Fa,[_Fe]))){var _Fg=function(_Fh,_Fi,_Fj){while(1){var _Fk=(function(_Fl,_Fm,_Fn){switch(E(_Fl)){case 0:var _Fo=E(_Fd);if(!_Fo[0]){var _Fp=B(_1s(_As,_Fm));if(!_Fp[0]){return E(_EX);}else{var _Fq=_Fp[2],_Fr=E(_Fp[1]),_Fs=function(_Ft){var _Fu=E(_Fq);return _Fu[0]==0?[1,_Fr,new T(function(){return B(unAppCStr(".0e",new T(function(){return B(_9R(0,E(_Fn)[1]-1|0,_q));})));})]:[1,_Fr,[1,_AL,new T(function(){return B(_K(_Fu,[1,_ES,new T(function(){return B(_9R(0,E(_Fn)[1]-1|0,_q));})]));})]];};return E(_Fr[1])==48?E(_Fq)[0]==0?E(_EY):B(_Fs(_)):B(_Fs(_));}}else{var _Fv=new T(function(){var _Fw=E(_Fo[1]);return _Fw[1]>1?E(_Fw):E(_zb);}),_Fx=function(_Fy){var _Fz=new T(function(){var _FA=B(_Ea(_EE,new T(function(){return [0,E(_Fv)[1]+1|0];}),_Fm));return [0,_FA[1],_FA[2]];}),_FB=new T(function(){return E(E(_Fz)[1]);}),_FC=new T(function(){if(E(_FB)[1]<=0){var _FD=B(_1s(_As,E(_Fz)[2])),_FE=_FD[0]==0?E(_ER):[0,_FD[1],_FD[2]];}else{var _FF=E(E(_Fz)[2]);if(!_FF[0]){var _FG=E(_EQ);}else{var _FH=B(_1s(_As,B(_EL(_FF[1],_FF[2])))),_FG=_FH[0]==0?E(_ER):[0,_FH[1],_FH[2]];}var _FI=_FG,_FE=_FI;}var _FJ=_FE,_FK=_FJ;return _FK;});return [1,new T(function(){return E(E(_FC)[1]);}),[1,_AL,new T(function(){return B(_K(E(_FC)[2],[1,_ES,new T(function(){return B(_9R(0,(E(_Fn)[1]-1|0)+E(_FB)[1]|0,_q));})]));})]];},_FL=E(_Fm);if(!_FL[0]){return new F(function(){return _Fx(_);});}else{return E(E(_FL[1])[1])==0?E(_FL[2])[0]==0?[1,_AJ,[1,_AL,new T(function(){var _FM=E(_Fv)[1];return _FM>0?B(_EG(_F1,_FM)):E(_EF);})]]:B(_Fx(_)):B(_Fx(_));}}break;case 1:var _FN=E(_Fd);if(!_FN[0]){var _FO=E(_Fn)[1];return _FO>0?B(_AM(_FO,_q,new T(function(){return B(_1s(_As,_Fm));}))):B(unAppCStr("0.",new T(function(){var _FP= -_FO;if(_FP>0){var _FQ=function(_FR){return _FR>1?[1,_AJ,new T(function(){return B(_FQ(_FR-1|0));})]:E([1,_AJ,new T(function(){return B(_1s(_As,_Fm));})]);},_FS=B(_FQ(_FP));}else{var _FS=B(_1s(_As,_Fm));}var _FT=_FS,_FU=_FT;return _FU;})));}else{var _FV=_FN[1],_FW=E(_Fn),_FX=_FW[1];if(_FX<0){var _FY=new T(function(){var _FZ= -_FX;if(_FZ>0){var _G0=function(_G1){return _G1>1?[1,_zg,new T(function(){return B(_G0(_G1-1|0));})]:E([1,_zg,_Fm]);},_G2=B(_Ea(_EE,new T(function(){var _G3=E(_FV);return _G3[1]>0?E(_G3):E(_zg);}),B(_G0(_FZ)))),_G4=B(_Ay(_G2[1],_G2[2]));}else{var _G5=B(_Ea(_EE,new T(function(){var _G6=E(_FV);return _G6[1]>0?E(_G6):E(_zg);}),_Fm)),_G4=B(_Ay(_G5[1],_G5[2]));}var _G7=_G4,_G8=_G7;return _G8;});return [1,new T(function(){return E(E(_FY)[1]);}),new T(function(){var _G9=E(E(_FY)[2]);return _G9[0]==0?[0]:[1,_AL,_G9];})];}else{var _Ga=B(_Ea(_EE,new T(function(){var _Gb=E(_FV)[1];if(_Gb>0){var _Gc=[0,_Gb+_FX|0];}else{var _Gc=E(_FW);}var _Gd=_Gc,_Ge=_Gd;return _Ge;}),_Fm)),_Gf=_Ga[2],_Gg=_FX+E(_Ga[1])[1]|0;if(_Gg>=0){var _Gh=B(_Et(_Gg,new T(function(){return B(_1s(_As,_Gf));}))),_Gi=_Gh[2],_Gj=E(_Gh[1]);return _Gj[0]==0?[1,_AJ,new T(function(){var _Gk=E(_Gi);return _Gk[0]==0?[0]:[1,_AL,_Gk];})]:B(_K(_Gj,new T(function(){var _Gl=E(_Gi);return _Gl[0]==0?[0]:[1,_AL,_Gl];})));}else{return [1,_AJ,new T(function(){var _Gm=B(_1s(_As,_Gf));return _Gm[0]==0?[0]:[1,_AL,_Gm];})];}}}break;default:var _Gn=E(_Fn),_Go=_Gn[1];if(_Go>=0){if(_Go<=7){_Fh=_ED;var _Gp=_Fm;_Fj=_Gn;_Fi=_Gp;return null;}else{_Fh=_EC;var _Gp=_Fm;_Fj=_Gn;_Fi=_Gp;return null;}}else{_Fh=_EC;var _Gp=_Fm;_Fj=_Gn;_Fi=_Gp;return null;}}})(_Fh,_Fi,_Fj);if(_Fk!=null){return _Fk;}}},_Gq=function(_Gr){return [1,_l8,new T(function(){var _Gs=B(_CE(E(E(E(E(_F3)[1])[2])[1])[1],_F4,_F5,_F6,_F7,_F8,_BP,new T(function(){return B(A(_EZ,[_Ff,_Fe]));})));return B(_Fg(_Fc,_Gs[1],_Gs[2]));})];};if(!B(A(_EA,[B(_wu(B(_AH(_F3)))),_Fe,new T(function(){return B(A(_wy,[_Ff,_nt]));})]))){if(!B(A(_Fb,[_Fe]))){var _Gt=B(_CE(E(E(E(E(_F3)[1])[2])[1])[1],_F4,_F5,_F6,_F7,_F8,_BP,_Fe));return new F(function(){return _Fg(_Fc,_Gt[1],_Gt[2]);});}else{return new F(function(){return _Gq(_);});}}else{return new F(function(){return _Gq(_);});}}else{return !B(A(_EA,[B(_wu(B(_AH(_F3)))),_Fe,new T(function(){return B(A(_wy,[_Ff,_nt]));})]))?E(_ET):E(_EU);}}else{return E(_EV);}},_Gu=function(_Gv){var _Gw=u_towlower(_Gv),_Gx=_Gw;return _Gx>>>0>1114111?B(_9X(_Gx)):_Gx;},_Gy=function(_Gz){return new F(function(){return err(B(unAppCStr("Printf.printf: ",_Gz)));});},_GA=new T(function(){return B(unCStr("bad argument"));}),_GB=new T(function(){return B(_Gy(_GA));}),_GC=new T(function(){return B(unCStr("Printf.dfmt: impossible"));}),_GD=new T(function(){return B(err(_GC));}),_GE=[0,45],_GF=[1,_GE,_q],_GG=new T(function(){return B(err(_GC));}),_GH=new T(function(){return B(unCStr("Negative exponent"));}),_GI=new T(function(){return B(err(_GH));}),_GJ=function(_GK,_GL,_GM){while(1){if(!(_GL%2)){var _GN=_GK*_GK,_GO=quot(_GL,2);_GK=_GN;_GL=_GO;continue;}else{var _GP=E(_GL);if(_GP==1){return _GK*_GM;}else{var _GN=_GK*_GK;_GL=quot(_GP-1|0,2);var _GQ=_GK*_GM;_GK=_GN;_GM=_GQ;continue;}}}},_GR=function(_GS,_GT){while(1){if(!(_GT%2)){var _GU=_GS*_GS,_GV=quot(_GT,2);_GS=_GU;_GT=_GV;continue;}else{var _GW=E(_GT);if(_GW==1){return E(_GS);}else{return new F(function(){return _GJ(_GS*_GS,quot(_GW-1|0,2),_GS);});}}}},_GX=function(_GY,_GZ){var _H0=E(_GY);return _H0[0]==0?function(_7w){return new F(function(){return _K(new T(function(){var _H1=jsShow(E(_GZ)[1]),_H2=_H1;return fromJSStr(_H2);}),_7w);});}:function(_7w){return new F(function(){return _K(new T(function(){var _H3=E(E(_H0[1])[1]);if(!_H3){var _H4=jsRound(E(_GZ)[1]),_H5=_H4,_H6=B(_q7(_H5)),_H7=_H6[1],_H8=_H6[2];if(_H8>=0){var _H9=jsShow(B(_nT(B(_so(_H7,_H8))))),_Ha=_H9,_Hb=fromJSStr(_Ha);}else{var _Hc=hs_uncheckedIShiftRA64(B(_sg(_H7)), -_H8),_Hd=_Hc,_He=jsShow(B(_nT(B(_rz(_Hd))))),_Hf=_He,_Hb=fromJSStr(_Hf);}var _Hg=_Hb,_Hh=_Hg,_Hi=_Hh,_Hj=_Hi;}else{if(_H3>=0){var _Hk=B(_GR(10,_H3)),_Hl=jsRound(E(_GZ)[1]*_Hk),_Hm=_Hl,_Hn=jsShow((_Hm&4294967295)/_Hk),_Ho=_Hn,_Hp=fromJSStr(_Ho);}else{var _Hp=E(_GI);}var _Hq=_Hp,_Hr=_Hq,_Hj=_Hr;}var _Hs=_Hj;return _Hs;}),_7w);});};},_Ht=function(_Hu,_Hv){var _Hw=E(_Hu);return _Hw[0]==0?function(_7w){return new F(function(){return _K(new T(function(){var _Hx=B(_yY(E(_Hv)[1])),_Hy=jsShow(B(_nH(_Hx[1],_Hx[2]))[1]),_Hz=_Hy;return fromJSStr(_Hz);}),_7w);});}:function(_7w){return new F(function(){return _K(new T(function(){var _HA=E(E(_Hw[1])[1]);if(!_HA){var _HB=jsRound(E(_Hv)[1]),_HC=_HB,_HD=decodeFloat(_HC),_HE=_HD[1],_HF=_HD[2];if(_HF>=0){var _HG=jsShow(B(_nT(B(_so(B(_8B(_HE)),_HF))))),_HH=_HG,_HI=fromJSStr(_HH);}else{var _HJ=jsShow(_HE>> -_HF),_HK=_HJ,_HI=fromJSStr(_HK);}var _HL=_HI,_HM=_HL,_HN=_HM,_HO=_HN;}else{var _HP=B(_yY(E(_Hv)[1]));if(_HA>=0){var _HQ=B(_GR(10,_HA)),_HR=jsRound(B(_nH(_HP[1],_HP[2]))[1]*_HQ),_HS=_HR,_HT=jsShow((_HS&4294967295)/_HQ),_HU=_HT,_HV=fromJSStr(_HU);}else{var _HV=E(_GI);}var _HW=_HV,_HX=_HW,_HY=_HX,_HZ=_HY,_HO=_HZ;}var _I0=_HO;return _I0;}),_7w);});};},_I1=function(_I2){var _I3=u_towupper(_I2),_I4=_I3;return _I4>>>0>1114111?B(_9X(_I4)):_I4;},_I5=function(_I6){return [0,B(_I1(E(_I6)[1]))];},_I7=function(_I8,_I9,_Ia){var _Ib=E(_Ia);switch(_Ib[0]){case 3:var _Ic=_Ib[1],_Id=u_iswupper(_I8),_Ie=_Id;switch(B(_Gu(_I8))){case 101:var _If=B(_F2(_Af,_q6,_qG,_qE,_qL,_qA,_qR,_qN,_qV,_EC,new T(function(){var _Ig=E(_I9);return _Ig[1]>=0?[1,_Ig]:[0];}),_Ic));break;case 102:var _If=B(_F2(_Af,_q6,_qG,_qE,_qL,_qA,_qR,_qN,_qV,_ED,new T(function(){var _Ih=E(_I9);return _Ih[1]>=0?[1,_Ih]:[0];}),_Ic));break;case 103:var _Ii=E(_I9),_If=_Ii[1]>=0?B(A(_Ht,[[1,_Ii],_Ic,_q])):B(A(_Ht,[_2x,_Ic,_q]));break;default:var _If=E(_GG);}var _Ij=_If,_Ik=E(_Ie);if(!_Ik){var _Il=E(_Ij);if(!_Il[0]){return [0,_q,_q];}else{var _Im=_Il[1],_In=_Il[2],_Io=E(_Im),_Ip=_Io[1],_Iq=E(_Ip);return _Iq==45?[0,_GF,_In]:[0,_q,_Il];}}else{var _Ir=B(_1s(_I5,_Ij));if(!_Ir[0]){return [0,_q,_q];}else{var _Is=_Ir[1],_It=_Ir[2],_Iu=E(_Is),_Iv=_Iu[1],_Iw=E(_Iv);return _Iw==45?[0,_GF,_It]:[0,_q,_Ir];}}break;case 4:var _Ix=_Ib[1],_Iy=u_iswupper(_I8),_Iz=_Iy;switch(B(_Gu(_I8))){case 101:var _IA=B(_F2(_yf,_oL,_qh,_qe,_qm,_qa,_qs,_qo,_qw,_EC,new T(function(){var _IB=E(_I9);return _IB[1]>=0?[1,_IB]:[0];}),_Ix));break;case 102:var _IA=B(_F2(_yf,_oL,_qh,_qe,_qm,_qa,_qs,_qo,_qw,_ED,new T(function(){var _IC=E(_I9);return _IC[1]>=0?[1,_IC]:[0];}),_Ix));break;case 103:var _ID=E(_I9),_IA=_ID[1]>=0?B(A(_GX,[[1,_ID],_Ix,_q])):B(A(_GX,[_2x,_Ix,_q]));break;default:var _IA=E(_GD);}var _IE=_IA,_IF=E(_Iz);if(!_IF){var _IG=E(_IE);if(!_IG[0]){return [0,_q,_q];}else{var _IH=_IG[1],_II=_IG[2],_IJ=E(_IH),_IK=_IJ[1],_IL=E(_IK);return _IL==45?[0,_GF,_II]:[0,_q,_IG];}}else{var _IM=B(_1s(_I5,_IE));if(!_IM[0]){return [0,_q,_q];}else{var _IN=_IM[1],_IO=_IM[2],_IP=E(_IN),_IQ=_IP[1],_IR=E(_IQ);return _IR==45?[0,_GF,_IO]:[0,_q,_IM];}}break;default:return E(_GB);}},_IS=function(_IT){return new F(function(){return _15(0,_IT,_q);});},_IU=function(_IV,_IW){while(1){var _IX=E(_IV);if(!_IX[0]){return E(_IW);}else{_IV=_IX[2];var _IY=_IW+1|0;_IW=_IY;continue;}}},_IZ=[0,48],_J0=function(_J1,_J2){var _J3=_J1-B(_IU(_J2,0))|0;if(_J3>0){var _J4=function(_J5){return _J5>1?[1,_IZ,new T(function(){return B(_J4(_J5-1|0));})]:E([1,_IZ,_J2]);};return new F(function(){return _J4(_J3);});}else{return E(_J2);}},_J6=[0,0],_J7=[0,-2147483648],_J8=function(_J9,_Ja){while(1){var _Jb=(function(_Jc,_Jd){var _Je=E(_Jd);switch(_Je[0]){case 0:_J9=_J6;_Ja=[2,_J7,new T(function(){return B(_8B(E(_Je[1])[1]));})];return null;case 2:var _Jf=_Je[2];return !B(_U(_Jf,_n3))?[0,_q,new T(function(){return B(_J0(E(_Jc)[1],B(_IS(_Jf))));})]:[0,_GF,new T(function(){return B(_J0(E(_Jc)[1],B(_15(0,B(_8v(_Jf)),_q))));})];default:return E(_GB);}})(_J9,_Ja);if(_Jb!=null){return _Jb;}}},_Jg=[1,_j6,_q],_Jh=function(_Ji){return new F(function(){return err(B(unAppCStr("Char.digitToInt: not a digit ",new T(function(){var _Jj=E(_Ji);return _Jj==39?E(_j8):[1,_j6,new T(function(){return B(_iQ(_Jj,_Jg));})];}))));});},_Jk=function(_Jl){var _Jm=function(_Jn){var _Jo=function(_Jp){if(_Jl<65){return new F(function(){return _Jh(_Jl);});}else{if(_Jl>70){return new F(function(){return _Jh(_Jl);});}else{return (_Jl-65|0)+10|0;}}};if(_Jl<97){return new F(function(){return _Jo(_);});}else{if(_Jl>102){return new F(function(){return _Jo(_);});}else{return (_Jl-97|0)+10|0;}}};if(_Jl<48){return new F(function(){return _Jm(_);});}else{if(_Jl>57){return new F(function(){return _Jm(_);});}else{return _Jl-48|0;}}},_Jq=function(_Jr,_Js){while(1){var _Jt=(function(_Ju,_Jv){var _Jw=E(_Jv);if(!_Jw[0]){return [0,_Ju,_q];}else{var _Jx=E(_Jw[1])[1];if(_Jx<48){return [0,_Ju,_Jw];}else{if(_Jx>57){return [0,_Ju,_Jw];}else{_Jr=new T(function(){return [0,(imul(E(_Ju)[1],10)|0)+B(_Jk(_Jx))|0];});_Js=_Jw[2];return null;}}}})(_Jr,_Js);if(_Jt!=null){return _Jt;}}},_Jy=new T(function(){return B(unCStr("argument list ended prematurely"));}),_Jz=new T(function(){return B(_Gy(_Jy));}),_JA=[0,-1],_JB=function(_JC){return [0,E(_JC)[1]];},_JD=function(_JE){var _JF=E(_JE);switch(_JF[0]){case 0:return new F(function(){return _JB(_JF[1]);});break;case 2:return new F(function(){return _vn(_JF[2]);});break;default:return E(_GB);}},_JG=function(_JH,_JI,_JJ,_JK,_JL){while(1){var _JM=(function(_JN,_JO,_JP,_JQ,_JR){var _JS=E(_JQ);if(!_JS[0]){return [0,_J6,_JA,_JN,_JO,_JP,_q,_JR];}else{var _JT=_JS[2],_JU=E(E(_JS[1])[1]);switch(_JU){case 42:var _JV=new T(function(){var _JW=E(_JR);return _JW[0]==0?E(_Jz):[0,_JW[2],new T(function(){return B(_JD(_JW[1]));})];}),_JX=new T(function(){var _JY=E(_JT);if(!_JY[0]){var _JZ=[0,_JA,_q,new T(function(){return E(E(_JV)[1]);})];}else{if(E(E(_JY[1])[1])==46){var _K0=E(_JY[2]);if(!_K0[0]){var _K1=B(_Jq(_J6,_q)),_K2=[0,_K1[1],_K1[2],new T(function(){return E(E(_JV)[1]);})];}else{if(E(E(_K0[1])[1])==42){var _K3=new T(function(){var _K4=E(E(_JV)[1]);return _K4[0]==0?E(_Jz):[0,_K4[2],new T(function(){return B(_JD(_K4[1]));})];}),_K5=[0,new T(function(){return E(E(_K3)[2]);}),_K0[2],new T(function(){return E(E(_K3)[1]);})];}else{var _K6=B(_Jq(_J6,_K0)),_K5=[0,_K6[1],_K6[2],new T(function(){return E(E(_JV)[1]);})];}var _K7=_K5,_K2=_K7;}var _K8=_K2;}else{var _K8=[0,_JA,_JY,new T(function(){return E(E(_JV)[1]);})];}var _K9=_K8,_JZ=_K9;}return _JZ;});return [0,new T(function(){return E(E(_JV)[2]);}),new T(function(){return E(E(_JX)[1]);}),_JN,_JO,_JP,new T(function(){return E(E(_JX)[2]);}),new T(function(){return E(E(_JX)[3]);})];case 43:var _Ka=_JN,_Kb=_JO;_JJ=_j;_JK=_JT;var _Kc=_JR;_JH=_Ka;_JI=_Kb;_JL=_Kc;return null;case 45:_JH=_j;var _Kb=_JO,_Kd=_JP;_JK=_JT;var _Kc=_JR;_JI=_Kb;_JJ=_Kd;_JL=_Kc;return null;case 46:var _Ke=new T(function(){var _Kf=E(_JT);if(!_Kf[0]){var _Kg=B(_Jq(_J6,_q)),_Kh=[0,_Kg[1],_Kg[2],_JR];}else{if(E(E(_Kf[1])[1])==42){var _Ki=new T(function(){var _Kj=E(_JR);return _Kj[0]==0?E(_Jz):[0,_Kj[2],new T(function(){return B(_JD(_Kj[1]));})];}),_Kk=[0,new T(function(){return E(E(_Ki)[2]);}),_Kf[2],new T(function(){return E(E(_Ki)[1]);})];}else{var _Kl=B(_Jq(_J6,_Kf)),_Kk=[0,_Kl[1],_Kl[2],_JR];}var _Km=_Kk,_Kh=_Km;}return _Kh;});return [0,_J6,new T(function(){return E(E(_Ke)[1]);}),_JN,_JO,_JP,new T(function(){return E(E(_Ke)[2]);}),new T(function(){return E(E(_Ke)[3]);})];case 48:var _Ka=_JN;_JI=_j;var _Kd=_JP;_JK=_JT;var _Kc=_JR;_JH=_Ka;_JJ=_Kd;_JL=_Kc;return null;default:if(_JU<48){return [0,_J6,_JA,_JN,_JO,_JP,_JS,_JR];}else{if(_JU>57){return [0,_J6,_JA,_JN,_JO,_JP,_JS,_JR];}else{var _Kn=new T(function(){var _Ko=B(_Jq(_J6,_JS));return [0,_Ko[1],_Ko[2]];}),_Kp=new T(function(){var _Kq=E(E(_Kn)[2]);if(!_Kq[0]){var _Kr=[0,_JA,_q,_JR];}else{if(E(E(_Kq[1])[1])==46){var _Ks=E(_Kq[2]);if(!_Ks[0]){var _Kt=B(_Jq(_J6,_q)),_Ku=[0,_Kt[1],_Kt[2],_JR];}else{if(E(E(_Ks[1])[1])==42){var _Kv=new T(function(){var _Kw=E(_JR);return _Kw[0]==0?E(_Jz):[0,_Kw[2],new T(function(){return B(_JD(_Kw[1]));})];}),_Kx=[0,new T(function(){return E(E(_Kv)[2]);}),_Ks[2],new T(function(){return E(E(_Kv)[1]);})];}else{var _Ky=B(_Jq(_J6,_Ks)),_Kx=[0,_Ky[1],_Ky[2],_JR];}var _Kz=_Kx,_Ku=_Kz;}var _KA=_Ku;}else{var _KA=[0,_JA,_Kq,_JR];}var _KB=_KA,_Kr=_KB;}var _KC=_Kr;return _KC;});return [0,new T(function(){return E(E(_Kn)[1]);}),new T(function(){return E(E(_Kp)[1]);}),_JN,_JO,_JP,new T(function(){return E(E(_Kp)[2]);}),new T(function(){return E(E(_Kp)[3]);})];}}}}})(_JH,_JI,_JJ,_JK,_JL);if(_JM!=null){return _JM;}}},_KD=new T(function(){return B(unCStr("formatting string ended prematurely"));}),_KE=new T(function(){return B(_Gy(_KD));}),_KF=function(_KG,_KH){if(!B(_U(_KH,_KG))){if(!B(_nz(_KG,_n3))){var _KI=B(_xk(_KH,_KG));return new F(function(){return _K(B(_KF(_KG,_KI[1])),[1,new T(function(){return [0,B(_Ao(B(_9Z(_KI[2]))))];}),_q]);});}else{return E(_u9);}}else{return [1,new T(function(){return [0,B(_Ao(B(_9Z(_KH))))];}),_q];}},_KJ=[0,2],_KK=function(_KL,_KM,_KN){var _KO=E(_KN);switch(_KO[0]){case 0:return new F(function(){return _KF(_KL,B(_8B(E(_KO[1])[1])));});break;case 2:var _KP=_KO[2],_KQ=E(_KM)[1];if(!B(_U(_KP,_n3))){return new F(function(){return _J0(_KQ,B(_KF(_KL,_KP)));});}else{return new F(function(){return _J0(_KQ,B(_KF(_KL,B(_8l(B(_8v(B(_8D(_KJ,_KO[1])))),_KP)))));});}break;default:return E(_GB);}},_KR=[0,37],_KS=[0,16],_KT=[0,10],_KU=[0,8],_KV=[0,43],_KW=[1,_KV,_q],_KX=[0,32],_KY=function(_KZ){return new F(function(){return _Gy(new T(function(){return B(unAppCStr("bad formatting char ",[1,_KZ,_q]));}));});},_L0=function(_L1,_L2){var _L3=E(_L1);if(!_L3){return [0];}else{var _L4=E(_L2);return _L4[0]==0?[0]:[1,_L4[1],new T(function(){return B(_L0(_L3-1|0,_L4[2]));})];}},_L5=function(_L6,_L7){var _L8=E(_L6);if(!_L8[0]){return E(_L7)[0]==0?[0]:E(_KE);}else{var _L9=_L8[2],_La=E(_L8[1]);if(E(_La[1])==37){var _Lb=function(_Lc){var _Ld=E(_L7);if(!_Ld[0]){return E(_Jz);}else{var _Le=B(_JG(_n,_n,_n,_L9,_Ld)),_Lf=_Le[2],_Lg=_Le[4],_Lh=E(_Le[6]);if(!_Lh[0]){return E(_KE);}else{var _Li=_Lh[2],_Lj=E(_Le[7]);if(!_Lj[0]){return E(_Jz);}else{var _Lk=_Lj[1],_Ll=_Lj[2],_Lm=E(_Lh[1]),_Ln=function(_Lo,_Lp){var _Lq=new T(function(){var _Lr=B(_IU(_Lp,0)),_Ls=B(_IU(_Lo,0)),_Lt=E(_Le[1])[1];if((_Lr+_Ls|0)>=_Lt){var _Lu=[0];}else{var _Lv=_Lt-(_Lr+_Ls|0)|0;if(_Lv>0){if(_Lv<0){var _Lw=[0];}else{var _Lx=new T(function(){return [1,new T(function(){return !E(_Lg)?E(_KX):E(_IZ);}),_Lx];}),_Lw=B(_L0(_Lv,_Lx));}var _Ly=_Lw,_Lz=_Ly;}else{var _Lz=[0];}var _LA=_Lz,_LB=_LA,_LC=_LB,_Lu=_LC;}var _LD=_Lu,_LE=_LD,_LF=_LE,_LG=_LF,_LH=_LG;return _LH;});return !E(_Le[3])?!E(_Lg)?B(_K(_Lq,new T(function(){return B(_K(_Lo,_Lp));}))):B(_K(_Lo,new T(function(){return B(_K(_Lq,_Lp));}))):B(_K(_Lo,new T(function(){return B(_K(_Lp,_Lq));})));},_LI=function(_LJ,_LK){var _LL=E(_LJ);return _LL[0]==0?!E(_Le[5])?B(_Ln(_q,_LK)):B(_Ln(_KW,_LK)):B(_Ln(_LL,_LK));};switch(E(_Lm[1])){case 69:var _LM=B(_I7(69,_Lf,_Lk));return new F(function(){return _K(B(_LI(_LM[1],_LM[2])),new T(function(){return B(_L5(_Li,_Ll));}));});break;case 71:var _LN=B(_I7(71,_Lf,_Lk));return new F(function(){return _K(B(_LI(_LN[1],_LN[2])),new T(function(){return B(_L5(_Li,_Ll));}));});break;case 88:return new F(function(){return _K(B(_Ln(_q,new T(function(){return B(_1s(_I5,B(_KK(_KS,_Lf,_Lk))));}))),new T(function(){return B(_L5(_Li,_Ll));}));});break;case 99:return new F(function(){return _K(B(_Ln(_q,[1,new T(function(){var _LO=E(_Lk);switch(_LO[0]){case 0:var _LP=E(_LO[1])[1];if(_LP>>>0>1114111){var _LQ=B(_9X(_LP));}else{var _LQ=[0,_LP];}var _LR=_LQ,_LS=_LR,_LT=_LS,_LU=_LT,_LV=_LU;break;case 2:var _LW=B(_9Z(_LO[2]));if(_LW>>>0>1114111){var _LX=B(_9X(_LW));}else{var _LX=[0,_LW];}var _LY=_LX,_LZ=_LY,_M0=_LZ,_LV=_M0;break;default:var _LV=E(_GB);}return _LV;}),_q])),new T(function(){return B(_L5(_Li,_Ll));}));});break;case 100:var _M1=B(_J8(_Lf,_Lk));return new F(function(){return _K(B(_LI(_M1[1],_M1[2])),new T(function(){return B(_L5(_Li,_Ll));}));});break;case 101:var _M2=B(_I7(101,_Lf,_Lk));return new F(function(){return _K(B(_LI(_M2[1],_M2[2])),new T(function(){return B(_L5(_Li,_Ll));}));});break;case 102:var _M3=B(_I7(102,_Lf,_Lk));return new F(function(){return _K(B(_LI(_M3[1],_M3[2])),new T(function(){return B(_L5(_Li,_Ll));}));});break;case 103:var _M4=B(_I7(103,_Lf,_Lk));return new F(function(){return _K(B(_LI(_M4[1],_M4[2])),new T(function(){return B(_L5(_Li,_Ll));}));});break;case 105:var _M5=B(_J8(_Lf,_Lk));return new F(function(){return _K(B(_LI(_M5[1],_M5[2])),new T(function(){return B(_L5(_Li,_Ll));}));});break;case 111:return new F(function(){return _K(B(_Ln(_q,new T(function(){return B(_KK(_KU,_Lf,_Lk));}))),new T(function(){return B(_L5(_Li,_Ll));}));});break;case 115:return new F(function(){return _K(B(_Ln(_q,new T(function(){var _M6=E(_Lk);if(_M6[0]==1){var _M7=_M6[1],_M8=E(_Lf)[1];if(_M8<0){var _M9=E(_M7);}else{var _M9=_M8>0?B(_L0(_M8,_M7)):[0];}var _Ma=_M9,_Mb=_Ma,_Mc=_Mb;}else{var _Mc=E(_GB);}return _Mc;}))),new T(function(){return B(_L5(_Li,_Ll));}));});break;case 117:return new F(function(){return _K(B(_Ln(_q,new T(function(){return B(_KK(_KT,_Lf,_Lk));}))),new T(function(){return B(_L5(_Li,_Ll));}));});break;case 120:return new F(function(){return _K(B(_Ln(_q,new T(function(){return B(_KK(_KS,_Lf,_Lk));}))),new T(function(){return B(_L5(_Li,_Ll));}));});break;default:return new F(function(){return _KY(_Lm);});}}}}},_Md=E(_L9);if(!_Md[0]){return new F(function(){return _Lb(_);});}else{if(E(E(_Md[1])[1])==37){return [1,_KR,new T(function(){return B(_L5(_Md[2],_L7));})];}else{return new F(function(){return _Lb(_);});}}}else{return [1,_La,new T(function(){return B(_L5(_L9,_L7));})];}}},_Me=new T(function(){return B(unCStr(" could be found!"));}),_Mf=function(_Mg){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_K(_Mg,_Me));}))));});},_Mh=function(_Mi,_){var _Mj=E(_n6),_Mk=jsFind(toJSStr(_Mj)),_Ml=_Mk,_Mm=E(_Ml);if(!_Mm[0]){return new F(function(){return _Mf(_Mj);});}else{var _Mn=B(_mg(_)),_Mo=_Mn,_Mp=jsSet(E(_Mm[1])[1],toJSStr(E(_n4)),toJSStr(B(_1s(_mN,B(_L5(_n5,new T(function(){return B(_n8([1,[1,new T(function(){return B(_1s(_mN,_Mi));})],[1,[2,_n3,_Mo],_q]],_q));}))))))),_Mq=jsSetTimeout(5000,function(_){var _Mr=jsEval(toJSStr(B(unAppCStr("$(\'#alert-",new T(function(){return B(_K(B(_15(0,_Mo,_q)),_n7));}))))),_Ms=_Mr;return _77;});return _77;}},_Mt=new T(function(){return B(unCStr("\u5b9f\u7e3e\u7372\u5f97: "));}),_Mu=new T(function(){return B(unCStr("\u4f9d\u5b58\u5ea6\u304c"));}),_Mv=new T(function(){return B(unCStr("\u3092\u8d85\u3048\u308b"));}),_Mw=function(_Mx,_My,_Mz){return function(_MA,_){var _MB=E(_MA),_MC=E(_MB[3]);if(_MC[1]<=E(new T(function(){return [0,B(_nT(_Mx))];}))[1]){return [0,_77,_MB];}else{var _MD=B(_Mh(new T(function(){var _ME=function(_MF){var _MG=E(_MF);return _MG[0]==0?E(_My):[1,_MG[1],new T(function(){return B(_ME(_MG[2]));})];};return B(_ME(_Mt));}),_)),_MH=_MD;return new F(function(){return _MI([0,_MB[1],_MB[2],_MC,_MB[4],_MB[5],new T(function(){return B(_40(E(_Mz)[1],[1,new T(function(){return B(_K(_Mu,new T(function(){return B(_K(B(_15(0,_Mx,_q)),_Mv));})));})],_MB[6]));}),_MB[7],_MB[8]],_);});}};},_MJ=new T(function(){return B(unCStr("\u4f9d\u5b58\u6ce8\u610f\u5831"));}),_MK=[0,100],_ML=function(_MM){return new F(function(){return _Mw(_MK,_MJ,_MM);});},_MN=new T(function(){return [0,_MJ,_ML];}),_MO=new T(function(){return B(unCStr("\u4f9d\u5b58\u30c9\u30e9\u30c3\u30b0"));}),_MP=[0,1000],_MQ=function(_MM){return new F(function(){return _Mw(_MP,_MO,_MM);});},_MR=new T(function(){return [0,_MO,_MQ];}),_MS=new T(function(){return B(unCStr("\u597d\u611f\u5ea6\u304c"));}),_MT=function(_MU,_MV,_MW){return function(_MX,_){var _MY=E(_MX),_MZ=E(_MY[2]);if(_MZ[1]<=E(new T(function(){return [0,B(_nT(_MU))];}))[1]){return [0,_77,_MY];}else{var _N0=B(_Mh(new T(function(){var _N1=function(_N2){var _N3=E(_N2);return _N3[0]==0?E(_MV):[1,_N3[1],new T(function(){return B(_N1(_N3[2]));})];};return B(_N1(_Mt));}),_)),_N4=_N0;return new F(function(){return _MI([0,_MY[1],_MZ,_MY[3],_MY[4],_MY[5],new T(function(){return B(_40(E(_MW)[1],[1,new T(function(){return B(_K(_MS,new T(function(){return B(_K(B(_15(0,_MU,_q)),_Mv));})));})],_MY[6]));}),_MY[7],_MY[8]],_);});}};},_N5=new T(function(){return B(unCStr("\u307e\u305a\u306f\u304a\u53cb\u9054\u304b\u3089"));}),_N6=[0,10],_N7=function(_MM){return new F(function(){return _MT(_N6,_N5,_MM);});},_N8=new T(function(){return [0,_N5,_N7];}),_N9=new T(function(){return B(unCStr("\u4e00\u7d1a\u30d5\u30e9\u30b0\u5efa\u7bc9\u58eb"));}),_Na=function(_MM){return new F(function(){return _MT(_MK,_N9,_MM);});},_Nb=new T(function(){return [0,_N9,_Na];}),_Nc=new T(function(){return B(unCStr("\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_Nd=[0,500],_Ne=function(_MM){return new F(function(){return _MT(_Nd,_Nc,_MM);});},_Nf=new T(function(){return [0,_Nc,_Ne];}),_Ng=new T(function(){return B(unCStr("\uff8a\uff72\uff8a\uff9f\uff70\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_Nh=function(_MM){return new F(function(){return _MT(_MP,_Ng,_MM);});},_Ni=new T(function(){return [0,_Ng,_Nh];}),_Nj=new T(function(){return B(unCStr("\u5168\u958b\u3089\u3076\u3071\u308f\u30fc"));}),_Nk=[0,10000],_Nl=function(_MM){return new F(function(){return _MT(_Nk,_Nj,_MM);});},_Nm=new T(function(){return [0,_Nj,_Nl];}),_Nn=new T(function(){return B(unCStr("\u611b\u60c5\u304c"));}),_No=function(_Np,_Nq,_Nr){return function(_Ns,_){var _Nt=E(_Ns),_Nu=E(_Nt[1]);if(_Nu[1]<=E(new T(function(){return [0,B(_nT(_Np))];}))[1]){return [0,_77,_Nt];}else{var _Nv=B(_Mh(new T(function(){var _Nw=function(_Nx){var _Ny=E(_Nx);return _Ny[0]==0?E(_Nq):[1,_Ny[1],new T(function(){return B(_Nw(_Ny[2]));})];};return B(_Nw(_Mt));}),_)),_Nz=_Nv;return new F(function(){return _MI([0,_Nu,_Nt[2],_Nt[3],_Nt[4],_Nt[5],new T(function(){return B(_40(E(_Nr)[1],[1,new T(function(){return B(_K(_Nn,new T(function(){return B(_K(B(_15(0,_Np,_q)),_Mv));})));})],_Nt[6]));}),_Nt[7],_Nt[8]],_);});}};},_NA=new T(function(){return B(unCStr("\u30a2\u30a4\u3068\u306e\u906d\u9047"));}),_NB=function(_MM){return new F(function(){return _No(_MK,_NA,_MM);});},_NC=new T(function(){return [0,_NA,_NB];}),_ND=new T(function(){return B(unCStr("\u611b\u3055\u308c\u6c17\u5206"));}),_NE=function(_MM){return new F(function(){return _No(_Nk,_ND,_MM);});},_NF=new T(function(){return [0,_ND,_NE];}),_NG=new T(function(){return B(unCStr("\u611b\u30e9\u30d6\u30e6\u30fc"));}),_NH=[0,1000000],_NI=function(_MM){return new F(function(){return _No(_NH,_NG,_MM);});},_NJ=new T(function(){return [0,_NG,_NI];}),_NK=new T(function(){return B(unCStr("\u611b\u3055\u3093\u306e\u611b\u304c\u91cd\u3044"));}),_NL=[0,100000000],_NM=function(_MM){return new F(function(){return _No(_NL,_NK,_MM);});},_NN=new T(function(){return [0,_NK,_NM];}),_NO=new T(function(){return B(unCStr("\u30b3\u30f3\u30d7\u30ea\u30fc\u30c8"));}),_NP=function(_NQ,_NR){while(1){var _NS=(function(_NT,_NU){var _NV=E(_NU);switch(_NV[0]){case 0:_NQ=new T(function(){return B(_NP(_NT,_NV[4]));});_NR=_NV[3];return null;case 1:return [1,[0,_NV[1]],_NT];default:return E(_NT);}})(_NQ,_NR);if(_NS!=null){return _NS;}}},_NW=function(_NX){var _NY=E(_NX);if(!_NY[0]){var _NZ=_NY[3],_O0=_NY[4];return _NY[2]>=0?B(_NP(new T(function(){return B(_NP(_q,_O0));}),_NZ)):B(_NP(new T(function(){return B(_NP(_q,_NZ));}),_O0));}else{return new F(function(){return _NP(_q,_NY);});}},_O1=function(_O2,_O3){while(1){var _O4=E(_O3);switch(_O4[0]){case 0:var _O5=_O4[3],_O6=B(_O1(_O2,_O4[4]));if(_O6[0]==2){_O3=_O5;continue;}else{var _O7=B(_O1(_O2,_O5));return _O7[0]==2?E(_O6):[0,_O4[1],_O4[2],E(_O7),E(_O6)];}break;case 1:return !B(A(_O2,[[0,_O4[1]],_O4[2]]))?[2]:E(_O4);default:return [2];}}},_O8=function(_O9,_Oa){return E(_O9)[1]>0;},_Ob=function(_Oc,_Od,_){return [0,_77,new T(function(){var _Oe=E(_Od);return [0,_Oe[1],new T(function(){return [0,E(_Oe[2])[1]+500];}),_Oe[3],_Oe[4],_Oe[5],_Oe[6],_Oe[7],_Oe[8]];})];},_Of=[0,0],_Og=[0,3],_Oh=[0,2],_Oi=new T(function(){return B(_nH(_Og,_Oh));}),_Oj=[0,-1],_Ok=function(_Ol,_Om){if(_Om>=0){var _On=function(_Oo){var _Op=B(_q7(_Ol*_Oo)),_Oq=_Op[1],_Or=_Op[2];if(_Or>=0){return new F(function(){return _so(_Oq,_Or);});}else{var _Os= -_Or;if(_Os<=52){var _Ot=hs_uncheckedIShiftRA64(B(_sg(_Oq)),_Os),_Ou=_Ot;return new F(function(){return _rz(_Ou);});}else{return !B(_U(_Oq,_Of))?E(_Of):E(_Oj);}}},_Ov=E(_Om);if(!_Ov){return new F(function(){return _On(1);});}else{return new F(function(){return _On(B(_GR(E(_Oi)[1],_Ov)));});}}else{return E(_GI);}},_Ow=function(_Ox){return new F(function(){return _Ok(10000000,E(_Ox)[1]);});},_Oy=new T(function(){return B(unCStr("\u5bb6<br>\u597d\u611f\u5ea6 +500.0"));}),_Oz=new T(function(){return B(unCStr("fa-home"));}),_OA=[0,_Oz,_Oy,_q],_OB=[0,_Ow,_Ob,_OA],_OC=new T(function(){return B(unCStr("\u8eca<br>\u597d\u611f\u5ea6 +100"));}),_OD=new T(function(){return B(unCStr("fa-car"));}),_OE=[0,_OD,_OC,_q],_OF=function(_OG,_OH,_){return [0,_77,new T(function(){var _OI=E(_OH);return [0,_OI[1],new T(function(){return [0,E(_OI[2])[1]+100];}),_OI[3],_OI[4],_OI[5],_OI[6],_OI[7],_OI[8]];})];},_OJ=function(_OK){return new F(function(){return _Ok(500000,E(_OK)[1]);});},_OL=[0,_OJ,_OF,_OE],_OM=new T(function(){return B(unCStr("\u65c5\u884c<br>\u597d\u611f\u5ea6 +40"));}),_ON=new T(function(){return B(unCStr("fa-plane"));}),_OO=[0,_ON,_OM,_q],_OP=function(_OQ,_OR,_){return [0,_77,new T(function(){var _OS=E(_OR);return [0,_OS[1],new T(function(){return [0,E(_OS[2])[1]+40];}),_OS[3],_OS[4],_OS[5],_OS[6],_OS[7],_OS[8]];})];},_OT=function(_OU){return new F(function(){return _Ok(20000,E(_OU)[1]);});},_OV=[0,_OT,_OP,_OO],_OW=new T(function(){return B(unCStr("\u55ab\u8336\u5e97<br>\u597d\u611f\u5ea6 +10"));}),_OX=new T(function(){return B(unCStr("fa-coffee"));}),_OY=[0,_OX,_OW,_q],_OZ=function(_P0,_P1,_){return [0,_77,new T(function(){var _P2=E(_P1);return [0,_P2[1],new T(function(){return [0,E(_P2[2])[1]+10];}),_P2[3],_P2[4],_P2[5],_P2[6],_P2[7],_P2[8]];})];},_P3=function(_P4){return new F(function(){return _Ok(1000,E(_P4)[1]);});},_P5=[0,_P3,_OZ,_OY],_P6=new T(function(){return B(unCStr("\u30e1\u30fc\u30eb<br>\u597d\u611f\u5ea6 +1.0"));}),_P7=new T(function(){return B(unCStr("fa-envelope"));}),_P8=[0,_P7,_P6,_q],_P9=function(_Pa,_Pb,_){return [0,_77,new T(function(){var _Pc=E(_Pb);return [0,_Pc[1],new T(function(){return [0,E(_Pc[2])[1]+1];}),_Pc[3],_Pc[4],_Pc[5],_Pc[6],_Pc[7],_Pc[8]];})];},_Pd=function(_Pe){return new F(function(){return _Ok(50,E(_Pe)[1]);});},_Pf=[0,_Pd,_P9,_P8],_Pg=new T(function(){return B(unCStr("\u4f1a\u8a71<br>\u597d\u611f\u5ea6 +0.2"));}),_Ph=new T(function(){return B(unCStr("fa-comments-o"));}),_Pi=[0,_Ph,_Pg,_q],_Pj=function(_Pk,_Pl,_){return [0,_77,new T(function(){var _Pm=E(_Pl);return [0,_Pm[1],new T(function(){return [0,E(_Pm[2])[1]+0.2];}),_Pm[3],_Pm[4],_Pm[5],_Pm[6],_Pm[7],_Pm[8]];})];},_Pn=function(_Po){return new F(function(){return _Ok(1,E(_Po)[1]);});},_Pp=[0,_Pn,_Pj,_Pi],_Pq=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u30b7\u30e7\u30c3\u30d7"));}),_Pr=new T(function(){return B(unCStr("\u30a2\u30a4\u30c6\u30e0\u30b7\u30e7\u30c3\u30d7<br>\u30a2\u30a4\u30c6\u30e0\u304c\u8cfc\u5165\u3067\u304d\u308b\u3088\u3046\u306b\u306a\u308a\u307e\u3059\u3002"));}),_Ps=new T(function(){return B(unCStr("fa-shopping-cart"));}),_Pt=[0,_Ps,_Pr,_Pq],_Pu=function(_Pv,_Pw,_Px,_Py){return new F(function(){return A(_Pv,[function(_){var _Pz=jsSetStyle(E(_Pw)[1],toJSStr(E(_Px)),toJSStr(E(_Py)));return _77;}]);});},_PA=function(_PB,_){var _PC=B(_mg(_)),_PD=_PC;return [0,_77,new T(function(){var _PE=E(_PB);return [0,_PE[1],_PE[2],_PE[3],_PD,_PE[5],_PE[6],_PE[7],_PE[8]];})];},_PF=new T(function(){return B(unCStr("block"));}),_PG=new T(function(){return B(unCStr("display"));}),_PH=function(_PI,_){return [0,_77,_PI];},_PJ=function(_PK,_PL,_){var _PM=B(A(_PK,[_])),_PN=_PM;return new F(function(){return A(_PL,[_PN,_]);});},_PO=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_PP=new T(function(){return B(unCStr("base"));}),_PQ=new T(function(){return B(unCStr("IOException"));}),_PR=new T(function(){var _PS=hs_wordToWord64(4053623282),_PT=_PS,_PU=hs_wordToWord64(3693590983),_PV=_PU;return [0,_PT,_PV,[0,_PT,_PV,_PP,_PO,_PQ],_q];}),_PW=function(_PX){return E(_PR);},_PY=function(_PZ){var _Q0=E(_PZ);return new F(function(){return _4L(B(_4J(_Q0[1])),_PW,_Q0[2]);});},_Q1=new T(function(){return B(unCStr(": "));}),_Q2=[0,41],_Q3=new T(function(){return B(unCStr(" ("));}),_Q4=new T(function(){return B(unCStr("already exists"));}),_Q5=new T(function(){return B(unCStr("does not exist"));}),_Q6=new T(function(){return B(unCStr("protocol error"));}),_Q7=new T(function(){return B(unCStr("failed"));}),_Q8=new T(function(){return B(unCStr("invalid argument"));}),_Q9=new T(function(){return B(unCStr("inappropriate type"));}),_Qa=new T(function(){return B(unCStr("hardware fault"));}),_Qb=new T(function(){return B(unCStr("unsupported operation"));}),_Qc=new T(function(){return B(unCStr("timeout"));}),_Qd=new T(function(){return B(unCStr("resource vanished"));}),_Qe=new T(function(){return B(unCStr("interrupted"));}),_Qf=new T(function(){return B(unCStr("resource busy"));}),_Qg=new T(function(){return B(unCStr("resource exhausted"));}),_Qh=new T(function(){return B(unCStr("end of file"));}),_Qi=new T(function(){return B(unCStr("illegal operation"));}),_Qj=new T(function(){return B(unCStr("permission denied"));}),_Qk=new T(function(){return B(unCStr("user error"));}),_Ql=new T(function(){return B(unCStr("unsatisified constraints"));}),_Qm=new T(function(){return B(unCStr("system error"));}),_Qn=function(_Qo,_Qp){switch(E(_Qo)){case 0:return new F(function(){return _K(_Q4,_Qp);});break;case 1:return new F(function(){return _K(_Q5,_Qp);});break;case 2:return new F(function(){return _K(_Qf,_Qp);});break;case 3:return new F(function(){return _K(_Qg,_Qp);});break;case 4:return new F(function(){return _K(_Qh,_Qp);});break;case 5:return new F(function(){return _K(_Qi,_Qp);});break;case 6:return new F(function(){return _K(_Qj,_Qp);});break;case 7:return new F(function(){return _K(_Qk,_Qp);});break;case 8:return new F(function(){return _K(_Ql,_Qp);});break;case 9:return new F(function(){return _K(_Qm,_Qp);});break;case 10:return new F(function(){return _K(_Q6,_Qp);});break;case 11:return new F(function(){return _K(_Q7,_Qp);});break;case 12:return new F(function(){return _K(_Q8,_Qp);});break;case 13:return new F(function(){return _K(_Q9,_Qp);});break;case 14:return new F(function(){return _K(_Qa,_Qp);});break;case 15:return new F(function(){return _K(_Qb,_Qp);});break;case 16:return new F(function(){return _K(_Qc,_Qp);});break;case 17:return new F(function(){return _K(_Qd,_Qp);});break;default:return new F(function(){return _K(_Qe,_Qp);});}},_Qq=[0,125],_Qr=new T(function(){return B(unCStr("{handle: "));}),_Qs=function(_Qt,_Qu,_Qv,_Qw,_Qx,_Qy){var _Qz=new T(function(){var _QA=new T(function(){return B(_Qn(_Qu,new T(function(){var _QB=E(_Qw);return _QB[0]==0?E(_Qy):B(_K(_Q3,new T(function(){return B(_K(_QB,[1,_Q2,_Qy]));})));})));}),_QC=E(_Qv);return _QC[0]==0?E(_QA):B(_K(_QC,new T(function(){return B(_K(_Q1,_QA));})));}),_QD=E(_Qx);if(!_QD[0]){var _QE=E(_Qt);if(!_QE[0]){return E(_Qz);}else{var _QF=E(_QE[1]);return _QF[0]==0?B(_K(_Qr,new T(function(){return B(_K(_QF[1],[1,_Qq,new T(function(){return B(_K(_Q1,_Qz));})]));}))):B(_K(_Qr,new T(function(){return B(_K(_QF[1],[1,_Qq,new T(function(){return B(_K(_Q1,_Qz));})]));})));}}else{return new F(function(){return _K(_QD[1],new T(function(){return B(_K(_Q1,_Qz));}));});}},_QG=function(_QH){var _QI=E(_QH);return new F(function(){return _Qs(_QI[1],_QI[2],_QI[3],_QI[4],_QI[6],_q);});},_QJ=function(_QK,_QL){var _QM=E(_QK);return new F(function(){return _Qs(_QM[1],_QM[2],_QM[3],_QM[4],_QM[6],_QL);});},_QN=function(_QO,_QP){return new F(function(){return _56(_QJ,_QO,_QP);});},_QQ=function(_QR,_QS,_QT){var _QU=E(_QS);return new F(function(){return _Qs(_QU[1],_QU[2],_QU[3],_QU[4],_QU[6],_QT);});},_QV=[0,_QQ,_QG,_QN],_QW=new T(function(){return [0,_PW,_QV,_QX,_PY];}),_QX=function(_QY){return [0,_QW,_QY];},_QZ=7,_R0=function(_R1){return [0,_2x,_QZ,_q,_R1,_2x,_2x];},_R2=function(_R3,_){return new F(function(){return die(new T(function(){return B(_QX(new T(function(){return B(_R0(_R3));})));}));});},_R4=function(_R5,_){return new F(function(){return _R2(_R5,_);});},_R6=function(_R7,_){return _R7;},_R8=function(_R9,_Ra,_){var _Rb=B(A(_R9,[_])),_Rc=_Rb;return new F(function(){return A(_Ra,[_]);});},_Rd=[0,_PJ,_R8,_R6,_R4],_Re=[0,_Rd,_7L],_Rf=function(_Rg){return E(E(_Rg)[1]);},_Rh=function(_Ri){return E(E(_Ri)[1]);},_Rj=function(_Rk){return E(E(_Rk)[2]);},_Rl=function(_Rm){return E(E(_Rm)[3]);},_Rn=function(_Ro,_Rp){var _Rq=new T(function(){return B(_Rf(_Ro));});return function(_Rr){return new F(function(){return A(new T(function(){return B(_Rh(_Rq));}),[new T(function(){return B(A(_Rj,[_Ro,_Rp]));}),function(_Rs){return new F(function(){return A(new T(function(){return B(_Rl(_Rq));}),[[0,_Rs,_Rr]]);});}]);});};},_Rt=function(_Ru){return new F(function(){return _Rn(_Re,_Ru);});},_Rv=new T(function(){return B(unCStr("monitor"));}),_Rw=function(_Rx){return function(_Ry,_){var _Rz=E(_Rx),_RA=jsFind(toJSStr(_Rz)),_RB=_RA,_RC=E(_RB);if(!_RC[0]){return new F(function(){return _Mf(_Rz);});}else{var _RD=B(A(_Pu,[_Rt,_RC[1],_PG,_PF,_Ry,_])),_RE=_RD;return new F(function(){return A(new T(function(){return !B(_6y(_Rx,_Rv))?E(_PH):E(_PA);}),[new T(function(){return E(E(_RE)[2]);}),_]);});}};},_RF=new T(function(){return B(unCStr("item-shop"));}),_RG=new T(function(){return B(_Rw(_RF));}),_RH=function(_RI){return E(_RG);},_RJ=[0,1],_RK=function(_RL){return E(_RJ);},_RM=[0,_RK,_RH,_Pt],_RN=new T(function(){return B(unCStr("\u3055\u3041\u59cb\u3081\u3088\u3046"));}),_RO=new T(function(){return B(unCStr("\u3055\u3041\u59cb\u3081\u3088\u3046<br>\u30b2\u30fc\u30e0\u3092\u59cb\u3081\u307e\u3057\u3087\u3046\u3002\u53f3\u306e\u30dc\u30bf\u30f3\u304b\u3089\u3053\u306e\u30a2\u30a4\u30c6\u30e0\u3092\u8cfc\u5165\u3057\u3066\u304f\u3060\u3055\u3044\u3002"));}),_RP=new T(function(){return B(unCStr("fa-power-off"));}),_RQ=[0,_RP,_RO,_RN],_RR=new T(function(){return B(_Rw(_Rv));}),_RS=function(_RT){return E(_RR);},_RU=function(_RV){return E(_Of);},_RW=[0,_RU,_RS,_RQ],_RX=new T(function(){return B(unCStr("\u30c7\u30fc\u30bf\u306e\u6d88\u53bb"));}),_RY=new T(function(){return B(unCStr("\u30c7\u30fc\u30bf\u306e\u6d88\u53bb<br>\u5168\u3066\u306e\u30c7\u30fc\u30bf\u304c\u6d88\u53bb\u3055\u308c\u307e\u3059\u3002\u3053\u306e\u64cd\u4f5c\u306f\u53d6\u308a\u6d88\u305b\u307e\u305b\u3093\u3002"));}),_RZ=new T(function(){return B(unCStr("fa-trash"));}),_S0=[0,_RZ,_RY,_RX],_S1=function(_S2){return E(_MK);},_S3=new T(function(){return B(unCStr("none"));}),_S4=[0,0],_S5=function(_S6,_S7,_){var _S8=B(_mg(_)),_S9=_S8,_Sa=B(_MI([0,_S4,_S4,_S4,_S9,_n,_3Z,_3Z,_S4],_)),_Sb=_Sa,_Sc=E(_Rv),_Sd=jsFind(toJSStr(_Sc)),_Se=_Sd,_Sf=E(_Se);if(!_Sf[0]){return new F(function(){return _Mf(_Sc);});}else{var _Sg=B(A(_Pu,[_Rt,_Sf[1],_PG,_S3,new T(function(){return E(E(_Sb)[2]);}),_])),_Sh=_Sg,_Si=E(_RF),_Sj=jsFind(toJSStr(_Si)),_Sk=_Sj,_Sl=E(_Sk);return _Sl[0]==0?B(_Mf(_Si)):B(A(_Pu,[_Rt,_Sl[1],_PG,_S3,new T(function(){return E(E(_Sh)[2]);}),_]));}},_Sm=new T(function(){return [0,_S1,_S5,_S0];}),_Sn=new T(function(){return B(unCStr("\u521d\u671f\u5316"));}),_So=new T(function(){return B(unCStr("\u521d\u671f\u5316<br>\u5b9f\u7e3e\u3092\u9664\u304f\u5168\u3066\u306e\u30c7\u30fc\u30bf\u304c\u521d\u671f\u5316\u3055\u308c\u307e\u3059"));}),_Sp=new T(function(){return B(unCStr("fa-history"));}),_Sq=[0,_Sp,_So,_Sn],_Sr=function(_Ss){return E(_N6);},_St=function(_Su,_Sv,_){var _Sw=B(_mg(_)),_Sx=_Sw,_Sy=B(_MI([0,_S4,_S4,_S4,_Sx,_n,new T(function(){return E(E(_Sv)[6]);}),_3Z,_S4],_)),_Sz=_Sy,_SA=E(_Rv),_SB=jsFind(toJSStr(_SA)),_SC=_SB,_SD=E(_SC);if(!_SD[0]){return new F(function(){return _Mf(_SA);});}else{var _SE=B(A(_Pu,[_Rt,_SD[1],_PG,_S3,new T(function(){return E(E(_Sz)[2]);}),_])),_SF=_SE,_SG=E(_RF),_SH=jsFind(toJSStr(_SG)),_SI=_SH,_SJ=E(_SI);return _SJ[0]==0?B(_Mf(_SG)):B(A(_Pu,[_Rt,_SJ[1],_PG,_S3,new T(function(){return E(E(_SF)[2]);}),_]));}},_SK=new T(function(){return [0,_Sr,_St,_Sq];}),_SL=new T(function(){return B(_sX(-1,-2));}),_SM=new T(function(){var _SN=E(_SL);return _SN[0]==0?[0]:[1,[0,_SN[1],_RW],new T(function(){var _SO=E(_SN[2]);return _SO[0]==0?[0]:[1,[0,_SO[1],_RM],new T(function(){var _SP=E(_SO[2]);return _SP[0]==0?[0]:[1,[0,_SP[1],_SK],new T(function(){var _SQ=E(_SP[2]);return _SQ[0]==0?[0]:[1,[0,_SQ[1],_Sm],_q];})];})];})];}),_SR=new T(function(){var _SS=B(_mW(1,2147483647));return _SS[0]==0?E(_SM):[1,[0,_SS[1],_Pp],new T(function(){var _ST=E(_SS[2]);return _ST[0]==0?E(_SM):[1,[0,_ST[1],_Pf],new T(function(){var _SU=E(_ST[2]);return _SU[0]==0?E(_SM):[1,[0,_SU[1],_P5],new T(function(){var _SV=E(_SU[2]);return _SV[0]==0?E(_SM):[1,[0,_SV[1],_OV],new T(function(){var _SW=E(_SV[2]);return _SW[0]==0?E(_SM):[1,[0,_SW[1],_OL],new T(function(){var _SX=E(_SW[2]);return _SX[0]==0?E(_SM):[1,[0,_SX[1],_OB],_SM];})];})];})];})];})];}),_SY=new T(function(){return B(_4r(_3Z,_SR));}),_SZ=new T(function(){return B(_O1(_O8,_SY));}),_T0=new T(function(){return B(_NW(_SZ));}),_T1=new T(function(){return B(unCStr("\u500b\u4ee5\u4e0a\u624b\u306b\u5165\u308c\u308b"));}),_T2=new T(function(){return B(unCStr("\u5168\u3066\u306e\u901a\u5e38\u30a2\u30a4\u30c6\u30e0\u3092"));}),_T3=function(_T4,_T5,_T6){return function(_T7,_){var _T8=new T(function(){return E(E(_T7)[7]);});if(!B(_DY(function(_T9){var _Ta=E(_T9)[1];return !B(_ma(_Ta,_T8))?false:B(_m3(_T8,_Ta))[1]>=E(_T4)[1];},_T0))){return [0,_77,_T7];}else{var _Tb=B(_Mh(new T(function(){var _Tc=function(_Td){var _Te=E(_Td);return _Te[0]==0?E(_T5):[1,_Te[1],new T(function(){return B(_Tc(_Te[2]));})];};return B(_Tc(_Mt));}),_)),_Tf=_Tb;return new F(function(){return _MI(new T(function(){var _Tg=E(_T7);return [0,_Tg[1],_Tg[2],_Tg[3],_Tg[4],_Tg[5],new T(function(){return B(_40(E(_T6)[1],[1,new T(function(){return B(_K(_T2,new T(function(){return B(_K(B(_9R(0,E(_T4)[1],_q)),_T1));})));})],_Tg[6]));}),_Tg[7],_Tg[8]];}),_);});}};},_Th=function(_MM){return new F(function(){return _T3(_mM,_NO,_MM);});},_Ti=new T(function(){return [0,_NO,_Th];}),_Tj=new T(function(){return B(unCStr("\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30b3\u30f3\u30d7\u30ea\u30fc\u30c8"));}),_Tk=[0,10],_Tl=function(_MM){return new F(function(){return _T3(_Tk,_Tj,_MM);});},_Tm=new T(function(){return [0,_Tj,_Tl];}),_Tn=new T(function(){var _To=B(_mW(1,2147483647));return _To[0]==0?[0]:[1,[0,_To[1],_NC],new T(function(){var _Tp=E(_To[2]);return _Tp[0]==0?[0]:[1,[0,_Tp[1],_NF],new T(function(){var _Tq=E(_Tp[2]);return _Tq[0]==0?[0]:[1,[0,_Tq[1],_NJ],new T(function(){var _Tr=E(_Tq[2]);return _Tr[0]==0?[0]:[1,[0,_Tr[1],_NN],new T(function(){var _Ts=E(_Tr[2]);return _Ts[0]==0?[0]:[1,[0,_Ts[1],_N8],new T(function(){var _Tt=E(_Ts[2]);return _Tt[0]==0?[0]:[1,[0,_Tt[1],_Nb],new T(function(){var _Tu=E(_Tt[2]);return _Tu[0]==0?[0]:[1,[0,_Tu[1],_Nf],new T(function(){var _Tv=E(_Tu[2]);return _Tv[0]==0?[0]:[1,[0,_Tv[1],_Ni],new T(function(){var _Tw=E(_Tv[2]);return _Tw[0]==0?[0]:[1,[0,_Tw[1],_Nm],new T(function(){var _Tx=E(_Tw[2]);return _Tx[0]==0?[0]:[1,[0,_Tx[1],_MN],new T(function(){var _Ty=E(_Tx[2]);return _Ty[0]==0?[0]:[1,[0,_Ty[1],_MR],new T(function(){var _Tz=E(_Ty[2]);return _Tz[0]==0?[0]:[1,[0,_Tz[1],_Ti],new T(function(){var _TA=E(_Tz[2]);return _TA[0]==0?[0]:[1,[0,_TA[1],_Tm],_q];})];})];})];})];})];})];})];})];})];})];})];})];}),_TB=new T(function(){return B(unCStr("Aichan"));}),_TC=new T(function(){return [0,toJSStr(_q)];}),_TD=[0,93],_TE=[1,_TD,_q],_TF=new T(function(){return [0,toJSStr(_TE)];}),_TG=[0,125],_TH=[1,_TG,_q],_TI=new T(function(){return [0,toJSStr(_TH)];}),_TJ=[0,58],_TK=[1,_TJ,_q],_TL=new T(function(){return [0,toJSStr(_TK)];}),_TM=[0,44],_TN=[1,_TM,_q],_TO=new T(function(){return [0,toJSStr(_TN)];}),_TP=new T(function(){return [0,"false"];}),_TQ=function(_TR){var _TS=jsShow(E(_TR)[1]),_TT=_TS;return [0,_TT];},_TU=function(_TV){var _TW=jsStringify(E(_TV)[1]),_TX=_TW;return [0,_TX];},_TY=new T(function(){return [0,"null"];}),_TZ=[0,91],_U0=[1,_TZ,_q],_U1=new T(function(){return [0,toJSStr(_U0)];}),_U2=[0,123],_U3=[1,_U2,_q],_U4=new T(function(){return [0,toJSStr(_U3)];}),_U5=[0,34],_U6=[1,_U5,_q],_U7=new T(function(){return [0,toJSStr(_U6)];}),_U8=new T(function(){return [0,"true"];}),_U9=function(_Ua,_Ub){var _Uc=E(_Ub);switch(_Uc[0]){case 0:return [0,new T(function(){return B(_TQ(_Uc[1]));}),_Ua];case 1:return [0,new T(function(){return B(_TU(_Uc[1]));}),_Ua];case 2:return !E(_Uc[1])?[0,_TP,_Ua]:[0,_U8,_Ua];case 3:var _Ud=E(_Uc[1]);return _Ud[0]==0?[0,_U1,[1,_TF,_Ua]]:[0,_U1,new T(function(){var _Ue=B(_U9(new T(function(){var _Uf=function(_Ug){var _Uh=E(_Ug);return _Uh[0]==0?E([1,_TF,_Ua]):[1,_TO,new T(function(){var _Ui=B(_U9(new T(function(){return B(_Uf(_Uh[2]));}),_Uh[1]));return [1,_Ui[1],_Ui[2]];})];};return B(_Uf(_Ud[2]));}),_Ud[1]));return [1,_Ue[1],_Ue[2]];})];case 4:var _Uj=E(_Uc[1]);if(!_Uj[0]){return [0,_U4,[1,_TI,_Ua]];}else{var _Uk=E(_Uj[1]);return [0,_U4,[1,new T(function(){return B(_TU(_Uk[1]));}),[1,_TL,new T(function(){var _Ul=B(_U9(new T(function(){var _Um=function(_Un){var _Uo=E(_Un);if(!_Uo[0]){return E([1,_TI,_Ua]);}else{var _Up=E(_Uo[1]);return [1,_TO,[1,_U7,[1,_Up[1],[1,_U7,[1,_TL,new T(function(){var _Uq=B(_U9(new T(function(){return B(_Um(_Uo[2]));}),_Up[2]));return [1,_Uq[1],_Uq[2]];})]]]]];}};return B(_Um(_Uj[2]));}),_Uk[2]));return [1,_Ul[1],_Ul[2]];})]]];}break;default:return [0,_TY,_Ua];}},_Ur=function(_Us){var _Ut=jsCat(new T(function(){var _Uu=B(_U9(_q,_Us));return [1,_Uu[1],_Uu[2]];}),E(_TC)[1]),_Uv=_Ut;return E(_Uv);},_Uw=function(_Ux){var _Uy=B(A(_Ux,[_])),_Uz=_Uy;return E(_Uz);},_UA=function(_UB){return new F(function(){return _Uw(function(_){var _=0;return new F(function(){return eval(_UB);});});});},_UC=new T(function(){return [0,"(function(k,v) {localStorage.setItem(k,v);})"];}),_UD=function(_UE,_UF){return function(_UG,_){var _UH=B(A(new T(function(){return B(A(_UA,[E(_UC)[1],E(toJSStr(E(_UF)))]));}),[E(B(_Ur(B(A(new T(function(){return B(_2d(_UE));}),[_UG]))))),_])),_UI=_UH;return _77;};},_UJ=new T(function(){return B(_UD(_hr,_TB));}),_UK=new T(function(){return B(unCStr("<tr><td>%s</td><td>%s</td></tr>"));}),_UL=new T(function(){return B(unCStr("<thead><tr><th>\u5b9f\u7e3e\u540d</th><th>\u5185\u5bb9</th></tr></thead>"));}),_UM=new T(function(){return B(unCStr("</tbody>"));}),_MI=function(_UN,_){var _UO=B(A(_UJ,[_UN,_])),_UP=_UO,_UQ=jsFind(toJSStr(E(_a))),_UR=_UQ,_US=E(_UR);if(!_US[0]){return new F(function(){return _R4(_mV,_);});}else{var _UT=jsSet(E(_US[1])[1],toJSStr(E(_n4)),toJSStr(B(_K(_UL,new T(function(){return B(unAppCStr("<tbody>",new T(function(){var _UU=function(_UV){var _UW=E(_UV);if(!_UW[0]){return [0];}else{var _UX=E(_UW[1]),_UY=function(_UZ){var _V0=E(_UZ);return _V0[0]==0?E(new T(function(){return B(_UU(_UW[2]));})):[1,_V0[1],new T(function(){return B(_UY(_V0[2]));})];};return new F(function(){return _UY(B(_L5(_UK,new T(function(){return B(_n8([1,[1,new T(function(){return B(_1s(_mN,_UX[2]));})],[1,[1,new T(function(){return B(_1s(_mN,_UX[1]));})],_q]],_q));}))));});}};return B(_K(B(_UU(B(_1s(function(_V1){var _V2=E(_V1),_V3=E(_V2[2])[1],_V4=B(_mP(E(_V2[1])[1],new T(function(){return E(E(_UN)[6]);})));if(!_V4[0]){return [0,_V3,_q];}else{var _V5=E(_V4[1]);return _V5[0]==0?[0,_V3,_q]:[0,_V3,_V5[1]];}},_Tn)))),_UM));})));})))));return [0,_77,_UN];}},_V6=new T(function(){return [0,"click"];}),_V7=new T(function(){return B(_5O("main.hs:(371,1)-(387,24)|function btnEvents"));}),_V8=function(_V9,_Va,_Vb){return new F(function(){return _vd(_Va,_Vb);});},_Vc=new T(function(){return B(unCStr("-btn"));}),_Vd=function(_Ve,_Vf,_){var _Vg=E(_Vf);if(!_Vg[0]){return E(_V7);}else{var _Vh=E(_Vg[1]),_Vi=_Vh[1],_Vj=function(_,_Vk){var _Vl=E(_Vk);if(!_Vl[0]){return _77;}else{var _Vm=E(_V6)[1],_Vn=jsSetCB(E(_Vl[1])[1],_Vm,function(_Vo,_Vp,_){var _Vq=E(_Ve)[1],_Vr=rMV(_Vq),_Vs=_Vr,_Vt=E(new T(function(){return B(_m3(_SY,_Vi));})),_Vu=B(A(_Vt[2],[_Vh,new T(function(){var _Vv=E(_Vs),_Vw=new T(function(){return B(_mk(_V8,_Vi,_mM,_Vv[7]));});return [0,new T(function(){return [0,E(_Vv[1])[1]-B(_nT(B(A(_Vt[1],[new T(function(){return [0,B(_m3(_Vw,_Vi))[1]-1|0];})]))))];}),_Vv[2],_Vv[3],_Vv[4],_Vv[5],_Vv[6],_Vw,_Vv[8]];}),_])),_Vx=_Vu,_Vy=B(_MI(new T(function(){return E(E(_Vx)[2]);}),_)),_Vz=_Vy,_=wMV(_Vq,new T(function(){return E(E(_Vz)[2]);})),_VA=rMV(_Vq),_VB=_VA,_VC=E(_VB),_VD=jsLog(toJSStr(B(A(_ly,[0,_VC[1],_VC[2],_VC[3],_VC[4],_VC[5],_VC[6],_VC[7],_VC[8],_q]))));return _77;}),_VE=_Vn,_VF=function(_VG,_VH,_){var _VI=E(_VH);if(!_VI[0]){return E(_V7);}else{var _VJ=E(_VI[1]),_VK=_VJ[1],_VL=function(_,_VM){var _VN=E(_VM);if(!_VN[0]){return _77;}else{var _VO=jsSetCB(E(_VN[1])[1],_Vm,function(_VP,_VQ,_){var _VR=E(_VG)[1],_VS=rMV(_VR),_VT=_VS,_VU=E(new T(function(){return B(_m3(_SY,_VK));})),_VV=B(A(_VU[2],[_VJ,new T(function(){var _VW=E(_VT),_VX=new T(function(){return B(_mk(_V8,_VK,_mM,_VW[7]));});return [0,new T(function(){return [0,E(_VW[1])[1]-B(_nT(B(A(_VU[1],[new T(function(){return [0,B(_m3(_VX,_VK))[1]-1|0];})]))))];}),_VW[2],_VW[3],_VW[4],_VW[5],_VW[6],_VX,_VW[8]];}),_])),_VY=_VV,_VZ=B(_MI(new T(function(){return E(E(_VY)[2]);}),_)),_W0=_VZ,_=wMV(_VR,new T(function(){return E(E(_W0)[2]);})),_W1=rMV(_VR),_W2=_W1,_W3=E(_W2),_W4=jsLog(toJSStr(B(A(_ly,[0,_W3[1],_W3[2],_W3[3],_W3[4],_W3[5],_W3[6],_W3[7],_W3[8],_q]))));return _77;}),_W5=_VO;return new F(function(){return _VF(_VG,_VI[2],_);});}};if(_VK<=0){var _W6=jsFind(toJSStr(B(unAppCStr("item-sp-",new T(function(){if(_VK<0){var _W7=B(_K(B(_9R(0, -_VK,_q)),_Vc));}else{var _W7=B(_K(B(_9R(0,_VK,_q)),_Vc));}var _W8=_W7;return _W8;}))))),_W9=_W6;return new F(function(){return _VL(_,_W9);});}else{var _Wa=jsFind(toJSStr(B(unAppCStr("item-",new T(function(){return B(_K(B(_9R(0,_VK,_q)),_Vc));}))))),_Wb=_Wa;return new F(function(){return _VL(_,_Wb);});}}};return new F(function(){return _VF(_Ve,_Vg[2],_);});}};if(_Vi<=0){var _Wc=jsFind(toJSStr(B(unAppCStr("item-sp-",new T(function(){if(_Vi<0){var _Wd=B(_K(B(_9R(0, -_Vi,_q)),_Vc));}else{var _Wd=B(_K(B(_9R(0,_Vi,_q)),_Vc));}var _We=_Wd;return _We;}))))),_Wf=_Wc;return new F(function(){return _Vj(_,_Wf);});}else{var _Wg=jsFind(toJSStr(B(unAppCStr("item-",new T(function(){return B(_K(B(_9R(0,_Vi,_q)),_Vc));}))))),_Wh=_Wg;return new F(function(){return _Vj(_,_Wh);});}}},_Wi=function(_Wj){var _Wk=E(_Wj);if(!_Wk[0]){return [0,_q,_q];}else{var _Wl=E(_Wk[1]),_Wm=new T(function(){var _Wn=B(_Wi(_Wk[2]));return [0,_Wn[1],_Wn[2]];});return E(_Wl[1])[1]<=0?[0,new T(function(){return E(E(_Wm)[1]);}),[1,_Wl,new T(function(){return E(E(_Wm)[2]);})]]:[0,[1,_Wl,new T(function(){return E(E(_Wm)[1]);})],new T(function(){return E(E(_Wm)[2]);})];}},_Wo=new T(function(){var _Wp=B(_Wi(_SR));return [0,_Wp[1],_Wp[2]];}),_Wq=new T(function(){return E(E(_Wo)[1]);}),_Wr=function(_){return _77;},_Ws=new T(function(){return E(E(_Wo)[2]);}),_Wt=function(_Wu){return _Wu>0;},_Wv=new T(function(){return B(_UA("(function(x) {return x === null;})"));}),_Ww=new T(function(){return B(unCStr("No such value"));}),_Wx=[0,_Ww],_Wy=new T(function(){return B(unCStr("Invalid JSON!"));}),_Wz=[0,_Wy],_WA=new T(function(){return [0,"(function(k) {return localStorage.getItem(k);})"];}),_WB=function(_WC,_WD,_){var _WE=B(A(_UA,[E(_WA)[1],E(toJSStr(E(_WD))),_])),_WF=_WE;return new T(function(){if(!B(_Uw(function(_){var _=0,_WG=B(A(_Wv,[E(_WF),_])),_WH=_WG;return new T(function(){return B(_Wt(_WH));});}))){var _WI=String(_WF),_WJ=_WI,_WK=jsParseJSON(_WJ),_WL=_WK,_WM=E(_WL),_WN=_WM[0]==0?E(_Wz):B(A(_2N,[_WC,_WM[1]]));}else{var _WN=E(_Wx);}return _WN;});},_WO=[0,10],_WP=[1,_WO,_q],_WQ=function(_WR,_WS,_){var _WT=jsWriteHandle(E(_WR)[1],toJSStr(E(_WS)));return _77;},_WU=function(_WV,_WW,_){var _WX=E(_WV),_WY=jsWriteHandle(_WX[1],toJSStr(E(_WW)));return new F(function(){return _WQ(_WX,_WP,_);});},_WZ=function(_X0,_X1,_){var _X2=jsCreateTextNode(toJSStr(E(_X0))),_X3=_X2,_X4=jsAppendChild(_X3,E(_X1)[1]);return [0,_X3];},_X5=function(_X6,_X7,_X8,_X9){return new F(function(){return A(_X6,[function(_){var _Xa=jsSetAttr(E(_X7)[1],toJSStr(E(_X8)),toJSStr(E(_X9)));return _77;}]);});},_Xb=function(_Xc,_Xd,_Xe,_Xf){return new F(function(){return A(_Xc,[function(_){var _Xg=jsSet(E(_Xd)[1],toJSStr(E(_Xe)),toJSStr(E(_Xf)));return _77;}]);});},_Xh=new T(function(){return B(unCStr("count"));}),_Xi=new T(function(){return B(unCStr("tip"));}),_Xj=new T(function(){return B(unCStr("list-group-item tooltips"));}),_Xk=new T(function(){return B(unCStr(" loves"));}),_Xl=new T(function(){return B(unCStr("fa fa-plus-circle"));}),_Xm=[0,32],_Xn=[1,_Xm,_q],_Xo=new T(function(){return B(unCStr("btn btn-default btn-buy"));}),_Xp=new T(function(){return B(unCStr("class"));}),_Xq=new T(function(){return B(unCStr("button"));}),_Xr=new T(function(){return B(unCStr("type"));}),_Xs=new T(function(){return B(unCStr("item-list"));}),_Xt=new T(function(){return B(unCStr("button"));}),_Xu=function(_Xv,_Xw,_Xx,_){var _Xy=jsCreateElem(toJSStr(E(_Xt))),_Xz=_Xy,_XA=jsAppendChild(_Xz,E(_Xx)[1]),_XB=[0,_Xz],_XC=B(A(_Xv,[_Xw,_XB,_])),_XD=_XC;return _XB;},_XE=[0,105],_XF=[1,_XE,_q],_XG=function(_XH,_XI,_XJ,_){var _XK=jsCreateElem(toJSStr(_XF)),_XL=_XK,_XM=jsAppendChild(_XL,E(_XJ)[1]),_XN=[0,_XL],_XO=B(A(_XH,[_XI,_XN,_])),_XP=_XO;return _XN;},_XQ=new T(function(){return B(unCStr("id"));}),_XR=new T(function(){return B(unCStr("li"));}),_XS=function(_XT,_XU,_XV,_){var _XW=jsCreateElem(toJSStr(E(_XR))),_XX=_XW,_XY=jsAppendChild(_XX,E(_XV)[1]),_XZ=[0,_XX],_Y0=B(A(_XT,[_XU,_XZ,_])),_Y1=_Y0;return _XZ;},_Y2=[0,48],_Y3=[1,_Y2,_q],_Y4=new T(function(){return B(unCStr("id"));}),_Y5=new T(function(){return B(unCStr("-icon"));}),_Y6=new T(function(){return B(unCStr("-num"));}),_Y7=new T(function(){return B(unCStr("-box"));}),_Y8=new T(function(){return B(unCStr("-cost"));}),_Y9=new T(function(){return B(unCStr("innerHTML"));}),_Ya=new T(function(){return B(unCStr("span"));}),_Yb=function(_Yc,_Yd,_Ye,_){var _Yf=jsCreateElem(toJSStr(E(_Ya))),_Yg=_Yf,_Yh=jsAppendChild(_Yg,E(_Ye)[1]),_Yi=[0,_Yg],_Yj=B(A(_Yc,[_Yd,_Yi,_])),_Yk=_Yj;return _Yi;},_Yl=function(_Ym){return E(_Ym);},_Yn=function(_Yo,_Yp,_Yq,_Yr){var _Ys=new T(function(){return B(unAppCStr("item-",new T(function(){var _Yt=E(_Yo)[1];return _Yt<=0?B(unAppCStr("sp-",new T(function(){if(_Yt<0){var _Yu=B(_9R(0, -_Yt,_q));}else{var _Yu=B(_9R(0,_Yt,_q));}var _Yv=_Yu;return _Yv;}))):B(_9R(0,_Yt,_q));})));});return function(_Yw,_){var _Yx=B(_XS(_Yl,function(_Yy,_){var _Yz=B(_Yb(_Yl,function(_YA,_){var _YB=B(A(_Xb,[_7L,_YA,_Y9,_Yq,_])),_YC=_YB;return _YA;},_Yy,_)),_YD=_Yz,_YE=B(A(_X5,[_7L,_YD,_Xp,_Xi,_])),_YF=_YE,_YG=B(_Yb(_Yl,function(_YH,_){var _YI=B(_Yb(_Yl,function(_YJ,_){var _YK=B(_XG(_WZ,_q,_YJ,_)),_YL=_YK,_YM=B(A(_X5,[_7L,_YL,_Xp,new T(function(){return B(unAppCStr("fa ",_Yp));}),_])),_YN=_YM,_YO=B(_WZ(_Xn,_YJ,_)),_YP=_YO;return _YJ;},_YH,_)),_YQ=_YI,_YR=B(A(_X5,[_7L,_YQ,_Y4,new T(function(){return B(_K(_Ys,_Y5));}),_])),_YS=_YR,_YT=B(_Yb(_WZ,_q,_YH,_)),_YU=_YT,_YV=B(A(_X5,[_7L,_YU,_Y4,new T(function(){return B(_K(_Ys,_Y6));}),_])),_YW=_YV;return _YH;},_Yy,_)),_YX=_YG,_YY=B(A(_X5,[_7L,_YX,_Xp,_Xh,_])),_YZ=_YY,_Z0=B(_Yb(_WZ,_Yr,_Yy,_)),_Z1=_Z0,_Z2=B(A(_X5,[_7L,_Z1,_Y4,new T(function(){return B(_K(_Ys,_Y7));}),_])),_Z3=_Z2,_Z4=B(A(_X5,[_7L,_Z1,_Xp,_Xs,_])),_Z5=_Z4,_Z6=B(_Xu(_Yl,function(_Z7,_){var _Z8=B(_XG(_WZ,_q,_Z7,_)),_Z9=_Z8,_Za=B(A(_X5,[_7L,_Z9,_Xp,_Xl,_])),_Zb=_Za,_Zc=B(_WZ(_Xn,_Z7,_)),_Zd=_Zc,_Ze=B(_Yb(_WZ,_Y3,_Z7,_)),_Zf=_Ze,_Zg=B(A(_X5,[_7L,_Zf,_Y4,new T(function(){return B(_K(_Ys,_Y8));}),_])),_Zh=_Zg,_Zi=B(_WZ(_Xk,_Z7,_)),_Zj=_Zi;return _Z7;},_Yy,_)),_Zk=_Z6,_Zl=B(A(_X5,[_7L,_Zk,_Xr,_Xq,_])),_Zm=_Zl,_Zn=B(A(_X5,[_7L,_Zk,_Y4,new T(function(){return B(_K(_Ys,_Vc));}),_])),_Zo=_Zn,_Zp=B(A(_X5,[_7L,_Zk,_Xp,_Xo,_])),_Zq=_Zp;return _Yy;},_Yw,_)),_Zr=_Yx,_Zs=B(A(_X5,[_7L,_Zr,_XQ,_Ys,_])),_Zt=_Zs,_Zu=B(A(_X5,[_7L,_Zr,_Xp,_Xj,_])),_Zv=_Zu;return _Zr;};},_Zw=new T(function(){return B(unCStr("\u30bb\u30fc\u30d6\u3057\u307e\u3057\u305f"));}),_Zx=function(_Zy,_){var _Zz=B(_MI(_Zy,_)),_ZA=_Zz,_ZB=B(_Mh(_Zw,_)),_ZC=_ZB;return [0,_ZC,new T(function(){return E(E(_ZA)[2]);})];},_ZD=[1,_jc,_q],_ZE=new T(function(){return B(unCStr("%.2f"));}),_ZF=function(_ZG,_){var _ZH=jsEval(toJSStr(B(unAppCStr("document.title = ",[1,_jc,new T(function(){return B(_je(B(_1s(_mN,B(_L5(_ZE,new T(function(){return B(_n8([1,[4,new T(function(){return E(E(_ZG)[1]);})],_q],_q));}))))),_ZD));})])))),_ZI=_ZH;return [0,_77,_ZG];},_ZJ=function(_ZK,_ZL,_ZM){while(1){var _ZN=(function(_ZO,_ZP,_ZQ){var _ZR=E(_ZQ);if(!_ZR[0]){return [0,_ZO,_ZP];}else{var _ZS=_ZR[1];_ZK=new T(function(){var _ZT=E(E(_ZS)[1]);switch(_ZT){case -1:var _ZU=[0,0];break;case 0:var _ZU=E(_u9);break;default:var _ZU=[0,B(_uD(E(_ZO)[1],_ZT))];}var _ZV=_ZU;return _ZV;});var _ZW=[1,new T(function(){return [0,B(_ud(E(_ZO)[1],E(_ZS)[1]))];}),_ZP];_ZM=_ZR[2];_ZL=_ZW;return null;}})(_ZK,_ZL,_ZM);if(_ZN!=null){return _ZN;}}},_ZX=function(_ZY,_ZZ,_100,_101){return new F(function(){return _ZJ(new T(function(){var _102=E(E(_100)[1]);switch(_102){case -1:var _103=[0,0];break;case 0:var _103=E(_u9);break;default:var _103=[0,B(_uD(E(_ZY)[1],_102))];}var _104=_103;return _104;}),[1,new T(function(){return [0,B(_ud(E(_ZY)[1],E(_100)[1]))];}),_ZZ],_101);});},_105=function(_106,_107){var _108=E(_106);if(!_108[0]){return [0];}else{var _109=_108[1];return _107>1?[1,_109,new T(function(){return B(_105(_108[2],_107-1|0));})]:[1,_109,_q];}},_10a=new T(function(){return B(_15(0,_Of,_q));}),_10b=new T(function(){return B(_15(0,_Oj,_q));}),_10c=function(_10d,_10e){var _10f=E(_10e);if(!_10f[0]){return [0,_q,_q];}else{var _10g=_10f[1];if(!B(A(_10d,[_10g]))){var _10h=new T(function(){var _10i=B(_10c(_10d,_10f[2]));return [0,_10i[1],_10i[2]];});return [0,[1,_10g,new T(function(){return E(E(_10h)[1]);})],new T(function(){return E(E(_10h)[2]);})];}else{return [0,_q,_10f];}}},_10j=function(_10k,_10l){var _10m=function(_10n,_10o){return !B(_6y(_10o,_q))?[0,_10n,new T(function(){var _10p=B(_10j(_10k,_10o));return [1,_10p[1],_10p[2]];})]:[0,_10n,_q];};if(_10k>=0){var _10q=B(_Et(_10k,_10l));return new F(function(){return _10m(_10q[1],_10q[2]);});}else{return new F(function(){return _10m(_q,_10l);});}},_10r=function(_10s){var _10t=E(_10s);if(!_10t[0]){return [0];}else{return new F(function(){return _K(_10t[1],new T(function(){return B(_10r(_10t[2]));}));});}},_10u=[0,44],_10v=[1,_10u,_q],_10w=function(_10x){return E(E(_10x)[1])==46?true:false;},_10y=function(_10z,_10A){var _10B=E(_10A);return _10B[0]==0?[0]:[1,_10z,[1,_10B[1],new T(function(){return B(_10y(_10z,_10B[2]));})]];},_10C=function(_10D){var _10E=new T(function(){var _10F=B(_10c(_10w,_10D));return [0,_10F[1],_10F[2]];}),_10G=B(_10j(3,new T(function(){return B(_n8(E(_10E)[1],_q));})));return new F(function(){return _K(B(_n8(B(_10r([1,_10G[1],new T(function(){return B(_10y(_10v,_10G[2]));})])),_q)),new T(function(){return E(E(_10E)[2]);}));});},_10H=function(_10I){return _10I>1000?B(_10C(new T(function(){var _10J=B(_q7(_10I)),_10K=_10J[1],_10L=_10J[2];if(_10L>=0){var _10M=B(_15(0,B(_so(_10K,_10L)),_q));}else{var _10N= -_10L;if(_10N<=52){var _10O=hs_uncheckedIShiftRA64(B(_sg(_10K)),_10N),_10P=_10O,_10Q=B(_15(0,B(_rz(_10P)),_q));}else{var _10Q=!B(_U(_10K,_Of))?E(_10a):E(_10b);}var _10R=_10Q,_10S=_10R,_10M=_10S;}var _10T=_10M,_10U=_10T;return _10U;}))):B(_10C(new T(function(){return B(_105(B(_L5(_ZE,new T(function(){return B(_n8([1,[4,[0,_10I]],_q],_q));}))),5));})));},_10V=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:241:9-14"));}),_10W=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:228:7-12"));}),_10X=[1,_mM,_q],_10Y=[0,5],_10Z=[1,_10Y,_10X],_110=[1,_Tk,_10Z],_111=[0,50],_112=[1,_111,_110],_113=[0,100],_114=[1,_113,_112],_115=[0,500],_116=[1,_115,_114],_117=new T(function(){return B(_10r(_q));}),_118=new T(function(){return B(_1s(_mN,_117));}),_119=[0,-2147483648],_11a=new T(function(){return B(unCStr("disabled"));}),_11b=new T(function(){return B(unCStr("%s<span class=\"item-%d\">%s</span>"));}),_11c=new T(function(){return B(_UA("(function(e,c){e.removeAttribute(c);})"));}),_11d=function(_11e){return function(_11f,_){var _11g=B(A(new T(function(){return B(A(_11c,[E(E(_11e)[1])]));}),[E(toJSStr(E(_11f))),_])),_11h=_11g;return _77;};},_11i=function(_11j,_11k){var _11l=E(_11j);if(!_11l[0]){return [0];}else{var _11m=E(_11k);return _11m[0]==0?[0]:[1,[0,_11l[1],_11m[1]],new T(function(){return B(_11i(_11l[2],_11m[2]));})];}},_11n=function(_,_11o){var _11p=jsFind(toJSStr(E(_2))),_11q=_11p,_11r=E(_11q);if(!_11r[0]){return new F(function(){return _R4(_10W,_);});}else{var _11s=E(_n4),_11t=toJSStr(_11s),_11u=E(E(_11o)[2]),_11v=_11u[7],_11w=jsSet(E(_11r[1])[1],_11t,toJSStr(B(_10H(E(_11u[2])[1])))),_11x=jsFind(toJSStr(E(_4))),_11y=_11x,_11z=E(_11y);if(!_11z[0]){return new F(function(){return _R4(_10W,_);});}else{var _11A=E(_11u[1])[1],_11B=jsSet(E(_11z[1])[1],_11t,toJSStr(B(_10H(_11A)))),_11C=jsFind(toJSStr(E(_0))),_11D=_11C,_11E=E(_11D);if(!_11E[0]){return new F(function(){return _R4(_10W,_);});}else{var _11F=jsSet(E(_11E[1])[1],_11t,toJSStr(B(_10H(E(_11u[3])[1])))),_11G=function(_11H){var _11I=E(_11H);return _11I[0]==0?E(_PH):function(_11J,_){var _11K=B(A(new T(function(){var _11L=E(_11I[1]),_11M=_11L[1],_11N=E(_11L[2])[1],_11O=new T(function(){var _11P=E(_11M)[1];return _11P<=0?B(unAppCStr("item-sp-",new T(function(){if(_11P<0){var _11Q=B(_9R(0, -_11P,_q));}else{var _11Q=B(_9R(0,_11P,_q));}var _11R=_11Q;return _11R;}))):B(unAppCStr("item-",new T(function(){return B(_9R(0,_11P,_q));})));}),_11S=new T(function(){var _11T=B(_mP(E(_11M)[1],_11v));return _11T[0]==0?E(_lk):E(_11T[1]);});return function(_11U,_){var _11V=B(A(new T(function(){if(E(_11M)[1]<=0){var _11W=E(_PH);}else{var _11W=function(_11X,_){var _11Y=E(new T(function(){return B(_K(_11O,_Y7));})),_11Z=jsFind(toJSStr(_11Y)),_120=_11Z,_121=E(_120);if(!_121[0]){return new F(function(){return _Mf(_11Y);});}else{var _122=jsFind(toJSStr(E(new T(function(){return B(_K(_11O,_Y5));})))),_123=_122,_124=E(_123);if(!_124[0]){return new F(function(){return _R4(_10V,_);});}else{var _125=jsGet(E(_124[1])[1],toJSStr(_11s)),_126=_125,_127=new T(function(){return fromJSStr(_126);}),_128=function(_129){return _129>1?[1,_127,new T(function(){return B(_128(_129-1|0));})]:E([1,_127,_q]);},_12a=jsSet(E(_121[1])[1],_11t,toJSStr(B((function(_12b,_12c){while(1){var _12d=(function(_12e,_12f){var _12g=E(_12f);if(!_12g[0]){return E(_12e);}else{var _12h=E(_12g[1]);_12b=B(_1s(_mN,B(_L5(_11b,new T(function(){return B(_n8([1,[1,new T(function(){var _12i=E(_12h[2])[1];if(_12i>0){var _12j=B(_1s(_mN,B(_10r(B(_128(_12i))))));}else{var _12j=E(_118);}var _12k=_12j,_12l=_12k;return _12l;})],[1,[2,_119,new T(function(){return B(_8B(E(_12h[1])[1]));})],[1,[1,new T(function(){return B(_1s(_mN,_12e));})],_q]]],_q));})))));_12c=_12g[2];return null;}})(_12b,_12c);if(_12d!=null){return _12d;}}})(_q,new T(function(){return B(_11i(_116,new T(function(){return B(_n8(B(_ZX(_11S,_q,_115,_114))[2],_q));})));}))))),_12m=E(new T(function(){return B(_K(_11O,_Y6));})),_12n=jsFind(toJSStr(_12m)),_12o=_12n,_12p=E(_12o);return _12p[0]==0?B(_Mf(_12m)):B(A(_Xb,[_Rt,_12p[1],_11s,new T(function(){return B(_9R(0,E(_11S)[1],_q));}),_11X,_]));}}};}var _12q=_11W,_12r=_12q;return _12r;}),[_11U,_])),_12s=_11V,_12t=E(new T(function(){return B(_K(_11O,_Vc));})),_12u=jsFind(toJSStr(_12t)),_12v=_12u,_12w=E(_12v);if(!_12w[0]){return new F(function(){return _Mf(_12t);});}else{var _12x=_12w[1];if(!E(new T(function(){var _12y=E(_11M)[1];if(_12y<=0){if(!B(_ma(_12y,_11v))){var _12z=B(_nT(B(A(_11N,[_11S]))))<=_11A;}else{if(B(_m3(_11v,_12y))[1]>=1){var _12A=false;}else{var _12A=B(_nT(B(A(_11N,[_11S]))))<=_11A;}var _12B=_12A,_12C=_12B,_12z=_12C;}var _12D=_12z;}else{var _12D=B(_nT(B(A(_11N,[_11S]))))<=_11A;}var _12E=_12D,_12F=_12E;return _12F;}))){var _12G=B(A(_X5,[_Rt,_12x,_11a,_11a,new T(function(){return E(E(_12s)[2]);}),_])),_12H=_12G,_12I=B(_K(_11O,_Y8)),_12J=jsFind(toJSStr(_12I)),_12K=_12J,_12L=E(_12K);if(!_12L[0]){return new F(function(){return _Mf(_12I);});}else{var _12M=jsSet(E(_12L[1])[1],toJSStr(_11s),toJSStr(B(_10C(new T(function(){return B(_15(0,B(A(_11N,[_11S])),_q));}))))),_12N=function(_12O,_){var _12P=E(_11O),_12Q=jsFind(toJSStr(_12P)),_12R=_12Q,_12S=E(_12R);if(!_12S[0]){return new F(function(){return _Mf(_12P);});}else{var _12T=E(_12S[1]),_12U=E(_PG),_12V=jsGetStyle(_12T[1],toJSStr(_12U)),_12W=_12V;return !B(_6K(fromJSStr(_12W),_PF))?B(A(_Pu,[_Rt,_12T,_12U,_PF,_12O,_])):[0,_77,_12O];}};if(!B(_ma(E(_11M)[1],_11v))){var _12X=E(E(_12H)[2]);return B(_nT(B(A(_11N,[_lk]))))>3*E(_12X[8])[1]?[0,_77,_12X]:B(_12N(_12X,_));}else{return new F(function(){return _12N(new T(function(){return E(E(_12H)[2]);}),_);});}}}else{var _12Y=B(A(_11d,[_12x,_11a,_])),_12Z=_12Y,_130=B(_K(_11O,_Y8)),_131=jsFind(toJSStr(_130)),_132=_131,_133=E(_132);if(!_133[0]){return new F(function(){return _Mf(_130);});}else{var _134=jsSet(E(_133[1])[1],toJSStr(_11s),toJSStr(B(_10C(new T(function(){return B(_15(0,B(A(_11N,[_11S])),_q));}))))),_135=function(_136,_){var _137=E(_11O),_138=jsFind(toJSStr(_137)),_139=_138,_13a=E(_139);if(!_13a[0]){return new F(function(){return _Mf(_137);});}else{var _13b=E(_13a[1]),_13c=E(_PG),_13d=jsGetStyle(_13b[1],toJSStr(_13c)),_13e=_13d;return !B(_6K(fromJSStr(_13e),_PF))?B(A(_Pu,[_Rt,_13b,_13c,_PF,_136,_])):[0,_77,_136];}};if(!B(_ma(E(_11M)[1],_11v))){var _13f=E(E(_12s)[2]);return B(_nT(B(A(_11N,[_lk]))))>3*E(_13f[8])[1]?[0,_77,_13f]:B(_135(_13f,_));}else{return new F(function(){return _135(new T(function(){return E(E(_12s)[2]);}),_);});}}}}};}),[_11J,_])),_13g=_11K;return new F(function(){return A(new T(function(){return B(_11G(_11I[2]));}),[new T(function(){return E(E(_13g)[2]);}),_]);});};};return new F(function(){return A(_11G,[_SR,_11u,_]);});}}}},_13h=function(_13i){var _13j=E(_13i);return _13j[0]==0?E(_PH):function(_13k,_){var _13l=B(A(new T(function(){var _13m=E(_13j[1]),_13n=_13m[1],_13o=new T(function(){return B(A(E(_13m[2])[2],[_13n]));});return function(_13p,_){var _13q=E(_13p),_13r=_13q[6],_13s=E(_13n)[1];return !B(_ma(_13s,_13r))?B(A(_13o,[_13q,_])):B(_m3(_13r,_13s))[0]==0?B(A(_13o,[_13q,_])):[0,_77,_13q];};}),[_13k,_])),_13t=_13l;return new F(function(){return A(new T(function(){return B(_13h(_13j[2]));}),[new T(function(){return E(E(_13t)[2]);}),_]);});};},_13u=new T(function(){return B(_13h(_Tn));}),_13v=function(_13w){return new F(function(){return err(B(unAppCStr("docFocused: ",[1,_jc,new T(function(){return B(_je(_13w,_ZD));})])));});},_13x=new T(function(){return B(unCStr("\u518d\u4f1a\u30dc\u30fc\u30ca\u30b9<br>\u4f9d\u5b58\u5ea6 +"));}),_13y=new T(function(){return B(unCStr("true"));}),_13z=new T(function(){return B(unCStr("false"));}),_13A=new T(function(){return B(unCStr("document.hasFocus()"));}),_13B=function(_13C,_13D){while(1){var _13E=E(_13C);if(!_13E[0]){return E(_13D)[0]==0?1:0;}else{var _13F=E(_13D);if(!_13F[0]){return 2;}else{var _13G=E(_13E[1])[1],_13H=E(_13F[1])[1];if(_13G!=_13H){return _13G>_13H?2:0;}else{_13C=_13E[2];_13D=_13F[2];continue;}}}}},_13I=new T(function(){return B(unCStr("Map.!: given key is not an element in the map"));}),_13J=new T(function(){return B(err(_13I));}),_13K=function(_13L,_13M){while(1){var _13N=E(_13L),_13O=E(_13M);if(!_13O[0]){switch(B(_13B(_13N,_13O[2]))){case 0:_13L=_13N;_13M=_13O[4];continue;case 1:return E(_13O[3]);default:_13L=_13N;_13M=_13O[5];continue;}}else{return E(_13J);}}},_13P=[1],_13Q=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_13R=function(_13S){return new F(function(){return err(_13Q);});},_13T=new T(function(){return B(_13R(_));}),_13U=function(_13V,_13W,_13X,_13Y){var _13Z=E(_13X);if(!_13Z[0]){var _140=_13Z[1],_141=E(_13Y);if(!_141[0]){var _142=_141[1],_143=_141[2],_144=_141[3];if(_142<=(imul(3,_140)|0)){return [0,(1+_140|0)+_142|0,E(E(_13V)),_13W,E(_13Z),E(_141)];}else{var _145=E(_141[4]);if(!_145[0]){var _146=_145[1],_147=_145[2],_148=_145[3],_149=_145[4],_14a=E(_141[5]);if(!_14a[0]){var _14b=_14a[1];if(_146>=(imul(2,_14b)|0)){var _14c=function(_14d){var _14e=E(_13V),_14f=E(_145[5]);return _14f[0]==0?[0,(1+_140|0)+_142|0,E(_147),_148,E([0,(1+_140|0)+_14d|0,E(_14e),_13W,E(_13Z),E(_149)]),E([0,(1+_14b|0)+_14f[1]|0,E(_143),_144,E(_14f),E(_14a)])]:[0,(1+_140|0)+_142|0,E(_147),_148,E([0,(1+_140|0)+_14d|0,E(_14e),_13W,E(_13Z),E(_149)]),E([0,1+_14b|0,E(_143),_144,E(_13P),E(_14a)])];},_14g=E(_149);return _14g[0]==0?B(_14c(_14g[1])):B(_14c(0));}else{return [0,(1+_140|0)+_142|0,E(_143),_144,E([0,(1+_140|0)+_146|0,E(E(_13V)),_13W,E(_13Z),E(_145)]),E(_14a)];}}else{return E(_13T);}}else{return E(_13T);}}}else{return [0,1+_140|0,E(E(_13V)),_13W,E(_13Z),E(_13P)];}}else{var _14h=E(_13Y);if(!_14h[0]){var _14i=_14h[1],_14j=_14h[2],_14k=_14h[3],_14l=_14h[5],_14m=E(_14h[4]);if(!_14m[0]){var _14n=_14m[1],_14o=_14m[2],_14p=_14m[3],_14q=_14m[4],_14r=E(_14l);if(!_14r[0]){var _14s=_14r[1];if(_14n>=(imul(2,_14s)|0)){var _14t=function(_14u){var _14v=E(_13V),_14w=E(_14m[5]);return _14w[0]==0?[0,1+_14i|0,E(_14o),_14p,E([0,1+_14u|0,E(_14v),_13W,E(_13P),E(_14q)]),E([0,(1+_14s|0)+_14w[1]|0,E(_14j),_14k,E(_14w),E(_14r)])]:[0,1+_14i|0,E(_14o),_14p,E([0,1+_14u|0,E(_14v),_13W,E(_13P),E(_14q)]),E([0,1+_14s|0,E(_14j),_14k,E(_13P),E(_14r)])];},_14x=E(_14q);return _14x[0]==0?B(_14t(_14x[1])):B(_14t(0));}else{return [0,1+_14i|0,E(_14j),_14k,E([0,1+_14n|0,E(E(_13V)),_13W,E(_13P),E(_14m)]),E(_14r)];}}else{return [0,3,E(_14o),_14p,E([0,1,E(E(_13V)),_13W,E(_13P),E(_13P)]),E([0,1,E(_14j),_14k,E(_13P),E(_13P)])];}}else{var _14y=E(_14l);return _14y[0]==0?[0,3,E(_14j),_14k,E([0,1,E(E(_13V)),_13W,E(_13P),E(_13P)]),E(_14y)]:[0,2,E(E(_13V)),_13W,E(_13P),E(_14h)];}}else{return [0,1,E(E(_13V)),_13W,E(_13P),E(_13P)];}}},_14z=function(_14A,_14B){return [0,1,E(E(_14A)),_14B,E(_13P),E(_13P)];},_14C=function(_14D,_14E,_14F){var _14G=E(_14F);if(!_14G[0]){return new F(function(){return _13U(_14G[2],_14G[3],_14G[4],B(_14C(_14D,_14E,_14G[5])));});}else{return new F(function(){return _14z(_14D,_14E);});}},_14H=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_14I=function(_14J){return new F(function(){return err(_14H);});},_14K=new T(function(){return B(_14I(_));}),_14L=function(_14M,_14N,_14O,_14P){var _14Q=E(_14P);if(!_14Q[0]){var _14R=_14Q[1],_14S=E(_14O);if(!_14S[0]){var _14T=_14S[1],_14U=_14S[2],_14V=_14S[3];if(_14T<=(imul(3,_14R)|0)){return [0,(1+_14T|0)+_14R|0,E(E(_14M)),_14N,E(_14S),E(_14Q)];}else{var _14W=E(_14S[4]);if(!_14W[0]){var _14X=_14W[1],_14Y=E(_14S[5]);if(!_14Y[0]){var _14Z=_14Y[1],_150=_14Y[2],_151=_14Y[3],_152=_14Y[4];if(_14Z>=(imul(2,_14X)|0)){var _153=function(_154){var _155=E(_14Y[5]);return _155[0]==0?[0,(1+_14T|0)+_14R|0,E(_150),_151,E([0,(1+_14X|0)+_154|0,E(_14U),_14V,E(_14W),E(_152)]),E([0,(1+_14R|0)+_155[1]|0,E(E(_14M)),_14N,E(_155),E(_14Q)])]:[0,(1+_14T|0)+_14R|0,E(_150),_151,E([0,(1+_14X|0)+_154|0,E(_14U),_14V,E(_14W),E(_152)]),E([0,1+_14R|0,E(E(_14M)),_14N,E(_13P),E(_14Q)])];},_156=E(_152);return _156[0]==0?B(_153(_156[1])):B(_153(0));}else{return [0,(1+_14T|0)+_14R|0,E(_14U),_14V,E(_14W),E([0,(1+_14R|0)+_14Z|0,E(E(_14M)),_14N,E(_14Y),E(_14Q)])];}}else{return E(_14K);}}else{return E(_14K);}}}else{return [0,1+_14R|0,E(E(_14M)),_14N,E(_13P),E(_14Q)];}}else{var _157=E(_14O);if(!_157[0]){var _158=_157[1],_159=_157[2],_15a=_157[3],_15b=_157[5],_15c=E(_157[4]);if(!_15c[0]){var _15d=_15c[1],_15e=E(_15b);if(!_15e[0]){var _15f=_15e[1],_15g=_15e[2],_15h=_15e[3],_15i=_15e[4];if(_15f>=(imul(2,_15d)|0)){var _15j=function(_15k){var _15l=E(_15e[5]);return _15l[0]==0?[0,1+_158|0,E(_15g),_15h,E([0,(1+_15d|0)+_15k|0,E(_159),_15a,E(_15c),E(_15i)]),E([0,1+_15l[1]|0,E(E(_14M)),_14N,E(_15l),E(_13P)])]:[0,1+_158|0,E(_15g),_15h,E([0,(1+_15d|0)+_15k|0,E(_159),_15a,E(_15c),E(_15i)]),E([0,1,E(E(_14M)),_14N,E(_13P),E(_13P)])];},_15m=E(_15i);return _15m[0]==0?B(_15j(_15m[1])):B(_15j(0));}else{return [0,1+_158|0,E(_159),_15a,E(_15c),E([0,1+_15f|0,E(E(_14M)),_14N,E(_15e),E(_13P)])];}}else{return [0,3,E(_159),_15a,E(_15c),E([0,1,E(E(_14M)),_14N,E(_13P),E(_13P)])];}}else{var _15n=E(_15b);return _15n[0]==0?[0,3,E(_15n[2]),_15n[3],E([0,1,E(_159),_15a,E(_13P),E(_13P)]),E([0,1,E(E(_14M)),_14N,E(_13P),E(_13P)])]:[0,2,E(E(_14M)),_14N,E(_157),E(_13P)];}}else{return [0,1,E(E(_14M)),_14N,E(_13P),E(_13P)];}}},_15o=function(_15p,_15q,_15r){var _15s=E(_15r);if(!_15s[0]){return new F(function(){return _14L(_15s[2],_15s[3],B(_15o(_15p,_15q,_15s[4])),_15s[5]);});}else{return new F(function(){return _14z(_15p,_15q);});}},_15t=function(_15u,_15v,_15w,_15x,_15y,_15z,_15A){return new F(function(){return _14L(_15x,_15y,B(_15o(_15u,_15v,_15z)),_15A);});},_15B=function(_15C,_15D,_15E,_15F,_15G,_15H,_15I,_15J){var _15K=E(_15E);if(!_15K[0]){var _15L=_15K[1],_15M=_15K[2],_15N=_15K[3],_15O=_15K[4],_15P=_15K[5];if((imul(3,_15L)|0)>=_15F){if((imul(3,_15F)|0)>=_15L){return [0,(_15L+_15F|0)+1|0,E(E(_15C)),_15D,E(_15K),E([0,_15F,E(_15G),_15H,E(_15I),E(_15J)])];}else{return new F(function(){return _13U(_15M,_15N,_15O,B(_15B(_15C,_15D,_15P,_15F,_15G,_15H,_15I,_15J)));});}}else{return new F(function(){return _14L(_15G,_15H,B(_15Q(_15C,_15D,_15L,_15M,_15N,_15O,_15P,_15I)),_15J);});}}else{return new F(function(){return _15t(_15C,_15D,_15F,_15G,_15H,_15I,_15J);});}},_15Q=function(_15R,_15S,_15T,_15U,_15V,_15W,_15X,_15Y){var _15Z=E(_15Y);if(!_15Z[0]){var _160=_15Z[1],_161=_15Z[2],_162=_15Z[3],_163=_15Z[4],_164=_15Z[5];if((imul(3,_15T)|0)>=_160){if((imul(3,_160)|0)>=_15T){return [0,(_15T+_160|0)+1|0,E(E(_15R)),_15S,E([0,_15T,E(_15U),_15V,E(_15W),E(_15X)]),E(_15Z)];}else{return new F(function(){return _13U(_15U,_15V,_15W,B(_15B(_15R,_15S,_15X,_160,_161,_162,_163,_164)));});}}else{return new F(function(){return _14L(_161,_162,B(_15Q(_15R,_15S,_15T,_15U,_15V,_15W,_15X,_163)),_164);});}}else{return new F(function(){return _14C(_15R,_15S,[0,_15T,E(_15U),_15V,E(_15W),E(_15X)]);});}},_165=function(_166,_167,_168,_169){var _16a=E(_168);if(!_16a[0]){var _16b=_16a[1],_16c=_16a[2],_16d=_16a[3],_16e=_16a[4],_16f=_16a[5],_16g=E(_169);if(!_16g[0]){var _16h=_16g[1],_16i=_16g[2],_16j=_16g[3],_16k=_16g[4],_16l=_16g[5];if((imul(3,_16b)|0)>=_16h){if((imul(3,_16h)|0)>=_16b){return [0,(_16b+_16h|0)+1|0,E(E(_166)),_167,E(_16a),E(_16g)];}else{return new F(function(){return _13U(_16c,_16d,_16e,B(_15B(_166,_167,_16f,_16h,_16i,_16j,_16k,_16l)));});}}else{return new F(function(){return _14L(_16i,_16j,B(_15Q(_166,_167,_16b,_16c,_16d,_16e,_16f,_16k)),_16l);});}}else{return new F(function(){return _14C(_166,_167,_16a);});}}else{return new F(function(){return _15o(_166,_167,_169);});}},_16m=function(_16n,_16o,_16p,_16q){var _16r=E(_16n);if(_16r==1){var _16s=E(_16q);return _16s[0]==0?[0,new T(function(){return [0,1,E(E(_16o)),_16p,E(_13P),E(_13P)];}),_q,_q]:B(_13B(_16o,E(_16s[1])[1]))==0?[0,new T(function(){return [0,1,E(E(_16o)),_16p,E(_13P),E(_13P)];}),_16s,_q]:[0,new T(function(){return [0,1,E(E(_16o)),_16p,E(_13P),E(_13P)];}),_q,_16s];}else{var _16t=B(_16m(_16r>>1,_16o,_16p,_16q)),_16u=_16t[1],_16v=_16t[3],_16w=E(_16t[2]);if(!_16w[0]){return [0,_16u,_q,_16v];}else{var _16x=E(_16w[1]),_16y=_16x[1],_16z=_16x[2],_16A=E(_16w[2]);if(!_16A[0]){return [0,new T(function(){return B(_14C(_16y,_16z,_16u));}),_q,_16v];}else{var _16B=E(_16A[1]),_16C=_16B[1];if(!B(_13B(_16y,_16C))){var _16D=B(_16m(_16r>>1,_16C,_16B[2],_16A[2]));return [0,new T(function(){return B(_165(_16y,_16z,_16u,_16D[1]));}),_16D[2],_16D[3]];}else{return [0,_16u,_q,_16w];}}}}},_16E=function(_16F,_16G,_16H){var _16I=E(_16F),_16J=E(_16H);if(!_16J[0]){var _16K=_16J[2],_16L=_16J[3],_16M=_16J[4],_16N=_16J[5];switch(B(_13B(_16I,_16K))){case 0:return new F(function(){return _14L(_16K,_16L,B(_16E(_16I,_16G,_16M)),_16N);});break;case 1:return [0,_16J[1],E(_16I),_16G,E(_16M),E(_16N)];default:return new F(function(){return _13U(_16K,_16L,_16M,B(_16E(_16I,_16G,_16N)));});}}else{return [0,1,E(_16I),_16G,E(_13P),E(_13P)];}},_16O=function(_16P,_16Q){while(1){var _16R=E(_16Q);if(!_16R[0]){return E(_16P);}else{var _16S=E(_16R[1]),_16T=B(_16E(_16S[1],_16S[2],_16P));_16Q=_16R[2];_16P=_16T;continue;}}},_16U=function(_16V,_16W,_16X,_16Y){return new F(function(){return _16O(B(_16E(_16W,_16X,_16V)),_16Y);});},_16Z=function(_170,_171,_172){var _173=E(_171);return new F(function(){return _16O(B(_16E(_173[1],_173[2],_170)),_172);});},_174=function(_175,_176,_177){while(1){var _178=E(_177);if(!_178[0]){return E(_176);}else{var _179=E(_178[1]),_17a=_179[1],_17b=_179[2],_17c=E(_178[2]);if(!_17c[0]){return new F(function(){return _14C(_17a,_17b,_176);});}else{var _17d=E(_17c[1]),_17e=_17d[1];if(!B(_13B(_17a,_17e))){var _17f=B(_16m(_175,_17e,_17d[2],_17c[2])),_17g=_17f[1],_17h=E(_17f[3]);if(!_17h[0]){var _17i=_175<<1,_17j=B(_165(_17a,_17b,_176,_17g));_177=_17f[2];_175=_17i;_176=_17j;continue;}else{return new F(function(){return _16Z(B(_165(_17a,_17b,_176,_17g)),_17h[1],_17h[2]);});}}else{return new F(function(){return _16U(_176,_17a,_17b,_17c);});}}}}},_17k=function(_17l,_17m,_17n,_17o,_17p){var _17q=E(_17p);if(!_17q[0]){return new F(function(){return _14C(_17n,_17o,_17m);});}else{var _17r=E(_17q[1]),_17s=_17r[1];if(!B(_13B(_17n,_17s))){var _17t=B(_16m(_17l,_17s,_17r[2],_17q[2])),_17u=_17t[1],_17v=E(_17t[3]);if(!_17v[0]){return new F(function(){return _174(_17l<<1,B(_165(_17n,_17o,_17m,_17u)),_17t[2]);});}else{return new F(function(){return _16Z(B(_165(_17n,_17o,_17m,_17u)),_17v[1],_17v[2]);});}}else{return new F(function(){return _16U(_17m,_17n,_17o,_17q);});}}},_17w=function(_17x){var _17y=E(_17x);if(!_17y[0]){return [1];}else{var _17z=E(_17y[1]),_17A=_17z[1],_17B=_17z[2],_17C=E(_17y[2]);if(!_17C[0]){return [0,1,E(E(_17A)),_17B,E(_13P),E(_13P)];}else{var _17D=_17C[2],_17E=E(_17C[1]),_17F=_17E[1],_17G=_17E[2];if(!B(_13B(_17A,_17F))){return new F(function(){return _17k(1,[0,1,E(E(_17A)),_17B,E(_13P),E(_13P)],_17F,_17G,_17D);});}else{return new F(function(){return _16U([0,1,E(E(_17A)),_17B,E(_13P),E(_13P)],_17F,_17G,_17D);});}}}},_17H=new T(function(){return B(unCStr("reset"));}),_17I=new T(function(){return B(unCStr("resetAll"));}),_17J=[1,_17I,_q],_17K=[1,_17H,_17J],_17L=[1,_RF,_17K],_17M=[1,_Rv,_17L],_17N=new T(function(){return B(_11i(_17M,_SL));}),_17O=new T(function(){return B(_17w(_17N));}),_17P=new T(function(){return B(_13K(_Rv,_17O));}),_17Q=function(_17R,_17S){while(1){var _17T=E(_17R);if(!_17T[0]){var _17U=_17T[1],_17V=E(_17S);if(!_17V[0]){var _17W=_17V[1],_17X=subC(_17U,_17W);if(!E(_17X[2])){return [0,_17X[1]];}else{_17R=[1,I_fromInt(_17U)];_17S=[1,I_fromInt(_17W)];continue;}}else{_17R=[1,I_fromInt(_17U)];_17S=_17V;continue;}}else{var _17Y=E(_17S);if(!_17Y[0]){_17R=_17T;_17S=[1,I_fromInt(_17Y[1])];continue;}else{return [1,I_sub(_17T[1],_17Y[1])];}}}},_17Z=function(_180,_){var _181=E(_180),_182=_181[1],_183=_181[2],_184=_181[3],_185=_181[4],_186=_181[6],_187=_181[7],_188=_181[8];if(!B(_ma(E(_17P)[1],_187))){return new F(function(){return _11n(_,[0,_77,_181]);});}else{var _189=jsEval(toJSStr(E(_13A))),_18a=_189,_18b=B(_mg(_)),_18c=_18b,_18d=fromJSStr(_18a);if(!B(_6y(_18d,_13z))){if(!B(_6y(_18d,_13y))){return new F(function(){return _13v(_18d);});}else{var _18e=new T(function(){return [0,B(_nT(B(_17Q(_18c,_185))))];});if(!E(_181[5])){var _18f=new T(function(){return [0,E(_18e)[1]/1000/50];}),_18g=B(_Mh(new T(function(){return B(_K(_13x,new T(function(){return B(_10H(E(_18f)[1]));})));}),_)),_18h=_18g,_18i=B(A(_13u,[[0,new T(function(){return [0,E(_182)[1]+E(_183)[1]/30];}),new T(function(){return [0,E(_183)[1]+E(_18e)[1]/1000/100];}),new T(function(){return [0,E(_184)[1]+E(_18f)[1]+E(_18e)[1]/1000/1000];}),_18c,_j,_186,_187,_188],_])),_18j=_18i,_18k=E(E(_18j)[2]),_18l=E(_18k[1]);return _18l[1]<=E(_18k[8])[1]?B(_11n(_,[0,_77,_18k])):B(_11n(_,[0,_77,[0,_18l,_18k[2],_18k[3],_18k[4],_18k[5],_18k[6],_18k[7],_18l]]));}else{var _18m=B(A(_13u,[[0,new T(function(){return [0,E(_182)[1]+E(_183)[1]/30];}),new T(function(){return [0,E(_183)[1]+E(_18e)[1]/1000/100];}),new T(function(){return [0,E(_184)[1]+E(_18e)[1]/1000/1000];}),_18c,_j,_186,_187,_188],_])),_18n=_18m,_18o=E(E(_18n)[2]),_18p=E(_18o[1]);return _18p[1]<=E(_18o[8])[1]?B(_11n(_,[0,_77,_18o])):B(_11n(_,[0,_77,[0,_18p,_18o[2],_18o[3],_18o[4],_18o[5],_18o[6],_18o[7],_18p]]));}}}else{var _18q=E(_184)[1],_18r=_18q-1.0e-2;if(_18q<=0){var _18s=B(A(_13u,[[0,new T(function(){return [0,E(_182)[1]+E(_183)[1]/30];}),_183,new T(function(){return _18r>0?[0,_18r]:E(_S4);}),_185,_n,_186,_187,_188],_])),_18t=_18s,_18u=E(E(_18t)[2]),_18v=E(_18u[1]);return _18v[1]<=E(_18u[8])[1]?B(_11n(_,[0,_77,_18u])):B(_11n(_,[0,_77,[0,_18v,_18u[2],_18u[3],_18u[4],_18u[5],_18u[6],_18u[7],_18v]]));}else{var _18w=B(A(_13u,[[0,new T(function(){return [0,E(_182)[1]+E(_183)[1]/30];}),new T(function(){return [0,E(_183)[1]+1.0e-2];}),new T(function(){return _18r>0?[0,_18r]:E(_S4);}),_185,_n,_186,_187,_188],_])),_18x=_18w,_18y=E(E(_18x)[2]),_18z=E(_18y[1]);return _18z[1]<=E(_18y[8])[1]?B(_11n(_,[0,_77,_18y])):B(_11n(_,[0,_77,[0,_18z,_18y[2],_18y[3],_18y[4],_18y[5],_18y[6],_18y[7],_18z]]));}}}},_18A=function(_18B){return new F(function(){return _13K(_18B,_17O);});},_18C=new T(function(){return B(_1s(_18A,_17K));}),_18D=function(_18E){return E(E(_18E)[2]);},_18F=function(_18G,_18H,_18I){while(1){var _18J=E(_18I);if(!_18J[0]){return true;}else{if(!B(A(_18D,[_18G,_18H,_18J[1]]))){return false;}else{_18I=_18J[2];continue;}}}},_18K=function(_18L,_18M){return new F(function(){return _18F(_vE,_18L,_18C);});},_18N=new T(function(){return B(_mW(1,2147483647));}),_18O=function(_){var _=0,_18P=jsMkStdout(),_18Q=_18P;return [0,_18Q];},_18R=new T(function(){return B(_Uw(_18O));}),_18S=function(_){var _18T=B(_mg(_)),_18U=_18T,_18V=B(_WB(_hr,_TB,_)),_18W=_18V,_18X=nMV(new T(function(){var _18Y=E(_18W);return _18Y[0]==0?[0,_S4,_S4,_S4,_18U,_n,_3Z,_3Z,_S4]:E(_18Y[1]);})),_18Z=_18X,_190=B(unCStr("list-group")),_191=jsFind(toJSStr(_190)),_192=_191,_193=E(_192);if(!_193[0]){return new F(function(){return _Mf(_190);});}else{var _194=B((function(_195,_){while(1){var _196=E(_195);if(!_196[0]){return _77;}else{var _197=E(_196[1]),_198=E(E(_197[2])[3]),_199=B(A(_Yn,[_197[1],_198[1],_198[2],_198[3],_193[1],_])),_19a=_199;_195=_196[2];continue;}}})(_Wq,_)),_19b=_194,_19c=B(unCStr("list-sp-group")),_19d=jsFind(toJSStr(_19c)),_19e=_19d,_19f=E(_19e);if(!_19f[0]){return new F(function(){return _Mf(_19c);});}else{var _19g=B((function(_19h,_){while(1){var _19i=E(_19h);if(!_19i[0]){return _77;}else{var _19j=E(_19i[1]),_19k=E(E(_19j[2])[3]),_19l=B(A(_Yn,[_19j[1],_19k[1],_19k[2],_19k[3],_19f[1],_])),_19m=_19l;_19h=_19i[2];continue;}}})(_Ws,_)),_19n=_19g,_19o=[0,_18Z],_19p=B(_Vd(_19o,_18N,_)),_19q=_19p,_19r=B(_Vd(_19o,_SL,_)),_19s=_19r,_19t=function(_){var _19u=B(_lP(33,_18Z,_17Z,_)),_19v=_19u,_19w=B(_WU(_18R,B(_lM(_19v)),_)),_19x=_19w,_19y=B(_lP(1000,_18Z,_ZF,_)),_19z=_19y,_19A=B(_WU(_18R,B(_lM(_19z)),_)),_19B=_19A,_19C=B(_lP(60000,_18Z,_Zx,_)),_19D=_19C;return new F(function(){return _WU(_18R,B(_lM(_19D)),_);});},_19E=function(_19F,_19G,_){while(1){var _19H=(function(_19I,_19J,_){var _19K=E(_19J);switch(_19K[0]){case 0:_19F=function(_){return new F(function(){return _19E(_19I,_19K[4],_);});};_19G=_19K[3];return null;case 1:var _19L=_19K[1],_19M=rMV(_18Z),_19N=_19M,_19O=E(_19N),_19P=_19O[7];if(!B(_ma(_19L,_19P))){var _=wMV(_18Z,_19O);return new F(function(){return A(_19I,[_]);});}else{if(B(_m3(_19P,_19L))[1]<=0){var _=wMV(_18Z,_19O);return new F(function(){return A(_19I,[_]);});}else{var _19Q=B(A(E(_19K[2])[2],[[0,_19L],_19O,_])),_19R=_19Q,_=wMV(_18Z,new T(function(){return E(E(_19R)[2]);}));return new F(function(){return A(_19I,[_]);});}}break;default:return new F(function(){return A(_19I,[_]);});}})(_19F,_19G,_);if(_19H!=null){return _19H;}}},_19S=B(_O1(_18K,_SY));if(!_19S[0]){var _19T=_19S[3],_19U=_19S[4];if(_19S[2]>=0){var _19V=B(_19E(function(_){return new F(function(){return _19E(_Wr,_19U,_);});},_19T,_)),_19W=_19V;return new F(function(){return _19t(_);});}else{var _19X=B(_19E(function(_){return new F(function(){return _19E(_Wr,_19T,_);});},_19U,_)),_19Y=_19X;return new F(function(){return _19t(_);});}}else{var _19Z=B(_19E(_Wr,_19S,_)),_1a0=_19Z;return new F(function(){return _19t(_);});}}}},_1a1=function(_){return new F(function(){return _18S(_);});};
var hasteMain = function() {B(A(_1a1, [0]));};window.onload = hasteMain;
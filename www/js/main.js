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

// Export Haste, A and E. Haste because we need to preserve exports, A and E
// because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;


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
        acc = A(f, [[0, str.charCodeAt(i)], acc]);
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
    var x = A(f, [mv.x]);
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
        return A(act,[0]);
    } catch(e) {
        return A(handler,[e, 0]);
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
                A(cb,[[0,k.keyCode],0]);
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
            A(cb,[[0,x.button],[0,mx,my],0]);
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,mx,my],0]);
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {A(cb,[[0,x.keyCode],0]);};
        break;        
    default:
        fun = function() {A(cb,[0]);};
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
    window.setTimeout(function() {A(cb,[0]);}, msecs);
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
                A(cb,[[1,[0,xhr.responseText]],0]);
            } else {
                A(cb,[[0],0]); // Nothing
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

var _0=new T(function(){return [0,"lastFocus"];}),_1=new T(function(){return [0,"depend"];}),_2=new T(function(){return [0,"lps"];}),_3=new T(function(){return [0,"loves"];}),_4=new T(function(){return [0,"achievements"];}),_5=function(_6){return [0,toJSStr(E(_6))];},_7=function(_8){return [1,new T(function(){return B(_5(_8));})];},_9=[0],_a=function(_b,_c){while(1){var _d=(function(_e,_f){var _g=E(_f);switch(_g[0]){case 0:_b=new T(function(){return B(_a(_e,_g[4]));});_c=_g[3];return null;case 1:return [1,[3,[1,[0,[0,_g[1]]],[1,new T(function(){var _h=E(_g[2]);return [3,[1,new T(function(){return B(_7(_h[1]));}),[1,new T(function(){return B(_7(_h[2]));}),_9]]];}),_9]]],_e];default:return E(_e);}})(_b,_c);if(_d!=null){return _d;}}},_i=function(_j,_k){var _l=E(_j);return _l[0]==0?E(_k):[1,_l[1],new T(function(){return B(_i(_l[2],_k));})];},_m=function(_n){while(1){var _o=E(_n);if(!_o[0]){_n=[1,I_fromInt(_o[1])];continue;}else{return new F(function(){return I_toString(_o[1]);});}}},_p=function(_q,_r){return new F(function(){return _i(fromJSStr(B(_m(_q))),_r);});},_s=function(_t,_u){var _v=E(_t);if(!_v[0]){var _w=_v[1],_x=E(_u);return _x[0]==0?_w<_x[1]:I_compareInt(_x[1],_w)>0;}else{var _y=_v[1],_z=E(_u);return _z[0]==0?I_compareInt(_y,_z[1])<0:I_compare(_y,_z[1])<0;}},_A=[0,41],_B=[0,40],_C=[0,0],_D=function(_E,_F,_G){return _E<=6?B(_p(_F,_G)):!B(_s(_F,_C))?B(_p(_F,_G)):[1,_B,new T(function(){return B(_i(fromJSStr(B(_m(_F))),[1,_A,_G]));})];},_H=function(_I,_J,_K,_L,_M){return [1,[0,_3,[0,_I]],[1,[0,_2,[0,_J]],[1,[0,_1,[0,_K]],[1,[0,_0,[1,new T(function(){return [0,toJSStr(B(_D(0,_L,_9)))];})]],[1,[0,_4,[3,new T(function(){var _N=E(_M);if(!_N[0]){var _O=_N[3],_P=_N[4],_Q=_N[2]>=0?B(_a(new T(function(){return B(_a(_9,_P));}),_O)):B(_a(new T(function(){return B(_a(_9,_O));}),_P));}else{var _Q=B(_a(_9,_N));}return _Q;})]],_9]]]]];},_R=function(_S){var _T=E(_S);return [4,B(_H(_T[1],_T[2],_T[3],_T[4],_T[7]))];},_U=function(_V,_W){var _X=E(_W);return _X[0]==0?[0]:[1,new T(function(){return B(A(_V,[_X[1]]));}),new T(function(){return B(_U(_V,_X[2]));})];},_Y=function(_Z){return [3,new T(function(){return B(_U(_R,_Z));})];},_10=function(_11,_12){var _13=strEq(E(_11)[1],E(_12)[1]),_14=_13;return E(_14)==0?true:false;},_15=function(_16,_17){var _18=strEq(E(_16)[1],E(_17)[1]),_19=_18;return E(_19)==0?false:true;},_1a=[0,_15,_10],_1b=[1,_9],_1c=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_1d=[0,_1c],_1e=new T(function(){return B(unCStr("Tried to deserialize a non-array into a pair!"));}),_1f=[0,_1e],_1g=function(_1h){return E(E(_1h)[3]);},_1i=function(_1j,_1k,_1l){var _1m=E(_1l);if(_1m[0]==3){var _1n=E(_1m[1]);if(!_1n[0]){return E(_1f);}else{var _1o=E(_1n[2]);if(!_1o[0]){return E(_1f);}else{if(!E(_1o[2])[0]){var _1p=B(A(_1g,[_1j,_1n[1]]));if(!_1p[0]){return [0,_1p[1]];}else{var _1q=B(A(_1g,[_1k,_1o[1]]));return _1q[0]==0?[0,_1q[1]]:[1,[0,_1p[1],_1q[1]]];}}else{return E(_1f);}}}}else{return E(_1f);}},_1r=function(_1s,_1t,_1u){var _1v=E(_1u);if(_1v[0]==3){var _1w=function(_1x){var _1y=E(_1x);if(!_1y[0]){return E(_1b);}else{var _1z=B(_1i(_1s,_1t,_1y[1]));if(!_1z[0]){return [0,_1z[1]];}else{var _1A=B(_1w(_1y[2]));return _1A[0]==0?[0,_1A[1]]:[1,[1,_1z[1],_1A[1]]];}}};return new F(function(){return _1w(_1v[1]);});}else{return E(_1d);}},_1B=new T(function(){return B(unCStr("Tried to deserialize a non-JSString to a JSString"));}),_1C=[0,_1B],_1D=new T(function(){return B(unCStr("Tried to deserialize a non-Number to a Double"));}),_1E=[0,_1D],_1F=new T(function(){return B(unCStr("Key not found"));}),_1G=[0,_1F],_1H=new T(function(){return B(unCStr("Tried to do lookup on non-object!"));}),_1I=[0,_1H],_1J=[0,0],_1K=new T(function(){return B(unCStr("Prelude.read: ambiguous parse"));}),_1L=new T(function(){return B(err(_1K));}),_1M=new T(function(){return B(unCStr("Prelude.read: no parse"));}),_1N=new T(function(){return B(err(_1M));}),_1O=new T(function(){return B(unCStr("Control.Exception.Base"));}),_1P=new T(function(){return B(unCStr("base"));}),_1Q=new T(function(){return B(unCStr("PatternMatchFail"));}),_1R=new T(function(){var _1S=hs_wordToWord64(18445595),_1T=_1S,_1U=hs_wordToWord64(52003073),_1V=_1U;return [0,_1T,_1V,[0,_1T,_1V,_1P,_1O,_1Q],_9];}),_1W=function(_1X){return E(_1R);},_1Y=function(_1Z){return E(E(_1Z)[1]);},_20=function(_21,_22,_23){var _24=B(A(_21,[_])),_25=B(A(_22,[_])),_26=hs_eqWord64(_24[1],_25[1]),_27=_26;if(!E(_27)){return [0];}else{var _28=hs_eqWord64(_24[2],_25[2]),_29=_28;return E(_29)==0?[0]:[1,_23];}},_2a=function(_2b){var _2c=E(_2b);return new F(function(){return _20(B(_1Y(_2c[1])),_1W,_2c[2]);});},_2d=function(_2e){return E(E(_2e)[1]);},_2f=function(_2g,_2h){return new F(function(){return _i(E(_2g)[1],_2h);});},_2i=[0,44],_2j=[0,93],_2k=[0,91],_2l=function(_2m,_2n,_2o){var _2p=E(_2n);return _2p[0]==0?B(unAppCStr("[]",_2o)):[1,_2k,new T(function(){return B(A(_2m,[_2p[1],new T(function(){var _2q=function(_2r){var _2s=E(_2r);return _2s[0]==0?E([1,_2j,_2o]):[1,_2i,new T(function(){return B(A(_2m,[_2s[1],new T(function(){return B(_2q(_2s[2]));})]));})];};return B(_2q(_2p[2]));})]));})];},_2t=function(_2u,_2v){return new F(function(){return _2l(_2f,_2u,_2v);});},_2w=function(_2x,_2y,_2z){return new F(function(){return _i(E(_2y)[1],_2z);});},_2A=[0,_2w,_2d,_2t],_2B=new T(function(){return [0,_1W,_2A,_2C,_2a];}),_2C=function(_2D){return [0,_2B,_2D];},_2E=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_2F=function(_2G,_2H){return new F(function(){return die(new T(function(){return B(A(_2H,[_2G]));}));});},_2I=function(_2J,_2K){var _2L=E(_2K);if(!_2L[0]){return [0,_9,_9];}else{var _2M=_2L[1];if(!B(A(_2J,[_2M]))){return [0,_9,_2L];}else{var _2N=new T(function(){var _2O=B(_2I(_2J,_2L[2]));return [0,_2O[1],_2O[2]];});return [0,[1,_2M,new T(function(){return E(E(_2N)[1]);})],new T(function(){return E(E(_2N)[2]);})];}}},_2P=[0,32],_2Q=[0,10],_2R=[1,_2Q,_9],_2S=function(_2T){return E(E(_2T)[1])==124?false:true;},_2U=function(_2V,_2W){var _2X=B(_2I(_2S,B(unCStr(_2V)))),_2Y=_2X[1],_2Z=function(_30,_31){return new F(function(){return _i(_30,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_i(_2W,new T(function(){return B(_i(_31,_2R));})));})));}));});},_32=E(_2X[2]);if(!_32[0]){return new F(function(){return _2Z(_2Y,_9);});}else{return E(E(_32[1])[1])==124?B(_2Z(_2Y,[1,_2P,_32[2]])):B(_2Z(_2Y,_9));}},_33=function(_34){return new F(function(){return _2F([0,new T(function(){return B(_2U(_34,_2E));})],_2C);});},_35=new T(function(){return B(_33("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_36=function(_37,_38){while(1){var _39=(function(_3a,_3b){var _3c=E(_3a);switch(_3c[0]){case 0:var _3d=E(_3b);if(!_3d[0]){return [0];}else{_37=B(A(_3c[1],[_3d[1]]));_38=_3d[2];return null;}break;case 1:var _3e=B(A(_3c[1],[_3b])),_3f=_3b;_37=_3e;_38=_3f;return null;case 2:return [0];case 3:return [1,[0,_3c[1],_3b],new T(function(){return B(_36(_3c[2],_3b));})];default:return E(_3c[1]);}})(_37,_38);if(_39!=null){return _39;}}},_3g=function(_3h,_3i){var _3j=function(_3k){var _3l=E(_3i);if(_3l[0]==3){return [3,_3l[1],new T(function(){return B(_3g(_3h,_3l[2]));})];}else{var _3m=E(_3h);if(_3m[0]==2){return E(_3l);}else{var _3n=E(_3l);if(_3n[0]==2){return E(_3m);}else{var _3o=function(_3p){var _3q=E(_3n);if(_3q[0]==4){return [1,function(_3r){return [4,new T(function(){return B(_i(B(_36(_3m,_3r)),_3q[1]));})];}];}else{var _3s=E(_3m);if(_3s[0]==1){var _3t=_3s[1],_3u=E(_3q);return _3u[0]==0?[1,function(_3v){return new F(function(){return _3g(B(A(_3t,[_3v])),_3u);});}]:[1,function(_3w){return new F(function(){return _3g(B(A(_3t,[_3w])),new T(function(){return B(A(_3u[1],[_3w]));}));});}];}else{var _3x=E(_3q);return _3x[0]==0?E(_35):[1,function(_3y){return new F(function(){return _3g(_3s,new T(function(){return B(A(_3x[1],[_3y]));}));});}];}}},_3z=E(_3m);switch(_3z[0]){case 1:var _3A=E(_3n);if(_3A[0]==4){return [1,function(_3B){return [4,new T(function(){return B(_i(B(_36(B(A(_3z[1],[_3B])),_3B)),_3A[1]));})];}];}else{return new F(function(){return _3o(_);});}break;case 4:var _3C=_3z[1],_3D=E(_3n);switch(_3D[0]){case 0:return [1,function(_3E){return [4,new T(function(){return B(_i(_3C,new T(function(){return B(_36(_3D,_3E));})));})];}];case 1:return [1,function(_3F){return [4,new T(function(){return B(_i(_3C,new T(function(){return B(_36(B(A(_3D[1],[_3F])),_3F));})));})];}];default:return [4,new T(function(){return B(_i(_3C,_3D[1]));})];}break;default:return new F(function(){return _3o(_);});}}}}},_3G=E(_3h);switch(_3G[0]){case 0:var _3H=E(_3i);if(!_3H[0]){return [0,function(_3I){return new F(function(){return _3g(B(A(_3G[1],[_3I])),new T(function(){return B(A(_3H[1],[_3I]));}));});}];}else{return new F(function(){return _3j(_);});}break;case 3:return [3,_3G[1],new T(function(){return B(_3g(_3G[2],_3i));})];default:return new F(function(){return _3j(_);});}},_3J=[0,41],_3K=[1,_3J,_9],_3L=[0,40],_3M=[1,_3L,_9],_3N=function(_3O,_3P){while(1){var _3Q=E(_3O);if(!_3Q[0]){return E(_3P)[0]==0?true:false;}else{var _3R=E(_3P);if(!_3R[0]){return false;}else{if(E(_3Q[1])[1]!=E(_3R[1])[1]){return false;}else{_3O=_3Q[2];_3P=_3R[2];continue;}}}}},_3S=function(_3T,_3U){return E(_3T)[1]!=E(_3U)[1];},_3V=function(_3W,_3X){return E(_3W)[1]==E(_3X)[1];},_3Y=[0,_3V,_3S],_3Z=function(_40,_41){while(1){var _42=E(_40);if(!_42[0]){return E(_41)[0]==0?true:false;}else{var _43=E(_41);if(!_43[0]){return false;}else{if(E(_42[1])[1]!=E(_43[1])[1]){return false;}else{_40=_42[2];_41=_43[2];continue;}}}}},_44=function(_45,_46){return !B(_3Z(_45,_46))?true:false;},_47=[0,_3Z,_44],_48=function(_49,_4a){var _4b=E(_49);switch(_4b[0]){case 0:return [0,function(_4c){return new F(function(){return _48(B(A(_4b[1],[_4c])),_4a);});}];case 1:return [1,function(_4d){return new F(function(){return _48(B(A(_4b[1],[_4d])),_4a);});}];case 2:return [2];case 3:return new F(function(){return _3g(B(A(_4a,[_4b[1]])),new T(function(){return B(_48(_4b[2],_4a));}));});break;default:var _4e=function(_4f){var _4g=E(_4f);if(!_4g[0]){return [0];}else{var _4h=E(_4g[1]);return new F(function(){return _i(B(_36(B(A(_4a,[_4h[1]])),_4h[2])),new T(function(){return B(_4e(_4g[2]));}));});}},_4i=B(_4e(_4b[1]));return _4i[0]==0?[2]:[4,_4i];}},_4j=[2],_4k=function(_4l){return [3,_4l,_4j];},_4m=0,_4n=function(_4o,_4p){var _4q=E(_4o);if(!_4q){return new F(function(){return A(_4p,[_4m]);});}else{return [0,function(_4r){return E(new T(function(){return B(_4n(_4q-1|0,_4p));}));}];}},_4s=function(_4t,_4u,_4v){return function(_4w){return new F(function(){return A(function(_4x,_4y,_4z){while(1){var _4A=(function(_4B,_4C,_4D){var _4E=E(_4B);switch(_4E[0]){case 0:var _4F=E(_4C);if(!_4F[0]){return E(_4u);}else{_4x=B(A(_4E[1],[_4F[1]]));_4y=_4F[2];var _4G=_4D+1|0;_4z=_4G;return null;}break;case 1:var _4H=B(A(_4E[1],[_4C])),_4I=_4C,_4G=_4D;_4x=_4H;_4y=_4I;_4z=_4G;return null;case 2:return E(_4u);case 3:return function(_4J){return new F(function(){return _4n(_4D,function(_4K){return E(new T(function(){return B(_48(_4E,_4J));}));});});};default:return function(_4L){return new F(function(){return _48(_4E,_4L);});};}})(_4x,_4y,_4z);if(_4A!=null){return _4A;}}},[new T(function(){return B(A(_4t,[_4k]));}),_4w,0,_4v]);});};},_4M=function(_4N){return new F(function(){return A(_4N,[_9]);});},_4O=function(_4P,_4Q){var _4R=function(_4S){var _4T=E(_4S);if(!_4T[0]){return E(_4M);}else{var _4U=_4T[1];return !B(A(_4P,[_4U]))?E(_4M):function(_4V){return [0,function(_4W){return E(new T(function(){return B(A(new T(function(){return B(_4R(_4T[2]));}),[function(_4X){return new F(function(){return A(_4V,[[1,_4U,_4X]]);});}]));}));}];};}};return function(_4Y){return new F(function(){return A(_4R,[_4Y,_4Q]);});};},_4Z=[6],_50=function(_51){return E(_51);},_52=new T(function(){return B(unCStr("valDig: Bad base"));}),_53=new T(function(){return B(err(_52));}),_54=function(_55,_56){var _57=function(_58,_59){var _5a=E(_58);if(!_5a[0]){return function(_5b){return new F(function(){return A(_5b,[new T(function(){return B(A(_59,[_9]));})]);});};}else{var _5c=E(_5a[1])[1],_5d=function(_5e){return function(_5f){return [0,function(_5g){return E(new T(function(){return B(A(new T(function(){return B(_57(_5a[2],function(_5h){return new F(function(){return A(_59,[[1,_5e,_5h]]);});}));}),[_5f]));}));}];};};switch(E(E(_55)[1])){case 8:if(48>_5c){return function(_5i){return new F(function(){return A(_5i,[new T(function(){return B(A(_59,[_9]));})]);});};}else{if(_5c>55){return function(_5j){return new F(function(){return A(_5j,[new T(function(){return B(A(_59,[_9]));})]);});};}else{return new F(function(){return _5d([0,_5c-48|0]);});}}break;case 10:if(48>_5c){return function(_5k){return new F(function(){return A(_5k,[new T(function(){return B(A(_59,[_9]));})]);});};}else{if(_5c>57){return function(_5l){return new F(function(){return A(_5l,[new T(function(){return B(A(_59,[_9]));})]);});};}else{return new F(function(){return _5d([0,_5c-48|0]);});}}break;case 16:if(48>_5c){if(97>_5c){if(65>_5c){return function(_5m){return new F(function(){return A(_5m,[new T(function(){return B(A(_59,[_9]));})]);});};}else{if(_5c>70){return function(_5n){return new F(function(){return A(_5n,[new T(function(){return B(A(_59,[_9]));})]);});};}else{return new F(function(){return _5d([0,(_5c-65|0)+10|0]);});}}}else{if(_5c>102){if(65>_5c){return function(_5o){return new F(function(){return A(_5o,[new T(function(){return B(A(_59,[_9]));})]);});};}else{if(_5c>70){return function(_5p){return new F(function(){return A(_5p,[new T(function(){return B(A(_59,[_9]));})]);});};}else{return new F(function(){return _5d([0,(_5c-65|0)+10|0]);});}}}else{return new F(function(){return _5d([0,(_5c-97|0)+10|0]);});}}}else{if(_5c>57){if(97>_5c){if(65>_5c){return function(_5q){return new F(function(){return A(_5q,[new T(function(){return B(A(_59,[_9]));})]);});};}else{if(_5c>70){return function(_5r){return new F(function(){return A(_5r,[new T(function(){return B(A(_59,[_9]));})]);});};}else{return new F(function(){return _5d([0,(_5c-65|0)+10|0]);});}}}else{if(_5c>102){if(65>_5c){return function(_5s){return new F(function(){return A(_5s,[new T(function(){return B(A(_59,[_9]));})]);});};}else{if(_5c>70){return function(_5t){return new F(function(){return A(_5t,[new T(function(){return B(A(_59,[_9]));})]);});};}else{return new F(function(){return _5d([0,(_5c-65|0)+10|0]);});}}}else{return new F(function(){return _5d([0,(_5c-97|0)+10|0]);});}}}else{return new F(function(){return _5d([0,_5c-48|0]);});}}break;default:return E(_53);}}};return function(_5u){return new F(function(){return A(_57,[_5u,_50,function(_5v){var _5w=E(_5v);return _5w[0]==0?[2]:B(A(_56,[_5w]));}]);});};},_5x=[0,10],_5y=[0,1],_5z=[0,2147483647],_5A=function(_5B,_5C){while(1){var _5D=E(_5B);if(!_5D[0]){var _5E=_5D[1],_5F=E(_5C);if(!_5F[0]){var _5G=_5F[1],_5H=addC(_5E,_5G);if(!E(_5H[2])){return [0,_5H[1]];}else{_5B=[1,I_fromInt(_5E)];_5C=[1,I_fromInt(_5G)];continue;}}else{_5B=[1,I_fromInt(_5E)];_5C=_5F;continue;}}else{var _5I=E(_5C);if(!_5I[0]){_5B=_5D;_5C=[1,I_fromInt(_5I[1])];continue;}else{return [1,I_add(_5D[1],_5I[1])];}}}},_5J=new T(function(){return B(_5A(_5z,_5y));}),_5K=function(_5L){var _5M=E(_5L);if(!_5M[0]){var _5N=E(_5M[1]);return _5N==(-2147483648)?E(_5J):[0, -_5N];}else{return [1,I_negate(_5M[1])];}},_5O=[0,10],_5P=[0,0],_5Q=function(_5R){return [0,_5R];},_5S=function(_5T,_5U){while(1){var _5V=E(_5T);if(!_5V[0]){var _5W=_5V[1],_5X=E(_5U);if(!_5X[0]){var _5Y=_5X[1];if(!(imul(_5W,_5Y)|0)){return [0,imul(_5W,_5Y)|0];}else{_5T=[1,I_fromInt(_5W)];_5U=[1,I_fromInt(_5Y)];continue;}}else{_5T=[1,I_fromInt(_5W)];_5U=_5X;continue;}}else{var _5Z=E(_5U);if(!_5Z[0]){_5T=_5V;_5U=[1,I_fromInt(_5Z[1])];continue;}else{return [1,I_mul(_5V[1],_5Z[1])];}}}},_60=function(_61,_62,_63){while(1){var _64=E(_63);if(!_64[0]){return E(_62);}else{var _65=B(_5A(B(_5S(_62,_61)),B(_5Q(E(_64[1])[1]))));_63=_64[2];_62=_65;continue;}}},_66=function(_67){var _68=new T(function(){return B(_3g(B(_3g([0,function(_69){return E(E(_69)[1])==45?[1,B(_54(_5x,function(_6a){return new F(function(){return A(_67,[[1,new T(function(){return B(_5K(B(_60(_5O,_5P,_6a))));})]]);});}))]:[2];}],[0,function(_6b){return E(E(_6b)[1])==43?[1,B(_54(_5x,function(_6c){return new F(function(){return A(_67,[[1,new T(function(){return B(_60(_5O,_5P,_6c));})]]);});}))]:[2];}])),new T(function(){return [1,B(_54(_5x,function(_6d){return new F(function(){return A(_67,[[1,new T(function(){return B(_60(_5O,_5P,_6d));})]]);});}))];})));});return new F(function(){return _3g([0,function(_6e){return E(E(_6e)[1])==101?E(_68):[2];}],[0,function(_6f){return E(E(_6f)[1])==69?E(_68):[2];}]);});},_6g=[0],_6h=function(_6i){return new F(function(){return A(_6i,[_6g]);});},_6j=function(_6k){return new F(function(){return A(_6k,[_6g]);});},_6l=function(_6m){return function(_6n){return E(E(_6n)[1])==46?[1,B(_54(_5x,function(_6o){return new F(function(){return A(_6m,[[1,_6o]]);});}))]:[2];};},_6p=function(_6q){return [0,B(_6l(_6q))];},_6r=function(_6s){return new F(function(){return _54(_5x,function(_6t){return [1,B(_4s(_6p,_6h,function(_6u){return [1,B(_4s(_66,_6j,function(_6v){return new F(function(){return A(_6s,[[5,[1,_6t,_6u,_6v]]]);});}))];}))];});});},_6w=function(_6x){return [1,B(_6r(_6x))];},_6y=function(_6z){return E(E(_6z)[1]);},_6A=function(_6B,_6C,_6D){while(1){var _6E=E(_6D);if(!_6E[0]){return false;}else{if(!B(A(_6y,[_6B,_6C,_6E[1]]))){_6D=_6E[2];continue;}else{return true;}}}},_6F=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_6G=function(_6H){return new F(function(){return _6A(_3Y,_6H,_6F);});},_6I=[0,8],_6J=[0,16],_6K=function(_6L){var _6M=function(_6N){return new F(function(){return A(_6L,[[5,[0,_6I,_6N]]]);});},_6O=function(_6P){return new F(function(){return A(_6L,[[5,[0,_6J,_6P]]]);});};return function(_6Q){return E(E(_6Q)[1])==48?E([0,function(_6R){switch(E(E(_6R)[1])){case 79:return [1,B(_54(_6I,_6M))];case 88:return [1,B(_54(_6J,_6O))];case 111:return [1,B(_54(_6I,_6M))];case 120:return [1,B(_54(_6J,_6O))];default:return [2];}}]):[2];};},_6S=function(_6T){return [0,B(_6K(_6T))];},_6U=false,_6V=true,_6W=function(_6X){var _6Y=new T(function(){return B(A(_6X,[_6I]));}),_6Z=new T(function(){return B(A(_6X,[_6J]));});return function(_70){switch(E(E(_70)[1])){case 79:return E(_6Y);case 88:return E(_6Z);case 111:return E(_6Y);case 120:return E(_6Z);default:return [2];}};},_71=function(_72){return [0,B(_6W(_72))];},_73=[0,92],_74=function(_75){return new F(function(){return A(_75,[_5x]);});},_76=function(_77,_78){var _79=jsShowI(_77),_7a=_79;return new F(function(){return _i(fromJSStr(_7a),_78);});},_7b=function(_7c,_7d,_7e){if(_7d>=0){return new F(function(){return _76(_7d,_7e);});}else{return _7c<=6?B(_76(_7d,_7e)):[1,_B,new T(function(){var _7f=jsShowI(_7d),_7g=_7f;return B(_i(fromJSStr(_7g),[1,_A,_7e]));})];}},_7h=function(_7i){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_7b(9,_7i,_9));}))));});},_7j=function(_7k){var _7l=E(_7k);return _7l[0]==0?E(_7l[1]):I_toInt(_7l[1]);},_7m=function(_7n,_7o){var _7p=E(_7n);if(!_7p[0]){var _7q=_7p[1],_7r=E(_7o);return _7r[0]==0?_7q<=_7r[1]:I_compareInt(_7r[1],_7q)>=0;}else{var _7s=_7p[1],_7t=E(_7o);return _7t[0]==0?I_compareInt(_7s,_7t[1])<=0:I_compare(_7s,_7t[1])<=0;}},_7u=function(_7v){return [2];},_7w=function(_7x){var _7y=E(_7x);if(!_7y[0]){return E(_7u);}else{var _7z=_7y[1],_7A=E(_7y[2]);return _7A[0]==0?E(_7z):function(_7B){return new F(function(){return _3g(B(A(_7z,[_7B])),new T(function(){return B(A(new T(function(){return B(_7w(_7A));}),[_7B]));}));});};}},_7C=function(_7D){return [2];},_7E=function(_7F,_7G){var _7H=function(_7I,_7J){var _7K=E(_7I);if(!_7K[0]){return function(_7L){return new F(function(){return A(_7L,[_7F]);});};}else{var _7M=E(_7J);return _7M[0]==0?E(_7C):E(_7K[1])[1]!=E(_7M[1])[1]?E(_7C):function(_7N){return [0,function(_7O){return E(new T(function(){return B(A(new T(function(){return B(_7H(_7K[2],_7M[2]));}),[_7N]));}));}];};}};return function(_7P){return new F(function(){return A(_7H,[_7F,_7P,_7G]);});};},_7Q=new T(function(){return B(unCStr("SOH"));}),_7R=[0,1],_7S=function(_7T){return [1,B(_7E(_7Q,function(_7U){return E(new T(function(){return B(A(_7T,[_7R]));}));}))];},_7V=new T(function(){return B(unCStr("SO"));}),_7W=[0,14],_7X=function(_7Y){return [1,B(_7E(_7V,function(_7Z){return E(new T(function(){return B(A(_7Y,[_7W]));}));}))];},_80=function(_81){return [1,B(_4s(_7S,_7X,_81))];},_82=new T(function(){return B(unCStr("NUL"));}),_83=[0,0],_84=function(_85){return [1,B(_7E(_82,function(_86){return E(new T(function(){return B(A(_85,[_83]));}));}))];},_87=new T(function(){return B(unCStr("STX"));}),_88=[0,2],_89=function(_8a){return [1,B(_7E(_87,function(_8b){return E(new T(function(){return B(A(_8a,[_88]));}));}))];},_8c=new T(function(){return B(unCStr("ETX"));}),_8d=[0,3],_8e=function(_8f){return [1,B(_7E(_8c,function(_8g){return E(new T(function(){return B(A(_8f,[_8d]));}));}))];},_8h=new T(function(){return B(unCStr("EOT"));}),_8i=[0,4],_8j=function(_8k){return [1,B(_7E(_8h,function(_8l){return E(new T(function(){return B(A(_8k,[_8i]));}));}))];},_8m=new T(function(){return B(unCStr("ENQ"));}),_8n=[0,5],_8o=function(_8p){return [1,B(_7E(_8m,function(_8q){return E(new T(function(){return B(A(_8p,[_8n]));}));}))];},_8r=new T(function(){return B(unCStr("ACK"));}),_8s=[0,6],_8t=function(_8u){return [1,B(_7E(_8r,function(_8v){return E(new T(function(){return B(A(_8u,[_8s]));}));}))];},_8w=new T(function(){return B(unCStr("BEL"));}),_8x=[0,7],_8y=function(_8z){return [1,B(_7E(_8w,function(_8A){return E(new T(function(){return B(A(_8z,[_8x]));}));}))];},_8B=new T(function(){return B(unCStr("BS"));}),_8C=[0,8],_8D=function(_8E){return [1,B(_7E(_8B,function(_8F){return E(new T(function(){return B(A(_8E,[_8C]));}));}))];},_8G=new T(function(){return B(unCStr("HT"));}),_8H=[0,9],_8I=function(_8J){return [1,B(_7E(_8G,function(_8K){return E(new T(function(){return B(A(_8J,[_8H]));}));}))];},_8L=new T(function(){return B(unCStr("LF"));}),_8M=[0,10],_8N=function(_8O){return [1,B(_7E(_8L,function(_8P){return E(new T(function(){return B(A(_8O,[_8M]));}));}))];},_8Q=new T(function(){return B(unCStr("VT"));}),_8R=[0,11],_8S=function(_8T){return [1,B(_7E(_8Q,function(_8U){return E(new T(function(){return B(A(_8T,[_8R]));}));}))];},_8V=new T(function(){return B(unCStr("FF"));}),_8W=[0,12],_8X=function(_8Y){return [1,B(_7E(_8V,function(_8Z){return E(new T(function(){return B(A(_8Y,[_8W]));}));}))];},_90=new T(function(){return B(unCStr("CR"));}),_91=[0,13],_92=function(_93){return [1,B(_7E(_90,function(_94){return E(new T(function(){return B(A(_93,[_91]));}));}))];},_95=new T(function(){return B(unCStr("SI"));}),_96=[0,15],_97=function(_98){return [1,B(_7E(_95,function(_99){return E(new T(function(){return B(A(_98,[_96]));}));}))];},_9a=new T(function(){return B(unCStr("DLE"));}),_9b=[0,16],_9c=function(_9d){return [1,B(_7E(_9a,function(_9e){return E(new T(function(){return B(A(_9d,[_9b]));}));}))];},_9f=new T(function(){return B(unCStr("DC1"));}),_9g=[0,17],_9h=function(_9i){return [1,B(_7E(_9f,function(_9j){return E(new T(function(){return B(A(_9i,[_9g]));}));}))];},_9k=new T(function(){return B(unCStr("DC2"));}),_9l=[0,18],_9m=function(_9n){return [1,B(_7E(_9k,function(_9o){return E(new T(function(){return B(A(_9n,[_9l]));}));}))];},_9p=new T(function(){return B(unCStr("DC3"));}),_9q=[0,19],_9r=function(_9s){return [1,B(_7E(_9p,function(_9t){return E(new T(function(){return B(A(_9s,[_9q]));}));}))];},_9u=new T(function(){return B(unCStr("DC4"));}),_9v=[0,20],_9w=function(_9x){return [1,B(_7E(_9u,function(_9y){return E(new T(function(){return B(A(_9x,[_9v]));}));}))];},_9z=new T(function(){return B(unCStr("NAK"));}),_9A=[0,21],_9B=function(_9C){return [1,B(_7E(_9z,function(_9D){return E(new T(function(){return B(A(_9C,[_9A]));}));}))];},_9E=new T(function(){return B(unCStr("SYN"));}),_9F=[0,22],_9G=function(_9H){return [1,B(_7E(_9E,function(_9I){return E(new T(function(){return B(A(_9H,[_9F]));}));}))];},_9J=new T(function(){return B(unCStr("ETB"));}),_9K=[0,23],_9L=function(_9M){return [1,B(_7E(_9J,function(_9N){return E(new T(function(){return B(A(_9M,[_9K]));}));}))];},_9O=new T(function(){return B(unCStr("CAN"));}),_9P=[0,24],_9Q=function(_9R){return [1,B(_7E(_9O,function(_9S){return E(new T(function(){return B(A(_9R,[_9P]));}));}))];},_9T=new T(function(){return B(unCStr("EM"));}),_9U=[0,25],_9V=function(_9W){return [1,B(_7E(_9T,function(_9X){return E(new T(function(){return B(A(_9W,[_9U]));}));}))];},_9Y=new T(function(){return B(unCStr("SUB"));}),_9Z=[0,26],_a0=function(_a1){return [1,B(_7E(_9Y,function(_a2){return E(new T(function(){return B(A(_a1,[_9Z]));}));}))];},_a3=new T(function(){return B(unCStr("ESC"));}),_a4=[0,27],_a5=function(_a6){return [1,B(_7E(_a3,function(_a7){return E(new T(function(){return B(A(_a6,[_a4]));}));}))];},_a8=new T(function(){return B(unCStr("FS"));}),_a9=[0,28],_aa=function(_ab){return [1,B(_7E(_a8,function(_ac){return E(new T(function(){return B(A(_ab,[_a9]));}));}))];},_ad=new T(function(){return B(unCStr("GS"));}),_ae=[0,29],_af=function(_ag){return [1,B(_7E(_ad,function(_ah){return E(new T(function(){return B(A(_ag,[_ae]));}));}))];},_ai=new T(function(){return B(unCStr("RS"));}),_aj=[0,30],_ak=function(_al){return [1,B(_7E(_ai,function(_am){return E(new T(function(){return B(A(_al,[_aj]));}));}))];},_an=new T(function(){return B(unCStr("US"));}),_ao=[0,31],_ap=function(_aq){return [1,B(_7E(_an,function(_ar){return E(new T(function(){return B(A(_aq,[_ao]));}));}))];},_as=new T(function(){return B(unCStr("SP"));}),_at=[0,32],_au=function(_av){return [1,B(_7E(_as,function(_aw){return E(new T(function(){return B(A(_av,[_at]));}));}))];},_ax=new T(function(){return B(unCStr("DEL"));}),_ay=[0,127],_az=function(_aA){return [1,B(_7E(_ax,function(_aB){return E(new T(function(){return B(A(_aA,[_ay]));}));}))];},_aC=[1,_az,_9],_aD=[1,_au,_aC],_aE=[1,_ap,_aD],_aF=[1,_ak,_aE],_aG=[1,_af,_aF],_aH=[1,_aa,_aG],_aI=[1,_a5,_aH],_aJ=[1,_a0,_aI],_aK=[1,_9V,_aJ],_aL=[1,_9Q,_aK],_aM=[1,_9L,_aL],_aN=[1,_9G,_aM],_aO=[1,_9B,_aN],_aP=[1,_9w,_aO],_aQ=[1,_9r,_aP],_aR=[1,_9m,_aQ],_aS=[1,_9h,_aR],_aT=[1,_9c,_aS],_aU=[1,_97,_aT],_aV=[1,_92,_aU],_aW=[1,_8X,_aV],_aX=[1,_8S,_aW],_aY=[1,_8N,_aX],_aZ=[1,_8I,_aY],_b0=[1,_8D,_aZ],_b1=[1,_8y,_b0],_b2=[1,_8t,_b1],_b3=[1,_8o,_b2],_b4=[1,_8j,_b3],_b5=[1,_8e,_b4],_b6=[1,_89,_b5],_b7=[1,_84,_b6],_b8=[1,_80,_b7],_b9=new T(function(){return B(_7w(_b8));}),_ba=[0,1114111],_bb=[0,34],_bc=[0,39],_bd=function(_be){var _bf=new T(function(){return B(A(_be,[_8x]));}),_bg=new T(function(){return B(A(_be,[_8C]));}),_bh=new T(function(){return B(A(_be,[_8H]));}),_bi=new T(function(){return B(A(_be,[_8M]));}),_bj=new T(function(){return B(A(_be,[_8R]));}),_bk=new T(function(){return B(A(_be,[_8W]));}),_bl=new T(function(){return B(A(_be,[_91]));});return new F(function(){return _3g([0,function(_bm){switch(E(E(_bm)[1])){case 34:return E(new T(function(){return B(A(_be,[_bb]));}));case 39:return E(new T(function(){return B(A(_be,[_bc]));}));case 92:return E(new T(function(){return B(A(_be,[_73]));}));case 97:return E(_bf);case 98:return E(_bg);case 102:return E(_bk);case 110:return E(_bi);case 114:return E(_bl);case 116:return E(_bh);case 118:return E(_bj);default:return [2];}}],new T(function(){return B(_3g([1,B(_4s(_71,_74,function(_bn){return [1,B(_54(_bn,function(_bo){var _bp=B(_60(new T(function(){return B(_5Q(E(_bn)[1]));}),_5P,_bo));return !B(_7m(_bp,_ba))?[2]:B(A(_be,[new T(function(){var _bq=B(_7j(_bp));if(_bq>>>0>1114111){var _br=B(_7h(_bq));}else{var _br=[0,_bq];}var _bs=_br,_bt=_bs,_bu=_bt;return _bu;})]));}))];}))],new T(function(){return B(_3g([0,function(_bv){return E(E(_bv)[1])==94?E([0,function(_bw){switch(E(E(_bw)[1])){case 64:return E(new T(function(){return B(A(_be,[_83]));}));case 65:return E(new T(function(){return B(A(_be,[_7R]));}));case 66:return E(new T(function(){return B(A(_be,[_88]));}));case 67:return E(new T(function(){return B(A(_be,[_8d]));}));case 68:return E(new T(function(){return B(A(_be,[_8i]));}));case 69:return E(new T(function(){return B(A(_be,[_8n]));}));case 70:return E(new T(function(){return B(A(_be,[_8s]));}));case 71:return E(_bf);case 72:return E(_bg);case 73:return E(_bh);case 74:return E(_bi);case 75:return E(_bj);case 76:return E(_bk);case 77:return E(_bl);case 78:return E(new T(function(){return B(A(_be,[_7W]));}));case 79:return E(new T(function(){return B(A(_be,[_96]));}));case 80:return E(new T(function(){return B(A(_be,[_9b]));}));case 81:return E(new T(function(){return B(A(_be,[_9g]));}));case 82:return E(new T(function(){return B(A(_be,[_9l]));}));case 83:return E(new T(function(){return B(A(_be,[_9q]));}));case 84:return E(new T(function(){return B(A(_be,[_9v]));}));case 85:return E(new T(function(){return B(A(_be,[_9A]));}));case 86:return E(new T(function(){return B(A(_be,[_9F]));}));case 87:return E(new T(function(){return B(A(_be,[_9K]));}));case 88:return E(new T(function(){return B(A(_be,[_9P]));}));case 89:return E(new T(function(){return B(A(_be,[_9U]));}));case 90:return E(new T(function(){return B(A(_be,[_9Z]));}));case 91:return E(new T(function(){return B(A(_be,[_a4]));}));case 92:return E(new T(function(){return B(A(_be,[_a9]));}));case 93:return E(new T(function(){return B(A(_be,[_ae]));}));case 94:return E(new T(function(){return B(A(_be,[_aj]));}));case 95:return E(new T(function(){return B(A(_be,[_ao]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_b9,[_be]));})));})));}));});},_bx=function(_by){return new F(function(){return A(_by,[_4m]);});},_bz=function(_bA){var _bB=E(_bA);if(!_bB[0]){return E(_bx);}else{var _bC=_bB[2],_bD=E(E(_bB[1])[1]);switch(_bD){case 9:return function(_bE){return [0,function(_bF){return E(new T(function(){return B(A(new T(function(){return B(_bz(_bC));}),[_bE]));}));}];};case 10:return function(_bG){return [0,function(_bH){return E(new T(function(){return B(A(new T(function(){return B(_bz(_bC));}),[_bG]));}));}];};case 11:return function(_bI){return [0,function(_bJ){return E(new T(function(){return B(A(new T(function(){return B(_bz(_bC));}),[_bI]));}));}];};case 12:return function(_bK){return [0,function(_bL){return E(new T(function(){return B(A(new T(function(){return B(_bz(_bC));}),[_bK]));}));}];};case 13:return function(_bM){return [0,function(_bN){return E(new T(function(){return B(A(new T(function(){return B(_bz(_bC));}),[_bM]));}));}];};case 32:return function(_bO){return [0,function(_bP){return E(new T(function(){return B(A(new T(function(){return B(_bz(_bC));}),[_bO]));}));}];};case 160:return function(_bQ){return [0,function(_bR){return E(new T(function(){return B(A(new T(function(){return B(_bz(_bC));}),[_bQ]));}));}];};default:var _bS=u_iswspace(_bD),_bT=_bS;return E(_bT)==0?E(_bx):function(_bU){return [0,function(_bV){return E(new T(function(){return B(A(new T(function(){return B(_bz(_bC));}),[_bU]));}));}];};}}},_bW=function(_bX){var _bY=new T(function(){return B(_bW(_bX));}),_bZ=[1,function(_c0){return new F(function(){return A(_bz,[_c0,function(_c1){return E([0,function(_c2){return E(E(_c2)[1])==92?E(_bY):[2];}]);}]);});}];return new F(function(){return _3g([0,function(_c3){return E(E(_c3)[1])==92?E([0,function(_c4){var _c5=E(E(_c4)[1]);switch(_c5){case 9:return E(_bZ);case 10:return E(_bZ);case 11:return E(_bZ);case 12:return E(_bZ);case 13:return E(_bZ);case 32:return E(_bZ);case 38:return E(_bY);case 160:return E(_bZ);default:var _c6=u_iswspace(_c5),_c7=_c6;return E(_c7)==0?[2]:E(_bZ);}}]):[2];}],[0,function(_c8){var _c9=E(_c8);return E(_c9[1])==92?E(new T(function(){return B(_bd(function(_ca){return new F(function(){return A(_bX,[[0,_ca,_6V]]);});}));})):B(A(_bX,[[0,_c9,_6U]]));}]);});},_cb=function(_cc,_cd){return new F(function(){return _bW(function(_ce){var _cf=E(_ce),_cg=E(_cf[1]);if(E(_cg[1])==34){if(!E(_cf[2])){return E(new T(function(){return B(A(_cd,[[1,new T(function(){return B(A(_cc,[_9]));})]]));}));}else{return new F(function(){return _cb(function(_ch){return new F(function(){return A(_cc,[[1,_cg,_ch]]);});},_cd);});}}else{return new F(function(){return _cb(function(_ci){return new F(function(){return A(_cc,[[1,_cg,_ci]]);});},_cd);});}});});},_cj=new T(function(){return B(unCStr("_\'"));}),_ck=function(_cl){var _cm=u_iswalnum(_cl),_cn=_cm;return E(_cn)==0?B(_6A(_3Y,[0,_cl],_cj)):true;},_co=function(_cp){return new F(function(){return _ck(E(_cp)[1]);});},_cq=new T(function(){return B(unCStr(",;()[]{}`"));}),_cr=new T(function(){return B(unCStr(".."));}),_cs=new T(function(){return B(unCStr("::"));}),_ct=new T(function(){return B(unCStr("->"));}),_cu=[0,64],_cv=[1,_cu,_9],_cw=[0,126],_cx=[1,_cw,_9],_cy=new T(function(){return B(unCStr("=>"));}),_cz=[1,_cy,_9],_cA=[1,_cx,_cz],_cB=[1,_cv,_cA],_cC=[1,_ct,_cB],_cD=new T(function(){return B(unCStr("<-"));}),_cE=[1,_cD,_cC],_cF=[0,124],_cG=[1,_cF,_9],_cH=[1,_cG,_cE],_cI=[1,_73,_9],_cJ=[1,_cI,_cH],_cK=[0,61],_cL=[1,_cK,_9],_cM=[1,_cL,_cJ],_cN=[1,_cs,_cM],_cO=[1,_cr,_cN],_cP=function(_cQ){return new F(function(){return _3g([1,function(_cR){return E(_cR)[0]==0?E(new T(function(){return B(A(_cQ,[_4Z]));})):[2];}],new T(function(){return B(_3g([0,function(_cS){return E(E(_cS)[1])==39?E([0,function(_cT){var _cU=E(_cT);switch(E(_cU[1])){case 39:return [2];case 92:return E(new T(function(){return B(_bd(function(_cV){return [0,function(_cW){return E(E(_cW)[1])==39?E(new T(function(){return B(A(_cQ,[[0,_cV]]));})):[2];}];}));}));default:return [0,function(_cX){return E(E(_cX)[1])==39?E(new T(function(){return B(A(_cQ,[[0,_cU]]));})):[2];}];}}]):[2];}],new T(function(){return B(_3g([0,function(_cY){return E(E(_cY)[1])==34?E(new T(function(){return B(_cb(_50,_cQ));})):[2];}],new T(function(){return B(_3g([0,function(_cZ){return !B(_6A(_3Y,_cZ,_cq))?[2]:B(A(_cQ,[[2,[1,_cZ,_9]]]));}],new T(function(){return B(_3g([0,function(_d0){return !B(_6A(_3Y,_d0,_6F))?[2]:[1,B(_4O(_6G,function(_d1){var _d2=[1,_d0,_d1];return !B(_6A(_47,_d2,_cO))?B(A(_cQ,[[4,_d2]])):B(A(_cQ,[[2,_d2]]));}))];}],new T(function(){return B(_3g([0,function(_d3){var _d4=E(_d3),_d5=_d4[1],_d6=u_iswalpha(_d5),_d7=_d6;return E(_d7)==0?E(_d5)==95?[1,B(_4O(_co,function(_d8){return new F(function(){return A(_cQ,[[3,[1,_d4,_d8]]]);});}))]:[2]:[1,B(_4O(_co,function(_d9){return new F(function(){return A(_cQ,[[3,[1,_d4,_d9]]]);});}))];}],new T(function(){return [1,B(_4s(_6S,_6w,_cQ))];})));})));})));})));})));}));});},_da=[0,0],_db=function(_dc,_dd){return function(_de){return new F(function(){return A(_bz,[_de,function(_df){return E(new T(function(){return B(_cP(function(_dg){var _dh=E(_dg);return _dh[0]==2?!B(_3N(_dh[1],_3M))?[2]:E(new T(function(){return B(A(_dc,[_da,function(_di){return [1,function(_dj){return new F(function(){return A(_bz,[_dj,function(_dk){return E(new T(function(){return B(_cP(function(_dl){var _dm=E(_dl);return _dm[0]==2?!B(_3N(_dm[1],_3K))?[2]:E(new T(function(){return B(A(_dd,[_di]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_dn=function(_do,_dp,_dq){var _dr=function(_ds,_dt){return new F(function(){return _3g([1,function(_du){return new F(function(){return A(_bz,[_du,function(_dv){return E(new T(function(){return B(_cP(function(_dw){var _dx=E(_dw);if(_dx[0]==4){var _dy=E(_dx[1]);if(!_dy[0]){return new F(function(){return A(_do,[_dx,_ds,_dt]);});}else{return E(E(_dy[1])[1])==45?E(_dy[2])[0]==0?E([1,function(_dz){return new F(function(){return A(_bz,[_dz,function(_dA){return E(new T(function(){return B(_cP(function(_dB){return new F(function(){return A(_do,[_dB,_ds,function(_dC){return new F(function(){return A(_dt,[new T(function(){return B(_5K(_dC));})]);});}]);});}));}));}]);});}]):B(A(_do,[_dx,_ds,_dt])):B(A(_do,[_dx,_ds,_dt]));}}else{return new F(function(){return A(_do,[_dx,_ds,_dt]);});}}));}));}]);});}],new T(function(){return [1,B(_db(_dr,_dt))];}));});};return new F(function(){return _dr(_dp,_dq);});},_dD=function(_dE,_dF){return [2];},_dG=function(_dH){var _dI=E(_dH);return _dI[0]==0?[1,new T(function(){return B(_60(new T(function(){return B(_5Q(E(_dI[1])[1]));}),_5P,_dI[2]));})]:E(_dI[2])[0]==0?E(_dI[3])[0]==0?[1,new T(function(){return B(_60(_5O,_5P,_dI[1]));})]:[0]:[0];},_dJ=function(_dK){var _dL=E(_dK);if(_dL[0]==5){var _dM=B(_dG(_dL[1]));return _dM[0]==0?E(_dD):function(_dN,_dO){return new F(function(){return A(_dO,[_dM[1]]);});};}else{return E(_dD);}},_dP=function(_dQ){return [1,function(_dR){return new F(function(){return A(_bz,[_dR,function(_dS){return E([3,_dQ,_4j]);}]);});}];},_dT=new T(function(){return B(_dn(_dJ,_da,_dP));}),_dU=function(_dV){return E(E(_dV)[1]);},_dW=function(_dX,_dY,_dZ){var _e0=E(_dZ);return [3,[1,new T(function(){return B(A(_dU,[_dX,_e0[1]]));}),[1,new T(function(){return B(A(_dU,[_dY,_e0[2]]));}),_9]]];},_e1=function(_e2,_e3,_e4){return [3,new T(function(){return B(_U(function(_e5){return new F(function(){return _dW(_e2,_e3,_e5);});},_e4));})];},_e6=function(_e7,_e8){return [0,function(_e5){return new F(function(){return _dW(_e7,_e8,_e5);});},function(_e5){return new F(function(){return _e1(_e7,_e8,_e5);});},function(_e5){return new F(function(){return _1i(_e7,_e8,_e5);});},function(_e5){return new F(function(){return _1r(_e7,_e8,_e5);});}];},_e9=new T(function(){return B(unCStr("Tried to deserialize long string to a Char"));}),_ea=[0,_e9],_eb=new T(function(){return B(unCStr("Tried to deserialize a non-string to a Char"));}),_ec=[0,_eb],_ed=function(_ee){var _ef=E(_ee);if(_ef[0]==1){var _eg=fromJSStr(E(_ef[1])[1]);return _eg[0]==0?E(_ea):E(_eg[2])[0]==0?[1,_eg[1]]:E(_ea);}else{return E(_ec);}},_eh=[0,_1B],_ei=function(_ej){return new F(function(){return fromJSStr(E(_ej)[1]);});},_ek=function(_el){var _em=E(_el);return _em[0]==1?[1,new T(function(){return B(_ei(_em[1]));})]:E(_eh);},_en=function(_eo){return [1,new T(function(){return [0,toJSStr([1,_eo,_9])];})];},_ep=[0,_en,_7,_ed,_ek],_eq=function(_er){return E(E(_er)[2]);},_es=function(_et,_eu){return [3,new T(function(){return B(_U(new T(function(){return B(_eq(_et));}),_eu));})];},_ev=[1,_9],_ew=[0,_1c],_ex=function(_ey){return E(E(_ey)[4]);},_ez=function(_eA,_eB){var _eC=E(_eB);if(_eC[0]==3){var _eD=function(_eE){var _eF=E(_eE);if(!_eF[0]){return E(_ev);}else{var _eG=B(A(new T(function(){return B(_ex(_eA));}),[_eF[1]]));if(!_eG[0]){return [0,_eG[1]];}else{var _eH=B(_eD(_eF[2]));return _eH[0]==0?[0,_eH[1]]:[1,[1,_eG[1],_eH[1]]];}}};return new F(function(){return _eD(_eC[1]);});}else{return E(_ew);}},_eI=function(_eJ){return [0,new T(function(){return B(_eq(_eJ));}),function(_e5){return new F(function(){return _es(_eJ,_e5);});},new T(function(){return B(_ex(_eJ));}),function(_e5){return new F(function(){return _ez(_eJ,_e5);});}];},_eK=new T(function(){return B(_eI(_ep));}),_eL=new T(function(){return B(_e6(_eK,_eK));}),_eM=function(_eN){return [0,new T(function(){return [0,E(_eN)[1]];})];},_eO=function(_eP){return [3,new T(function(){return B(_U(_eM,_eP));})];},_eQ=new T(function(){return B(unCStr("The given Number can\'t be represented as an Int"));}),_eR=[0,_eQ],_eS=new T(function(){return B(unCStr("Tried to deserialize a non-Number to an Int"));}),_eT=[0,_eS],_eU=function(_eV){var _eW=E(_eV);if(!_eW[0]){var _eX=E(_eW[1])[1],_eY=_eX&4294967295;return _eY!=_eX?E(_eR):[1,[0,_eY]];}else{return E(_eT);}},_eZ=[0,_1c],_f0=[1,_9],_f1=[0,_eQ],_f2=[0,_eS],_f3=function(_f4){var _f5=E(_f4);if(!_f5[0]){return E(_f0);}else{var _f6=E(_f5[1]);if(!_f6[0]){var _f7=E(_f6[1])[1],_f8=_f7&4294967295;if(_f8!=_f7){return E(_f1);}else{var _f9=B(_f3(_f5[2]));return _f9[0]==0?[0,_f9[1]]:[1,[1,[0,_f8],_f9[1]]];}}else{return E(_f2);}}},_fa=function(_fb){var _fc=E(_fb);return _fc[0]==3?B(_f3(_fc[1])):E(_eZ);},_fd=[0,_eM,_eO,_eU,_fa],_fe=[2],_ff=function(_fg,_fh,_fi){var _fj=E(_fi);switch(_fj[0]){case 0:var _fk=_fj[1],_fl=_fj[2],_fm=_fj[3],_fn=_fj[4],_fo=_fl>>>0;if(((_fg>>>0&((_fo-1>>>0^4294967295)>>>0^_fo)>>>0)>>>0&4294967295)==_fk){return (_fg>>>0&_fo)>>>0==0?[0,_fk,_fl,E(B(_ff(_fg,_fh,_fm))),E(_fn)]:[0,_fk,_fl,E(_fm),E(B(_ff(_fg,_fh,_fn)))];}else{var _fp=(_fg>>>0^_fk>>>0)>>>0,_fq=(_fp|_fp>>>1)>>>0,_fr=(_fq|_fq>>>2)>>>0,_fs=(_fr|_fr>>>4)>>>0,_ft=(_fs|_fs>>>8)>>>0,_fu=(_ft|_ft>>>16)>>>0,_fv=(_fu^_fu>>>1)>>>0&4294967295,_fw=_fv>>>0;return (_fg>>>0&_fw)>>>0==0?[0,(_fg>>>0&((_fw-1>>>0^4294967295)>>>0^_fw)>>>0)>>>0&4294967295,_fv,E([1,_fg,_fh]),E(_fj)]:[0,(_fg>>>0&((_fw-1>>>0^4294967295)>>>0^_fw)>>>0)>>>0&4294967295,_fv,E(_fj),E([1,_fg,_fh])];}break;case 1:var _fx=_fj[1];if(_fg!=_fx){var _fy=(_fg>>>0^_fx>>>0)>>>0,_fz=(_fy|_fy>>>1)>>>0,_fA=(_fz|_fz>>>2)>>>0,_fB=(_fA|_fA>>>4)>>>0,_fC=(_fB|_fB>>>8)>>>0,_fD=(_fC|_fC>>>16)>>>0,_fE=(_fD^_fD>>>1)>>>0&4294967295,_fF=_fE>>>0;return (_fg>>>0&_fF)>>>0==0?[0,(_fg>>>0&((_fF-1>>>0^4294967295)>>>0^_fF)>>>0)>>>0&4294967295,_fE,E([1,_fg,_fh]),E(_fj)]:[0,(_fg>>>0&((_fF-1>>>0^4294967295)>>>0^_fF)>>>0)>>>0&4294967295,_fE,E(_fj),E([1,_fg,_fh])];}else{return [1,_fg,_fh];}break;default:return [1,_fg,_fh];}},_fG=function(_fH,_fI){while(1){var _fJ=E(_fI);if(!_fJ[0]){return E(_fH);}else{var _fK=E(_fJ[1]),_fL=B(_ff(E(_fK[1])[1],_fK[2],_fH));_fI=_fJ[2];_fH=_fL;continue;}}},_fM=function(_fN){return new F(function(){return _fG(_fe,_fN);});},_fO=function(_fP,_fQ,_fR){while(1){var _fS=E(_fR);if(!_fS[0]){return [0];}else{var _fT=E(_fS[1]);if(!B(A(_6y,[_fP,_fQ,_fT[1]]))){_fR=_fS[2];continue;}else{return [1,_fT[2]];}}}},_fU=function(_fV){while(1){var _fW=(function(_fX){var _fY=E(_fX);if(!_fY[0]){return [0];}else{var _fZ=_fY[2],_g0=E(_fY[1]);if(!E(_g0[2])[0]){return [1,_g0[1],new T(function(){return B(_fU(_fZ));})];}else{_fV=_fZ;return null;}}})(_fV);if(_fW!=null){return _fW;}}},_g1=function(_g2){var _g3=E(_g2);if(_g3[0]==4){var _g4=_g3[1],_g5=B(_fO(_1a,_3,_g4));if(!_g5[0]){return E(_1G);}else{var _g6=E(_g5[1]);if(!_g6[0]){var _g7=B(_fO(_1a,_2,_g4));if(!_g7[0]){return E(_1G);}else{var _g8=E(_g7[1]);if(!_g8[0]){var _g9=B(_fO(_1a,_1,_g4));if(!_g9[0]){return E(_1G);}else{var _ga=E(_g9[1]);if(!_ga[0]){var _gb=B(_fO(_1a,_0,_g4));if(!_gb[0]){return E(_1G);}else{var _gc=E(_gb[1]);if(_gc[0]==1){var _gd=B(_fO(_1a,_4,_g4));if(!_gd[0]){return E(_1G);}else{var _ge=B(_1r(_fd,_eL,_gd[1]));return _ge[0]==0?[0,_ge[1]]:[1,[0,_g6[1],_g8[1],_ga[1],new T(function(){var _gf=B(_fU(B(_36(_dT,new T(function(){return fromJSStr(E(_gc[1])[1]);})))));return _gf[0]==0?E(_1N):E(_gf[2])[0]==0?E(_gf[1]):E(_1L);}),_1J,_6U,new T(function(){return B(_fM(_ge[1]));})]];}}else{return E(_1C);}}}else{return E(_1E);}}}else{return E(_1E);}}}else{return E(_1E);}}}else{return E(_1I);}},_gg=new T(function(){return B(unCStr("Tried to deserialie a non-array to a list!"));}),_gh=[0,_gg],_gi=[1,_9],_gj=function(_gk){var _gl=E(_gk);if(!_gl[0]){return E(_gi);}else{var _gm=B(_g1(_gl[1]));if(!_gm[0]){return [0,_gm[1]];}else{var _gn=B(_gj(_gl[2]));return _gn[0]==0?[0,_gn[1]]:[1,[1,_gm[1],_gn[1]]];}}},_go=function(_gp){var _gq=E(_gp);return _gq[0]==3?B(_gj(_gq[1])):E(_gh);},_gr=[0,_R,_Y,_g1,_go],_gs=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_gt=new T(function(){return B(err(_gs));}),_gu=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_gv=new T(function(){return B(err(_gu));}),_gw=function(_gx,_gy){while(1){var _gz=E(_gx);if(!_gz[0]){return E(_gv);}else{var _gA=E(_gy);if(!_gA){return E(_gz[1]);}else{_gx=_gz[2];_gy=_gA-1|0;continue;}}}},_gB=new T(function(){return B(unCStr("ACK"));}),_gC=new T(function(){return B(unCStr("BEL"));}),_gD=new T(function(){return B(unCStr("BS"));}),_gE=new T(function(){return B(unCStr("SP"));}),_gF=[1,_gE,_9],_gG=new T(function(){return B(unCStr("US"));}),_gH=[1,_gG,_gF],_gI=new T(function(){return B(unCStr("RS"));}),_gJ=[1,_gI,_gH],_gK=new T(function(){return B(unCStr("GS"));}),_gL=[1,_gK,_gJ],_gM=new T(function(){return B(unCStr("FS"));}),_gN=[1,_gM,_gL],_gO=new T(function(){return B(unCStr("ESC"));}),_gP=[1,_gO,_gN],_gQ=new T(function(){return B(unCStr("SUB"));}),_gR=[1,_gQ,_gP],_gS=new T(function(){return B(unCStr("EM"));}),_gT=[1,_gS,_gR],_gU=new T(function(){return B(unCStr("CAN"));}),_gV=[1,_gU,_gT],_gW=new T(function(){return B(unCStr("ETB"));}),_gX=[1,_gW,_gV],_gY=new T(function(){return B(unCStr("SYN"));}),_gZ=[1,_gY,_gX],_h0=new T(function(){return B(unCStr("NAK"));}),_h1=[1,_h0,_gZ],_h2=new T(function(){return B(unCStr("DC4"));}),_h3=[1,_h2,_h1],_h4=new T(function(){return B(unCStr("DC3"));}),_h5=[1,_h4,_h3],_h6=new T(function(){return B(unCStr("DC2"));}),_h7=[1,_h6,_h5],_h8=new T(function(){return B(unCStr("DC1"));}),_h9=[1,_h8,_h7],_ha=new T(function(){return B(unCStr("DLE"));}),_hb=[1,_ha,_h9],_hc=new T(function(){return B(unCStr("SI"));}),_hd=[1,_hc,_hb],_he=new T(function(){return B(unCStr("SO"));}),_hf=[1,_he,_hd],_hg=new T(function(){return B(unCStr("CR"));}),_hh=[1,_hg,_hf],_hi=new T(function(){return B(unCStr("FF"));}),_hj=[1,_hi,_hh],_hk=new T(function(){return B(unCStr("VT"));}),_hl=[1,_hk,_hj],_hm=new T(function(){return B(unCStr("LF"));}),_hn=[1,_hm,_hl],_ho=new T(function(){return B(unCStr("HT"));}),_hp=[1,_ho,_hn],_hq=[1,_gD,_hp],_hr=[1,_gC,_hq],_hs=[1,_gB,_hr],_ht=new T(function(){return B(unCStr("ENQ"));}),_hu=[1,_ht,_hs],_hv=new T(function(){return B(unCStr("EOT"));}),_hw=[1,_hv,_hu],_hx=new T(function(){return B(unCStr("ETX"));}),_hy=[1,_hx,_hw],_hz=new T(function(){return B(unCStr("STX"));}),_hA=[1,_hz,_hy],_hB=new T(function(){return B(unCStr("SOH"));}),_hC=[1,_hB,_hA],_hD=new T(function(){return B(unCStr("NUL"));}),_hE=[1,_hD,_hC],_hF=[0,92],_hG=new T(function(){return B(unCStr("\\DEL"));}),_hH=new T(function(){return B(unCStr("\\a"));}),_hI=new T(function(){return B(unCStr("\\\\"));}),_hJ=new T(function(){return B(unCStr("\\SO"));}),_hK=new T(function(){return B(unCStr("\\r"));}),_hL=new T(function(){return B(unCStr("\\f"));}),_hM=new T(function(){return B(unCStr("\\v"));}),_hN=new T(function(){return B(unCStr("\\n"));}),_hO=new T(function(){return B(unCStr("\\t"));}),_hP=new T(function(){return B(unCStr("\\b"));}),_hQ=function(_hR,_hS){if(_hR<=127){var _hT=E(_hR);switch(_hT){case 92:return new F(function(){return _i(_hI,_hS);});break;case 127:return new F(function(){return _i(_hG,_hS);});break;default:if(_hT<32){var _hU=E(_hT);switch(_hU){case 7:return new F(function(){return _i(_hH,_hS);});break;case 8:return new F(function(){return _i(_hP,_hS);});break;case 9:return new F(function(){return _i(_hO,_hS);});break;case 10:return new F(function(){return _i(_hN,_hS);});break;case 11:return new F(function(){return _i(_hM,_hS);});break;case 12:return new F(function(){return _i(_hL,_hS);});break;case 13:return new F(function(){return _i(_hK,_hS);});break;case 14:return new F(function(){return _i(_hJ,new T(function(){var _hV=E(_hS);if(!_hV[0]){var _hW=[0];}else{var _hW=E(E(_hV[1])[1])==72?B(unAppCStr("\\&",_hV)):E(_hV);}return _hW;}));});break;default:return new F(function(){return _i([1,_hF,new T(function(){var _hX=_hU;return _hX>=0?B(_gw(_hE,_hX)):E(_gt);})],_hS);});}}else{return [1,[0,_hT],_hS];}}}else{return [1,_hF,new T(function(){var _hY=jsShowI(_hR),_hZ=_hY;return B(_i(fromJSStr(_hZ),new T(function(){var _i0=E(_hS);if(!_i0[0]){var _i1=[0];}else{var _i2=E(_i0[1])[1];if(_i2<48){var _i3=E(_i0);}else{var _i3=_i2>57?E(_i0):B(unAppCStr("\\&",_i0));}var _i4=_i3,_i5=_i4,_i1=_i5;}return _i1;})));})];}},_i6=[0,39],_i7=[1,_i6,_9],_i8=new T(function(){return B(unCStr("\'\\\'\'"));}),_i9=function(_ia){var _ib=E(E(_ia)[1]);return _ib==39?E(_i8):[1,_i6,new T(function(){return B(_hQ(_ib,_i7));})];},_ic=[0,34],_id=new T(function(){return B(unCStr("\\\""));}),_ie=function(_if,_ig){var _ih=E(_if);if(!_ih[0]){return E(_ig);}else{var _ii=_ih[2],_ij=E(E(_ih[1])[1]);if(_ij==34){return new F(function(){return _i(_id,new T(function(){return B(_ie(_ii,_ig));}));});}else{return new F(function(){return _hQ(_ij,new T(function(){return B(_ie(_ii,_ig));}));});}}},_ik=function(_il,_im){return [1,_ic,new T(function(){return B(_ie(_il,[1,_ic,_im]));})];},_in=function(_io){return new F(function(){return _i(_i8,_io);});},_ip=function(_iq,_ir){var _is=E(E(_ir)[1]);return _is==39?E(_in):function(_it){return [1,_i6,new T(function(){return B(_hQ(_is,[1,_i6,_it]));})];};},_iu=[0,_ip,_i9,_ik],_iv=function(_iw){return E(E(_iw)[3]);},_ix=function(_iy,_iz){return new F(function(){return A(_iv,[_iy,_iz,_9]);});},_iA=function(_iB,_iC,_iD){return new F(function(){return _2l(new T(function(){return B(_iv(_iB));}),_iC,_iD);});},_iE=function(_iF){return [0,function(_iG){return E(new T(function(){return B(_iv(_iF));}));},function(_io){return new F(function(){return _ix(_iF,_io);});},function(_iH,_io){return new F(function(){return _iA(_iF,_iH,_io);});}];},_iI=new T(function(){return B(_iE(_iu));}),_iJ=function(_iK,_iL,_iM){return new F(function(){return A(_iK,[[1,_2i,new T(function(){return B(A(_iL,[_iM]));})]]);});},_iN=new T(function(){return B(unCStr(": empty list"));}),_iO=new T(function(){return B(unCStr("Prelude."));}),_iP=function(_iQ){return new F(function(){return err(B(_i(_iO,new T(function(){return B(_i(_iQ,_iN));}))));});},_iR=new T(function(){return B(unCStr("foldr1"));}),_iS=new T(function(){return B(_iP(_iR));}),_iT=function(_iU,_iV){var _iW=E(_iV);if(!_iW[0]){return E(_iS);}else{var _iX=_iW[1],_iY=E(_iW[2]);if(!_iY[0]){return E(_iX);}else{return new F(function(){return A(_iU,[_iX,new T(function(){return B(_iT(_iU,_iY));})]);});}}},_iZ=[1,_A,_9],_j0=[0,0],_j1=function(_j2){return E(E(_j2)[1]);},_j3=function(_j4,_j5,_j6){var _j7=E(_j6);return [1,_B,new T(function(){return B(A(_iT,[_iJ,[1,new T(function(){return B(A(_j1,[_j4,_j0,_j7[1]]));}),[1,new T(function(){return B(A(_j1,[_j5,_j0,_j7[2]]));}),_9]],_iZ]));})];},_j8=function(_j9,_ja,_jb,_jc){return new F(function(){return _2l(function(_jd,_je){var _jf=E(_jd);return [1,_B,new T(function(){return B(A(_iT,[_iJ,[1,new T(function(){return B(A(new T(function(){return B(_j1(_j9));}),[_j0,_jf[1]]));}),[1,new T(function(){return B(A(new T(function(){return B(_j1(_ja));}),[_j0,_jf[2]]));}),_9]],[1,_A,_je]]));})];},_jb,_jc);});},_jg=function(_jh,_ji,_jj,_jk,_jl){var _jm=E(_jk);return [1,_B,new T(function(){return B(A(_iT,[_iJ,[1,new T(function(){return B(A(_j1,[_jh,_j0,_jm[1]]));}),[1,new T(function(){return B(A(_j1,[_ji,_j0,_jm[2]]));}),_9]],[1,_A,_jl]]));})];},_jn=function(_jo,_jp){return [0,function(_jq,_iH,_io){return new F(function(){return _jg(_jo,_jp,_jq,_iH,_io);});},function(_io){return new F(function(){return _j3(_jo,_jp,_io);});},function(_iH,_io){return new F(function(){return _j8(_jo,_jp,_iH,_io);});}];},_jr=new T(function(){return B(_jn(_iI,_iI));}),_js=function(_jt){var _ju=jsShow(E(_jt)[1]),_jv=_ju;return new F(function(){return fromJSStr(_jv);});},_jw=function(_jx){return function(_4L){return new F(function(){return _i(new T(function(){return B(_js(_jx));}),_4L);});};},_jy=function(_jz){return new F(function(){return _7b(0,E(_jz)[1],_9);});},_jA=function(_jB,_jC){return new F(function(){return _7b(0,E(_jB)[1],_jC);});},_jD=function(_jE,_jF){return new F(function(){return _2l(_jA,_jE,_jF);});},_jG=function(_jH,_jI,_jJ){return new F(function(){return _7b(E(_jH)[1],E(_jI)[1],_jJ);});},_jK=[0,_jG,_jy,_jD],_jL=new T(function(){return B(unCStr("fromList "));}),_jM=function(_jN,_jO){while(1){var _jP=(function(_jQ,_jR){var _jS=E(_jR);switch(_jS[0]){case 0:_jN=new T(function(){return B(_jM(_jQ,_jS[4]));});_jO=_jS[3];return null;case 1:return [1,[0,[0,_jS[1]],_jS[2]],_jQ];default:return E(_jQ);}})(_jN,_jO);if(_jP!=null){return _jP;}}},_jT=function(_jU){var _jV=E(_jU);if(!_jV[0]){var _jW=_jV[3],_jX=_jV[4];return _jV[2]>=0?B(_jM(new T(function(){return B(_jM(_9,_jX));}),_jW)):B(_jM(new T(function(){return B(_jM(_9,_jW));}),_jX));}else{return new F(function(){return _jM(_9,_jV);});}},_jY=function(_jZ,_k0,_k1){var _k2=new T(function(){return B(_jT(_k1));});return _k0<=10?function(_k3){return new F(function(){return _i(_jL,new T(function(){return B(_j8(_jK,_jZ,_k2,_k3));}));});}:function(_k4){return [1,_B,new T(function(){return B(_i(_jL,new T(function(){return B(_j8(_jK,_jZ,_k2,[1,_A,_k4]));})));})];};},_k5=[0,45],_k6=function(_k7,_k8,_k9){var _ka=function(_kb){var _kc=new T(function(){return B(A(_k7,[[0, -_k9]]));});return E(_k8)[1]<=6?function(_kd){return [1,_k5,new T(function(){return B(A(_kc,[_kd]));})];}:function(_ke){return [1,_B,[1,_k5,new T(function(){return B(A(_kc,[[1,_A,_ke]]));})]];};};if(_k9>=0){var _kf=isDoubleNegativeZero(_k9),_kg=_kf;return E(_kg)==0?B(A(_k7,[[0,_k9]])):B(_ka(_));}else{return new F(function(){return _ka(_);});}},_kh=new T(function(){return B(unCStr("Aichan {"));}),_ki=new T(function(){return B(unCStr("_loves = "));}),_kj=[0,125],_kk=[0,0],_kl=new T(function(){return B(unCStr(", "));}),_km=new T(function(){return B(unCStr("_lps = "));}),_kn=new T(function(){return B(unCStr("_depend = "));}),_ko=new T(function(){return B(unCStr("_lastFocus = "));}),_kp=new T(function(){return B(unCStr("_interval = "));}),_kq=new T(function(){return B(unCStr("_hasFocus = "));}),_kr=new T(function(){return B(unCStr("_achieves = "));}),_ks=new T(function(){return B(unCStr("True"));}),_kt=new T(function(){return B(unCStr("False"));}),_ku=function(_kv,_kw,_kx,_ky,_kz,_kA,_kB,_kC){var _kD=function(_kE){return new F(function(){return _i(_ki,new T(function(){return B(A(new T(function(){return B(_k6(_jw,_kk,E(_kw)[1]));}),[new T(function(){return B(_i(_kl,new T(function(){return B(_i(_km,new T(function(){return B(A(new T(function(){return B(_k6(_jw,_kk,E(_kx)[1]));}),[new T(function(){return B(_i(_kl,new T(function(){return B(_i(_kn,new T(function(){return B(A(new T(function(){return B(_k6(_jw,_kk,E(_ky)[1]));}),[new T(function(){return B(_i(_kl,new T(function(){return B(_i(_ko,new T(function(){return B(_D(0,_kz,new T(function(){return B(_i(_kl,new T(function(){return B(_i(_kp,new T(function(){return B(_D(0,_kA,new T(function(){return B(_i(_kl,new T(function(){return B(_i(_kq,new T(function(){var _kF=new T(function(){return B(_i(_kl,new T(function(){return B(_i(_kr,new T(function(){return B(A(new T(function(){return B(_jY(_jr,0,_kC));}),[[1,_kj,_kE]]));})));})));});return !E(_kB)?B(_i(_kt,_kF)):B(_i(_ks,_kF));})));})));})));})));})));})));})));})));})]));})));})));})]));})));})));})]));}));});};return _kv<11?function(_kG){return new F(function(){return _i(_kh,new T(function(){return B(_kD(_kG));}));});}:function(_kH){return [1,_B,new T(function(){return B(_i(_kh,new T(function(){return B(_kD([1,_A,_kH]));})));})];};},_kI=function(_kJ){var _kK=E(_kJ);return new F(function(){return A(_ku,[0,_kK[1],_kK[2],_kK[3],_kK[4],_kK[5],_kK[6],_kK[7],_9]);});},_kL=function(_kM,_kN,_kO,_){var _kP=rMV(_kN),_kQ=_kP,_kR=B(A(_kO,[_kQ,_])),_kS=_kR,_=wMV(_kN,new T(function(){return E(E(_kS)[2]);})),_kT=jsSetTimeout(_kM,function(_){var _kU=B(_kL(_kM,_kN,_kO,_)),_kV=_kU;return _4m;});return new F(function(){return rMV(_kN);});},_kW=function(_){var _kX=jsEval("Date.now()"),_kY=_kX;return new T(function(){var _kZ=B(_fU(B(_36(_dT,new T(function(){return fromJSStr(_kY);})))));return _kZ[0]==0?E(_1N):E(_kZ[2])[0]==0?E(_kZ[1]):E(_1L);});},_l0=function(_l1){return E(_l1);},_l2=new T(function(){return B(unCStr("<tr><td>%s</td><td>%s</td></tr>"));}),_l3=function(_l4,_l5){while(1){var _l6=E(_l4);if(!_l6[0]){return E(_l5);}else{_l4=_l6[2];var _l7=[1,_l6[1],_l5];_l5=_l7;continue;}}},_l8=function(_l9){var _la=E(_l9)[1];return [0,Math.log(_la+(_la+1)*Math.sqrt((_la-1)/(_la+1)))];},_lb=function(_lc){var _ld=E(_lc)[1];return [0,Math.log(_ld+Math.sqrt(1+_ld*_ld))];},_le=function(_lf){var _lg=E(_lf)[1];return [0,0.5*Math.log((1+_lg)/(1-_lg))];},_lh=function(_li,_lj){return [0,Math.log(E(_lj)[1])/Math.log(E(_li)[1])];},_lk=[0,3.141592653589793],_ll=new T(function(){return [0,0/0];}),_lm=new T(function(){return [0,-1/0];}),_ln=new T(function(){return [0,1/0];}),_lo=[0,0],_lp=function(_lq,_lr){while(1){var _ls=E(_lq);if(!_ls[0]){_lq=[1,I_fromInt(_ls[1])];continue;}else{var _lt=E(_lr);if(!_lt[0]){_lq=_ls;_lr=[1,I_fromInt(_lt[1])];continue;}else{return new F(function(){return I_fromRat(_ls[1],_lt[1]);});}}}},_lu=function(_lv,_lw){var _lx=E(_lv);if(!_lx[0]){var _ly=_lx[1],_lz=E(_lw);return _lz[0]==0?_ly==_lz[1]:I_compareInt(_lz[1],_ly)==0?true:false;}else{var _lA=_lx[1],_lB=E(_lw);return _lB[0]==0?I_compareInt(_lA,_lB[1])==0?true:false:I_compare(_lA,_lB[1])==0?true:false;}},_lC=function(_lD,_lE){return !B(_lu(_lE,_lo))?[0,B(_lp(_lD,_lE))]:!B(_lu(_lD,_lo))?!B(_s(_lD,_lo))?E(_ln):E(_lm):E(_ll);},_lF=function(_lG){var _lH=E(_lG);return new F(function(){return _lC(_lH[1],_lH[2]);});},_lI=function(_lJ){return [0,1/E(_lJ)[1]];},_lK=function(_lL){var _lM=E(_lL),_lN=_lM[1];return _lN<0?[0, -_lN]:E(_lM);},_lO=function(_lP){var _lQ=E(_lP);return _lQ[0]==0?_lQ[1]:I_toNumber(_lQ[1]);},_lR=function(_lS){return [0,B(_lO(_lS))];},_lT=[0,0],_lU=[0,1],_lV=[0,-1],_lW=function(_lX){var _lY=E(E(_lX)[1]);return _lY==0?E(_lT):_lY<=0?E(_lV):E(_lU);},_lZ=function(_m0,_m1){return [0,E(_m0)[1]-E(_m1)[1]];},_m2=function(_m3){return [0, -E(_m3)[1]];},_m4=function(_m5,_m6){return [0,E(_m5)[1]+E(_m6)[1]];},_m7=function(_m8,_m9){return [0,E(_m8)[1]*E(_m9)[1]];},_ma=[0,_m4,_m7,_lZ,_m2,_lK,_lW,_lR],_mb=function(_mc,_md){return [0,E(_mc)[1]/E(_md)[1]];},_me=[0,_ma,_mb,_lI,_lF],_mf=function(_mg){return [0,Math.acos(E(_mg)[1])];},_mh=function(_mi){return [0,Math.asin(E(_mi)[1])];},_mj=function(_mk){return [0,Math.atan(E(_mk)[1])];},_ml=function(_mm){return [0,Math.cos(E(_mm)[1])];},_mn=function(_mo){return [0,cosh(E(_mo)[1])];},_mp=function(_mq){return [0,Math.exp(E(_mq)[1])];},_mr=function(_ms){return [0,Math.log(E(_ms)[1])];},_mt=function(_mu,_mv){return [0,Math.pow(E(_mu)[1],E(_mv)[1])];},_mw=function(_mx){return [0,Math.sin(E(_mx)[1])];},_my=function(_mz){return [0,sinh(E(_mz)[1])];},_mA=function(_mB){return [0,Math.sqrt(E(_mB)[1])];},_mC=function(_mD){return [0,Math.tan(E(_mD)[1])];},_mE=function(_mF){return [0,tanh(E(_mF)[1])];},_mG=[0,_me,_lk,_mp,_mA,_mr,_mt,_lh,_mw,_mC,_ml,_mh,_mj,_mf,_my,_mE,_mn,_lb,_le,_l8],_mH=function(_mI){var _mJ=E(_mI)[1];return [0,Math.log(_mJ+(_mJ+1)*Math.sqrt((_mJ-1)/(_mJ+1)))];},_mK=function(_mL){var _mM=E(_mL)[1];return [0,Math.log(_mM+Math.sqrt(1+_mM*_mM))];},_mN=function(_mO){var _mP=E(_mO)[1];return [0,0.5*Math.log((1+_mP)/(1-_mP))];},_mQ=function(_mR,_mS){return [0,Math.log(E(_mS)[1])/Math.log(E(_mR)[1])];},_mT=[0,3.141592653589793],_mU=new T(function(){return [0,0/0];}),_mV=new T(function(){return [0,-1/0];}),_mW=new T(function(){return [0,1/0];}),_mX=function(_mY,_mZ){return !B(_lu(_mZ,_lo))?[0,B(_lp(_mY,_mZ))]:!B(_lu(_mY,_lo))?!B(_s(_mY,_lo))?E(_mW):E(_mV):E(_mU);},_n0=function(_n1){var _n2=E(_n1);return new F(function(){return _mX(_n2[1],_n2[2]);});},_n3=function(_n4){return [0,1/E(_n4)[1]];},_n5=function(_n6){var _n7=E(_n6),_n8=_n7[1];return _n8<0?[0, -_n8]:E(_n7);},_n9=function(_na){var _nb=E(_na);return _nb[0]==0?_nb[1]:I_toNumber(_nb[1]);},_nc=function(_nd){return [0,B(_n9(_nd))];},_ne=[0,0],_nf=[0,1],_ng=[0,-1],_nh=function(_ni){var _nj=E(E(_ni)[1]);return _nj==0?E(_ne):_nj<=0?E(_ng):E(_nf);},_nk=function(_nl,_nm){return [0,E(_nl)[1]-E(_nm)[1]];},_nn=function(_no){return [0, -E(_no)[1]];},_np=function(_nq,_nr){return [0,E(_nq)[1]+E(_nr)[1]];},_ns=function(_nt,_nu){return [0,E(_nt)[1]*E(_nu)[1]];},_nv=[0,_np,_ns,_nk,_nn,_n5,_nh,_nc],_nw=function(_nx,_ny){return [0,E(_nx)[1]/E(_ny)[1]];},_nz=[0,_nv,_nw,_n3,_n0],_nA=function(_nB){return [0,Math.acos(E(_nB)[1])];},_nC=function(_nD){return [0,Math.asin(E(_nD)[1])];},_nE=function(_nF){return [0,Math.atan(E(_nF)[1])];},_nG=function(_nH){return [0,Math.cos(E(_nH)[1])];},_nI=function(_nJ){return [0,cosh(E(_nJ)[1])];},_nK=function(_nL){return [0,Math.exp(E(_nL)[1])];},_nM=function(_nN){return [0,Math.log(E(_nN)[1])];},_nO=function(_nP,_nQ){return [0,Math.pow(E(_nP)[1],E(_nQ)[1])];},_nR=function(_nS){return [0,Math.sin(E(_nS)[1])];},_nT=function(_nU){return [0,sinh(E(_nU)[1])];},_nV=function(_nW){return [0,Math.sqrt(E(_nW)[1])];},_nX=function(_nY){return [0,Math.tan(E(_nY)[1])];},_nZ=function(_o0){return [0,tanh(E(_o0)[1])];},_o1=[0,_nz,_mT,_nK,_nV,_nM,_nO,_mQ,_nR,_nX,_nG,_nC,_nE,_nA,_nT,_nZ,_nI,_mK,_mN,_mH],_o2=function(_o3){var _o4=I_decodeDouble(_o3);return [0,[1,_o4[2]],_o4[1]];},_o5=function(_o6){var _o7=B(_o2(E(_o6)[1]));return [0,_o7[1],[0,_o7[2]]];},_o8=[0,53],_o9=function(_oa){return E(_o8);},_ob=[0,2],_oc=function(_od){return E(_ob);},_oe=[0,1024],_of=[0,-1021],_og=[0,_of,_oe],_oh=function(_oi){return E(_og);},_oj=function(_ok){var _ol=isDoubleInfinite(E(_ok)[1]),_om=_ol;return E(_om)==0?false:true;},_on=function(_oo){var _op=isDoubleNaN(E(_oo)[1]),_oq=_op;return E(_oq)==0?false:true;},_or=function(_os){var _ot=isDoubleNegativeZero(E(_os)[1]),_ou=_ot;return E(_ou)==0?false:true;},_ov=function(_ow){var _ox=decodeFloat(E(_ow)[1]);return [0,new T(function(){return B(_5Q(_ox[1]));}),[0,_ox[2]]];},_oy=[0,24],_oz=function(_oA){return E(_oy);},_oB=function(_oC){return E(_ob);},_oD=[0,128],_oE=[0,-125],_oF=[0,_oE,_oD],_oG=function(_oH){return E(_oF);},_oI=function(_oJ){var _oK=isFloatInfinite(E(_oJ)[1]),_oL=_oK;return E(_oL)==0?false:true;},_oM=function(_oN){var _oO=isFloatNaN(E(_oN)[1]),_oP=_oO;return E(_oP)==0?false:true;},_oQ=function(_oR){var _oS=isFloatNegativeZero(E(_oR)[1]),_oT=_oS;return E(_oT)==0?false:true;},_oU=function(_oV,_oW){return E(_oV)[1]!=E(_oW)[1]?true:false;},_oX=function(_oY,_oZ){return E(_oY)[1]==E(_oZ)[1];},_p0=[0,_oX,_oU],_p1=function(_p2,_p3){return E(_p2)[1]<E(_p3)[1];},_p4=function(_p5,_p6){return E(_p5)[1]<=E(_p6)[1];},_p7=function(_p8,_p9){return E(_p8)[1]>E(_p9)[1];},_pa=function(_pb,_pc){return E(_pb)[1]>=E(_pc)[1];},_pd=function(_pe,_pf){var _pg=E(_pe)[1],_ph=E(_pf)[1];return _pg>=_ph?_pg!=_ph?2:1:0;},_pi=function(_pj,_pk){var _pl=E(_pj),_pm=E(_pk);return _pl[1]>_pm[1]?E(_pl):E(_pm);},_pn=function(_po,_pp){var _pq=E(_po),_pr=E(_pp);return _pq[1]>_pr[1]?E(_pr):E(_pq);},_ps=[0,_p0,_pd,_p1,_pa,_p7,_p4,_pi,_pn],_pt=[0,1],_pu=function(_pv){var _pw=hs_intToInt64(2147483647),_px=_pw,_py=hs_leInt64(_pv,_px),_pz=_py;if(!E(_pz)){return [1,I_fromInt64(_pv)];}else{var _pA=hs_intToInt64(-2147483648),_pB=_pA,_pC=hs_geInt64(_pv,_pB),_pD=_pC;if(!E(_pD)){return [1,I_fromInt64(_pv)];}else{var _pE=hs_int64ToInt(_pv),_pF=_pE;return new F(function(){return _5Q(_pF);});}}},_pG=new T(function(){var _pH=newByteArr(256),_pI=_pH,_=_pI["v"]["i8"][0]=8,_=B((function(_pJ,_pK,_pL,_){while(1){if(_pL>=256){if(_pJ>=256){return E(_);}else{var _pM=imul(2,_pJ)|0,_pN=_pK+1|0,_pO=_pJ;_pJ=_pM;_pK=_pN;_pL=_pO;continue;}}else{var _=_pI["v"]["i8"][_pL]=_pK,_pO=_pL+_pJ|0;_pL=_pO;continue;}}})(2,0,1,_)),_pP=_pI,_pQ=_pP;return [0,_pQ];}),_pR=function(_pS,_pT){while(1){var _pU=(function(_pV,_pW){var _pX=hs_int64ToInt(_pV),_pY=_pX,_pZ=E(_pG)[1]["v"]["i8"][(255&_pY>>>0)>>>0&4294967295];if(_pW>_pZ){if(_pZ>=8){var _q0=hs_uncheckedIShiftRA64(_pV,8),_q1=_q0;_pS=_q1;var _q2=_pW-8|0;_pT=_q2;return null;}else{return [0,new T(function(){var _q3=hs_uncheckedIShiftRA64(_pV,_pZ),_q4=_q3;return B(_pu(_q4));}),_pW-_pZ|0];}}else{return [0,new T(function(){var _q5=hs_uncheckedIShiftRA64(_pV,_pW),_q6=_q5;return B(_pu(_q6));}),0];}})(_pS,_pT);if(_pU!=null){return _pU;}}},_q7=function(_q8){var _q9=hs_intToInt64(_q8),_qa=_q9;return E(_qa);},_qb=function(_qc){var _qd=E(_qc);return _qd[0]==0?B(_q7(_qd[1])):I_toInt64(_qd[1]);},_qe=function(_qf){return I_toInt(_qf)>>>0;},_qg=function(_qh){var _qi=E(_qh);return _qi[0]==0?_qi[1]>>>0:B(_qe(_qi[1]));},_qj=function(_qk,_ql){while(1){var _qm=E(_qk);if(!_qm[0]){_qk=[1,I_fromInt(_qm[1])];continue;}else{return [1,I_shiftLeft(_qm[1],_ql)];}}},_qn=function(_qo){var _qp=B(_o2(_qo)),_qq=_qp[1],_qr=_qp[2];if(_qr<0){var _qs=function(_qt){if(!_qt){return [0,E(_qq),B(_qj(_pt, -_qr))];}else{var _qu=B(_pR(B(_qb(_qq)), -_qr));return [0,E(_qu[1]),B(_qj(_pt,_qu[2]))];}};return (B(_qg(_qq))&1)>>>0==0?B(_qs(1)):B(_qs(0));}else{return [0,B(_qj(_qq,_qr)),_pt];}},_qv=function(_qw){var _qx=B(_qn(E(_qw)[1]));return [0,E(_qx[1]),E(_qx[2])];},_qy=[0,_ma,_ps,_qv],_qz=function(_qA){return E(E(_qA)[1]);},_qB=[0,1],_qC=function(_qD,_qE){if(_qD<=_qE){var _qF=function(_qG){return [1,[0,_qG],new T(function(){if(_qG!=_qE){var _qH=B(_qF(_qG+1|0));}else{var _qH=[0];}var _qI=_qH;return _qI;})];};return new F(function(){return _qF(_qD);});}else{return [0];}},_qJ=function(_qK){return new F(function(){return _qC(E(_qK)[1],2147483647);});},_qL=function(_qM,_qN,_qO){return _qO<=_qN?[1,[0,_qM],new T(function(){var _qP=_qN-_qM|0,_qQ=function(_qR){return _qR>=(_qO-_qP|0)?[1,[0,_qR],new T(function(){return B(_qQ(_qR+_qP|0));})]:[1,[0,_qR],_9];};return B(_qQ(_qN));})]:_qO<=_qM?[1,[0,_qM],_9]:[0];},_qS=function(_qT,_qU,_qV){return _qV>=_qU?[1,[0,_qT],new T(function(){var _qW=_qU-_qT|0,_qX=function(_qY){return _qY<=(_qV-_qW|0)?[1,[0,_qY],new T(function(){return B(_qX(_qY+_qW|0));})]:[1,[0,_qY],_9];};return B(_qX(_qU));})]:_qV>=_qT?[1,[0,_qT],_9]:[0];},_qZ=function(_r0,_r1){return _r1<_r0?B(_qL(_r0,_r1,-2147483648)):B(_qS(_r0,_r1,2147483647));},_r2=function(_r3,_r4){return new F(function(){return _qZ(E(_r3)[1],E(_r4)[1]);});},_r5=function(_r6,_r7,_r8){return _r7<_r6?B(_qL(_r6,_r7,_r8)):B(_qS(_r6,_r7,_r8));},_r9=function(_ra,_rb,_rc){return new F(function(){return _r5(E(_ra)[1],E(_rb)[1],E(_rc)[1]);});},_rd=function(_re,_rf){return new F(function(){return _qC(E(_re)[1],E(_rf)[1]);});},_rg=function(_rh){return E(_rh);},_ri=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_rj=new T(function(){return B(err(_ri));}),_rk=function(_rl){var _rm=E(E(_rl)[1]);return _rm==(-2147483648)?E(_rj):[0,_rm-1|0];},_rn=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_ro=new T(function(){return B(err(_rn));}),_rp=function(_rq){var _rr=E(E(_rq)[1]);return _rr==2147483647?E(_ro):[0,_rr+1|0];},_rs=[0,_rp,_rk,_rg,_rg,_qJ,_r2,_rd,_r9],_rt=function(_ru,_rv){if(_ru<=0){if(_ru>=0){return new F(function(){return quot(_ru,_rv);});}else{if(_rv<=0){return new F(function(){return quot(_ru,_rv);});}else{return quot(_ru+1|0,_rv)-1|0;}}}else{if(_rv>=0){if(_ru>=0){return new F(function(){return quot(_ru,_rv);});}else{if(_rv<=0){return new F(function(){return quot(_ru,_rv);});}else{return quot(_ru+1|0,_rv)-1|0;}}}else{return quot(_ru-1|0,_rv)-1|0;}}},_rw=new T(function(){return B(unCStr("ArithException"));}),_rx=new T(function(){return B(unCStr("GHC.Exception"));}),_ry=new T(function(){return B(unCStr("base"));}),_rz=new T(function(){var _rA=hs_wordToWord64(4194982440),_rB=_rA,_rC=hs_wordToWord64(3110813675),_rD=_rC;return [0,_rB,_rD,[0,_rB,_rD,_ry,_rx,_rw],_9];}),_rE=function(_rF){return E(_rz);},_rG=function(_rH){var _rI=E(_rH);return new F(function(){return _20(B(_1Y(_rI[1])),_rE,_rI[2]);});},_rJ=new T(function(){return B(unCStr("arithmetic underflow"));}),_rK=new T(function(){return B(unCStr("arithmetic overflow"));}),_rL=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_rM=new T(function(){return B(unCStr("denormal"));}),_rN=new T(function(){return B(unCStr("divide by zero"));}),_rO=new T(function(){return B(unCStr("loss of precision"));}),_rP=function(_rQ){switch(E(_rQ)){case 0:return E(_rK);case 1:return E(_rJ);case 2:return E(_rO);case 3:return E(_rN);case 4:return E(_rM);default:return E(_rL);}},_rR=function(_rS){return new F(function(){return _i(_rJ,_rS);});},_rT=function(_rS){return new F(function(){return _i(_rK,_rS);});},_rU=function(_rS){return new F(function(){return _i(_rL,_rS);});},_rV=function(_rS){return new F(function(){return _i(_rM,_rS);});},_rW=function(_rS){return new F(function(){return _i(_rN,_rS);});},_rX=function(_rS){return new F(function(){return _i(_rO,_rS);});},_rY=function(_rZ){switch(E(_rZ)){case 0:return E(_rT);case 1:return E(_rR);case 2:return E(_rX);case 3:return E(_rW);case 4:return E(_rV);default:return E(_rU);}},_s0=function(_s1,_s2){return new F(function(){return _2l(_rY,_s1,_s2);});},_s3=function(_s4,_s5){switch(E(_s5)){case 0:return E(_rT);case 1:return E(_rR);case 2:return E(_rX);case 3:return E(_rW);case 4:return E(_rV);default:return E(_rU);}},_s6=[0,_s3,_rP,_s0],_s7=new T(function(){return [0,_rE,_s6,_s8,_rG];}),_s8=function(_rS){return [0,_s7,_rS];},_s9=3,_sa=new T(function(){return B(_s8(_s9));}),_sb=new T(function(){return die(_sa);}),_sc=0,_sd=new T(function(){return B(_s8(_sc));}),_se=new T(function(){return die(_sd);}),_sf=function(_sg,_sh){var _si=E(_sh);switch(_si){case -1:var _sj=E(_sg);return _sj==(-2147483648)?E(_se):B(_rt(_sj,-1));case 0:return E(_sb);default:return new F(function(){return _rt(_sg,_si);});}},_sk=function(_sl,_sm){return [0,B(_sf(E(_sl)[1],E(_sm)[1]))];},_sn=[0,0],_so=[0,_se,_sn],_sp=function(_sq,_sr){var _ss=E(_sq)[1],_st=E(E(_sr)[1]);switch(_st){case -1:var _su=E(_ss);if(_su==(-2147483648)){return E(_so);}else{if(_su<=0){if(_su>=0){var _sv=quotRemI(_su,-1);return [0,[0,_sv[1]],[0,_sv[2]]];}else{var _sw=quotRemI(_su,-1);return [0,[0,_sw[1]],[0,_sw[2]]];}}else{var _sx=quotRemI(_su-1|0,-1);return [0,[0,_sx[1]-1|0],[0,(_sx[2]+(-1)|0)+1|0]];}}break;case 0:return E(_sb);default:if(_ss<=0){if(_ss>=0){var _sy=quotRemI(_ss,_st);return [0,[0,_sy[1]],[0,_sy[2]]];}else{if(_st<=0){var _sz=quotRemI(_ss,_st);return [0,[0,_sz[1]],[0,_sz[2]]];}else{var _sA=quotRemI(_ss+1|0,_st);return [0,[0,_sA[1]-1|0],[0,(_sA[2]+_st|0)-1|0]];}}}else{if(_st>=0){if(_ss>=0){var _sB=quotRemI(_ss,_st);return [0,[0,_sB[1]],[0,_sB[2]]];}else{if(_st<=0){var _sC=quotRemI(_ss,_st);return [0,[0,_sC[1]],[0,_sC[2]]];}else{var _sD=quotRemI(_ss+1|0,_st);return [0,[0,_sD[1]-1|0],[0,(_sD[2]+_st|0)-1|0]];}}}else{var _sE=quotRemI(_ss-1|0,_st);return [0,[0,_sE[1]-1|0],[0,(_sE[2]+_st|0)+1|0]];}}}},_sF=function(_sG,_sH){var _sI=_sG%_sH;if(_sG<=0){if(_sG>=0){return E(_sI);}else{if(_sH<=0){return E(_sI);}else{var _sJ=E(_sI);return _sJ==0?0:_sJ+_sH|0;}}}else{if(_sH>=0){if(_sG>=0){return E(_sI);}else{if(_sH<=0){return E(_sI);}else{var _sK=E(_sI);return _sK==0?0:_sK+_sH|0;}}}else{var _sL=E(_sI);return _sL==0?0:_sL+_sH|0;}}},_sM=function(_sN,_sO){var _sP=E(E(_sO)[1]);switch(_sP){case -1:return E(_sn);case 0:return E(_sb);default:return [0,B(_sF(E(_sN)[1],_sP))];}},_sQ=function(_sR,_sS){var _sT=E(_sR)[1],_sU=E(E(_sS)[1]);switch(_sU){case -1:var _sV=E(_sT);return _sV==(-2147483648)?E(_se):[0,quot(_sV,-1)];case 0:return E(_sb);default:return [0,quot(_sT,_sU)];}},_sW=function(_sX,_sY){var _sZ=E(_sX)[1],_t0=E(E(_sY)[1]);switch(_t0){case -1:var _t1=E(_sZ);if(_t1==(-2147483648)){return E(_so);}else{var _t2=quotRemI(_t1,-1);return [0,[0,_t2[1]],[0,_t2[2]]];}break;case 0:return E(_sb);default:var _t3=quotRemI(_sZ,_t0);return [0,[0,_t3[1]],[0,_t3[2]]];}},_t4=function(_t5,_t6){var _t7=E(E(_t6)[1]);switch(_t7){case -1:return E(_sn);case 0:return E(_sb);default:return [0,E(_t5)[1]%_t7];}},_t8=function(_t9){return new F(function(){return _5Q(E(_t9)[1]);});},_ta=function(_tb){return [0,E(B(_5Q(E(_tb)[1]))),E(_qB)];},_tc=function(_td,_te){return [0,imul(E(_td)[1],E(_te)[1])|0];},_tf=function(_tg,_th){return [0,E(_tg)[1]+E(_th)[1]|0];},_ti=function(_tj,_tk){return [0,E(_tj)[1]-E(_tk)[1]|0];},_tl=function(_tm){var _tn=E(_tm),_to=_tn[1];return _to<0?[0, -_to]:E(_tn);},_tp=function(_tq){return [0,B(_7j(_tq))];},_tr=function(_ts){return [0, -E(_ts)[1]];},_tt=[0,-1],_tu=[0,0],_tv=[0,1],_tw=function(_tx){var _ty=E(_tx)[1];return _ty>=0?E(_ty)==0?E(_tu):E(_tv):E(_tt);},_tz=[0,_tf,_tc,_ti,_tr,_tl,_tw,_tp],_tA=function(_tB,_tC){return E(_tB)[1]==E(_tC)[1];},_tD=function(_tE,_tF){return E(_tE)[1]!=E(_tF)[1];},_tG=[0,_tA,_tD],_tH=function(_tI,_tJ){var _tK=E(_tI),_tL=E(_tJ);return _tK[1]>_tL[1]?E(_tK):E(_tL);},_tM=function(_tN,_tO){var _tP=E(_tN),_tQ=E(_tO);return _tP[1]>_tQ[1]?E(_tQ):E(_tP);},_tR=function(_tS,_tT){return _tS>=_tT?_tS!=_tT?2:1:0;},_tU=function(_tV,_tW){return new F(function(){return _tR(E(_tV)[1],E(_tW)[1]);});},_tX=function(_tY,_tZ){return E(_tY)[1]>=E(_tZ)[1];},_u0=function(_u1,_u2){return E(_u1)[1]>E(_u2)[1];},_u3=function(_u4,_u5){return E(_u4)[1]<=E(_u5)[1];},_u6=function(_u7,_u8){return E(_u7)[1]<E(_u8)[1];},_u9=[0,_tG,_tU,_u6,_tX,_u0,_u3,_tH,_tM],_ua=[0,_tz,_u9,_ta],_ub=[0,_ua,_rs,_sQ,_t4,_sk,_sM,_sW,_sp,_t8],_uc=function(_ud){return E(E(_ud)[1]);},_ue=function(_uf,_ug,_uh){while(1){if(!(_ug%2)){var _ui=B(_5S(_uf,_uf)),_uj=quot(_ug,2);_uf=_ui;_ug=_uj;continue;}else{var _uk=E(_ug);if(_uk==1){return new F(function(){return _5S(_uf,_uh);});}else{var _ui=B(_5S(_uf,_uf));_ug=quot(_uk-1|0,2);var _ul=B(_5S(_uf,_uh));_uf=_ui;_uh=_ul;continue;}}}},_um=function(_un,_uo){while(1){if(!(_uo%2)){var _up=B(_5S(_un,_un)),_uq=quot(_uo,2);_un=_up;_uo=_uq;continue;}else{var _ur=E(_uo);if(_ur==1){return E(_un);}else{return new F(function(){return _ue(B(_5S(_un,_un)),quot(_ur-1|0,2),_un);});}}}},_us=function(_ut){return E(E(_ut)[2]);},_uu=function(_uv){return E(E(_uv)[1]);},_uw=function(_ux){return E(E(_ux)[2]);},_uy=[0,0],_uz=[0,2],_uA=function(_uB){return E(E(_uB)[7]);},_uC=function(_uD,_uE,_uF,_uG,_uH){return new F(function(){return A(E(E(_uE)[1])[1],[new T(function(){return B(A(_uG,[_uH,new T(function(){return B(A(_uA,[_uD,_uz]));})]));}),new T(function(){return B(A(_uA,[_uD,_uy]));})]);});},_uI=function(_uJ){return E(E(_uJ)[3]);},_uK=new T(function(){return B(unCStr("Negative exponent"));}),_uL=new T(function(){return B(err(_uK));}),_uM=function(_uN,_uO,_uP,_uQ){var _uR=B(_qz(_uO)),_uS=_uR[1],_uT=E(_uR[2]);if(!B(A(_uT[3],[_uQ,new T(function(){return B(A(_uA,[_uS,_uy]));})]))){if(!B(A(E(_uT[1])[1],[_uQ,new T(function(){return B(A(_uA,[_uS,_uy]));})]))){var _uU=B(_qz(_uO)),_uV=_uU[1],_uW=new T(function(){return B(_qz(_uO));}),_uX=new T(function(){return B(_uc(_uW));});return new F(function(){return (function(_uY,_uZ){while(1){var _v0=(function(_v1,_v2){var _v3=E(_uO),_v4=_v3[3],_v5=E(_v3[1]);if(!B(_uC(_v5[1],_v5[2],_v5[3],_v3[4],_v2))){return !B(A(E(E(_uU[2])[1])[1],[_v2,new T(function(){return B(A(_uA,[_uV,_qB]));})]))?B((function(_v6,_v7,_v8){while(1){var _v9=(function(_va,_vb,_vc){var _vd=E(_uO),_ve=_vd[3],_vf=E(_vd[1]);if(!B(_uC(_vf[1],_vf[2],_vf[3],_vd[4],_vb))){if(!B(A(new T(function(){return B(_6y(new T(function(){return B(_uu(new T(function(){return B(_uw(_uW));})));})));}),[_vb,new T(function(){return B(A(_uA,[_uX,_qB]));})]))){_v6=new T(function(){return B(A(new T(function(){return B(_us(_uN));}),[_va,_va]));});_v7=new T(function(){return B(A(_ve,[new T(function(){return B(A(new T(function(){return B(_uI(_uX));}),[_vb,new T(function(){return B(A(_uA,[_uX,_qB]));})]));}),new T(function(){return B(A(_uA,[_uX,_uz]));})]));});_v8=new T(function(){return B(A(new T(function(){return B(_us(_uN));}),[_va,_vc]));});return null;}else{return new F(function(){return A(new T(function(){return B(_us(_uN));}),[_va,_vc]);});}}else{_v6=new T(function(){return B(A(new T(function(){return B(_us(_uN));}),[_va,_va]));});_v7=new T(function(){return B(A(_ve,[_vb,new T(function(){return B(A(_uA,[_uX,_uz]));})]));});var _vg=_vc;_v8=_vg;return null;}})(_v6,_v7,_v8);if(_v9!=null){return _v9;}}})(new T(function(){return B(A(new T(function(){return B(_us(_uN));}),[_v1,_v1]));}),new T(function(){return B(A(_v4,[new T(function(){return B(A(new T(function(){return B(_uI(_uV));}),[_v2,new T(function(){return B(A(_uA,[_uV,_qB]));})]));}),new T(function(){return B(A(_uA,[_uV,_uz]));})]));}),_v1)):E(_v1);}else{_uY=new T(function(){return B(A(new T(function(){return B(_us(_uN));}),[_v1,_v1]));});_uZ=new T(function(){return B(A(_v4,[_v2,new T(function(){return B(A(_uA,[_uV,_uz]));})]));});return null;}})(_uY,_uZ);if(_v0!=null){return _v0;}}})(_uP,_uQ);});}else{return new F(function(){return A(_uA,[_uN,_qB]);});}}else{return E(_uL);}},_vh=new T(function(){return B(err(_uK));}),_vi=function(_vj,_vk){var _vl=E(_vj);return _vl[0]==0?_vl[1]*Math.pow(2,_vk):I_toNumber(_vl[1])*Math.pow(2,_vk);},_vm=function(_vn,_vo){while(1){var _vp=E(_vn);if(!_vp[0]){var _vq=E(_vp[1]);if(_vq==(-2147483648)){_vn=[1,I_fromInt(-2147483648)];continue;}else{var _vr=E(_vo);if(!_vr[0]){var _vs=_vr[1];return [0,[0,quot(_vq,_vs)],[0,_vq%_vs]];}else{_vn=[1,I_fromInt(_vq)];_vo=_vr;continue;}}}else{var _vt=E(_vo);if(!_vt[0]){_vn=_vp;_vo=[1,I_fromInt(_vt[1])];continue;}else{var _vu=I_quotRem(_vp[1],_vt[1]);return [0,[1,_vu[1]],[1,_vu[2]]];}}}},_vv=function(_vw,_vx){var _vy=B(_o2(_vx)),_vz=_vy[1],_vA=_vy[2],_vB=new T(function(){return B(_uc(new T(function(){return B(_qz(_vw));})));});if(_vA<0){var _vC= -_vA;if(_vC>=0){var _vD=E(_vC),_vE=_vD==0?E(_qB):B(_um(_ob,_vD));if(!B(_lu(_vE,_lo))){var _vF=B(_vm(_vz,_vE));return [0,new T(function(){return B(A(_uA,[_vB,_vF[1]]));}),new T(function(){return [0,B(_vi(_vF[2],_vA))];})];}else{return E(_sb);}}else{return E(_vh);}}else{return [0,new T(function(){return B(A(_us,[_vB,new T(function(){return B(A(_uA,[_vB,_vz]));}),new T(function(){return B(_uM(_vB,_ub,new T(function(){return B(A(_uA,[_vB,_ob]));}),[0,_vA]));})]));}),_lT];}},_vG=function(_vH,_vI){var _vJ=B(_vv(_vH,E(_vI)[1])),_vK=_vJ[1];if(E(_vJ[2])[1]<=0){return E(_vK);}else{var _vL=E(B(_qz(_vH))[1]);return new F(function(){return A(_vL[1],[_vK,new T(function(){return B(A(_vL[7],[_pt]));})]);});}},_vM=function(_vN,_vO){var _vP=B(_vv(_vN,E(_vO)[1])),_vQ=_vP[1];if(E(_vP[2])[1]>=0){return E(_vQ);}else{var _vR=E(B(_qz(_vN))[1]);return new F(function(){return A(_vR[3],[_vQ,new T(function(){return B(A(_vR[7],[_pt]));})]);});}},_vS=function(_vT,_vU){var _vV=B(_vv(_vT,E(_vU)[1]));return [0,_vV[1],_vV[2]];},_vW=function(_vX,_vY){var _vZ=B(_vv(_vX,_vY)),_w0=_vZ[1],_w1=E(_vZ[2])[1],_w2=new T(function(){var _w3=E(B(_qz(_vX))[1]),_w4=_w3[7];return _w1>=0?B(A(_w3[1],[_w0,new T(function(){return B(A(_w4,[_pt]));})])):B(A(_w3[3],[_w0,new T(function(){return B(A(_w4,[_pt]));})]));});if(_w1<0){var _w5= -_w1-0.5;if(_w5>=0){if(!E(_w5)){var _w6=E(_vX),_w7=E(_w6[1]);return !B(_uC(_w7[1],_w7[2],_w7[3],_w6[4],_w0))?E(_w2):E(_w0);}else{return E(_w2);}}else{return E(_w0);}}else{var _w8=_w1-0.5;if(_w8>=0){if(!E(_w8)){var _w9=E(_vX),_wa=E(_w9[1]);return !B(_uC(_wa[1],_wa[2],_wa[3],_w9[4],_w0))?E(_w2):E(_w0);}else{return E(_w2);}}else{return E(_w0);}}},_wb=function(_wc,_wd){return new F(function(){return _vW(_wc,E(_wd)[1]);});},_we=function(_wf,_wg){return E(B(_vv(_wf,E(_wg)[1]))[1]);},_wh=[0,_qy,_me,_vS,_we,_wb,_vG,_vM],_wi=function(_wj,_wk){return E(_wj)[1]!=E(_wk)[1]?true:false;},_wl=function(_wm,_wn){return E(_wm)[1]==E(_wn)[1];},_wo=[0,_wl,_wi],_wp=function(_wq,_wr){return E(_wq)[1]<E(_wr)[1];},_ws=function(_wt,_wu){return E(_wt)[1]<=E(_wu)[1];},_wv=function(_ww,_wx){return E(_ww)[1]>E(_wx)[1];},_wy=function(_wz,_wA){return E(_wz)[1]>=E(_wA)[1];},_wB=function(_wC,_wD){var _wE=E(_wC)[1],_wF=E(_wD)[1];return _wE>=_wF?_wE!=_wF?2:1:0;},_wG=function(_wH,_wI){var _wJ=E(_wH),_wK=E(_wI);return _wJ[1]>_wK[1]?E(_wJ):E(_wK);},_wL=function(_wM,_wN){var _wO=E(_wM),_wP=E(_wN);return _wO[1]>_wP[1]?E(_wP):E(_wO);},_wQ=[0,_wo,_wB,_wp,_wy,_wv,_ws,_wG,_wL],_wR=function(_wS,_wT){while(1){var _wU=(function(_wV,_wW){var _wX=E(_pG)[1]["v"]["i8"][(255&_wV>>>0)>>>0&4294967295];if(_wW>_wX){if(_wX>=8){var _wY=_wV>>8,_wZ=_wW-8|0;_wS=_wY;_wT=_wZ;return null;}else{return [0,new T(function(){return B(_5Q(_wV>>_wX));}),_wW-_wX|0];}}else{return [0,new T(function(){return B(_5Q(_wV>>_wW));}),0];}})(_wS,_wT);if(_wU!=null){return _wU;}}},_x0=function(_x1){var _x2=decodeFloat(_x1),_x3=_x2[1],_x4=_x2[2];if(_x4<0){var _x5=function(_x6){if(!_x6){return [0,B(_5Q(_x3)),B(_qj(_pt, -_x4))];}else{var _x7=B(_wR(_x3, -_x4));return [0,E(_x7[1]),B(_qj(_pt,_x7[2]))];}};return (_x3>>>0&1)>>>0==0?B(_x5(1)):B(_x5(0));}else{return [0,B(_qj(B(_5Q(_x3)),_x4)),_pt];}},_x8=function(_x9){var _xa=B(_x0(E(_x9)[1]));return [0,E(_xa[1]),E(_xa[2])];},_xb=[0,_nv,_wQ,_x8],_xc=[0,-1],_xd=[0,1],_xe=function(_xf,_xg){var _xh=E(_xf);return _xh[0]==0?_xh[1]*Math.pow(2,_xg):I_toNumber(_xh[1])*Math.pow(2,_xg);},_xi=[0,0],_xj=function(_xk,_xl){var _xm=decodeFloat(_xl),_xn=_xm[1],_xo=_xm[2],_xp=new T(function(){return B(_uc(new T(function(){return B(_qz(_xk));})));});if(_xo<0){var _xq=new T(function(){if(_xn<0){var _xr= -_xo;if(_xr<32){var _xs=[0, -( -_xn>>_xr)];}else{var _xs= -_xn>=0?E(_xi):E(_xd);}var _xt=_xs,_xu=_xt,_xv=_xu;}else{var _xw= -_xo;if(_xw<32){var _xx=[0,_xn>>_xw];}else{var _xx=_xn>=0?E(_xi):E(_xc);}var _xy=_xx,_xz=_xy,_xv=_xz;}var _xA=_xv;return _xA;});return [0,new T(function(){return B(A(_uA,[_xp,new T(function(){return B(_5Q(E(_xq)[1]));})]));}),new T(function(){var _xB= -_xo;if(_xB<32){var _xC=[0,B(_xe(B(_5Q(_xn-(E(_xq)[1]<<_xB)|0)),_xo))];}else{var _xC=[0,B(_xe(B(_5Q(_xn)),_xo))];}var _xD=_xC,_xE=_xD,_xF=_xE;return _xF;})];}else{return [0,new T(function(){return B(A(_us,[_xp,new T(function(){return B(A(_uA,[_xp,new T(function(){return B(_5Q(_xn));})]));}),new T(function(){return B(_uM(_xp,_ub,new T(function(){return B(A(_uA,[_xp,_ob]));}),[0,_xo]));})]));}),_ne];}},_xG=function(_xH,_xI){var _xJ=B(_xj(_xH,E(_xI)[1])),_xK=_xJ[1];if(E(_xJ[2])[1]<=0){return E(_xK);}else{var _xL=E(B(_qz(_xH))[1]);return new F(function(){return A(_xL[1],[_xK,new T(function(){return B(A(_xL[7],[_pt]));})]);});}},_xM=function(_xN,_xO){var _xP=B(_xj(_xN,E(_xO)[1])),_xQ=_xP[1];if(E(_xP[2])[1]>=0){return E(_xQ);}else{var _xR=E(B(_qz(_xN))[1]);return new F(function(){return A(_xR[3],[_xQ,new T(function(){return B(A(_xR[7],[_pt]));})]);});}},_xS=function(_xT,_xU){var _xV=B(_xj(_xT,E(_xU)[1]));return [0,_xV[1],_xV[2]];},_xW=function(_xX,_xY){var _xZ=B(_xj(_xX,_xY)),_y0=_xZ[1],_y1=E(_xZ[2])[1],_y2=new T(function(){var _y3=E(B(_qz(_xX))[1]),_y4=_y3[7];return _y1>=0?B(A(_y3[1],[_y0,new T(function(){return B(A(_y4,[_pt]));})])):B(A(_y3[3],[_y0,new T(function(){return B(A(_y4,[_pt]));})]));});if(_y1<0){var _y5= -_y1-0.5;if(_y5>=0){if(!E(_y5)){var _y6=E(_xX),_y7=E(_y6[1]);return !B(_uC(_y7[1],_y7[2],_y7[3],_y6[4],_y0))?E(_y2):E(_y0);}else{return E(_y2);}}else{return E(_y0);}}else{var _y8=_y1-0.5;if(_y8>=0){if(!E(_y8)){var _y9=E(_xX),_ya=E(_y9[1]);return !B(_uC(_ya[1],_ya[2],_ya[3],_y9[4],_y0))?E(_y2):E(_y0);}else{return E(_y2);}}else{return E(_y0);}}},_yb=function(_yc,_yd){return new F(function(){return _xW(_yc,E(_yd)[1]);});},_ye=function(_yf,_yg){return E(B(_xj(_yf,E(_yg)[1]))[1]);},_yh=[0,_xb,_nz,_xS,_ye,_yb,_xG,_xM],_yi=function(_yj){return new F(function(){return err(B(unAppCStr("Char.intToDigit: not a digit ",new T(function(){if(_yj>=0){var _yk=jsShowI(_yj),_yl=_yk,_ym=fromJSStr(_yl);}else{var _yn=jsShowI(_yj),_yo=_yn,_ym=fromJSStr(_yo);}var _yp=_ym;return _yp;}))));});},_yq=function(_yr){var _ys=function(_yt){if(_yr<10){return new F(function(){return _yi(_yr);});}else{if(_yr>15){return new F(function(){return _yi(_yr);});}else{return (97+_yr|0)-10|0;}}};if(_yr<0){return new F(function(){return _ys(_);});}else{if(_yr>9){return new F(function(){return _ys(_);});}else{return 48+_yr|0;}}},_yu=function(_yv){return [0,B(_yq(E(_yv)[1]))];},_yw=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_yx=function(_yy){return new F(function(){return _2F([0,new T(function(){return B(_2U(_yy,_yw));})],_2C);});},_yz=new T(function(){return B(_yx("GHC/Float.lhs:619:11-64|d : ds\'"));}),_yA=function(_yB,_yC){if(E(_yB)[1]<=0){var _yD=B(_U(_yu,[1,_xi,_yC]));return _yD[0]==0?E(_yz):[0,_yD[1],_yD[2]];}else{var _yE=B(_U(_yu,_yC));return _yE[0]==0?E(_yz):[0,_yE[1],_yE[2]];}},_yF=function(_yG){return E(E(_yG)[1]);},_yH=function(_yI){return E(E(_yI)[1]);},_yJ=function(_yK){return E(E(_yK)[1]);},_yL=[0,48],_yM=[1,_yL,_9],_yN=[0,46],_yO=function(_yP,_yQ,_yR){while(1){var _yS=(function(_yT,_yU,_yV){var _yW=E(_yT);if(!_yW){var _yX=B(_l3(_yU,_9));return _yX[0]==0?[1,_yL,[1,_yN,new T(function(){var _yY=E(_yV);return _yY[0]==0?E(_yM):E(_yY);})]]:B(_i(_yX,[1,_yN,new T(function(){var _yZ=E(_yV);return _yZ[0]==0?E(_yM):E(_yZ);})]));}else{var _z0=E(_yV);if(!_z0[0]){_yP=_yW-1|0;var _z1=[1,_yL,_yU];_yR=_9;_yQ=_z1;return null;}else{_yP=_yW-1|0;var _z1=[1,_z0[1],_yU];_yR=_z0[2];_yQ=_z1;return null;}}})(_yP,_yQ,_yR);if(_yS!=null){return _yS;}}},_z2=[0,0],_z3=new T(function(){return B(unCStr(" out of range "));}),_z4=new T(function(){return B(unCStr("}.index: Index "));}),_z5=new T(function(){return B(unCStr("Ix{"));}),_z6=[1,_A,_9],_z7=[1,_A,_z6],_z8=function(_z9,_za,_zb,_zc,_zd){return new F(function(){return err(B(_i(_z5,new T(function(){return B(_i(_z9,new T(function(){return B(_i(_z4,[1,_B,new T(function(){return B(A(_zd,[_z2,_za,[1,_A,new T(function(){return B(_i(_z3,[1,_B,[1,_B,new T(function(){return B(A(_iT,[_iJ,[1,new T(function(){return B(A(_zd,[_j0,_zb]));}),[1,new T(function(){return B(A(_zd,[_j0,_zc]));}),_9]],_z7]));})]]));})]]));})]));})));}))));});},_ze=function(_zf,_zg,_zh,_zi){var _zj=E(_zh);return new F(function(){return _z8(_zf,_zg,_zj[1],_zj[2],E(_zi)[1]);});},_zk=function(_zl,_zm,_zn,_zo){return new F(function(){return _ze(_zo,_zn,_zm,_zl);});},_zp=new T(function(){return B(unCStr("Int"));}),_zq=function(_zr,_zs,_zt){return new F(function(){return _zk(_jK,[0,_zs,_zt],_zr,_zp);});},_zu=new T(function(){return B(unCStr("(Array.!): undefined array element"));}),_zv=new T(function(){return B(err(_zu));}),_zw=[0,1100],_zx=[0,_xi,_zw],_zy=function(_zz){return new F(function(){return _zk(_jK,_zx,[0,_zz],_zp);});},_zA=function(_){var _zB=newArr(1101,_zv),_zC=_zB;return new F(function(){return (function(_zD,_){while(1){var _zE=(function(_zF,_){if(0>_zF){return new F(function(){return _zy(_zF);});}else{if(_zF>1100){return new F(function(){return _zy(_zF);});}else{var _=_zC[_zF]=new T(function(){if(_zF>=0){var _zG=E(_zF),_zH=_zG==0?E(_qB):B(_um(_ob,_zG));}else{var _zH=E(_vh);}var _zI=_zH;return _zI;}),_zJ=E(_zF);if(_zJ==1100){var _zK=_zC,_zL=_zK;return [0,E(_xi),E(_zw),1101,_zL];}else{_zD=_zJ+1|0;return null;}}}})(_zD,_);if(_zE!=null){return _zE;}}})(0,_);});},_zM=function(_zN){var _zO=B(A(_zN,[_])),_zP=_zO;return E(_zP);},_zQ=new T(function(){return B(_zM(_zA));}),_zR=[0,10],_zS=[0,324],_zT=[0,_xi,_zS],_zU=function(_zV){return new F(function(){return _zk(_jK,_zT,[0,_zV],_zp);});},_zW=function(_){var _zX=newArr(325,_zv),_zY=_zX;return new F(function(){return (function(_zZ,_){while(1){var _A0=(function(_A1,_){if(0>_A1){return new F(function(){return _zU(_A1);});}else{if(_A1>324){return new F(function(){return _zU(_A1);});}else{var _=_zY[_A1]=new T(function(){if(_A1>=0){var _A2=E(_A1),_A3=_A2==0?E(_qB):B(_um(_zR,_A2));}else{var _A3=E(_vh);}var _A4=_A3;return _A4;}),_A5=E(_A1);if(_A5==324){var _A6=_zY,_A7=_A6;return [0,E(_xi),E(_zS),325,_A7];}else{_zZ=_A5+1|0;return null;}}}})(_zZ,_);if(_A0!=null){return _A0;}}})(0,_);});},_A8=new T(function(){return B(_zM(_zW));}),_A9=function(_Aa,_Ab){var _Ac=[0,_Ab],_Ad=function(_Ae){if(!B(_lu(_Aa,_zR))){if(_Ab>=0){var _Af=E(_Ab);return _Af==0?E(_qB):B(_um(_Aa,_Af));}else{return E(_vh);}}else{if(_Ab>324){if(_Ab>=0){var _Ag=E(_Ab);return _Ag==0?E(_qB):B(_um(_Aa,_Ag));}else{return E(_vh);}}else{var _Ah=E(_A8),_Ai=E(_Ah[1]),_Aj=_Ai[1],_Ak=E(_Ah[2]);if(_Aj>_Ab){return new F(function(){return _zq(_Ac,_Ai,_Ak);});}else{if(_Ab>_Ak[1]){return new F(function(){return _zq(_Ac,_Ai,_Ak);});}else{return E(_Ah[4][_Ab-_Aj|0]);}}}}};if(!B(_lu(_Aa,_ob))){return new F(function(){return _Ad(_);});}else{if(_Ab<0){return new F(function(){return _Ad(_);});}else{if(_Ab>1100){return new F(function(){return _Ad(_);});}else{var _Al=E(_zQ),_Am=E(_Al[1]),_An=_Am[1],_Ao=E(_Al[2]);if(_An>_Ab){return new F(function(){return _zq(_Ac,_Am,_Ao);});}else{if(_Ab>_Ao[1]){return new F(function(){return _zq(_Ac,_Am,_Ao);});}else{return E(_Al[4][_Ab-_An|0]);}}}}}},_Ap=function(_Aq,_Ar){var _As=E(_Aq);if(!_As[0]){var _At=_As[1],_Au=E(_Ar);return _Au[0]==0?_At>_Au[1]:I_compareInt(_Au[1],_At)<0;}else{var _Av=_As[1],_Aw=E(_Ar);return _Aw[0]==0?I_compareInt(_Av,_Aw[1])>0:I_compare(_Av,_Aw[1])>0;}},_Ax=[1,_xi,_9],_Ay=function(_Az,_AA){while(1){var _AB=E(_Az);if(!_AB[0]){var _AC=E(_AB[1]);if(_AC==(-2147483648)){_Az=[1,I_fromInt(-2147483648)];continue;}else{var _AD=E(_AA);if(!_AD[0]){return [0,quot(_AC,_AD[1])];}else{_Az=[1,I_fromInt(_AC)];_AA=_AD;continue;}}}else{var _AE=_AB[1],_AF=E(_AA);return _AF[0]==0?[0,I_toInt(I_quot(_AE,I_fromInt(_AF[1])))]:[1,I_quot(_AE,_AF[1])];}}},_AG=function(_AH,_AI,_AJ,_AK,_AL,_AM,_AN,_AO){if(!B(A(_AH,[_AO,new T(function(){return B(A(_uA,[B(_yH(B(_yF(_AI)))),_lo]));})]))){var _AP=new T(function(){return B(A(_AJ,[_AO]));}),_AQ=new T(function(){return B(A(_AK,[_AO]));}),_AR=new T(function(){return [0,E(B(A(_AL,[_AO]))[1])[1]-E(_AQ)[1]|0];}),_AS=new T(function(){return B(A(_AM,[_AO]));}),_AT=new T(function(){return E(E(_AS)[2]);}),_AU=new T(function(){var _AV=E(_AT),_AW=_AV[1],_AX=E(_AR)[1]-_AW|0;if(_AX<=0){var _AY=[0,new T(function(){return E(E(_AS)[1]);}),_AV];}else{var _AY=[0,new T(function(){var _AZ=B(_A9(_AP,_AX));if(!B(_lu(_AZ,_lo))){var _B0=B(_Ay(E(_AS)[1],_AZ));}else{var _B0=E(_sb);}var _B1=_B0;return _B1;}),[0,_AW+_AX|0]];}var _B2=_AY,_B3=_B2,_B4=_B3,_B5=_B4;return _B5;}),_B6=new T(function(){return E(E(_AU)[2]);}),_B7=new T(function(){return E(E(_AU)[1]);}),_B8=new T(function(){var _B9=E(_B6)[1];if(_B9<0){if(_B9<=E(_AR)[1]){var _Ba=[0,new T(function(){return B(_5S(_B7,_ob));}),new T(function(){return B(_5S(B(_A9(_AP, -_B9)),_ob));}),_pt,_pt];}else{var _Ba=!B(_lu(_B7,B(_A9(_AP,E(_AQ)[1]-1|0))))?[0,new T(function(){return B(_5S(_B7,_ob));}),new T(function(){return B(_5S(B(_A9(_AP, -_B9)),_ob));}),_pt,_pt]:[0,new T(function(){return B(_5S(B(_5S(_B7,_AP)),_ob));}),new T(function(){return B(_5S(B(_A9(_AP, -_B9+1|0)),_ob));}),_AP,_pt];}var _Bb=_Ba,_Bc=_Bb,_Bd=_Bc;}else{var _Be=new T(function(){return B(_A9(_AP,_B9));}),_Bd=!B(_lu(_B7,B(_A9(_AP,E(_AQ)[1]-1|0))))?[0,new T(function(){return B(_5S(B(_5S(_B7,_Be)),_ob));}),_ob,_Be,_Be]:[0,new T(function(){return B(_5S(B(_5S(B(_5S(_B7,_Be)),_AP)),_ob));}),new T(function(){return B(_5S(_ob,_AP));}),new T(function(){return B(_5S(_Be,_AP));}),_Be];}var _Bf=_Bd,_Bg=_Bf;return _Bg;}),_Bh=new T(function(){return E(E(_B8)[2]);}),_Bi=new T(function(){return E(E(_B8)[3]);}),_Bj=new T(function(){return E(E(_B8)[1]);}),_Bk=new T(function(){var _Bl=new T(function(){return B(_5A(_Bj,_Bi));}),_Bm=function(_Bn){var _Bo=(Math.log(B(_n9(B(_5A(_B7,_pt)))))+E(_B6)[1]*Math.log(B(_n9(_AP))))/Math.log(B(_n9(_AN))),_Bp=_Bo&4294967295;return _Bp>=_Bo?E(_Bp):_Bp+1|0;},_Bq=function(_Br){while(1){if(_Br<0){if(!B(_7m(B(_5S(B(_A9(_AN, -_Br)),_Bl)),_Bh))){var _Bs=_Br+1|0;_Br=_Bs;continue;}else{return E(_Br);}}else{if(!B(_7m(_Bl,B(_5S(B(_A9(_AN,_Br)),_Bh))))){var _Bs=_Br+1|0;_Br=_Bs;continue;}else{return E(_Br);}}}};if(!B(_lu(_AP,_ob))){var _Bt=[0,B(_Bq(B(_Bm(_))))];}else{if(!B(_lu(_AN,_zR))){var _Bu=[0,B(_Bq(B(_Bm(_))))];}else{var _Bv=(E(_AQ)[1]-1|0)+E(_AT)[1]|0;if(_Bv<0){var _Bw=[0,B(_Bq(quot(imul(_Bv,8651)|0,28738)))];}else{var _Bw=[0,B(_Bq(quot(imul(_Bv,8651)|0,28738)+1|0))];}var _Bx=_Bw,_By=_Bx,_Bz=_By,_BA=_Bz,_BB=_BA,_Bu=_BB;}var _Bt=_Bu;}return _Bt;});return [0,new T(function(){var _BC=E(_Bk)[1],_BD=function(_BE,_BF,_BG,_BH,_BI){while(1){var _BJ=(function(_BK,_BL,_BM,_BN,_BO){if(!B(_lu(_BM,_lo))){var _BP=B(_vm(B(_5S(_BL,_AN)),_BM)),_BQ=_BP[1],_BR=_BP[2],_BS=B(_5S(_BO,_AN)),_BT=B(_5S(_BN,_AN));if(!B(_s(_BR,_BS))){if(!B(_Ap(B(_5A(_BR,_BT)),_BM))){var _BU=[1,_BQ,_BK];_BF=_BR;var _BV=_BM;_BH=_BT;_BI=_BS;_BE=_BU;_BG=_BV;return null;}else{return [1,new T(function(){return B(_5A(_BQ,_pt));}),_BK];}}else{return !B(_Ap(B(_5A(_BR,_BT)),_BM))?[1,_BQ,_BK]:!B(_s(B(_5S(_BR,_ob)),_BM))?[1,new T(function(){return B(_5A(_BQ,_pt));}),_BK]:[1,_BQ,_BK];}}else{return E(_sb);}})(_BE,_BF,_BG,_BH,_BI);if(_BJ!=null){return _BJ;}}};if(_BC<0){var _BW=B(_A9(_AN, -_BC)),_BX=B(_U(_tp,B(_l3(B(_BD(_9,B(_5S(_Bj,_BW)),_Bh,B(_5S(_Bi,_BW)),B(_5S(E(_B8)[4],_BW)))),_9))));}else{var _BX=B(_U(_tp,B(_l3(B(_BD(_9,_Bj,B(_5S(_Bh,B(_A9(_AN,_BC)))),_Bi,E(_B8)[4])),_9))));}var _BY=_BX,_BZ=_BY;return _BZ;}),_Bk];}else{return [0,_Ax,_xi];}},_C0=function(_C1,_C2){while(1){var _C3=E(_C2);if(!_C3[0]){return true;}else{if(!B(A(_C1,[_C3[1]]))){return false;}else{_C2=_C3[2];continue;}}}},_C4=function(_C5){return E(_C5)[1]%2==0?true:false;},_C6=new T(function(){return B(unCStr("roundTo: bad Value"));}),_C7=new T(function(){return B(err(_C6));}),_C8=function(_C9){return E(E(_C9)[1])==0?true:false;},_Ca=function(_Cb){return _Cb>1?[1,_xi,new T(function(){return B(_Ca(_Cb-1|0));})]:E(_Ax);},_Cc=function(_Cd,_Ce,_Cf){var _Cg=function(_Ch,_Ci,_Cj){var _Ck=E(_Cj);if(!_Ck[0]){return [0,_xi,new T(function(){var _Cl=E(_Ch)[1];return _Cl>0?B(_Ca(_Cl)):[0];})];}else{var _Cm=_Ck[1],_Cn=_Ck[2],_Co=E(E(_Ch)[1]);if(!_Co){var _Cp=E(_Cm)[1],_Cq=E(new T(function(){return [0,quot(E(_Cd)[1],2)];}))[1];return _Cp!=_Cq?[0,new T(function(){return _Cp<_Cq?E(_xi):E(_xd);}),_9]:!E(_Ci)?[0,new T(function(){return _Cp<_Cq?E(_xi):E(_xd);}),_9]:!B(_C0(_C8,_Cn))?[0,new T(function(){return _Cp<_Cq?E(_xi):E(_xd);}),_9]:[0,_xi,_9];}else{var _Cr=B(_Cg([0,_Co-1|0],new T(function(){return B(_C4(_Cm));}),_Cn)),_Cs=_Cr[2],_Ct=E(_Cr[1])[1]+E(_Cm)[1]|0;return _Ct!=E(_Cd)[1]?[0,_xi,[1,[0,_Ct],_Cs]]:[0,_xd,[1,_xi,_Cs]];}}},_Cu=B(_Cg(_Ce,_6V,_Cf));switch(E(E(_Cu[1])[1])){case 0:return E(_Cu);case 1:return [0,_xd,[1,_xd,_Cu[2]]];default:return E(_C7);}},_Cv=function(_Cw,_Cx){var _Cy=E(_Cw);if(!_Cy){return [0,_9,_Cx];}else{var _Cz=E(_Cx);if(!_Cz[0]){return [0,_9,_9];}else{var _CA=new T(function(){var _CB=B(_Cv(_Cy-1|0,_Cz[2]));return [0,_CB[1],_CB[2]];});return [0,[1,_Cz[1],new T(function(){return E(E(_CA)[1]);})],new T(function(){return E(E(_CA)[2]);})];}}},_CC=function(_CD){return E(E(_CD)[3]);},_CE=0,_CF=1,_CG=[0,10],_CH=new T(function(){return B(unCStr("e0"));}),_CI=function(_CJ,_CK){var _CL=E(_CJ);if(!_CL[0]){return E(_CH);}else{var _CM=_CL[1];return _CK>1?[1,_CM,new T(function(){return B(_CI(_CL[2],_CK-1|0));})]:[1,_CM,_CH];}},_CN=function(_CO,_CP){var _CQ=E(_CP);return _CQ[0]==0?[0]:[1,_CO,new T(function(){return B(_CN(_CQ[1],_CQ[2]));})];},_CR=new T(function(){return B(unCStr("init"));}),_CS=new T(function(){return B(_iP(_CR));}),_CT=new T(function(){return B(_yx("GHC/Float.lhs:591:12-70|(d : ds\')"));}),_CU=[0,101],_CV=new T(function(){return B(unCStr("Infinity"));}),_CW=new T(function(){return B(unCStr("-Infinity"));}),_CX=new T(function(){return B(unCStr("NaN"));}),_CY=new T(function(){return B(unCStr("formatRealFloat/doFmt/FFExponent: []"));}),_CZ=new T(function(){return B(err(_CY));}),_D0=new T(function(){return B(unCStr("0.0e0"));}),_D1=function(_D2){return E(E(_D2)[4]);},_D3=new T(function(){return [1,_yL,_D3];}),_D4=function(_D5,_D6,_D7,_D8,_D9,_Da,_Db,_Dc,_Dd,_De,_Df,_Dg){if(!B(A(_Db,[_Dg]))){var _Dh=new T(function(){return B(_yH(new T(function(){return B(_yF(_D6));})));});if(!B(A(_Dc,[_Dg]))){var _Di=function(_Dj,_Dk,_Dl){while(1){var _Dm=(function(_Dn,_Do,_Dp){switch(E(_Dn)){case 0:var _Dq=E(_Df);if(!_Dq[0]){var _Dr=B(_U(_yu,_Do));if(!_Dr[0]){return E(_CZ);}else{var _Ds=_Dr[2],_Dt=E(_Dr[1]),_Du=function(_Dv){var _Dw=E(_Ds);return _Dw[0]==0?[1,_Dt,new T(function(){return B(unAppCStr(".0e",new T(function(){return B(_7b(0,E(_Dp)[1]-1|0,_9));})));})]:[1,_Dt,[1,_yN,new T(function(){return B(_i(_Dw,[1,_CU,new T(function(){return B(_7b(0,E(_Dp)[1]-1|0,_9));})]));})]];};return E(_Dt[1])==48?E(_Ds)[0]==0?E(_D0):B(_Du(_)):B(_Du(_));}}else{var _Dx=new T(function(){var _Dy=E(_Dq[1]);return _Dy[1]>1?E(_Dy):E(_xd);}),_Dz=function(_DA){var _DB=new T(function(){var _DC=B(_Cc(_CG,new T(function(){return [0,E(_Dx)[1]+1|0];}),_Do));return [0,_DC[1],_DC[2]];}),_DD=new T(function(){return E(E(_DB)[1]);}),_DE=new T(function(){if(E(_DD)[1]<=0){var _DF=B(_U(_yu,E(_DB)[2])),_DG=_DF[0]==0?E(_CT):[0,_DF[1],_DF[2]];}else{var _DH=E(E(_DB)[2]);if(!_DH[0]){var _DI=E(_CS);}else{var _DJ=B(_U(_yu,B(_CN(_DH[1],_DH[2])))),_DI=_DJ[0]==0?E(_CT):[0,_DJ[1],_DJ[2]];}var _DK=_DI,_DG=_DK;}var _DL=_DG,_DM=_DL;return _DM;});return [1,new T(function(){return E(E(_DE)[1]);}),[1,_yN,new T(function(){return B(_i(E(_DE)[2],[1,_CU,new T(function(){return B(_7b(0,(E(_Dp)[1]-1|0)+E(_DD)[1]|0,_9));})]));})]];},_DN=E(_Do);if(!_DN[0]){return new F(function(){return _Dz(_);});}else{return E(E(_DN[1])[1])==0?E(_DN[2])[0]==0?[1,_yL,[1,_yN,new T(function(){var _DO=E(_Dx)[1];return _DO>0?B(_CI(_D3,_DO)):E(_CH);})]]:B(_Dz(_)):B(_Dz(_));}}break;case 1:var _DP=E(_Df);if(!_DP[0]){var _DQ=E(_Dp)[1];return _DQ>0?B(_yO(_DQ,_9,new T(function(){return B(_U(_yu,_Do));}))):B(unAppCStr("0.",new T(function(){var _DR= -_DQ;if(_DR>0){var _DS=function(_DT){return _DT>1?[1,_yL,new T(function(){return B(_DS(_DT-1|0));})]:E([1,_yL,new T(function(){return B(_U(_yu,_Do));})]);},_DU=B(_DS(_DR));}else{var _DU=B(_U(_yu,_Do));}var _DV=_DU,_DW=_DV;return _DW;})));}else{var _DX=_DP[1],_DY=E(_Dp),_DZ=_DY[1];if(_DZ<0){var _E0=new T(function(){var _E1= -_DZ;if(_E1>0){var _E2=function(_E3){return _E3>1?[1,_xi,new T(function(){return B(_E2(_E3-1|0));})]:E([1,_xi,_Do]);},_E4=B(_Cc(_CG,new T(function(){var _E5=E(_DX);return _E5[1]>0?E(_E5):E(_xi);}),B(_E2(_E1)))),_E6=B(_yA(_E4[1],_E4[2]));}else{var _E7=B(_Cc(_CG,new T(function(){var _E8=E(_DX);return _E8[1]>0?E(_E8):E(_xi);}),_Do)),_E6=B(_yA(_E7[1],_E7[2]));}var _E9=_E6,_Ea=_E9;return _Ea;});return [1,new T(function(){return E(E(_E0)[1]);}),new T(function(){var _Eb=E(E(_E0)[2]);return _Eb[0]==0?[0]:[1,_yN,_Eb];})];}else{var _Ec=B(_Cc(_CG,new T(function(){var _Ed=E(_DX)[1];if(_Ed>0){var _Ee=[0,_Ed+_DZ|0];}else{var _Ee=E(_DY);}var _Ef=_Ee,_Eg=_Ef;return _Eg;}),_Do)),_Eh=_Ec[2],_Ei=_DZ+E(_Ec[1])[1]|0;if(_Ei>=0){var _Ej=B(_Cv(_Ei,new T(function(){return B(_U(_yu,_Eh));}))),_Ek=_Ej[2],_El=E(_Ej[1]);return _El[0]==0?[1,_yL,new T(function(){var _Em=E(_Ek);return _Em[0]==0?[0]:[1,_yN,_Em];})]:B(_i(_El,new T(function(){var _En=E(_Ek);return _En[0]==0?[0]:[1,_yN,_En];})));}else{return [1,_yL,new T(function(){var _Eo=B(_U(_yu,_Eh));return _Eo[0]==0?[0]:[1,_yN,_Eo];})];}}}break;default:var _Ep=E(_Dp),_Eq=_Ep[1];if(_Eq>=0){if(_Eq<=7){_Dj=_CF;var _Er=_Do;_Dl=_Ep;_Dk=_Er;return null;}else{_Dj=_CE;var _Er=_Do;_Dl=_Ep;_Dk=_Er;return null;}}else{_Dj=_CE;var _Er=_Do;_Dl=_Ep;_Dk=_Er;return null;}}})(_Dj,_Dk,_Dl);if(_Dm!=null){return _Dm;}}},_Es=function(_Et){return [1,_k5,new T(function(){var _Eu=B(_AG(E(E(E(E(_D5)[1])[2])[1])[1],_D6,_D7,_D8,_D9,_Da,_zR,new T(function(){return B(A(_D1,[_Dh,_Dg]));})));return B(_Di(_De,_Eu[1],_Eu[2]));})];};if(!B(A(_CC,[B(_uw(B(_yJ(_D5)))),_Dg,new T(function(){return B(A(_uA,[_Dh,_lo]));})]))){if(!B(A(_Dd,[_Dg]))){var _Ev=B(_AG(E(E(E(E(_D5)[1])[2])[1])[1],_D6,_D7,_D8,_D9,_Da,_zR,_Dg));return new F(function(){return _Di(_De,_Ev[1],_Ev[2]);});}else{return new F(function(){return _Es(_);});}}else{return new F(function(){return _Es(_);});}}else{return !B(A(_CC,[B(_uw(B(_yJ(_D5)))),_Dg,new T(function(){return B(A(_uA,[_Dh,_lo]));})]))?E(_CV):E(_CW);}}else{return E(_CX);}},_Ew=function(_Ex){var _Ey=u_towlower(_Ex),_Ez=_Ey;return _Ez>>>0>1114111?B(_7h(_Ez)):_Ez;},_EA=function(_EB){return new F(function(){return err(B(unAppCStr("Printf.printf: ",_EB)));});},_EC=new T(function(){return B(unCStr("bad argument"));}),_ED=new T(function(){return B(_EA(_EC));}),_EE=new T(function(){return B(unCStr("Printf.dfmt: impossible"));}),_EF=new T(function(){return B(err(_EE));}),_EG=[0,45],_EH=[1,_EG,_9],_EI=new T(function(){return B(err(_EE));}),_EJ=new T(function(){return B(unCStr("Negative exponent"));}),_EK=new T(function(){return B(err(_EJ));}),_EL=function(_EM,_EN,_EO){while(1){if(!(_EN%2)){var _EP=_EM*_EM,_EQ=quot(_EN,2);_EM=_EP;_EN=_EQ;continue;}else{var _ER=E(_EN);if(_ER==1){return _EM*_EO;}else{var _EP=_EM*_EM;_EN=quot(_ER-1|0,2);var _ES=_EM*_EO;_EM=_EP;_EO=_ES;continue;}}}},_ET=function(_EU,_EV){while(1){if(!(_EV%2)){var _EW=_EU*_EU,_EX=quot(_EV,2);_EU=_EW;_EV=_EX;continue;}else{var _EY=E(_EV);if(_EY==1){return E(_EU);}else{return new F(function(){return _EL(_EU*_EU,quot(_EY-1|0,2),_EU);});}}}},_EZ=function(_F0,_F1){var _F2=E(_F0);return _F2[0]==0?function(_4L){return new F(function(){return _i(new T(function(){var _F3=jsShow(E(_F1)[1]),_F4=_F3;return fromJSStr(_F4);}),_4L);});}:function(_4L){return new F(function(){return _i(new T(function(){var _F5=E(E(_F2[1])[1]);if(!_F5){var _F6=jsRound(E(_F1)[1]),_F7=_F6,_F8=B(_o2(_F7)),_F9=_F8[1],_Fa=_F8[2];if(_Fa>=0){var _Fb=jsShow(B(_lO(B(_qj(_F9,_Fa))))),_Fc=_Fb,_Fd=fromJSStr(_Fc);}else{var _Fe=hs_uncheckedIShiftRA64(B(_qb(_F9)), -_Fa),_Ff=_Fe,_Fg=jsShow(B(_lO(B(_pu(_Ff))))),_Fh=_Fg,_Fd=fromJSStr(_Fh);}var _Fi=_Fd,_Fj=_Fi,_Fk=_Fj,_Fl=_Fk;}else{if(_F5>=0){var _Fm=B(_ET(10,_F5)),_Fn=jsRound(E(_F1)[1]*_Fm),_Fo=_Fn,_Fp=jsShow((_Fo&4294967295)/_Fm),_Fq=_Fp,_Fr=fromJSStr(_Fq);}else{var _Fr=E(_EK);}var _Fs=_Fr,_Ft=_Fs,_Fl=_Ft;}var _Fu=_Fl;return _Fu;}),_4L);});};},_Fv=function(_Fw,_Fx){var _Fy=E(_Fw);return _Fy[0]==0?function(_4L){return new F(function(){return _i(new T(function(){var _Fz=B(_x0(E(_Fx)[1])),_FA=jsShow(B(_lC(_Fz[1],_Fz[2]))[1]),_FB=_FA;return fromJSStr(_FB);}),_4L);});}:function(_4L){return new F(function(){return _i(new T(function(){var _FC=E(E(_Fy[1])[1]);if(!_FC){var _FD=jsRound(E(_Fx)[1]),_FE=_FD,_FF=decodeFloat(_FE),_FG=_FF[1],_FH=_FF[2];if(_FH>=0){var _FI=jsShow(B(_lO(B(_qj(B(_5Q(_FG)),_FH))))),_FJ=_FI,_FK=fromJSStr(_FJ);}else{var _FL=jsShow(_FG>> -_FH),_FM=_FL,_FK=fromJSStr(_FM);}var _FN=_FK,_FO=_FN,_FP=_FO,_FQ=_FP;}else{var _FR=B(_x0(E(_Fx)[1]));if(_FC>=0){var _FS=B(_ET(10,_FC)),_FT=jsRound(B(_lC(_FR[1],_FR[2]))[1]*_FS),_FU=_FT,_FV=jsShow((_FU&4294967295)/_FS),_FW=_FV,_FX=fromJSStr(_FW);}else{var _FX=E(_EK);}var _FY=_FX,_FZ=_FY,_G0=_FZ,_G1=_G0,_FQ=_G1;}var _G2=_FQ;return _G2;}),_4L);});};},_G3=function(_G4){var _G5=u_towupper(_G4),_G6=_G5;return _G6>>>0>1114111?B(_7h(_G6)):_G6;},_G7=function(_G8){return [0,B(_G3(E(_G8)[1]))];},_G9=function(_Ga,_Gb,_Gc){var _Gd=E(_Gc);switch(_Gd[0]){case 3:var _Ge=_Gd[1],_Gf=u_iswupper(_Ga),_Gg=_Gf;switch(B(_Ew(_Ga))){case 101:var _Gh=B(_D4(_yh,_o1,_oB,_oz,_oG,_ov,_oM,_oI,_oQ,_CE,new T(function(){var _Gi=E(_Gb);return _Gi[1]>=0?[1,_Gi]:[0];}),_Ge));break;case 102:var _Gh=B(_D4(_yh,_o1,_oB,_oz,_oG,_ov,_oM,_oI,_oQ,_CF,new T(function(){var _Gj=E(_Gb);return _Gj[1]>=0?[1,_Gj]:[0];}),_Ge));break;case 103:var _Gk=E(_Gb),_Gh=_Gk[1]>=0?B(A(_Fv,[[1,_Gk],_Ge,_9])):B(A(_Fv,[_6g,_Ge,_9]));break;default:var _Gh=E(_EI);}var _Gl=_Gh,_Gm=E(_Gg);if(!_Gm){var _Gn=E(_Gl);if(!_Gn[0]){return [0,_9,_9];}else{var _Go=_Gn[1],_Gp=_Gn[2],_Gq=E(_Go),_Gr=_Gq[1],_Gs=E(_Gr);return _Gs==45?[0,_EH,_Gp]:[0,_9,_Gn];}}else{var _Gt=B(_U(_G7,_Gl));if(!_Gt[0]){return [0,_9,_9];}else{var _Gu=_Gt[1],_Gv=_Gt[2],_Gw=E(_Gu),_Gx=_Gw[1],_Gy=E(_Gx);return _Gy==45?[0,_EH,_Gv]:[0,_9,_Gt];}}break;case 4:var _Gz=_Gd[1],_GA=u_iswupper(_Ga),_GB=_GA;switch(B(_Ew(_Ga))){case 101:var _GC=B(_D4(_wh,_mG,_oc,_o9,_oh,_o5,_on,_oj,_or,_CE,new T(function(){var _GD=E(_Gb);return _GD[1]>=0?[1,_GD]:[0];}),_Gz));break;case 102:var _GC=B(_D4(_wh,_mG,_oc,_o9,_oh,_o5,_on,_oj,_or,_CF,new T(function(){var _GE=E(_Gb);return _GE[1]>=0?[1,_GE]:[0];}),_Gz));break;case 103:var _GF=E(_Gb),_GC=_GF[1]>=0?B(A(_EZ,[[1,_GF],_Gz,_9])):B(A(_EZ,[_6g,_Gz,_9]));break;default:var _GC=E(_EF);}var _GG=_GC,_GH=E(_GB);if(!_GH){var _GI=E(_GG);if(!_GI[0]){return [0,_9,_9];}else{var _GJ=_GI[1],_GK=_GI[2],_GL=E(_GJ),_GM=_GL[1],_GN=E(_GM);return _GN==45?[0,_EH,_GK]:[0,_9,_GI];}}else{var _GO=B(_U(_G7,_GG));if(!_GO[0]){return [0,_9,_9];}else{var _GP=_GO[1],_GQ=_GO[2],_GR=E(_GP),_GS=_GR[1],_GT=E(_GS);return _GT==45?[0,_EH,_GQ]:[0,_9,_GO];}}break;default:return E(_ED);}},_GU=[0,0],_GV=function(_GW){return new F(function(){return _D(0,_GW,_9);});},_GX=function(_GY,_GZ){while(1){var _H0=E(_GY);if(!_H0[0]){return E(_GZ);}else{_GY=_H0[2];var _H1=_GZ+1|0;_GZ=_H1;continue;}}},_H2=[0,48],_H3=function(_H4,_H5){var _H6=_H4-B(_GX(_H5,0))|0;if(_H6>0){var _H7=function(_H8){return _H8>1?[1,_H2,new T(function(){return B(_H7(_H8-1|0));})]:E([1,_H2,_H5]);};return new F(function(){return _H7(_H6);});}else{return E(_H5);}},_H9=[0,0],_Ha=[0,-2147483648],_Hb=function(_Hc,_Hd){while(1){var _He=(function(_Hf,_Hg){var _Hh=E(_Hg);switch(_Hh[0]){case 0:_Hc=_H9;_Hd=[2,_Ha,new T(function(){return B(_5Q(E(_Hh[1])[1]));})];return null;case 2:var _Hi=_Hh[2];return !B(_s(_Hi,_GU))?[0,_9,new T(function(){return B(_H3(E(_Hf)[1],B(_GV(_Hi))));})]:[0,_EH,new T(function(){return B(_H3(E(_Hf)[1],B(_D(0,B(_5K(_Hi)),_9))));})];default:return E(_ED);}})(_Hc,_Hd);if(_He!=null){return _He;}}},_Hj=[1,_i6,_9],_Hk=function(_Hl){return new F(function(){return err(B(unAppCStr("Char.digitToInt: not a digit ",new T(function(){var _Hm=E(_Hl);return _Hm==39?E(_i8):[1,_i6,new T(function(){return B(_hQ(_Hm,_Hj));})];}))));});},_Hn=function(_Ho){var _Hp=function(_Hq){var _Hr=function(_Hs){if(_Ho<65){return new F(function(){return _Hk(_Ho);});}else{if(_Ho>70){return new F(function(){return _Hk(_Ho);});}else{return (_Ho-65|0)+10|0;}}};if(_Ho<97){return new F(function(){return _Hr(_);});}else{if(_Ho>102){return new F(function(){return _Hr(_);});}else{return (_Ho-97|0)+10|0;}}};if(_Ho<48){return new F(function(){return _Hp(_);});}else{if(_Ho>57){return new F(function(){return _Hp(_);});}else{return _Ho-48|0;}}},_Ht=function(_Hu,_Hv){while(1){var _Hw=(function(_Hx,_Hy){var _Hz=E(_Hy);if(!_Hz[0]){return [0,_Hx,_9];}else{var _HA=E(_Hz[1])[1];if(_HA<48){return [0,_Hx,_Hz];}else{if(_HA>57){return [0,_Hx,_Hz];}else{_Hu=new T(function(){return [0,(imul(E(_Hx)[1],10)|0)+B(_Hn(_HA))|0];});_Hv=_Hz[2];return null;}}}})(_Hu,_Hv);if(_Hw!=null){return _Hw;}}},_HB=new T(function(){return B(unCStr("argument list ended prematurely"));}),_HC=new T(function(){return B(_EA(_HB));}),_HD=[0,-1],_HE=function(_HF){return [0,E(_HF)[1]];},_HG=function(_HH){var _HI=E(_HH);switch(_HI[0]){case 0:return new F(function(){return _HE(_HI[1]);});break;case 2:return new F(function(){return _tp(_HI[2]);});break;default:return E(_ED);}},_HJ=function(_HK,_HL,_HM,_HN,_HO){while(1){var _HP=(function(_HQ,_HR,_HS,_HT,_HU){var _HV=E(_HT);if(!_HV[0]){return [0,_H9,_HD,_HQ,_HR,_HS,_9,_HU];}else{var _HW=_HV[2],_HX=E(E(_HV[1])[1]);switch(_HX){case 42:var _HY=new T(function(){var _HZ=E(_HU);return _HZ[0]==0?E(_HC):[0,_HZ[2],new T(function(){return B(_HG(_HZ[1]));})];}),_I0=new T(function(){var _I1=E(_HW);if(!_I1[0]){var _I2=[0,_HD,_9,new T(function(){return E(E(_HY)[1]);})];}else{if(E(E(_I1[1])[1])==46){var _I3=E(_I1[2]);if(!_I3[0]){var _I4=B(_Ht(_H9,_9)),_I5=[0,_I4[1],_I4[2],new T(function(){return E(E(_HY)[1]);})];}else{if(E(E(_I3[1])[1])==42){var _I6=new T(function(){var _I7=E(E(_HY)[1]);return _I7[0]==0?E(_HC):[0,_I7[2],new T(function(){return B(_HG(_I7[1]));})];}),_I8=[0,new T(function(){return E(E(_I6)[2]);}),_I3[2],new T(function(){return E(E(_I6)[1]);})];}else{var _I9=B(_Ht(_H9,_I3)),_I8=[0,_I9[1],_I9[2],new T(function(){return E(E(_HY)[1]);})];}var _Ia=_I8,_I5=_Ia;}var _Ib=_I5;}else{var _Ib=[0,_HD,_I1,new T(function(){return E(E(_HY)[1]);})];}var _Ic=_Ib,_I2=_Ic;}return _I2;});return [0,new T(function(){return E(E(_HY)[2]);}),new T(function(){return E(E(_I0)[1]);}),_HQ,_HR,_HS,new T(function(){return E(E(_I0)[2]);}),new T(function(){return E(E(_I0)[3]);})];case 43:var _Id=_HQ,_Ie=_HR;_HM=_6V;_HN=_HW;var _If=_HU;_HK=_Id;_HL=_Ie;_HO=_If;return null;case 45:_HK=_6V;var _Ie=_HR,_Ig=_HS;_HN=_HW;var _If=_HU;_HL=_Ie;_HM=_Ig;_HO=_If;return null;case 46:var _Ih=new T(function(){var _Ii=E(_HW);if(!_Ii[0]){var _Ij=B(_Ht(_H9,_9)),_Ik=[0,_Ij[1],_Ij[2],_HU];}else{if(E(E(_Ii[1])[1])==42){var _Il=new T(function(){var _Im=E(_HU);return _Im[0]==0?E(_HC):[0,_Im[2],new T(function(){return B(_HG(_Im[1]));})];}),_In=[0,new T(function(){return E(E(_Il)[2]);}),_Ii[2],new T(function(){return E(E(_Il)[1]);})];}else{var _Io=B(_Ht(_H9,_Ii)),_In=[0,_Io[1],_Io[2],_HU];}var _Ip=_In,_Ik=_Ip;}return _Ik;});return [0,_H9,new T(function(){return E(E(_Ih)[1]);}),_HQ,_HR,_HS,new T(function(){return E(E(_Ih)[2]);}),new T(function(){return E(E(_Ih)[3]);})];case 48:var _Id=_HQ;_HL=_6V;var _Ig=_HS;_HN=_HW;var _If=_HU;_HK=_Id;_HM=_Ig;_HO=_If;return null;default:if(_HX<48){return [0,_H9,_HD,_HQ,_HR,_HS,_HV,_HU];}else{if(_HX>57){return [0,_H9,_HD,_HQ,_HR,_HS,_HV,_HU];}else{var _Iq=new T(function(){var _Ir=B(_Ht(_H9,_HV));return [0,_Ir[1],_Ir[2]];}),_Is=new T(function(){var _It=E(E(_Iq)[2]);if(!_It[0]){var _Iu=[0,_HD,_9,_HU];}else{if(E(E(_It[1])[1])==46){var _Iv=E(_It[2]);if(!_Iv[0]){var _Iw=B(_Ht(_H9,_9)),_Ix=[0,_Iw[1],_Iw[2],_HU];}else{if(E(E(_Iv[1])[1])==42){var _Iy=new T(function(){var _Iz=E(_HU);return _Iz[0]==0?E(_HC):[0,_Iz[2],new T(function(){return B(_HG(_Iz[1]));})];}),_IA=[0,new T(function(){return E(E(_Iy)[2]);}),_Iv[2],new T(function(){return E(E(_Iy)[1]);})];}else{var _IB=B(_Ht(_H9,_Iv)),_IA=[0,_IB[1],_IB[2],_HU];}var _IC=_IA,_Ix=_IC;}var _ID=_Ix;}else{var _ID=[0,_HD,_It,_HU];}var _IE=_ID,_Iu=_IE;}var _IF=_Iu;return _IF;});return [0,new T(function(){return E(E(_Iq)[1]);}),new T(function(){return E(E(_Is)[1]);}),_HQ,_HR,_HS,new T(function(){return E(E(_Is)[2]);}),new T(function(){return E(E(_Is)[3]);})];}}}}})(_HK,_HL,_HM,_HN,_HO);if(_HP!=null){return _HP;}}},_IG=new T(function(){return B(unCStr("formatting string ended prematurely"));}),_IH=new T(function(){return B(_EA(_IG));}),_II=function(_IJ,_IK){if(!B(_s(_IK,_IJ))){if(!B(_lu(_IJ,_GU))){var _IL=B(_vm(_IK,_IJ));return new F(function(){return _i(B(_II(_IJ,_IL[1])),[1,new T(function(){return [0,B(_yq(B(_7j(_IL[2]))))];}),_9]);});}else{return E(_sb);}}else{return [1,new T(function(){return [0,B(_yq(B(_7j(_IK))))];}),_9];}},_IM=[0,2],_IN=function(_IO,_IP,_IQ){var _IR=E(_IQ);switch(_IR[0]){case 0:return new F(function(){return _II(_IO,B(_5Q(E(_IR[1])[1])));});break;case 2:var _IS=_IR[2],_IT=E(_IP)[1];if(!B(_s(_IS,_GU))){return new F(function(){return _H3(_IT,B(_II(_IO,_IS)));});}else{return new F(function(){return _H3(_IT,B(_II(_IO,B(_5A(B(_5K(B(_5S(_IM,_IR[1])))),_IS)))));});}break;default:return E(_ED);}},_IU=[0,37],_IV=[0,16],_IW=[0,10],_IX=[0,8],_IY=[0,43],_IZ=[1,_IY,_9],_J0=[0,32],_J1=function(_J2){return new F(function(){return _EA(new T(function(){return B(unAppCStr("bad formatting char ",[1,_J2,_9]));}));});},_J3=function(_J4,_J5){var _J6=E(_J4);if(!_J6){return [0];}else{var _J7=E(_J5);return _J7[0]==0?[0]:[1,_J7[1],new T(function(){return B(_J3(_J6-1|0,_J7[2]));})];}},_J8=function(_J9,_Ja){var _Jb=E(_J9);if(!_Jb[0]){return E(_Ja)[0]==0?[0]:E(_IH);}else{var _Jc=_Jb[2],_Jd=E(_Jb[1]);if(E(_Jd[1])==37){var _Je=function(_Jf){var _Jg=E(_Ja);if(!_Jg[0]){return E(_HC);}else{var _Jh=B(_HJ(_6U,_6U,_6U,_Jc,_Jg)),_Ji=_Jh[2],_Jj=_Jh[4],_Jk=E(_Jh[6]);if(!_Jk[0]){return E(_IH);}else{var _Jl=_Jk[2],_Jm=E(_Jh[7]);if(!_Jm[0]){return E(_HC);}else{var _Jn=_Jm[1],_Jo=_Jm[2],_Jp=E(_Jk[1]),_Jq=function(_Jr,_Js){var _Jt=new T(function(){var _Ju=B(_GX(_Js,0)),_Jv=B(_GX(_Jr,0)),_Jw=E(_Jh[1])[1];if((_Ju+_Jv|0)>=_Jw){var _Jx=[0];}else{var _Jy=_Jw-(_Ju+_Jv|0)|0;if(_Jy>0){if(_Jy<0){var _Jz=[0];}else{var _JA=new T(function(){return [1,new T(function(){return !E(_Jj)?E(_J0):E(_H2);}),_JA];}),_Jz=B(_J3(_Jy,_JA));}var _JB=_Jz,_JC=_JB;}else{var _JC=[0];}var _JD=_JC,_JE=_JD,_JF=_JE,_Jx=_JF;}var _JG=_Jx,_JH=_JG,_JI=_JH,_JJ=_JI,_JK=_JJ;return _JK;});return !E(_Jh[3])?!E(_Jj)?B(_i(_Jt,new T(function(){return B(_i(_Jr,_Js));}))):B(_i(_Jr,new T(function(){return B(_i(_Jt,_Js));}))):B(_i(_Jr,new T(function(){return B(_i(_Js,_Jt));})));},_JL=function(_JM,_JN){var _JO=E(_JM);return _JO[0]==0?!E(_Jh[5])?B(_Jq(_9,_JN)):B(_Jq(_IZ,_JN)):B(_Jq(_JO,_JN));};switch(E(_Jp[1])){case 69:var _JP=B(_G9(69,_Ji,_Jn));return new F(function(){return _i(B(_JL(_JP[1],_JP[2])),new T(function(){return B(_J8(_Jl,_Jo));}));});break;case 71:var _JQ=B(_G9(71,_Ji,_Jn));return new F(function(){return _i(B(_JL(_JQ[1],_JQ[2])),new T(function(){return B(_J8(_Jl,_Jo));}));});break;case 88:return new F(function(){return _i(B(_Jq(_9,new T(function(){return B(_U(_G7,B(_IN(_IV,_Ji,_Jn))));}))),new T(function(){return B(_J8(_Jl,_Jo));}));});break;case 99:return new F(function(){return _i(B(_Jq(_9,[1,new T(function(){var _JR=E(_Jn);switch(_JR[0]){case 0:var _JS=E(_JR[1])[1];if(_JS>>>0>1114111){var _JT=B(_7h(_JS));}else{var _JT=[0,_JS];}var _JU=_JT,_JV=_JU,_JW=_JV,_JX=_JW,_JY=_JX;break;case 2:var _JZ=B(_7j(_JR[2]));if(_JZ>>>0>1114111){var _K0=B(_7h(_JZ));}else{var _K0=[0,_JZ];}var _K1=_K0,_K2=_K1,_K3=_K2,_JY=_K3;break;default:var _JY=E(_ED);}return _JY;}),_9])),new T(function(){return B(_J8(_Jl,_Jo));}));});break;case 100:var _K4=B(_Hb(_Ji,_Jn));return new F(function(){return _i(B(_JL(_K4[1],_K4[2])),new T(function(){return B(_J8(_Jl,_Jo));}));});break;case 101:var _K5=B(_G9(101,_Ji,_Jn));return new F(function(){return _i(B(_JL(_K5[1],_K5[2])),new T(function(){return B(_J8(_Jl,_Jo));}));});break;case 102:var _K6=B(_G9(102,_Ji,_Jn));return new F(function(){return _i(B(_JL(_K6[1],_K6[2])),new T(function(){return B(_J8(_Jl,_Jo));}));});break;case 103:var _K7=B(_G9(103,_Ji,_Jn));return new F(function(){return _i(B(_JL(_K7[1],_K7[2])),new T(function(){return B(_J8(_Jl,_Jo));}));});break;case 105:var _K8=B(_Hb(_Ji,_Jn));return new F(function(){return _i(B(_JL(_K8[1],_K8[2])),new T(function(){return B(_J8(_Jl,_Jo));}));});break;case 111:return new F(function(){return _i(B(_Jq(_9,new T(function(){return B(_IN(_IX,_Ji,_Jn));}))),new T(function(){return B(_J8(_Jl,_Jo));}));});break;case 115:return new F(function(){return _i(B(_Jq(_9,new T(function(){var _K9=E(_Jn);if(_K9[0]==1){var _Ka=_K9[1],_Kb=E(_Ji)[1];if(_Kb<0){var _Kc=E(_Ka);}else{var _Kc=_Kb>0?B(_J3(_Kb,_Ka)):[0];}var _Kd=_Kc,_Ke=_Kd,_Kf=_Ke;}else{var _Kf=E(_ED);}return _Kf;}))),new T(function(){return B(_J8(_Jl,_Jo));}));});break;case 117:return new F(function(){return _i(B(_Jq(_9,new T(function(){return B(_IN(_IW,_Ji,_Jn));}))),new T(function(){return B(_J8(_Jl,_Jo));}));});break;case 120:return new F(function(){return _i(B(_Jq(_9,new T(function(){return B(_IN(_IV,_Ji,_Jn));}))),new T(function(){return B(_J8(_Jl,_Jo));}));});break;default:return new F(function(){return _J1(_Jp);});}}}}},_Kg=E(_Jc);if(!_Kg[0]){return new F(function(){return _Je(_);});}else{if(E(E(_Kg[1])[1])==37){return [1,_IU,new T(function(){return B(_J8(_Kg[2],_Ja));})];}else{return new F(function(){return _Je(_);});}}}else{return [1,_Jd,new T(function(){return B(_J8(_Jc,_Ja));})];}}},_Kh=function(_Ki){var _Kj=E(_Ki);if(!_Kj[0]){return [0];}else{var _Kk=E(E(_Kj[1])[2]),_Kl=function(_Km){var _Kn=E(_Km);return _Kn[0]==0?E(new T(function(){return B(_Kh(_Kj[2]));})):[1,_Kn[1],new T(function(){return B(_Kl(_Kn[2]));})];};return new F(function(){return _Kl(B(_J8(_l2,new T(function(){return B(_l3([1,[1,new T(function(){return B(_U(_l0,_Kk[2]));})],[1,[1,new T(function(){return B(_U(_l0,_Kk[1]));})],_9]],_9));}))));});}},_Ko=function(_Kp,_Kq,_Kr){var _Ks=function(_Kt){var _Ku=E(_Kt);return _Ku[0]==0?E(new T(function(){return B(_Kh(_Kr));})):[1,_Ku[1],new T(function(){return B(_Ks(_Ku[2]));})];};return new F(function(){return _Ks(B(_J8(_l2,new T(function(){return B(_l3([1,[1,new T(function(){return B(_U(_l0,_Kq));})],[1,[1,new T(function(){return B(_U(_l0,_Kp));})],_9]],_9));}))));});},_Kv=function(_Kw,_Kx,_Ky,_Kz){return new F(function(){return A(_Kw,[function(_){var _KA=jsSet(E(_Kx)[1],toJSStr(E(_Ky)),toJSStr(E(_Kz)));return _4m;}]);});},_KB=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:108:3-8"));}),_KC=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_KD=new T(function(){return B(unCStr("base"));}),_KE=new T(function(){return B(unCStr("IOException"));}),_KF=new T(function(){var _KG=hs_wordToWord64(4053623282),_KH=_KG,_KI=hs_wordToWord64(3693590983),_KJ=_KI;return [0,_KH,_KJ,[0,_KH,_KJ,_KD,_KC,_KE],_9];}),_KK=function(_KL){return E(_KF);},_KM=function(_KN){var _KO=E(_KN);return new F(function(){return _20(B(_1Y(_KO[1])),_KK,_KO[2]);});},_KP=new T(function(){return B(unCStr(": "));}),_KQ=[0,41],_KR=new T(function(){return B(unCStr(" ("));}),_KS=new T(function(){return B(unCStr("already exists"));}),_KT=new T(function(){return B(unCStr("does not exist"));}),_KU=new T(function(){return B(unCStr("protocol error"));}),_KV=new T(function(){return B(unCStr("failed"));}),_KW=new T(function(){return B(unCStr("invalid argument"));}),_KX=new T(function(){return B(unCStr("inappropriate type"));}),_KY=new T(function(){return B(unCStr("hardware fault"));}),_KZ=new T(function(){return B(unCStr("unsupported operation"));}),_L0=new T(function(){return B(unCStr("timeout"));}),_L1=new T(function(){return B(unCStr("resource vanished"));}),_L2=new T(function(){return B(unCStr("interrupted"));}),_L3=new T(function(){return B(unCStr("resource busy"));}),_L4=new T(function(){return B(unCStr("resource exhausted"));}),_L5=new T(function(){return B(unCStr("end of file"));}),_L6=new T(function(){return B(unCStr("illegal operation"));}),_L7=new T(function(){return B(unCStr("permission denied"));}),_L8=new T(function(){return B(unCStr("user error"));}),_L9=new T(function(){return B(unCStr("unsatisified constraints"));}),_La=new T(function(){return B(unCStr("system error"));}),_Lb=function(_Lc,_Ld){switch(E(_Lc)){case 0:return new F(function(){return _i(_KS,_Ld);});break;case 1:return new F(function(){return _i(_KT,_Ld);});break;case 2:return new F(function(){return _i(_L3,_Ld);});break;case 3:return new F(function(){return _i(_L4,_Ld);});break;case 4:return new F(function(){return _i(_L5,_Ld);});break;case 5:return new F(function(){return _i(_L6,_Ld);});break;case 6:return new F(function(){return _i(_L7,_Ld);});break;case 7:return new F(function(){return _i(_L8,_Ld);});break;case 8:return new F(function(){return _i(_L9,_Ld);});break;case 9:return new F(function(){return _i(_La,_Ld);});break;case 10:return new F(function(){return _i(_KU,_Ld);});break;case 11:return new F(function(){return _i(_KV,_Ld);});break;case 12:return new F(function(){return _i(_KW,_Ld);});break;case 13:return new F(function(){return _i(_KX,_Ld);});break;case 14:return new F(function(){return _i(_KY,_Ld);});break;case 15:return new F(function(){return _i(_KZ,_Ld);});break;case 16:return new F(function(){return _i(_L0,_Ld);});break;case 17:return new F(function(){return _i(_L1,_Ld);});break;default:return new F(function(){return _i(_L2,_Ld);});}},_Le=[0,125],_Lf=new T(function(){return B(unCStr("{handle: "));}),_Lg=function(_Lh,_Li,_Lj,_Lk,_Ll,_Lm){var _Ln=new T(function(){var _Lo=new T(function(){return B(_Lb(_Li,new T(function(){var _Lp=E(_Lk);return _Lp[0]==0?E(_Lm):B(_i(_KR,new T(function(){return B(_i(_Lp,[1,_KQ,_Lm]));})));})));}),_Lq=E(_Lj);return _Lq[0]==0?E(_Lo):B(_i(_Lq,new T(function(){return B(_i(_KP,_Lo));})));}),_Lr=E(_Ll);if(!_Lr[0]){var _Ls=E(_Lh);if(!_Ls[0]){return E(_Ln);}else{var _Lt=E(_Ls[1]);return _Lt[0]==0?B(_i(_Lf,new T(function(){return B(_i(_Lt[1],[1,_Le,new T(function(){return B(_i(_KP,_Ln));})]));}))):B(_i(_Lf,new T(function(){return B(_i(_Lt[1],[1,_Le,new T(function(){return B(_i(_KP,_Ln));})]));})));}}else{return new F(function(){return _i(_Lr[1],new T(function(){return B(_i(_KP,_Ln));}));});}},_Lu=function(_Lv){var _Lw=E(_Lv);return new F(function(){return _Lg(_Lw[1],_Lw[2],_Lw[3],_Lw[4],_Lw[6],_9);});},_Lx=function(_Ly,_Lz){var _LA=E(_Ly);return new F(function(){return _Lg(_LA[1],_LA[2],_LA[3],_LA[4],_LA[6],_Lz);});},_LB=function(_LC,_LD){return new F(function(){return _2l(_Lx,_LC,_LD);});},_LE=function(_LF,_LG,_LH){var _LI=E(_LG);return new F(function(){return _Lg(_LI[1],_LI[2],_LI[3],_LI[4],_LI[6],_LH);});},_LJ=[0,_LE,_Lu,_LB],_LK=new T(function(){return [0,_KK,_LJ,_LL,_KM];}),_LL=function(_LM){return [0,_LK,_LM];},_LN=7,_LO=function(_LP){return [0,_6g,_LN,_9,_LP,_6g,_6g];},_LQ=function(_LR,_){return new F(function(){return die(new T(function(){return B(_LL(new T(function(){return B(_LO(_LR));})));}));});},_LS=function(_LT,_){return new F(function(){return _LQ(_LT,_);});},_LU=new T(function(){return B(unCStr("innerHTML"));}),_LV=new T(function(){return B(unCStr("\u5b9f\u7e3e\u540d"));}),_LW=new T(function(){return B(unCStr("\u5185\u5bb9"));}),_LX=function(_LY,_LZ,_){var _M0=B(A(_LY,[_])),_M1=_M0;return new F(function(){return A(_LZ,[_M1,_]);});},_M2=function(_M3,_){return _M3;},_M4=function(_M5,_M6,_){var _M7=B(A(_M5,[_])),_M8=_M7;return new F(function(){return A(_M6,[_]);});},_M9=[0,_LX,_M4,_M2,_LS],_Ma=[0,_M9,_50],_Mb=function(_Mc){return E(E(_Mc)[1]);},_Md=function(_Me){return E(E(_Me)[1]);},_Mf=function(_Mg){return E(E(_Mg)[2]);},_Mh=function(_Mi){return E(E(_Mi)[3]);},_Mj=function(_Mk,_Ml){var _Mm=new T(function(){return B(_Mb(_Mk));});return function(_Mn){return new F(function(){return A(new T(function(){return B(_Md(_Mm));}),[new T(function(){return B(A(_Mf,[_Mk,_Ml]));}),function(_Mo){return new F(function(){return A(new T(function(){return B(_Mh(_Mm));}),[[0,_Mo,_Mn]]);});}]);});};},_Mp=function(_Mq){return new F(function(){return _Mj(_Ma,_Mq);});},_Mr=new T(function(){return B(unCStr("achievements"));}),_Ms=function(_Mt){return function(_Mu,_){var _Mv=jsFind(toJSStr(E(_Mr))),_Mw=_Mv,_Mx=E(_Mw);return _Mx[0]==0?B(_LS(_KB,_)):B(A(_Kv,[_Mp,_Mx[1],_LU,new T(function(){return B(_Ko(_LV,_LW,new T(function(){return B(_jT(_Mt));})));}),_Mu,_]));};},_My=new T(function(){return [0,"click"];}),_Mz=function(_MA){return _MA>0;},_MB=function(_MC){var _MD=B(A(_MC,[_])),_ME=_MD;return E(_ME);},_MF=function(_MG){return new F(function(){return _MB(function(_){var _=0;return new F(function(){return eval(_MG);});});});},_MH=new T(function(){return B(_MF("(function(x) {return x === null;})"));}),_MI=new T(function(){return B(unCStr("No such value"));}),_MJ=[0,_MI],_MK=new T(function(){return B(unCStr("Invalid JSON!"));}),_ML=[0,_MK],_MM=new T(function(){return [0,"(function(k) {return localStorage.getItem(k);})"];}),_MN=function(_MO,_MP,_){var _MQ=B(A(_MF,[E(_MM)[1],E(toJSStr(E(_MP))),_])),_MR=_MQ;return new T(function(){if(!B(_MB(function(_){var _=0,_MS=B(A(_MH,[E(_MR),_])),_MT=_MS;return new T(function(){return B(_Mz(_MT));});}))){var _MU=String(_MR),_MV=_MU,_MW=jsParseJSON(_MV),_MX=_MW,_MY=E(_MX),_MZ=_MY[0]==0?E(_ML):B(A(_1g,[_MO,_MY[1]]));}else{var _MZ=E(_MJ);}return _MZ;});},_N0=[0,10],_N1=[1,_N0,_9],_N2=function(_N3,_N4,_){var _N5=jsWriteHandle(E(_N3)[1],toJSStr(E(_N4)));return _4m;},_N6=function(_N7,_N8,_){var _N9=E(_N7),_Na=jsWriteHandle(_N9[1],toJSStr(E(_N8)));return new F(function(){return _N2(_N9,_N1,_);});},_Nb=[0,0],_Nc=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:204:3-8"));}),_Nd=new T(function(){return B(unCStr("Aichan"));}),_Ne=new T(function(){return [0,toJSStr(_9)];}),_Nf=[0,93],_Ng=[1,_Nf,_9],_Nh=new T(function(){return [0,toJSStr(_Ng)];}),_Ni=[0,125],_Nj=[1,_Ni,_9],_Nk=new T(function(){return [0,toJSStr(_Nj)];}),_Nl=[0,58],_Nm=[1,_Nl,_9],_Nn=new T(function(){return [0,toJSStr(_Nm)];}),_No=[0,44],_Np=[1,_No,_9],_Nq=new T(function(){return [0,toJSStr(_Np)];}),_Nr=new T(function(){return [0,"false"];}),_Ns=function(_Nt){var _Nu=jsShow(E(_Nt)[1]),_Nv=_Nu;return [0,_Nv];},_Nw=function(_Nx){var _Ny=jsStringify(E(_Nx)[1]),_Nz=_Ny;return [0,_Nz];},_NA=new T(function(){return [0,"null"];}),_NB=[0,91],_NC=[1,_NB,_9],_ND=new T(function(){return [0,toJSStr(_NC)];}),_NE=[0,123],_NF=[1,_NE,_9],_NG=new T(function(){return [0,toJSStr(_NF)];}),_NH=[0,34],_NI=[1,_NH,_9],_NJ=new T(function(){return [0,toJSStr(_NI)];}),_NK=new T(function(){return [0,"true"];}),_NL=function(_NM,_NN){var _NO=E(_NN);switch(_NO[0]){case 0:return [0,new T(function(){return B(_Ns(_NO[1]));}),_NM];case 1:return [0,new T(function(){return B(_Nw(_NO[1]));}),_NM];case 2:return !E(_NO[1])?[0,_Nr,_NM]:[0,_NK,_NM];case 3:var _NP=E(_NO[1]);return _NP[0]==0?[0,_ND,[1,_Nh,_NM]]:[0,_ND,new T(function(){var _NQ=B(_NL(new T(function(){var _NR=function(_NS){var _NT=E(_NS);return _NT[0]==0?E([1,_Nh,_NM]):[1,_Nq,new T(function(){var _NU=B(_NL(new T(function(){return B(_NR(_NT[2]));}),_NT[1]));return [1,_NU[1],_NU[2]];})];};return B(_NR(_NP[2]));}),_NP[1]));return [1,_NQ[1],_NQ[2]];})];case 4:var _NV=E(_NO[1]);if(!_NV[0]){return [0,_NG,[1,_Nk,_NM]];}else{var _NW=E(_NV[1]);return [0,_NG,[1,new T(function(){return B(_Nw(_NW[1]));}),[1,_Nn,new T(function(){var _NX=B(_NL(new T(function(){var _NY=function(_NZ){var _O0=E(_NZ);if(!_O0[0]){return E([1,_Nk,_NM]);}else{var _O1=E(_O0[1]);return [1,_Nq,[1,_NJ,[1,_O1[1],[1,_NJ,[1,_Nn,new T(function(){var _O2=B(_NL(new T(function(){return B(_NY(_O0[2]));}),_O1[2]));return [1,_O2[1],_O2[2]];})]]]]];}};return B(_NY(_NV[2]));}),_NW[2]));return [1,_NX[1],_NX[2]];})]]];}break;default:return [0,_NA,_NM];}},_O3=function(_O4){var _O5=jsCat(new T(function(){var _O6=B(_NL(_9,_O4));return [1,_O6[1],_O6[2]];}),E(_Ne)[1]),_O7=_O5;return E(_O7);},_O8=new T(function(){return [0,"(function(k,v) {localStorage.setItem(k,v);})"];}),_O9=function(_Oa,_Ob){return function(_Oc,_){var _Od=B(A(new T(function(){return B(A(_MF,[E(_O8)[1],E(toJSStr(E(_Ob)))]));}),[E(B(_O3(B(A(new T(function(){return B(_dU(_Oa));}),[_Oc]))))),_])),_Oe=_Od;return _4m;};},_Of=new T(function(){return B(_O9(_gr,_Nd));}),_Og=function(_Oh,_){var _Oi=B(A(_Of,[_Oh,_])),_Oj=_Oi;return new F(function(){return A(_Ms,[new T(function(){return E(E(_Oh)[7]);}),_Oh,_]);});},_Ok=function(_Ol,_Om){while(1){var _On=E(_Ol);if(!_On[0]){var _Oo=E(_On[1]);if(_Oo==(-2147483648)){_Ol=[1,I_fromInt(-2147483648)];continue;}else{var _Op=E(_Om);if(!_Op[0]){return [0,B(_rt(_Oo,_Op[1]))];}else{_Ol=[1,I_fromInt(_Oo)];_Om=_Op;continue;}}}else{var _Oq=_On[1],_Or=E(_Om);return _Or[0]==0?[0,I_toInt(I_div(_Oq,I_fromInt(_Or[1])))]:[1,I_div(_Oq,_Or[1])];}}},_Os=new T(function(){return B(unCStr("%.2f"));}),_Ot=[0,2],_Ou=[0,3],_Ov=new T(function(){return B(_lu(_Ou,_1J));}),_Ow=[1,_ic,_9],_Ox=function(_Oy,_Oz){while(1){var _OA=E(_Oy);if(!_OA[0]){var _OB=_OA[1],_OC=E(_Oz);if(!_OC[0]){var _OD=_OC[1],_OE=subC(_OB,_OD);if(!E(_OE[2])){return [0,_OE[1]];}else{_Oy=[1,I_fromInt(_OB)];_Oz=[1,I_fromInt(_OD)];continue;}}else{_Oy=[1,I_fromInt(_OB)];_Oz=_OC;continue;}}else{var _OF=E(_Oz);if(!_OF[0]){_Oy=_OA;_Oz=[1,I_fromInt(_OF[1])];continue;}else{return [1,I_sub(_OA[1],_OF[1])];}}}},_OG=function(_OH,_){var _OI=jsEval(toJSStr(B(unAppCStr("document.title = ",[1,_ic,new T(function(){return B(_ie(B(_U(_l0,B(_J8(_Os,new T(function(){return B(_l3([1,[4,new T(function(){return E(E(_OH)[1]);})],_9],_9));}))))),_Ow));})])))),_OJ=_OI,_OK=E(_OH);if(!E(_OK[6])){return [0,_4m,_OK];}else{var _OL=B(_kW(_)),_OM=_OL;return [0,_4m,[0,_OK[1],_OK[2],_OK[3],_OM,new T(function(){if(!E(_Ov)){var _ON=B(_Ok(B(_5A(B(_Ox(_OM,_OK[4])),B(_5S(_OK[5],_Ot)))),_Ou));}else{var _ON=E(_sb);}return _ON;}),_6V,_OK[7]]];}},_OO=function(_OP,_OQ){var _OR=E(_OQ);if(!_OR[0]){return [0,_9,_9];}else{var _OS=_OR[1];if(!B(A(_OP,[_OS]))){var _OT=new T(function(){var _OU=B(_OO(_OP,_OR[2]));return [0,_OU[1],_OU[2]];});return [0,[1,_OS,new T(function(){return E(E(_OT)[1]);})],new T(function(){return E(E(_OT)[2]);})];}else{return [0,_9,_OR];}}},_OV=function(_OW,_OX){var _OY=function(_OZ,_P0){return !B(_3N(_P0,_9))?[0,_OZ,new T(function(){var _P1=B(_OV(_OW,_P0));return [1,_P1[1],_P1[2]];})]:[0,_OZ,_9];};if(_OW>=0){var _P2=B(_Cv(_OW,_OX));return new F(function(){return _OY(_P2[1],_P2[2]);});}else{return new F(function(){return _OY(_9,_OX);});}},_P3=function(_P4){var _P5=E(_P4);if(!_P5[0]){return [0];}else{return new F(function(){return _i(_P5[1],new T(function(){return B(_P3(_P5[2]));}));});}},_P6=function(_P7){return E(E(_P7)[1])==46?true:false;},_P8=[0,44],_P9=[1,_P8,_9],_Pa=function(_Pb,_Pc){var _Pd=E(_Pc);return _Pd[0]==0?[0]:[1,_Pb,[1,_Pd[1],new T(function(){return B(_Pa(_Pb,_Pd[2]));})]];},_Pe=function(_Pf){var _Pg=new T(function(){var _Ph=B(_OO(_P6,_Pf));return [0,_Ph[1],_Ph[2]];}),_Pi=B(_OV(3,new T(function(){return B(_l3(E(_Pg)[1],_9));})));return new F(function(){return _i(B(_l3(B(_P3([1,_Pi[1],new T(function(){return B(_Pa(_P9,_Pi[2]));})])),_9)),new T(function(){return E(E(_Pg)[2]);}));});},_Pj=new T(function(){return B(_D(0,_1J,_9));}),_Pk=[0,-1],_Pl=new T(function(){return B(_D(0,_Pk,_9));}),_Pm=function(_Pn){return _Pn<100?B(_Pe(new T(function(){return B(_U(_l0,B(_J8(_Os,new T(function(){return B(_l3([1,[4,[0,_Pn]],_9],_9));})))));}))):B(_Pe(new T(function(){var _Po=B(_o2(_Pn)),_Pp=_Po[1],_Pq=_Po[2];if(_Pq>=0){var _Pr=B(_D(0,B(_qj(_Pp,_Pq)),_9));}else{var _Ps= -_Pq;if(_Ps<=52){var _Pt=hs_uncheckedIShiftRA64(B(_qb(_Pp)),_Ps),_Pu=_Pt,_Pv=B(_D(0,B(_pu(_Pu)),_9));}else{var _Pv=!B(_s(_Pp,_1J))?E(_Pj):E(_Pl);}var _Pw=_Pv,_Px=_Pw,_Pr=_Px;}var _Py=_Pr,_Pz=_Py;return _Pz;})));},_PA=function(_PB,_PC){return new F(function(){return (function(_PD){while(1){var _PE=E(_PD);switch(_PE[0]){case 0:var _PF=_PE[2]>>>0;if(((_PB>>>0&((_PF-1>>>0^4294967295)>>>0^_PF)>>>0)>>>0&4294967295)==_PE[1]){if(!((_PB>>>0&_PF)>>>0)){_PD=_PE[3];continue;}else{_PD=_PE[4];continue;}}else{return false;}break;case 1:return _PB==_PE[1];default:return false;}}})(_PC);});},_PG=function(_PH,_PI,_){var _PJ=jsGet(_PH,toJSStr(E(_PI))),_PK=_PJ;return new T(function(){return fromJSStr(_PK);});},_PL=new T(function(){return B(unCStr("alerts"));}),_PM=new T(function(){return B(unCStr("<div id=\"alert-%d\" class=\"alert alert-info fade in\" role=\"alert\">  <button type=\"button\" class=\"close\" data-dismiss=\"alert\"><span aria-hidden=\"true\">&times;</span><span class=\"sr-only\">Close</span></button>%s</div>%s"));}),_PN=new T(function(){return B(unCStr("\').alert(\'close\')"));}),_PO=new T(function(){return B(unCStr("\').animate({  top: \"50px\"  })"));}),_PP=new T(function(){return B(unCStr(" could be found!"));}),_PQ=function(_PR){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_i(_PR,_PP));}))));});},_PS=function(_PT,_){var _PU=E(_PL),_PV=jsFind(toJSStr(_PU)),_PW=_PV,_PX=E(_PW);if(!_PX[0]){return new F(function(){return _PQ(_PU);});}else{var _PY=E(_PX[1])[1],_PZ=B(_PG(_PY,_LU,_)),_Q0=_PZ,_Q1=B(_kW(_)),_Q2=_Q1,_Q3=jsSet(_PY,toJSStr(E(_LU)),toJSStr(B(_U(_l0,B(_J8(_PM,new T(function(){return B(_l3([1,[1,new T(function(){return B(_U(_l0,_Q0));})],[1,[1,new T(function(){return B(_U(_l0,_PT));})],[1,[2,_GU,_Q2],_9]]],_9));}))))))),_Q4=jsEval(toJSStr(B(unAppCStr("$(\'#alert-",new T(function(){return B(_i(B(_D(0,_Q2,_9)),_PO));}))))),_Q5=_Q4,_Q6=jsSetTimeout(5000,function(_){var _Q7=jsEval(toJSStr(B(unAppCStr("$(\'#alert-",new T(function(){return B(_i(B(_D(0,_Q2,_9)),_PN));}))))),_Q8=_Q7;return _4m;});return _4m;}},_Q9=new T(function(){return B(unCStr("\u3092\u8d85\u3048\u308b"));}),_Qa=new T(function(){return B(unCStr("\u5b9f\u7e3e\u7372\u5f97\uff1a "));}),_Qb=new T(function(){return B(unCStr("\u597d\u611f\u5ea6\u304c"));}),_Qc=function(_Qd,_Qe,_Qf){return function(_Qg,_){var _Qh=E(_Qg),_Qi=_Qh[7],_Qj=E(_Qd)[1];if(!B(_PA(_Qj,_Qi))){var _Qk=E(_Qh[2]);if(_Qk[1]<=E(_Qe)[1]){return [0,_4m,_Qh];}else{var _Ql=B(_PS(new T(function(){return B(_i(_Qa,_Qf));}),_)),_Qm=_Ql,_Qn=new T(function(){return B(_ff(_Qj,[0,_Qf,new T(function(){return B(_i(_Qb,new T(function(){return B(_i(B(A(_k6,[_jw,_j0,E(_Qe)[1],_9])),_Q9));})));})],_Qi));});return new F(function(){return A(_Ms,[_Qn,[0,_Qh[1],_Qk,_Qh[3],_Qh[4],_Qh[5],_Qh[6],_Qn],_]);});}}else{return [0,_4m,_Qh];}};},_Qo=[0,22],_Qp=[0,5],_Qq=new T(function(){return B(unCStr("\u4e8c\u7d1a\u30d5\u30e9\u30b0\u5efa\u7bc9\u58eb"));}),_Qr=new T(function(){return B(_Qc(_Qo,_Qp,_Qq));}),_Qs=[0,23],_Qt=[0,10],_Qu=new T(function(){return B(unCStr("\u4e00\u7d1a\u30d5\u30e9\u30b0\u5efa\u7bc9\u58eb"));}),_Qv=new T(function(){return B(_Qc(_Qs,_Qt,_Qu));}),_Qw=[0,100],_Qx=[0,24],_Qy=new T(function(){return B(unCStr("\u5927\u597d\u304d\uff8b\uff9e\uff70\uff91"));}),_Qz=new T(function(){return B(_Qc(_Qx,_Qw,_Qy));}),_QA=[0,25],_QB=[0,500],_QC=new T(function(){return B(unCStr("\u5168\u958b\u3089\u3076\u3071\u308f\u30fc"));}),_QD=new T(function(){return B(_Qc(_QA,_QB,_QC));}),_QE=new T(function(){return B(unCStr("\u4f9d\u5b58\u5ea6\u304c"));}),_QF=function(_QG,_QH,_QI){return function(_QJ,_){var _QK=E(_QJ),_QL=_QK[7],_QM=E(_QG)[1];if(!B(_PA(_QM,_QL))){var _QN=E(_QK[3]);if(_QN[1]<=E(_QH)[1]){return [0,_4m,_QK];}else{var _QO=B(_PS(new T(function(){return B(_i(_Qa,_QI));}),_)),_QP=_QO,_QQ=new T(function(){return B(_ff(_QM,[0,_QI,new T(function(){return B(_i(_QE,new T(function(){return B(_i(B(A(_k6,[_jw,_j0,E(_QH)[1],_9])),_Q9));})));})],_QL));});return new F(function(){return A(_Ms,[_QQ,[0,_QK[1],_QK[2],_QN,_QK[4],_QK[5],_QK[6],_QQ],_]);});}}else{return [0,_4m,_QK];}};},_QR=[0,1],_QS=[0,31],_QT=new T(function(){return B(unCStr("multiplier 1.0"));}),_QU=new T(function(){return B(_QF(_QS,_QR,_QT));}),_QV=[0,32],_QW=new T(function(){return B(unCStr("multiplier 10.0"));}),_QX=new T(function(){return B(_QF(_QV,_Qt,_QW));}),_QY=[0,33],_QZ=new T(function(){return B(unCStr("\u4f9d\u5b58\u6ce8\u610f\u5831"));}),_R0=new T(function(){return B(_QF(_QY,_Qw,_QZ));}),_R1=[0,34],_R2=[0,1000],_R3=new T(function(){return B(unCStr("\u4f9d\u5b58\u30c9\u30e9\u30c3\u30b0"));}),_R4=new T(function(){return B(_QF(_R1,_R2,_R3));}),_R5=new T(function(){return B(unCStr("Pattern match failure in do expression at main.hs:102:7-12"));}),_R6=new T(function(){return B(unCStr("lps"));}),_R7=new T(function(){return B(unCStr("loves"));}),_R8=new T(function(){return B(unCStr("depend"));}),_R9=new T(function(){return B(unCStr("interval"));}),_Ra=new T(function(){return B(unCStr("\u611b\u60c5\u304c"));}),_Rb=function(_Rc,_Rd,_Re){return function(_Rf,_){var _Rg=E(_Rf),_Rh=_Rg[7],_Ri=E(_Rc)[1];if(!B(_PA(_Ri,_Rh))){var _Rj=E(_Rg[1]);if(_Rj[1]<=E(_Rd)[1]){return [0,_4m,_Rg];}else{var _Rk=B(_PS(new T(function(){return B(_i(_Qa,_Re));}),_)),_Rl=_Rk,_Rm=new T(function(){return B(_ff(_Ri,[0,_Re,new T(function(){return B(_i(_Ra,new T(function(){return B(_i(B(A(_k6,[_jw,_j0,E(_Rd)[1],_9])),_Q9));})));})],_Rh));});return new F(function(){return A(_Ms,[_Rm,[0,_Rj,_Rg[2],_Rg[3],_Rg[4],_Rg[5],_Rg[6],_Rm],_]);});}}else{return [0,_4m,_Rg];}};},_Rn=[0,1],_Ro=new T(function(){return B(unCStr("\u30a2\u30a4\u3068\u306e\u906d\u9047"));}),_Rp=new T(function(){return B(_Rb(_Rn,_QR,_Ro));}),_Rq=[0,12],_Rr=new T(function(){return B(unCStr("\u611b\u3055\u308c\u6c17\u5206"));}),_Rs=new T(function(){return B(_Rb(_Rq,_Qw,_Rr));}),_Rt=[0,13],_Ru=[0,10000],_Rv=new T(function(){return B(unCStr("\u611b\u30e9\u30d6\u30e6\u30fc"));}),_Rw=new T(function(){return B(_Rb(_Rt,_Ru,_Rv));}),_Rx=[0,14],_Ry=[0,10000000],_Rz=new T(function(){return B(unCStr("\u611b\u3055\u3093\u306e\u611b\u304c\u91cd\u3044"));}),_RA=new T(function(){return B(_Rb(_Rx,_Ry,_Rz));}),_RB=[0,21],_RC=new T(function(){return B(unCStr("\u307e\u305a\u306f\u304a\u53cb\u9054\u304b\u3089"));}),_RD=new T(function(){return B(_Qc(_RB,_QR,_RC));}),_RE=function(_,_RF){var _RG=B(A(_Rp,[new T(function(){return E(E(_RF)[2]);}),_])),_RH=_RG,_RI=B(A(_Rs,[new T(function(){return E(E(_RH)[2]);}),_])),_RJ=_RI,_RK=B(A(_Rw,[new T(function(){return E(E(_RJ)[2]);}),_])),_RL=_RK,_RM=B(A(_RA,[new T(function(){return E(E(_RL)[2]);}),_])),_RN=_RM,_RO=B(A(_RD,[new T(function(){return E(E(_RN)[2]);}),_])),_RP=_RO,_RQ=B(A(_Qr,[new T(function(){return E(E(_RP)[2]);}),_])),_RR=_RQ,_RS=B(A(_Qv,[new T(function(){return E(E(_RR)[2]);}),_])),_RT=_RS,_RU=B(A(_Qz,[new T(function(){return E(E(_RT)[2]);}),_])),_RV=_RU,_RW=B(A(_QD,[new T(function(){return E(E(_RV)[2]);}),_])),_RX=_RW,_RY=B(A(_QU,[new T(function(){return E(E(_RX)[2]);}),_])),_RZ=_RY,_S0=B(A(_QX,[new T(function(){return E(E(_RZ)[2]);}),_])),_S1=_S0,_S2=B(A(_R0,[new T(function(){return E(E(_S1)[2]);}),_])),_S3=_S2,_S4=B(A(_R4,[new T(function(){return E(E(_S3)[2]);}),_])),_S5=_S4,_S6=jsFind(toJSStr(E(_R6))),_S7=_S6,_S8=E(_S7);if(!_S8[0]){return new F(function(){return _LS(_R5,_);});}else{var _S9=toJSStr(E(_LU)),_Sa=E(E(_S5)[2]),_Sb=jsSet(E(_S8[1])[1],_S9,toJSStr(B(_Pm(E(_Sa[2])[1])))),_Sc=jsFind(toJSStr(E(_R7))),_Sd=_Sc,_Se=E(_Sd);if(!_Se[0]){return new F(function(){return _LS(_R5,_);});}else{var _Sf=jsSet(E(_Se[1])[1],_S9,toJSStr(B(_Pm(E(_Sa[1])[1])))),_Sg=jsFind(toJSStr(E(_R8))),_Sh=_Sg,_Si=E(_Sh);if(!_Si[0]){return new F(function(){return _LS(_R5,_);});}else{var _Sj=jsSet(E(_Si[1])[1],_S9,toJSStr(B(_Pm(E(_Sa[3])[1])))),_Sk=jsFind(toJSStr(E(_R9))),_Sl=_Sk,_Sm=E(_Sl);if(!_Sm[0]){return new F(function(){return _LS(_R5,_);});}else{var _Sn=jsSet(E(_Sm[1])[1],_S9,toJSStr(B(_Pm(B(_lO(_Sa[5]))))));return [0,_4m,_Sa];}}}}},_So=new T(function(){return B(unCStr("\u653e\u7f6e\u671f\u9593 +"));}),_Sp=function(_Sq){return new F(function(){return err(B(unAppCStr("docFocused: ",new T(function(){return fromJSStr(_Sq);}))));});},_Sr=new T(function(){return B(unCStr("false"));}),_Ss=new T(function(){return B(unCStr("true"));}),_St=new T(function(){return B(unCStr("document.hasFocus()"));}),_Su=function(_Sv,_){var _Sw=jsEval(toJSStr(E(_St))),_Sx=_Sw,_Sy=strEq(_Sx,toJSStr(E(_Ss))),_Sz=_Sy;if(!E(_Sz)){var _SA=strEq(_Sx,toJSStr(E(_Sr))),_SB=_SA;return E(_SB)==0?B(_Sp(_Sx)):B(_RE(_,[0,_4m,new T(function(){var _SC=E(_Sv),_SD=_SC[2],_SE=_SC[3],_SF=_SC[5],_SG=new T(function(){return [0,B(_lO(_SF))/1000/60/120];});return [0,new T(function(){return [0,E(_SC[1])[1]+E(_SD)[1]*E(_SE)[1]/30];}),new T(function(){return [0,E(_SD)[1]+E(_SG)[1]];}),new T(function(){var _SH=E(_SE)[1]-E(_SG)[1];return _SH>1?[0,_SH]:E(_QR);}),_SC[4],_SF,_6U,_SC[7]];})]));}else{var _SI=E(_Sv),_SJ=_SI[1],_SK=_SI[2],_SL=_SI[3],_SM=_SI[4],_SN=_SI[5],_SO=_SI[7];if(!E(_SI[6])){var _SP=B(_PS(new T(function(){return B(_i(_So,new T(function(){return B(_D(0,_SN,_9));})));}),_)),_SQ=_SP;return new F(function(){return _RE(_,[0,_4m,[0,new T(function(){return [0,E(_SJ)[1]+E(_SK)[1]*E(_SL)[1]/30];}),_SK,new T(function(){return [0,E(_SL)[1]+B(_lO(_SN))/1000/60/15];}),_SM,_SN,_6V,_SO]]);});}else{return new F(function(){return _RE(_,[0,_4m,[0,new T(function(){return [0,E(_SJ)[1]+E(_SK)[1]*E(_SL)[1]/30];}),_SK,new T(function(){return [0,E(_SL)[1]+B(_lO(_SN))/1000/60/15];}),_SM,_SN,_6V,_SO]]);});}}},_SR=function(_){var _=0,_SS=jsMkStdout(),_ST=_SS;return [0,_ST];},_SU=new T(function(){return B(_MB(_SR));}),_SV=function(_){var _SW=B(_kW(_)),_SX=_SW,_SY=B(_MN(_gr,_Nd,_)),_SZ=_SY,_T0=nMV(new T(function(){var _T1=E(_SZ);return _T1[0]==0?[0,_Nb,_Nb,_Nb,_SX,_1J,_6U,_fe]:E(_T1[1]);})),_T2=_T0,_T3=B(_kL(33,_T2,_Su,_)),_T4=_T3,_T5=B(_N6(_SU,B(_kI(_T4)),_)),_T6=_T5,_T7=B(_kL(1000,_T2,_OG,_)),_T8=_T7,_T9=B(_N6(_SU,B(_kI(_T8)),_)),_Ta=_T9,_Tb=B(_kL(60000,_T2,_Og,_)),_Tc=_Tb,_Td=B(_N6(_SU,B(_kI(_Tc)),_)),_Te=_Td,_Tf=jsFind("reset"),_Tg=_Tf,_Th=E(_Tg);if(!_Th[0]){return new F(function(){return _LS(_Nc,_);});}else{var _Ti=jsSetCB(E(_Th[1])[1],E(_My)[1],function(_Tj,_Tk,_){var _Tl=B(_kW(_)),_Tm=_Tl,_=wMV(_T2,[0,_Nb,_Nb,_Nb,_Tm,_1J,_6U,_fe]),_Tn=rMV(_T2),_To=_Tn,_Tp=B(A(_Ms,[new T(function(){return E(E(_To)[7]);}),_To,_])),_Tq=_Tp,_=wMV(_T2,new T(function(){return E(E(_Tq)[2]);}));return _4m;}),_Tr=_Ti;return new T(function(){return E(_Tr)==0?false:true;});}},_Ts=function(_){return new F(function(){return _SV(_);});};
var hasteMain = function() {B(A(_Ts, [0]));};window.onload = hasteMain;
(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.expect.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.expect.b, xhr)); });
		$elm$core$Maybe$isJust(request.tracker) && _Http_track(router, xhr, request.tracker.a);

		try {
			xhr.open(request.method, request.url, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.url));
		}

		_Http_configureRequest(xhr, request);

		request.body.a && xhr.setRequestHeader('Content-Type', request.body.a);
		xhr.send(request.body.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.headers; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.timeout.a || 0;
	xhr.responseType = request.expect.d;
	xhr.withCredentials = request.allowCookiesFromOtherDomains;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		url: xhr.responseURL,
		statusCode: xhr.status,
		statusText: xhr.statusText,
		headers: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			sent: event.loaded,
			size: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			received: event.loaded,
			size: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}


var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$application = _Browser_application;
var $author$project$Datatypes$FetchedData = function (a) {
	return {$: 'FetchedData', a: a};
};
var $author$project$Datatypes$Loading = {$: 'Loading'};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $author$project$Datatypes$AppData = F8(
	function (clues, grid, cluegridSize, cluegridInfo, activeClueIndex, activeCell, modal, otherClueIndex) {
		return {activeCell: activeCell, activeClueIndex: activeClueIndex, cluegridInfo: cluegridInfo, cluegridSize: cluegridSize, clues: clues, grid: grid, modal: modal, otherClueIndex: otherClueIndex};
	});
var $author$project$Datatypes$CluegridInfo = F5(
	function (date, title, author, editor, copyright) {
		return {author: author, copyright: copyright, date: date, editor: editor, title: title};
	});
var $author$project$Datatypes$CluegridSize = F2(
	function (rows, cols) {
		return {cols: cols, rows: rows};
	});
var $author$project$Datatypes$Empty = {$: 'Empty'};
var $author$project$Datatypes$Clue = F6(
	function (startCol, startRow, solution, direction, gridNumber, clue_text) {
		return {clue_text: clue_text, direction: direction, gridNumber: gridNumber, solution: solution, startCol: startCol, startRow: startRow};
	});
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $author$project$Datatypes$Across = {$: 'Across'};
var $author$project$Datatypes$Down = {$: 'Down'};
var $elm$json$Json$Decode$fail = _Json_fail;
var $author$project$Clue$directionFromString = function (dir) {
	switch (dir) {
		case 'Across':
			return $elm$json$Json$Decode$succeed($author$project$Datatypes$Across);
		case 'Down':
			return $elm$json$Json$Decode$succeed($author$project$Datatypes$Down);
		default:
			return $elm$json$Json$Decode$fail('Invalid direction ' + dir);
	}
};
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Clue$decodeDirection = A2($elm$json$Json$Decode$andThen, $author$project$Clue$directionFromString, $elm$json$Json$Decode$string);
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$Clue$subtract1 = function (n) {
	return $elm$json$Json$Decode$succeed(n - 1);
};
var $author$project$Clue$decodeStartRowCol = A2($elm$json$Json$Decode$andThen, $author$project$Clue$subtract1, $elm$json$Json$Decode$int);
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$map6 = _Json_map6;
var $author$project$Clue$decodeClue = A7(
	$elm$json$Json$Decode$map6,
	$author$project$Datatypes$Clue,
	A2($elm$json$Json$Decode$field, 'start_col', $author$project$Clue$decodeStartRowCol),
	A2($elm$json$Json$Decode$field, 'start_row', $author$project$Clue$decodeStartRowCol),
	A2($elm$json$Json$Decode$field, 'solution', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'direction', $author$project$Clue$decodeDirection),
	A2($elm$json$Json$Decode$field, 'number', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'text', $elm$json$Json$Decode$string));
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$Clue$decodeClues = $elm$json$Json$Decode$list($author$project$Clue$decodeClue);
var $author$project$Datatypes$Cell = F7(
	function (solution, row, col, gridNumber, acrossClueIndex, downClueIndex, entry) {
		return {acrossClueIndex: acrossClueIndex, col: col, downClueIndex: downClueIndex, entry: entry, gridNumber: gridNumber, row: row, solution: solution};
	});
var $elm$json$Json$Decode$map7 = _Json_map7;
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$nullable = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder)
			]));
};
var $author$project$Cell$decodeCell = A8(
	$elm$json$Json$Decode$map7,
	$author$project$Datatypes$Cell,
	A2($elm$json$Json$Decode$field, 'solution', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'row', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'col', $elm$json$Json$Decode$int),
	A2(
		$elm$json$Json$Decode$field,
		'grid_number',
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$int)),
	A2(
		$elm$json$Json$Decode$field,
		'across_clue_index',
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$int)),
	A2(
		$elm$json$Json$Decode$field,
		'down_clue_index',
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$int)),
	$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing));
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $author$project$Cell$listListCellToGrid = function (l) {
	return $elm$json$Json$Decode$succeed(
		$elm$core$Array$fromList(
			A2(
				$elm$core$List$map,
				function (row) {
					return $elm$core$Array$fromList(row);
				},
				l)));
};
var $author$project$Cell$decodeGrid = A2(
	$elm$json$Json$Decode$andThen,
	$author$project$Cell$listListCellToGrid,
	$elm$json$Json$Decode$list(
		$elm$json$Json$Decode$list($author$project$Cell$decodeCell)));
var $elm$json$Json$Decode$map5 = _Json_map5;
var $elm$json$Json$Decode$map8 = _Json_map8;
var $author$project$Data$decodeAppData = A9(
	$elm$json$Json$Decode$map8,
	$author$project$Datatypes$AppData,
	A2($elm$json$Json$Decode$field, 'clues', $author$project$Clue$decodeClues),
	A2($elm$json$Json$Decode$field, 'grid', $author$project$Cell$decodeGrid),
	A3(
		$elm$json$Json$Decode$map2,
		$author$project$Datatypes$CluegridSize,
		A2(
			$elm$json$Json$Decode$field,
			'size',
			A2($elm$json$Json$Decode$field, 'rows', $elm$json$Json$Decode$int)),
		A2(
			$elm$json$Json$Decode$field,
			'size',
			A2($elm$json$Json$Decode$field, 'cols', $elm$json$Json$Decode$int))),
	A6(
		$elm$json$Json$Decode$map5,
		$author$project$Datatypes$CluegridInfo,
		A2(
			$elm$json$Json$Decode$field,
			'info',
			A2($elm$json$Json$Decode$field, 'date', $elm$json$Json$Decode$string)),
		A2(
			$elm$json$Json$Decode$field,
			'info',
			A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string)),
		A2(
			$elm$json$Json$Decode$field,
			'info',
			A2($elm$json$Json$Decode$field, 'author', $elm$json$Json$Decode$string)),
		A2(
			$elm$json$Json$Decode$field,
			'info',
			A2($elm$json$Json$Decode$field, 'editor', $elm$json$Json$Decode$string)),
		A2(
			$elm$json$Json$Decode$field,
			'info',
			A2($elm$json$Json$Decode$field, 'copyright', $elm$json$Json$Decode$string))),
	$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing),
	$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing),
	$elm$json$Json$Decode$succeed($author$project$Datatypes$Empty),
	$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing));
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 'BadStatus_', a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 'BadUrl_', a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 'GoodStatus_', a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 'NetworkError_'};
var $elm$http$Http$Receiving = function (a) {
	return {$: 'Receiving', a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 'Sending', a: a};
};
var $elm$http$Http$Timeout_ = {$: 'Timeout_'};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 'BadBody', a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 'BadStatus', a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var $elm$http$Http$NetworkError = {$: 'NetworkError'};
var $elm$http$Http$Timeout = {$: 'Timeout'};
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 'BadUrl_':
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 'Timeout_':
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 'NetworkError_':
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 'BadStatus_':
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.statusCode));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			$elm$http$Http$resolve(
				function (string) {
					return A2(
						$elm$core$Result$mapError,
						$elm$json$Json$Decode$errorToString,
						A2($elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$Request = function (a) {
	return {$: 'Request', a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {reqs: reqs, subs: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (cmd.$ === 'Cancel') {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 'Nothing') {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.tracker;
							if (_v4.$ === 'Nothing') {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.reqs));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.subs)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 'Cancel', a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (cmd.$ === 'Cancel') {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					allowCookiesFromOtherDomains: r.allowCookiesFromOtherDomains,
					body: r.body,
					expect: A2(_Http_mapExpect, func, r.expect),
					headers: r.headers,
					method: r.method,
					timeout: r.timeout,
					tracker: r.tracker,
					url: r.url
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 'MySub', a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{allowCookiesFromOtherDomains: false, body: r.body, expect: r.expect, headers: r.headers, method: r.method, timeout: r.timeout, tracker: r.tracker, url: r.url}));
};
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{body: $elm$http$Http$emptyBody, expect: r.expect, headers: _List_Nil, method: 'GET', timeout: $elm$core$Maybe$Nothing, tracker: $elm$core$Maybe$Nothing, url: r.url});
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Main$sendRequestAllCells = _Platform_outgoingPort('sendRequestAllCells', $elm$json$Json$Encode$string);
var $author$project$Main$init = F3(
	function (flags, url, key) {
		return _Utils_Tuple2(
			$author$project$Datatypes$Loading,
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						$elm$http$Http$get(
						{
							expect: A2($elm$http$Http$expectJson, $author$project$Datatypes$FetchedData, $author$project$Data$decodeAppData),
							url: '/Oct01-2019.json'
						}),
						$author$project$Main$sendRequestAllCells('')
					])));
	});
var $author$project$Datatypes$NoOp = {$: 'NoOp'};
var $author$project$Main$onUrlChange = function (url) {
	return $author$project$Datatypes$NoOp;
};
var $author$project$Main$onUrlRequest = function (urlRequest) {
	return $author$project$Datatypes$NoOp;
};
var $author$project$Datatypes$CellUpdate = function (a) {
	return {$: 'CellUpdate', a: a};
};
var $author$project$Datatypes$HandleSocketMessage = function (a) {
	return {$: 'HandleSocketMessage', a: a};
};
var $author$project$Datatypes$KeyPressed = function (a) {
	return {$: 'KeyPressed', a: a};
};
var $author$project$Datatypes$OtherClueUpdated = function (a) {
	return {$: 'OtherClueUpdated', a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $author$project$Main$recieveCellUpdate = _Platform_incomingPort(
	'recieveCellUpdate',
	A2(
		$elm$json$Json$Decode$andThen,
		function (letter) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (cell) {
					return $elm$json$Json$Decode$succeed(
						{cell: cell, letter: letter});
				},
				A2(
					$elm$json$Json$Decode$field,
					'cell',
					A2(
						$elm$json$Json$Decode$andThen,
						function (row) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (col) {
									return $elm$json$Json$Decode$succeed(
										{col: col, row: row});
								},
								A2($elm$json$Json$Decode$field, 'col', $elm$json$Json$Decode$int));
						},
						A2($elm$json$Json$Decode$field, 'row', $elm$json$Json$Decode$int))));
		},
		A2(
			$elm$json$Json$Decode$field,
			'letter',
			$elm$json$Json$Decode$oneOf(
				_List_fromArray(
					[
						$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
						A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, $elm$json$Json$Decode$string)
					])))));
var $author$project$Main$recieveKeyPress = _Platform_incomingPort('recieveKeyPress', $elm$json$Json$Decode$string);
var $author$project$Main$recieveOtherClueUpdate = _Platform_incomingPort(
	'recieveOtherClueUpdate',
	$elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, $elm$json$Json$Decode$int)
			])));
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Main$recieveSocketMessage = _Platform_incomingPort('recieveSocketMessage', $elm$json$Json$Decode$value);
var $author$project$Main$subscriptions = function (model) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$author$project$Main$recieveCellUpdate($author$project$Datatypes$CellUpdate),
				$author$project$Main$recieveKeyPress($author$project$Datatypes$KeyPressed),
				$author$project$Main$recieveSocketMessage($author$project$Datatypes$HandleSocketMessage),
				$author$project$Main$recieveOtherClueUpdate($author$project$Datatypes$OtherClueUpdated)
			]));
};
var $author$project$Datatypes$Failure = {$: 'Failure'};
var $author$project$Datatypes$Info = {$: 'Info'};
var $author$project$Datatypes$Loaded = function (a) {
	return {$: 'Loaded', a: a};
};
var $elm$core$Elm$JsArray$appendN = _JsArray_appendN;
var $elm$core$Elm$JsArray$slice = _JsArray_slice;
var $elm$core$Array$appendHelpBuilder = F2(
	function (tail, builder) {
		var tailLen = $elm$core$Elm$JsArray$length(tail);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(builder.tail)) - tailLen;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, builder.tail, tail);
		return (notAppended < 0) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: A3($elm$core$Elm$JsArray$slice, notAppended, tailLen, tail)
		} : ((!notAppended) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: $elm$core$Elm$JsArray$empty
		} : {nodeList: builder.nodeList, nodeListSize: builder.nodeListSize, tail: appended});
	});
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Elm$JsArray$push = _JsArray_push;
var $elm$core$Elm$JsArray$singleton = _JsArray_singleton;
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$insertTailInTree = F4(
	function (shift, index, tail, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		if (_Utils_cmp(
			pos,
			$elm$core$Elm$JsArray$length(tree)) > -1) {
			if (shift === 5) {
				return A2(
					$elm$core$Elm$JsArray$push,
					$elm$core$Array$Leaf(tail),
					tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, $elm$core$Elm$JsArray$empty));
				return A2($elm$core$Elm$JsArray$push, newSub, tree);
			}
		} else {
			var value = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (value.$ === 'SubTree') {
				var subTree = value.a;
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, subTree));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4(
						$elm$core$Array$insertTailInTree,
						shift - $elm$core$Array$shiftStep,
						index,
						tail,
						$elm$core$Elm$JsArray$singleton(value)));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$unsafeReplaceTail = F2(
	function (newTail, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var originalTailLen = $elm$core$Elm$JsArray$length(tail);
		var newTailLen = $elm$core$Elm$JsArray$length(newTail);
		var newArrayLen = len + (newTailLen - originalTailLen);
		if (_Utils_eq(newTailLen, $elm$core$Array$branchFactor)) {
			var overflow = _Utils_cmp(newArrayLen >>> $elm$core$Array$shiftStep, 1 << startShift) > 0;
			if (overflow) {
				var newShift = startShift + $elm$core$Array$shiftStep;
				var newTree = A4(
					$elm$core$Array$insertTailInTree,
					newShift,
					len,
					newTail,
					$elm$core$Elm$JsArray$singleton(
						$elm$core$Array$SubTree(tree)));
				return A4($elm$core$Array$Array_elm_builtin, newArrayLen, newShift, newTree, $elm$core$Elm$JsArray$empty);
			} else {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					newArrayLen,
					startShift,
					A4($elm$core$Array$insertTailInTree, startShift, len, newTail, tree),
					$elm$core$Elm$JsArray$empty);
			}
		} else {
			return A4($elm$core$Array$Array_elm_builtin, newArrayLen, startShift, tree, newTail);
		}
	});
var $elm$core$Array$appendHelpTree = F2(
	function (toAppend, array) {
		var len = array.a;
		var tree = array.c;
		var tail = array.d;
		var itemsToAppend = $elm$core$Elm$JsArray$length(toAppend);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(tail)) - itemsToAppend;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, tail, toAppend);
		var newArray = A2($elm$core$Array$unsafeReplaceTail, appended, array);
		if (notAppended < 0) {
			var nextTail = A3($elm$core$Elm$JsArray$slice, notAppended, itemsToAppend, toAppend);
			return A2($elm$core$Array$unsafeReplaceTail, nextTail, newArray);
		} else {
			return newArray;
		}
	});
var $elm$core$Elm$JsArray$foldl = _JsArray_foldl;
var $elm$core$Array$builderFromArray = function (_v0) {
	var len = _v0.a;
	var tree = _v0.c;
	var tail = _v0.d;
	var helper = F2(
		function (node, acc) {
			if (node.$ === 'SubTree') {
				var subTree = node.a;
				return A3($elm$core$Elm$JsArray$foldl, helper, acc, subTree);
			} else {
				return A2($elm$core$List$cons, node, acc);
			}
		});
	return {
		nodeList: A3($elm$core$Elm$JsArray$foldl, helper, _List_Nil, tree),
		nodeListSize: (len / $elm$core$Array$branchFactor) | 0,
		tail: tail
	};
};
var $elm$core$Array$append = F2(
	function (a, _v0) {
		var aTail = a.d;
		var bLen = _v0.a;
		var bTree = _v0.c;
		var bTail = _v0.d;
		if (_Utils_cmp(bLen, $elm$core$Array$branchFactor * 4) < 1) {
			var foldHelper = F2(
				function (node, array) {
					if (node.$ === 'SubTree') {
						var tree = node.a;
						return A3($elm$core$Elm$JsArray$foldl, foldHelper, array, tree);
					} else {
						var leaf = node.a;
						return A2($elm$core$Array$appendHelpTree, leaf, array);
					}
				});
			return A2(
				$elm$core$Array$appendHelpTree,
				bTail,
				A3($elm$core$Elm$JsArray$foldl, foldHelper, a, bTree));
		} else {
			var foldHelper = F2(
				function (node, builder) {
					if (node.$ === 'SubTree') {
						var tree = node.a;
						return A3($elm$core$Elm$JsArray$foldl, foldHelper, builder, tree);
					} else {
						var leaf = node.a;
						return A2($elm$core$Array$appendHelpBuilder, leaf, builder);
					}
				});
			return A2(
				$elm$core$Array$builderToArray,
				true,
				A2(
					$elm$core$Array$appendHelpBuilder,
					bTail,
					A3(
						$elm$core$Elm$JsArray$foldl,
						foldHelper,
						$elm$core$Array$builderFromArray(a),
						bTree)));
		}
	});
var $author$project$Datatypes$CellUpdateData = F2(
	function (cell, letter) {
		return {cell: cell, letter: letter};
	});
var $author$project$Datatypes$RowCol = F2(
	function (row, col) {
		return {col: col, row: row};
	});
var $author$project$Controls$checkCorrect = function (cell) {
	var soln = function () {
		var _v0 = cell.entry;
		if (_v0.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var letter = _v0.a;
			return _Utils_eq(letter, cell.solution) ? $elm$core$Maybe$Just(letter) : $elm$core$Maybe$Nothing;
		}
	}();
	return A2(
		$author$project$Datatypes$CellUpdateData,
		A2($author$project$Datatypes$RowCol, cell.row, cell.col),
		soln);
};
var $elm$core$Array$filter = F2(
	function (isGood, array) {
		return $elm$core$Array$fromList(
			A3(
				$elm$core$Array$foldr,
				F2(
					function (x, xs) {
						return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
					}),
				_List_Nil,
				array));
	});
var $elm$core$Array$foldl = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldl, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldl, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldl,
			func,
			A3($elm$core$Elm$JsArray$foldl, helper, baseCase, tree),
			tail);
	});
var $author$project$Controls$isPartOfClue = F2(
	function (index, cell) {
		var isDown = function () {
			var _v1 = cell.downClueIndex;
			if (_v1.$ === 'Just') {
				var downIndex = _v1.a;
				return _Utils_eq(downIndex, index);
			} else {
				return false;
			}
		}();
		var isAcross = function () {
			var _v0 = cell.acrossClueIndex;
			if (_v0.$ === 'Just') {
				var acrossIndex = _v0.a;
				return _Utils_eq(acrossIndex, index);
			} else {
				return false;
			}
		}();
		return isAcross || isDown;
	});
var $elm$core$Elm$JsArray$map = _JsArray_map;
var $elm$core$Array$map = F2(
	function (func, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = function (node) {
			if (node.$ === 'SubTree') {
				var subTree = node.a;
				return $elm$core$Array$SubTree(
					A2($elm$core$Elm$JsArray$map, helper, subTree));
			} else {
				var values = node.a;
				return $elm$core$Array$Leaf(
					A2($elm$core$Elm$JsArray$map, func, values));
			}
		};
		return A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A2($elm$core$Elm$JsArray$map, helper, tree),
			A2($elm$core$Elm$JsArray$map, func, tail));
	});
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$Maybe$destruct = F3(
	function (_default, func, maybe) {
		if (maybe.$ === 'Just') {
			var a = maybe.a;
			return func(a);
		} else {
			return _default;
		}
	});
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $author$project$Controls$sendCellUpdate = _Platform_outgoingPort(
	'sendCellUpdate',
	function ($) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'cell',
					function ($) {
						return $elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'col',
									$elm$json$Json$Encode$int($.col)),
									_Utils_Tuple2(
									'row',
									$elm$json$Json$Encode$int($.row))
								]));
					}($.cell)),
					_Utils_Tuple2(
					'letter',
					function ($) {
						return A3($elm$core$Maybe$destruct, $elm$json$Json$Encode$null, $elm$json$Json$Encode$string, $);
					}($.letter))
				]));
	});
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $author$project$Cell$getCellFromRowCol = F2(
	function (grid, _v0) {
		var row = _v0.a;
		var col = _v0.b;
		var _v1 = A2($elm$core$Array$get, row, grid);
		if (_v1.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var correctRow = _v1.a;
			var _v2 = A2($elm$core$Array$get, col, correctRow);
			if (_v2.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var cell = _v2.a;
				return $elm$core$Maybe$Just(cell);
			}
		}
	});
var $author$project$Cell$isCellEqual = F2(
	function (cell1, cell2) {
		return _Utils_eq(cell1.row, cell2.row) && _Utils_eq(cell1.col, cell2.col);
	});
var $author$project$Cell$updateCellInRow = F3(
	function (cellToUpdate, row, letter) {
		return A2(
			$elm$core$Array$map,
			function (cell) {
				return A2($author$project$Cell$isCellEqual, cell, cellToUpdate) ? _Utils_update(
					cell,
					{entry: letter}) : cell;
			},
			row);
	});
var $author$project$Cell$updateCellEntry = F3(
	function (cellToUpdate, letter, grid) {
		return A2(
			$elm$core$Array$map,
			function (row) {
				return A3($author$project$Cell$updateCellInRow, cellToUpdate, row, letter);
			},
			grid);
	});
var $author$project$Controls$updateCellData = F2(
	function (cellUpdateData, appData) {
		var _v0 = A2(
			$author$project$Cell$getCellFromRowCol,
			appData.grid,
			_Utils_Tuple2(cellUpdateData.cell.row, cellUpdateData.cell.col));
		if (_v0.$ === 'Nothing') {
			return appData;
		} else {
			var cell = _v0.a;
			var newGrid = A3($author$project$Cell$updateCellEntry, cell, cellUpdateData.letter, appData.grid);
			return _Utils_update(
				appData,
				{grid: newGrid});
		}
	});
var $author$project$Controls$checkActiveClue = function (appData) {
	var _v0 = appData.activeClueIndex;
	if (_v0.$ === 'Nothing') {
		return _Utils_Tuple2(
			$author$project$Datatypes$Loaded(appData),
			$elm$core$Platform$Cmd$none);
	} else {
		var index = _v0.a;
		var updateData = A2(
			$elm$core$Array$map,
			function (cell) {
				return $author$project$Controls$checkCorrect(cell);
			},
			A2(
				$elm$core$Array$filter,
				function (cell) {
					return A2($author$project$Controls$isPartOfClue, index, cell);
				},
				A3(
					$elm$core$Array$foldl,
					$elm$core$Array$append,
					$elm$core$Array$fromList(_List_Nil),
					appData.grid)));
		var newData = A3($elm$core$Array$foldl, $author$project$Controls$updateCellData, appData, updateData);
		return _Utils_Tuple2(
			$author$project$Datatypes$Loaded(newData),
			$elm$core$Platform$Cmd$batch(
				$elm$core$Array$toList(
					A2(
						$elm$core$Array$map,
						function (cellUpdate) {
							return $author$project$Controls$sendCellUpdate(cellUpdate);
						},
						updateData))));
	}
};
var $author$project$Cell$crosswordCellisBlank = function (cell) {
	return cell.solution === '.';
};
var $author$project$Cell$isRowColEqual = F3(
	function (cell, row, col) {
		return _Utils_eq(cell.row, row) && _Utils_eq(cell.col, col);
	});
var $author$project$Controls$resolveCellClueIndex = F2(
	function (cell, clueDirection) {
		if (clueDirection.$ === 'Across') {
			var _v1 = cell.acrossClueIndex;
			if (_v1.$ === 'Just') {
				return cell.acrossClueIndex;
			} else {
				return cell.downClueIndex;
			}
		} else {
			var _v2 = cell.downClueIndex;
			if (_v2.$ === 'Just') {
				return cell.downClueIndex;
			} else {
				return cell.acrossClueIndex;
			}
		}
	});
var $author$project$Controls$updateActiveClue = F4(
	function (activeClueIndex, cell, activeCell, clues) {
		if (activeClueIndex.$ === 'Nothing') {
			return A2($author$project$Controls$resolveCellClueIndex, cell, $author$project$Datatypes$Across);
		} else {
			var activeIndex = activeClueIndex.a;
			var currentDirection = function () {
				var _v4 = A2(
					$elm$core$Array$get,
					activeIndex,
					$elm$core$Array$fromList(clues));
				if (_v4.$ === 'Just') {
					var clue = _v4.a;
					return clue.direction;
				} else {
					return $author$project$Datatypes$Across;
				}
			}();
			var otherDirection = function () {
				if (currentDirection.$ === 'Across') {
					return $author$project$Datatypes$Down;
				} else {
					return $author$project$Datatypes$Across;
				}
			}();
			var cellReclicked = function () {
				if (activeCell.$ === 'Nothing') {
					return false;
				} else {
					var _v2 = activeCell.a;
					var row = _v2.a;
					var col = _v2.b;
					return A3($author$project$Cell$isRowColEqual, cell, row, col) ? true : false;
				}
			}();
			return cellReclicked ? A2($author$project$Controls$resolveCellClueIndex, cell, otherDirection) : A2($author$project$Controls$resolveCellClueIndex, cell, currentDirection);
		}
	});
var $author$project$Controls$selectCell = F3(
	function (appData, rowNum, colNum) {
		var _v0 = A2(
			$author$project$Cell$getCellFromRowCol,
			appData.grid,
			_Utils_Tuple2(rowNum, colNum));
		if (_v0.$ === 'Just') {
			var cellAtRowCol = _v0.a;
			return $author$project$Cell$crosswordCellisBlank(cellAtRowCol) ? appData : _Utils_update(
				appData,
				{
					activeCell: $elm$core$Maybe$Just(
						_Utils_Tuple2(rowNum, colNum)),
					activeClueIndex: A4($author$project$Controls$updateActiveClue, appData.activeClueIndex, cellAtRowCol, appData.activeCell, appData.clues)
				});
		} else {
			return appData;
		}
	});
var $author$project$Controls$moveCell = F4(
	function (originalAppData, appData, rowChange, colChange) {
		moveCell:
		while (true) {
			var _v0 = appData.activeCell;
			if (_v0.$ === 'Nothing') {
				return A3($author$project$Controls$selectCell, appData, 0, 0);
			} else {
				var _v1 = _v0.a;
				var row = _v1.a;
				var col = _v1.b;
				var _v2 = A2(
					$author$project$Cell$getCellFromRowCol,
					appData.grid,
					_Utils_Tuple2(row + rowChange, col + colChange));
				if (_v2.$ === 'Just') {
					var cellAtRowCol = _v2.a;
					if ($author$project$Cell$crosswordCellisBlank(cellAtRowCol)) {
						var $temp$originalAppData = originalAppData,
							$temp$appData = _Utils_update(
							appData,
							{
								activeCell: $elm$core$Maybe$Just(
									_Utils_Tuple2(row + rowChange, col + colChange))
							}),
							$temp$rowChange = rowChange,
							$temp$colChange = colChange;
						originalAppData = $temp$originalAppData;
						appData = $temp$appData;
						rowChange = $temp$rowChange;
						colChange = $temp$colChange;
						continue moveCell;
					} else {
						return A3($author$project$Controls$selectCell, appData, row + rowChange, col + colChange);
					}
				} else {
					return originalAppData;
				}
			}
		}
	});
var $author$project$Controls$moveDown = function (appData) {
	return A4($author$project$Controls$moveCell, appData, appData, 1, 0);
};
var $author$project$Controls$moveRight = function (appData) {
	return A4($author$project$Controls$moveCell, appData, appData, 0, 1);
};
var $author$project$Controls$moveNext = function (appData) {
	var _v0 = appData.activeClueIndex;
	if (_v0.$ === 'Just') {
		var clueIndex = _v0.a;
		var _v1 = A2(
			$elm$core$Array$get,
			clueIndex,
			$elm$core$Array$fromList(appData.clues));
		if (_v1.$ === 'Just') {
			var clue = _v1.a;
			var _v2 = clue.direction;
			if (_v2.$ === 'Across') {
				return $author$project$Controls$moveRight(appData);
			} else {
				return $author$project$Controls$moveDown(appData);
			}
		} else {
			return appData;
		}
	} else {
		return appData;
	}
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $author$project$Controls$moveLeft = function (appData) {
	return A4($author$project$Controls$moveCell, appData, appData, 0, -1);
};
var $author$project$Controls$moveUp = function (appData) {
	return A4($author$project$Controls$moveCell, appData, appData, -1, 0);
};
var $author$project$Controls$movePrevious = function (appData) {
	var _v0 = appData.activeClueIndex;
	if (_v0.$ === 'Just') {
		var clueIndex = _v0.a;
		var _v1 = A2(
			$elm$core$Array$get,
			clueIndex,
			$elm$core$Array$fromList(appData.clues));
		if (_v1.$ === 'Just') {
			var clue = _v1.a;
			var _v2 = clue.direction;
			if (_v2.$ === 'Across') {
				return $author$project$Controls$moveLeft(appData);
			} else {
				return $author$project$Controls$moveUp(appData);
			}
		} else {
			return appData;
		}
	} else {
		return appData;
	}
};
var $author$project$Controls$changeActiveEntry = F2(
	function (appData, letter) {
		var _v0 = appData.activeCell;
		if (_v0.$ === 'Just') {
			var _v1 = _v0.a;
			var row = _v1.a;
			var col = _v1.b;
			var _v2 = A2(
				$author$project$Cell$getCellFromRowCol,
				appData.grid,
				_Utils_Tuple2(row, col));
			if (_v2.$ === 'Just') {
				var cellAtRowCol = _v2.a;
				var grid = A3($author$project$Cell$updateCellEntry, cellAtRowCol, letter, appData.grid);
				var updatedAppData = _Utils_update(
					appData,
					{grid: grid});
				if (letter.$ === 'Just') {
					return $author$project$Controls$moveNext(updatedAppData);
				} else {
					return $author$project$Controls$movePrevious(updatedAppData);
				}
			} else {
				return appData;
			}
		} else {
			return appData;
		}
	});
var $author$project$Datatypes$ArrowKey = function (a) {
	return {$: 'ArrowKey', a: a};
};
var $author$project$Datatypes$ArrowKeyDown = {$: 'ArrowKeyDown'};
var $author$project$Datatypes$ArrowKeyLeft = {$: 'ArrowKeyLeft'};
var $author$project$Datatypes$ArrowKeyRight = {$: 'ArrowKeyRight'};
var $author$project$Datatypes$ArrowKeyUp = {$: 'ArrowKeyUp'};
var $author$project$Datatypes$BackspaceKey = {$: 'BackspaceKey'};
var $author$project$Datatypes$ControlKey = function (a) {
	return {$: 'ControlKey', a: a};
};
var $author$project$Datatypes$EnterKey = {$: 'EnterKey'};
var $author$project$Datatypes$EscapeKey = {$: 'EscapeKey'};
var $author$project$Datatypes$LetterKey = function (a) {
	return {$: 'LetterKey', a: a};
};
var $author$project$Datatypes$ShiftTabKey = {$: 'ShiftTabKey'};
var $author$project$Datatypes$TabKey = {$: 'TabKey'};
var $author$project$Datatypes$UnsupportedKey = {$: 'UnsupportedKey'};
var $author$project$Controls$keyToKeyboardInput = function (code) {
	if (A2($elm$core$String$startsWith, 'Arrow', code)) {
		switch (code) {
			case 'ArrowRight':
				return $author$project$Datatypes$ArrowKey($author$project$Datatypes$ArrowKeyRight);
			case 'ArrowLeft':
				return $author$project$Datatypes$ArrowKey($author$project$Datatypes$ArrowKeyLeft);
			case 'ArrowUp':
				return $author$project$Datatypes$ArrowKey($author$project$Datatypes$ArrowKeyUp);
			case 'ArrowDown':
				return $author$project$Datatypes$ArrowKey($author$project$Datatypes$ArrowKeyDown);
			default:
				return $author$project$Datatypes$UnsupportedKey;
		}
	} else {
		if (A2($elm$core$String$startsWith, 'Key', code)) {
			return $author$project$Datatypes$LetterKey(
				A3($elm$core$String$slice, 3, 4, code));
		} else {
			if (code === 'Enter') {
				return $author$project$Datatypes$ControlKey($author$project$Datatypes$EnterKey);
			} else {
				if (code === 'Backspace') {
					return $author$project$Datatypes$ControlKey($author$project$Datatypes$BackspaceKey);
				} else {
					if (code === 'Tab') {
						return $author$project$Datatypes$ControlKey($author$project$Datatypes$TabKey);
					} else {
						if (code === 'ShiftTab') {
							return $author$project$Datatypes$ControlKey($author$project$Datatypes$ShiftTabKey);
						} else {
							if (code === 'Escape') {
								return $author$project$Datatypes$ControlKey($author$project$Datatypes$EscapeKey);
							} else {
								return $author$project$Datatypes$UnsupportedKey;
							}
						}
					}
				}
			}
		}
	}
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $author$project$Controls$setActiveClue = F2(
	function (appData, clueIndex) {
		var _v0 = A2(
			$elm$core$Array$get,
			clueIndex,
			$elm$core$Array$fromList(appData.clues));
		if (_v0.$ === 'Just') {
			var clue = _v0.a;
			var _v1 = A2(
				$author$project$Cell$getCellFromRowCol,
				appData.grid,
				_Utils_Tuple2(clue.startRow, clue.startCol));
			if (_v1.$ === 'Just') {
				var cell = _v1.a;
				var _v2 = appData.activeClueIndex;
				if (_v2.$ === 'Just') {
					var index = _v2.a;
					return _Utils_eq(index, clueIndex) ? appData : A3(
						$author$project$Controls$selectCell,
						_Utils_update(
							appData,
							{
								activeClueIndex: $elm$core$Maybe$Just(clueIndex)
							}),
						cell.row,
						cell.col);
				} else {
					return A3(
						$author$project$Controls$selectCell,
						_Utils_update(
							appData,
							{
								activeClueIndex: $elm$core$Maybe$Just(clueIndex)
							}),
						cell.row,
						cell.col);
				}
			} else {
				return appData;
			}
		} else {
			return appData;
		}
	});
var $author$project$Controls$changeClueIndex = F2(
	function (appData, change) {
		var clueIndex = function () {
			var _v0 = appData.activeClueIndex;
			if (_v0.$ === 'Nothing') {
				return 0;
			} else {
				var index = _v0.a;
				return A2(
					$elm$core$Basics$modBy,
					$elm$core$List$length(appData.clues),
					index + change);
			}
		}();
		return A2($author$project$Controls$setActiveClue, appData, clueIndex);
	});
var $author$project$Controls$selectNextClue = function (appData) {
	return A2($author$project$Controls$changeClueIndex, appData, 1);
};
var $author$project$Controls$selectPreviousClue = function (appData) {
	return A2($author$project$Controls$changeClueIndex, appData, -1);
};
var $author$project$Datatypes$SetScroll = {$: 'SetScroll'};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Task$onError = _Scheduler_onError;
var $elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2(
					$elm$core$Task$onError,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
						$elm$core$Result$Err),
					A2(
						$elm$core$Task$andThen,
						A2(
							$elm$core$Basics$composeL,
							A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
							$elm$core$Result$Ok),
						task))));
	});
var $author$project$Clue$getClueId = function (index) {
	return 'cluegrid-clue-number-' + $elm$core$String$fromInt(index);
};
var $elm$browser$Browser$Dom$getElement = _Browser_getElement;
var $author$project$Controls$getScrollPortHeight = F3(
	function (viewport, clue, scrollArea) {
		return (_Utils_cmp(clue.element.y, scrollArea.element.y) < 0) ? ((viewport.viewport.y + clue.element.y) - scrollArea.element.y) : ((_Utils_cmp(clue.element.y + clue.element.height, scrollArea.element.height + scrollArea.element.y) > 0) ? ((((viewport.viewport.y + clue.element.y) + clue.element.height) - scrollArea.element.y) - scrollArea.element.height) : viewport.viewport.y);
	});
var $elm$browser$Browser$Dom$getViewportOf = _Browser_getViewportOf;
var $author$project$Controls$sendClueIndexUpdate = _Platform_outgoingPort(
	'sendClueIndexUpdate',
	function ($) {
		return A3($elm$core$Maybe$destruct, $elm$json$Json$Encode$null, $elm$json$Json$Encode$int, $);
	});
var $elm$browser$Browser$Dom$setViewportOf = _Browser_setViewportOf;
var $author$project$Controls$scrollToClue = function (appData) {
	var clueIndex = function () {
		var _v1 = appData.activeClueIndex;
		if (_v1.$ === 'Just') {
			var index = _v1.a;
			return index;
		} else {
			return 0;
		}
	}();
	return $elm$core$Platform$Cmd$batch(
		_List_fromArray(
			[
				A2(
				$elm$core$Task$attempt,
				function (_v0) {
					return $author$project$Datatypes$SetScroll;
				},
				A2(
					$elm$core$Task$andThen,
					function (clue) {
						return A2(
							$elm$core$Task$andThen,
							function (scrollAreaElement) {
								return A2(
									$elm$core$Task$andThen,
									function (scrollAreaViewport) {
										var scrollPortHeight = A3($author$project$Controls$getScrollPortHeight, scrollAreaViewport, clue, scrollAreaElement);
										return A3($elm$browser$Browser$Dom$setViewportOf, 'cluegrid-clues-scrollable-area', 0, scrollPortHeight);
									},
									$elm$browser$Browser$Dom$getViewportOf('cluegrid-clues-scrollable-area'));
							},
							$elm$browser$Browser$Dom$getElement('cluegrid-clues-scrollable-area'));
					},
					$elm$browser$Browser$Dom$getElement(
						$author$project$Clue$getClueId(clueIndex)))),
				$author$project$Controls$sendClueIndexUpdate(appData.activeClueIndex)
			]));
};
var $author$project$Controls$sendScrollToClue = function (appData) {
	return _Utils_Tuple2(
		$author$project$Datatypes$Loaded(appData),
		$author$project$Controls$scrollToClue(appData));
};
var $author$project$Controls$sendUpdateData = F4(
	function (letter, row, col, appData) {
		var cellUpdateData = A2(
			$author$project$Datatypes$CellUpdateData,
			A2($author$project$Datatypes$RowCol, row, col),
			letter);
		return _Utils_Tuple2(
			$author$project$Datatypes$Loaded(appData),
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						$author$project$Controls$sendCellUpdate(cellUpdateData),
						$author$project$Controls$scrollToClue(appData)
					])));
	});
var $author$project$Controls$toggleActiveClue = function (appData) {
	var _v0 = appData.activeCell;
	if (_v0.$ === 'Just') {
		var _v1 = _v0.a;
		var row = _v1.a;
		var col = _v1.b;
		return A3($author$project$Controls$selectCell, appData, row, col);
	} else {
		return appData;
	}
};
var $author$project$Controls$handleKeyInput = F2(
	function (key, appData) {
		var keyInput = $author$project$Controls$keyToKeyboardInput(key);
		switch (keyInput.$) {
			case 'ControlKey':
				var control = keyInput.a;
				switch (control.$) {
					case 'EnterKey':
						return $author$project$Controls$sendScrollToClue(
							$author$project$Controls$toggleActiveClue(appData));
					case 'BackspaceKey':
						var _v2 = function () {
							var _v3 = appData.activeCell;
							if (_v3.$ === 'Nothing') {
								return _Utils_Tuple2(-1, -1);
							} else {
								var _v4 = _v3.a;
								var r = _v4.a;
								var c = _v4.b;
								return _Utils_Tuple2(r, c);
							}
						}();
						var row = _v2.a;
						var col = _v2.b;
						return A4(
							$author$project$Controls$sendUpdateData,
							$elm$core$Maybe$Nothing,
							row,
							col,
							A2($author$project$Controls$changeActiveEntry, appData, $elm$core$Maybe$Nothing));
					case 'TabKey':
						return $author$project$Controls$sendScrollToClue(
							$author$project$Controls$selectNextClue(appData));
					case 'ShiftTabKey':
						return $author$project$Controls$sendScrollToClue(
							$author$project$Controls$selectPreviousClue(appData));
					case 'EscapeKey':
						return _Utils_Tuple2(
							$author$project$Datatypes$Loaded(
								_Utils_update(
									appData,
									{modal: $author$project$Datatypes$Empty})),
							$elm$core$Platform$Cmd$none);
					default:
						return $author$project$Controls$sendScrollToClue(appData);
				}
			case 'ArrowKey':
				var arrow = keyInput.a;
				switch (arrow.$) {
					case 'ArrowKeyRight':
						return $author$project$Controls$sendScrollToClue(
							$author$project$Controls$moveRight(appData));
					case 'ArrowKeyLeft':
						return $author$project$Controls$sendScrollToClue(
							$author$project$Controls$moveLeft(appData));
					case 'ArrowKeyUp':
						return $author$project$Controls$sendScrollToClue(
							$author$project$Controls$moveUp(appData));
					default:
						return $author$project$Controls$sendScrollToClue(
							$author$project$Controls$moveDown(appData));
				}
			case 'LetterKey':
				var letter = keyInput.a;
				var _v6 = function () {
					var _v7 = appData.activeCell;
					if (_v7.$ === 'Nothing') {
						return _Utils_Tuple2(-1, -1);
					} else {
						var _v8 = _v7.a;
						var r = _v8.a;
						var c = _v8.b;
						return _Utils_Tuple2(r, c);
					}
				}();
				var row = _v6.a;
				var col = _v6.b;
				return A4(
					$author$project$Controls$sendUpdateData,
					$elm$core$Maybe$Just(letter),
					row,
					col,
					A2(
						$author$project$Controls$changeActiveEntry,
						appData,
						$elm$core$Maybe$Just(letter)));
			default:
				return $author$project$Controls$sendScrollToClue(appData);
		}
	});
var $author$project$Controls$selectCellAndScroll = F3(
	function (appData, rowNum, colNum) {
		return $author$project$Controls$sendScrollToClue(
			A3($author$project$Controls$selectCell, appData, rowNum, colNum));
	});
var $author$project$Controls$getSolution = function (cell) {
	return A2(
		$author$project$Datatypes$CellUpdateData,
		A2($author$project$Datatypes$RowCol, cell.row, cell.col),
		$elm$core$Maybe$Just(cell.solution));
};
var $author$project$Controls$solveActiveClue = function (appData) {
	var _v0 = appData.activeClueIndex;
	if (_v0.$ === 'Nothing') {
		return _Utils_Tuple2(
			$author$project$Datatypes$Loaded(appData),
			$elm$core$Platform$Cmd$none);
	} else {
		var index = _v0.a;
		var clueCells = A2(
			$elm$core$Array$filter,
			function (cell) {
				return A2($author$project$Controls$isPartOfClue, index, cell);
			},
			A3(
				$elm$core$Array$foldl,
				$elm$core$Array$append,
				$elm$core$Array$fromList(_List_Nil),
				appData.grid));
		var updateData = A2(
			$elm$core$Array$map,
			function (cell) {
				return $author$project$Controls$getSolution(cell);
			},
			clueCells);
		var newData = A3($elm$core$Array$foldl, $author$project$Controls$updateCellData, appData, updateData);
		return _Utils_Tuple2(
			$author$project$Datatypes$Loaded(newData),
			$elm$core$Platform$Cmd$batch(
				$elm$core$Array$toList(
					A2(
						$elm$core$Array$map,
						function (cellUpdate) {
							return $author$project$Controls$sendCellUpdate(cellUpdate);
						},
						updateData))));
	}
};
var $author$project$Controls$updateOtherClue = F2(
	function (otherClueIndex, appData) {
		return _Utils_update(
			appData,
			{otherClueIndex: otherClueIndex});
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (model.$) {
			case 'Loading':
				if (msg.$ === 'FetchedData') {
					var data = msg.a;
					if (data.$ === 'Ok') {
						var appData = data.a;
						return _Utils_Tuple2(
							$author$project$Datatypes$Loaded(appData),
							$elm$core$Platform$Cmd$none);
					} else {
						return _Utils_Tuple2($author$project$Datatypes$Failure, $elm$core$Platform$Cmd$none);
					}
				} else {
					return _Utils_Tuple2($author$project$Datatypes$Loading, $elm$core$Platform$Cmd$none);
				}
			case 'Loaded':
				var appData = model.a;
				switch (msg.$) {
					case 'KeyPressed':
						var key = msg.a;
						return A2($author$project$Controls$handleKeyInput, key, appData);
					case 'CellClicked':
						var rowNum = msg.a;
						var colNum = msg.b;
						return A3($author$project$Controls$selectCellAndScroll, appData, rowNum, colNum);
					case 'ClueClicked':
						var clueIndex = msg.a;
						return _Utils_Tuple2(
							$author$project$Datatypes$Loaded(
								A2($author$project$Controls$setActiveClue, appData, clueIndex)),
							$elm$core$Platform$Cmd$none);
					case 'CellUpdate':
						var cellUpdateData = msg.a;
						return _Utils_Tuple2(
							$author$project$Datatypes$Loaded(
								A2($author$project$Controls$updateCellData, cellUpdateData, appData)),
							$elm$core$Platform$Cmd$none);
					case 'CloseModal':
						return _Utils_Tuple2(
							$author$project$Datatypes$Loaded(
								_Utils_update(
									appData,
									{modal: $author$project$Datatypes$Empty})),
							$elm$core$Platform$Cmd$none);
					case 'SetModalInfo':
						return _Utils_Tuple2(
							$author$project$Datatypes$Loaded(
								_Utils_update(
									appData,
									{modal: $author$project$Datatypes$Info})),
							$elm$core$Platform$Cmd$none);
					case 'SolveActiveClue':
						return $author$project$Controls$solveActiveClue(appData);
					case 'CheckActiveClue':
						return $author$project$Controls$checkActiveClue(appData);
					case 'HandleSocketMessage':
						var message = msg.a;
						return _Utils_Tuple2(
							$author$project$Datatypes$Loaded(appData),
							$elm$core$Platform$Cmd$none);
					case 'OtherClueUpdated':
						var otherClueIndex = msg.a;
						return _Utils_Tuple2(
							$author$project$Datatypes$Loaded(
								A2($author$project$Controls$updateOtherClue, otherClueIndex, appData)),
							$elm$core$Platform$Cmd$none);
					default:
						return _Utils_Tuple2(
							$author$project$Datatypes$Loaded(appData),
							$elm$core$Platform$Cmd$none);
				}
			default:
				return _Utils_Tuple2($author$project$Datatypes$Failure, $elm$core$Platform$Cmd$none);
		}
	});
var $elm$browser$Browser$Document = F2(
	function (title, body) {
		return {body: body, title: title};
	});
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $author$project$Datatypes$ClueClicked = function (a) {
	return {$: 'ClueClicked', a: a};
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$html$Html$Attributes$classList = function (classes) {
	return $elm$html$Html$Attributes$class(
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2($elm$core$List$filter, $elm$core$Tuple$second, classes))));
};
var $author$project$Clue$clueDirectionToText = function (direction) {
	if (direction.$ === 'Across') {
		return 'Across';
	} else {
		return 'Down';
	}
};
var $author$project$Clue$getCurrentSolution = F5(
	function (result, row, col, direction, grid) {
		var _v0 = A2(
			$author$project$Cell$getCellFromRowCol,
			grid,
			_Utils_Tuple2(row, col));
		if (_v0.$ === 'Nothing') {
			return result;
		} else {
			var cell = _v0.a;
			if ($author$project$Cell$crosswordCellisBlank(cell)) {
				return result;
			} else {
				var entry = function () {
					var _v2 = cell.entry;
					if (_v2.$ === 'Just') {
						var currentEntry = _v2.a;
						return currentEntry + ' ';
					} else {
						return '_ ';
					}
				}();
				if (direction.$ === 'Across') {
					return _Utils_ap(
						entry,
						A5($author$project$Clue$getCurrentSolution, result, row, col + 1, direction, grid));
				} else {
					return _Utils_ap(
						entry,
						A5($author$project$Clue$getCurrentSolution, result, row + 1, col, direction, grid));
				}
			}
		}
	});
var $author$project$Clue$getClueCurrentSolution = F2(
	function (clue, grid) {
		var _v0 = A2(
			$author$project$Cell$getCellFromRowCol,
			grid,
			_Utils_Tuple2(clue.startRow, clue.startCol));
		if (_v0.$ === 'Just') {
			var cell = _v0.a;
			return A5($author$project$Clue$getCurrentSolution, '', clue.startRow, clue.startCol, clue.direction, grid);
		} else {
			return '';
		}
	});
var $author$project$Clue$isActiveClue = F3(
	function (clue, activeClueIndex, clues) {
		if (activeClueIndex.$ === 'Just') {
			var index = activeClueIndex.a;
			var _v1 = A2(
				$elm$core$Array$get,
				index,
				$elm$core$Array$fromList(clues));
			if (_v1.$ === 'Just') {
				var activeClue = _v1.a;
				return (_Utils_eq(activeClue.clue_text, clue.clue_text) && _Utils_eq(activeClue.gridNumber, clue.gridNumber)) ? true : false;
			} else {
				return false;
			}
		} else {
			return false;
		}
	});
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$convert = F2(
	function (convertChars, string) {
		return $elm$core$String$concat(
			A2(
				$elm$core$List$map,
				$elm$core$String$fromChar,
				convertChars(
					$elm$core$String$toList(string))));
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Basics$not = _Basics_not;
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$String$fromList = _String_fromList;
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$convertCode = F5(
	function (mayber, lister, pre, post, list) {
		var string = $elm$core$String$fromList(list);
		var maybe = mayber(string);
		if (maybe.$ === 'Nothing') {
			return $elm$core$List$concat(
				_List_fromArray(
					[pre, list, post]));
		} else {
			var something = maybe.a;
			return lister(something);
		}
	});
var $elm$core$Char$fromCode = _Char_fromCode;
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$friendlyConverterDictionary = $elm$core$Dict$fromList(
	A2(
		$elm$core$List$map,
		function (_v0) {
			var a = _v0.a;
			var b = _v0.b;
			return _Utils_Tuple2(
				a,
				$elm$core$Char$fromCode(b));
		},
		_List_fromArray(
			[
				_Utils_Tuple2('quot', 34),
				_Utils_Tuple2('amp', 38),
				_Utils_Tuple2('lt', 60),
				_Utils_Tuple2('gt', 62),
				_Utils_Tuple2('nbsp', 160),
				_Utils_Tuple2('iexcl', 161),
				_Utils_Tuple2('cent', 162),
				_Utils_Tuple2('pound', 163),
				_Utils_Tuple2('curren', 164),
				_Utils_Tuple2('yen', 165),
				_Utils_Tuple2('brvbar', 166),
				_Utils_Tuple2('sect', 167),
				_Utils_Tuple2('uml', 168),
				_Utils_Tuple2('copy', 169),
				_Utils_Tuple2('ordf', 170),
				_Utils_Tuple2('laquo', 171),
				_Utils_Tuple2('not', 172),
				_Utils_Tuple2('shy', 173),
				_Utils_Tuple2('reg', 174),
				_Utils_Tuple2('macr', 175),
				_Utils_Tuple2('deg', 176),
				_Utils_Tuple2('plusmn', 177),
				_Utils_Tuple2('sup2', 178),
				_Utils_Tuple2('sup3', 179),
				_Utils_Tuple2('acute', 180),
				_Utils_Tuple2('micro', 181),
				_Utils_Tuple2('para', 182),
				_Utils_Tuple2('middot', 183),
				_Utils_Tuple2('cedil', 184),
				_Utils_Tuple2('sup1', 185),
				_Utils_Tuple2('ordm', 186),
				_Utils_Tuple2('raquo', 187),
				_Utils_Tuple2('frac14', 188),
				_Utils_Tuple2('frac12', 189),
				_Utils_Tuple2('frac34', 190),
				_Utils_Tuple2('iquest', 191),
				_Utils_Tuple2('Agrave', 192),
				_Utils_Tuple2('Aacute', 193),
				_Utils_Tuple2('Acirc', 194),
				_Utils_Tuple2('Atilde', 195),
				_Utils_Tuple2('Auml', 196),
				_Utils_Tuple2('Aring', 197),
				_Utils_Tuple2('AElig', 198),
				_Utils_Tuple2('Ccedil', 199),
				_Utils_Tuple2('Egrave', 200),
				_Utils_Tuple2('Eacute', 201),
				_Utils_Tuple2('Ecirc', 202),
				_Utils_Tuple2('Euml', 203),
				_Utils_Tuple2('Igrave', 204),
				_Utils_Tuple2('Iacute', 205),
				_Utils_Tuple2('Icirc', 206),
				_Utils_Tuple2('Iuml', 207),
				_Utils_Tuple2('ETH', 208),
				_Utils_Tuple2('Ntilde', 209),
				_Utils_Tuple2('Ograve', 210),
				_Utils_Tuple2('Oacute', 211),
				_Utils_Tuple2('Ocirc', 212),
				_Utils_Tuple2('Otilde', 213),
				_Utils_Tuple2('Ouml', 214),
				_Utils_Tuple2('times', 215),
				_Utils_Tuple2('Oslash', 216),
				_Utils_Tuple2('Ugrave', 217),
				_Utils_Tuple2('Uacute', 218),
				_Utils_Tuple2('Ucirc', 219),
				_Utils_Tuple2('Uuml', 220),
				_Utils_Tuple2('Yacute', 221),
				_Utils_Tuple2('THORN', 222),
				_Utils_Tuple2('szlig', 223),
				_Utils_Tuple2('agrave', 224),
				_Utils_Tuple2('aacute', 225),
				_Utils_Tuple2('acirc', 226),
				_Utils_Tuple2('atilde', 227),
				_Utils_Tuple2('auml', 228),
				_Utils_Tuple2('aring', 229),
				_Utils_Tuple2('aelig', 230),
				_Utils_Tuple2('ccedil', 231),
				_Utils_Tuple2('egrave', 232),
				_Utils_Tuple2('eacute', 233),
				_Utils_Tuple2('ecirc', 234),
				_Utils_Tuple2('euml', 235),
				_Utils_Tuple2('igrave', 236),
				_Utils_Tuple2('iacute', 237),
				_Utils_Tuple2('icirc', 238),
				_Utils_Tuple2('iuml', 239),
				_Utils_Tuple2('eth', 240),
				_Utils_Tuple2('ntilde', 241),
				_Utils_Tuple2('ograve', 242),
				_Utils_Tuple2('oacute', 243),
				_Utils_Tuple2('ocirc', 244),
				_Utils_Tuple2('otilde', 245),
				_Utils_Tuple2('ouml', 246),
				_Utils_Tuple2('divide', 247),
				_Utils_Tuple2('oslash', 248),
				_Utils_Tuple2('ugrave', 249),
				_Utils_Tuple2('uacute', 250),
				_Utils_Tuple2('ucirc', 251),
				_Utils_Tuple2('uuml', 252),
				_Utils_Tuple2('yacute', 253),
				_Utils_Tuple2('thorn', 254),
				_Utils_Tuple2('yuml', 255),
				_Utils_Tuple2('Amacr', 256),
				_Utils_Tuple2('amacr', 257),
				_Utils_Tuple2('Abreve', 258),
				_Utils_Tuple2('abreve', 259),
				_Utils_Tuple2('Aogon', 260),
				_Utils_Tuple2('aogon', 261),
				_Utils_Tuple2('Cacute', 262),
				_Utils_Tuple2('cacute', 263),
				_Utils_Tuple2('Ccirc', 264),
				_Utils_Tuple2('ccirc', 265),
				_Utils_Tuple2('Cdod', 266),
				_Utils_Tuple2('cdot', 267),
				_Utils_Tuple2('Ccaron', 268),
				_Utils_Tuple2('ccaron', 269),
				_Utils_Tuple2('Dcaron', 270),
				_Utils_Tuple2('dcaron', 271),
				_Utils_Tuple2('Dstork', 272),
				_Utils_Tuple2('dstork', 273),
				_Utils_Tuple2('Emacr', 274),
				_Utils_Tuple2('emacr', 275),
				_Utils_Tuple2('Edot', 278),
				_Utils_Tuple2('edot', 279),
				_Utils_Tuple2('Eogon', 280),
				_Utils_Tuple2('eogon', 281),
				_Utils_Tuple2('Ecaron', 282),
				_Utils_Tuple2('ecaron', 283),
				_Utils_Tuple2('Gcirc', 284),
				_Utils_Tuple2('gcirc', 285),
				_Utils_Tuple2('Gbreve', 286),
				_Utils_Tuple2('gbreve', 287),
				_Utils_Tuple2('Gdot', 288),
				_Utils_Tuple2('gdot', 289),
				_Utils_Tuple2('Gcedil', 290),
				_Utils_Tuple2('gcedil', 291),
				_Utils_Tuple2('Hcirc', 292),
				_Utils_Tuple2('hcirc', 293),
				_Utils_Tuple2('Hstork', 294),
				_Utils_Tuple2('hstork', 295),
				_Utils_Tuple2('Itilde', 296),
				_Utils_Tuple2('itilde', 297),
				_Utils_Tuple2('Imacr', 298),
				_Utils_Tuple2('imacr', 299),
				_Utils_Tuple2('Iogon', 302),
				_Utils_Tuple2('iogon', 303),
				_Utils_Tuple2('Idot', 304),
				_Utils_Tuple2('inodot', 305),
				_Utils_Tuple2('IJlog', 306),
				_Utils_Tuple2('ijlig', 307),
				_Utils_Tuple2('Jcirc', 308),
				_Utils_Tuple2('jcirc', 309),
				_Utils_Tuple2('Kcedil', 310),
				_Utils_Tuple2('kcedil', 311),
				_Utils_Tuple2('kgreen', 312),
				_Utils_Tuple2('Lacute', 313),
				_Utils_Tuple2('lacute', 314),
				_Utils_Tuple2('Lcedil', 315),
				_Utils_Tuple2('lcedil', 316),
				_Utils_Tuple2('Lcaron', 317),
				_Utils_Tuple2('lcaron', 318),
				_Utils_Tuple2('Lmodot', 319),
				_Utils_Tuple2('lmidot', 320),
				_Utils_Tuple2('Lstork', 321),
				_Utils_Tuple2('lstork', 322),
				_Utils_Tuple2('Nacute', 323),
				_Utils_Tuple2('nacute', 324),
				_Utils_Tuple2('Ncedil', 325),
				_Utils_Tuple2('ncedil', 326),
				_Utils_Tuple2('Ncaron', 327),
				_Utils_Tuple2('ncaron', 328),
				_Utils_Tuple2('napos', 329),
				_Utils_Tuple2('ENG', 330),
				_Utils_Tuple2('eng', 331),
				_Utils_Tuple2('Omacr', 332),
				_Utils_Tuple2('omacr', 333),
				_Utils_Tuple2('Odblac', 336),
				_Utils_Tuple2('odblac', 337),
				_Utils_Tuple2('OEling', 338),
				_Utils_Tuple2('oelig', 339),
				_Utils_Tuple2('Racute', 340),
				_Utils_Tuple2('racute', 341),
				_Utils_Tuple2('Rcedil', 342),
				_Utils_Tuple2('rcedil', 343),
				_Utils_Tuple2('Rcaron', 344),
				_Utils_Tuple2('rcaron', 345),
				_Utils_Tuple2('Sacute', 346),
				_Utils_Tuple2('sacute', 347),
				_Utils_Tuple2('Scirc', 348),
				_Utils_Tuple2('scirc', 349),
				_Utils_Tuple2('Scedil', 350),
				_Utils_Tuple2('scedil', 351),
				_Utils_Tuple2('Scaron', 352),
				_Utils_Tuple2('scaron', 353),
				_Utils_Tuple2('Tcedil', 354),
				_Utils_Tuple2('tcedil', 355),
				_Utils_Tuple2('Tcaron', 356),
				_Utils_Tuple2('tcaron', 357),
				_Utils_Tuple2('Tstork', 358),
				_Utils_Tuple2('tstork', 359),
				_Utils_Tuple2('Utilde', 360),
				_Utils_Tuple2('utilde', 361),
				_Utils_Tuple2('Umacr', 362),
				_Utils_Tuple2('umacr', 363),
				_Utils_Tuple2('Ubreve', 364),
				_Utils_Tuple2('ubreve', 365),
				_Utils_Tuple2('Uring', 366),
				_Utils_Tuple2('uring', 367),
				_Utils_Tuple2('Udblac', 368),
				_Utils_Tuple2('udblac', 369),
				_Utils_Tuple2('Uogon', 370),
				_Utils_Tuple2('uogon', 371),
				_Utils_Tuple2('Wcirc', 372),
				_Utils_Tuple2('wcirc', 373),
				_Utils_Tuple2('Ycirc', 374),
				_Utils_Tuple2('ycirc', 375),
				_Utils_Tuple2('Yuml', 376),
				_Utils_Tuple2('Zacute', 377),
				_Utils_Tuple2('zacute', 378),
				_Utils_Tuple2('Zdot', 379),
				_Utils_Tuple2('zdot', 380),
				_Utils_Tuple2('Zcaron', 381),
				_Utils_Tuple2('zcaron', 382),
				_Utils_Tuple2('fnof', 402),
				_Utils_Tuple2('imped', 437),
				_Utils_Tuple2('gacute', 501),
				_Utils_Tuple2('jmath', 567),
				_Utils_Tuple2('circ', 710),
				_Utils_Tuple2('tilde', 732),
				_Utils_Tuple2('Alpha', 913),
				_Utils_Tuple2('Beta', 914),
				_Utils_Tuple2('Gamma', 915),
				_Utils_Tuple2('Delta', 916),
				_Utils_Tuple2('Epsilon', 917),
				_Utils_Tuple2('Zeta', 918),
				_Utils_Tuple2('Eta', 919),
				_Utils_Tuple2('Theta', 920),
				_Utils_Tuple2('Iota', 921),
				_Utils_Tuple2('Kappa', 922),
				_Utils_Tuple2('Lambda', 923),
				_Utils_Tuple2('Mu', 924),
				_Utils_Tuple2('Nu', 925),
				_Utils_Tuple2('Xi', 926),
				_Utils_Tuple2('Omicron', 927),
				_Utils_Tuple2('Pi', 928),
				_Utils_Tuple2('Rho', 929),
				_Utils_Tuple2('Sigma', 931),
				_Utils_Tuple2('Tau', 932),
				_Utils_Tuple2('Upsilon', 933),
				_Utils_Tuple2('Phi', 934),
				_Utils_Tuple2('Chi', 935),
				_Utils_Tuple2('Psi', 936),
				_Utils_Tuple2('Omega', 937),
				_Utils_Tuple2('alpha', 945),
				_Utils_Tuple2('beta', 946),
				_Utils_Tuple2('gamma', 947),
				_Utils_Tuple2('delta', 948),
				_Utils_Tuple2('epsilon', 949),
				_Utils_Tuple2('zeta', 950),
				_Utils_Tuple2('eta', 951),
				_Utils_Tuple2('theta', 952),
				_Utils_Tuple2('iota', 953),
				_Utils_Tuple2('kappa', 954),
				_Utils_Tuple2('lambda', 955),
				_Utils_Tuple2('mu', 956),
				_Utils_Tuple2('nu', 957),
				_Utils_Tuple2('xi', 958),
				_Utils_Tuple2('omicron', 959),
				_Utils_Tuple2('pi', 960),
				_Utils_Tuple2('rho', 961),
				_Utils_Tuple2('sigmaf', 962),
				_Utils_Tuple2('sigma', 963),
				_Utils_Tuple2('tau', 934),
				_Utils_Tuple2('upsilon', 965),
				_Utils_Tuple2('phi', 966),
				_Utils_Tuple2('chi', 967),
				_Utils_Tuple2('psi', 968),
				_Utils_Tuple2('omega', 969),
				_Utils_Tuple2('thetasym', 977),
				_Utils_Tuple2('upsih', 978),
				_Utils_Tuple2('straightphi', 981),
				_Utils_Tuple2('piv', 982),
				_Utils_Tuple2('Gammad', 988),
				_Utils_Tuple2('gammad', 989),
				_Utils_Tuple2('varkappa', 1008),
				_Utils_Tuple2('varrho', 1009),
				_Utils_Tuple2('straightepsilon', 1013),
				_Utils_Tuple2('backepsilon', 1014),
				_Utils_Tuple2('ensp', 8194),
				_Utils_Tuple2('emsp', 8195),
				_Utils_Tuple2('thinsp', 8201),
				_Utils_Tuple2('zwnj', 8204),
				_Utils_Tuple2('zwj', 8205),
				_Utils_Tuple2('lrm', 8206),
				_Utils_Tuple2('rlm', 8207),
				_Utils_Tuple2('ndash', 8211),
				_Utils_Tuple2('mdash', 8212),
				_Utils_Tuple2('lsquo', 8216),
				_Utils_Tuple2('rsquo', 8217),
				_Utils_Tuple2('sbquo', 8218),
				_Utils_Tuple2('ldquo', 8220),
				_Utils_Tuple2('rdquo', 8221),
				_Utils_Tuple2('bdquo', 8222),
				_Utils_Tuple2('dagger', 8224),
				_Utils_Tuple2('Dagger', 8225),
				_Utils_Tuple2('bull', 8226),
				_Utils_Tuple2('hellip', 8230),
				_Utils_Tuple2('permil', 8240),
				_Utils_Tuple2('prime', 8242),
				_Utils_Tuple2('Prime', 8243),
				_Utils_Tuple2('lsaquo', 8249),
				_Utils_Tuple2('rsaquo', 8250),
				_Utils_Tuple2('oline', 8254),
				_Utils_Tuple2('frasl', 8260),
				_Utils_Tuple2('sigma', 963),
				_Utils_Tuple2('euro', 8364),
				_Utils_Tuple2('image', 8465),
				_Utils_Tuple2('weierp', 8472),
				_Utils_Tuple2('real', 8476),
				_Utils_Tuple2('trade', 8482),
				_Utils_Tuple2('alefsym', 8501),
				_Utils_Tuple2('larr', 8592),
				_Utils_Tuple2('uarr', 8593),
				_Utils_Tuple2('rarr', 8594),
				_Utils_Tuple2('darr', 8595),
				_Utils_Tuple2('harr', 8596),
				_Utils_Tuple2('crarr', 8629),
				_Utils_Tuple2('lArr', 8656),
				_Utils_Tuple2('uArr', 8657),
				_Utils_Tuple2('rArr', 8658),
				_Utils_Tuple2('dArr', 8659),
				_Utils_Tuple2('hArr', 8660),
				_Utils_Tuple2('forall', 8704),
				_Utils_Tuple2('part', 8706),
				_Utils_Tuple2('exist', 8707),
				_Utils_Tuple2('empty', 8709),
				_Utils_Tuple2('nabla', 8711),
				_Utils_Tuple2('isin', 8712),
				_Utils_Tuple2('notin', 8713),
				_Utils_Tuple2('ni', 8715),
				_Utils_Tuple2('prod', 8719),
				_Utils_Tuple2('sum', 8721),
				_Utils_Tuple2('minus', 8722),
				_Utils_Tuple2('lowast', 8727),
				_Utils_Tuple2('radic', 8730),
				_Utils_Tuple2('prop', 8733),
				_Utils_Tuple2('infin', 8734),
				_Utils_Tuple2('ang', 8736),
				_Utils_Tuple2('and', 8743),
				_Utils_Tuple2('or', 8744),
				_Utils_Tuple2('cap', 8745),
				_Utils_Tuple2('cup', 8746),
				_Utils_Tuple2('int', 8747),
				_Utils_Tuple2('there4', 8756),
				_Utils_Tuple2('sim', 8764),
				_Utils_Tuple2('cong', 8773),
				_Utils_Tuple2('asymp', 8776),
				_Utils_Tuple2('ne', 8800),
				_Utils_Tuple2('equiv', 8801),
				_Utils_Tuple2('le', 8804),
				_Utils_Tuple2('ge', 8805),
				_Utils_Tuple2('sub', 8834),
				_Utils_Tuple2('sup', 8835),
				_Utils_Tuple2('nsub', 8836),
				_Utils_Tuple2('sube', 8838),
				_Utils_Tuple2('supe', 8839),
				_Utils_Tuple2('oplus', 8853),
				_Utils_Tuple2('otimes', 8855),
				_Utils_Tuple2('perp', 8869),
				_Utils_Tuple2('sdot', 8901),
				_Utils_Tuple2('loz', 9674),
				_Utils_Tuple2('spades', 9824),
				_Utils_Tuple2('clubs', 9827),
				_Utils_Tuple2('hearts', 9829),
				_Utils_Tuple2('diams', 9830)
			])));
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$convertFriendlyCodeToChar = function (string) {
	return A2($elm$core$Dict$get, string, $marcosh$elm_html_to_unicode$ElmEscapeHtml$friendlyConverterDictionary);
};
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$convertFriendlyCode = A2(
	$marcosh$elm_html_to_unicode$ElmEscapeHtml$convertCode,
	$marcosh$elm_html_to_unicode$ElmEscapeHtml$convertFriendlyCodeToChar,
	function (_char) {
		return _List_fromArray(
			[_char]);
	});
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$convertDecimalCode = A2(
	$marcosh$elm_html_to_unicode$ElmEscapeHtml$convertCode,
	$elm$core$String$toInt,
	function (_int) {
		return _List_fromArray(
			[
				$elm$core$Char$fromCode(_int)
			]);
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$charOffset = F2(
	function (basis, c) {
		return $elm$core$Char$toCode(c) - $elm$core$Char$toCode(basis);
	});
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$isBetween = F3(
	function (lower, upper, c) {
		var ci = $elm$core$Char$toCode(c);
		return (_Utils_cmp(
			$elm$core$Char$toCode(lower),
			ci) < 1) && (_Utils_cmp(
			ci,
			$elm$core$Char$toCode(upper)) < 1);
	});
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$intFromChar = function (c) {
	var validInt = function (i) {
		return (i < 16) ? $elm$core$Maybe$Just(i) : $elm$core$Maybe$Nothing;
	};
	var toInt = A3(
		$marcosh$elm_html_to_unicode$ElmEscapeHtml$isBetween,
		_Utils_chr('0'),
		_Utils_chr('9'),
		c) ? $elm$core$Maybe$Just(
		A2(
			$marcosh$elm_html_to_unicode$ElmEscapeHtml$charOffset,
			_Utils_chr('0'),
			c)) : (A3(
		$marcosh$elm_html_to_unicode$ElmEscapeHtml$isBetween,
		_Utils_chr('a'),
		_Utils_chr('z'),
		c) ? $elm$core$Maybe$Just(
		10 + A2(
			$marcosh$elm_html_to_unicode$ElmEscapeHtml$charOffset,
			_Utils_chr('a'),
			c)) : (A3(
		$marcosh$elm_html_to_unicode$ElmEscapeHtml$isBetween,
		_Utils_chr('A'),
		_Utils_chr('Z'),
		c) ? $elm$core$Maybe$Just(
		10 + A2(
			$marcosh$elm_html_to_unicode$ElmEscapeHtml$charOffset,
			_Utils_chr('A'),
			c)) : $elm$core$Maybe$Nothing));
	return A2($elm$core$Maybe$andThen, validInt, toInt);
};
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$parseIntR = function (string) {
	var _v0 = $elm$core$String$uncons(string);
	if (_v0.$ === 'Nothing') {
		return $elm$core$Maybe$Just(0);
	} else {
		var _v1 = _v0.a;
		var c = _v1.a;
		var tail = _v1.b;
		return A2(
			$elm$core$Maybe$andThen,
			function (ci) {
				return A2(
					$elm$core$Maybe$andThen,
					function (ri) {
						return $elm$core$Maybe$Just(ci + (ri * 16));
					},
					$marcosh$elm_html_to_unicode$ElmEscapeHtml$parseIntR(tail));
			},
			$marcosh$elm_html_to_unicode$ElmEscapeHtml$intFromChar(c));
	}
};
var $elm$core$String$reverse = _String_reverse;
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$parseIntHex = function (string) {
	return $marcosh$elm_html_to_unicode$ElmEscapeHtml$parseIntR(
		$elm$core$String$reverse(string));
};
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$convertHexadecimalCode = A2(
	$marcosh$elm_html_to_unicode$ElmEscapeHtml$convertCode,
	$marcosh$elm_html_to_unicode$ElmEscapeHtml$parseIntHex,
	function (_int) {
		return _List_fromArray(
			[
				$elm$core$Char$fromCode(_int)
			]);
	});
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$convertNumericalCode = F3(
	function (pre, post, list) {
		if (!list.b) {
			return $elm$core$List$concat(
				_List_fromArray(
					[pre, post]));
		} else {
			if ('x' === list.a.valueOf()) {
				var tail = list.b;
				return A3(
					$marcosh$elm_html_to_unicode$ElmEscapeHtml$convertHexadecimalCode,
					A2(
						$elm$core$List$append,
						pre,
						_List_fromArray(
							[
								_Utils_chr('x')
							])),
					post,
					tail);
			} else {
				var anyOtherList = list;
				return A3($marcosh$elm_html_to_unicode$ElmEscapeHtml$convertDecimalCode, pre, post, anyOtherList);
			}
		}
	});
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$noAmpUnicodeConverter = F3(
	function (pre, post, list) {
		if (!list.b) {
			return _List_fromArray(
				[pre, post]);
		} else {
			if ('#' === list.a.valueOf()) {
				var tail = list.b;
				return A3(
					$marcosh$elm_html_to_unicode$ElmEscapeHtml$convertNumericalCode,
					_List_fromArray(
						[
							pre,
							_Utils_chr('#')
						]),
					_List_fromArray(
						[post]),
					tail);
			} else {
				var head = list.a;
				var tail = list.b;
				return A3(
					$marcosh$elm_html_to_unicode$ElmEscapeHtml$convertFriendlyCode,
					_List_fromArray(
						[pre]),
					_List_fromArray(
						[post]),
					A2($elm$core$List$cons, head, tail));
			}
		}
	});
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$unicodeConverter = F2(
	function (post, list) {
		if (!list.b) {
			return _List_fromArray(
				[post]);
		} else {
			var head = list.a;
			var tail = list.b;
			return A3($marcosh$elm_html_to_unicode$ElmEscapeHtml$noAmpUnicodeConverter, head, post, tail);
		}
	});
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$parser = F3(
	function (charsToBeParsed, charsOnParsing, charsParsed) {
		parser:
		while (true) {
			if (!charsToBeParsed.b) {
				return charsParsed;
			} else {
				var head = charsToBeParsed.a;
				var tail = charsToBeParsed.b;
				if (_Utils_eq(
					head,
					_Utils_chr('&'))) {
					var $temp$charsToBeParsed = tail,
						$temp$charsOnParsing = _List_fromArray(
						[head]),
						$temp$charsParsed = charsParsed;
					charsToBeParsed = $temp$charsToBeParsed;
					charsOnParsing = $temp$charsOnParsing;
					charsParsed = $temp$charsParsed;
					continue parser;
				} else {
					if (_Utils_eq(
						head,
						_Utils_chr(';'))) {
						var $temp$charsToBeParsed = tail,
							$temp$charsOnParsing = _List_Nil,
							$temp$charsParsed = A2(
							$elm$core$List$append,
							charsParsed,
							A2($marcosh$elm_html_to_unicode$ElmEscapeHtml$unicodeConverter, head, charsOnParsing));
						charsToBeParsed = $temp$charsToBeParsed;
						charsOnParsing = $temp$charsOnParsing;
						charsParsed = $temp$charsParsed;
						continue parser;
					} else {
						if (!$elm$core$List$isEmpty(charsOnParsing)) {
							var $temp$charsToBeParsed = tail,
								$temp$charsOnParsing = A2(
								$elm$core$List$append,
								charsOnParsing,
								_List_fromArray(
									[head])),
								$temp$charsParsed = charsParsed;
							charsToBeParsed = $temp$charsToBeParsed;
							charsOnParsing = $temp$charsOnParsing;
							charsParsed = $temp$charsParsed;
							continue parser;
						} else {
							var $temp$charsToBeParsed = tail,
								$temp$charsOnParsing = _List_Nil,
								$temp$charsParsed = A2(
								$elm$core$List$append,
								charsParsed,
								_List_fromArray(
									[head]));
							charsToBeParsed = $temp$charsToBeParsed;
							charsOnParsing = $temp$charsOnParsing;
							charsParsed = $temp$charsParsed;
							continue parser;
						}
					}
				}
			}
		}
	});
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$unescapeChars = function (list) {
	return A3($marcosh$elm_html_to_unicode$ElmEscapeHtml$parser, list, _List_Nil, _List_Nil);
};
var $marcosh$elm_html_to_unicode$ElmEscapeHtml$unescape = $marcosh$elm_html_to_unicode$ElmEscapeHtml$convert($marcosh$elm_html_to_unicode$ElmEscapeHtml$unescapeChars);
var $author$project$Clue$renderClueText = function (clue) {
	return $marcosh$elm_html_to_unicode$ElmEscapeHtml$unescape(clue.clue_text) + (' (' + ($elm$core$String$fromInt(
		$elm$core$String$length(clue.solution)) + ')'));
};
var $elm$html$Html$strong = _VirtualDom_node('strong');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Clue$renderClue = F5(
	function (clue, clueIndex, activeClueIndex, clues, grid) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('cluegrid-clue', true),
							_Utils_Tuple2(
							'cluegrid-clue-is-active',
							A3($author$project$Clue$isActiveClue, clue, activeClueIndex, clues))
						])),
					$elm$html$Html$Attributes$id(
					$author$project$Clue$getClueId(clueIndex)),
					$elm$html$Html$Events$onClick(
					$author$project$Datatypes$ClueClicked(clueIndex))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$strong,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('cluegrid-clue-number')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(clue.gridNumber))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('cluegrid-clue-spacer')
						]),
					_List_Nil),
					A2(
					$elm$html$Html$strong,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('cluegrid-clue-direction')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$Clue$clueDirectionToText(clue.direction))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('cluegrid-clue-spacer')
						]),
					_List_Nil),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('cluegrid-clue-text')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$Clue$renderClueText(clue))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('cluegrid-clue-current-solution')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							A2($author$project$Clue$getClueCurrentSolution, clue, grid))
						]))
				]));
	});
var $author$project$Clue$renderCluesData = F3(
	function (clues, grid, activeClueIndex) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('cluegrid-clues-container')
				]),
			A2(
				$elm$core$List$append,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$strong,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('cluegrid-clues-header')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('CLUES')
							]))
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('cluegrid-clues-cluelist'),
								$elm$html$Html$Attributes$id('cluegrid-clues-scrollable-area')
							]),
						A2(
							$elm$core$List$indexedMap,
							F2(
								function (index, clue) {
									return A5($author$project$Clue$renderClue, clue, index, activeClueIndex, clues, grid);
								}),
							clues))
					])));
	});
var $author$project$Datatypes$CellClicked = F2(
	function (a, b) {
		return {$: 'CellClicked', a: a, b: b};
	});
var $author$project$Cell$isActiveCell = F2(
	function (cell, activeCell) {
		if (activeCell.$ === 'Just') {
			var _v1 = activeCell.a;
			var rowNum = _v1.a;
			var colNum = _v1.b;
			return _Utils_eq(rowNum, cell.row) && _Utils_eq(colNum, cell.col);
		} else {
			return false;
		}
	});
var $author$project$Cell$isActiveCellClue = F2(
	function (cell, activeClueIndex) {
		if (activeClueIndex.$ === 'Just') {
			var index = activeClueIndex.a;
			return function () {
				var _v1 = cell.acrossClueIndex;
				if (_v1.$ === 'Just') {
					var clueIndex = _v1.a;
					return _Utils_eq(index, clueIndex);
				} else {
					return false;
				}
			}() || function () {
				var _v2 = cell.downClueIndex;
				if (_v2.$ === 'Just') {
					var clueIndex = _v2.a;
					return _Utils_eq(index, clueIndex);
				} else {
					return false;
				}
			}();
		} else {
			return false;
		}
	});
var $author$project$Cell$renderCell = F4(
	function (cell, activeClueIndex, otherActiveClueIndex, activeCell) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('cluegrid-crossword-cell', true),
							_Utils_Tuple2(
							'cluegrid-crossword-cell-is-blank',
							$author$project$Cell$crosswordCellisBlank(cell)),
							_Utils_Tuple2(
							'cluegrid-crossword-cell-is-active',
							A2($author$project$Cell$isActiveCell, cell, activeCell)),
							_Utils_Tuple2(
							'cluegrid-crossword-cell-is-other-clue',
							A2($author$project$Cell$isActiveCellClue, cell, otherActiveClueIndex)),
							_Utils_Tuple2(
							'cluegrid-crossword-cell-is-active-clue',
							A2($author$project$Cell$isActiveCellClue, cell, activeClueIndex))
						])),
					$elm$html$Html$Events$onClick(
					A2($author$project$Datatypes$CellClicked, cell.row, cell.col))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('cluegrid-crossword-cell-grid-number', true)
								]))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							function () {
								var _v0 = cell.gridNumber;
								if (_v0.$ === 'Just') {
									var num = _v0.a;
									return $elm$core$String$fromInt(num);
								} else {
									return '';
								}
							}())
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('cluegrid-crossword-cell-solution')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							function () {
								var _v1 = cell.entry;
								if (_v1.$ === 'Just') {
									var entry = _v1.a;
									return entry;
								} else {
									return '';
								}
							}())
						]))
				]));
	});
var $author$project$Cell$renderRow = F4(
	function (row, activeClueIndex, otherActiveClueIndex, activeCell) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('cluegrid-crossword-row')
				]),
			$elm$core$Array$toList(
				A2(
					$elm$core$Array$map,
					function (cell) {
						return A4($author$project$Cell$renderCell, cell, activeClueIndex, otherActiveClueIndex, activeCell);
					},
					row)));
	});
var $author$project$Cell$renderGrid = F4(
	function (grid, activeClueIndex, otherActiveClueIndex, activeCell) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('cluegrid-crossword-container')
				]),
			$elm$core$Array$toList(
				A2(
					$elm$core$Array$map,
					function (row) {
						return A4($author$project$Cell$renderRow, row, activeClueIndex, otherActiveClueIndex, activeCell);
					},
					grid)));
	});
var $author$project$Datatypes$CloseModal = {$: 'CloseModal'};
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $author$project$Controls$renderModal = function (appData) {
	var _v0 = appData.modal;
	if (_v0.$ === 'Info') {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('cluegrid-modal-background'),
					$elm$html$Html$Events$onClick($author$project$Datatypes$CloseModal)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('cluegrid-modal-container')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('cluegrid-modal-header')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('cluegrid')
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('cluegrid-modal-vert-spacer')
								]),
							_List_Nil),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('cluegrid-modal-vert-spacer')
								]),
							_List_Nil),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('cluegrid-modal-info-title')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(appData.cluegridInfo.title)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('cluegrid-modal-info cluegrid-modal-bold')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Author: ' + appData.cluegridInfo.author)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('cluegrid-modal-info cluegrid-modal-bold')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Editor: ' + appData.cluegridInfo.editor)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('cluegrid-modal-info')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(appData.cluegridInfo.copyright)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('cluegrid-modal-vert-spacer')
								]),
							_List_Nil),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('cluegrid-modal-vert-spacer')
								]),
							_List_Nil),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('cluegrid-modal-vert-spacer')
								]),
							_List_Nil),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('cluegrid-modal-info')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('data from '),
									A2(
									$elm$html$Html$a,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$href('https://www.xwordinfo.com/')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('xwordinfo')
										]))
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('cluegrid-modal-info')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('created with ❤️ by '),
									A2(
									$elm$html$Html$a,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$href('https://samhattangady.com')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('chapliboy')
										]))
								]))
						]))
				]));
	} else {
		return A2($elm$html$Html$div, _List_Nil, _List_Nil);
	}
};
var $author$project$Controls$renderAppData = function (appData) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('cluegrid-container')
			]),
		_List_fromArray(
			[
				A4($author$project$Cell$renderGrid, appData.grid, appData.activeClueIndex, appData.otherClueIndex, appData.activeCell),
				A3($author$project$Clue$renderCluesData, appData.clues, appData.grid, appData.activeClueIndex),
				$author$project$Controls$renderModal(appData)
			]));
};
var $author$project$Datatypes$CheckActiveClue = {$: 'CheckActiveClue'};
var $author$project$Datatypes$SetModalInfo = {$: 'SetModalInfo'};
var $author$project$Datatypes$SolveActiveClue = {$: 'SolveActiveClue'};
var $author$project$Controls$renderHeaderCell = function (_v0) {
	var letter = _v0.a;
	var num = _v0.b;
	var solution = function () {
		if (letter === '') {
			return '.';
		} else {
			return '';
		}
	}();
	return A4(
		$author$project$Cell$renderCell,
		A7(
			$author$project$Datatypes$Cell,
			solution,
			-1,
			-1,
			num,
			$elm$core$Maybe$Nothing,
			$elm$core$Maybe$Nothing,
			$elm$core$Maybe$Just(letter)),
		$elm$core$Maybe$Nothing,
		$elm$core$Maybe$Nothing,
		$elm$core$Maybe$Nothing);
};
var $author$project$Controls$renderHeaderRow = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			$elm$html$Html$Attributes$class('cluegrid-header-container')
		]),
	_List_fromArray(
		[
			A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('cluegrid-header-row')
				]),
			A2(
				$elm$core$List$map,
				function (letter) {
					return $author$project$Controls$renderHeaderCell(letter);
				},
				_List_fromArray(
					[
						_Utils_Tuple2(
						'C',
						$elm$core$Maybe$Just(3)),
						_Utils_Tuple2('L', $elm$core$Maybe$Nothing),
						_Utils_Tuple2('U', $elm$core$Maybe$Nothing),
						_Utils_Tuple2('E', $elm$core$Maybe$Nothing),
						_Utils_Tuple2('', $elm$core$Maybe$Nothing),
						_Utils_Tuple2(
						'G',
						$elm$core$Maybe$Just(7)),
						_Utils_Tuple2('R', $elm$core$Maybe$Nothing),
						_Utils_Tuple2('I', $elm$core$Maybe$Nothing),
						_Utils_Tuple2('D', $elm$core$Maybe$Nothing)
					]))),
			A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('cluegrid-header-buttons')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('cluegrid-header-button'),
							$elm$html$Html$Events$onClick($author$project$Datatypes$SolveActiveClue)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('SOLVE CLUE')
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('cluegrid-header-button'),
							$elm$html$Html$Events$onClick($author$project$Datatypes$CheckActiveClue)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('CHECK CLUE')
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('cluegrid-header-button'),
							$elm$html$Html$Events$onClick($author$project$Datatypes$SetModalInfo)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('INFO')
						]))
				]))
		]));
var $author$project$Main$view = function (model) {
	var body = function () {
		switch (model.$) {
			case 'Loaded':
				var appData = model.a;
				return $author$project$Controls$renderAppData(appData);
			case 'Failure':
				return A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('cluegrid-data-not-loaded')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Could not fetch data ‾\\_(ツ)_/‾')
						]));
			default:
				return A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('cluegrid-data-not-loaded')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('loading data...')
						]));
		}
	}();
	return A2(
		$elm$browser$Browser$Document,
		'Cluegrid',
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('cluegrid-fullscreen-container')
					]),
				_List_fromArray(
					[
						$author$project$Controls$renderHeaderRow,
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('cluegrid-application-container')
							]),
						_List_fromArray(
							[body]))
					]))
			]));
};
var $author$project$Main$main = $elm$browser$Browser$application(
	{init: $author$project$Main$init, onUrlChange: $author$project$Main$onUrlChange, onUrlRequest: $author$project$Main$onUrlRequest, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));
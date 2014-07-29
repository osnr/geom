/*! caress-client - v0.1.0 - 2013-01-17
* https://github.com/ekryski/caress-client
* Copyright (c) 2013 Eric Kryski; Licensed MIT */

/*!
 * Lo-Dash v0.9.1 <http://lodash.com>
 * (c) 2012 John-David Dalton <http://allyoucanleet.com/>
 * Based on Underscore.js 1.4.2 <http://underscorejs.org>
 * (c) 2009-2012 Jeremy Ashkenas, DocumentCloud Inc.
 * Available under MIT license <http://lodash.com/license>
 */
;(function(window, undefined) {

  /** Detect free variable `exports` */
  var freeExports = typeof exports == 'object' && exports;

  /** Detect free variable `global` and use it as `window` */
  var freeGlobal = typeof global == 'object' && global;
  if (freeGlobal.global === freeGlobal) {
    window = freeGlobal;
  }

  /** Used for array and object method references */
  var arrayRef = [],
      objectRef = {};

  /** Used to generate unique IDs */
  var idCounter = 0;

  /** Used internally to indicate various things */
  var indicatorObject = {};

  /** Used by `cachedContains` as the default size when optimizations are enabled for large arrays */
  var largeArraySize = 30;

  /** Used to restore the original `_` reference in `noConflict` */
  var oldDash = window._;

  /** Used to detect delimiter values that should be processed by `tokenizeEvaluate` */
  var reComplexDelimiter = /[-?+=!~*%&^<>|{(\/]|\[\D|\b(?:delete|in|instanceof|new|typeof|void)\b/;

  /** Used to match HTML entities */
  var reEscapedHtml = /&(?:amp|lt|gt|quot|#x27);/g;

  /** Used to match empty string literals in compiled template source */
  var reEmptyStringLeading = /\b__p \+= '';/g,
      reEmptyStringMiddle = /\b(__p \+=) '' \+/g,
      reEmptyStringTrailing = /(__e\(.*?\)|\b__t\)) \+\n'';/g;

  /** Used to match regexp flags from their coerced string values */
  var reFlags = /\w*$/;

  /** Used to insert the data object variable into compiled template source */
  var reInsertVariable = /(?:__e|__t = )\(\s*(?![\d\s"']|this\.)/g;

  /** Used to detect if a method is native */
  var reNative = RegExp('^' +
    (objectRef.valueOf + '')
      .replace(/[.*+?^=!:${}()|[\]\/\\]/g, '\\$&')
      .replace(/valueOf|for [^\]]+/g, '.+?') + '$'
  );

  /** Used to ensure capturing order and avoid matches for undefined delimiters */
  var reNoMatch = /($^)/;

  /** Used to match HTML characters */
  var reUnescapedHtml = /[&<>"']/g;

  /** Used to match unescaped characters in compiled string literals */
  var reUnescapedString = /['\n\r\t\u2028\u2029\\]/g;

  /** Used to fix the JScript [[DontEnum]] bug */
  var shadowed = [
    'constructor', 'hasOwnProperty', 'isPrototypeOf', 'propertyIsEnumerable',
    'toLocaleString', 'toString', 'valueOf'
  ];

  /** Used to make template sourceURLs easier to identify */
  var templateCounter = 0;

  /** Native method shortcuts */
  var ceil = Math.ceil,
      concat = arrayRef.concat,
      floor = Math.floor,
      getPrototypeOf = reNative.test(getPrototypeOf = Object.getPrototypeOf) && getPrototypeOf,
      hasOwnProperty = objectRef.hasOwnProperty,
      push = arrayRef.push,
      propertyIsEnumerable = objectRef.propertyIsEnumerable,
      slice = arrayRef.slice,
      toString = objectRef.toString;

  /* Native method shortcuts for methods with the same name as other `lodash` methods */
  var nativeBind = reNative.test(nativeBind = slice.bind) && nativeBind,
      nativeIsArray = reNative.test(nativeIsArray = Array.isArray) && nativeIsArray,
      nativeIsFinite = window.isFinite,
      nativeIsNaN = window.isNaN,
      nativeKeys = reNative.test(nativeKeys = Object.keys) && nativeKeys,
      nativeMax = Math.max,
      nativeMin = Math.min,
      nativeRandom = Math.random;

  /** `Object#toString` result shortcuts */
  var argsClass = '[object Arguments]',
      arrayClass = '[object Array]',
      boolClass = '[object Boolean]',
      dateClass = '[object Date]',
      funcClass = '[object Function]',
      numberClass = '[object Number]',
      objectClass = '[object Object]',
      regexpClass = '[object RegExp]',
      stringClass = '[object String]';

  /**
   * Detect the JScript [[DontEnum]] bug:
   *
   * In IE < 9 an objects own properties, shadowing non-enumerable ones, are
   * made non-enumerable as well.
   */
  var hasDontEnumBug;

  /** Detect if own properties are iterated after inherited properties (IE < 9) */
  var iteratesOwnLast;

  /** Detect if an `arguments` object's indexes are non-enumerable (IE < 9) */
  var noArgsEnum = true;

  (function() {
    var props = [];
    function ctor() { this.x = 1; }
    ctor.prototype = { 'valueOf': 1, 'y': 1 };
    for (var prop in new ctor) { props.push(prop); }
    for (prop in arguments) { noArgsEnum = !prop; }

    hasDontEnumBug = !/valueOf/.test(props);
    iteratesOwnLast = props[0] != 'x';
  }(1));

  /** Detect if an `arguments` object's [[Class]] is unresolvable (Firefox < 4, IE < 9) */
  var noArgsClass = !isArguments(arguments);

  /** Detect if `Array#slice` cannot be used to convert strings to arrays (Opera < 10.52) */
  var noArraySliceOnStrings = slice.call('x')[0] != 'x';

  /**
   * Detect lack of support for accessing string characters by index:
   *
   * IE < 8 can't access characters by index and IE 8 can only access
   * characters by index on string literals.
   */
  var noCharByIndex = ('x'[0] + Object('x')[0]) != 'xx';

  /**
   * Detect if a node's [[Class]] is unresolvable (IE < 9)
   * and that the JS engine won't error when attempting to coerce an object to
   * a string without a `toString` property value of `typeof` "function".
   */
  try {
    var noNodeClass = ({ 'toString': 0 } + '', toString.call(window.document || 0) == objectClass);
  } catch(e) { }

  /* Detect if `Function#bind` exists and is inferred to be fast (all but V8) */
  var isBindFast = nativeBind && /\n|Opera/.test(nativeBind + toString.call(window.opera));

  /* Detect if `Object.keys` exists and is inferred to be fast (IE, Opera, V8) */
  var isKeysFast = nativeKeys && /^.+$|true/.test(nativeKeys + !!window.attachEvent);

  /**
   * Detect if sourceURL syntax is usable without erroring:
   *
   * The JS engine in Adobe products, like InDesign, will throw a syntax error
   * when it encounters a single line comment beginning with the `@` symbol.
   *
   * The JS engine in Narwhal will generate the function `function anonymous(){//}`
   * and throw a syntax error.
   *
   * Avoid comments beginning `@` symbols in IE because they are part of its
   * non-standard conditional compilation support.
   * http://msdn.microsoft.com/en-us/library/121hztk3(v=vs.94).aspx
   */
  try {
    var useSourceURL = (Function('//@')(), !window.attachEvent);
  } catch(e) { }

  /** Used to identify object classifications that `_.clone` supports */
  var cloneableClasses = {};
  cloneableClasses[argsClass] = cloneableClasses[funcClass] = false;
  cloneableClasses[arrayClass] = cloneableClasses[boolClass] = cloneableClasses[dateClass] =
  cloneableClasses[numberClass] = cloneableClasses[objectClass] = cloneableClasses[regexpClass] =
  cloneableClasses[stringClass] = true;

  /** Used to determine if values are of the language type Object */
  var objectTypes = {
    'boolean': false,
    'function': true,
    'object': true,
    'number': false,
    'string': false,
    'undefined': false
  };

  /** Used to escape characters for inclusion in compiled string literals */
  var stringEscapes = {
    '\\': '\\',
    "'": "'",
    '\n': 'n',
    '\r': 'r',
    '\t': 't',
    '\u2028': 'u2028',
    '\u2029': 'u2029'
  };

  /*--------------------------------------------------------------------------*/

  /**
   * The `lodash` function.
   *
   * @name _
   * @constructor
   * @category Chaining
   * @param {Mixed} value The value to wrap in a `lodash` instance.
   * @returns {Object} Returns a `lodash` instance.
   */
  function lodash(value) {
    // exit early if already wrapped
    if (value && value.__wrapped__) {
      return value;
    }
    // allow invoking `lodash` without the `new` operator
    if (!(this instanceof lodash)) {
      return new lodash(value);
    }
    this.__wrapped__ = value;
  }

  /**
   * By default, the template delimiters used by Lo-Dash are similar to those in
   * embedded Ruby (ERB). Change the following template settings to use alternative
   * delimiters.
   *
   * @static
   * @memberOf _
   * @type Object
   */
  lodash.templateSettings = {

    /**
     * Used to detect `data` property values to be HTML-escaped.
     *
     * @static
     * @memberOf _.templateSettings
     * @type RegExp
     */
    'escape': /<%-([\s\S]+?)%>/g,

    /**
     * Used to detect code to be evaluated.
     *
     * @static
     * @memberOf _.templateSettings
     * @type RegExp
     */
    'evaluate': /<%([\s\S]+?)%>/g,

    /**
     * Used to detect `data` property values to inject.
     *
     * @static
     * @memberOf _.templateSettings
     * @type RegExp
     */
    'interpolate': /<%=([\s\S]+?)%>/g,

    /**
     * Used to reference the data object in the template text.
     *
     * @static
     * @memberOf _.templateSettings
     * @type String
     */
    'variable': ''
  };

  /*--------------------------------------------------------------------------*/

  /**
   * The template used to create iterator functions.
   *
   * @private
   * @param {Obect} data The data object used to populate the text.
   * @returns {String} Returns the interpolated text.
   */
  var iteratorTemplate = function(obj) {
    
    var __p = 'var index, value, iteratee = ' +
    (obj.firstArg ) +
    ', result = ' +
    (obj.firstArg ) +
    ';\nif (!' +
    (obj.firstArg ) +
    ') return result;\n' +
    (obj.top ) +
    ';\n';
     if (obj.arrayLoop) {
    __p += 'var length = iteratee.length; index = -1;\nif (typeof length == \'number\') {  ';
     if (obj.noCharByIndex) {
    __p += '\n  if (toString.call(iteratee) == stringClass) {\n    iteratee = iteratee.split(\'\')\n  }  ';
     } ;
    __p += '\n  while (++index < length) {\n    value = iteratee[index];\n    ' +
    (obj.arrayLoop ) +
    '\n  }\n}\nelse {  ';
      } else if (obj.noArgsEnum) {
    __p += '\n  var length = iteratee.length; index = -1;\n  if (length && isArguments(iteratee)) {\n    while (++index < length) {\n      value = iteratee[index += \'\'];\n      ' +
    (obj.objectLoop ) +
    '\n    }\n  } else {  ';
     } ;
    
     if (!obj.hasDontEnumBug) {
    __p += '\n  var skipProto = typeof iteratee == \'function\' && \n    propertyIsEnumerable.call(iteratee, \'prototype\');\n  ';
     } ;
    
     if (obj.isKeysFast && obj.useHas) {
    __p += '\n  var ownIndex = -1,\n      ownProps = objectTypes[typeof iteratee] ? nativeKeys(iteratee) : [],\n      length = ownProps.length;\n\n  while (++ownIndex < length) {\n    index = ownProps[ownIndex];\n    ';
     if (!obj.hasDontEnumBug) {
    __p += 'if (!(skipProto && index == \'prototype\')) {\n  ';
     } ;
    __p += '    value = iteratee[index];\n    ' +
    (obj.objectLoop ) +
    '';
     if (!obj.hasDontEnumBug) {
    __p += '}\n';
     } ;
    __p += '  }  ';
     } else {
    __p += '\n  for (index in iteratee) {';
        if (!obj.hasDontEnumBug || obj.useHas) {
    __p += '\n    if (';
          if (!obj.hasDontEnumBug) {
    __p += '!(skipProto && index == \'prototype\')';
     }      if (!obj.hasDontEnumBug && obj.useHas) {
    __p += ' && ';
     }      if (obj.useHas) {
    __p += 'hasOwnProperty.call(iteratee, index)';
     }    ;
    __p += ') {    ';
     } ;
    __p += '\n    value = iteratee[index];\n    ' +
    (obj.objectLoop ) +
    ';    ';
     if (!obj.hasDontEnumBug || obj.useHas) {
    __p += '\n    }';
     } ;
    __p += '\n  }  ';
     } ;
    
     if (obj.hasDontEnumBug) {
    __p += '\n\n  var ctor = iteratee.constructor;\n    ';
     for (var k = 0; k < 7; k++) {
    __p += '\n  index = \'' +
    (obj.shadowed[k] ) +
    '\';\n  if (';
          if (obj.shadowed[k] == 'constructor') {
    __p += '!(ctor && ctor.prototype === iteratee) && ';
          } ;
    __p += 'hasOwnProperty.call(iteratee, index)) {\n    value = iteratee[index];\n    ' +
    (obj.objectLoop ) +
    '\n  }    ';
     } ;
    
     } ;
    
     if (obj.arrayLoop || obj.noArgsEnum) {
    __p += '\n}';
     } ;
    __p += 
    (obj.bottom ) +
    ';\nreturn result';
    
    
    return __p
  };

  /**
   * Reusable iterator options shared by `forEach`, `forIn`, and `forOwn`.
   */
  var forEachIteratorOptions = {
    'args': 'collection, callback, thisArg',
    'top': 'callback = createCallback(callback, thisArg)',
    'arrayLoop': 'if (callback(value, index, collection) === false) return result',
    'objectLoop': 'if (callback(value, index, collection) === false) return result'
  };

  /** Reusable iterator options for `defaults`, and `extend` */
  var extendIteratorOptions = {
    'useHas': false,
    'args': 'object',
    'top':
      'for (var argsIndex = 1, argsLength = arguments.length; argsIndex < argsLength; argsIndex++) {\n' +
      '  if (iteratee = arguments[argsIndex]) {',
    'objectLoop': 'result[index] = value',
    'bottom': '  }\n}'
  };

  /** Reusable iterator options for `forIn` and `forOwn` */
  var forOwnIteratorOptions = {
    'arrayLoop': null
  };

  /*--------------------------------------------------------------------------*/

  /**
   * Creates a function optimized for searching large arrays for a given `value`,
   * starting at `fromIndex`, using strict equality for comparisons, i.e. `===`.
   *
   * @private
   * @param {Array} array The array to search.
   * @param {Mixed} value The value to search for.
   * @param {Number} [fromIndex=0] The index to start searching from.
   * @param {Number} [largeSize=30] The length at which an array is considered large.
   * @returns {Boolean} Returns `true` if `value` is found, else `false`.
   */
  function cachedContains(array, fromIndex, largeSize) {
    fromIndex || (fromIndex = 0);

    var length = array.length,
        isLarge = (length - fromIndex) >= (largeSize || largeArraySize),
        cache = isLarge ? {} : array;

    if (isLarge) {
      // init value cache
      var index = fromIndex - 1;
      while (++index < length) {
        // manually coerce `value` to string because `hasOwnProperty`, in some
        // older versions of Firefox, coerces objects incorrectly
        var key = array[index] + '';
        (hasOwnProperty.call(cache, key) ? cache[key] : (cache[key] = [])).push(array[index]);
      }
    }
    return function(value) {
      if (isLarge) {
        var key = value + '';
        return hasOwnProperty.call(cache, key) && indexOf(cache[key], value) > -1;
      }
      return indexOf(cache, value, fromIndex) > -1;
    }
  }

  /**
   * Used by `sortBy` to compare transformed `collection` values, stable sorting
   * them in ascending order.
   *
   * @private
   * @param {Object} a The object to compare to `b`.
   * @param {Object} b The object to compare to `a`.
   * @returns {Number} Returns the sort order indicator of `1` or `-1`.
   */
  function compareAscending(a, b) {
    var ai = a.index,
        bi = b.index;

    a = a.criteria;
    b = b.criteria;

    // ensure a stable sort in V8 and other engines
    // http://code.google.com/p/v8/issues/detail?id=90
    if (a !== b) {
      if (a > b || a === undefined) {
        return 1;
      }
      if (a < b || b === undefined) {
        return -1;
      }
    }
    return ai < bi ? -1 : 1;
  }

  /**
   * Creates a function that, when called, invokes `func` with the `this`
   * binding of `thisArg` and prepends any `partailArgs` to the arguments passed
   * to the bound function.
   *
   * @private
   * @param {Function|String} func The function to bind or the method name.
   * @param {Mixed} [thisArg] The `this` binding of `func`.
   * @param {Array} partialArgs An array of arguments to be partially applied.
   * @returns {Function} Returns the new bound function.
   */
  function createBound(func, thisArg, partialArgs) {
    var isFunc = isFunction(func),
        isPartial = !partialArgs,
        methodName = func;

    // juggle arguments
    if (isPartial) {
      partialArgs = thisArg;
    }

    function bound() {
      // `Function#bind` spec
      // http://es5.github.com/#x15.3.4.5
      var args = arguments,
          thisBinding = isPartial ? this : thisArg;

      if (!isFunc) {
        func = thisArg[methodName];
      }
      if (partialArgs.length) {
        args = args.length
          ? partialArgs.concat(slice.call(args))
          : partialArgs;
      }
      if (this instanceof bound) {
        // get `func` instance if `bound` is invoked in a `new` expression
        noop.prototype = func.prototype;
        thisBinding = new noop;

        // mimic the constructor's `return` behavior
        // http://es5.github.com/#x13.2.2
        var result = func.apply(thisBinding, args);
        return result && objectTypes[typeof result]
          ? result
          : thisBinding
      }
      return func.apply(thisBinding, args);
    }
    return bound;
  }

  /**
   * Produces an iteration callback bound to an optional `thisArg`. If `func` is
   * a property name, the callback will return the property value for a given element.
   *
   * @private
   * @param {Function|String} [func=identity|property] The function called per
   * iteration or property name to query.
   * @param {Mixed} [thisArg] The `this` binding of `callback`.
   * @returns {Function} Returns a callback function.
   */
  function createCallback(func, thisArg) {
    if (!func) {
      return identity;
    }
    if (typeof func != 'function') {
      return function(object) {
        return object[func];
      };
    }
    if (thisArg !== undefined) {
      return function(value, index, object) {
        return func.call(thisArg, value, index, object);
      };
    }
    return func;
  }

  /**
   * Creates compiled iteration functions.
   *
   * @private
   * @param {Object} [options1, options2, ...] The compile options object(s).
   *  useHas - A boolean to specify using `hasOwnProperty` checks in the object loop.
   *  args - A string of comma separated arguments the iteration function will accept.
   *  top - A string of code to execute before the iteration branches.
   *  arrayLoop - A string of code to execute in the array loop.
   *  objectLoop - A string of code to execute in the object loop.
   *  bottom - A string of code to execute after the iteration branches.
   *
   * @returns {Function} Returns the compiled function.
   */
  function createIterator() {
    var data = {
      'arrayLoop': '',
      'bottom': '',
      'hasDontEnumBug': hasDontEnumBug,
      'isKeysFast': isKeysFast,
      'objectLoop': '',
      'noArgsEnum': noArgsEnum,
      'noCharByIndex': noCharByIndex,
      'shadowed': shadowed,
      'top': '',
      'useHas': true
    };

    // merge options into a template data object
    for (var object, index = 0; object = arguments[index]; index++) {
      for (var key in object) {
        data[key] = object[key];
      }
    }
    var args = data.args;
    data.firstArg = /^[^,]+/.exec(args)[0];

    // create the function factory
    var factory = Function(
        'createCallback, hasOwnProperty, isArguments, objectTypes, nativeKeys, ' +
        'propertyIsEnumerable, stringClass, toString',
      'return function(' + args + ') {\n' + iteratorTemplate(data) + '\n}'
    );
    // return the compiled function
    return factory(
      createCallback, hasOwnProperty, isArguments, objectTypes, nativeKeys,
      propertyIsEnumerable, stringClass, toString
    );
  }

  /**
   * Used by `template` to escape characters for inclusion in compiled
   * string literals.
   *
   * @private
   * @param {String} match The matched character to escape.
   * @returns {String} Returns the escaped character.
   */
  function escapeStringChar(match) {
    return '\\' + stringEscapes[match];
  }

  /**
   * Used by `escape` to convert characters to HTML entities.
   *
   * @private
   * @param {String} match The matched character to escape.
   * @returns {String} Returns the escaped character.
   */
  function escapeHtmlChar(match) {
    return htmlEscapes[match];
  }

  /**
   * A no-operation function.
   *
   * @private
   */
  function noop() {
    // no operation performed
  }

  /**
   * Used by `unescape` to convert HTML entities to characters.
   *
   * @private
   * @param {String} match The matched character to unescape.
   * @returns {String} Returns the unescaped character.
   */
  function unescapeHtmlChar(match) {
    return htmlUnescapes[match];
  }

  /*--------------------------------------------------------------------------*/

  /**
   * Checks if `value` is an `arguments` object.
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {Mixed} value The value to check.
   * @returns {Boolean} Returns `true` if the `value` is an `arguments` object, else `false`.
   * @example
   *
   * (function() { return _.isArguments(arguments); })(1, 2, 3);
   * // => true
   *
   * _.isArguments([1, 2, 3]);
   * // => false
   */
  function isArguments(value) {
    return toString.call(value) == argsClass;
  }
  // fallback for browsers that can't detect `arguments` objects by [[Class]]
  if (noArgsClass) {
    isArguments = function(value) {
      return value ? hasOwnProperty.call(value, 'callee') : false;
    };
  }

  /**
   * Iterates over `object`'s own and inherited enumerable properties, executing
   * the `callback` for each property. The `callback` is bound to `thisArg` and
   * invoked with three arguments; (value, key, object). Callbacks may exit iteration
   * early by explicitly returning `false`.
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {Object} object The object to iterate over.
   * @param {Function} callback The function called per iteration.
   * @param {Mixed} [thisArg] The `this` binding of `callback`.
   * @returns {Object} Returns `object`.
   * @example
   *
   * function Dog(name) {
   *   this.name = name;
   * }
   *
   * Dog.prototype.bark = function() {
   *   alert('Woof, woof!');
   * };
   *
   * _.forIn(new Dog('Dagny'), function(value, key) {
   *   alert(key);
   * });
   * // => alerts 'name' and 'bark' (order is not guaranteed)
   */
  var forIn = createIterator(forEachIteratorOptions, forOwnIteratorOptions, {
    'useHas': false
  });

  /**
   * Iterates over `object`'s own enumerable properties, executing the `callback`
   * for each property. The `callback` is bound to `thisArg` and invoked with three
   * arguments; (value, key, object). Callbacks may exit iteration early by explicitly
   * returning `false`.
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {Object} object The object to iterate over.
   * @param {Function} callback The function called per iteration.
   * @param {Mixed} [thisArg] The `this` binding of `callback`.
   * @returns {Object} Returns `object`.
   * @example
   *
   * _.forOwn({ '0': 'zero', '1': 'one', 'length': 2 }, function(num, key) {
   *   alert(key);
   * });
   * // => alerts '0', '1', and 'length' (order is not guaranteed)
   */
  var forOwn = createIterator(forEachIteratorOptions, forOwnIteratorOptions);

  /**
   * A fallback implementation of `isPlainObject` that checks if a given `value`
   * is an object created by the `Object` constructor, assuming objects created
   * by the `Object` constructor have no inherited enumerable properties and that
   * there are no `Object.prototype` extensions.
   *
   * @private
   * @param {Mixed} value The value to check.
   * @returns {Boolean} Returns `true` if `value` is a plain object, else `false`.
   */
  function shimIsPlainObject(value) {
    // avoid non-objects and false positives for `arguments` objects
    var result = false;
    if (!(value && typeof value == 'object') || isArguments(value)) {
      return result;
    }
    // IE < 9 presents DOM nodes as `Object` objects except they have `toString`
    // methods that are `typeof` "string" and still can coerce nodes to strings.
    // Also check that the constructor is `Object` (i.e. `Object instanceof Object`)
    var ctor = value.constructor;
    if ((!noNodeClass || !(typeof value.toString != 'function' && typeof (value + '') == 'string')) &&
        (!isFunction(ctor) || ctor instanceof ctor)) {
      // IE < 9 iterates inherited properties before own properties. If the first
      // iterated property is an object's own property then there are no inherited
      // enumerable properties.
      if (iteratesOwnLast) {
        forIn(value, function(value, key, object) {
          result = !hasOwnProperty.call(object, key);
          return false;
        });
        return result === false;
      }
      // In most environments an object's own properties are iterated before
      // its inherited properties. If the last iterated property is an object's
      // own property then there are no inherited enumerable properties.
      forIn(value, function(value, key) {
        result = key;
      });
      return result === false || hasOwnProperty.call(value, result);
    }
    return result;
  }

  /**
   * A fallback implementation of `Object.keys` that produces an array of the
   * given object's own enumerable property names.
   *
   * @private
   * @param {Object} object The object to inspect.
   * @returns {Array} Returns a new array of property names.
   */
  function shimKeys(object) {
    var result = [];
    forOwn(object, function(value, key) {
      result.push(key);
    });
    return result;
  }

  /**
   * Used to convert characters to HTML entities:
   *
   * Though the `>` character is escaped for symmetry, characters like `>` and `/`
   * don't require escaping in HTML and have no special meaning unless they're part
   * of a tag or an unquoted attribute value.
   * http://mathiasbynens.be/notes/ambiguous-ampersands (under "semi-related fun fact")
   */
  var htmlEscapes = {
    '&': '&amp;',
    '<': '&lt;',
    '>': '&gt;',
    '"': '&quot;',
    "'": '&#x27;'
  };

  /** Used to convert HTML entities to characters */
  var htmlUnescapes = {'&amp;':'&','&lt;':'<','&gt;':'>','&quot;':'"','&#x27;':"'"};

  /*--------------------------------------------------------------------------*/

  /**
   * Creates a sorted array of all enumerable properties, own and inherited,
   * of `object` that have function values.
   *
   * @static
   * @memberOf _
   * @alias methods
   * @category Objects
   * @param {Object} object The object to inspect.
   * @returns {Array} Returns a new array of property names that have function values.
   * @example
   *
   * _.functions(_);
   * // => ['all', 'any', 'bind', 'bindAll', 'clone', 'compact', 'compose', ...]
   */
  function functions(object) {
    var result = [];
    forIn(object, function(value, key) {
      if (isFunction(value)) {
        result.push(key);
      }
    });
    return result.sort();
  }

  /**
   * Checks if `value` is an array.
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {Mixed} value The value to check.
   * @returns {Boolean} Returns `true` if the `value` is an array, else `false`.
   * @example
   *
   * (function() { return _.isArray(arguments); })();
   * // => false
   *
   * _.isArray([1, 2, 3]);
   * // => true
   */
  var isArray = nativeIsArray || function(value) {
    return toString.call(value) == arrayClass;
  };

  /**
   * Checks if `value` is a function.
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {Mixed} value The value to check.
   * @returns {Boolean} Returns `true` if the `value` is a function, else `false`.
   * @example
   *
   * _.isFunction(_);
   * // => true
   */
  function isFunction(value) {
    return typeof value == 'function';
  }
  // fallback for older versions of Chrome and Safari
  if (isFunction(/x/)) {
    isFunction = function(value) {
      return toString.call(value) == funcClass;
    };
  }

  /**
   * Creates an array composed of the own enumerable property names of `object`.
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {Object} object The object to inspect.
   * @returns {Array} Returns a new array of property names.
   * @example
   *
   * _.keys({ 'one': 1, 'two': 2, 'three': 3 });
   * // => ['one', 'two', 'three'] (order is not guaranteed)
   */
  var keys = !nativeKeys ? shimKeys : function(object) {
    var type = typeof object;

    // avoid iterating over the `prototype` property
    if (type == 'function' && propertyIsEnumerable.call(object, 'prototype')) {
      return shimKeys(object);
    }
    return object && objectTypes[type]
      ? nativeKeys(object)
      : [];
  };

  /*--------------------------------------------------------------------------*/

  /**
   * Iterates over a `collection`, executing the `callback` for each element in
   * the `collection`. The `callback` is bound to `thisArg` and invoked with three
   * arguments; (value, index|key, collection). Callbacks may exit iteration early
   * by explicitly returning `false`.
   *
   * @static
   * @memberOf _
   * @alias each
   * @category Collections
   * @param {Array|Object|String} collection The collection to iterate over.
   * @param {Function} callback The function called per iteration.
   * @param {Mixed} [thisArg] The `this` binding of `callback`.
   * @returns {Array|Object|String} Returns `collection`.
   * @example
   *
   * _([1, 2, 3]).forEach(alert).join(',');
   * // => alerts each number and returns '1,2,3'
   *
   * _.forEach({ 'one': 1, 'two': 2, 'three': 3 }, alert);
   * // => alerts each number (order is not guaranteed)
   */
  var forEach = createIterator(forEachIteratorOptions);

  /**
   * Creates an array of values by running each element in the `collection`
   * through a `callback`. The `callback` is bound to `thisArg` and invoked with
   * three arguments; (value, index|key, collection).
   *
   * @static
   * @memberOf _
   * @alias collect
   * @category Collections
   * @param {Array|Object|String} collection The collection to iterate over.
   * @param {Function} [callback=identity] The function called per iteration.
   * @param {Mixed} [thisArg] The `this` binding of `callback`.
   * @returns {Array} Returns a new array of the results of each `callback` execution.
   * @example
   *
   * _.map([1, 2, 3], function(num) { return num * 3; });
   * // => [3, 6, 9]
   *
   * _.map({ 'one': 1, 'two': 2, 'three': 3 }, function(num) { return num * 3; });
   * // => [3, 6, 9] (order is not guaranteed)
   */
  function map(collection, callback, thisArg) {
    var index = -1,
        length = collection ? collection.length : 0,
        result = Array(typeof length == 'number' ? length : 0);

    callback = createCallback(callback, thisArg);
    if (isArray(collection)) {
      while (++index < length) {
        result[index] = callback(collection[index], index, collection);
      }
    } else {
      forEach(collection, function(value, key, collection) {
        result[++index] = callback(value, key, collection);
      });
    }
    return result;
  }

  /*--------------------------------------------------------------------------*/

  /**
   * Creates an array of `array` elements not present in the other arrays
   * using strict equality for comparisons, i.e. `===`.
   *
   * @static
   * @memberOf _
   * @category Arrays
   * @param {Array} array The array to process.
   * @param {Array} [array1, array2, ...] Arrays to check.
   * @returns {Array} Returns a new array of `array` elements not present in the
   *  other arrays.
   * @example
   *
   * _.difference([1, 2, 3, 4, 5], [5, 2, 10]);
   * // => [1, 3, 4]
   */
  function difference(array) {
    var index = -1,
        length = array ? array.length : 0,
        flattened = concat.apply(arrayRef, arguments),
        contains = cachedContains(flattened, length),
        result = [];

    while (++index < length) {
      var value = array[index];
      if (!contains(value)) {
        result.push(value);
      }
    }
    return result;
  }

  /**
   * Gets the index at which the first occurrence of `value` is found using
   * strict equality for comparisons, i.e. `===`. If the `array` is already
   * sorted, passing `true` for `fromIndex` will run a faster binary search.
   *
   * @static
   * @memberOf _
   * @category Arrays
   * @param {Array} array The array to search.
   * @param {Mixed} value The value to search for.
   * @param {Boolean|Number} [fromIndex=0] The index to start searching from or
   *  `true` to perform a binary search on a sorted `array`.
   * @returns {Number} Returns the index of the matched value or `-1`.
   * @example
   *
   * _.indexOf([1, 2, 3, 1, 2, 3], 2);
   * // => 1
   *
   * _.indexOf([1, 2, 3, 1, 2, 3], 2, 3);
   * // => 4
   *
   * _.indexOf([1, 1, 2, 2, 3, 3], 2, true);
   * // => 2
   */
  function indexOf(array, value, fromIndex) {
    var index = -1,
        length = array ? array.length : 0;

    if (typeof fromIndex == 'number') {
      index = (fromIndex < 0 ? nativeMax(0, length + fromIndex) : fromIndex || 0) - 1;
    } else if (fromIndex) {
      index = sortedIndex(array, value);
      return array[index] === value ? index : -1;
    }
    while (++index < length) {
      if (array[index] === value) {
        return index;
      }
    }
    return -1;
  }

  /**
   * Uses a binary search to determine the smallest index at which the `value`
   * should be inserted into `array` in order to maintain the sort order of the
   * sorted `array`. If `callback` is passed, it will be executed for `value` and
   * each element in `array` to compute their sort ranking. The `callback` is
   * bound to `thisArg` and invoked with one argument; (value). The `callback`
   * argument may also be the name of a property to order by.
   *
   * @static
   * @memberOf _
   * @category Arrays
   * @param {Array} array The array to iterate over.
   * @param {Mixed} value The value to evaluate.
   * @param {Function|String} [callback=identity|property] The function called
   *  per iteration or property name to order by.
   * @param {Mixed} [thisArg] The `this` binding of `callback`.
   * @returns {Number} Returns the index at which the value should be inserted
   *  into `array`.
   * @example
   *
   * _.sortedIndex([20, 30, 50], 40);
   * // => 2
   *
   * _.sortedIndex([{ 'x': 20 }, { 'x': 30 }, { 'x': 50 }], { 'x': 40 }, 'x');
   * // => 2
   *
   * var dict = {
   *   'wordToNumber': { 'twenty': 20, 'thirty': 30, 'fourty': 40, 'fifty': 50 }
   * };
   *
   * _.sortedIndex(['twenty', 'thirty', 'fifty'], 'fourty', function(word) {
   *   return dict.wordToNumber[word];
   * });
   * // => 2
   *
   * _.sortedIndex(['twenty', 'thirty', 'fifty'], 'fourty', function(word) {
   *   return this.wordToNumber[word];
   * }, dict);
   * // => 2
   */
  function sortedIndex(array, value, callback, thisArg) {
    var low = 0,
        high = array ? array.length : low;

    // explicitly reference `identity` for better engine inlining
    callback = callback ? createCallback(callback, thisArg) : identity;
    value = callback(value);
    while (low < high) {
      var mid = (low + high) >>> 1;
      callback(array[mid]) < value
        ? low = mid + 1
        : high = mid;
    }
    return low;
  }

  /*--------------------------------------------------------------------------*/

  /**
   * Creates a function that, when called, invokes `func` with the `this`
   * binding of `thisArg` and prepends any additional `bind` arguments to those
   * passed to the bound function.
   *
   * @static
   * @memberOf _
   * @category Functions
   * @param {Function} func The function to bind.
   * @param {Mixed} [thisArg] The `this` binding of `func`.
   * @param {Mixed} [arg1, arg2, ...] Arguments to be partially applied.
   * @returns {Function} Returns the new bound function.
   * @example
   *
   * var func = function(greeting) {
   *   return greeting + ' ' + this.name;
   * };
   *
   * func = _.bind(func, { 'name': 'moe' }, 'hi');
   * func();
   * // => 'hi moe'
   */
  function bind(func, thisArg) {
    // use `Function#bind` if it exists and is fast
    // (in V8 `Function#bind` is slower except when partially applied)
    return isBindFast || (nativeBind && arguments.length > 2)
      ? nativeBind.call.apply(nativeBind, arguments)
      : createBound(func, thisArg, slice.call(arguments, 2));
  }

  /**
   * Binds methods on `object` to `object`, overwriting the existing method.
   * If no method names are provided, all the function properties of `object`
   * will be bound.
   *
   * @static
   * @memberOf _
   * @category Functions
   * @param {Object} object The object to bind and assign the bound methods to.
   * @param {String} [methodName1, methodName2, ...] Method names on the object to bind.
   * @returns {Object} Returns `object`.
   * @example
   *
   * var buttonView = {
   *  'label': 'lodash',
   *  'onClick': function() { alert('clicked: ' + this.label); }
   * };
   *
   * _.bindAll(buttonView);
   * jQuery('#lodash_button').on('click', buttonView.onClick);
   * // => When the button is clicked, `this.label` will have the correct value
   */
  function bindAll(object) {
    var funcs = arguments,
        index = funcs.length > 1 ? 0 : (funcs = functions(object), -1),
        length = funcs.length;

    while (++index < length) {
      var key = funcs[index];
      object[key] = bind(object[key], object);
    }
    return object;
  }

  /*--------------------------------------------------------------------------*/

  /**
   * This function returns the first argument passed to it.
   *
   * Note: It is used throughout Lo-Dash as a default callback.
   *
   * @static
   * @memberOf _
   * @category Utilities
   * @param {Mixed} value Any value.
   * @returns {Mixed} Returns `value`.
   * @example
   *
   * var moe = { 'name': 'moe' };
   * moe === _.identity(moe);
   * // => true
   */
  function identity(value) {
    return value;
  }

  /**
   * Reverts the '_' variable to its previous value and returns a reference to
   * the `lodash` function.
   *
   * @static
   * @memberOf _
   * @category Utilities
   * @returns {Function} Returns the `lodash` function.
   * @example
   *
   * var lodash = _.noConflict();
   */
  function noConflict() {
    window._ = oldDash;
    return this;
  }

  /*--------------------------------------------------------------------------*/

  /**
   * Enables method chaining on the wrapper object.
   *
   * @name chain
   * @deprecated
   * @memberOf _
   * @category Chaining
   * @returns {Mixed} Returns the wrapper object.
   * @example
   *
   * _([1, 2, 3]).value();
   * // => [1, 2, 3]
   */
  function wrapperChain() {
    this.__chain__ = true;
    return this;
  }

  /**
   * Extracts the wrapped value.
   *
   * @name value
   * @memberOf _
   * @category Chaining
   * @returns {Mixed} Returns the wrapped value.
   * @example
   *
   * _([1, 2, 3]).value();
   * // => [1, 2, 3]
   */
  function wrapperValue() {
    return this.__wrapped__;
  }

  /*--------------------------------------------------------------------------*/

  /**
   * The semantic version number.
   *
   * @static
   * @memberOf _
   * @type String
   */
  lodash.VERSION = '0.9.1';
  lodash.bind = bind;
  lodash.bindAll = bindAll;
  lodash.difference = difference;
  lodash.forEach = forEach;
  lodash.forIn = forIn;
  lodash.forOwn = forOwn;
  lodash.functions = functions;
  lodash.identity = identity;
  lodash.indexOf = indexOf;
  lodash.isArguments = isArguments;
  lodash.isArray = isArray;
  lodash.isFunction = isFunction;
  lodash.keys = keys;
  lodash.map = map;
  lodash.noConflict = noConflict;
  lodash.sortedIndex = sortedIndex;
  lodash.collect = map;
  lodash.each = forEach;
  lodash.methods = functions;

  /*--------------------------------------------------------------------------*/

  // expose Lo-Dash
  // some AMD build optimizers, like r.js, check for specific condition patterns like the following:
  if (typeof define == 'function' && typeof define.amd == 'object' && define.amd) {
    // Expose Lo-Dash to the global object even when an AMD loader is present in
    // case Lo-Dash was injected by a third-party script and not intended to be
    // loaded as a module. The global assignment can be reverted in the Lo-Dash
    // module via its `noConflict()` method.
    window._ = lodash;

    // define as an anonymous module so, through path mapping, it can be
    // referenced as the "underscore" module
    define(function() {
      return lodash;
    });
  }
  // check for `exports` after `define` in case a build optimizer adds an `exports` object
  else if (freeExports) {
    // in Node.js or RingoJS v0.8.0+
    if (typeof module == 'object' && module && module.exports == freeExports) {
      (module.exports = lodash)._ = lodash;
    }
    // in Narwhal or RingoJS v0.7.0-
    else {
      freeExports._ = lodash;
    }
  }
  else {
    // in a browser or Rhino
    window._ = lodash;
  }
}(this));

(function(namespace) {
  /**
  * Boilerplate to make Caress compatible with AMD,
  * CommonJS 'require' (NodeJS) and vanilla JavaScript.
  *
  * Courtesy of David Luecke @daffl
  * http://daffl.github.com/2012/02/22/javascript-modules.html
  */
  if(typeof define == "undefined") {
    define = function(fn) {
      var myModule = fn();
      if (typeof exports == "undefined") {
        window[namespace] = myModule; //Vanilla JS
      }
      else {
        module.exports = myModule; //NodeJS
      }
    };
  }

  define(function(require, exports, module) {

    var root = root || window;
    var _ = root._.noConflict();

    if (!_ && (typeof require !== "undefined")) {
        _ = require('underscore');
    }

    /**
     * Caress namespace.
     *
     * @namespace
     */

    var Caress = exports || {};

    /**
     * Caress version
     *
     * @api public
     */

    Caress.version = '0.1.0';

    /**
     * TUIO Protocol implemented.
     *
     * @api public
     */

    Caress.protocol = '1.1';

    /**
     * Expose any NodeJS specific stuff
     */
    if ('object' === typeof module && 'function' === typeof require) {

      /**
       * Expose utils
       *
       * @api private
       */
    }

    /**
     * The Caress Client Object
     */
    var Client = Caress.Client = function Client(options) {
      options = options || {};
      this.host = options.host ||'127.0.0.1';
      this.port = options.port || 5000;
      this.connected = false;
      this.sessions = {}; // {id: pointerToObjectInList}
      this.cursors = {};
      this.objects = {};
      this.blobs = {};
      this.touches = {};
      this.touchList = document.createTouchList();

      // _.bindAll(this, 'connect', 'onConnect', 'onDisconnect', 'processPacket', 'processMessage', 'process2dCursorMessage', 'source2dCursor', 'alive2dCursor', 'set2dCursor');
      _.bindAll(this, 'connect', 'onConnect', 'onDisconnect', 'processPacket', 'processMessage', 'processCursorSource', 'processObjectSource', 'processBlobSource',
        'processCursorAlive', 'processObjectAlive', 'processBlobAlive', 'processCursorSet', 'processObjectSet', 'processBlobSet', 'processFseq');
    };

    Client.prototype.connect = function(){
      this.socket = io.connect('http://' + this.host + ':' + this.port);
      this.socket.on("connect", this.onConnect);
      this.socket.on("disconnect", this.onDisconnect);
    };

    Client.prototype.onConnect = function(){
      this.connected = true;

      this.socket.on('tuio', this.processPacket);
      // this.trigger("connect");
      console.log('Connected to Socket.io');
    };

    Client.prototype.onDisconnect = function(){
      this.connected = false;

      // We disconnected from the server so we emit touch cancel
      // events for each touch point still remaining.
      for (var namespace in this.touches) {
        for (var touch in this.touches[namespace]) {
          var cancelledTouch = this.touches[namespace][touch];
          delete this.touches[namespace][touch];

          this.createTouchEvent('touchcancel', cancelledTouch);
        }
      }

      // Clean up all the TUIO and touch lists
      this.touches = {};
      this.cursors = {};
      this.objects = {};
      this.blobs = {};

      // this.trigger("disconnect");
      console.log('Disconnected from Socket.io');
    };

    Client.prototype.processPacket = function(packet){
      if (packet.bundle){
        this.processMessage(packet);
      }
      else {
        // It's a regular message and not a bundle
        // TODO: Figure out what to do. Haven't seen one of these yet
      }
    };

    Client.prototype.processMessage = function(packet){
      var cursorTypes = {
        'source': this.processCursorSource,
        'alive': this.processCursorAlive,
        'set': this.processCursorSet,
        'fseq': this.processFseq
      };

      var objectTypes = {
        'source': this.processObjectSource,
        'alive': this.processObjectAlive,
        'set': this.processObjectSet,
        'fseq': this.processFseq
      };

      var blobTypes = {
        'source': this.processBlobSource,
        'alive': this.processBlobAlive,
        'set': this.processBlobSet,
        'fseq': this.processFseq
      };

      // console.log('PACKET', packet);

      // Ignore duplicate packets for now
      if (!packet.duplicate){

        // Default all the sources to localhost, assuming that if
        // we don't have an address then it is from localhost. Maybe
        // this is a bad assumption to make. We override this if
        // a source was actually provided!
        packet.source = 'localhost';

        for (var message in packet.messages) {
          var key = packet.messages[message].type;

          switch (packet.messages[message].profile) {
              case "/tuio/2Dcur":
              case "/tuio/25Dcur":
              case "/tuio/3Dcur":
                cursorTypes[key](packet, packet.messages[message]);
                break;
              case "/tuio/2Dobj":
              case "/tuio/25Dobj":
              case "/tuio/3Dobj":
                objectTypes[key](packet, packet.messages[message]);
                break;
              case "/tuio/2Dblb":
              case "/tuio/25Dblb":
              case "/tuio/3Dblb":
                blobTypes[key](packet, packet.messages[message]);
                break;
          }
        }
      }
    };

    Client.prototype.processCursorSource = function(packet, message){
      packet.source = message.address;
      if (this.cursors[packet.source] === undefined){
        this.cursors[packet.source] = {};
      }

      if (this.touches[packet.source] === undefined){
        this.touches[packet.source] = {};
      }
    };

    Client.prototype.processObjectSource = function(packet, message){
      packet.source = message.address;
      if (this.objects[packet.source] === undefined){
        this.objects[packet.source] = {};
      }

      if (this.touches[packet.source] === undefined){
        this.touches[packet.source] = {};
      }
    };

    Client.prototype.processBlobSource = function(packet, message){
      packet.source = message.address;
      if (this.blobs[packet.source] === undefined){
        this.blobs[packet.source] = {};
      }

      if (this.touches[packet.source] === undefined){
        this.touches[packet.source] = {};
      }
    };

    Client.prototype.processCursorAlive = function(packet, message){
      // Setup multiplexing namespacing if it doesn't already exist.
      // Also needs to be done in here because sometimes you don't get source
      // messages.
      if (this.cursors[packet.source] === undefined){
        this.cursors[packet.source] = {};
      }

      if (this.touches[packet.source] === undefined){
        this.touches[packet.source] = {};
      }

      // Remove the non-active cursors from the cursor namespace
      var activeCursors = _.map(message.sessionIds, function(id){ return id.toString(); });
      var notActiveCursors = _.difference(_.keys(this.cursors[packet.source]), activeCursors);

      for (var i = 0; i < notActiveCursors.length; i++){
        var key = notActiveCursors[i];
        var touch = this.touches[packet.source][key];

        if (touch !== undefined){
          delete this.touches[packet.source][key];
          delete this.cursors[packet.source][key];
          this.createTouchEvent('touchend', touch);
        }
      }
    };

    Client.prototype.processObjectAlive = function(packet, message){
      // Setup multiplexing namespacing if it doesn't already exist.
      // Also needs to be done in here because sometimes you don't get source
      // messages.
      if (this.objects[packet.source] === undefined){
        this.objects[packet.source] = {};
      }

      if (this.touches[packet.source] === undefined){
        this.touches[packet.source] = {};
      }

      // Remove the non-active objects from the object namespace
      var activeObjects = _.map(message.sessionIds, function(id){ return id.toString(); });
      var notActiveObjects = _.difference(_.keys(this.objects[packet.source]), activeObjects);

      for (var i = 0; i < notActiveObjects.length; i++){
        var key = notActiveObjects[i];
        var touch = this.touches[packet.source][key];

        if (touch !== undefined){
          delete this.touches[packet.source][key];
          delete this.objects[packet.source][key];
          this.createTouchEvent('touchend', touch);
        }
      }
    };

    Client.prototype.processBlobAlive = function(packet, message){
      // Setup multiplexing namespacing if it doesn't already exist.
      // Also needs to be done in here because sometimes you don't get source
      // messages.
      if (this.blobs[packet.source] === undefined){
        this.blobs[packet.source] = {};
      }

      if (this.touches[packet.source] === undefined){
        this.touches[packet.source] = {};
      }

      // Remove the non-active blobs from the blob namespace
      var activeBlobs = _.map(message.sessionIds, function(id){ return id.toString(); });
      var notActiveBlobs = _.difference(_.keys(this.blobs[packet.source]), activeBlobs);

      for (var i = 0; i < notActiveBlobs.length; i++){
        var key = notActiveBlobs[i];
        var touch = this.touches[packet.source][key];

        if (touch !== undefined){
          delete this.touches[packet.source][key];
          delete this.blobs[packet.source][key];
          this.createTouchEvent('touchend', touch);
        }
      }
    };

    Client.prototype.processCursorSet = function(packet, message){
      var cursor = new TuioCursor(message);
      var touch = cursor.coherceToTouch();
      var id = message.sessionId.toString();

      if (this.cursors[packet.source][id] !== undefined && this.cursors[packet.source][id].sessionId.toString() === id){

        // Existing cursor so we update it
        this.cursors[packet.source][id] = cursor;

        // Find existing touch in touches hash, replace it with the
        // updated touch and then create a 'touchmove' event
        if (this.touches[packet.source][id] !== undefined && this.touches[packet.source][id].identifier.toString() === id){
          this.touches[packet.source][id] = touch;
          this.createTouchEvent('touchmove', touch);
          // console.log('UPDATE', this.cursors, this.touches);

          return;
        }

        // Shouldn't really get here unless somebody removed the touch from
        // the touches hash without removing the cursor from cursors as well.
        return;
      }

      // New cursor
      this.cursors[packet.source][id] = cursor;
      this.touches[packet.source][id] = touch;

      this.createTouchEvent('touchstart', touch);
      // console.log('SET', this.cursors[packet.source], this.touches[packet.source]);
    };

    Client.prototype.processObjectSet = function(packet, message){
      var tuioObject;
      var touch;
      var id = message.sessionId.toString();

      if (this.objects[packet.source][id] !== undefined && this.objects[packet.source][id].sessionId.toString() === id){

        // Existing object so we update it
        this.objects[packet.source][id] = new TuioObject(message);

        // Find existing touch in touches hash, replace it with the
        // updated touch and then create a 'touchmove' event
        if (this.touches[packet.source][id] !== undefined && this.touches[packet.source][id].identifier.toString() === id){
          touch = this.objects[packet.source][id].coherceToTouch();
          this.touches[packet.source][id] = touch;
          this.createTouchEvent('touchmove', touch);
          // console.log('UPDATE', this.objects, this.touches);

          return;
        }

        // Shouldn't really get here unless somebody removed the touch from
        // the touches hash without removing the object from objects as well.
        return;
      }

      // New TUIO object
      tuioObject = new TuioObject(message);
      touch = tuioObject.coherceToTouch();

      this.objects[packet.source][id] = tuioObject;
      this.touches[packet.source][id] = touch;

      this.createTouchEvent('touchstart', touch);
      // console.log('SET', this.objects[packet.source], this.touches[packet.source]);
    };

    Client.prototype.processBlobSet = function(packet, message){
      var blob;
      var touch;
      var id = message.sessionId.toString();

      if (this.blobs[packet.source][id] !== undefined && this.blobs[packet.source][id].sessionId.toString() === id){

        // Existing blob so we update it
        this.blobs[packet.source][id] = new TuioBlob(message);

        // Find existing touch in touches hash, replace it with the
        // updated touch and then create a 'touchmove' event
        if (this.touches[packet.source][id] !== undefined && this.touches[packet.source][id].identifier.toString() === id){
          touch = this.blobs[packet.source][id].coherceToTouch();
          this.touches[packet.source][id] = touch;
          this.createTouchEvent('touchmove', touch);
          // console.log('UPDATE', this.blobs, this.touches);

          return;
        }

        // Shouldn't really get here unless somebody removed the touch from
        // the touches hash without removing the blob from blobs as well.
        return;
      }

      // New blob
      blob = new TuioBlob(message);
      touch = blob.coherceToTouch();

      this.blobs[packet.source][id] = blob;
      this.touches[packet.source][id] = touch;

      this.createTouchEvent('touchstart', touch);
      // console.log('SET', this.blobs[packet.source], this.touches[packet.source]);
    };

    Client.prototype.processFseq = function(packet, message){
      // TODO: Figure out what to do with fseq messages.
    };

    Client.prototype.getCursor = function(sessionID){
      return this.cursors[sessionId];
    };

    Client.prototype.getObject = function(sessionId){
      return this.objects[sessionId];
    };

    Client.prototype.getBlob = function(sessionId){
      return this.blobs[sessionId];
    };

    Client.prototype.getCursors = function(){
      return this.cursors;
    };

    Client.prototype.getObjects = function(){
      return this.objects;
    };

    Client.prototype.getBlobs = function(){
      return this.blobs;
    };

    // Create our custom TouchEvent
    Client.prototype.createTouchEvent = function(type, touch){

      // Get all currently active touches so they can be attached
      // to the touchEvent
      var touches = [];

      // Convert touches hash to array because that's what W3C says
      // it should be.
      // TODO: Find a better way! This is super efficient, NOT!
      for (var namespace in this.touches){
        for (var key in this.touches[namespace]){
          touches.push(this.touches[namespace][key]);
        }
      }

      // Get the touches that started on the attribute so they can
      // be attached to the touchEvent
      var targetTouches = touch.getTargetTouches();

      // Get the touches that contributed to the event so they can
      // be attached to the touchEvent
      var changedTouches = document.createTouchList([touch]);

      // This is used in place of document.createEvent('TouchEvent');
      // because almost all browsers except for Firefox at the moment
      // do not support it.
      var touchEvent = document.createEvent('UIEvent');

      switch(type){
        // Init as canBubble and is cancelable
        case 'touchstart':
        case 'touchend':
        case 'touchmove':
          touchEvent.initUIEvent(type || "", true, true, touch.view || null, 0);
          break;
        // Init as not cancelable
        case 'touchcancel':
          touchEvent.initUIEvent(type || "", true, false, touch.view || null, 0);
          break;
        // Init as cannot bubble
        case 'touchenter':
        case 'touchleave':
          touchEvent.initUIEvent(type || "", false, true, touch.view || null, 0);
          break;
      }

      touchEvent.initTouchEvent(touches, targetTouches, changedTouches, type, window, touch.screenX, touch.screenY, touch.clientX, touch.clientY, touchEvent.ctrlKey, touchEvent.altKey, touchEvent.shiftKey, touchEvent.metaKey);

      // Dispatch the event
      if (touch.target) {
        touch.target.dispatchEvent(touchEvent);
      }
      else {
        var val = document.dispatchEvent(touchEvent);
      }
    };

    /**
     * A TUIO Cursor Object
     */
    var TuioCursor = Caress.TuioCursor = function TuioCursor(options){
      for (var key in options){
        this[key] = options[key];
      }
    };

    TuioCursor.prototype.coherceToTouch = function() {
      var identifier = this.sessionId;

      //TODO: Verify? I think these are correct but not 100% sure
      var clientX = window.innerWidth * this.xPosition;
      var clientY = window.innerHeight * this.yPosition;
      var pageX = document.documentElement.clientWidth * this.xPosition;
      var pageY = document.documentElement.clientHeight * this.yPosition;
      var target = document.elementFromPoint(pageX, pageY);
      var screenX = screen.width * this.xPosition;
      var screenY = screen.height * this.yPosition;
      var radiusX = this.radius;
      var radiusY = this.radius;
      var rotationAngle = this.rotationAngle;
      var force = this.force;

      return document.createTouch(target, identifier, clientX, clientY, pageX, pageY, screenX, screenY, radiusX, radiusY, rotationAngle, force);
    };

    /**
     * A TUIO Object Object (an Object Object? whaaat?)
     */
    var TuioObject = Caress.TuioObject = function TuioObject(options){
      for (var key in options){
        this[key] = options[key];
      }
    };

    TuioObject.prototype.coherceToTouch = function() {
      var identifier = this.sessionId;

      var clientX = window.innerWidth * this.xPosition;
      var clientY = window.innerHeight * this.yPosition;
      var pageX = document.documentElement.clientWidth * this.xPosition;
      var pageY = document.documentElement.clientHeight * this.yPosition;
      var target = document.elementFromPoint(pageX, pageY);
      var screenX = screen.width * this.xPosition;
      var screenY = screen.height * this.yPosition;
      var radiusX = this.radius;
      var radiusY = this.radius;
      var rotationAngle = this.rotationAngle;
      var force = this.force;

      return document.createTouch(target, identifier, clientX, clientY, pageX, pageY, screenX, screenY, radiusX, radiusY, rotationAngle, force);
    };

    /**
     * A TUIO Blob Object
     */
    var TuioBlob = Caress.TuioBlob = function TuioBlob(options){
      for (var key in options){
        this[key] = options[key];
      }
    };

    TuioBlob.prototype.coherceToTouch = function() {
      var identifier = this.sessionId;

      //TODO: Verify? I think these are correct but not 100% sure
      var clientX = window.innerWidth * this.xPosition;
      var clientY = window.innerHeight * this.yPosition;
      var pageX = document.documentElement.clientWidth * this.xPosition;
      var pageY = document.documentElement.clientHeight * this.yPosition;
      var target = document.elementFromPoint(pageX, pageY);
      var screenX = screen.width * this.xPosition;
      var screenY = screen.height * this.yPosition;
      var radiusX = this.radius;
      var radiusY = this.radius;
      var rotationAngle = this.rotationAngle;
      var force = this.force;

      return document.createTouch(target, identifier, clientX, clientY, pageX, pageY, screenX, screenY, radiusX, radiusY, rotationAngle, force);
    };

    /**
     * A W3C Touch Object
     * http://dvcs.w3.org/hg/webevents/raw-file/tip/touchevents.html#touch-interface
     */
    var Touch = Caress.Touch = function Touch(target, identifier, clientX, clientY, pageX, pageY, screenX, screenY, radiusX, radiusY, rotationAngle, force){
      // TODO: Still need to type check the input parameters
      // if ( view === null || view === undefined || target === null || target === undefined || identifier === null || identifier === undefined || pageX === null || pageX === undefined || pageY === null || pageY === undefined || screenX === null || screenX === undefined || screenY === null || screenY === undefined){
      //   return undefined;
      // }

      this.identifier = identifier;
      this.target = target;
      this.screenX = screenX;
      this.screenY = screenY;
      this.clientX = clientX;
      this.clientY = clientY;
      this.pageX = pageX;
      this.pageY = pageY;
      this.radiusX = radiusX;
      this.radiusY = radiusY;
      this.rotationAngle = rotationAngle;
      this.force = force;

      return this;
    };

    Touch.prototype.getTargetTouches = function() {
      var targetTouches = document.createTouchList();

      for (var namespace in window.client.touches) {
        for (var key in window.client.touches[namespace]){
          var touch = window.client.touches[namespace][key];

          if (touch.target == this.target) {
            targetTouches.push(touch);
          }
        }
      }

      return targetTouches;
    };

    /**
     * A W3C TouchList Object
     * http://dvcs.w3.org/hg/webevents/raw-file/tip/touchevents.html#touchlist-interface
     */
    var TouchList = Caress.TouchList = function TouchList(){
      // TODO: Maybe should check to make sure that only Touch objects are being passed in
      var args = [].slice.call( arguments );
      var length = args.length;
      var i = 0;

      this.length = length;

      for ( ; i < length; i++ ) {
          this[ i ] = args[ i ];
      }

      return this;
    };

    TouchList.prototype = [];

    TouchList.prototype.identifiedTouch = function(identifier){

      for (var i = 0; i < this.length; i++){
        if (this[i].identifier === identifier) return this[i];
      }

      return undefined;
    };

    TouchList.prototype.item = function(index){
      return this[index];
    };


    /**
     * A W3C TouchEvent Object
     * http://dvcs.w3.org/hg/webevents/raw-file/tip/touchevents.html#touchevent-interface
     */
    var TouchEvent = Caress.TouchEvent = function TouchEvent(){
      this.touches = document.createTouchList(); // a TouchList
      this.targetTouches = document.createTouchList(); // a TouchList
      this.changedTouches = document.createTouchList(); // a TouchList
      this.altKey = false;
      this.metaKey = false;
      this.ctrlKey = false;
      this.shiftKey = false;
      this.relatedTarget = null; // an EventTarget - used for touchenter and touchleave events
    };

    // TouchEvent.prototype = document.createEvent('UIEvent');
    TouchEvent.prototype = UIEvent.prototype;

    TouchEvent.prototype.initTouchEvent = function(touches, targetTouches, changedTouches, type, view, screenX, screenY, clientX, clientY, ctrlKey, altKey, shiftKey, metaKey) {
      this.touches = touches || document.createTouchList(); // a TouchList
      this.targetTouches = targetTouches || document.createTouchList(); // a TouchList
      this.changedTouches = changedTouches || document.createTouchList(); // a TouchList
      this.screenX = screenX || null;
      this.screenY = screenY || null;
      this.clientX = clientX || null;
      this.clientY = clientY || null;
      this.altKey = altKey || false;
      this.metaKey = metaKey || false;
      this.ctrlKey = ctrlKey || false;
      this.shiftKey = shiftKey || false;
      this.relatedTarget = null; // an EventTarget - used for touchenter and touchleave events

      // TODO: Look at maybe adding isTrusted = true;
    };

    TouchEvent.prototype.isTouchEvent = function() {
      return true;
    };


    /**********************************************************************************************
     * DOM FUNCTIONS
     **********************************************************************************************/

    /**
     * A W3C Document Interface Extensions
     * http://dvcs.w3.org/hg/webevents/raw-file/tip/touchevents.html#extensions-to-the-document-interface
     */

    document.createTouch = function(view, target, identifier, pageX, pageY, screenX, screenY, radiusX, radiusY, rotationAngle, force){
      return new Touch(view, target, identifier, pageX, pageY, screenX, screenY, radiusX, radiusY, rotationAngle, force);
    };

    document.createTouchList = function(touches){
      // TODO: Maybe should check to make sure that only Touch objects are being passed in
      var touchList = new TouchList();

      // return TouchList with multiple touches
      if (_.isArray(touches) && touches.length){
        for (var i = 0; i < touches.length; i++){
          touchList.push(touches[i]);
        }

        return touchList;
      }
      // return TouchList with a single touch
      else if (!_.isArray(touches) && touches !== undefined) {
        touchList.push(touches);
        return touchList;
      }

      // return an empty TouchList
      return touchList;
    };

    /*
    * Commence Dirty Hacks for typical touch detection
    *
    * You SHOULD NOT use ontouchstart, ontouchend, ontouchmove to bind for touch events!
    * That shit is attributed to old balls DOM 2 spec anyway. Instead use:
    *
    *    addEventListener( 'touchstart', function(evt){...})
    *
    *    or
    *
    *    jQuery's $.on('touchstart', function) or $.bind('touchstart', function)
    *
    * TODO: Figure out how to actually implement this is JavaScript. Not sure how these
    * functions get called when a 'touchstart' event is dispatched. Might only be possible
    * to do this in native browser code. Plus it is old DOM 2 spec so maybe I shouldn't bother,
    * unless I want to support old shitty browsers (ie5, ie6, ie7, ie8).
    *
    * Maybe look into:
    * http://www.w3.org/TR/DOM-Level-2-Events/
    * http://www.w3.org/TR/DOM-Level-3-Events/
    */

    window.ontouchstart = document.ontouchstart = null;
    window.ontouchend = document.ontouchend = null;
    window.ontouchmove = document.ontouchmove = null;
    window.ontouchcancel = document.ontouchcancel = null;
    window.ontouchenter = document.ontouchenter = null;
    window.ontouchleave = document.ontouchleave = null;


    // Return the Caress module API
    return Caress;
  });
})('Caress');
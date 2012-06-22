/* -*- Mode: js; js-indent-level: 4; -*- */
/*
  Copyright (C) 2012 Stephen Crane <culda.rinon@gmail.com>
  Copyright (C) 2012 John Freeman <jfreeman08@gmail.com>
  Copyright (C) 2012 Ariya Hidayat <ariya.hidayat@gmail.com>
  Copyright (C) 2012 Mathias Bynens <mathias@qiwi.be>
  Copyright (C) 2012 Joost-Wim Boekesteijn <joost-wim@boekesteijn.nl>
  Copyright (C) 2012 Kris Kowal <kris.kowal@cixar.com>
  Copyright (C) 2012 Yusuke Suzuki <utatane.tea@gmail.com>
  Copyright (C) 2012 Arpad Borsos <arpad.borsos@googlemail.com>
  Copyright (C) 2011 Ariya Hidayat <ariya.hidayat@gmail.com>

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*jslint bitwise:true */
/*global escodegen:true, exports:true, generateStatement: true*/

(function (exports) {
    'use strict';

    var Syntax,
        Precedence,
        BinaryPrecedence,
        VisitorKeys,
        VisitorOption,
        isArray,
        base,
        indent,
        extra,
        parse,
        Result;

    Syntax = {
        AssignmentExpression: 'AssignmentExpression',
        ArrayExpression: 'ArrayExpression',
        BlockStatement: 'BlockStatement',
        BinaryExpression: 'BinaryExpression',
        BreakStatement: 'BreakStatement',
        CallExpression: 'CallExpression',
        CatchClause: 'CatchClause',
        ConditionalExpression: 'ConditionalExpression',
        ContinueStatement: 'ContinueStatement',
        DoWhileStatement: 'DoWhileStatement',
        DebuggerStatement: 'DebuggerStatement',
        EmptyStatement: 'EmptyStatement',
        ExpressionStatement: 'ExpressionStatement',
        ForStatement: 'ForStatement',
        ForInStatement: 'ForInStatement',
        FunctionDeclaration: 'FunctionDeclaration',
        FunctionExpression: 'FunctionExpression',
        Identifier: 'Identifier',
        IfStatement: 'IfStatement',
        Literal: 'Literal',
        LabeledStatement: 'LabeledStatement',
        LogicalExpression: 'LogicalExpression',
        MemberExpression: 'MemberExpression',
        NewExpression: 'NewExpression',
        ObjectExpression: 'ObjectExpression',
        Program: 'Program',
        Property: 'Property',
        ReturnStatement: 'ReturnStatement',
        SequenceExpression: 'SequenceExpression',
        SwitchStatement: 'SwitchStatement',
        SwitchCase: 'SwitchCase',
        ThisExpression: 'ThisExpression',
        ThrowStatement: 'ThrowStatement',
        TryStatement: 'TryStatement',
        UnaryExpression: 'UnaryExpression',
        UpdateExpression: 'UpdateExpression',
        VariableDeclaration: 'VariableDeclaration',
        VariableDeclarator: 'VariableDeclarator',
        WhileStatement: 'WhileStatement',
        WithStatement: 'WithStatement'
    };

    Precedence = {
        Sequence: 0,
        Assignment: 1,
        Conditional: 2,
        LogicalOR: 3,
        LogicalAND: 4,
        BitwiseOR: 5,
        BitwiseXOR: 6,
        BitwiseAND: 7,
        Equality: 8,
        Relational: 9,
        BitwiseSHIFT: 10,
        Additive: 11,
        Multiplicative: 12,
        Unary: 13,
        Postfix: 14,
        Call: 15,
        New: 16,
        Member: 17,
        Primary: 18
    };

    BinaryPrecedence = {
        '||': Precedence.LogicalOR,
        '&&': Precedence.LogicalAND,
        '|': Precedence.BitwiseOR,
        '^': Precedence.BitwiseXOR,
        '&': Precedence.BitwiseAND,
        '==': Precedence.Equality,
        '!=': Precedence.Equality,
        '===': Precedence.Equality,
        '!==': Precedence.Equality,
        '<': Precedence.Relational,
        '>': Precedence.Relational,
        '<=': Precedence.Relational,
        '>=': Precedence.Relational,
        'in': Precedence.Relational,
        'instanceof': Precedence.Relational,
        '<<': Precedence.BitwiseSHIFT,
        '>>': Precedence.BitwiseSHIFT,
        '>>>': Precedence.BitwiseSHIFT,
        '+': Precedence.Additive,
        '-': Precedence.Additive,
        '*': Precedence.Multiplicative,
        '%': Precedence.Multiplicative,
        '/': Precedence.Multiplicative
    };

    function getDefaultOptions() {
        // default options
        return {
            indent: null,
            base: null,
            parse: null,
            comment: false,
            format: {
                indent: {
                    style: '    ',
                    base: 0,
                    adjustMultilineComment: false
                }
            },
            sourceFile: '',
            sourceMap: null
        };
    }

    function unicodeEscape(ch) {
        var result = ch.charCodeAt(0).toString(16);
        return '\\u' + "0000".slice(result.length) + result;
    }

    function stringToArray(str) {
        var length = str.length,
            result = [],
            i;
        for (i = 0; i < length; i += 1) {
            result[i] = str.charAt(i);
        }
        return result;
    }

    function stringRepeat(str, num) {
        var result = '';

        for (num |= 0; num > 0; num >>>= 1, str += str) {
            if (num & 1) {
                result += str;
            }
        }

        return result;
    }

    isArray = Array.isArray;
    if (!isArray) {
        isArray = function isArray(array) {
            return Object.prototype.toString.call(array) === '[object Array]';
        };
    }

    function endsWithLineTerminator(value) {
        return (/(?:\r\n|[\n\r])$/).test(value);
    }

    function shallowCopy(obj) {
        var ret = {}, key;
        for (key in obj) {
            if (obj.hasOwnProperty(key)) {
                ret[key] = obj[key];
            }
        }
        return ret;
    }

    function deepCopy(obj) {
        var ret = {}, key, val;
        for (key in obj) {
            if (obj.hasOwnProperty(key)) {
                val = obj[key];
                if (typeof val === 'object' && val !== null) {
                    ret[key] = deepCopy(val);
                } else {
                    ret[key] = val;
                }
            }
        }
        return ret;
    }

    function updateDeeply(target, override) {
        var key, val;

        function isHashObject(target) {
            return typeof target === 'object' && target instanceof Object && !(target instanceof RegExp);
        }

        for (key in override) {
            if (override.hasOwnProperty(key)) {
                val = override[key];
                if (isHashObject(val)) {
                    if (isHashObject(target[key])) {
                        updateDeeply(target[key], val);
                    } else {
                        target[key] = updateDeeply({}, val);
                    }
                } else {
                    target[key] = val;
                }
            }
        }
        return target;
    }

    function escapeString(str) {
        var result = '', i, len, ch;

        if (typeof str[0] === 'undefined') {
            str = stringToArray(str);
        }

        for (i = 0, len = str.length; i < len; i += 1) {
            ch = str[i];
            if ('\'\\\b\f\n\r\t'.indexOf(ch) >= 0) {
                result += '\\';
                switch (ch) {
                case '\'':
                    result += '\'';
                    break;
                case '\\':
                    result += '\\';
                    break;
                case '\b':
                    result += 'b';
                    break;
                case '\f':
                    result += 'f';
                    break;
                case '\n':
                    result += 'n';
                    break;
                case '\r':
                    result += 'r';
                    break;
                case '\t':
                    result += 't';
                    break;
                }
            } else if (ch < ' ' || ch.charCodeAt(0) >= 0x80) {
                result += unicodeEscape(ch);
            } else {
                result += ch;
            }
        }

        return '\'' + result + '\'';
    }

    function isWhiteSpace(ch) {
        return (ch === ' ') || (ch === '\u0009') || (ch === '\u000B') ||
            (ch === '\u000C') || (ch === '\u00A0') ||
            (ch.charCodeAt(0) >= 0x1680 &&
             '\u1680\u180E\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000\uFEFF'.indexOf(ch) >= 0);
    }

    function addIndent(stmt) {
        if (typeof stmt === 'string') {
            return base + stmt;
        } else if (extra.sourceMap) {
            stmt.prepend(base);
            return stmt;
        } else {
            stmt.string = base + stmt.string;
            return stmt;
        }
    }

    function adjustMultilineComment(value) {
        var array, i, len, line, j, ch, spaces;

        spaces = Number.MAX_VALUE;
        array = value.split(/\r\n|[\r\n]/);

        // first line doesn't have indentation
        for (i = 1, len = array.length; i < len; i += 1) {
            line = array[i];
            j = 0;
            while (j < line.length && isWhiteSpace(line[j])) {
                j += 1;
            }
            if (spaces > j) {
                spaces = j;
            }
        }

        if (spaces % 2 === 1) {
            // /*
            //  *
            //  */
            // If spaces are odd number, above pattern is considered.
            // We waste 1 space.
            spaces -= 1;
        }
        for (i = 1, len = array.length; i < len; i += 1) {
            array[i] = addIndent(array[i].slice(spaces));
        }
        return array.join('\n');
    }

    function generateComment(comment) {
        if (comment.type === 'Line') {
            // Esprima always produce last line comment with LineTerminator
            return '//' + comment.value;
        }
        if (extra.format.indent.adjustMultilineComment && /[\n\r]/.test(comment.value)) {
            return adjustMultilineComment('/*' + comment.value + '*/');
        }
        return '/*' + comment.value + '*/';
    }

    function parenthesize(text, current, should) {
        if (current < should) {
            if (typeof text === 'string') {
                return '(' + text + ')';
            } else if (extra.sourceMap) {
                text.prepend('(');
                text.add(')');
                return text;
            } else {
                text.string = '(' + text.string + ')';
                return text;
            }
        }
        return text;
    }

    function maybeBlock(stmt, suffix) {
        var previousBase, result;

        result = new Result(stmt);

        if (stmt.type === Syntax.BlockStatement && (!extra.comment || !stmt.leadingComments)) {
            result.add(' ');
            result.add(generateStatement(stmt));
            if (suffix) {
                result.add(' ');
            }
            return result.valueOf();
        }

        if (stmt.type === Syntax.EmptyStatement && (!extra.comment || !stmt.leadingComments)) {
            result.add(';');
        } else {
            previousBase = base;
            base += indent;
            result.add('\n');
            result.add(addIndent(generateStatement(stmt)));
            base = previousBase;
        }

        if (suffix) {
            result.add('\n' + addIndent(''));
        }
        return result.valueOf();
    }

    function generateFunctionBody(node) {
        var i, len;

        var result = new Result(node);

        result.add('(');
        for (i = 0, len = node.params.length; i < len; i += 1) {
            result.add(node.params[i].name);
            if ((i + 1) < len) {
                result.add(', ');
            }
        }
        result.add(')');
        result.add(maybeBlock(node.body));
        return result.valueOf();
    }

    function generateExpression(expr, option) {
        var result, precedence, currentPrecedence, previousBase, i, len, raw, allowIn, allowCall;

        precedence = option.precedence;
        allowIn = option.allowIn;
        allowCall = option.allowCall;

        result = new Result(expr);

        switch (expr.type) {
        case Syntax.SequenceExpression:
            result.add('');
            allowIn |= (Precedence.Sequence < precedence);
            for (i = 0, len = expr.expressions.length; i < len; i += 1) {
                result.add(generateExpression(expr.expressions[i], {
                    precedence: Precedence.Assignment,
                    allowIn: allowIn,
                    allowCall: true
                }));
                if ((i + 1) < len) {
                    result.add(', ');
                }
            }
            result = parenthesize(result, Precedence.Sequence, precedence);
            break;

        case Syntax.AssignmentExpression:
            allowIn |= (Precedence.Assignment < precedence);
            result.add(generateExpression(expr.left, {
                precedence: Precedence.Call,
                allowIn: allowIn,
                allowCall: true
            }));
            result.add(' ' + expr.operator + ' ');
            result.add(generateExpression(expr.right, {
                precedence: Precedence.Assignment,
                allowIn: allowIn,
                allowCall: true
            }));
            result = parenthesize(result, Precedence.Assignment, precedence);
            break;

        case Syntax.ConditionalExpression:
            allowIn |= (Precedence.Conditional < precedence);
            result.add(generateExpression(expr.test, {
                precedence: Precedence.LogicalOR,
                allowIn: allowIn,
                allowCall: true
            }));
            result.add(' ? ');
            result.add(generateExpression(expr.consequent, {
                precedence: Precedence.Assignment,
                allowIn: allowIn,
                allowCall: true
            }));
            result.add(' : ');
            result.add(generateExpression(expr.alternate, {
                precedence: Precedence.Assignment,
                allowIn: allowIn,
                allowCall: true
            }));
            result = parenthesize(result, Precedence.Conditional, precedence);
            break;

        case Syntax.LogicalExpression:
        case Syntax.BinaryExpression:
            currentPrecedence = BinaryPrecedence[expr.operator];

            allowIn |= (currentPrecedence < precedence);

            result.add(generateExpression(expr.left, {
                precedence: currentPrecedence,
                allowIn: allowIn,
                allowCall: true
            }));
            result.add(' ' + expr.operator + ' ');
            result.add(generateExpression(expr.right, {
                precedence: currentPrecedence + 1,
                allowIn: allowIn,
                allowCall: true
            }));

            if (expr.operator === 'in' && !allowIn) {
                result = parenthesize(result, 0, 1);
            } else {
                result = parenthesize(result, currentPrecedence, precedence);
            }

            break;

        case Syntax.CallExpression:
            result.add(generateExpression(expr.callee, {
                precedence: Precedence.Call,
                allowIn: true,
                allowCall: true
            }));

            result.add('(');
            for (i = 0, len = expr['arguments'].length; i < len; i += 1) {
                result.add(generateExpression(expr['arguments'][i], {
                    precedence: Precedence.Assignment,
                    allowIn: true,
                    allowCall: true
                }));
                if ((i + 1) < len) {
                    result.add(', ');
                }
            }
            result.add(')');

            if (!allowCall) {
                result = parenthesize(result, 0, 1);
            } else {
                result = parenthesize(result, Precedence.Call, precedence);
            }
            break;

        case Syntax.NewExpression:
            result.add('new ');
            result.add(generateExpression(expr.callee, {
                precedence: Precedence.New,
                allowIn: true,
                allowCall: false
            }));

            result.add('(');
            for (i = 0, len = expr['arguments'].length; i < len; i += 1) {
                result.add(generateExpression(expr['arguments'][i], {
                    precedence: Precedence.Assignment,
                    allowIn: true,
                    allowCall: true
                }));
                if ((i + 1) < len) {
                    result.add(', ');
                }
            }
            result.add(')');

            result = parenthesize(result, Precedence.New, precedence);
            break;

        case Syntax.MemberExpression:
            result.add(generateExpression(expr.object, {
                precedence: Precedence.Call,
                allowIn: true,
                allowCall: allowCall
            }));

            if (expr.computed) {
                result.add('[');
                result.add(generateExpression(expr.property, {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: allowCall
                }));
                result.add(']');
            } else {
                if (expr.object.type === Syntax.Literal && typeof expr.object.value === 'number') {
                    var resultString = result.toString();
                    if (resultString.indexOf('.') < 0) {
                        if (!/[eExX]/.test(resultString) && !(resultString.length >= 2 && resultString[0] === '0')) {
                            result.add('.');
                        }
                    }
                }
                result.add('.' + expr.property.name);
            }

            result = parenthesize(result, Precedence.Member, precedence);
            break;

        case Syntax.UnaryExpression:
            result.add(expr.operator);
            if (result.toString().length > 2) {
                result.add(' ');
            }
            result.add(generateExpression(expr.argument, {
                precedence: Precedence.Unary + (
                    expr.argument.type === Syntax.UnaryExpression &&
                        expr.operator.length < 3 &&
                        expr.argument.operator === expr.operator ? 1 : 0
                ),
                allowIn: true,
                allowCall: true
            }));
            result = parenthesize(result, Precedence.Unary, precedence);
            break;

        case Syntax.UpdateExpression:
            if (expr.prefix) {
                result.add(expr.operator);
                result.add(generateExpression(expr.argument, {
                    precedence: Precedence.Unary,
                    allowIn: true,
                    allowCall: true
                }));
                result = parenthesize(result, Precedence.Unary, precedence);
            } else {
                result.add(generateExpression(expr.argument, {
                    precedence: Precedence.Unary,
                    allowIn: true,
                    allowCall: true
                }));
                result.add(expr.operator);
                result = parenthesize(result, Precedence.Postfix, precedence);
            }
            break;

        case Syntax.FunctionExpression:
            result.add('function ');
            if (expr.id) {
                result.add(expr.id.name);
            }
            result.add(generateFunctionBody(expr));
            break;

        case Syntax.ArrayExpression:
            if (!expr.elements.length) {
                result.add('[]');
                break;
            }
            result.add('[\n');
            previousBase = base;
            base += indent;
            for (i = 0, len = expr.elements.length; i < len; i += 1) {
                if (!expr.elements[i]) {
                    result.add(addIndent(''));
                    if ((i + 1) === len) {
                        result.add(',');
                    }
                } else {
                    result.add(addIndent(generateExpression(expr.elements[i], {
                        precedence: Precedence.Assignment,
                        allowIn: true,
                        allowCall: true
                    })));
                }
                if ((i + 1) < len) {
                    result.add(',\n');
                }
            }
            base = previousBase;
            result.add('\n' + addIndent(']'));
            break;

        case Syntax.Property:
            if (expr.kind === 'get' || expr.kind === 'set') {
                result.add(expr.kind + ' ');
                result.add(generateExpression(expr.key, {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: true
                }));
                result.add(generateFunctionBody(expr.value));
            } else {
                result.add(generateExpression(expr.key, {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: true
                }));
                result.add(': ');
                result.add(generateExpression(expr.value, {
                    precedence: Precedence.Assignment,
                    allowIn: true,
                    allowCall: true
                }));
            }
            break;

        case Syntax.ObjectExpression:
            if (!expr.properties.length) {
                result.add('{}');
                break;
            }
            result.add('{\n');
            previousBase = base;
            base += indent;
            for (i = 0, len = expr.properties.length; i < len; i += 1) {
                result.add(addIndent(generateExpression(expr.properties[i], {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: true
                })));
                if ((i + 1) < len) {
                    result.add(',\n');
                }
            }
            base = previousBase;
            result.add('\n' + addIndent('}'));
            break;

        case Syntax.ThisExpression:
            result.add('this');
            break;

        case Syntax.Identifier:
            result.add(expr.name);
            break;

        case Syntax.Literal:
            if (expr.hasOwnProperty('raw') && parse) {
                try {
                    raw = parse(expr.raw).body[0].expression;
                    if (raw.type === Syntax.Literal) {
                        if (raw.value === expr.value) {
                            result.add(expr.raw);
                            break;
                        }
                    }
                } catch (e) {
                    // not use raw property
                }
            }

            if (expr.value === null) {
                result.add('null');
                break;
            }

            if (typeof expr.value === 'string') {
                result.add(escapeString(expr.value));
                break;
            }

            if (typeof expr.value === 'number' && expr.value === Infinity) {
                // Infinity is variable
                result.add('1e+1000');
                break;
            }

            result.add(expr.value.toString());
            break;

        default:
            throw new Error('Unknown expression type: ' + expr.type);
            break;
        }
        return result;
    }

    function generateStatement(stmt, option) {
        var i, len, previousBase, comment, save, ret, node, allowIn, sourceNode;

        // Create SourceNode for Source Map
        sourceNode = new Result(stmt);

        if (extra.comment) {
            if (stmt.leadingComments) {
                for (i = 0, len = stmt.leadingComments.length; i < len; i += 1) {
                    comment = stmt.leadingComments[i];
                    ret = generateComment(comment);
                    if (i > 0) {
                        sourceNode.add(addIndent(ret));
                    } else {
                        sourceNode.add(ret);
                    }
                    if (!endsWithLineTerminator(ret)) {
                        sourceNode.add('\n');
                    }
                }
                sourceNode.add(addIndent(''));
            }
        }


        allowIn = true;
        if (option) {
            allowIn = option.allowIn;
        }

        switch (stmt.type) {
        case Syntax.BlockStatement:
            sourceNode.add('{\n');

            previousBase = base;
            base += indent;
            for (i = 0, len = stmt.body.length; i < len; i += 1) {
                sourceNode.add(addIndent(generateStatement(stmt.body[i])));
                sourceNode.add('\n');
            }
            base = previousBase;

            sourceNode.add(addIndent('}'));
            break;

        case Syntax.BreakStatement:
            if (stmt.label) {
                sourceNode.add('break ' + stmt.label.name + ';');
            } else {
                sourceNode.add('break;');
            }
            break;

        case Syntax.ContinueStatement:
            if (stmt.label) {
                sourceNode.add('continue ' + stmt.label.name + ';');
            } else {
                sourceNode.add('continue;');
            }
            break;

        case Syntax.DoWhileStatement:
            sourceNode.add('do');
            sourceNode.add(maybeBlock(stmt.body, true));
            sourceNode.add('while (');
            sourceNode.add(generateExpression(stmt.test, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            }));
            sourceNode.add(');');
            break;

        case Syntax.CatchClause:
            previousBase = base;
            base += indent;
            sourceNode.add(' catch (');
            sourceNode.add(generateExpression(stmt.param, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            }));
            sourceNode.add(')');
            base = previousBase;
            sourceNode.add(maybeBlock(stmt.body));
            break;

        case Syntax.DebuggerStatement:
            sourceNode.add('debugger;');
            break;

        case Syntax.EmptyStatement:
            sourceNode.add(';');
            break;

        case Syntax.ExpressionStatement:
            ret = generateExpression(stmt.expression, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            });
            // 12.4 '{', 'function' is not allowed in this position.
            // wrap espression with parentheses
            if (ret.toString()[0] === '{' || ret.toString().indexOf('function ') === 0) {
                sourceNode.add('(');
                sourceNode.add(ret);
                sourceNode.add(');');
            } else {
                sourceNode.add(ret);
                sourceNode.add(';');
            }
            break;

        case Syntax.VariableDeclarator:
            if (stmt.init) {
                sourceNode.add(stmt.id.name + ' = ');
                sourceNode.add(generateExpression(stmt.init, {
                    precedence: Precedence.Assignment,
                    allowIn: allowIn,
                    allowCall: true
                }));
            } else {
                sourceNode.add(stmt.id.name);
            }
            break;

        case Syntax.VariableDeclaration:
            sourceNode.add(stmt.kind);
            // special path for
            // var x = function () {
            // };
            if (stmt.declarations.length === 1 && stmt.declarations[0].init &&
                    stmt.declarations[0].init.type === Syntax.FunctionExpression) {
                sourceNode.add(' ');
                sourceNode.add(generateStatement(stmt.declarations[0], {
                    allowIn: allowIn
                }));
            } else {
                // VariableDeclarator is typed as Statement,
                // but joined with comma (not LineTerminator).
                // So if comment is attached to target node, we should specialize.
                previousBase = base;
                base += indent;

                node = stmt.declarations[0];
                if (extra.comment && node.leadingComments) {
                    sourceNode.add('\n');
                    sourceNode.add(addIndent(generateStatement(node, {
                        allowIn: allowIn
                    })));
                } else {
                    sourceNode.add(' ');
                    sourceNode.add(generateStatement(node, {
                        allowIn: allowIn
                    }));
                }

                for (i = 1, len = stmt.declarations.length; i < len; i += 1) {
                    node = stmt.declarations[i];
                    if (extra.comment && node.leadingComments) {
                        sourceNode.add(',\n');
                        sourceNode.add(addIndent(generateStatement(node, {
                            allowIn: allowIn
                        })));
                    } else {
                        sourceNode.add(', ');
                        sourceNode.add(generateStatement(node, {
                            allowIn: allowIn
                        }));
                    }
                }
                base = previousBase;
            }
            sourceNode.add(';');
            break;

        case Syntax.ThrowStatement:
            sourceNode.add('throw ');
            sourceNode.add(generateExpression(stmt.argument, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            }));
            sourceNode.add(';');
            break;

        case Syntax.TryStatement:
            sourceNode.add('try');
            sourceNode.add(maybeBlock(stmt.block));
            for (i = 0, len = stmt.handlers.length; i < len; i += 1) {
                sourceNode.add(generateStatement(stmt.handlers[i]));
            }
            if (stmt.finalizer) {
                sourceNode.add(' finally');
                sourceNode.add(maybeBlock(stmt.finalizer));
            }
            break;

        case Syntax.SwitchStatement:
            previousBase = base;
            base += indent;
            sourceNode.add('switch (');
            sourceNode.add(generateExpression(stmt.discriminant, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            }));
            sourceNode.add(') {\n');
            base = previousBase;
            if (stmt.cases) {
                for (i = 0, len = stmt.cases.length; i < len; i += 1) {
                    sourceNode.add(addIndent(generateStatement(stmt.cases[i])));
                    sourceNode.add('\n');
                }
            }
            sourceNode.add(addIndent('}'));
            break;

        case Syntax.SwitchCase:
            previousBase = base;
            base += indent;
            if (stmt.test) {
                sourceNode.add('case ');
                sourceNode.add(generateExpression(stmt.test, {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: true
                }));
                sourceNode.add(':');
            } else {
                sourceNode.add('default:');
            }

            i = 0;
            len = stmt.consequent.length;
            if (len && stmt.consequent[0].type === Syntax.BlockStatement) {
                sourceNode.add(maybeBlock(stmt.consequent[0]));
                i = 1;
            }

            for (; i < len; i += 1) {
                sourceNode.add('\n');
                sourceNode.add(addIndent(generateStatement(stmt.consequent[i])));
            }

            base = previousBase;
            break;

        case Syntax.IfStatement:
            if (stmt.alternate) {
                if (stmt.alternate.type === Syntax.IfStatement) {
                    previousBase = base;
                    base += indent;
                    sourceNode.add('if (');
                    sourceNode.add(generateExpression(stmt.test, {
                        precedence: Precedence.Sequence,
                        allowIn: true,
                        allowCall: true
                    }));
                    sourceNode.add(')');
                    base = previousBase;
                    sourceNode.add(maybeBlock(stmt.consequent, true));
                    sourceNode.add('else ');
                    sourceNode.add(generateStatement(stmt.alternate));
                } else {
                    previousBase = base;
                    base += indent;
                    sourceNode.add('if (');
                    sourceNode.add(generateExpression(stmt.test, {
                        precedence: Precedence.Sequence,
                        allowIn: true,
                        allowCall: true
                    }));
                    sourceNode.add(')');
                    base = previousBase;
                    sourceNode.add(maybeBlock(stmt.consequent, true));
                    sourceNode.add('else');
                    sourceNode.add(maybeBlock(stmt.alternate));
                }
            } else {
                previousBase = base;
                base += indent;
                sourceNode.add('if (');
                sourceNode.add(generateExpression(stmt.test, {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: true
                }));
                sourceNode.add(')');
                base = previousBase;
                sourceNode.add(maybeBlock(stmt.consequent));
            }
            break;

        case Syntax.ForStatement:
            previousBase = base;
            base += indent;
            sourceNode.add('for (');
            if (stmt.init) {
                if (stmt.init.type === Syntax.VariableDeclaration) {
                    sourceNode.add(generateStatement(stmt.init, {
                        allowIn: false
                    }));
                } else {
                    sourceNode.add(generateExpression(stmt.init, {
                        precedence: Precedence.Sequence,
                        allowIn: false,
                        allowCall: true
                    }));
                    sourceNode.add(';');
                }
            } else {
                sourceNode.add(';');
            }

            if (stmt.test) {
                sourceNode.add(' ');
                sourceNode.add(generateExpression(stmt.test, {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: true
                }));
                sourceNode.add(';');
            } else {
                sourceNode.add(';');
            }

            if (stmt.update) {
                sourceNode.add(' ');
                sourceNode.add(generateExpression(stmt.update, {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: true
                }));
                sourceNode.add(')');
            } else {
                sourceNode.add(')');
            }
            base = previousBase;

            sourceNode.add(maybeBlock(stmt.body));
            break;

        case Syntax.ForInStatement:
            sourceNode.add('for (');
            if (stmt.left.type === Syntax.VariableDeclaration) {
                previousBase = base;
                base += indent + indent;
                sourceNode.add(stmt.left.kind + ' ');
                sourceNode.add(generateStatement(stmt.left.declarations[0], {
                    allowIn: false
                }));
                base = previousBase;
            } else {
                previousBase = base;
                base += indent;
                sourceNode.add(generateExpression(stmt.left, {
                    precedence: Precedence.Call,
                    allowIn: true,
                    allowCall: true
                }));
                base = previousBase;
            }

            previousBase = base;
            base += indent;
            sourceNode.add(' in ');
            sourceNode.add(generateExpression(stmt.right, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            }));
            sourceNode.add(')');
            base = previousBase;
            sourceNode.add(maybeBlock(stmt.body));
            break;

        case Syntax.LabeledStatement:
            sourceNode.add(stmt.label.name + ':');
            sourceNode.add(maybeBlock(stmt.body));
            break;

        case Syntax.Program:
            sourceNode.add('');
            for (i = 0, len = stmt.body.length; i < len; i += 1) {
                sourceNode.add(addIndent(generateStatement(stmt.body[i])));
                if ((i + 1) < len) {
                    sourceNode.add('\n');
                }
            }
            break;

        case Syntax.FunctionDeclaration:
            sourceNode.add('function ');
            if (stmt.id) {
                sourceNode.add(stmt.id.name);
            }
            sourceNode.add(generateFunctionBody(stmt));
            break;

        case Syntax.ReturnStatement:
            if (stmt.argument) {
                sourceNode.add('return ');
                sourceNode.add(generateExpression(stmt.argument, {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: true
                }));
                sourceNode.add(';');
            } else {
                sourceNode.add('return;');
            }
            break;

        case Syntax.WhileStatement:
            previousBase = base;
            base += indent;
            sourceNode.add('while (');
            sourceNode.add(generateExpression(stmt.test, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            }));
            sourceNode.add(')');
            base = previousBase;
            sourceNode.add(maybeBlock(stmt.body));
            break;

        case Syntax.WithStatement:
            previousBase = base;
            base += indent;
            sourceNode.add('with (');
            sourceNode.add(generateExpression(stmt.object, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            }));
            sourceNode.add(')');
            base = previousBase;
            sourceNode.add(maybeBlock(stmt.body));
            break;

        default:
            throw new Error('Unknown statement type: ' + stmt.type);
            break;
        }

        // Attach comments
        // had to split leading comments out before the switch because we cannot
        // prepend to SourceNodes

        if (extra.comment) {
            if (stmt.trailingComments) {
                for (i = 0, len = stmt.trailingComments.length; i < len; i += 1) {
                    comment = stmt.trailingComments[i];
                    ret = generateComment(comment);
                    sourceNode.add(addIndent(ret));
                    if (!endsWithLineTerminator(ret)) {
                        sourceNode.add('\n');
                    }
                }
            }
        }

        return sourceNode.valueOf();
    }

    function generate(node, options) {
        var defaultOptions = getDefaultOptions();

        if (typeof options !== 'undefined') {
            // Obsolete options
            //
            //   `options.indent`
            //   `options.base`
            //
            // Instead of them, we can use `option.format.indent`.
            if (typeof options.indent === 'string') {
                defaultOptions.format.indent.style = options.indent;
            }

            options = updateDeeply(defaultOptions, options);
            indent = options.format.indent.style;
            if (typeof options.base === 'string') {
                base = options.base;
            } else {
                base = stringRepeat(indent, options.format.indent.base);
            }
            parse = options.parse;
        } else {
            options = defaultOptions;
            indent = options.format.indent.style;
            base = stringRepeat(indent, options.format.indent.base);
            parse = options.parse;
        }
        extra = options;

        if (extra.sourceMap) {
            Result = function (node) {
                var line = null;
                var column = null;
                var source = '';

                if (typeof node.loc !== 'undefined') {
                    line = node.loc.start.line;
                    column = node.loc.start.column;
                    //source = node.loc.source;
                    source = node.loc.source || extra.sourceFile;
                }
                return new extra.sourceMap.SourceNode(line, column, source);
            }
            Result.prototype.valueOf = function() {
                return this;
            }
        } else {
            Result = function() {
                this.string = '';
            }
            Result.prototype.add = function(jsString) {
                this.string += jsString;
            }
            Result.prototype.valueOf = function() {
                return this.string;
            }
            Result.prototype.toString = function() {
                return this.string;
            }
        }

        switch (node.type) {
        case Syntax.BlockStatement:
        case Syntax.BreakStatement:
        case Syntax.CatchClause:
        case Syntax.ContinueStatement:
        case Syntax.DoWhileStatement:
        case Syntax.DebuggerStatement:
        case Syntax.EmptyStatement:
        case Syntax.ExpressionStatement:
        case Syntax.ForStatement:
        case Syntax.ForInStatement:
        case Syntax.FunctionDeclaration:
        case Syntax.IfStatement:
        case Syntax.LabeledStatement:
        case Syntax.Program:
        case Syntax.ReturnStatement:
        case Syntax.SwitchStatement:
        case Syntax.SwitchCase:
        case Syntax.ThrowStatement:
        case Syntax.TryStatement:
        case Syntax.VariableDeclaration:
        case Syntax.VariableDeclarator:
        case Syntax.WhileStatement:
        case Syntax.WithStatement:
            return generateStatement(node).valueOf();

        case Syntax.AssignmentExpression:
        case Syntax.ArrayExpression:
        case Syntax.BinaryExpression:
        case Syntax.CallExpression:
        case Syntax.ConditionalExpression:
        case Syntax.FunctionExpression:
        case Syntax.Identifier:
        case Syntax.Literal:
        case Syntax.LogicalExpression:
        case Syntax.MemberExpression:
        case Syntax.NewExpression:
        case Syntax.ObjectExpression:
        case Syntax.Property:
        case Syntax.SequenceExpression:
        case Syntax.ThisExpression:
        case Syntax.UnaryExpression:
        case Syntax.UpdateExpression:
            return generateExpression(node, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            }).valueOf();

        default:
            break;
        }
        throw new Error('Unknown node type: ' + node.type);
    }

    // simple visitor implementation

    VisitorKeys = {
        AssignmentExpression: ['left', 'right'],
        ArrayExpression: ['elements'],
        BlockStatement: ['body'],
        BinaryExpression: ['left', 'right'],
        BreakStatement: ['label'],
        CallExpression: ['callee', 'arguments'],
        CatchClause: ['param', 'body'],
        ConditionalExpression: ['test', 'consequent', 'alternate'],
        ContinueStatement: ['label'],
        DoWhileStatement: ['body', 'test'],
        DebuggerStatement: [],
        EmptyStatement: [],
        ExpressionStatement: ['expression'],
        ForStatement: ['init', 'test', 'update', 'body'],
        ForInStatement: ['left', 'right', 'body'],
        FunctionDeclaration: ['id', 'params', 'body'],
        FunctionExpression: ['id', 'params', 'body'],
        Identifier: [],
        IfStatement: ['test', 'consequent', 'alternate'],
        Literal: [],
        LabeledStatement: ['label', 'body'],
        LogicalExpression: ['left', 'right'],
        MemberExpression: ['object', 'property'],
        NewExpression: ['callee', 'arguments'],
        ObjectExpression: ['properties'],
        Program: ['body'],
        Property: ['key', 'value'],
        ReturnStatement: ['argument'],
        SequenceExpression: ['expressions'],
        SwitchStatement: ['descriminant', 'cases'],
        SwitchCase: ['test', 'consequent'],
        ThisExpression: [],
        ThrowStatement: ['argument'],
        TryStatement: ['block', 'handlers', 'finalizer'],
        UnaryExpression: ['argument'],
        UpdateExpression: ['argument'],
        VariableDeclaration: ['declarations'],
        VariableDeclarator: ['id', 'init'],
        WhileStatement: ['test', 'body'],
        WithStatement: ['object', 'body']
    };

    VisitorOption = {
        Break: 1,
        Skip: 2
    };

    function traverse(top, visitor) {
        var worklist, leavelist, node, ret, current, current2, candidates, candidate;

        worklist = [ top ];
        leavelist = [];

        while (worklist.length) {
            node = worklist.pop();

            if (node) {
                if (visitor.enter) {
                    ret = visitor.enter(node);
                } else {
                    ret = undefined;
                }

                if (ret === VisitorOption.Break) {
                    return;
                }

                worklist.push(null);
                leavelist.push(node);

                if (ret !== VisitorOption.Skip) {
                    candidates = VisitorKeys[node.type];
                    current = candidates.length;
                    while ((current -= 1) >= 0) {
                        candidate = node[candidates[current]];
                        if (candidate) {
                            if (isArray(candidate)) {
                                current2 = candidate.length;
                                while ((current2 -= 1) >= 0) {
                                    if (candidate[current2]) {
                                        worklist.push(candidate[current2]);
                                    }
                                }
                            } else {
                                worklist.push(candidate);
                            }
                        }
                    }
                }
            } else {
                node = leavelist.pop();
                if (visitor.leave) {
                    ret = visitor.leave(node);
                } else {
                    ret = undefined;
                }
                if (ret === VisitorOption.Break) {
                    return;
                }
            }
        }
    }


    // based on LLVM libc++ upper_bound / lower_bound
    // MIT License

    function upperBound(array, func) {
        var diff, len, i, current;

        len = array.length;
        i = 0;

        while (len) {
            diff = len >>> 1;
            current = i + diff;
            if (func(array[current])) {
                len = diff;
            } else {
                i = current + 1;
                len -= diff + 1;
            }
        }
        return i;
    }

    function lowerBound(array, func) {
        var diff, len, i, current;

        len = array.length;
        i = 0;

        while (len) {
            diff = len >>> 1;
            current = i + diff;
            if (func(array[current])) {
                i = current + 1;
                len -= diff + 1;
            } else {
                len = diff;
            }
        }
        return i;
    }

    function extendCommentRange(comment, tokens) {
        var target, token;

        target = upperBound(tokens, function search(token) {
            return token.range[0] > comment.range[0];
        });

        comment.extendedRange = [comment.range[0], comment.range[1]];

        if (target !== tokens.length) {
            comment.extendedRange[1] = tokens[target].range[0];
        }

        target -= 1;
        if (target >= 0) {
            if (target < tokens.length) {
                comment.extendedRange[0] = tokens[target].range[1];
            } else if (token.length) {
                comment.extendedRange[1] = tokens[tokens.length - 1].range[0];
            }
        }

        return comment;
    }

    function attachComments(tree, providedComments, tokens) {
        // At first, we should calculate extended comment ranges.
        var comments = [], len, i;

        if (!tree.range) {
            throw new Error('attachComments needs range information');
        }

        for (i = 0, len = providedComments.length; i < len; i += 1) {
            comments.push(extendCommentRange(deepCopy(providedComments[i]), tokens));
        }

        // This is based on John Freeman's implementation.
        traverse(tree, {
            cursor: 0,
            enter: function (node) {
                var comment;

                while (this.cursor < comments.length) {
                    comment = comments[this.cursor];
                    if (comment.extendedRange[1] > node.range[0]) {
                        break;
                    }

                    if (comment.extendedRange[1] === node.range[0]) {
                        if (!node.leadingComments) {
                            node.leadingComments = [];
                        }
                        node.leadingComments.push(comment);
                        comments.splice(this.cursor, 1);
                    } else {
                        this.cursor += 1;
                    }
                }

                // already out of owned node
                if (this.cursor === comments.length) {
                    return VisitorOption.Break;
                }

                if (comments[this.cursor].extendedRange[0] > node.range[1]) {
                    return VisitorOption.Skip;
                }
            }
        });

        traverse(tree, {
            cursor: 0,
            leave: function (node) {
                var comment;

                while (this.cursor < comments.length) {
                    comment = comments[this.cursor];
                    if (node.range[1] < comment.extendedRange[0]) {
                        break;
                    }

                    if (node.range[1] === comment.extendedRange[0]) {
                        if (!node.trailingComments) {
                            node.trailingComments = [];
                        }
                        node.trailingComments.push(comment);
                        comments.splice(this.cursor, 1);
                    } else {
                        this.cursor += 1;
                    }
                }

                // already out of owned node
                if (this.cursor === comments.length) {
                    return VisitorOption.Break;
                }

                if (comments[this.cursor].extendedRange[0] > node.range[1]) {
                    return VisitorOption.Skip;
                }
            }
        });

        return tree;
    }

    // Sync with package.json.
    exports.version = '0.0.5-dev';

    exports.generate = generate;
    exports.traverse = traverse;
    exports.attachComments = attachComments;

}(typeof exports === 'undefined' ? (escodegen = {}) : exports));
/* vim: set sw=4 ts=4 et tw=80 : */

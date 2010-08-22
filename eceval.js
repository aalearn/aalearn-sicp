
$(document).ready(function () {
    announce_html_output("<b>JQuery-Scheme Interpreter</b><br>Interpreter loaded.");

    wait_for_input();
});

// --- I/O Functions ---
function announce_html_output(out) {
    $('#content').append($('<div class="output" />').html(out));
}

function announce_output(out) {
    $('#content').append($('<div class="output" />').text(out));
}

function wait_for_input() {
    var new_input = $('<div class="input" contenteditable="true" />');
    $('#content').append('<div class="prompt">&raquo;&nbsp;</div>').append(new_input);
    add_bindings(new_input, receive_input).focus();
}

function add_bindings(element, input_receiver) {
    return element.bind('keydown','return', function() {
	input_receiver($(this));
	return false; // prevent bubble
    }).bind('keydown','up', function() {
	var current_history_selection = $('.history-selection');
	if (current_history_selection.length) {
	    current_history_selection.removeClass('history-selection');
	} else {
	    current_history_selection = $(this);
	}
	// TODO: filter out items without text
	set_new_history($(this), current_history_selection.prevAll('.input').first());
	return false;
    }).bind('keydown','down', function() {
	var current_history_selection = $('.history-selection');
	if (current_history_selection.length) {
	    current_history_selection.removeClass('history-selection');
	    var next_item = current_history_selection.nextAll('.input').first();
	    set_new_history($(this), next_item[0] == this ? $(undefined) : next_item);
	}
	return false;
    });
}

function set_new_history(target, source) {
    target.text(source.addClass('history-selection').text());
}
    
function receive_input(element) {
    element.attr('contentEditable', false);
    $('.input .history-selection').removeClass('history-selection');
    var input_expression = parse(tokenize(element.text()));
    $.map($.map(input_expression, evaluate), function (o) { announce_output($.toJSON(o)) });

    // announce_output($.toJSON($.map(input_expression, evaluate)));
    wait_for_input();
}

function qeval(exp) { // quick eval of a string for debugging purposes
    return evaluate(parse(tokenize(exp))[0]);
}
// --- Lexical Analysis, etc. ---
function tokenize(text) {
    var tokens = [];
    var last_token = null;
    var in_text_literal = false;
    for (i = 0, len = text.length; i < len; i++) {
	var c = text[i];
	if (in_text_literal) {
	    if (last_token[0] == 'escape') {
		if (c == 'n') {
		    last_token[1] += "\n";
		} else {
		    last_token[1] += c;
		}
	    } else if (c == '"') {
		in_text_literal = false;
	    } else {
		last_token[1] += c;
	    }
	} else if (c == '"') {
	    in_text_literal = true;
	    tokens.push(['text_literal', '']);
	} else if (/[a-zA-Z_<>\+\/\-\=\*\/!?-]/.exec(c)) {
	    if (last_token && last_token[0] == 'symbol') {
		last_token[1] += c;
	    } else {
		tokens.push(['symbol', c]);
	    }
	} else if (/[0-9.]/.exec(c)) {
	    // TODO: handle negative numbers!
	    if (last_token && (last_token[0] == 'symbol' || last_token[0] == 'number')) {
		last_token[1] += c;
	    } else {
		tokens.push(['number', c]);
	    }
	} else if (/\s/.exec(c)) {
	    tokens.push(['whitespace', c]);
	} else if (c == '(') {
	    tokens.push(['open-paren', c]);
	} else if (c == ')') {
	    tokens.push(['close-paren', c]);
	} else if (c == '\\') {
	    tokens.push(['escape', c]);
	} else if (c == '\'') {
	    tokens.push(['quote',c]);
	} else {
	    tokens.push(['error','error']);
	}
	last_token = tokens[tokens.length-1];
    }
    return tokens;
}

function parse(tokens) {
    var exp = [];
    var insert_points = [exp];
    var quote_next = false;
    // todo: block/detect invalid parsings, e.g. quote then close-paren
    for (i = 0, len = tokens.length; i < len; i++) {
	token = tokens[i];
	token_type = token[0];
	if (token_type == 'open-paren') {
	    var new_nested_exp = [];
	    if (quote_next) {
		new_nested_exp = [['symbol','quote']];
		quote_next = false;
	    }
	    insert_points[insert_points.length-1].push(new_nested_exp);

	    var list_end = [];
	    insert_points[insert_points.length-1].push(list_end);
	    insert_points[insert_points.length-1] = list_end;
	    insert_points.push(new_nested_exp);
	} else if (token_type == 'close-paren') {
	    insert_points.pop();
	} else if (token_type == 'quote') {
	    quote_next = true;
	} else if (token_type == 'whitespace') {
	    // ignore
	} else {
	    if (quote_next) {
		insert_points[insert_points.length-1].push(['quote',token]);
		quote_next = false;
	    } else {
		insert_points[insert_points.length-1].push(token);
		// These lines make parse-tree into scheme-style lists ([A, [B, [] ] ])
		var list_end = [];
		insert_points[insert_points.length-1].push(list_end);
		insert_points[insert_points.length-1] = list_end;
	    }
	}
    }
    return exp;
}

// --- Eval/Apply ---
// just a sketch for now
function evaluate(exp) {
    if (self_evaluating(exp)) {
	return parseFloat(exp[1]);
    } else if (symbol(exp)) {
	// check if defined locally
	if (primitive_operation(exp)) {
	    return primitive_operation_function(exp);
	}
    } else if (exp[0] instanceof Array) {
	console.log(exp);
	console.log(stringify(exp));
	var evaled = $.map(to_js_style_array(exp), evaluate);
	var op = evaled[0];
	var argl = evaled.slice(1);
	console.log($.toJSON(argl));
	if (op instanceof Function) {
	    return op.apply(undefined, argl); 
	} else {
	    return "non-primitive op: " + op;
	}
	if (op == '+') {
	    return argl[0] + argl[1];
	} else {
	    return "cannot handle op";
	}
    }
}

function self_evaluating(exp) {
    return exp[0] == 'number'; // also handle: || exp[0] == 'string';
}

function symbol(exp) {
    return exp[0] == 'symbol';
}

var primitive_operations = {
    '+': function(a,b) { return a + b },
    '-': function(a,b) { return a - b },
    '*': function(a,b) { return a * b },
    '/': function(a,b) { return a / b },
    '=': function(a,b) { return a == b },
    'display': announce_output
};

function primitive_operation(exp) {
    return exp[1] in primitive_operations;
}

function primitive_operation_function(exp) {
    return primitive_operations[exp[1]];
}

function cons(a, b) { return [a, b]; }
function car(a) { return a[0]; }
function cdr(a) { return a[1]; }

function to_js_style_array(a) { 
    if (a.length == 0) { 
	return [] 
    } else if (a.length == 1) { 
	console.log("should never happen");
    } else {
	var out = [];
	while (a.length > 0) {
	    out.push(car(a));
	    a = cdr(a);
	}
    }
    return out;
}

// debugging statements (okay to be recursive, not tail-call-optimized)
function is_scheme_style_list(a) {
    return ((a instanceof Array) && (a.length == 2) && is_scheme_style_list(a[1]))
}

function is_token(exp) {
    return (exp instanceof Array) && !(exp[0] instanceof Array);
}

function is_nil(exp) {
    return (exp instanceof Array) && exp[0]==undefined;
}

function stringify(exp, skip_parens) {
    if (exp == undefined) {
	return 'fail';
    } else if (is_token(exp)) {
	return is_nil(exp) ? 'nil' : exp[0] + '/' + exp[1];
    } else {
	var o = stringify(exp[0]);
	if (!is_nil(exp[1])) {
	    o += (is_token(exp[1]) ? ' . ' : ' ') + stringify(exp[1], 'skip');
	}
	return skip_parens ? o : '(' + o + ')';
    }
}

// when we reach the + we want to do it, when we reach the + 1 2

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
    announce_output($.toJSON($.map(input_expression, evaluate)));
    wait_for_input();
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
	    // TODO: probably need to make these lists similar to lisp arrays 
	    // otherwise it's not easy to make set-cdr! work as expected
	    insert_points[insert_points.length-1].push(new_nested_exp);
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
	    }
	}
    }
    return exp;
}

// --- Eval/Apply ---
// just a sketch for now
function evaluate(exp) {
    // if (exp instanceof Array) {
	
    if (self_evaluating(exp)) {
	return parseFloat(exp);
    } else if (exp == '+') {
	return '+';
    } else if (exp instanceof Array) {
	var evaled = $.map(exp, evaluate);
	var op = evaled[0];
	var argl = evaled.slice(1);
	if (op == '+') {
	    return argl[0] + argl[1];
	} else {
	    return "cannot handle op";
	}
    }
}

function self_evaluating(exp) {
    return (/^[0-9.]*$/).exec(exp);
}


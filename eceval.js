
$(document).ready(function () {
    install_page_level_bindings();
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
    new_input.addBindings().focus();
}

$.fn.addBindings = function() {
    return this.bind('keydown','return', function() {
	$('.input').unbind('keydown'); // disable all others
	receive_input($(this));
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
};


function set_new_history(target, source) {
    target.text(source.addClass('history-selection').text());
}
    

var user_stop = undefined;

function receive_input(element) {
    element.attr('contentEditable', false);
    $('.input .history-selection').removeClass('history-selection');
    var input_expression = parse(tokenize(element.text()));
    user_stop = undefined;
    while (input_expression && input_expression.length > 0) {
	announce_output(stringify_scheme_exp(evaluate(car(input_expression))));
	input_expression = cdr(input_expression);
    }
    wait_for_input();
}

function qeval(exp) { // quick eval of a string for debugging purposes
    return evaluate(parse(tokenize(exp))[0]);
}


function install_page_level_bindings() {
    $(document).bind('keydown','ctrl+c', function() {
	// currently not-enabled -- needs to set user_stop
	// eceval also needs to allow this event to run in between iterations
    });
    $('#content').bind('click', function() {
	$('.input').last().focus();
    });
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
	} else if (/[a-zA-Z_<>\+\/\-\=\*\/\!\?\-]/.exec(c)) {
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
		insert_points[insert_points.length-1].push([['symbol','quote'],[token, []]]);
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

// --- Stack ---
// stored as a javascript array
var stack = ['STACK'];
function save(x) { 
    // console.log('saving ' + $.toJSON(x) + ' to stack');
    stack.push(x);
    // console.log('stack is now ' + $.toJSON(stack));
}
function restore() { return stack.pop(); }
function debug_stack() { console.log('Stack:'); console.log(stack) }

// --- Environment ---
// stored as a javascript array of javascript hashes (objects)
var env = [{}];
var unbound_variable_error = ['error', 'unbound_variable'];

function new_env() { return {} };

function make_frame(variables, values) {
    var frame = {};
    for (var i = 0, len = variables.length; i < len; i++) {
	frame[variables[i]] = values[i];
    }
    return frame;
}

function extend_environment(variables, values, env) {
    return [make_frame(
	$.map(ast_to_js_style_array(variables), symbol_name), 
	ast_to_js_style_array(values))].concat(env);
}

function lookup_variable_value(variable, env) {
    for (i = 0, len = env.length; i < len; i++) {
	if (env[i].hasOwnProperty(variable)) {
	    return env[i][variable];
	}
    }
    return unbound_variable_error;
}

function set_variable_value(variable, value, env) {
    for (i = 0, len = env.length; i < len; i++) {
	if (env[i].hasOwnProperty(variable)) {
	    env[i][variable] = value;
	    return;
	}
    }
    env[0][variable] = value;
}
    

// --- Eval/Apply ---
// only exp/unev stored in 'token' metadata form, rest are in 'simple' form
var exp;
var unev;

var argl;
var branch;
var continue_to;
var val;
var proc;

function evaluate(ast) {
    continue_to = 'done';
    branch = 'eval-dispatch';
    val = undefined;
    exp = ast;
    var count = 0;
    while (branch !== 'done') {
	// console.log(branch);
	eceval_step();
	if (count++ > 999) {
	    branch = 'done';
	    console.log('infinite loop guard');
	}
    }
    return val;
}

function eceval_step() {
    // console.log(branch);
    switch(branch) {
	
    case 'eval-dispatch':
	// console.log(exp);
	if (self_evaluating(exp)) {
	    // ev-self-eval
	    val = self_evaluated(exp);
	    branch = continue_to;
	} else if (variable(exp)) { 
	    // ev-variable
	    val = lookup_variable_value(symbol_name(exp), env);
	    if (val == unbound_variable_error) {
		val = 'unbound symbol: ' + symbol_name(exp);
		branch = 'signal-error';
	    } else {
		branch = continue_to;
	    }
	} else if (quoted(exp)) {
	    val = text_of_quotation(exp);
	    branch = continue_to;
	} else if (assignment(exp)) {
	    branch = 'ev-assignment';
	} else if (definition(exp)) {
	    branch = 'ev-definition';
	} else if (if_exp(exp)) {
	    branch = 'ev-if';
	} else if (lambda(exp)) {
	    branch = 'ev-lambda';
	} else if (begin(exp)) {
	    branch = 'ev-begin';
	} else if (application(exp)) {
	    branch = 'ev-application';
	} else {
	    // unknown expression type
	    val = 'unknown expression type: ' + stringify_abstract_syntax_tree(exp);
	    branch = 'signal-error';
	}

	break;

    case 'unknown-procedure-type':
	val = 'unknown procedure: ' + proc;
	branch = 'signal-error';
	break;

    case 'ev-lambda':
	unev = lambda_parameters(exp);
	exp = lambda_body(exp);
	val = make_procedure(unev, exp, env);
	branch = continue_to;
	break;

    case 'ev-application':
	save(continue_to);
	unev = operands(exp);
	exp = operator(exp);
	if (symbol(exp)) {
	    // ev-operator-symbol
	    proc = lookup_variable_value(symbol_name(exp), env);

	    if (proc == unbound_variable_error) {
		// hacked -- a little different from the text
		if (primitive_op(exp)) {
		    proc = primitive_procedure_proc(exp);
		    branch = 'ev-appl-did-operator-symbol';
		} else {
		    branch = 'unknown-procedure-type';
		}
	    } else {
		branch = 'ev-appl-did-operator-symbol';
	    }
	} else {
	    save(env);
	    save(unev);
	    continue_to = 'ev-appl-did-operator';
	    branch = 'eval-dispatch';
	}
	break;
    case 'ev-appl-did-operator':
	unev = restore();
	env = restore();
	proc = val;
	// no break
    case 'ev-appl-did-operator-symbol':
	argl = [];
	if (no_operands(unev)) {
	    branch = 'apply-dispatch';
	} else {
	    save(proc);
	    branch = 'ev-appl-operand-loop';
	}
	break;
    case 'ev-appl-operand-loop':
	save(argl);
	exp = first_operand(unev);
	if (last_operand(unev)) {
	    branch = 'ev-appl-last-arg';
	} else {
	    save(env);
	    save(unev);
	    continue_to = 'ev-appl-accumulate-arg';
	    branch = 'eval-dispatch';
	}
	break;
    case 'ev-appl-accumulate-arg':
	unev = restore();
	env = restore();
	argl = restore();
	argl = adjoin_arg(val, argl);
	unev = rest_operands(unev);
	branch = 'ev-appl-operand-loop';
	break;
    case 'ev-appl-last-arg':
	continue_to = 'ev-appl-accum-last-arg';
	branch = 'eval-dispatch';
	break;
    case 'ev-appl-accum-last-arg':
	argl = restore();
	argl = adjoin_arg(val, argl);
	proc = restore();
	branch = 'apply-dispatch';
	break;
    case 'apply-dispatch':
	if (primitive_procedure(proc)) {
	    // primitive-apply
	    val = apply_primitive_procedure(proc, argl);
	    continue_to = restore();
	    branch = continue_to;
	} else if (compound_procedure(proc)) {
	    branch = 'compound-apply';
	} else {
	    branch = 'unknown-procedure-type';
	}
	break;
    case 'compound-apply':
	unev = procedure_parameters(proc);
	env = procedure_environment(proc);
	env = extend_environment(unev, argl, env);
	unev = procedure_body(proc);
	branch = 'ev-sequence';
	break;
    case 'ev-begin':
	unev = begin_actions(exp);
	save(continue_to);
	branch = 'ev-sequence';
	break;
    case 'ev-sequence':
	exp = first_exp(unev);
	if (last_exp(unev)) {
	    branch = 'ev-sequence-last-exp';
	} else {
	    save(unev);
	    save(env);
	    continue_to = 'ev-sequence-continue';
	    branch = 'eval-dispatch';
	}
	break;
    case 'ev-sequence-continue':
	env = restore();
	unev = restore();
	unev = rest_exps(unev);
	branch = 'ev-sequence';
	break;
    case 'ev-sequence-last-exp':
	continue_to = restore();
	branch = 'eval-dispatch';
	break;

// ev-if
//   (save exp)
//   (save env)
//   (save continue)
//   (assign continue (label ev-if-decide))
//   (assign exp (op if-predicate) (reg exp))
//   (goto (label eval-dispatch))
// ev-if-decide
//   (restore continue)
//   (restore env)
//   (restore exp)
//   (test (op true?) (reg val))
//   (branch (label ev-if-consequent))
// ev-if-alternative
//   (assign exp (op if-alternative) (reg exp))
//   (goto (label eval-dispatch))
// ev-if-consequent
//   (assign exp (op if-consequent) (reg exp))
//   (goto (label eval-dispatch))

    case 'ev-assignment':
	unev = assignment_variable(exp);
	save(unev);
	exp = assignment_value(exp);
	save(env);
	save(continue_to);
	continue_to = 'ev-assignment-1';
	branch = 'eval-dispatch';
	break;
    case 'ev-assignment-1':
	continue_to = restore();
	env = restore();
	unev = restore();
	set_variable_value(unev, val, env);
	val = 'ok';
	branch = continue_to;
	break;
	
    case 'signal-error':
	val = 'ERROR: ' + val;
	branch = 'done';
	break;
    default:
	console.log('INTERPRETER ERROR: unknown branch: ' + branch);
	branch = 'done';
    }
}



// ev-definition
//   (assign unev (op definition-variable) (reg exp))
//   (save unev)
//   (assign exp (op definition-value) (reg exp))
//   (save env)
//   (save continue)
//   (assign continue (label ev-definition-1))
//   (goto (label eval-dispatch))
// ev-definition-1
//   (restore continue)
//   (restore env)
//   (restore unev)
//   (perform
//    (op define-variable!) (reg unev) (reg val) (reg env))
//   (assign val (const ok))
//   (goto (reg continue))
//    )))



// ---- primitives ----
function cons(a, b) { return [a, b]; }
function car(a) { return a[0]; }
function cdr(a) { return a[1]; }
function cadr(a) { return car(cdr(a)); }
function caddr(a) { return cadr(cdr(a)); }
function cadddr(a) { return caddr(cdr(a)); }
function cddr(a) { return cdr(cdr(a)); }

function last(a) { return cdr(a).length == 0 }

var primitive_operations = {
    '+': function(a,b) { return a + b },
    '-': function(a,b) { return a - b },
    '*': function(a,b) { return a * b },
    '/': function(a,b) { return a / b },
    '=': function(a,b) { return a == b },
    'display': announce_output
};

function primitive_op(exp) {
    return exp[1] in primitive_operations;
}

function primitive_procedure(proc) {
    return tagged_list(proc, 'primitive');
}

function primitive_procedure_proc(exp) {
    return [['symbol','primitive'], [ primitive_operations[exp[1]], [] ] ];
}

function apply_primitive_procedure(proc, argl) {
    return cadr(proc).apply(undefined, scheme_to_js_style_array(argl));
}

// ---- Syntax ----
function self_evaluating(exp) {
    return exp[0] == 'number'; // also handle: || exp[0] == 'string';
}

function self_evaluated(exp) {
    return exp[0] == 'number' ? parseFloat(exp[1]) : exp[1];
}
	    
function symbol(exp) {
    return exp[0] == 'symbol';
}

function symbol_name(exp) {
    return exp[1];
}

function variable(exp) {
    return symbol(exp);
}

function pair(exp) {
    return (exp instanceof Array) && !is_token(exp) && (exp.length == 2);
}

function tagged_list(exp, tag) {
    return pair(exp) && symbol(car(exp)) && symbol_name(car(exp)) == tag;
}

function assignment(exp) { 
    return tagged_list(exp, 'set!'); 
}

function assignment_variable(exp) {
    return symbol_name(cadr(exp));
}

function assignment_value(exp) {
    return caddr(exp);
}

function definition(exp) { return tagged_list(exp, 'define'); }
function if_exp(exp) { return tagged_list(exp, 'if'); }
function lambda(exp) { return tagged_list(exp, 'lambda'); }
var lambda_parameters = cadr;
var lambda_body = cddr;

function make_procedure(parameters, body, env) {
    // todo: consider simplifying this internal representation 
    return [['symbol','procedure'], [parameters, [body, [env, []]]]];
}
var procedure_parameters = cadr;
var procedure_body = caddr;
var procedure_environment = cadddr;

function begin(exp) { return tagged_list(exp, 'begin'); }
var begin_actions = cdr;
var first_exp = car;
var rest_exps = cdr;
var last_exp = last;

function application(exp) { return pair(exp); }
var operator = car;
var operands = cdr;
function no_operands(exp) { return exp.length == 0; }
var first_operand = car;
var rest_operands = cdr;
var last_operand = last;

function quoted(exp) {
    return tagged_list(exp,'quote');
}

function text_of_quotation(exp) {
    return cadr(exp)[1];
}

// ---- support functions (analogous to eceval-support) ----
function adjoin_arg(arg, arglist) {
    if (arglist.length == 0) {
	return [arg, []];
    } else {
	var dupe = arglist.slice(0);
	dupe[dupe.length - 1] = [arg, []];
	return dupe;
    }
}

// ---- functions analogous to mceval ----
function compound_procedure(proc) {
    return tagged_list(proc, 'procedure');
}

// ---- misc. functions relating to parsing ----
function ast_to_js_style_array(a) { 
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

function scheme_to_js_style_array(a) {
    var o = [];
    while (a.length > 0) {
	o.push(a[0]);
	a = a[1];
    }
    return o;
}

// debugging statements (okay to be recursive, not tail-call-optimized)
function is_scheme_style_list(a) {
    return ((a instanceof Array) && (a.length == 2) && is_scheme_style_list(a[1]))
}

function is_token(exp) {
    return (exp instanceof Array) && !(exp[0] instanceof Array);
}

function is_nil_syntax_tree(exp) {
    return (exp instanceof Array) && exp[0]==undefined;
}

function stringify_abstract_syntax_tree(exp, skip_parens) {
    if (exp == undefined) {
	return 'fail';
    } else if (is_token(exp)) {
	return is_nil_syntax_tree(exp) ? 'nil' : exp[0] + '/' + exp[1];
    } else {
	var o = stringify_abstract_syntax_tree(exp[0]);
	if (!is_nil_syntax_tree(exp[1])) {
	    o += (is_token(exp[1]) ? ' . ' : ' ') + stringify_abstract_syntax_tree(exp[1], 'skip');
	}
	return skip_parens ? o : '(' + o + ')';
    }
}

function stringify_scheme_exp(exp, skip_parens) {
    if (exp == undefined) {
	return 'nil';
    } else if (!(exp instanceof Array)) {
	return exp;
    } else {
	var o = stringify_scheme_exp(exp[0]);
	if (exp[1] !== undefined) {
	    o += (is_token(exp[1]) ? ' . ' : ' ') + stringify_scheme_exp(exp[1], 'skip_parens');
	}
	return skip_parens ? o : '(' + o + ')';
    }
}

// when we reach the + we want to do it, when we reach the + 1 2
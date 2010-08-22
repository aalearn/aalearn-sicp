
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
    

var user_stop = undefined;

function receive_input(element) {
    element.attr('contentEditable', false);
    $('.input .history-selection').removeClass('history-selection');
    var input_expression = parse(tokenize(element.text()));
    user_stop = undefined;
    while (input_expression.length > 0) {
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

// --- Stack ---
// stored as a javascript array
var stack = [];
function save(x) { stack.push(x) }
function restore() { return stack.pop(); }
function debug_stack() { console.log(stack) }

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
    env.unshift(make_frame(variables, values));
    return env;
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
function evaluate(exp) {
    return eceval(exp);
    if (self_evaluating(exp)) {
	return parseFloat(exp[1]);
    } else if (symbol(exp)) {
	// check if defined locally
	if (primitive_operation(exp)) {
	    return primitive_operation_function(exp);
	}
    } else if (exp[0] instanceof Array) {
	console.log(exp);
	console.log(stringify_abstract_syntax_tree(exp));

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


 // only variables stored in 'token' metadata form, rest are in 'simple' form
var exp;
var unev;

var branch;
var continue_to;
var val;

function eceval(ast) {
    continue_to = 'done';
    branch = 'eval-dispatch';
    val = undefined;
    exp = ast;
    var count = 0;
    while (branch !== 'done') {
	console.log(branch);
	eceval_step();
	if (count++ > 29) {
	    branch = 'done';
	    console.log('infinite loop guard');
	}
    }
    return val;
}

function eceval_step() {
    switch(branch) {
 
   case 'eval-dispatch':
	console.log(exp);
	if (self_evaluating(exp)) {
	    val = self_evaluated(exp);
	    branch = continue_to;
	} else if (variable(exp)) {
	    branch = 'ev-variable';
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

    case 'ev-variable':
	val = lookup_variable_value(symbol_name(exp), env);
	if (val == unbound_variable_error) {
	    val = 'unbound symbol: ' + symbol_name(exp);
	    branch = 'signal-error';
	}
	branch = continue_to;
	break;

// ev-variable
//   (assign val (op lookup-variable-value) (reg exp) (reg env))
//   (test (op unbound-variable-error?) (reg val))
//   (branch (label signal-error))
//   (goto (reg continue))
// ev-quoted
//   (assign val (op text-of-quotation) (reg exp))
//   (goto (reg continue))
// ev-lambda
//   (assign unev (op lambda-parameters) (reg exp))
//   (assign exp (op lambda-body) (reg exp))
//   (assign val (op make-procedure)
//               (reg unev) (reg exp) (reg env))
//   (goto (reg continue))
// ev-self-eval
//   (assign val (reg exp))
//   (goto (reg continue))
// ev-variable
//   (assign val (op lookup-variable-value) (reg exp) (reg env))
//   (test (op unbound-variable-error?) (reg val))
//   (branch (label signal-error))
//   (goto (reg continue))
// ev-quoted
//   (assign val (op text-of-quotation) (reg exp))
//   (goto (reg continue))
// ev-lambda
//   (assign unev (op lambda-parameters) (reg exp))
//   (assign exp (op lambda-body) (reg exp))
//   (assign val (op make-procedure)
//               (reg unev) (reg exp) (reg env))
//   (goto (reg continue))


    case 'ev-application':
	save(continue_to);
	unev = operands(exp);
	exp = operator(exp);
	
	
// ev-application
//   (save continue)
//   (assign unev (op operands) (reg exp))
//   (assign exp (op operator) (reg exp))
//   (test (op symbol?) (reg exp))
//   (branch (label ev-operator-symbol))
//   (save env)
//   (save unev)
//   (assign continue (label ev-appl-did-operator))
//   (goto (label eval-dispatch))
// ev-operator-symbol
//   (assign proc (op lookup-variable-value) (reg exp) (reg env))
//   (test (op unbound-variable-error?) (reg val))
//   (branch (label signal-error))
//   (goto (label ev-appl-did-operator-symbol))

// ev-appl-did-operator
//   (restore unev)
//   (restore env)
//   (assign proc (reg val))
// ev-appl-did-operator-symbol
//   (assign argl (op empty-arglist))
//   (test (op no-operands?) (reg unev))
//   (branch (label apply-dispatch))
//   (test (op compound-procedure?) (reg proc))   ; lazy evaluation
//   (branch (label ev-lazy-appl-operand-loop))   ; "
//   (save proc)
// ev-appl-operand-loop
//   (save argl)
//   (assign exp (op first-operand) (reg unev))
//   (test (op last-operand?) (reg unev))
//   (branch (label ev-appl-last-arg))
//   (save env)
//   (save unev)
//   (assign continue (label ev-appl-accumulate-arg))
//   (goto (label eval-dispatch))
// ev-appl-accumulate-arg
//   (restore unev)
//   (restore env)
//   (restore argl)
//   (assign argl (op adjoin-arg) (reg val) (reg argl))
//   (assign unev (op rest-operands) (reg unev))
//   (goto (label ev-appl-operand-loop))
// ev-appl-last-arg
//   (assign continue (label ev-appl-accum-last-arg))
//   (goto (label eval-dispatch))
// ev-appl-accum-last-arg
//   (restore argl)
//   (assign argl (op adjoin-arg) (reg val) (reg argl))
//   (restore proc)
//   (goto (label apply-dispatch))
// apply-dispatch
//   (test (op primitive-procedure?) (reg proc))
//   (branch (label primitive-apply))
//   (test (op compound-procedure?) (reg proc))  
//   (branch (label compound-apply))
//   (goto (label unknown-procedure-type))

// ; this section for lazy evaluation
// ev-lazy-appl-operand-loop
//   (assign exp (op first-operand) (reg unev))
//   (test (op last-operand?) (reg unev))
//   (branch (label ev-lazy-appl-last-arg))
//   (assign continue (label ev-lazy-appl-accumulate-arg))
// ev-defer-if-necessary
//   (test (op self-evaluating?) (reg exp))
//   (branch (label ev-self-eval))
//   (assign val (op make-thunk) (reg exp) (reg env))
//   (goto (reg continue))
// ev-lazy-appl-last-arg
//   (assign continue (label ev-lazy-appl-accum-last-arg))
//   (goto (label ev-defer-if-necessary))
// ev-lazy-appl-accumulate-arg
//   (assign argl (op adjoin-arg) (reg val) (reg argl))
//   (assign unev (op rest-operands) (reg unev))
//   (goto (label ev-lazy-appl-operand-loop))
// ev-lazy-appl-accum-last-arg
//   (assign argl (op adjoin-arg) (reg val) (reg argl))
//   (goto (label apply-dispatch))

// primitive-apply
//   (assign val (op apply-primitive-procedure)
//               (reg proc)
//               (reg argl))
//   (restore continue)
//   (goto (reg continue))

// compound-apply
//   (assign unev (op procedure-parameters) (reg proc))
//   (assign env (op procedure-environment) (reg proc))
//   (assign env (op extend-environment)
//               (reg unev) (reg argl) (reg env))
//   (assign unev (op procedure-body) (reg proc))
//   (goto (label ev-sequence))
	break;

// ev-begin
//   (assign unev (op begin-actions) (reg exp))
//   (save continue)
//   (goto (label ev-sequence))

// ev-sequence
//   (assign exp (op first-exp) (reg unev))
//   (test (op last-exp?) (reg unev))
//   (branch (label ev-sequence-last-exp))
//   (save unev)
//   (save env)
//   (assign continue (label ev-sequence-continue))
//   (goto (label eval-dispatch))
// ev-sequence-continue
//   (restore env)
//   (restore unev)
//   (assign unev (op rest-exps) (reg unev))
//   (goto (label ev-sequence))
// ev-sequence-last-exp
//   (restore continue)
//   (goto (label eval-dispatch))

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
	

	
// ev-assignment
//   (assign unev (op assignment-variable) (reg exp))
//   (save unev)
//   (assign exp (op assignment-value) (reg exp))
//   (save env)
//   (save continue)
//   (assign continue (label ev-assignment-1))
//   (goto (label eval-dispatch))
// ev-assignment-1
//   (restore continue)
//   (restore env)
//   (restore unev)
//   (perform
//    (op set-variable-value!) (reg unev) (reg val) (reg env))
//   (assign val (const ok))
//   (goto (reg continue))

    case 'signal-error':
	val = 'ERROR: ' + val;
	branch = 'done';
	break;
    default:
	console.log('unknown branch: ' + branch);
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
function cadr(a) { return car(cdr(a)); }
function caddr(a) { return cadr(cdr(a)); }


// ---- Syntax ----
function self_evaluating(exp) {
    return exp[0] == 'number'; // also handle: || exp[0] == 'string';
}

function self_evaluated(exp) {
    return exp[1];
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
function begin(exp) { return tagged_list(exp, 'lambda'); }

function application(exp) {
    return pair(exp);
}

function operator(exp) {
    return symbol_name(car(exp));
}

function operands(exp) {
    return cdr(exp);
}

function no_operands(exp) {
    return exp.length == 0;
}

function first_operands(ops) {
    return car(ops);
}

function rest_operands(ops) {
    return cdr(ops);
}

// ---- misc. functions relating to parsing ----
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

function is_nil_syntax_tree(exp) {
    return (exp instanceof Array) && exp[0]==undefined;
}

function stringify_abstract_syntax_tree(exp, skip_parens) {
    if (exp == undefined) {
	return 'fail';
    } else if (is_token(exp)) {
	return is_nil_syntax_tree(exp) ? 'nil' : exp[0] + '/' + exp[1];
    } else {
	var o = stringify(exp[0]);
	if (!is_nil_syntax_tree(exp[1])) {
	    o += (is_token(exp[1]) ? ' . ' : ' ') + stringify(exp[1], 'skip');
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
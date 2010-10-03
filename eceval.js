// --- Stack ---
// stored as a javascript array
var stack = ['STACK'];
function save(x) { 
    stack.push(x);
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

// different from text: okay to set something previously undefined
function set_variable_value(variable, value, env) {
    for (i = 0, len = env.length; i < len; i++) {
	if (env[i].hasOwnProperty(variable)) {
	    env[i][variable] = value;
	    return;
	}
    }
    env[0][variable] = value;
}

function define_variable(variable, value, env) {
    set_variable_value(variable, value, env);
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

var run_tree, call_stack;
var start_exp, prev_exp;

function evaluate(ast) {
    // console.log($.toJSON(ast));
    continue_to = 'done';
    branch = 'eval-dispatch';
    val = undefined;
    exp = ast;
    var count = 0;
    run_tree = [];
    start_exp = undefined;
    prev_exp = undefined;
    while (branch !== 'done') {
	prev_exp = start_exp;
	start_exp = exp;
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
    // console.log(exp);
    switch(branch) {
	
    case 'eval-dispatch':
	if (typeof(exp) == 'undefined') {
	    val = 'interpreter produced invalid expression after ' + $.toJSON(prev_exp) + ' ' + code_source(prev_exp);
	    branch = 'signal-error';
	} else if (self_evaluating(exp)) {
	    // ev-self-eval
	    val = self_evaluated(exp);
	    branch = continue_to;
	} else if (quoted(exp)) {
	    val = text_of_quotation(exp);
	    branch = continue_to;
	} else if (variable(exp)) { 
	    // ev-variable
	    val = lookup_variable_value(symbol_name(exp), env);
	    if (val == unbound_variable_error) {
		val = 'unbound symbol: ' + symbol_name(exp) + ', ' + code_source(exp);
		branch = 'signal-error';
	    } else {
		branch = continue_to;
	    }
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
	// console.log('-->' + branch);
	break;

    case 'unknown-procedure-type':
	val = 'unknown procedure "' + exp[1] + '" ' + code_source(exp);
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
	var original_expression = exp; // hack for macro_expand!
	unev = operands(exp);
	exp = operator(exp);
	if (symbol(exp)) {
	    // ev-operator-symbol
	    proc = lookup_variable_value(symbol_name(exp), env);

	    if (proc == unbound_variable_error) {
		if (macro(exp)) {
		    exp = macro_expand(original_expression);
		    branch = 'eval-dispatch'; // try again, with mutated exp
		}
		// hacked -- a little different from the text
		else if (primitive_op(exp)) {
		    proc = primitive_procedure_proc(exp);
		    branch = 'ev-appl-did-operator-symbol';
		} else if (symbol_name(exp) == "error") {
		    proc = [['symbol','error'], []];
		    branch = 'ev-appl-did-operator-symbol'; // hacked in
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
	if (error_procedure(proc)) {
	    val = argl.join(' ');
	    branch = 'signal-error';
	} else if (primitive_procedure(proc)) {
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
    case 'ev-if':
	save(exp);
	save(env);
	save(continue_to);
	continue_to = 'ev-if-decide';
	exp = if_predicate(exp);
	branch = 'eval-dispatch';
	break;
    case 'ev-if-decide':
	continue_to = restore();
	env = restore();
	exp = restore();
	if (evaluates_to_true(val)) {
	    exp = if_consequent(exp);
	} else {
	    exp = if_alternative(exp);
	}
	branch = 'eval-dispatch';
	break;

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

    case 'ev-definition':
	unev = definition_variable(exp);
	save(unev);
	exp = definition_value(exp);
	save(env);
	save(continue_to);
	continue_to = 'ev-definition-1';
	branch = 'eval-dispatch';
	break;
    case 'ev-definition-1':
	continue_to = restore();
	env = restore();
	unev = restore();
	define_variable(symbol_name(unev), val, env);
	val = 'ok: ' +  symbol_name(unev);
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

// ---- primitives ----
function cons(a, b) { return [a, b]; }
function car(a) { return a[0]; }
function cdr(a) { return a[1]; }
function caar(a) { return car(car(a)); }
function cadr(a) { return car(cdr(a)); }
function cdar(a) { return cdr(car(a)); }
function cddr(a) { return cdr(cdr(a)); }
function caadr(a) { return car(cadr(a)); }
function caddr(a) { return cadr(cdr(a)); }
function cdadr(a) { return cdr(cadr(a)); }
function cadddr(a) { return caddr(cdr(a)); }

function last(a) { return cdr(a).length == 0 }

function list(a) { return a.length == 0 ? [] : cons(car(a), list(cdr(a))); }
function and(a) { 
    if (a.length == 0) {
	return true;
    } else if (evaluates_to_true(car(a))) {
	return last(a) ? car(a) : and(cdr(a));
    } else {
	return false;
    }
}
function or(a) {
    if (a.length == 0) {
	return false;
    } else if (evaluates_to_true(car(a))) {
	return car(a);
    } else {
	return or(cdr(a));
    }
}
function equal(a) {
    if (a.length == 0 || last(a)) {
	return true;
    } else {
	return car(a) == cadr(a) && equal(cdr(a));
    }
}

var primitive_operations = {
    '+': function(a) { return car(a) + cadr(a) },
    '-': function(a) { return car(a) - cadr(a) },
    '*': function(a) { return car(a) * cadr(a) },
    '/': function(a) { return car(a) / cadr(a) },
    '=': function(a) { return car(a) == cadr(a) },
    '>': function(a) { return car(a) > cadr(a) },
    '<': function(a) { return car(a) < cadr(a) },
    '>=': function(a) { return car(a) >= cadr(a) },
    '<=': function(a) { return car(a) <= cadr(a) },
    'cons': function(a) { return cons(car(a), cadr(a)); },
    'car': function(a) { return car(car(a)) },
    'cdr': function(a) { return cdr(car(a)) },
    'caar': function(a) { return caar(car(a)) },
    'cadr': function(a) { return cadr(car(a)) },
    'cdar': function(a) { return cdar(car(a)) },
    'cddr': function(a) { return cddr(car(a)) },
    'list': list,
    'and': and,
    'or': or,
    'equal?': equal,
    'display': announce_output
};

function primitive_op(exp) {
    return exp[1] in primitive_operations;
}

function error_procedure(proc) {
    return tagged_list(proc, 'error')
}

function primitive_procedure(proc) {
    return tagged_list(proc, 'primitive');
}

function primitive_procedure_proc(exp) {
    return [['symbol','primitive'], [ primitive_operations[exp[1]], [] ] ];
}

function apply_primitive_procedure(proc, argl) {
    return cadr(proc).apply(undefined, [argl]);
}

// ---- Macros ----
function macro(exp) {
    return symbol_name(exp) == 'let' || symbol_name(exp) == 'cond';
}

function macro_expand(exp) {
    if (symbol_name(car(exp)) == "let") {
	return let_to_lambda(exp);
    } else if (symbol_name(exp) == "cond") {
	console.log("error: cond is not implemented");
    } else {
	console.log("error: unable to expand macro!");
    }
}
function let_assignments(a) { return cadr(a) };
function let_body(a) { return cddr(a) }; // is car right?
function scheme_map(f, a) {
    if (car(a)) {
	return [f(car(a)), scheme_map(f, cdr(a))];
    } else {
	return [];
    }
}
function let_to_lambda(a) {
    return [[['symbol','lambda',exp[2]],
	     [ scheme_map(car, let_assignments(a)),
	       let_body(a) ]],
	    car(scheme_map(cdr, let_assignments(a)))];
}

// ---- Syntax ----
function self_evaluating(exp) {
    return exp[0] == 'number' || exp[0] == 'text-literal' 
	|| exp[1] == 'true' || exp[1] == 'false';
}

function self_evaluated(exp) {
    if (exp[0] == 'number') {
	return parseFloat(exp[1]);
    } else if (exp[0] == 'text-literal') {
	return exp[1];
    } else if (exp[1] == 'false') {
	return false;
    } else if (exp[1] == 'true') {
	return true;
    }
}
	    
function symbol(exp) {
    return exp[0] == 'symbol';
}

function symbol_name(exp) {
    return exp[1];
}

function code_source(exp) {
    if (exp[2] == 'repl' || !exp) {
	return 'from repl';
    } else if (exp[2] == 'n/a') {
	return 'n/a';
    } else {
	return ' on line ' + exp[2];
    }
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

function make_lambda(params, body) {
    return [['symbol','lambda'], [ params, body ]];
};
    
function definition(exp) { return tagged_list(exp, 'define'); }
var definition_variable = caadr;
function definition_value(exp) {
    if (symbol(exp)) {
	return caddr(exp);
    } else {
	return make_lambda(cdadr(exp), cddr(exp));
    }
}

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

function application(exp) { return pair(exp) && !is_token(exp); }
var operator = car;
var operands = cdr;
function no_operands(exp) { return exp.length == 0; }
var first_operand = car;
var rest_operands = cdr;
var last_operand = last;

function if_exp(exp) { return tagged_list(exp, 'if'); }
var if_predicate = cadr;
var if_consequent = caddr;
var if_alternative = cadddr;

// placeholder, in case we need to change this
function evaluates_to_true(x) { return x ? true : false }; 

function quoted(exp) {
    return exp[0][1] == 'quote';
}

function text_of_quotation(exp) {
    return text_of_item(cdr(exp));
}

function text_of_item(exp) {
    if (exp instanceof Array && !exp.length) {
	return undefined;
    } else if (is_token(exp) || symbol(exp)) {
	return exp[1];
    } else {
	return [text_of_item(car(exp)), text_of_item(cdr(exp))];
    }
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

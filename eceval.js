// --- Stack ---
// stored as a javascript array
var stack = ['STACK'];
function save(x) { 
    stack.push(x);
}
function restore() { return stack.pop(); }
function debug_stack() { console.log('Stack:'); console.log(stack) }

// enhanced value object wraps all scheme values!
var Value = {
    init: function(val, source_exp) {
	var newObject = Object.create(this);
	newObject.value = val;
	newObject.source_exp = source_exp;
	newObject.wrapped_value = true;
	return newObject;
    },
    with_lookup_exp: function(lookup_exp) {
	this.lookup_exp = lookup_exp;
	return this;
    },
};

// --- Environment ---
// stored as a javascript array of javascript hashes (objects)
var Frame = {
    // ---- basics, utilities ----
    init: function(variables, values, code_source, code_symbol) {
	var newObject = Object.create(this);
	newObject.data = this.make_map(variables, values);
	newObject.code_source = code_source;
	newObject.code_symbol = code_symbol;
	return newObject;
    },
    make_map: function(variables, values) {
	var frame = {};
	for (var i = 0, len = variables.length; i < len; i++) {
	    frame[variables[i]] = Value.init(values[i],'n/a');
	}
	return frame;
    },
    trace_line: function() {
	return this.code_symbol + this.code_source;
    },
};

var env = [Frame.init([],[],' at top-level')];
var unbound_variable_error = ['error', 'unbound_variable'];

function new_env() { return {} };

function extend_environment(variables, values, env, code_source, code_symbol) {
    return [Frame.init(
	$.map(ast_to_js_style_array(variables), symbol_name), 
	ast_to_js_style_array(values),
	code_source, code_symbol)].concat(env);
}

function lookup_variable_value(variable, env, lookup_exp, include_context) {
    for (i = 0, len = env.length; i < len; i++) {
	frame_data = env[i].data;
	if (frame_data.hasOwnProperty(variable)) {
	    if (include_context) {
		// record where the variable was looked up, e.g. for stack trace
		return frame_data[variable].with_lookup_exp(lookup_exp);
	    } else {
		return frame_data[variable].value;
	    }
	}
    }
    return unbound_variable_error;
}

// different from text: okay to set something previously undefined
function set_variable_value(variable, value, env, exp) {
    for (i = 0, len = env.length; i < len; i++) {
	frame_data = env[i].data;
	if (frame_data.hasOwnProperty(variable)) {
	    frame_data[variable] = Value.init(value, exp);
	    return;
	}
    }
    frame_data[variable] = Value.init(value, exp);
}

function define_variable(variable, value, env, exp) {
    set_variable_value(variable, value, env, exp);
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

var proc_call_stack;
var start_exp, prev_exp;

function evaluate(ast) {
    // console.log($.toJSON(ast));
    continue_to = 'done';
    branch = 'eval-dispatch';
    val = undefined;
    exp = ast;
    var count = 0;
    proc_call_stack = [];
    start_exp = undefined;
    prev_exp = undefined;
    while (branch !== 'done') {
	prev_exp = start_exp;
	start_exp = exp;
	eceval_step();
	if (count++ > 12999) {
	    branch = 'done';
	    console.log('infinite loop guard');
	}
    }
    return val;
}

var debug_p;

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
	    val = wrap_self_evaluated(exp);
	    branch = continue_to;
	} else if (quoted(exp)) {
	    val = wrap_text_of_quotation(exp);
	    branch = continue_to;
	} else if (variable(exp)) { 
	    // ev-variable
	    val = lookup_variable_value(symbol_name(exp), env);
	    if (val == unbound_variable_error) {
		if (primitive_op(exp)) {
		    val = primitive_procedure_proc(exp);
		    branch = continue_to;
		} else {
		    val = 'unbound symbol: ' + symbol_name(exp) + ', ' + code_source(exp);
		    branch = 'signal-error';
		}
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
	    proc = lookup_variable_value(symbol_name(exp), env, exp, 'include_context');
	    if (proc == unbound_variable_error) {
		if (macro(exp)) {
		    exp = macro_expand(original_expression);
		    branch = 'eval-dispatch'; // try again, with mutated exp
		} else if (primitive_op(exp)) {
		    // hacked -- a little different from the text
		    proc = primitive_procedure_proc(exp);
		    branch = 'ev-appl-did-operator-symbol';
		} else if (symbol_name(exp) == "apply") {
		    proc = [['symbol','apply',exp[2]],[]];
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
	    val = scheme_to_js_style_array(argl).join(' ');
	    branch = 'signal-error';
	} else if (explicit_apply_procedure(proc)) {
	    proc = car(argl);
	    argl = cadr(argl);
	    branch = 'apply-dispatch';	    
	} else if (primitive_procedure(proc)) {
	    // primitive-apply
	    try {
		proc_call_stack.push(
		    '('
			+ symbol_name(primitive_procedure_exp(proc)) + '[primitive]'
			+ printable_argl(argl)
			+ ') called' 
			+ code_source(primitive_procedure_exp(proc)));

		val = wrap_apply_primitive_procedure(proc, argl, symbol_name(primitive_procedure_exp(proc)));
		continue_to = restore();
		branch = continue_to;
		proc_call_stack.pop();
	    } catch(err) {
		val = 'applying primitive: ' + err;
		branch = 'signal-error';
	    }
	} else if (compound_procedure(proc.wrapped_value ? proc.value : proc)) {
	    branch = 'compound-apply';
	} else {
	    branch = 'unknown-procedure-type';
	}
	break;
    case 'compound-apply':
	// currently this has access to both where/how a function was defined
	//  and where it was called from
	if (proc.wrapped_value) {
	    var calling_exp = proc.lookup_exp;
	    // TODO: replace with richer data structure
	    proc_call_stack.push(
		'(' + 
		    (symbol(calling_exp) 
		     ? symbol_name(calling_exp) 
		     : "[anonymous]")
		    + printable_argl(argl)
		    + ") called " + code_source(calling_exp));
	    proc = proc.value;
	} else {
	    proc_call_stack.push('computed procedure called');
	}

	// TODO: trap wrong arity

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
	continue_to = 'pop-proc-call-stack';
	branch = 'eval-dispatch';
	break;
    case 'pop-proc-call-stack':
	// new branch just for handling proc_call_stack just in case of compound apply's last expressions
	//  when evaluating the last expression in compound apply,
	//  we want the proc_call_stack to still be complete
	//  but once we're done evaluating that expression, we want to get rid of potentially
	//  many entries in the proc_call_stack all at once.
	proc_call_stack.pop();
	continue_to = restore();
	branch = continue_to;
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
	    exp = if_alternative(exp) || ['symbol','false','n/a']; // handle if without else clause
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
	// changed to keep around more of "exp" in order to keep
	// code-source details for better stacktraces
	unev = exp;
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
	var original_define_exp = unev;
	unev = definition_variable(unev);
	define_variable(symbol_name(unev), val, env, original_define_exp);
	val = 'ok: ' +  symbol_name(unev);
	branch = continue_to;
	break;

    case 'signal-error':
	val = '<span class="error-beacon">ERROR: </span> ' + val;
	val = val + "\n* " + proc_call_stack.reverse().join("\n* ");
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

function last(a) { return !cdr(a) || cdr(a).length == 0 }

function list(a) { return a.length == 0 ? undefined : cons(car(a), list(cdr(a))); }
// function list(a) { return a.slice(0) }

function is_null(a) { return !car(a) || car(a).length == 0 }

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

function endlessly(f, a) {
    if (a.length == 0) {
	return 0;
    } else if (last(a)) {
	return car(a);
    } else {
	return endlessly(f, cons(f(car(a), cadr(a)), cddr(a)));
    }
}
	
var primitive_operations = {
    '+': function(a) { return endlessly(function(x,y) {return x + y}, a) },
    '-': function(a) { return endlessly(function(x,y) {return x - y}, a) },
    '*': function(a) { return endlessly(function(x,y) {return x * y}, a) },
    '/': function(a) { return endlessly(function(x,y) {return x / y}, a) },
    '=': function(a) { return car(a) == cadr(a) }, // make the rest smarter too
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
    'pair?': function(a) { return pair(car(a)) },
    'list': list,
    'null?': is_null,
    'and': and,
    'or': or,
    'equal?': equal,
    'eq?': equal,
    'display': announce_output
};

function primitive_op(exp) {
    return exp[1] in primitive_operations;
}

function error_procedure(proc) {
    return tagged_list(proc, 'error')
}

function explicit_apply_procedure(proc) {
    return tagged_list(proc, 'apply');
}

function primitive_procedure(proc) {
    return tagged_list(proc, 'primitive');
}

function primitive_procedure_proc(exp) {
    return [['symbol','primitive'], [ primitive_operations[exp[1]], [exp, []] ] ];
}

function apply_primitive_procedure(proc, argl) {
    return cadr(proc).apply(undefined, [argl]);
}

function primitive_procedure_exp(proc) {
    return car(cddr(proc));
}

// ---- Macros ----
function macro(exp) {
    return symbol_name(exp) == 'let' || symbol_name(exp) == 'cond';
}

function macro_expand(exp) {
    if (symbol_name(car(exp)) == "let") {
	return let_to_lambda(exp);
    } else if (symbol_name(car(exp)) == "cond") {
	return cond_to_if(exp);
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
    return [[['symbol','lambda',a[2]],
	     [ scheme_map(car, let_assignments(a)),
	       let_body(a) ]],
	    car(scheme_map(cdr, let_assignments(a)))];
}

function make_if(predicate, consequent, alternative) {
    return [['symbol','if', 'n/a'], // TODO: this n/a could be fixed
	     [ predicate, [ consequent, [alternative, []]]]];
}

function sequence_to_exp(seq) {
    if (!seq || seq.length == 0) {
	return seq;
    } else if (last(seq)) {
	return car(seq);
    } else {
	return cons(['symbol','begin','n/a'], seq);
    }
}
function expand_clauses(clauses) {
    if (!clauses || clauses.length == 0) {
	return ['symbol','false','n/a'];
    } else if (symbol_name(caar(clauses)) == 'else') {
	if (last(clauses)) {
	    return sequence_to_exp(cdar(clauses));
	} else {
	    console.log('else clause is not last in cond macro');
	    return [];
	}
    } else {
	return make_if(
	    caar(clauses), 
	    sequence_to_exp(cdar(clauses)), 
	    expand_clauses(cdr(clauses)));
    }
}
function cond_to_if(a) {
    var o = expand_clauses(cdr(a));
    return o;
}

// ---- Syntax ----
function self_evaluating(exp) {
    return exp[0] == 'number' || exp[0] == 'text-literal' 
	|| exp[1] == 'true' || exp[1] == 'false'
	|| exp[1] == '#t' || exp[1] == '#f';
}

function self_evaluated(exp) {
    if (exp[0] == 'number') {
	return parseFloat(exp[1]);
    } else if (exp[0] == 'text-literal') {
	return exp[1];
    } else if (exp[1] == 'false' || exp[1] == '#f') {
	return false;
    } else if (exp[1] == 'true' || exp[1] == '#t') {
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
	return ' from repl';
    } else if (exp[2] == 'n/a') {
	return ' n/a';
    } else {
	return ' on <a class="code-source" num="' + exp[2] + '">line ' + exp[2] + "</a>";
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
var procedure_exp = function(p) { return cadddr(cdr(p)); }

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
    return exp[0] == 'quoted-token' || exp[0][1] == 'quote';
}

function text_of_quotation(exp) {
    if (exp[0] == 'quoted-token') {
	return exp[1]; // TODO: convert to Value?
    } else {
	return text_of_item(cdr(exp));
    }
}

function text_of_item(exp) {
    if (exp instanceof Array && !exp.length) {
	return undefined; // our js represenation of nil
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
	var iter = dupe;
	while (!last(iter)) { iter = cdr(iter) }
	iter[1] = [arg, []];
	return dupe;
    }
}

// ---- functions analogous to mceval ----
function compound_procedure(proc) {
    return tagged_list(proc, 'procedure');
}

// ---- debugger support ----
function unwrap_values(argl) {
    return scheme_map(function(x) { return x.value }, argl);
}

function printable_argl(argl) {
    return argl.length > 0 ? ' ' 
    
	+ $.map(scheme_to_js_style_array(unwrap_values(argl)), function(e,i) { 
	    return compound_procedure(e) ? "[compound-procedure]" : stringify_scheme_exp(e);
	}).join(' ')

	: '';
}

function wrap_self_evaluated(exp) {
    return Value.init(self_evaluated(exp), 'self-evaluated' + code_source(exp));
}

function wrap_apply_primitive_procedure(proc, argl, symbol_name) {
    return Value.init(
	apply_primitive_procedure(proc, unwrap_values(argl)),
	'(' + symbol_name + '[primitive] ' + printable_argl(argl) + ')');
}

function wrap_text_of_quotation(exp) {
    return Value.init(text_of_quotation(exp), 'quoted' + code_source(exp));
}

// --- Input/output (eceval-io.js) ---
function announce_output(out) {
    $('#repl').append($('<div class="output" />').html(('' + out).replace(/\n/g,'<br />')));
}


// --- Primitive support (eceval.js) ---
// note: not the same as eceval.js versions, using js-native style arrays everywhere
function car(a) { return a[0] }
function cdr(a) { return a.slice(1) }
function cadr(a) { return a[1]; }
function caddr(a) { return a[2]; }

function sum(a) { return a.length ? a[0] + sum(cdr(a)) : 0; }
function product(a) { return a.length ? a[0] * product(cdr(a)) : 1 }
function difference(a) { return a[0] - a[1] }
function dividend(a) { return a[0] / a[1] }

function lookup_primitive_op(variable) {
    return { 
	'+': sum,
	'*': product,
	'-': difference,
	'/': dividend,
    }[variable];
}
function get_primitive_procedure(variable) {
    return ['primitive', lookup_primitive_op(variable)];
}
function is_tagged_list(a, tag) {
    return (a instanceof Array) && a[0] == tag;
}
function primitive_procedure(p) { return is_tagged_list(p, 'primitive') }

// --- Other procedure support (see ch5-eceval-support.scm) ---
function make_compiled_procedure(entry, env) {
    return ['compiled', entry, env];
}
function compiled_procedure(p) { return is_tagged_list(p, 'compiled') }
function compiled_procedure_entry(p) { return cadr(p) }
function compiled_procedure_env(p) { return caddr(p) }


// could be used to allow compiled code to call interpreted code (ex 5.47)
// not supporting this currently!
function compound_procedure(p) { return false }

function explicit_apply_procedure(p) { return p == 'apply' }

// --- Stack, Environment and data storage (eceval.js) ---
var stack = ['STACK'];
function save(x) { stack.push(x) };
function restore() { return stack.pop() };

var Frame = {
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
	    frame[variables[i]] = values[i]; // was: Value.init(values[i],'n/a');
	}
	return frame;
    },
    trace_line: function() {
	return this.code_symbol + this.code_source;
    },
};

var env = [Frame.init([],[],' at top-level')];
var unbound_variable_error = ['error', 'unbound_variable'];
function extend_environment(variables, values, env, code_source, code_symbol) {
    var ee = [Frame.init(variables, values)].concat(env);
    return ee;
}

// variables -- note, some cruft (include_context, etc.)
function lookup_variable_value(variable, env, lookup_exp, include_context) {
    for (i = 0, len = env.length; i < len; i++) {
	frame_data = env[i].data;
	if (frame_data.hasOwnProperty(variable)) {
	    if (include_context) {
		// record where the variable was looked up, e.g. for stack trace
		return frame_data[variable].with_lookup_exp(lookup_exp);
	    } else {
		return frame_data[variable];
	    }
	}
    }
    if (lookup_primitive_op(variable)) {
	return get_primitive_procedure(variable);
    }
    return unbound_variable_error;
}

// different from text: okay to set something previously undefined
function set_variable_value(variable, value, env, exp) {
    for (i = 0, len = env.length; i < len; i++) {
	frame_data = env[i].data;
	if (frame_data.hasOwnProperty(variable)) {
	    frame_data[variable] = value; // was: Value.init(value, exp);
	    return;
	}
    }
    env[0].data[variable] = value; // was: Value.init(value, exp);
}
define_variable = set_variable_value;


// --- Machine loop ---
var exp;
var unev;

var argl;
var branch;
var continue_to;
var val;
var proc;

var compapp; // NOTE: shouldn't ever be used; unsupported!

function machine_loop() {
    branch = 'main';
    var count = 0;
    while(branch !== 'done') {
	console.log(branch);
	console.log(argl);
	step();
	if (count++ > 12399) {
	    branch = 'done';
	    console.log('infinite loop guard');
	}
    }
    announce_output(val);
}

$(document).ready(function () {
    machine_loop();
});
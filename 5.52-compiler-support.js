
// --- Input/output (eceval-io.js) ---
function announce_output(out) {
    $('#repl').append($('<div class="output" />').html(('' + out).replace(/\n/g,'<br />')));
}


// --- Primitive support (eceval.js) ---
// note: not the same as eceval.js versions, using js-native style arrays everywhere
function car(a) { return a[0] }
function cdr(a) { return a.slice(1) }

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
function primitive_procedure(p) {
    return (p instanceof Array) && p[0] == 'primitive';
}


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

// variables
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
	    frame_data[variable] = Value.init(value, exp);
	    return;
	}
    }
    env[0].data[variable] = Value.init(value, exp);
}


// --- Machine loop ---
var exp;
var unev;

var argl;
var branch;
var continue_to;
var val;
var proc;

function machine_loop() {
    branch = 'main';
    var count = 0;
    while(branch !== 'done') {
	// console.log(branch);
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
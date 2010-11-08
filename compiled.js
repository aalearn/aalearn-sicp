function step() {
switch (branch) {
case 'main':

val = make_compiled_procedure('entry532', env);
branch = 'after-lambda531';
break;
;
case 'entry532':
env = compiled_procedure_env(proc);
env = extend_environment('n', argl, env);
;
save(continue_to);
save(env);
proc = lookup_variable_value('<', env);
;
;
val = 2;
;
;
argl = [val];
;
val = lookup_variable_value('n', env);
;
;
argl.unshift(val);
;
;
case 'compile-procedure-call-start571':
;
if (primitive_procedure(proc)) {
  branch = 'primitive-branch569';
  break;
} else if (compound_procedure(proc)) {
  branch = 'interpreted-branch567';
  break;
} else if (explicit_apply_procedure(proc)) {
  branch = 'explicit-apply-branch570';
  break;
}
;
;
case 'compiled-branch568':
;
continue_to = 'after-call566';
val = compiled_procedure_entry(proc);
branch = val;
break;
;
case 'interpreted-branch567':
;
continue_to = 'after-call566';
save(continue_to);
branch = compapp;
break;
;
case 'explicit-apply-branch570':
;
proc = explicit_apply_procedure(proc);
// argl? = explicit-apply-args argl;
branch = compile-procedure-call-start571l;
break;
;
case 'primitive-branch569':
;
val = proc[1](argl);
;
;
;
case 'after-call566':
;
;
;
env = restore();
continue_to = restore();
;
branch = val ? 'true-branch535' : 'false-branch534';
break;
case 'true-branch535':
;
val = 1;
branch = continue_to;
break;
;
;
case 'false-branch534':
;
proc = lookup_variable_value('+', env);
;
;
save(continue_to);
save(proc);
save(env);
proc = lookup_variable_value('fib', env);
;
;
save(proc);
proc = lookup_variable_value('-', env);
;
;
val = 2;
;
;
argl = [val];
;
val = lookup_variable_value('n', env);
;
;
argl.unshift(val);
;
;
case 'compile-procedure-call-start553':
;
if (primitive_procedure(proc)) {
  branch = 'primitive-branch551';
  break;
} else if (compound_procedure(proc)) {
  branch = 'interpreted-branch549';
  break;
} else if (explicit_apply_procedure(proc)) {
  branch = 'explicit-apply-branch552';
  break;
}
;
;
case 'compiled-branch550':
;
continue_to = 'after-call548';
val = compiled_procedure_entry(proc);
branch = val;
break;
;
case 'interpreted-branch549':
;
continue_to = 'after-call548';
save(continue_to);
branch = compapp;
break;
;
case 'explicit-apply-branch552':
;
proc = explicit_apply_procedure(proc);
// argl? = explicit-apply-args argl;
branch = compile-procedure-call-start553l;
break;
;
case 'primitive-branch551':
;
val = proc[1](argl);
;
;
;
case 'after-call548':
;
;
;
;
argl = [val];
proc = restore();
;
case 'compile-procedure-call-start559':
;
if (primitive_procedure(proc)) {
  branch = 'primitive-branch557';
  break;
} else if (compound_procedure(proc)) {
  branch = 'interpreted-branch555';
  break;
} else if (explicit_apply_procedure(proc)) {
  branch = 'explicit-apply-branch558';
  break;
}
;
;
case 'compiled-branch556':
;
continue_to = 'after-call554';
val = compiled_procedure_entry(proc);
branch = val;
break;
;
case 'interpreted-branch555':
;
continue_to = 'after-call554';
save(continue_to);
branch = compapp;
break;
;
case 'explicit-apply-branch558':
;
proc = explicit_apply_procedure(proc);
// argl? = explicit-apply-args argl;
branch = compile-procedure-call-start559l;
break;
;
case 'primitive-branch557':
;
val = proc[1](argl);
;
;
;
case 'after-call554':
;
;
;
;
argl = [val];
env = restore();
;
save(argl);
proc = lookup_variable_value('fib', env);
;
;
save(proc);
proc = lookup_variable_value('-', env);
;
;
val = 1;
;
;
argl = [val];
;
val = lookup_variable_value('n', env);
;
;
argl.unshift(val);
;
;
case 'compile-procedure-call-start541':
;
if (primitive_procedure(proc)) {
  branch = 'primitive-branch539';
  break;
} else if (compound_procedure(proc)) {
  branch = 'interpreted-branch537';
  break;
} else if (explicit_apply_procedure(proc)) {
  branch = 'explicit-apply-branch540';
  break;
}
;
;
case 'compiled-branch538':
;
continue_to = 'after-call536';
val = compiled_procedure_entry(proc);
branch = val;
break;
;
case 'interpreted-branch537':
;
continue_to = 'after-call536';
save(continue_to);
branch = compapp;
break;
;
case 'explicit-apply-branch540':
;
proc = explicit_apply_procedure(proc);
// argl? = explicit-apply-args argl;
branch = compile-procedure-call-start541l;
break;
;
case 'primitive-branch539':
;
val = proc[1](argl);
;
;
;
case 'after-call536':
;
;
;
;
argl = [val];
proc = restore();
;
case 'compile-procedure-call-start547':
;
if (primitive_procedure(proc)) {
  branch = 'primitive-branch545';
  break;
} else if (compound_procedure(proc)) {
  branch = 'interpreted-branch543';
  break;
} else if (explicit_apply_procedure(proc)) {
  branch = 'explicit-apply-branch546';
  break;
}
;
;
case 'compiled-branch544':
;
continue_to = 'after-call542';
val = compiled_procedure_entry(proc);
branch = val;
break;
;
case 'interpreted-branch543':
;
continue_to = 'after-call542';
save(continue_to);
branch = compapp;
break;
;
case 'explicit-apply-branch546':
;
proc = explicit_apply_procedure(proc);
// argl? = explicit-apply-args argl;
branch = compile-procedure-call-start547l;
break;
;
case 'primitive-branch545':
;
val = proc[1](argl);
;
;
;
case 'after-call542':
;
;
;
argl = restore();
;
argl.unshift(val);
;
proc = restore();
continue_to = restore();
;
case 'compile-procedure-call-start565':
;
if (primitive_procedure(proc)) {
  branch = 'primitive-branch563';
  break;
} else if (compound_procedure(proc)) {
  branch = 'interpreted-branch561';
  break;
} else if (explicit_apply_procedure(proc)) {
  branch = 'explicit-apply-branch564';
  break;
}
;
;
case 'compiled-branch562':
;
val = compiled_procedure_entry(proc);
branch = val;
break;
;
case 'interpreted-branch561':
;
save(continue_to);
branch = compapp;
break;
;
case 'explicit-apply-branch564':
;
proc = explicit_apply_procedure(proc);
// argl? = explicit-apply-args argl;
branch = compile-procedure-call-start565l;
break;
;
case 'primitive-branch563':
;
val = proc[1](argl);
branch = continue_to;
break;
;
;
case 'after-call560':
;
;
;
;
;
after-if533;
;
;
;
case 'after-lambda531':
;
;
define_variable('fib', val, env);
val = 'ok: fib set';
;
;
;
;
proc = lookup_variable_value('fib', env);
;
;
val = 11;
;
;
argl = [val];
;
case 'compile-procedure-call-start530':
;
if (primitive_procedure(proc)) {
  branch = 'primitive-branch528';
  break;
} else if (compound_procedure(proc)) {
  branch = 'interpreted-branch526';
  break;
} else if (explicit_apply_procedure(proc)) {
  branch = 'explicit-apply-branch529';
  break;
}
;
;
case 'compiled-branch527':
;
continue_to = 'after-call525';
val = compiled_procedure_entry(proc);
branch = val;
break;
;
case 'interpreted-branch526':
;
continue_to = 'after-call525';
save(continue_to);
branch = compapp;
break;
;
case 'explicit-apply-branch529':
;
proc = explicit_apply_procedure(proc);
// argl? = explicit-apply-args argl;
branch = compile-procedure-call-start530l;
break;
;
case 'primitive-branch528':
;
val = proc[1](argl);
;
;
;
case 'after-call525':
;
;
;
;


branch = 'done';
break;
case 'unbound_variable':
case 'unbound-variable':
  val = 'unbound-variable!';
  branch = 'signal-error';
case 'signal-error':
  val = 'ERROR: ' + val;
  branch = 'done';
  break;
default:
  val = 'COMPILER ERROR: bad-branch: ' + branch;
  break;
} }

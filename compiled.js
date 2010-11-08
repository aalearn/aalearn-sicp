function step() {
switch (branch) {
case 'main':

  val = make_compiled_procedure('entry1096', env);
  branch = 'after-lambda1095';
break;
case 'entry1096':
  env = compiled_procedure_env(proc);
  env = extend_environment('n', argl, env);
  save(continue_to);
  save(env);
  proc = lookup_variable_value('<', env);
  val = 2;
  argl = [val];
  val = lookup_variable_value('n', env);
  argl.unshift(val);
case 'compile-procedure-call-start1135':
  if (primitive_procedure(proc)) {
    branch = 'primitive-branch1133';
    break;
  } else if (compound_procedure(proc)) {
    branch = 'interpreted-branch1131';
    break;
  } else if (explicit_apply_procedure(proc)) {
    branch = 'explicit-apply-branch1134';
    break;
  }
case 'compiled-branch1132':
  continue_to = 'after-call1130';
  branch = compiled_procedure_entry(proc);
  break;
case 'interpreted-branch1131':
  continue_to = 'after-call1130';
  save(continue_to);
  branch = compapp;
  break;
case 'explicit-apply-branch1134':
  proc = explicit_apply_procedure(proc);
  // argl? = explicit-apply-args argl;
  branch = compile-procedure-call-start1135l;
  break;
case 'primitive-branch1133':
  val = proc[1](argl);
case 'after-call1130':
  env = restore();
  continue_to = restore();
  branch = val ? 'true-branch1099' : 'false-branch1098';
  break;
case 'true-branch1099':
  val = 1;
  branch = continue_to;
break;
case 'false-branch1098':
  proc = lookup_variable_value('+', env);
  save(continue_to);
  save(proc);
  save(env);
  proc = lookup_variable_value('fib', env);
  save(proc);
  proc = lookup_variable_value('-', env);
  val = 2;
  argl = [val];
  val = lookup_variable_value('n', env);
  argl.unshift(val);
case 'compile-procedure-call-start1117':
  if (primitive_procedure(proc)) {
    branch = 'primitive-branch1115';
    break;
  } else if (compound_procedure(proc)) {
    branch = 'interpreted-branch1113';
    break;
  } else if (explicit_apply_procedure(proc)) {
    branch = 'explicit-apply-branch1116';
    break;
  }
case 'compiled-branch1114':
  continue_to = 'after-call1112';
  branch = compiled_procedure_entry(proc);
  break;
case 'interpreted-branch1113':
  continue_to = 'after-call1112';
  save(continue_to);
  branch = compapp;
  break;
case 'explicit-apply-branch1116':
  proc = explicit_apply_procedure(proc);
  // argl? = explicit-apply-args argl;
  branch = compile-procedure-call-start1117l;
  break;
case 'primitive-branch1115':
  val = proc[1](argl);
case 'after-call1112':
  argl = [val];
  proc = restore();
case 'compile-procedure-call-start1123':
  if (primitive_procedure(proc)) {
    branch = 'primitive-branch1121';
    break;
  } else if (compound_procedure(proc)) {
    branch = 'interpreted-branch1119';
    break;
  } else if (explicit_apply_procedure(proc)) {
    branch = 'explicit-apply-branch1122';
    break;
  }
case 'compiled-branch1120':
  continue_to = 'after-call1118';
  branch = compiled_procedure_entry(proc);
  break;
case 'interpreted-branch1119':
  continue_to = 'after-call1118';
  save(continue_to);
  branch = compapp;
  break;
case 'explicit-apply-branch1122':
  proc = explicit_apply_procedure(proc);
  // argl? = explicit-apply-args argl;
  branch = compile-procedure-call-start1123l;
  break;
case 'primitive-branch1121':
  val = proc[1](argl);
case 'after-call1118':
  argl = [val];
  env = restore();
  save(argl);
  proc = lookup_variable_value('fib', env);
  save(proc);
  proc = lookup_variable_value('-', env);
  val = 1;
  argl = [val];
  val = lookup_variable_value('n', env);
  argl.unshift(val);
case 'compile-procedure-call-start1105':
  if (primitive_procedure(proc)) {
    branch = 'primitive-branch1103';
    break;
  } else if (compound_procedure(proc)) {
    branch = 'interpreted-branch1101';
    break;
  } else if (explicit_apply_procedure(proc)) {
    branch = 'explicit-apply-branch1104';
    break;
  }
case 'compiled-branch1102':
  continue_to = 'after-call1100';
  branch = compiled_procedure_entry(proc);
  break;
case 'interpreted-branch1101':
  continue_to = 'after-call1100';
  save(continue_to);
  branch = compapp;
  break;
case 'explicit-apply-branch1104':
  proc = explicit_apply_procedure(proc);
  // argl? = explicit-apply-args argl;
  branch = compile-procedure-call-start1105l;
  break;
case 'primitive-branch1103':
  val = proc[1](argl);
case 'after-call1100':
  argl = [val];
  proc = restore();
case 'compile-procedure-call-start1111':
  if (primitive_procedure(proc)) {
    branch = 'primitive-branch1109';
    break;
  } else if (compound_procedure(proc)) {
    branch = 'interpreted-branch1107';
    break;
  } else if (explicit_apply_procedure(proc)) {
    branch = 'explicit-apply-branch1110';
    break;
  }
case 'compiled-branch1108':
  continue_to = 'after-call1106';
  branch = compiled_procedure_entry(proc);
  break;
case 'interpreted-branch1107':
  continue_to = 'after-call1106';
  save(continue_to);
  branch = compapp;
  break;
case 'explicit-apply-branch1110':
  proc = explicit_apply_procedure(proc);
  // argl? = explicit-apply-args argl;
  branch = compile-procedure-call-start1111l;
  break;
case 'primitive-branch1109':
  val = proc[1](argl);
case 'after-call1106':
  argl = restore();
  argl.unshift(val);
  proc = restore();
  continue_to = restore();
case 'compile-procedure-call-start1129':
  if (primitive_procedure(proc)) {
    branch = 'primitive-branch1127';
    break;
  } else if (compound_procedure(proc)) {
    branch = 'interpreted-branch1125';
    break;
  } else if (explicit_apply_procedure(proc)) {
    branch = 'explicit-apply-branch1128';
    break;
  }
case 'compiled-branch1126':
  branch = compiled_procedure_entry(proc);
  break;
case 'interpreted-branch1125':
  save(continue_to);
  branch = compapp;
  break;
case 'explicit-apply-branch1128':
  proc = explicit_apply_procedure(proc);
  // argl? = explicit-apply-args argl;
  branch = compile-procedure-call-start1129l;
  break;
case 'primitive-branch1127':
  val = proc[1](argl);
  branch = continue_to;
break;
case 'after-call1124':
case 'after-if1097':
case 'after-lambda1095':
  define_variable('fib', val, env);
  val = 'ok: fib set';
  proc = lookup_variable_value('fib', env);
  val = 13;
  argl = [val];
case 'compile-procedure-call-start1094':
  if (primitive_procedure(proc)) {
    branch = 'primitive-branch1092';
    break;
  } else if (compound_procedure(proc)) {
    branch = 'interpreted-branch1090';
    break;
  } else if (explicit_apply_procedure(proc)) {
    branch = 'explicit-apply-branch1093';
    break;
  }
case 'compiled-branch1091':
  continue_to = 'after-call1089';
  branch = compiled_procedure_entry(proc);
  break;
case 'interpreted-branch1090':
  continue_to = 'after-call1089';
  save(continue_to);
  branch = compapp;
  break;
case 'explicit-apply-branch1093':
  proc = explicit_apply_procedure(proc);
  // argl? = explicit-apply-args argl;
  branch = compile-procedure-call-start1094l;
  break;
case 'primitive-branch1092':
  val = proc[1](argl);
case 'after-call1089':


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

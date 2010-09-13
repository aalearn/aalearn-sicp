$(document).ready(function () {
    install_page_level_bindings();
    announce_html_output("<b>JQuery-Scheme Interpreter</b><br>Interpreter loaded.");
    wait_for_input(true);
});

function announce_html_output(out) {
    $('#repl').append($('<div class="output" />').html(out));
}

function announce_output(out) {
    $('#repl').append($('<div class="output" />').text(out));
}

function wait_for_input(focus_on_repl) {
    var new_input = $('<div class="input" contenteditable="true" />');
    $('#repl').append('<div class="prompt">&raquo;&nbsp;</div>').append(new_input);
    new_input.addBindings()
    if (focus_on_repl) {
	new_input.focus();
    }
}

$.fn.addBindings = function() {
    return this.bind('keydown','return', function() {
	$('.input').unbind('keydown'); // disable all others
	receive_input($(this).text(), true);
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

function receive_input(input, from_repl) {
    $('.input').last().attr('contentEditable', false);
    $('.input .history-selection').removeClass('history-selection');
    var input_expression = parse(tokenize(input, from_repl));
    user_stop = undefined;
    while (input_expression && input_expression.length > 0) {
	announce_output(stringify_scheme_exp(evaluate(car(input_expression))));
	input_expression = cdr(input_expression);
    }
    wait_for_input(from_repl);
}

function qeval(exp) { // quick eval of a string for debugging purposes
    return evaluate(parse(tokenize(exp))[0]);
}


function install_page_level_bindings() {
    $(document).bind('keydown','ctrl+c', function() {
	// currently not-enabled -- needs to set user_stop
	// eceval also needs to allow this event to run in between iterations
    });
    $('#repl, #footer').bind('click', function() {
	$('.input').last().focus();
    });
}

// --- Buffer-related ---
$(document).ready(function () {
    fix_buffer_display();
    $('#buffer').bind('keyup', function() { fix_line_numbering() });

    $('#content').bind('keydown', 'alt+r', function() {
	receive_input($('#buffer').html());
	return false;
    });
});

function fix_buffer_display() {
    var buffer_lines = $('#buffer').text().split(/\n/).slice(1);
    buffer_lines = $.map(buffer_lines, function(x) { return x.replace(/ /g, '&nbsp;') })
    $('#buffer').html(buffer_lines.join('<br/>'));
    fix_line_numbering(buffer_lines.length - 1);

}

function fix_line_numbering(len) {
    if (!len) {
	// contenteditable appears to insert divs if there's content,
	// and insert brs for whitespace only lines
	len = $('#buffer br').length;
	$('#buffer div').each(function(i, e) {
	    if (!$(e).find('br').length) len++
	});
    }
    var line_numbering = '';
    for (i = 1; i <= len; i++) {
	line_numbering += i + '<br/>';
    }
    $('#line-numbers').html(line_numbering);
}
/**
 * 
 */

var show;

$(function() {
	$(".menu-left ").click(function(){
		$( this ).toggle('slide');
	});
});

$(function(){
	$( ".menuB" ).click( function() {
		$( ".menu-left").toggle('slide');
	});
});

$(function(){
	$( "a" ).click(function(e) {
		e.stopPropagation();
	});
});
$(function(){
	$( ".has-sub" ).click(function( e ){
	    $( this ).siblings( ).children("ul").hide();
		$(  this ).children( "ul" ).show('slide', {direction: 'up'});

		  e.stopImmediatePropagation();
	  

	});
});
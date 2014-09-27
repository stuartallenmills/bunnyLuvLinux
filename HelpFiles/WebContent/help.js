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
		$( ".menu-left").toggle('slide');
		var hr = $(this).attr("href");
		hr = hr.substring(1, hr.length);
		var targ = $( "a[name='"+hr+"']" );
		var p= targ.parents('div');
		var f= p.first();
		var sib = f.siblings('div');
		alert("parents="+p+ "  length="+p.length+" name="+hr+" array="+$.isArray(p)+" text="+f.text());
		$(p).show();
	    $(sib).hide();
		e.stopPropagation();
	});
});
$(function(){
	$( ".has-sub" ).click(function( e ){
	    $( this ).siblings( ).children("ul").hide();
	    $( this ).siblings().removeClass("active");
		$(  this ).children( "ul" ).show('slide', {direction: 'up'});
		$( this ).addClass("active");

		  e.stopImmediatePropagation();
	  

	});
});
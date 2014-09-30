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

$(function () {
	$( "#leftD[a]").click(function(){
		alert("click ban a");
	});
});

$(function(){
	$( "a" ).click(function(e) {
		$( ".menu-left").hide('slide');
		var hr = $(this).attr("href");
		hr = hr.substring(1, hr.length);
		var targ = $( "a[name='"+hr+"']" );
		var p= targ.parents('div');
		var f= p.first();
		var l = $(this).parents('div').last();
		var pdev;
		var ln = l.attr("id");
		   if (ln=="content") {
			    pdev = $(this).parents("div").first();
			    
		   } else {
			   pdev = f.prev('div');
		   }
		var sib = f.siblings('div');
		var ndev = f.next( 'div');
		var na = ndev.find( 'a');
		var nm = na.first().text();
		var nn = na.first().attr("name");
		$( "#rightD a").attr('href', "#"+nn);
		$( "#rightD a" ).text(nm);
		
		var pa = pdev.find('a');
		var pam=pa.first().text();
		var pan=pa.first().attr("name");
		
		$( "#leftD a").attr('href', "#"+pan);
		$( "#leftD a").text(pam);
		
		targ.css('padding-top', '70px');
//		alert("nm="+nm+ "  na="+na+" name="+hr+" ndev="+ndev.text()+" text="+f.text());
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
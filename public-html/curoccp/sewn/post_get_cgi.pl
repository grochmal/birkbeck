#!/usr/bin/env perl

print "Content-type: text/html

<head>
<title> Text area example</title>
</head>
<body>
<hr>
<h1> Text area example</h1>
<hr>
<p>I read with interest your review. It said:
<blockquote>
";

%incoming = &read_input;  # Read information into associated
                          # array %incoming.
$your_text = $incoming{'review'};  # Fetch the text from the array.
print $your_text;                  # Print the text.

print "</blockquote>

I think you're wrong, but you're entitled to your opinion, I suppose.

</body>
";

sub read_input {
    local ($buffer, @pairs, $pair, $name, $value, %FORM);
    # Read in text
    $ENV{'REQUEST_METHOD'} =~ tr/a-z/A-Z/;
    if ($ENV{'REQUEST_METHOD'} eq "POST") {
	read(STDIN, $buffer, $ENV{'CONTENT_LENGTH'});
    } else {  # REQUEST_METHOD is "GET"
	$buffer = $ENV{'QUERY_STRING'};
    }
    # Split information into name/value pairs
    @pairs = split(/&/, $buffer);
    foreach $pair (@pairs) {
	($name, $value) = split(/=/, $pair);
	$value =~ tr/+/ /;
	$value =~ s/%(..)/pack("C", hex($1))/eg;
	$FORM{$name} = $value;
    }
    %FORM;
}


true;

sub printoffset($\@\@\@)
{
    my ($offset, $max, $bitsize, $aoffset) = @_;

    print "        int offset = $offset;\n";
    for ($i = 1; $i <= @$max; $i++) {
	print "        if (index$i < 0 || index$i >= $$max[$i - 1]) throw new ArrayIndexOutOfBoundsException();\n";
	print "        offset += $$aoffset[$i - 1] + index$i * $$bitsize[$i - 1];\n";
    }
}

sub gen() {
    my ($classname, @spec) = @_;

    &usage("no classname name specified") if !defined($java_classname);

    $java_extends = "net.tinyos.message.Message" if !defined($java_extends);
    if ($java_classname =~ /(.*)\.([^.]*)$/) {
	$package = $1;
	$java_classname = $2;
    }
    else {
	print STDERR "no package specification in class name $java_classname\n";
	exit 2;
    }
    print STDERR @spec;
    $_ = shift @spec;
    /^struct .* ([0-9]+) ([-0-9]+)/ or die;
    $size = $1;
    $amtype = $2;

    print "package $package;\n\n";
    print "public class $java_classname extends $java_extends {\n";

    print "    $java_classname() {\n";
    print "        super(new byte[$size]);\n";
    print "    }\n\n";

    print "    $java_classname(byte[] packet) {\n";
    print "        this(packet, 0);\n";
    print "    }\n\n";

    print "    $java_classname(byte[] packet, int offset) {\n";
    print "        this();\n";
    print "        setData(packet, offset);\n";
    print "    }\n\n";

    print "    public int getType() {\n";
    print "        return $amtype;\n";
    print "    }\n\n";


    $base = 0;
    for (@spec) {
	/ *(.*)/;
	split / /, $1;
	($field, $type, $offset, $bitlength) = @_;

	$javafield = $field;
	$javafield =~ s/\./_/g;
	
	($javatype, $java_access) = &basetype($type);
	@field_array_max = &arraymax($type);	
	@field_array_bitsize = &arraybitsize($bitlength, @field_array_max);

	push @array_max, @field_array_max;
	push @array_bitsize, @field_array_bitsize;
	for ($i = 0; $i < @field_array_max; $i++) {
	    push @array_offset, 0;
	}

	if ($javatype eq "PUSH") {
	    if (@field_array_max) {
		push @array_push_size, $#field_array_max + 1;
		$array_offset[$#array_offset - $#field_array_max] = $base + $offset;
		push @bases, $base;
		$base = 0;
	    }
	    else {
		push @bases, $base;
		push @array_push_size, 0;
		$base += $offset;
	    }
	}
	elsif ($javatype eq "POP") {
	    $base = pop @bases;
	    &poparray(pop @array_push_size);
	}
	else {
	    $index = 0;
	    @args = map { $index++; "int index$index" } @array_max;
	    $argspec = join(", ", @args);
	    print "    $javatype get\u$javafield($argspec) {\n";
	    printoffset($base + $offset, @array_max, @array_bitsize, @array_offset);
	    print "        return get$java_access(offset, $bitlength);\n";
	    print "    }\n\n";

	    push @args, "$javatype value";
	    $argspec = join(", ", @args);
	    print "    void set\u$javafield($argspec) {\n";
	    printoffset($base + $offset, @array_max, @array_bitsize, @array_offset);
	    print "        set$java_access(offset, $bitlength, value);\n";
	    print "    }\n\n";

	    &poparray($#field_array_max + 1);
	}
    }

    print "}\n";
}

sub poparray()
{
    my ($n) = @_;

    while ($n-- > 0) {
	pop @array_max;
	pop @array_bitsize;
	pop @array_offset;
    }
}

sub basetype()
{
    my ($type) = @_;
    my $basetype;

    $type =~ /([A-Z]*)$/ or die;
    $basetype = $1;

    return ("PUSH", 0) if ($basetype eq "AS" || $basetype eq "AU");
    return ("POP", 0) if ($basetype eq "AX");

    return ("long", "UIntElement") if ($basetype eq "U");
    return ("long", "SIntElement") if ($basetype eq "I");
    return ("float", "FloatElement")
	if ($basetype eq "F" || $basetype eq "D" || $basetype eq "LD");

    return (0, 0);
}

sub arraymax()
{
    my ($type, $bsize) = @_;
    my @max;

    while ($type =~ /\[([0-9]+)\](.*)/) {
	push @max, $1;
	$type = $2;
    }
    return @max;
}

sub arraybitsize()
{
    my ($bsize, @max) = @_;
    my @bitsize;

    for ($i = $#max; $i >= 0; $i--) {
	$bitsize[$i] = $bsize;
	$bsize *= $max[$i];
    }

    return @bitsize;
}


# This file is part of the nesC compiler.
#    Copyright (C) 2002 Intel Corporation
# 
# The attached "nesC" software is provided to you under the terms and
# conditions of the GNU General Public License Version 2 as published by the
# Free Software Foundation.
# 
# nesC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with nesC; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

true;

sub gen() {
    my ($classname, @spec) = @_;

    require migdecode;
    &decode(@spec);

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

    print "package $package;\n\n";
    print "public class $java_classname extends $java_extends {\n";

    print "    $java_classname(int size) {\n";
    print "        super(new byte[size]);\n";
    print "    }\n\n";

    print "    $java_classname() {\n";
    print "        this($size);\n";
    print "    }\n\n";

    print "    $java_classname(byte[] packet, int size) {\n";
    print "        this(size);\n";
    print "        dataSet(packet);\n";
    print "    }\n\n";

    print "    $java_classname(byte[] packet) {\n";
    print "        this(packet, $size);\n";
    print "    }\n\n";

    print "    $java_classname(net.tinyos.message.ByteArray packet, int size) {\n";
    print "        this(size);\n";
    print "        dataSet(packet);\n";
    print "    }\n\n";

    print "    $java_classname(net.tinyos.message.ByteArray packet) {\n";
    print "        this(packet, $size);\n";
    print "    }\n\n";

    print "    public int amType() {\n";
    print "        return $amtype;\n";
    print "    }\n\n";


    for (@fields) {
	($field, $type, $bitlength, $offset, $amax, $abitsize, $aoffset) = @{$_};

	$javafield = $field;
	$javafield =~ s/\./_/g;
	
	($javatype, $java_access) = &javabasetype($type, $bitlength);

	$index = 0;
	@args = map { $index++; "int index$index" } @{$amax};
	$argspec = join(", ", @args);

	$index = 0;
	@passargs = map { $index++; "index$index" } @{$amax};
	$passargs = join(", ", @passargs);

	print "    public static int offset\u$javafield($argspec) {\n";
	printoffset($base + $offset, $amax, $abitsize, $aoffset);
	print "        return offset;\n";
	print "    }\n\n";

	print "    public $javatype get\u$javafield($argspec) {\n";
	print "        return ($javatype)get$java_access(offset\u$javafield($passargs), $bitlength);\n";
	print "    }\n\n";

	push @args, "$javatype value";
	$argspec = join(", ", @args);
	print "    public void set\u$javafield($argspec) {\n";
	print "        set$java_access(offset\u$javafield($passargs), $bitlength, value);\n";
	print "    }\n\n";

	print "    public static int size\u$javafield() {\n";
	if (@$amax) {
	    $bitsize = $$abitsize[0] * $$amax[0];
	}
	else {
	    $bitsize = $bitlength;
	}
	print "        return $bitsize;\n";
	print "    }\n\n";

    }

    print "}\n";
}

sub javabasetype()
{
    my ($basetype, $bitlength) = @_;
    my $jtype, $acc;

    return ("float", "FloatElement")
	if ($basetype eq "F" || $basetype eq "D" || $basetype eq "LD");

    # Pick the java type whose range is closest to the corresponding C type
    if ($basetype eq "U") {
	$acc = "UIntElement";
	return ("byte", $acc) if $bitlength < 8;
	return ("char", $acc) if $bitlength <= 16;
	return ("int", $acc) if $bitlength < 32;
	return ("long", $acc);
    }
    if ($basetype eq "I") {
	$acc = "SIntElement";
	return ("byte", $acc) if $bitlength <= 8;
	return ("short", $acc) if $bitlength <= 16;
	return ("int", $acc) if $bitlength <= 32;
	return ("long", $acc);
    }

    return (0, 0);
}

sub printoffset()
{
    my ($offset, $max, $bitsize, $aoffset) = @_;

    print "        int offset = $offset;\n";
    for ($i = 1; $i <= @$max; $i++) {
	# check index bounds. 0-sized arrays don't get an upper-bound check
	# (they represent variable size arrays. Normally they should only
	# occur as the first-dimension of the last element of the structure)
	if ($$max[$i - 1] != 0) {
	    print "        if (index$i < 0 || index$i >= $$max[$i - 1]) throw new ArrayIndexOutOfBoundsException();\n";
	}
	else {
	    print "        if (index$i < 0) throw new ArrayIndexOutOfBoundsException();\n";
	}
	print "        offset += $$aoffset[$i - 1] + index$i * $$bitsize[$i - 1];\n";
    }
}

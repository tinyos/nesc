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


    for (@fields) {
	($field, $type, $bitlength, $offset, $amax, $abitsize, $aoffset) = @{$_};

	$javafield = $field;
	$javafield =~ s/\./_/g;
	
	($javatype, $java_access) = &javabasetype($type);

	$index = 0;
	@args = map { $index++; "int index$index" } @{$amax};
	$argspec = join(", ", @args);
	print "    $javatype get\u$javafield($argspec) {\n";
	printoffset($base + $offset, $amax, $abitsize, $aoffset);
	print "        return get$java_access(offset, $bitlength);\n";
	print "    }\n\n";

	push @args, "$javatype value";
	$argspec = join(", ", @args);
	print "    void set\u$javafield($argspec) {\n";
	printoffset($base + $offset, $amax, $abitsize, $aoffset);
	print "        set$java_access(offset, $bitlength, value);\n";
	print "    }\n\n";
    }

    print "}\n";
}

sub javabasetype()
{
    my ($basetype) = @_;

    return ("long", "UIntElement") if ($basetype eq "U");
    return ("long", "SIntElement") if ($basetype eq "I");
    return ("float", "FloatElement")
	if ($basetype eq "F" || $basetype eq "D" || $basetype eq "LD");

    return (0, 0);
}

sub printoffset()
{
    my ($offset, $max, $bitsize, $aoffset) = @_;

    print "        int offset = $offset;\n";
    for ($i = 1; $i <= @$max; $i++) {
	print "        if (index$i < 0 || index$i >= $$max[$i - 1]) throw new ArrayIndexOutOfBoundsException();\n";
	print "        offset += $$aoffset[$i - 1] + index$i * $$bitsize[$i - 1];\n";
    }
}

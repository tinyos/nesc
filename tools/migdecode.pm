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

# Decode spec into a generator-friendly form
# takes spec as arguments, leaves results in the following global vars:
# $size = bit size of message
# $amtype = active message type (-1 if unknown)
# @fields: array of field-specs. Each field-spec is a reference to an array:
#   0: name
#   1: base_type (U, I, BU, BI, F, D, LD)
#   2: length (in bits)
#   3-6: field access spec, as follows:
#     3: base_offset
#     4-6: refs to arrays with 1 element per index needed to access field
#       4: amax (the original C array size)
#	5: abit_size (of corresponding array element)
#	6: abit_offset (of corresponding array)
#   the offset of the field, given index[1], ..., index[n] (n >= 0) is 
#   computed as follows:
#     offset = base offset
#     for i = 1 to n:
#       assert index[i] >= 0 && index[i] < amax[i]
#       offset += abit_size[i] * index[i] + abit_offset[i]

sub decode() {
    @spec = @_;

    $_ = shift @spec;
    /^((nx_)?struct|union) .* ([0-9]+) ([-0-9]+)/ or die;
    $size = $3;
    $amtype = $4;

    $base = 0;
    for (@spec) {
	# remove end of line stuff to avoid confusion w/ cygwin
	chop;
	s/\r$//;

	/ *(.*)/;
	@specline = split / /, $1;
	($field, $type, $offset, $bitlength) = @specline;

	$basetype = &basetype($type);
	@field_array_max = &arraymax($type);	
	@field_array_bitsize = &arraybitsize($bitlength, @field_array_max);

	push @array_max, @field_array_max;
	push @array_bitsize, @field_array_bitsize;
	for ($i = 0; $i < @field_array_max; $i++) {
	    push @array_offset, 0;
	}

	if ($basetype eq "PUSH") {
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
	elsif ($basetype eq "POP") {
	    $base = pop @bases;
	    &poparray(pop @array_push_size);
	}
	else {
	    push @fields, [ $field, $basetype, $bitlength, $base + $offset,
			    [ @array_max ], [ @array_bitsize ], [ @array_offset ] ];
	    &poparray($#field_array_max + 1);
	}
    }
}

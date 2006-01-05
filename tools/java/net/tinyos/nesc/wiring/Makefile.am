# Copyright (c) 2004-2005 Intel Corporation
# All rights reserved.
#
# This file is distributed under the terms in the attached INTEL-LICENSE     
# file. If you do not find this file, a copy can be found by writing to
# Intel Research Berkeley, 2150 Shattuck Avenue, Suite 1300, Berkeley, CA, 
# 94704.  Attention:  Intel License Inquiry.

AUTOMAKE_OPTIONS = foreign

java_files = $(wildcard *.java)
class_files = $(java_files:.java=.class)
EXTRA_DIST = README $(java_files)

noinst_DATA = $(class_files)

$(class_files): $(java_files)
	javac $(java_files)

clean mostlyclean:
	rm -f *.class

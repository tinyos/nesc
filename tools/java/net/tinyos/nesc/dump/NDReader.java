// $Id$
/*									tab:4
 * Copyright (c) 2004-2005 Intel Corporation
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached INTEL-LICENSE     
 * file. If you do not find these files, copies can be found by writing to
 * Intel Research Berkeley, 2150 Shattuck Avenue, Suite 1300, Berkeley, CA, 
 * 94704.  Attention:  Intel License Inquiry.
 */

package net.tinyos.nesc.dump;

import net.tinyos.nesc.dump.xml.NDElement;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import java.io.*;
import java.util.*;
import java.lang.reflect.*;

public class NDReader extends DefaultHandler
{
    XMLReader parser;
    Stack activeElements = new Stack();
    static Class[] builderArgs = new Class[1];
    NDElement top;
    String userPkg;
    final static String standardPkg = "net.tinyos.nesc.dump.xml";

    static {
	try {
	    builderArgs[0] = Class.forName("org.xml.sax.Attributes");
	}
	catch (ClassNotFoundException e) {
	    System.err.println("xml not configured");
	}
    }

    public NDReader() throws SAXException {
	this(null);
    }

    public NDReader(String userPkg) throws SAXException {
	this.userPkg = userPkg;
	parser = XMLReaderFactory.createXMLReader();
	parser.setContentHandler(this);
    }

    NDElement parse(InputSource source) throws IOException {
	top = null;
	try {
	    parser.parse(source);
	}
	catch (SAXException e) {
	}
	return top;
    }

    NDElement parse(String id) throws IOException {
	return parse(new InputSource(id));
    }

    protected NDElement makeElementIn(String pkg, String name)
	throws Exception {
	/* Invoke the default constructor for the class for element name
	   in pkg pkg */
	return (NDElement)Class.forName(pkg + "." + name).newInstance();
    }

    public NDElement makeElement(String name) throws Exception {
	name = "X" + name.replace('-', '_');
	if (userPkg != null) {
	    try {
		return makeElementIn(userPkg, name);
	    }
	    catch (Exception e) { }
	}
	return makeElementIn(standardPkg, name);
    }

    public void startElement(String uri, String localName, String qName,
			     Attributes attrs) {
	NDElement element = null;

	try {
	    element = makeElement(localName);
	}
	catch (Exception e) {
	    System.err.println("element " + localName + " not supported. " + e);
	}
	if (element != null)
	    element = element.start(this, attrs);
	activeElements.push(element);
    }

    public void endElement(String uri, String localName, String qName) {
	/* Finalize element, and signal parent */
	NDElement current = (NDElement)activeElements.pop();

	if (current == null)
	    return;
	current = current.end();
	if (current == null)
	    return;

	if (activeElements.empty()) 
	    top = current; /* top level element */
	else {
	    NDElement parent = (NDElement)activeElements.peek();
	    parent.child(current);
	}
    }

    public void ignorableWhitespace(char[] ch, int start, int length) {
	NDElement current = (NDElement)activeElements.peek();
	if (current != null) {
	    current.whitespace();
	}
    }

    public void characters(char[] ch, int start, int length) {
	NDElement current = (NDElement)activeElements.peek();
	if (current != null) {
	    current.characters(ch, start, length);
	}
    }

    public static void main(String[] args) throws IOException {
	try {
	    NDElement t = new NDReader().parse(args[0]);
	    System.out.println("" + t);
	}
	catch (SAXException e) {
	    System.err.println("no xml reader found");
	}
    }
}

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

/**
 * Reader for nesC XML dump files.  The parser is based on SAX XML
 * parser. For each nesC element baz, the reader attempts to create an
 * instance of the Xbaz class (and -'s are replaced with _'s). All
 * such classes must extend (directly or indirectly)
 * net.tinyos.nesc.dump.xml.NDElement.  The attributes and subelements
 * of baz will be passed to various methods of
 * net.tinyos.nesc.dump.xml.NDElement, see it's documentations for
 * details.
 * <p>
 * The Xbaz classes are in the net.tinyos.nesc.dump.xml package. Users
 * can extend these classes and have the NDReader create their classes
 * rather than the standard ones if they so choose.  
 * <p>
 * See the documentation of the Xbaz classes in net.tinyos.nesc.dump.xml
 * for information on the Java representation of the XML elements.
 *
 * @see net.tinyos.nesc.dump.xml
 * @see net.tinyos.nesc.dump.xml.NDElement
 *
 * @author David Gay
 */

package net.tinyos.nesc.dump;

import net.tinyos.nesc.dump.xml.NDElement;
import net.tinyos.nesc.dump.xml.Xnesc;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import java.io.*;
import java.util.*;
import java.lang.reflect.*;

public class NDReader extends DefaultHandler
{
    protected XMLReader parser;
    /* The elements currently being parsed */
    protected Stack activeElements = new Stack();

    /* Packages in which to search for XML element classes */
    protected String userPkg; /* user-specfied package to search */
    final static String standardPkg = "net.tinyos.nesc.dump.xml";

    /**
     * Create a new nesC dump reader, that builds the standard elements
     * found in net.tinyos.nesc.dump.xml 
     */
    public NDReader() throws SAXException {
	this(null);
    }

    /**
     * Create a new nesC dump reader, that checks userPkg for XML
     * element classes. If there is no user-specified class, the class
     * from net.tinyos.nesc.dump.xml is used.
     *
     * @param userPkg package to search for XML element classes before
     *   net.tinyos.nesc.dump.xml
     */
    public NDReader(String userPkg) throws SAXException {
	this.userPkg = userPkg;
	/* Try to create an XML reader, starting with the default one, then 
	   trying those provided with Sun and IBM's JREs */
	try {
	    parser = XMLReaderFactory.createXMLReader();
	}
	catch (SAXException e1) {
	    try {
		parser = XMLReaderFactory.createXMLReader("org.apache.xerces.parsers.SAXParser");
	    }
	    catch (SAXException e2) {
		parser = XMLReaderFactory.createXMLReader("org.apache.crimson.parser.XMLReaderImpl");
	    }
	}
	parser.setContentHandler(this);
    }

    /**
     * Parse nesC XML elements from input source 'source'.
     * @param source InputSource to read from.
     * @return TRUE if the parse completed successfully (no SAX exceptions)
     */
    public boolean parse(InputSource source) throws IOException {
	try {
	    Xnesc.reset();
	    parser.parse(source);
	    return true;
	}
	catch (SAXException e) {
	    return false;
	}
    }

    /**
     * Parse nesC XML elements from input source 'id'.
     * @param id input source string to read from.
     * @return TRUE if the parse completed successfully (no SAX exceptions)
     */
    public boolean parse(String id) throws IOException {
	return parse(new InputSource(id));
    }

    protected NDElement makeElementIn(String pkg, String name)
	throws Exception {
	/* Invoke the default constructor for the class for element name
	   in pkg pkg */
	return (NDElement)Class.forName(pkg + "." + name).newInstance();
    }

    /**
     * Create Java class for XML element 'name', checking the user package
     * (if any) before the net.tinyos.nesc.dump.xml package.
     * @param XML element name
     * @return The newly created class
     */
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

    /**
     * Return the parent of the element currently being parsed
     * (note: does not work from within the NDElement.end() method)
     * @return Parent of current element.
     */
    public NDElement parent() {
	return (NDElement)activeElements.elementAt(activeElements.size() - 2);
    }

    /* SAX parsing methods */

    public void startElement(String uri, String localName, String qName,
			     Attributes attrs) {
	NDElement element = null;

	/* Create the Java class for this element, call it's start method
	   and save the result to the active elements stack */
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
	/* Finalize element, and signal parent's child method */
	NDElement current = (NDElement)activeElements.pop();

	if (current == null)
	    return;
	current = current.end(this);
	if (current == null)
	    return;

	if (!activeElements.empty()) {
	    NDElement parent = (NDElement)activeElements.peek();
	    parent.child(this, current);
	}
    }

    /* The current spec doesn't actually use any whitespace or character
       contents */

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

    /**
     * Test method.
     */
    public static void main(String[] args) throws IOException {
	try {
        for (int i = 0; i < args.length; i++) {
        	if (new NDReader().parse(args[i]))
        		System.out.println("parse ok");
        	else
        		System.out.println("parse exceptions occured");
        }
	}
	catch (SAXException e) {
	    System.err.println("no xml reader found");
	}
    }
}

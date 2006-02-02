#ifndef FOO
#define FOO
typedef unsigned char result_t;
enum { SUCCESS, FAIL };
#endif

// $Id$

/*									tab:4
 * "Copyright (c) 2000-2003 The Regents of the University  of California.  
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice, the following
 * two paragraphs and the author appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS."
 *
 * Copyright (c) 2002-2003 Intel Corporation
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached INTEL-LICENSE     
 * file. If you do not find these files, copies can be found by writing to
 * Intel Research Berkeley, 2150 Shattuck Avenue, Suite 1300, Berkeley, CA, 
 * 94704.  Attention:  Intel License Inquiry.
 */
/*
 * Authors:		Jason Hill, David Gay, Philip Levis
 * Date last modified:  6/1/03
 *
 *
 */

/**
 * Abstraction of the LEDs.
 *
 * @author Jason Hill
 * @author David Gay
 * @author Philip Levis
 */

interface Leds {

  /**
   * Initialize the LEDs; among other things, initialization turns
   * them all off.
   *
   * @return SUCCESS always.
   *
   */
  
  async command result_t init();

  /**
   * Turn the red LED on.
   *
   * @return SUCCESS always.
   *
   */
  async command result_t redOn();

  /**
   * Turn the red LED off.
   *
   * @return SUCCESS always.
   *
   */
  async command result_t redOff();

  /**
   * Toggle the red LED. If it was on, turn it off. If it was off,
   * turn it on.
   *
   * @return SUCCESS always.
   *
   */
  async command result_t redToggle();

  /**
   * Turn the green LED on.
   *
   * @return SUCCESS always.
   *
   */
  async command result_t greenOn();

  /**
   * Turn the green LED off.
   *
   * @return SUCCESS always.
   *
   */
  async command result_t greenOff();

  /**
   * Toggle the green LED. If it was on, turn it off. If it was off,
   * turn it on.
   *
   * @return SUCCESS always.
   *
   */
  async command result_t greenToggle();

  /**
   * Turn the yellow LED on.
   *
   * @return SUCCESS always.
   *
   */
  async command result_t yellowOn();

  /**
   * Turn the yellow LED off.
   *
   * @return SUCCESS always.
   *
   */
  async command result_t yellowOff();

  /**
   * Toggle the yellow LED. If it was on, turn it off. If it was off,
   * turn it on.
   *
   * @return SUCCESS always.
   *
   */
  async command result_t yellowToggle();
  
  /**
   * Get current Leds information
   *
   * @return A uint8_t typed value representing Leds status
   *
   */
   async command unsigned char get();

  /**
   * Set Leds to a specified value
   *
   * @param value ranging from 0 to 7 inclusive
   *
   * @return SUCCESS Always
   *
   */
   async command result_t set(unsigned char value);
}

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
 *
 * Authors:		Jason Hill, David Gay, Philip Levis
 * Date last modified:  6/25/02
 *
 */

/**
 * @author Jason Hill
 * @author David Gay
 * @author Philip Levis
 */



module NoLeds {
  provides interface Leds;
}
implementation
{
  async command result_t Leds.init() {
    return SUCCESS;
  }

  async command result_t Leds.redOn() {
    return SUCCESS;
  }

  async command result_t Leds.redOff() {
    return SUCCESS;
  }

  async command result_t Leds.redToggle() {
    return SUCCESS;
  }

  async command result_t Leds.greenOn() {
    return SUCCESS;
  }

  async command result_t Leds.greenOff() {
    return SUCCESS;
  }

  async command result_t Leds.greenToggle() {
    return SUCCESS;
  }

  async command result_t Leds.yellowOn() {
    return SUCCESS;
  }

  async command result_t Leds.yellowOff() {
    return SUCCESS;
  }

  async command result_t Leds.yellowToggle() {
    return SUCCESS;
  }

  async command uint8_t Leds.get() {
    return 0;
  }

  async command result_t Leds.set(uint8_t value) {
    return SUCCESS;
  }
}

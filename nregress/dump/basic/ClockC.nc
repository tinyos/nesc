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
 *                      1/8/03 Su Ping Added StdControl interface +
 *                      some new commands in Clock interface
 *
 */

// The hardware presentation layer. See hpl.h for the C side.
// Note: there's a separate C side (hpl.h) to get access to the avr macros

// The model is that HPL is stateless. If the desired interface is as stateless
// it can be implemented here (Clock, FlashBitSPI). Otherwise you should
// create a separate component


/**
 * @author Jason Hill
 * @author David Gay
 * @author Philip Levis
 */

module ClockC {
  provides interface Clock;
  provides interface StdControl;
}
implementation
{
    char clockRate, set_flag;
    norace unsigned char mscale, nextScale, minterval ;

    command result_t StdControl.init() {
        mscale = DEFAULT_SCALE; 
        minterval = DEFAULT_INTERVAL;
        return SUCCESS;
    }

    command result_t StdControl.start() {
      return SUCCESS;
    }

    command result_t StdControl.stop() {
      return SUCCESS;
    }


    async command void Clock.setInterval(uint8_t value) {
    }
    
    async command void Clock.setNextInterval(uint8_t value) {
        minterval = value;
        set_flag = 1;
    }

    async command uint8_t Clock.getInterval() {
      return 0;
    }

    async command uint8_t Clock.getScale() {
      return mscale;
    }

    async command void Clock.setNextScale(uint8_t scale) {
      set_flag=1;
      nextScale= scale;
    }
       

    async command result_t Clock.setIntervalAndScale(uint8_t interval, uint8_t scale) {
        
        if (scale >7) return FAIL;
        scale|=0x8;
        return SUCCESS;
    }
        
    async command uint8_t Clock.readCounter() {
      return 0;
    }

    async command void Clock.setCounter(uint8_t n) {
    }

    async command void Clock.intDisable() {
    }
    async command void Clock.intEnable() {
    }

    async command result_t Clock.setRate(char interval, char scale) {
      mscale = scale;
      minterval = interval;
      return SUCCESS;
    }
    
    default async event result_t Clock.fire() { return SUCCESS; }
}


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

/**
 * The TinyOS standard control interface. All components that require
 * initialization or can be powered down should provide this
 * interface. start() and stop() are synonymous with powering on and
 * off, when appropriate.
 *
 * On boot, the init() of all wired components must be called. init()
 * may be called multiple times, and in subcomponents before some of
 * their supercomponents (e.g. if they are the subcomponent of
 * multiple components). After init() has been called, start() and
 * stop() may be called multiple times, in any order. The call
 * sequence is therefore:
 *
 * <p>init* (start|stop)*</p>
 *
 * @author Jason Hill
 * @author David Gay
 * @author Philip Levis
 * @modified  6/25/02
 *
 *
 */


interface StdControl
{
  /**
   * Initialize the component and its subcomponents.
   *
   * @return Whether initialization was successful.
   */
  command result_t init();

  /**
   * Start the component and its subcomponents.
   *
   * @return Whether starting was successful.
   */
  command result_t start();

  /**
   * Stop the component and pertinent subcomponents (not all
   * subcomponents may be turned off due to wakeup timers, etc.).
   *
   * @return Whether stopping was successful.
   */
  command result_t stop();
}

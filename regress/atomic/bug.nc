/* "Copyright (c) 2000-2002 The Regents of the University  of
California.
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written
agreement is
 * hereby granted, provided that the above copyright notice, the
following
 * two paragraphs and the author appear in all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE
UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED
HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO
OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS."
 */
/*
 *  IMPORTANT: READ BEFORE DOWNLOADING, COPYING, INSTALLING OR USING. 
By
 *  downloading, copying, installing or using the software you agree to
 *  this license.  If you do not agree to this license, do not download,
 *  install, copy or use the software.
 *
 *  Intel Open Source License
 *
 *  Copyright (c) 2002 Intel Corporation
 *  All rights reserved.
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
are
 *  met:
 *
 *	Redistributions of source code must retain the above copyright
 *  notice, this list of conditions and the following disclaimer.
 *	Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *      Neither the name of the Intel Corporation nor the names of its
 *  contributors may be used to endorse or promote products derived from
 *  this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE INTEL OR
ITS
 *  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

/*
 * Authors: Phil Buonadonna, David Culler, Matt Welsh
 *
 * $Revision$
 *
 * This MODULE implements queued send with optional retransmit.
 * NOTE: This module only queues POINTERS to the application messages.
 * IT DOES NOT COPY THE MESSAGE DATA ITSELF! Applications must maintain
 * their own data queues if more than one outstanding message is
required.
 *
 */

includes AM;
includes AM_HANDLERS;
includes RoutingTable;
includes StatMsg;

/**
 * A packet queue that enforces packet priorities.
 *
 **/
module PriQueuedSendM
{
	provides
	{
		interface StdControl;
		interface QueueSendMsg[uint8_t id];
		interface Queue;
	}

	uses
	{
		interface SendMsg as SerialSendMsg[uint8_t id];
		interface Leds;
		interface RoutingTable;
		interface RadioSettings;
		interface Timer;
	}
}

implementation
{

	enum
	{
		MESSAGE_QUEUE_SIZE 		= 8,
		MAX_RETRANSMIT_COUNT 	= 3,
		TIMER_PERIOD			= 10000,
		PREEMPTION 				= 0,
		UNINIT					= 255,
		ALPHA					= 10
	};

	struct _msgq_entry
	{
		uint16_t address;
		uint8_t length;
		uint8_t id;
		uint8_t xmit_count;
		bool local;
		TOS_Msg msg;
	};
	
	/**
	 * The low-priority buffer.
	 */
	struct _msgq_entry msgqueue0[MESSAGE_QUEUE_SIZE];
	
	/**
	 * The high-priority buffer
	 */
	struct _msgq_entry msgqueue1[MESSAGE_QUEUE_SIZE];

	/**
	 * A one-entry buffer that stores a packet that is currently
	 * being sent (i.e. we are currently waiting for a sendDone),
	 * if there is one.
	 */
	struct _msgq_entry m_pending_entry;

	/**
	 * Where to next enqueue low-priority packets.
	 */
	uint8_t enqueue_next0;

	/**
	 * Where to next dequeue low-priority packets.
	 */
	uint8_t dequeue_next0;

	/**
	 * Where to next enqueue high-priority packets.
	 */
	uint8_t enqueue_next1;

	/**
	 * Where to next dequeue high-priority packets.
	 */
	uint8_t dequeue_next1;

	bool 	retransmit;
	
	/**
	 * Whether there is currently a packet being sent (true iff
	 * m_pending_entry is filled and valid.
	 */
	bool 	bSendPending;
	bool 	m_bPaused;
	uint8_t avg_queue_size;
	uint8_t nNackCount;
	uint8_t nLastNackCount;
	uint8_t	cur_seqno;
	uint32_t nSentAvg;
	uint8_t nSent;
	bool m_bFC;
	TOS_MHopNeighbor** ppParent;

	// STATS
	uint16_t 	nSentCount;
	uint16_t 	nDropped;
	uint16_t 	nAcks;
	uint16_t 	nNacks;
	bool		m_bUpdateStats;

	void updatePacket( TOS_MsgPtr pMsg );
	uint8_t getQueueSize( );

	void initStats( )
	{
		nSentCount 		= 0;
		nDropped 	= 0;
		nAcks	 	= 0;
		nNacks 		= 0;
		m_bUpdateStats = FALSE;
	}

	command result_t StdControl.init()
	{
		int i;
		for (i = 0; i < MESSAGE_QUEUE_SIZE; i++) {
		  msgqueue0[i].length = 0;
		  msgqueue1[i].length = 0;
		}
		//retransmit = TRUE;  // Set to FALSE to disable retransmission
		retransmit = FALSE;  // Set to TRUE to enable retransmission
		bSendPending = FALSE;
		enqueue_next0 = 0;
		dequeue_next0 = 0;
		enqueue_next1 = 0;
		dequeue_next1 = 0;
		avg_queue_size = 0;
		nNackCount = 0;
		nLastNackCount = 0;
		cur_seqno = 0;
		nSentAvg = 0;
		nSent = 0;
		m_bPaused = FALSE;
		m_bFC = TRUE;
		ppParent = call RoutingTable.getParent();

		initStats();
		call RadioSettings.setAck( MSG_ACK );
		dbg(DBG_USR1,"QUEUEDSEND: initialized\n");
		return SUCCESS;
	}

	command result_t StdControl.start()
	{
		call Timer.start(TIMER_REPEAT, TIMER_PERIOD );
		return SUCCESS;
	}
	command result_t StdControl.stop()
	{
		return SUCCESS;
	}

	void flowControl( char ack )
	{
		if ( m_bFC )
		{
			if ( ack == MSG_NACK && (*ppParent) && (*ppParent)->hopcount > 1)
			{
				m_bPaused = TRUE;
				call RoutingTable.listen(MESSAGE_QUEUE_SIZE/4);
			}
			else
			{
				m_bPaused = FALSE;
			}
		}
	}

	void updateStats( TOS_MsgPtr pMsg )
	{
		if ( m_bUpdateStats )
		{
			if ( pMsg->ack == MSG_ACK ) nAcks++;
			else if ( pMsg->ack == MSG_NACK ) nNacks++;
			nSentCount++;
		}
	}

	task void QueueServiceTask() {
		uint8_t id;

		TOSH_interrupt_disable();
		if ((msgqueue0[dequeue_next0].length ||
			   msgqueue1[dequeue_next1].length) && !bSendPending && !m_bPaused) {
			// There is a packet to send, either lo pri or hi pri.

			if (msgqueue1[dequeue_next1].length) {
				// There is a high priority packet to send.  Copy the
				// candidate into m_pending_entry and bump the queue.

				dbg(DBG_USR2, "PriQueuedSendM: sending hi pri msg (0x%x)\n",
					dequeue_next1);
				memcpy (&m_pending_entry, &msgqueue1[dequeue_next1],
					sizeof (struct _msgq_entry));
				msgqueue1[dequeue_next1].length = 0;
				dequeue_next1 = (dequeue_next1 + 1) % MESSAGE_QUEUE_SIZE;
			} else {
				// There is a low priority packet to send.  Same thing.

				dbg(DBG_USR2, "PriQueuedSendM: sending lo pri msg (0x%x)\n",
					dequeue_next0);
				memcpy (&m_pending_entry, &msgqueue0[dequeue_next0],
					sizeof (struct _msgq_entry));
				msgqueue0[dequeue_next0].length = 0;
				dequeue_next0 = (dequeue_next0 + 1) % MESSAGE_QUEUE_SIZE;
			}

			// Now the pending (low or high) priority entry is in
			// m_pending_entry.
			id = m_pending_entry.id;
			if ( id == AM_STATMSG )
				dbg(DBG_USR1, "Sending stat msg\n");

			updatePacket(&m_pending_entry.msg);

			call Leds.greenOn();

			if (!(call SerialSendMsg.send[id](m_pending_entry.address,
					  m_pending_entry.length, &m_pending_entry.msg))) {
				dbg(DBG_USR1, "PriQueuedSend: send request failed.\n");
			} else {
				dbg(DBG_USR1, "PriQueuedSend: send req OK, bSendPending set.\n");
				bSendPending = TRUE;
			}
		}
		TOSH_interrupt_enable();
	}

	/**
	 * Called on the priqueue to send a packet.  Buffer the
	 * packet, possibly replacing a low-priority packet, or drop
	 * the packet and indicate failure.
	 *
	 * @param address address to which this packet is destined.
	 * @param length length of this packet
	 * @param local is this packet locally-generated?
	 * 
	 * @return the success status of the operation
	 */
	command result_t QueueSendMsg.send[uint8_t id]
		(uint16_t address, uint8_t length, TOS_MsgPtr msg, bool local) {

		struct _msgq_entry *msgqueue;
		uint8_t enqueue_next, dequeue_next;
		MHopMsg *pMHMsg = (MHopMsg*)(msg->data);

		// debugging output
		//call Leds.greenToggle();

		if (pMHMsg->priority == 0)
			dbg(DBG_USR2, "PriQueuedSend: lo pri queue msg enq 0x%x deq 0x%x\n",
				enqueue_next0, dequeue_next0);
		else if (pMHMsg->priority == 1)
			dbg(DBG_USR2, "PriQueuedSend: hi pri queue msg enq 0x%x deq 0x%x\n",
				enqueue_next1, dequeue_next1);
		else {
			dbg(DBG_USR2, "PriQueuedSend: UNEXPECTED PACKET PRIORITY!\n");
		}

		if ( nNackCount != UNINIT && (nNackCount + 1) != UNINIT &&
			 getQueueSize() >= MESSAGE_QUEUE_SIZE / 2 ) {
			nNackCount++;
			call RadioSettings.setAck(MSG_NACK);
		}

		atomic {
			if (pMHMsg->priority == 0) {
				// A low priority packet just arrived

				msgqueue = msgqueue0;
				enqueue_next = enqueue_next0;
				dequeue_next = dequeue_next0;
				enqueue_next0 = (enqueue_next0 + 1) % MESSAGE_QUEUE_SIZE;
			} else {
				// A high-priority packet just arrived

				msgqueue = msgqueue1;
				enqueue_next = enqueue_next1;
				dequeue_next = dequeue_next1;
				enqueue_next1 = (enqueue_next1 + 1) % MESSAGE_QUEUE_SIZE;

				if (getQueueSize() == MESSAGE_QUEUE_SIZE &&
						enqueue_next0 != dequeue_next0) {
					// The virtual queue is full and the low-priority queue
					// is not empty.  Dump a low-priority packet to maintain
					// the invariant that getQueueSize() <=
					// MESSAGE_QUEUE_SIZE.

					if (msgqueue0[dequeue_next0].local) {
						uint8_t dumped_id = msgqueue0[dequeue_next0].id;
						signal QueueSendMsg.sendDone[dumped_id] (msg, FAIL);
					}
					msgqueue0[dequeue_next0].length = 0;
					dequeue_next0 = (dequeue_next0 + 1) % MESSAGE_QUEUE_SIZE;
				}
			}
		}

		if ((((enqueue_next + 1) % MESSAGE_QUEUE_SIZE) == dequeue_next) ||
				(getQueueSize() == MESSAGE_QUEUE_SIZE)) {
			// Fail if (virtual or low-priority) queue is full

			dbg(DBG_USR1,"PRIQUEUEDSEND: virtual or low-pri queue full!\n");
			if (!bSendPending)
				post QueueServiceTask();

			if( m_bUpdateStats )
				nDropped++;

			//if (msgqueue[*dequeue_next].local)
			if (local)
				signal QueueSendMsg.sendDone[id] (msg, FAIL);

			call Leds.redToggle();

			TOSH_interrupt_enable();
			return FAIL;
		}

		memcpy( &msgqueue[enqueue_next].msg, msg, sizeof(TOS_Msg) );
		msgqueue[enqueue_next].address = address;
		msgqueue[enqueue_next].length = length;
		msgqueue[enqueue_next].id = id;
		msgqueue[enqueue_next].xmit_count = 0;
		msgqueue[enqueue_next].msg.ack = 0;
		msgqueue[enqueue_next].msg.addr = address;
		msgqueue[enqueue_next].msg.type = id;
		msgqueue[enqueue_next].local = local;


		dbg(DBG_USR2,
			"PriQueuedSend: Successfully queued msg to 0x%x\n", address);

		post QueueServiceTask();

		return SUCCESS;
	}

	/**
	 * Called on the queue when an outgoing send completes.
	 */
  event result_t
	SerialSendMsg.sendDone[uint8_t id] (TOS_MsgPtr msg, result_t success) {

		dbg(DBG_USR2,"PRIQUEUEDSEND: send done called for id %i\n", id);
		
		// Check invariants on m_pending_entry and bSendPending; update
		// state.
		if ((msg != &m_pending_entry.msg) || (bSendPending == FALSE)) {
			dbg(DBG_USR1, "PRIQUEUEDSEND: internal failure.\n");
			return FAIL;
		}

		call Leds.greenOff();

		/* XXX TODO only ditch  m_pending_entry if not a control
		 * message.  need to move the following assignment */
		bSendPending = FALSE;

		if ((*ppParent)) {
			if (msg->ack != 0 || msg->addr == TOS_BCAST_ADDR ||
			  msg->addr == TOS_UART_ADDR)	{
				(*ppParent)->recv_f++;
			} else {
				(*ppParent)->fail_f++;
			}
		}

		if( nSent != 0xff)
			nSent++;

		dbg(DBG_USR1,"PRIQUEUEDSEND: nSent: %i\n", nSent );

		// filter out non-queuesend msgs
		updateStats( msg );
		flowControl(msg->ack);

		if ((msg->ack != 0) ||
			(m_pending_entry.address == TOS_UART_ADDR) ||
				(m_pending_entry.address == TOS_BCAST_ADDR)) {
			if (m_pending_entry.local) {
				signal QueueSendMsg.sendDone[id] (msg, SUCCESS);
			}
		} else {
			m_pending_entry.xmit_count++;
			if ((m_pending_entry.xmit_count > MAX_RETRANSMIT_COUNT) ||
				(id != AM_CONTROLMHMSG)) {
				// Tried to send too many times, just drop
				if (m_pending_entry.local) {
					signal QueueSendMsg.sendDone[id] (msg, FAIL);
				}
			}
		}
		m_pending_entry.length = 0;

		post QueueServiceTask();

		if ( getQueueSize() <= MESSAGE_QUEUE_SIZE/2 ) {
			call RadioSettings.setAck(MSG_ACK);
		}

		//if ( id == AM_MHOPMSG ) {
		//	call Leds.yellowToggle();
		//}

		//if ( getQueueSize() > 0 ) { call Leds.redOff(); }
		//else { call Leds.redOn(); }
		return SUCCESS;
  }

  default event result_t
	QueueSendMsg.sendDone[uint8_t id] (TOS_MsgPtr msg, result_t success) {
		//call Leds.redToggle();
		return SUCCESS;
  }

	/**
	 * Correctly computes the current queue size, taking into
	 * account virtual limitations on the size, and the pending
	 * packet, if any exists.
	 * 
	 * Only reads--does not write--shared queue variables.
	 *
	 * @return the queue size as an 8-bit unsigned integer
	 */
	uint8_t getQueueSize( )
	{
		uint8_t queueSize0 = 0, queueSize1 = 0;

		if (enqueue_next0 < dequeue_next0) {
			queueSize0 = MESSAGE_QUEUE_SIZE + enqueue_next0 - dequeue_next0;
		} else {
			queueSize0 = enqueue_next0 - dequeue_next0;
		}

		if (enqueue_next1 < dequeue_next1) {
			queueSize1 = MESSAGE_QUEUE_SIZE + enqueue_next1 - dequeue_next1;
		} else {
			queueSize1 = enqueue_next1 - dequeue_next1;
		}

		return queueSize0 + queueSize1 + (bSendPending ? 1 : 0);
	}

	void updatePacket( TOS_MsgPtr pMsg )
	{
		if( pMsg->type > 200 )
		{
			MHopMsg* pMHMsg = (MHopMsg*)(pMsg->data);
			if ( (*ppParent) )
			{
				//uint16_t queue_a = (*ppParent)->queue_a + nLastNackCount;
				//if( queue_a > 0xff ) { pMHMsg->queue_a = 0xff; }
				//else { pMHMsg->queue_a = (uint8_t) queue_a; }

				if ( (*ppParent) && (*ppParent)->addr == TOS_UART_ADDR )
				{
					pMHMsg->queue_a = (uint8_t)(nSentAvg >> 16);
					pMHMsg->sap 	= TOS_LOCAL_ADDRESS;
				}
				else if ( (*ppParent) )
				{
					pMHMsg->queue_a = (*ppParent)->queue_a;
					pMHMsg->sap 	= (*ppParent)->sap;
				}
				else
				{
					pMHMsg->queue_a = 0xff;
					pMHMsg->sap		= 0xff;
				}

				pMHMsg->p_a = (uint8_t) ( ((uint32_t) (*ppParent)->p_a * 
					((*ppParent)->p_r)>>16) / (uint32_t)128 );
				pMHMsg->hopcount = (*ppParent)->hopcount + 1;
				pMHMsg->sourceaddr = TOS_LOCAL_ADDRESS;
				if ( pMsg->addr == TOS_UART_ADDR ) { pMHMsg->seqno = 0xff; }
				else {	pMHMsg->seqno = cur_seqno++; }
				//dbg(DBG_USR1,"QUEUEDSEND: advertising p_a: %i p_f: %i seq:%i\n",(*ppParent)->p_a, (*ppParent)->p_f,  pMHMsg->seqno);

			}
			else
			{
				dbg(DBG_USR1,"PRIQUEUEDSEND: no parent\n");
				pMHMsg->p_a = 0;
				pMHMsg->hopcount = 255;
			}
		}
	}

	void updateEWMA( )
	{

		nSentAvg = (uint32_t) (((nSentAvg * (uint32_t)(ALPHA - 1)) +
			(((uint32_t)nSent) << 16)) / (uint32_t)ALPHA);
		dbg(DBG_USR1,"PRIQUEUEDSEND: nSentAvg: %i nSent:%i\n", nSentAvg>>24,
nSent );
		nSent = 0;
	}

	command void Queue.setLoad( uint8_t load )
	{
		nNackCount 		= UNINIT;
		nLastNackCount 	= load;
	}

	command void Queue.resume()
	{
		//call Leds.yellowOff();
		m_bPaused = FALSE;
		post QueueServiceTask();
	}

	command void Queue.setFlowControl( bool enable )
	{
		m_bFC = enable;
	}

	command void Queue.updateStats( StatMsg* pStatMsg )
	{
		pStatMsg->stats[0] = nSentCount;
		pStatMsg->stats[1] = nDropped;
		pStatMsg->stats[2] = nAcks;
		pStatMsg->stats[3] = nNacks;
	}

	command void Queue.resetStats( )
	{
		initStats();
	}

	command void Queue.recordStats( bool val )
	{
		m_bUpdateStats = val;
	}

	command uint8_t Queue.getSentAvg() {
		return (uint8_t)(nSentAvg >> 16);
	}

	void updateNackCount()
	{
		if ( nNackCount != UNINIT)
		{
			nLastNackCount = nNackCount;
			nNackCount = 0;
		}
	}

	event result_t Timer.fired( )
	{
		updateNackCount( );
		updateEWMA();
		return SUCCESS;
	}
}

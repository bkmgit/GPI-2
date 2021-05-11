/*
Copyright (c) Fraunhofer ITWM - Carsten Lojewski <lojewski@itwm.fhg.de>, 2013-2021

This file is part of GPI-2.

GPI-2 is free software; you can redistribute it
and/or modify it under the terms of the GNU General Public License
version 3 as published by the Free Software Foundation.

GPI-2 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GPI-2. If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef _GPI2_H_
#define _GPI2_H_

#include "GASPI.h"
#include "GPI2_GRP.h"
#include "GPI2_Types.h"
#include "GPI2_Utility.h"

#include "GPI2_Stats.h"

/* Notifications space size:
   allowed number of number of notifications + read notification value
   space. For the latter we use 64 though, to have a larger alignment
   of the data space (hacky!).
*/
#define NOTIFICATIONS_SPACE_SIZE ((65536 * sizeof(gaspi_notification_t)) + (64))

extern gaspi_context_t glb_gaspi_ctx;

static inline gaspi_cycles_t
gaspi_get_cycles (void)
{
#if defined(__x86_64__)

  unsigned low, high;
  unsigned long long val;

  __asm__ volatile ("rdtsc":"=a" (low), "=d" (high));

  val = high;
  val = (val << 32) | low;
  return val;

#elif defined(__aarch64__)

  unsigned long ts;
  asm volatile ("isb; mrs %0, cntvct_el0" : "=r" (ts));
  return ts;

#elif defined (__PPC64__)

  unsigned long cycles;
  asm volatile ("mftb %0" : "=r" (cycles) : );
  return cycles;

#endif
}

#ifdef MIC
static inline unsigned char
gaspi_atomic_xchg (volatile unsigned char *addr, const char new_val)
{
  unsigned char res;
  __asm__ volatile ("lock; xchgb %0, %1":"+m" (*addr),
                "=a" (res):"1" (new_val):"memory");
  return res;
}

#define GASPI_ATOMIC_TRY_LOCK(l) gaspi_atomic_xchg(l, 1)
#define GASPI_ATOMIC_UNLOCK(l)   gaspi_atomic_xchg(l, 0)

#else //!MIC

#define GASPI_ATOMIC_TRY_LOCK(l) __sync_lock_test_and_set (l, 1)
#define GASPI_ATOMIC_UNLOCK(l)  __sync_lock_release (l)

#endif

static inline void
lock_gaspi (gaspi_lock_t * l)
{
  while (GASPI_ATOMIC_TRY_LOCK (&l->lock))
  {
    while (l->lock)
    {
      GASPI_DELAY();
    }
  }
}

static inline int
lock_gaspi_tout (gaspi_lock_t * l, const gaspi_timeout_t timeout_ms)
{

  if (timeout_ms == GASPI_BLOCK)
  {
    while (GASPI_ATOMIC_TRY_LOCK (&l->lock))
    {
      while (l->lock)
      {
        GASPI_DELAY();
      }
    }
    return 0;
  }
  else if (timeout_ms == GASPI_TEST)
  {
    const unsigned char val = GASPI_ATOMIC_TRY_LOCK (&l->lock);

    return val;
  }

  //timeout
  const gaspi_cycles_t s0 = gaspi_get_cycles();

  while (GASPI_ATOMIC_TRY_LOCK (&l->lock))
  {
    while (l->lock)
    {
      const gaspi_cycles_t s1 = gaspi_get_cycles();
      const gaspi_cycles_t tdelta = s1 - s0;

      const float ms = (float) tdelta * glb_gaspi_ctx.cycles_to_msecs;

      if (ms > (float) timeout_ms)
      {
        return 1;
      }

      GASPI_DELAY();
    }
  }

  return 0;
}

static inline void
unlock_gaspi (gaspi_lock_t * l)
{
  GASPI_ATOMIC_UNLOCK (&l->lock);
}

#endif //_GPI2_H_

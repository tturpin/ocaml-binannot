/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1997 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <mlvalues.h>
#include "unixsupport.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

value unix_setsid(value unit)              /* ML */
{
#ifdef HAS_SETSID
  return Val_int(setsid());
#else
  invalid_argument("setsid not implemented");
  return Val_unit;
#endif
}

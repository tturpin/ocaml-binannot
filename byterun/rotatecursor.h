/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Damien Doligez, projet Para, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _rotatecursor_
#define _rotatecursor_

/* [rotatecursor_flag] will be automagically set to 1 when the time comes
   to call [rotatecursor_action].
*/
extern int volatile rotatecursor_flag;


/* Use [rotatecursor_options] to set advanced parameters:

   1. [p1] is a pointer to another variable that will be set to 1 when
      the time is up, unless it is already nonzero.  Typical use is when
      you already have a variable in your program that is set
      asynchronously for something else, and you want to avoid testing
      two different variables in your inner loop.  Pass NULL if you don't
      need this feature.

   2. [period] is the interval (in milliseconds) between calls to
      RotateCursor.  Reasonable values are between 10 and 200;
      the default is 50.
*/
void rotatecursor_options (int volatile *p1, int period);

/* [reverse] is 0 to rotate the cursor clockwise, anything else to
   rotate counterclockwise.  This function resets [rotatecursor_flag]
   to 0.
   This function always returns 0.  It returns an int so you can use
   it in an expression as well as a statement.  Useful for some macros.
 */
int rotatecursor_action (int reverse);

/* Simple interface to [rotatecursor_flag] and [rotatecursor_action].
   Can be used as a statement (followed by a semicolon) or in an
   expression (followed by a comma).
*/
#define ROTATECURSOR_MAGIC() (rotatecursor_flag ? rotatecursor_action (0) : 0)

#endif /* _rotatecursor_ */

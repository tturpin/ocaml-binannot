#if !macintosh
#include <sys/types.h>
#else
#include <SizeTDef.h>
#endif
#include <string.h>
#include <regex.h>
#include <mlvalues.h>
#include <alloc.h>
#include <fail.h>
#include <memory.h>
#include <str.h>

struct regexp_struct {
  final_fun finalization;
  struct re_pattern_buffer re;
};

typedef struct regexp_struct * regexp;

static void free_regexp(vexpr)
     value vexpr;
{
  regexp expr = (regexp) Bp_val(vexpr);
  expr->re.translate = NULL;
  regfree(&(expr->re));
}

static regexp alloc_regexp()
{
  value res =
    alloc_final(sizeof(struct regexp_struct) / sizeof(value),
                free_regexp, 1, 1000);
  return (regexp) res;
}

#define RE_SYNTAX RE_SYNTAX_EMACS

static char * case_fold_table = NULL;

value str_compile_regexp(src, fold) /* ML */
     value src, fold;
{
  regexp expr;
  char * msg;

  Begin_root(src);
  expr = alloc_regexp();
  End_roots();
  re_syntax_options = RE_SYNTAX;
  if (Bool_val(fold) && case_fold_table == NULL) {
    int i;
    case_fold_table = stat_alloc(256);
    for (i = 0; i <= 255; i++) case_fold_table[i] = i;
    for (i = 'A'; i <= 'Z'; i++) case_fold_table[i] = i + 32;
    for (i = 192; i <= 214; i++) case_fold_table[i] = i + 32;
    for (i = 216; i <= 222; i++) case_fold_table[i] = i + 32;
  }
  expr->re.translate = Bool_val(fold) ? case_fold_table : NULL;
  expr->re.fastmap = stat_alloc(256);
  expr->re.buffer = NULL;
  expr->re.allocated = 0;
  msg = (char *) re_compile_pattern(String_val(src), string_length(src),
                                    &(expr->re));
  if (msg != NULL) failwith(msg);
  re_compile_fastmap(&(expr->re));
  expr->re.regs_allocated = REGS_FIXED;
  return (value) expr;
}

static regoff_t start_regs[10], end_regs[10];

static struct re_registers match_regs = { 10, start_regs, end_regs };

value str_string_match(expr, text, pos) /* ML */
     regexp expr;
     value text, pos;
{
  switch (re_match(&(expr->re), String_val(text), string_length(text),
                   Int_val(pos), &match_regs)) {
  case -2:
    invalid_argument("Str.string_match");
  case -1:
    return Val_false;
  default:
    return Val_true;
  }
}

value str_search_forward(expr, text, pos) /* ML */
     regexp expr;
     value text, pos;
{
  int len = string_length(text);
  int start = Int_val(pos);
  int res = re_search(&(expr->re), String_val(text), len, start, len-start,
                      &match_regs);
  switch(res) {
  case -2:
    invalid_argument("Str.search_forward");
  case -1:
    raise_not_found();
  default:
    return Val_int(res);
  }
}

value str_search_backward(expr, text, pos) /* ML */
     regexp expr;
     value text, pos;
{
  int len = string_length(text);
  int start = Int_val(pos);
  int res = re_search(&(expr->re), String_val(text), len, start, -start-1,
                      &match_regs);
  switch(res) {
  case -2:
    invalid_argument("Str.search_backward");
  case -1:
    raise_not_found();
  default:
    return Val_int(res);
  }
}

value str_beginning_group(ngroup) /* ML */
     value ngroup;
{
  return Val_int(start_regs[Int_val(ngroup)]);
}

value str_end_group(ngroup)     /* ML */
     value ngroup;
{
  return Val_int(end_regs[Int_val(ngroup)]);
}

value str_replacement_text(repl, orig) /* ML */
     value repl, orig;
{
  value res;
  mlsize_t len, n;
  char * p, * q;
  int c;

  len = 0;
  p = String_val(repl);
  n = string_length(repl);
  while (n > 0) {
    c = *p++; n--;
    if(c != '\\')
      len++;
    else {
      if (n == 0) failwith("Str.replace: illegal backslash sequence");
      c = *p++; n--;
      switch (c) {
      case '\\':
        len++; break;
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        c -= '0';
        len += end_regs[c] - start_regs[c]; break;
      default:
        len += 2; break;
      }
    }
  }
  Begin_roots2(orig,repl);
    res = alloc_string(len);
  End_roots();
  p = String_val(repl);
  q = String_val(res);
  n = string_length(repl);
  while (n > 0) {
    c = *p++; n--;
    if(c != '\\')
      *q++ = c;
    else {
      c = *p++; n--;
      switch (c) {
      case '\\':
        *q++ = '\\'; break;
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        c -= '0';
        len = end_regs[c] - start_regs[c];
        bcopy(&Byte(orig, start_regs[c]), q, len);
        q += len;
        break;
      default:
        *q++ = '\\'; *q++ = c; break;
      }
    }
  }
  return res;
}


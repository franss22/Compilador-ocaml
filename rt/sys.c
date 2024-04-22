#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <inttypes.h>

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>
#include <sys/resource.h>

/* synonym to ease typing/reading */
typedef uint64_t u64;

/* configuration */
u64 STACK_SIZE = 0x800000;
u64 HEAP_SIZE = 100;
int USE_GC = 1;

/* externs */
extern void error(u64 err_code, u64 val) asm("error");
extern u64 print(u64 val) asm("print");
extern u64 *try_gc(u64 *alloc_ptr, u64 words_needed, u64 *cur_frame, u64 *cur_sp) asm("try_gc");
extern u64 our_code_starts_here(u64 *heap) asm("our_code_starts_here");
extern void set_stack_bottom(u64 *stack_bottom) asm("set_stack_bottom");
/* */

/*
  ALL YOUR OTHER RTSYS.C STUFF GOES HERE
  (constants, tagging, error handling, printing)

*/
u64 copy_tuple(u64 *);
u64 copy_closure(u64 *);

typedef uint64_t u64;

extern u64 our_code_starts_here(u64 *heap) asm("our_code_starts_here");

const u64 BOOL_TAG = 0x0000000000000001;
const u64 TUPLE_TAG = 0x0000000000000003;
const u64 LAMBDA_TAG = 0x0000000000000005;
const u64 SPECIAL_TYPE_TAG = 0x0000000000000007;
const u64 FORWARD_TAG = 0x0000000000000007;
const u64 BOOL_TRUE = 0x8000000000000001;  // These must be the same values
const u64 BOOL_FALSE = 0x0000000000000001; // as chosen in compile.ml

enum
{
  ERR_NOT_NUMBER = 1,
  ERR_NOT_BOOLEAN = 2,
  ERR_NOT_TUPLE = 3,
  ERR_NOT_CLOSURE = 4,
  ERR_ARITY = 5,
  ERR_OVERFLOW = 6,
  ERR_DIVISION_BY_ZERO = 7,
  ERR_INDEX_OUT_OF_BOUNDS = 8
};
enum
{
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_MOD
};

u64 printResult(u64 val)
{
  if ((val & BOOL_TAG) == 0)
  {                                      // val is even ==> number
    printf("%ld", ((int64_t)(val)) / 2); // shift bits right to remove tag
  }
  else if ((val & SPECIAL_TYPE_TAG) == TUPLE_TAG)
  {
    u64 untagged_val = val - 3;
    u64 *tup = (u64 *)untagged_val;
    int length = (int)*tup;
    printf("(tup");
    for (int i = 0; i < length; i++)
    {
      printf(" ");
      printResult(tup[i + 1]);
    }
    printf(")");
    // printf("tuple at %#018lx", val - 3);
  }
  else if ((val & SPECIAL_TYPE_TAG) == LAMBDA_TAG)
  {
    u64 untagged_val = val - 5;
    u64 *tup = (u64 *)untagged_val;
    int arity = (int)*tup;
    printf("<clos:%d>", arity);
    // for (int i = 0; i < arity; i++) {
    //   printf(" ");
    //   printResult(tup[i + 1]);
    // }
    // printf(")");
    // printf("tuple at %#018lx", val - 3);
  }
  else if (val == BOOL_TRUE)
  {
    printf("true");
  }
  else if (val == BOOL_FALSE)
  {
    printf("false");
  }
  else
  {
    printf("Unknown value: %#018lx", val); // print unknown val in hex
  }
  return val;
}

u64 print(u64 val)
{
  printf("> ");
  printResult(val);
  printf("\n");
  return val;
}

char *getValueTypeString(int value)
{
  switch (value)
  {
  case ERR_NOT_NUMBER:
    return "integer";
  case ERR_NOT_BOOLEAN:
    return "boolean";
  case ERR_NOT_TUPLE:
    return "tuple";
  case ERR_NOT_CLOSURE:
    return "closure";
  default:
    return "null";
  }
}

void typeError(int expected_type, u64 value)
{

  printf("Type error: Expected %s but got ", getValueTypeString(expected_type));
  printResult(value);
  printf("\n");
  exit(expected_type);
}

void arityError(int expected_amount, int given_amount)
{
  printf("Arity mismatch: closure expected %i arguments but got %i\n", expected_amount, given_amount);
  exit(ERR_ARITY);
}

char *getOpString(int op)
{
  switch (op)
  {
  case OP_ADD:
    return "+";
  case OP_SUB:
    return "-";
  case OP_MUL:
    return "*";
  case OP_DIV:
    return "/";
  case OP_MOD:
    return "\%";
  default:
    return "";
  }
}

void overflowError(int op)
{
  char *_flow;
  switch (op)
  {
  case OP_ADD:
  case OP_MUL:
    _flow = "over";
    break;
  case OP_SUB:
    _flow = "under";
    break;
  }
  printf("Arithmetic error: %s produced an %sflow", getOpString(op), _flow);
  exit(ERR_OVERFLOW);
}

void divByZeroError(int op)
{
  char *op_name;
  switch (op)
  {
  case OP_DIV:
    op_name = "Division";
    break;
  case OP_MOD:
    op_name = "Modulo";
    break;
  }
  printf("Arithmetic error: %s by 0", op_name);
  exit(ERR_DIVISION_BY_ZERO);
}

void indexError(int index, u64 tup)
{
  printf("Index out of bounds: Tried to access index %d of ", index);
  printResult(tup);
  exit(ERR_INDEX_OUT_OF_BOUNDS);
}

/* GC */
u64 *HEAP_START;
u64 *HEAP_END;
u64 *HEAP_MID;
u64 *TO_SPACE;
u64 *FROM_SPACE;
u64 *ALLOC_PTR = 0;
u64 *SCAN_PTR = 0;
u64 *STACK_BOTTOM = 0;

void set_stack_bottom(u64 *stack_bottom)
{
  STACK_BOTTOM = stack_bottom;
}

bool is_heap_ptr(u64 val)
{
  return (u64 *)val < HEAP_END && (u64 *)val >= HEAP_START;
}

bool is_tuple(u64 val)
{
  return (val & SPECIAL_TYPE_TAG) == TUPLE_TAG;
}

bool is_forward(u64 val)
{
  return (val & SPECIAL_TYPE_TAG) == FORWARD_TAG;
}

bool is_closure(u64 val)
{
  return (val & SPECIAL_TYPE_TAG) == LAMBDA_TAG;
}

u64 *tuple_ptr(u64 ptr)
{
  return (u64*)((u64)ptr & ~TUPLE_TAG);
}
u64 *closure_ptr(u64 ptr)
{
    return (u64*)((u64)ptr & ~LAMBDA_TAG);

}

void print_stack(u64 *rbp, u64 *rsp)
{
  printf("|------- frame %p to %p  ------\n", rsp, rbp);
  for (u64 *cur_word = rsp; cur_word < rbp; cur_word++)
  {
    u64 val = (u64)*cur_word;
    printf("| %p: %p", cur_word, (u64 *)*cur_word);
    if (is_heap_ptr(val))
    {
      if (is_tuple(val))
      {
        printf(" (tuple)");
      }
      else if (is_closure(val))
      {
        printf(" (closure: %p)", closure_ptr(val));
      }
    }
    printf("\n");
  }
  if (rbp < STACK_BOTTOM)
  {
    print_stack((u64 *)*rbp, rbp + 2);
  }
  else
  {
    printf("|------- bottom %p  ------\n", STACK_BOTTOM);
  }
}

void print_heap(u64 *heap_start, u64 *heap_end)
{
  printf("| Heap from %p to %p\n", heap_start, heap_end);
  for (u64 i = 0; i <= (u64)(heap_end - heap_start); i++)
  {
    printf("|  %lu/%p: %p \n", i, (heap_start + i), (u64 *)*(heap_start + i));
  }
}

void print_heaps()
{
  printf("|\n|=======HEAP 1==========\n");
  print_heap(HEAP_START, HEAP_MID - 1);
  printf("|=======HEAP 2==========\n");
  print_heap(HEAP_MID, HEAP_END);
  printf("|=================\n\n");
}

u64 copy_tuple(u64 *obj_ptr)
{
  if (is_forward(obj_ptr[0]))
  {
    u64 forward_ptr = obj_ptr[0] - FORWARD_TAG + TUPLE_TAG;
    return forward_ptr;
  }
  u64 len = obj_ptr[0];
  // leave forward address
  obj_ptr[0] = (u64)ALLOC_PTR | FORWARD_TAG;

  u64 *tup_ptr = ALLOC_PTR;
  tup_ptr[0] = len;
  ALLOC_PTR += len + 1;
  for (u64 i = 1; i <= len; i++)
  {
    u64 val = obj_ptr[i];

    if (is_tuple(val))
    {
      val = copy_tuple(tuple_ptr(val));
    }
    else if (is_closure(val))
    {
      val = copy_closure(closure_ptr(val));
    }

    tup_ptr[i] = val;
  }
  return (u64)tup_ptr | TUPLE_TAG;
}
u64 copy_closure(u64 *obj_ptr)
{
  if (is_forward(obj_ptr[0]))
  {
    u64 forward_ptr = obj_ptr[0] - FORWARD_TAG + LAMBDA_TAG;
    return forward_ptr;
  }
  /*
  0: arity
  1: label
  2: freevar_len
  3+: freevars...
  */
  u64 *clos_ptr = ALLOC_PTR;
  u64 arity = obj_ptr[0];
  u64 label = obj_ptr[1];
  u64 freevar_amt = obj_ptr[2];

  // leave forward address
  obj_ptr[0] = (u64)ALLOC_PTR | FORWARD_TAG;

  clos_ptr[0] = arity;
  clos_ptr[1] = label;
  clos_ptr[2] = freevar_amt;

  ALLOC_PTR += freevar_amt + 3;
  for (u64 i = 0; i < freevar_amt; i++)
  {
    u64 val = obj_ptr[i+3];

    if (is_tuple(val))
    {
      val = copy_tuple(tuple_ptr(val));
    }
    else if (is_closure(val))
    {
      val = copy_closure(closure_ptr(val));

    }

    clos_ptr[i+3] = val;
  }
  return (u64)clos_ptr | TUPLE_TAG;
}

u64 try_copy(u64 val)
{
  //copies the value if it is a tuple or closure pointer, otherwise does nothing and returns it
  if (is_heap_ptr(val))
  {
    // printf("Copying %lu", val);
    if (is_tuple(val))
    {
      val = copy_tuple(tuple_ptr(val));
    }
    else if (is_closure(val))
    {
      val = copy_closure(closure_ptr(val));
    }
  }
  return val;
}

u64 *collect(u64 *cur_frame, u64 *cur_sp)
{
  /* TBD: see https://en.wikipedia.org/wiki/Cheney%27s_algorithm */
  // swap from-space to-space
  

  SCAN_PTR = TO_SPACE;
  ALLOC_PTR = TO_SPACE;
  // printf("fs: %lu, ts: %lu, alloc: %lu, SB: %lu, sp:%lu\n", FROM_SPACE, TO_SPACE, ALLOC_PTR, STACK_BOTTOM, cur_sp);

  u64 *stack_scan = STACK_BOTTOM;
  // printf("collecting");
  while (stack_scan > cur_sp)
  {
    u64 stack_val = *stack_scan;
    stack_val = try_copy(stack_val);
    *stack_scan = stack_val;
    stack_scan--;
  }

  for (u64 i = 0; i < HEAP_SIZE; i++)
  {
    FROM_SPACE[i] = 0;
  }
  

  // init spaces
  // scan stack and copy roots
  // scan objects in the heap
  // clean old space
  u64* old_FROM_SPACE = FROM_SPACE;
  
  FROM_SPACE = TO_SPACE;
  TO_SPACE = old_FROM_SPACE;
  return ALLOC_PTR;
}

/* trigger GC if enabled and needed, out-of-memory error if insufficient */
u64 *try_gc(u64 *alloc_ptr, u64 words_needed, u64 *cur_frame, u64 *cur_sp)
{

  if (USE_GC == 1 && ((alloc_ptr + words_needed) > (FROM_SPACE + HEAP_SIZE)))
  {
    printf("| need memory: GC!\n");
    // print_stack(cur_frame, cur_sp);
    // print_heaps();
    
    alloc_ptr = collect(cur_frame, cur_sp);
    // print_heaps();
  }

  if (alloc_ptr + words_needed > FROM_SPACE + HEAP_SIZE)
  {
    printf("| Error: out of memory!\n\n");
    print_stack(cur_frame, cur_sp);
    // print_heaps();
    exit(-1);
  }
  return alloc_ptr;
}

// int main(int argc, char** argv) {
//   u64* HEAP = calloc(1024, sizeof(u64));
//   u64 result = our_code_starts_here(HEAP);
//   // printf( PRId64 "\n" );
//   printResult(result);
//   printf("\n");
//   free(HEAP);
//   return 0;
// }

/* start */
int main(int argc, char **argv)
{

  /* stack size config */
  char *stack_size_envvar = getenv("STACK_SIZE");
  if (stack_size_envvar)
    STACK_SIZE = strtoull(stack_size_envvar, NULL, 0);
  // printf("| Setting stack size to %" PRId64 " .\n", STACK_SIZE);
  struct rlimit limit;
  getrlimit(RLIMIT_STACK, &limit);
  limit.rlim_cur = STACK_SIZE < limit.rlim_max ? STACK_SIZE : limit.rlim_max;
  int res = setrlimit(RLIMIT_STACK, &limit);
  if (res != 0)
  {
    printf("| Setting rlimit failed...\n");
  }

  /* GC config */
  char *use_gc_envvar = getenv("USE_GC");
  if (use_gc_envvar)
    USE_GC = strtoull(use_gc_envvar, NULL, 0);
  // printf("| Use GC: %d\n", USE_GC);

  /* heap size config */
  char *heap_size_envvar = getenv("HEAP_SIZE");
  if (heap_size_envvar)
    HEAP_SIZE = strtoull(heap_size_envvar, NULL, 0);
  // printf("| Heap size: %" PRId64 " .\n", HEAP_SIZE);

  /* setting up two space heap for GC */
  u64 *heap = (u64 *)calloc((HEAP_SIZE * 2) + 15, sizeof(u64));
  HEAP_START = (u64 *)(((u64)heap + 15) & ~0xF);
  /* TBD: initialize HEAP_MID, HEAP_END, FROM_SPACE, TO_SPACE */
  HEAP_MID = &(HEAP_START[HEAP_SIZE]); /* TBD */

  HEAP_END = &(HEAP_START[HEAP_SIZE*2-1]); /* TBD */
  TO_SPACE = HEAP_MID;
  FROM_SPACE = HEAP_START;
  ALLOC_PTR = FROM_SPACE;

  // printf(" Positions: start:%p mid:%p end:%p \n", HEAP_START, HEAP_MID, HEAP_END);
  u64 result = our_code_starts_here(ALLOC_PTR);
  printResult(result);
  // print_heaps();
  free(heap);
  printf("\n");
  return 0;
}

/*
 * AVL-trees for CLISP
 * Bruno Haible 1993-1999
 */
/*
 Goal: Keep a set of elements sorted, where every now and then an
 element is added or an element changes its sorting key.
*/
/* ========================================================================
 Specification:

 Settings that have to be provided from the outside:
 Identifier AVLID :
   Identifier that identifies the incarnation of this package
 Type AVL_ELEMENT :
   Type of the elements that are entered into an AVL-tree.
 Function AVL_EQUAL:
    local bool AVL_EQUAL (AVL_ELEMENT element1, AVL_ELEMENT element2);
   determines if two elements are considered equal.
   It is not allowed to store two equal elements in an AVL-tree.
   (I.e.m no element may be stored twice.)
 Type AVL_KEY :
   Type of the key, which is used to sort the AVL-tree.
 Function AVL_KEYOF:
    local AVL_KEY AVL_KEYOF (AVL_ELEMENT element);
   returns the sorting-key of an element, that is located in a AVL-tree.
 Type AVL_SIGNED_INT :
   Signed integer type, wide enough for AVL_COMPARE-values.
   (Usually `sintL'. `signean' is too short.)
 Function AVL_COMPARE:
    local AVL_SIGNED_INT AVL_COMPARE (AVL_KEY key1, AVL_KEY key2);
   returns >0 if key1>key2, <0 if key1<key2, 0 if key1=key2.
 NB: AVL_EQUAL(element1,element2) should imply
     AVL_COMPARE(KEY_OF(element1),KEY_OF(element2)) = 0 ,
     otherwise avl_member and avl_delete do not work!
 Then include avl.c.
 Then, your own struct-type NODE can be defined:
   typedef struct NODE { ...; NODEDATA nodedata; ...; } NODE;
   #define HAVE_NODE  // just in order to indicate, that NODE was defined
 Then include avl.c again.
 If some of the macros NO_AVL_MEMBER, NO_AVL_INSERT[1], NO_AVL_DELETE[1],
 NO_AVL_LEAST, NO_AVL_MOVE, NO_AVL_SORT are defined, some of the
 corresponding functions will not be defined.
 =========================================================================== */

#ifndef __AVL_D_
#define __AVL_D_

/* declarations: */

#ifndef ALLOC
  #ifndef NO_AVL_INSERT
    #define ALLOC(eltype,number)  ((eltype*) malloc((uintL)sizeof(eltype) * (uintL)(number)))
  #endif
  #ifndef NO_AVL_DELETE
    #define FREE(item)  free(item)
  #endif
#endif

#ifndef AVL
  /* A kind of "AVL-Package" for identifiers of types and functions: */
  #define AVL(incarnation,identifier)  CONCAT4(avl_,incarnation,_,identifier)
#endif

#define NODE        AVL(AVLID,node)
#define ELEMENT     AVL_ELEMENT
#define EQUAL       AVL_EQUAL
#define KEY         AVL_KEY
#define KEYOF       AVL_KEYOF
#define HEIGHT      uintBWL
#define MAXHEIGHT   41
#define SIGNED_INT  AVL_SIGNED_INT
#define COMPARE     AVL_COMPARE

#define NODEDATA                                                        \
  struct {                                                              \
    struct NODE * left;  /* left subtree */                             \
    struct NODE * right; /* right subtree */                            \
    HEIGHT height;       /* 1+max(heightof(left),heightof(right)) */    \
    ELEMENT value;       /* element at the top of this tree */          \
  }

#else

/* ------------------------------------------------------------------------ */
/* implementation: */

#ifndef HAVE_NODE
  typedef struct NODE { NODEDATA nodedata; } NODE;
#endif

/* An AVL-tree is either empty or a NODE.
 The empty tree has the height 0, a NODE has as height the maximum of the
 heights of the two subtrees + 1. */
#define EMPTY  ((NODE *) 0)
#define heightof(tree)  ((tree)==EMPTY ? 0 : (tree)->nodedata.height)

/* Invariants of each AVL-tree:
 1. the height of each NODE is correctly calculated as:
    node.height = 1+max(heightof(node.left),heightof(node.right))
 2. The heights of the subtrees of each NODE differ at most by 1:
    | heightof(node.left) - heightof(node.right) | <= 1
 3. For each NODE applies:
    forall x in node.left : COMPARE(KEYOF(x.value),KEYOF(node.value)) <= 0,
    forall x in node.right : COMPARE(KEYOF(x.value),KEYOF(node.value)) >= 0.
 A AVL-tree of height h has at least F_(h+2) [Fibonacci-number] and
 at most 2^h - 1 elements. So, h<=41 (because a tree of height h>=42 would
 have at least F_44 elements, and because of sizeof(NODE) * F_44 > 2^32, this
 does not fit into a 32-Bit-address space.)
 That is why a uintB is big enough for holding HEIGHT. */

/* Determines if an element with a given key is in the tree. */
#ifndef NO_AVL_MEMBER0
local NODE* AVL(AVLID,member0) (KEY key, NODE * tree) {
  loop {
    if (tree == EMPTY)
      return (NODE*)NULL;
    var SIGNED_INT sign = COMPARE(key,KEYOF(tree->nodedata.value));
    if (sign == 0) /* found? */
      return tree;
    if (sign < 0)
      /* key < KEYOF(tree->nodedata.value)  --> search in the left subtree: */
      tree = tree->nodedata.left;
    else
      /* key > KEYOF(tree->nodedata.value)  --> search in the right subtree: */
      tree = tree->nodedata.right;
  }
}
#endif

/* Determines, if a certain element is in the tree.
   Requires that there are no two elements with the same key in the tree. */
#ifndef NO_AVL_MEMBER
local NODE* AVL(AVLID,member) (ELEMENT element, NODE * tree) {
  var KEY key = KEYOF(element);
  loop {
    if (tree == EMPTY)
      return (NODE*)NULL;
    var SIGNED_INT sign = COMPARE(key,KEYOF(tree->nodedata.value));
    if (sign == 0) {
      if (EQUAL(element,tree->nodedata.value)) /* found? */
        return tree;
      else
        return (NODE*)NULL;
    }
    if (sign < 0)
      /* key < KEYOF(tree->nodedata.value)  --> search in the left subtree: */
      tree = tree->nodedata.left;
    else
      /* key > KEYOF(tree->nodedata.value)  --> search in the right subtree: */
      tree = tree->nodedata.right;
  }
}
#endif

/* Rebalances again: On insertion resp. deletion of an element
 in a tree, the sequence nodes[0],...,nodes[k-1] of subtrees
 (with nodes[i+1] = nodes[i] -> (left or right) for all i)
 has to be rebalanced. As the root of a subtree might change
 thereby, all nodes[i] may not be NODE*, but NODE** . */
local void AVL(AVLID,rebalance) (NODE** * nodeplaces_ptr, uintC count) {
  dotimesC(count,count, {
    var NODE** nodeplace = *--nodeplaces_ptr;
    var NODE* node = *nodeplace; /* next subtree to be balanced */
    var NODE* nodeleft = node->nodedata.left;
    var NODE* noderight = node->nodedata.right;
    var HEIGHT heightleft = heightof(nodeleft);
    var HEIGHT heightright = heightof(noderight);
    if (heightright + 1 < heightleft) {
      /* subtree is heavier on the left side, rotate from left to right:

                                  *
                                /   \
                             n+2      n
      */
      var NODE* nodeleftleft = nodeleft->nodedata.left;
      var NODE* nodeleftright = nodeleft->nodedata.right;
      var HEIGHT heightleftright = heightof(nodeleftright);
      if (heightof(nodeleftleft) >= heightleftright) {
        /*
                *                    n+2|n+3
              /   \                  /    \
           n+2      n      -->      /   n+1|n+2
           / \                      |    /    \
         n+1 n|n+1                 n+1  n|n+1  n
        */
        node->nodedata.left = nodeleftright; nodeleft->nodedata.right = node;
        nodeleft->nodedata.height = 1 +
          (node->nodedata.height = 1 + heightleftright);
        *nodeplace = nodeleft;
      } else {
        /*
                *                     n+2
              /   \                 /     \
           n+2      n      -->    n+1     n+1
           / \                    / \     / \
          n  n+1                 n   L   R   n
             / \
            L   R

        */
        nodeleft->nodedata.right = nodeleftright->nodedata.left;
        node->nodedata.left = nodeleftright->nodedata.right;
        nodeleftright->nodedata.left = nodeleft;
        nodeleftright->nodedata.right = node;
        nodeleft->nodedata.height = node->nodedata.height = heightleftright;
        nodeleftright->nodedata.height = heightleft;
        *nodeplace = nodeleftright;
      }
    } else if (heightleft + 1 < heightright) {
      /* subtree is heavier on the right side, rotate from right to left:
         (Analogous to above procedure, only swapped 'left' <--> 'right' .) */
      var NODE* noderightright = noderight->nodedata.right;
      var NODE* noderightleft = noderight->nodedata.left;
      var HEIGHT heightrightleft = heightof(noderightleft);
      if (heightof(noderightright) >= heightrightleft) {
        node->nodedata.right = noderightleft; noderight->nodedata.left = node;
        noderight->nodedata.height = 1 +
          (node->nodedata.height = 1 + heightrightleft);
        *nodeplace = noderight;
      } else {
        noderight->nodedata.left = noderightleft->nodedata.right;
        node->nodedata.right = noderightleft->nodedata.left;
        noderightleft->nodedata.right = noderight;
        noderightleft->nodedata.left = node;
        noderight->nodedata.height = node->nodedata.height = heightrightleft;
        noderightleft->nodedata.height = heightright;
        *nodeplace = noderightleft;
      }
    } else {
      var HEIGHT height = /* new total height */
        (heightleft<heightright ? heightright : heightleft) + 1;
      /* total height of this subtree remains unchanged ->
         the subtrees that contain this one are already balanced. */
      if (height == node->nodedata.height)
        break;
      node->nodedata.height = height;
    }
  });
}

/* Inserts an element into the AVL-tree and returns the new AVL-tree. */
#ifndef NO_AVL_INSERT
local NODE* AVL(AVLID,insert) (ELEMENT value, NODE* tree) {
  var KEY key = KEYOF(value);
  var NODE** nodeplace = &tree;
  var NODE** stack[MAXHEIGHT]; /* a little private-stack */
  var uintC stack_count = 0; /* number of elements on the stack */
  var NODE** * stack_ptr = &stack[0]; /* always = &stack[stack_count] */
  loop {
    var NODE* node = *nodeplace;
    if (node == EMPTY)
      break;
    *stack_ptr++ = nodeplace; stack_count++;
    if (COMPARE(key,KEYOF(node->nodedata.value)) < 0)
      /* key < KEYOF(node->nodedata.value)  --> insert in the left subtree: */
      nodeplace = &node->nodedata.left;
    else
      /* key >= KEYOF(node->nodedata.value) --> insert in the right subtree: */
      nodeplace = &node->nodedata.right;
  }
  var NODE* new_node = ALLOC(NODE,1);
  new_node->nodedata.left = EMPTY;
  new_node->nodedata.right = EMPTY;
  new_node->nodedata.height = 1;
  new_node->nodedata.value = value;
  *nodeplace = new_node;
  AVL(AVLID,rebalance)(stack_ptr,stack_count);
  return tree;
}
#endif
/* Ditto, but without calling ALLOC: */
#ifndef NO_AVL_INSERT1
local NODE* AVL(AVLID,insert1) (NODE* new_node, NODE* tree) {
  var KEY key = KEYOF(new_node->nodedata.value);
  var NODE** nodeplace = &tree;
  var NODE** stack[MAXHEIGHT]; /* a little private-stack */
  var uintC stack_count = 0; /* number of elements on the stack */
  var NODE** * stack_ptr = &stack[0]; /* always = &stack[stack_count] */
  loop {
    var NODE* node = *nodeplace;
    if (node == EMPTY)
      break;
    *stack_ptr++ = nodeplace; stack_count++;
    if (COMPARE(key,KEYOF(node->nodedata.value)) < 0)
      /* key < KEYOF(node->nodedata.value)  --> insert in the left subtree: */
      nodeplace = &node->nodedata.left;
    else
      /* key >= KEYOF(node->nodedata.value) --> insert in the right subtree: */
      nodeplace = &node->nodedata.right;
  }
  new_node->nodedata.left = EMPTY;
  new_node->nodedata.right = EMPTY;
  new_node->nodedata.height = 1;
  *nodeplace = new_node;
  AVL(AVLID,rebalance)(stack_ptr,stack_count);
  return tree;
}
#endif

/* Removes an element from the AVL-tree and returns a new AVL-tree.
   Requires, that there are no two elements with the same key in the tree. */
#ifndef NO_AVL_DELETE
local NODE* AVL(AVLID,delete) (ELEMENT value, NODE* tree) {
  var KEY key = KEYOF(value);
  var NODE** nodeplace = &tree;
  var NODE** stack[MAXHEIGHT]; /* a little private-stack */
  var uintC stack_count = 0; /* number of elements on the stack */
  var NODE** * stack_ptr = &stack[0]; /* always = &stack[stack_count] */
  var NODE* node_to_delete;
  loop {
    var NODE* node = *nodeplace;
    if (node == EMPTY)
      goto fertig; /* element not found */
    *stack_ptr++ = nodeplace; stack_count++;
    var SIGNED_INT sign = COMPARE(key,KEYOF(node->nodedata.value));
    if (sign == 0) {
      if (EQUAL(value,node->nodedata.value)) { /* found? */
        node_to_delete = node; break;
      } else
        goto fertig;
    }
    if (sign < 0)
      /* key < KEYOF(node->nodedata.value)  --> remove in left subtree: */
      nodeplace = &node->nodedata.left;
    else
      /* key > KEYOF(node->nodedata.value)  --> remove in right subtree: */
      nodeplace = &node->nodedata.right;
  }
  var NODE** nodeplace_to_delete = nodeplace;
  /* node_to_delete = *nodeplace_to_delete has to be removed. */
  if (node_to_delete->nodedata.left == EMPTY) {
    /* node_to_delete is replaced by node_to_delete->nodedata.right. */
    *nodeplace_to_delete = node_to_delete->nodedata.right;
    stack_ptr--; stack_count--; /* still no rebalance at *nodeplace_to_delete! */
  } else {
    /* node_to_delete is replaced by the element
       of node_to_delete->nodedata.left that is situated rightmost. */
    var NODE** * stack_ptr_to_delete = stack_ptr;
    var NODE** nodeplace = &node_to_delete->nodedata.left;
    var NODE* node;
    loop {
      node = *nodeplace;
      if (node->nodedata.right == EMPTY)
        break;
      *stack_ptr++ = nodeplace; stack_count++;
      nodeplace = &node->nodedata.right;
    }
    *nodeplace = node->nodedata.left;
    /* node takes the position of node_to_delete: */
    node->nodedata.left = node_to_delete->nodedata.left;
    node->nodedata.right = node_to_delete->nodedata.right;
    node->nodedata.height = node_to_delete->nodedata.height;
    *nodeplace_to_delete = node; /* instead of node_to_delete */
    /* the rebalance-stack (path from the root downwards) does not
       contain node_to_delete anymore, but node: */
    *stack_ptr_to_delete = &node->nodedata.left; /* instead of &node_to_delete->nodedata.left */
  }
  FREE(node_to_delete);
  AVL(AVLID,rebalance)(stack_ptr,stack_count);
 fertig:
  return tree;
}
#endif

/* Removes an element from the AVL-tree and returns a new AVL-tree.
   Without calling FREE. */
#ifndef NO_AVL_DELETE1
/* Determines, where the element node_to_delete (with Key key) is situated
   in the tree. Stores the path at stack_ptr, and returns the new stack_ptr. */
local NODE** * AVL(AVLID,delete1find) (NODE* node_to_delete, KEY key,
                                       NODE* tree, NODE** * stack_ptr) {
  loop {
    if (tree == EMPTY)
      return (NODE***)NULL;
    var SIGNED_INT sign = COMPARE(key,KEYOF(tree->nodedata.value));
    if (sign == 0) {
      /* key = KEYOF(tree->nodedata.value)  --> search in both subtrees: */
      if (tree == node_to_delete)
        return stack_ptr;
      *stack_ptr = &tree->nodedata.left;
      { var NODE*** part = AVL(AVLID,delete1find)(node_to_delete,key,tree->nodedata.left,stack_ptr+1);
        if (part)
          return part;
      }
      *stack_ptr = &tree->nodedata.right;
      { var NODE*** part = AVL(AVLID,delete1find)(node_to_delete,key,tree->nodedata.right,stack_ptr+1);
        if (part)
          return part;
      }
      return (NODE***)NULL;
    }
    if (sign < 0) {
      /* key < KEYOF(tree->nodedata.value)  --> search in the left subtree: */
      *stack_ptr++ = &tree->nodedata.left; tree = tree->nodedata.left;
    } else {
      /* key > KEYOF(tree->nodedata.value)  --> search in the right subtree */
      *stack_ptr++ = &tree->nodedata.right; tree = tree->nodedata.right;
    }
  }
}
local NODE* AVL(AVLID,delete1) (NODE* node_to_delete, NODE* tree) {
  var KEY key = KEYOF(node_to_delete->nodedata.value);
  var NODE** nodeplace = &tree;
  var NODE** stack[MAXHEIGHT]; /* a little private-stack */
  var uintC stack_count = 0; /* number of elements on the stack */
  var NODE** * stack_ptr = &stack[0]; /* always = &stack[stack_count] */
  loop {
    var NODE* node = *nodeplace;
    if (node == EMPTY)
      goto fertig; /* element not found */
    *stack_ptr++ = nodeplace; stack_count++;
    var SIGNED_INT sign = COMPARE(key,KEYOF(node->nodedata.value));
    if (sign == 0) {
      var NODE** * new_stack_ptr =
        AVL(AVLID,delete1find)(node_to_delete,key,node,stack_ptr);
      if (new_stack_ptr) { /* or found somewhere in the tree at node? */
        stack_count += (new_stack_ptr - stack_ptr);
        stack_ptr = new_stack_ptr;
        nodeplace = stack_ptr[-1];
        break;
      } else
        goto fertig; /* not found */
    }
    if (sign < 0)
      /* key < KEYOF(node->nodedata.value)  --> remove in left subtree: */
      nodeplace = &node->nodedata.left;
    else
      /* key > KEYOF(node->nodedata.value)  --> remove in right subtree: */
      nodeplace = &node->nodedata.right;
  }
  {
    /* stack_ptr = &stack[stack_count], nodeplace = stack_ptr[-1], */
    var NODE** nodeplace_to_delete = nodeplace;
    /* node_to_delete = *nodeplace_to_delete has to be removed. */
    if (node_to_delete->nodedata.left == EMPTY) {
      /* node_to_delete is replaced by node_to_delete->nodedata.right. */
      *nodeplace_to_delete = node_to_delete->nodedata.right;
      stack_ptr--; stack_count--; /* still no rebalance at *nodeplace_to_delete! */
    } else {
      /* node_to_delete is replaced by the element
         of node_to_delete->nodedata.left that is situated rightmost. */
      var NODE** * stack_ptr_to_delete = stack_ptr;
      var NODE** nodeplace = &node_to_delete->nodedata.left;
      var NODE* node;
      loop {
        node = *nodeplace;
        if (node->nodedata.right == EMPTY)
          break;
        *stack_ptr++ = nodeplace; stack_count++;
        nodeplace = &node->nodedata.right;
      }
      *nodeplace = node->nodedata.left;
      /* node takes the position of node_to_delete: */
      node->nodedata.left = node_to_delete->nodedata.left;
      node->nodedata.right = node_to_delete->nodedata.right;
      node->nodedata.height = node_to_delete->nodedata.height;
      *nodeplace_to_delete = node; /* instead of node_to_delete */
      /* the rebalance-stack (path from the root downwards) does not
         contain node_to_delete anymore, but node: */
      *stack_ptr_to_delete = &node->nodedata.left; /* instead of &node_to_delete->nodedata.left */
    }
  }
  AVL(AVLID,rebalance)(stack_ptr,stack_count);
 fertig:
  return tree;
}
#endif

/* Macros for traversing through an AVL-tree:
 AVL_map(tree,node,statement);
 A tree is traversed, binding a node at a time and executing the statement.
 order of traversal:
               AVL_map : in order  L N R
     N         AVL_map_reverse : in reversed order  R N L
    / \        AVL_map_preorder : in prefix-order  N L R
   L   R       AVL_map_postorder : in postfix-order  L R N */

typedef struct { NODE* node; bool rightp; } AVL(AVLID,mapstackitem);
typedef AVL(AVLID,mapstackitem) AVL(AVLID,mapstack)[MAXHEIGHT];
#define AVL_map(tree,nodevar,statement)                                 \
    { var NODE* nodevar = (tree);                                       \
      var AVL(AVLID,mapstack) stack; /* a little private-stack */       \
      var uintC stack_count = 0; /* number of elements on the stack */  \
      var AVL(AVLID,mapstackitem) * stack_ptr = &stack[0]; /* always = &stack[stack_count] */ \
      GENTAG(down): /* descend recursively */                           \
        if (nodevar == EMPTY) goto GENTAG(up);                          \
        stack_ptr->node = nodevar;                                      \
        stack_ptr->rightp = false; nodevar = nodevar->nodedata.left;    \
        stack_ptr++; stack_count++;                                     \
        goto GENTAG(down);                                              \
      GENTAG(up): /* climb up again */                                  \
        if (stack_count == 0) goto GENTAG(end);                         \
        stack_count--; stack_ptr--;                                     \
        if (stack_ptr->rightp) goto GENTAG(up);                         \
        nodevar = stack_ptr->node;                                      \
        statement;                                                      \
        stack_ptr->rightp = true; nodevar = nodevar->nodedata.right;    \
        stack_ptr++; stack_count++;                                     \
        goto GENTAG(down);                                              \
      GENTAG(end): ; /* finished */                                     \
    }
#define AVL_map_reverse(tree,nodevar,statement)                         \
    { var NODE* nodevar = (tree);                                       \
      var AVL(AVLID,mapstack) stack; /* a little private-stack */       \
      var uintC stack_count = 0; /* number of elements on the stack */  \
      var AVL(AVLID,mapstackitem) * stack_ptr = &stack[0]; /* always = &stack[stack_count] */ \
      GENTAG(down): /* descend recursively */                           \
        if (nodevar == EMPTY) goto GENTAG(up);                          \
        stack_ptr->node = nodevar;                                      \
        stack_ptr->rightp = true; nodevar = nodevar->nodedata.right;    \
        stack_ptr++; stack_count++;                                     \
        goto GENTAG(down);                                              \
      GENTAG(up): /* climb up again */                                  \
        if (stack_count == 0) goto GENTAG(end);                         \
        stack_count--; stack_ptr--;                                     \
        if (!(stack_ptr->rightp)) goto GENTAG(up);                      \
        nodevar = stack_ptr->node;                                      \
        statement;                                                      \
        stack_ptr->rightp = false; nodevar = nodevar->nodedata.left;    \
        stack_ptr++; stack_count++;                                     \
        goto GENTAG(down);                                              \
      GENTAG(end): ; /* finished */                                     \
    }
#define AVL_map_preorder(tree,nodevar,statement)                        \
    { var NODE* nodevar = (tree);                                       \
      var AVL(AVLID,mapstack) stack; /* a little private-stack */       \
      var uintC stack_count = 0; /* number of elements on the stack */  \
      var AVL(AVLID,mapstackitem) * stack_ptr = &stack[0]; /* always = &stack[stack_count] */ \
      GENTAG(down): /* descend recursively */                           \
        if (nodevar == EMPTY) goto GENTAG(up);                          \
        statement;                                                      \
        stack_ptr->node = nodevar;                                      \
        stack_ptr->rightp = false; nodevar = nodevar->nodedata.left;    \
        stack_ptr++; stack_count++;                                     \
        goto GENTAG(down);                                              \
      GENTAG(up): /* climb up again */                                  \
        if (stack_count == 0) goto GENTAG(end);                         \
        stack_count--; stack_ptr--;                                     \
        if (stack_ptr->rightp) goto GENTAG(up);                         \
        nodevar = stack_ptr->node;                                      \
        stack_ptr->rightp = true; nodevar = nodevar->nodedata.right;    \
        stack_ptr++; stack_count++;                                     \
        goto GENTAG(down);                                              \
      GENTAG(end): ; /* finished */                                     \
    }
#define AVL_map_postorder(tree,nodevar,statement)                       \
    { var NODE* nodevar = (tree);                                       \
      var AVL(AVLID,mapstack) stack; /* a little private-stack */       \
      var uintC stack_count = 0; /* number of elements on the stack */  \
      var AVL(AVLID,mapstackitem) * stack_ptr = &stack[0]; /* always = &stack[stack_count] */ \
      GENTAG(down): /* descend recursively */                           \
        if (nodevar == EMPTY) goto GENTAG(up);                          \
        stack_ptr->node = nodevar;                                      \
        stack_ptr->rightp = false; nodevar = nodevar->nodedata.left;    \
        stack_ptr++; stack_count++;                                     \
        goto GENTAG(down);                                              \
      GENTAG(up): /* climb up again */                                  \
        if (stack_count == 0) goto GENTAG(end);                         \
        stack_count--; stack_ptr--;                                     \
        nodevar = stack_ptr->node;                                      \
        if (stack_ptr->rightp) { statement; goto GENTAG(up); }          \
        stack_ptr->rightp = true; nodevar = nodevar->nodedata.right;    \
        stack_ptr++; stack_count++;                                     \
        goto GENTAG(down);                                              \
      GENTAG(end): ; /* finished */                                     \
    }

/* example of application of AVL(AVLID,least) and AVL(AVLID,move):
   { var NODE* tree = ...;
     var KEY limit = ...;
     // search in tree after the smallest Key >= limit:
     var AVL(AVLID,stack) stack;
     var NODE* bestfit = AVL(AVLID,least)(limit,&tree,&stack);
     if (bestfit == EMPTY) { error(); }
     // Now COMPARE(KEYOF(bestfit->nodedata.value),limit) >= 0.
     ...; KEYOF(bestfit->nodedata.value) -= limit; ...;
     // reposition the found and modified element in the AVL-tree:
     AVL(AVLID,move)(&stack);
   } */

typedef struct { uintC count; NODE** path[MAXHEIGHT]; } AVL(AVLID,stack);

/* returns the element from a AVL-tree, whose key is the smallest possible, but
   still >= than a given limit. (EMPTY, if all elements are < limit.)
   Thereto as a preparation for the deletion, the path from the root downwards
   to that element (inclusive, i.e. result = stack->path[stack->count-1] ). */
#ifndef NO_AVL_LEAST
local NODE* AVL(AVLID,least) (KEY limit, NODE** tree_ptr,
                              AVL(AVLID,stack) * stack) {
  var NODE* mark = EMPTY;
  var uintC markdepth = 0;
  var NODE** nodeplace = tree_ptr;
  var uintC nodedepth = 0;
  /* mark = current subtree, node = last considered element within.
     markdepth = stack depth up to mark, nodedepth = stack depth up to node.
     markdepth <= nodedepth. */
  loop {
    stack->path[nodedepth++] = nodeplace;
    var NODE* node = *nodeplace;
    /* all elements with Key >= Limit are either in the subtree
       below the node or to the right of mark (including mark). */
    if (node==EMPTY)
      break;
    if (COMPARE(KEYOF(node->nodedata.value),limit) < 0) {
      /* all elements below the node, that are >= limit, must already be
         situated underneath of node->nodedata.right. */
      nodeplace = &node->nodedata.right;
    } else {
      /* Limit <= node <= mark.
         hence, only consider the subtree underneath of node: */
      mark = node; markdepth = nodedepth;
      nodeplace = &node->nodedata.left;
    }
  }
  /* all element >= Limit are situated to the right of mark
     (including mark). */
  stack->count = markdepth; return mark;
}
#endif

/* Repositions an element within a AVL-tree, after its key has changed. */
#ifndef NO_AVL_MOVE
local void AVL(AVLID,move) (AVL(AVLID,stack) * stack) {
  var uintC stack_count = stack->count; /* number of elements on the stack */
  var NODE** * stack_ptr = &stack->path[stack_count]; /* always = &stack->path[stack_count] */
  /* first step, cf. AVL(AVLID,delete) : */
  var NODE** nodeplace_to_delete = stack_ptr[-1];
  var NODE* node_to_delete = *nodeplace_to_delete; /* element to be removed */
  /* node_to_delete = *nodeplace_to_delete is to be removed. */
  if (node_to_delete->nodedata.left == EMPTY) {
    /* node_to_delete is replaced by node_to_delete->nodedata.right. */
    *nodeplace_to_delete = node_to_delete->nodedata.right;
    stack_ptr--; stack_count--; /* no rebalance at *nodeplace_to_delete! */
  } else {
    /* node_to_delete is replaced by the rightmost
       element of node_to_delete->nodedata.left. */
    var NODE** * stack_ptr_to_delete = stack_ptr;
    var NODE** nodeplace = &node_to_delete->nodedata.left;
    var NODE* node;
    loop {
      node = *nodeplace;
      if (node->nodedata.right == EMPTY)
        break;
      *stack_ptr++ = nodeplace; stack_count++;
      nodeplace = &node->nodedata.right;
    }
    *nodeplace = node->nodedata.left;
    /* node is positioned at node_to_delete: */
    node->nodedata.left = node_to_delete->nodedata.left;
    node->nodedata.right = node_to_delete->nodedata.right;
    node->nodedata.height = node_to_delete->nodedata.height;
    *nodeplace_to_delete = node; /* instead of node_to_delete */
    /* the rebalance-stack (path from the root downwards) does not
       contain node_to_delete anymore, but node: */
    *stack_ptr_to_delete = &node->nodedata.left; /* instead of &node_to_delete->nodedata.left */
  }
  AVL(AVLID,rebalance)(stack_ptr,stack_count);
  /* second step, cf. AVL(AVLID,insert) : */
  var KEY key = KEYOF(node_to_delete->nodedata.value);
  var NODE** nodeplace = stack->path[0]; /* = &tree */
  stack_count = 0; stack_ptr = &stack->path[0];
  loop {
    var NODE* node = *nodeplace;
    if (node == EMPTY)
      break;
    *stack_ptr++ = nodeplace; stack_count++;
    if (COMPARE(key,KEYOF(node->nodedata.value)) < 0)
      /* key < KEYOF(node->nodedata.value)  --> insert in the left subtree: */
      nodeplace = &node->nodedata.left;
    else
      /* key >= KEYOF(node->nodedata.value) --> insert in the right subtree: */
      nodeplace = &node->nodedata.right;
  }
  node_to_delete->nodedata.left = EMPTY;
  node_to_delete->nodedata.right = EMPTY;
  node_to_delete->nodedata.height = 1;
  *nodeplace = node_to_delete;
  AVL(AVLID,rebalance)(stack_ptr,stack_count);
}
#endif

/* Sorts the AVL-tree, after the keys have changed and
   returns the new AVL-tree. */
#ifndef NO_AVL_SORT
local NODE* AVL(AVLID,sort) (NODE* tree) {
  var NODE* new_tree = EMPTY;
  AVL_map_postorder(tree,node, new_tree = AVL(AVLID,insert1)(node,new_tree); );
  return new_tree;
}
#endif

#ifdef DEBUG_AVL
/* prints an AVL-tree. */
local void AVL(AVLID,out) (NODE* tree) {
  if (tree!=EMPTY) {
    printf("(");
    if (!(tree->nodedata.left==EMPTY)) {
      AVL(AVLID,out)(tree->nodedata.left); printf("<");
    }
    printf("%lx",tree);
    if (!(tree->nodedata.right==EMPTY)) {
      printf(">"); AVL(AVLID,out)(tree->nodedata.right);
    }
    printf(")");
  }
}
#endif

#ifdef DEBUG_AVL
/* check the invariants of an AVL-tree: */
local void AVL(AVLID,check) (NODE* tree);
local void AVL(AVLID,checkleft) (NODE* tree, KEY key);
local void AVL(AVLID,checkright) (NODE* tree, KEY key);
local void AVL(AVLID,check) (NODE* tree) {
  /* check rules 1 and 2: */
  AVL_map_postorder(tree,node, {
    var HEIGHT h = node->nodedata.height;
    var HEIGHT hl = heightof(node->nodedata.left);
    var HEIGHT hr = heightof(node->nodedata.right);
    if (!(   ((h == hl+1) && (hr <= hl) && (hl <= hr+1))
          || ((h == hr+1) && (hl <= hr) && (hr <= hl+1))))
      abort();
  });
  /* check rule 3: */
  AVL_map(tree,node, {
    AVL(AVLID,checkleft)(node->nodedata.left,KEYOF(node->nodedata.value));
    AVL(AVLID,checkright)(node->nodedata.right,KEYOF(node->nodedata.value));
  });
}
/* check, if all elements of tree have a value <= key : */
local void AVL(AVLID,checkleft) (NODE* tree, KEY key) {
  AVL_map(tree,node,
          if (!( COMPARE(KEYOF(node->nodedata.value),key) <= 0)) abort(););
}
/* check, if all elements of tree have a value >= key : */
local void AVL(AVLID,checkright) (NODE* tree, KEY key) {
  AVL_map(tree,node,
          if (!( COMPARE(KEYOF(node->nodedata.value),key) >= 0)) abort(););
}
#endif

#undef heightof

#endif

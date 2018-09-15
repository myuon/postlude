[[src]](https://github.com/ghc/ghc/tree/master/compiler/prelude/PrimOp.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Primitive operations (machine-level)

# \subsection[PrimOp-datatype]{Datatype for @PrimOp@ (an enumeration)}


These are in \tr{state-interface.verb} order.


# \subsection[PrimOp-info]{The essential info about each @PrimOp@}


The @String@ in the @PrimOpInfos@ is the ``base name'' by which the user may
refer to the primitive operation.  The conventional \tr{#}-for-
unboxed ops is added on later.

The reason for the funny characters in the names is so we do not
interfere with the programmer's Haskell name spaces.

We use @PrimKinds@ for the ``type'' information, because they're
(slightly) more convenient to use than @TyCons@.


# \subsubsection{Strictness}


Not all primops are strict!


# \subsubsection{Fixity}


# \subsubsection[PrimOp-comparison]{PrimOpInfo basic comparison ops}


@primOpInfo@ gives all essential information (from which everything
else, notably a type, can be constructed) for each @PrimOp@.



Here are a load of comments from the old primOp info:

A @Word#@ is an unsigned @Int#@.

@decodeFloat#@ is given w/ Integer-stuff (it's similar).

@decodeDouble#@ is given w/ Integer-stuff (it's similar).

Decoding of floating-point numbers is sorta Integer-related.  Encoding
is done with plain ccalls now (see PrelNumExtra.hs).

A @Weak@ Pointer is created by the @mkWeak#@ primitive:

In practice, you'll use the higher-level

        data Weak v = Weak# v
        mkWeak :: k -> v -> IO () -> IO (Weak v)

The following operation dereferences a weak pointer.  The weak pointer
may have been finalized, so the operation returns a result code which
must be inspected before looking at the dereferenced value.

Only look at v if the Int# returned is /= 0 !!

The higher-level op is

        deRefWeak :: Weak v -> IO (Maybe v)

Weak pointers can be finalized early by using the finalize# operation:

The Int# returned is either

        0 if the weak pointer has already been finalized, or it has no
          finalizer (the third component is then invalid).

        1 if the weak pointer is still alive, with the finalizer returned
          as the third component.

A {\em stable name/pointer} is an index into a table of stable name
entries.  Since the garbage collector is told about stable pointers,
it is safe to pass a stable pointer to external systems such as C
routines.

It may seem a bit surprising that @makeStablePtr#@ is a @IO@
operation since it doesn't (directly) involve IO operations.  The
reason is that if some optimisation pass decided to duplicate calls to
@makeStablePtr#@ and we only pass one of the stable pointers over, a
massive space leak can result.  Putting it into the IO monad
prevents this.  (Another reason for putting them in a monad is to
ensure correct sequencing wrt the side-effecting @freeStablePtr@
operation.)

An important property of stable pointers is that if you call
makeStablePtr# twice on the same object you get the same stable
pointer back.

Note that we can implement @freeStablePtr#@ using @_ccall_@ (and,
besides, it's not likely to be used from Haskell) so it's not a
primop.

Question: Why @RealWorld@ - won't any instance of @_ST@ do the job? [ADR]

# Names


A stable name is like a stable pointer, but with three important differences:

        (a) You can't deRef one to get back to the original object.
        (b) You can convert one to an Int.
        (c) You don't need to 'freeStableName'

The existence of a stable name doesn't guarantee to keep the object it
points to alive (unlike a stable pointer), hence (a).

Invariants:

        (a) makeStableName always returns the same value for a given
            object (same as stable pointers).

        (b) if two stable names are equal, it implies that the objects
            from which they were created were the same.

        (c) stableNameToInt always returns the same Int for a given
            stable name.


These primops are pretty weird.

        dataToTag# :: a -> Int    (arg must be an evaluated data type)
        tagToEnum# :: Int -> a    (result type must be an enumerated type)

The constraints aren't currently checked by the front end, but the
code generator will fall over if they aren't satisfied.

# Which PrimOps are out-of-line


Some PrimOps need to be called out-of-line because they either need to
perform a heap check or they block.


# Failure and side effects


### Note: PrimOp can_fail and has_side_effects

Both can_fail and has_side_effects mean that the primop has
some effect that is not captured entirely by its result value.

----------  has_side_effects ---------------------
A primop "has_side_effects" if it has some *write* effect, visible
elsewhere
    - writing to the world (I/O)
    - writing to a mutable data structure (writeIORef)
    - throwing a synchronous Haskell exception

Often such primops have a type like
   State -> input -> (State, output)
so the state token guarantees ordering.  In general we rely *only* on
data dependencies of the state token to enforce write-effect ordering

 * NB1: if you inline unsafePerformIO, you may end up with
   side-effecting ops whose 'state' output is discarded.
   And programmers may do that by hand; see Trac #9390.
   That is why we (conservatively) do not discard write-effecting
   primops even if both their state and result is discarded.

 * NB2: We consider primops, such as raiseIO#, that can raise a
   (Haskell) synchronous exception to "have_side_effects" but not
   "can_fail".  We must be careful about not discarding such things;
   see the paper "A semantics for imprecise exceptions".

 * NB3: *Read* effects (like reading an IORef) don't count here,
   because it doesn't matter if we don't do them, or do them more than
   once.  *Sequencing* is maintained by the data dependency of the state
   token.

----------  can_fail ----------------------------
A primop "can_fail" if it can fail with an *unchecked* exception on
some elements of its input domain. Main examples:
   division (fails on zero demoninator)
   array indexing (fails if the index is out of bounds)

An "unchecked exception" is one that is an outright error, (not
turned into a Haskell exception,) such as seg-fault or
divide-by-zero error.  Such can_fail primops are ALWAYS surrounded
with a test that checks for the bad cases, but we need to be
very careful about code motion that might move it out of
the scope of the test.

### Note: Transformations affected by can_fail and has_side_effects

The can_fail and has_side_effects properties have the following effect
on program transformations.  Summary table is followed by details.

            can_fail     has_side_effects
Discard        YES           NO
Float in       YES           YES
Float out      NO            NO
Duplicate      YES           NO

  However, it's fine to discard a can_fail primop.  For example
     case (indexIntArray# a i) of _ -> True
  We can discard indexIntArray#; it has can_fail, but not
  has_side_effects; see Trac #5658 which was all about this.
  Notice that indexIntArray# is (in a more general handling of
  effects) read effect, but we don't care about that here, and
  treat read effects as *not* has_side_effects.

  Similarly (a `/#` b) can be discarded.  It can seg-fault or
  cause a hardware exception, but not a synchronous Haskell
  exception.



  Synchronous Haskell exceptions, e.g. from raiseIO#, are treated
  as has_side_effects and hence are not discarded.

* Float in.  You can float a can_fail or has_side_effects primop
  *inwards*, but not inside a lambda (see Duplication below).

* Float out.  You must not float a can_fail primop *outwards* lest
  you escape the dynamic scope of the test.  Example:
      case d ># 0# of
        True  -> case x /# d of r -> r +# 1
        False -> 0
  Here we must not float the case outwards to give
      case x/# d of r ->
      case d ># 0# of
        True  -> r +# 1
        False -> 0

  (All these bindings are boxed.)  If we inline p at its two call
  sites, we get a catastrophe: because the read is performed once when
  s' is demanded, and once when 'r' is demanded, which may be much
  later.  Utterly wrong.  Trac #3207 is real example of this happening.

  However, it's fine to duplicate a can_fail primop.  That is really
  the only difference between can_fail and has_side_effects.

### Note: Implementation: how can_fail/has_side_effects affect transformations

How do we ensure that that floating/duplication/discarding are done right
in the simplifier?

Two main predicates on primpops test these flags:
  primOpOkForSideEffects <=> not has_side_effects
  primOpOkForSpeculation <=> not (has_side_effects || can_fail)

  * The "no-float-out" thing is achieved by ensuring that we never
    let-bind a can_fail or has_side_effects primop.  The RHS of a
    let-binding (which can float in and out freely) satisfies
    exprOkForSpeculation; this is the let/app invariant.  And
    exprOkForSpeculation is false of can_fail and has_side_effects.

  * So can_fail and has_side_effects primops will appear only as the
    scrutinees of cases, and that's why the FloatIn pass is capable
    of floating case bindings inwards.

  * The no-duplicate thing is done via primOpIsCheap, by making
    has_side_effects things (very very very) not-cheap!


### Note: primOpIsCheap

@primOpIsCheap@, as used in \tr{SimplUtils.hs}.  For now (HACK
WARNING), we just borrow some other predicates for a
what-should-be-good-enough test.  "Cheap" means willing to call it more
than once, and/or push it inside a lambda.  The latter could change the
behaviour of 'seq' for primops that can fail, so we don't treat them as cheap.


# PrimOp code size


primOpCodeSize
~~~~~~~~~~~~~~

Gives an indication of the code size of a primop, for the purposes of
calculating unfolding sizes; see CoreUnfold.sizeExpr.


# PrimOp types



We do not currently make use of whether primops are commutable.

We used to try to move constants to the right hand side for strength
reduction.



commutableOp :: PrimOp -> Bool
#include "primop-commutable.hs-incl"


# \subsubsection[PrimCall]{User-imported primitive calls}

;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : procedural.lisp
;;; Version     : 2.0a1
;;; 
;;; Description : Implements the procedural module (productions).
;;; 
;;; Bugs        : 
;;;
;;; To do       : [x] Investigate possible problem with !eval! and variables.
;;;             : [ ] Double check that conflict set ordering is right with
;;;             :     respect to :er and when the learning is turned on.  It
;;;             :     seems right with the basic testing, but I want to really
;;;             :     investigate that to be sure when I have a chance.
;;;             : [ ] Fix the production structure accessors so that they're
;;;             :     different from the extended parameter accessors...
;;;             : [ ] Why is print-production-text not using the pre-parsed
;;;             :     production components and why isn't it in with pm-commands?
;;;             : [ ] Revisit the fix for search buffer bindings for efficiency
;;;             :     when I get some more "real world" test cases.
;;; ----- History -----
;;;
;;; 2004.11.24 : Dan
;;;              Creation.
;;; 2005.01.05 : Dan
;;;            : Fixed PRINT-PRODUCTION-OUTPUT so that it works for all cases
;;;            : correctly.
;;; 2005.01.07 : Dan
;;;            : Fixed a problem that LispWorks has with my use of
;;;            : set-parameter in parameters-fct.
;;; 2005.01.09 : Dan
;;;            : Adding two new tracing parameters :lhst and :rhst which
;;;            : control whether all the buffer actions show in the trace or
;;;            : not.
;;;            : Changed production-selection to production-selected and
;;;            : production-action to production-fired
;;; 2005.01.13 Dan
;;;            : * Put the device-lock in place when a production is going
;;;            :   to issue a +visual.  Took a little bit of trickery since
;;;            :   the production needs to actually lock things down after
;;;            :   conflict-resolution and before anything else is allowed
;;;            :   to occur.  It can't schedule that to occur because it
;;;            :   must be atomic and there wasn't a mechanism for that nor
;;;            :   anything in the production to indicate such.
;;;            : * Fixed an error in how it handles the return value from the
;;;            :   conflict-set-hook because production-selected wanted the
;;;            :   actual production and not just the name.
;;; 2005.01.15 Dan
;;;            : * Moved the user functions to a file in the tools directory.
;;; 2005.01.16 Dan
;;;            : * Added the delayed-resolution slot so that non-scheduled 
;;;            :   changes to procedural (sgp, spp, or new/changed production) 
;;;            :   can force conflict-resolution back into the event queue.
;;; 2005.01.17 Dan
;;;            : * Removed calls to format in the scheduling.
;;; 2005.01.18 Dan
;;;            : * Removed calls to get-parameters.
;;; 2005.01.19 Dan
;;;            : * Updates to conflict resolution and production struct to
;;;            :   enable production breaks and disabling.
;;;            : * Added support for the user functions.
;;; 2005.02.03 Dan
;;;             : * Added ":output 'medium"  or ":output 'low" to some of the 
;;;             :   events scheduled to play friendly with the new detail level.
;;; 2005.02.09 Dan
;;;             : * Added the *-parse-table slots to make production parsing
;;;             :   faster on a reset (if it's not a reload).
;;; 2005.02.10 Dan
;;;             : * Swtched to using expt-coerced.
;;; 2005.02.13 Dan
;;;             : * Some modifications to conflict-resolution trying to speed
;;;             :   things up.  Needs to be tested and cleaned up...
;;; 2005.03.18 Dan
;;;             : * Adding the :do-not-harvest parameter to make "strict-
;;;             :   harvesting" more of a module writer/user level control 
;;;             :   instead of an architectural mandate.
;;; 2005.03.21 Dan
;;;             : * Updating the version number with the change to the
;;;             :   strict harvesting mechanism. Now 1.0b1.
;;; 2005.04.08 Dan
;;;             : * Actually make cst do something...
;;; 2005.04.14 Dan
;;;             : * Changed :conflict-set-hook so that nil actually clears it.
;;; 2005.04.19 Dan
;;;             : * Added pprint-instantiation for users/my use.
;;; 2005.04.26 Dan
;;;             : * Added the :cycle-hook parameter that gets called when
;;;             :   a production "fires".  It gets passed the name of the
;;;             :   production.
;;; 2005.05.02 Dan
;;;             : * Changed the print-production-text function so that keywords
;;;             :   for request-parameters show up correctly.
;;;             : * Added the buffer-indices and rhs-buffers slots to the 
;;;             :   production structure for compilation's use.
;;; 2005.05.03 Dan
;;;             : * Changed the utility-?-hook parameters to not report being
;;;             :   overwritten if it's with nil.
;;;             : * Fixed print-production-text to show direct requests
;;;             :   and direct assignments correctly.
;;; 2005.05.05 Dan
;;;             : * Added the standard-rep slot to productions for use in
;;;             :   production compilation.
;;;             : * Patched up a minor error introduced into print-production-
;;;             :   text dealing with numbers in rhs modification requests.
;;; 2005.05.18 Dan
;;;             : * Fixed a bug in conflict-resolution that crashed if a 
;;;             :   production had no LHS tests.
;;; 2005.06.15 Dan
;;;             : * The conflict-set passed to the conflict-set-hook function
;;;             :   wasn't ordered such that the car was the production that
;;;             :   would be fired next.  That's a problem for the stepper in
;;;             :   the environment, so I've fixed that.
;;;             : * Updated version to 1.0b2.
;;;             : * Fixed the cst so that it doesn't print if v is nil (because
;;;             :   it was using command-output it didn't get stopped on its
;;;             :   own).
;;; 2005.06.17 Dan 
;;;             : * Adjusted :ut so that it will also take nil to turn off
;;;             :   utility threshold.
;;; 2005.08.03 Dan
;;;             : * Added specific hooks for probability and cost computiations
;;;             :   when learning is enabled:
;;;             :   utility-learned-p-hook and utility-learned-c-hook
;;;             :   These are a lower priority than the p and c hooks.  So, if 
;;;             :   those are set, then these won't even get called.
;;;             :   The important note is that right now production compilation
;;;             :   uses the major p and c hooks, so if you turn on production
;;;             :   compilation essentially all of the utility hooks are off
;;;             :   limits for user's use.
;;; 2005.09.01 Dan
;;;             : * Added a new slot to the procedural struct to let me flag
;;;             :   whether or not to check the validity of an = action when
;;;             :   parsing a p*.  The reason for that is because production
;;;             :   compilation could produce a production that modifies a
;;;             :   slot which gets added by the second production that doesn't
;;;             :   exist until after the 2nd production fires.
;;;             : * To avoid problems using the flag the default is to test
;;;             :   as always and there's a with-unchecked-p* macro one needs
;;;             :   to wrap around a call to p* to not do the check.
;;; 2005.09.14 Dan
;;;             : * Big change!
;;;             :   The priorities of the RHS actions are being reordered so 
;;;             :   that modifications (=) now have a higher priority than
;;;             :   requests (+).
;;;             :   I don't expect any problems to be generated from this for
;;;             :   the core or other existing modules.
;;;             :   However, it does now mean that it isn't (easily) possible
;;;             :   for a module to "know" the contents of the buffers at the
;;;             :   time of the production matching when a request is sent.
;;;             :   The reason for the change is driven mainly by the use of
;;;             :   direct requests within a production that also makes a 
;;;             :   modification.  The old way reesulted in outcomes different
;;;             :   from what ACT-R 5 would do and didn't seem "obvious".
;;;             :   A prototypical example would be this production:
;;;             :   (p test
;;;             :      =goal>
;;;             :        isa target
;;;             :        state =next
;;;             :     ==>
;;;             :      +retrieval> =goal
;;;             :        
;;;             :      =goal>
;;;             :        state encode
;;;             :        
;;;             :      -goal>)
;;;             :   
;;;             :   Which under the old ordering was almost guaranteed to never
;;;             :   result in the the "old" goal chunk being retrieved (unless
;;;             :   =next actually was bound to encode or partial matching let
;;;             :   a match happen).
;;;             : * Updating the module version to 1.0.
;;; 2005.12.19 Dan
;;;             : * Moved the utility computations to the utility-and-reward
;;;             :   file in the modules directory.
;;; 2005.12.21 Dan
;;;             : * Started work on making productions use a flexible parameter
;;;             :   system like chunks have.
;;;             : * Removed dependence on production-effort and production-c
;;;             :   for action timeing - now it calls productions-action-time
;;;             :   to get the value which must be defined elsewhere - now
;;;             :   its in utility-and-reward with the other parameter dependent
;;;             :   code.
;;; 2006.01.03 Dan
;;;             : * Modified extend-productions like I did to extend-chunks so
;;;             :   that the explicit compile call isn't necessary to guarantee
;;;             :   that the functions get compiled.
;;; 2006.01.25 Dan
;;;             : * Slightly wild change here - the procedural module will now
;;;             :   have a buffer so that it can be queried for its state just
;;;             :   like any of the other modules.  It won't use the buffer for
;;;             :   storing chunks, but it will use it to handle the production
;;;             :   selection event (so that it can be traced more easily).
;;;             :   The motivation behind this is purely functional at this 
;;;             :   point - it allows me to implement a "buffer based" trace
;;;             :   that tracks the state of the modules (which can only be done
;;;             :   through their buffers).  However, one could imagine that
;;;             :   it may be necessary to track the module's state for meta-
;;;             :   cognitive purposes, so it's not entirely unreasonable.
;;; 2006.02.20 Dan
;;;             : * Modified extend-productions to match the fix done to extend-
;;;             :   chunks even though productions don't suffer the same issue
;;;             :   because there's no merging.
;;; 2006.03.09 Dan
;;;             : * Removed all of the utility computation code and removed the
;;;             :   corresponding parameters from the procedural module.  They
;;;             :   now belong to the utility module.  There is an API given in
;;;             :   the utility-and-reward.lisp file that shows what's needed
;;;             :   by procedural, but otherwise one can now completely change
;;;             :   utility without having to touch the procedural module.
;;; 2006.03.10 Dan
;;;             : * Changed print-production to print when a production is 
;;;             :   disabled.
;;; 2006.03.10 Dan
;;;             : * Fixed a bug in print-production-text that broke for this
;;;             :   sequence in the actions: +goal> =val !output! ("...").
;;; 2006.03.10 Dan
;;;             : * Removed print-production-condition because it wasn't being
;;;             :   used anymore by anything.
;;; 2006.03.10 Dan
;;;             : * Modified the extend-production macro so that the accessor
;;;             :   works with the production name instead of the actual
;;;             :   structure.  This makes things a bit more confusing because
;;;             :   the "extended" accessors still look just like the actual
;;;             :   structure accessors but take a different value.  
;;;             :   The to do list has been modified to attend to this.
;;; 2006.03.10 Dan
;;;             : * Calling compile-productions explicitly now from production-
;;;             :   fired instead of implicitly throught he cycle-hook (makes
;;;             :   more sense now that things are stable).
;;; 2006.06.26 Dan
;;;             : * Fixed a minor bug in print-production-text that would 
;;;             :   misprint productions that had chunk-types that end in a > or
;;;             :   a ! in it (like phrase! from the vision module). 
;;;             : * Could still be problems if someone names a chunk isa and
;;;             :   uses it in a direct request (though you can't actually do
;;;             :   that because define-chunks won't allow it).
;;; 2006.11.07 Dan
;;;             : * Fixed a bug in get-production which caused errors if there
;;;             :   were more than one model defined and it was called with no
;;;             :   current model (not an issue for running models).
;;; 2006.11.08 Dan
;;;             : * Added the option of having production firing times be 
;;;             :   noisy (using the randomize-time command like the PM 
;;;             :   components do).  There's a new parameter :vpft (variable
;;;             :   production firing times) which defaults to nil, but if it
;;;             :   and :randomize-time are both true, then production
;;;             :   firing times will be randomized.
;;;             : * Updated the module version to 1.3.
;;;             : * Changed how the conflict-set-hook works slightly.  Now one
;;;             :   can explicitly cancel the selection of all productions with
;;;             :   the hook and have the reason output to the trace and there 
;;;             :   will be warnings printed if invalid values are returned i.e.
;;;             :   productions which weren't on the conflict set or any other
;;;             :   non-string values (which before would implicitly cancel the
;;;             :   selection process).
;;;             : * Changed un-delay-conflict-resolution to not break when there
;;;             :   is no current model and to always return nil.
;;; 2006.11.10 Dan
;;;             : * Added the dynamic slot to the production structure so that
;;;             :   there's an easy flag for determining whether a production
;;;             :   was created with p or p* (should parse the production to check
;;;             :   because a p* may not actually be dynamic but this is the
;;;             :   easy test for now).
;;;             : * Changed print-production-text so that it properly indicates
;;;             :   if it's a p or p* (based on the dynamic slot of the production)
;;;             :   and got rid of the pre-formating before sending to command-output
;;;             :   (why was it like that?).
;;;             : * DOH! I know why it was like that...and I've undone that 
;;;             :   change now - the issue for future reference is that if no-output
;;;             :   is wrapped around a call to command-output the args to command-
;;;             :   output are not evaluated.  Thus if they have important side effects
;;;             :   you need to be careful.
;;; 2006.11.15 Dan
;;;             : * Fixed a bug with un-delay-conflict-resolution which could
;;;             :   result in the model being stuck executing conflict-resolution
;;;             :   events without advancing the clock...
;;; 2006.03.29 Dan
;;;             : * Finally got around to fixing an issue that was pointed out
;;;             :   a while ago with print-produciton-text.  If there were any
;;;             :   format directives in the production itself (for instance in
;;;             :   a !eval! or !bind! call) then it would break in the 
;;;             :   command-output macro when trying to print.  
;;; 2007.06.27 Dan
;;;             : * Changed the conflict-resolution-hook suppression warning
;;;             :   to be a model-warning so that it goes away when :v nil.
;;; 2007.10.26 Dan
;;;             : * Updated the conflict resolution trace to indicate when a
;;;             :   production isn't selected because it falls below the utility
;;;             :   minimum value.
;;; 2008.03.20 Dan
;;;             : * Added a case for !mv-bind! in the production printing code.
;;; 2008.07.22 Dan
;;;             : * Changed the call to compute-utility in conflict-resolution
;;;             :   to add the optional parameter with a value of t to have it
;;;             :   save the utility value - all other computed values are not
;;;             :   to be saved (for instance pmatches/whynot calls).
;;; 2008.07.31 Dan
;;;             : * Instead of using a table in conflict-resolution for holding
;;;             :   the utilities make that an extra slot of the production 
;;;             :   struct and use the productions instead of the names for the
;;;             :   conflict-set until it needs to go out to the hooks or
;;;             :   elsewhere.
;;; 2008.08.01 Dan
;;;             : * To avoid calling sgp procedural now owns :dat and monitors
;;;             :   :v.
;;; 2008.11.03 Dan
;;;             : * Updated conflict resolution to cache the buffer chunk in an
;;;             :   array and use cr-buffer-read to get them out for LHS tests.
;;; 2008.11.25 Dan
;;;             : * Added some new functions to make changing the internals
;;;             :   easier to improve for performance:
;;;             :   - add-production and remove-production functions abstract
;;;             :   the internal storage method for productions
;;;             :   - get-production-internal is added so that some code doesn't
;;;             :   need to do two look-ups for the procedural module - not
;;;             :   sure why I took that out back in 06...
;;;             : * Added productions-list to abstract away from using procedural-
;;;             :   productions in other places (procedural-cmds in particular).
;;;             : * Added a table to store productions to improve the average
;;;             :   operation of get-production.
;;; 2008.12.05 Dan
;;;             : * Moved away from the production conditions being represented
;;;             :   with lambdas to funcall during conflict resolution.
;;;             : * Also added a table for slot values like I did for buffer
;;;             :   chunks so that conflict-resolution doesn't need to repeatedly
;;;             :   get chunk slot values (multiple hash table lookups).
;;; 2008.12.08 Dan [1.4]
;;;             : * Significant overhaul of the internal representation of 
;;;             :   productions and the mechanisms used for conflict resolution.
;;;             :   No longer create lambdas for the conditions.  Instead, it
;;;             :   creates more specific single condition tests which are used
;;;             :   instead of relying on the general chunk-spec matcher.
;;;             : * Also added code to cache the buffer and slot values tested
;;;             :   so it doesn't have to look up the buffer/chunk/slot value
;;;             :   again and again.
;;; 2008.12.12 Dan
;;;             : * Fixed a bug in test-and-perfrom-bindings (not the typo in
;;;             :   the name however) for !bind! conditions.
;;; 2008.12.23 Dan
;;;             : * Added a new parameter :use-tree which if enabled uses a
;;;             :   decision tree in the conflict-resolution event to speed up
;;;             :   production matching.
;;;             :   The tree gets built when the model is loaded and doesn't
;;;             :   need to be recreated at reset time unless something in the
;;;             :   model has changed.
;;;             :   Should improve performance for most models.
;;; 2009.08.13 Dan [2.0a1]
;;;             : * Added two new parameters to allow for testing out the 
;;;             :   possibility of a "new" idea in the production matching.
;;;             :   The new concept is to allow partial matching to occur on
;;;             :   the LHS in the tests of the buffers' slots.
;;;             :
;;;             :   Here is how it will work at this point:
;;;             :
;;;             :   If the :ppm parameter (procedural partial matching)
;;;             :   is set to nil then there is no change in how production
;;;             :   matching occurs.  This will be the default value and
;;;             :   thus all existing models will continue to work exactly
;;;             :   the same as they did before this was added.
;;;             :   If :ppm is set to a number, then all of the slots tested
;;;             :   for equality on the LHS of a production (those without a
;;;             :   modifier or with the explicit = modifier) will be allowed
;;;             :   to have a partial matching.  The slot value will be 
;;;             :   considered a match if:
;;;             :   
;;;             :   - it is a symbolic match as would normally occur without 
;;;             :   this addition
;;;             :   
;;;             :   OR  
;;;             :   
;;;             :   - there is a similarity value between the indicated value 
;;;             :   and the value in the slot that is > the maximum similarity 
;;;             :   difference
;;;             :   
;;;             :   
;;;             :   The similarity value used is the standard one returned by 
;;;             :   the declarative similarity function and thus the existing 
;;;             :   :similarity-hook can be used if custom similarities are 
;;;             :   needed.  The requirement that it be strictly greater than 
;;;             :   the max difference means that only things which have an 
;;;             :   explicit similarity setting can possibly be mismatched, and 
;;;             :   it provides a way to specify a "don't match these in the 
;;;             :   productions" even if a similarity value may be needed for 
;;;             :   declarative retrieval purposes.
;;;             :   
;;;             :   All slots must still be a match for the production to be 
;;;             :   considered in the conflict set.
;;;             :   
;;;             :   If all of the production's tests are perfect matches then 
;;;             :   its utility will be unaffected.
;;;             :   
;;;             :   If any slot is mismatched, then the utility of the 
;;;             :   production will be adjusted, by default, by adding the 
;;;             :   similarity of all the mismatches times a penalty factor.  
;;;             :   The penalty factor will be the setting of :ppm.  Alternatively, 
;;;             :   there is a hook :ppm-hook which can be used to override the 
;;;             :   utility penalty when slot tests are mismatched.  The hook
;;;             :   function will be passed a production name and a list of 
;;;             :   mismatch lists, one for each mismatch which occurred while
;;;             :   testing the named production. A mismatch list will be a 5 
;;;             :   element list consisting of: a buffer name, a slot name, the 
;;;             :   requested slot value, the actual value in the slot of the 
;;;             :   chunk in the buffer, and the reported similarity between those
;;;             :   two items.  If the hook returns a number that will be added to
;;;             :   the production's utility, any other return value will result in
;;;             :   the default calculation being added.
;;;             :   
;;;             :   It is important to note that this is very exploratory at this
;;;             :   time and may undergo significant changes if it persists at all.
;;; 2009.08.14 Dan
;;;             : * Changed print-instantiation so that it prints partially matched
;;;             :   information when appropriate.
;;; 2009.08.17 Dan
;;;             : * Fixed test-other-condition with respect to PPM so that it 
;;;             :   can partial match a value in a variablized slot in a p*.
;;; 2009.09.10 Dan
;;;             : * Added the require-compiled for the code that's now in production-
;;;             :   parsing-support.lisp.
;;; 2009.09.14 Dan
;;;             : * Simplified conflict-resolution by colapsing the tree/non-tree
;;;             :   branch.
;;;             : * Moved the warning about using the tree with :ppm or :crt on
;;;             :   to reset time instead of every conflict-resolution.
;;; 2009.09.18 Dan
;;;             : * Fixed a bug introduced with moving the warning from the
;;;             :   previous update.
;;;             : * Took the require-compiled of production-parsing out for now
;;;             :   to avoid an issue with when the production structure is
;;;             :   created.
;;; 2009.10.15 Dan
;;;             : * All new conflict-resolution to support the ability of a
;;;             :   production to search a multi-buffer for a chunk to match.
;;;             :   The search occurs after the constants have been tested and
;;;             :   things not dependent on search bindings have occurred (evals
;;;             :   explicit binds and so forth).  A searched buffer cannot
;;;             :   be used to name a slot for dynamic matching, but can have
;;;             :   dynamic slots that are named in standard buffers.
;;;             : * The general description is that the searches occur in parallel
;;;             :   across all the searced buffers and only one match is found
;;;             :   for each.  Then conditions among those searched buffers are
;;;             :   tested.  If there is a failure at that point the production
;;;             :   doesn't match -- it doesn't search for a set of chunks which
;;;             :   satisfy all the conditions.
;;; 2009.12.03 Dan
;;;             : * Added the reporting of utilities to the conflict-set trace.
;;; 2009.12.04 Dan
;;;             : * Flag conflict-resolution events as dynamic when scheduled so
;;;             :   that they can be moved "back" in a real-time slack hook that
;;;             :   uses dynamics.
;;; 2010.01.11 Dan
;;;             : * Updates to the buffer search code to address some problems
;;;             :   in the search code (doesn't affect non-search matching).
;;; 2010.01.12 Dan
;;;             : * Fixed a bug with the utility printing in the conflict-set
;;;             :   trace.
;;; 2010.01.12 Dan
;;;             : * Fixed some issues with the utility-offset code for searched
;;;             :   buffers.
;;; 2010.01.15 Dan
;;;             : * Fixed an issue with test-search-buffers because a module may
;;;             :   be adding the items to the buffer set in the search call.
;;; 2010.01.18 Dan
;;;             : * Added a keyword parameter to conflict-tests to allow the
;;;             :   :crt info to be suppressed during whynot and other user
;;;             :   commands. 
;;; 2010.04.23 Dan
;;;             : * Updates to remove some assumptions about tests and generally
;;;             :   be more strict about ANSI CL requirements (in particular
;;;             :   that test functions don't have to return t for true).
;;;             :   - cr-condition-equal now uses eql for the value tests to
;;;             :     make sure numerical slot conditions can be considered
;;;             :     the same (doesn't affect the matching itself).
;;;             :   - Changed any tests for chunk-slot-equal to safe-chunk-slot-equal
;;;             :     since that's now the function used.
;;; 2010.10.15 Dan
;;;             : * Updates to how the conflict set trace prints because it had
;;;             :   some "unusual" output because of the search buffer addition.
;;; 2010.11.07 Dan
;;;             : * Fixed a potential bug with the search buffer code that could
;;;             :   result in bindings in the selected production from being set
;;;             :   to values from other search results.  [There're actually two
;;;             :   potential fixes here, but one is commented out for now.  The
;;;             :   reason for that is because I don't know which is really more 
;;;             :   efficient without enough test cases, but I think the one I've
;;;             :   used should be the better one in most cases.]
;;; 2010.12.07 Dan
;;;             : * Changed print-production and print-production-text to add
;;;             :   an optional parameter that can be used to direct the output
;;;             :   to the model trace instead of the command trace.  It's just
;;;             :   a bunch of duplicated code with ifs for now...
;;; 2010.12.22 Dan
;;;             : * Adding a run-notify function to the module so that it can
;;;             :   check to see if it needs to schedule a conflict-resolution
;;;             :   action because of an abnormal situaiton (a hard break
;;;             :   during a conflict-resolution processing for example).
;;; 2011.01.11 Dan
;;;             : * Changed :run-notify to :run-start since define-module was
;;;             :   changed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; Important note to module writers:
;;;
;;;   The RHS actions of a production are scheduled with the following
;;;   priorities:
;;;      = priority 100
;;;      = (buffer overwrite) 90
;;;      + priority 50
;;;      - priority 10
;;;
;;;   So, the module does NOT get a chance to look at the buffers before the
;;;   production has its way with them.
;;;  
;;;   The recommended thing to do when you get your request (from a +)
;;;   is to schedule your action for a priority < 10.  The production's
;;;   actions should all occur before your module starts to do anything,
;;;   and you shouldn't change the state of things before the other
;;;   modules have had a chance to look at it if they need to.
;;;   
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; This module is special in that it relies on some of the "internal" framework
;;; code instead of only using the API.  Since this is specific to ACT-R it 
;;; isn't part of the framework, so maybe I need to make more of the internals
;;; available through the API.  
;;;
;;; Unbound RHS variables are currently not useable.  Most uses were as a short-
;;; hand for "- {slot} nil" which can just be made explicit now.  The only other
;;; use (in ACT-R 5 terms at least) was in retrievals for checking that two
;;; slots had the same value.  The question becomes whether that's a desired
;;; thing or just a hold over from when retrievals were LHS tests.  For now,
;;; it's not going to be possible, but if it's wanted it can be added back in
;;; in any of a couple of different ways.
;;;
;;; For now, production parameters can't be set to non-constant values.
;;;
;;; Permanent change is not being able to set production parameters to 
;;; values that use the variable bindings of the instantiation in thier
;;; setting.
;;;
;;; Why isn't there an optimized learning form for p and c? The previous 
;;; didn't have one so I haven't put one in here either.  
;;; Answer: Because it does have one...  It turns out that with :pl t you
;;; get the same result as if you ran it through the equivalent :ol t equation.
;;; One could imagine an :ol # version that used a few specific instances
;;; might be useful, but for now it's not going to change.
;;;
;;; Production chance parameter is gone - productions cannot "fail to fire"
;;; anymore.
;;;
;;; I've come to realize why returning a list with the first element being 
;;; the count and the rest being the references is good for the successes, 
;;; failures and efforts production parameters...  So, that's not going
;;; to change despite my separating those pieces internally.
;;; 
;;; When setting efforts with a number and pl is a number it sets the
;;; list of efforts based on the munber of successes and failures instead
;;; of just successes as was done previously.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; It's going to use the central parameters so make sure they're available

(require-compiled "CENTRAL-PARAMETERS" "ACT-R6:support;central-parameters")

;;; Uses some functions related to parsing productions.

;(require-compiled "PRODUCTION-PARSING" "ACT-R6:support;production-parsing-support")

;;; The structures for the module and a production


(defstruct procedural productions p-table
  er crt cst v dat
  conflict-set-hook 
  cycle-hook
  bindings 
  lhst rhst
  delayed-resolution
  unharvested-buffers
  busy
  random-times
  (check-p*-mods t)
  
  buffer-indices
  buffer-lookup
  buffer-lookup-size
  slot-lookup
  
  conflict-tree
  last-conflict-tree
  delay-tree
  use-tree
  
  req-spec
  
  current-p
  ppm
  md
  ppm-hook
  
  used-search-buffers
  search-buffer-table
  search-matches-table
  temp-search
  last-cr-time
  
  (action-parse-table (make-hash-table :test #'equal))
  (condition-parse-table (make-hash-table :test #'equal)))

;;; This is not intended as a public structure - there will be accessors
;;; defined for getting at the pieces of this (eventually).

(defstruct (production (:predicate production?))
  text name documentation 
  variables bindings conditions actions
  lhs rhs lhs-buffers rhs-buffers
  conflict-code
  break
  disabled
  buffer-indices
  standard-rep
  dynamic
  conflict-val
  (parameter-values (make-hash-table :size 23))
  constants binds others selection-code implicit
    
  failure-condition
  
  partial-matched-slots
  
  searches search-binds search-others
)


(defstruct act-r-production-parameter
  "The internal structure of a production parameter"
  name default-value default-function accessor)


(defvar *production-parameters-list* nil)


;;; Instead of pre-specifying the paramters for a production
;;; move it to a system like chunks that allows for extending
;;; things "on the fly" so that alternative utility equations
;;; can be tried out without having to muck around with the
;;; base definitions.  All changes can then be confined to the
;;; utility-and-rewuard file.


(defmacro extend-productions (parameter-name &key (default-value nil)
                                        (default-function nil))
  "Add new parameters to all productions"
  (let ((accessor-name (intern (concatenate 'string "PRODUCTION-" (string-upcase parameter-name))))
        (setf-name (intern (concatenate 'string "PRODUCTION-" (string-upcase parameter-name) "-SETF"))))
    
    (if (find parameter-name *production-parameters-list* :key #'act-r-production-parameter-name)
        (progn
          (print-warning "Parameter ~s already defined for productions" parameter-name)
          :duplicate-parameter)
      `(eval-when (:compile-toplevel :load-toplevel :execute) 
         (when (or (fboundp ',setf-name) (fboundp ',accessor-name))
           (print-warning "The following ~:[~;2 ~]warning~:[~;s~] can be ignored for the main ACT-R modules provided, but may be a serious problem if seen otherwise." 
                          (and (fboundp ',setf-name) (fboundp ',accessor-name))
                          (and (fboundp ',setf-name) (fboundp ',accessor-name))))
         (when (fboundp ',accessor-name)
           (print-warning "Function ~s already exists and is being redefined." ',accessor-name))
         (when (fboundp ',setf-name)
           (print-warning "Function ~s already exists and is being redefined." ',setf-name))
         
         (when (find ',parameter-name *production-parameters-list* :key #'act-r-production-parameter-name)
           (setf *production-parameters-list* (remove ',parameter-name *production-parameters-list* :key #'act-r-production-parameter-name)))
         
         (push (make-act-r-production-parameter :name ',parameter-name
                                                :default-value ',default-value
                                                :default-function ',default-function
                                                :accessor ',accessor-name)
               *production-parameters-list*)
         
         (defun ,accessor-name (production-name)
           (let ((production (get-production production-name)))
             (if (production? production) 
               (multiple-value-bind (value exists)
                   (gethash ',parameter-name (production-parameter-values production))
                 (if exists
                     value
                   (setf (gethash ',parameter-name (production-parameter-values production)) 
                     (production-parameter-default 
                      (find ',parameter-name *production-parameters-list* 
                            :key #'act-r-production-parameter-name)
                      production))))
             (print-warning "~S called with invalid production name." ',accessor-name))))
         
         (defun ,setf-name (production-name new-value)
           (let ((production (get-production production-name)))
             (if (production? production)
                 (setf (gethash ',parameter-name (production-parameter-values production)) new-value)
               (print-warning "Setf of ~S called with invalid production." ',accessor-name))))
         
         (defsetf ,accessor-name ,setf-name)
         ',accessor-name))))

(defun production-parameter-default (param production)
  "Return a default value for a parameter in a production"
  (if (act-r-production-parameter-default-function param)
      (funcall (act-r-production-parameter-default-function param) production)
    (act-r-production-parameter-default-value param)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper functions.

(defun productions-list (prod)
  (procedural-productions prod))

(defun get-production (production-name)
  (let ((procedural (get-module procedural)))
    (when procedural
      (get-production-internal production-name procedural))))

(defun get-production-internal (production-name procedural)
  (gethash production-name (procedural-p-table procedural)))

(defun add-production (production procedural)
  (push-last production (procedural-productions procedural))
  (setf (gethash (production-name production) (procedural-p-table procedural)) production)
  
  (when (and (procedural-use-tree procedural) (not (procedural-delay-tree procedural)))
    (add-production-to-tree production procedural)))

(defun remove-production (production procedural)
  (setf (procedural-productions procedural)
    (remove production (procedural-productions procedural)))
  (remhash (production-name production) (procedural-p-table procedural))
  
  (when (and (procedural-use-tree procedural) (not (procedural-delay-tree procedural)))
    (remove-production-from-tree production procedural)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A macro for use in production compilation with the new chunk-extending p*'s

(defmacro with-unchecked-p* (&body body)
  (let ((p (gensym)))
    `(let ((,p (get-module-fct 'procedural)))
       (setf (procedural-check-p*-mods ,p) nil)  
       (unwind-protect (progn ,@body)
         (setf (procedural-check-p*-mods ,p) t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun procedural-params (prod param)
  (cond ((consp param)
         
         ;; Changing procedural parameters reschedules conflict resolution
         ;; if it's waiting to happen
         
         (un-delay-conflict-resolution)
         
         (case (car param)
           (:use-tree (setf (procedural-use-tree prod) (cdr param)))
           
           (:er (setf (procedural-er prod) (cdr param)))
           
           (:v (setf (procedural-v prod) (cdr param)))
           (:md (setf (procedural-md prod) (cdr param)))
           (:ppm (if (cdr param)
                     (unless (find 'ppm-offset (no-output (car (sgp :utility-offsets))))
                       (sgp :utility-offsets ppm-offset))
                   (let ((previous (car (no-output (sgp :utility-offsets)))))
                     (sgp :utility-offsets nil)
                     (dolist (x previous)
                       (unless (eq x 'ppm-offset)
                         (sgp-fct (list :utility-offsets x))))))
                 (setf (procedural-ppm prod) (cdr param)))
                 
           
           (:ppm-hook (setf (procedural-ppm-hook prod) (cdr param)))
           (:dat (setf (procedural-dat prod) (cdr param)))
           
           (:crt (setf (procedural-crt prod) (cdr param)))
           (:cst (setf (procedural-cst prod) (cdr param)))
           
           (:lhst (setf (procedural-lhst prod) (cdr param)))
           (:rhst (setf (procedural-rhst prod) (cdr param)))
           
           (:vpft (setf (procedural-random-times prod) (cdr param)))
           
           (:do-not-harvest
            (if (cdr param)
              (if (find (cdr param) (procedural-unharvested-buffers prod))
                (print-warning 
                 "Setting parameter ~s failed because ~s already on the list."
                 :do-not-harvest  (cdr param))
                (push (cdr param) (procedural-unharvested-buffers prod)))
              ;; setting to nil clears all automatically
              (setf (procedural-unharvested-buffers prod) nil)))
           
           (:cycle-hook
            (if (cdr param)
                (if (member (cdr param) (procedural-cycle-hook prod))
                    (print-warning 
                     "Setting parameter ~s failed because ~s already on the hook."
                     :cycle-hook
                     (cdr param))
                  (push (cdr param) (procedural-cycle-hook prod)))
              (setf (procedural-cycle-hook prod) nil)))
           
           (:conflict-set-hook
            (if (cdr param)
                (if (member (cdr param) (procedural-conflict-set-hook prod))
                    (print-warning 
                     "Setting parameter ~s failed because ~s already on the hook."
                     :conflict-set-hook
                     (cdr param))
                  (push (cdr param) (procedural-conflict-set-hook prod)))
              (setf (procedural-conflict-set-hook prod) nil)))))
        (t 
         (case param
           (:use-tree (procedural-use-tree prod))
           (:ppm (procedural-ppm prod))
           (:ppm-hook (procedural-ppm-hook prod))
           
           (:dat (procedural-dat prod))
           (:crt (procedural-crt prod))
           (:cst (procedural-cst prod))
           
           (:lhst (procedural-lhst prod))
           (:rhst (procedural-rhst prod))
           
           (:vpft (procedural-random-times prod))
           
           (:do-not-harvest
            (procedural-unharvested-buffers prod))
           
           (:cycle-hook (procedural-cycle-hook prod))
           (:conflict-set-hook (procedural-conflict-set-hook prod))))))


(defun reset-procedural-module (prod)
  (setf (procedural-productions prod) nil)
  (setf (procedural-p-table prod) (make-hash-table :size 31 :test 'eq))
  (setf (procedural-bindings prod) nil)
  (setf (procedural-delayed-resolution prod) nil)
  
  (setf (procedural-buffer-indices prod) nil)  
  (setf (procedural-buffer-lookup-size prod) 0) 
  
  (setf (procedural-busy prod) nil)
  
  (setf (procedural-req-spec prod) (define-chunk-spec isa chunk))
  
  (setf (procedural-delay-tree prod) t)
  
  (unless (procedural-conflict-tree prod)
    (setf (procedural-conflict-tree prod) (make-root-node)))
  
  (setf (procedural-search-buffer-table prod) (make-hash-table))
  (setf (procedural-search-matches-table prod) (make-hash-table))
  
  (setf (procedural-last-cr-time prod) nil)
  
  (schedule-event-relative 0 'conflict-resolution :module 'procedural 
                           :priority :min
                           :destination 'procedural  
                           :output 'medium))



(defun finalize-procedural-reset (prod)
  
  (setf (procedural-delay-tree prod) nil)

  (when (procedural-use-tree prod)
    
    (when (or (procedural-crt prod)
              (procedural-ppm prod))
      (model-warning "Conflict resolution cannot use the decision tree when :crt or :ppm is enabled."))
   
    (cond ((null (procedural-last-conflict-tree prod))
           (build-conflict-tree prod)
           (setf (procedural-last-conflict-tree prod) (copy-conflict-tree (procedural-conflict-tree prod) nil)))
          ((equal (mapcar #'production-name (productions-list prod)) (conflict-node-valid (procedural-conflict-tree prod)))
           ;; assume everything still valid
           )
          ((equal (mapcar #'production-name (productions-list prod)) (conflict-node-valid (procedural-last-conflict-tree prod)))
           ;; assume saved tree is valid
           (setf (procedural-conflict-tree prod) (copy-conflict-tree (procedural-last-conflict-tree prod) nil)))
          (t ;; same as the first case
           (build-conflict-tree prod)
           (setf (procedural-last-conflict-tree prod) (copy-conflict-tree (procedural-conflict-tree prod) nil))))))


(defun cr-buffer-read (prod buffer index)
  (let ((val (aref (procedural-buffer-lookup prod) index)))
    (if (eq val :untested)
        (setf (aref (procedural-buffer-lookup prod) index) (buffer-read buffer))
      val)))

(defun cr-buffer-slot-read (prod buffer bi si)
  (let ((val (aref (procedural-slot-lookup prod) bi si)))
    (if (eq val :untested)
      (setf (aref (procedural-slot-lookup prod) bi si)
        (slot-value-from-index (cr-buffer-read prod buffer bi) si))
      val)))

(defun slot-value-from-index (chunk index)
  (if (chunk-p-fct chunk)
      (let ((slot-name (nth index (chunk-type-slot-names-fct (chunk-chunk-type-fct chunk)))))
        (if slot-name
            (fast-chunk-slot-value-fct chunk slot-name)
          :unbound))
    :unbound))

(defun get-slot-index (chunk-type slot)
  (position slot (chunk-type-slot-names-fct chunk-type)))
      

(defstruct cr-condition
  type
  buffer
  bi
  slot
  si
  value
  test
  result)

;;; Function to test if two conditions are equivalent 
;;; only valid for constant tests i.e. those that could
;;; be in a conflict-resolution tree.
;;;
;;; Changed the value tests to eql instead of eq because
;;; the value could be a number.

(defun cr-condition-equal (a b)
  (and (eq (cr-condition-type a) (cr-condition-type b))
       (eq (cr-condition-buffer a) (cr-condition-buffer b))
       (case (cr-condition-type a)
         (isa (eq (cr-condition-value a) (cr-condition-value b)))
         (slot (= (cr-condition-si a) (cr-condition-si b)))
         (query (and (eq (cr-condition-slot a) (cr-condition-slot b))
                     (eql (cr-condition-value a) (cr-condition-value b))))
         (test-slot (and (eq (cr-condition-test a) (cr-condition-test b))
                         (= (cr-condition-si a) (cr-condition-si b))
                         (eql (cr-condition-value a) (cr-condition-value b))))
         (t nil))))

(defun test-constant-condition (prod test &optional (used nil))
  (case (cr-condition-type test)
    (isa 
     (aif (cr-buffer-read prod (cr-condition-buffer test) (cr-condition-bi test)) 
          (chunk-type-subtype-p-fct (chunk-chunk-type-fct it) (cr-condition-value test))
          nil))
    (slot 
     
     (let ((real (cr-buffer-slot-read prod (cr-condition-buffer test) (cr-condition-bi test) (cr-condition-si test))))
       (if (chunk-slot-equal (cr-condition-value test) real)
         t
         (when (procedural-ppm prod)
           ;;; try a partial match and save the result if it's valid
           (let ((sim (similarity-fct (cr-condition-value test) real)))
             (when (and (numberp sim) (> sim (procedural-md prod)))
               (push (list (cr-condition-buffer test) (cr-condition-slot test) (cr-condition-value test) real sim)
                     (production-partial-matched-slots (procedural-current-p prod)))))))))
    (query 
     (eq (cr-condition-result test) (query-buffer (cr-condition-buffer test) (list (cons (cr-condition-slot test) (cr-condition-value test))))))
    (test-slot 
     (eq (cr-condition-result test) 
         (funcall (cr-condition-test test) (cr-buffer-slot-read prod (cr-condition-buffer test) (cr-condition-bi test) (cr-condition-si test)) (cr-condition-value test))))))


(defun test-search-constants (prod test production)
  
  ;; don't use the cache table for slot values since they'd need to be continuously cleared.
  ;; Instead just use the accessors for the chunk from the table.
  
  (case (cr-condition-type test)
    (isa 
     (aif (cr-buffer-read prod (cr-condition-buffer test) (cr-condition-bi test)) 
          (chunk-type-subtype-p-fct (chunk-chunk-type-fct it) (cr-condition-value test))
          nil))
    
    (slot 
     (let ((real (slot-value-from-index (cr-buffer-read prod (cr-condition-buffer test) (cr-condition-bi test)) (cr-condition-si test))))
       (if (chunk-slot-equal (cr-condition-value test) real)
         t
         (when (procedural-ppm prod)
           ;;; try a partial match and save the result if it's valid
           (let ((sim (similarity-fct (cr-condition-value test) real)))
             (when (and (numberp sim) (> sim (procedural-md prod)))
               (push (list (cr-condition-buffer test) (cr-condition-slot test) (cr-condition-value test) real sim)
                     (production-partial-matched-slots (procedural-current-p prod)))))))))
    
    (test-slot 
     (if (eq (cr-condition-result test) (funcall (cr-condition-test test) (slot-value-from-index (cr-buffer-read prod (cr-condition-buffer test) (cr-condition-bi test)) (cr-condition-si test)) (replace-variables (cr-condition-value test) (production-bindings production))))
         t
       (when (and (procedural-ppm prod) (eq (cr-condition-test test) 'safe-chunk-slot-equal) (cr-condition-result test))
         ;;; try a partial match and save the result if it's valid
         (let* ((desired (replace-variables (cr-condition-value test) (production-bindings production)))
                (real (slot-value-from-index (cr-buffer-read prod (cr-condition-buffer test) (cr-condition-bi test)) (cr-condition-si test)))
                (sim (similarity-fct desired real)))
           (when (and (numberp sim) (> sim (procedural-md prod)))
             (push (list (cr-condition-buffer test) (cr-condition-slot test) desired real sim)
                   (production-partial-matched-slots (procedural-current-p prod))))))))
    
    (test-var-slot
     (let* ((ct (chunk-chunk-type-fct (cr-buffer-read prod (cr-condition-buffer test) (cr-condition-bi test))))
           (index (get-slot-index ct (replace-variables (cr-condition-slot test) (production-bindings production)))))
       
       (if (numberp index)
           (if (eq (cr-condition-result test) (funcall (cr-condition-test test) (slot-value-from-index (cr-buffer-read prod (cr-condition-buffer test) (cr-condition-bi test)) index) (replace-variables (cr-condition-value test) (production-bindings production))))
               t
             (when (and (procedural-ppm prod) (eq (cr-condition-test test) 'safe-chunk-slot-equal) (cr-condition-result test))
               ;;; try a partial match and save the result if it's valid
               (let* ((desired (replace-variables (cr-condition-value test) (production-bindings production)))
                      (real (slot-value-from-index (cr-buffer-read prod (cr-condition-buffer test) (cr-condition-bi test)) index))
                      (sim (similarity-fct desired real)))
                 (when (and (numberp sim) (> sim (procedural-md prod)))
                   (push (list (cr-condition-buffer test) (replace-variables (cr-condition-slot test) (production-bindings production)) desired real sim)
                         (production-partial-matched-slots (procedural-current-p prod)))))))
         nil))))
  )
  

(defun test-and-perfrom-bindings (procedural bind production)
  (case (cr-condition-type bind)
    (bind-slot
     (bind-variable (cr-condition-value bind) (cr-buffer-slot-read procedural (cr-condition-buffer bind) (cr-condition-bi bind) (cr-condition-si bind)) production))
    (bind-buffer
     (bind-variable (cr-condition-value bind) (cr-buffer-read procedural (cr-condition-buffer bind) (cr-condition-bi bind)) production))
    (bind-var-slot
     (let* ((ct (chunk-chunk-type-fct (cr-buffer-read procedural (cr-condition-buffer bind) (cr-condition-bi bind))))
            (index (get-slot-index ct (replace-variables (cr-condition-slot bind) (production-bindings production)))))
       (if (numberp index)
           (bind-variable (cr-condition-value bind) (cr-buffer-slot-read procedural (cr-condition-buffer bind) (cr-condition-bi bind) index) production)
         nil)))
    (bind
     (let ((result (eval (replace-variables-for-eval (cr-condition-result bind) (production-bindings production)))))
       (if result
           (bind-variable (cr-condition-value bind) result production)
         nil)))
    
    (mv-bind
     (let ((all-vars (cr-condition-value bind))
           (results (multiple-value-list (eval (replace-variables-for-eval (cr-condition-result bind) (production-bindings production))))))
       (cond ((not (= (length results) (length all-vars)))
              nil)
             ((find nil results)
              nil)
             (t
              (do ((vars all-vars (cdr vars))
                   (vals results (cdr vals)))
                  ((null vars) t)
                (bind-variable (car vars) (car vals) production))))))))


(defun test-and-perfrom-bindings-search (procedural bind production)
  (case (cr-condition-type bind)
    (bind-slot
     (bind-variable (cr-condition-value bind) 
                    ;; can't use the lookup because the slots may be invalid
                    ;;(cr-buffer-slot-read procedural (cr-condition-buffer bind) (cr-condition-bi bind) (cr-condition-si bind)) 
                    (slot-value-from-index 
                           (cr-buffer-read procedural (cr-condition-buffer bind) (cr-condition-bi bind)) (cr-condition-si bind))
                    production))
    (bind-buffer
     (bind-variable (cr-condition-value bind) (cr-buffer-read procedural (cr-condition-buffer bind) (cr-condition-bi bind)) production))
    (bind-var-slot
     (let* ((ct (chunk-chunk-type-fct (cr-buffer-read procedural (cr-condition-buffer bind) (cr-condition-bi bind))))
            (index (get-slot-index ct (replace-variables (cr-condition-slot bind) (production-bindings production)))))
       (if (numberp index)
           (bind-variable (cr-condition-value bind) 
                           ;; Can't use the cached lookup for a search buffer since it may not be the "Right" chunk
                           ;; (cr-buffer-slot-read procedural (cr-condition-buffer bind) (cr-condition-bi bind) index) 
                          (slot-value-from-index 
                           (cr-buffer-read procedural (cr-condition-buffer bind) (cr-condition-bi bind)) index)
                          production)
         nil)))
    (bind
     (let ((result (eval (replace-variables-for-eval (cr-condition-result bind) (production-bindings production)))))
       (if result
           (bind-variable (cr-condition-value bind) result production)
         nil)))
    
    (mv-bind
     (let ((all-vars (cr-condition-value bind))
           (results (multiple-value-list (eval (replace-variables-for-eval (cr-condition-result bind) (production-bindings production))))))
       (cond ((not (= (length results) (length all-vars)))
              nil)
             ((find nil results)
              nil)
             (t
              (do ((vars all-vars (cdr vars))
                   (vals results (cdr vals)))
                  ((null vars) t)
                (bind-variable (car vars) (car vals) production))))))))

(defun test-other-condition (prod test production)
  (case (cr-condition-type test)
    (query 
     (eq (cr-condition-result test) (query-buffer (cr-condition-buffer test) (list (cons (cr-condition-slot test) (replace-variables (cr-condition-value test) (production-bindings production)))))))
    (test-slot 
     (if (eq (cr-condition-result test) (funcall (cr-condition-test test) (cr-buffer-slot-read prod (cr-condition-buffer test) (cr-condition-bi test) (cr-condition-si test)) (replace-variables (cr-condition-value test) (production-bindings production))))
         t
       (when (and (procedural-ppm prod) (eq (cr-condition-test test) 'safe-chunk-slot-equal) (cr-condition-result test))
         ;;; try a partial match and save the result if it's valid
         (let* ((desired (replace-variables (cr-condition-value test) (production-bindings production)))
                (real (cr-buffer-slot-read prod (cr-condition-buffer test) (cr-condition-bi test) (cr-condition-si test)))
                (sim (similarity-fct desired real)))
           (when (and (numberp sim) (> sim (procedural-md prod)))
             (push (list (cr-condition-buffer test) (cr-condition-slot test) desired real sim)
                   (production-partial-matched-slots (procedural-current-p prod))))))))
    (eval
     (eval (replace-variables-for-eval (cr-condition-value test) (production-bindings production))))
    (test-var-slot
     (let* ((ct (chunk-chunk-type-fct (cr-buffer-read prod (cr-condition-buffer test) (cr-condition-bi test))))
           (index (get-slot-index ct (replace-variables (cr-condition-slot test) (production-bindings production)))))
          
       (if (numberp index)
           (if (eq (cr-condition-result test) (funcall (cr-condition-test test) (cr-buffer-slot-read prod (cr-condition-buffer test) (cr-condition-bi test) index) (replace-variables (cr-condition-value test) (production-bindings production))))
               t
             (when (and (procedural-ppm prod) (eq (cr-condition-test test) 'safe-chunk-slot-equal) (cr-condition-result test))
               ;;; try a partial match and save the result if it's valid
               (let* ((desired (replace-variables (cr-condition-value test) (production-bindings production)))
                      (real (cr-buffer-slot-read prod (cr-condition-buffer test) (cr-condition-bi test) index))
                      (sim (similarity-fct desired real)))
                 (when (and (numberp sim) (> sim (procedural-md prod)))
                   (push (list (cr-condition-buffer test) (replace-variables (cr-condition-slot test) (production-bindings production)) desired real sim)
                         (production-partial-matched-slots (procedural-current-p prod)))))))
         nil)))
    ))


(defun test-search-buffers (prod test production)
  (let* ((buffer (cr-condition-buffer test))
         (chunk-list (multiple-value-bind (val exists)
                         (gethash buffer (procedural-search-buffer-table prod)) 
                       (if exists
                           val
                         (multiple-value-bind (e new-val)
                             (m-buffer-search buffer)
                           (setf (gethash buffer (procedural-search-buffer-table prod)) 
                             (if e 
                                 (let ((valid (get-m-buffer-chunks buffer)))
                                   (remove-if-not (lambda (x) (member x valid)) new-val))
                               (get-m-buffer-chunks buffer))))))))
         
         
    
    (dolist (c chunk-list)
      (overwrite-buffer-chunk buffer c)
      
      ;; Only worry about the buffer lookup table
      ;; test the slots from the buffer chunk directly
      
      (setf (aref (procedural-buffer-lookup prod) (cr-condition-bi test)) :untested)
      
      #|
      Alternative fix for the binding of search buffer variables.

      Current fix is to use the test-and-perfrom-bindings-search command
      when binding search buffers because it uses the assumption
      stated above about testing slots directly.
      
      With this loop in place the original test-and-perform-bindings can
      be used instead.  I don't know which is better right now, but
      my gut feeling is that there're fewer bindings than slots which would have
      to be cleared from the cache so it's probably more efficient to 
      just take small hit for binding than to always clear the whole cache row.
 
      ;; clear the cached slots
      
      (dotimes (i (largest-chunk-type-size))
        
        (setf (aref (procedural-slot-lookup prod) (cr-condition-bi test) i) :untested))
      |#
      
      
      
      (let ((crt (procedural-crt prod)))
        (setf (procedural-crt prod) nil)
        (let ((result (conflict-tests prod (cr-condition-value test) production 'test-search-constants)))
          (setf (procedural-crt prod) crt)
          (when result
            (push (cons buffer c) (procedural-temp-search prod))
            (return-from test-search-buffers t)))))
    nil))
                          


(defun failure-reason-string (condition procedural production)
  
  (if (production-disabled production)
      "The production is disabled."
    (case (cr-condition-type condition)
      (isa (if (buffer-read (cr-condition-buffer condition))
               (format nil "The chunk in the ~S buffer is not of chunk-type ~S." (cr-condition-buffer condition) (cr-condition-value condition))
             (format nil "The ~s buffer is empty." (cr-condition-buffer condition))))
      (search (format nil "The searched multi-buffer ~s did not have a matching chunk." (cr-condition-buffer condition)))
      (slot (format nil "The ~s slot of the chunk in the ~s buffer does not have the value ~s." (cr-condition-slot condition) (cr-condition-buffer condition) (cr-condition-value condition)))
      (query (format nil "The ~s ~s query of the ~s buffer failed." (cr-condition-slot condition) (cr-condition-value condition) (cr-condition-buffer condition)))
      (test-slot (if (and (eq (cr-condition-test condition) 'safe-chunk-slot-equal) (null (cr-condition-value condition)) (null (cr-condition-result condition)))
                     (format nil "The ~s slot of the chunk in the ~s buffer is empty." (cr-condition-slot condition) (cr-condition-buffer condition))
                   (format nil "The value in the ~s slot of the chunk in the ~s buffer does not satisfy the constraints." (cr-condition-slot condition) (cr-condition-buffer condition))))
      (eval (format nil "The evaluation of the expression ~s returned nil." (cr-condition-value condition)))
      (test-var-slot (let* ((ct (chunk-chunk-type-fct (cr-buffer-read procedural (cr-condition-buffer condition) (cr-condition-bi condition))))
                            (index (get-slot-index ct (replace-variables (cr-condition-slot condition) (production-bindings production)))))
                       (if (numberp index)
                           (format nil "The value in the ~s slot (the value of the ~s variable) of the chunk of the ~s buffer does not satisfy the constraints."
                             (replace-variables (cr-condition-slot condition) (production-bindings production)) (cr-condition-slot condition) (cr-condition-buffer condition))
                         (format nil "The value of the ~s variable does not name a valid slot in the chunk in the ~s buffer." (cr-condition-slot condition) (cr-condition-buffer condition)))))
      (bind (format nil "The evaluation of the expression ~s returned nil." (cr-condition-result condition)))
      (mv-bind (format nil "The evaluation of the expression ~s either returned a nil value or too few values to bind to all of the variables." (cr-condition-result condition)))
      
      (bind-buffer (format nil "The ~s buffer is empty." (cr-condition-buffer condition))) ;; This shouldn't happen anymore since the isa will fail first
      (bind-slot (format nil "The variable ~s cannont be bound to nil in the ~s slot of the ~s buffer." (cr-condition-value condition) (cr-condition-slot condition) (cr-condition-buffer condition)))
      (bind-var-slot (let* ((ct (chunk-chunk-type-fct (cr-buffer-read procedural (cr-condition-buffer condition) (cr-condition-bi condition))))
                            (index (get-slot-index ct (replace-variables (cr-condition-slot condition) (production-bindings production)))))
                       (if (numberp index)
                           (format nil "The value in the ~s slot (the value of the ~s variable) of the chunk of the ~s buffer is nil and cannot be bound to ~s."
                             (replace-variables (cr-condition-slot condition) (production-bindings production)) (cr-condition-slot condition) (cr-condition-buffer condition) (cr-condition-value condition))
                         (format nil "The value of the ~s variable does not name a valid slot in the chunk in the ~s buffer." (cr-condition-slot condition) (cr-condition-buffer condition))))))))


(defun bind-variable (var value production)
  (aif (assoc var (production-bindings production))
       (setf (cdr it) value)
       (progn
         (push (cons var value) (production-bindings production))
         value)))


(defun conflict-tests (procedural test-list production tester &key (report t))
  (if (null test-list) 
      t
    (let ((outcome (do* ((tests test-list (cdr tests))
                         (result (funcall tester procedural (car tests) production)
                                 (funcall tester procedural (car tests) production)))
                        
                        ((or (null result) (null (cdr tests))) (cons result (car tests))))))
      
      (if (null (car outcome))
          (progn
            (setf (production-failure-condition production) (cdr outcome))
            (when (and (procedural-crt procedural) report); report failures..
              (model-output "Fails because: ")
              (model-output (failure-reason-string (cdr outcome) procedural production)))
            nil)
        t))))
        

(defun conflict-resolution (procedural)
  
  (setf (procedural-delayed-resolution procedural) nil)
  
  (setf (procedural-buffer-lookup procedural) (make-array (list (procedural-buffer-lookup-size procedural)) :initial-element :untested))
  (setf (procedural-slot-lookup procedural) (make-array (list (procedural-buffer-lookup-size procedural) (largest-chunk-type-size)) :initial-element :untested))
  
  (let* ((conflict-set nil)
         (hook-set nil)
         (best nil)
         (best-ut (minimum-utility))
         (mu best-ut)
         (offsets-table (make-hash-table))
         (test-set (if (or (null (procedural-use-tree procedural))
                           (procedural-crt procedural)
                           (procedural-ppm procedural))
                       (procedural-productions procedural)
                     (mapcar (lambda (x) (get-production-internal x procedural)) (get-valid-productions procedural))))
         (saved-search-chunks (make-hash-table)))
    
    (setf (procedural-last-cr-time procedural) (mp-time))
    (clrhash (procedural-search-matches-table procedural))
    
    (dolist (b (procedural-used-search-buffers procedural))
      (aif (buffer-read b)
           (setf (gethash b saved-search-chunks) it)
           (setf (gethash b saved-search-chunks) :clear)))
    
    (clrhash (procedural-search-buffer-table procedural))
    
    (dolist (production test-set)
      
      (setf (production-bindings production) nil)
      (setf (production-failure-condition production) nil)
      
      (setf (procedural-current-p procedural) production)
      (setf (production-partial-matched-slots production) nil)
      
      
      (setf (procedural-temp-search procedural) nil)
      
      
      
      (unless (production-disabled production)
        (when (procedural-crt procedural)
          (model-output "Trying production: ~s" (production-name production)))
        
        (when (and (conflict-tests procedural (production-constants production) production 'test-constant-condition)
                   (conflict-tests procedural (production-binds production) production 'test-and-perfrom-bindings)
                   (conflict-tests procedural (production-others production) production 'test-other-condition)
                   (conflict-tests procedural (production-searches production) production 'test-search-buffers)
                   (conflict-tests procedural (production-search-binds production) production 'test-and-perfrom-bindings-search)
                   (conflict-tests procedural (production-search-others production) production 'test-other-condition)
                   ) 
          
          (dolist (s (procedural-temp-search procedural))
            (pushnew (cdr s)
                     (gethash (car s) (procedural-search-matches-table procedural))))
          
          (push-last production conflict-set)
          )))
    
    
    ;; get and save any potential chunk offsets 
         
    (maphash (lambda (buffer chunks)
               (multiple-value-bind (exists offsets)
                   (m-buffer-offset buffer chunks)
                 (when (and exists offsets (every 'numberp offsets))
                   (setf (gethash buffer offsets-table)
                     (mapcar 'cons chunks offsets)))))
             (procedural-search-matches-table procedural))
         
    
    (dolist (production conflict-set)
      
      (let ((u (compute-utility (production-name production) t)))
        
        (when (procedural-crt procedural)
          (model-output "Production ~s matches" (production-name production)))
        
        
        (dolist (s (production-searches production))
          (let* ((buf (cr-condition-buffer s))
                 (buf-var (cr-condition-test s))
                 (bound-chunk (cdr (assoc buf-var (production-bindings production))))
                 (offset (cdr (assoc bound-chunk (gethash buf offsets-table)))))
            
            (when (numberp offset)
              (incf u offset))))        
        
        
        (setf (production-conflict-val production) u)
        
        (when (and (procedural-crt procedural) mu (< u mu))
          (model-output "Fails because:~%Utility ~s is below the threshold ~s" u mu))
        
        (cond ((or (null best-ut) (> u best-ut))
               (setf best-ut u)
               (setf best (list production)))
              ((= u best-ut)
               (if (procedural-er procedural)
                   (push-last production best)
                 (setf best (list production)))))))
         
         
    
    
    (when (and (listp best) best (procedural-er procedural))
      (setf best (permute-list best)))
    
    (when (procedural-conflict-set-hook procedural)
      (let ((val nil)
            (old-val nil)
            
            (cs-names (mapcar #'production-name
                        (sort (copy-list conflict-set)
                              #'(lambda (x y) 
                                  (sort-productions x y best))))))
        
        (dolist (hook (procedural-conflict-set-hook procedural))
          (when val
            (setf old-val val))
          (setf val (funcall hook cs-names))
          (unless (or (null val) (stringp val) (member val cs-names))
            (print-warning "Only productions in the conflict set, a string, or nil are valid return values of a conflict-set-hook function.")
            (setf val nil))
          (when (and val old-val)
            (print-warning "Multiple functions on the conflict-set-hook returned a value")))
        (setf hook-set (or val old-val))))
    
    (when (and (procedural-cst procedural) (procedural-v procedural))
      (dolist (x conflict-set)
        (print-instantiation x)
        (spp-fct (list (production-name x) :utility :u))
        (unless (= (production-conflict-val x) (no-output (caar (spp-fct (list (production-name x) :utility)))))
          (command-output "{buffer search adjusted u value is ~,3f}" (production-conflict-val x)))))
    
    ;; restore the chunks to the search buffers then any "new" search results will overwrite
    ;; those in the conflict-code
    
    (dolist (b (procedural-used-search-buffers procedural))
      (let ((val (gethash b saved-search-chunks)))
        (when val
          (if (eq val :clear)
              (erase-buffer b)
            (overwrite-buffer-chunk b val)))))
    
    (cond ((null hook-set) ;; default mechanims are used
           (let ((best-production (car best))) ; not (car conflict-set) because that's only sorted for the hook
             (if best-production 
                 (progn
                   (schedule-event-relative 0 'production-selected 
                                            :module 'procedural
                                            :destination 'procedural
                                            :priority :max
                                            :params (list best-production)
                                            :details 
                                            (concatenate 'string
                                              (symbol-name 'production-selected)
                                              " "
                                              (symbol-name (production-name best-production))))
                   
                   (awhen (production-conflict-code best-production)
                          (dolist (code it)
                            (funcall code)))
                   
                   
                   (when (production-break best-production)
                     
                     (schedule-event-relative 0 'print-instantiation
                                              :module 'procedural
                                              :output nil
                                              :priority :max
                                              :params (list best-production))
                     
                     (schedule-break-relative 0 :priority :max 
                                              :details 
                                              (concatenate 'string
                                                (symbol-name 'production)
                                                " "
                                                (symbol-name (production-name best-production))))))
               
               (setf (procedural-delayed-resolution procedural) 
                 (schedule-event-after-change 'conflict-resolution
                                              :module 'procedural
                                              :destination 'procedural
                                              :output 'medium
                                              :dynamic t)))))
          
          ((symbolp hook-set) ;; an over-ride production specified
           
           (schedule-event-relative 0 'production-selected :module 'procedural
                                    :destination 'procedural :priority :max
                                    :params (list (get-production-internal hook-set procedural))
                                    :details 
                                    (concatenate 'string
                                      (symbol-name 'production-selected)
                                      " "
                                      (symbol-name hook-set)))
           
           (awhen (production-conflict-code (get-production-internal hook-set procedural))
                  (dolist (code it)
                    (funcall code)))
           
           (when (production-break (get-production-internal hook-set procedural))
             
             (schedule-event-relative 0 'print-instantiation
                                      :module 'procedural
                                      :output nil
                                      :priority :max
                                      :params (list (get-production-internal hook-set procedural)))
             
             (schedule-break-relative 0 :priority :max 
                                      :details (concatenate 'string
                                                 (symbol-name 'production)
                                                 " "
                                                 (symbol-name hook-set)))))
          
          ((stringp hook-set) ;; an abort selection reason provided
           (model-warning "conflict-set-hook function canceled selection because : ~a" hook-set)
           (schedule-event-relative (procedural-dat procedural) 'conflict-resolution
                                    :module 'procedural
                                    :destination 'procedural
                                    :output 'medium))
          (t ;; shouldn't happen but this is a saftey case
           (print-warning "Illegal conflict resolution situation occured. Contact Dan to let him know.")))))



(defun ppm-offset (production) ;; it's the name called through the utility-offsets
  (let ((prod (get-module procedural))
        (p (get-production production)))
    (when (and prod p (numberp (procedural-ppm prod)) (production-partial-matched-slots p))
      (let ((override (awhen (procedural-ppm-hook prod)
                             (funcall it production (production-partial-matched-slots p)))))
        (if (numberp override)
            override
          (* (procedural-ppm prod) (reduce #'+ (production-partial-matched-slots p) :key #'fifth)))))))


(defun un-delay-conflict-resolution ()
  
  (let ((p (get-module procedural)))
    
    (when (and p (procedural-delayed-resolution p))
      (let ((deleted (delete-event (procedural-delayed-resolution p))))
        (setf (procedural-delayed-resolution p) nil)
        (when deleted
          (schedule-event-relative 0 'conflict-resolution :module 'procedural
                                   :destination 'procedural
                                   :output 'medium)))))
  nil)


(defmacro pprint-instantiation (production-name)
  `(pprint-instantiation-fct ',production-name))

(defun pprint-instantiation-fct (production-name)
  (let ((p (get-production production-name)))
    (if p
        (print-instantiation p)
      (model-warning "No production named ~a." p))))
                                 
                                 
                                 
(defun print-instantiation (production)
  
  (let ((prod (get-module procedural)))
    (when prod
      (if (and (procedural-ppm prod)
               (production-partial-matched-slots production))
          (print-partial-matched-production production)
        (print-production-text (replace-variables 
                                (production-text production)
                                (production-bindings production)))))))


(defun print-production (production &optional model-output)
  (when (production-disabled production)
    (if model-output
        (model-output ";;; Production ~s is DISABLED" (production-name production))
      (command-output ";;; Production ~s is DISABLED" (production-name production))))
  (print-production-text (production-text production) model-output))



(defun print-partial-matched-production (p)
  
  (let* ((text (copy-tree (production-text p)))
         (p-name (pop text))
         (str nil)
         (buffer nil)
         (slot nil)
         (value nil)
         (lhs t))
    (if (production-dynamic p)
        (command-output "(P* ~a" p-name)
      (command-output "(P ~a" p-name))
    
    (loop
      (when (null text) (return))
      (cond ((stringp (car text))
             (setf str (format nil "  ~S" (pop text)))
             (command-output str))
            ((equal (car text) '==>)
             (setf lhs nil)
             (setf str (format nil " ~a" (pop text)))
             (command-output str))
            ;; The direct request special case
            ((and
              (> (length (symbol-name (car text))) 1)
              (equal #\> (aref (reverse (symbol-name (car text))) 0))
              (equal #\+ (aref (symbol-name (car text)) 0))
              (or (= (length text) 2)
                  (and (> (length text) 2)
                       (symbolp (third text))
                       (not (eq (second text) 'isa))
                       (> (length (symbol-name (third text))) 1)
                       (or (equal #\> (aref (reverse (symbol-name (third text))) 0))
                           (equal #\! (aref (reverse (symbol-name (third text))) 0))))))
             (setf str (format nil "   ~a ~s" (pop text) (replace-variables 
                                                          (pop text)
                                                          (production-bindings p))))
             (command-output str))
            
            ;; direct assignments are a trick too
            ((and
              (> (length (symbol-name (car text))) 1)
              (equal #\> (aref (reverse (symbol-name (car text))) 0))
              (equal #\= (aref (symbol-name (car text)) 0))
              (> (length text) 1)
              (not (and (> (length (symbol-name (second text))) 1)
                        (equal #\> (aref (reverse (symbol-name (second text))) 0))))
              (or (= (length text) 2)
                  (and (symbolp (third text))
                       (> (length (symbol-name (third text))) 1)
                       (not (eq (second text) 'isa))
                       (or (equal #\> (aref (reverse (symbol-name (third text))) 0))
                           (equal #\! (aref (reverse (symbol-name (third text))) 0))))))
              (setf str (format nil "   ~a ~s" (pop text) (replace-variables 
                                                           (pop text)
                                                           (production-bindings p))))
             (command-output str))
            
            ((and
              (> (length (symbol-name (car text))) 1)
              (equal #\> (aref (reverse (symbol-name (car text))) 0)))
             (let* ((val (pop text))
                    (string (symbol-name val)))
               (setf buffer (if (and lhs (equal #\= (aref string 0)))
                                (intern (subseq string 1 (1- (length string))))
                              nil))
             (setf str (format nil "   ~a" val))
             (command-output str)))
            
            ((equal (car text) '!stop!)
             (setf str (format nil "   ~a" (pop text)))
             (command-output str))
            ((or
              (equal (car text) '!output!)
              (equal (car text) '!eval!)
              (equal (car text) '!safe-eval!))
             (setf buffer nil)
             (setf str (format nil "   ~a ~s" (pop text) (replace-variables 
                                                           (pop text)
                                                           (production-bindings p))))
             (command-output "~a" str))
            ((or
              (equal (car text) '!bind!)
              (equal (car text) '!mv-bind!)
              (equal (car text) '!safe-bind!))
             (setf buffer nil)
             (setf str 
               (format nil "   ~a ~s ~s" (pop text) (replace-variables 
                                                     (pop text)
                                                     (production-bindings p))
                 (replace-variables 
                  (pop text)
                  (production-bindings p))))
             (command-output "~a" str))
            ((member (car text) '(= - < > <= >=))
             (if (and lhs buffer (eq (car text) '=))
                 (setf slot 
                   (replace-variables 
                    (second text)
                    (production-bindings p))
                   
                   value
                   (replace-variables 
                    (third text)
                    (production-bindings p)))
               (setf slot nil value nil))
             (let ((partial (find (list buffer slot value) (production-partial-matched-slots p) :test 'equal :key (lambda (x) (subseq x 0 3)))))
               
               (if partial
                   (setf str
                     (format nil "    ~2a ~s [~s, ~s, ~f]" (pop text) 
                       (replace-variables 
                        (pop text)
                        (production-bindings p))
                       (replace-variables 
                        (pop text)
                        (production-bindings p))
                       (fourth partial) (fifth partial)))
                 
                 (setf str 
                   (format nil "    ~2a ~s ~s" (pop text) 
                     (replace-variables 
                      (pop text)
                      (production-bindings p))
                     (replace-variables 
                      (pop text)
                      (production-bindings p)))))
             (command-output "~a" str)))
            
            
            (t
             (if (and lhs buffer)
                 (setf slot 
                   (replace-variables 
                    (first text)
                    (production-bindings p))
                   
                   value
                   (replace-variables 
                    (second text)
                    (production-bindings p)))
               (setf slot nil value nil))
             (let ((partial (find (list buffer slot value) (production-partial-matched-slots p) :test 'equal :key (lambda (x) (subseq x 0 3)))))
               
               (if partial
                   (setf str
                     (format nil "       ~s [~s, ~s, ~f]"  
                       (replace-variables 
                        (pop text)
                        (production-bindings p))
                       (replace-variables 
                        (pop text)
                        (production-bindings p))
                       (fourth partial) (fifth partial)))
                 
                 (setf str 
                   (format nil "       ~s ~s"  
                     (replace-variables 
                      (pop text)
                      (production-bindings p))
                     (replace-variables 
                      (pop text)
                      (production-bindings p)))))
               (command-output "~a" str)))))
    (command-output ")")))


(defun print-production-text (p-text &optional model-output)
  
  (let* ((text (copy-tree p-text))
         (p-name (pop text))
         (p (get-production p-name))
         (str nil))
    (if (production-dynamic p)
        (if model-output 
            (model-output "(P* ~a" p-name)
          (command-output "(P* ~a" p-name))
      (if model-output
          (model-output "(P ~a" p-name)
        (command-output "(P ~a" p-name)))
    
    (loop
      (when (null text) (return))
      (cond ((stringp (car text))
             (setf str (format nil "  ~S" (pop text)))
             (if model-output
                 (model-output str)
               (command-output str)))
            ((equal (car text) '==>)
             (setf str (format nil " ~a" (pop text)))
             (if model-output
                 (model-output str)
               (command-output str)))
            ;; The direct request special case
            ((and
              (> (length (symbol-name (car text))) 1)
              (equal #\> (aref (reverse (symbol-name (car text))) 0))
              (equal #\+ (aref (symbol-name (car text)) 0))
              (or (= (length text) 2)
                  (and (> (length text) 2)
                       (symbolp (third text))
                       (not (eq (second text) 'isa))
                       (> (length (symbol-name (third text))) 1)
                       (or (equal #\> (aref (reverse (symbol-name (third text))) 0))
                           (equal #\! (aref (reverse (symbol-name (third text))) 0))))))
             (setf str (format nil "   ~a ~s" (pop text) (pop text)))
             (if model-output
                 (model-output str)
               (command-output str)))
            
            ;; direct assignments are a trick too
            ((and
              (> (length (symbol-name (car text))) 1)
              (equal #\> (aref (reverse (symbol-name (car text))) 0))
              (equal #\= (aref (symbol-name (car text)) 0))
              (> (length text) 1)
              (not (and (> (length (symbol-name (second text))) 1)
                        (equal #\> (aref (reverse (symbol-name (second text))) 0))))
              (or (= (length text) 2)
                  (and (symbolp (third text))
                       (> (length (symbol-name (third text))) 1)
                       (not (eq (second text) 'isa))
                       (or (equal #\> (aref (reverse (symbol-name (third text))) 0))
                           (equal #\! (aref (reverse (symbol-name (third text))) 0)))))
              
              )
              (setf str (format nil "   ~a ~s" (pop text) (pop text)))
             (if model-output
                 (model-output str)
               (command-output str)))
            ((and
              (> (length (symbol-name (car text))) 1)
              (equal #\> (aref (reverse (symbol-name (car text))) 0)))
             (setf str (format nil "   ~a" (pop text)))
             (if model-output
                 (model-output str)
               (command-output str)))
            ((equal (car text) '!stop!)
             (setf str (format nil "   ~a" (pop text)))
             (if model-output
                 (model-output str)
               (command-output str)))
            ((or
              (equal (car text) '!output!)
              (equal (car text) '!eval!)
              (equal (car text) '!safe-eval!))
             (setf str (format nil "   ~a ~s" (pop text) (pop text)))
             (if model-output
                 (model-output "~a" str)
               (command-output "~a" str)))
            ((or
              (equal (car text) '!bind!)
              (equal (car text) '!mv-bind!)
              (equal (car text) '!safe-bind!))
             (setf str 
               (format nil "   ~a ~s ~s" (pop text) (pop text) (pop text)))
             (if model-output
                 (model-output "~a" str)
               (command-output "~a" str)))
            ((member (car text) '(= - < > <= >=))
             (setf str 
               (format nil "    ~2a ~s ~s" (pop text) (pop text) (pop text)))
             (if model-output
                 (model-output "~a" str)
               (command-output "~a" str)))
            (t
             (setf str (format nil "       ~s ~s" (pop text) (pop text)))
             (if model-output
                 (model-output "~a" str)
               (command-output "~a" str)))))
    (if model-output
        (model-output ")")
      (command-output ")"))))


(defun sort-productions (p1 p2 best)
  (let ((p1-u (production-conflict-val p1))
        (p2-u (production-conflict-val p2)))
  (cond ((= p1-u p2-u)
         (< (or (position p1 best) (1+ (length best)))
            (or (position p2 best) (1+ (length best)))))
        (t (> p1-u p2-u)))))
    
    
(defun production-selected (procedural production)
  
  (dolist (x (production-selection-code production))
    (case (car x)
      (query-buffer 
       (when (procedural-lhst procedural)
         (schedule-query-buffer (second x) (replace-variables (third x) (production-bindings production)) 0 :module 'procedural )))
      (buffer-read 
       (when (procedural-lhst procedural)
         (schedule-buffer-read (second x) 0 :module 'procedural)))
      (buffer-search
       (when (procedural-lhst procedural)
         (schedule-event-relative 0 'buffer-search :module 'procedural :params (cdr x))))))
  
  (note-production-selection (production-name production))
  
  (setf (procedural-busy procedural) t)
  (schedule-module-request 'production (procedural-req-spec procedural) 0 :module 'procedural :output nil :details (symbol-name (production-name production)) :priority :max)
    
  (schedule-event-relative 
   (if (procedural-random-times procedural)
       (randomize-time (productions-action-time (production-name production)))
     (productions-action-time (production-name production)))
   'production-fired 
   :module 'procedural
   :destination 'procedural
   :params (list production)
   :details (concatenate 'string
              (symbol-name 'production-fired)
              " "
              (symbol-name (production-name production)))
   :output 'low))
      
      
(defun buffer-search (buffer-name)
  "dummy function to show in the trace"
  (declare (ignore buffer-name)))
      
(defun production-fired (procedural production)
  ;(if t 
  ; productions can't fail to fire 
  ;(< (act-r-random 1.0)
  ;   (or (production-chance production)
  ;       (production-p production)))
  
  
  (dolist (x (production-actions production))
    (when (car x)
      (funcall (car x))))
  
  ;; This never happens now
  ;  (schedule-event-relative 0 'production-failed :module 'procedural
  ;                           :prioriy :max :output 'low))
  
  (learn-parameters (production-name production))

  (schedule-event-relative 
   0 
   'conflict-resolution :module 'procedural 
   :priority :min 
   :destination 'procedural
   :output 'medium)
  
  (dolist (hook (procedural-cycle-hook procedural))
    (funcall hook (production-name production)))
  
  ;; Call this explicitly now...
  (compile-productions production)
  
  (setf (procedural-busy procedural) nil))


;;; Dummy function to indicate a production "failing to fire"

(defun production-failed ())

(defun procedural-query (instance buffer-name slot value)
  (declare (ignore slot)) ; the only slot is state
  (case value
    (busy (procedural-busy instance))
    (free (not (procedural-busy instance)))
    (error nil)
    (t (print-warning "Unknown state query ~S to ~S buffer" 
                      value buffer-name)
       nil)))

(defun procedural-request (instance buffer-name chunk-spec)
  (declare (ignore instance buffer-name chunk-spec))
  )


(defun procedural-run-check (instance)
  (declare (ignore instance))
  
  ;; if there aren't any procedural events put a new
  ;; conflict-resolution out there...
  (unless (mp-modules-events 'procedural)
    (schedule-event-after-change 'conflict-resolution
                                 :module 'procedural
                                 :destination 'procedural
                                 :output 'medium
                                 :dynamic t)))
                                

(define-module-fct 'procedural '(production)
  (list (define-parameter :er :owner nil)
        
        (define-parameter :v :owner nil)    
        (define-parameter :md :owner nil)
        (define-parameter :ppm :valid-test #'numornil :default-value nil
          :warning "a number or nil" :documentation "Procedural partial matching")
        
        (define-parameter :dat :valid-test #'numberp :default-value .05
          :warning "a number" :documentation "Default Action Time")
        
        (define-parameter :crt :valid-test #'tornil :default-value nil
          :warning "T or nil" :documentation "Conflict Resolution Trace")
        (define-parameter :cst :valid-test #'tornil :default-value nil
          :warning "T or nil" :documentation "Conflict Set Trace")
        
        (define-parameter :lhst :valid-test #'tornil :default-value t
          :warning "T or nil" 
          :documentation "Left Hand Side Trace")
        (define-parameter :rhst :valid-test #'tornil :default-value t
          :warning "T or nil" 
          :documentation "Right Hand Side Trace")
        
        (define-parameter :ppm-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Procedural partial matching utility adjustment hook")
        
        (define-parameter :cycle-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Cycle hook")
            
        (define-parameter :vpft :valid-test #'tornil :default-value nil
          :warning "T or nil" 
          :documentation "Variable Production Firing Time")

        (define-parameter :conflict-set-hook :valid-test #'fctornil 
          :default-value nil
          :warning "a function or nil" 
          :documentation "Conflict set hook")
        
        ;;; There is another list parameter but not a hook
        ;;; it's the list of buffers not to use for strict harvesting
        
        (define-parameter :do-not-harvest :valid-test #'symbolp 
          :default-value nil
          :warning "a symbol" 
          :documentation "Buffers that are not strict harvested")
        
        (define-parameter :use-tree :valid-test #'tornil :default-value nil
          :warning "T or nil" 
          :documentation "Use a decision tree in production matching")
        )
  
  :version "2.0a1" 
  :documentation 
  "The procedural module handles production definition and execution"
    
  :creation (lambda (x) (declare (ignore x)) (make-procedural))
  :query #'procedural-query
  :request #'procedural-request
  :reset '(reset-procedural-module nil finalize-procedural-reset)
  :params #'procedural-params
  :run-start #'procedural-run-check
  )




#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#

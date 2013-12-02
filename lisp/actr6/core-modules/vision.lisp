;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne & Dan Bothell
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2007 Mike Byrne/Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : vision.lisp
;;; Version     : 3.1
;;; 
;;; Description : Source code for the ACT-R 6 Vision Module.  
;;;
;;; Bugs        : [X] Should object-present-p do a chunk match to determine if
;;;                   the object is still the "same"?  I'm doing that now, but
;;;                   may want to consider something else long term...
;;;                   UPDATE: not doing the chunk test now because it results 
;;;                   in differences where a change is not re-encoded that 
;;;                   which should be with update-attended-loc.  May still
;;;                   want to consider other options.
;;;             : [ ] Not so much a bug, but an inconsistency with the old
;;;                   version.  Find-best-feature doesn't "short circuit"
;;;                   as often with the legacy devices as the old system which
;;;                   means that it calls random-item at times where the same
;;;                   model under the old module didn't.  The behavior of this
;;;                   module is the same regardless, but the extra call to
;;;                   act-r-random will result in different "random" results
;;;                   downstream from the extra call in other modules or code.
;;;                   Only really matters if one has set the seed explicitly
;;;                   to produce a specific output (the testing traces).  Shouldn't
;;;                   cause problems otherwise.  
;;;             : [X] Current module doesn't work quite the same as the old version
;;;                   when items overlap in the display.  Particularly, a screen
;;;                   change during an attention shift for items at the same location
;;;                   in the old module you got the "old" attended item but in the
;;;                   current module you get the "new" item.  Not sure what is
;;;                   really right there.  Switched it so that now you get the
;;;                   old item as well.
;;;
;;; Todo        : [ ] Check the icon-entry code again now that the feats get
;;;                   mapped from old to new when there's no clear because it
;;;                   probably doesn't need to do the "feat match" check anymore...
;;;             : [X] Add in the support for other scales/synthesizing letters from
;;;                   primitives and re-enable the :optimize-visual parameter.
;;;             : [X] Add the tracking code in.
;;;             : [ ] Add hooks into proc-display and find-location to let users
;;;                   configure things and filter the matching (pre and post default)
;;;                   as needed.
;;;             : [ ] Consider generalizing the scale parameter so that the devices
;;;                   can take advantage of it for other uses.
;;;             : [ ] What's the right attended status for something that's no
;;;                   longer in the visicon or for overlapping new items?
;;;                   *Essentially added an "undefined" attended status for
;;;                   *items that are no longer in the visicon - it always
;;;                   *returns nil for the query regardless of the request
;;;                   *which varies from the old module because the old one would 
;;;                   *consider items no longer in the display as "attended nil".
;;;                   -- changed this because if it's a visual-location then it
;;;                   should return "attended nil" as t if it's truely not attended.
;;;                   However, this and the old module now differ because if
;;;                   attention goes back to one of these "old" locations the
;;;                   "old" location chunk reports as attended t in the current
;;;                   module, but it was attended nil in all past versions.
;;;             : [ ] Fix the add/delete/update-visicon-item code to work right
;;;                   when deleteing visicon chunks and test-feats is true.
;;;                   Pretty low priority since most people use add/delete/update
;;;                   for performance and test-feats can have a noticeable cost.
;;;             : [X] There is no feat-table now! 
;;;                   Should the feat-table be purged other than on a clear, for
;;;                   instance if an object goes away without a clear?
;;;             : [ ] Does test-feats do the right thing in enter-into-visicon
;;;                   with respect to checking the real-visual-value i.e. does
;;;                   that always get set correctly elsewhere or can that break
;;;                   with a custom device?
;;; 
;;; ----- History ----- [look also at function comments]
;;;
;;; 2007.05.23 Dan 
;;;             : * Start of a rewrite to use chunks internally for the visicon
;;;             :   and as the objects themselves.
;;;             : * Will work in conjunction with updated device interface, devices,
;;;             :   and motor (mouse cursor stuff) files.
;;; 2007.06.20 Dan
;;;             : * Works in the abstract case at this point (modeler supplied
;;;             :   device which handles everything explicitly).
;;;             : * Took the nearest slot out of visual-locations and made it
;;;             :   a request parameter - should have happened in the old system
;;;             :   long ago...
;;;             : * Also took the userprop slots out and put height and width in.
;;; 2007.07.05 Dan
;;;             : * Modified visicon-chunks to sort based on x-y coordinates to
;;;             :   provide more consistency across platforms/Lisps since the
;;;             :   visicon hash-table doesn't maintain the entry order.  It's
;;;             :   a slight performance hit, but consistency in the tutorial 
;;;             :   and elsewhere is worth it (for now at least, and it could
;;;             :   be put onto a parameter to check if people don't like it).
;;; 2007.07.06 Dan
;;;             : * Added a default case to icon-entry to just return the
;;;             :   chunks explicit chunk-visicon-entry value when the "matching"
;;;             :   process fails (fixes a bug in re-encoding of the screen when
;;;             :   a "close" but new object is the one that gets re-encoded).
;;; 2007.07.06 Dan
;;;             : * Added the text-feats function even though it's not really
;;;             :   needed yet...
;;; 2007.07.09 Dan
;;;             : * Fixed a bug when move tolerance was 0.0 - needed to add
;;;             :   a couple chunk-visicion-entry calls around chunk names in a
;;;             :   couple of places to make sure the "right" chunk was being
;;;             :   referenced.
;;; 2007.07.12 Dan
;;;             : * Fixed a bug with the move-attention requests.  It didn't
;;;             :   verify that the chunk for the screen-pos was of type visual-
;;;             :   location before sending it off for further processing.
;;; 2007.10.04 Dan
;;;             : * Added the pieces to handle the "special" processing of
;;;             :   text items into sub-features (:optimize-visual nil), and
;;;             :   the processing of scale {phrase or word} in the move-attention
;;;             :   requests for text items.
;;;             : * Updated the version to a2.
;;; 2007.11.01 Dan
;;;             : * Fixed bugs in attend-visual-coordinates and enter-into-visicon.
;;; 2007.11.16 Dan
;;;             : * Added a check of moving-atteniton to the drop-out check for
;;;             :   update-attended-loc.
;;; 2007.12.11 Dan
;;;             : * Added the randomizing of times for the reencoding events.
;;;             :   That was a bug introduced in the 5->6 transition.
;;; 2007.12.17 Dan
;;;             : * Finished first pass at adding the tracking code in.
;;;             :   It works with either the old "screen-obj" style mechanism
;;;             :   (though that's now depricated) or through the visual-location
;;;             :   feature chunk (must return the same location chunk to move
;;;             :   the item for the model).
;;; 2007.12.18 Dan
;;;             : * Clean-up of the tracking code to fix some of the cases
;;;             :   that weren't signaled or handled correctly.
;;; 2007.12.19 Dan
;;;             : * Minor fix for update-tracking-mth - make sure vis-loc-to-obj
;;;             :   called with the current device or object as needed.
;;; 2007.12.20 Dan
;;;             : * Changed object-present-p to not do the chunk testing for
;;;             :   checking for a similar object because it prevented some
;;;             :   re-encodings that should have happened.  (Also see the note
;;;             :   under bugs above.)
;;; 2007.12.21 Dan
;;;             : * Changed find-current-locs-with-spec to catch the case where
;;;             :   :nearest current is specified but there isn't yet a currently
;;;             :   attended location.  To be backward compatible it assumes 0,0
;;;             :   and prints a warning about it.
;;; 2008.01.07 Dan
;;;             : * Added merging functions for visicon-entry, visual-object, and
;;;             :   real-visual-value chunk parameters so that the new chunk's
;;;             :   values are set in the merged chunk.  Vision needs the newest
;;;             :   info available to properly align with the visicon and current
;;;             :   device...
;;; 2008.01.09 Dan
;;;             : * To maintain the determinism of things the chunks to be
;;;             :   stuffed are first sorted by name before a random one is 
;;;             :   picked.  Shouldn't be too much of a performance hit and
;;;             :   it's needed if the test models are to always produce the
;;;             :   same traces.
;;; 2008.01.16 Dan
;;;             : * Fixed a bug in icon-entry that could throw a warning if the
;;;             :   visual-location being used in an attention shift hadn't 
;;;             :   orignially come from the vision module.
;;; 2008.01.31 Dan
;;;             : * Added in a version of the add-screen-object and delete-screen-object
;;;             :   commands for those that used them in the old module.  The
;;;             :   delete method differs from the old version in that it takes
;;;             :   the same object as add does.
;;; 2008.02.14 Dan
;;;             : * Adding a new request to the visual-location buffer - set-visloc-default.
;;;             :   That allows the model to configure the buffer stuffing test as
;;;             :   would be done with the set-visloc-default command without needing
;;;             :   to resort to a !eval!.
;;;             : * Re-fixed a bug that got lost somewhere along the way!
;;;             :   find-current-locs-with-spec mis-tested :attended.
;;;             :   Was fixed on 2007.12.07 (it's in the commit logs) but that
;;;             :   change and note isn't here now so somewhere along the way
;;;             :   I modified and committed an old version...
;;;             : * Added some more saftey tests to set-visloc-default command.
;;; 2008.02.14 Dan [3.0b1]
;;;             : * Corrected an oversight in the visual-location matching.
;;;             :   The value slot wasn't being compared to the "real" visual
;;;             :   value for something like text where the visual-location
;;;             :   chunk's value isn't the same as what print-visicon shows.
;;;             :   Thus, unlike the old version of the module, requests like
;;;             :   this wouldn't work as expected prior to this fix:
;;;             :   +visual-location> isa visual-location value "a"
;;;             :   for "standard" text display items because the default operation 
;;;             :   for text items is to set the value slot to text in the 
;;;             :   visual-location chunk and that's what the comparison was using.
;;;             :   Now, when the chunk-real-visual-value parameter is set, that's
;;;             :   what gets used for the value slot tests.
;;; 2008.02.15 Dan
;;;             : * Added a remove-finst function since update-cursor-feat 
;;;             :   called it!
;;; 2008.03.13 Dan
;;;             : * Changed the priority on the set-visloc-default request
;;;             :   action that gets scheduled to have a value of 9 which puts
;;;             :   it just below the implicit buffer clearing due to the 
;;;             :   request.  That makes it very unlikely that a cooccuring
;;;             :   proc-display will sneak in between the clearing of the 
;;;             :   buffer and the changing of the defaults and thus stuff
;;;             :   an "unwanted" chunk in there.  It could avoid that by
;;;             :   scheduling the priority higher than the clear, but I don't
;;;             :   think that's a good practice in general.
;;; 2008.03.21 Dan
;;;             : * Fixed a bug in test-attended because it assumed the chunk
;;;             :   for which the attended status was being checked was still
;;;             :   a valid member of the visicon.  Brings up a question which
;;;             :   I've added under bugs - what's the right status for such
;;;             :   a chunk?  Should it just always "fail" the query?  
;;;             : * A related question the test model brought up is what 
;;;             :   about when there are "overlapping" features - should they
;;;             :   share a finst?
;;;             : * Fixed an issue with enter-into-visicon because it didn't
;;;             :   consider the "real" value of the items when eliminating
;;;             :   possible duplicates.
;;; 2008.03.31 Dan
;;;             : * Changed visicon-chunks so that it has an optional parameter
;;;             :   which indicates when to sort things.  Only using that for
;;;             :   the print-visicon command and find-location requests now to 
;;;             :   improve performance and still maintain the "cross platform"
;;;             :   consistency.
;;; 2008.04.01 Dan
;;;             : * Added add-visicon-item and delete-visicon-item as more
;;;             :   "user" level equivalents of add-screen-object and delete-...
;;;             :   because they don't require passing the vision module itself
;;;             :   as a parameter.  Otherwise they're the same commands.
;;; 2008.04.07 Dan
;;;             : * Added an automatic call to stuff-visloc-buffer when a
;;;             :   set-visloc-default request is made to trigger any possible 
;;;             :   stuffing based on the new settings. 
;;;             : * Adding a new parameter :auto-attend.
;;;             :   When it is set to t (defaults to nil) it will cause the 
;;;             :   vision module to automatically start a move-attention after
;;;             :   a successful visual-location request completes to attend
;;;             :   to that visual-location.
;;;             :   Basically it's a modeling shortcut for simple situations
;;;             :   where the result of the visual-location request aren't
;;;             :   that important and one is just going to attend it anyway.
;;;             :   Saves some productions in the model but doesn't affect the
;;;             :   timing because the request is delayed 50ms to compensate for 
;;;             :   the skipped production.
;;;             :   The module is not marked as busy during the 50ms delay.  So
;;;             :   one should be careful about using explicit move-attention
;;;             :   requests when this parameter is enabled.  The assumption is
;;;             :   that you won't be using explicit requests if you turn this
;;;             :   option on.
;;; 2008.04.08 Dan
;;;             : * Fixed an issue with re-encoding - it didn't mark the chunk
;;;             :   being set into the buffer as unrequested.  Now it does.
;;; 2008.04.10 Dan
;;;             : * Added a new parameter :test-feats which can allow for some
;;;             :   significant improvements in run time for proc-display calls
;;;             :   if it is set to nil (the default value is t).  There are 
;;;             :   limited situations where one can safely set it to nil:
;;;             :   - All proc-display calls specify :clear t
;;;             :   - If each visual item in the device returns the "same" chunk 
;;;             :     from build-vis-locs-for every time (by same it means same 
;;;             :     name though the contents could differ)
;;;             :   The next step is to update the built in devices so that 
;;;             :   they satisfy the second situation and thus work correctly
;;;             :   with the parameter set to nil.
;;; 2008.05.22 Dan
;;;             : * Added a warning to add-finst since it can be called by the
;;              :   model through an assign-finst request with a "bad" value.
;;; 2008.06.20 Dan
;;;             : * Removed the -fct from slot-specs-to-chunk-spec-list and
;;;             :   slot-specs-to-chunk-spec since the macros were removed from 
;;;             :   the chunk-spec code.
;;; 2008.07.02 Dan [3.0b2]
;;;             : * Added code to delete the "internal" visicon chunks when a
;;;             :   proc-display occurs.
;;;             :   Requires locking the device during visual-location buffer
;;;             :   settings to avoid possible deletion of a chunk that hasn't
;;;             :   yet been copied for the buffer.
;;; 2008.07.14 Dan
;;;             : * Fixed an issue when test-feats was t that overlapping text
;;;             :   could persist - the old icon entry may stick around when 
;;;             :   :clear t not given to proc-display.
;;; 2008.07.15 Dan
;;;             : * Changed the output setting for find-loc-failure so that it
;;;             :   prints in the low detail trace.
;;; 2008.07.22 Dan
;;;             : * Fixing the overlapping text issue on the 14th re-introduced
;;;             :   a bug where overlapping text items with different values
;;;             :   were "merged" into one feature.  Re-fixed that issue.
;;; 2008.07.24 Dan
;;;             : * Adding yet another parameter  - :delete-visicon-chunks.
;;;             :   Defaults to t, and when on the visicon chunks get purged
;;;             :   during proc-display.
;;; 2008.08.11 Dan [3.0b3]
;;;             : * Adding a new query to the module to allow one to detect
;;;             :   visual changes other than through the visual-location buffer
;;;             :   stuffing. 
;;;             :   
;;;             :   There's a new parameter called :scene-change-threshold
;;;             :   and if during a proc-display the computed change equals
;;;             :   or exceeds that setting the scene-change query of the visual
;;;             :   buffer will go true.
;;;             :   It will stay true for a time equal to the :visual-onset-span 
;;;             :   value.
;;;             :   It can also be explicitly cleared by using the new request
;;;             :   which takes no parameters and no time:
;;;             :     +visual>
;;;             :       isa clear-scene-change
;;;             :   It is also cleared with the existing clear request:
;;;             :     +visual>
;;;             :       isa clear 
;;;             :   The change computation is computed as a proportion of
;;;             :   items which differ between the two sets and the setting of the 
;;;             :   :scene-change-threshold must be a value in the range 0.0-1.0.
;;;             :   
;;;             :   Here's the computation used to determine the change value:
;;;             :   
;;;             :   ov = the set of items in the visicon when proc-display called.
;;;             :   o  = the number of items in ov.
;;;             :   d  = number of items from ov not in the new visicon.
;;;             :   n  = number of items in the new visicon which were not in ov.
;;;             :   
;;;             :   change = (d+n)/(o+n)
;;;             :   
;;;             :   If o and n are both 0 then the change is 0.    
;;; 2008.08.13 Dan
;;;             : * Added a new query for the visual buffer: scene-change-value.
;;;             :   It's mostly there as a debugging aid to show what the last
;;;             :   scene-change proprotion was through the buffer-status 
;;;             :   command.  However, it can be queried normally.  The value
;;;             :   given should be a number.  If the last scene-change had a
;;;             :   proportion of change greater than that value the query will
;;;             :   respond as true.  It does not time out like the scene-change
;;;             :   query does (after the onset span time).
;;; 2009.03.31 Dan [3.0]
;;;             : * Seems stable enough now to take the beta version off...
;;;             : * Added a hash-table based compare mechanism like DM uses for
;;;             :   fast-merging to the process-display code.  About the same
;;;             :   speed-wise for small (0~15) displays, but significantly
;;;             :   faster as they get larger.
;;;             : * Added a new command remove-visual-finsts since people have 
;;;             :   been using proc-display :clear t to do that even without
;;;             :   "changing" the display.
;;;             : * Fixed a bug related to "proc-display :clear t" - when the
;;;             :   same chunks were returned by the build-vis-locs-for method(s)
;;;             :   as it had previously any finsts on those items may have
;;;             :   persisted once the item was no longer "new" again.
;;; 2009.04.27 Dan
;;;             : * Added a check to see if there's a lock set in stuff-visloc-
;;;             :   buffer so it doesn't try to add more than one item at a time.
;;;             :   It was already setting and clearing a lock, but just wasn't
;;;             :   testing for it...
;;; 2009.04.29 Dan
;;;             : * Changed the phrase parsing so that the phrase! chunk gets a 
;;;             :   color based on the majority color among the individual words.
;;;             :   Also added a colors slot to hold the list of item colors
;;;             :   corresponding to the words and objects slots.
;;; 2009.08.27 Dan
;;;             : * Changed visicon-update so that it has an optional parameter
;;;             :   to determine whether or not it needs to count the visicon 
;;;             :   items for return.
;;;             : * Modified delete-screen-object (and thus delete-visicon-item)
;;;             :   so that the internal chunk gets purged if :delete-visicon-chunks
;;;             :   is true.
;;; 2009.08.28 Dan
;;;             : * Added optional update parameters to the internal add-screen-object
;;;             :   and delete-screen-object and the user commands add-visicon-item
;;;             :   and delete-visicon-item so that multiple adds & deletes can avoid
;;;             :   all having to run the updating code.  One benefit to that is that 
;;;             :   it won't always be the first item added to the screen that gets 
;;;             :   stuffed if one doesn't want that.
;;;             : * Added an update-visicon-item command to go along with the 
;;;             :   add- and delete- ones.  It has a required parameter of the object
;;;             :   an optional parameter for updating (like add and delete now have)
;;;             :   and two possible keword parameters :same-chunks and :chunks.
;;;             :   It works in one of three ways based on which if any of the 
;;;             :   keyword parameters are provided (if the parameters are invalid
;;;             :   then it just prints a warning and does nothing):
;;;             :
;;;             :   - If neither is given then it does the same thing as if the object 
;;;             :   had been deleted and then added again.
;;;             :
;;;             :   - If :chunks is non-nil and is either a symbol naming a chunk or a 
;;;             :   list of chunk names all of which correspond to the chunk(s) that
;;;             :   were originally added to the visicon for the object with add-visicon-
;;;             :   item then it will update the module's internal representation of 
;;;             :   those features.
;;;             :
;;;             :   - If :chunks is nil but :same-chunks is t then it will call 
;;;             :   build-vis-locs-for for that object and treat the list of chunks
;;;             :   that are returned the same as if the :chunks parameter had been
;;;             :   given with that list.
;;;             :
;;;             :   One note on using update-visicon-item is that you must set :test-feats
;;;             :   to nil for now otherwise it will not work.  That means if you want
;;;             :   overlapping duplicate features removed you will have to handle that
;;;             :   explicitly outside of the module.
;;; 2009.08.31 Dan
;;;             : * Added more warnings since delete-visicon-item also has a potential
;;;             :   issue with test-feats if it's set to purge chunks.
;;; 2009.11.18 Dan
;;;             : * Fixed an issue with a '(lambda ...)  in synthesize-phrase
;;;             :   because LispWorks doesn't like that construct.
;;; 2010.02.04 Dan
;;;             : * Make sure tracking keeps a finst on something tracked for 
;;;             :   longer than the finst duration time.
;;; 2010.02.12 Dan
;;;             : * Let a move-attention break tracking without having to
;;;             :   explicitly clear it first.  The issue was that tracking 
;;;             :   keeps the module busy so the move-attention would jam, but
;;;             :   now it ignores that jam if it's due to tracking.
;;; 2010.02.18 Dan
;;;             : * Fixed a bug which could cause problems with a device that
;;;             :   was modifying the chunks for its features prior to a 
;;;             :   proc-display call.  Within-move was returning the original
;;;             :   chunk and not the internal copy and featlis-to-chunks was
;;;             :   using that orignial chunk to set the icon-entry for the
;;;             :   object.  Now within-move uses the internal copy and featlis-to-
;;;             :   chunks gets the appropriate visicon-entry from that.
;;; 2010.02.19 Dan
;;;             : * Modified featlis-to-focus and determine-focus-dmo to go
;;;             :   along with the last couple of fixes to make sure everything
;;;             :   stays in sync.
;;; 2010.03.02 Dan
;;;             : * Fixed a bug with featlis-to-focus that could cause problems
;;;             :   when an attended item leaves the screen.
;;; 2010.03.25 Dan
;;;             : * Added a purge-chunk to enter-into-visicon for those which
;;;             :   already existed in the table since they wouldn't have been
;;;             :   purged previously.
;;; 2010.04.30 Dan
;;;             : * Adjusted how :auto-attend works to now mark the module as
;;;             :   busy from the time of the find-location all the way through
;;;             :   the encoding-complete.
;;;             : * Also added an event to show in the trace and make sure that
;;;             :   it is using the buffer's chunk and not the internal chunk
;;;             :   to avoid problems with deleting visicon chunks.
;;; 2010.05.03 Dan
;;;             : * Changed the :output of the "No visual-object found" event
;;;             :   from 'high to 'medium since that's how it has always printed
;;;             :   anyway because of a bug in filter-test.
;;; 2010.08.03 Dan
;;;             : * Changed update-tracking-loc-chunk to also set the current-marker
;;;             :   so that the focus ring updates as the model tracks the item.
;;;             : * Also set current-marker in update-tracking-mth because there
;;;             :   could be a model issue that prevents update-tracing-loc-chunk
;;;             :   from being called (cases 6, 7, 8, and 9).
;;;             : * Don't set the object's screen-pos slot in update-tracking-mth
;;;             :   now except in the cases where it won't get set via update-tracking-loc-chunk.
;;;             :   That prevents an issue with deleting that visicion chunk 
;;;             :   later in most cases.  May want to not set it at all in those
;;;             :   cases.
;;; 2011.01.19 Dan
;;;             : * Changed the attended querying so that chunks which aren't
;;;             :   part of the visicon now may still report that they are
;;;             :   attended nil instead of having all the attended queries fail
;;;             :   as was done previously and noted in the todo.  This differs
;;;             :   from the old vision because the old one only allowed the attended
;;;             :   tests for chunks which came from vision at some point -- thus
;;;             :   arbitrary locations would have been in the all queries fail
;;;             :   situation which the new version no longer has.  Neither however
;;;             :   addresses issue of attention "returning".
;;;             : * Depricating update-tracking now in preparation for fixing some
;;;             :   of the lingering finst/object issues in tracking.
;;;             : * Added unlock-tracking since unlock-device used to call
;;;             :   update-tracking.
;;;             : * Changed update-tracking-mth to account for the fact that things
;;;             :   should always be valid when it starts which avoids some warnings
;;;             :   and problems that used to happen when it tried to update an
;;;             :   'object' on the fly.
;;;             : * Update-attended-loc schedules it's state change so that the
;;;             :   buffer trace will always catch it.
;;; 2011.02.04 Dan [3.1]
;;;             : * A reworking of how the visicon gets maintained with respect to
;;;             :   recording features.  Instead of two tables, one for feature
;;;             :   comparison and one for the visicon, there's only a visicon
;;;             :   table and the keys are the feature lists when :test-feats is
;;;             :   set to t, otherwise the device's loc chunk is the key.
;;;             : * Fixes some issues with re-attending for an object that moves.
;;;             : * Update cursor doesn't modify the existing feature anymore
;;;             :   and instead just creates a new one.
;;;             : * Find-current-loc-with-spec now uses the real-visual-value
;;;             :   value of the "current" when 'value current' is requested if
;;;             :   there is such a value.
;;;             : * Added a new chunk parameter visual-feature-name since the
;;;             :   visicon-entry can now be a list of features instead of the
;;;             :   device's chunk name.
;;;             : * Updated the synth-feat code so that the colors get set
;;;             :   based on the majority color of the lower level when :optimize-
;;;             :   visual is nil too.
;;;             : * Fixed an issue with how the features are marked for a phrase
;;;             :   when optimize-visual is nil.
;;; 2011.03.28 Dan
;;;             : * Fixed a bug with how current-marker gets set during tracking.
;;; 2011.04.01 Dan
;;;             : * Very minor change to tracking code to avoid an unnecessary
;;;             :   copy.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General description of update
;;;
;;; The idea is to take advantage of the capabilities in ACT-R 6 to use chunks
;;; for the visicon.  Replace build-features-for with build-vis-locs-for that just 
;;; needs to return a list of chunks which can be subtypes of visual-location.  That 
;;; allows the modeler more freedom in representation.  Along with that, the "find-
;;; location" requests will now use the full power of the general chunk matcher which 
;;; means that slots can be specified any number of times and variables will
;;; be available that would allow for something like saying "find a location where
;;; the x and y coordinates are the same" or "find a location where the width
;;; is greater than the height".
;;;
;;; Along with that will be a corresponding shift from feat-to-dmo to vis-loc-to-obj 
;;; which again will shift toward returning the chunk representation directly.
;;; 
;;; It should be completely backward compatible with existing models, but not
;;; with existing devices or code that takes advantage of "hidden" features of
;;; the vision module (some undocumented capabilities like attn-trace have been
;;; removed to simplify the support code, but if people really need those they
;;; can be restored later).
;;;
;;; This does sacrifice a lot of the OO functionality for features and specs,
;;; but my observation is that many  users aren't taking advantage of that 
;;; now and in fact some don't want to bother with it to the point that they 
;;; implement a complete replacement module for vision instead of creating a new 
;;; device to use (yes, really)!  The hope is that by making it chunk based it will 
;;; be more accessible to the average user without giving up too much that the 
;;; advanced user relies on.
;;;
;;; Because it's going to be less restrictive in terms of the find-locations that
;;; it can do there is the the possibility of abusing the system in a "super-human"
;;; way, but that's an issue in other parts of the system as well and one of those
;;; things that the modeler will have to consider his/her own feelings of 
;;; what's plausible.  Basically, the idea is that the tool is very flexible
;;; and it's up to the users to determine how best to use it for their purposes.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "GENERAL-PM" "ACT-R6:support;general-pm")


#+:allegro (eval-when (:compile-toplevel :Load-toplevel :execute)
             (setf *enable-package-locked-errors* nil))


(eval-when (:compile-toplevel :Load-toplevel :execute)
  (proclaim '(optimize (speed 3) (space 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-chunks synth-feat :copy-function identity)
(extend-chunks visual-new-p)


(defclass vision-module (attn-module)
  ((visicon :accessor visicon :initarg :visicon :initform (make-hash-table :test #'equalp))
   (optimize-visual :accessor optimize-p :initarg :optimize-p :initform t)
   (move-attention-latency :accessor move-attn-latency :initarg :move-attn-latency :initform 0.085)
   (tracked-object :accessor tracked-obj :initarg :tracked-obj :initform nil)
   (tracked-object-last-location :accessor tracked-obj-lastloc :initarg :tracked-obj-lastloc :initform nil)
   (tracked-object-last-feat :accessor tracked-obj-last-feat :initarg :tracked-obj-last-feat :initform nil)
   (tracked-object-last-obj :accessor tracked-obj-last-obj :initarg :tracked-obj-last-obj :initform nil)
   (last-scale :accessor last-scale :initarg :last-scale :initform nil)
   (moving-attention :accessor moving-attention :initarg :moving-attention :initform nil)
   (move-allowance :accessor move-allowance :initarg :move-allowance :initform 0)
   (synthd-objs :accessor synthd-objs :initarg :synthd-objs :initform (make-hash-table)) 
   (found-locs :accessor found-locs :initarg :found-locs :initform (make-hash-table))
   (feature-sets :accessor feature-sets :initarg :feature-sets :initform (all-feature-sets))
   (active-cfs :accessor active-cfs :initarg :active-cfs :initform nil)
   (num-finst :accessor num-finst :initarg :num-finst :initform 4)
   (finst-lst :accessor finst-lst :initarg :finst-lst :initform nil)
   (finst-span :accessor finst-span :initarg :finst-span :initform 3.0)
   (new-span :accessor new-span :initarg :new-span :initform 0.5)
   (default-spec :accessor default-spec :initarg :default-spec :initform nil)  ;; (define-chunk-spec isa visual-location screen-x lowest :attended new)
   (visual-lock :accessor visual-lock :initform nil)
   (last-obj :accessor last-obj :initform nil) ;; added for use in tracking
   (auto-attend :accessor auto-attend :initform nil)
   (test-feats :accessor test-feats :initform t) ;; runtime optimization parameter
   (purge-visicon :accessor purge-visicon :initform t)
   (scene-change :accessor scene-change :initform nil)
   (change-threshold :accessor change-threshold :initform 0.25)
   (view-dist :accessor view-dist :initform 15)
   ;(feat-table :accessor feat-table :initform (make-hash-table :test 'equal))
   (current-cursor :accessor current-cursor :initform nil))
  (:default-initargs
    :name :VISION
    :version-string "3.1"))


(defmethod vis-loc-to-obj (device vis-loc)
  (declare (ignore device))
  (let ((ct (chunk-slot-value-fct vis-loc 'kind)))
    (fill-default-vis-obj-slots (car (define-chunks-fct `((isa ,ct)))) vis-loc)))


(defun merge-visicon-entry (first second)
  (declare (ignore first))
  (chunk-visicon-entry second))

(extend-chunks visicon-entry :copy-function identity :merge-function merge-visicon-entry)

(extend-chunks visual-feature-name :copy-function identity :merge-function merge-visicon-entry)

(defun hash-visual-chunk-contents (chunk)
  (let* ((ct (chunk-chunk-type-fct chunk))
         (res (list ct)))
    (dolist (slot (chunk-type-slot-names-fct ct) res)
      (if (and (eq slot 'value) (chunk-real-visual-value chunk))
          (push (chunk-real-visual-value chunk) res)
        (push (true-chunk-name-fct (fast-chunk-slot-value-fct chunk slot)) res)))))


(defmethod process-display ((devin device-interface) 
                            (vis-mod vision-module) &optional (clear nil))
  "Build a new visicon and initiate any buffer stuffing that may result"

  (let ((feature-list nil)
        (tempicon nil)
        (o (hash-table-count (visicon vis-mod)))
        (n 0)
        (d 0))
    ;; make sure a device is actually installed
    (unless (device devin)
      (print-warning "Cannot process display--no device is installed.")
      (return-from process-display nil))
    
    ;; make sure it's safe to change things right now
    ;; and save the clear status if not for future use
    
    (unless (zerop (locks devin))
      (push clear (pending-procs devin))
      (return-from process-display nil))
    
    ;; build the temp visicon
    (setf feature-list (flatten (build-vis-locs-for (device devin) vis-mod)))
    
    (when (with-cursor-p devin)
      
      (awhen (cursor-to-vis-loc (device devin))
             (setf (current-cursor vis-mod) it)
             (push it feature-list)))
    
    ;; Verify that they're all valid visicon items (chunks which are subtypes of visual-location)
    ;; and make sure that they've got size and distance values
    
    (dolist (x feature-list)
      (if (and (chunk-p-fct x)
               (chunk-type-subtype-p-fct (chunk-chunk-type-fct x) 'visual-location))
          
          (progn
            
            (unless (numberp (chunk-slot-value-fct x 'size))
              (compute-vis-loc-size x))
            
            (unless (numberp (chunk-slot-value-fct x 'distance))
              (set-chunk-slot-value-fct x 'distance (view-dist vis-mod)))
            
            (push (setf (chunk-visicon-entry x)
                    (if (test-feats vis-mod)
                        (hash-visual-chunk-contents x)
                      x))                  
                  tempicon)
            
            (setf (chunk-visual-feature-name x) x))
        
        (progn 
          (print-warning "Invalid visicon item ~s found when processing the display.  Must be a chunk which is a subtype of visual-location." x)
          (setf feature-list (remove x feature-list)))))
    

    ;; if clear, then remove all the old entries first
    ;; do both of these steps iteratively so that the individual chunks can
    ;; be purged if desired.
    
    (when clear
      (setf d o)
      (when (purge-visicon vis-mod)
        (maphash (lambda (key value)
                   (declare (ignore key))
                   (when (chunk-p-fct value)
                     (purge-chunk-fct value)))
                 (visicon vis-mod)))
      (clrhash (visicon vis-mod))
            
      ;; just punt the finsts??
      ;; should something be able to survive a :clear ?
      
      (setf (finst-lst vis-mod) nil))
    
    (unless clear
      
      ;; take the old ones out of the visicon
      
      (maphash (lambda (key val)
                 (unless (find key tempicon :test 'equal)
                   
                   ;; Not a repeat chunkname or a match to one of the new items
                   
                   ;; increment the number of deleted items
                   (incf d)

                   ;; If desired delete the internal chunk now
                   (when (purge-visicon vis-mod)
                     (purge-chunk-fct val))
                   
                   ;; take this out of the visicon now
                   (remhash key (visicon vis-mod))))
               (visicon vis-mod)))
        
    ;; compute how many new items there are
    
    (setf n (+ (length tempicon) d (- o)))
    
    ;; put the new ones in
    
    (mapcar #'(lambda (vl) (enter-into-visicon vl vis-mod)) feature-list)
  
    ;; other bookkeeping 
    
    ;; don't need to do this since
    ;; presumably the cursor-to-visloc
    ;; should have the right value already...
    ;(synch-mouse devin)
    
    (when (tracked-obj-last-feat vis-mod)
      (update-tracking-mth vis-mod t))
    
    ;; Compute the change value and determine if the flag should be
    ;; set at this time.
    
    (let ((change-val (if (zerop (+ d n))
                          0
                        (* 1.0 (/ (+ d n) (+ o n))))))
        (setf (scene-change vis-mod) (cons change-val (mp-time))))
    
    (visicon-update vis-mod)))


(defmethod visicon-chunks ((vis-mod vision-module) &optional sorted)
  (let ((values nil))
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (push val values))
             (visicon vis-mod))
    (if sorted
        (sort values #'loc-sort)
      values)))


(defun loc-sort (i1 i2)
  (let ((x1 (chunk-slot-value-fct i1 'screen-x))
        (x2 (chunk-slot-value-fct i2 'screen-x))
        (y1 (chunk-slot-value-fct i1 'screen-y))
        (y2 (chunk-slot-value-fct i2 'screen-y)))
    (and (numberp x1) (numberp x2) (numberp y1) (numberp y2)
         (or
        (< x1 x2)
        (and (= x1 x2)
             (< y1 y2))))))

(defmethod update-cursor-feat ((devin device-interface) (vis-mod vision-module))
  (when (and (with-cursor-p devin) (device devin))
    (let ((new-pos (get-mouse-coordinates (device devin)))
          (cur-crsr (current-cursor vis-mod)))
      
      (when (and cur-crsr (not (vpt= new-pos (xy-loc cur-crsr))))
        
        ;; if it has moved just punt the old one and get a new one instead of 
        ;; trying to modify the existing chunk values like was done previously
        
        (remove-finst vis-mod cur-crsr)
        
        (let ((entry (gethash (chunk-visicon-entry cur-crsr) (visicon vis-mod))))
          
          (when entry
            (when (purge-visicon vis-mod)
              (purge-chunk-fct entry))
            
            ;; take this out of the visicon now
            (remhash (chunk-visicon-entry cur-crsr) (visicon vis-mod))))
        
        (setf cur-crsr nil))
      
      (when (null cur-crsr)
          (awhen (cursor-to-vis-loc (device devin))
                 (if (and (chunk-p-fct it)
                          (chunk-type-subtype-p-fct (chunk-chunk-type-fct it) 'visual-location))
                     (progn
                       (setf (current-cursor vis-mod) it)
                       
                       (unless (numberp (chunk-slot-value-fct it 'size))
                         (compute-vis-loc-size it))
                       
                       (unless (numberp (chunk-slot-value-fct it 'distance))
                         (set-chunk-slot-value-fct it 'distance (view-dist vis-mod)))
                       
                       (setf (chunk-visicon-entry it)
                         (if (test-feats vis-mod)
                             (hash-visual-chunk-contents it)
                           it))
                       
                       (setf (chunk-visual-feature-name it) it)
                       
                       (enter-into-visicon it vis-mod)
                       ;; Might have been looking at it so update that now (didn't do this previously)
                       (visicon-update vis-mod))
                   
                   (print-warning "Invalid cursor ~s found when updating the cursor feature." it)))))))


(defun xy-loc (chunk)
  (vector (fast-chunk-slot-value-fct chunk 'screen-x)
          (fast-chunk-slot-value-fct chunk 'screen-y)))
          

(defgeneric visicon-update (vis-mod &optional count)
  (:documentation "To be called after every time the visicon changes."))

(defmethod visicon-update ((vis-mod vision-module) &optional (count t))
  (check-finsts vis-mod)
  (update-attended-loc vis-mod)
  (stuff-visloc-buffer vis-mod)
  (when count
    (length (visicon-chunks vis-mod))))


(defgeneric update-attended-loc (vis-mod)
  (:documentation  "If the attended location needs an update, update."))

(defmethod update-attended-loc ((vis-mod vision-module))
  ;; if we're tracking or moving around, ignore this 
  (when (or (tracked-obj-last-feat vis-mod) (moving-attention vis-mod)
            (eq 'BUSY (exec-s vis-mod)))
    (return-from update-attended-loc nil))
  ;; when do we update?
  ;; [1] when we're looking at an object and it's gone
  ;; [2] when we're looking at nothing and something appears 
  (when (or (and (currently-attended vis-mod)
                 (or 
                  ;; it's not in the visicon
                  (not (gethash (currently-attended vis-mod) (visicon vis-mod)))
                  ;; there isn't a feature like it in the visicon -- not needed now?
                  ;(not (and (chunk-p-fct (currently-attended vis-mod)) (object-present-p vis-mod (currently-attended vis-mod))))
                  ))
            (and (current-marker vis-mod)
                 (null (currently-attended vis-mod))
                 (within-move vis-mod (xy-loc (current-marker vis-mod)))))
    
      
    (schedule-event-relative 
     (randomize-time (move-attn-latency vis-mod))
     'encoding-complete
     :destination :vision
     :module :vision
     :params `(,(current-marker vis-mod) ,(last-scale vis-mod) :requested nil)
     :details (concatenate 'string "Encoding-complete "
                (symbol-name (current-marker vis-mod))
                " "  (symbol-name (last-scale vis-mod)))
     :output 'medium)
    
    (schedule-event-relative 0 (lambda () (change-state vis-mod :exec 'busy)) :module :vision :output nil)))


(defun objs-max-val (list accessor)
  (let ((value nil)
        (matches nil))
          
    (dolist (y list)
      (let ((cur-val (funcall accessor y)))
        (if (or (null value) 
                (> cur-val value))
            (progn
              (setf value cur-val)
              (setf matches (list y)))
          (when (= cur-val value)
            (push y matches)))))
    matches))


(defmethod stuff-visloc-buffer ((vis-mod vision-module))
  
  (unless (or (buffer-read 'visual-location)
              (not (zerop (locks (current-device-interface))))
              (tracked-obj-last-feat vis-mod))
    (awhen (find-current-locs-with-spec vis-mod (default-spec vis-mod))
           
           (schedule-set-buffer-chunk 'visual-location
                                      (random-item (sort (objs-max-val it 'chunk-visual-tstamp) #'string< :key #'symbol-name))
                                      0
                                      :module :vision
                                      :requested nil
                                      :priority 10)
           (lock-device (current-device-interface))
           (schedule-event-relative 0 'unlock-device 
                                :module :vision
                                :destination :device
                                :priority 9
                                :output nil
                                :maintenance t))))


(defun test-attended (attended-spec chunk)
  "Assume it's a visual-location chunk, but could be either
   the key or value from the visicon or it could be an unrelated location chunk"
  
  ;; Let the unrelated chunk match to attended nil now
  ;; instead of failing all queries as was the case previously
  
  (let* ((vm (get-module :vision))
         (visicon-key (chunk-visicon-entry chunk))
         (visicon-value (aif (and visicon-key (gethash visicon-key (visicon vm)))
                             it
                             chunk))
         (value (third attended-spec))
         (result nil)
         (marker (feat-attended visicon-value vm)))
    
    
    (cond ((eq value 'new)
           (setf result (eq marker 'new)))
          ((null value)
           (setf result (or (eq marker 'new) (eq marker nil))))
          (t
           (setf result (eq marker t))))
        
    (if (eq (car attended-spec) '-)
        (not result)
      result)))

(defun matching-attended-chunks (attended-spec chunks)
  (remove-if-not (lambda (x) (test-attended attended-spec x)) chunks))

(defmethod find-current-locs-with-spec ((vis-mod vision-module) spec)
  "Assume that it's a valid visual-location chunk-spec with at most 1
   attended slot specification and one nearest spec"
  
  (let* ((main-spec (strip-request-parameters-from-chunk-spec spec))
         (attended (when (slot-in-chunk-spec-p spec :attended)
                     (car (chunk-spec-slot-spec spec :attended))))
         (slots (chunk-spec-slot-spec main-spec))
         (current (current-marker vis-mod))
         (current-type (when current (chunk-chunk-type-fct current)))
         (nearest (when (slot-in-chunk-spec-p spec :nearest)
                    (car (chunk-spec-slot-spec spec :nearest))))
         (min-max-tests nil))
    
      
    ;; Remap all current values to the current chunk
    
    (if current
        (dolist (x slots)
          (when (eq (third x) 'current)
            (if (find (second x) (chunk-type-slot-names-fct current-type))
                (if (and (eq (second x) 'value) (chunk-real-visual-value current)) 
                   (setf (third x) (chunk-real-visual-value current)) 
                  (setf (third x) (chunk-slot-value-fct current (second x))))
              (progn
                (print-warning "Current visual-location does not have a slot named ~S so it is ignored in the request."
                               (second x))
                (setf slots (remove x slots))))))
      (dolist (x slots)
        (when (eq (third x) 'current)
          (print-warning "There is no currently attended location.  So, request specifying ~S as current is being ignored."
                         (second x))
          (setf slots (remove x slots)))))
    
    ;; Remove all tests for highest and lowest for later
    
    (dolist (x slots)
      (when (or (eq (third x) 'lowest)
                (eq (third x) 'highest))
        (push-last x min-max-tests)
        (setf slots (remove x slots))))
    
    ;; update the finsts and new markers if attended is needed
    
    (when attended
      (update-new vis-mod)
      (check-finsts vis-mod))
  
    ;; find the chunks that match
    
    (let ((possible-chunks (if attended
                               (matching-attended-chunks attended (visicon-chunks vis-mod t))
                             (visicon-chunks vis-mod t)))
          (changed nil))
      
      ;; Hack to reassign value slots as needed before testing
      
      (dolist (check possible-chunks)
        (when (chunk-real-visual-value check)
          (push (cons check (chunk-slot-value-fct check 'value)) changed)
          (fast-set-chunk-slot-value-fct check 'value (chunk-real-visual-value check)))) 
      
    (let ((matching-chunks (find-matching-chunks (slot-specs-to-chunk-spec (chunk-spec-chunk-type main-spec) slots)
                                                 :chunks possible-chunks :variable-char #\&)))
      ;; apply all of the lowest/highest constraints
      ;; in the order provided
      
      (dolist (x min-max-tests)
        (let ((value nil)
              (truth (first x))
              (slot (second x))
              (test (third x)))
          
          ;; find the min/max value
          (dolist (y matching-chunks)
            (let ((cur-val (fast-chunk-slot-value-fct y slot)))
              (unless (numberp cur-val)
                (setf value :fail)
                (print-warning "Cannot apply ~S constraint because not all chunks have a numerical value." x)
                (return))
              (when (or (null value) 
                        (and (eq test 'lowest)
                             (< cur-val value))
                      (and (eq test 'highest)
                           (> cur-val value)))
                (setf value cur-val))))
          
          (setf matching-chunks (remove-if-not (lambda (z)
                                                 (if (eq truth '=)
                                                     (= value z)
                                                   (not (= value z))))
                                               matching-chunks
                                               :key (lambda (z) 
                                                      (fast-chunk-slot-value-fct z slot))))))
      
      ;; if there's a nearest constraint then
      ;; apply that filter now
      
      (when (and nearest matching-chunks)
                
        (if (or (eq (third nearest) 'current)
                (eq (third nearest) 'current-x)
                (eq (third nearest) 'current-y)
                (and (chunk-p-fct (third nearest))
                     (chunk-type-subtype-p-fct (chunk-chunk-type-fct (third nearest)) 'visual-location)))
        
            (let ((value nil)
                  (truth (first nearest))
                  (test (third nearest))
                  (matches nil)
                  (current-loc (aif (current-marker vis-mod) 
                                    it 
                                    (progn 
                                      (model-warning "No location has yet been attended so current is assumed to be at 0,0.")
                                      (car (define-chunks (isa visual-location screen-x 0 screen-y 0)))))))
          
          ;; find the min value
          (dolist (y matching-chunks)
            (let ((cur-val (cond ((eq test 'current)
                                  (dist (xy-loc y) (xy-loc current-loc)))
                                 ((eq test 'current-x)
                                  (abs (- (fast-chunk-slot-value-fct y 'screen-x) (fast-chunk-slot-value-fct current-loc 'screen-x))))
                                 ((eq test 'current-y)
                                  (abs (- (fast-chunk-slot-value-fct y 'screen-y) (fast-chunk-slot-value-fct current-loc 'screen-y))))
                                 (t
                                  (dist (xy-loc y) (xy-loc test))))))
              (if (or (null value) 
                      (< cur-val value))
                  (progn
                    (setf value cur-val)
                    (setf matches (list y)))
                (when (= cur-val value)
                  (push y matches)))))
          
              (setf matching-chunks matches))
          
          (progn
            (print-warning "Nearest test in a visual-location request must be current or a chunk that is a subtype of visual-location.")
            (print-warning "Ignoring nearest request for ~S." (third nearest))))
          ) 
      
      ;; undo the value slots that were changed for matching purposes
      
      (dolist (restore changed)
        (fast-set-chunk-slot-value-fct (car restore) 'value (cdr restore)))
            
      matching-chunks))))

(extend-chunks visual-tstamp)

(defun enter-into-visicon (vis-loc vis-mod)
  
  (let* ((existing (gethash (chunk-visicon-entry vis-loc) (visicon vis-mod)))
         (tstamp (if existing (chunk-visual-tstamp existing) (mp-time)))
         (new (if existing (chunk-visual-new-p existing) 'new))
         (entry (copy-chunk-fct vis-loc)))
    
    (when existing
      ;; take the old one out of the visicon
      (remhash (chunk-visicon-entry existing) (visicon vis-mod))
      
      ;; transfer any finst from the old to the new
      
      (let ((finst (find (chunk-visicon-entry existing) (finst-lst vis-mod) :key 'id :test 'equal)))
        (when finst
          (setf (id finst) (chunk-visicon-entry existing))))
      
      
      ;; catch the possibility of a constituent part of a synthed feature being replaced...
      
      (dolist (x (finst-lst vis-mod))
        (when (and (synthed-from x) (find (chunk-visicon-entry existing) (synthed-from x) :test 'equal))
          (setf (synthed-from x) (substitute (chunk-visicon-entry existing) (chunk-visicon-entry existing) (synthed-from x) :test 'equal))))
      
      ; not needed since currently-attended is now always a visicon-entry
      ;(when (eq (currently-attended vis-mod) (chunk-visicon-entry existing))
      ;  (setf (currently-attended vis-mod) vis-loc))
      
      ;; after updating the new entry delete the old chunk if
      ;; set to do so since it shouldn't be needed now
      
      (when (purge-visicon vis-mod)
        (unless (eq existing (current-marker vis-mod))
          (purge-chunk-fct existing))))
    
    (setf (gethash (chunk-visicon-entry vis-loc) (visicon vis-mod)) entry)
    
    (setf (chunk-visual-tstamp entry) tstamp)
    (setf (chunk-visual-new-p entry) new)
      
    ;; probably dont want to do this for each item, right?
    ;; it should happen in the calling function - process-display or add-item...
    ;; (update-new vis-mod)
    
    ;; for consistency - let the user hard code the location
    ;; chunks into the model if desired and this keeps things
    ;; recorded properly.
    
    ;; Why would this have been done since it's only the entry that matters?
    ;(setf (chunk-visual-tstamp vis-loc) tstamp)
  
    entry))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Supporting NEW tags



(defun tstamp-elapsed (vis-loc)
  (- (mp-time) (chunk-visual-tstamp vis-loc)))

(defun checknew (vis-loc vis-mod)
  (when (and (eq (chunk-visual-new-p vis-loc) 'NEW)
             (> (tstamp-elapsed vis-loc) (new-span vis-mod)))
    (setf (chunk-visual-new-p vis-loc) nil))
  vis-loc)

(defmethod update-new ((vis-mod vision-module))
  (mapc #'(lambda (vl) (checknew vl vis-mod)) (visicon-chunks vis-mod)))


;;;; FInsts

(defclass finst ()
  ((id :accessor id :initarg :id :initform nil) ;; the visicon key 
   (tstamp :accessor tstamp :initarg :tstamp :initform 0.0)
   (synthed-from :accessor synthed-from :initarg :synthed-from :initform nil)))

(defgeneric check-finsts (vis-mod)
  (:documentation  "Update finsts against what's on the display."))

(defmethod check-finsts ((vis-mod vision-module))
  (setf (finst-lst vis-mod)
    (delete-if #'(lambda (f) 
                   (or (and (not (synthed-from f))
                            (null (gethash (id f) (visicon vis-mod))))
                       (and (synthed-from f)
                            (not (some (lambda (x) (gethash x (visicon vis-mod))) (synthed-from f))))
                       (> (- (mp-time) (tstamp f))
                          (finst-span vis-mod))))
                   (finst-lst vis-mod))))


(defgeneric feat-attended (feat vis-mod)
  (:documentation  "Return the attended status of a visicon feature object."))

(defmethod feat-attended (loc vis-mod)
  (if (find (chunk-visicon-entry loc) (finst-lst vis-mod) :key (lambda (x) (cons (id x) (synthed-from x))) :test (lambda (item list) (member item list :test 'equal)))
      t
    (when (gethash (chunk-visicon-entry loc) (visicon vis-mod))
      (if (eq (chunk-visual-new-p loc) 'NEW)
          'NEW
        nil))))

(defun add-finst (vis-mod loc-or-obj) 
  (let* ((feature (chunk-visicon-entry loc-or-obj))
         (current (when feature (gethash feature (visicon vis-mod)))))
    (if current
        (progn
          (aif (find feature (finst-lst vis-mod) :key 'id :test 'equal)
               (setf (tstamp it) (mp-time))
               
               (push
                (make-instance 'finst :id feature :tstamp (mp-time)
                  :synthed-from (chunk-synth-feat loc-or-obj))
                (finst-lst vis-mod)))
          
          (if (chunk-synth-feat loc-or-obj)
              (dolist (x (chunk-synth-feat loc-or-obj))
                (setf (chunk-visual-new-p (gethash x (visicon vis-mod))) nil))
            (setf (chunk-visual-new-p (gethash feature (visicon vis-mod))) nil))
          
          (when (> (length (finst-lst vis-mod)) (num-finst vis-mod))
            (sort-finsts vis-mod)
            (pop (finst-lst vis-mod))))
      (model-warning "~S does not name an object or feature in the current visicon. No finst created." loc-or-obj))))


(defun remove-finst (vis-mod loc-or-obj) 
  (let ((name (chunk-visicon-entry loc-or-obj)))
    
    (setf (finst-lst vis-mod) (remove name (finst-lst vis-mod) :key 'id :test 'equal))
    (setf (finst-lst vis-mod) (remove name (finst-lst vis-mod) :key 'synthed-from :test 'equal))))


(defgeneric assign-finst (vis-mod &key object location)
  (:documentation "Assign a finst to an object or location."))

(defmethod assign-finst ((vm vision-module) &key object location)
  (if (and (null object) (null location))
    (print-warning "ASSIGN-FINST called with two null arguments")
    (add-finst vm (if object object location))))
  
  
  
(defgeneric sort-finsts (vis-mod)
  (:documentation  "Sort finsts according to time stamp."))

(defmethod sort-finsts ((vis-mod vision-module))
  (setf (finst-lst vis-mod) (sort (finst-lst vis-mod) #'< :key #'tstamp)))



;;; OBJECT-PRESENT-P      [Method]
;;; Description : A visual object is present if its ID is still in the icon
;;;             : or if all the features from which it was synthesized are
;;;             : still in the icon.

(defgeneric object-present-p (vis-mod obj-id)
  (:documentation  "Returns NIL if the object ID passed to it is no longer in the icon."))

(defmethod object-present-p ((vis-mod vision-module) obj-id)
  (if (chunk-synth-feat obj-id)
      
      ;; For an object synthesized from features check all the features
      
      (every (lambda (x) (gethash (chunk-visicon-entry x) (visicon vis-mod))) (chunk-synth-feat obj-id))
    
    (gethash (chunk-visicon-entry obj-id) (visicon vis-mod))))


;;; WITHIN-MOVE      [Method]
;;; Date        : 99.03.29
;;; Description : Simply walk the icon and accumulate features that are within
;;;             : the movement tolerance.

(defgeneric within-move (vis-mod xy-loc)
  (:documentation "Return a list of icon feature within the move allowance of loc."))

(defmethod within-move ((vis-mod vision-module) xy-loc)
  (if (= (move-allowance vis-mod) 0)
    (feat-match-xy (visicon-chunks vis-mod) xy-loc)
    
    (let ((max (pm-angle-to-pixels (move-allowance vis-mod)))
          (accum nil))
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (when (>= max (dist (xy-loc value) xy-loc))
                   (push value accum)))
               (visicon vis-mod))
      accum)))

(defmethod feat-match-xy (feat-list xy-loc)
  (let ((outlis nil)
        (x (px xy-loc))
        (y (py xy-loc)))
    (dolist (chunk feat-list)
      (when (and (= x (fast-chunk-slot-value-fct chunk 'screen-x)) 
                 (= y (fast-chunk-slot-value-fct chunk 'screen-y)))
        (push chunk outlis)))
    outlis))


;;; ENCODING-COMPLETE      [Method]
;;; Description : Several things to do when focusing attention on a location.
;;;             : [1] Make the location attended, set state to FREE.

;;;             : [3] If there was nothing there, or we were non-conserving
;;;             :     get the thing at that location.
;;;             : [4] If there is something, synch it up with the location
;;;             :     chunk.
;;;             : [5] If requested, print it.

(defgeneric encoding-complete (vis-mod loc-dmo scale &key requested)
  (:documentation "When MOVE-ATTENTION completes, focus on a place with this."))

(defmethod encoding-complete ((vis-mod vision-module) loc scale &key (requested t))
  
  (setf (moving-attention vis-mod) nil)
  (change-state vis-mod :exec 'free :proc 'free)
  (setf (current-marker vis-mod) loc)
  
  (let ((return-obj (get-obj-at-location vis-mod loc scale)))
    
    (if return-obj
        (progn
          (set-attended vis-mod (chunk-visicon-entry return-obj))
          (attend-to-object vis-mod return-obj :requested requested)
          
          return-obj)
      
      (progn
        (clear-attended vis-mod)
        (setf (last-obj vis-mod) nil)
        (setf (attend-failure vis-mod) t)
      
        (schedule-event-relative 0 'no-visual-object-found :maintenance t :module :vision :output 'medium :details "No visual-object found")
      
        nil))))


(defun icon-entry (vis-mod visual-object)
  "Given a visual object chunk return the visicon key of the location which it came from
   or the one that's in the visicon that best matches it now, or if there isn't one 
   which matches the 'original' then just return the one associated with the object now"
  (let* ((loc-name (fast-chunk-slot-value-fct visual-object 'screen-pos))
         (original (chunk-visicon-entry loc-name)))
    
    (cond ((gethash original (visicon vis-mod))
           original)
          ((chunk-p-fct original)
           
           (aif (car (find-matching-chunks (chunk-name-to-chunk-spec original) :chunks (visicon-chunks vis-mod)))
                (chunk-visicon-entry it)
                (chunk-visicon-entry visual-object)))
          (t
           (aif (car (find-matching-chunks (chunk-name-to-chunk-spec loc-name) :chunks (visicon-chunks vis-mod)))
                (chunk-visicon-entry it)
                (chunk-visicon-entry visual-object))))))

(defun no-visual-object-found ()
  "Dummy function to indicate failure to encode - someone may want to do something with this later"
  )


(defmethod attend-to-object ((vis-mod vision-module) obj &key (requested t))
  
  ;;; put the chunk in the buffer
  
  (schedule-set-buffer-chunk 'visual obj 0 :module :vision :priority 10 :requested requested)
  
  ;; record the object for tracking purposes
  
  (setf (last-obj vis-mod) obj)
  
  ;; update the time-stamp on the finst if it's already attended or
  ;; add a new finst if it's not
  
  (aif (member (icon-entry vis-mod obj) (finst-lst vis-mod) :key #'id :test 'equal)
       
       (setf (tstamp (first it)) (mp-time))
       
       (add-finst vis-mod obj)))


(defgeneric get-obj-at-location (vis-mod loc scale)
  (:documentation  "Given a location and a scale, return a chunk representing what's there."))

(defmethod get-obj-at-location ((vis-mod vision-module) loc scale)
  (let ((xy-loc (xy-loc loc)))
    
    (cond ((eq scale 'PHRASE)
           (get-phrase-at vis-mod loc))
          ((and (eq scale 'WORD) (not (optimize-p vis-mod)))
           (get-word-at-noopt vis-mod loc))
          (t
           (let ((feat-lis (within-move vis-mod xy-loc)))
             (when (eq scale 'WORD)
               (setf feat-lis (text-feats feat-lis)))
             (when feat-lis
               (featlis-to-focus vis-mod loc feat-lis)))))))

(defun text-feats (feat-lst)
  "Given a list, return only those features which are TEXT features."
  (remove-if (lambda (f) (not (eq (chunk-slot-value-fct f 'kind) 'text))) feat-lst))


;;; FEATLIS-TO-FOCUS      [Method]

(defgeneric featlis-to-focus (vis-mod loc-dmo feat-lst)
  (:documentation  "Given the source location and a list of features, return the DMO that should be the focus."))

(defmethod featlis-to-focus ((vis-mod vision-module) loc feat-lis)
  (let* ((best-feature 
          (find-best-feature vis-mod feat-lis 
                             (aif (chunk-p-fct (gethash (chunk-visicon-entry loc) (visicon vis-mod))) 
                                 (gethash (chunk-visicon-entry loc) (visicon vis-mod))
                               loc)))
         
         (dmo-lis (featlis-to-chunks vis-mod (xy-loc best-feature)
                                     (feat-match-xy feat-lis (xy-loc best-feature))))
         (return-chunk (determine-focus-dmo vis-mod dmo-lis best-feature)))
    
    ;; don't mark everything just the one that's being returned   (dolist (obj dmo-lis)
    
    (when return-chunk
      (set-chunk-slot-value-fct return-chunk 'screen-pos loc) ;;;  Should it use the cannonical loc instead? (gethash best-feature (visicon vis-mod)) 
      )
    
    return-chunk))

;;; DETERMINE-FOCUS-DMO      [Method]
;;; Date        : 99.03.29
;;; Description : Basically, look for a DMO with the same ID as the feature.
;;;             : If none, see if the DMO was synthesized from that feature.
;;;             : If none of those, return a random one.

(defgeneric determine-focus-dmo (vis-mod dmo-lst feat)
  (:documentation  "Determine which DMO corresponds to <feat>, which should be the 'best' feature."))

(defmethod determine-focus-dmo ((vis-mod vision-module) (dmo-lis list) feat )
  (when (= 1 (length dmo-lis))
    (return-from determine-focus-dmo (first dmo-lis)))
  (aif (member (chunk-visicon-entry feat) dmo-lis :key #'chunk-visicon-entry :test 'equal)
       (first it)
       (print-warning "Multiple matching chunks found for attention shift.  This should not happen.  Please report the warning to Dan.")
     ;(dolist (dmo dmo-lis (random-item dmo-lis))
     ; (when (member feat 
     ;               (synthed-to-features vis-mod dmo nil))
     ;   (return-from determine-focus-dmo dmo)))))
       ))

(defun merge-visual-object (first second)
  (declare (ignore first))
  (chunk-visual-object second))

(extend-chunks visual-object :copy-function identity :merge-function merge-visual-object)

;;; FEATLIS-TO-chunks      [Method]
;;; Date        : 99.03.29
;;; Description : Actually, some of the features could be CHAR-PRIMITIVE 
;;;             : features, in which case they're part of characters.  Save 
;;;             : those out and make a character out of 'em.

(defgeneric featlis-to-chunks (vis-mod loc feat-lis)
  (:documentation  "Given a list of features, make a chunk for each."))

(defmethod featlis-to-chunks ((vis-mod vision-module) (loc vector) (feat-lis list))
  (let ((primitive-feats nil)
        (chunk-lis nil))
    (dolist (feat feat-lis)
      
      ;; If it's a char primitive, push the feature, else push the feature chunk.
      (if (chunk-type-subtype-p-fct (chunk-chunk-type-fct feat) 'char-primitive)
        (push feat primitive-feats)
        (let ((obj (vis-loc-to-obj (aif (chunk-visual-object feat)
                                        it
                                        (current-device))
                                   (chunk-visual-feature-name feat))))
          (if (and (chunk-p-fct obj)
                   (chunk-type-subtype-p-fct (chunk-chunk-type-fct obj) 'visual-object))
              (progn
                (setf (chunk-visicon-entry obj) (chunk-visicon-entry feat))
                ;; (set-chunk-slot-value-fct obj 'screen-pos feat)
                (push obj chunk-lis))
            (print-warning "vis-loc-to-obj returned ~S which is not a chunk that is a subtype of visual-object" obj))))
      )
    (when primitive-feats
      (push (synthesize-letter vis-mod primitive-feats) chunk-lis))
    chunk-lis))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Synthesizing 


;;; GET-WORD-AT-NOOPT      [Method]
;;; Date        : 99.04.02
;;; Description : Getting a word when optimizing is off involves calling
;;;             : SYNTHESIZE-WORD method, but we need to collect the 
;;;             : locations first, and also check the return if what was
;;;             : sent in was a DMO.

(defgeneric get-word-at-noopt (vis-mod loc-dmo)
  (:documentation  "Synthesize a word at the given location and synch it with the location."))

(defmethod get-word-at-noopt ((vis-mod vision-module) (loc symbol))
  (let ((xy-loc (xy-loc loc)))
    (multiple-value-bind (locs xmin xmax) (adjoining-led-locs vis-mod xy-loc)
      (when locs
        (let ((rtn-chunk (synthesize-word vis-mod locs (- xmax xmin) xy-loc)))
          (when rtn-chunk
            (set-chunk-slot-value-fct rtn-chunk 'screen-pos loc)
            (values rtn-chunk xmin xmax)))))))


(defmethod get-word-at-noopt ((vis-mod vision-module) (loc vector))
  (multiple-value-bind (locs xmin xmax) (adjoining-led-locs vis-mod loc)
    (when locs
      (let ((rtn-chunk (synthesize-word vis-mod locs  (- xmax xmin) loc)))
        (when rtn-chunk
          (set-chunk-slot-value-fct rtn-chunk 'screen-pos loc)
          (values rtn-chunk xmin xmax))))))


;;; GET-WORD-DMOS-NOOPT      [Method]
;;; Date        : 99.04.02
;;; Description : OK, when optimizing is off and a phrase needs to be built,
;;;             : the tricky bit is figuring out which locations you need
;;;             : to grab words from, since if you hit every x location, 
;;;             : you'll generate multiple copies of each word.

(defgeneric get-word-dmos-noopt (vis-mod x-lst y)
  (:documentation  "Return a list of DMOs representing words at the given xlocs, with optimizing off."))

(defmethod get-word-dmos-noopt ((vis-mod vision-module) (x-ls list) y)
  (let ((rtn-dmos nil)
        (curr-x -1))
    (dolist (x x-ls (remove nil (nreverse rtn-dmos)))
      (when (> x curr-x)
        (multiple-value-bind (word min max)
                             (get-word-at-noopt vis-mod (vector x y))
          (declare (ignore min))
          (push word rtn-dmos)
          (setf curr-x max))))))


;;; GET-WORD-DMOS-OPT      [Method]
;;; Date        : 99.04.02
;;; Description : This is simpler when optimizing--just walk the xlist
;;;             : and accumulate words.
;;;             : Might be vestigial as of beta 6.

(defgeneric get-word-dmos-opt (vis-mod x-lst y)
  (:documentation  "Return a list of DMOs representing words at the given xlocs, with optimizing on."))

(defmethod get-word-dmos-opt ((vis-mod vision-module) (x-ls list) y)
  (let (accum)
    (dolist (x x-ls (nreverse accum))
      (dolist (feat (text-feats (feat-match-xy (visicon-chunks vis-mod) (vector x y))))
        (setf accum (append (featlis-to-chunks vis-mod (vector x y) (list feat)) accum))))))



;;; SYNTHESIZE-LETTER      [Method]
;;; Date        : 98.07.27
;;; Description : From a list of LED-style icon features, synthesize a letter
;;;             : feature and note the synthesis.  The real worker here is
;;;             : Mike Matessa's FIND-BEST-OBJECT function, which is based on
;;;             : "rational categorization" of the features into a letter.

(defgeneric synthesize-letter (vis-mod feats)
  (:documentation  "Build a DMO representing a letter from a list of LED features."))

(defmethod synthesize-letter ((vis-mod vision-module) (feats list))
  (let* ((base-feat (first feats))
         (letter (prob-best-character (active-cfs vis-mod) (mapcar (lambda (x) (chunk-slot-value-fct x 'value)) feats)))
         (return-chunk nil)
         (colors (mapcar (lambda (x) (chunk-slot-value-fct x 'color)) feats))
         (color (caar (sort (mapcar (lambda (x) (cons x (count x colors))) (remove-duplicates colors)) #'> :key 'cdr))))
    
    (setf return-chunk
      (car (define-chunks-fct (list (list 'isa 'visual-object
                                     'value letter
                                     'color color)))))
    
    
    (setf (chunk-visual-feature-name return-chunk) base-feat)
    (setf (chunk-visicon-entry return-chunk) (chunk-visicon-entry base-feat))
    (setf (chunk-synth-feat return-chunk) (mapcan (lambda (x) (list (chunk-visicon-entry x))) feats))
    return-chunk))


;;; SYNTHESIZE-WORD      [Method]
;;; Date        : 98.07.27
;;; Description : Given the list of contiguous locations, get the letter 
;;;             : at each location, and then build the word from the list
;;;             : of letters.

(defgeneric synthesize-word (vis-mod loc-lis width center)
  (:documentation  "Build a DMO representing a word from a location."))

(defmethod synthesize-word ((vis-mod vision-module) (loc-lis list) 
                               (width number) (center vector))
  (let ((return-chunk nil)
        (letter-chunks nil)
        (used-feats nil))
        
    (dolist (xloc loc-lis)
      (let* ((feats (feat-match-xy (visicon-chunks vis-mod) xloc))
             (letter-chunk (synthesize-letter vis-mod feats)))
        
        (when (stringp (chunk-slot-value-fct letter-chunk 'value))
          (push letter-chunk letter-chunks)
          (setf used-feats (append feats used-feats)))))
    
    (when letter-chunks
      (let* ((colors (mapcar (lambda (x) (chunk-slot-value-fct x 'color)) letter-chunks))
             (color (caar (sort (mapcar (lambda (x) (cons x (count x colors))) (remove-duplicates colors)) #'> :key 'cdr))))
        
        (setf letter-chunks (nreverse letter-chunks))
        (setf return-chunk
          (car (define-chunks-fct `((isa text color ,color value ,(word-accum 
                                                                   (mapcar #'(lambda (x) (chunk-slot-value-fct x 'value))
                                                                     letter-chunks)))))))
        
        
        (setf (chunk-visicon-entry return-chunk) (chunk-visicon-entry (car used-feats)))
        (setf (chunk-visual-feature-name return-chunk) (car used-feats))
        (setf (chunk-synth-feat return-chunk) (mapcan (lambda (x) (list (chunk-visicon-entry x))) used-feats))
        
        return-chunk))))


(defun word-accum (lis)
  "Accumulate a list of letters into a word, and downcase it."
  (let ((accum (first lis)))
    (dolist (item (rest lis) (string-downcase accum))
      (setf accum (mkstr accum item)))))

;;; GET-PHRASE-AT      [Method]
;;; Date        : 98.07.27
;;; Description : The only way to get a phrase is to synthesize one, there is
;;;             : no "phrase" primitive in RPM.


(defmethod get-phrase-at ((vis-mod vision-module) (loc symbol))
  (awhen (find-matching-chunks (define-chunk-spec-fct `(isa visual-location screen-y ,(py (xy-loc loc))))
                               :chunks (visicon-chunks vis-mod))
         
         (synthesize-phrase vis-mod it loc)))


(defgeneric synthesize-phrase (vis-mod feature-locs loc)
  (:documentation  "Build a DMO representing a phrase."))

(defmethod synthesize-phrase ((vis-mod vision-module) (feature-locs list) (loc symbol))
  (let ((xy-loc (xy-loc loc))
        (x-locs (mapcar (lambda (x) (chunk-slot-value-fct x 'screen-x)) feature-locs))
        (word-chunks nil) 
        (words nil)
        (colors nil)
        (return-chunk nil))
    (setf x-locs (sort (remove-duplicates x-locs) #'<))
    
    
    (if (optimize-p vis-mod)
      (setf word-chunks (get-word-dmos-opt vis-mod x-locs (py xy-loc)))
      (setf word-chunks (get-word-dmos-noopt vis-mod x-locs (py xy-loc))))
    
    (when word-chunks
      (setf words (mapcar #'(lambda (x)
                              (chunk-slot-value-fct x 'value)) 
                    word-chunks))
      
      (dolist (w word-chunks)
        (let ((c (chunk-slot-value-fct w 'color)))
          
          (aif (assoc c colors) 
               (incf (cdr it))
               (push-last (cons c 1) colors))))
           
      (setf return-chunk
        (car (define-chunks-fct 
                 `((isa phrase! value ,(phrase-accum words)
                        objects ,word-chunks 
                        colors ,(mapcar #'(lambda (x) (chunk-slot-value-fct x 'color)) word-chunks)
                        words ,words
                        color ,(car (rassoc (apply #'max (mapcar #'cdr colors)) colors))
                        screen-pos ,loc)))))
      
      
      (setf (chunk-visicon-entry return-chunk) (chunk-visicon-entry loc))
      (setf (chunk-visual-feature-name return-chunk) loc) 
      
      
      
      (setf (chunk-synth-feat return-chunk) 
        (if (optimize-p vis-mod)
            (mapcan (lambda (x) (list (chunk-visicon-entry x))) word-chunks)
          (mapcan (lambda (x) (chunk-synth-feat x)) word-chunks)))
      
      
      
      return-chunk)))

(defun phrase-accum (lis)
  "Accumulate a list of words into a phrase."
  (let ((accum (first lis)))
    (dolist (item (rest lis) accum)
      (setf accum (mkstr accum " " item)))))

;;;;;;;;;;

;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Those wacky LED features for letters.
;;;; ---------------------------------------------------------------------- ;;;;


;;; ADJOINING-LED-LOCS      [Method]
;;; Date        : 99.04.02
;;; Description : To synthesize a word, we need to know which locations have
;;;             : LED-LINE features at adjoining locations.  However, 
;;;             : because letter width is variable, the icon has to be
;;;             : searched for LED-LINE locations that have features that
;;;             : are near the boundaries.  Also, because we need to know the
;;;             : width of the word, return the min and max x values, too.

(defgeneric adjoining-led-locs (vis-mod loc)
  (:documentation  "Return a list of locations adjoining <loc>, as well as the max and min x locs."))

(defmethod adjoining-led-locs ((vis-mod vision-module) (loc vector))
  (let ((feat-ls (feat-match-xy (visicon-chunks vis-mod) loc))
        (feat nil))
    (while (and (null feat) feat-ls)
      (setf feat (pop feat-ls))
      (unless (chunk-type-subtype-p-fct (chunk-chunk-type-fct feat) 'char-primitive)
        (setf feat nil)))
    (when feat
      (setf feat-ls (find-matching-chunks (define-chunk-spec-fct `(isa visual-location screen-y ,(chunk-slot-value-fct feat 'screen-y)))
                                          :chunks (visicon-chunks vis-mod)))
      (multiple-value-bind 
        (lowlocs xmin) (left-adjoining-led-locs feat-ls (chunk-slot-value-fct feat 'left) nil)
        (multiple-value-bind
          (hilocs xmax) (right-adjoining-led-locs feat-ls (chunk-slot-value-fct feat 'right) nil)
          (values
           (append lowlocs (list loc) hilocs)
           xmin xmax))))))


;;; RIGHT-ADJOINING-LED-LOCS      [Method]
;;; Date        : 99.04.02
;;; Description : Recursively go right and accumulate locations that share
;;;             : the boundary.

(defgeneric right-adjoining-led-locs (feat-ls x accum)
  (:documentation  "Return a list of all the right-adjoining locs with led features and the min x."))

(defmethod right-adjoining-led-locs ((feat-ls list) x accum)
  (dolist (feat feat-ls)
    (when (chunk-type-subtype-p-fct (chunk-chunk-type-fct feat) 'char-primitive)
      (when (> 1.5 (abs (- (chunk-slot-value-fct feat 'left) x)))
        (return-from right-adjoining-led-locs
          (right-adjoining-led-locs feat-ls (chunk-slot-value-fct feat 'right)
                                    (append accum (list (xy-loc feat))))))))
  (values accum x))


;;; LEFT-ADJOINING-LED-LOCS      [Method]
;;; Date        : 99.04.02
;;; Description : Recursively go left and accumulate locations that share
;;;             : the boundary.

(defgeneric left-adjoining-led-locs (feat-ls x accum)
  (:documentation  "Return a list of all the left-adjoining locs with led features and the max x."))

(defmethod left-adjoining-led-locs ((feat-ls list) x accum)
  (dolist (feat feat-ls)
    (when (chunk-type-subtype-p-fct (chunk-chunk-type-fct feat) 'char-primitive)
      (when (> 1.5 (abs (- (chunk-slot-value-fct feat 'right) x)))
        (return-from left-adjoining-led-locs
          (left-adjoining-led-locs feat-ls (chunk-slot-value-fct feat 'left)
                                   (append (list (xy-loc feat)) accum))))))
  (values accum x))




;;; FIND-BEST-FEATURE      [Method]
;;; Date        : 99.03.29
;;; Description : The "best" feature is the one with the same ID as the spec. 
;;;             : The next best feature is one that matches the spec and is
;;;             : nearest to that location.  Failing any matches to the spec,
;;;             : then return the nearest feature.

(defgeneric find-best-feature (vis-mod feat-lst fs)
  (:documentation  "Given a list of features and a visicon entry or visual-loc, return the 'best' feature."))

(defmethod find-best-feature (vis-mod (feat-lis list) entry)
  (awhen (member entry feat-lis)
    (return-from find-best-feature (first it)))
  (let ((matches (awhen (gethash entry (found-locs vis-mod)) 
                        (find-matching-chunks it :chunks feat-lis :variable-char #\$))))
    (if matches
      (random-item (nearest-feat vis-mod matches (xy-loc entry)))
      (random-item (nearest-feat vis-mod feat-lis (xy-loc entry))))))


(defun nearest-feat (vis-mod feat-list xy-loc)
  "Returns list of features nearest to a given location"
  (unless (vectorp xy-loc)
    (setf xy-loc (xy-loc xy-loc)))
  (when feat-list
    (let* ((feat-chunks (mapcan (lambda (x) (aif (gethash (chunk-visicon-entry x) (visicon vis-mod)) (list it) nil)) feat-list))
           (best (if feat-chunks (dist (xy-loc (first feat-chunks)) xy-loc) 999))
           (outlis (list (first feat-chunks)))
           (the-dist nil))
      (dolist (feat (rest feat-chunks) outlis)
        (setf the-dist (dist (xy-loc feat) xy-loc))
        (cond ((= the-dist best) (push feat outlis))
              ((< the-dist best) (setf best the-dist)
               (setf outlis (list feat))))))))




;;; UPDATE-TRACKING-MTH      [Method]
;;; Date        : 97.05.15
;;; Description : Updating is kind of a pain.  First, if the tracked object
;;;             : hasn't moved, then do nothing.  If it has moved, however,
;;;             : there's a lot of bookkeeping to be done:
;;;             : [1] The old location need to have its object stripped and
;;;             : removed as an activation source.
;;;             : [2] The new location needs to be created, added to the 
;;;             : attention focus, and have its OBJECTS slot set.
;;;             : [3] The object chunk needs to have its location changed.


#|

Proposed new ACT-R 6 mechanism to properly deal with buffer issues:

Then, whenever there's a change to the display the buffers
will be updated as follows:

   First, if the build-features-for returns nil then assume
   that the tracked object is out of sight now.  As with an
   encoding failure, clear visual and set the error state also
   stop the tracking.


   If both buffers are empty:

   A new location is created and the object is modified
   to have that as its screen-pos.  Both chunks (the
   new loc. and the object) are stuffed into the buffers.

  If vis-loc empty, visual holds the tracked item:

   Create a new location chunk and stuff it into
   the visual-location buffer.  Modify the chunk in the
   visual buffer to have that screen-pos.

  vis-loc holds tracked item's loc. and visual is empty:

   Modify the chunk in the visual-location buffer with
   the new info and put the object back into the visual
   buffer (it shouldn't have to be modified at all since
   its loc. is in the vis-loc buffer.

  both buffers hold the appropriate chunks:

   Modify the chunk in the visual-location buffer,
   no other changes necessary.

  If either buffer holds a chunk that isn't related
  to the tracked item:

   Make the appropriate updates to the underlying chunks
   as needed (modify the chunk in the buffer if it's
   the tracked one or create/modify the internal one)
   but don't overwrite a non-tracked chunk in the
   buffer.
|#

;;; START-TRACKING      [Method]
;;; Date        : 97.05.15
;;; Description : Starting tracking is pretty easy, actually.  The target
;;;             : object needs to be found and installed in the relevant
;;;             : slots, and the ACT hook functions need to be set.

(defgeneric start-tracking (vis-mod)
  (:documentation  "Begin tracking currently attended object"))

(defmethod start-tracking ((vis-mod vision-module))
   (if (null (currently-attended vis-mod))
      (print-warning "Request to track object but nothing is currently being attended.")
    (progn
      (change-state vis-mod :exec 'BUSY)
      (setf (tracked-obj vis-mod) (chunk-visual-object (gethash (currently-attended vis-mod) (visicon vis-mod))))
      (setf (tracked-obj-lastloc vis-mod) nil)  ;; don't assume anything about vis-loc buffer at this point
      (let ((vis-obj (buffer-read 'visual))) ;; should always be empty since the request clears it but
                                             ;; if not record it for later checking
        
        (if (and vis-obj (eq (chunk-copied-from-fct vis-obj) (last-obj vis-mod)))
            
            (setf (tracked-obj-last-obj vis-mod) vis-obj)
          (setf (tracked-obj-last-obj vis-mod) nil)))
      (setf (tracked-obj-last-feat vis-mod) (currently-attended vis-mod))
      (update-tracking-mth vis-mod)
      
      (tracked-obj vis-mod)))
  )


;;; When tracking it may be necessary to get the name of
;;; the chunk from the vis-loc buffer to set the internal
;;; information correctly.  This method does that, but
;;; it must be scheduled approproately to ensure the right
;;; chunk gets recorded.  
;;; It could be possible for something else to intrude and
;;; mess this up, but the default mechanisms shouldn't 
;;; result in problems.

(defmethod update-tracking-loc-chunk ((vis-mod vision-module) &optional (modify nil))
  
  (let* ((vis-loc (buffer-read 'visual-location)))
    
    (setf (tracked-obj-lastloc vis-mod) vis-loc)
    (setf (current-marker vis-mod) vis-loc)
    
    (when modify 
      (mod-buffer-chunk 'visual (list 'screen-pos vis-loc)))))

;;; Record the visual object chunk placed into the buffer
;;; for later comparisons when needed

(defmethod update-tracking-obj-chunk ((vis-mod vision-module))
  
  (let* ((vis-obj (buffer-read 'visual)))
    
    (setf (tracked-obj-last-obj vis-mod) vis-obj)))


(defgeneric update-tracking-mth (vis-mod &optional from-proc-display)
  (:documentation  "Update the state of a tracked object"))

(defmethod update-tracking-mth ((vis-mod vision-module) &optional from-proc-display)
  
  (let ((devin (current-device-interface)))
    
    ;; Don't change anything now if there's a lock on the
    ;; device.
    
    (unless (zerop (locks devin))
      (push :tracking (pending-procs devin))
      (return-from update-tracking-mth nil))
    
    (let* ((new-feat ;; the feature's visicon entry -- chunk name or feature list            
            (if from-proc-display
                
                ;; The visicon contains the updated info
                ;; just get the appropriate one out
                
                (if (tracked-obj vis-mod)
                    ;; find the one with the same object
                    (let ((f (find (tracked-obj vis-mod) (visicon-chunks vis-mod) :key 'chunk-visual-object)))
                      (when f
                        (chunk-visicon-entry f)))
                  ;; If it's not object based it must have the same visual-location
                  ;; chunk of the feature originally tracked so make sure that's still
                  ;; a feature in the visicon
                  (when (gethash (tracked-obj-last-feat vis-mod) (visicon vis-mod))
                    (tracked-obj-last-feat vis-mod)))
              
              ;; proc-display wasn't called which only happens when the
              ;; tracking starts now so just use the feature that was
              ;; valid at that point.
              
              (tracked-obj-last-feat vis-mod)))
           (new-loc (when new-feat (gethash new-feat (visicon vis-mod)))) ;; name of the chunk
           (old-loc (tracked-obj-lastloc vis-mod))
           (old-obj (tracked-obj-last-obj vis-mod))
           (vis-loc-chunk (buffer-read 'visual-location))
           (vis-obj-chunk (buffer-read 'visual)))
      
      (unless new-loc
        (schedule-clear-buffer 'visual 0 :module :vision :output 'high :priority 13)
        
        (change-state vis-mod :exec 'free :proc 'free)
        (clear-attended vis-mod)
        (setf (current-marker vis-mod) nil)
        (setf (attend-failure vis-mod) t)
        (setf (tracked-obj vis-mod) nil)
        (setf (tracked-obj-last-feat vis-mod) nil)
        (setf (last-obj vis-mod) nil)
        (return-from update-tracking-mth nil))
      
      (setf (tracked-obj-last-feat vis-mod) new-feat)
      (setf (current-marker vis-mod) new-loc)
      
      
      (let ((new-obj (vis-loc-to-obj (aif (chunk-visual-object new-loc)
                                          it
                                          (current-device))
                                     (chunk-visual-feature-name new-loc))))
        
        (unless new-obj ;; for some reason we have a location but no object
          
          (schedule-clear-buffer 'visual 0 :module :vision :output 'high :priority 13)
          
          (change-state vis-mod :exec 'free :proc 'free)
          (clear-attended vis-mod)
          (setf (current-marker vis-mod) nil)
          (setf (attend-failure vis-mod) t)
          (setf (tracked-obj vis-mod) nil)
          (setf (tracked-obj-last-feat vis-mod) nil)
          (setf (last-obj vis-mod) nil)
          (return-from update-tracking-mth nil))
        
        
        (setf (currently-attended vis-mod) new-feat)
        (setf (last-obj vis-mod) new-obj)        
        
        
        ;; always one layer of separation between the 
        ;; user's feature chunk and what gets into
        ;; the buffer for saftey reasons.
        ;; 
        ;; Not actually necessary now since new-loc is safe, but it's easier
        ;; to leave the let than to search and replace...
        
        (let ((new-buffer-loc new-loc))
                    
          ;; Don't set this here now.  Only set it for the cases where there
          ;; isn't an update to the visual-location later which will overwrite
          ;; it.  That avoids an issue with deleting new-buffer-loc later
          ;; anyway for most cases.
          ;; The remaining question is is it better to have a reference to
          ;; a chunk which will go away or no chunk at all in the "bad" cases?
          
         ; (unless (chunk-slot-value-fct new-obj 'screen-pos)
         ;   (set-chunk-slot-value-fct new-obj 'screen-pos new-buffer-loc))
       
          
          ;;; Make sure there's still a finst on the tracked item
          
          
          (aif (member new-feat (finst-lst vis-mod) :key #'id :test 'equal)
               (setf (tstamp (first it)) (mp-time))
               (add-finst vis-mod new-loc))
          
          
          ;(format t "New-loc: ~S~%New-buffer-loc: ~S~%New-obj: ~S~%Old-loc: ~S~%Old-obj: ~S~%vis-loc-chunk: ~S~%vis-obj-chunk: ~S~%"
          ;  new-loc new-buffer-loc new-obj old-loc old-obj vis-loc-chunk vis-obj-chunk)
          
          
          (cond ((and (null vis-loc-chunk)
                      (null vis-obj-chunk))
                 
                 ;(pprint 'case-1)
                 
                 #|
                 Stuff both buffers and then update the obj with the 
                 buffer-chunk's name
                 |#
                 
                 (schedule-set-buffer-chunk 'visual-location new-buffer-loc 0
                                            :module :vision :output 'high :requested nil
                                            :priority 15)
                 
                 ;; need to make sure the chunk
                 ;; being set in the buffer isn't deleted before 
                 ;; it gets there
                 
                 (when from-proc-display
                   (lock-device (current-device-interface))
                   (schedule-event-relative 0 'unlock-device 
                                            :module :vision
                                            :destination :device
                                            :priority 14
                                            :output nil
                                            :maintenance t))
                 
                 (schedule-set-buffer-chunk 'visual new-obj 0
                                            :module :vision :output 'high :requested nil
                                            :priority 14)
                 (schedule-event-relative 0 #'update-tracking-loc-chunk
                                          :module :vision
                                          :destination :vision
                                          :params (list t)
                                          :priority 13
                                          :output nil)
                 (schedule-event-relative 0 #'update-tracking-obj-chunk
                                          :module :vision
                                          :destination :vision
                                          :params nil
                                          :priority 12
                                          :output nil)
                 )

                ((and (null vis-loc-chunk)
                      (eq vis-obj-chunk old-obj))
                 
                ;(pprint 'case-2)

                 #|
                 stuff a new location chunk  into
                 the visual-location buffer.  Modify the chunk in the
                 visual buffer with all the new info. and make sure
                 to sync. with the loc in the buffer.

                 Note - need to set priority of the buffer stuffing
                 so that if there's a find-location scheduled
                 but not completed this happens first, so that
                 the find-loc overwrites.  SO, the priority of this
                 needs to be set to > 10.
                 |#
                 
                 
                 (schedule-set-buffer-chunk 'visual-location new-buffer-loc 0
                                            :module :vision :output 'high :requested nil
                                            :priority 15)
                 (when from-proc-display
                   (lock-device (current-device-interface))
                   (schedule-event-relative 0 'unlock-device 
                                            :module :vision
                                            :destination :device
                                            :priority 14
                                            :output nil
                                            :maintenance t))
                 
                 (schedule-mod-buffer-chunk 'visual 
                                            ;; This is an ugly way to do things, but
                                            ;; it works for now...
                                            (cddr (chunk-spec-to-chunk-def 
                                                   (chunk-name-to-chunk-spec new-obj))) 
                                            0  
                                            :module :vision 
                                            :output 'high 
                                            :priority 14)
                 
                 (schedule-event-relative 0 #'update-tracking-loc-chunk
                                          :module :vision
                                          :destination :vision
                                          :params (list t)
                                          :priority 13
                                          :output nil)
                 
                 )

                ((null vis-loc-chunk) ;; The visual chunk is unknown
                 
                 ;(pprint 'case-3)

                 #|
                 stuff a new location chunk  into
                 the visual-location buffer.  
                 Don't touch the chunk in the visual buffer
                 |#
                 
                 
                 (schedule-set-buffer-chunk 'visual-location new-buffer-loc 0
                                            :module :vision :output 'high :requested nil
                                            :priority 15)
                                  
                 (when from-proc-display
                   (lock-device (current-device-interface))
                   (schedule-event-relative 0 'unlock-device 
                                            :module :vision
                                            :destination :device
                                            :priority 14
                                            :output nil
                                            :maintenance t))
                 
                 (schedule-event-relative 0 #'update-tracking-loc-chunk
                                          :module :vision
                                          :destination :vision
                                          :params nil
                                          :priority 13
                                          :output nil)
                 
                 )
      
                ((and (eq vis-loc-chunk old-loc)
                      (null vis-obj-chunk))
                                  
                ;(pprint 'case-4)

                #|
                Modify the chunk in the visual-location buffer
                
                put new obj into visual buffer
                update it with current location
                |#
                 
                 
                 (schedule-mod-buffer-chunk 'visual-location 
                                            ;; This is an ugly way to do things, but
                                            ;; it works for now...
                                            (cddr (chunk-spec-to-chunk-def 
                                                   (chunk-name-to-chunk-spec new-buffer-loc))) 
                                            0  
                                            :module :vision 
                                            :output 'high 
                                            :priority 15)
                 
                 (schedule-set-buffer-chunk 'visual new-obj 0
                                            :module :vision :output 'high :requested nil
                                            :priority 14)
                 (schedule-event-relative 0 #'update-tracking-loc-chunk
                                          :module :vision
                                          :destination :vision
                                          :params (list t)
                                          :priority 13
                                          :output nil)
                 (schedule-event-relative 0 #'update-tracking-obj-chunk
                                          :module :vision
                                          :destination :vision
                                          :params nil
                                          :priority 12
                                          :output nil)
                 )
                
                
                ((and (eq vis-loc-chunk old-loc)
                      (eq vis-obj-chunk old-obj))
                 
                 ;(pprint 'case-5)


                 #|
                 
                 Modify both chunks and make sure the obj points to the
                 right loc just to be safe.
                |#
                 
                 (schedule-mod-buffer-chunk 'visual-location 
                                            ;; This is an ugly way to do things, but
                                            ;; it works for now...
                                            (cddr (chunk-spec-to-chunk-def 
                                                   (chunk-name-to-chunk-spec new-buffer-loc))) 
                                            0  
                                            :module :vision 
                                            :output 'high 
                                            :priority 15)
                 
                 (schedule-mod-buffer-chunk 'visual 
                                            ;; This is an ugly way to do things, but
                                            ;; it works for now...
                                            (cddr (chunk-spec-to-chunk-def 
                                                   (chunk-name-to-chunk-spec new-obj))) 
                                            0  
                                            :module :vision 
                                            :output 'high 
                                            :priority 14)
                 (schedule-event-relative 0 #'update-tracking-loc-chunk
                                          :module :vision
                                          :destination :vision
                                          :params (list t) 
                                          :priority 13
                                          :output nil)
                 
                 )
      
                ((eq vis-loc-chunk old-loc) 
                 ;; Don't know about the visual buffer
                 
                 ;(pprint 'case-6)

                 
                 #|
                 
                 Modify the loc

                |#
                 
                 (schedule-mod-buffer-chunk 'visual-location 
                                            ;; This is an ugly way to do things, but
                                            ;; it works for now...
                                            (cddr (chunk-spec-to-chunk-def 
                                                   (chunk-name-to-chunk-spec new-buffer-loc))) 
                                            0  
                                            :module :vision 
                                            :output 'high 
                                            :priority 15)
                )
                
                
                ((null vis-obj-chunk) ;; Don't know about the vis-loc buffer
                 ;(pprint 'case-7)


                 #|
                  Just put the new object in place 
                 |#
                 
                 (unless (chunk-slot-value-fct new-obj 'screen-pos)
                   (set-chunk-slot-value-fct new-obj 'screen-pos new-buffer-loc))
                 
                 (schedule-set-buffer-chunk 'visual new-obj 0
                                            :module :vision :output 'high :requested nil
                                            :priority 14)
                 
                 (schedule-event-relative 0 #'update-tracking-obj-chunk
                                          :module :vision
                                          :destination :vision
                                          :params nil
                                          :priority 12
                                          :output nil)
                 )
                
                ((eq vis-obj-chunk old-obj) ;; Don't know about vis-loc buffer
                 
                 ;(pprint 'case-8)


                 #|
                  Just modify the object chunk
                 |#
                 
                 (unless (chunk-slot-value-fct new-obj 'screen-pos)
                   (set-chunk-slot-value-fct new-obj 'screen-pos new-buffer-loc))
                 
                 (schedule-mod-buffer-chunk 'visual 
                                            ;; This is an ugly way to do things, but
                                            ;; it works for now...
                                            (cddr (chunk-spec-to-chunk-def 
                                                   (chunk-name-to-chunk-spec new-obj))) 
                                            0  
                                            :module :vision 
                                            :output 'high 
                                            :priority 14)
                 
                 )
                
                (t ;; Don't do anything 
                 ;(pprint 'case-9)


                 ))))))

  nil)


;;; REMOVE-TRACKING      [Method]
;;; Date        : 97.05.15
;;; Description : When tracking stops, the slots for tracked objects need to
;;;             : to be cleared, and the ACT hook functions need to be cleared.

(defgeneric remove-tracking (vis-mod)
  (:documentation  "Clears out all the tracking stuff"))

(defmethod remove-tracking ((vis-mod vision-module))
  (setf (tracked-obj-last-feat vis-mod) nil)
  (setf (tracked-obj-lastloc vis-mod) nil)
  (setf (tracked-obj-last-obj vis-mod) nil)
  (setf (tracked-obj vis-mod) nil)
  (change-state vis-mod :exec 'FREE)
)


;;; UPDATE-TRACKING      [Function]
;;; Date        : 97.05.15
;;; Description : To be called by the user to indicate movement.

(defun update-tracking ()
  "Call the Vision Module's tracking update method."
  ;(update-tracking-mth (get-module :vision))
  (print-warning "Update-tracking has been depricated.  All updates must be done through proc-display or the add/delete/update actions."))


;;; This is not a user command -- forcing an explicit  update can
;;; lead to invalid finsts and it won't pick up any new info until
;;; that change has occured through proc-display or update-visicon-item.

(defun unlock-tracking ()
  (update-tracking-mth (get-module :vision)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defgeneric set-cfs-mth (vis-mod kwrd)
  (:documentation "Set the current character feature set."))

(defmethod set-cfs-mth ((vis-mod vision-module) kwrd)
  (setf (active-cfs vis-mod) (get-cfs-mth vis-mod kwrd)))


(defgeneric get-cfs-mth (vis-mod kwrd)
  (:documentation "Given a keyword, find a character feature set."))

(defmethod get-cfs-mth ((vis-mod vision-module) kwrd)
  (aif (member kwrd (feature-sets vis-mod) :key #'name)
    (first it)
    (pm-warning "Feature set ~S is unknown" kwrd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod clear :after ((vis-mod vision-module))
  (remove-tracking vis-mod)
  
  ;; make sure to clear this too
  
  (setf (last-obj vis-mod) nil)
  
  ;;; Dan this seems to make sense
  (setf (loc-failure vis-mod) nil)
  (setf (attend-failure vis-mod) nil)
  (setf (scene-change vis-mod) nil)
  )



;;; FIND-LOCATION      [Method]
;;; Date        : 98.08.11R (rewrite), delta 99.06.24
;;; Description : To find a location, we need to search the icon.  To do this,
;;;             : construct a feature spec based on the parameters passed,
;;;             : and filter the list to those that match the spec.  This 
;;;             : should be faster for large icons because it makes only
;;;             : one pass over the icon.  Regardless, some
;;;             : post-processing of that list might be necessary if LOWEST
;;;             : or HIGHEST was passed as a coordinate or if NEAREST is 
;;;             : true.
;;;             : Once we have a list, randomly pick one (that may change in
;;;             : the future) feature and build a location out of it.

(defgeneric find-location (vis-mod chunk-spec)
  (:documentation  "Given a set of constraints, build a DMO for that screen location if one matches."))

(defmethod find-location ((vis-mod vision-module) chunk-spec)
  
  (update-new vis-mod)
  (check-finsts vis-mod)
  
  (let ((loc (awhen (find-current-locs-with-spec vis-mod chunk-spec)
                    (construct-location vis-mod (random-item (objs-max-val it 'chunk-visual-tstamp)) chunk-spec))))
    (if loc
       (progn
         (setf (loc-failure vis-mod) nil)
         (schedule-set-buffer-chunk 'visual-location loc 0 :module :vision :priority 10)
         (lock-device (current-device-interface))
         (schedule-event-relative 0 'unlock-device 
                                  :module :vision
                                  :destination :device
                                  :priority 9
                                  :output nil
                                  :maintenance t))
      (progn
         (setf (loc-failure vis-mod) t)
        (schedule-event-relative 0 'find-loc-failure :module :vision :output 'low)))
    (when (and loc (auto-attend vis-mod))
      (schedule-event-relative 0 'visual-auto-attend :destination :vision :output t
                               :module :vision :details (concatenate 'string "automatically attending " (symbol-name loc))))
    loc))


(defmethod visual-auto-attend ((vis-mod vision-module))
  (aif (buffer-read 'visual-location)
       (progn   
         ;; mark the module as busy between now and the attention shift
         
         (change-state vis-mod :exec 'BUSY :proc 'BUSY)
         
         ;; schedule the attention shift to be 50ms from now - should probably use the default action time
         ;; instead but not going to do that at this point...
         
         (schedule-event-relative .05 'move-attention :params (list :location it) :destination :vision :output 'medium
                                  :module :vision :details (concatenate 'string "Move-attention " (symbol-name it)) :priority 0)
         
         ;; Hack to clear the module state just before the attention shift so as not to 
         ;; jam things.
         
         (schedule-event-relative .05 'change-state :params (list :exec 'free :proc 'free) :destination :vision :output nil
                                  :module :vision :priority 1))
       
       (model-warning "Auto-attend failed because visual-location buffer was empty.")))


(defun find-loc-failure ()
  "Dummy event function to signal a find-location failure in the trace"
  nil)



(defgeneric construct-location (vis-mod feat spec)
  (:documentation  "Find or build a DMO based on a feature and the spec used to generate that feature."))


(defmethod construct-location ((vis-mod vision-module) loc spec)
  
  ;; record that this loc was found with this spec
  (setf (gethash (chunk-visicon-entry loc) (found-locs vis-mod)) spec)
  
  loc)

;;; MOVE-ATTENTION      [Method]
;;; Date        : 97.02.09
;;; Description : This is a toplevel command for the Vision Module, causing
;;;             : attention to move.  Latencey is 185 ms, so nothing actually
;;;             : happens right away other than setting state variables.
;;;             : A method is dispatched based on the scale that's passed.

(defgeneric move-attention (vis-mod &key location scale)
  (:documentation "Shift attention to a particular location at a particular scale."))

(defmethod move-attention ((vis-mod vision-module) &key location scale)
  (if (and (eq (exec-s vis-mod) 'BUSY) (not (tracked-obj-last-feat vis-mod)))
    (model-warning "Attention shift requested at ~S while one was already in progress." (mp-time))
    (progn
      (when (tracked-obj-last-feat vis-mod) (remove-tracking vis-mod))
      
      (setf (moving-attention vis-mod) t)
      (clear-attended vis-mod)
      (setf (last-scale vis-mod) scale)
      
      (setf (attend-failure vis-mod) nil)
        
      (schedule-event-relative 
         (randomize-time (move-attn-latency vis-mod))
         'encoding-complete
         :destination :vision
         :module :vision
         :params (list location scale)
         :details (concatenate 'string "Encoding-complete " (symbol-name location)
                    " " (symbol-name scale))
       :output 'medium)
      
      (setf (current-marker vis-mod) location)
      ;; not being used right now (set-clof vis-mod (dmo-to-xy location))
      (change-state vis-mod :exec 'BUSY :proc 'BUSY))))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun create-vision-module (model-name)
  (declare (ignore model-name))
  (make-instance 'vision-module))

(defun reset-vision-module (vis-mod)
  
  (reset-pm-module vis-mod)
  
  ;; This stuff was moved from an after method on reset-pm-module...
  
  (clrhash (visicon vis-mod))
  ;(clrhash (feat-table vis-mod))
  
  (setf (current-cursor vis-mod) nil)
  
  (remove-tracking vis-mod)
  (setf (last-scale vis-mod) nil)
  (set-cfs-mth vis-mod :RM-ORIG)
  (setf (synthd-objs vis-mod) (clrhash (synthd-objs vis-mod)))
  (setf (finst-lst vis-mod) nil)
  
  (setf (scene-change vis-mod) nil)
  
  (setf (last-obj vis-mod) nil)
  
  ;;(setf (attn-trace vis-mod) nil)
  
  ;; Not needed at this point (set-clof vis-mod #(0 0))
  
  (setf (loc-failure vis-mod) nil)
  (setf (attend-failure vis-mod) nil)
    
  (chunk-type visual-object screen-pos value status color height width)
  (chunk-type abstract-object value line-pos bin-pos)
  (chunk-type (abstract-letter (:include abstract-object)))
  (chunk-type (abstract-number (:include abstract-object)))
  (chunk-type (text (:include visual-object)))
  (chunk-type (empty-space (:include visual-object)))
  
  (chunk-type (line (:include visual-object)) end1-x end1-y end2-x end2-y)
  (chunk-type (oval (:include visual-object)))
  (chunk-type (cursor (:include visual-object)))
  
  (chunk-type (phrase! (:include visual-object)) objects words colors)
  
  (chunk-type visual-location screen-x screen-y distance kind color 
              value height width size)
  
  (chunk-type set-visloc-default type screen-x screen-y distance kind color 
              value height width size)
  
  (chunk-type (char-primitive (:include visual-location)) left right)
  
  (chunk-type vision-command)
  
  (unless (chunk-type-p pm-constant)
    (chunk-type pm-constant))
  (chunk-type color)
  
  (chunk-type (move-attention (:include vision-command)) screen-pos scale)
  
  (chunk-type (start-tracking (:include vision-command)))
  
  (chunk-type (assign-finst (:include vision-command)) object location)
  
  (chunk-type (clear-scene-change (:include vision-command)))

  
  (unless (chunk-type-p clear)
    (chunk-type clear))
  
  (define-chunks 
    (lowest isa pm-constant)
    (highest isa pm-constant)
    (current isa pm-constant)
    (external isa pm-constant)
    (internal isa pm-constant)
    (find-location isa vision-command)
    (move-attention isa vision-command)
    (assign-finst isa vision-command)
    (start-tracking isa vision-command)
    
    (black isa color)
    (red isa color)
    (blue isa color)
    (green isa color)
    (white isa color)
    (magenta isa color)
    (yellow isa color)
    (cyan isa color)
    (dark-green isa color)
    (dark-red isa color)
    (dark-cyan isa color)
    (dark-blue isa color)
    (dark-magenta isa color)
    (dark-yellow isa color)
    (light-gray isa color)
    (dark-gray isa color)
        
    (text isa chunk)
    (box isa chunk)
    (line isa chunk)
    (oval isa chunk)
    
    (new isa chunk)
    (clear isa chunk))
  
  
  (setf (default-spec vis-mod)
        (define-chunk-spec isa visual-location screen-x lowest :attended new)))

(defun clear-scene-change (vis-mod)
  (setf (scene-change vis-mod) nil))


(defun query-vision-module (vis-mod buffer slot value)
  (case buffer
    (visual
     (cond ((and (eq slot 'state) (eq value 'error))
            (attend-failure vis-mod))
           ((eq slot 'scene-change)
            (let ((change-detect (and (numberp (car (scene-change vis-mod)))
                                      (>= (car (scene-change vis-mod)) (change-threshold vis-mod))
                                      (<= (mp-time) (+ (cdr (scene-change vis-mod)) (new-span vis-mod))))))
              (or (and change-detect value)
                  (null (or change-detect value)))))
           ((eq slot 'scene-change-value)
            (and (numberp value)
                 (numberp (car (scene-change vis-mod)))
                 (>= (car (scene-change vis-mod)) value)))
           (t (generic-state-query vis-mod buffer slot value))))
    (visual-location
     (case slot
       (state
        (case value
          (busy nil) ;; visual-location requests are always free
          (free t)
          (error (loc-failure vis-mod))
          (t (print-warning 
              "Invalid query made of the ~S buffer with slot ~S and value ~S" 
              buffer slot value))))
       (attended
        (let ((vis-loc-chunk (buffer-read 'visual-location)))
          (when (and vis-loc-chunk (chunk-type-subtype-p-fct (chunk-chunk-type-fct vis-loc-chunk) 'visual-location))
            (let* ((old-loc  (chunk-visicon-entry vis-loc-chunk))
                   (loc (if (chunk-p-fct old-loc) old-loc vis-loc-chunk)))
            
              (update-new vis-mod)
              (check-finsts vis-mod)
              
              (test-attended (list '= :attended value) loc)))))))))


(defmethod warn-vision ((vis-mod vision-module) buffer-name chunk-type)
  (declare (ignore chunk-type))
  (when (and (eq buffer-name 'visual)
             (null (visual-lock vis-mod)))
    (lock-device (current-device-interface))
    (setf (visual-lock vis-mod) t)))

(defmethod pm-module-request ((vis-mod vision-module) buffer-name chunk-spec)
  (case buffer-name
    (visual
     (when (visual-lock vis-mod)
       (setf (visual-lock vis-mod) nil)
       (schedule-event-relative 0 'unlock-device 
                                :module :vision
                                :destination :device
                                :priority :min
                                :output nil
                                :maintenance t))
       
     (case (chunk-spec-chunk-type chunk-spec)
       (clear ;; replaces the implicit clear from -visual
        (schedule-event-relative 0 'clear :module :vision :destination :vision :output 'low))
       (clear-scene-change
        (schedule-event-relative 0 'clear-scene-change :module :vision :destination :vision :output 'low))
       (start-tracking
        (schedule-event-relative 0 'start-tracking 
                                 :destination :vision
                                 :module :vision
                                 :output 'medium))
       (assign-finst
        (let ((object (if (slot-in-chunk-spec-p chunk-spec 'object) 
                         (verify-single-explicit-value 
                          (chunk-spec-slot-spec chunk-spec 'object) 
                          :vision 'assign-finst 'object)
                         nil))
              (location (if (slot-in-chunk-spec-p chunk-spec 'location)
                            (verify-single-explicit-value 
                             (chunk-spec-slot-spec chunk-spec 'location) 
                             :vision 'assign-finst 'location)
                            nil)))
          
          (if (or object location)
            (schedule-event-relative 0 'assign-finst 
                                     :params (list vis-mod :object object 
                                                   :location location)
                                     :module :vision
                                     :output 'medium)
            (print-warning "An object or location is required for an assign-finst request"))))
       
       
       (visual-object
        (print-warning "Move attention requests are now done with an isa move-attention"))
       
       
       (move-attention
        (let ((sp (if (slot-in-chunk-spec-p chunk-spec 'screen-pos) 
                     (verify-single-explicit-value 
                      (chunk-spec-slot-spec chunk-spec 'screen-pos) 
                      :vision 'visual-object 'screen-pos)
                     nil))
              (scale (if (slot-in-chunk-spec-p chunk-spec 'scale)
                        (verify-single-explicit-value 
                         (chunk-spec-slot-spec chunk-spec 'scale) 
                         :vision 'visual-object 'scale)
                        nil)))
         ; (when scale
         ;     (print-warning "Scale values are not yet handled by the new vision module - ignoring it."))
          (when sp
            (if (chunk-p-fct sp)
                (if (chunk-type-subtype-p-fct (chunk-chunk-type-fct sp) 'visual-location)
                    (schedule-event-relative 0 'move-attention 
                                     :params (list vis-mod :scale scale :location sp)
                                     :details 
                                     (concatenate 'string "Move-attention " (symbol-name sp)
                                                  " " (symbol-name scale))                                     
                                             :module :vision)
                  (print-warning "screen-pos value ~s in a move-attention request was not a visual-location chunk" sp))
              (print-warning "screen-pos value ~s in a move-attention request was not a chunk" sp)))))
       (t
        (print-warning "Invalid command ~a sent to the visual buffer" 
                       (chunk-spec-chunk-type chunk-spec)))))
    
    (visual-location
     (cond ((chunk-type-subtype-p-fct (chunk-spec-chunk-type chunk-spec) 'visual-location)
            (let ((nearest (if (slot-in-chunk-spec-p chunk-spec :nearest) 
                               (verify-single-explicit-value 
                                (chunk-spec-slot-spec chunk-spec :nearest) 
                                :vision 'visual-location :nearest)
                             :none))              
                  (attended (if (slot-in-chunk-spec-p chunk-spec :attended) 
                                (multiple-value-bind (val valid)
                                    (verify-single-explicit-value 
                                     (chunk-spec-slot-spec chunk-spec :attended) 
                                     :vision 'visual-location :attended)
                                  valid)
                              :none)))           
              
              (if (or (null nearest) (null attended))
                  (print-warning "Invalid value in a request to the visual-location buffer")
                
                (schedule-event-relative 0 'find-location :module :vision 
                                         :destination :vision 
                                         :details "Find-location" 
                                         :output 'medium
                                         :params (list chunk-spec)))))
           ((chunk-type-subtype-p-fct (chunk-spec-chunk-type chunk-spec) 'set-visloc-default)
            (let ((nearest (if (slot-in-chunk-spec-p chunk-spec :nearest) 
                               (verify-single-explicit-value 
                                (chunk-spec-slot-spec chunk-spec :nearest) 
                                :vision 'set-visloc-default :nearest)
                             :none))              
                  (attended (if (slot-in-chunk-spec-p chunk-spec :attended) 
                                (multiple-value-bind (val valid)
                                    (verify-single-explicit-value 
                                     (chunk-spec-slot-spec chunk-spec :attended) 
                                     :vision 'set-visloc-default :attended)
                                  valid)
                              :none))
                  (type (if (slot-in-chunk-spec-p chunk-spec 'type) 
                                (multiple-value-bind (val valid)
                                    (verify-single-explicit-value 
                                     (chunk-spec-slot-spec chunk-spec 'type) 
                                     :vision 'set-visloc-default 'type)
                                  val)
                              :none)))           
              
              (if (or (null nearest) (null attended))
                  (print-warning "Invalid value in a request to the visual-location buffer")
                (if (and type (not (eq type :none)) (not (chunk-type-p-fct type)))
                    (print-warning "Invalid type specified in set-visloc-default request.")
                  (schedule-event-relative 0 'set-visloc-default-request :module :vision 
                                           :destination :vision 
                                           :details "Set-visloc-default" 
                                           :output 'medium
                                           :priority 9 ; just below the buffer clearing by the production
                                           :params (list chunk-spec))))))
           (t (print-warning "Invalid command ~a sent to the visual-location buffer" 
                             (chunk-spec-chunk-type chunk-spec)))))))



(defun params-vision-module (vis-mod param)
  (if (consp param)
    (case (car param)
      
      (:optimize-visual
       (setf (optimize-p vis-mod) (cdr param)))
      (:viewing-distance (setf (view-dist vis-mod) (cdr param)))
      (:visual-attention-latency
       (setf (move-attn-latency vis-mod) (cdr param)))
      (:scene-change-threshold
       (setf (change-threshold vis-mod) (cdr param)))
      (:visual-finst-span
       (setf (finst-span vis-mod) (cdr param))
       (check-finsts vis-mod)
       (cdr param))   
      (:visual-movement-tolerance
       (setf (move-allowance vis-mod) (cdr param)))
      (:visual-num-finsts
       (setf (num-finst vis-mod) (cdr param))
       (check-finsts vis-mod)
       (cdr param))
      (:visual-onset-span
       (setf (new-span vis-mod) (cdr param)))
      (:auto-attend
       (setf (auto-attend vis-mod) (cdr param)))
      (:test-feats
       (setf (test-feats vis-mod) (cdr param)))
      (:delete-visicon-chunks
       (setf (purge-visicon vis-mod) (cdr param))))
    
    (case param
      
      (:optimize-visual
       (optimize-p vis-mod))  
      (:scene-change-threshold
       (change-threshold vis-mod))
      (:visual-attention-latency
       (move-attn-latency vis-mod))
      (:visual-finst-span
       (finst-span vis-mod))   
      (:visual-movement-tolerance
       (move-allowance vis-mod))
      (:visual-num-finsts
       (num-finst vis-mod))
      (:visual-onset-span
       (new-span vis-mod))
      (:auto-attend
       (auto-attend vis-mod))
      (:test-feats
       (test-feats vis-mod))
      (:delete-visicon-chunks
       (purge-visicon vis-mod)))))


(define-module-fct :vision 
    (list (list 'visual-location nil '(:attended :nearest) '(attended)
                            #'(lambda ()
                               (command-output "  attended new          : ~S"
                                               (query-buffer 'visual-location 
                                                             '((attended . new))))
                               (command-output "  attended nil          : ~S"
                                               (query-buffer 'visual-location
                                                             '((attended . nil))))
                               (command-output "  attended t            : ~S"
                                               (query-buffer 'visual-location
                                                             '((attended . t)))))) 
        (list 'visual nil nil '(scene-change-value scene-change modality preparation execution processor last-command)
                 #'(lambda () 
                     (let ((v (get-module :vision)))
                       (print-module-status v)
                       (command-output "  scene-change          : ~S"
                                       (query-buffer 'visual 
                                                     '((scene-change . t))))
                       (command-output "  scene-change-value    : ~S"
                                       (car (scene-change v)))))))
  (list 
   
   (define-parameter :scene-change-threshold
     :valid-test (lambda (x) (and (numberp x) (<= 0.0 x 1.0))) 
     :default-value 0.25
     :warning "a number in the range [0.0,1.0]"
     :documentation "Proportion of visicon which must change to signal a scene change")
   (define-parameter :optimize-visual
     :valid-test #'tornil 
     :default-value T
     :warning "T or NIL"
     :documentation "If set to nil the default devices process text into sub-letter features")
    (define-parameter :visual-attention-latency
     :valid-test #'nonneg 
     :default-value 0.085
     :warning "a non-negative number"
     :documentation "Time for a shift of visual attention")
   (define-parameter :visual-finst-span
     :valid-test #'nonneg 
     :default-value 3.0
     :warning "a non-negative number"
     :documentation "Lifespan of a visual finst")
   (define-parameter :visual-movement-tolerance
     :valid-test #'nonneg 
     :default-value 0.5
     :warning "a non-negative number"
     :documentation 
     "How far something can move while still being seen as the same object.")
   (define-parameter :visual-num-finsts
     :valid-test #'posnum 
     :default-value 4
     :warning "a positive number"
     :documentation "Number of visual finsts.")
   (define-parameter :visual-onset-span
     :valid-test #'nonneg 
     :default-value 0.5
     :warning "a non-negative number"
     :documentation "Lifespan of new visual objects being marked as NEW")
   (define-parameter :auto-attend
     :valid-test #'tornil 
     :default-value nil
     :warning "T or NIL"
     :documentation "Whether visual-location requests automatically generate an attention shift")
   
   (define-parameter :test-feats
     :valid-test #'tornil 
     :default-value T
     :warning "T or NIL"
     :documentation "Whether proc-display should use the features to compare items instead of just the chunk names")
   (define-parameter :delete-visicon-chunks
     :valid-test #'tornil 
     :default-value T
     :warning "T or NIL"
     :documentation "Whether proc-display should delete and unintern the name of old chunks that were in the visicon")
   (define-parameter :viewing-distance :owner nil))
  
  :version "3.1"
  :documentation "A module to provide a model with a visual attention system (which now uses chunks internally)"
  :creation 'create-vision-module
  :reset 'reset-vision-module
  :query 'query-vision-module
  :request 'pm-module-request
  :params 'params-vision-module
  :warning 'warn-vision)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; BUILD-STRING-FEATS      [Method]
;;; Date        : 99.03.30
;;; Description : In order to build all the features for a string, there is a
;;;             : great deal of stuff that is needed from the interface beyond
;;;             : just the string itself.  Need to know the y coordinate, the
;;;             : starting x coordinate, the line height, the associated
;;;             : screen object, and some way of determining the width (in
;;;             : pixels) of a string.
;;;             : 
;;;             : There are two ways to do it, with and without optimizing. 
;;;             : With optimizing it's easy, just accumulate words.  When
;;;             : optimzing is off, though, have to walk the string
;;;             : character-by-character and build features for each one.

(defgeneric build-string-feats (vis-mod &key text start-x y-pos width-fct height obj)
  (:documentation  "Build a list of visual-locations representing a string with the given geometry."))

(defun merge-real-visual-values (first second)
  (declare (ignore first))
  (chunk-real-visual-value second))

(extend-chunks real-visual-value :copy-function identity :merge-function merge-real-visual-values)

(defmethod build-string-feats ((vis-mod vision-module) &key text start-x y-pos width-fct height obj)
  (declare (fixnum start-x y-pos)  (function width-fct) (string text) (number height))
  (when (not (and text start-x y-pos width-fct height))
    (error "NIL passed to BUILD-STRING-FEATS."))
  (unless (equal text "")
    (let ((curr-x start-x)
          (f-accum nil)
          (spc-wdth (funcall width-fct " "))
          (curr-width nil))
      (if (optimize-p vis-mod)
          
        ;; if optimizing is on, then carve the string into words (strings)
        ;; and space runs (numbers)
        (dolist (word (chop-string text) (nreverse f-accum))
          (when (stringp word)
            (setf curr-width (funcall width-fct word))
            (let ((vl (car (define-chunks-fct (list (list 'isa 'visual-location
                                                    'screen-y y-pos 'height height 'value 'text 'kind 'text
                                                    'width curr-width 'screen-x (+ curr-x (round curr-width 2))))))))
              (push vl f-accum)
              (setf (chunk-real-visual-value vl) (string-downcase word))))
          (incf curr-x (if (stringp word) curr-width (* word spc-wdth))))
        
        ;; if not optimizing, then blast it character-by-character
        (let ((char nil))
          (dotimes (idx (length text) (nreverse (flatten f-accum)))
            (setf char (char text idx))
            (setf curr-width (funcall width-fct (subseq text idx (1+ idx))))
            (cond ((alphanumericp char)
                   (push (char-to-features vis-mod (char-upcase char)
                                           curr-x
                                           (- (+ curr-x curr-width) 1)
                                           y-pos
                                           height obj) f-accum))
                  ((and (graphic-char-p char)
                        (not (char-equal char #\space)))
                   
                   (let ((vl (car (define-chunks-fct (list (list 'isa 'visual-location
                                                           'screen-y y-pos 'height height 'value 'text 'kind 'text
                                                           'width curr-width 'screen-x (+ curr-x (round curr-width 2))))))))
                     (push vl f-accum)
                     (setf (chunk-real-visual-value vl) (mkstr char)))
                   
                   )
                  (t nil))
            (incf curr-x curr-width)))))))

;;; CHAR-TO-FEATURES      [Method]
;;; Date        : 99.03.30
;;; Description : For each character, there will usually be many of those
;;;             : CHAR-PRIMITIVE features.  Grab the list of features 
;;;             : associated with a character, and build 'em.

(defgeneric char-to-features (vis-mod char left right y height obj)
  (:documentation  "Returns a list of basic visual-location chunks for a characer"))

(defmethod char-to-features ((vis-mod vision-module) (char character) 
                                (left number) (right number) (y number) 
                             (height number) obj)
  (declare (ignore obj))
  (let ((xpos (+ left (round (- right left) 2)))
        (width (1+ (- right left)))
        (features (pairlis
                     (getfeats (active-cfs vis-mod) char)
                     (get-icon-feats (active-cfs vis-mod) char)))
        (accum nil))
    (dolist (feats features accum)
      (let ((vl (car (define-chunks-fct (list (list 'isa 'char-primitive
                                                    'screen-y y 'height height 'value (rest feats)
                                              'width width 'screen-x xpos 'left left
                                              'right right))))))
        (push vl accum)
        (setf (chunk-real-visual-value vl) (first feats))))))



(defun chop-string (str)
  (declare (string str))
  (let* ((oldstate (char->state (char str 0)))
         (state nil)
         (chr nil)
         (wrd "")
         (cnt 0)
         (accum nil))
    (dotimes (i (length str))
      (setf chr (char str i))
      (setf state (char->state chr))
      (cond
       ;; if we're accumulating chars and the new char matches our state,
       ;; just concat the char
       ((or (and (eq state :WORD) (eq oldstate :WORD))
            (and (eq state :MISC) (eq oldstate :MISC)))
        (setf wrd (mkstr wrd chr))
        (setf cnt 0))
       ;; If we get a state change that finishes a word, then grab the
       ;; word.
       ((and (or (eq oldstate :WORD) (eq oldstate :MISC))
             (not (eq oldstate state)))
        (push wrd accum)
        (if (not (eq state :SPACE))
          (setf wrd (mkstr chr))
          (setf wrd "")))
       ;; when switching from spaces to words, push the counter and start
       ;; the word
       ((and (eq oldstate :SPACE) (not (eq state oldstate)))
        (push (1+ cnt) accum)
        (setf wrd (mkstr wrd chr)))
       ;; from whitespace to whitespace, inc the counter
       (t (incf cnt)))
      (setf oldstate state))
    (when (not (string= wrd ""))
      (push wrd accum))
    (setf accum (nreverse accum))
    (when (numberp (first accum))
      (decf (first accum)))
    accum))

(defun char->state (char)
  "Given a character, return :WORD, :SPACE, or :MISC"
  (declare (character char))
  (cond ((alphanumericp char) :WORD)
        ((whitespace-p char) :SPACE)
        (t :MISC)))

(defun whitespace-p (char)
  "Returns T if <char> is a whitespace character (non-printing or space)"
  (declare (character char))
  (or (not (graphic-char-p char))
      (eq char #\Space)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fill-default-vis-obj-slots (vis-obj vis-loc)
  (if (and (chunk-p-fct vis-obj)
           (chunk-p-fct vis-loc)
           (chunk-type-subtype-p-fct (chunk-chunk-type-fct vis-obj) 'visual-object)
           (chunk-type-subtype-p-fct (chunk-chunk-type-fct vis-loc) 'visual-location))
      (mod-chunk-fct vis-obj `(height ,(chunk-slot-value-fct vis-loc 'height)
                               width ,(chunk-slot-value-fct vis-loc 'width)
                               color ,(chunk-slot-value-fct vis-loc 'color)
                               value ,(if (chunk-real-visual-value vis-loc)
                                         (chunk-real-visual-value vis-loc)
                                       (chunk-slot-value-fct vis-loc 'value))))
    (print-warning "Invalid chunk passed to fill-default-vis-obj-slots ~S not updated using ~s." vis-obj vis-loc)))


(defun compute-vis-loc-size (vis-loc)
  (set-chunk-slot-value-fct vis-loc 'size
                            (aif (simple-size vis-loc)
                                 it
                                 1.0)))


(defun simple-size (vis-loc)
  (let ((w (chunk-slot-value-fct vis-loc 'width))
        (h (chunk-slot-value-fct vis-loc 'height)))
    (when (and (numberp w) (numberp h))
      (* 0.01 (round
               (* (pm-pixels-to-angle w)
                  (pm-pixels-to-angle h))
               0.01)))))
  


(extend-chunks visual-approach-width-fn :copy-function identity)

;;; APPROACH-WIDTH      [Method]
;;; Date        : 99.03.30
;;; Description : Remember in high school when someone asked in trig why we
;;;             : needed to know this crap?  Well, this is why.  I'm pretty
;;;             : sure this is all the right trig, but there could be some
;;;             : missed math in here.

(defun approach-width (vis-loc theta)
  (aif (chunk-visual-approach-width-fn vis-loc)
      (funcall it vis-loc theta)
       (let* ((x (chunk-slot-value-fct vis-loc 'width))
              (y (chunk-slot-value-fct vis-loc 'height)))
         (if (or (not (numberp x)) (not (numberp y)))
             (aif (get-module :motor)
                  (default-target-width it)
                  1.0)
           (let ((critical-theta (atan y x))
                 (theta (abs theta))
                 (ret-width nil))
             (when (> theta (/ pi 2))
               (setf theta (- pi theta)))
             (setf ret-width
               (cond ((= theta 0) x)
                     ((= theta (/ pi 2)) y)
                     ((= theta critical-theta) (sqrt (+ (* y y) (* x x))))
                     ((< theta critical-theta) (/ x (cos theta)))
                     (t (/ y (cos (- (/ pi 2) theta))))))
             (pm-pixels-to-angle ret-width))))))
       




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-chunks special-visual-object :copy-function identity)

(defun add-visicon-item (obj &optional (update t))
  (add-screen-object obj (get-module :vision) update))

(defun delete-visicon-item (obj &optional (update t))
  (delete-screen-object obj (get-module :vision) update))


(defun update-visicon-item (obj &optional (update t) &key same-chunks chunks)
  (let ((module (get-module :vision)))
    (cond ((not (or same-chunks chunks)) 
           ;; simple case just delete and add the item
           (delete-screen-object obj module nil)
           (add-screen-object obj module update))
          
          (t ;; just update the chunks for the item
           
           (cond ((null chunks) ;; if none given get them from build-vis-locs-for
                  (setf chunks (flatten (build-vis-locs-for obj module))))
                 ((atom chunks) ;; if it's an atom assume that's a chunk name
                  (setf chunks (list chunks))))
                 
           
           (dolist (x chunks)
             (if (eq (chunk-special-visual-object x) obj) ;; make sure it matches the object
                 
                 (let ((old (gethash (chunk-visicon-entry x) (visicon module))))
                   (if old
                       (progn
                         
                         ;; set the visicon entry with the new features
                         
                         (setf (chunk-visicon-entry x) 
                           (if (test-feats module)
                               (hash-visual-chunk-contents x)
                             x))

                         (let ((entry (copy-chunk-fct x)))
                         
                           ;; put the new one in place and set the relevant info for it.
                           
                           (setf (gethash (chunk-visicon-entry x) (visicon module)) entry)
                           
                           (setf (chunk-visual-tstamp entry) (chunk-visual-tstamp old))
                           (setf (chunk-visual-new-p entry) (chunk-visual-new-p old))
    
                         
                         #| assuming that nobody is going to use "synthed" features
                            in their own add/update/delete code, but if needed something like 
                            this old code from enter-into-visicon will need to be made to work
                            right for the update.

                         (dolist (x (finst-lst module))
                           (when (and (synthed-from x) (find (chunk-visicon-entry existing) (synthed-from x)))
                             (setf (synthed-from x) (substitute vis-loc (chunk-visicon-entry existing) (synthed-from x)))))
                         |#
                         
                           ;; get rid of the old chunk if allowed 
                           
                           (when (purge-visicon module)
                             ;; might need to test this to make sure it's not
                             ;; currently attened and stuff...
                             
                             (purge-chunk-fct old))))
                     (print-warning "Chunk ~s is not currently in the visicon.  No update made." x)))
               (print-warning "Chunk ~s is not a feature of the object ~s.  No update made." x obj)))
           
           (when update
             (visicon-update module nil))))))
             

(defmethod add-screen-object (obj (vm vision-module) &optional (update t))
  (let ((vfeats (flatten (build-vis-locs-for obj vm))))
    (dolist (x vfeats)
      (if (and (chunk-p-fct x)
               (chunk-type-subtype-p-fct (chunk-chunk-type-fct x) 'visual-location))
          
          (progn
            (unless (numberp (chunk-slot-value-fct x 'size))
              (compute-vis-loc-size x))
            
            (unless (numberp (chunk-slot-value-fct x 'distance))
              (set-chunk-slot-value-fct x 'distance (no-output (car (sgp :VIEWING-DISTANCE))))))
        
        (progn 
          (print-warning "Invalid visicon item ~s found when processing the display.  Must be a chunk which is a subtype of visual-location." x)
          (setf vfeats (remove x vfeats)))))
    
    (mapcar #'(lambda (vl)
                (setf (chunk-visicon-entry vl)
                    (if (test-feats vm)
                        (hash-visual-chunk-contents vl)
                      vl))
                (setf (chunk-visual-feature-name vl) vl))
      vfeats)
    
    (mapcar #'(lambda (vl) 
                (setf (chunk-special-visual-object vl) obj)
                (enter-into-visicon vl vm)) 
      vfeats)
    
    (when update
      (visicon-update vm nil))))

(defmethod delete-screen-object (obj (vm vision-module) &optional (update t))
  (maphash (lambda (key val)
             (when (eq obj (chunk-special-visual-object val))
               (remhash key (visicon vm))
               (when (and (purge-visicon vm) (chunk-p-fct val))
                 (purge-chunk-fct val))))
           (visicon vm))
  (when update
    (visicon-update vm nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-char-feature-set (setname)
  "Sets the feature set used to represent characters when optimizing is off. <setname> should be a keyword."
  (set-cfs-mth (get-module :vision) setname))


(defmethod set-visloc-default-request ((vm vision-module) spec)
  "Assumes all the checking has been done already for validity"
  (let ((slot-specs (remove 'type (chunk-spec-slot-spec spec) :key #'second))
        (type (if (SLOT-IN-CHUNK-SPEC-P spec 'type) (third (first (chunk-spec-slot-spec spec 'type))) nil)))
    (setf (default-spec vm) 
      (define-chunk-spec-fct  (slot-specs-to-chunk-spec-list (if type type 'visual-location) 
                                                                 slot-specs))))
  (update-new vm)
  (check-finsts vm)
  (stuff-visloc-buffer vm))

(defmacro set-visloc-default (&rest params)
  "Macro to set the default specification for visual location buffer stuffing."
  `(set-visloc-default-fct ',params))

(defun set-visloc-default-fct (params)
  "Function to set the default specification for visual location buffer stuffing."
  (verify-current-mp
   "No current meta-process.  Cannot set visloc defaults."
   (verify-current-model 
    "No current model.  Cannot set visloc defaults."
    (if (get-module :vision)
        (let* ((spec (funcall #'define-chunk-spec-fct params))
               (attended (when (slot-in-chunk-spec-p spec :attended)
                           (chunk-spec-slot-spec spec :attended)))
               (nearest (when (slot-in-chunk-spec-p spec :nearest)
                          (chunk-spec-slot-spec spec :nearest))))
          (if (or (> (length attended) 1)
                  (> (length nearest) 1))
              (progn
                (print-warning "The :attended and :nearest specification for set-visloc-default can only be specified at most once.")
                (print-warning "Visloc defaults not changed."))
            (if spec
                (progn (setf (default-spec (get-module :vision)) spec) t)
              (print-warning "Invalid chunk specification.  Default not changed."))))
              
      (print-warning "No vision module found.  Cannot set visloc defaults.")))))

(defun print-visicon ()
  "Print the Vision Module's visicon. For debugging."
  (awhen (get-module :vision)  ;; Test that there is a vision module
         (update-new it)
         (check-finsts it) 
         (command-output "Loc        Att   Kind           Value             Color           ID")
         (command-output "---------  ---   -------------  ----------------  --------------  -------------")
         
         (mapcar 'print-icon-feature (visicon-chunks it t))
         nil))


(defun print-icon-feature (chunk)
  (command-output "(~3D ~3D)~11T~A~17T~A~32T~S~50T~A~66T~A"
                  (chunk-slot-value-fct  chunk 'screen-x) 
                  (chunk-slot-value-fct  chunk 'screen-y) 
                  (feat-attended chunk (get-module :vision))
                  (chunk-slot-value-fct  chunk 'kind) 
                  (if (null (chunk-real-visual-value chunk))
                      (chunk-slot-value-fct  chunk 'value) 
                    (chunk-real-visual-value chunk))
                  (chunk-slot-value-fct  chunk 'color) 
                  (chunk-visual-feature-name chunk)))



(defun attend-visual-coordinates (x y)
  "Tells the Vision Module to start with attention at a certain location."
  (if (get-module :vision)
      (setf (current-marker (get-module :vision)) 
        (car (define-chunks-fct `((isa visual-location
                                       screen-x ,x
                                       screen-y ,y
                                       distance ,(car (no-output (sgp :VIEWING-DISTANCE))))))))
    (print-warning "No vision module found.  Cannot set visual coordinates.")))

(defun remove-visual-finsts (&key (set-new nil) (restuff nil))
  (let ((vis-m (get-module :vision)))
    (if vis-m
        (progn
          (setf (finst-lst vis-m) nil)
          (if set-new
            (maphash (lambda (key value)
                       (declare (ignore key))
                       (setf (chunk-visual-tstamp value) (mp-time))
                       (setf (chunk-visual-new-p value) 'new))
                     (visicon vis-m))
            (maphash (lambda (key value)
                       (declare (ignore key))
                       
                       (setf (chunk-visual-new-p value) 
                         (if (<= (- (mp-time) (chunk-visual-tstamp value) (new-span vis-m)))
                             'new 
                           nil)))
                     (visicon vis-m)))
                     
          (when restuff           
            (stuff-visloc-buffer vis-m))
          nil)
            
        (print-warning "No vision module found.  Cannot remove the visual finsts."))))

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

(in-package #:haptic-vicon-synchronizer)

(defmacro setdocs (&body pairs)
  `(progn
     ,@(loop for (var doc) in pairs
             collect (destructuring-bind (var &optional (type 'function))
                         (if (listp var) var (list var))
                       `(setf (documentation ',var ',type) ,doc)))))

(setdocs
  (interleave
   "Interleave each form in BODIES by setting it as the last element of the preceding form.

Probably more easily illustrated by example:
 (interleave
  (foo bar)
  (baz))
=> (foo bar (baz))")
  
  (duration
   "Returns the duration specified by the LOCAL-TIME:TIMESTAMPs FROM and TO.")
  
  (curry
   "Curries FUNC from the left by ARG.")
  
  (comb
   "Combs functions A and B by returning a function of one argument that first calls B and then A with the result.")
  
  (channel-duration
   "Return the duration (in seconds) that the CHANNEL encompasses.")
  
  (max-range
   "Determine the maximum range covered by all CHANNELS.")
  
  (max-length
   "Determine the maximum number of events of all CHANNELS.")
  
  (maybe-update
   "If CURRENT is too old for STAMP, return a new event from CHANNEL that is just ahead of STAMP if possible, or the last event in CHANNEL otherwise.")
  
  (synchronize-channels
   "Synchronize all channels in INPUTs to their corresponding channels in OUTPUTs.
All channels in OUTPUTs will contain as many events as MAX-EVENTS of all INPUTs
and will be as long as MAX-RANGE of all INPUTs. Events will be duplicated as necessary.")
  
  (synchronize
   "Synchronize the TIDE files for HAPTIC and VICON to a TIDE file in OUTPUT.")
  
  (print-help
   "Print a help screen on how to use the standalone binary to *STANDARD-OUTPUT*.")

  (check-compatibility
   "Check that the FILEs is a list of compatible input files.
Signals an error if multiple TIDE files are detected.")  

  (ensure-tide
   "Ensure that FILEs are a TIDE file, if necessary converted by ENSURE-FUNC.
Returns the pathname to that TIDE file.")
  
  (main
   "Entry point for the standalone binary."))

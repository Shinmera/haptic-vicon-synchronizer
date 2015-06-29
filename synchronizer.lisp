(in-package #:haptic-vicon-synchronizer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro interleave (&rest bodies)
    (labels ((ins (bodies)
               (if (cdr bodies)
                   (append (car bodies) (list (ins (cdr bodies))))
                   (car bodies))))
      (ins bodies))))

(defun duration (from to)
  (abs
   (- (local-time:timestamp-to-universal from)
      (local-time:timestamp-to-universal to))))

(defun curry (func arg)
  (lambda (&rest args)
    (apply func arg args)))

(defun comb (a b)
  (lambda (arg)
    (funcall a (funcall b arg))))

(defun channel-duration (channel)
  (duration (rsbag:start-timestamp channel)
            (rsbag:end-timestamp channel)))

(defun max-range (&rest channels)
  (values (apply #'min (mapcar (comb #'local-time:timestamp-to-universal #'rsbag:start-timestamp) channels))
          (apply #'max (mapcar (comb #'local-time:timestamp-to-universal #'rsbag:end-timestamp) channels))))

(defun max-length (&rest channels)
  (loop for channel in channels
        maximize (length channel)))

(defun maybe-update (stamp current channel)
  (flet ((next (event)
           (let ((maybe-current (rsbag:entry channel (rsb:event-sequence-number event))))
             (if (eql event maybe-current)
                 (rsbag:entry channel (1+ (rsb:event-sequence-number event)))
                 event))))
    (loop for event = current then (next event)
          while (and (< (rsb:event-sequence-number event) (length channel))
                     (local-time:timestamp< (getf (rsb:event-timestamps event) :create) stamp))
          finally (return event))))

(defun synchronize-channels (inputs outputs)
  (multiple-value-bind (start end) (apply #'max-range inputs)
    (let* ((length (apply #'max-length inputs))
           (events/s (float (/ length (- end start)) 1.0d0))
           (s/event (float (/ (- end start) length) 1.0d0)))
      (format T "~&RANGE: ~a - ~a~
                 ~&LENGTH: ~a~
                 ~&EVENTS/S: ~a~
                 ~&S/EVENT: ~a"
              start end length events/s s/event)
      (loop for time from start to end by s/event
            for stamp = (make-precise-timestamp time)
            for i from 0
            for currents = (mapcar (lambda (c) (rsbag:entry c 0)) inputs)
            then (mapcar (lambda (c i) (maybe-update stamp c i)) currents inputs)
            do (loop for output in outputs
                     for current in currents
                     do (make-entry output (rsb:event-data current) stamp
                                    :sequence-number i))))))

(defun synchronize (&key output haptic vicon)
  (interleave
   (with-default-bag (bag vicon :direction :input))
   (with-vicon-channel (vicon-input bag))
   (with-default-bag (bag haptic :direction :input))
   (with-haptic-channel (haptic-input bag))
   (with-default-bag (bag output :direction :io))
   (with-vicon-channel (vicon-output bag))
   (with-haptic-channel (haptic-output bag))
   (synchronize-channels
    (list (channel vicon-input) (channel haptic-input))
    (list vicon-output haptic-output))))

(defun print-help ()
  (let ((system (asdf:find-system :haptic-vicon-synchronizer)))
    (format T "~a v~a

~a

Usage:
synchronizer output [-h haptic-input] [-v vicon-input]

  output        Path to the resulting output TIDE file
  vicon-input   Path to a Vicon input file
  haptic-input  Path to a Haptic input file

An input file can be specified multiple times through repeated
usage of the option. An input file can be a TIDE file, a ZIP
containing further input files, or a CSV file (the default
assumption on an unknown file type). In the case of a TIDE file,
the option can only be specified once.

Project URL:   ~a
Maintained by: ~a
Compiled against
  ~@<~{~{~36a ~a~}~^~@:_~}~@:>
"
            (asdf:component-name system)
            (asdf:component-version system)
            (asdf:system-description system)
            (asdf:system-homepage system)
            (asdf:system-maintainer system)
            (mapcar (lambda (dep)
                      (let ((system (asdf:find-system dep)))
                        (list (asdf:component-name system)
                              (asdf:component-version system))))
                    (asdf:system-depends-on system)))))

(defun check-compatibility (files)
  (loop for file in (cdr files)
        when (string-equal (pathname-type file) "tide")
        do (error "TIDE input files may only be specified alone.")))

(defun ensure-tide (files ensure-func)
  (let ((files (mapcar #'uiop:parse-native-namestring files)))
    (cond ((string-equal (pathname-type (first files)) "tide")
           (first files))
          (T ;; I have no idea why this isn't exported.
           (let ((out (uiop/stream::get-temporary-file :type "tide")))
             (funcall ensure-func files out)
             out)))))

(defun main (&rest noop)
  (declare (ignore noop))
  (let ((args (uiop:command-line-arguments)))
    (case (length args)
      ((0 1)
       (print-help))
      (T
       (let ((output (first args))
             (haptics NIL)
             (vicons NIL))
         (loop for (option arg) on (cdr args) by #'cddr
               do (cond ((string-equal option "-h")
                         (push arg haptics))
                        ((string-equal option "-v")
                         (push arg vicons))
                        (T (print-help)
                           (return-from main))))
         (check-compatibility haptics)
         (check-compatibility vicons)
         (let ((haptic (ensure-tide haptics #'haptic-bag-translator:convert))
               (vicon (ensure-tide vicons #'vicon-bag-translator:convert)))
           (synchronize :output output :vicon vicon :haptic haptic)))))))

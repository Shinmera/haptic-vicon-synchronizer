(in-package #:haptic-vicon-synchronizer)

(defmacro interleave (&rest bodies)
  (labels ((ins (bodies)
             (if (cdr bodies)
                 (append (car bodies) (list (ins (cdr bodies))))
                 (car bodies))))
    (ins bodies)))

(defun duration (from to)
  (- (local-time:timestamp-to-universal to)
     (local-time:timestamp-to-universal from)))

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

(defun event-timestamp (event)
  )

(defun event-index (event)
  )

(defun event-payload (event)
  )

(defun maybe-update (stamp current channel)
  (if (local-time:timestamp<= stamp (event-timestamp current))
      (rsbag:entry channel (1+ (event-index current)))
      current))

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
            for currents = (mapcar #'(lambda (c) (rsbag:entry c 0)) inputs)
            then (mapcar (curry #'maybe-update stamp) currents inputs)
            do (loop for output in outputs
                     for current in currents
                     do (make-entry output (event-payload current) stamp
                                    :sequence-number i))))))

(defun synchronize (&key vicon haptic output)
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
synchronizer vicon haptic output

  vicon         Path to a vicon TIDE file
  haptic        Path to a haptic TIDE file
  output        Path to the resulting TIDE file


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

(defun main (&rest noop)
  (declare (ignore noop))
  (let ((args (uiop:command-line-arguments)))
    (case (length args)
      ((0 1 2)
       (print-help))
      (T
       (let ((args (mapcar #'uiop:parse-native-namestring args)))
         (synchronize :vicon (first args)
                      :haptic (second args)
                      :output (third args)))))))

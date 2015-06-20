(in-package #:cl-user)
(defpackage #:haptic-vicon-synchronizer
  (:nicknames #:synchronizer)
  (:use #:cl #:rsbag-helper)
  (:import-from #:vicon-bag-translator #:with-vicon-channel)
  (:import-from #:haptic-bag-translator #:with-haptic-channel)
  (:export
   #:synchronize-channels
   #:synchronize
   #:main))

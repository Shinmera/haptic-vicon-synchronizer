(in-package #:cl-user)
(asdf:defsystem haptic-vicon-synchronizer
  :version "1.0.0"
  :license "LGPL2.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Synchronize Haptic and Vicon files."
  :homepage "https://github.com/Shinmera/haptic-vicon-synchronizer/"
  :serial T
  :components ((:file "package")
               (:file "synchronizer")
               (:file "documentation"))
  :depends-on (:rsbag-helper
               :cl-rsbag
               :cl-rsb-common
               :rsbag-tidelog
               :rsb-converter-protocol-buffer
               :haptic-bag-translator
               :vicon-bag-translator)
  :build-operation asdf:program-op
  :build-pathname "synchronizer"
  :entry-point "synchronizer:main")

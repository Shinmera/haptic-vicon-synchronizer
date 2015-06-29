all: haptic-vicon-synchronizer

haptic-vicon-synchronizer: $(concat $< .asd) *.lisp
	cl                                                    \
	  -Q                                                  \
	  -S "(:source-registry                               \
	       (:directory \""$$(pwd)"\")                     \
	       (:directory \""$$(pwd)/../rsbag-helper"\")     \
	       (:directory \""$$(pwd)/../vicon-parser"\")     \
	       (:directory \""$$(pwd)/../haptic-parser"\")    \
	       :inherit-configuration)"                       \
	  -s $@                                               \
	  --dump ! --output $@ -r "haptic-vicon-synchronizer:main"

.PHONY: all

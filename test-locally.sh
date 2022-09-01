#!/bin/bash

ros install sbcl
ros install ccl-bin
ros install ecl

ros -L sbcl testscr.ros
bell
ros -L ccl-bin testscr.ros
bell
ros -L ecl testscr.ros
bell

#!/bin/sh
# Set the screen DPI (uncomment this if needed!)
# xrdb ~/.emacs.d/exwm/Xresources

# Run the screen compositor
compton &

# Enable screen locking on suspend
xss-lock -- slock &

~/.fehbg &

# Fire it up
exec dbus-launch --exit-with-session /home/joe/usr/bin/emacs -mm --debug-init -l ~/.emacs.d/desktop.el

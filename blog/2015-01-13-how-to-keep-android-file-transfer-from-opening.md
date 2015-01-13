% How to keep Android File Transfer from opening on OSX
%
% 2015-01-13

Here's a version for the lazy of the [long story](https://trjlive.com/2013/11/how-to-prevent-android-file-transfer-from-opening-each-time-a-device-is-connected/):

    killall "Android File Transfer Agent"
    rm -r "$HOME/Library/Application Support/Google/Android File Transfer/Android File Transfer Agent.app"
    touch "$HOME/Library/Application Support/Google/Android File Transfer/Android File Transfer Agent.app"

And to undo the above:

    rm "$HOME/Library/Application Support/Google/Android File Transfer/Android File Transfer Agent.app"

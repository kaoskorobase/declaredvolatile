% Building Tahoe-LAFS on Mac OSX 10.8.5
%
% 2013-09-08

I've encountered two quirks when trying to build Tahoe-LAFS on Mac OSX 10.8.5 with XCode 5.5.1.

Some packages use a flag not supported by XCode's version of clang:

    clang: error: unknown argument: '-mno-fused-madd' ...

The remedy is to build with the following environment variables set:

    $ CFLAGS=-Qunused-arguments CPPFLAGS=-Qunused-arguments \
        python setup.py build

Another quirk is that the installed version of the Twisted framework conflicts with the one required by Nevow (see also <https://github.com/twisted/nevow/issues/43>):

    error: Installed distribution Twisted 12.0.0
           conflicts with requirement twisted>=13.0

Here's what I did:

* Download [Twisted-13.0.0-py2.7-macosx-10.8-intel.egg](https://tahoe-lafs.org/trac/tahoe-lafs/raw-attachment/ticket/2001/Twisted-13.0.0-py2.7-macosx-10.8-intel.egg) to `support/lib/python2.7/site-packages/`
* Insert the dependency `./Twisted-13.0.0-py2.7-macosx-10.8-intel.egg` in `support/lib/python2.7/site-packages/easy-install.pth`, before the Nevow library (I don't know if dependency order is significant).

Then build with the above command.

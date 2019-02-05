chicken-wlroots [WIP]
=====================

Chicken Scheme bindings for wlroots.

Building
--------

Dependencies:

* CHICKEN 5
* chicken-wayland-server
* chicken-xkbcommon (for example programs)
* wlroots

Simply run <code>chicken-install</code> in this directory to build and install
chicken-wlroots as an egg.

It is also possible to build a static archive, by running
<code>make chicken-wlroots.a</code>. You will need to put the various
<code>\*.import.scm</code> files on your include path in order to import the corresponding
modules if you use static linking.

Usage
-----

If you install chicken-wlroots as an egg, you must load the wlroots extension
before importing any of its modules. The easiest way is to add
<code>-R wlroots</code> to the command line when invoking the chicken compiler.

### Memory Management

These are low level bindings. Most wlroots objects allocated via these bindings
are not garbage collected (with a few exceptions). You may set up your own
memory management scheme using <code>set-finalizer!</code> where appropriate.

The wlroots objects which *are* garbage collected are:

* wlr-box
* wlr-matrix (not actually a wlroots type, but a wrapper for a float[9])

### Naming Conventions

Module naming mimics the structure of the wlroots headers. E.g., the module
<code>(wlr types wlr-box)</code> exports all of the identifiers declared in the
header <code>"wlr/types/wlr\_box.h"</code>.

SRFI-17 getter/setter procedures are defined for accessing struct members.
They take the form <code>structname-membername</code>. For nested structs,
only getters are defined.

Constructors for wlroots types take the form <code>make-wlr-type</code>.
Constructors are only exported where it makes sense to do so (e.g. wlr-box); in
most cases you should use the <code>wlr-\*-create</code> functions provided by
wlroots.

Procedures use the usual <code>kebab-case</code> convention. For C functions
taking output parameters, the output parameters are removed (unless their input
value is also meaningful) and multiple values are returned instead. The order
of the values is the C return value, followed by the output parameters in
order.

Enums use the convention <code>enum-prefix/kind</code>, e.g.
<code>WLR\_INPUT\_DEVICE\_KEYBOARD</code> becomes
<code>wlr-input-device/keyboard</code>.

Examples
--------

Example programs can be found in the example directory. These are fairly
direct translations from C, so do not expect idiomatic Scheme.

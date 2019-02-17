chicken-wlroots [WIP]
=====================

Chicken Scheme bindings for wlroots.

Building
--------

Dependencies:

* CHICKEN 5
* [chicken-wayland-server](https://github.com/drewt/chicken-wayland-server)
* [chicken-xkbcommon](https://github.com/drewt/chicken-xkbcommon) (for example programs)
* [wlroots](https://github.com/swaywm/wlroots)

Simply run <code>chicken-install</code> in this directory to build and install
chicken-wlroots as an egg. To avoid building the egg as root, you can either
run <code>chicken-install -sudo</code> to get permissions via sudo, or else
[change the repository location](https://wiki.call-cc.org/man/5/Extension%20tools#changing-the-repository-location)
to a directory that you can write to.

Usage
-----

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

Procedures use the usual <code>kebab-case</code> convention.

Enums use the convention <code>enum-prefix/kind</code>, e.g.
<code>WLR\_INPUT\_DEVICE\_KEYBOARD</code> becomes
<code>wlr-input-device/keyboard</code>.

### Differences from the C API

For C functions taking output parameters, these bindings remove the output
parameters (unless their input value is also meaningful) and return multiple
values instead. The order of the returned values is the C return value,
followed by the output parameters in order.

Because some wlroots functions take a <code>struct timespec</code> as an
argument, these bindings provide a module <code>(wlr time)</code> which exports
the <code>clock-gettime</code> function.

Examples
--------

Example programs can be found in the src/examples directory. These are fairly
direct translations from C, so do not expect idiomatic Scheme.

To build the examples, run <code>make</code> after installing chicken-wlroots.

DeepProps
=========
@ https://keynslug.github.com/deepprops

A set of utility routines used to operate on deep proplists.

Overview
--------

**DeepProps** is actually a single module which is designed to hit three primary goals:

 - allow you to access deeply nested properties inside proplists;
 - admit you to mutate proplists;
 - allow you to access a group of properties with a single call.
 
In fact the interface is very clean and you may consult module documentation at any time for the further details. 

Building and configuring
------------------------

In order to build a project you should execute something like that:
```
git clone git://github.com/keynslug/deepprops.git
cd deep
make compile
```

Though that way this is useless so consider including it in your projects say as a rebar dependency:
```
{'deep_props', ".*", {git, "git://github.com/keynslug/deepprops.git", "master"}}
```

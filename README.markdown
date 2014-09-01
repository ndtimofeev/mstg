The Mysterious Spineless Tagless G-machine
==========================================

MSTG is an translator from simple lazy functional STG-machine language to a Lua
language.

Motivation
----------

* Reading too much papers about lazy language implementation.
* Writing tools for running lazy functional language as embeded scripting language.
* Investigation platform specific optimisation and implementation details for
STG.

What work
---------

* Graph rewriting.
* Partial apply.
* Factorial computation.

What does not work
------------------

* Indentation based input language syntax.

Usage
-----

    mstgc fact.stg num.stg high.stg | lua -
    mstgc fact.stg num.stg high.stg | luajit -
    mstgc -e fact.stg num.stg high.stg

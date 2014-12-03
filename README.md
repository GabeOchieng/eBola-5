eBola
=====

Final project for COMP50 - Concurrent Programming at Tufts University.
Created by Rob Ruenes, Paul Chang, and Hyung-seo Park.

How to use:
=========== 
start erl interpreter

1> c(ebola_server).

2> c(patient).

3> ebola_server:start().

Important files:
================

frontend.py : Contains the two python processes for the front end gui

ebola_server.erl : Contains the server backend that is used to start the 
program

patient.erl : Contains an individual erlang patient process used for the
backend.

Dependencies
============

 Must have erlang and python. Must install erlport and pygame.

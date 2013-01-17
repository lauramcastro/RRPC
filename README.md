RRPC: REST Reverse Polish Calculator
====================================

This is a pet project to build a reverse polish calculator that offers a pure REST API.

If you are familiar with REST, you have probably notice to some extent that most of the self-claimed
REST APIs out there are

* either not REST at all (at best, as [Roy Fielding himself calls them, they are REST-RPC hybrids](http://roy.gbiv.com/untangled/2008/rest-apis-must-be-hypertext-driven))
* storage services

for research purposes, I am interested in testing REST APIs, and I would like to see alternative
examples that do not fall in either of the categories above. Thus, if you know of any examples,
please point me to them!


Build and run RRPC
------------------

To build this code, run the following command:

erlc -o ./ebin ./src/*.erl ./test/*.erl

To run the program, first start Erlang like this:

erl -pa ./ebin

Then, run the following in the Erlang shell:

1> application:start(rrpc).


Run QuickCheck tests
--------------------

For the time being, this requires a [Quviq QuickCheck license](http://www.quviq.com).

After building and running RRPC, type on the Erlang shell:

2> eqc:quickcheck(rrpc_eqc:prop_rrpc()).

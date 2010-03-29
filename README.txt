**************************************************************************
*                                                                        *
*  Functory: a distributed computing library for Ocaml                   *
*  Copyright (C) 2010 Jean-Christophe Filliatre and Kalyan Krishnamani   *
*                                                                        *
*  This software is free software; you can redistribute it and/or        *
*  modify it under the terms of the GNU Library General Public           *
*  License version 2.1, with the special exception on linking            *
*  described in file LICENSE.                                            *
*                                                                        *
*  This software is distributed in the hope that it will be useful,      *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
*                                                                        *
**************************************************************************

This is Factory mini tutorial.

Installation
============

	./configure

	make

	sudo make install


Usage
=====

Assume you want to apply a function "map" such as

  let map x = x+1

to some list elements such as 

  [1;2;3;4;5]

and sum the results with a function "fold" such as

  let fold = (+)

You can do that using function "map_local_fold" from the library, as follows:

  let () = Printf.printf "%d@." (map_local_fold ~map ~fold 0 [1;2;3;4;5])


The factory library allows you to perform this computation in three
different ways: either sequentially, or using several cores on the
same machine, or using a network of different machines.

To use the sequential implementation, you simply use the following
line of code

  open Factory.Sequential

To use several cores (say 4) on a single machine, you should add
instead

  open Factory.Cores
  let () = set_number_of_cores 4

Finally, to use a network of, say 2 cores on machine "mach1" and 4
cores on machine "mach2", you should add instead

  open Factory.Network
  let () = declare_workers ~n:2 "mach1"
  let () = declare_workers ~n:4 "mach2"

Your program is compiled in the following way (in any case):

	ocamlopt -I +factory unix.cmxa factory.cmxa <your files...>

and then run as usual. In the network case, the same program should be
run on the three machines, that are the two workers and the master. 
(It is also possible to run different programs for master and workers;
see the documentation).


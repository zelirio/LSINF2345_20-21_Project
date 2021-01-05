# Comments
- sources are well organized and with clear comments
- comments in erl sources help to understand how did you implement the PS service
-
- I also appreciate the brief discussion about the curves you present, although, it is clear that there is a lack of argumentation that explains the behavior of curves, you describe how the curves behave with no further details. Here one example of how to do it: *the observed variance of in-degree when nodes recover reflects that the in-degree is not equally balanced among all nodes in the network (as shown before nodes crash); or even simpler, there might be partitions where one observe that certain clusters contain more nodes than others*

- no script is provided to deploy the network and/or plotting procedure
- impossible to execute the submitted version, the reason is that you didn't export the function `binaryTree:start_link()` (see error below). After a couple of fixes (see my commit) the code runs smoothly

```bash
erl -noshell -eval 'main:start2()' -s init stop
{"init terminating in do_boot",{undef,[{binaryTreeServer,start_link,[server],[]},{main,init,6,[{file,"main.erl"},{line,90}]},{main,start,6,[{file,"main.erl"},{line,84}]},{erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,670}]},{init,start_it,1,[]},{init,start_em,1,[]},{init,do_boot,3,[]}]}}
init terminating in do_boot ()

```

- error in python script
```bash
$ python statLog.py
Traceback (most recent call last):
  File "statLog.py", line 26, in <module>
    data.close()
AttributeError: 'list' object has no attribute 'close'
```

# Grade
| Bootstrap network (20%) | PS service implementation (50%) | Experimental scenario (30%) | Grade in % | Points (up to 5) |
|---|---|---|---|---|
|20 |	50 |	20 |	90 |	4.5|

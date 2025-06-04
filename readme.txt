Core implementation in sh2cpue.vhdAdd commentMore actions
Extra credit of sh2 mutli-bit shift instructions implemented
Mul instructions implmented with * operator to utilize artix 7 built-in dsp multiplier

See cpu_dataflow_and_signals.jpg, alu_dataflow.jpg, addr_unit_dataflow.jpg for the detailed wiring/signal definition

Run simulation:
$ make
memory.vhd:170:9:@9us:(report note): 256/256 matches

Tested with program compiled from test_mem_content/program.asm, covering branch, alu and mov commands.
Final memory content compared with simulation results.


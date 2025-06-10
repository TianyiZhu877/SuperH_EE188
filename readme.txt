Core implementation in sh2cpue.vhdAdd commentMore actions
Extra credit of sh2 mutli-bit shift instructions implemented
Mul instructions implmented with * operator to utilize artix 7 built-in dsp multiplier

See cpu_dataflow_and_signals.jpg, alu_dataflow.jpg, addr_unit_dataflow.jpg for the detailed wiring/signal definition.
See instructions.pdf for the supported instructions. Those marked in red are not implemented.

Run simulation:
Choose the test to run in memory.vhd:186
$ make
memory.vhd:170:9:(report note): 256/256 matches
data transfer test (xfern) would fail three datapoints due to the lack of MOV.B/W R0 <-> @(disp, Rn) commands

Tested with program compiled from test_mem_content/program.asm, covering branch, alu and mov commands.
Final memory content compared with simulation results.

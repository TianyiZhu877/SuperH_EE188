GDHL=ghdl
FLAGS=--std=08 # --ieee=synopsys

SOURCES_ADDRESSING = mau_general.vhd data_addr_unit.vhd program_addr_unit.vhd
SOURCES_RAM_ROUTING = ram_routing.vhd
SOURCES_REG = reg_general.vhd regfile.vhd
SOURCES_ALU = alu_general.vhd ALU.vhd

ALL_SOURCES = $(SOURCES_ADDRESSING)  \
			$(SOURCES_RAM_ROUTING) \
			$(SOURCES_REG)	 \
			$(SOURCES_ALU) \
			sh2cpue.vhd

TB_FILES = memory.vhd sh2cpu_tb.vhd

all:  tb

tb:
	@$(GDHL) -a $(FLAGS) $(ALL_SOURCES) $(TB_FILES)
	@$(GDHL) -e $(FLAGS) SH2_CPU_tb
	@$(GDHL) -r $(FLAGS) SH2_CPU_tb   --stop-time=10us  --vcd=wave.vcd --stop-delta=100000 
	--wave=wave.ghw

data_addr_unit:
	@$(GDHL) -a $(FLAGS) $(SOURCES_ADDRESSING)
	@$(GDHL) -e $(FLAGS) dataAddrUnit
	@$(GDHL) -r $(FLAGS) dataAddrUnit

program_addr_unit:
	@$(GDHL) -a $(FLAGS) $(SOURCES_ADDRESSING)
	@$(GDHL) -e $(FLAGS) programAddrUnit
	@$(GDHL) -r $(FLAGS) programAddrUnit

memory:
	@$(GDHL) -a $(FLAGS) $(SOURCES_RAM_ROUTING)
	@$(GDHL) -e $(FLAGS) RAM
	@$(GDHL) -r $(FLAGS) RAM

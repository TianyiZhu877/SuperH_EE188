GDHL=ghdl
FLAGS=--std=08 --ieee=synopsys

all: data_addr_unit program_addr_unit

data_addr_unit:
	@$(GDHL) -a $(FLAGS) mau_general.vhd data_addr_unit.vhd 
	@$(GDHL) -e $(FLAGS) dataAddrUnit
	@$(GDHL) -r $(FLAGS) dataAddrUnit

program_addr_unit:
	@$(GDHL) -a $(FLAGS) mau_general.vhd program_addr_unit.vhd 
	@$(GDHL) -e $(FLAGS) programAddrUnit
	@$(GDHL) -r $(FLAGS) programAddrUnit

memory:
	@$(GDHL) -a $(FLAGS) memory_general.vhd memory.vhd 
	@$(GDHL) -e $(FLAGS) RAM
	@$(GDHL) -r $(FLAGS) RAM

----------------------------------------------------------------------------
--
--  Hitachi SH-2 CPU Entity Declaration
--
--  This is the entity declaration for the complete SH-2 CPU.  The design
--  should implement this entity to make testing possible.
--
--  Revision History:
--     28 Apr 25  Glen George       Initial revision.
--
----------------------------------------------------------------------------


--
--  SH2_CPU
--
--  This is the complete entity declaration for the SH-2 CPU.  It is used to
--  test the complete design.
--
--  Inputs:
--    Reset  - active low reset signal
--    NMI    - active falling edge non-maskable interrupt
--    INT    - active low maskable interrupt
--    clock  - the system clock
--
--  Outputs:
--    AB     - memory address bus (32 bits)
--    RE0    - first byte read signal, active low
--    RE1    - second byte read signal, active low
--    RE2    - third byte read signal, active low
--    RE3    - fourth byte read signal, active low
--    WE0    - first byte write signal, active low
--    WE1    - second byte write signal, active low
--    WE2    - third byte write signal, active low
--    WE3    - fourth byte write signal, active low
--
--  Inputs/Outputs:
--    DB     - memory data bus (32 bits)
--

library ieee;
use ieee.std_logic_1164.all;
-- use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use work.RamAccessMode.all;

--library opcodes;
--use opcodes.opcodes.all;


entity  SH2_CPU  is

    port (
        Reset   :  in     std_logic;                       -- reset signal (active low)
        NMI     :  in     std_logic;                       -- non-maskable interrupt signal (falling edge)
        INT     :  in     std_logic;                       -- maskable interrupt signal (active low)
        clk   :  in     std_logic;                       -- system clock
        AB      :  out    std_logic_vector(31 downto 0);   -- memory address bus
        RE0     :  out    std_logic;                       -- first byte active low read enable
        RE1     :  out    std_logic;                       -- second byte active low read enable
        RE2     :  out    std_logic;                       -- third byte active low read enable
        RE3     :  out    std_logic;                       -- fourth byte active low read enable
        WE0     :  out    std_logic;                       -- first byte active low write enable
        WE1     :  out    std_logic;                       -- second byte active low write enable
        WE2     :  out    std_logic;                       -- third byte active low write enable
        WE3     :  out    std_logic;                       -- fourth byte active low write enable
        DB      :  inout  std_logic_vector(31 downto 0);    -- memory data bus

        -- debug input signals
        PC_reset_addr_debug      :  in  std_logic_vector(31 downto 0);    
        -- debug probe signals
        IR_debug :  out  std_logic_vector(15 downto 0);
        PC_debug: out  std_logic_vector(31 downto 0);
        ram_data_address_debug: out  std_logic_vector(31 downto 0);
        PC_LD_sel_debug: out integer range 0 to 3; 
        ram_data_read_debug : out std_logic_vector(31 downto 0);
        reg_out_a_debug : out std_logic_vector(31 downto 0);
        exception_debug : out std_logic

    );

end  SH2_CPU;




architecture  structural  of  SH2_CPU  is
    component RAMRouting
        port (
            clk          : in    std_logic;  
            EN          : in    std_logic;      -- 1 for enable, 0 for disable
            PD          : in    std_logic;      -- 0 for program, 1 for data memory 
            RW          : in    std_logic;      -- 0 for read, 1 for write, force read if program
            access_mode : in    std_logic_vector(1 downto 0);       -- force WORD_ACCESS if program     
            program_address      :  in    std_logic_vector(31 downto 0);   -- memory address bus
            data_address      :  in    std_logic_vector(31 downto 0);   -- memory address bus
        
            write_data :  in  std_logic_vector(31 downto 0);
            DB      :  inout  std_logic_vector(31 downto 0);
            read_data  :  out  std_logic_vector(31 downto 0);
            AB  :  out  std_logic_vector(31 downto 0);

            RE0     :  out    std_logic;                       -- first byte active low read enable
            RE1     :  out    std_logic;                       -- second byte active low read enable
            RE2     :  out    std_logic;                       -- third byte active low read enable
            RE3     :  out    std_logic;                       -- fourth byte active low read enable
            WE0     :  out    std_logic;                       -- first byte active low write enable
            WE1     :  out    std_logic;                       -- second byte active low write enable
            WE2     :  out    std_logic;                       -- third byte active low write enable
            WE3     :  out    std_logic;                       -- fourth byte active low write enable
            exception   : out   std_logic
        );
    end component;

    component  dataAddrUnit
        port (
        -- inputs for base addr
            SrcSel      : in    integer  range 3 downto 0;       -- singal for selection 
            PC          : in     std_logic_vector(31 downto 0);  -- selected when 0
            Rn          : in     std_logic_vector(31 downto 0);   -- selected when 1
            GBR         : in     std_logic_vector(31 downto 0);   -- selected when 2
        -- inputs for offset
            OffsetSel   : in    integer  range 4 downto 0;       -- singal for selection 
            R0          : in     std_logic_vector(31 downto 0);  -- selected when 0
            IncDecVal   :  in     std_logic_vector(3 downto 0);
            -- selected when +-2 (no shift), +-3 (shift 1), +-4 (shift 2), and 0 for some operations
            Disp        : in     std_logic_vector(7 downto 0);  
            -- choose to use last 0 (4 bits), 1 (8 bits) of disp
            DispCutoff  : in    integer  range 1 downto 0;       
        -- signals from control unit, directly connect to the wrapped general mau
            PrePostSel : in      std_logic;
        -- outputs, directly connects to the wrapped general mau output
            Address    : out     std_logic_vector(31 downto 0);
            AddrSrcOut : buffer  std_logic_vector(31 downto 0)
        );
    end component;

    component  programAddrUnit
        port (
        -- inputs for base addr
            SrcSel      : in    integer  range 1 downto 0;       -- singal for selection 
            PC          : in     std_logic_vector(31 downto 0);  -- selected when 0
        -- inputs for offset
            OffsetSel   : in    integer  range 2 downto 0;       -- singal for selection 
            Rm          : in     std_logic_vector(31 downto 0);  -- selected when 0
            -- selected when +-2 (no shift), +-3 (shift 1), +-4 (shift 2), and 0 for some operations
            Disp        : in     std_logic_vector(11 downto 0);  
            -- choose to use last 0 (4 bits), 1 (8 bits) of disp
            DispCutoff  : in    integer  range 1 downto 0;       
        -- signals from control unit, directly connect to the wrapped general mau
            PrePostSel : in      std_logic;
        -- outputs, directly connects to the wrapped general mau output
            Address    : out     std_logic_vector(31 downto 0);
            AddrSrcOut : buffer  std_logic_vector(31 downto 0)
        );
    end component;

    component ALU
        port (
            ALUOpA   : in      std_logic_vector(31 downto 0);   -- first operand
            ALUOpB   : in      std_logic_vector(31 downto 0);   -- second operand
            immd   : in  std_logic_vector(7 downto 0);   -- immediate value (IR 7-0)
            T        : in      std_logic;                       -- T flag 
    
            op_a_sel   : in    std_logic;
            op_b_sel   : in    integer  range 0 to 2;
            adder_cin_sel   : in    integer  range 0 to 2;
            
            AddSub   : in      std_logic;                       -- 1 for add, 0 for sub, always the 2nd bit of IR for SH2!  
            ALUCmd   : in      std_logic_vector(1 downto 0);    -- ALU result select
            FCmd     : in      std_logic_vector(3 downto 0);    -- F-Block operation
            SCmd     : in      std_logic_vector(2 downto 0);    -- shift operation
            
            Result   : buffer  std_logic_vector(31 downto 0);   -- ALU result
            C     : out     std_logic;                       -- carry out
            V : out     std_logic;                        -- overflow
            S : out     std_logic;                        -- sign
            Z : out     std_logic                        -- zero                      -- signed overflow
        );
    end component;

    component RegFile
        port (
            clk        : in  std_logic;                         -- clock
            data_in    : in  std_logic_vector(31 downto 0);     -- the data to write to a register
            write_en   : in  std_logic;                         -- write the data if 1
            write_sel  : in  integer range 15 downto 0;         -- select which reg to write to
            read_sel_a : in  integer range 15 downto 0;         -- select the first register to output
            read_sel_b : in  integer range 15 downto 0;         -- select the second register to output
            data_out_a : out std_logic_vector(31 downto 0);     -- the data output for the first register
            data_out_b : out std_logic_vector(31 downto 0);     -- the data output for the second register
            data_out_0 : out std_logic_vector(31 downto 0)      -- the data output for the second register
        );
    end component;

    
-- control signals
    -- memory
    signal SrcSel_P      :    integer  range 1 downto 0;       -- singal for selection 
    signal OffsetSel_P   :    integer  range 2 downto 0;       -- singal for selection 
    signal DispCutoff_P  :    integer  range 1 downto 0;       
    signal PrePostSel_P :      std_logic;
    signal SrcSel_D      :    integer  range 1 downto 0;       -- singal for selection 
    signal IncDecVal_D   :     std_logic_vector(3 downto 0);
    signal OffsetSel_D   :    integer  range 2 downto 0;       -- singal for selection 
    signal DispCutoff_D  :    integer  range 1 downto 0;       
    signal PrePostSel_D :      std_logic;
    -- ALU
    signal alu_op_a_sel   : std_logic;
    signal alu_op_b_sel: integer  range 0 to 2;
    signal adder_cin_sel : integer  range 0 to 2;
    signal AddSub   :   std_logic;                       -- 1 for add, 0 for sub, always the bit2 or 1 when 0111!  
    signal ALUCmd   :   std_logic_vector(1 downto 0);    -- ALU result select
    signal FCmd     :   std_logic_vector(3 downto 0);    -- F-Block operation
    signal SCmd     :   std_logic_vector(2 downto 0);    -- shift operation
    signal ALU_opA_bypass: std_logic;
    -- register
    signal reg_read_a_mux : std_logic;         -- select the first register to output
    signal reg_read_b_mux : integer range 0 to 2;         -- select the first register to output
    signal reg_write_addr_mux : integer range 0 to 2;         -- select the register to write
    signal reg_write_in_mux : integer range 0 to 3;         -- select the register to write
    signal reg_write_in_sel_regs : integer range 0 to 7;         -- select when reg_write_in_mux = 3
    signal reg_write_en: std_logic;
    -- control register
    -- signal LD_PC: std_logic;
    signal LD_PR: std_logic;
    signal LD_IR: std_logic;
    signal T_LD_sel: integer range 0 to 4;
    signal LD_S: std_logic;                             -- unused
    signal LD_SR_I: std_logic;                          -- unused
    signal LD_GBR: std_logic;
    signal PC_LD_sel : integer range 0 to 3;         -- select the register to write
    -- signal GBR_LD_sel : integer range 0 to 2;         -- select the register to write
    -- RAM routing
    signal ram_EN          : std_logic;      -- 1 for enable, 0 for disable
    signal ram_PD          : std_logic;      -- 0 for program, 1 for data memory 
    signal ram_RW          : std_logic;      -- 0 for read, 1 for write, force read if program
    signal ram_access_mode : std_logic_vector(1 downto 0);       -- force WORD_ACCESS if program 


-- pipeline registers
    signal opcode: std_logic_vector(15 downto 0);
    signal PC_EX: std_logic_vector(31 downto 0);   -- address for a program currently at executiong stage
-- registers
    signal PC: std_logic_vector(31 downto 0);
    signal PR: std_logic_vector(31 downto 0);
    signal IR: std_logic_vector(15 downto 0);
    signal GBR: std_logic_vector(31 downto 0);
    signal SR_I:  std_logic_vector(3 downto 0);
    signal SR_S:  std_logic;
    signal SR_T:  std_logic;
    signal SR: std_logic_vector(31 downto 0);

-- interconnect signals
    signal program_address      : std_logic_vector(31 downto 0);   -- program address bus
    signal ram_data_address         : std_logic_vector(31 downto 0);   -- data address bus
    signal program_addr_src_out : std_logic_vector(31 downto 0);
    signal PC_pre                : std_logic_vector(31 downto 0);
    -- signal write_data : std_logic_vector(31 downto 0);
    -- signal read_data  : std_logic_vector(31 downto 0);
    signal ram_data_read : std_logic_vector(31 downto 0);
    -- register signals
    signal reg_sel_a : integer range 15 downto 0;         -- select the first register to output
    signal reg_sel_b : integer range 15 downto 0;         -- select the second register to output
    signal reg_out_a : std_logic_vector(31 downto 0);     -- the data output for the first register
    signal reg_out_b : std_logic_vector(31 downto 0);      -- the data output for the second register
    signal reg_out_R0 : std_logic_vector(31 downto 0);      -- the data output for the second register
    signal reg_write_in: std_logic_vector(31 downto 0);
    signal reg_in_reg_sel_result: std_logic_vector(31 downto 0);
    signal reg_write_addr: integer range 15 downto 0;
    signal reg_read_b: integer range 15 downto 0;
    signal reg_read_a: integer range 15 downto 0;
    -- alu flags
    signal ALU_C: std_logic;
    signal ALU_V: std_logic;
    signal ALU_S: std_logic;
    signal ALU_Z: std_logic;
    signal SR_T_in: std_logic;
    signal SR_S_in: std_logic;
    signal ALU_cout         : std_logic;
    signal ALU_overflow         : std_logic;


-- register saved for pipelining, first state are combinational
-- execute stage generated signals
    signal ALU_result_EX         : std_logic_vector(31 downto 0);
    signal ALU_result_WB         : std_logic_vector(31 downto 0);
    signal data_addr_writeback_EX  : std_logic_vector(31 downto 0);
    signal data_addr_writeback_WB  : std_logic_vector(31 downto 0);
-- decode stage generated signals
    signal reg_out_a_WB:        std_logic_vector(31 downto 0);
    signal reg_out_b_WB:        std_logic_vector(31 downto 0);

-- exceptions from components
    signal ram_exception : std_logic;    

-- for four stage pipeline
    -- type controlUnitStates is (FE, DE, EX, WB);
    signal state : std_logic_vector(1 downto 0);

begin
-- connecting debug lines
    IR_debug <= IR;
    PC_debug <= PC;
    ram_data_address_debug <= ram_data_address;
    PC_LD_sel_debug <= PC_LD_sel; 
    ram_data_read_debug <= ram_data_read;
    reg_out_a_debug <= reg_out_a;
    exception_debug <= ram_exception;


-- PC increment
    PC_pre <= PC when PC_LD_sel = 0 else
            std_logic_vector(unsigned(PC)+2)  when PC_LD_sel = 1  else
            program_addr_src_out  when PC_LD_sel = 2  else
            PR  when PC_LD_sel = 3  else
            PC_reset_addr_debug;

    process(clk) begin
        if rising_edge(clk) then

            if (reset = '1') then 
            
-- simple FSM, loop between the three states for unpipelined
-- the DE and EX is also merged!!!
                case state is
                    when "00" =>
                        state <= "10";
                    -- when "01" =>
                    --     state <= "10";
                    when "10" =>
                        state <= "11";
                    when "11" =>
                        state <= "00";
                    when others =>
                        state <= "00";
                end case;


    -- control registers
                if (LD_PR = '1') then
                    PR <= PC;
                elsif (LD_PR = '0') then
                    PR <= PR;
                else
                    PR <= (others => 'X');
                end if;

                if (LD_IR = '1') then
                    IR <= ram_data_read(15 downto 0);
                elsif (LD_IR = '0') then
                    IR <= IR;
                else
                    IR <= (others => 'X');
                end if;

                if (LD_GBR) then
                    GBR <= reg_out_b_WB;
                else
                    GBR <= GBR;
                end if;


    -- ALU results
                reg_out_a_WB <= reg_out_a;
                reg_out_b_WB <= reg_out_b;
                ALU_result_WB <= ALU_result_EX;
                -- ALU_cout_1 <= ALU_cout_0;
                -- ALU_overflow_1 <= ALU_overflow_0;
                data_addr_writeback_WB <= data_addr_writeback_EX;


    -- pipelining register update
                PC <= PC_pre;
                PC_EX <= PC;

            else
                PC <= PC_reset_addr_debug;
                state <= "00";
                SR_S <= 'X';
                SR_T <= 'X';
                SR_I <= "XXXX";
            end if;
                 
                -- if (LD_S) then
                --     SR_S <= '0';            
                -- end if;
                
    -- SR       to-do: add branch for sel=1!!!
            if (T_LD_sel = 0) then
                SR_T <= '0';
            elsif  (T_LD_sel = 2) then
                SR_T <= reg_out_b(0);
            elsif  (T_LD_sel = 3) then
                SR_T <= '0';
            elsif  (T_LD_sel = 4) then
                SR_T <= '1';
            else
                SR_T <= 'X'; 
            end if;

        end if;
    end process;
    
    SR <= (31 downto 8 => '0') & SR_I & "00" & SR_S & SR_T;


    -- only for unpipelined
    opcode <= ram_data_read(15 downto 0) when state = "10" else
                    IR;
    -- opcode <= IR;

-- decoding
    process(all) begin
        
        SrcSel_P <= 0;       -- singal for selection 
        OffsetSel_P <= 0;       -- singal for selection 
        DispCutoff_P <= 0;       
        PrePostSel_P <= '0';
        SrcSel_D <= 0;       -- singal for selection 
        IncDecVal_D <= "0000";
        OffsetSel_D <= 0;       -- singal for selection 
        DispCutoff_D <= 0;       
        PrePostSel_D <= '0';
        -- ALU
        alu_op_a_sel <= '0'; 
        alu_op_b_sel <= 0;
        adder_cin_sel <= 0;
        AddSub <= '0';                       -- 1 for add, 0 for sub, always the bit2 or 1 when 0111!  
        ALUCmd <= "00";    -- ALU result select
        FCmd <= "0000";    -- F-Block operation
        SCmd <= "000";    -- shift operation
        ALU_opA_bypass <= '0';
        -- register
        reg_read_a_mux <= '0';         -- select the first register to output
        reg_read_b_mux <= 0;         -- select the first register to output
        reg_write_addr_mux <= 0;         -- select the register to write
        reg_write_in_mux <= 0;         -- select the register to write
        reg_write_in_sel_regs <= 0;
        reg_write_en <= '0';
        -- control register
        LD_PR <= '0';
        LD_IR <= '0';
        T_LD_SEL <= 0;
        LD_S <= '0';
        PC_LD_sel <= 0;         -- select the register to write, to-do: for unpipelined, increment in decode stage, 
        -- but for pipelined, increment almost all the time, thus needs reset address to one previous first command!
        LD_GBR <= '0';         -- select the register to write
        LD_PR <= '0';         -- select the register to write
        ram_EN <= '0';      -- 1 for enable, 0 for disable
        ram_PD <= '0';      -- 0 for program, 1 for data memory 
        ram_RW  <= '0';      -- 0 for read, 1 for write, force read if program
        ram_access_mode <= "00";       -- force WORD_ACCESS if program 

        if (state = "00") then
            ram_EN <= '1';
            ram_RW <= '0';
            ram_access_mode <= WORD_ACCESS;
            ram_PD <= '0';
        end if;

    -- to-do: unpipelined only!
        if (state = "10") then
            PC_LD_sel <= 1;
            LD_IR <= '1';
        end if;

        case opcode(15 downto 12) is
            when "0000" =>
                if (opcode(3 downto 0) = "0010") then 
                    if (state = "11") then
                        if (opcode(7 downto 4) = "0000") then
                        -- STC SR, Rn
                            reg_write_in_sel_regs <= 1;
                            reg_write_en <= '1';
                        
                        elsif (opcode(7 downto 4) = "0001") then
                            -- STC GBR, Rn
                            reg_write_in_sel_regs <= 2;
                            reg_write_en <= '1';

                        elsif (opcode(7 downto 4) = "0010") then
                        -- STC VBR, Rn
                            reg_write_in_sel_regs <= 3;
                            reg_write_en <= '1';
                        end if;


                        reg_write_in_mux <= 3;
                        reg_write_addr_mux <= 0;
                    end if;

                elsif (opcode(3 downto 0) = "0100") then 
                    if (state = "10") then
                        reg_read_a_mux <= '0';
                        SrcSel_D <= 0;
                        OffsetSel_D <= 0;
                        PrePostSel_D <= '0';
                        ram_RW <= '1';
                        ram_PD <= '1';
                        ram_EN <= '1';
                        reg_read_b_mux <= 1;
                        ram_access_mode <= BYTE_ACCESS;
                    end if;


                        
                elsif (opcode(3 downto 0) = "0100") then 
                    if (state = "10") then
                        reg_read_a_mux <= '0';
                        SrcSel_D <= 0;
                        OffsetSel_D <= 0;
                        PrePostSel_D <= '0';
                        ram_RW <= '1';
                        ram_PD <= '1';
                        ram_EN <= '1';
                        reg_read_b_mux <= 1;
                        ram_access_mode <= BYTE_ACCESS;
                    end if;

                
                elsif (opcode(3 downto 0) = "0101") then 
                    if (state = "10") then
                        reg_read_a_mux <= '0';
                        SrcSel_D <= 0;
                        OffsetSel_D <= 0;
                        PrePostSel_D <= '0';
                        ram_RW <= '1';
                        ram_PD <= '1';
                        ram_EN <= '1';
                        reg_read_b_mux <= 1;
                        ram_access_mode <= WORD_ACCESS;
                    end if;

                        
                
                elsif (opcode(3 downto 0) = "0110") then 
                    if (state = "10") then
                        reg_read_a_mux <= '0';
                        SrcSel_D <= 0;
                        OffsetSel_D <= 0;
                        PrePostSel_D <= '0';
                        ram_RW <= '1';
                        ram_PD <= '1';
                        ram_EN <= '1';
                        reg_read_b_mux <= 1;
                        ram_access_mode <= LONG_ACCESS;
                    end if;


                elsif (opcode(3 downto 0) = "1100") then 
                    if (state = "10") then
                        reg_read_a_mux <= '1';
                        SrcSel_D <= 0;
                        OffsetSel_D <= 0;
                        PrePostSel_D <= '0';
                        ram_RW <= '0';
                        ram_PD <= '1';
                        ram_EN <= '1';
                        ram_access_mode <= BYTE_ACCESS;
                    elsif (state = "11") then
                        reg_write_addr_mux <= 0;
                        reg_write_en <= '1';
                        reg_write_in_mux <= 0;
                    end if;
                    
                elsif (opcode(3 downto 0) = "1101") then 
                    if (state = "10") then
                        reg_read_a_mux <= '1';
                        SrcSel_D <= 0;
                        OffsetSel_D <= 0;
                        PrePostSel_D <= '0';
                        ram_RW <= '0';
                        ram_PD <= '1';
                        ram_EN <= '1';
                        ram_access_mode <= WORD_ACCESS;
                    elsif (state = "11") then
                        reg_write_addr_mux <= 0;
                        reg_write_en <= '1';
                        reg_write_in_mux <= 0;
                    end if;


                        
                elsif (opcode(3 downto 0) = "1110") then 
                    if (state = "10") then
                        reg_read_a_mux <= '1';
                        SrcSel_D <= 0;
                        OffsetSel_D <= 0;
                        PrePostSel_D <= '0';
                        ram_RW <= '0';
                        ram_PD <= '1';
                        ram_EN <= '1';
                        ram_access_mode <= LONG_ACCESS;
                    elsif (state = "11") then
                        reg_write_addr_mux <= 0;
                        reg_write_en <= '1';
                        reg_write_in_mux <= 0;
                    end if;

                    
                elsif (opcode(3 downto 0) = "1010") then 
                    if (state = "11") then
                        if (opcode(7 downto 4) = "0000") then
                        -- STC MACH, Rn
                            reg_write_in_sel_regs <= 4;
                            reg_write_en <= '1';
                        
                        elsif (opcode(7 downto 4) = "0001") then
                        -- STC MACHL, Rn
                            reg_write_in_sel_regs <= 5;
                            reg_write_en <= '1';

                        elsif (opcode(7 downto 4) = "0010") then
                        -- STC PR, Rn
                            reg_write_in_sel_regs <= 6;
                            reg_write_en <= '1';
                        end if;

                        reg_write_in_mux <= 3;
                        reg_write_addr_mux <= 0;
                    end if;
                    
                elsif (opcode(3 downto 0) = "1010") then 
                    if (state = "11") then
                        if (opcode(7 downto 4) = "0010") then
                        -- MOVT Rn
                            reg_write_in_sel_regs <= 7;
                            reg_write_en <= '1';
                            reg_write_in_mux <= 3;
                            reg_write_addr_mux <= 0;
                        end if;
                    end if;

                elsif (opcode(3 downto 0) = "1011") then 
                    if (opcode(7 downto 4) = "0001") then
                        -- SLEEP   0000 0000 0001 1011
                        if (state = "10") then
                            PC_LD_sel <= 0;
                        end if;
                    end if;


                        
                    



                end if;
            
            
            when others =>
                null;
        end case;
    
    end process;


-- register input select
    reg_read_a  <= to_integer(unsigned(opcode(11 downto 8))) when  reg_read_a_mux = '0'  else
                to_integer(unsigned(opcode(7 downto 4))) when   reg_read_a_mux = '1'  else
                10;     
    reg_read_b <= to_integer(unsigned(opcode(11 downto 8))) when  reg_read_b_mux = 0  else
                to_integer(unsigned(opcode(7 downto 4))) when   reg_read_b_mux = 1  else
                0 when   reg_read_b_mux = 2  else
                11;  
    reg_write_addr <= to_integer(unsigned(opcode(11 downto 8))) when  reg_write_addr_mux = 0  else
            to_integer(unsigned(opcode(7 downto 4))) when   reg_write_addr_mux = 1  else
             0 when   reg_write_addr_mux = 2  else
            12;              -- to-do: change to opcode_WB for pipelined!!!

    reg_in_reg_sel_result <= reg_out_b_WB when reg_write_in_sel_regs = 0 else
                            SR when reg_write_in_sel_regs = 1 else
                            GBR when reg_write_in_sel_regs = 2 else
                            PR when reg_write_in_sel_regs = 6 else
                            (31 downto 1 => '0') & SR_T when reg_write_in_sel_regs = 7 else
                            (others => 'X');

    reg_write_in <= data_addr_writeback_WB when reg_write_in_mux = 0 else
                ALU_result_WB when reg_write_in_mux = 1 else
                ram_data_read when reg_write_in_mux = 2 else
                reg_in_reg_sel_result when reg_write_in_mux = 3 else
                (others => 'X');

-- connecting components

    data_addr_unit:  dataAddrUnit
        port map (
        -- inputs for base addr
            SrcSel      => SrcSel_D,       -- singal for selection 
            PC          => PC,  -- to-do: might be changed later
            Rn          => reg_out_a,   -- selected when 1
            GBR         => GBR,   -- selected when 2
        -- inputs for offset
            OffsetSel   => OffsetSel_D,       -- singal for selection 
            R0          => reg_out_R0,  -- selected when 0
            IncDecVal   => IncDecVal_D,
            -- selected when +-2 (no shift), +-3 (shift 1), +-4 (shift 2), and 0 for some operations
            Disp     => opcode(7 downto 0),  
            -- choose to use last 0 (4 bits), 1 (8 bits) of disp
            DispCutoff  => DispCutoff_D,       
        -- signals from control unit, directly connect to the wrapped general mau
            PrePostSel => PrePostSel_D,
        -- outputs, directly connects to the wrapped general mau output
            Address    => ram_data_address,
            AddrSrcOut => data_addr_writeback_EX
        );

        program_addr_unit:  programAddrUnit
            port map(
            -- inputs for base addr
                SrcSel => SrcSel_P,       -- singal for selection 
                PC  => PC_EX,  -- selected when 0
            -- inputs for offset
                OffsetSel => OffsetSel_P,       -- singal for selection 
                Rm  => reg_out_b,  -- selected when 0
                -- selected when +-2 (no shift), +-3 (shift 1), +-4 (shift 2), and 0 for some operations
                Disp => opcode(11 downto 0),  
                -- choose to use last 0 (4 bits), 1 (8 bits) of disp
                DispCutoff => DispCutoff_P,       
            -- signals from control unit, directly connect to the wrapped general mau
                PrePostSel => '1',
            -- outputs, directly connects to the wrapped general mau output
                Address => open,
                AddrSrcOut => program_addr_src_out
            );


        reg_file: RegFile
            port map (
                clk        => clk,                         -- clock
                data_in    => reg_write_in,     -- the data to write to a register
                write_en   => reg_write_en,                         -- write the data if 1
                write_sel  => reg_write_addr,         -- select which reg to write to
                read_sel_a => reg_read_a,         -- select the first register to output
                read_sel_b => reg_read_b,         -- select the second register to output
                data_out_a => reg_out_a,     -- the data output for the first register
                data_out_b => reg_out_b,      -- the data output for the second register
                data_out_0 => reg_out_R0      -- the data output for the second register
            );
    
        alu_0: ALU
            port map(
                ALUOpA  => reg_out_a,   -- first operand
                ALUOpB  => reg_out_b,   -- second operand
                immd   => opcode(7 downto 0),   -- immediate value (IR 7-0)
                T     => SR_T,                       -- T flag 
                op_a_sel  => alu_op_a_sel,
                op_b_sel => alu_op_b_sel,
                adder_cin_sel => adder_cin_sel,
                AddSub   => AddSub,                       -- 1 for add, 0 for sub, always the 2nd bit of IR for SH2!  
                ALUCmd   => ALUCmd,    -- ALU result select
                FCmd    => FCmd,    -- F-Block operation
                SCmd    => SCmd,    -- shift operation
                
                Result  => ALU_result_EX,   -- ALU result
                C   => ALU_C,                       -- carry out
                V => ALU_V,                        -- overflow
                S => ALU_S,                    -- sign
                Z => ALU_Z                        -- zero
            );

        ram_rounting_0: RAMRouting
            port map (
                clk => clk,
                EN  => ram_EN,      -- 1 for enable, 0 for disable
                PD  => ram_PD,      -- 0 for program, 1 for data memory 
                RW  => ram_RW,      -- 0 for read, 1 for write, force read if program
                access_mode => ram_access_mode,       -- force WORD_ACCESS if program     
                program_address => PC_pre,   -- memory address bus
                data_address  => ram_data_address,   -- memory address bus

                write_data => reg_out_b,
                DB     => DB,
                read_data => ram_data_read,
                AB => AB,
                

                RE0   => RE0,                       -- first byte active low read enable
                RE1  => RE1,                       -- second byte active low read enable
                RE2  => RE2,                       -- third byte active low read enable
                RE3  => RE3,                       -- fourth byte active low read enable
                WE0 => WE0,                       -- first byte active low write enable
                WE1 => WE1,                       -- second byte active low write enable
                WE2 => WE2,                       -- third byte active low write enable
                WE3 => WE3,                       -- fourth byte active low write enable
                exception => ram_exception
            );
    
end structural;

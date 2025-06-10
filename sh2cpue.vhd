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
use work.ALUConstants.all;

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
        DB_write      :  out  std_logic_vector(31 downto 0);
        DB_read      :  in  std_logic_vector(31 downto 0);

        -- debug input signals
        PC_reset_addr_debug      :  in  std_logic_vector(31 downto 0);    
        -- debug probe signals
        opcode_debug :  out  std_logic_vector(15 downto 0);
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
            DB_write      :  out  std_logic_vector(31 downto 0);
            DB_read      :  in  std_logic_vector(31 downto 0);
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

    component  AddrUnit
        port (
        -- inputs for base addr
            SrcSel      : in    integer  range 4 downto 0;       -- singal for selection 
            PC          : in     std_logic_vector(31 downto 0);  -- selected when 0
            Rn          : in     std_logic_vector(31 downto 0);   -- selected when 1
            GBR         : in     std_logic_vector(31 downto 0);   -- selected when 2

        -- inputs for offset
            OffsetSel   : in    integer  range 5 downto 0;       -- singal for selection 
            R0          : in     std_logic_vector(31 downto 0);  -- selected when 0
            Rm          : in     std_logic_vector(31 downto 0);  -- selected when 0
            IncDecVal   :  in     std_logic_vector(3 downto 0);
            -- selected when +-2 (no shift), +-3 (shift 1), +-4 (shift 2), and 0 for some operations
            Disp        : in     std_logic_vector(11 downto 0);  
            -- choose to use last 0 (4 bits), 1 (8 bits) of disp
            DispCutoff  : in    integer  range 3 downto 0;       

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

            op_a_sel   : in    integer  range 0 to 2;
            op_b_sel   : in    integer  range 0 to 3;
            adder_cin_sel   : in    integer  range 0 to 2;
            adder_T_out_sel : in    integer range 0 to 8;

            ALU_special_cmd : in std_logic;
            MULCmd: in      integer  range 0 to 3;
            
            AddSub   : in      std_logic;                       -- 1 for add, 0 for sub, always the 2nd bit of IR for SH2!  
            ALUCmd   : in      std_logic_vector(1 downto 0);    -- ALU result select
            FCmd     : in      std_logic_vector(3 downto 0);    -- F-Block operation
            SCmd     : in      std_logic_vector(2 downto 0);    -- shift operation
            
            Result   : buffer  std_logic_vector(31 downto 0);   -- ALU result
            MACH_out : out std_logic_vector(31 downto 0);  
            T_out: out std_logic                   -- signed overflow
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

-- All signals used for this project. See the cpu_dataflow_and_signals.jpg for definition. The 
-- signals in blue are control signals.
    
-- control signals
    -- register file
    signal reg_read_a_mux : std_logic;         -- select the first register to output
    signal reg_read_b_mux : integer range 0 to 2;         -- select the first register to output
    signal reg_write_addr_mux : integer range 0 to 2;         -- select the register to write
    signal reg_write_in_mux : integer range 0 to 3;         -- select the register to write
    signal reg_write_en: std_logic;
    signal reg_write_in_sel_regs : integer range 0 to 7;         -- select when reg_write_in_mux = 3
    -- branch control
    signal PC_LD_sleep: std_logic;
    -- below all stops at EX stage:
    signal br_flush: std_logic;
    signal PC_LD_sel : integer range 0 to 3;         -- select the register to write
    signal PC_conditional_sel       : integer range 0 to 2; 
    -- control register
    signal PR_LD_sel: integer range 0 to 3;
    signal T_LD_sel: integer range 0 to 4;
    signal LD_GBR: std_logic;
    signal MACH_LD_sel, MACL_LD_sel : integer range 0 to 3;         -- select the register to write
    -- ALU
    signal alu_op_a_sel   : integer  range 0 to 2;
    signal alu_op_b_sel: integer  range 0 to 3;
    signal adder_cin_sel : integer  range 0 to 2;
    signal adder_T_out_sel : integer range 0 to 8;
    signal ALU_special_cmd : std_logic;
    signal MULCmd: integer  range 0 to 3;
    signal AddSub   :   std_logic;                       -- 1 for add, 0 for sub, always the bit2 or 1 when 0111!  
    signal ALUCmd   :   std_logic_vector(1 downto 0);    -- ALU result select
    signal FCmd     :   std_logic_vector(3 downto 0);    -- F-Block operation
    signal SCmd     :   std_logic_vector(2 downto 0);    -- shift operation
    -- memory addr unit
    signal SrcSel      :    integer  range 4 downto 0;       -- singal for selection 
    signal IncDecVal   :     std_logic_vector(3 downto 0);
    signal OffsetSel   :    integer  range 5 downto 0;       -- singal for selection 
    signal DispCutoff  :    integer  range 3 downto 0;       
    signal PrePostSel :      std_logic;
    -- RAM routing
    signal ram_EN          : std_logic;      -- 1 for enable, 0 for disable
    signal ram_PD          : std_logic;      -- 0 for program, 1 for data memory 
    signal ram_RW          : std_logic;      -- 0 for read, 1 for write, force read if program
    signal ram_access_mode : std_logic_vector(1 downto 0);       -- force WORD_ACCESS if program 
    -- signal LD_PC: std_logic;
    -- signal LD_IR: std_logic;
    -- signal GBR_LD_sel : integer range 0 to 2;         -- select the register to write
-- pipeline delayed control signals
    signal reg_write_addr_mux_EX : integer range 0 to 2;         
    signal reg_write_addr_mux_WB : integer range 0 to 2;        
    signal reg_write_in_mux_EX : integer range 0 to 3;             
    signal reg_write_in_mux_WB : integer range 0 to 3;        
    signal reg_write_en_EX: std_logic;    
    signal reg_write_en_WB: std_logic;     
    signal reg_write_in_sel_regs_EX : integer range 0 to 7;     
    -- branch control, below all stops at EX stage:
    signal br_flush_EX: std_logic;
    signal PC_LD_sel_EX : integer range 0 to 3;        
    signal PC_conditional_sel_EX       : integer range 0 to 2; 
    -- control register
    signal PR_LD_sel_EX: integer range 0 to 3;
    signal T_LD_sel_EX: integer range 0 to 4;
    signal LD_GBR_EX: std_logic;
    signal MACH_LD_sel_EX, MACL_LD_sel_EX : integer range 0 to 3;
    -- ALU
    signal alu_op_a_sel_EX   : integer  range 0 to 2;
    signal alu_op_b_sel_EX: integer  range 0 to 3;
    signal adder_cin_sel_EX : integer  range 0 to 2;
    signal adder_T_out_sel_EX : integer range 0 to 8;
    signal ALU_special_cmd_EX : std_logic;
    signal MULCmd_EX: integer  range 0 to 3;
    signal AddSub_EX   :   std_logic;                      
    signal ALUCmd_EX   :   std_logic_vector(1 downto 0);   
    signal FCmd_EX     :   std_logic_vector(3 downto 0);   
    signal SCmd_EX     :   std_logic_vector(2 downto 0);          
    -- memory addr unit
    signal SrcSel_EX      :    integer  range 4 downto 0;      
    signal IncDecVal_EX   :     std_logic_vector(3 downto 0);
    signal OffsetSel_EX   :    integer  range 5 downto 0;     
    signal DispCutoff_EX  :    integer  range 3 downto 0;       
    signal PrePostSel_EX :      std_logic;
    -- RAM routing
    signal ram_EN_EX          : std_logic;    
    signal ram_PD_EX          : std_logic;   
    signal ram_RW_EX          : std_logic;     
    signal ram_access_mode_EX : std_logic_vector(1 downto 0);      


-- system registers
    signal PR: std_logic_vector(31 downto 0);
    signal GBR: std_logic_vector(31 downto 0);
    signal SR_I:  std_logic_vector(3 downto 0);
    signal SR_S:  std_logic;
    signal SR_T:  std_logic;
    signal SR: std_logic_vector(31 downto 0);
    signal MACH, MACL: std_logic_vector(31 downto 0);

-- register saved for pipelining, first state are combinational
    signal ALU_result_ex         : std_logic_vector(31 downto 0);
    signal ALU_result_WB         : std_logic_vector(31 downto 0);

    signal addr_writeback_ex  : std_logic_vector(31 downto 0);
    signal addr_writeback_WB  : std_logic_vector(31 downto 0);

    signal IR_de: std_logic_vector(15 downto 0);
    signal opcode: std_logic_vector(15 downto 0);
    signal IR_EX: std_logic_vector(15 downto 0);
    signal IR_WB: std_logic_vector(15 downto 0);

    signal PC_pre                : std_logic_vector(31 downto 0);
    signal PC: std_logic_vector(31 downto 0);
    signal PC_EX: std_logic_vector(31 downto 0);

    signal reg_out_a_de : std_logic_vector(31 downto 0);     -- the data output for the first register
    signal reg_out_b_de : std_logic_vector(31 downto 0);      -- the data output for the second register
    signal reg_out_R0_de : std_logic_vector(31 downto 0);      
    signal reg_out_a_EX : std_logic_vector(31 downto 0);     -- the data output for the first register
    signal reg_out_b_EX : std_logic_vector(31 downto 0);      -- the data output for the second register
    signal reg_out_R0_EX : std_logic_vector(31 downto 0);      
    -- signal reg_out_a_WB:        std_logic_vector(31 downto 0);
    -- signal reg_out_b_WB:        std_logic_vector(31 downto 0);
    signal reg_in_reg_sel_result_ex: std_logic_vector(31 downto 0);
    signal reg_in_reg_sel_result_WB: std_logic_vector(31 downto 0);
    



-- interconnect signals
    signal program_address      : std_logic_vector(31 downto 0);   -- program address bus
    signal ram_data_address         : std_logic_vector(31 downto 0);   -- data address bus
    signal PC_EX_p4             : std_logic_vector(31 downto 0);
    signal pc_ld_actual                : integer range 0 to 3;
    signal PC_conditional       : std_logic;
    -- signal write_data : std_logic_vector(31 downto 0);
    -- signal read_data  : std_logic_vector(31 downto 0);
    signal ram_data_read : std_logic_vector(31 downto 0);
    -- register signals
    signal reg_sel_a : integer range 15 downto 0;         -- select the first register to output
    signal reg_sel_b : integer range 15 downto 0;         -- select the second register to output
    signal reg_write_in: std_logic_vector(31 downto 0);
    signal reg_write_addr: integer range 15 downto 0;
    signal reg_read_b: integer range 15 downto 0;
    signal reg_read_a: integer range 15 downto 0;
    -- alu flags
    -- signal SR_T_in: std_logic;
    -- signal SR_S_in: std_logic;
    -- signal ALU_cout         : std_logic;
    -- signal ALU_overflow         : std_logic;
    signal ALU_MACH_out : std_logic_vector(31 downto 0);  
    signal ALU_T_out: std_logic;




-- exceptions from components
    signal ram_exception : std_logic;    

-- for four stage pipeline
    -- type controlUnitStates is (FE, DE, EX, WB);
    signal state : std_logic_vector(1 downto 0);

begin
-- connecting debug lines
    opcode_debug <= opcode;
    PC_debug <= PC;
    ram_data_address_debug <= ram_data_address;
    PC_LD_sel_debug <= PC_LD_sel_EX; 
    ram_data_read_debug <= ram_data_read;
    reg_out_a_debug <= reg_out_a_EX;
    exception_debug <= ram_exception;


-- PC increment
    PC_pre <= PC when PC_LD_actual = 0 else
            std_logic_vector(unsigned(PC)+2)  when PC_LD_actual = 1  else
            addr_writeback_ex  when PC_LD_actual = 2  else
            PR  when PC_LD_actual = 3  else
            PC_reset_addr_debug;

    PC_conditional <= '0' when PC_conditional_sel_EX = 0 else
                    (not SR_T) when PC_conditional_sel_EX = 1 else
                    SR_T when PC_conditional_sel_EX = 2 else
                    'X';

    PC_LD_actual <= PC_LD_sel_EX when PC_conditional = '0' else
                    1 when PC_conditional = '1' else
                    0;

    PC_EX_p4 <= std_logic_vector(unsigned(PC_EX) + 4);  -- to-do: change RHS to PC_EX

    process(clk) begin
        if rising_edge(clk) then

            if (reset = '1') then 
    -- pipeline control signals update
            -- de -> EX:

        reg_write_addr_mux_EX <= reg_write_addr_mux;
        reg_write_in_mux_EX <= reg_write_in_mux;     
        reg_write_en_EX <= reg_write_en;  
        reg_write_in_sel_regs_EX <= reg_write_in_sel_regs;    
        -- branch control
        br_flush_EX <= br_flush;
        PC_LD_sel_EX <= PC_LD_sel;      
        PC_conditional_sel_EX <= PC_conditional_sel;
        -- control register
        PR_LD_sel_EX <=  PR_LD_sel;
        T_LD_sel_EX <=  T_LD_sel;
        LD_GBR_EX <=  LD_GBR;
        MACH_LD_sel_EX <=  MACH_LD_sel;
        MACL_LD_sel_EX <= MACL_LD_sel;
        -- ALU
        alu_op_a_sel_EX <= alu_op_a_sel;
        alu_op_b_sel_EX <= alu_op_b_sel;
        adder_cin_sel_EX <= adder_cin_sel;
        adder_T_out_sel_EX <= adder_T_out_sel;
        ALU_special_cmd_EX <= ALU_special_cmd;
        MULCmd_EX <= MULCmd;
        AddSub_EX <=  AddSub;                    
        ALUCmd_EX  <= ALUCmd; 
        FCmd_EX  <=   FCmd;  
        SCmd_EX  <=   SCmd;         
        -- memory addr unit
        SrcSel_EX   <=  SrcSel;      
        IncDecVal_EX  <= IncDecVal;
        OffsetSel_EX <= OffsetSel;     
        DispCutoff_EX <= DispCutoff;       
        PrePostSel_EX <= PrePostSel;
        -- RAM routing
        ram_EN_EX <= ram_EN;    
        ram_PD_EX <= ram_PD;   
        ram_RW_EX <= ram_RW;     
        ram_access_mode_EX <= ram_access_mode;  

            -- WB->EX:
        reg_write_addr_mux_WB <= reg_write_addr_mux_EX;
        reg_write_in_mux_WB <= reg_write_in_mux_EX;
        reg_write_en_WB <= reg_write_en_EX;



    -- control registers
                if (PR_LD_sel_EX = 0) then
                    PR <= PR;
                elsif (PR_LD_sel_EX = 1) then
                    PR <= reg_out_b_EX;
                elsif (PR_LD_sel_EX = 2) then
                    PR <= PC_EX;
                elsif (PR_LD_sel_EX = 3) then
                    PR <= PC_EX_p4;
                else
                    PR <= (others => 'X');
                end if;


                if (LD_GBR_EX) then
                    GBR <= reg_out_b_EX;
                else
                    GBR <= GBR;
                end if;



            else
                PC <= PC_reset_addr_debug;
                state <= "00";
                SR_S <= '0';
                SR_T <= '0';
                SR_I <= "1111";

                -- pipeline delayed control signals
                reg_write_addr_mux_EX <= 0;         
                reg_write_addr_mux_WB <= 0;     
                reg_write_in_mux_EX <= 0;             
                reg_write_in_mux_WB <= 0;       
                reg_write_en_EX <= '0';     
                reg_write_en_WB <= '0';  
                reg_write_in_sel_regs_EX <= 0;   
                -- branch control
                br_flush_EX <= '0'; 
                PC_LD_sel_EX <= 0;        
                PC_conditional_sel_EX    <= 0; 
                -- control register
                PR_LD_sel_EX <= 0; 
                T_LD_sel_EX <= 0; 
                LD_GBR_EX <= '0'; 
                MACH_LD_sel_EX<= 0; 
                MACL_LD_sel_EX <= 0; 
                -- ALU
                alu_op_a_sel_EX   <= 0; 
                alu_op_b_sel_EX <= 0; 
                adder_cin_sel_EX <= 0; 
                adder_T_out_sel_EX <= 0; 
                ALU_special_cmd_EX  <= '0'; 
                MULCmd_EX <= 0; 
                AddSub_EX    <= '0';                    
                ALUCmd_EX    <= "00";   
                FCmd_EX    <= "0000";     
                SCmd_EX    <= "000";        
                -- memory addr unit
                SrcSel_EX      <= 0;     
                IncDecVal_EX   <= "0000";     
                OffsetSel_EX  <= 0;   
                DispCutoff_EX  <= 0; 
                PrePostSel_EX  <= '0'; 
                -- RAM routing
                ram_EN_EX   <= '0'; 
                ram_PD_EX   <= '0'; 
                ram_RW_EX   <= '0'; 
                ram_access_mode_EX  <= "00";     
            end if;
             
    -- compute results cache
            -- ALU/addr_unit result
                ALU_result_WB <= ALU_result_ex;
                addr_writeback_WB <= addr_writeback_ex;
            -- IR
                IR_EX <= IR_de;
                IR_WB <= IR_EX;
            -- PC register update
                PC <= PC_pre;
                PC_EX <= PC;
            -- register readouts
                reg_out_a_EX <= reg_out_a_de;
                -- reg_out_a_WB <= reg_out_a_EX;
                reg_out_b_EX <= reg_out_b_de;
                -- reg_out_b_WB <= reg_out_b_EX;
                reg_out_R0_EX <= reg_out_R0_de;
            -- other registers for write back to regfile
            reg_in_reg_sel_result_WB <= reg_in_reg_sel_result_ex;


                
    -- SR       
            if (T_LD_sel_EX = 0) then
                SR_T <= SR_T;
            elsif  (T_LD_sel_EX = 1) then
                SR_T <= ALU_T_out;
            elsif  (T_LD_sel_EX = 2) then
                SR_T <= reg_out_b_EX(0);
            elsif  (T_LD_sel_EX = 3) then
                SR_T <= '0';
            elsif  (T_LD_sel_EX = 4) then
                SR_T <= '1';
            else
                SR_T <= 'X'; 
            end if;
            
            if (MACH_LD_sel_EX = 0) then
                MACH <= MACH;
            elsif (MACH_LD_sel_EX = 1) then
                MACH <= reg_out_b_EX;
            elsif (MACH_LD_sel_EX = 2) then
                MACH <= (others => '0');
            elsif (MACH_LD_sel_EX = 3) then
                MACH <= ALU_MACH_out;
            else
                MACH <= (others => 'X');
            end if;

            if (MACL_LD_sel_EX = 0) then
                MACL <= MACL;
            elsif (MACL_LD_sel_EX = 1) then
                MACL <= reg_out_b_EX;
            elsif (MACL_LD_sel_EX = 2) then
                MACL <= (others => '0');
            elsif (MACL_LD_sel_EX = 3) then
                MACL <= ALU_result_ex;
                -- MACL <= (others => 'X');
            else
                MACL <= (others => 'X');
            end if;
                

        end if;
    end process;
    
    SR <= (31 downto 8 => '0') & SR_I & "00" & SR_S & SR_T;


    -- to-do: update this for selction with fetch_stall!!!
    IR_de <= ram_data_read(15 downto 0);
    opcode <= IR_de;        -- alias for IR_de

    ram_EN <= '1';      -- keep ram enabled for now

-- decoding
    process(all) begin
        
        SrcSel <= 0;       -- singal for selection 
        IncDecVal <= "0000";
        OffsetSel <= 0;       -- singal for selection 
        DispCutoff <= 0;       
        PrePostSel <= '0';
        -- ALU
        alu_op_a_sel <= 0; 
        alu_op_b_sel <= 0;
        adder_cin_sel <= 0;
        adder_T_out_sel <= 0;
        ALU_special_cmd <= '0';
        MULCmd <= 0;
        AddSub <= '0';                       -- 1 for add, 0 for sub, always the bit2 or 1 when 0111!  
        ALUCmd <= "00";    -- ALU result select
        FCmd <= "0000";    -- F-Block operation
        SCmd <= "000";    -- shift operation
        -- register
        reg_read_a_mux <= '0';         -- select the first register to output
        reg_read_b_mux <= 0;         -- select the first register to output
        reg_write_addr_mux <= 0;         -- select the register to write
        reg_write_in_mux <= 0;         -- select the register to write
        reg_write_in_sel_regs <= 0;
        reg_write_en <= '0';
        -- ram rounting
        ram_PD <= '0';      -- 0 for program, 1 for data memory 
        ram_RW  <= '0';      -- 0 for read, 1 for write, force read if program
        ram_access_mode <= "00";       -- force WORD_ACCESS if program 
        -- control register
        PR_LD_sel <= 0;         -- select the register to write
        -- LD_IR <= '0';
        T_LD_sel <= 0;
        -- LD_S <= '0';
        LD_GBR <= '0';         -- select the register to write
        PC_conditional_sel <= 0;
        PC_LD_sel <= 0;         -- select the register to write, to-do: for unpipelined, increment in decode stage, 
        -- but for pipelined, increment almost all the time, thus needs reset address to one previous first command!
        MACL_LD_sel <= 0;
        MACH_LD_sel <= 0;


        if (state = "00") then
            -- ram_EN <= '1';
            ram_RW <= '0';
            ram_access_mode <= WORD_ACCESS;
            ram_PD <= '0';
        end if;


    
    --Decoder
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

                elsif (opcode(3 downto 0) = "0011") then
                    if (opcode(7 downto 4) = "0000") then
                    -- bsrf Rm
                        if (state = "10") then
                            PR_LD_sel <= 3;
                            PC_LD_sel <= 2;
                        end if;
                    elsif (opcode(7 downto 4) = "0010") then
                    -- braf Rm
                        if (state = "10") then
                            PR_LD_sel <= 0;
                            PC_LD_sel <= 2;
                        end if;
                    end if;

                    if (state = "10") then
                        reg_read_b_mux <= 0;
                        Srcsel <= 1;
                        OffsetSel <= 5;
                    end if;

                elsif (opcode(3 downto 0) = "0100") then 
                -- MOV.B Rm, @(R0, Rn)
                    if (state = "10") then
                        reg_read_a_mux <= '0';
                        SrcSel <= 0;
                        OffsetSel <= 0;
                        PrePostSel <= '0';
                        ram_RW <= '1';
                        ram_PD <= '1';
                        -- ram_EN <= '1';
                        reg_read_b_mux <= 1;
                        ram_access_mode <= BYTE_ACCESS;
                    end if;

                
                elsif (opcode(3 downto 0) = "0101") then 
                -- MOV.W Rm, @(R0, Rn)
                    if (state = "10") then
                        reg_read_a_mux <= '0';
                        SrcSel <= 0;
                        OffsetSel <= 0;
                        PrePostSel <= '0';
                        ram_RW <= '1';
                        ram_PD <= '1';
                        -- ram_EN <= '1';
                        reg_read_b_mux <= 1;
                        ram_access_mode <= WORD_ACCESS;
                    end if;  
                
                elsif (opcode(3 downto 0) = "0110") then 
                -- MOV.L Rm, @(R0, Rn)
                    if (state = "10") then
                        reg_read_a_mux <= '0';
                        SrcSel <= 0;
                        OffsetSel <= 0;
                        PrePostSel <= '0';
                        ram_RW <= '1';
                        ram_PD <= '1';
                        -- ram_EN <= '1';
                        reg_read_b_mux <= 1;
                        ram_access_mode <= LONG_ACCESS;
                        
                    end if;

                elsif (opcode(3 downto 0) = "1000") then 
                    case opcode(11 downto 4) is
                        when "00000000" =>
                        -- CLRT
                            if (state = "10") then
                                T_LD_sel <= 3;
                            end if;
                            
                        when "00000001" =>
                        -- SETT
                            if (state = "10") then
                                T_LD_sel <= 4;
                            end if;

                        when "00000010" =>
                        -- CLRT
                            if (state = "10") then
                                MACH_LD_sel <= 2;
                                MACL_LD_sel <= 2;
                            end if;
                        
                        when others =>
                            null;
                    end case;


                elsif (opcode(3 downto 0) = "1100") then 
                -- MOV.B @(R0, Rn), Rm 
                    if (state = "10") then
                        reg_read_a_mux <= '1';
                        SrcSel <= 0;
                        OffsetSel <= 0;
                        PrePostSel <= '0';
                        ram_RW <= '0';
                        ram_PD <= '1';
                        -- ram_EN <= '1';
                        ram_access_mode <= BYTE_ACCESS;
                    elsif (state = "11") then
                        reg_write_addr_mux <= 0;
                        reg_write_en <= '1';
                        reg_write_in_mux <= 2;
                    end if;
                    
                elsif (opcode(3 downto 0) = "1101") then 
                -- MOV.W @(R0, Rn), Rm 
                    if (state = "10") then
                        reg_read_a_mux <= '1';
                        SrcSel <= 0;
                        OffsetSel <= 0;
                        PrePostSel <= '0';
                        ram_RW <= '0';
                        ram_PD <= '1';
                        -- ram_EN <= '1';
                        ram_access_mode <= WORD_ACCESS;
                    elsif (state = "11") then
                        reg_write_addr_mux <= 0;
                        reg_write_en <= '1';
                        reg_write_in_mux <= 2;
                    end if;
                        
                elsif (opcode(3 downto 0) = "1110") then 
                -- MOV.L @(R0, Rn), Rm 
                    if (state = "10") then
                        reg_read_a_mux <= '1';
                        SrcSel <= 0;
                        OffsetSel <= 0;
                        PrePostSel <= '0';
                        ram_RW <= '0';
                        ram_PD <= '1';
                        -- ram_EN <= '1';
                        ram_access_mode <= LONG_ACCESS;
                    elsif (state = "11") then
                        reg_write_addr_mux <= 0;
                        reg_write_en <= '1';
                        reg_write_in_mux <= 2;
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
                    
                elsif (opcode(3 downto 0) = "1001") then 
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
                    case opcode(7 downto 4) is
                        when "0000" =>
                        -- rts
                            if (state = "10") then
                                PC_LD_sel <= 3;
                            end if;

                        when "0001" =>
                        -- SLEEP   0000 0000 0001 1011
                            if (state = "10") then
                                PC_LD_sel <= 0;
                            end if;
                        
                        when others =>
                            null;
                    end case;
                end if;

            when "0001" => 
            -- mov.l Rm, @(diso, Rn)
                if (state = "10") then
                    reg_read_b_mux <= 1;
                    reg_read_a_mux <= '0';
                    SrcSel <= 0;
                    DispCutoff <= 0;
                    OffsetSel <= 4;
                    PrePostSel <= '0';

                    ram_access_mode <= LONG_ACCESS;
                    ram_RW <= '1';
                    ram_PD <= '1';
                    -- ram_EN <= '1';
                end if;

            
            when "0010" => 
                if (opcode(3 downto 2) = "00") then
                -- mov.B/W/L Rm, @Rn
                    case opcode(1 downto 0) is 
                        when "00" =>
                            if (state = "10") then
                                ram_access_mode <= BYTE_ACCESS;
                                -- ram_EN <= '1';
                            end if;
                        when "01" =>
                            if (state = "10") then
                                ram_access_mode <= WORD_ACCESS;
                                -- ram_EN <= '1';
                            end if;
                        when "10" => 
                            if (state = "10") then
                                ram_access_mode <= LONG_ACCESS;
                                -- ram_EN <= '1';
                            end if;
                        when others =>
                            null;
                    end case;
                                    
                    if (state = "10") then
                        reg_read_b_mux <= 1;
                        reg_read_a_mux <= '0';
                        SrcSel <= 0;
                        OffsetSel <= 1;
                        IncDecVal <= "0000";
                        PrePostSel <= '0';

                        ram_RW <= '1';
                        ram_PD <= '1';
                    end if;


                elsif (opcode(3 downto 2) = "01") then
                -- mov.B/W/L Rm, @-Rn
                    case opcode(1 downto 0) is 
                        when "00" =>
                            if (state = "10") then
                                IncDecVal <= "1111";
                                ram_access_mode <= BYTE_ACCESS;
                                -- ram_EN <= '1';
                            elsif (state = "11") then
                                reg_write_en <= '1';
                            end if;
                        
                        when "01" =>
                            if (state = "10") then
                                IncDecVal <= "1110";
                                ram_access_mode <= WORD_ACCESS;
                                -- ram_EN <= '1';
                            elsif (state = "11") then
                                reg_write_en <= '1';
                            end if;
                        
                        when "10" =>
                            if (state = "10") then
                                IncDecVal <= "1100";
                                ram_access_mode <= LONG_ACCESS;
                                -- ram_EN <= '1';
                            elsif (state = "11") then
                                reg_write_en <= '1';
                            end if;
                        when others =>
                            null;
                    end case;
                    
                    if (state = "10") then
                        reg_read_b_mux <= 1;
                        reg_read_a_mux <= '0';
                        SrcSel <= 0;
                        OffsetSel <= 1;
                        PrePostSel <= '0';

                        ram_RW <= '1';
                        ram_PD <= '1';
                    
                    elsif (state = "11") then
                        reg_write_in_mux <= 0;
                    end if;
                
                
                elsif (opcode(3 downto 2) = "10") then
                    case opcode(1 downto 0) is
                    -- tst Rm, Rn
                        when "00" =>
                            if (state = "10") then
                                reg_read_a_mux <= '0';
                                reg_read_b_mux <= 1;
                                ALUCmd <= ALUCmd_FBLOCK;
                                FCmd <= "1000";
                                alu_op_a_sel <= 0;
                                alu_op_b_sel <= 0;
                                ALU_special_cmd <= '0';
                                T_LD_sel <= 1;
                            end if;
                            
                        when "01" => 
                        -- and Rm, Rn
                            if (state = "10") then
                                reg_read_a_mux <= '0';
                                reg_read_b_mux <= 1;
                                ALUCmd <= ALUCmd_FBLOCK;
                                FCmd <= "1000";
                                alu_op_a_sel <= 0;
                                alu_op_b_sel <= 0;
                                ALU_special_cmd <= '0';
                            elsif (state = "11") then
                                reg_write_addr_mux <= 0;
                                reg_write_en <= '1';
                                reg_write_in_mux <= 1;
                            end if;

                            
                        when "10" => 
                        -- xor Rm, Rn
                            if (state = "10") then
                                reg_read_a_mux <= '0';
                                reg_read_b_mux <= 1;
                                ALUCmd <= ALUCmd_FBLOCK;
                                FCmd <= "0110";
                                alu_op_a_sel <= 0;
                                alu_op_b_sel <= 0;
                                ALU_special_cmd <= '0';
                            elsif (state = "11") then
                                reg_write_addr_mux <= 0;
                                reg_write_en <= '1';
                                reg_write_in_mux <= 1;
                            end if;

                    
                        when "11" => 
                        -- or Rm, Rn
                            if (state = "10") then
                                reg_read_a_mux <= '0';
                                reg_read_b_mux <= 1;
                                ALUCmd <= ALUCmd_FBLOCK;
                                FCmd <= "1110";
                                alu_op_a_sel <= 0;
                                alu_op_b_sel <= 0;
                                ALU_special_cmd <= '0';
                            elsif (state = "11") then
                                reg_write_addr_mux <= 0;
                                reg_write_en <= '1';
                                reg_write_in_mux <= 1;
                            end if;
                        
                        when others =>
                            null;
                    end case;
                
                elsif (opcode(3 downto 2) = "11") then
                    case opcode(1 downto 0) is
                        when "01" =>
                        -- xtruct Rm, Rn
                            if (state = "10") then
                                reg_read_a_mux <= '1';
                                reg_read_b_mux <= 0;

                                ALU_special_cmd <= '1';
                                ALUCmd <= SpecCmd_XTUCT;
                            elsif (state = "11") then
                                reg_write_addr_mux <= 0;
                                reg_write_en <= '1';
                                reg_write_in_mux <= 1;
                            end if;


                        when "10" =>
                        -- mulu.w Rm, Rn
                            if (state = "10") then
                                reg_read_a_mux <= '0';
                                reg_read_b_mux <= 1;
                                
                                ALU_special_cmd <= '1';
                                ALUCmd <= SpecCmd_MUL;
                                MULCmd <= 3;

                                MACL_LD_sel <= 3;
                            end if;

                        when "11" =>
                        -- mulu.w Rm, Rn
                            if (state = "10") then
                                reg_read_a_mux <= '0';
                                reg_read_b_mux <= 1;
                                
                                ALU_special_cmd <= '1';
                                ALUCmd <= SpecCmd_MUL;
                                MULCmd <= 2;

                                MACL_LD_sel <= 3;
                            end if;

                        when others =>
                            null;
                    end case;

                end if;
                       
            when "0011" =>   
                if (opcode(3 downto 2) = "00") then  
                    case opcode(1 downto 0) is
                        when "00" =>
                        -- cmp/eq Rm, Rn
                            if (state = "10") then
                                T_LD_sel <= 1;
                                adder_T_out_sel <= 1;
                            end if;
                            
                        when "10" =>
                        -- cmp/hs Rm, Rn
                            if (state = "10") then
                                T_LD_sel <= 1;
                                adder_T_out_sel <= 0;
                            end if;
                            
                        when "11" =>
                        -- cmp/ge Rm, Rn
                            if (state = "10") then
                                T_LD_sel <= 1;
                                adder_T_out_sel <= 5;
                            end if;
                        when others =>
                            null;
                    end case;
                        
                    if (state = "10") then
                        reg_read_a_mux <= '0';
                        reg_read_b_mux <= 1;
                        adder_cin_sel <= 0;
                        alu_op_a_sel <= 0;
                        alu_op_b_sel <= 0;
                        addSub <= '0';
                        ALUCmd <= ALUCmd_ADDER;
                        ALU_special_cmd <= '0';
                    end if;

                elsif (opcode(3 downto 2) = "01") then  
                    case opcode(1 downto 0) is
                        when "01" =>
                        -- dmulu.l	Rm,Rn
                            if (state = "10") then
                                reg_read_a_mux <= '0';
                                reg_read_b_mux <= 1;
                                
                                ALU_special_cmd <= '1';
                                ALUCmd <= SpecCmd_MUL;
                                MULCmd <= 1;

                                MACL_LD_sel <= 3;
                                MACH_LD_sel <= 3;
                            end if;

                        when "10" =>
                            if (state = "10") then
                            -- cmp/hi Rm, Rn
                                reg_read_a_mux <= '0';
                                reg_read_b_mux <= 1;

                                adder_cin_sel <= 0;
                                alu_op_a_sel <= 0;
                                alu_op_b_sel <= 0;
                                addSub <= '0';
                                ALUCmd <= ALUCmd_ADDER;
                                ALU_special_cmd <= '0';
                                adder_T_out_sel <= 3;

                                T_LD_sel <= 1;
                            end if;

                        when "11" =>
                            if (state = "10") then
                            -- cmp/gt Rm, Rn
                                reg_read_a_mux <= '0';
                                reg_read_b_mux <= 1;

                                adder_cin_sel <= 0;
                                alu_op_a_sel <= 0;
                                alu_op_b_sel <= 0;
                                addSub <= '0';
                                ALUCmd <= ALUCmd_ADDER;
                                ALU_special_cmd <= '0';
                                adder_T_out_sel <= 4;

                                T_LD_sel <= 1;
                            end if;
                          
                        when others =>
                            null;
                    end case;

                elsif (opcode(3 downto 2) = "10" or opcode(3 downto 2) = "11")
                            and (opcode(1 downto 0) /= "01") then  
                    case opcode(3 downto 0) is
                        when "1000" =>
                        -- sub Rm, Rn
                            if (state = "10") then
                                addSub <= '0';
                                adder_cin_sel <= 0;
                                T_LD_sel <= 0;
                            end if;
                            
                        when "1010" =>
                        -- subc Rm, Rn
                            if (state = "10") then
                                addSub <= '0';
                                adder_cin_sel <= 1;
                                T_LD_sel <= 1;
                                adder_T_out_sel <= 2;
                            end if;
                            
                        when "1011" =>
                        -- subv Rm, Rn
                            if (state = "10") then
                                addSub <= '0';
                                adder_cin_sel <= 0;
                                T_LD_sel <= 1;
                                adder_T_out_sel <= 6;
                            end if;
                        
                            
                        when "1100" =>
                        -- add Rm, Rn
                            if (state = "10") then
                                addSub <= '1';
                                adder_cin_sel <= 0;
                                T_LD_sel <= 0;
                            end if;
                            
                        when "1110" =>
                        -- addc Rm, Rn
                            if (state = "10") then
                                addSub <= '1';
                                adder_cin_sel <= 1;
                                T_LD_sel <= 1;
                                adder_T_out_sel <= 0;
                            end if;
                            
                        when "1111" =>
                        -- addv Rm, Rn
                            if (state = "10") then
                                addSub <= '1';
                                adder_cin_sel <= 0;
                                T_LD_sel <= 1;
                                adder_T_out_sel <= 6;
                            end if;

                        when others =>
                            null;
                    end case;

                    if (state = "10") then
                        reg_read_a_mux <= '0';
                        reg_read_b_mux <= 1;

                        alu_op_a_sel <= 0;
                        alu_op_b_sel <= 0;
                        ALUCmd <= ALUCmd_ADDER;
                        ALU_special_cmd <= '0';
                    elsif (state = "11") then
                        reg_write_addr_mux <= 0;
                        reg_write_en <= '1';
                        reg_write_in_mux <= 1;
                    end if;


                elsif (opcode(3 downto 0) = "1101") then
                -- dmuls.l	Rm,Rn
                    if (state = "10") then
                        reg_read_a_mux <= '0';
                        reg_read_b_mux <= 1;
                        
                        ALU_special_cmd <= '1';
                        ALUCmd <= SpecCmd_MUL;
                        MULCmd <= 0;

                        MACL_LD_sel <= 3;
                        MACH_LD_sel <= 3;
                    end if;

                end if;
                  
            
            when "0101" =>
            -- mov.L @(disp:4, Rm), Rn
                if (state = "10") then
                    reg_read_a_mux <= '1';
                    SrcSel <= 0;
                    DispCutoff <= 0;
                    OffsetSel <= 4;
                    PrePostSel <= '0';

                    ram_access_mode <= LONG_ACCESS;
                    ram_RW <= '0';
                    ram_PD <= '1';
                    -- ram_EN <= '1';
                elsif (state = "11") then
                    reg_write_en <= '1';
                    reg_write_in_mux <= 2;
                    reg_write_addr_mux <= 0;
                end if;


            when "0110" =>
                if (opcode(3 downto 2) = "00") then 
                -- mov @Rm, Rn
                    case opcode(1 downto 0) is
                        when "00" =>
                            if (state = "10") then
                                ram_access_mode <= BYTE_ACCESS;
                                reg_read_a_mux <= '1';
                                SrcSel <= 0;
                                IncDecVal <= "0000";
                                OffsetSel <= 1;
                                PrePostSel <= '0';
                                ram_RW <= '0';
                                ram_PD <= '1';
                                -- ram_EN <= '1';
                            elsif (state = "11") then
                                reg_write_en <= '1';
                                reg_write_in_mux <= 2;
                                reg_write_addr_mux <= 0;
                            end if;
  
                        when "01" =>
                            if (state = "10") then
                                ram_access_mode <= WORD_ACCESS;
                                reg_read_a_mux <= '1';
                                SrcSel <= 0;
                                IncDecVal <= "0000";
                                OffsetSel <= 1;
                                PrePostSel <= '0';
                                ram_RW <= '0';
                                ram_PD <= '1';
                                -- ram_EN <= '1';
                            elsif (state = "11") then
                                reg_write_en <= '1';
                                reg_write_in_mux <= 2;
                                reg_write_addr_mux <= 0;
                            end if;
                            
                        when "10" =>
                            if (state = "10") then
                                reg_read_a_mux <= '1';
                                SrcSel <= 0;
                                IncDecVal <= "0000";
                                OffsetSel <= 1;
                                PrePostSel <= '0';
                                ram_access_mode <= LONG_ACCESS;
                                ram_RW <= '0';
                                ram_PD <= '1';
                                -- ram_EN <= '1';
                            elsif (state = "11") then
                                reg_write_en <= '1';
                                reg_write_in_mux <= 2;
                                reg_write_addr_mux <= 0;
                            end if;
                
                        when "11" =>
                            if (state = "10") then
                                reg_read_b_mux <= 1;
                            elsif (state = "11") then
                                reg_write_en <= '1';
                                reg_write_in_mux <= 3;
                                reg_write_addr_mux <= 0;
                                reg_write_in_sel_regs <= 0;
                            end if;
                        
                        when others =>
                            null;
                    end case;
                    
                elsif (opcode(3 downto 0) = "0111") then 
                -- not Rm, Rn
                    if (state = "10") then
                        reg_read_b_mux <= 1;

                        alu_op_a_sel <= 1;
                        alu_op_b_sel <= 0;
                        ALUCmd <= ALUCmd_FBLOCK;
                        FCmd <= "0101";
                        ALU_special_cmd <= '0';
                    elsif (state = "11") then
                        reg_write_addr_mux <= 0;
                        reg_write_en <= '1';
                        reg_write_in_mux <= 1;
                    end if;
                    

                elsif (opcode(3 downto 0) = "1001") then 
                -- swap.W Rm, Rn
                    if state = "10" then
                        reg_read_a_mux <= '1';
                        
                        ALUCmd <= ALUCmd_SHIFT;
                        SCmd <= SCmd_SWAP;
                        ALU_special_cmd <= '0';
                        ALU_op_a_sel <= 0;
                    elsif state = "11" then
                        reg_write_addr_mux <= 0;
                        reg_write_en <= '1';
                        reg_write_in_mux <= 1;
                    end if;
                    
                    
                elsif (opcode(3 downto 0) = "1010") then 
                    -- negc Rm, Rn
                    if state = "10" then
                        reg_read_b_mux <= 1;
                        
                        ALU_op_a_sel <= 1;
                        ALU_op_b_sel <= 0;
                        ALUCmd <= ALUCmd_ADDER;
                        addSub <= '0';
                        ALU_special_cmd <= '0';
                        adder_cin_sel <= 1;

                        adder_T_out_sel <= 2;
                        T_LD_sel <= 1;
                    elsif state = "11" then
                        reg_write_addr_mux <= 0;
                        reg_write_en <= '1';
                        reg_write_in_mux <= 1;
                    end if;
                    
                    
                elsif (opcode(3 downto 0) = "1011") then 
                    -- neg Rm, Rn
                    if state = "10" then
                        reg_read_b_mux <= 1;
                        
                        ALU_op_a_sel <= 1;
                        ALU_op_b_sel <= 0;
                        ALUCmd <= ALUCmd_ADDER;
                        addSub <= '0';
                        ALU_special_cmd <= '0';
                        adder_cin_sel <= 0;
                    elsif state = "11" then
                        reg_write_addr_mux <= 0;
                        reg_write_en <= '1';
                        reg_write_in_mux <= 1;
                    end if;

                    
                elsif (opcode(3 downto 2) = "11") then 
                    -- extU/S.B/W Rm, Rn
                    if state = "10" then
                        reg_read_a_mux <= '1';
                        ALUCmd <= SpecCmd_EXT;
                        ALU_special_cmd <= '1';
                    elsif state = "11" then
                        reg_write_addr_mux <= 0;
                        reg_write_en <= '1';
                        reg_write_in_mux <= 1;
                    end if;
                end if;


            when "0100" =>
                if ((opcode(7 downto 4) = "0000") or (opcode(7 downto 4) = "0010")) and
                ((opcode(3 downto 0) = "0000") or (opcode(3 downto 0) = "0001") or (opcode(3 downto 0) = "0100") or (opcode(3 downto 0) = "0101")) then
                -- normal shifts Rn
                    case opcode(7 downto 0) is
                        when "00000000" =>  -- SHLL
                            if state = "10" then
                                SCmd <= SCmd_LSL;
                            end if;
                        when "00100000" =>  -- SHAL
                            if state = "10" then
                                SCmd <= SCmd_LSL;
                            end if;                            
                        when "00000001" =>  -- SHLR
                            if state = "10" then
                                SCmd <= SCmd_LSR;
                            end if;
                        when "00100001" =>  -- SHSR
                            if state = "10" then
                                SCmd <= SCmd_ASR;
                            end if;
                        when "00000100" =>  -- ROTL
                            if state = "10" then
                                SCmd <= SCmd_ROL;
                            end if;
                        when "00100100" =>  -- ROTCL
                            if state = "10" then
                                SCmd <= SCmd_RLC;
                            end if;
                        when "00000101" =>  -- ROTR
                            if state = "10" then
                                SCmd <= SCmd_ROR;
                            end if;
                        when "00100101" =>  -- ROTCR
                            if state = "10" then
                                SCmd <= SCmd_RRC;
                            end if; 
                        when others =>
                            null;
                    end case;  
                    
                    if state = "10" then
                        reg_read_a_mux <= '0';
                        ALUCmd <= ALUCmd_SHIFT;
                        ALU_special_cmd <= '0';
                        ALU_op_a_sel <= 0;

                        T_LD_sel <= 1;
                    elsif state = "11" then
                        reg_write_addr_mux <= 0;
                        reg_write_en <= '1';
                        reg_write_in_mux <= 1;
                    end if;
                
                elsif opcode(7 downto 0) = "00010000" then
                -- dt Rn
                    if state = "10" then
                        reg_read_a_mux <= '0';

                        ALUCmd <= ALUCmd_ADDER;
                        ALU_special_cmd <= '0';
                        ALU_op_a_sel <= 0;
                        ALU_op_b_sel <= 3;
                        adder_cin_sel <= 0;
                        addSub <= '1';

                        adder_T_out_sel <= 1;
                        T_LD_sel <= 1;
                    elsif state = "11" then
                        reg_write_addr_mux <= 0;
                        reg_write_en <= '1';
                        reg_write_in_mux <= 1;
                    end if;
                    
                elsif opcode(7 downto 0) = "00010101" then
                -- cmp/pl Rn
                    if state = "10" then
                        reg_read_b_mux <= 0;

                        ALUCmd <= ALUCmd_ADDER;
                        ALU_op_a_sel <= 1;
                        ALU_op_b_sel <= 0;
                        adder_cin_sel <= 0;
                        addSub <= '1';

                        adder_T_out_sel <= 7;
                        T_LD_sel <= 1;
                    end if;
                    
                elsif opcode(7 downto 0) = "00010001" then
                -- cmp/pz Rn
                    if state = "10" then
                        reg_read_b_mux <= 0;

                        ALUCmd <= ALUCmd_ADDER;
                        ALU_op_a_sel <= 1;
                        ALU_op_b_sel <= 0;
                        adder_cin_sel <= 0;
                        addSub <= '1';

                        adder_T_out_sel <= 8;
                        T_LD_sel <= 1;
                    end if;

                elsif ((opcode(3 downto 0) = "1000") or (opcode(3 downto 0) = "1001")) and 
                ((opcode(7 downto 4) = "0000") or (opcode(7 downto 4) = "0001") or (opcode(7 downto 4) = "0010")) then
                -- SHLL/SHLRb Rn
                    if state = "10" then
                        reg_read_a_mux <= '0';
                        ALUCmd <= SpecCmd_SHIFT;
                        ALU_special_cmd <= '1';
                    elsif state = "11" then
                        reg_write_addr_mux <= 0;
                        reg_write_en <= '1';
                        reg_write_in_mux <= 1;
                    end if;


                elsif opcode(3 downto 0) = "1011" then 
                
                    if (opcode(7 downto 4) = "0000") then
                    -- jsr Rm
                        if (state = "10") then
                            PR_LD_sel <= 3;
                            PC_LD_sel <= 2;
                            reg_read_a_mux <= '0';
                            Srcsel <= 0;
                            OffsetSel <= 1;
                            IncDecVal <= "0000";
                        end if;

                    elsif (opcode(7 downto 4) = "0010") then
                    -- jmp Rm
                        if (state = "10") then
                            PR_LD_sel <= 0;
                            PC_LD_sel <= 2;
                            reg_read_a_mux <= '0';
                            Srcsel <= 0;
                            OffsetSel <= 1;
                            IncDecVal <= "0000";
                        end if;
                    end if;
                

                elsif opcode(3 downto 0) = "1110" or opcode(3 downto 0) = "1010" then
                    case opcode(7 downto 2) is
                        -- LDC/S Rm
                        when "000010" =>
                        -- MACH
                            if (state = "10") then
                                MACH_LD_sel <= 2;
                            end if;
                        when "000110" =>
                        -- MACL
                            if (state = "10") then
                                MACL_LD_sel <= 2;
                            end if;
                        when "001010" =>
                        -- PR
                            if (state = "10") then
                                PR_LD_sel <= 1;
                            end if;
                        when "000011" =>
                        -- SR
                            if (state = "10") then
                                T_LD_sel <= 2;
                            end if;
                        when "000111" =>
                        -- GBR
                            if (state = "10") then
                                LD_GBR <= '1';
                            end if;
                        when others =>
                            null;
                    end case;
                    
                    if (state = "10") then
                        reg_read_b_mux <= 0;
                    end if;
                end if;

            when "0111" =>
            -- add #imm, Rn
                if (state = "10") then
                    reg_read_a_mux <= '0';
                    alu_op_b_sel <= 1;
                    alu_op_a_sel <= 0;
                    adder_cin_sel <= 0;
                    ALUCmd <= ALUCmd_ADDER;
                    addSub <= '1';
                elsif (state = "11") then
                    reg_write_en <= '1';
                    reg_write_in_mux <= 1;
                    reg_write_addr_mux <= 0;
                end if;

            when "1000" =>
                if (opcode(11 downto 8) = "1000") then
                -- cmp/eq #imm, R0
                    if (state = "10") then
                        reg_read_b_mux <= 2;

                        alu_op_a_sel <= 2;
                        alu_op_b_sel <= 1;
                        adder_cin_sel <= 0;
                        ALUCmd <= ALUCmd_ADDER;
                        addSub <= '0';

                        adder_T_out_sel <= 1;
                        T_LD_sel <= 1;
                    end if;

                elsif ((opcode(11 downto 10) = "10" or opcode(11 downto 10) = "11") and
                    (opcode(9 downto 8) = "01" or opcode(9 downto 8) = "11")) then
                
                    case opcode(11 downto 8) is
                        when "1001" =>
                        -- bt disp:8
                            if (state = "10") then
                                PC_LD_sel <= 2;
                                PC_conditional_sel <= 1;
                            end if;
                        when "1011" =>
                        -- bf disp:8
                            if (state = "10") then
                                PC_LD_sel <= 2;
                                PC_conditional_sel <= 2;
                            end if;
                        when "1101" =>
                        -- bt/s disp:8
                            if (state = "10") then
                                PC_LD_sel <= 2;
                                PC_conditional_sel <= 1;
                                -- to-do: add flushing here
                            end if;
                        when "1111" =>
                        -- bf/s disp:8
                            if (state = "10") then
                                PC_LD_sel <= 2;
                                PC_conditional_sel <= 2;
                                -- to-do: add flushing here
                            end if;
                        when others =>
                            null;
                    end case;

                    if (state = "10") then
                        SrcSel <= 1;
                        DispCutoff <= 2;
                        OffsetSel <= 3;
                    end if;

                end if;
                

            when "1001" =>
            -- mov.W @(disp:8, PC), Rn
                if (state = "10") then
                    SrcSel <= 1;
                    DispCutoff <= 1;
                    OffsetSel <= 3;
                    PrePostSel <= '0';

                    ram_access_mode <= WORD_ACCESS;
                    ram_RW <= '0';
                    ram_PD <= '1';
                    -- ram_EN <= '1';
                elsif (state = "11") then
                    reg_write_en <= '1';
                    reg_write_in_mux <= 2;
                    reg_write_addr_mux <= 0;
                end if;


            when "1010" =>
            -- bra disp:12
                if (state = "10") then
                    SrcSel <= 1;
                    DispCutoff <= 3;
                    OffsetSel <= 3;
                    PC_LD_sel <= 2;
                end if;

            when "1011" =>
            -- bsr disp:12
                if (state = "10") then
                    SrcSel <= 1;
                    DispCutoff <= 3;
                    OffsetSel <= 3;
                    PC_LD_sel <= 2;
                    PR_LD_sel <= 3;
                end if;
            

            when "1100" =>
                if opcode(11 downto 10) = "00" then
                -- mov.B/W/L R0, @(disp:8, GBR)
                    case opcode(9 downto 8) is
                        when "00" =>
                            if (state = "10") then
                                ram_access_mode <= BYTE_ACCESS;
                                OffsetSel <= 2;
                            end if;
                        when "01" =>
                            if (state = "10") then
                                ram_access_mode <= WORD_ACCESS;
                                OffsetSel <= 3;
                            end if;
                        when "10" =>
                            if (state = "10") then
                                ram_access_mode <= LONG_ACCESS;
                                OffsetSel <= 4;
                            end if;
                        when others =>
                            null;
                    end case;
                    
                    if (state = "10") then
                        SrcSel <= 3;
                        DispCutoff <= 1;
                        PrePostSel <= '0';
                        reg_read_b_mux <= 2;

                        ram_RW <= '1';
                        ram_PD <= '1';
                        -- ram_EN <= '1';
                    end if;

                elsif (opcode(11 downto 10) = "01") and (opcode(9 downto 8) /= "11") then
                -- mov.B/W/L @(disp:8, GBR), R0
                    case opcode(9 downto 8) is
                        when "00" =>
                            if (state = "10") then
                                ram_access_mode <= BYTE_ACCESS;
                                OffsetSel <= 2;
                            end if;
                        when "01" =>
                            if (state = "10") then
                                ram_access_mode <= WORD_ACCESS;
                                OffsetSel <= 3;
                            end if;
                        when "10" =>
                            if (state = "10") then
                                ram_access_mode <= LONG_ACCESS;
                                OffsetSel <= 4;
                            end if;
                        when others =>
                            null;
                    end case;
                    
                    if (state = "10") then
                        SrcSel <= 3;
                        DispCutoff <= 1;
                        PrePostSel <= '0';

                        ram_RW <= '0';
                        ram_PD <= '1';
                        -- ram_EN <= '1';
                    elsif (state = "11") then
                        reg_write_en <= '1';
                        reg_write_in_mux <= 2;
                        reg_write_addr_mux <= 2;
                    end if;
                
                elsif opcode(11 downto 8) = "0111" then
                -- mova @(disp, PC), R0
                    if (state = "10") then
                        SrcSel <= 2;
                        DispCutoff <= 1;
                        OffsetSel <= 4;
                        PrePostSel <= '0';
                        -- ram_EN <= '0';
                    elsif (state = "11") then
                        reg_write_en <= '1';
                        reg_write_in_mux <= 0;
                        reg_write_addr_mux <= 2;
                    end if;


                elsif (opcode(11 downto 10) = "10") then
                    case opcode(9 downto 8) is
                    -- tst #imm, Rn
                        when "00" =>
                            if (state = "10") then
                                reg_read_b_mux <= 2;

                                ALUCmd <= ALUCmd_FBLOCK;
                                FCmd <= "1000";
                                alu_op_a_sel <= 2;
                                alu_op_b_sel <= 2;
                                ALU_special_cmd <= '0';
                                T_LD_sel <= 1;
                            end if;
                            
                        when "01" => 
                        -- and #imm, Rn
                            if (state = "10") then
                                reg_read_b_mux <= 2;
                                
                                ALUCmd <= ALUCmd_FBLOCK;
                                FCmd <= "1000";
                                alu_op_a_sel <= 2;
                                alu_op_b_sel <= 2;
                                ALU_special_cmd <= '0';
                            elsif (state = "11") then
                                reg_write_addr_mux <= 2;
                                reg_write_en <= '1';
                                reg_write_in_mux <= 1;
                            end if;

                            
                        when "10" => 
                        -- xor #imm, Rn
                            if (state = "10") then
                                reg_read_b_mux <= 2;
                                
                                ALUCmd <= ALUCmd_FBLOCK;
                                FCmd <= "0110";
                                alu_op_a_sel <= 2;
                                alu_op_b_sel <= 2;
                                ALU_special_cmd <= '0';
                            elsif (state = "11") then
                                reg_write_addr_mux <= 2;
                                reg_write_en <= '1';
                                reg_write_in_mux <= 1;
                            end if;

                    
                        when "11" => 
                        -- or #imm, Rn
                            if (state = "10") then
                                reg_read_b_mux <= 2;
                                
                                ALUCmd <= ALUCmd_FBLOCK;
                                FCmd <= "1110";
                                alu_op_a_sel <= 2;
                                alu_op_b_sel <= 2;
                                ALU_special_cmd <= '0';
                            elsif (state = "11") then
                                reg_write_addr_mux <= 2;
                                reg_write_en <= '1';
                                reg_write_in_mux <= 1;
                            end if;
                        
                        when others =>
                            null;
                    end case;

                end if;

            -- mov.B/W/L
                

            when "1101" =>
            -- mov.L @(disp:8, PC), Rn
                if (state = "10") then
                    SrcSel <= 2;
                    DispCutoff <= 1;
                    OffsetSel <= 4;
                    PrePostSel <= '0';

                    ram_access_mode <= LONG_ACCESS;
                    ram_RW <= '0';
                    ram_PD <= '1';
                    -- ram_EN <= '1';
                elsif (state = "11") then
                    reg_write_en <= '1';
                    reg_write_in_mux <= 2;
                    reg_write_addr_mux <= 0;
                end if;

            when "1110" =>
            -- mov imm, Rn
                if (state = "10") then
                    alu_op_b_sel <= 1;
                    alu_op_a_sel <= 1;
                    FCmd <= "1010";
                    ALUCmd <= ALUCmd_FBLOCK;
                elsif (state = "11") then
                    reg_write_en <= '1';
                    reg_write_in_mux <= 1;
                    reg_write_addr_mux <= 0;
                end if;
        
            
            when others =>
                null;
        end case;
    
    end process;


-- register input select
    reg_read_a  <= to_integer(unsigned(IR_de(11 downto 8))) when  reg_read_a_mux = '0'  else
                to_integer(unsigned(IR_de(7 downto 4))) when   reg_read_a_mux = '1'  else
                10;     
    reg_read_b <= to_integer(unsigned(IR_de(11 downto 8))) when  reg_read_b_mux = 0  else
                to_integer(unsigned(IR_de(7 downto 4))) when   reg_read_b_mux = 1  else
                0 when   reg_read_b_mux = 2  else
                11;  
    reg_write_addr <= to_integer(unsigned(IR_WB(11 downto 8))) when  reg_write_addr_mux_WB = 0  else
            to_integer(unsigned(IR_WB(7 downto 4))) when   reg_write_addr_mux_WB = 1  else
             0 when   reg_write_addr_mux_WB = 2  else
            12;              

    reg_in_reg_sel_result_ex <= reg_out_b_EX when reg_write_in_sel_regs_EX = 0 else
                            SR when reg_write_in_sel_regs_EX = 1 else
                            GBR when reg_write_in_sel_regs_EX = 2 else
                            MACH when reg_write_in_sel_regs_EX = 4 else
                            MACL when reg_write_in_sel_regs_EX = 5 else
                            PR when reg_write_in_sel_regs_EX = 6 else
                            (31 downto 1 => '0') & SR_T when reg_write_in_sel_regs_EX = 7 else
                            (others => 'X');

    reg_write_in <= addr_writeback_WB when reg_write_in_mux_WB = 0 else
                ALU_result_WB when reg_write_in_mux_WB = 1 else
                ram_data_read when reg_write_in_mux_WB = 2 else
                reg_in_reg_sel_result_WB when reg_write_in_mux_WB = 3 else
                (others => 'X');

-- connecting components

    data_addr_unit:  AddrUnit
        port map (
        -- inputs for base addr
            SrcSel      => SrcSel_EX,       -- singal for selection 
-- to-do: switch this back to PC_EX for pipeline !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            PC          => PC_EX_p4,  -- to-do: might be changed later
            Rn          => reg_out_a_EX,   -- selected when 1
            GBR         => GBR,   -- selected when 2
        -- inputs for offset
            OffsetSel   => OffsetSel_EX,       -- singal for selection 
            R0          => reg_out_R0_EX,  -- selected when 0
            Rm          => reg_out_b_EX,   -- selected when 1
            IncDecVal   => IncDecVal_EX,
            -- selected when +-2 (no shift), +-3 (shift 1), +-4 (shift 2), and 0 for some operations
            Disp     => IR_EX(11 downto 0),  
            -- choose to use last 0 (4 bits), 1 (8 bits) of disp
            DispCutoff  => DispCutoff_EX,       
        -- signals from control unit, directly connect to the wrapped general mau
            PrePostSel => PrePostSel_EX,
        -- outputs, directly connects to the wrapped general mau output
            Address    => ram_data_address,
            AddrSrcOut => addr_writeback_EX
        );



        reg_file: RegFile
            port map (
                clk        => clk,                         -- clock
                data_in    => reg_write_in,     -- the data to write to a register
                write_en   => reg_write_en_WB,                         -- write the data if 1
                write_sel  => reg_write_addr,         -- select which reg to write to
                read_sel_a => reg_read_a,         -- select the first register to output
                read_sel_b => reg_read_b,         -- select the second register to output
                data_out_a => reg_out_a_de,     -- the data output for the first register
                data_out_b => reg_out_b_de,      -- the data output for the second register
                data_out_0 => reg_out_R0_de      -- the data output for the second register
            );
    
        alu_0: ALU
            port map(
                ALUOpA  => reg_out_a_EX,   -- first operand
                ALUOpB  => reg_out_b_EX,   -- second operand
                immd   => IR_EX(7 downto 0),   -- immediate value (IR 7-0)
                T     => SR_T,                       -- T flag 

                op_a_sel  => alu_op_a_sel_EX,
                op_b_sel => alu_op_b_sel_EX,
                adder_cin_sel => adder_cin_sel_EX,
                adder_T_out_sel => adder_T_out_sel_EX,
                ALU_special_cmd => ALU_special_cmd_EX,
                MULCmd => MULCmd_EX,
                AddSub   => AddSub_EX,                       -- 1 for add, 0 for sub, always the 2nd bit of IR for SH2!  
                ALUCmd   => ALUCmd_EX,    -- ALU result select
                FCmd    => FCmd_EX,    -- F-Block operation
                SCmd    => SCmd_EX,    -- shift operation
                
                Result  => ALU_result_ex,   -- ALU result
                MACH_out => ALU_MACH_out,
                T_out => ALU_T_out
            );

        ram_rounting_0: RAMRouting
            port map (
                clk => clk,
                EN  => ram_EN_EX,      -- 1 for enable, 0 for disable
                PD  => ram_PD_EX,      -- 0 for program, 1 for data memory 
                RW  => ram_RW_EX,      -- 0 for read, 1 for write, force read if program
                access_mode => ram_access_mode_EX,       -- force WORD_ACCESS if program     
                program_address => PC_pre,   -- memory address bus
                data_address  => ram_data_address,   -- memory address bus
                write_data => reg_out_b_EX,
                read_data => ram_data_read,

                DB_write => DB_write,
                DB_read  => DB_read,
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

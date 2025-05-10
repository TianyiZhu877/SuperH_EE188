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
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use work.RamAccessMode.all;

--library opcodes;
--use opcodes.opcodes.all;


entity  SH2_CPU  is

    port (
        Reset   :  in     std_logic;                       -- reset signal (active low)
        NMI     :  in     std_logic;                       -- non-maskable interrupt signal (falling edge)
        INT     :  in     std_logic;                       -- maskable interrupt signal (active low)
        clock   :  in     std_logic;                       -- system clock
        AB      :  out    std_logic_vector(31 downto 0);   -- memory address bus
        RE0     :  out    std_logic;                       -- first byte active low read enable
        RE1     :  out    std_logic;                       -- second byte active low read enable
        RE2     :  out    std_logic;                       -- third byte active low read enable
        RE3     :  out    std_logic;                       -- fourth byte active low read enable
        WE0     :  out    std_logic;                       -- first byte active low write enable
        WE1     :  out    std_logic;                       -- second byte active low write enable
        WE2     :  out    std_logic;                       -- third byte active low write enable
        WE3     :  out    std_logic;                       -- fourth byte active low write enable
        DB      :  inout  std_logic_vector(31 downto 0)    -- memory data bus
    );

end  SH2_CPU;




architecture  structural  of  SH2_CPU  is
    component RAMRouting
        port (
            PD          : in    std_logic;      -- 0 for program, 1 for data memory 
            RW          : in    std_logic;      -- 0 for read, 1 for write, force read if program
            access_mode : in    std_logic_vector(1 downto 0);       -- force WORD_ACCESS if program     
            program_address      :  in    std_logic_vector(31 downto 0);   -- memory address bus
            data_address      :  in    std_logic_vector(31 downto 0);   -- memory address bus
            
            RE0     :  out    std_logic;                       -- first byte active low read enable
            RE1     :  out    std_logic;                       -- second byte active low read enable
            RE2     :  out    std_logic;                       -- third byte active low read enable
            RE3     :  out    std_logic;                       -- fourth byte active low read enable
            WE0     :  out    std_logic;                       -- first byte active low write enable
            WE1     :  out    std_logic;                       -- second byte active low write enable
            WE2     :  out    std_logic;                       -- third byte active low write enable
            WE3     :  out    std_logic;                       -- fourth byte active low write enable
            exception   : out   std_logic;
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
            Cin      : in      std_logic;                       -- carry in
    
            AddSub   : in      std_logic;                       -- 1 for add, 0 for sub, always the 2nd bit of IR for SH2!  
            ALUCmd   : in      std_logic_vector(1 downto 0);    -- ALU result select
            FCmd     : in      std_logic_vector(3 downto 0);    -- F-Block operation
            SCmd     : in      std_logic_vector(2 downto 0);    -- shift operation

            Result   : buffer  std_logic_vector(31 downto 0);   -- ALU result
            Cout     : out     std_logic;                       -- carry out
            Overflow : out     std_logic                        -- signed overflow
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
            data_out_b : out std_logic_vector(31 downto 0)      -- the data output for the second register
        );
    end component;

    
-- control signals
    -- memory
    signal SrcSel_P      :    integer  range 3 downto 0;       -- singal for selection 
    signal IncDecVal_P   :     std_logic_vector(3 downto 0);
    signal OffsetSel_P   :    integer  range 4 downto 0;       -- singal for selection 
    signal DispCutoff_P  :    integer  range 1 downto 0;       
    signal PrePostSel_P :      std_logic;
    signal SrcSel_D      :    integer  range 1 downto 0;       -- singal for selection 
    signal OffsetSel_D   :    integer  range 2 downto 0;       -- singal for selection 
    signal DispCutoff_D  :    integer  range 1 downto 0;       
    signal PrePostSel_D :      std_logic;
    -- ALU
    signal AddSub   :   std_logic;                       -- 1 for add, 0 for sub, always the bit2 or 1 when 0111!  
    signal ALUCmd   :   std_logic_vector(1 downto 0);    -- ALU result select
    signal FCmd     :   std_logic_vector(3 downto 0);    -- F-Block operation
    signal SCmd     :   std_logic_vector(2 downto 0);    -- shift operation
    -- register
    signal reg_a_mux : std_logic;         -- select the first register to output
    signal reg_b_mux : integer range 2 downto 0;         -- select the first register to output
    signal reg_write_mux : integer range 2 downto 0;         -- select the register to write
    


-- interconnect signals
    signal reg_sel_a : integer range 15 downto 0;         -- select the first register to output
    signal reg_sel_b : integer range 15 downto 0;         -- select the second register to output
    signal reg_out_a : std_logic_vector(31 downto 0);     -- the data output for the first register
    signal reg_out_b : std_logic_vector(31 downto 0)      -- the data output for the second register

begin

    program_address      :  in    std_logic_vector(31 downto 0);   -- memory address bus
    data_address      :  in    std_logic_vector(31 downto 0);   -- memory address bus
    
end structural;

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
    component SH2_CPU_wrapped

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
    end component;


    signal DB_read                     : std_logic_vector(31 downto 0) := (others => 'Z');
    signal DB_write                     : std_logic_vector(31 downto 0) := (others => 'Z');

    signal PC_reset_addr_debug    : std_logic_vector(31 downto 0) := (others => '0');    
    signal opcode_debug : std_logic_vector(15 downto 0);
    signal PC_debug:   std_logic_vector(31 downto 0);
    signal PC_LD_sel_debug:  integer range 0 to 3; 
    signal ram_data_read_debug :  std_logic_vector(31 downto 0);
    signal reg_out_a_debug :  std_logic_vector(31 downto 0);
    signal exception_debug :  std_logic;

    
    signal AB_trimmed :  std_logic_vector(31 downto 0);

begin

    -- AB <= '0' & AB_trimmed(30 downto 0);
    -- AB <= (others => '0');
    AB <= AB_trimmed;

    cpu_wrapped: SH2_CPU_wrapped
        port map (
            Reset               => Reset,
            NMI                 => NMI,
            INT                 => INT,
            clk                 => clock,
            AB                  => AB_trimmed,
            RE0                 => RE0,
            RE1                 => RE1,
            RE2                 => RE2,
            RE3                 => RE3,
            WE0                 => WE0,
            WE1                 => WE1,
            WE2                 => WE2,
            WE3                 => WE3,
            DB_read  => DB_read,
            DB_write  => DB_write,

            PC_reset_addr_debug => (others => '0'),
            -- PC_reset_addr_debug =>  x"00001000";

            opcode_debug            => opcode_debug,
            PC_debug            => PC_debug,
            PC_LD_sel_debug     => PC_LD_sel_debug,
            ram_data_read_debug => ram_data_read_debug,
            reg_out_a_debug => reg_out_a_debug,
            exception_debug => exception_debug
        );

    process (clock) begin
        DB_read <= DB;
    end process;

    DB <= DB_write;

end structural;

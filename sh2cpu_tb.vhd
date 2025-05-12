library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity SH2_CPU_tb is
end entity;

architecture sim of SH2_CPU_tb is

    signal Reset                  : std_logic := '1';
    signal NMI                    : std_logic := '1';
    signal INT                    : std_logic := '1';
    signal clk                  : std_logic := '0';
    signal AB                     : std_logic_vector(31 downto 0);
    signal RE0, RE1, RE2, RE3     : std_logic;
    signal WE0, WE1, WE2, WE3     : std_logic;
    signal DB                     : std_logic_vector(31 downto 0) := (others => 'Z');

    signal PC_reset_addr_debug    : std_logic_vector(31 downto 0) := (others => '0');    
    signal IR_debug : std_logic_vector(15 downto 0);
    signal PC_debug:   std_logic_vector(31 downto 0);
    signal ram_data_address_debug:   std_logic_vector(31 downto 0);
    signal PC_LD_sel_debug:  integer range 0 to 3; 
    signal ram_data_read_debug :  std_logic_vector(31 downto 0);
    signal reg_out_a_debug :  std_logic_vector(31 downto 0);
    signal exception_debug :  std_logic;

    component  MEMORY32x32
        generic (
            MEMSIZE     : integer := 256;   -- default size is 256 words
            START_ADDR0 : integer;          -- starting address of first block
            START_ADDR1 : integer;          -- starting address of second block
            START_ADDR2 : integer;          -- starting address of third block
            START_ADDR3 : integer           -- starting address of fourth block
        );
    
        port (
            clk     : std_logic;
            RE0    : in     std_logic;      -- low byte read enable (active low)
            RE1    : in     std_logic;      -- byte 1 read enable (active low)
            RE2    : in     std_logic;      -- byte 2 read enable (active low)
            RE3    : in     std_logic;      -- high byte read enable (active low)
            WE0    : in     std_logic;      -- low byte write enable (active low)
            WE1    : in     std_logic;      -- byte 1 write enable (active low)
            WE2    : in     std_logic;      -- byte 2 write enable (active low)
            WE3    : in     std_logic;      -- high byte write enable (active low)
            MemAB  : in     std_logic_vector(31 downto 0);  -- memory address bus
            MemDB  : inout  std_logic_vector(31 downto 0)   -- memory data bus
        );
    end  component;

    component  SH2_CPU
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
    end component;

    -- Clock period constant
    constant clk_period : time := 10 ns;

begin

    -- Instantiate the CPU
    uut: SH2_CPU
        port map (
            Reset               => Reset,
            NMI                 => NMI,
            INT                 => INT,
            clk               => clk,
            AB                  => AB,
            RE0                 => RE0,
            RE1                 => RE1,
            RE2                 => RE2,
            RE3                 => RE3,
            WE0                 => WE0,
            WE1                 => WE1,
            WE2                 => WE2,
            WE3                 => WE3,
            DB                  => DB,

            PC_reset_addr_debug => PC_reset_addr_debug,
            IR_debug            => IR_debug,
            PC_debug            => PC_debug,
            PC_LD_sel_debug     => PC_LD_sel_debug,
            ram_data_read_debug => ram_data_read_debug,
            reg_out_a_debug => reg_out_a_debug,
            exception_debug => exception_debug
        );


    ram:  MEMORY32x32
        generic map(
            START_ADDR0 => 0,
            START_ADDR1 => 256,
            START_ADDR2 => 512,
            START_ADDR3 => 768
        )
        port map(
            clk => clk,
            MemAB                  => AB,
            RE0                 => RE0,
            RE1                 => RE1,
            RE2                 => RE2,
            RE3                 => RE3,
            WE0                 => WE0,
            WE1                 => WE1,
            WE2                 => WE2,
            WE3                 => WE3,
            MemDB                  => DB
        );

    -- clk generation
    clk_process : process
    begin
        while true loop
            clk <= '0';
            wait for clk_period / 2;
            clk <= '1';
            wait for clk_period / 2;
        end loop;
    end process;

    -- Stimulus process
    stim_proc: process
    begin
        -- Initial values
        PC_reset_addr_debug <= x"00000000";
        Reset <= '1';
        NMI <= '1';
        INT <= '1';

        wait for 30 ns;
        Reset <= '0'; -- Assert reset
        wait for 20 ns;
        Reset <= '1'; -- Deassert reset

        -- Wait for some time to observe behavior
        wait for 5 us;

        -- End simulation
        -- assert false report "End of simulation." severity failure;
    end process;

end architecture;

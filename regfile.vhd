library ieee;
use ieee.std_logic_1164.all;


entity RegFile is
    port (
        clk        : in  std_logic;                         -- clock
        data_in    : in  std_logic_vector(31 downto 0);     -- the data to write to a register
        write_en   : in  std_logic;                         -- write the data if 1
        write_sel  : in  integer range 15 downto 0;         -- select which reg to write to
        read_sel_a : in  integer range 15 downto 0;         -- select the first register to output
        read_sel_b : in  integer range 15 downto 0;         -- select the second register to output
        data_out_a : out std_logic_vector(31 downto 0);     -- the data output for the first register
        data_out_b : out std_logic_vector(31 downto 0);     -- the data output for the second register
        data_out_0 : out std_logic_vector(31 downto 0)      -- the data output always connected to register 0 register
    );
end RegFile;

architecture behavioral of RegFile is

    component RegArray is
        generic (
            regcnt   : integer := 32;    -- default number of registers is 32
            wordsize : integer := 8      -- default width is 8-bits
        );
    
        port(
            RegIn      : in   std_logic_vector(wordsize - 1 downto 0);
            RegInSel   : in   integer  range regcnt - 1 downto 0;
            RegStore   : in   std_logic;
            RegASel    : in   integer  range regcnt - 1 downto 0;
            RegBSel    : in   integer  range regcnt - 1 downto 0;
            RegAxIn    : in   std_logic_vector(wordsize - 1 downto 0);
            RegAxInSel : in   integer  range regcnt - 1 downto 0;
            RegAxStore : in   std_logic;
            RegA1Sel   : in   integer  range regcnt - 1 downto 0;
            RegA2Sel   : in   integer  range regcnt - 1 downto 0;
            RegDIn     : in   std_logic_vector(2 * wordsize - 1 downto 0);
            RegDInSel  : in   integer  range regcnt/2 - 1 downto 0;
            RegDStore  : in   std_logic;
            RegDSel    : in   integer  range regcnt/2 - 1 downto 0;
            clock      : in   std_logic;
            RegA       : out  std_logic_vector(wordsize - 1 downto 0);
            RegB       : out  std_logic_vector(wordsize - 1 downto 0);
            RegA1      : out  std_logic_vector(wordsize - 1 downto 0);
            RegA2      : out  std_logic_vector(wordsize - 1 downto 0);
            RegD       : out  std_logic_vector(2 * wordsize - 1 downto 0)
        );
    end component;



begin

    u_reg_array: RegArray
        generic map (
            regcnt   => 16,         -- 16 registers for sh2
            wordsize => 32          -- 32 bit wordsize for sh2
        )
        port map (
            RegIn      => data_in,
            RegInSel   => write_sel,
            RegStore   => write_en,
            RegASel    => read_sel_a,
            RegBSel    => read_sel_b,
            RegAxIn    => (others => '0'),
            RegAxInSel => 0,
            RegAxStore => '0',
            RegA1Sel   => 0,
            RegA2Sel   => 0,
            RegDIn     => (others => '0'),
            RegDInSel  => 0,
            RegDStore  => '0',
            RegDSel    => 0,
            clock      => clk,
            RegA       => data_out_a,
            RegB       => data_out_b,
            RegA1      => data_out_0,
            RegA2      => open,
            RegD       => open
        );

end behavioral;

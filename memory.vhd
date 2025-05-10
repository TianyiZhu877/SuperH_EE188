library ieee;
use ieee.std_logic_1164.all;
package RamAccessMode is
    constant BYTE_ACCESS : std_logic_vector(1 downto 0) := "00";  
    constant WORD_ACCESS : std_logic_vector(1 downto 0) := "01";  
    constant LONG_ACCESS : std_logic_vector(1 downto 0) := "10"; 
end package;


library ieee;
use ieee.std_logic_1164.all;
use work.RamAccessMode.all;

entity RAM is
    generic (
        MEMSIZE     : integer := 256;   -- default size is 256 words
        START_ADDR0 : integer := 0;          -- starting address of first block
        START_ADDR1 : integer := 256;   -- starting address of second block
        START_ADDR2 : integer := 512;   -- starting address of third block
        START_ADDR3 : integer := 768           -- starting address of fourth block
    );

    port (
        access_mode : in    std_logic_vector(1 downto 0);
        RW          : in    std_logic;
        MemAB       : in    std_logic_vector(31 downto 0);  -- memory address bus

        exception   : out   std_logic;
        MemDB       : inout std_logic_vector(31 downto 0)   -- memory data bus
    );

end RAM;


architecture  behavioral  of  RAM  is

    component  MEMORY32x32

        generic (
            MEMSIZE     : integer := 256;   -- default size is 256 words
            START_ADDR0 : integer;          -- starting address of first block
            START_ADDR1 : integer;          -- starting address of second block
            START_ADDR2 : integer;          -- starting address of third block
            START_ADDR3 : integer           -- starting address of fourth block
        );
    
        port (
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

    signal byte0: std_logic;
    signal byte1: std_logic;
    signal byte2: std_logic;
    signal byte3: std_logic;

    signal WE0: std_logic;
    signal WE1: std_logic;
    signal WE2: std_logic;
    signal WE3: std_logic;

    signal RE0: std_logic;
    signal RE1: std_logic;
    signal RE2: std_logic;
    signal RE3: std_logic;

begin
    
    process(all) begin

        case access_mode is

            when BYTE_ACCESS =>

                case MemAB(1 downto 0) is
                    when "00" =>
                        byte0 <= '1';
                        byte1 <= '0';
                        byte2 <= '0';
                        byte3 <= '0';
                    when "01" =>
                        byte0 <= '0';
                        byte1 <= '1';
                        byte2 <= '0';
                        byte3 <= '0';
                    when "10" =>
                        byte0 <= '0';
                        byte1 <= '0';
                        byte2 <= '1';
                        byte3 <= '0';
                    when "11" =>
                        byte0 <= '0';
                        byte1 <= '0';
                        byte2 <= '0';
                        byte3 <= '1';
                    when others =>
                        byte0 <= 'X';
                        byte1 <= 'X';
                        byte2 <= 'X';
                        byte3 <= 'X';
                end case;

                exception <= '0';



            when WORD_ACCESS =>
                case MemAB(1) is
                    when '0' =>
                        byte0 <= '1';
                        byte1 <= '1';
                        byte2 <= '0';
                        byte3 <= '0';
                    when '1' =>
                        byte0 <= '0';
                        byte1 <= '0';
                        byte2 <= '1';
                        byte3 <= '1';
                    when others =>
                        byte0 <= 'X';
                        byte1 <= 'X';
                        byte2 <= 'X';
                        byte3 <= 'X';
                end case;

                exception <= MemAB(0);



            when LONG_ACCESS =>
                byte0 <= '1';
                byte1 <= '1';
                byte2 <= '1';
                byte3 <= '1';

                exception <= MemAB(0) or MemAB(1);

            when others =>
                exception <= 'X';
                byte0 <= 'X';
                byte1 <= 'X';
                byte2 <= 'X';
                byte3 <= 'X';
                
        end case;

    end process;

    RE0 <= byte0 and not(RW);
    RE1 <= byte1 and not(RW);
    RE2 <= byte2 and not(RW);
    RE3 <= byte3 and not(RW);

    
    WE0 <= byte0 and RW;
    WE1 <= byte1 and RW;
    WE2 <= byte2 and RW;
    WE3 <= byte3 and RW;


    memory_general: MEMORY32x32
        generic map (
            MEMSIZE     => MEMSIZE,
            START_ADDR0 => START_ADDR0,
            START_ADDR1 => START_ADDR1,
            START_ADDR2 => START_ADDR2,
            START_ADDR3 => START_ADDR3
        )
        port map (
            RE0    => RE0,
            RE1    => RE1,
            RE2    => RE2,
            RE3    => RE3,
            WE0    => WE0,
            WE1    => WE1,
            WE2    => WE2,
            WE3    => WE3,
            MemAB  => MemAB,
            MemDB  => MemDB
        );
end behavioral;





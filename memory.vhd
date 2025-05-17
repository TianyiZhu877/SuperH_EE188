----------------------------------------------------------------------------
--
--  Memory Subsystem
--
--  This component describes the memory for a 32-bit byte-addressable CPU
--  with a 32-bit address bus.  Only a portion of the full address space is
--  filled in.  Addresses outside the filled in range return 'X' when read
--  and generate error messages when written.
--
--  Revision History:
--     28 Apr 25  Glen George       Initial revision.
--     29 Apr 25  Glen George       Fixed some syntax errors.
--     29 Apr 25  Glen George       Fixed inconsistencies in byte vs word
--                                  addressing.
--
----------------------------------------------------------------------------


--
--  MEMORY32x32
--
--  This is a memory component that supports a byte-addressable 32-bit wide
--  memory with 32-bits of address.  No timing restrictions are implemented,
--  but if the address bus changes while a WE signal is active an error is
--  generated.  Only a portion of the memory is actually usable.  Addresses
--  outside of the four usable ranges return 'X' on read and generate error
--  messages on write.  The size and address of each memory chunk are generic
--  parameters.
--
--  Generics:
--    MEMSIZE     - size of the four memory blocks in 32-bit words
--    START_ADDR0 - starting address of first memory block/chunk
--    START_ADDR1 - starting address of second memory block/chunk
--    START_ADDR2 - starting address of third memory block/chunk
--    START_ADDR3 - starting address of fourth memory block/chunk
--
--  Inputs:
--    RE0    - low byte read enable (active low)
--    RE1    - byte 1 read enable (active low)
--    RE2    - byte 2 read enable (active low)
--    RE3    - high byte read enable (active low)
--    WE0    - low byte write enable (active low)
--    WE1    - byte 1 write enable (active low)
--    WE2    - byte 2 write enable (active low)
--    WE3    - high byte write enable (active low)
--    MemAB  - memory address bus (32 bits)
--
--  Inputs/Outputs:
--    MemDB  - memory data bus (32 bits)
--

library ieee;
use ieee.std_logic_1164.all;
-- use ieee.std_logic_arith.all;
-- use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
-- use IEEE.std_logic_textio.all;  -- Needed to print std_logic_vector
use std.textio.all;



entity  MEMORY32x32  is

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

end  MEMORY32x32;


architecture  behavioral  of  MEMORY32x32  is

    -- define the type for the RAM chunks
    type  RAMtype  is array (0 to MEMSIZE - 1) of std_logic_vector(31 downto 0);

    -- now define the RAMs (initialized to X)
    signal  RAMbits0  :  RAMtype := (
        -- 0 => "00000000000010000100000000000111"
        -- 0 => (15 downto 0 => "0000000000001000", 31 downto 16 => "0100000000000111"),
        -- 1 => (15 downto  0 => "0100000000000100", 31 downto 16 => "0000000000001001"),
        -- 2 => (15 downto  0 => "0000000000000011", 31 downto 16 => "0100000000000010"),
        -- 3 => (15 downto  0 => "0000000000000000", 31 downto 16 => "0000000001000001"),
        -- 4 => (15 downto  0 => "0000000000000000", 31 downto 16 => "0100000001000001"),
        -- 5 => (15 downto  0 => "0000000001000010", 31 downto 16 => "0000000001000011"),
        -- 6 => ( 31 downto 16=> "0000000001000100", 15 downto  0 => "0000000000011011"),    -- SLEEP
        -- 7 => (15 downto  0 => "0000000000011100", 31 downto 16 => "0000000000011101"),
        -- 8 => (15 downto  0 => "0000000001011110", 31 downto 16 => "0000000000011111"),
        others => (others => 'Z')
    );
    -- signal  RAMbits0  :  RAMtype  := (others => (others => 'X'));
    signal  RAMbits1  :  RAMtype  := (others => (others => 'X'));
    signal  RAMbits2  :  RAMtype  := (others => (others => 'X'));
    signal  RAMbits3  :  RAMtype  := (others => (others => 'X'));

    -- general read and write signals
    signal  RE  :  std_logic;
    signal  WE  :  std_logic;


    -- data read from memory
    signal  MemData  :  std_logic_vector(31 downto 0);


    procedure load_ram(
        variable ram : inout RAMtype;
        constant filename : in string
    ) is
        file f       : text open read_mode is filename;
        variable line_buf : line;
        variable hex_str  : string(1 to 8);
        variable word_val : std_logic_vector(31 downto 0);
    begin
        for i in ram'range loop
            exit when endfile(f);
            readline(f, line_buf);
            -- read(line_buf, hex_str);
            -- word_val := std_logic_vector(to_unsigned(to_integer(image => hex_str), 32));
            hread(line_buf, word_val);
            ram(i) := word_val;
            -- report "Value read: " & to_hstring(ram(i));
            -- report "word_val: " & to_hstring(word_val);
        end loop;
    end procedure;

begin   

    -- compute the general read and write signals (active low signals)
    RE  <=  RE0  and  RE1  and  RE2  and  RE3;
    WE  <=  WE0  and  WE1  and  WE2  and  WE3;



    process(clk)
    begin

        -- wait for an input to change
        -- wait on  RE, RE0, RE1, RE2, RE3, WE, WE0, WE1, WE2, WE3, MemAB;
        if  rising_edge(clk)  then

            -- first check if reading
            if  (RE = '0')  then
                -- reading, put the data out (check the address)
                if  ((to_integer(unsigned(MemAB)) >= START_ADDR0) and
                    ((to_integer(unsigned(MemAB)) - START_ADDR0) < 4 * MEMSIZE))  then
                    MemDB <= RAMbits0(to_integer(unsigned(MemAB(31 downto 2))) - START_ADDR0 / 4);
                elsif  ((to_integer(unsigned(MemAB)) >= START_ADDR1) and
                    ((to_integer(unsigned(MemAB)) - START_ADDR1) < 4 * MEMSIZE))  then
                    MemDB <= RAMbits1(to_integer(unsigned(MemAB(31 downto 2)) - START_ADDR1 / 4));
                elsif  ((to_integer(unsigned(MemAB)) >= START_ADDR2) and
                    ((to_integer(unsigned(MemAB)) - START_ADDR2) < 4 * MEMSIZE))  then
                    MemDB <= RAMbits2(to_integer(unsigned(MemAB(31 downto 2))) - START_ADDR2 / 4);
                elsif  ((to_integer(unsigned(MemAB)) >= START_ADDR3) and
                    ((to_integer(unsigned(MemAB)) - START_ADDR3) < 4 * MEMSIZE))  then
                    MemDB <= RAMbits3(to_integer(unsigned(MemAB(31 downto 2))) - START_ADDR3 / 4);
                else
                    -- outside of any allowable address range - set output to X
                    MemDB <= (others => 'X');
                end if;

                -- only set the bytes that are being read
                if  RE0 /= '0'  then
                    MemDB(7 downto 0) <= (others => 'Z');
                end if;
                if  RE1 /= '0'  then
                    MemDB(15 downto 8) <= (others => 'Z');
                end if;
                if  RE2 /= '0'  then
                    MemDB(23 downto 16) <= (others => 'Z');
                end if;
                if  RE3 /= '0'  then
                    MemDB(31 downto 24) <= (others => 'Z');
                end if;

            else

                -- not reading, send data bus to hi-Z
                MemDB <= (others => 'Z');
            end if;

            -- now check if writing
            -- if  (WE'event and (WE = '1'))  then
            if  (WE = '0')  then
                -- rising edge of write - write the data (check which address range)
                -- first get current value of the byte
                if  ((to_integer(unsigned(MemAB)) >= START_ADDR0) and
                    (to_integer(unsigned(MemAB)) - START_ADDR0 < (4 * MEMSIZE)))  then
                    MemData <= RAMbits0(to_integer(unsigned(MemAB(31 downto 2))) - START_ADDR0 / 4);
                elsif  (((to_integer(unsigned(MemAB))) >= START_ADDR1) and
                        (to_integer(unsigned(MemAB)) - START_ADDR1) < (4 * MEMSIZE))  then
                    MemData <= RAMbits1(to_integer(unsigned(MemAB(31 downto 2))) - START_ADDR1 / 4);
                elsif  (((to_integer(unsigned(MemAB))) >= START_ADDR2) and
                (to_integer(unsigned(MemAB)) - START_ADDR2) < (4 * MEMSIZE)) then
                    MemData <= RAMbits2(to_integer(unsigned(MemAB(31 downto 2))) - START_ADDR2 / 4);
                elsif  (((to_integer(unsigned(MemAB))) >= START_ADDR3) and
                        (to_integer(unsigned(MemAB)) - START_ADDR3) < (4 * MEMSIZE))  then
                    MemData <= RAMbits3(to_integer(unsigned(MemAB(31 downto 2))) - START_ADDR3 / 4);
                else
                    MemData <= (others => 'X');
                    -- null;
                end if;

                -- now update the data based on the write enable signals
                -- set any byte being written to its new value
                if  WE0 = '0'  then
                    MemData(7 downto 0) <= MemDB(7 downto 0);
                end if;
                if  WE1 = '0'  then
                    MemData(15 downto 8) <= MemDB(15 downto 8);
                end if;
                if  WE2 = '0'  then
                    MemData(23 downto 16) <= MemDB(23 downto 16);
                end if;
                if  WE3 = '0'  then
                    MemData(31 downto 24) <= MemDB(31 downto 24);
                end if;

                -- finally write the updated value to memory
                if  ((to_integer(unsigned(MemAB)) >= START_ADDR0) and
                    (to_integer(unsigned(MemAB)) - START_ADDR0) < (4 * MEMSIZE))  then
                    -- "writing to RAMbits0 (" & 
                    -- & ") = " & MemData;
                    RAMbits0(to_integer(unsigned(MemAB(31 downto 2))) - START_ADDR0 / 4) <= MemData;
                    -- RAMbits0(0) <= (others =>'1');
                    -- report "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!writing to RAMbits0 (";
                    -- report to_hstring(std_logic_vector(to_unsigned(to_integer(unsigned(MemAB(31 downto 2))) - START_ADDR0 / 4, 32)));
                    null;
                elsif  ((to_integer(unsigned(MemAB)) >= START_ADDR1) and
                        (to_integer(unsigned(MemAB)) - START_ADDR1) < (4 * MEMSIZE))  then
                    RAMbits1(to_integer(unsigned(MemAB(31 downto 2))) - START_ADDR1 / 4) <= MemData;
                elsif  ((to_integer(unsigned(MemAB)) >= START_ADDR2) and
                    (to_integer(unsigned(MemAB)) - START_ADDR2) < (4 * MEMSIZE))  then
                    RAMbits2(to_integer(unsigned(MemAB(31 downto 2))) - START_ADDR2 / 4) <= MemData;
                elsif  ((to_integer(unsigned(MemAB)) >= START_ADDR3) and
                    (to_integer(unsigned(MemAB)) - START_ADDR3) < (4 * MEMSIZE))  then
                    RAMbits3(to_integer(unsigned(MemAB(31 downto 2))) - START_ADDR3 / 4) <= MemData;
                else
                    -- outside of any allowable address range - generate an error
                    assert (false)
                        report  "Attempt to write to a non-existant address"
                        severity  WARNING;
                end if;

                -- wait for the update to happen
                -- wait for 0 ns;

            end if;

        end if;
        -- finally check if WE low with the address changing



        if  (MemAB'event and (WE = '0'))  then
            -- output error message
            REPORT "Glitch on Memory Address bus"
            SEVERITY  ERROR;
        end if;


    end process;


    process
        variable ram_temp : RAMtype;
    begin
        -- RAMbits0(0) <= "00000000000010000100000000000111";
        report "Initial RAMbits0(0): " & to_hstring(RAMbits0(0));
        wait for 0 ns;
        -- RAMbits0(0) <= x"01234567";
        -- RAMbits0(10) <= x"76543210";
        -- wait for 0 ns;
        -- RAMbits0(0)(15 downto  0) <= "0000000000001000";
        -- RAMbits0(0)(31 downto 16) <= "0100000000000111";

        load_ram(ram_temp, "test_mem_content/0.txt");
        RAMbits0 <= ram_temp;
        -- wait;
        wait for 0 ns;
        report "\n***********\nFinal RAMbits0(0): " & to_hstring(RAMbits0(0)) & "    \n***********\n";
        report "\n***********\nFinal RAMbits0(8): " & to_hstring(RAMbits0(8)) & "    \n***********\n";
        wait;
    end process;



end  behavioral;

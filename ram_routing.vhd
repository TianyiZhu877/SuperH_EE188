library ieee;
use ieee.std_logic_1164.all;
package RamAccessMode is
    constant BYTE_ACCESS : std_logic_vector(1 downto 0) := "00";  
    constant WORD_ACCESS : std_logic_vector(1 downto 0) := "01";  
    constant LONG_ACCESS : std_logic_vector(1 downto 0) := "10"; 
end package;

-- the entire system is byte addressed with big endianess (0th byte at 31 downto 24),
-- but in small endian within a word 
library ieee;
use ieee.std_logic_1164.all;
use work.RamAccessMode.all;

entity RAMRouting is
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

end RAMRouting;


architecture  structural  of  RAMRouting  is

    signal byte0:   std_logic;
    signal byte1:   std_logic;
    signal byte2:   std_logic;
    signal byte3:   std_logic;

    signal MemAB:   std_logic_vector(31 downto 0); 

    -- signal write_data_internal : std_logic_vector(31 downto 0);
    -- signal read_data_internal  : std_logic_vector(31 downto 0);

    -- buffers
    signal last_access_mode: std_logic_vector(1 downto 0);
    signal last_ab_10: std_logic_vector(1 downto 0);

begin
    process(clk) begin
        if  rising_edge(clk)  then
            last_access_mode <= access_mode;
            last_ab_10 <= MemAB(1 downto 0);
        end if;
    end process;


    AB <= MemAB;
    -- AB <= (others => '0');
    MemAB <= program_address  when   PD = '0' else
            data_address   when   PD = '1' else
            (others => 'X');



    process(all) begin
        DB_write <= (others => 'X');

        case access_mode is

            when BYTE_ACCESS =>

                case MemAB(1 downto 0) is
                    when "00" =>
                        byte0 <= '0';
                        byte1 <= '0';
                        byte2 <= '0';
                        byte3 <= '1';
                        DB_write(31 downto 24) <= write_data(7 downto 0);

                    when "01" =>
                        byte0 <= '0';
                        byte1 <= '0';
                        byte2 <= '1';
                        byte3 <= '0';
                        DB_write(23 downto 16) <= write_data(7 downto 0);

                    when "10" =>
                        byte0 <= '0';
                        byte1 <= '1';
                        byte2 <= '0';
                        byte3 <= '0';
                        DB_write(15 downto 8) <= write_data(7 downto 0);

                    when "11" =>
                        byte0 <= '1';
                        byte1 <= '0';
                        byte2 <= '0';
                        byte3 <= '0';
                        DB_write(7 downto 0) <= write_data(7 downto 0);

                    when others =>
                        byte0 <= 'X';
                        byte1 <= 'X';
                        byte2 <= 'X';
                        byte3 <= 'X';
                        DB_write <= (others => 'X');

                end case;

                exception <= '0';



            when WORD_ACCESS =>
                case MemAB(1) is
                    when '0' =>
                        byte0 <= '0';
                        byte1 <= '0';
                        byte2 <= '1';
                        byte3 <= '1';
                        DB_write(31 downto 16) <= write_data(15 downto 0);

                    when '1' =>
                        byte0 <= '1';
                        byte1 <= '1';
                        byte2 <= '0';
                        byte3 <= '0';
                        DB_write(15 downto 0) <= write_data(15 downto 0);

                    when others =>
                        byte0 <= 'X';
                        byte1 <= 'X';
                        byte2 <= 'X';
                        byte3 <= 'X';
                        -- read_data(7 downto 0) <= (others => 'X');
                        DB_write <= (others => 'X');
                end case;

                exception <= MemAB(0);


            when LONG_ACCESS =>
                byte0 <= '1';
                byte1 <= '1';
                byte2 <= '1';
                byte3 <= '1';
                DB_write <= write_data;

                exception <= MemAB(0) or MemAB(1);

            when others =>
                exception <= 'X';
                byte0 <= 'X';
                byte1 <= 'X';
                byte2 <= 'X';
                byte3 <= 'X';
                DB_write <= (others => 'X');
                
        end case;



-- for read_data output routing
        case last_access_mode is
            when BYTE_ACCESS =>
                case last_ab_10 is
                    when "00" =>
                        read_data(7 downto 0) <= DB_read(31 downto 24);
                    when "01" =>
                        read_data(7 downto 0) <= DB_read(23 downto 16);
                    when "10" =>
                        read_data(7 downto 0) <= DB_read(15 downto 8);
                    when "11" =>
                        read_data(7 downto 0) <= DB_read(7 downto 0);
                    when others =>
                        read_data(7 downto 0) <= (others => 'X');
                end case;
                    read_data(31 downto 8) <= (31 downto 8 => read_data(7));

            when WORD_ACCESS =>
                case last_ab_10(1) is
                    when '0' =>
                        read_data(15 downto 0) <= DB_read(31 downto 16);
                    when '1' =>
                        read_data(15 downto 0) <= DB_read(15 downto 0);
                    when others =>
                        read_data(15 downto 0) <= (others => 'X');
                end case;
                read_data(31 downto 16) <= (31 downto 16 => read_data(15));

            when LONG_ACCESS =>
                read_data <= DB_read;

            when others =>
                read_data <= (others => 'X');
                
        end case;

    end process;

    -- read_data_internal <= DB;
    -- DB <= write_data_internal  when ((EN = '1' and RW = '1')) else (others => 'Z');

    RE0 <= not((byte0 and not(RW)) and EN);
    RE1 <= not((byte1 and not(RW)) and EN);
    RE2 <= not((byte2 and not(RW)) and EN);
    RE3 <= not((byte3 and not(RW)) and EN);

    
    WE0 <= not(byte0 and RW and EN);
    WE1 <= not(byte1 and RW and EN);
    WE2 <= not(byte2 and RW and EN);
    WE3 <= not(byte3 and RW and EN);


end structural;





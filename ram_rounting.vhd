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

entity RAMRouting is
    port (
        EN          : in    std_logic;      -- 1 for enable, 0 for disable
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

end RAMRouting;


architecture  structural  of  RAMRouting  is

    signal byte0:   std_logic;
    signal byte1:   std_logic;
    signal byte2:   std_logic;
    signal byte3:   std_logic;

    signal MemAB:   std_logic_vector(31 downto 0); 


begin
    
    MemAB <= program_address  when   PD = '0' else
            data_address   when   PD = '1' else
            (others => 'X');


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

    RE0 <= (byte0 and not(RW)) and EN;
    RE1 <= (byte1 and not(RW)) and EN;
    RE2 <= (byte2 and not(RW)) and EN;
    RE3 <= (byte3 and not(RW)) and EN;

    
    WE0 <= byte0 and RW and EN;
    WE1 <= byte1 and RW and EN;
    WE2 <= byte2 and RW and EN;
    WE3 <= byte3 and RW and EN;


end structural;





library ieee;
use ieee.std_logic_1164.all;

entity  compareDecideUnit  is
    generic (
        int_range   : integer := 16
    );
    port (
        clk     :   in  std_logic;
        reset   :   in  std_logic;
        cmp_src :   in  integer  range int_range - 1 downto 0;
        cmp_dst :   in  integer  range int_range - 1 downto 0;
        en  :   std_logic;

        decision_delay_0: out std_logic;
        decision_delay_1: out std_logic;
        decision_delay_2: out std_logic
    );
end compareDecideUnit;


architecture behavioral of compareDecideUnit is
    signal decision_0   :   std_logic;

    signal decision_delay_1_reg :   std_logic;
    signal decision_delay_2_reg :   std_logic;
begin
    decision_0 <= '1' when (cmp_src = cmp_dst) and (en = '1') else
                '0';

    process(clk) begin
        if  rising_edge(clk)  then 
            decision_delay_1_reg <= decision_0;
            decision_delay_2_reg <= decision_delay_1_reg;
            
            if (reset = '0') then 
                decision_delay_1_reg <= '0';
                decision_delay_2_reg <= '0';
            end if;
        end if;
    end process;
    
    decision_delay_0 <= decision_0;
    decision_delay_1 <= decision_delay_1_reg;
    decision_delay_2 <= decision_delay_2_reg;

end behavioral;

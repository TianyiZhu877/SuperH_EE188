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
        data_out_0 : out std_logic_vector(31 downto 0)      -- the data output always connected to register 0
    );
end RegFile;

architecture behavioral of RegFile is

    subtype word_t       is std_logic_vector(31 downto 0);
    type    reg_array_t  is array (0 to 15) of word_t;

    signal regs : reg_array_t := (others => (others => '0'));    
    
    signal R0_debug: std_logic_vector(31 downto 0);
    signal R1_debug: std_logic_vector(31 downto 0);
    signal R2_debug: std_logic_vector(31 downto 0);
    signal R3_debug: std_logic_vector(31 downto 0);
    signal R4_debug: std_logic_vector(31 downto 0);
    signal R5_debug: std_logic_vector(31 downto 0);
    signal R6_debug: std_logic_vector(31 downto 0);
    signal R7_debug: std_logic_vector(31 downto 0);
    signal R8_debug: std_logic_vector(31 downto 0);
    signal R9_debug: std_logic_vector(31 downto 0);
    signal R10_debug: std_logic_vector(31 downto 0);
    signal R11_debug: std_logic_vector(31 downto 0);
    signal R12_debug: std_logic_vector(31 downto 0);
    signal R13_debug: std_logic_vector(31 downto 0);
    signal R14_debug: std_logic_vector(31 downto 0);
    signal R15_debug: std_logic_vector(31 downto 0);

begin
    R0_debug <= regs(0);
    R1_debug <= regs(1);
    R2_debug <= regs(2);
    R3_debug <= regs(3);
    R4_debug <= regs(4);
    R5_debug <= regs(5);
    R6_debug <= regs(6);
    R7_debug <= regs(7);
    R8_debug <= regs(8);
    R9_debug <= regs(9);
    R10_debug <= regs(10);
    R11_debug <= regs(11);
    R12_debug <= regs(12);
    R13_debug <= regs(13);
    R14_debug <= regs(14);
    R15_debug <= regs(15);

    process (clk) begin
        if rising_edge(clk) then
            if write_en = '1' then
                regs(write_sel) <= data_in;
            end if;
        end if;
    end process;

    -- asynchronous with single-cycle bypass “data_in” wins whenever the same register is being written.
    data_out_a <= data_in when (write_en = '1' and write_sel = read_sel_a) else
                  regs(read_sel_a);

    data_out_b <= data_in when (write_en = '1' and write_sel = read_sel_b) else
                  regs(read_sel_b);

    -- dedicated wire to R0 (same bypass logic)
    data_out_0 <= data_in when (write_en = '1' and write_sel = 0)         else
                  regs(0);

                  
end behavioral;

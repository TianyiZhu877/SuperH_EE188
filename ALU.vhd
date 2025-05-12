library ieee;
use ieee.std_logic_1164.all;
use work.ALUConstants.all;

-- The ALU designed specifically for SH2. Most signal connects directly into the wrapped general alu, but I also add the 
-- capability to flip the bits and do subtraction. Later I want to extend this for multipilcation/division
entity ALU is
    port (
        ALUOpA   : in      std_logic_vector(31 downto 0);   -- first operand
        ALUOpB   : in      std_logic_vector(31 downto 0);   -- second operand
        immd   : in  std_logic_vector(7 downto 0);   -- immediate value (IR 7-0)
        T        : in      std_logic;                       -- T flag 

        op_a_sel   : in    std_logic;
        op_b_sel   : in    integer  range 0 to 2;
        adder_cin_sel   : in    integer  range 0 to 2;
        
        AddSub   : in      std_logic;                       -- 1 for add, 0 for sub, always the 2nd bit of IR for SH2!  
        ALUCmd   : in      std_logic_vector(1 downto 0);    -- ALU result select
        FCmd     : in      std_logic_vector(3 downto 0);    -- F-Block operation
        SCmd     : in      std_logic_vector(2 downto 0);    -- shift operation
        
        Result   : buffer  std_logic_vector(31 downto 0);   -- ALU result
        C     : out     std_logic;                       -- carry out
        V : out     std_logic;                        -- overflow
        S : out     std_logic;                        -- sign
        Z : out     std_logic                        -- zero
    );
end ALU;


architecture structural of ALU is

    component  ALUGeneral  is

        generic (
            wordsize : integer := 8      -- default width is 8-bits
        );

        port(
            ALUOpA   : in      std_logic_vector(wordsize - 1 downto 0);   -- first operand
            ALUOpB   : in      std_logic_vector(wordsize - 1 downto 0);   -- second operand
            Cin      : in      std_logic;                                 -- carry in
            FCmd     : in      std_logic_vector(3 downto 0);              -- F-Block operation
            CinCmd   : in      std_logic_vector(1 downto 0);              -- carry in operation
            SCmd     : in      std_logic_vector(2 downto 0);              -- shift operation
            ALUCmd   : in      std_logic_vector(1 downto 0);              -- ALU result select
            Result   : buffer  std_logic_vector(wordsize - 1 downto 0);   -- ALU result
            Cout     : out     std_logic;                                 -- carry out
            HalfCout : out     std_logic;                                 -- half carry out
            Overflow : out     std_logic;                                 -- signed overflow
            Zero     : out     std_logic;                                 -- result is zero
            Sign     : out     std_logic                                  -- sign of result
        );

    end  component;


    signal is_adder_and_subtracting: std_logic;
    signal Cin_shifter: std_logic;
    signal Cin_add_sub :  std_logic;
    signal cin_adder :  std_logic;
    signal Cin_internal: std_logic;
    signal OpB_internal: std_logic_vector(31 downto 0);
    -- signal immd_sext: std_logic_vector(31 downto 0);
    -- signal immd_ext: std_logic_vector(31 downto 0);
    signal ALUOpA_internal : std_logic_vector(31 downto 0); 
    signal ALUOpB_internal : std_logic_vector(31 downto 0); 

    -- signal Overflow_internal: std_logic;
    -- signal Sign_internal: std_logic;

begin
    

    ALUOpA_internal <= ALUOpA when op_a_sel = '0' else
                     (others => '0') when op_a_sel = '1' else
                    (others => 'X');

    ALUOpB_internal <= ALUOpB when op_b_sel = 0 else
                    (31 downto 8 => immd(7)) & immd when op_b_sel = 1 else
                    (31 downto 8 => '0') & immd when op_b_sel = 2 else
                    (others => 'X');

    Cin_internal <= Cin_add_sub when ALUCmd = ALUCmd_ADDER else
                    T       when ALUCmd = ALUCmd_SHIFT else
                    'X';

-- block for flipping the opB and carry in in case of subtraction
    cin_adder <= '0' when adder_cin_sel = 0 else
                    T when adder_cin_sel = 1 else
                    '1' when adder_cin_sel = 2 else
                'X';
    is_adder_and_subtracting <= ALUCmd(0) and (not AddSub);
    process (OpB_internal, ALUOpB, is_adder_and_subtracting, AddSub, ALUCmd, Cin_internal) begin        
        for i in ALUOpB'range loop
            OpB_internal(i) <= ALUOpB(i) xor is_adder_and_subtracting;
        end loop;
    end process;
    Cin_add_sub <= cin_adder xor is_adder_and_subtracting;


    u_alu: ALUGeneral
        generic map (
            wordsize => 32
        )
        port map (
            ALUOpA   => ALUOpA,
            ALUOpB   => OpB_internal,
            Cin      => Cin_internal,
            FCmd     => FCmd,
            CinCmd   => CinCmd_CIN,
            SCmd     => SCmd,
            ALUCmd   => ALUCmd,
            Result   => Result,
            Cout     => C,               -- to-do: check if cout and overflow needs to be flip for subtraction for sh2
            HalfCout => open,
            Overflow => V,
            Zero     => Z,
            Sign     => S
        );

end  structural;

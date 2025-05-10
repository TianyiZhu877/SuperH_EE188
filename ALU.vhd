library ieee;
use ieee.std_logic_1164.all;
use work.ALUConstants.all;

-- The ALU designed specifically for SH2. Most signal connects directly into the wrapped general alu, but I also add the 
-- capability to flip the bits and do subtraction. Later I want to extend this for multipilcation/division
entity ALU is
    port (
        ALUOpA   : in      std_logic_vector(31 downto 0);   -- first operand
        ALUOpB   : in      std_logic_vector(31 downto 0);   -- second operand
        Cin      : in      std_logic;                       -- carry in

        AddSub   : in      std_logic;                       -- 1 for add, 0 for sub, always the 2nd bit of IR for SH2!  
        ALUCmd   : in      std_logic_vector(1 downto 0);    -- ALU result select
        FCmd     : in      std_logic_vector(3 downto 0);    -- F-Block operation
        SCmd     : in      std_logic_vector(2 downto 0);    -- shift operation
        Result   : buffer  std_logic_vector(31 downto 0);   -- ALU result
        Cout     : out     std_logic;                       -- carry out
        Overflow : out     std_logic                        -- signed overflow
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
    signal Cin_internal: std_logic;
    signal OpB_internal: std_logic_vector(31 downto 0);

begin
    
-- block for flipping the opB and carry in in case of subtraction
    process (OpB_internal, ALUOpB, is_adder_and_subtracting, AddSub, ALUCmd, Cin_internal) begin
        is_adder_and_subtracting <= ALUCmd(0) and (not AddSub);
        
        for i in ALUOpB'range loop
            OpB_internal(i) <= ALUOpB(i) xor is_adder_and_subtracting;
        end loop;

        Cin_internal <= Cin xor is_adder_and_subtracting;
    end process;

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
            Cout     => Cout,               -- to-do: check if cout and overflow needs to be flip for subtraction for sh2
            HalfCout => open,
            Overflow => Overflow,
            Zero     => open,
            Sign     => open
        );
end  structural;

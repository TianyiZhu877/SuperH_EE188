library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ALUConstants.all;

-- The ALU designed specifically for SH2. Most signal connects directly into the wrapped general alu, but I also add the 
-- capability to flip the bits and do subtraction. Later I want to extend this for multipilcation/division
-- Signals/wiring @alu_dataflow.jpg

entity ALU is
    port (
        ALUOpA   : in      std_logic_vector(31 downto 0);   -- first operand
        ALUOpB   : in      std_logic_vector(31 downto 0);   -- second operand
        immd   : in  std_logic_vector(7 downto 0);   -- immediate value (IR 7-0)
        T        : in      std_logic;                       -- T flag 

        op_a_sel   : in    integer  range 0 to 2;
        op_b_sel   : in    integer  range 0 to 3;
        adder_cin_sel   : in    integer  range 0 to 2;
        adder_T_out_sel : in    integer range 0 to 8;

        ALU_special_cmd : in std_logic;
        MULCmd: in      integer  range 0 to 3;
        
        AddSub   : in      std_logic;                       -- 1 for add, 0 for sub, always the 2nd bit of IR for SH2!  
        ALUCmd   : in      std_logic_vector(1 downto 0);    -- ALU result select
        FCmd     : in      std_logic_vector(3 downto 0);    -- F-Block operation
        SCmd     : in      std_logic_vector(2 downto 0);    -- shift operation
        
        Result   : buffer  std_logic_vector(31 downto 0);   -- ALU result
        MACH_out : out std_logic_vector(31 downto 0);  
        T_out: out std_logic
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
    -- signal immd_sext: std_logic_vector(31 downto 0);
    -- signal immd_ext: std_logic_vector(31 downto 0);
    signal ALUOpA_internal : std_logic_vector(31 downto 0); 
    signal ALUOpB_internal : std_logic_vector(31 downto 0); 
    signal ALUOpB_flipped_for_adder: std_logic_vector(31 downto 0);

-- T out internal signals
    signal C, V, S, Z: std_logic;
    signal adder_T_out: std_logic;
-- Result temp signals
    signal mul64_signed_Result, mul64_unsigned_Result: std_logic_vector(63 downto 0); 
    signal Result_special, Result_normal: std_logic_vector(31 downto 0);   
    signal EXT_result, MUL_result, special_shift_result: std_logic_vector(31 downto 0);   

    -- signal Overflow_internal: std_logic;
    -- signal Sign_internal: std_logic;

begin
    

    ALUOpA_internal <= ALUOpA when op_a_sel = 0 else
                     (others => '0') when op_a_sel = 1 else
                     ALUOpB when op_a_sel = 2 else
                    (others => 'X');

    ALUOpB_internal <= ALUOpB when op_b_sel = 0 else
                    (31 downto 8 => immd(7)) & immd when op_b_sel = 1 else
                    (31 downto 8 => '0') & immd when op_b_sel = 2 else
                    (others => '1') when op_b_sel = 3 else
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
    process (all) begin        
        for i in ALUOpB'range loop
            ALUOpB_flipped_for_adder(i) <= ALUOpB_internal(i) xor is_adder_and_subtracting;
        end loop;
    end process;
    Cin_add_sub <= cin_adder xor is_adder_and_subtracting;


    -- routing special command outputs
    process (all) begin
        case ALUCmd is
            when SpecCmd_SHIFT =>
                Result_special <= special_shift_result;
            when SpecCmd_XTUCT =>
                Result_special <= ALUOpA(15 downto 0) & ALUOpB(31 downto 16);
            when SpecCmd_EXT =>
                Result_special <= EXT_result;
            when SpecCmd_MUL =>
                Result_special <= MUL_result;
            when others =>
                Result_special <= (others => 'X');
        end case;
    end process;
    Result <= Result_normal when ALU_special_cmd = '0' else
                 Result_special when ALU_special_cmd = '1' else
                 (others => 'X');

-- multiplication, to-do: add this back after implemeting two-stage multiplication!!!
    -- mul64_signed_Result <= std_logic_vector(signed(ALUOpA) * signed(ALUOpB));
    -- mul64_unsigned_Result <= std_logic_vector(unsigned(ALUOpA) * unsigned(ALUOpB));
    mul64_signed_Result <= (others => 'X');
    mul64_unsigned_Result <= (others => 'X');
    process (all) begin
        MACH_out <= (others => 'X');
        case MULCmd is
            when 0 => 
                MUL_result <= mul64_signed_Result(31 downto 0);
                MACH_out <= mul64_signed_Result(63 downto 32);
            when 1 => 
                MUL_result <= mul64_unsigned_Result(31 downto 0);
                MACH_out <= mul64_unsigned_Result(63 downto 32);
            when 2 => 
                MUL_result <= std_logic_vector(signed(ALUOpA(15 downto 0)) * signed(ALUOpB(15 downto 0)));
                -- MUL_result <= (others => '1');
            when 3 => 
                MUL_result <= std_logic_vector(unsigned(ALUOpA(15 downto 0)) * unsigned(ALUOpB(15 downto 0)));
                -- MUL_result <= (others => '1');
            when others =>
                MUL_result <= (others => 'X');
        end case;


-- extension
        case immd(1 downto 0) is
            when "10" =>
                EXT_result <= (31 downto 8 => ALUOpA(7)) & ALUOpA(7 downto 0);
            when "11" =>
                EXT_result <= (31 downto 16 => ALUOpA(15)) & ALUOpA(15 downto 0);
            when "00" =>
                EXT_result <= (31 downto 8 => '0') & ALUOpA(7 downto 0);
            when "01" =>
                EXT_result <= (31 downto 16 => '0') & ALUOpA(15 downto 0);
            when others =>
                EXT_result <= (others => 'X');
        end case;
    
-- special shifts
        case immd(5 downto 4) & immd(0) is
            when "000" =>
                special_shift_result <= ALUOpA(29 downto 0) & "00";
            when "010" =>
                special_shift_result <= ALUOpA(23 downto 0) & (7 downto 0 => '0');
            when "100" =>
                special_shift_result <= ALUOpA(15 downto 0) & (15 downto 0 => '0');
            when "001" =>
                special_shift_result <= "00" & ALUOpA(31 downto 2);
            when "011" =>
                special_shift_result <= (31 downto 24 => '0') & ALUOpA(31 downto 8);
            when "101" =>
                special_shift_result <= (31 downto 16 => '0') & ALUOpA(31 downto 16);
            when others =>
                special_shift_result <= (others => '0');
        end case;
            
    end process;

    
    

    u_alu: ALUGeneral
        generic map (
            wordsize => 32
        )
        port map (
            ALUOpA   => ALUOpA_internal,
            ALUOpB   => ALUOpB_flipped_for_adder,
            Cin      => Cin_internal,
            FCmd     => FCmd,
            CinCmd   => CinCmd_CIN,
            SCmd     => SCmd,
            ALUCmd   => ALUCmd,
            Result   => Result_normal,
            Cout     => C,               -- to-do: check if cout and overflow needs to be flip for subtraction for sh2
            HalfCout => open,
            Overflow => V,
            Zero     => Z,
            Sign     => S
        );

    -- T_out logic
    process (all) begin
        case adder_T_out_sel is
            when 0 => 
                adder_T_out <= C;
            when 1 => 
                adder_T_out <= Z;
            when 2 => 
                adder_T_out <= not C;
            when 3 => 
                adder_T_out <= C and (not Z);
            when 4 => 
                adder_T_out <= (not(S xor V)) and (not Z);
            
            when 5 => 
                adder_T_out <= (not (S xor V)) or Z;
            when 6 => 
                adder_T_out <= V;
            when 7 => 
                adder_T_out <= (not S) and (not Z);
            when 8 => 
                adder_T_out <= (not S);
            when others => 
                adder_T_out <= 'X';
        end case;

        case ALUCmd is
            when ALUCmd_SHIFT =>
                T_out <= C;
            when ALUCmd_FBLOCK =>
                T_out <= Z;
            when ALUCmd_ADDER =>
                T_out <= adder_T_out;
            when others =>
                T_out <= 'X';
        end case;
    end process;

    

end  structural;

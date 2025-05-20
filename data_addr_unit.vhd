library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.array_type_pkg.all;

-- The AddrUnit includes both the program addressing and data addressing. I make them into one as they
-- share a general structure and many component. There's no need to add two instances as two addresses 
-- cold not be accessed at the same time.

-- In this design, I try to preseve as much signal as input/output as possible to allow a fine-grinded 
-- control by the control unit. Also I did not include register such as GBR and PC in this unit, rather 
-- just use them as input signals, because their loading could be controlled by some signals form the control
-- unit, and I want to leave the design when making the control unit.

-- A more detailed diagram could be found in memory_unit_structure.png

entity  dataAddrUnit  is
    port (
    -- inputs for base addr
        SrcSel      : in    integer  range 4 downto 0;       -- singal for selection 
        PC          : in     std_logic_vector(31 downto 0);  -- selected when 0
        Rn          : in     std_logic_vector(31 downto 0);   -- selected when 1
        GBR         : in     std_logic_vector(31 downto 0);   -- selected when 2

    -- inputs for offset
        OffsetSel   : in    integer  range 4 downto 0;       -- singal for selection 
        R0          : in     std_logic_vector(31 downto 0);  -- selected when 0
        IncDecVal   :  in     std_logic_vector(3 downto 0);
        -- selected when +-2 (no shift), +-3 (shift 1), +-4 (shift 2), and 0 for some operations
        Disp        : in     std_logic_vector(7 downto 0);  
        -- choose to use last 0 (4 bits), 1 (8 bits) of disp
        DispCutoff  : in    integer  range 1 downto 0;       

    -- signals from control unit, directly connect to the wrapped general mau
        PrePostSel : in      std_logic;
    
    -- outputs, directly connects to the wrapped general mau output
        Address    : out     std_logic_vector(31 downto 0);
        AddrSrcOut : buffer  std_logic_vector(31 downto 0)
    );
end dataAddrUnit;


architecture  behavioral  of  dataAddrUnit  is

-- the general memory component
    component  MemUnit
        generic (
            srcCnt       : integer;
            offsetCnt    : integer;
            wordsize     : integer := 16 -- default address width is 16 bits
        );

        port(
            AddrSrc    : in      std_logic_array(srccnt - 1 downto 0)(wordsize - 1 downto 0);
            SrcSel     : in      integer  range srccnt - 1 downto 0;
            AddrOff    : in      std_logic_array(offsetcnt - 1 downto 0)(wordsize - 1 downto 0);
            OffsetSel  : in      integer  range offsetcnt - 1 downto 0;
            PrePostSel : in      std_logic;

            Address    : out     std_logic_vector(wordsize - 1 downto 0);
            AddrSrcOut : buffer  std_logic_vector(wordsize - 1 downto 0)
        );

    end component;

-- imtermeidate after cutting off bits of disp:
    signal disp_internal: std_logic_vector(11 downto 0);   
    signal PC_plus_4: std_logic_vector(31 downto 0);        

-- inputs to general mau:
    signal AddrSrc: std_logic_array(4 downto 0)(31 downto 0);   
    signal AddrOff: std_logic_array(4 downto 0)(31 downto 0);

begin
    -- cutting off higher bits by DispCutoff
    disp_internal <=    (11 downto 8 => Disp(7)) & Disp  when DispCutoff = 1  else
                        (11 downto 4 => Disp(3)) & Disp(3 downto 0)  when DispCutoff = 0  else
                        (others => 'X');

    PC_plus_4 <= std_logic_vector(unsigned(PC) + 4);
    process (all) begin
    -- for base addr selections, selected when SrcSel:
        AddrSrc(0) <= Rn;                       -- 0    Rn
        AddrSrc(1) <= PC_plus_4;                -- 1
        AddrSrc(2) <= PC_plus_4(31 downto 2) & "00";                -- 2
        AddrSrc(3) <= GBR;                      -- 3
        AddrSrc(4) <= (others => '0');                      -- 4

    -- inputs for offset selection, selected when OffsetSel:
        AddrOff(0) <= R0;                               -- 0    R0
        AddrOff(1) <= (31 downto 4 => IncDecVal(3)) & IncDecVal;                  -- 1 inc dec val                              
        AddrOff(2) <= (31 downto 12 => disp_internal(11)) & disp_internal;  -- 2 (disp)
        AddrOff(3) <= (31 downto 13 => disp_internal(11)) & disp_internal & '0';    -- 3 (disp << 1)
        AddrOff(4) <= (31 downto 14 => disp_internal(11)) & disp_internal & "00";   -- 4 (disp << 2)
    end process;

    mau_general: MemUnit
        generic map (
            srcCnt   => 5,
            offsetCnt => 5,
            wordsize => 32               -- 32-bit address bus
        )
        port map (
            AddrSrc    => AddrSrc,
            SrcSel     => SrcSel,
            AddrOff    => AddrOff,
            OffsetSel  => OffsetSel,
            PrePostSel => PrePostSel,  

            Address    => Address, 
            AddrSrcOut => AddrSrcOut
        );

end behavioral;


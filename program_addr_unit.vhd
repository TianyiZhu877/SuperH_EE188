library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.array_type_pkg.all;


entity  programAddrUnit  is
    port (
    -- inputs for base addr
        SrcSel      : in    integer  range 1 downto 0;       -- singal for selection 
        PC          : in     std_logic_vector(31 downto 0);  -- selected when 0

    -- inputs for offset
        OffsetSel   : in    integer  range 2 downto 0;       -- singal for selection 
        Rm          : in     std_logic_vector(31 downto 0);  -- selected when 0
        -- selected when +-2 (no shift), +-3 (shift 1), +-4 (shift 2), and 0 for some operations
        Disp        : in     std_logic_vector(11 downto 0);  
        -- choose to use last 0 (4 bits), 1 (8 bits) of disp
        DispCutoff  : in    integer  range 1 downto 0;       

    -- signals from control unit, directly connect to the wrapped general mau
        PrePostSel : in      std_logic;
    
    -- outputs, directly connects to the wrapped general mau output
        Address    : out     std_logic_vector(31 downto 0);
        AddrSrcOut : buffer  std_logic_vector(31 downto 0)
    );
end programAddrUnit;


architecture  behavioral  of  programAddrUnit  is

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

-- inputs to general mau:
    signal AddrSrc: std_logic_array(1 downto 0)(31 downto 0);   
    signal AddrOff: std_logic_array(2 downto 0)(31 downto 0);

begin
    -- cutting off higher bits by DispCutoff
    disp_internal <=    Disp  when DispCutoff = 1  else
                        (11 downto 8 => Disp(7)) & Disp(7 downto 0)  when DispCutoff = 0  else
                        (others => 'X');


    process (all) begin
    -- for base addr selections, selected when SrcSel:
        AddrSrc(0) <= PC;                       -- 0
        AddrSrc(1) <= (others => '0');

    -- inputs for offset selection, selected when OffsetSel:
        AddrOff(0) <= x"00000002";                  -- 1 inc dec val 
        AddrOff(1) <= Rm;                               -- 0                             
        AddrOff(2) <= (31 downto 13 => disp_internal(11)) & disp_internal & '0';    -- 3 (disp << 1)
    end process;

    mau_general: MemUnit
        generic map (
            srcCnt   => 2,
            offsetCnt => 3,
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


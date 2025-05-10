----------------------------------------------------------------------------
--
--  Generic Memory Access Unit
--
--  This is an implementation of a generic memory access unit for
--  microprocessors.  This unit generates the memory address for either load
--  and store operations or instruction access.  It is parameterized by the
--  number of sources and the data width.
--
--  Packages included are:
--     MemUnitConstants - constants for the memory access unit
--     array_type_pkg   - type definition for 2-D arrays of std_logic
--
--  Entities included are:
--     AdderBit - one bit of an adder (a full adder)
--     MemUnit  - generic memory access unit
--
--  Revision History:
--     27 Jan 21  Glen George       Initial revision.
--      4 Feb 21  Glen George       Added missing library declaration.
--      4 Feb 21  Glen George       Added initialization of low bit of carry
--                                  for the adder.
--
----------------------------------------------------------------------------


--
--  Package containing the type definition for 2-D arrays of std_logic.  The
--  type is an unconstrained 2-D array which is supported in VHDL-2008.
--

library ieee;
use ieee.std_logic_1164.all;

package array_type_pkg is

--  a 2D array of std_logic (VHDL-2008)
   type  std_logic_array  is  array (natural range<>) of std_logic_vector;


end package;



--
--  Package containing the constants for the Memory Unit
--

library ieee;
use ieee.std_logic_1164.all;

package MemUnitConstants is

--  memory access unit constants for pre- and post- increment and decrement
--     these constants may be freely changed

   constant MemUnit_PRE  : std_logic := '0';            -- pre- inc/dec
   constant MemUnit_POST : std_logic := '1';            -- post- inc/dec
   constant MemUnit_INC  : std_logic := '0';            -- pre/post increment
   constant MemUnit_DEC  : std_logic := '1';            -- pre/post decrement


end package;



--
--  AdderBit
--
--  This is a bit of the adder for doing addition in the ALU.
--
--  Inputs:
--    A  - first operand bit (bus A)
--    B  - second operand bit (bus B)
--    Ci - carry in (from previous bit)
--
--  Outputs:
--    S  - sum for this bit
--    Co - carry out for this bit
--

library ieee;
use ieee.std_logic_1164.all;

entity  AdderBit  is

    port(
        A  : in   std_logic;        -- first operand
        B  : in   std_logic;        -- second operand
        Ci : in   std_logic;        -- carry in from previous bit
        S  : out  std_logic;        -- sum (result)
        Co : out  std_logic         -- carry out to next bit
    );

end  AdderBit;


architecture  dataflow  of  AdderBit  is
begin

    S  <=  A  xor  B  xor  Ci;
    Co <=  (A  and  B)  or  (A  and Ci)  or  (B  and  Ci);

end  dataflow;



--
--  MemUnit
--
--  This is a generic memory access unit.  It allows for pre- or post-
--  increment/decrement of an address as well as multiple sources for the
--  address and offset.
--
--  Generics:
--    srcCnt       - number of possible sources
--    offsetCnt    - number of possible address offsets
--    wordsize     - address width
--
--  Inputs:
--    AddrSrc    - array (srccnt x wordsize) of address sources
--    SrcSel     - source to use (log srccnt bits)
--    AddrOff    - array (offsetcnt x wordsize) of address offsets
--    OffsetSel  - offset to use (log offsetcnt bits)
--    PrePostSel - whether to pre- (0) or post- (1) inc/dec address source
--
--  Outputs:
--    Address    - address bus (wordsize bits)
--    AddrSrcOut - incremented/decremented source (wordsize bits)
--

library ieee;
use ieee.std_logic_1164.all;
use work.array_type_pkg.all;
use work.MemUnitConstants.all;
use ieee.numeric_std.all;

entity  MemUnit  is

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

end  MemUnit;


architecture  dataflow  of  MemUnit  is


    signal  SrcVal : std_logic_vector(wordsize - 1 downto 0);
    signal  OffsetVal : std_logic_vector(wordsize - 1 downto 0);
    signal  SumVal : std_logic_vector(wordsize - 1 downto 0);


begin
    SrcVal <= AddrSrc(SrcSel);
    OffsetVal <= AddrOff(OffsetSel);
    SumVal <= std_logic_vector(unsigned(SrcVal) + unsigned(OffsetVal));

    Address <=  SrcVal when PrePostSel = '0' else
                SumVal;

    AddrSrcOut  <=  SumVal;

end  dataflow;

library ieee;
use ieee.std_logic_1164.all;
package ROMPackage is

	component ROM is
		port (rom_addr: in std_logic_vector(15 downto 0);
--			rom_rd: in std_logic;
			rom_dataout: out std_logic_vector(15 downto 0)
		);
	end component ROM;
	
end package ROMPackage;

library std;
use std.standard.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_arith.all;	 
use ieee.std_logic_unsigned.all;

-- since The Memory is asynchronous read, there is no read signal, but you can use it based on your preference.
-- this memory gives 16 Bit data in one clock cycle, so edit the file to your requirement.

entity ROM is 
	port (rom_addr: in std_logic_vector(15 downto 0);
--		rom_rd: in std_logic;
		rom_dataout: out std_logic_vector(15 downto 0)
		);
end entity;

architecture Struct of ROM is 
type regarray is array(65535 downto 0) of std_logic_vector(15 downto 0);   -- defining a new type
signal Memory: regarray:=(
	0 => "1000000000001111",	--0
	1 => "1100000001001010",	--1
	2 => "0011100111100011",	--2 
	3 => "0000001010011010",	--3
	4 => "0011100000000000", 	--4
	5 => "0100101100000001",	--5
	6 => "0000101010100000",	--6
	7 => "0010011100010000",	--7
	8 => "0011011111011010",	--8
	9 => "1100010011000010",	--9
	10 => "0011011000000000",	--A
	11 => "0000011100010000",	--B
	12 => "0010010010001000",	--C
	13 => "0010010001001001",	--D
	14 => "1000011000000010",	--E
	15 => "1001001010000000",	--F
	16 => "0000010001001000",	--10
	17 => "0000011001011010",	--11
	18 => "0011000000000000",	--12
	19 => "0001000000010110",	--13
	20 => "1001110000000000",	--14
	21 => "0010110110110010",	--15
	22 => "0010110001110010",	--16
	23 => "0010110001110000",	--17
	24 => "0011000000000000",	--18
	25 => "0011110001101000",	--19
	26 => "0001110110000001",	--1A
	27 => "0101110000011100",	--1B
	28 => "0011010000000001",	--1C
	29 => "0000010000001000",	--1D
	30 => "0110000001000100",	--1E
	31 => "0000010110010000",	--1F
	32 => "0000010110110000",	--20
	33 => "0011010000001000",	--21
	34 => "0001010010001000",	--22
	35 => "0011100000001110",	--23
	36 => "0001100100010000",	--24
	37 => "0001000000011111",	--25
	38 => "0001000000001001",	--26
	39 => "0111000000010100",	--27
	40 => "0011111000000000", --Loop 28
	others => "0000000000000000");
-- you can use the above mentioned way to initialise the memory with the instructions and the data as required to test your processor
begin
	rom_dataout <= Memory(conv_integer(rom_addr));
end Struct;


-- 0 => "0011001111111111",	--0
-- 	1 => "0001001001111111",	--1
-- 	2 => "0011010000000001",	--2
-- 	3 => "0000001010011010",	--3
-- 	4 => "0011100000000000", --4
-- 	5 => "0100101100000001",	--5
-- 	6 => "0000101010100000",	--6
-- 	7 => "0010011100010000",	--7
-- 	8 => "0011011111011010",	--8
-- 	9 => "1100010011000010",	--9
-- 	10 => "0011011000000000",	--A
-- 	11 => "0000011100010000",	--B
-- 	12 => "0010010010001000",	--C
-- 	13 => "0010010001001001",	--D
-- 	14 => "1000011000000010",	--E
-- 	15 => "0011011000000000",	--F
-- 	16 => "0000010001001000",	--10
-- 	17 => "0000011001011010",	--11
-- 	18 => "0011000000000000",	--12
-- 	19 => "0001000000010110",	--13
-- 	20 => "1001110000000000",	--14
-- 	21 => "0010110110110010",	--15
-- 	22 => "0010110001110010",	--16
-- 	23 => "0010110001110000",	--17
-- 	24 => "0011000000000000",	--18
-- 	25 => "0011110001101000",	--19
-- 	26 => "0001110110000001",	--1A
-- 	27 => "0101110000011100",	--1B
-- 	28 => "0011010000000001",	--1C
-- 	29 => "0000010000001000",	--1D
-- 	30 => "0110000001000100",	--1E
-- 	31 => "0000010110010000",	--1F
-- 	32 => "0000010110110000",	--20
-- 	33 => "0011010000001000",	--21
-- 	34 => "0001010010001000",	--22
-- 	35 => "0011100000001110",	--23
-- 	36 => "0001100100010000",	--24
-- 	37 => "0001000000011111",	--25
-- 	38 => "0001000000001001",	--26
-- 	39 => "0111000000010100",	--27
-- 	40 => "0011111000000000", --Loop 28
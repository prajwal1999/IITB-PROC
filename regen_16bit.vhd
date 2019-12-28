library ieee;
use ieee.std_logic_1164.all;

package regenPackage is 
	component regen_16bit
		port(
			data_in  : in  std_logic_vector(15 downto 0);
			en       : in  std_logic;
			data_out : out std_logic_vector(15 downto 0)
		);
	end component regen_16bit;
end package regenPackage;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity regen_16bit is
	port(
		data_in : in std_logic_vector(15 downto 0);
		en : in std_logic;
		data_out: out std_logic_vector(15 downto 0)
	);
end entity regen_16bit;

architecture RTL of regen_16bit is
	
begin
	process(data_in, en)
	begin
		if(en='1') then
			data_out <= data_in;
		end if;
	end process;
end architecture RTL;

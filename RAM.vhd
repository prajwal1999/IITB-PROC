library ieee;
use ieee.std_logic_1164.all;
package RAMPackage is	
	component RAM is
		port (
			wr_en, clk: in std_logic;
			rd_addr, wr_addr, wr_data: in std_logic_vector(15 downto 0);
			rd_data: out std_logic_vector(15 downto 0)
			  );
	end component RAM;
end package RAMPackage;

library std;
use std.standard.all;
library work;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_arith.all;	 
use ieee.std_logic_unsigned.all;

-- since The Memory is asynchronous read, there is no read signal, but you can use it based on your preference.
-- this memory gives 16 Bit data in one clock cycle, so edit the file to your requirement.

entity RAM is 
	port (
			wr_en, clk: in std_logic;
			rd_addr, wr_addr, wr_data: in std_logic_vector(15 downto 0);
			rd_data: out std_logic_vector(15 downto 0)
		  );
end entity;

architecture Struct of RAM is 
type regarray is array(65535 downto 0) of std_logic_vector(15 downto 0);   -- defining a new type
signal Memory: regarray:=(
	20 => x"00C8",
	21 => x"00C9",
	22 => x"00CA",
	23 => x"00CB",
	24 => x"00CC",
	25 => x"00CD",
	26 => x"00CE",
	27 => x"00CF",
	others => "0000000000000000"
	);
-- you can use the above mentioned way to initialise the memory with the instructions and the data as required to test your processor
begin
	
	rd_data <= Memory(conv_integer(rd_addr));
	
	process (clk)
			begin
				if(falling_edge(clk)) then
						if(wr_en = '1') then
							Memory(conv_integer(wr_addr)) <= wr_data;
						end if;
				end if;
	end process;
		
end Struct;
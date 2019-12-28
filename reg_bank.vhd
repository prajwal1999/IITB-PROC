
library ieee;
use ieee.std_logic_1164.all;
package RegisterBankPackage is
	component RegisterBank is
		port (
			wr_en, clk: in std_logic;
			wr_addr, rd_addr1, rd_addr2: in std_logic_vector(2 downto 0);
			wr_data: in std_logic_vector(15 downto 0);
			
			data_out1, data_out2, R0, R1, R2, R3, R4, R5, R6, R7: out std_logic_vector(15 downto 0)
		);
	end component RegisterBank;
end package RegisterBankPackage;



library ieee;
use ieee.std_logic_1164.all;
library work;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_arith.all;	 
use ieee.std_logic_unsigned.all;

entity RegisterBank is
   port (
			wr_en, clk: in std_logic;
			wr_addr, rd_addr1, rd_addr2: in std_logic_vector(2 downto 0);
			wr_data: in std_logic_vector(15 downto 0);
			
			data_out1, data_out2, R0, R1, R2, R3, R4, R5, R6, R7: out std_logic_vector(15 downto 0)
	);
end entity RegisterBank;

architecture Struct of RegisterBank is
	type regarray is array(7 downto 0) of std_logic_vector(15 downto 0);   -- defining a new type
	signal Bank: regarray:=(0 => x"0050", 1 => x"0050", 2 => x"0150", 3 => x"0002", 4 => x"0250", 5 => x"0300", 6 => x"0350", 7 => x"0400");
		
	begin
		data_out1 <= Bank(conv_integer(rd_addr1));
		data_out2 <= Bank(conv_integer(rd_addr2));
		
		R0 <= Bank(0);
		R1 <= Bank(1);
		R2 <= Bank(2);
		R3 <= Bank(3);
		R4 <= Bank(4);
		R5 <= Bank(5);
		R6 <= Bank(6);
		R7 <= Bank(7);
		
		RR_bank_write:
		process (clk)
			begin

				if(falling_edge(clk)) then
					if(wr_en = '1') then
						Bank(conv_integer(wr_addr)) <= wr_data;
					end if;
				end if;
		end process;
		
end Struct;
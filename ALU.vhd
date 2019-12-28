library ieee;
use ieee.std_logic_1164.all;

package ALU_16Package is

	component ALU_16 is
		port (
			ALU_OP: in std_logic_vector(1 downto 0);
			A, B: in std_logic_vector(15 downto 0);
			cin, zin, alu_en: in std_logic;
			S: out std_logic_vector(15 downto 0);
			C, Z: out std_logic
		);
	end component ALU_16;
	
end package ALU_16Package;

library ieee;
use ieee.std_logic_1164.all;
library work;
use work.Blocks.all;

entity ALU_16 is 	
	port(
		ALU_OP: in std_logic_vector(1 downto 0);
		A, B: in std_logic_vector(15 downto 0);
		cin, zin, alu_en: in std_logic;
		S: out std_logic_vector(15 downto 0);
		C, Z: out std_logic
	);
end ALU_16;

architecture Struct of ALU_16 is
		signal A_S, N_S, X_S, temp_S: std_logic_vector(15 downto 0);
		signal A_C: std_logic;
		signal alu_op_code: std_logic_vector(1 downto 0);
		signal sig_c, sig_z: std_logic;
	begin 
		add1: Adder_16 port map(A=>A, B=>B, cin=>cin, S=>A_S, C=>A_C);
		nand1: Nand_16 port map(A=>A, B=>B, S=>N_S);
		xor1: XOR_16 port map(A=>A, B=>B, S=>X_S);
		alu_op_code <= ALU_OP;
	
	-- Mux for output
	with alu_op_code select
		temp_S <= A_S when "00",
			  N_S when "01",
			  X_S when "10",
		     "0000000000000000" when others;
  
	-- Mux for carry
  	with alu_op_code select
		sig_c <= A_C when "00",
		     cin when others;
	-- For zero 
	sig_z <= '1' when temp_S = "0000000000000000" else
		  '0';
	
	process(alu_en, sig_c, sig_z, temp_S)
	begin
		if(alu_en='1') then
			S <= temp_S;
			C <= sig_c;
			Z <= sig_z;
		end if;
	end process;
	
	
end Struct;

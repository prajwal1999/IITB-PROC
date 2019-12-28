library ieee;
use ieee.std_logic_1164.all;
package Blocks is

	component Adder_16 is
		port(A, B: in std_logic_vector(15 downto 0); cin: std_logic; S: out std_logic_vector(15 downto 0); C: out std_logic);
	end component Adder_16;
	
	component Nand_16 is
		port(A, B: in std_logic_vector(15 downto 0); S: out std_logic_vector(15 downto 0));
	end component Nand_16;
	
	component XOR_16 is
		port(A, B: in std_logic_vector(15 downto 0); S: out std_logic_vector(15 downto 0));
	end component XOR_16;
	
	component XOR_3 is
		port(A, B: in std_logic_vector(2 downto 0); S: out std_logic_vector(2 downto 0));
	end component XOR_3;

	component Plus1Adder
		port(A: in  std_logic_vector(15 downto 0); PC_inc_en: in  std_logic; S: out std_logic_vector(15 downto 0));
	end component Plus1Adder;
	
end package Blocks;

library ieee;
use ieee.std_logic_1164.all;
library work;
use work.Gates.all;

entity Adder_16 is
	port(
		A, B: in std_logic_vector(15 downto 0);
		cin: in std_logic;
		S: out std_logic_vector(15 downto 0);
		C: out std_logic
	);
end Adder_16;

architecture Struct of Adder_16 is
	
	signal tc, ts: std_logic_vector(15 downto 0);
	
	begin 

	f0: FULL_ADDER port map(A => A(0), B => B(0), cin => cin, S => ts(0), C=> tc(0) );
	f1: FULL_ADDER port map(A => A(1), B => B(1), cin => tc(0), S => ts(1), C=> tc(1) );
	f2: FULL_ADDER port map(A => A(2), B => B(2), cin => tc(1), S => ts(2), C=> tc(2) );
	f3: FULL_ADDER port map(A => A(3), B => B(3), cin => tc(2), S => ts(3), C=> tc(3) );
	f4: FULL_ADDER port map(A => A(4), B => B(4), cin => tc(3), S => ts(4), C=> tc(4) );
	f5: FULL_ADDER port map(A => A(5), B => B(5), cin => tc(4), S => ts(5), C=> tc(5) );
	f6: FULL_ADDER port map(A => A(6), B => B(6), cin => tc(5), S => ts(6), C=> tc(6) );
	f7: FULL_ADDER port map(A => A(7), B => B(7), cin => tc(6), S => ts(7), C=> tc(7) );
	f8: FULL_ADDER port map(A => A(8), B => B(8), cin => tc(7), S => ts(8), C=> tc(8) );
	f9: FULL_ADDER port map(A => A(9), B => B(9), cin => tc(8), S => ts(9), C=> tc(9) );
	f10: FULL_ADDER port map(A => A(10), B => B(10), cin => tc(9), S => ts(10), C=> tc(10) );
	f11: FULL_ADDER port map(A => A(11), B => B(11), cin => tc(10), S => ts(11), C=> tc(11) );
	f12: FULL_ADDER port map(A => A(12), B => B(12), cin => tc(11), S => ts(12), C=> tc(12) );
	f13: FULL_ADDER port map(A => A(13), B => B(13), cin => tc(12), S => ts(13), C=> tc(13) );
	f14: FULL_ADDER port map(A => A(14), B => B(14), cin => tc(13), S => ts(14), C=> tc(14) );
	f15: FULL_ADDER port map(A => A(15), B => B(15), cin => tc(14), S => ts(15), C=> tc(15) );
	C <= tc(15);
	S <= ts; 
end Struct;




library ieee;
use ieee.std_logic_1164.all;
library work;
use work.Gates.all;

entity Nand_16 is 	
	port(
		A, B: in std_logic_vector(15 downto 0);
		S: out std_logic_vector(15 downto 0)
	);
end Nand_16;

architecture Struct of Nand_16 is
		
	begin 
	n0: NAND_2 port map(A=>A(0), B=>B(0), Y=>S(0));
	n1: NAND_2 port map(A=>A(1), B=>B(1), Y=>S(1));
	n2: NAND_2 port map(A=>A(2), B=>B(2), Y=>S(2));
	n3: NAND_2 port map(A=>A(3), B=>B(3), Y=>S(3));
	n4: NAND_2 port map(A=>A(4), B=>B(4), Y=>S(4));
	n5: NAND_2 port map(A=>A(5), B=>B(5), Y=>S(5));
	n6: NAND_2 port map(A=>A(6), B=>B(6), Y=>S(6));
	n7: NAND_2 port map(A=>A(7), B=>B(7), Y=>S(7));
	n8: NAND_2 port map(A=>A(8), B=>B(8), Y=>S(8));
	n9: NAND_2 port map(A=>A(9), B=>B(9), Y=>S(9));
	n10: NAND_2 port map(A=>A(10), B=>B(10), Y=>S(10));
	n11: NAND_2 port map(A=>A(11), B=>B(11), Y=>S(11));
	n12: NAND_2 port map(A=>A(12), B=>B(12), Y=>S(12));
	n13: NAND_2 port map(A=>A(13), B=>B(13), Y=>S(13));
	n14: NAND_2 port map(A=>A(14), B=>B(14), Y=>S(14));
	n15: NAND_2 port map(A=>A(15), B=>B(15), Y=>S(15));
	
 
end Struct;




library ieee;
use ieee.std_logic_1164.all;
library work;
use work.Gates.all;

entity XOR_16 is 	
	port(
		A, B: in std_logic_vector(15 downto 0);
		S: out std_logic_vector(15 downto 0)
	);
end XOR_16;

architecture Struct of XOR_16 is
		
	begin 
	X0: XOR_2 port map(A=>A(0), B=>B(0), Y=>S(0));
	X1: XOR_2 port map(A=>A(1), B=>B(1), Y=>S(1));
	X2: XOR_2 port map(A=>A(2), B=>B(2), Y=>S(2));
	X3: XOR_2 port map(A=>A(3), B=>B(3), Y=>S(3));
	X4: XOR_2 port map(A=>A(4), B=>B(4), Y=>S(4));
	X5: XOR_2 port map(A=>A(5), B=>B(5), Y=>S(5));
	X6: XOR_2 port map(A=>A(6), B=>B(6), Y=>S(6));
	X7: XOR_2 port map(A=>A(7), B=>B(7), Y=>S(7));
	X8: XOR_2 port map(A=>A(8), B=>B(8), Y=>S(8));
	X9: XOR_2 port map(A=>A(9), B=>B(9), Y=>S(9));
	X10: XOR_2 port map(A=>A(10), B=>B(10), Y=>S(10));
	X11: XOR_2 port map(A=>A(11), B=>B(11), Y=>S(11));
	X12: XOR_2 port map(A=>A(12), B=>B(12), Y=>S(12));
	X13: XOR_2 port map(A=>A(13), B=>B(13), Y=>S(13));
	X14: XOR_2 port map(A=>A(14), B=>B(14), Y=>S(14));
	X15: XOR_2 port map(A=>A(15), B=>B(15), Y=>S(15));
 
end Struct;

library ieee;
use ieee.std_logic_1164.all;
library work;
use work.Gates.all;

entity XOR_3 is
	port(A, B: in std_logic_vector(2 downto 0); S: out std_logic_vector(2 downto 0));
end entity XOR_3;

architecture Struct_xor3 of XOR_3 is
	begin
	X_30: XOR_2 port map(A=>A(0), B=>B(0), Y=>S(0));
	X_31: XOR_2 port map(A=>A(1), B=>B(1), Y=>S(1));
	X_32: XOR_2 port map(A=>A(2), B=>B(2), Y=>S(2));
end Struct_xor3;


-- +1 adder for incrementing PC
library ieee;
use ieee.std_logic_1164.all;
library work;
use work.Gates.all;

entity Plus1Adder is
	port(
		A: in std_logic_vector(15 downto 0);
		PC_inc_en: in std_logic;
		S: out std_logic_vector(15 downto 0)
	);
end Plus1Adder;

architecture Struct of Plus1Adder is
	
	signal tc, ts: std_logic_vector(15 downto 0);

	begin 
	
	f0: FULL_ADDER port map(A => A(0), B => '1', cin => '0', S => ts(0), C=> tc(0) );
	f1: FULL_ADDER port map(A => A(1), B => '0', cin => tc(0), S => ts(1), C=> tc(1) );
	f2: FULL_ADDER port map(A => A(2), B => '0', cin => tc(1), S => ts(2), C=> tc(2) );
	f3: FULL_ADDER port map(A => A(3), B => '0', cin => tc(2), S => ts(3), C=> tc(3) );
	f4: FULL_ADDER port map(A => A(4), B => '0', cin => tc(3), S => ts(4), C=> tc(4) );
	f5: FULL_ADDER port map(A => A(5), B => '0', cin => tc(4), S => ts(5), C=> tc(5) );
	f6: FULL_ADDER port map(A => A(6), B => '0', cin => tc(5), S => ts(6), C=> tc(6) );
	f7: FULL_ADDER port map(A => A(7), B => '0', cin => tc(6), S => ts(7), C=> tc(7) );
	f8: FULL_ADDER port map(A => A(8), B => '0', cin => tc(7), S => ts(8), C=> tc(8) );
	f9: FULL_ADDER port map(A => A(9), B => '0', cin => tc(8), S => ts(9), C=> tc(9) );
	f10: FULL_ADDER port map(A => A(10), B => '0', cin => tc(9), S => ts(10), C=> tc(10) );
	f11: FULL_ADDER port map(A => A(11), B => '0', cin => tc(10), S => ts(11), C=> tc(11) );
	f12: FULL_ADDER port map(A => A(12), B => '0', cin => tc(11), S => ts(12), C=> tc(12) );
	f13: FULL_ADDER port map(A => A(13), B => '0', cin => tc(12), S => ts(13), C=> tc(13) );
	f14: FULL_ADDER port map(A => A(14), B => '0', cin => tc(13), S => ts(14), C=> tc(14) );
	f15: FULL_ADDER port map(A => A(15), B => '0', cin => tc(14), S => ts(15), C=> tc(15) );
	
	S <= ts when PC_inc_en = '1' else
  		A;
	
end Struct;
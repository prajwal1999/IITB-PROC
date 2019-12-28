library ieee;
use ieee.std_logic_1164.all;
package Gates is
  component INVERTER is
   port (A: in std_logic; Y: out std_logic);
  end component INVERTER;

  component AND_2 is
   port (A, B: in std_logic; Y: out std_logic);
  end component AND_2;

  component NAND_2 is
   port (A, B: in std_logic; Y: out std_logic);
  end component NAND_2;

  component OR_2 is
   port (A, B: in std_logic; Y: out std_logic);
  end component OR_2;

  component NOR_2 is
   port (A, B: in std_logic; Y: out std_logic);
  end component NOR_2;

  component XOR_2 is
   port (A, B: in std_logic; Y: out std_logic);
  end component XOR_2;

  component XNOR_2 is
   port (A, B: in std_logic; Y: out std_logic);
  end component XNOR_2;

  component HALF_ADDER is
   port (A, B: in std_logic; S, C: out std_logic);
  end component HALF_ADDER;

  component FULL_ADDER is
   port (A, B, cin: in std_logic; S, C: out std_logic);
  end component FULL_ADDER;
  
--  component XOR_16 is
--   port (A, B: in std_logic_vector(15 downto 0); C: out std_logic_vector(15 downto 0));
--  end component XOR_16;
end package Gates;


library ieee;
use ieee.std_logic_1164.all;
entity INVERTER is
   port (A: in std_logic; Y: out std_logic);
end entity INVERTER;

architecture Equations of INVERTER is
begin
   Y <= not A;
end Equations;
  

library ieee;
use ieee.std_logic_1164.all;
entity AND_2 is
   port (A, B: in std_logic; Y: out std_logic);
end entity AND_2;

architecture Equations of AND_2 is
begin
   Y <= A and B;
end Equations;
  
library ieee;
use ieee.std_logic_1164.all;
entity NAND_2 is
   port (A, B: in std_logic; Y: out std_logic);
end entity NAND_2;

architecture Equations of NAND_2 is
begin
   Y <= not (A and B);
end Equations;
  
library ieee;
use ieee.std_logic_1164.all;
entity OR_2 is
   port (A, B: in std_logic; Y: out std_logic);
end entity OR_2;

architecture Equations of OR_2 is
begin
   Y <= A or B;
end Equations;
  
library ieee;
use ieee.std_logic_1164.all;
entity NOR_2 is
   port (A, B: in std_logic; Y: out std_logic);
end entity NOR_2;

architecture Equations of NOR_2 is
begin
   Y <= not (A or B);
end Equations;
  

library ieee;
use ieee.std_logic_1164.all;
entity XOR_2 is
   port (A, B: in std_logic; Y: out std_logic);
end entity XOR_2;

architecture Equations of XOR_2 is
begin
   Y <= A xor B;
end Equations;
  
library ieee;
use ieee.std_logic_1164.all;
entity XNOR_2 is
   port (A, B: in std_logic; Y: out std_logic);
end entity XNOR_2;

architecture Equations of XNOR_2 is
begin
   Y <= not (A xor B);
end Equations;
  
library ieee;
use ieee.std_logic_1164.all;
entity HALF_ADDER is
   port (A, B: in std_logic; S, C: out std_logic);
end entity HALF_ADDER;

architecture Equations of HALF_ADDER is
begin
   S <= (A xor B);
   C <= (A and B);
end Equations;
  
  
library ieee;
use ieee.std_logic_1164.all;
entity FULL_ADDER is
   port (A, B, cin: in std_logic; S, C: out std_logic);
end entity FULL_ADDER;

architecture Equations of FULL_ADDER is
	signal ts, tc1, tc2 : std_logic;
  component HALF_ADDER is
   port (A, B: in std_logic; S, C: out std_logic);
  end component HALF_ADDER;
begin	
	h1: HALF_ADDER port map(A => A, B => B, S => ts, C => tc1);
	h2: HALF_ADDER port map(A => ts, B => cin, S => S, C => tc2);
	C <= tc2 or tc1;
end Equations;

--
--library ieee;
--use ieee.std_logic_1164.all;
--entity XOR_16 is
--   port (A, B: in std_logic_vector(15 downto 0); C: out std_logic_vector(15 downto 0));
--end entity XOR_16;
--
--architecture Equations of FULL_ADDER is
--   -- signal sig : std_logic_vector(15 downto 0);
--begin	
--   C(0) <= A(0) xor B(0);
--   C(1) <= A(1) xor B(1);
--   C(2) <= A(2) xor B(2);
--   C(3) <= A(3) xor B(3);
--   C(4) <= A(4) xor B(4);
--   C(5) <= A(5) xor B(5);
--   C(6) <= A(6) xor B(6);
--   C(7) <= A(7) xor B(7);
--   C(8) <= A(8) xor B(8);
--   C(9) <= A(9) xor B(9);
--   C(10) <= A(10) xor B(10);
--   C(11) <= A(11) xor B(11);
--   C(12) <= A(12) xor B(12);
--   C(13) <= A(13) xor B(13);
--   C(14) <= A(14) xor B(14);
--   C(15) <= A(15) xor B(15);
--end Equations;
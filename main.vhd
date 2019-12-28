library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.standard.all;
use ieee.std_logic_arith.all;	 
use ieee.std_logic_unsigned.all;
library work;
use work.blocks.all;
use work.ROMPackage.all;
use work.regenPackage.all;
use work.RegisterBankPackage.all;
use work.ALU_16Package.all;
use work.RAMPackage.all;

entity Main is
    port (
        clk, reset: std_logic
    );
end Main;

architecture Struct of Main is 
	signal sig_ir, sig_pc, sig_ir_in,  sig_pc_in, INC_res: std_logic_vector(15 downto 0);
	signal rb_wr_addr, rb_rd_addr1, rb_rd_addr2: std_logic_vector(2 downto 0);
	signal rb_wr_data, rb_data1, rb_data2: std_logic_vector(15 downto 0);
	signal sig_r0, sig_r1, sig_r2, sig_r3, sig_r4, sig_r5, sig_r6, sig_r7: std_logic_vector(15 downto 0);
	signal alu_in1, alu_in2, alu_res, ram_rd_addr, ram_rd_data, ram_wr_addr, ram_wr_data : std_logic_vector(15 downto 0);
    
    signal alu_op: std_logic_vector(1 downto 0);
    signal pc_wr, ir_wr, rb_wr_en, ram_wr_en, sig_alu_en, carry, zero, carry_new, zero_new: std_logic;
    type StateSymbol  is (RST, S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19, S20, S21, S22, S23, S24, S25, S26, S27, S28, S29);
  	signal fsm_state: StateSymbol;
    signal is_AB_eq, en_inc_pc: std_logic;
    signal AB_xor: std_logic_vector(15 downto 0);
  	signal Imm6, Imm9, Immlhi, Imm: std_logic_vector(15 downto 0);
    signal sig_add_1, sig_add_2, sig_add_res, sig_temp_R: std_logic_vector(15 downto 0);
    
begin
	Imm6(15 downto 6) <= "0000000000";
	Imm6(5 downto 0) <= sig_ir(5 downto 0);
	Imm9(15 downto 9) <= "0000000";
	Imm9(8 downto 0) <= sig_ir(8 downto 0);
	Immlhi(15 downto 7) <= sig_ir(8 downto 0);
	Immlhi(6 downto 0) <= "0000000";
	
	with sig_ir(15 downto 12) select 
		Imm <= Imm6 when "0001",
			Immlhi when "0011",
			Imm6 when "0100",
			Imm6 when "0101",
			Imm9 when "0110",
			Imm9 when "0111",
			Imm6 when "1100",
			Imm9 when "1000",
		 	x"0000" when others;
			 	
	PC: regen_16bit port map(data_in=>sig_pc_in, en=>pc_wr, data_out=>sig_pc);
	rom1: ROM port map(rom_addr=> sig_pc, rom_dataout=>sig_ir_in);
	IR: regen_16bit port map(data_in=>sig_ir_in, en=>ir_wr, data_out=>sig_ir);
	reg_bank: RegisterBank port map(wr_en=>rb_wr_en, clk=>clk, wr_addr=>rb_wr_addr, rd_addr1=>rb_rd_addr1, rd_addr2=>rb_rd_addr2, wr_data=>rb_wr_data, 
						data_out1 => rb_data1, data_out2 => rb_data2, 
						R0=>sig_r0, R1=>sig_r1, R2=>sig_r2, R3=>sig_r3, R4=>sig_r4, R5=>sig_r5, R6=>sig_r6, R7=>sig_r7);
	
	alu: ALU_16 port map(ALU_OP=>alu_op, alu_en=>sig_alu_en, A=>alu_in1, B=>alu_in2, cin=>carry, zin=>zero, S=>alu_res, C=>carry_new, Z=>zero_new);
	memory: RAM port map(wr_en=>ram_wr_en, clk=>clk, rd_addr=>ram_rd_addr, wr_addr=>ram_wr_addr, wr_data=>ram_wr_data, rd_data=>ram_rd_data);
	plus1: Plus1Adder port map(A=>sig_pc, PC_inc_en=> en_inc_pc, S=>INC_res);
	adder1: Adder_16 port map(A=>sig_add_1, B=>sig_add_2, cin=>'0', S=>sig_add_res);
	
	xor1: XOR_16 port map(A => rb_data1, B => rb_data2, S => AB_xor );
	is_AB_eq <= '1' when AB_xor=x"0000" else
				'0';

    process(clk, fsm_state, Imm, Immlhi, alu_res, carry, carry_new, ram_rd_data, rb_data1, rb_data2, sig_ir, zero, zero_new, INC_res, sig_add_res, sig_temp_R, is_AB_eq, sig_pc)
    	variable var_pc_in, var_rb_wr_data, var_alu_in1, var_alu_in2, var_ram_rd_addr, var_ram_wr_addr, var_ram_wr_data, var_add_1, var_add_2, var_temp_R :std_logic_vector(15 downto 0);
    	variable var_pc_wr, var_ir_wr, var_rb_wr_en, var_ram_wr_en, var_alu_en, var_carry, var_zero, var_inc_pc : std_logic;
    	variable var_rb_wr_addr, var_rb_rd_addr1, var_rb_rd_addr2: std_logic_vector(2 downto 0); 
    	variable var_alu_op: std_logic_vector(1 downto 0);
    	variable nq_var : StateSymbol;
    	
    begin
    	nq_var := fsm_state;
--        default values
		var_rb_wr_data := x"0000";
		var_ram_rd_addr := x"0000";
		var_ram_wr_addr := x"0000";
		var_ram_wr_data := x"0000";
		var_pc_wr := '0';
		var_ir_wr := '0';
		var_rb_wr_en := '0';
		var_ram_wr_en := '0';
		var_rb_wr_addr := "000";
		var_alu_op := "11";
		var_alu_en := '0';
		var_inc_pc := '0';
		var_add_1 := x"0000";
		var_add_2 := x"0000";
		
		case(fsm_state) is 
			when RST =>
				var_temp_R := x"0000";
				var_pc_in := x"0000";
				var_alu_in1 := x"0000";
				var_alu_in2 := x"0000";
				var_rb_rd_addr1 := "000";
				var_rb_rd_addr2 := "000";
				var_carry := '0';
				var_zero := '0';
				var_pc_wr := '1';
				nq_var := S0;
			when S0 =>
				var_ir_wr := '1';
				var_inc_pc := '1';
				var_pc_in := INC_res;
				if(sig_ir(15 downto 12)="0000" or sig_ir(15 downto 12)="0010") then
					nq_var := S1;
				elsif(sig_ir(15 downto 12)="0001") then
					nq_var := S3;
				elsif(sig_ir(15 downto 12)="0011") then
					nq_var := S5;
				elsif(sig_ir(15 downto 12)="0100") then
					nq_var := S6;
				elsif(sig_ir(15 downto 12)="0101") then
					nq_var := S7;
				elsif(sig_ir(15 downto 12)="0110") then
					nq_var := S8;
				elsif(sig_ir(15 downto 12)="0111") then
					nq_var := S16;
				elsif(sig_ir(15 downto 12)="1100") then
					nq_var := S24;
				elsif(sig_ir(15 downto 12)="1000") then
					nq_var := S26;
				elsif(sig_ir(15 downto 12)="1001") then
					nq_var := S28;
				end if;
			when S1 =>
				var_rb_rd_addr1 := sig_ir(11 downto 9);
				var_rb_rd_addr2 := sig_ir(8 downto 6);
				var_alu_in1 := rb_data1;
				var_alu_in2 := rb_data2;
				var_alu_en := '1';
				if(sig_ir(15 downto 12)="0000") then
					var_alu_op := "00";
				elsif(sig_ir(15 downto 12)="0010") then
					var_alu_op := "01";
				end if;
				nq_var := S2;
			when S2 =>
				var_rb_wr_addr := sig_ir(5 downto 3);
				var_rb_wr_data := alu_res;
				if( sig_ir(1)='1' and  carry='0') then
					var_rb_wr_en := '0';
				elsif( sig_ir(0)='1' and zero='0' ) then
					var_rb_wr_en := '0';
				else
					var_rb_wr_en := '1';
					var_carry := carry_new;
					var_zero := zero_new;
				end if;
				var_pc_wr := '1';
				nq_var := S0;
			when S3 =>
				var_rb_rd_addr1 := sig_ir(11 downto 9);
				var_alu_in1 := rb_data1;
				var_alu_in2 := Imm;
				var_alu_en := '1';
				var_alu_op := "00";
				nq_var := S4;
			when S4 =>
				var_rb_wr_addr := sig_ir(8 downto 6);
				var_rb_wr_data := alu_res; 
				var_rb_wr_en := '1';
				var_carry := carry_new;
				var_zero := zero_new;
				var_pc_wr := '1';
				nq_var := S0;
			when S5 =>
				var_rb_wr_addr := sig_ir(11 downto 9);
				var_rb_wr_data := Immlhi;
				var_rb_wr_en := '1';
				var_pc_wr := '1';
				nq_var := S0;
			when S6 =>
				var_rb_rd_addr1 := sig_ir(8 downto 6);
				var_alu_in1 := rb_data1;
				var_alu_in2 := Imm;
				var_alu_en := '1';
				var_alu_op := "00";
				var_ram_rd_addr := alu_res;
				var_rb_wr_data := ram_rd_data;
				var_rb_wr_addr := sig_ir(11 downto 9);
				var_rb_wr_en := '1';
				var_zero := zero_new;
				var_pc_wr := '1';
				nq_var := S0;
			when S7 =>
				var_rb_rd_addr1 := sig_ir(8 downto 6);
				var_rb_rd_addr2 := sig_ir(11 downto 9);
				var_alu_in1 := rb_data1;
				var_alu_in2 := Imm;
				var_alu_op := "00";
				var_alu_en := '1';
				var_ram_wr_addr := alu_res;
				var_ram_wr_data := rb_data2;
				var_ram_wr_en := '1';
				var_pc_wr := '1';
				nq_var := S0;
			when S8 =>
				var_rb_rd_addr1 := sig_ir(11 downto 9);
				var_ram_rd_addr := rb_data1;
				var_temp_R := rb_data1;
				var_rb_wr_data := ram_rd_data;
				var_rb_wr_addr := "000";
				var_rb_wr_en := '1';
				nq_var := S9;
			when S9 =>
				var_add_1 := var_temp_R;
				var_add_2 := x"0001";
				var_ram_rd_addr := sig_add_res;
				var_rb_wr_data := ram_rd_data;
				var_rb_wr_addr := "001";
				var_rb_wr_en := '1';
				nq_var := S10;
			when S10 =>
				var_add_1 := var_temp_R;
				var_add_2 := x"0002";
				var_ram_rd_addr := sig_add_res;
				var_rb_wr_data := ram_rd_data;
				var_rb_wr_addr := "010";
				var_rb_wr_en := '1';
				nq_var := S11;
			when S11 =>
				var_add_1 := var_temp_R;
				var_add_2 := x"0003";
				var_ram_rd_addr := sig_add_res;
				var_rb_wr_data := ram_rd_data;
				var_rb_wr_addr := "011";
				var_rb_wr_en := '1';
				nq_var := S12;
			when S12 =>
				var_add_1 := var_temp_R;
				var_add_2 := x"0004";
				var_ram_rd_addr := sig_add_res;
				var_rb_wr_data := ram_rd_data;
				var_rb_wr_addr := "100";
				var_rb_wr_en := '1';
				nq_var := S13;
			when S13 =>
				var_add_1 := var_temp_R;
				var_add_2 := x"0005";
				var_ram_rd_addr := sig_add_res;
				var_rb_wr_data := ram_rd_data;
				var_rb_wr_addr := "101";
				var_rb_wr_en := '1';
				nq_var := S14;
			when S14 =>
				var_add_1 := var_temp_R;
				var_add_2 := x"0006";
				var_ram_rd_addr := sig_add_res;
				var_rb_wr_data := ram_rd_data;
				var_rb_wr_addr := "110";
				var_rb_wr_en := '1';
				nq_var := S15;
			when S15 =>
				var_add_1 := var_temp_R;
				var_add_2 := x"0007";
				var_ram_rd_addr := sig_add_res;
				var_rb_wr_data := ram_rd_data;
				var_rb_wr_addr := "111";
				var_rb_wr_en := '1';
				var_pc_wr := '1';
				nq_var := S0;
			when S16 =>
				var_rb_rd_addr1 := sig_ir(11 downto 9);
				var_temp_R := rb_data1;
				var_rb_rd_addr2 := "000";
				var_ram_wr_addr := rb_data1;
				var_ram_wr_data := rb_data2;
				var_ram_wr_en := '1';
				nq_var := S17;
			when S17 =>
				var_add_1 := sig_temp_R;
				var_add_2 := x"0001";
				var_rb_rd_addr1 := "001";
				var_ram_wr_addr := sig_add_res;
				var_ram_wr_data := rb_data1;
				var_ram_wr_en := '1';
				nq_var := S18;
			when S18 =>
				var_add_1 := sig_temp_R;
				var_add_2 := x"0002";
				var_rb_rd_addr1 := "010";
				var_ram_wr_addr := sig_add_res;
				var_ram_wr_data := rb_data1;
				var_ram_wr_en := '1';
				nq_var := S19;
			when S19 =>
				var_add_1 := sig_temp_R;
				var_add_2 := x"0003";
				var_rb_rd_addr1 := "011";
				var_ram_wr_addr := sig_add_res;
				var_ram_wr_data := rb_data1;
				var_ram_wr_en := '1';
				nq_var := S20;
			when S20 =>
				var_add_1 := sig_temp_R;
				var_add_2 := x"0004";
				var_rb_rd_addr1 := "100";
				var_ram_wr_addr := sig_add_res;
				var_ram_wr_data := rb_data1;
				var_ram_wr_en := '1';
				nq_var := S21;
			when S21 =>
				var_add_1 := sig_temp_R;
				var_add_2 := x"0005";
				var_rb_rd_addr1 := "101";
				var_ram_wr_addr := sig_add_res;
				var_ram_wr_data := rb_data1;
				var_ram_wr_en := '1';
				nq_var := S22;
			when S22 =>
				var_add_1 := sig_temp_R;
				var_add_2 := x"0006";
				var_rb_rd_addr1 := "110";
				var_ram_wr_addr := sig_add_res;
				var_ram_wr_data := rb_data1;
				var_ram_wr_en := '1';
				nq_var := S23;
			when S23 =>
				var_add_1 := sig_temp_R;
				var_add_2 := x"0007";
				var_rb_rd_addr1 := "111";
				var_ram_wr_addr := sig_add_res;
				var_ram_wr_data := rb_data1;
				var_ram_wr_en := '1';
				var_pc_wr := '1';
				nq_var := S0;
			when S24 =>
				var_rb_rd_addr1 := sig_ir(11 downto 9);
				var_rb_rd_addr2 := sig_ir(8 downto 6);
				var_add_1 := sig_pc;
				var_add_2 := Imm;
				if(is_AB_eq='1') then
					var_temp_R := sig_add_res;
				else
				end if;
				nq_var := S25;
			when S25 =>
				if(is_AB_eq='1') then
					var_pc_in := sig_temp_R;
				end if;
				var_pc_wr := '1';
				nq_var := S0;
			when S26 =>
				var_add_1 := sig_pc;
				var_add_2 := Imm;
				var_temp_R := sig_add_res;
				var_rb_wr_data := sig_pc;
				var_rb_wr_addr := sig_ir(11 downto 9);
				var_rb_wr_en := '1';
				nq_var := S27;
			when S27 =>
				var_pc_in := sig_temp_R;
				var_pc_wr := '1';
				nq_var := S0;
			when S28 =>
				var_rb_wr_data := sig_pc;
				var_rb_wr_addr := sig_ir(11 downto 9);
				var_rb_wr_en := '1';
				var_rb_rd_addr1 := sig_ir(8 downto 6);
				var_temp_R := rb_data1;
				nq_var := S29;
			when S29 =>
				var_pc_in := sig_temp_R;
				var_pc_wr := '1';
				nq_var := S0;
		end case;
		
		sig_pc_in <= var_pc_in;
		rb_wr_data <= var_rb_wr_data;
		alu_in1 <= var_alu_in1;
		alu_in2 <= var_alu_in2;
		ram_rd_addr <= var_ram_rd_addr;
		ram_wr_addr <= var_ram_wr_addr;
		ram_wr_data <= var_ram_wr_data;
		pc_wr <= var_pc_wr;
		ir_wr <= var_ir_wr;
		rb_wr_en <= var_rb_wr_en;
		ram_wr_en <= var_ram_wr_en;
		carry <=var_carry;
		zero <= var_zero;
		rb_wr_addr <= var_rb_wr_addr;
		rb_rd_addr1 <= var_rb_rd_addr1;
		rb_rd_addr2 <= var_rb_rd_addr2;
		alu_op <= var_alu_op;
		sig_alu_en <= var_alu_en;
		en_inc_pc <= var_inc_pc;
		sig_add_1 <= var_add_1;
		sig_add_2 <= var_add_2;
		sig_temp_R <= var_temp_R;
		
		if(rising_edge(clk)) then
			if(reset='1') then
				fsm_state <= RST;
			else 
				fsm_state <= nq_var;
			end if;
		end if;
		
    end process;
end Struct;



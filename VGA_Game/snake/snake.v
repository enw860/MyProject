//SW[9] enable
//SW[0] reset
//KEY[3:0] -> up,down,left,right
//change d_clk to freq of 4HZ in lab
//HEX0 and HEX1 shows score in hex
module snake
	(
		CLOCK_50,						//	On Board 50 MHz
		// Your inputs and outputs here
        KEY,
        SW,
		HEX0,
		HEX1,
		// The ports below are for the VGA output.  Do not change.
		VGA_CLK,   						//	VGA Clock
		VGA_HS,							//	VGA H_SYNC
		VGA_VS,							//	VGA V_SYNC
		VGA_BLANK_N,						//	VGA BLANK
		VGA_SYNC_N,						//	VGA SYNC
		VGA_R,   						//	VGA Red[9:0]
		VGA_G,	 						//	VGA Green[9:0]
		VGA_B,   						//	VGA Blue[9:0]
		PS2_DAT,
		PS2_CLK
	);

	input			CLOCK_50;				//	50 MHz
	input   [9:0]   SW;
	input   [3:0]   KEY;
	input    PS2_DAT;
	input    PS2_CLK;

	// Declare your inputs and outputs here
	// Do not change the following outputs
	output			VGA_CLK;   				//	VGA Clock
	output			VGA_HS;					//	VGA H_SYNC
	output			VGA_VS;					//	VGA V_SYNC
	output			VGA_BLANK_N;				//	VGA BLANK
	output			VGA_SYNC_N;				//	VGA SYNC
	output	[9:0]	VGA_R;   				//	VGA Red[9:0]
	output	[9:0]	VGA_G;	 				//	VGA Green[9:0]
	output	[9:0]	VGA_B;   				//	VGA Blue[9:0]
	output 	[6:0]	HEX0;
	output 	[6:0]	HEX1;
	
	wire resetn;
	assign resetn = SW[1];
	
	// Create the colour, x, y and writeEn wires that are inputs to the controller.
	wire [2:0] vga_colour;
	wire [7:0] vga_x;
	wire [6:0] vga_y;
	wire writeEn;
	wire left,right,up,down;
	
	// Create an Instance of a VGA controller - there can be only one!
	// Define the number of colours as well as the initial background
	// image file (.MIF) for the controller.
	vga_adapter VGA(
			.resetn(resetn),
			.clock(CLOCK_50),
			.colour(vga_colour),
			.x(vga_x),
			.y(vga_y),
			.plot(writeEn),
			.VGA_R(VGA_R),
			.VGA_G(VGA_G),
			.VGA_B(VGA_B),
			.VGA_HS(VGA_HS),
			.VGA_VS(VGA_VS),
			.VGA_BLANK(VGA_BLANK_N),
			.VGA_SYNC(VGA_SYNC_N),
			.VGA_CLK(VGA_CLK));
		defparam VGA.RESOLUTION = "160x120";
		defparam VGA.MONOCHROME = "FALSE";
		defparam VGA.BITS_PER_COLOUR_CHANNEL = 1;
		defparam VGA.BACKGROUND_IMAGE = "black.mif";
			
	// Put your code here. Your code should produce signals x,y,colour and writeEn/plot
	// for the VGA controller, in addition to any other functionality your design may require.
    
    // Instansiate datapath
	wire d_clock,dead,getfood,makefood,finish;
	wire plot,remove,press;
	wire [7:0] x;
	wire [6:0] y;
	wire [2:0] colour;
	wire [7:0]rand_x;
	wire [6:0]rand_y;
	
	wire [7:0]not_dead_x;
	wire [6:0]not_dead_y;
	wire [2:0]not_dead_colour;
	
	wire [7:0]score;
	
	wire kbdreset = 1'b0;
	wire read, scan_ready;
	wire [7:0] scan_code;
	reg kbd_left,kbd_right,kbd_up,kbd_down,kbd_reset;
	
	always @(posedge scan_ready)
	begin
		 kbd_left <=(scan_code == 8'h6B)? 0:1;
		 kbd_right <=(scan_code == 8'h74)? 0:1;
		 kbd_up <= (scan_code == 8'h75)? 0:1;
		 kbd_down <= (scan_code == 8'h72)? 0:1;
		 kbd_reset <= (scan_code == 8'h29);
	end
	
	oneshot pulser(
		.pulse_out(read),
		.trigger_in(scan_ready),
		.clk(CLOCK_50)
	);

	keyboard kbd(
	  .keyboard_clk(PS2_CLK),
	  .keyboard_data(PS2_DAT),
	  .clock50(CLOCK_50),
	  .reset(kbdreset),
	  .read(read),
	  .scan_ready(scan_ready),
	  .scan_code(scan_code)
	);
	
	delay_clock d_fq(.clock(CLOCK_50),.reset(SW[0]),.d_clk(d_clock));
	
	resetSceen cls(
		.clk(CLOCK_50),
		.dead(dead), 
		//.reset(SW[0]),
		.reset(kbd_reset),
		.not_dead_x(not_dead_x),
		.not_dead_y(not_dead_y),
		.not_dead_colour(not_dead_colour),
		.done(finish),
		.reset_x(vga_x),
		.reset_y(vga_y),
		.colourout(vga_colour)
	);
	
	datapath d0(
		.up(up),
		.down(down),
		.left(left),
		.right(right),
		.clk(d_clock),
		.plot(plot),
		.remove(remove),
		.press(press),
		.check_clk(CLOCK_50),
		.reset(kbd_reset),
		//.reset(SW[0]),
		.score(score[7:0]),
		.x(x[7:0]),
		.y(y[6:0]),
		.colour(colour[2:0]),
		.dead(dead)
	);

    // Instansiate FSM control
    control c0(
		.clock(d_clock),
		//.c_up(KEY[0]),
		//.c_down(KEY[1]),
		//.c_left(KEY[2]),
		//.c_right(KEY[3]),
		.press(press),
		.done(finish),
		.c_up(kbd_up),
		.c_down(kbd_down),
		.c_left(kbd_left),
		.c_right(kbd_right),
		.dead(dead),
		.getfood(getfood),
		//.reset(SW[0]),
		.reset(kbd_reset),
		.up(up),
		.down(down),
		.left(left),
		.right(right),
		.enable(SW[9]),
		.makefood(makefood),
		.plot(plot),
		.remove(remove),
		.writeEn(writeEn)
	);
	
	food fd(
		.rand_X(rand_x[7:0]),
		.rand_Y(rand_y[6:0]),
		.foodcolour(3'b101),
		.x(x[7:0]),
		.y(y[6:0]),
		.colour(colour),
		//.reset(SW[0]),
		.reset(kbd_reset),
		.clk(d_clock),
		.score(score[7:0]),
		.getfood(getfood),
		.makefood(makefood),
		.xout(not_dead_x),
		.yout(not_dead_y),
		.colourout(not_dead_colour)
	);
	
	hex_decoder H0(
        .hex_digit(score[3:0]), 
        .segments(HEX0)
        );
        
    hex_decoder H1(
        .hex_digit(score[7:4]), 
        .segments(HEX1)
        );
		
	randomGrid rand_food_position(
		.clk(d_clock),
		//.reset(SW[0]),
		.reset(kbd_reset),
		.rand_X(rand_x[7:0]),
		.rand_Y(rand_y[6:0])
	);
endmodule

module datapath(
	input left, right, up, down,reset,clk,plot,remove,press,check_clk,
	input [7:0]score,
	output reg [7:0]x,
	output reg [6:0]y,
	output reg [2:0]colour,
	output reg dead
); 
	reg [7:0]local_x = 8'd80;
	reg [6:0]local_y = 7'd60;
	reg [2:0]de_colour = 3'b111;
	
	reg [7:0]x_counter = 1'd0;
	reg [6:0]y_counter = 1'd0;
	
	reg [7:0] snakeX [0:100];
	reg [6:0] snakeY [0:100];
	reg [8:0] count;
	
	reg [7:0]check_counter;
	reg found;
	
	always @(posedge check_clk, posedge reset) begin
		if(reset) begin
			found = 1'd0;
			check_counter <= 1'd1;
		end
		else begin
			if(check_counter-1'd1 != score) begin
				check_counter <= check_counter + 1'd1;
				found <= (snakeX[check_counter]== x_counter + local_x)&&(snakeY[check_counter]== y_counter + local_y);
			end
			else
				check_counter <= 1'd1;
		end
	end
	
	always @(posedge clk, posedge reset) begin
		if(reset) begin
			x<=local_x;
			y<=local_y;
			x_counter<=1'd0;
			y_counter<=1'd0;
			colour<=de_colour;
			dead<=found;
			count <= 1'd0;
			snakeX[0] <= local_x;
			snakeY[0] <= local_y;
		end
		else begin
			if(left)
				x_counter<=x_counter-1'b1;
			if(right)
				x_counter<=x_counter+1'b1;
			if(up)
				y_counter<=y_counter-1'b1;
			if(down)
				y_counter<=y_counter+1'b1;
			
			if(x == 8'd159) begin
				dead<=1'b1;
			end
			if(y == 7'd119) begin
				dead<=1'b1;
			end
			if(x == 1'd0) begin
				dead<=1'b1;
			end
			if(y == 1'd0) begin
				dead<=1'b1;
			end
			
			if (press) begin
				for (count = 100;  count > 0; count = count - 1) begin
					snakeX[count] = snakeX[count - 1];
					snakeY[count] = snakeY[count - 1];
				end
				snakeX[0] = x_counter + local_x;
				snakeY[0] = y_counter + local_y;
			end
			
			if(plot) begin
				colour<=de_colour;
				x<=snakeX[0];
				y<=snakeY[0];
			end
			if(remove) begin 
				colour<=1'd0;
				x<=snakeX[score+1];
				y<=snakeY[score+1];
			end
		end
	end
endmodule

//1/4 of CLOCK_50
module delay_clock(
	input clock,reset,
	output reg d_clk
);

	reg [31:0] counter;
	
	always @(posedge clock, posedge reset) begin
		if(reset) begin
			counter <= 1'b0;
			d_clk<= 1'b0;
		end
		else begin
			if(counter == 32'd600000/*2'd2*/) begin
				counter <= 1'b0;
				d_clk <= !d_clk;
			end
			else begin
				counter <= counter + 1'b1;
			end
		end
	end	
endmodule

module control(
	input enable,reset,dead,clock, getfood,done,
	input c_left,c_right,c_up,c_down,
	output reg left,right,up,down,
	output reg plot,remove, writeEn, makefood,press
);
	reg [3:0] current_state, next_state;
	reg [3:0] direction;
	
	localparam 
				START			= 4'd0,
				PLOTE			= 4'd1,
				LEFT			= 4'd2,
				RIGHT			= 4'd3,
				UP				= 4'd4,
				DOWN			= 4'd5,
				DEAD			= 4'd6,
				MAKEFOOD		= 4'd7,
				CLEAN			= 4'd8;

  // Next state logic aka our state table
    always@(*)
    begin: state_table 
            case (current_state)
					START: begin
						if(enable) begin
							direction = LEFT;
							//next_state = PLOTE;
							next_state = MAKEFOOD;
						end
						else 
							next_state = START;
					end
					LEFT: begin
						if(dead)
							next_state = DEAD;
						else if (getfood)
							next_state = MAKEFOOD;
						else begin
							if(!c_up)
								direction = UP;
							if(!c_down)
								direction = DOWN;
							next_state = PLOTE;
						end
					end
					RIGHT: begin
						if(dead)
							next_state = DEAD;
						else if (getfood)
							next_state = MAKEFOOD;
						else begin
							if(!c_up)
								direction = UP;
							if(!c_down)
								direction = DOWN;
							next_state = PLOTE;
						end
					end
					UP: begin
						if(dead)
							next_state = DEAD;
						else if (getfood)
							next_state = MAKEFOOD;
						else begin
							if(!c_left)
								direction = LEFT;
							if(!c_right)
								direction = RIGHT;
							next_state = PLOTE;
						end
					end
					DOWN: begin
						if(dead)
							next_state = DEAD;
						else if (getfood)
							next_state = MAKEFOOD;
						else begin
							if(!c_left)
								direction = LEFT;
							if(!c_right)
								direction = RIGHT;
							next_state = PLOTE;
						end
					end
					PLOTE: begin
						if(dead)
							next_state = DEAD;
						//else if(getfood)
						//	next_state = MAKEFOOD;
						else begin
							next_state = direction;
						end
					end
					MAKEFOOD: next_state = dead ? DEAD : direction;
					DEAD: next_state = done ? CLEAN : DEAD;
					CLEAN: next_state = enable ? CLEAN : START;
            default: next_state = START;
        endcase
    end // state_table
	
	always @(*)
    begin: enable_signals 
        left = 1'b0;
		right = 1'b0;
		up = 1'b0;
		down = 1'b0;
		plot = 1'b0;
		remove = 1'b0;
		writeEn = 1'b0;
		makefood = 1'b0;
		press = 1'b0;

        case (current_state)
			LEFT: begin 
				writeEn = 1'b1;
				left = 1'b1;
				plot = 1'b1;
				press = 1'b1;
            end
			RIGHT: begin 
				writeEn = 1'b1;
				right = 1'b1;
				plot = 1'b1;
				press = 1'b1;
            end
			UP: begin 
				writeEn = 1'b1;
				up = 1'b1;
				plot = 1'b1;
				press = 1'b1;
            end
			DOWN: begin 
				writeEn = 1'b1;
				down = 1'b1;
				plot = 1'b1;
				press = 1'b1;
            end
			PLOTE:begin
				writeEn = 1'b1;
				remove = 1'b1;
			end
			MAKEFOOD: begin
				writeEn = 1'b1;
				makefood = 1'b1;
			end
			DEAD: 
				writeEn = 1'b1;
        endcase
    end // enable_signals

 	// current_state registers
    always@(posedge clock, posedge reset)
    begin: state_FFs
        if(reset)
            current_state <= START;
        else
            current_state <= next_state;
    end // state_FFS
endmodule

module food(
	input [7:0]rand_X,
	input [6:0]rand_Y,
	input [2:0]foodcolour,
	input [7:0]x,
	input [6:0]y,
	input [2:0]colour,
	input reset,clk,makefood,
	output reg [7:0]score,
	output reg getfood,
	output reg [7:0]xout,
	output reg [6:0]yout,
	output reg [2:0]colourout
);
	reg [7:0]foodx;
	reg [6:0]foody;
	reg [2:0]count;
	always @(posedge clk, posedge reset) begin
		if(reset) begin
			score <= 8'b11111111;
			getfood <= 1'b0;
			xout <= x;
			yout <= y;
			foodx <= 8'd75;
			foody <= 7'd60;
			count <= 1'b1;
			colourout <= colour;
		end
		else begin
			if(makefood) begin
				foodx <= rand_X;
				foody <= rand_Y;
				xout <= rand_X;
				yout <= rand_Y;	
				score <= score + makefood;
				colourout <= foodcolour; 
			end
			else begin 
				xout <= x;
				yout <= y;
				colourout <= colour;
			end
		
			if((foodx==x)&&(foody==y))
				getfood <= 1'b1;
			else
				getfood <= 1'b0;
		end
	end
endmodule

module hex_decoder(hex_digit, segments);
    input [3:0] hex_digit;
    output reg [6:0] segments;
   
    always @(*)
        case (hex_digit)
            4'h0: segments = 7'b100_0000;
            4'h1: segments = 7'b111_1001;
            4'h2: segments = 7'b010_0100;
            4'h3: segments = 7'b011_0000;
            4'h4: segments = 7'b001_1001;
            4'h5: segments = 7'b001_0010;
            4'h6: segments = 7'b000_0010;
            4'h7: segments = 7'b111_1000;
            4'h8: segments = 7'b000_0000;
            4'h9: segments = 7'b001_1000;
            4'hA: segments = 7'b000_1000;
            4'hB: segments = 7'b000_0011;
            4'hC: segments = 7'b100_0110;
            4'hD: segments = 7'b010_0001;
            4'hE: segments = 7'b000_0110;
            4'hF: segments = 7'b000_1110;   
            default: segments = 7'h7f;
        endcase
endmodule

module keyboard(keyboard_clk, keyboard_data, clock50, reset, read, scan_ready, scan_code);
input keyboard_clk;
input keyboard_data;
input clock50; // 50 Mhz system clock
input reset;
input read;
output scan_ready;
output [7:0] scan_code;
reg ready_set;
reg [7:0] scan_code;
reg scan_ready;
reg read_char;
reg clock; // 25 Mhz internal clock

reg [3:0] incnt;
reg [8:0] shiftin;

reg [7:0] filter;
reg keyboard_clk_filtered;

// scan_ready is set to 1 when scan_code is available.
// user should set read to 1 and then to 0 to clear scan_ready

always @ (posedge ready_set or posedge read)
if (read == 1) scan_ready <= 0;
else scan_ready <= 1;

// divide-by-two 50MHz to 25MHz
always @(posedge clock50)
    clock <= ~clock;



// This process filters the raw clock signal coming from the keyboard 
// using an eight-bit shift register and two AND gates

always @(posedge clock)
begin
   filter <= {keyboard_clk, filter[7:1]};
   if (filter==8'b1111_1111) keyboard_clk_filtered <= 1;
   else if (filter==8'b0000_0000) keyboard_clk_filtered <= 0;
end


// This process reads in serial data coming from the terminal

always @(posedge keyboard_clk_filtered)
begin
   if (reset==1)
   begin
      incnt <= 4'b0000;
      read_char <= 0;
   end
   else if (keyboard_data==0 && read_char==0)
   begin
    read_char <= 1;
    ready_set <= 0;
   end
   else
   begin
       // shift in next 8 data bits to assemble a scan code    
       if (read_char == 1)
           begin
              if (incnt < 9) 
              begin
                incnt <= incnt + 1'b1;
                shiftin = { keyboard_data, shiftin[8:1]};
                ready_set <= 0;
            end
        else
            begin
                incnt <= 0;
                scan_code <= shiftin[7:0];
                read_char <= 0;
                ready_set <= 1;
            end
        end
    end
end

endmodule

module oneshot(output reg pulse_out, input trigger_in, input clk);
reg delay;

always @ (posedge clk)
begin
    if (trigger_in && !delay) pulse_out <= 1'b1;
    else pulse_out <= 1'b0;
    delay <= trigger_in;
end 
endmodule

module randomGrid(clk, rand_X, rand_Y,reset);
	input clk,reset;
	output reg [7:0]rand_X;
	output reg [6:0]rand_Y;
	reg [5:0]pointX, pointY = 6'd10;

	always @(posedge clk,posedge reset) begin
		if(reset) begin
			rand_X <= 8'd30;
			rand_Y <= 7'd20;
			pointX <= 6'd10;
			pointY <= 6'd10;
		end
		else begin
			pointX <= pointX + 6'd3;	
			pointY <= pointY + 6'd1;	
			
			if(pointX>6'd51)
				rand_X <= 6'd108;
			else if (pointX<6'd2)
				rand_X <= 6'd13;
			else
				rand_X <= (pointX * 3);

			if(pointY>39)
				rand_Y <= 100;
			else if (pointY<2)
				rand_Y <= 13;
			else
				rand_Y <= (pointY * 3);
		end
	end
endmodule

module resetSceen(
	input clk,dead, reset,
	input [7:0]not_dead_x,
	input [6:0]not_dead_y,
	input [2:0]not_dead_colour,
	output reg done,
	output reg [7:0]reset_x,
	output reg [6:0]reset_y,
	output reg [2:0]colourout
);
	reg [7:0]x_start = 1'b0;
	reg [6:0]y_start = 1'b0;
	
	always @(posedge clk, posedge reset) begin
		if(reset) begin
			reset_x <= not_dead_x;
			reset_y <= not_dead_y;
			colourout <= not_dead_colour;
			done <= 1'b0;
			x_start <= 1'b0;
			y_start <= 1'b0;
		end
		else begin
			if(dead) begin
				colourout <= 3'b000;
				if(x_start == 8'd158) begin
					x_start <= 1'b0;
					reset_x<=x_start;
					if(y_start<7'd119) begin
						y_start <= y_start+1'b1;
						reset_y<=y_start;
					end
					else begin
						y_start <= 1'b0;
						reset_y<=y_start;
						done <= 1'b1;
					end
				end
				else begin	
					x_start <= x_start+1'b1;
					reset_x <= x_start;
				end
			end
			else begin
				reset_x <= not_dead_x;
				reset_y <= not_dead_y;
				colourout <= not_dead_colour;
			end
		end
	end
endmodule
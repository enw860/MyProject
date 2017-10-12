/*
Given an n x n array, return the array elements arranged from outermost elements to the middle element, traveling clockwise.

array = [[1,2,3],
         [4,5,6],
         [7,8,9]]
snail(array) #=> [1,2,3,6,9,8,7,4,5]

*/

var printSnail = function(matrix){
	if(matrix[0].length==0) return [];

	var result = [];
	var result_len = matrix.length*matrix.length;
	var init_x = 0, init_y = 0;
	var range_x = matrix.length-1, range_y = matrix.length-1;
	
	var forward_x = true, forward_y = false;
	var backword_x = false, backword_y = false; 

	var x = init_x;
	var y = init_y;
	while(result.length!=result_len){
		result.push(matrix[y][x]);
		
		if(forward_x){
			x++;
			if(x+1>range_x){
				init_y++;
				forward_x = false;
				forward_y = true;
			}
		}else if(forward_y){
			y++;
			if(y+1>range_y){
				range_x--;
				forward_y = false;
				backword_x = true;
			}
		}else if(backword_x){
			x--;
			if(x-1<init_x){
				range_y--;
				backword_x = false;
				backword_y = true;
			}
		}else if(backword_y){
			y--;
			if(y-1<init_y){
				init_x++;
				backword_y = false;
				forward_x = true;
			}
		}
	}
	return result;
}

var matrix = [[1,2,3,4,5],
         [6,7,8,9,10],
         [6,4,8,9,44],
         [6,7,23,9,55],
         [6,12,8,9,823]]
console.log(printSnail(matrix));





var arr;
//arr = [[3,2,3],[4,5,6],[7,8,9]];
//arr = [[1,3],[2,5]];
//arr = [[1]];
arr = [[2,5,3],[1,-2,-1],[1,3,4]];

function powerOf(numb, pow){
	if(pow==0) return 1;
	return numb * powerOf(numb,pow-1);
}

function determinant(arr){
	if(arr.length==0) return console.log("Not valid");
	if(arr.length==1) return arr[0][0];

	var result = 0;

	if(arr.length==2){
		result = arr[0][0]*arr[1][1]-arr[0][1]*arr[1][0];
		return result;
	}

	for(var i=0; i<arr[0].length; i++){
		var sign = powerOf(-1,i);

		var subarray = [];
		for(var j=1; j<arr.length;j++){
			var line = [];
			for(var k=0; k<arr[j].length; k++){
				if(k==i) continue;
				line.push(arr[j][k]);
			}
			subarray.push(line);
		}
		result += sign*arr[0][i]*determinant(subarray);
	}
	return result;
}

console.log(determinant(arr));
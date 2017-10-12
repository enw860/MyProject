var arr;
//arr = [0, 1, 2, 5, 1, 0];
//arr = [3, 2, 3, 6, 4, 1, 2, 3, 2, 1, 2, 3];
//arr = [1, 2, 2, 2, 1];
//arr = [1,2,5,4,5,6,7,6,9,1,1,2,4,5,6,7,8,2,3,4,1,3]
arr = [3,2,3,6,4,1,2,3,2,1,2,2,2,1];

function pickPeaks(arr){
  	var pos = [];
  	var peaks = [];
  
  	if(arr.length > 2) {
    	var position = -1;
    	for(var i=1; i<arr.length;i++){
     	 	if(arr[i] > arr[i-1]) {
      	  		position = i;
      		} else if(arr[i] < arr[i-1] && position != -1) {
		        pos.push(position);
		        peaks.push(arr[position]);
		        position = -1;
		    }
		}
  	}
  return {pos,peaks};
}

console.log(pickPeaks(arr));
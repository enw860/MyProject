var abs = function(numb){
	return (numb<0)? numb*(-1):numb;
}

var som = function(x, y){
	return x+y;
}

var mini = function(x, y){
	return (x>y)? y:x;	
}

var maxi = function(x, y){
	return (x>y)? x:y;
}

var lcmu = function(x, y){
	return abs(x*y)/gcdi(x,y);
}

var gcdi = function(x, y){
	x = abs(x);
	y = abs(y);

	if(x==0) return y;
	if(y==0) return x;
	
	return (x>y)? gcdi(y ,x%y):gcdi(x,y%x);
}

function oper_array(fct, arr, init){
	var backArr = new Array(arr.length);
	backArr[0] = fct(init, arr[0]);
	for(var i=0; i<arr.length-1; i++){
		backArr[i+1] = fct(backArr[i], arr[i+1]);
	}
	return backArr;
}

a = [18, 69, -90, -78, 65, 40];

console.log(oper_array(gcdi, a, a[0])); //=> [18, 3, 3, 3, 1, 1]
console.log(oper_array(lcmu, a, a[0])); //=> [18, 414, 2070, 26910, 26910, 107640]
console.log(oper_array(som, a, 0)); //=> [18, 87, -3, -81, -16, 24]
console.log(oper_array(mini, a, a[0])); //=> [18, 18, -90, -90, -90, -90]
console.log(oper_array(maxi, a, a[0])); //=> [18, 69, 69, 69, 69, 69]


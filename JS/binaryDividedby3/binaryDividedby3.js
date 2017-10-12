function binaryExp(numb){
	if(numb==0) return "0";

	var code = "";
	while(numb>0){
        code = ((numb%2==0)? "0":"1")+code;
        numb=parseInt(numb/2);
	}
	return code;
}

function regexMatch(str){
	var values = str.match("^(0*|^0*(1(01*0)*10*)+)$");
	return (values!=null);
}

console.log(regexMatch(binaryExp(18)));
console.log(regexMatch(binaryExp(56278)));
console.log(regexMatch(binaryExp(71)));
console.log(regexMatch(binaryExp(91)));
function calculate(numb1, numb2, opt){
	if(!(typeof numb1 === 'number')||!(typeof numb2 === 'number')){
		return "unknown value";
	}

	switch(opt){
		case("-"): return numb1-numb2;
		case("*"): return numb1*numb2;
		case("+"): return numb1+numb2;
		case("/"): return numb1/numb2;
		default: return "unknown value"
	}
}

console.log(calculate(1,2,"+"));
console.log(calculate(2,1,"*"));
console.log(calculate(3,99,"-"));
console.log(calculate(2,123,"/"));
console.log(calculate(1,2,"&"));
console.log(calculate(1,"k","*"));

console.log(9);
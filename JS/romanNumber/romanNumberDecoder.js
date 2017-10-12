var special = {'IX':9,'XC':90,'CM':900,'IV':4,'XL':40, 'CD':400};
var regular = {'I':1,'V':5,'X':10,'L':50,'C':100,'D':500,'M':1000};

function checkBoundary(str){
	if(str.length==1) return false;
	return (special.hasOwnProperty(str))? special[str]:false;
}

function solution(str){
	var result = 0;
	for(var i=0; i<str.length; i++){
		var temp = str.substring(i,i+2);
		temp = checkBoundary(temp);
		
		if(temp==false){
			result += regular[str[i]];
		}else{
			i++;
			result += temp;
		}
	}
	return result;
}

var testStr;
//testStr = "MCMXC";
testStr = "MDCLXVI";
console.log(solution(testStr));
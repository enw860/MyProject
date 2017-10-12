var a = "29404072602600431150703083248028007484944560094618635473955513955109161655832239910324";
var b = "7850849018988076861089339891810806139136297473440860668025431010365969426045781821";
var c = "9007199254740992";

function clearFrontZero(str){
	for(var i=0; i<str.length; i++){
  		if(str[i]!=0){
  			return str.substring(i,str.length);
  			break;
  		}
  	}
  	return str;
}

function getQuote(a,b){
	var count=1;
	while(a-b>0){
		a-=b;
		count++;
	}
	return count;
}

function divideStrings(a,b){
	var quotient = '', remainder = '';
  	if(b.length>a.length) return ['0', a];
  	var aIndex = 0;

  	while(aIndex<a.length){
  		remainder += a[aIndex];

  		if((remainder=='00')&&(a[aIndex]=='0')){
  			remainder = '0';
  		}

  		if(parseInt(remainder)>=parseInt(b)){
  			//quotient += Math.floor(remainder/b);
  			quotient += getQuote(remainder,b);
  			remainder = remainder%b+'';
  		}else{
  			quotient+='0';
  		}

  		aIndex++;
  	}
  	
   	return [clearFrontZero(quotient),clearFrontZero(remainder)];
}

function divideNormal(a,b){
	return [Math.floor(a/b), a%b]
}

console.log(divideStrings(a,b));
console.log(divideNormal(a,b));
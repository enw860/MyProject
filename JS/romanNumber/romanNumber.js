function strOutput(numb, small, big, bigbig){
	var output="";

	if(numb==4) return small+big;
	else if(numb==9) return small+bigbig;
	else if(numb>=5){
		output+=big;
		numb-=5;
	}

	if(numb<=3){
		for(var i=0; i<numb; i++) output+=small;
	}
	return output;
}

function solution(numb){
	var codeValues = "";
	//if((numb<=0)||(numb>3999)) console.log("Out of range");
	
	var kn = parseInt(numb/1000,10);
	var hn = parseInt((numb-kn*1000)/100,10);
	var tn = parseInt((numb-kn*1000-hn*100)/10,10);
	var sn = parseInt((numb-kn*1000-hn*100-tn*10),10);

	for(var i=0; i<kn; i++) codeValues+="M";
	codeValues += strOutput(hn, "C","D","M");
	codeValues += strOutput(tn, "X","L","C");
	codeValues += strOutput(sn, "I","V","X");

	return codeValues;
}

console.log(solution(849));
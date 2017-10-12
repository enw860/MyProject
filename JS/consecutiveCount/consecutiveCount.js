function countConsecutive(items,key){
	items = items.toString();
	key = key.toString();
	var maxCount = 0;
	var count = 0;

	for(var i=0; i<items.length-1; i++){
		var isKey = (items[i]==key);
		if((isKey)&&(count==0)) count++;
		if((isKey)&&(items[i]==items[i+1])){
			count++;
		}else{
			if(count>maxCount) maxCount=count;
			else count=0;
		}
	}
	if((items[items.length-1]==key)&&(maxCount==0)) maxCount=1;
	return (count>maxCount)? count:maxCount;
};

console.log(countConsecutive(1002,2));
console.log(countConsecutive(10000,0));
console.log(countConsecutive("abdbbbab","b"));
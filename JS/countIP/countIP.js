function chopDownIp(ip){
	var ips = [];
	var initposition = 0;

	for(var i=0; i<ip.length; i++){
		var temp;
		if(ip[i]==="."){
			temp = ip.substring(initposition,i);
			ips.push(parseInt(temp));
			initposition = i+1;
		}

		if(i==ip.length-1){
			if(i==initposition){
				ips.push(parseInt(ip[i]));
			}else{
				temp = ip.substring(initposition,i+1);
				ips.push(parseInt(temp));
			}
		} 
	}
	return ips;
}

function ipsBetween(ip1, ip2){
	ip1 = chopDownIp(ip1);
	ip2 = chopDownIp(ip2);

	console.log(ip1,ip2);
	var difference = [0,0,0,0];
	for(var i=0; i<4; i++){
		difference[i] = ip1[i]-ip2[i];
	}

	var sum=0;
	sum+=256*256*256*difference[0];
	sum+=256*256*difference[1];
	sum+=256*difference[2];
	sum+=difference[3];

	return (sum<0)? -1*sum:sum;
}

console.log(ipsBetween("20.0.0.10", "20.0.1.0"));


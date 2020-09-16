function RecStr(pref, n, digits)
{
	var ns = n.toString();
	var ret = pref;

	for(var z = ns.length; z < digits; z++)
		ret += '0';

	return ret + ns; 
}
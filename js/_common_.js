function RecStr(pref, n, digits)
{
	var ns = n.toString();
	var ret = pref;

	for(var z = ns.length; z < digits; z++)
		ret += '0';

	return ret + ns; 
}

Object.defineProperties(_G, {
	'ENTITY_VER_28a': { value: 280, writable: false }, // Last Light build 2012-10-19
	'ENTITY_VER_28b': { value: 281, writable: false }, // Last Light build 2012-12-03
	'ENTITY_VER_29':  { value: 290, writable: false }  // Last Light release
});
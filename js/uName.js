this.get_name = function(crc) {
    var name = hash_table_names[crc];
    if (!name) {
        print("unknown name found");
        return "";
    } else
        return name;
}

var names = [
    "default",
    "station",
    "city",
    "jail"
];

var hash_table_names = new Object;
for (var i = 0; i < names.length; i++) {
    hash_table_names[crc32(names[i])] = names[i];
}
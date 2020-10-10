/*
 * Script for decomple redux load screens like dead city
 */

var menus = reader.ReadArray("menus")
for (var i = 0; i < menus.count; i++) {
    var item = menus.ReadSection(RecStr("item_", i, 4))
}
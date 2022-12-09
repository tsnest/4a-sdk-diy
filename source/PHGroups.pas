unit PHGroups;

interface

const
	PH_GROUP_DEFAULT             : Longword = 0;
	PH_GROUP_MANIPULATOR         : Longword = 10;
	PH_GROUP_SHAPE               : Longword = 11;
	PH_GROUP_ENV_ZONE            : Longword = 12;
	
	PH_GROUP_DEFAULT_MASK        : Longword = 1;
	PH_GROUP_MANIPULATOR_MASK    : Longword = 1 shl 10;
	PH_GROUP_SHAPE_MASK          : Longword = 1 shl 11;
	PH_GROUP_ENV_ZONE_MASK       : Longword = 1 shl 12;
implementation

end.
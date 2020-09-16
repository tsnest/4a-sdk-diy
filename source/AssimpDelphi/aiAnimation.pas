unit aiAnimation;

interface
uses aiTypes, aiVector3D, aiQuaternion;

type TaiVectorKey = record
	mTime: Double;
	mValue: TaiVector3D;
end;
TaiVectorKeyArray = array[0.._AI_MAX_ARRAY] of TaiVectorKey;
PaiVectorKeyArray = ^TaiVectorKeyArray;

type TaiQuatKey = record
	mTime: Double;
	mValue: TaiQuaternion;
end;
TaiQuatKeyArray = array[0.._AI_MAX_ARRAY] of TaiQuatKey;
PaiQuatKeyArray = ^TaiQuatKeyArray;

{$Z4}
type TaiAnimBehaviour = (
	aiAnimBehaviour_DEFAULT			= $00,
	aiAnimBehaviour_CONSTANT		= $01,
	aiAnimBehaviour_LINEAR			= $02,
	aiAnimBehaviour_REPEAT			= $03
);

type TaiNodeAnim = record
	mNodeName: aiString;
	
	mNumPositionKeys: Cardinal;
	mPositionKeys: PaiVectorKeyArray;
	
	mNumRotationKeys: Cardinal;
	mRotationKeys: PaiQuatKeyArray;
	
	mNumScalingKeys: Cardinal;
	mScalingKeys: PaiVectorKeyArray;
	
	mPreState: TaiAnimBehaviour;
	mPostState: TaiAnimBehaviour;
end;
PaiNodeAnim = ^TaiNodeAnim;
PaiNodeAnimArray = array[0.._AI_MAX_ARRAY] of PaiNodeAnim;
PPaiNodeAnimArray = ^PaiNodeAnimArray;

type TaiAnimation = record
	mName: aiString;
	mDuration: Double;
	mTicksPerSecond: Double;
	
	mNumChannels: Cardinal;
	mChannels: PPaiNodeAnimArray;
	
	mNumMeshChannels: Cardinal;
	mMeshChannels: Pointer;
	
	mNumMorphMeshChannels: Cardinal;
	mMorphMeshChannels: Pointer;
end;
PaiAnimation = ^TaiAnimation;
PaiAnimationArray = array[0.._AI_MAX_ARRAY] of PaiAnimation;
PPaiAnimationArray = ^PaiAnimationArray;

implementation

end.
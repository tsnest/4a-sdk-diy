unit GLExt;

interface
uses GL;

// TODO: Change ugly PFNGL*PROC type names so something more nice looking?

// OpenGL 1.1
// TODO: Texture objects
const
  GL_POLYGON_OFFSET_UNITS         = $2A00;
  GL_POLYGON_OFFSET_POINT         = $2A01;
  GL_POLYGON_OFFSET_LINE          = $2A02;
  GL_POLYGON_OFFSET_FILL          = $8037;
  GL_POLYGON_OFFSET_FACTOR        = $8038;

  GL_VERTEX_ARRAY                 = $8074;
  GL_NORMAL_ARRAY                 = $8075;
  GL_COLOR_ARRAY                  = $8076;
  GL_INDEX_ARRAY                  = $8077;
  GL_TEXTURE_COORD_ARRAY          = $8078;
  GL_EDGE_FLAG_ARRAY              = $8079;
  GL_VERTEX_ARRAY_SIZE            = $807A;
  GL_VERTEX_ARRAY_TYPE            = $807B;
  GL_VERTEX_ARRAY_STRIDE          = $807C;
  GL_NORMAL_ARRAY_TYPE            = $807E;
  GL_NORMAL_ARRAY_STRIDE          = $807F;
  GL_COLOR_ARRAY_SIZE             = $8081;
  GL_COLOR_ARRAY_TYPE             = $8082;
  GL_COLOR_ARRAY_STRIDE           = $8083;
  GL_INDEX_ARRAY_TYPE             = $8085;
  GL_INDEX_ARRAY_STRIDE           = $8086;
  GL_TEXTURE_COORD_ARRAY_SIZE     = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE     = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE   = $808A;
  GL_EDGE_FLAG_ARRAY_STRIDE       = $808C;
  GL_VERTEX_ARRAY_POINTER         = $808E;
  GL_NORMAL_ARRAY_POINTER         = $808F;
  GL_COLOR_ARRAY_POINTER          = $8090;
  GL_INDEX_ARRAY_POINTER          = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER  = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER      = $8093;

procedure glGenTextures(n : GLsizei; textures : PGLuint); stdcall; external 'opengl32.dll';
procedure glDeleteTextures(n : GLsizei; textures : PGLuint); stdcall; external 'opengl32.dll';
procedure glBindTexture(target : GLenum; texture : GLuint); stdcall; external 'opengl32.dll';

procedure glPolygonOffset(factor, units : GLfloat); stdcall; external 'opengl32.dll';

procedure glEnableClientState(state : GLenum); stdcall; external 'opengl32.dll';
procedure glDisableClientState(state : GLenum); stdcall; external 'opengl32.dll';

procedure glArrayElement(i : GLint); stdcall; external 'opengl32.dll';
procedure glColorPointer(size : GLint; _type : GLenum; stride : GLsizei; ptr : Pointer); stdcall; external 'opengl32.dll';
procedure glDrawArrays(mode : GLenum; first : GLint; count : GLsizei); stdcall; external 'opengl32.dll';
procedure glDrawElements(mode : GLenum; count : GLsizei; _type : GLenum; indices : Pointer); stdcall; external 'opengl32.dll';
procedure glEdgeFlagPointer(stride : GLsizei; ptr : Pointer); stdcall; external 'opengl32.dll';
procedure glGetPointerv(mode : GLenum; result : PPointer); stdcall; external 'opengl32.dll';
procedure glIndexPointer(_type : GLenum; stride : GLsizei; ptr : Pointer); stdcall; external 'opengl32.dll';
procedure glNormalPointer(_type : GLenum; stride : GLsizei; ptr : Pointer); stdcall; external 'opengl32.dll';
procedure glTexCoordPointer(size : GLint; _type : GLenum; stride : GLsizei; ptr : Pointer); stdcall; external 'opengl32.dll';
procedure glVertexPointer(size : GLint; _type : GLenum; stride : GLsizei; ptr : Pointer); stdcall; external 'opengl32.dll';

// OpenGL 1.2
const
  GL_UNSIGNED_BYTE_3_3_2            = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4         = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1         = $8034;
  GL_UNSIGNED_INT_8_8_8_8           = $8035;
  GL_UNSIGNED_INT_10_10_10_2        = $8036;
  GL_TEXTURE_BINDING_3D             = $806A;
  GL_PACK_SKIP_IMAGES               = $806B;
  GL_PACK_IMAGE_HEIGHT              = $806C;
  GL_UNPACK_SKIP_IMAGES             = $806D;
  GL_UNPACK_IMAGE_HEIGHT            = $806E;
  GL_TEXTURE_3D                     = $806F;
  GL_PROXY_TEXTURE_3D               = $8070;
  GL_TEXTURE_DEPTH                  = $8071;
  GL_TEXTURE_WRAP_R                 = $8072;
  GL_MAX_3D_TEXTURE_SIZE            = $8073;
  GL_UNSIGNED_BYTE_2_3_3_REV        = $8362;
  GL_UNSIGNED_SHORT_5_6_5           = $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV       = $8364;
  GL_UNSIGNED_SHORT_4_4_4_4_REV     = $8365;
  GL_UNSIGNED_SHORT_1_5_5_5_REV     = $8366;
  GL_UNSIGNED_INT_8_8_8_8_REV       = $8367;
  GL_UNSIGNED_INT_2_10_10_10_REV    = $8368;
  GL_BGR                            = $80E0;
  GL_BGRA                           = $80E1;
  GL_MAX_ELEMENTS_VERTICES          = $80E8;
  GL_MAX_ELEMENTS_INDICES           = $80E9;
  GL_CLAMP_TO_EDGE                  = $812F;
  GL_TEXTURE_MIN_LOD                = $813A;
  GL_TEXTURE_MAX_LOD                = $813B;
  GL_TEXTURE_BASE_LEVEL             = $813C;
  GL_TEXTURE_MAX_LEVEL              = $813D;
  GL_SMOOTH_POINT_SIZE_RANGE        = $0B12;
  GL_SMOOTH_POINT_SIZE_GRANULARITY  = $0B13;
  GL_SMOOTH_LINE_WIDTH_RANGE        = $0B22;
  GL_SMOOTH_LINE_WIDTH_GRANULARITY  = $0B23;
  GL_ALIASED_LINE_WIDTH_RANGE       = $846E;

  GL_RESCALE_NORMAL                 = $803A;
  GL_LIGHT_MODEL_COLOR_CONTROL      = $81F8;
  GL_SINGLE_COLOR                   = $81F9;
  GL_SEPARATE_SPECULAR_COLOR        = $81FA;
  GL_ALIASED_POINT_SIZE_RANGE       = $846D;

// OpenGL 1.3
// TODO: Cube Maps
const
  GL_TEXTURE0                       = $84C0;
  GL_TEXTURE1                       = $84C1;
  GL_TEXTURE2                       = $84C2;
  GL_TEXTURE3                       = $84C3;
  GL_TEXTURE4                       = $84C4;
  GL_TEXTURE5                       = $84C5;
  GL_TEXTURE6                       = $84C6;
  GL_TEXTURE7                       = $84C7;
  GL_TEXTURE8                       = $84C8;
  GL_TEXTURE9                       = $84C9;
  GL_TEXTURE10                      = $84CA;
  GL_TEXTURE11                      = $84CB;
  GL_TEXTURE12                      = $84CC;
  GL_TEXTURE13                      = $84CD;
  GL_TEXTURE14                      = $84CE;
  GL_TEXTURE15                      = $84CF;
  GL_TEXTURE16                      = $84D0;
  GL_TEXTURE17                      = $84D1;
  GL_TEXTURE18                      = $84D2;
  GL_TEXTURE19                      = $84D3;
  GL_TEXTURE20                      = $84D4;
  GL_TEXTURE21                      = $84D5;
  GL_TEXTURE22                      = $84D6;
  GL_TEXTURE23                      = $84D7;
  GL_TEXTURE24                      = $84D8;
  GL_TEXTURE25                      = $84D9;
  GL_TEXTURE26                      = $84DA;
  GL_TEXTURE27                      = $84DB;
  GL_TEXTURE28                      = $84DC;
  GL_TEXTURE29                      = $84DD;
  GL_TEXTURE30                      = $84DE;
  GL_TEXTURE31                      = $84DF;
  GL_ACTIVE_TEXTURE                 = $84E0;
  GL_MULTISAMPLE                    = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE       = $809E;
  GL_SAMPLE_ALPHA_TO_ONE            = $809F;
  GL_SAMPLE_COVERAGE                = $80A0;
  GL_SAMPLE_BUFFERS                 = $80A8;
  GL_SAMPLES                        = $80A9;
  GL_SAMPLE_COVERAGE_VALUE          = $80AA;
  GL_SAMPLE_COVERAGE_INVERT         = $80AB;
  GL_NORMAL_MAP                     = $8511;
  GL_REFLECTION_MAP                 = $8512;
  GL_TEXTURE_CUBE_MAP               = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP       = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X    = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X    = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y    = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y    = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z    = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z    = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP         = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE      = $851C;
  GL_COMPRESSED_RGB                 = $84ED;
  GL_COMPRESSED_RGBA                = $84EE;
  GL_TEXTURE_COMPRESSION_HINT       = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE  = $86A0;
  GL_TEXTURE_COMPRESSED             = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS     = $86A3;
  GL_CLAMP_TO_BORDER                = $812D;
  
type
  PFNGLACTIVETEXTUREPROC        = procedure(texture : GLenum); stdcall;
  PFNGLCLIENTACTIVETEXTUREPROC  = procedure(texture : GLenum); stdcall;
  PFNGLCOMPRESSEDTEXIMAGE3DPROC = procedure(target : GLenum; level : GLint; internalformat : GLenum; width, height, depth : GLsizei; border : GLint; imagesize : GLsizei; data : Pointer); stdcall;
  PFNGLCOMPRESSEDTEXIMAGE2DPROC = procedure(target : GLenum; level : GLint; internalformat : GLenum; width, height : GLsizei; border : GLint; imagesize : GLsizei; data : Pointer); stdcall;
  PFNGLCOMPRESSEDTEXIMAGE1DPROC = procedure(target : GLenum; level : GLint; internalformat : GLenum; width : GLsizei; border : GLint; imagesize : GLsizei; data : Pointer); stdcall;
  PFNGLMULTITEXCOORD1DPROC      = procedure(target : GLenum; s : GLdouble);
  PFNGLMULTITEXCOORD1DVPROC     = procedure(target : GLenum; v : PGLdouble);
  PFNGLMULTITEXCOORD1FPROC      = procedure(target : GLenum; s : GLfloat);
  PFNGLMULTITEXCOORD1FVPROC     = procedure(target : GLenum; v : PGLfloat);
  PFNGLMULTITEXCOORD1IPROC      = procedure(target : GLenum; s : GLint);
  PFNGLMULTITEXCOORD1IVPROC     = procedure(target : GLenum; v : PGLint);
  PFNGLMULTITEXCOORD1SPROC      = procedure(target : GLenum; s : GLshort);
  PFNGLMULTITEXCOORD1SVPROC     = procedure(target : GLenum; v : PGLshort);
  PFNGLMULTITEXCOORD2DPROC      = procedure(target : GLenum; s, t : GLdouble);
  PFNGLMULTITEXCOORD2DVPROC     = procedure(target : GLenum; v : PGLdouble);
  PFNGLMULTITEXCOORD2FPROC      = procedure(target : GLenum; s, t : GLfloat);
  PFNGLMULTITEXCOORD2FVPROC     = procedure(target : GLenum; v : PGLfloat);
  PFNGLMULTITEXCOORD2IPROC      = procedure(target : GLenum; s, t : GLint);
  PFNGLMULTITEXCOORD2IVPROC     = procedure(target : GLenum; v : PGLint);
  PFNGLMULTITEXCOORD2SPROC      = procedure(target : GLenum; s, t : GLshort);
  PFNGLMULTITEXCOORD2SVPROC     = procedure(target : GLenum; v : PGLshort);
  PFNGLMULTITEXCOORD3DPROC      = procedure(target : GLenum; s, t, r : GLdouble);
  PFNGLMULTITEXCOORD3DVPROC     = procedure(target : GLenum; v : PGLdouble);
  PFNGLMULTITEXCOORD3FPROC      = procedure(target : GLenum; s, t, r : GLfloat);
  PFNGLMULTITEXCOORD3FVPROC     = procedure(target : GLenum; v : PGLfloat);
  PFNGLMULTITEXCOORD3IPROC      = procedure(target : GLenum; s, t, r : GLint);
  PFNGLMULTITEXCOORD3IVPROC     = procedure(target : GLenum; v : PGLint);
  PFNGLMULTITEXCOORD3SPROC      = procedure(target : GLenum; s, t, r : GLshort);
  PFNGLMULTITEXCOORD3SVPROC     = procedure(target : GLenum; v : PGLshort);
  PFNGLMULTITEXCOORD4DPROC      = procedure(target : GLenum; s, t, r, q : GLdouble);
  PFNGLMULTITEXCOORD4DVPROC     = procedure(target : GLenum; v : PGLdouble);
  PFNGLMULTITEXCOORD4FPROC      = procedure(target : GLenum; s, t, r, q : GLfloat);
  PFNGLMULTITEXCOORD4FVPROC     = procedure(target : GLenum; v : PGLfloat);
  PFNGLMULTITEXCOORD4IPROC      = procedure(target : GLenum; s, t, r, q : GLint);
  PFNGLMULTITEXCOORD4IVPROC     = procedure(target : GLenum; v : PGLint);
  PFNGLMULTITEXCOORD4SPROC      = procedure(target : GLenum; s, t, r, q : GLshort);
  PFNGLMULTITEXCOORD4SVPROC     = procedure(target : GLenum; v : PGLshort);

// OpenGL 1.5
const
  GL_BUFFER_SIZE                    = $8764;
  GL_BUFFER_USAGE                   = $8765;
  GL_QUERY_COUNTER_BITS             = $8864;
  GL_CURRENT_QUERY                  = $8865;
  GL_QUERY_RESULT                   = $8866;
  GL_QUERY_RESULT_AVAILABLE         = $8867;
  GL_ARRAY_BUFFER                   = $8892;
  GL_ELEMENT_ARRAY_BUFFER           = $8893;
  GL_ARRAY_BUFFER_BINDING           = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING   = $8895;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
  GL_READ_ONLY                      = $88B8;
  GL_WRITE_ONLY                     = $88B9;
  GL_READ_WRITE                     = $88BA;
  GL_BUFFER_ACCESS                  = $88BB;
  GL_BUFFER_MAPPED                  = $88BC;
  GL_BUFFER_MAP_POINTER             = $88BD;
  GL_STREAM_DRAW                    = $88E0;
  GL_STREAM_READ                    = $88E1;
  GL_STREAM_COPY                    = $88E2;
  GL_STATIC_DRAW                    = $88E4;
  GL_STATIC_READ                    = $88E5;
  GL_STATIC_COPY                    = $88E6;
  GL_DYNAMIC_DRAW                   = $88E8;
  GL_DYNAMIC_READ                   = $88E9;
  GL_DYNAMIC_COPY                   = $88EA;
  GL_SAMPLES_PASSED                 = $8914;

  GL_VERTEX_ARRAY_BUFFER_BINDING    = $8896;
  GL_NORMAL_ARRAY_BUFFER_BINDING    = $8897;
  GL_COLOR_ARRAY_BUFFER_BINDING     = $8898;
  GL_INDEX_ARRAY_BUFFER_BINDING     = $8899;
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING = $889A;
  GL_EDGE_FLAG_ARRAY_BUFFER_BINDING = $889B;
  GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING = $889C;
  GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING = $889D;
  GL_WEIGHT_ARRAY_BUFFER_BINDING    = $889E;
  GL_FOG_COORD_SRC                  = $8450;
  GL_FOG_COORD                      = $8451;
  GL_CURRENT_FOG_COORD              = $8453;
  GL_FOG_COORD_ARRAY_TYPE           = $8454;
  GL_FOG_COORD_ARRAY_STRIDE         = $8455;
  GL_FOG_COORD_ARRAY_POINTER        = $8456;
  GL_FOG_COORD_ARRAY                = $8457;
  GL_FOG_COORD_ARRAY_BUFFER_BINDING = $889D;
  GL_SRC0_RGB                       = $8580;
  GL_SRC1_RGB                       = $8581;
  GL_SRC2_RGB                       = $8582;
  GL_SRC0_ALPHA                     = $8588;
  GL_SRC1_ALPHA                     = $8589;
  GL_SRC2_ALPHA                     = $858A;

type
  PFNGLGENQUERIESPROC               = procedure(n : GLsizei; ids : PGLuint); stdcall;
  PFNGLDELETEQUERIESPROC            = procedure(n : GLsizei; ids : PGLuint); stdcall;
  PFNGLISQUERYPROC                  = function(id : GLuint) : GLboolean; stdcall;
  PFNGLBEGINQUERYPROC               = procedure(target : GLenum; id : GLuint); stdcall;
  PFNGLENDQUERYPROC                 = procedure(target : GLenum); stdcall;
  PFNGLGETQUERYIVPROC               = procedure(target : GLenum; pname : GLenum; params : PGLint); stdcall;
  PFNGLGETQUERYOBJECTIVPROC         = procedure(id : GLuint; pname : GLenum; params : PGLint); stdcall;
  PFNGLGETQUERYOBJECTUIVPROC        = procedure(id : GLuint; pname : GLenum; params : PGLuint); stdcall;
  PFNGLBINDBUFFERPROC               = procedure(target : GLenum; buffer : GLuint); stdcall;
  PFNGLDELETEBUFFERSPROC            = procedure(n : GLsizei; buffers : PGLuint); stdcall;
  PFNGLGENBUFFERSPROC               = procedure(n : GLsizei; buffers : PGLuint); stdcall;
  PFNGLISBUFFERPROC                 = function(buffer : GLuint) : GLboolean; stdcall;
  PFNGLBUFFERDATAPROC               = procedure(target : GLenum; size : GLsizei; data : Pointer; usage : GLenum); stdcall;
  PFNGLBUFFERSUBDATAPROC            = procedure(target : GLenum; offset : GLsizei; size : GLsizei; data : Pointer); stdcall;
  PFNGLGETBUFFERSUBDATAPROC         = procedure(target : GLenum; offset : GLsizei; size : GLsizei; data : Pointer); stdcall;
  PFNGLMAPBUFFERPROC                = function(target : GLenum; access : GLenum) : Pointer; stdcall;
  PFNGLUNMAPBUFFERPROC              = function(target : GLenum) : GLboolean; stdcall;
  PFNGLGETBUFFERPARAMETERIVPROC     = procedure(target : GLenum; pname : GLenum; params : PGLint); stdcall;
  PFNGLGETBUFFERPOINTERVPROC        = procedure(target : GLenum; pname : GLenum; params : Pointer); stdcall;

// GL_EXT_texture_compression_s3tc
const
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT            = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT           = $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT           = $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT           = $83F3;
  
// GL_EXT_texture_filter_anisotropic
const
  GL_TEXTURE_MAX_ANISOTROPY_EXT              = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT          = $84FF;

// GL_ARB_texture_compression_bptc
const
  GL_COMPRESSED_RGBA_BPTC_UNORM_ARB          = $8E8C;
  GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM_ARB    = $8E8D;
  GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT_ARB    = $8E8E;
  GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_ARB  = $8E8F;

// GL_ARB_vertex_program
const
  GL_COLOR_SUM_ARB                           = $8458;
  GL_VERTEX_PROGRAM_ARB                      = $8620;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB         = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB            = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB          = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB            = $8625;
  GL_CURRENT_VERTEX_ATTRIB_ARB               = $8626;
  GL_PROGRAM_LENGTH_ARB                      = $8627;
  GL_PROGRAM_STRING_ARB                      = $8628;
  GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB      = $862E;
  GL_MAX_PROGRAM_MATRICES_ARB                = $862F;
  GL_CURRENT_MATRIX_STACK_DEPTH_ARB          = $8640;
  GL_CURRENT_MATRIX_ARB                      = $8641;
  GL_VERTEX_PROGRAM_POINT_SIZE_ARB           = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE_ARB             = $8643;
  GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB         = $8645;
  GL_PROGRAM_ERROR_POSITION_ARB              = $864B;
  GL_PROGRAM_BINDING_ARB                     = $8677;
  GL_MAX_VERTEX_ATTRIBS_ARB                  = $8869;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB      = $886A;
  GL_PROGRAM_ERROR_STRING_ARB                = $8874;
  GL_PROGRAM_FORMAT_ASCII_ARB                = $8875;
  GL_PROGRAM_FORMAT_ARB                      = $8876;
  GL_PROGRAM_INSTRUCTIONS_ARB                = $88A0;
  GL_MAX_PROGRAM_INSTRUCTIONS_ARB            = $88A1;
  GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB         = $88A2;
  GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB     = $88A3;
  GL_PROGRAM_TEMPORARIES_ARB                 = $88A4;
  GL_MAX_PROGRAM_TEMPORARIES_ARB             = $88A5;
  GL_PROGRAM_NATIVE_TEMPORARIES_ARB          = $88A6;
  GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB      = $88A7;
  GL_PROGRAM_PARAMETERS_ARB                  = $88A8;
  GL_MAX_PROGRAM_PARAMETERS_ARB              = $88A9;
  GL_PROGRAM_NATIVE_PARAMETERS_ARB           = $88AA;
  GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB       = $88AB;
  GL_PROGRAM_ATTRIBS_ARB                     = $88AC;
  GL_MAX_PROGRAM_ATTRIBS_ARB                 = $88AD;
  GL_PROGRAM_NATIVE_ATTRIBS_ARB              = $88AE;
  GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB          = $88AF;
  GL_PROGRAM_ADDRESS_REGISTERS_ARB           = $88B0;
  GL_MAX_PROGRAM_ADDRESS_REGISTERS_ARB       = $88B1;
  GL_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB    = $88B2;
  GL_MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB = $88B3;
  GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB        = $88B4;
  GL_MAX_PROGRAM_ENV_PARAMETERS_ARB          = $88B5;
  GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB         = $88B6;
  GL_TRANSPOSE_CURRENT_MATRIX_ARB            = $88B7;
  GL_MATRIX0_ARB                             = $88C0;
  GL_MATRIX1_ARB                             = $88C1;
  GL_MATRIX2_ARB                             = $88C2;
  GL_MATRIX3_ARB                             = $88C3;
  GL_MATRIX4_ARB                             = $88C4;
  GL_MATRIX5_ARB                             = $88C5;
  GL_MATRIX6_ARB                             = $88C6;
  GL_MATRIX7_ARB                             = $88C7;
  GL_MATRIX8_ARB                             = $88C8;
  GL_MATRIX9_ARB                             = $88C9;
  GL_MATRIX10_ARB                            = $88CA;
  GL_MATRIX11_ARB                            = $88CB;
  GL_MATRIX12_ARB                            = $88CC;
  GL_MATRIX13_ARB                            = $88CD;
  GL_MATRIX14_ARB                            = $88CE;
  GL_MATRIX15_ARB                            = $88CF;
  GL_MATRIX16_ARB                            = $88D0;
  GL_MATRIX17_ARB                            = $88D1;
  GL_MATRIX18_ARB                            = $88D2;
  GL_MATRIX19_ARB                            = $88D3;
  GL_MATRIX20_ARB                            = $88D4;
  GL_MATRIX21_ARB                            = $88D5;
  GL_MATRIX22_ARB                            = $88D6;
  GL_MATRIX23_ARB                            = $88D7;
  GL_MATRIX24_ARB                            = $88D8;
  GL_MATRIX25_ARB                            = $88D9;
  GL_MATRIX26_ARB                            = $88DA;
  GL_MATRIX27_ARB                            = $88DB;
  GL_MATRIX28_ARB                            = $88DC;
  GL_MATRIX29_ARB                            = $88DD;
  GL_MATRIX30_ARB                            = $88DE;
  GL_MATRIX31_ARB                            = $88DF;

type
PFNGLVERTEXATTRIB1DARBPROC                   = procedure (index : GLuint; x : GLdouble); stdcall;
PFNGLVERTEXATTRIB1DVARBPROC                  = procedure (index : GLuint; v : PGLdouble); stdcall;
PFNGLVERTEXATTRIB1FARBPROC                   = procedure (index : GLuint; x : GLfloat); stdcall;
PFNGLVERTEXATTRIB1FVARBPROC                  = procedure (index : GLuint; v : PGLfloat); stdcall;
PFNGLVERTEXATTRIB1SARBPROC                   = procedure (index : GLuint; x : GLshort); stdcall;
PFNGLVERTEXATTRIB1SVARBPROC                  = procedure (index : GLuint; v : PGLshort); stdcall;
PFNGLVERTEXATTRIB2DARBPROC                   = procedure (index : GLuint; x, y : GLdouble); stdcall;
PFNGLVERTEXATTRIB2DVARBPROC                  = procedure (index : GLuint; v : PGLdouble); stdcall;
PFNGLVERTEXATTRIB2FARBPROC                   = procedure (index : GLuint; x, y : GLfloat); stdcall;
PFNGLVERTEXATTRIB2FVARBPROC                  = procedure (index : GLuint; v : PGLfloat); stdcall;
PFNGLVERTEXATTRIB2SARBPROC                   = procedure (index : GLuint; x, y : GLshort); stdcall;
PFNGLVERTEXATTRIB2SVARBPROC                  = procedure (index : GLuint; v : PGLshort); stdcall;
PFNGLVERTEXATTRIB3DARBPROC                   = procedure (index : GLuint; x, y, z : GLdouble); stdcall;
PFNGLVERTEXATTRIB3DVARBPROC                  = procedure (index : GLuint; v : PGLdouble); stdcall;
PFNGLVERTEXATTRIB3FARBPROC                   = procedure (index : GLuint; x, y, z : GLfloat); stdcall;
PFNGLVERTEXATTRIB3FVARBPROC                  = procedure (index : GLuint; v : PGLfloat); stdcall;
PFNGLVERTEXATTRIB3SARBPROC                   = procedure (index : GLuint; x, y, z : GLshort); stdcall;
PFNGLVERTEXATTRIB3SVARBPROC                  = procedure (index : GLuint; v : PGLshort); stdcall;
PFNGLVERTEXATTRIB4NBVARBPROC                 = procedure (index : GLuint; v : PGLbyte); stdcall;
PFNGLVERTEXATTRIB4NIVARBPROC                 = procedure (index : GLuint; v : PGLint); stdcall;
PFNGLVERTEXATTRIB4NSVARBPROC                 = procedure (index : GLuint; v : PGLshort); stdcall;
PFNGLVERTEXATTRIB4NUBARBPROC                 = procedure (index : GLuint; x, y, z, w : GLubyte); stdcall;
PFNGLVERTEXATTRIB4NUBVARBPROC                = procedure (index : GLuint; v : PGLubyte); stdcall;
PFNGLVERTEXATTRIB4NUIVARBPROC                = procedure (index : GLuint; v : PGLuint); stdcall;
PFNGLVERTEXATTRIB4NUSVARBPROC                = procedure (index : GLuint; v : PGLushort); stdcall;
PFNGLVERTEXATTRIB4BVARBPROC                  = procedure (index : GLuint; v : PGLbyte); stdcall;
PFNGLVERTEXATTRIB4DARBPROC                   = procedure (index : GLuint; x, y, z, w : GLdouble); stdcall;
PFNGLVERTEXATTRIB4DVARBPROC                  = procedure (index : GLuint; v : PGLdouble); stdcall;
PFNGLVERTEXATTRIB4FARBPROC                   = procedure (index : GLuint; x, y, z, w : GLfloat); stdcall;
PFNGLVERTEXATTRIB4FVARBPROC                  = procedure (index : GLuint; v : PGLfloat); stdcall;
PFNGLVERTEXATTRIB4IVARBPROC                  = procedure (index : GLuint; v : PGLint); stdcall;
PFNGLVERTEXATTRIB4SARBPROC                   = procedure (index : GLuint; x, y, z, w : GLshort); stdcall;
PFNGLVERTEXATTRIB4SVARBPROC                  = procedure (index : GLuint; v : PGLshort); stdcall;
PFNGLVERTEXATTRIB4UBVARBPROC                 = procedure (index : GLuint; const v : PGLubyte); stdcall;
PFNGLVERTEXATTRIB4UIVARBPROC                 = procedure (index : GLuint; const v : PGLuint); stdcall;
PFNGLVERTEXATTRIB4USVARBPROC                 = procedure (index : GLuint; const v : PGLushort); stdcall;
PFNGLVERTEXATTRIBPOINTERARBPROC              = procedure (index : GLuint; size : GLint; type_ : GLenum; normalized : GLboolean; stride : GLsizei; ptr : Pointer); stdcall;
PFNGLENABLEVERTEXATTRIBARRAYARBPROC          = procedure (index : GLuint); stdcall;
PFNGLDISABLEVERTEXATTRIBARRAYARBPROC         = procedure (index : GLuint); stdcall;
PFNGLPROGRAMSTRINGARBPROC                    = procedure (target : GLenum; format : GLenum; len : GLsizei; str : Pointer); stdcall;
PFNGLBINDPROGRAMARBPROC                      = procedure (target : GLenum; program_ : GLuint); stdcall;
PFNGLDELETEPROGRAMSARBPROC                   = procedure (n : GLsizei; programs : PGLuint); stdcall;
PFNGLGENPROGRAMSARBPROC                      = procedure (n : GLsizei; programs : PGLuint); stdcall;
PFNGLPROGRAMENVPARAMETER4DARBPROC            = procedure (target : GLenum; index : GLuint; x, y, z, w : GLdouble); stdcall;
PFNGLPROGRAMENVPARAMETER4DVARBPROC           = procedure (target : GLenum; index : GLuint; params : PGLdouble); stdcall;
PFNGLPROGRAMENVPARAMETER4FARBPROC            = procedure (target : GLenum; index : GLuint; x, y, z, w : GLfloat); stdcall;
PFNGLPROGRAMENVPARAMETER4FVARBPROC           = procedure (target : GLenum; index : GLuint; params : PGLfloat); stdcall;
PFNGLPROGRAMLOCALPARAMETER4DARBPROC          = procedure (target : GLenum; index : GLuint; x, y, z, w : GLdouble); stdcall;
PFNGLPROGRAMLOCALPARAMETER4DVARBPROC         = procedure (target : GLenum; index : GLuint; params : PGLdouble); stdcall;
PFNGLPROGRAMLOCALPARAMETER4FARBPROC          = procedure (target : GLenum; index : GLuint; x, y, z, w : GLfloat); stdcall;
PFNGLPROGRAMLOCALPARAMETER4FVARBPROC         = procedure (target : GLenum; index : GLuint; params : PGLfloat); stdcall;
PFNGLGETPROGRAMENVPARAMETERDVARBPROC         = procedure (target : GLenum; index : GLuint; params : PGLdouble); stdcall;
PFNGLGETPROGRAMENVPARAMETERFVARBPROC         = procedure (target : GLenum; index : GLuint; params : PGLfloat); stdcall;
PFNGLGETPROGRAMLOCALPARAMETERDVARBPROC       = procedure (target : GLenum; index : GLuint; params : PGLdouble); stdcall;
PFNGLGETPROGRAMLOCALPARAMETERFVARBPROC       = procedure (target : GLenum; index : GLuint; params : PGLfloat); stdcall;
PFNGLGETPROGRAMIVARBPROC                     = procedure (target : GLenum; pname : GLenum; params : PGLint); stdcall;
PFNGLGETPROGRAMSTRINGARBPROC                 = procedure (target : GLenum; pname : GLenum; str : Pointer); stdcall;
PFNGLGETVERTEXATTRIBDVARBPROC                = procedure (index : GLuint; pname : GLenum; params : PGLdouble); stdcall;
PFNGLGETVERTEXATTRIBFVARBPROC                = procedure (index : GLuint; pname : GLenum; params : PGLfloat); stdcall;
PFNGLGETVERTEXATTRIBIVARBPROC                = procedure (index : GLuint; pname : GLenum; params : PGLint); stdcall;
PFNGLGETVERTEXATTRIBPOINTERVARBPROC          = procedure (index : GLuint; pname : GLenum; ptr : Pointer); stdcall;
PFNGLISPROGRAMARBPROC                        = function (program_ : GLuint) : GLboolean; stdcall;

// GL_ARB_fragment_program
const
  GL_FRAGMENT_PROGRAM_ARB                    = $8804;
  GL_PROGRAM_ALU_INSTRUCTIONS_ARB            = $8805;
  GL_PROGRAM_TEX_INSTRUCTIONS_ARB            = $8806;
  GL_PROGRAM_TEX_INDIRECTIONS_ARB            = $8807;
  GL_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB     = $8808;
  GL_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB     = $8809;
  GL_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB     = $880A;
  GL_MAX_PROGRAM_ALU_INSTRUCTIONS_ARB        = $880B;
  GL_MAX_PROGRAM_TEX_INSTRUCTIONS_ARB        = $880C;
  GL_MAX_PROGRAM_TEX_INDIRECTIONS_ARB        = $880D;
  GL_MAX_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB = $880E;
  GL_MAX_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB = $880F;
  GL_MAX_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB = $8810;
  GL_MAX_TEXTURE_COORDS_ARB                  = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS_ARB             = $8872;
  
// GL_ARB_draw_buffers
const
  GL_MAX_DRAW_BUFFERS_ARB                    = $8824;
  GL_DRAW_BUFFER0_ARB                        = $8825;
  GL_DRAW_BUFFER1_ARB                        = $8826;
  GL_DRAW_BUFFER2_ARB                        = $8827;
  GL_DRAW_BUFFER3_ARB                        = $8828;
  GL_DRAW_BUFFER4_ARB                        = $8829;
  GL_DRAW_BUFFER5_ARB                        = $882A;
  GL_DRAW_BUFFER6_ARB                        = $882B;
  GL_DRAW_BUFFER7_ARB                        = $882C;
  GL_DRAW_BUFFER8_ARB                        = $882D;
  GL_DRAW_BUFFER9_ARB                        = $882E;
  GL_DRAW_BUFFER10_ARB                       = $882F;
  GL_DRAW_BUFFER11_ARB                       = $8830;
  GL_DRAW_BUFFER12_ARB                       = $8831;
  GL_DRAW_BUFFER13_ARB                       = $8832;
  GL_DRAW_BUFFER14_ARB                       = $8833;
  GL_DRAW_BUFFER15_ARB                       = $8834;
  
type
  PFNGLDRAWBUFFERSARBPROC                    = procedure (n : GLsizei; bufs : PGLenum); stdcall;
  
// GL_ARB_draw_elements_base_vertex
type
  PFNGLDRAWELEMENTSBASEVERTEXPROC            = procedure(mode : GLenum; count : GLsizei; _type : GLenum; indices : Pointer; basevertex : GLint); stdcall;
  PFNGLDRAWRANGEELEMENTSBASEVERTEXPROC       = procedure(mode : GLenum; start, _end : GLuint; count : GLsizei; _type : GLenum; indices : Pointer; basevertex : GLint); stdcall;
  PFNGLDRAWELEMENTSINSTANCEDBASEVERTEXPROC   = procedure(mode : GLenum; count : GLsizei; _type : GLenum; indices : Pointer; primcount : GLsizei; basevertex : GLint); stdcall;
  PFNGLMULTIDRAWELEMENTSBASEVERTEXPROC       = procedure(mode : GLenum; pcount : PGLsizei; _type : GLenum; indices : PPointer; primcount : GLsizei; pbasevertex : PGLint); stdcall;

// GL_ARB_instanced_arrays
const
  GL_VERTEX_ATTRIB_ARRAY_DIVISOR_ARB         = $88FE;

type
  PFNGLVERTEXATTRIBDIVISORARBPROC            = procedure(index : GLuint; divisor : GLuint); stdcall;

///////////////////
var
  glActiveTexture         : PFNGLACTIVETEXTUREPROC;
  glClientActiveTexture   : PFNGLCLIENTACTIVETEXTUREPROC;
  glCompressedTexImage3D  : PFNGLCOMPRESSEDTEXIMAGE3DPROC;
  glCompressedTexImage2D  : PFNGLCOMPRESSEDTEXIMAGE2DPROC;
  glCompressedTexImage1D  : PFNGLCOMPRESSEDTEXIMAGE1DPROC;
  glMultiTexCoord1d       : PFNGLMULTITEXCOORD1DPROC;
  glMultiTexCoord1dv      : PFNGLMULTITEXCOORD1DVPROC;
  glMultiTexCoord1f       : PFNGLMULTITEXCOORD1FPROC;
  glMultiTexCoord1fv      : PFNGLMULTITEXCOORD1FVPROC;
  glMultiTexCoord1i       : PFNGLMULTITEXCOORD1IPROC;
  glMultiTexCoord1iv      : PFNGLMULTITEXCOORD1IVPROC;
  glMultiTexCoord1s       : PFNGLMULTITEXCOORD1SPROC;
  glMultiTexCoord1sv      : PFNGLMULTITEXCOORD1SVPROC;
  glMultiTexCoord2d       : PFNGLMULTITEXCOORD2DPROC;
  glMultiTexCoord2dv      : PFNGLMULTITEXCOORD2DVPROC;
  glMultiTexCoord2f       : PFNGLMULTITEXCOORD2FPROC;
  glMultiTexCoord2fv      : PFNGLMULTITEXCOORD2FVPROC;
  glMultiTexCoord2i       : PFNGLMULTITEXCOORD2IPROC;
  glMultiTexCoord2iv      : PFNGLMULTITEXCOORD2IVPROC;
  glMultiTexCoord2s       : PFNGLMULTITEXCOORD2SPROC;
  glMultiTexCoord2sv      : PFNGLMULTITEXCOORD2SVPROC;
  glMultiTexCoord3d       : PFNGLMULTITEXCOORD3DPROC;
  glMultiTexCoord3dv      : PFNGLMULTITEXCOORD3DVPROC;
  glMultiTexCoord3f       : PFNGLMULTITEXCOORD3FPROC;
  glMultiTexCoord3fv      : PFNGLMULTITEXCOORD3FVPROC;
  glMultiTexCoord3i       : PFNGLMULTITEXCOORD3IPROC;
  glMultiTexCoord3iv      : PFNGLMULTITEXCOORD3IVPROC;
  glMultiTexCoord3s       : PFNGLMULTITEXCOORD3SPROC;
  glMultiTexCoord3sv      : PFNGLMULTITEXCOORD3SVPROC;
  glMultiTexCoord4d       : PFNGLMULTITEXCOORD4DPROC;
  glMultiTexCoord4dv      : PFNGLMULTITEXCOORD4DVPROC;
  glMultiTexCoord4f       : PFNGLMULTITEXCOORD4FPROC;
  glMultiTexCoord4fv      : PFNGLMULTITEXCOORD4FVPROC;
  glMultiTexCoord4i       : PFNGLMULTITEXCOORD4IPROC;
  glMultiTexCoord4iv      : PFNGLMULTITEXCOORD4IVPROC;
  glMultiTexCoord4s       : PFNGLMULTITEXCOORD4SPROC;
  glMultiTexCoord4sv      : PFNGLMULTITEXCOORD4SVPROC;
  
  glGenQueries           : PFNGLGENQUERIESPROC;
  glDeleteQueries        : PFNGLDELETEQUERIESPROC;
  glIsQuery              : PFNGLISQUERYPROC;
  glBeginQuery           : PFNGLBEGINQUERYPROC;
  glEndQuery             : PFNGLENDQUERYPROC;
  glGetQueryiv           : PFNGLGETQUERYIVPROC;
  glGetQueryObjectiv     : PFNGLGETQUERYOBJECTIVPROC;
  glGetQueryObjectuiv    : PFNGLGETQUERYOBJECTUIVPROC;
  glBindBuffer           : PFNGLBINDBUFFERPROC;
  glDeleteBuffers        : PFNGLDELETEBUFFERSPROC;
  glGenBuffers           : PFNGLGENBUFFERSPROC;
  glIsBuffer             : PFNGLISBUFFERPROC;
  glBufferData           : PFNGLBUFFERDATAPROC;
  glBufferSubData        : PFNGLBUFFERSUBDATAPROC;
  glGetBufferSubData     : PFNGLGETBUFFERSUBDATAPROC;
  glMapBuffer            : PFNGLMAPBUFFERPROC;
  glUnmapBuffer          : PFNGLUNMAPBUFFERPROC;
  glGetBufferParameteriv : PFNGLGETBUFFERPARAMETERIVPROC;
  glGetBufferPointerv    : PFNGLGETBUFFERPOINTERVPROC;

  glVertexAttrib1dARB            : PFNGLVERTEXATTRIB1DARBPROC;
  glVertexAttrib1dvARB           : PFNGLVERTEXATTRIB1DVARBPROC;
  glVertexAttrib1fARB            : PFNGLVERTEXATTRIB1FARBPROC;
  glVertexAttrib1fvARB           : PFNGLVERTEXATTRIB1FVARBPROC;
  glVertexAttrib1sARB            : PFNGLVERTEXATTRIB1SARBPROC;
  glVertexAttrib1svARB           : PFNGLVERTEXATTRIB1SVARBPROC;
  glVertexAttrib2dARB            : PFNGLVERTEXATTRIB2DARBPROC;
  glVertexAttrib2dvARB           : PFNGLVERTEXATTRIB2DVARBPROC;
  glVertexAttrib2fARB            : PFNGLVERTEXATTRIB2FARBPROC;
  glVertexAttrib2fvARB           : PFNGLVERTEXATTRIB2FVARBPROC;
  glVertexAttrib2sARB            : PFNGLVERTEXATTRIB2SARBPROC;
  glVertexAttrib2svARB           : PFNGLVERTEXATTRIB2SVARBPROC;
  glVertexAttrib3dARB            : PFNGLVERTEXATTRIB3DARBPROC;
  glVertexAttrib3dvARB           : PFNGLVERTEXATTRIB3DVARBPROC;
  glVertexAttrib3fARB            : PFNGLVERTEXATTRIB3FARBPROC;
  glVertexAttrib3fvARB           : PFNGLVERTEXATTRIB3FVARBPROC;
  glVertexAttrib3sARB            : PFNGLVERTEXATTRIB3SARBPROC;
  glVertexAttrib3svARB           : PFNGLVERTEXATTRIB3SVARBPROC;
  glVertexAttrib4NbvARB          : PFNGLVERTEXATTRIB4NBVARBPROC;
  glVertexAttrib4NivARB          : PFNGLVERTEXATTRIB4NIVARBPROC;
  glVertexAttrib4NsvARB          : PFNGLVERTEXATTRIB4NSVARBPROC;
  glVertexAttrib4NubARB          : PFNGLVERTEXATTRIB4NUBARBPROC;
  glVertexAttrib4NubvARB         : PFNGLVERTEXATTRIB4NUBVARBPROC;
  glVertexAttrib4NuivARB         : PFNGLVERTEXATTRIB4NUIVARBPROC;
  glVertexAttrib4NusvARB         : PFNGLVERTEXATTRIB4NUSVARBPROC; 
  glVertexAttrib4bvARB           : PFNGLVERTEXATTRIB4BVARBPROC;
  glVertexAttrib4dARB            : PFNGLVERTEXATTRIB4DARBPROC;
  glVertexAttrib4dvARB           : PFNGLVERTEXATTRIB4DVARBPROC;
  glVertexAttrib4fARB            : PFNGLVERTEXATTRIB4FARBPROC;
  glVertexAttrib4fvARB           : PFNGLVERTEXATTRIB4FVARBPROC;
  glVertexAttrib4ivARB           : PFNGLVERTEXATTRIB4IVARBPROC;
  glVertexAttrib4sARB            : PFNGLVERTEXATTRIB4SARBPROC;
  glVertexAttrib4svARB           : PFNGLVERTEXATTRIB4SVARBPROC;
  glVertexAttrib4ubvARB          : PFNGLVERTEXATTRIB4UBVARBPROC;
  glVertexAttrib4uivARB          : PFNGLVERTEXATTRIB4UIVARBPROC;
  glVertexAttrib4usvARB          : PFNGLVERTEXATTRIB4USVARBPROC;
  glVertexAttribPointerARB       : PFNGLVERTEXATTRIBPOINTERARBPROC;
  glEnableVertexAttribArrayARB   : PFNGLENABLEVERTEXATTRIBARRAYARBPROC;
  glDisableVertexAttribArrayARB  : PFNGLDisableVERTEXATTRIBARRAYARBPROC;
  glProgramStringARB             : PFNGLPROGRAMSTRINGARBPROC;
  glBindProgramARB               : PFNGLBINDPROGRAMARBPROC;
  glDeleteProgramsARB            : PFNGLDELETEPROGRAMSARBPROC;
  glGenProgramsARB               : PFNGLGENPROGRAMSARBPROC;
  glProgramEnvParameter4dARB     : PFNGLPROGRAMENVPARAMETER4DARBPROC;
  glProgramEnvParameter4dvARB    : PFNGLPROGRAMENVPARAMETER4DVARBPROC;  
  glProgramEnvParameter4fARB     : PFNGLPROGRAMENVPARAMETER4FARBPROC;
  glProgramEnvParameter4fvARB    : PFNGLPROGRAMENVPARAMETER4FVARBPROC;
  glProgramLocalParameter4dARB   : PFNGLPROGRAMLOCALPARAMETER4DARBPROC;
  glProgramLocalParameter4dvARB  : PFNGLPROGRAMLOCALPARAMETER4DVARBPROC; 
  glProgramLocalParameter4fARB   : PFNGLPROGRAMLOCALPARAMETER4FARBPROC;
  glProgramLocalParameter4fvARB  : PFNGLPROGRAMLOCALPARAMETER4FVARBPROC;
  glGetProgramEnvParameterdvARB  : PFNGLGETPROGRAMENVPARAMETERDVARBPROC;
  glGetProgramEnvParameterfvARB  : PFNGLGETPROGRAMENVPARAMETERFVARBPROC;
  glGetProgramLocalParameterdvARB : PFNGLGETPROGRAMLOCALPARAMETERDVARBPROC;
  glGetProgramLocalParameterfvARB : PFNGLGETPROGRAMLOCALPARAMETERFVARBPROC;
  glGetProgramivARB              : PFNGLGETPROGRAMIVARBPROC;
  glGetProgramStringARB          : PFNGLGETPROGRAMSTRINGARBPROC;
  glGetVertexAttribdvARB         : PFNGLGETVERTEXATTRIBDVARBPROC;
  glGetVertexAttribfvARB         : PFNGLGETVERTEXATTRIBFVARBPROC;
  glGetVertexAttribivARB         : PFNGLGETVERTEXATTRIBIVARBPROC;
  glGetVertexAttribPointervARB   : PFNGLGETVERTEXATTRIBPOINTERVARBPROC;
  glIsProgramARB                 : PFNGLISPROGRAMARBPROC;
  
  glDrawBuffersARB               : PFNGLDRAWBUFFERSARBPROC;
  
  glDrawElementsBaseVertex          : PFNGLDRAWELEMENTSBASEVERTEXPROC;
  glDrawRangeElementsBaseVertex     : PFNGLDRAWRANGEELEMENTSBASEVERTEXPROC;
  glDrawElementsInstancedBaseVertex : PFNGLDRAWELEMENTSINSTANCEDBASEVERTEXPROC;
  glMultiDrawElementsBaseVertex     : PFNGLMULTIDRAWELEMENTSBASEVERTEXPROC;
  
  glVertexAttribDivisorARB          : PFNGLVERTEXATTRIBDIVISORARBPROC;
  
procedure InitializeGLExtensions;

implementation
uses Windows;

function wglGetProcAddress(name : PAnsiChar) : Pointer;
begin
  Result := Windows.wglGetProcAddress(name);
  if Result = nil then WriteLn('!! ', name, ' = nil');
end;

procedure InitializeGLExtensions;
begin
  // OpenGL 1.3
  glActiveTexture        := wglGetProcAddress('glActiveTexture');
  glClientActiveTexture  := wglGetProcAddress('glClientActiveTexture');
  glCompressedTexImage1D := wglGetProcAddress('glCompressedTexImage1D');
  glCompressedTexImage2D := wglGetProcAddress('glCompressedTexImage2D');
  glCompressedTexImage3D := wglGetProcAddress('glCompressedTexImage3D');
  glMultiTexCoord1d      := wglGetProcAddress('glMultiTexCoord1d');
  glMultiTexCoord1dv     := wglGetProcAddress('glMultiTexCoord1dv');
  glMultiTexCoord1f      := wglGetProcAddress('glMultiTexCoord1f');
  glMultiTexCoord1fv     := wglGetProcAddress('glMultiTexCoord1fv');
  glMultiTexCoord1i      := wglGetProcAddress('glMultiTexCoord1i');
  glMultiTexCoord1iv     := wglGetProcAddress('glMultiTexCoord1iv');
  glMultiTexCoord1s      := wglGetProcAddress('glMultiTexCoord1s');
  glMultiTexCoord1sv     := wglGetProcAddress('glMultiTexCoord1sv');
  glMultiTexCoord2d      := wglGetProcAddress('glMultiTexCoord2d');
  glMultiTexCoord2dv     := wglGetProcAddress('glMultiTexCoord2dv');
  glMultiTexCoord2f      := wglGetProcAddress('glMultiTexCoord2f');
  glMultiTexCoord2fv     := wglGetProcAddress('glMultiTexCoord2fv');
  glMultiTexCoord2i      := wglGetProcAddress('glMultiTexCoord2i');
  glMultiTexCoord2iv     := wglGetProcAddress('glMultiTexCoord2iv');
  glMultiTexCoord2s      := wglGetProcAddress('glMultiTexCoord2s');
  glMultiTexCoord2sv     := wglGetProcAddress('glMultiTexCoord2sv');
  glMultiTexCoord3d      := wglGetProcAddress('glMultiTexCoord3d');
  glMultiTexCoord3dv     := wglGetProcAddress('glMultiTexCoord3dv');
  glMultiTexCoord3f      := wglGetProcAddress('glMultiTexCoord3f');
  glMultiTexCoord3fv     := wglGetProcAddress('glMultiTexCoord3fv');
  glMultiTexCoord3i      := wglGetProcAddress('glMultiTexCoord3i');
  glMultiTexCoord3iv     := wglGetProcAddress('glMultiTexCoord3iv');
  glMultiTexCoord3s      := wglGetProcAddress('glMultiTexCoord3s');
  glMultiTexCoord3sv     := wglGetProcAddress('glMultiTexCoord3sv');
  glMultiTexCoord4d      := wglGetProcAddress('glMultiTexCoord4d');
  glMultiTexCoord4dv     := wglGetProcAddress('glMultiTexCoord4dv');
  glMultiTexCoord4f      := wglGetProcAddress('glMultiTexCoord4f');
  glMultiTexCoord4fv     := wglGetProcAddress('glMultiTexCoord4fv');
  glMultiTexCoord4i      := wglGetProcAddress('glMultiTexCoord4i');
  glMultiTexCoord4iv     := wglGetProcAddress('glMultiTexCoord4iv');
  glMultiTexCoord4s      := wglGetProcAddress('glMultiTexCoord4s');
  glMultiTexCoord4sv     := wglGetProcAddress('glMultiTexCoord4sv');
  
  // OpenGL 1.5
  glGenQueries           := wglGetProcAddress('glGenQueries');
  glDeleteQueries        := wglGetProcAddress('glDeleteQueries');
  glIsQuery              := wglGetProcAddress('glIsQuery');
  glBeginQuery           := wglGetProcAddress('glBeginQuery');
  glEndQuery             := wglGetProcAddress('glEndQuery');
  glGetQueryiv           := wglGetProcAddress('glGetQueryiv');
  glGetQueryObjectiv     := wglGetProcAddress('glGetQueryObjectiv');
  glGetQueryObjectuiv    := wglGetProcAddress('glGetQueryObjectuiv');
  glBindBuffer           := wglGetProcAddress('glBindBuffer');
  glDeleteBuffers        := wglGetProcAddress('glDeleteBuffers');
  glGenBuffers           := wglGetProcAddress('glGenBuffers');
  glIsBuffer             := wglGetProcAddress('glIsBuffer');
  glBufferData           := wglGetProcAddress('glBufferData');
  glBufferSubData        := wglGetProcAddress('glBufferSubData');
  glGetBufferSubData     := wglGetProcAddress('glGetBufferSubData');
  glMapBuffer            := wglGetProcAddress('glMapBuffer');
  glUnmapBuffer          := wglGetProcAddress('glUnmapBuffer');
  glGetBufferParameteriv := wglGetProcAddress('glGetBufferParameteriv');
  glGetBufferPointerv    := wglGetProcAddress('glGetBufferPointerv');

  // GL_ARB_vertex_program
  glVertexAttrib1dARB            := wglGetProcAddress('glVertexAttrib1dARB');
  glVertexAttrib1dvARB           := wglGetProcAddress('glVertexAttrib1dvARB');
  glVertexAttrib1fARB            := wglGetProcAddress('glVertexAttrib1fARB');
  glVertexAttrib1fvARB           := wglGetProcAddress('glVertexAttrib1fvARB');
  glVertexAttrib1sARB            := wglGetProcAddress('glVertexAttrib1sARB');
  glVertexAttrib1svARB           := wglGetProcAddress('glVertexAttrib1svARB');
  glVertexAttrib2dARB            := wglGetProcAddress('glVertexAttrib2dARB');
  glVertexAttrib2dvARB           := wglGetProcAddress('glVertexAttrib2dvARB');
  glVertexAttrib2fARB            := wglGetProcAddress('glVertexAttrib2fARB');
  glVertexAttrib2fvARB           := wglGetProcAddress('glVertexAttrib2fvARB');
  glVertexAttrib2sARB            := wglGetProcAddress('glVertexAttrib2sARB');
  glVertexAttrib2svARB           := wglGetProcAddress('glVertexAttrib2svARB');
  glVertexAttrib3dARB            := wglGetProcAddress('glVertexAttrib3dARB');
  glVertexAttrib3dvARB           := wglGetProcAddress('glVertexAttrib3dvARB');
  glVertexAttrib3fARB            := wglGetProcAddress('glVertexAttrib3fARB');
  glVertexAttrib3fvARB           := wglGetProcAddress('glVertexAttrib3fvARB');
  glVertexAttrib3sARB            := wglGetProcAddress('glVertexAttrib3sARB');
  glVertexAttrib3svARB           := wglGetProcAddress('glVertexAttrib3svARB');
  glVertexAttrib4NbvARB          := wglGetProcAddress('glVertexAttrib4NbvARB');
  glVertexAttrib4NivARB          := wglGetProcAddress('glVertexAttrib4NivARB');
  glVertexAttrib4NsvARB          := wglGetProcAddress('glVertexAttrib4NsvARB');
  glVertexAttrib4NubARB          := wglGetProcAddress('glVertexAttrib4NubARB');
  glVertexAttrib4NubvARB         := wglGetProcAddress('glVertexAttrib4NubvARB');
  glVertexAttrib4NuivARB         := wglGetProcAddress('glVertexAttrib4NuivARB');
  glVertexAttrib4NusvARB         := wglGetProcAddress('glVertexAttrib4NusvARB');
  glVertexAttrib4bvARB           := wglGetProcAddress('glVertexAttrib4bvARB');
  glVertexAttrib4dARB            := wglGetProcAddress('glVertexAttrib4dARB');
  glVertexAttrib4dvARB           := wglGetProcAddress('glVertexAttrib4dvARB');  
  glVertexAttrib4fARB            := wglGetProcAddress('glVertexAttrib4fARB');
  glVertexAttrib4fvARB           := wglGetProcAddress('glVertexAttrib4fvARB');
  glVertexAttrib4ivARB           := wglGetProcAddress('glVertexAttrib4ivARB');
  glVertexAttrib4sARB            := wglGetProcAddress('glVertexAttrib4sARB');
  glVertexAttrib4svARB           := wglGetProcAddress('glVertexAttrib4svARB');
  glVertexAttrib4ubvARB          := wglGetProcAddress('glVertexAttrib4ubvARB');
  glVertexAttrib4usvARB          := wglGetProcAddress('glVertexAttrib4usvARB');
  glVertexAttrib4uivARB          := wglGetProcAddress('glVertexAttrib4uivARB');
  glVertexAttribPointerARB       := wglGetProcAddress('glVertexAttribPointerARB');
  glEnableVertexAttribArrayARB   := wglGetProcAddress('glEnableVertexAttribArrayARB');
  glDisableVertexAttribArrayARB  := wglGetProcAddress('glDisableVertexAttribArrayARB');
  glProgramStringARB             := wglGetProcAddress('glProgramStringARB');  
  glBindProgramARB               := wglGetProcAddress('glBindProgramARB');
  glDeleteProgramsARB            := wglGetProcAddress('glDeleteProgramsARB');
  glGenProgramsARB               := wglGetProcAddress('glGenProgramsARB');
  glProgramEnvParameter4dARB     := wglGetProcAddress('glProgramEnvParameter4dARB');
  glProgramEnvParameter4dvARB    := wglGetProcAddress('glProgramEnvParameter4dvARB');
  glProgramEnvParameter4fARB     := wglGetProcAddress('glProgramEnvParameter4fARB');
  glProgramEnvParameter4fvARB    := wglGetProcAddress('glProgramEnvParameter4fvARB');
  glProgramLocalParameter4dARB   := wglGetProcAddress('glProgramLocalParameter4dARB');
  glProgramLocalParameter4dvARB  := wglGetProcAddress('glProgramLocalParameter4dvARB');  
  glProgramLocalParameter4fARB   := wglGetProcAddress('glProgramLocalParameter4fARB');
  glProgramLocalParameter4fvARB  := wglGetProcAddress('glProgramLocalParameter4fvARB');
  glGetProgramEnvParameterdvARB  := wglGetProcAddress('glGetProgramEnvParameterdvARB');
  glGetProgramEnvParameterfvARB  := wglGetProcAddress('glGetProgramEnvParameterfvARB');
  glGetProgramLocalParameterdvARB  := wglGetProcAddress('glGetProgramLocalParameterdvARB');
  glGetProgramLocalParameterfvARB  := wglGetProcAddress('glGetProgramLocalParameterfvARB');
  glGetProgramivARB              := wglGetProcAddress('glGetProgramivARB');
  glGetProgramStringARB          := wglGetProcAddress('glGetProgramStringARB');
  glGetVertexAttribdvARB         := wglGetProcAddress('glGetVertexAttribdvARB');
  glGetVertexAttribfvARB         := wglGetProcAddress('glGetVertexAttribfvARB');
  glGetVertexAttribivARB         := wglGetProcAddress('glGetVertexAttribivARB');
  glGetVertexAttribPointervARB   := wglGetProcAddress('glGetVertexAttribPointervARB');
  glIsProgramARB                 := wglGetProcAddress('glIsProgramARB');
  
  // GL_ARB_draw_buffers
  glDrawBuffersARB       := wglGetProcAddress('glDrawBuffersARB');
  
  // GL_ARB_draw_elements_base_vertex
  glDrawElementsBaseVertex          := wglGetProcAddress('glDrawElementsBaseVertex');
  glDrawRangeElementsBaseVertex     := wglGetProcAddress('glDrawRangeElementsBaseVertex');
  glDrawElementsInstancedBaseVertex := wglGetProcAddress('glDrawElementsInstancedBaseVertex');
  glMultiDrawElementsBaseVertex     := wglGetProcAddress('glMultiDrawElementsBaseVertex');
  
  // GL_ARB_instanced_arrays
  glVertexAttribDivisorARB          := wglGetProcAddress('glVertexAttribDivisorARB');
end;

end.
module Graphics.Resource where

import Graphics.Vbo
import Graphics.Shaders
import Graphics.Util
import Graphics.Texture
import Graphics.Rendering.OpenGL
import Control.Monad
import Control.Applicative
import Data.Maybe
import qualified Data.Map as M
import Data.List ( intercalate )

{- Defining -}
data Resource = Resource { _programs     :: [Program]
                         , _buffers      :: [InterleavedVbo]
                         , _textures     :: [TextureObject]
                         , _uniforms     :: [[GLfloat]]
                         , _render       :: Resource -> IO ()
                         }

instance Show Resource where
    show r = let p   = show $ _programs r
                 vbo = show $ _buffers r
                 t   = show $ _textures r
                 us  = show $ _uniforms r
                 rend= "Resource -> IO ()"
             in intercalate ", " [ "Resource{programs="++p
                                 , "buffers="++vbo
                                 , "textures="++t
                                 , "uniforms="++us
                                 , "render="++rend
                                 ]

data ProgramDef = ProgramDef { _vertSrc :: FilePath
                             , _fragSrc :: FilePath
                             , _attribs :: [String]
                             } deriving (Show, Eq)

data ProgramStore = ProgramStore { _programId :: String
                                 , _program   :: Program
                                 , _attLocs   :: [AttribLocation]
                                 }

data AttribArrayDef = AttribArrayDef Int [GLfloat]

data VboDef = VboDef String [AttribArrayDef]
data VboStore = VboStore String InterleavedVbo

data TexDef = TexDef Int FilePath
data TexStore = TexStore String TextureObject

data ResourceDef = ResourceDef { _programDefs :: [ProgramDef]
                               , _texDefs     :: [TexDef]
                               , _vboDefs     :: [VboDef]
                               }

type Map a = M.Map String a

data ResourceStore = ResourceStore { _programMap :: Map ProgramStore
                                   , _textureMap :: Map TextureObject
                                   , _vboMap     :: Map InterleavedVbo
                                   }

{- Loading -}
loadResourceDef :: ResourceDef -> IO (Maybe ResourceStore)
loadResourceDef def = do
    let sdefs = _programDefs def
        tdefs = _texDefs def
        vdefs = _vboDefs def

    pStores <- mapM loadProgramDef sdefs
    tStores <- mapM loadTexDef tdefs
    vStores <- mapM loadVboDef vdefs
    -- Take the values out of Maybe
    -- and stick them in maps.
    let programs = sequence pStores
        texs     = sequence tStores
        vbos     = sequence vStores
        pMap     = mapFromProgramStores $ fromJust programs
        tMap     = mapFromTexStores $ fromJust texs
        vMap     = mapFromVboStores $ fromJust vbos

    return $ if isNothing programs || isNothing texs || isNothing vbos
           then Nothing
           else Just ResourceStore{ _programMap = pMap
                                  , _textureMap = tMap
                                  , _vboMap     = vMap
                                  }

loadProgramDef :: ProgramDef -> IO (Maybe ProgramStore)
loadProgramDef def = do
    let vs   = _vertSrc def
        fs   = _fragSrc def
        atts = _attribs def
        locs = AttribLocation . fromIntegral <$> [0..length atts]

    putStrLn $ "Loading shaders " ++ show [vs,fs]
    v   <- loadShader vs :: IO VertexShader
    f   <- loadShader fs :: IO FragmentShader
    [p] <- genObjectNames 1
    attachedShaders p $= ([v],[f])
    zipWithM_ (\att n ->
        attribLocation p att $= n) atts locs
    printError

    let glGet = Graphics.Rendering.OpenGL.get
    linkProgram p
    linked <- glGet $ linkStatus p
    unless linked $ do
        programLog <- glGet $ programInfoLog p
        putStrLn programLog

    -- Use this program.
    currentProgram $= Just p
    validateProgram p
    printError

    return $ if not linked
        then Nothing
        else Just ProgramStore{ _programId = vs++fs
                              , _program   = p
                              , _attLocs   = locs
                              }

loadTexDef :: TexDef -> IO (Maybe TexStore)
loadTexDef (TexDef unit file) = do
    mTex <- loadTexture file unit
    return $ case mTex of
           Nothing  -> Nothing
           Just tex -> Just $ TexStore file tex

loadVboDef :: VboDef -> IO (Maybe VboStore)
loadVboDef (VboDef name attribDefs) = do
    let (comps,datas) = foldr f ([],[]) attribDefs
        f (AttribArrayDef c d) (cs,ds) = (fromIntegral c:cs,d:ds)
        locs = AttribLocation . fromIntegral <$> [0..length comps - 1]

    ivbo <- interleavedVbo datas comps locs
    return $ Just $ VboStore name ivbo

{- Mapping -}
mapFromProgramStores :: [ProgramStore] -> Map ProgramStore
mapFromProgramStores = foldl (\m p -> M.insert (_programId p) p m) M.empty

mapFromTexStores :: [TexStore] -> Map TextureObject
mapFromTexStores = foldl (\m (TexStore s o) -> M.insert s o m) M.empty

mapFromVboStores :: [VboStore] -> Map InterleavedVbo
mapFromVboStores = foldl (\m (VboStore s v) -> M.insert s v m) M.empty

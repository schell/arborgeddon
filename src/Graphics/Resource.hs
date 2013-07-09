{-# LANGUAGE TemplateHaskell #-}
module Graphics.Resource where

import Graphics.Vbo
import Graphics.Shaders
import Graphics.Texture
import Control.Monad
import Control.Applicative
import Control.Lens
import Data.Maybe
import Graphics.Rendering.OpenGL
import qualified Data.Map as M
import Data.List ( intercalate )

{- Defining -}
data RenderStore = RenderStore { _programs     :: [ShaderProgram]
                               , _buffers      :: [InterleavedVbo]
                               , _textures     :: [TextureObject]
                               }
makeLenses ''RenderStore


instance Show RenderStore where
    show r = let p   = show $ _programs r
                 vbo = show $ _buffers r
                 t   = show $ _textures r
             in intercalate ", " [ "RenderStore{programs="++p
                                 , "buffers="++vbo
                                 , "textures="++t
                                 ]

data VboDef = VboDef String [ShaderAttributeData GLfloat]
data VboStore = VboStore String InterleavedVbo

data TexParamsRunner = TexParamsRunner { _runTexParams :: IO () }

instance Show TexParamsRunner where
    show _ = "TexParamsRunner"
instance Eq TexParamsRunner where
    _ == _ = True

data TexDef = TexDef String Int FilePath TexParamsRunner deriving (Show, Eq)
data TexStore = TexStore String TextureObject deriving (Show, Eq)

type ShaderDef = ShaderProgram

data ResourceDef = ResourceDef { _shaderDefs  :: [ShaderDef]
                               , _texDefs     :: [TexDef]
                               , _vboDefs     :: [VboDef]
                               }
makeLenses ''ResourceDef

type Map a = M.Map String a

data ResourceStore = ResourceStore { _programMap :: Map ShaderProgram
                                   , _textureMap :: Map TextureObject
                                   , _vboMap     :: Map InterleavedVbo
                                   }
makeLenses ''ResourceStore

type RenderFunction = (ResourceStore -> IO ())

{- Loading -}
loadResourceDef :: ResourceDef -> IO (Maybe ResourceStore)
loadResourceDef def = do
    let sdefs = _shaderDefs def
        tdefs = _texDefs def
        vdefs = _vboDefs def

    pStores <- mapM loadShaderDef sdefs
    tStores <- mapM loadTexDef tdefs
    vStores <- mapM loadVboDef vdefs
    -- Take the values out of Maybe
    -- and stick them in maps.
    let pgrms = sequence pStores
        texs  = sequence tStores
        vbos  = sequence vStores
        pMap  = mapFromProgramStores $ fromJust pgrms
        tMap  = mapFromTexStores $ fromJust texs
        vMap  = mapFromVboStores $ fromJust vbos

    return $ if isNothing pgrms || isNothing texs || isNothing vbos
           then Nothing
           else Just ResourceStore{ _programMap = pMap
                                  , _textureMap = tMap
                                  , _vboMap     = vMap
                                  }

loadShaderDef :: ShaderDef -> IO (Maybe ShaderProgram)
loadShaderDef def = do
    s <- loadShaderProgram def
    return $ if isNothing $ s ^. program
             then Nothing
             else Just s

loadTexDef :: TexDef -> IO (Maybe TexStore)
loadTexDef (TexDef name unit file runner) = do
    mTex <- loadTexture file unit
    unless (isNothing mTex) $ _runTexParams runner
    return $ case mTex of
           Nothing  -> Nothing
           Just tex -> Just $ TexStore name tex

loadVboDef :: VboDef -> IO (Maybe VboStore)
loadVboDef (VboDef name attribDefs) = do
    let (comps,datas) = foldr f ([],[]) attribDefs
        f (AttributeData (Attribute _ t) d) (cs,ds) = (fromIntegral (numComponents t):cs,d:ds)
        locs = AttribLocation . fromIntegral <$> [0..length comps - 1]
    ivbo <- interleavedVbo datas comps locs
    return $ Just $ VboStore name ivbo

{- Mapping -}
mapFromProgramStores :: [ShaderProgram] -> Map ShaderProgram
mapFromProgramStores = foldl (\m p -> M.insert (_shaderName p) p m) M.empty

mapFromTexStores :: [TexStore] -> Map TextureObject
mapFromTexStores = foldl (\m (TexStore s o) -> M.insert s o m) M.empty

mapFromVboStores :: [VboStore] -> Map InterleavedVbo
mapFromVboStores = foldl (\m (VboStore s v) -> M.insert s v m) M.empty

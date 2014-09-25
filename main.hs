{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Graphics.Rendering.OpenGL.GL as GL -- hiding (scale,rotate)
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw
import qualified Graphics.UI.GLUT as GLUT
import Data.List(inits)
import Data.IORef
import qualified Data.Map as M
import Data.Tree
import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.OpenGL
import qualified Control.Monad.Reader as R
import Control.Monad.Trans
import Control.Monad(when, liftM)
import Text.Printf
import System.FilePath
import qualified System.FilePath.Windows as FW
import System.FilePath.Find
import System.Directory

import Data.WOW.FileSystem
import Data.WOW.Matrix
import Data.WOW.World
import Data.WOW.M2Model
import Data.WOW.Bone
import Data.WOW.Creature
import Data.WOW.GL.ResourceLoader
import Data.WOW.GL.Types
import Data.WOW.GL.Mesh

canvas_width, canvas_height :: Num a => a
canvas_width  = 500
canvas_height = 500

db_prefix :: [String]
db_prefix     = ["res"]

data M2Object = ONull | OCreature Creature

data MyWorldState = MyWorldState {
    _m2world :: IORef (WorldState World MockMPQ GLResource),
    _anim    :: IORef Int,
    _time    :: IORef Int,
    _obj     :: IORef M2Object
}

newtype World a = World { unWorld :: R.ReaderT MyWorldState IO a }

instance M2World World MockMPQ GLResource where
    getWorld   = World $ R.asks _m2world >>= (liftIO . readIORef)
    modWorld f = World $ R.asks _m2world >>= (liftIO . flip modifyIORef' f)
instance Monad World where
    return = World . return
    a >>= f = World (unWorld a >>= (\v -> unWorld (f v)))
instance MonadIO World where
    liftIO = World . liftIO

runInWorld :: MyWorldState -> World a -> IO a
runInWorld w a = R.runReaderT (unWorld a) w

getObj  = World (R.asks _obj  >>= (liftIO . readIORef))
getAnim = World (R.asks _anim >>= (liftIO . readIORef))
getTime = World (R.asks _time >>= (liftIO . readIORef))

setObj  o = World (R.asks _obj  >>= (liftIO . flip writeIORef o))
setAnim i = World (R.asks _anim >>= (liftIO . flip writeIORef i))
setTime t = World (R.asks _time >>= (liftIO . flip writeIORef t))

modTime f = World (R.asks _time >>= (liftIO . flip modifyIORef' f))

main = do
  initGUI
  initGL

  m2world <- newIORef (WorldState {
                 _filesystem = MockMPQ db_prefix
               , _resLibrary = (ResourceLibrary glResourceLoader M.empty)
               , _db_creaturemodel = Nothing 
               , _db_creatureskin  = Nothing 
               })
  m2anim  <- newIORef (-1)
  m2time  <- newIORef 0
  m2obj   <- newIORef ONull
  let worldst = MyWorldState m2world m2anim m2time m2obj

  -- create window
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 8
             , windowTitle := "Animation" ]

  -- a hbox, left of which is glcanvas, right of which is configurations
  hbox  <- hBoxNew False 8
  vbox  <- vBoxNew False 8

  config <- glConfigNew [GLModeRGBA, GLModeAlpha, GLModeDepth, GLModeDouble]
  canvas <- glDrawingAreaNew config
  widgetSetSizeRequest canvas canvas_width canvas_height

  -- a tree of all available models
  sw       <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
  mdl_tree <- buildM2Tree >>= treeStoreNew
  mdl_view <- treeViewNewWithModel mdl_tree
  mdl_colm <- treeViewColumnNew
  set mdl_colm [ treeViewColumnTitle := "M2 Model"
               , treeViewColumnSizing := TreeViewColumnAutosize
               , treeViewColumnFixedWidth := 200 ]
  mdl_rend <- cellRendererTextNew
  cellLayoutPackStart     mdl_colm mdl_rend True
  cellLayoutSetAttributes mdl_colm mdl_rend mdl_tree (\r -> [ cellText := r])
  treeViewAppendColumn mdl_view mdl_colm

  -- a combo box
  -- 1st. Model is ListStore [Animatioin]
  ani_list  <- listStoreNew []
  -- 2nd. View
  ani_comb  <- comboBoxNewWithModel ani_list
  -- 3rd. Renderer
  ani_rend  <- cellRendererTextNew
  cellLayoutPackStart     ani_comb ani_rend True
  cellLayoutSetAttributes ani_comb ani_rend ani_list (\(idx,ani) -> [ cellText := printf "Anim:%2d" (idx :: Int) ])
  skin_comb <- comboBoxNewText 

  set window [ containerChild := hbox ]
  set hbox   [ containerChild := canvas
             , containerChild := vbox ]
  set vbox   [ containerChild := sw
             , boxChildPacking sw := PackGrow
             , containerChild := ani_comb
             , boxChildPacking ani_comb := PackNatural
             , containerChild := skin_comb
             , boxChildPacking skin_comb := PackNatural ]
  set sw     [ containerChild := mdl_view
             , widgetWidthRequest := 200 ]

  -- setup events
  onRealize canvas (withGLDrawingArea canvas $ \_ -> myInit)
  onExpose  canvas (\_ -> do withGLDrawingArea canvas (\w -> do myDisplay worldst
                                                                glDrawableSwapBuffers w )
                             return True )
  timeoutAddFull (myUpdate worldst >> widgetQueueDraw canvas >> return True) priorityDefaultIdle 50
  on skin_comb changed (myChangeSkin skin_comb worldst)
  on ani_comb  changed (myChangeAnim (ani_list, ani_comb) worldst)
  onRowActivated mdl_view (\path _ -> do p <- mapM (treeStoreGetValue mdl_tree) (tail $ inits path)
                                         myChangeModel p worldst (ani_list,ani_comb) skin_comb)

  widgetShowAll window
  mainGUI

buildM2Tree = do
  m2 <- fmap (map (splitDirectories . dropExtensions)) $ find always (extension ==? ".m2") (joinPath db_prefix)
  return $ foldl merge [] (map (line . drop (length db_prefix)) m2)
    where
      line :: [String] -> Forest String
      line []     = []
      line (n:ns) = [Node{ rootLabel = n, subForest = line ns }]
      merge [] f2     = f2
      merge (t:f1) f2 = merge f1 (addto t f2)
      addto tree@(Node l s) forest = if not (elem l (map rootLabel forest))
                                     then tree : forest
                                     else map (\(Node l' s') -> if l == l' then Node l (merge s s') else Node l' s') forest

myInit = do
  clearColor $= Color4 0.4 0.4 0.4 1
  clearDepth $= 1
  depthFunc  $= Just Less
  blend      $= Disabled
  shadeModel $= Smooth
  glEnable gl_TEXTURE_2D
  lighting   $= Enabled
  ambient  (Light 1) $= Color4 1.0 1.0 1.0 1.0
  diffuse  (Light 1) $= Color4 1.0 1.0 1.0 1.0
  position (Light 1) $= Vertex4 0 0 5 (1.0 :: GLfloat)
  light    (Light 1) $= Enabled
  hint PerspectiveCorrection $= Nicest
  matrixMode $= Projection
  loadIdentity
  gluPerspective 45 (canvas_width / canvas_height) 1 1000
  matrixMode $= Modelview 0
  loadIdentity

myDisplay :: MyWorldState -> IO ()
myDisplay worldst = do
  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity
  runInWorld worldst $ do
    obj <- getObj
    case obj of 
      ONull          -> return ()
      OCreature crea -> do
        res <- findResource (_crea_resource crea)
        case res of 
          Nothing                -> return ()
          Just (GLModel mdl msh) -> do
            liftIO $ resetCamera mdl
            liftIO $ GL.rotate (90 :: GLfloat) (Vector3 (-1) 0 0)
            view <- liftIO $ (GLUT.get $ matrix $ Just $ Modelview 0 :: IO (GLmatrix GLfloat))
                                >>= getMatrixComponents RowMajor
            anim <- getAnim
            time <- getTime
            if anim < 0
              then renderAll [] msh
              else do -- lift $ drawBone' (fromList view) anim time (m_bones_ mdl)
                      -- todo : the `mod` should be move into the `Animated`
                      let max    = anim_length_ (m_animations_ mdl !! anim)
                      let matrix = transform (fromList view) anim (time `mod` max) (m_bones_ mdl)
                      skeletonAnim matrix msh >>= renderAll (_crea_skin crea)
  
    where resetCamera mdl = let r = realToFrac $ vertexRadius_ $ m_bounding_ mdl
                            in  gluLookAt 0.0 0.0       (2 * r)
                                          0.0 (0.3 * r) 0.0
                                          0.0 1.0       0.0
                                        


myUpdate worldst = runInWorld worldst (modTime (+33))

myChangeAnim (ani_list, ani_comb) worldst = do
  size <- listStoreGetSize  ani_list
  idx  <- comboBoxGetActive ani_comb
  when (size > 0 && idx >=0)
       (do (_,anim) <- listStoreGetValue ani_list idx
           let id   = printf "%d-%d"  (anim_Id_ anim) (anim_subId_ anim) :: String
               len  = anim_length_ anim
               spd  = printf "%.2f" (anim_move_speed_  anim) :: String
           putStrLn $ printf "Id:%s Len:%d Speed:%s" id len spd)
  runInWorld worldst (setAnim idx >> setTime 0)

myChangeSkin skin_comb worldst = do
  idx <- comboBoxGetActive skin_comb
  runInWorld worldst $ do
    mo <- getObj
    case mo of
      OCreature c -> do c' <- creatureChangeSkin c idx
                        setObj (OCreature c')
      _ -> liftIO (putStrLn "? no creature when changing skin")

myChangeModel path worldst (ani_list,ani_comb) skin_comb = do
  let file = addExtension (joinPath $ db_prefix ++ path) ".m2"
  let res  = FW.joinPath path
  b <- doesFileExist file
  putStrLn res
  if not b 
    then return ()
    else runInWorld worldst $ do
      setAnim (-1)
      setTime 0
      setObj  ONull
      when (elem "creature" path) $ do
        crea <- newCreature res (0 :: Int)
        let object = OCreature crea
        setObj object
        sl  <- creatureSkinList (_crea_name crea)
        Just (GLModel mdl msh) <- findResource (_crea_resource crea)
        liftIO $ do mapM_ (\t -> case t of
                                   Data.WOW.M2Model.Texture f _ -> putStrLn f
                                   _ -> return ()
                          ) (m_textures_ mdl)
        liftIO $ mapM_ (putStrLn . drop 4) (catMaybes $ concat sl)
        liftIO $ do -- fill in the model of combobox
                    listStoreClear ani_list
                    mapM_ (listStoreAppend ani_list) (zip [1::Int .. ] $ m_animations_ mdl)
                    -- select the first item if any
                    when (not $ null $ m_animations_ mdl)
                         (comboBoxSetActive ani_comb 0)
                    skin_list <- comboBoxGetModelText skin_comb
                    listStoreClear skin_list
                    mapM_ (listStoreAppend skin_list . takeFileName . head . catMaybes) sl
                    when (not $ null $ sl)
                         (comboBoxSetActive skin_comb 0)

drawBone' view anim time bones
    = let matrix = transform view anim time bones
          pivots = zipWith multVec3 matrix (map bone_pivot_ bones)
      in  drawBone $ map (\(bn,np) -> bn{ bone_pivot_ = np}) $ zip bones pivots
    
drawBone bones
    = do let gf = realToFrac :: Float -> GLfloat
         depthFunc $= Nothing
         lighting  $= Disabled
         GLUT.lineWidth $= 2
         color (Color3 (gf 0) 0 0)
         renderPrimitive Lines (mapM_ (\(idx,b0)-> 
                                           let (Vector3 a b c) = bone_pivot_ b0
                                               par = bone_parent_ (bones !! idx)
                                           in  when (par /= -1)
                                                    (let Vector3 e f g = bone_pivot_ (bones !! par)
                                                     in  vertex (Vertex3 (gf a) (gf b) (gf c)) >>
                                                         vertex (Vertex3 (gf e) (gf f) (gf g)))
                                      ) $ zip [0..] bones)
         GLUT.pointSize $= 3
         renderPrimitive Points (mapM_ (\(idx,b0)-> 
                                            let (Vector3 a b c) = bone_pivot_ b0
                                                par = bone_parent_ (bones !! idx)
                                            in  when (par == -1)
                                                     (vertex (Vertex3 (gf a) (gf b) (gf c)))
                                       ) $ zip [0..] bones)
         GLUT.pointSize $= 10
         let Vector3 x0 y0 z0 = bone_pivot_ $ head bones
         color (Color3 (gf 1) 0 0)
         renderPrimitive Points (vertex (Vertex3 (gf x0) (gf y0) (gf z0)))

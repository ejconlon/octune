module Minipat.Octune.Ana where

import Bowtie.Anno (Anno (..))
import Bowtie.Memo (Memo (..), memoKey)
import Control.Exception (Exception)
import Control.Monad (foldM, unless, when)
import Control.Monad.Except (throwError)
import Data.Foldable (fold, toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Minipat.Octune.Ast
import Minipat.Octune.Parser (Pos, PosFile)
import Minipat.Octune.Topo (SortErr, topoAnnoIncM)

data Ana = Ana
  { anaPos :: !Pos
  , anaBeats :: !Beats
  }
  deriving stock (Eq, Ord, Show)

data AnaFile = AnaFile
  { afPos :: !Pos
  , afMod :: !ModName
  , afDecls :: !(Map Text AnaExp)
  }
  deriving stock (Eq, Ord, Show)

type AnaExp = Memo ExpF Ana

data AnaExpErr = AnaExpErrCheck !Pos !QualName !Beats !Beats
  deriving stock (Eq, Ord, Show)

instance Exception AnaExpErr

data AnaFileErr
  = AnaFileErrDupeMod !Pos !ModName
  | AnaFileErrDupeDecl !Pos !Text
  | AnaFileErrSort !Pos !(SortErr QualName)
  | AnaFileErrExp !AnaExpErr
  deriving stock (Eq, Ord, Show)

instance Exception AnaFileErr

anaFileErrPos :: AnaFileErr -> Pos
anaFileErrPos = \case
  AnaFileErrDupeMod p _ -> p
  AnaFileErrDupeDecl p _ -> p
  AnaFileErrSort p _ -> p
  AnaFileErrExp (AnaExpErrCheck p _ _ _) -> p

anaExp :: (QualName -> Either AnaExpErr Ana) -> QualName -> Pos -> ExpF Ana -> Either AnaExpErr Ana
anaExp onRef qnRoot loc ex =
  fmap (Ana loc) $ case ex of
    ExpVar qn -> fmap anaBeats (onRef qn)
    ExpNote (Note _ b _) -> pure b
    ExpApp f xs -> do
      let sumLen = case xs of Empty -> 0; _ -> sum (fmap anaBeats (toList xs))
          maxLen = case xs of Empty -> 0; _ -> maximum (fmap anaBeats (toList xs))
      pure $ case f of
        FunSequ -> sumLen
        FunMerge -> maxLen
        FunRepeat n -> sumLen * fromRational n
        FunWaveform _ -> sumLen
        FunVolume _ -> sumLen
        FunSlice s e -> max 0 (e - s)
    ExpCheck b x -> do
      let c = anaBeats x
      unless (c == b) (throwError (AnaExpErrCheck loc qnRoot c b))
      pure b
    ExpBpm _ x -> pure (anaBeats x)

resolveMod :: ModName -> QualName -> QualName
resolveMod modn qn@(QualName x y) = if Seq.null x then QualName modn y else qn

memoExpRefs :: ModName -> ExpF (Set QualName) -> Set QualName
memoExpRefs modn = \case
  ExpVar qn -> Set.singleton (resolveMod modn qn)
  ExpNote _ -> Set.empty
  ExpApp _ rs -> fold rs
  ExpCheck _ r -> r
  ExpBpm _ r -> r

anaFile
  :: Map ModName AnaFile -> Map QualName Ana -> PosFile -> Either AnaFileErr (AnaFile, Map ModName AnaFile, Map QualName Ana)
anaFile mm0 qm0 (Anno floc (File modn ds0)) = do
  when (Map.member modn mm0) (throwError (AnaFileErrDupeMod floc modn))
  ds1 <- (\f -> foldM f Map.empty ds0) $ \ds1 ldec@(Anno dloc (Decl n _)) ->
    if Map.member n ds1
      then throwError (AnaFileErrDupeDecl dloc n)
      else pure (Map.insert n ldec ds1)
  let ds2 = Map.fromList (fmap (\(n, Anno _ (Decl _ ex)) -> (QualName modn n, ex)) (Map.toList ds1))
      getRefs = memoExpRefs modn
  case topoAnnoIncM qm0 getRefs ds2 anaExp of
    Left sortErr -> throwError (AnaFileErrSort floc sortErr)
    Right (Left expErr) -> throwError (AnaFileErrExp expErr)
    Right (Right ds3) -> do
      let af = AnaFile floc modn (Map.mapKeys qnVar ds3)
          mm1 = Map.insert modn af mm0
          qm1 = Map.union qm0 (fmap memoKey ds3)
      pure (af, mm1, qm1)

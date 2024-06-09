import Data.List (foldl', nub)
import Data.Map (toList)

type Coord = (Integer, Integer)
type Path = ((Coord, Coord), Bool)
type DisjointSet = Map Path Path

connected :: [Path] -> Bool
connected paths =
    let
        initDS :: DisjointSet
        initDS = Map.fromList $ zip paths paths

        findPath :: Path -> DisjointSet -> Path
        findPath path ds =
            let
                p = ds Map.! path
            in
                if p == path then p else findPath p ds

        unionPaths :: Path -> Path -> DisjointSet -> DisjointSet
        unionPaths path1 path2 ds =
            let
                p1 = findPath path1 ds
                p2 = findPath path2 ds
            in
                if p1 == p2 then ds else Map.insert p1 p2 ds

        disjointSet :: DisjointSet
        disjointSet = foldl' unionPaths initDS (concatMap pairs paths)
    in
        length (nub $ map snd $ Map.toList disjointSet) > 1

pairs :: Path -> [Path]
pairs path = [(path, path') | path' <- paths, intersect path path']
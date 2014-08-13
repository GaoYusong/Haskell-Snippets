import Data.List

data Point2D = Point2D Integer Integer
             deriving (Show)
                      
data Vector = Vector Integer Integer
            deriving (Show)
                     
data Direction = LEFT | RIGHT | STRAIGHT
               deriving (Eq, Show)

grahamScan :: [Point2D] -> [Point2D]
grahamScan points =
  let 
    sortedPoints = Data.List.sortBy pointCompare points
  in
   if length sortedPoints >= 3 then
     grahamScanImpl (reverse (take 3 sortedPoints)) (drop 3 sortedPoints)
   else
     sortedPoints

grahamScanImpl :: [Point2D] -> [Point2D] -> [Point2D]
grahamScanImpl (p2:p1:stacks) (p:points)
  | direction p1 p2 p /= LEFT =
      grahamScanImpl (p1:stacks) (p:points)
  | otherwise =
      grahamScanImpl (p:p2:p1:stacks) points
grahamScanImpl stacks (p:points) =
  grahamScanImpl (p:stacks) points
grahamScanImpl stacks [] =
  stacks

directions :: [Point2D] -> [Direction]
directions (a:b:c:points) = (direction a b c) : (directions (b:c:points))
directions _ = []

direction :: Point2D ->  Point2D -> Point2D -> Direction
direction a b c =
  let
    cp = crossProduct (vector a b) (vector a c)
  in if cp > 0 then
       LEFT
     else if cp == 0 then
            STRAIGHT
          else
            RIGHT


vector :: Point2D -> Point2D -> Vector
vector (Point2D x1 y1) (Point2D x2 y2) = Vector (x2 - x1) (y2 - y1)

pointCompare :: Point2D -> Point2D -> Ordering
pointCompare (Point2D x1 y1) (Point2D x2 y2) = compare (x1, y1) (x2, y2)


crossProduct :: Vector -> Vector -> Integer
crossProduct (Vector x1 y1) (Vector x2 y2) = x1 * y2 - y1 * x2

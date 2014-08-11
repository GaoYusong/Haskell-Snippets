data Point2D = Point2D Integer Integer
             deriving (Show)
                      
data Vector = Vector Integer Integer
            deriving (Show)
                     
data Direction = LEFT | RIGHT | STRAIGHT
               deriving (Show)

directions :: [Point2D] -> [Direction]
directions (a:b:c:points) = (direction a b c) : (directions (b:c:points))
directions _ = []

direction :: Point2D ->  Point2D -> Point2D -> Direction
direction a b c =
  let
    cp = cross_product (vector a b) (vector a c)
  in if cp > 0 then
       LEFT
     else if cp == 0 then
            STRAIGHT
          else
            RIGHT

vector :: Point2D -> Point2D -> Vector
vector (Point2D x1 y1) (Point2D x2 y2) = Vector (x2 - x1) (y2 - y1)

cross_product :: Vector -> Vector -> Integer
cross_product (Vector x1 y1) (Vector x2 y2) = x1 * y2 - y1 * x2

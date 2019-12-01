# Helper library for making generative art
## Overview
Some things already here:
* Pt
* Vec
* Shapes
  * Polygon (functions mostly for convex)
    * Inscribing of triangles / convex polygon
    * Very basic triangulation of convex polygons
  * Circle
  * Line
  * Curve
    * Chaikin curve subdivision
* Grid
* Randomness
  * Random number
  * Random vector
  * Perlin noise
* RenderProgress for animations
* HSV & HEX colors


Things to add:
* Bezier curves, b-splines
* Picking random point in polygon
  * Polygon triangulation
* Checking whether point is in a shape
* Voronoi / delaunay
* Convex hull
* Circumcircle
* Distance functions
  * Point to circle
  * Point to line
  * Distance between circles
  * Point to polygon (minimum of point to line segments)
  * ...
* Data structures
  * Graph datastructure
    * edge bundling?
  * Quadtree
* Path finding? ([maybe this library](http://hackage.haskell.org/package/search-algorithms-0.3.1/docs/Algorithm-Search.html))
* Scaling, rotation etc. for shapes
* Tilings (e.g. Penrose)
* Color palettes
  * a module that export color1 ... color8 or something, makes it easy to switch to other palette
  * some standard colors like red, blue, orange

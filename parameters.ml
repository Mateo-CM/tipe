type point = float * float
type arete = point * point
type voronoi = {
    mutable na : int;
    mutable np : int;
    mutable indice_a : (arete,int) Hashtbl.t;
    mutable indice_p : (point,int) Hashtbl.t;
    
    mutable polygone_d : point array;
    mutable polygone_g : point array;
    mutable debut_a : point array;
    mutable fin_a : point array;
    mutable h_precedent : arete option array;
    mutable ah_precedent : arete option array;
    mutable h_suivant : arete option array;
    mutable ah_suivant : arete option array;
    
    mutable arete_autour_p : arete list array;
    
}

type quadtree = |Nil | Noeud of point * quadtree*quadtree*quadtree*quadtree

let tw,gw = 25,25
let th,gh = 25,25
let p1 = (0.5,3.0*.sqrt(2.0)/.2.0 +. 1.)
let p2 = (-.3.0*.sqrt(6.0)/.4.0 +. 0.5, -.3.0*.sqrt(2.0)/.2.0 +. 1. )
let p3 = (3.0*.sqrt(6.0)/.4.0 +. 0.5, -.3.0*.sqrt(2.0)/.2.0 +. 1. )


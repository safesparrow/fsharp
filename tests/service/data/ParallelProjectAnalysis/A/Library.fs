module A.Impl

open C

let compare (root: Root) =
    let b1 = B1.Impl.solve root
    let b2 = B2.Impl.solve root
    b1 > b2
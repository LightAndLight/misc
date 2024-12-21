//! CoreGraphics.framework

/// CGBase.h
pub mod cg_base {
    pub type CGFloat = f64;
}

/// CGGeometry.h
pub mod cg_geometry {
    use super::cg_base::CGFloat;

    #[repr(C)]
    pub struct CGPoint {
        pub x: CGFloat,
        pub y: CGFloat,
    }

    #[repr(C)]
    pub struct CGSize {
        pub width: CGFloat,
        pub height: CGFloat,
    }

    #[repr(C)]
    pub struct CGRect {
        pub origin: CGPoint,
        pub size: CGSize,
    }
}

#![cfg_attr(not(feature = "std"), no_std)]

mod lib {
    pub mod core {
        #[cfg(not(feature = "std"))]
        pub use core::*;
        #[cfg(feature = "std")]
        pub use std::*;
    }
}

use lib::core::ops::{
    Add, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Neg, Not, Shl, ShlAssign,
    Shr, ShrAssign, Sub,
};

use lib::core::hash::Hash;

use lib::core::cmp::{Ord, PartialOrd};

use lib::core::convert::From;

use lib::core::fmt::{Binary, Debug, Display, Error, Formatter, LowerHex, Octal, UpperHex};

pub use self::implementation::{int, uint};

pub trait As<Value> {
    type Output;

    /// Performs lossy conversion.
    ///
    /// # Example
    ///
    /// ```
    /// use arbintrary::*;
    /// assert_eq!(uint::<4>::as_(uint::<12>::new(0xFFF)), uint::<4>::new(0xF));
    /// ```
    #[must_use]
    fn as_(value: Value) -> Self::Output;
}

mod implementation {
    use super::*;

    pub trait IntImpl<const N: usize> {
        type Type;
    }

    pub trait UIntImpl<const N: usize> {
        type Type;
    }

    #[allow(non_camel_case_types)]
    pub type int<const N: usize> = <() as IntImpl<N>>::Type;

    #[allow(non_camel_case_types)]
    pub type uint<const N: usize> = <() as UIntImpl<N>>::Type;

    macro_rules! impl_uint {
    ($name:ident, $base:ty, $base_bits:literal, [$($bits:literal,)*]) => {
        #[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
        #[repr(transparent)]
        pub struct $name<const N: usize>($base);

        impl<const N: usize> $name<N> {
            pub const MAX: Self = Self(if N > 0 {<$base>::MAX >> ($base_bits - (N as u32))} else {0});
            pub const MIN: Self = Self(0);

            fn mask(self) -> Self {
                $name(self.0 & Self::MAX.0)
            }
        }

        impl_common!($name, $base);

        $(impl UIntImpl<$bits> for () {
            type Type = $name<$bits>;
        })*
        };
    }

    macro_rules! impl_int {
    ($name:ident, $base:ty, $base_bits:literal, [$($bits:literal,)*]) => {
        #[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd, Hash)]
        #[repr(transparent)]
        pub struct $name<const N: usize>($base);

        impl<const N: usize> $name<N> {
            pub const MAX: Self = Self(if N > 0 {<$base>::MAX >> ($base_bits - (N as u32))} else {0});
            pub const MIN: Self = Self(if N > 0 {-(<$base>::MAX >> ($base_bits - (N as u32))) - 1} else {0});

            fn mask(self) -> Self {
                if ( self.0 & (1<<(N-1)) ) == 0 {
                    Self(self.0 & Self::MAX.0)
                } else {
                    Self(self.0 | !Self::MAX.0)
                }
            }
        }

        impl_common!($name, $base);

        impl<const N: usize> Neg for $name<N> {
            type Output = Self;
            fn neg(self) -> Self {
                Self(-self.0).mask()
            }
        }

        $(impl IntImpl<$bits> for () {
            type Type = $name<$bits>;
        })*
        };
    }

    macro_rules! impl_common {
        ($name:ident, $type:ty) => {
            impl<const N: usize> $name<N> {
                pub fn new(value: $type) -> Self {
                    assert!(value <= Self::MAX.0 && value >= Self::MIN.0);
                    Self(value)
                }

                /// Wrapping (modular) subtraction. Computes `self - other`,
                /// wrapping around at the boundary of the type.
                ///
                /// # Examples
                ///
                /// Basic usage:
                ///
                /// ```
                /// use arbintrary::*;
                ///
                /// assert_eq!(int::<5>::MIN.wrapping_sub(int::<5>::new(1)), int::<5>::MAX);
                ///
                /// assert_eq!(int::<5>::new(-10).wrapping_sub(int::<5>::new(5)), int::<5>::new(-15));
                /// assert_eq!(int::<5>::new(-15).wrapping_sub(int::<5>::new(5)), int::<5>::new(12));
                /// ```
                pub fn wrapping_sub(self, rhs: Self) -> Self {
                    Self(self.0.wrapping_sub(rhs.0)).mask()
                }

                /// Wrapping (modular) addition. Computes `self + other`,
                /// wrapping around at the boundary of the type.
                ///
                /// # Examples
                ///
                /// Basic usage:
                ///
                /// ```
                /// use arbintrary::*;
                ///
                /// assert_eq!(int::<5>::MAX.wrapping_add(int::<5>::new(1)), int::<5>::MIN);
                ///
                /// assert_eq!(int::<5>::new(10).wrapping_add(int::<5>::new(5)), int::<5>::new(15));
                /// assert_eq!(int::<5>::new(15).wrapping_add(int::<5>::new(5)), int::<5>::new(-12));
                /// ```
                pub fn wrapping_add(self, rhs: Self) -> Self {
                    Self(self.0.wrapping_add(rhs.0)).mask()
                }
            }

            // Implement formatting functions
            impl<const N: usize> Display for $name<N> {
                fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                    Display::fmt(&self.0, f)
                }
            }

            impl<const N: usize> Debug for $name<N> {
                fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                    Debug::fmt(&self.0, f)
                }
            }

            impl<const N: usize> UpperHex for $name<N> {
                fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
                    UpperHex::fmt(&self.0, f)
                }
            }

            impl<const N: usize> LowerHex for $name<N> {
                fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
                    LowerHex::fmt(&self.0, f)
                }
            }

            impl<const N: usize> Octal for $name<N> {
                fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
                    Octal::fmt(&self.0, f)
                }
            }

            impl<const N: usize> Binary for $name<N> {
                fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
                    Binary::fmt(&self.0, f)
                }
            }

            impl<T, const N: usize> Shr<T> for $name<N>
            where
                $type: Shr<T, Output = $type>,
            {
                type Output = Self;

                fn shr(self, rhs: T) -> Self {
                    Self(self.0.shr(rhs)).mask()
                }
            }

            impl<T, const N: usize> Shl<T> for $name<N>
            where
                $type: Shl<T, Output = $type>,
            {
                type Output = Self;

                fn shl(self, rhs: T) -> Self {
                    Self(self.0.shl(rhs)).mask()
                }
            }

            impl<T, const N: usize> ShrAssign<T> for $name<N>
            where
                $type: ShrAssign<T>,
            {
                fn shr_assign(&mut self, rhs: T) {
                    self.0.shr_assign(rhs);
                    *self = self.mask();
                }
            }

            impl<T, const N: usize> ShlAssign<T> for $name<N>
            where
                $type: ShlAssign<T>,
            {
                fn shl_assign(&mut self, rhs: T) {
                    self.0.shl_assign(rhs);
                    *self = self.mask();
                }
            }

            impl<const N: usize> BitOr<$name<N>> for $name<N> {
                type Output = $name<N>;

                fn bitor(self, rhs: Self) -> Self::Output {
                    Self(self.0.bitor(rhs.0)).mask()
                }
            }

            impl<'a, const N: usize> BitOr<&'a $name<N>> for $name<N> {
                type Output = <$name<N> as BitOr<$name<N>>>::Output;

                fn bitor(self, rhs: &'a $name<N>) -> Self::Output {
                    $name::<N>(self.0.bitor(rhs.0)).mask()
                }
            }

            impl<'a, const N: usize> BitOr<$name<N>> for &'a $name<N> {
                type Output = <$name<N> as BitOr<$name<N>>>::Output;

                fn bitor(self, rhs: $name<N>) -> Self::Output {
                    $name::<N>(self.0.bitor(rhs.0)).mask()
                }
            }

            impl<'a, const N: usize> BitOr<&'a $name<N>> for &'a $name<N> {
                type Output = <$name<N> as BitOr<$name<N>>>::Output;

                fn bitor(self, rhs: &'a $name<N>) -> Self::Output {
                    $name::<N>(self.0.bitor(rhs.0)).mask()
                }
            }

            impl<const N: usize> BitOrAssign<$name<N>> for $name<N> {
                fn bitor_assign(&mut self, other: Self) {
                    self.0.bitor_assign(other.0);
                    *self = self.mask();
                }
            }

            impl<const N: usize> BitXor<$name<N>> for $name<N> {
                type Output = $name<N>;

                fn bitxor(self, rhs: $name<N>) -> Self::Output {
                    $name::<N>(self.0.bitxor(rhs.0)).mask()
                }
            }

            impl<'a, const N: usize> BitXor<&'a $name<N>> for $name<N> {
                type Output = <$name<N> as BitOr<$name<N>>>::Output;

                fn bitxor(self, rhs: &'a $name<N>) -> Self::Output {
                    $name::<N>(self.0.bitxor(rhs.0)).mask()
                }
            }

            impl<'a, const N: usize> BitXor<$name<N>> for &'a $name<N> {
                type Output = <$name<N> as BitOr<$name<N>>>::Output;

                fn bitxor(self, rhs: $name<N>) -> Self::Output {
                    $name::<N>(self.0.bitxor(rhs.0)).mask()
                }
            }

            impl<'a, const N: usize> BitXor<&'a $name<N>> for &'a $name<N> {
                type Output = <$name<N> as BitOr<$name<N>>>::Output;

                fn bitxor(self, rhs: &'a $name<N>) -> Self::Output {
                    $name::<N>(self.0.bitxor(rhs.0)).mask()
                }
            }

            impl<const N: usize> BitXorAssign<$name<N>> for $name<N> {
                fn bitxor_assign(&mut self, other: $name<N>) {
                    self.0.bitxor_assign(other.0);
                    *self = self.mask();
                }
            }

            impl<const N: usize> Not for $name<N> {
                type Output = Self;

                fn not(self) -> Self {
                    Self(self.0.not()).mask()
                }
            }

            impl<'a, const N: usize> Not for &'a $name<N> {
                type Output = <$name<N> as Not>::Output;

                fn not(self) -> $name<N> {
                    $name::<N>(self.0.not()).mask()
                }
            }

            impl<const N: usize> BitAnd<$name<N>> for $name<N> {
                type Output = $name<N>;

                fn bitand(self, rhs: $name<N>) -> Self::Output {
                    $name::<N>(self.0.bitand(rhs.0)).mask()
                }
            }

            impl<'a, const N: usize> BitAnd<&'a $name<N>> for $name<N> {
                type Output = <$name<N> as BitOr<$name<N>>>::Output;

                fn bitand(self, rhs: &'a $name<N>) -> Self::Output {
                    $name::<N>(self.0.bitand(rhs.0)).mask()
                }
            }

            impl<'a, const N: usize> BitAnd<$name<N>> for &'a $name<N> {
                type Output = <$name<N> as BitOr<$name<N>>>::Output;

                fn bitand(self, rhs: $name<N>) -> Self::Output {
                    $name::<N>(self.0.bitand(rhs.0)).mask()
                }
            }

            impl<'a, const N: usize> BitAnd<&'a $name<N>> for &'a $name<N> {
                type Output = <$name<N> as BitOr<$name<N>>>::Output;

                fn bitand(self, rhs: &'a $name<N>) -> Self::Output {
                    $name::<N>(self.0.bitand(rhs.0)).mask()
                }
            }

            impl<const N: usize> BitAndAssign<$name<N>> for $name<N> {
                fn bitand_assign(&mut self, other: $name<N>) {
                    self.0.bitand_assign(other.0);
                    *self = self.mask();
                }
            }

            impl<const N: usize> Add for $name<N> {
                type Output = Self;
                #[allow(unused_comparisons)]
                fn add(self, other: Self) -> Self {
                    if self.0 > 0 && other.0 > 0 {
                        debug_assert!(Self::MAX.0 - other.0 >= self.0);
                    } else if self.0 < 0 && other.0 < 0 {
                        debug_assert!(Self::MIN.0 - other.0 <= self.0);
                    }
                    self.wrapping_add(other)
                }
            }

            impl<const N: usize> Sub for $name<N> {
                type Output = Self;
                #[allow(unused_comparisons)]
                fn sub(self, other: Self) -> Self {
                    if self > other {
                        debug_assert!(Self::MAX.0 + other.0 >= self.0);
                    } else if self < other {
                        debug_assert!(Self::MIN.0 + other.0 <= self.0);
                    }
                    self.wrapping_sub(other)
                }
            }
        };
    }

    macro_rules! impl_as {
    ($name:ident, $base:ty, [$($other:ident,)*]) => {
        $(impl<const N: usize, const M: usize> As<$other<M>> for $name<N> {
            type Output = Self;

            fn as_(value: $other<M>) -> Self {
                Self(value.0 as $base).mask()
            }
        })*
    }}

    macro_rules! impl_from {
    ($name:ident, $bits:literal, $other:ident, [$($other_bits:literal,)*]) => {
        $(impl From<$other<$other_bits>> for $name<$bits> {
            fn from(value: $other<$other_bits>) -> Self {
                Self(value.0.into()).mask()
            }
        })*
    }}

    impl_uint!(UInt8Impl, u8, 8, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_uint!(UInt16Impl, u16, 16, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_uint!(
        UInt32Impl,
        u32,
        32,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_uint!(
        UInt64Impl,
        u64,
        64,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_uint!(
        UInt128Impl,
        u128,
        128,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
            124, 125, 126, 127, 128,
        ]
    );

    impl_int!(Int8Impl, i8, 8, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_int!(Int16Impl, i16, 16, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_int!(
        Int32Impl,
        i32,
        32,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_int!(
        Int64Impl,
        i64,
        64,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_int!(
        Int128Impl,
        i128,
        128,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
            124, 125, 126, 127, 128,
        ]
    );

    impl_as!(
        UInt8Impl,
        u8,
        [
            UInt8Impl,
            UInt16Impl,
            UInt32Impl,
            UInt64Impl,
            UInt128Impl,
            Int8Impl,
            Int16Impl,
            Int32Impl,
            Int64Impl,
            Int128Impl,
        ]
    );
    impl_as!(
        UInt16Impl,
        u16,
        [
            UInt8Impl,
            UInt16Impl,
            UInt32Impl,
            UInt64Impl,
            UInt128Impl,
            Int8Impl,
            Int16Impl,
            Int32Impl,
            Int64Impl,
            Int128Impl,
        ]
    );
    impl_as!(
        UInt32Impl,
        u32,
        [
            UInt8Impl,
            UInt16Impl,
            UInt32Impl,
            UInt64Impl,
            UInt128Impl,
            Int8Impl,
            Int16Impl,
            Int32Impl,
            Int64Impl,
            Int128Impl,
        ]
    );
    impl_as!(
        UInt64Impl,
        u64,
        [
            UInt8Impl,
            UInt16Impl,
            UInt32Impl,
            UInt64Impl,
            UInt128Impl,
            Int8Impl,
            Int16Impl,
            Int32Impl,
            Int64Impl,
            Int128Impl,
        ]
    );
    impl_as!(
        UInt128Impl,
        u128,
        [
            UInt8Impl,
            UInt16Impl,
            UInt32Impl,
            UInt64Impl,
            UInt128Impl,
            Int8Impl,
            Int16Impl,
            Int32Impl,
            Int64Impl,
            Int128Impl,
        ]
    );

    impl_as!(
        Int8Impl,
        i8,
        [
            UInt8Impl,
            UInt16Impl,
            UInt32Impl,
            UInt64Impl,
            UInt128Impl,
            Int8Impl,
            Int16Impl,
            Int32Impl,
            Int64Impl,
            Int128Impl,
        ]
    );
    impl_as!(
        Int16Impl,
        i16,
        [
            UInt8Impl,
            UInt16Impl,
            UInt32Impl,
            UInt64Impl,
            UInt128Impl,
            Int8Impl,
            Int16Impl,
            Int32Impl,
            Int64Impl,
            Int128Impl,
        ]
    );
    impl_as!(
        Int32Impl,
        i32,
        [
            UInt8Impl,
            UInt16Impl,
            UInt32Impl,
            UInt64Impl,
            UInt128Impl,
            Int8Impl,
            Int16Impl,
            Int32Impl,
            Int64Impl,
            Int128Impl,
        ]
    );
    impl_as!(
        Int64Impl,
        i64,
        [
            UInt8Impl,
            UInt16Impl,
            UInt32Impl,
            UInt64Impl,
            UInt128Impl,
            Int8Impl,
            Int16Impl,
            Int32Impl,
            Int64Impl,
            Int128Impl,
        ]
    );
    impl_as!(
        Int128Impl,
        i128,
        [
            UInt8Impl,
            UInt16Impl,
            UInt32Impl,
            UInt64Impl,
            UInt128Impl,
            Int8Impl,
            Int16Impl,
            Int32Impl,
            Int64Impl,
            Int128Impl,
        ]
    );

    // UInt8Impl::from()
    impl_from!(UInt8Impl, 1, UInt8Impl, [0,]);
    impl_from!(UInt8Impl, 2, UInt8Impl, [0, 1,]);
    impl_from!(UInt8Impl, 3, UInt8Impl, [0, 1, 2,]);
    impl_from!(UInt8Impl, 4, UInt8Impl, [0, 1, 2, 3,]);
    impl_from!(UInt8Impl, 5, UInt8Impl, [0, 1, 2, 3, 4,]);
    impl_from!(UInt8Impl, 6, UInt8Impl, [0, 1, 2, 3, 4, 5,]);
    impl_from!(UInt8Impl, 7, UInt8Impl, [0, 1, 2, 3, 4, 5, 6,]);
    impl_from!(UInt8Impl, 8, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7,]);

    // UInt16Impl::from()
    impl_from!(UInt16Impl, 9, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt16Impl, 10, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt16Impl, 10, UInt16Impl, [9,]);
    impl_from!(UInt16Impl, 11, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt16Impl, 11, UInt16Impl, [9, 10,]);
    impl_from!(UInt16Impl, 12, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt16Impl, 12, UInt16Impl, [9, 10, 11,]);
    impl_from!(UInt16Impl, 13, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt16Impl, 13, UInt16Impl, [9, 10, 11, 12,]);
    impl_from!(UInt16Impl, 14, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt16Impl, 14, UInt16Impl, [9, 10, 11, 12, 13,]);
    impl_from!(UInt16Impl, 15, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt16Impl, 15, UInt16Impl, [9, 10, 11, 12, 13, 14,]);
    impl_from!(UInt16Impl, 16, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt16Impl, 16, UInt16Impl, [9, 10, 11, 12, 13, 14, 15,]);

    // UInt32Impl::from()
    impl_from!(UInt32Impl, 17, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 17, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(UInt32Impl, 18, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 18, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(UInt32Impl, 18, UInt32Impl, [17,]);
    impl_from!(UInt32Impl, 19, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 19, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(UInt32Impl, 19, UInt32Impl, [17, 18,]);
    impl_from!(UInt32Impl, 20, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 20, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(UInt32Impl, 20, UInt32Impl, [17, 18, 19,]);
    impl_from!(UInt32Impl, 21, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 21, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(UInt32Impl, 21, UInt32Impl, [17, 18, 19, 20,]);
    impl_from!(UInt32Impl, 22, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 22, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(UInt32Impl, 22, UInt32Impl, [17, 18, 19, 20, 21,]);
    impl_from!(UInt32Impl, 23, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 23, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(UInt32Impl, 23, UInt32Impl, [17, 18, 19, 20, 21, 22,]);
    impl_from!(UInt32Impl, 24, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 24, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(UInt32Impl, 24, UInt32Impl, [17, 18, 19, 20, 21, 22, 23,]);
    impl_from!(UInt32Impl, 25, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 25, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt32Impl,
        25,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24,]
    );
    impl_from!(UInt32Impl, 26, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 26, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt32Impl,
        26,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25,]
    );
    impl_from!(UInt32Impl, 27, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 27, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt32Impl,
        27,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26,]
    );
    impl_from!(UInt32Impl, 28, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 28, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt32Impl,
        28,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,]
    );
    impl_from!(UInt32Impl, 29, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 29, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt32Impl,
        29,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,]
    );
    impl_from!(UInt32Impl, 30, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 30, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt32Impl,
        30,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,]
    );
    impl_from!(UInt32Impl, 31, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 31, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt32Impl,
        31,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,]
    );
    impl_from!(UInt32Impl, 32, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt32Impl, 32, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt32Impl,
        32,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,]
    );

    // UInt64Impl::from()
    impl_from!(UInt64Impl, 33, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 33, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        33,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(UInt64Impl, 34, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 34, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        34,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(UInt64Impl, 34, UInt64Impl, [33,]);
    impl_from!(UInt64Impl, 35, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 35, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        35,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(UInt64Impl, 35, UInt64Impl, [33, 34,]);
    impl_from!(UInt64Impl, 36, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 36, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        36,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(UInt64Impl, 36, UInt64Impl, [33, 34, 35,]);
    impl_from!(UInt64Impl, 37, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 37, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        37,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(UInt64Impl, 37, UInt64Impl, [33, 34, 35, 36,]);
    impl_from!(UInt64Impl, 38, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 38, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        38,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(UInt64Impl, 38, UInt64Impl, [33, 34, 35, 36, 37,]);
    impl_from!(UInt64Impl, 39, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 39, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        39,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(UInt64Impl, 39, UInt64Impl, [33, 34, 35, 36, 37, 38,]);
    impl_from!(UInt64Impl, 40, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 40, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        40,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(UInt64Impl, 40, UInt64Impl, [33, 34, 35, 36, 37, 38, 39,]);
    impl_from!(UInt64Impl, 41, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 41, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        41,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        41,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40,]
    );
    impl_from!(UInt64Impl, 42, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 42, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        42,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        42,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41,]
    );
    impl_from!(UInt64Impl, 43, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 43, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        43,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        43,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42,]
    );
    impl_from!(UInt64Impl, 44, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 44, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        44,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        44,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43,]
    );
    impl_from!(UInt64Impl, 45, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 45, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        45,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        45,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44,]
    );
    impl_from!(UInt64Impl, 46, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 46, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        46,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        46,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,]
    );
    impl_from!(UInt64Impl, 47, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 47, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        47,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        47,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46,]
    );
    impl_from!(UInt64Impl, 48, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 48, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        48,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        48,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,]
    );
    impl_from!(UInt64Impl, 49, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 49, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        49,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        49,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,]
    );
    impl_from!(UInt64Impl, 50, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 50, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        50,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        50,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,]
    );
    impl_from!(UInt64Impl, 51, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 51, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        51,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        51,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,]
    );
    impl_from!(UInt64Impl, 52, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 52, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        52,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        52,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,]
    );
    impl_from!(UInt64Impl, 53, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 53, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        53,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        53,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,]
    );
    impl_from!(UInt64Impl, 54, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 54, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        54,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        54,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53,]
    );
    impl_from!(UInt64Impl, 55, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 55, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        55,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        55,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,]
    );
    impl_from!(UInt64Impl, 56, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 56, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        56,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        56,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55,
        ]
    );
    impl_from!(UInt64Impl, 57, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 57, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        57,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        57,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56,
        ]
    );
    impl_from!(UInt64Impl, 58, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 58, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        58,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        58,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57,
        ]
    );
    impl_from!(UInt64Impl, 59, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 59, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        59,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        59,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58,
        ]
    );
    impl_from!(UInt64Impl, 60, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 60, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        60,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        60,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59,
        ]
    );
    impl_from!(UInt64Impl, 61, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 61, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        61,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        61,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60,
        ]
    );
    impl_from!(UInt64Impl, 62, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 62, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        62,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        62,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61,
        ]
    );
    impl_from!(UInt64Impl, 63, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 63, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        63,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        63,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62,
        ]
    );
    impl_from!(UInt64Impl, 64, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(UInt64Impl, 64, UInt16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        UInt64Impl,
        64,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt64Impl,
        64,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63,
        ]
    );

    // UInt128Impl::from()
    impl_from!(UInt128Impl, 65, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        65,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        65,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        65,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(UInt128Impl, 66, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        66,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        66,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        66,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(UInt128Impl, 66, UInt128Impl, [65,]);
    impl_from!(UInt128Impl, 67, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        67,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        67,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        67,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(UInt128Impl, 67, UInt128Impl, [65, 66,]);
    impl_from!(UInt128Impl, 68, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        68,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        68,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        68,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(UInt128Impl, 68, UInt128Impl, [65, 66, 67,]);
    impl_from!(UInt128Impl, 69, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        69,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        69,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        69,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(UInt128Impl, 69, UInt128Impl, [65, 66, 67, 68,]);
    impl_from!(UInt128Impl, 70, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        70,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        70,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        70,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(UInt128Impl, 70, UInt128Impl, [65, 66, 67, 68, 69,]);
    impl_from!(UInt128Impl, 71, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        71,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        71,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        71,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(UInt128Impl, 71, UInt128Impl, [65, 66, 67, 68, 69, 70,]);
    impl_from!(UInt128Impl, 72, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        72,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        72,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        72,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(UInt128Impl, 72, UInt128Impl, [65, 66, 67, 68, 69, 70, 71,]);
    impl_from!(UInt128Impl, 73, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        73,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        73,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        73,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        73,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72,]
    );
    impl_from!(UInt128Impl, 74, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        74,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        74,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        74,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        74,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73,]
    );
    impl_from!(UInt128Impl, 75, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        75,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        75,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        75,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        75,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74,]
    );
    impl_from!(UInt128Impl, 76, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        76,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        76,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        76,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        76,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,]
    );
    impl_from!(UInt128Impl, 77, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        77,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        77,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        77,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        77,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,]
    );
    impl_from!(UInt128Impl, 78, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        78,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        78,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        78,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        78,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77,]
    );
    impl_from!(UInt128Impl, 79, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        79,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        79,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        79,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        79,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78,]
    );
    impl_from!(UInt128Impl, 80, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        80,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        80,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        80,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        80,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,]
    );
    impl_from!(UInt128Impl, 81, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        81,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        81,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        81,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        81,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,]
    );
    impl_from!(UInt128Impl, 82, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        82,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        82,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        82,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        82,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81,]
    );
    impl_from!(UInt128Impl, 83, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        83,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        83,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        83,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        83,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,]
    );
    impl_from!(UInt128Impl, 84, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        84,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        84,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        84,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        84,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83,]
    );
    impl_from!(UInt128Impl, 85, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        85,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        85,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        85,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        85,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84,]
    );
    impl_from!(UInt128Impl, 86, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        86,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        86,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        86,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        86,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85,]
    );
    impl_from!(UInt128Impl, 87, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        87,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        87,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        87,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        87,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,]
    );
    impl_from!(UInt128Impl, 88, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        88,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        88,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        88,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        88,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87,
        ]
    );
    impl_from!(UInt128Impl, 89, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        89,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        89,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        89,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        89,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88,
        ]
    );
    impl_from!(UInt128Impl, 90, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        90,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        90,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        90,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        90,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89,
        ]
    );
    impl_from!(UInt128Impl, 91, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        91,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        91,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        91,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        91,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90,
        ]
    );
    impl_from!(UInt128Impl, 92, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        92,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        92,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        92,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        92,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91,
        ]
    );
    impl_from!(UInt128Impl, 93, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        93,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        93,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        93,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        93,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92,
        ]
    );
    impl_from!(UInt128Impl, 94, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        94,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        94,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        94,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        94,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93,
        ]
    );
    impl_from!(UInt128Impl, 95, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        95,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        95,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        95,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        95,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94,
        ]
    );
    impl_from!(UInt128Impl, 96, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        96,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        96,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        96,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        96,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95,
        ]
    );
    impl_from!(UInt128Impl, 97, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        97,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        97,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        97,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        97,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96,
        ]
    );
    impl_from!(UInt128Impl, 98, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        98,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        98,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        98,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        98,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97,
        ]
    );
    impl_from!(UInt128Impl, 99, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        99,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        99,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        99,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        99,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,
        ]
    );
    impl_from!(UInt128Impl, 100, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        100,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        100,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        100,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        100,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
        ]
    );
    impl_from!(UInt128Impl, 101, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        101,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        101,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        101,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        101,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100,
        ]
    );
    impl_from!(UInt128Impl, 102, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        102,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        102,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        102,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        102,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101,
        ]
    );
    impl_from!(UInt128Impl, 103, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        103,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        103,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        103,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        103,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102,
        ]
    );
    impl_from!(UInt128Impl, 104, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        104,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        104,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        104,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        104,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103,
        ]
    );
    impl_from!(UInt128Impl, 105, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        105,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        105,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        105,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        105,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104,
        ]
    );
    impl_from!(UInt128Impl, 106, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        106,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        106,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        106,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        106,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105,
        ]
    );
    impl_from!(UInt128Impl, 107, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        107,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        107,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        107,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        107,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
        ]
    );
    impl_from!(UInt128Impl, 108, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        108,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        108,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        108,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        108,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107,
        ]
    );
    impl_from!(UInt128Impl, 109, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        109,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        109,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        109,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        109,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108,
        ]
    );
    impl_from!(UInt128Impl, 110, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        110,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        110,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        110,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        110,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109,
        ]
    );
    impl_from!(UInt128Impl, 111, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        111,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        111,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        111,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        111,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110,
        ]
    );
    impl_from!(UInt128Impl, 112, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        112,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        112,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        112,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        112,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111,
        ]
    );
    impl_from!(UInt128Impl, 113, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        113,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        113,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        113,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        113,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112,
        ]
    );
    impl_from!(UInt128Impl, 114, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        114,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        114,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        114,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        114,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113,
        ]
    );
    impl_from!(UInt128Impl, 115, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        115,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        115,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        115,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        115,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114,
        ]
    );
    impl_from!(UInt128Impl, 116, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        116,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        116,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        116,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        116,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115,
        ]
    );
    impl_from!(UInt128Impl, 117, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        117,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        117,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        117,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        117,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116,
        ]
    );
    impl_from!(UInt128Impl, 118, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        118,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        118,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        118,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        118,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
        ]
    );
    impl_from!(UInt128Impl, 119, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        119,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        119,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        119,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        119,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118,
        ]
    );
    impl_from!(UInt128Impl, 120, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        120,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        120,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        120,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        120,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
        ]
    );
    impl_from!(UInt128Impl, 121, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        121,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        121,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        121,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        121,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120,
        ]
    );
    impl_from!(UInt128Impl, 122, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        122,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        122,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        122,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        122,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
        ]
    );
    impl_from!(UInt128Impl, 123, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        123,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        123,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        123,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        123,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122,
        ]
    );
    impl_from!(UInt128Impl, 124, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        124,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        124,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        124,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        124,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
        ]
    );
    impl_from!(UInt128Impl, 125, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        125,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        125,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        125,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        125,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
            124,
        ]
    );
    impl_from!(UInt128Impl, 126, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        126,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        126,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        126,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        126,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
            124, 125,
        ]
    );
    impl_from!(UInt128Impl, 127, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        127,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        127,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        127,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        127,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
            124, 125, 126,
        ]
    );
    impl_from!(UInt128Impl, 128, UInt8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(
        UInt128Impl,
        128,
        UInt16Impl,
        [9, 10, 11, 12, 13, 14, 15, 16,]
    );
    impl_from!(
        UInt128Impl,
        128,
        UInt32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        UInt128Impl,
        128,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        UInt128Impl,
        128,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
            124, 125, 126, 127,
        ]
    );

    // Int8Impl::from()
    impl_from!(Int8Impl, 1, Int8Impl, [0,]);
    impl_from!(Int8Impl, 2, Int8Impl, [0, 1,]);
    impl_from!(Int8Impl, 3, Int8Impl, [0, 1, 2,]);
    impl_from!(Int8Impl, 4, Int8Impl, [0, 1, 2, 3,]);
    impl_from!(Int8Impl, 5, Int8Impl, [0, 1, 2, 3, 4,]);
    impl_from!(Int8Impl, 6, Int8Impl, [0, 1, 2, 3, 4, 5,]);
    impl_from!(Int8Impl, 7, Int8Impl, [0, 1, 2, 3, 4, 5, 6,]);
    impl_from!(Int8Impl, 8, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7,]);

    // Int16Impl::from()
    impl_from!(Int16Impl, 9, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int16Impl, 10, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int16Impl, 10, Int16Impl, [9,]);
    impl_from!(Int16Impl, 11, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int16Impl, 11, Int16Impl, [9, 10,]);
    impl_from!(Int16Impl, 12, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int16Impl, 12, Int16Impl, [9, 10, 11,]);
    impl_from!(Int16Impl, 13, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int16Impl, 13, Int16Impl, [9, 10, 11, 12,]);
    impl_from!(Int16Impl, 14, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int16Impl, 14, Int16Impl, [9, 10, 11, 12, 13,]);
    impl_from!(Int16Impl, 15, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int16Impl, 15, Int16Impl, [9, 10, 11, 12, 13, 14,]);
    impl_from!(Int16Impl, 16, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int16Impl, 16, Int16Impl, [9, 10, 11, 12, 13, 14, 15,]);

    // Int32Impl::from()
    impl_from!(Int32Impl, 17, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 17, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(Int32Impl, 18, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 18, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(Int32Impl, 18, Int32Impl, [17,]);
    impl_from!(Int32Impl, 19, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 19, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(Int32Impl, 19, Int32Impl, [17, 18,]);
    impl_from!(Int32Impl, 20, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 20, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(Int32Impl, 20, Int32Impl, [17, 18, 19,]);
    impl_from!(Int32Impl, 21, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 21, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(Int32Impl, 21, Int32Impl, [17, 18, 19, 20,]);
    impl_from!(Int32Impl, 22, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 22, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(Int32Impl, 22, Int32Impl, [17, 18, 19, 20, 21,]);
    impl_from!(Int32Impl, 23, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 23, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(Int32Impl, 23, Int32Impl, [17, 18, 19, 20, 21, 22,]);
    impl_from!(Int32Impl, 24, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 24, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(Int32Impl, 24, Int32Impl, [17, 18, 19, 20, 21, 22, 23,]);
    impl_from!(Int32Impl, 25, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 25, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(Int32Impl, 25, Int32Impl, [17, 18, 19, 20, 21, 22, 23, 24,]);
    impl_from!(Int32Impl, 26, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 26, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int32Impl,
        26,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25,]
    );
    impl_from!(Int32Impl, 27, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 27, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int32Impl,
        27,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26,]
    );
    impl_from!(Int32Impl, 28, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 28, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int32Impl,
        28,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,]
    );
    impl_from!(Int32Impl, 29, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 29, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int32Impl,
        29,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,]
    );
    impl_from!(Int32Impl, 30, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 30, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int32Impl,
        30,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,]
    );
    impl_from!(Int32Impl, 31, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 31, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int32Impl,
        31,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,]
    );
    impl_from!(Int32Impl, 32, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int32Impl, 32, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int32Impl,
        32,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,]
    );

    // Int64Impl::from()
    impl_from!(Int64Impl, 33, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 33, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        33,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(Int64Impl, 34, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 34, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        34,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(Int64Impl, 34, Int64Impl, [33,]);
    impl_from!(Int64Impl, 35, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 35, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        35,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(Int64Impl, 35, Int64Impl, [33, 34,]);
    impl_from!(Int64Impl, 36, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 36, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        36,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(Int64Impl, 36, Int64Impl, [33, 34, 35,]);
    impl_from!(Int64Impl, 37, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 37, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        37,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(Int64Impl, 37, Int64Impl, [33, 34, 35, 36,]);
    impl_from!(Int64Impl, 38, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 38, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        38,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(Int64Impl, 38, Int64Impl, [33, 34, 35, 36, 37,]);
    impl_from!(Int64Impl, 39, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 39, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        39,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(Int64Impl, 39, Int64Impl, [33, 34, 35, 36, 37, 38,]);
    impl_from!(Int64Impl, 40, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 40, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        40,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(Int64Impl, 40, Int64Impl, [33, 34, 35, 36, 37, 38, 39,]);
    impl_from!(Int64Impl, 41, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 41, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        41,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(Int64Impl, 41, Int64Impl, [33, 34, 35, 36, 37, 38, 39, 40,]);
    impl_from!(Int64Impl, 42, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 42, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        42,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        42,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41,]
    );
    impl_from!(Int64Impl, 43, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 43, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        43,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        43,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42,]
    );
    impl_from!(Int64Impl, 44, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 44, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        44,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        44,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43,]
    );
    impl_from!(Int64Impl, 45, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 45, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        45,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        45,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44,]
    );
    impl_from!(Int64Impl, 46, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 46, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        46,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        46,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,]
    );
    impl_from!(Int64Impl, 47, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 47, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        47,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        47,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46,]
    );
    impl_from!(Int64Impl, 48, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 48, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        48,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        48,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,]
    );
    impl_from!(Int64Impl, 49, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 49, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        49,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        49,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,]
    );
    impl_from!(Int64Impl, 50, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 50, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        50,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        50,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,]
    );
    impl_from!(Int64Impl, 51, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 51, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        51,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        51,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,]
    );
    impl_from!(Int64Impl, 52, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 52, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        52,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        52,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,]
    );
    impl_from!(Int64Impl, 53, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 53, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        53,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        53,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,]
    );
    impl_from!(Int64Impl, 54, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 54, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        54,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        54,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53,]
    );
    impl_from!(Int64Impl, 55, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 55, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        55,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        55,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,]
    );
    impl_from!(Int64Impl, 56, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 56, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        56,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        56,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55,
        ]
    );
    impl_from!(Int64Impl, 57, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 57, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        57,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        57,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56,
        ]
    );
    impl_from!(Int64Impl, 58, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 58, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        58,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        58,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57,
        ]
    );
    impl_from!(Int64Impl, 59, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 59, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        59,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        59,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58,
        ]
    );
    impl_from!(Int64Impl, 60, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 60, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        60,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        60,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59,
        ]
    );
    impl_from!(Int64Impl, 61, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 61, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        61,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        61,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60,
        ]
    );
    impl_from!(Int64Impl, 62, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 62, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        62,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        62,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61,
        ]
    );
    impl_from!(Int64Impl, 63, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 63, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        63,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        63,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62,
        ]
    );
    impl_from!(Int64Impl, 64, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int64Impl, 64, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int64Impl,
        64,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int64Impl,
        64,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63,
        ]
    );

    // Int128Impl::from()
    impl_from!(Int128Impl, 65, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 65, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        65,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        65,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(Int128Impl, 66, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 66, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        66,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        66,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(Int128Impl, 66, Int128Impl, [65,]);
    impl_from!(Int128Impl, 67, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 67, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        67,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        67,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(Int128Impl, 67, Int128Impl, [65, 66,]);
    impl_from!(Int128Impl, 68, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 68, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        68,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        68,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(Int128Impl, 68, Int128Impl, [65, 66, 67,]);
    impl_from!(Int128Impl, 69, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 69, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        69,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        69,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(Int128Impl, 69, Int128Impl, [65, 66, 67, 68,]);
    impl_from!(Int128Impl, 70, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 70, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        70,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        70,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(Int128Impl, 70, Int128Impl, [65, 66, 67, 68, 69,]);
    impl_from!(Int128Impl, 71, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 71, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        71,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        71,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(Int128Impl, 71, Int128Impl, [65, 66, 67, 68, 69, 70,]);
    impl_from!(Int128Impl, 72, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 72, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        72,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        72,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(Int128Impl, 72, Int128Impl, [65, 66, 67, 68, 69, 70, 71,]);
    impl_from!(Int128Impl, 73, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 73, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        73,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        73,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        73,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72,]
    );
    impl_from!(Int128Impl, 74, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 74, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        74,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        74,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        74,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73,]
    );
    impl_from!(Int128Impl, 75, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 75, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        75,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        75,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        75,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74,]
    );
    impl_from!(Int128Impl, 76, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 76, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        76,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        76,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        76,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,]
    );
    impl_from!(Int128Impl, 77, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 77, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        77,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        77,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        77,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,]
    );
    impl_from!(Int128Impl, 78, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 78, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        78,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        78,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        78,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77,]
    );
    impl_from!(Int128Impl, 79, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 79, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        79,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        79,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        79,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78,]
    );
    impl_from!(Int128Impl, 80, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 80, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        80,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        80,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        80,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,]
    );
    impl_from!(Int128Impl, 81, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 81, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        81,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        81,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        81,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,]
    );
    impl_from!(Int128Impl, 82, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 82, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        82,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        82,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        82,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81,]
    );
    impl_from!(Int128Impl, 83, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 83, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        83,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        83,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        83,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,]
    );
    impl_from!(Int128Impl, 84, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 84, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        84,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        84,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        84,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83,]
    );
    impl_from!(Int128Impl, 85, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 85, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        85,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        85,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        85,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84,]
    );
    impl_from!(Int128Impl, 86, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 86, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        86,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        86,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        86,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85,]
    );
    impl_from!(Int128Impl, 87, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 87, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        87,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        87,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        87,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,]
    );
    impl_from!(Int128Impl, 88, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 88, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        88,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        88,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        88,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87,
        ]
    );
    impl_from!(Int128Impl, 89, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 89, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        89,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        89,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        89,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88,
        ]
    );
    impl_from!(Int128Impl, 90, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 90, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        90,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        90,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        90,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89,
        ]
    );
    impl_from!(Int128Impl, 91, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 91, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        91,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        91,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        91,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90,
        ]
    );
    impl_from!(Int128Impl, 92, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 92, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        92,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        92,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        92,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91,
        ]
    );
    impl_from!(Int128Impl, 93, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 93, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        93,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        93,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        93,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92,
        ]
    );
    impl_from!(Int128Impl, 94, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 94, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        94,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        94,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        94,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93,
        ]
    );
    impl_from!(Int128Impl, 95, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 95, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        95,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        95,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        95,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94,
        ]
    );
    impl_from!(Int128Impl, 96, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 96, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        96,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        96,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        96,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95,
        ]
    );
    impl_from!(Int128Impl, 97, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 97, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        97,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        97,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        97,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96,
        ]
    );
    impl_from!(Int128Impl, 98, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 98, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        98,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        98,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        98,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97,
        ]
    );
    impl_from!(Int128Impl, 99, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 99, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        99,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        99,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        99,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,
        ]
    );
    impl_from!(Int128Impl, 100, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 100, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        100,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        100,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        100,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
        ]
    );
    impl_from!(Int128Impl, 101, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 101, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        101,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        101,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        101,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100,
        ]
    );
    impl_from!(Int128Impl, 102, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 102, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        102,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        102,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        102,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101,
        ]
    );
    impl_from!(Int128Impl, 103, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 103, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        103,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        103,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        103,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102,
        ]
    );
    impl_from!(Int128Impl, 104, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 104, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        104,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        104,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        104,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103,
        ]
    );
    impl_from!(Int128Impl, 105, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 105, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        105,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        105,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        105,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104,
        ]
    );
    impl_from!(Int128Impl, 106, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 106, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        106,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        106,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        106,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105,
        ]
    );
    impl_from!(Int128Impl, 107, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 107, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        107,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        107,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        107,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
        ]
    );
    impl_from!(Int128Impl, 108, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 108, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        108,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        108,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        108,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107,
        ]
    );
    impl_from!(Int128Impl, 109, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 109, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        109,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        109,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        109,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108,
        ]
    );
    impl_from!(Int128Impl, 110, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 110, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        110,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        110,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        110,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109,
        ]
    );
    impl_from!(Int128Impl, 111, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 111, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        111,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        111,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        111,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110,
        ]
    );
    impl_from!(Int128Impl, 112, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 112, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        112,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        112,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        112,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111,
        ]
    );
    impl_from!(Int128Impl, 113, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 113, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        113,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        113,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        113,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112,
        ]
    );
    impl_from!(Int128Impl, 114, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 114, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        114,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        114,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        114,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113,
        ]
    );
    impl_from!(Int128Impl, 115, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 115, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        115,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        115,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        115,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114,
        ]
    );
    impl_from!(Int128Impl, 116, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 116, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        116,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        116,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        116,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115,
        ]
    );
    impl_from!(Int128Impl, 117, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 117, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        117,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        117,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        117,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116,
        ]
    );
    impl_from!(Int128Impl, 118, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 118, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        118,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        118,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        118,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
        ]
    );
    impl_from!(Int128Impl, 119, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 119, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        119,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        119,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        119,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118,
        ]
    );
    impl_from!(Int128Impl, 120, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 120, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        120,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        120,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        120,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
        ]
    );
    impl_from!(Int128Impl, 121, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 121, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        121,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        121,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        121,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120,
        ]
    );
    impl_from!(Int128Impl, 122, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 122, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        122,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        122,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        122,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
        ]
    );
    impl_from!(Int128Impl, 123, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 123, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        123,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        123,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        123,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122,
        ]
    );
    impl_from!(Int128Impl, 124, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 124, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        124,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        124,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        124,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
        ]
    );
    impl_from!(Int128Impl, 125, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 125, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        125,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        125,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        125,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
            124,
        ]
    );
    impl_from!(Int128Impl, 126, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 126, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        126,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        126,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        126,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
            124, 125,
        ]
    );
    impl_from!(Int128Impl, 127, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 127, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        127,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        127,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        127,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
            124, 125, 126,
        ]
    );
    impl_from!(Int128Impl, 128, Int8Impl, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_from!(Int128Impl, 128, Int16Impl, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_from!(
        Int128Impl,
        128,
        Int32Impl,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]
    );
    impl_from!(
        Int128Impl,
        128,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 58, 59, 60, 61, 62, 63, 64,
        ]
    );
    impl_from!(
        Int128Impl,
        128,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
            124, 125, 126, 127,
        ]
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn min_max_values() {
        assert_eq!(uint::<0>::MAX, uint::<0>::new(0));
        assert_eq!(uint::<1>::MAX, uint::<1>::new(1));
        assert_eq!(uint::<2>::MAX, uint::<2>::new(3));
        assert_eq!(uint::<3>::MAX, uint::<3>::new(7));
        assert_eq!(uint::<7>::MAX, uint::<7>::new(127));
        assert_eq!(uint::<8>::MAX, uint::<8>::new(255));
        assert_eq!(uint::<9>::MAX, uint::<9>::new(511));

        assert_eq!(int::<0>::MAX, int::<0>::new(0));
        assert_eq!(int::<1>::MAX, int::<1>::new(0));
        assert_eq!(int::<2>::MAX, int::<2>::new(1));
        assert_eq!(int::<3>::MAX, int::<3>::new(3));
        assert_eq!(int::<7>::MAX, int::<7>::new(63));
        assert_eq!(int::<8>::MAX, int::<8>::new(127));
        assert_eq!(int::<9>::MAX, int::<9>::new(255));

        assert_eq!(uint::<0>::MIN, uint::<0>::new(0));
        assert_eq!(uint::<1>::MIN, uint::<1>::new(0));
        assert_eq!(uint::<2>::MIN, uint::<2>::new(0));
        assert_eq!(uint::<3>::MIN, uint::<3>::new(0));
        assert_eq!(uint::<7>::MIN, uint::<7>::new(0));
        assert_eq!(uint::<8>::MIN, uint::<8>::new(0));
        assert_eq!(uint::<9>::MIN, uint::<9>::new(0));
        assert_eq!(uint::<127>::MIN, uint::<127>::new(0));

        assert_eq!(int::<0>::MIN, int::<0>::new(0));
        assert_eq!(int::<1>::MIN, int::<1>::new(-1));
        assert_eq!(int::<2>::MIN, int::<2>::new(-2));
        assert_eq!(int::<3>::MIN, int::<3>::new(-4));
        assert_eq!(int::<7>::MIN, int::<7>::new(-64));
        assert_eq!(int::<8>::MIN, int::<8>::new(-128));
        assert_eq!(int::<9>::MIN, int::<9>::new(-256));
    }

    #[test]
    fn test_wrapping_add() {
        assert_eq!(
            uint::<1>::MAX.wrapping_add(uint::<1>::new(1)),
            uint::<1>::new(0)
        );
        assert_eq!(
            uint::<1>::MAX.wrapping_add(uint::<1>::new(0)),
            uint::<1>::new(1)
        );

        assert_eq!(
            uint::<5>::MAX.wrapping_add(uint::<5>::new(1)),
            uint::<5>::new(0)
        );
        assert_eq!(
            uint::<5>::MAX.wrapping_add(uint::<5>::new(4)),
            uint::<5>::new(3)
        );

        assert_eq!(
            uint::<127>::MAX.wrapping_add(uint::<127>::new(100)),
            uint::<127>::new(99)
        );
        assert_eq!(
            uint::<127>::MAX.wrapping_add(uint::<127>::new(1)),
            uint::<127>::new(0)
        );

        assert_eq!(
            int::<1>::MAX.wrapping_add(int::<1>::new(0)),
            int::<1>::new(0)
        );
        assert_eq!(
            int::<1>::MAX.wrapping_add(int::<1>::new(-1)),
            int::<1>::new(-1)
        );

        assert_eq!(int::<7>::MAX.wrapping_add(int::<7>::new(1)), int::<7>::MIN);
        assert_eq!(
            int::<7>::MAX.wrapping_add(int::<7>::new(4)),
            int::<7>::new(-61)
        );
    }

    #[test]
    #[should_panic]
    fn test_add_overflow_u5() {
        let _s = uint::<5>::MAX + uint::<5>::new(1);
    }

    #[test]
    #[should_panic]
    fn test_add_overflow_u127() {
        let _s = uint::<127>::MAX + uint::<127>::new(1);
    }

    #[test]
    #[should_panic]
    fn test_add_overflow_i96() {
        let _s = int::<96>::MAX + int::<96>::new(100);
    }

    #[test]
    #[should_panic]
    fn test_add_underflow_i96() {
        let _s = int::<96>::MIN + int::<96>::new(-100);
    }

    #[test]
    #[should_panic]
    fn test_add_underflow_i17() {
        let _s = int::<17>::MIN + int::<17>::new(-1);
    }

    #[test]
    fn test_add() {
        assert_eq!(uint::<5>::new(1) + uint::<5>::new(2), uint::<5>::new(3));
        assert_eq!(uint::<8>::new(254) + uint::<8>::new(1), uint::<8>::new(255));

        assert_eq!(int::<7>::MAX + int::<7>::MIN, int::<7>::new(-1));
        assert_eq!(int::<7>::new(4) + int::<7>::new(-3), int::<7>::new(1));
        assert_eq!(int::<7>::new(-4) + int::<7>::new(3), int::<7>::new(-1));
        assert_eq!(int::<7>::new(-3) + int::<7>::new(-20), int::<7>::new(-23));
        assert_eq!(int::<8>::new(126) + int::<8>::new(1), int::<8>::new(127));
    }

    #[test]
    #[should_panic]
    fn test_sub_overflow_i23() {
        let _s = int::<23>::MIN - int::<23>::MAX;
    }

    #[test]
    #[should_panic]
    fn test_sub_underflow_u5() {
        let _s = uint::<5>::MIN - uint::<5>::new(1);
    }

    #[test]
    #[should_panic]
    fn test_sub_underflow_i5() {
        let _s = int::<5>::MIN - int::<5>::new(1);
    }

    #[test]
    fn test_sub() {
        assert_eq!(uint::<5>::new(1) - uint::<5>::new(1), uint::<5>::new(0));
        assert_eq!(uint::<5>::new(3) - uint::<5>::new(2), uint::<5>::new(1));

        assert_eq!(int::<1>::new(-1) - int::<1>::new(-1), int::<1>::new(0));
        assert_eq!(int::<7>::MIN - int::<7>::MIN, int::<7>::new(0));
        assert_eq!(int::<7>::new(4) - int::<7>::new(-3), int::<7>::new(7));
        assert_eq!(int::<7>::new(-4) - int::<7>::new(3), int::<7>::new(-7));
        assert_eq!(int::<7>::new(-3) - int::<7>::new(-20), int::<7>::new(17));
    }

    #[test]
    fn test_shr() {
        assert_eq!(uint::<5>::new(8) >> 1usize, uint::<5>::new(4));
        assert_eq!(uint::<5>::new(8) >> 1u8, uint::<5>::new(4));
        assert_eq!(uint::<5>::new(8) >> 1u16, uint::<5>::new(4));
        assert_eq!(uint::<5>::new(8) >> 1u32, uint::<5>::new(4));
        assert_eq!(uint::<5>::new(8) >> 1u64, uint::<5>::new(4));
        assert_eq!(uint::<5>::new(8) >> 1isize, uint::<5>::new(4));
        assert_eq!(uint::<5>::new(8) >> 1i8, uint::<5>::new(4));
        assert_eq!(uint::<5>::new(8) >> 1i16, uint::<5>::new(4));
        assert_eq!(uint::<5>::new(8) >> 1i32, uint::<5>::new(4));
        assert_eq!(uint::<5>::new(8) >> 1i64, uint::<5>::new(4));

        assert_eq!(uint::<5>::MAX >> 4, uint::<5>::new(1));

        assert_eq!(int::<7>::new(-1) >> 5, int::<7>::new(-1));
    }

    #[test]
    fn test_shl() {
        assert_eq!(uint::<6>::new(16) << 1usize, uint::<6>::new(32));
        assert_eq!(uint::<6>::new(16) << 1u8, uint::<6>::new(32));
        assert_eq!(uint::<6>::new(16) << 1u16, uint::<6>::new(32));
        assert_eq!(uint::<6>::new(16) << 1u32, uint::<6>::new(32));
        assert_eq!(uint::<6>::new(16) << 1u64, uint::<6>::new(32));
        assert_eq!(uint::<6>::new(16) << 1isize, uint::<6>::new(32));
        assert_eq!(uint::<6>::new(16) << 1i8, uint::<6>::new(32));
        assert_eq!(uint::<6>::new(16) << 1i16, uint::<6>::new(32));
        assert_eq!(uint::<6>::new(16) << 1i32, uint::<6>::new(32));
        assert_eq!(uint::<6>::new(16) << 1i64, uint::<6>::new(32));

        assert_eq!(uint::<5>::MAX << 4, uint::<5>::new(16));

        assert_eq!(int::<6>::new(16) << 1, int::<6>::new(-32));
        assert_eq!(int::<7>::new(1) << 3, int::<7>::new(8));
    }

    #[test]
    fn test_shr_assign() {
        let mut x = uint::<10>::new(512);
        x >>= 1usize;
        assert_eq!(x, uint::<10>::new(256));
        x >>= 1isize;
        assert_eq!(x, uint::<10>::new(128));
        x >>= 1u8;
        assert_eq!(x, uint::<10>::new(64));
        x >>= 1i8;
        assert_eq!(x, uint::<10>::new(32));
        x >>= 2u64;
        assert_eq!(x, uint::<10>::new(8));
        x >>= 3i32;
        assert_eq!(x, uint::<10>::new(1));
    }

    #[test]
    fn test_shl_assign() {
        let mut x = uint::<9>::new(1);
        x <<= 3i32;
        assert_eq!(x, uint::<9>::new(8));
        x <<= 2u64;
        assert_eq!(x, uint::<9>::new(32));
        x <<= 1usize;
        assert_eq!(x, uint::<9>::new(64));
        x <<= 1isize;
        assert_eq!(x, uint::<9>::new(128));
        x <<= 1u8;
        assert_eq!(x, uint::<9>::new(256));
    }

    #[test]
    fn test_bitor() {
        assert_eq!(uint::<9>::new(1) | uint::<9>::new(8), uint::<9>::new(9));
        assert_eq!(&uint::<9>::new(1) | uint::<9>::new(8), uint::<9>::new(9));
        assert_eq!(uint::<9>::new(1) | &uint::<9>::new(8), uint::<9>::new(9));
        assert_eq!(&uint::<9>::new(1) | &uint::<9>::new(8), uint::<9>::new(9));
    }

    #[test]
    fn test_bitor_assign() {
        let mut x = uint::<12>::new(4);
        x |= uint::<12>::new(1);
        assert_eq!(x, uint::<12>::new(5));
        x |= uint::<12>::new(128);
        assert_eq!(x, uint::<12>::new(133));
        x = uint::<12>::new(1);
        x |= uint::<12>::new(127);
        assert_eq!(x, uint::<12>::new(127));
    }

    #[test]
    fn test_bitxor() {
        assert_eq!(
            uint::<7>::new(0x7F) ^ uint::<7>::new(42),
            uint::<7>::new(85)
        );
        assert_eq!(&uint::<7>::new(0) ^ uint::<7>::new(42), uint::<7>::new(42));
        assert_eq!(
            uint::<7>::new(0x10) ^ &uint::<7>::new(0x1),
            uint::<7>::new(0x11)
        );
        assert_eq!(&uint::<7>::new(11) ^ &uint::<7>::new(1), uint::<7>::new(10));
    }

    #[test]
    fn test_bitxor_assign() {
        let mut x = uint::<12>::new(4);
        x ^= uint::<12>::new(1);
        assert_eq!(x, uint::<12>::new(5));
        x ^= uint::<12>::new(128);
        assert_eq!(x, uint::<12>::new(133));
        x ^= uint::<12>::new(1);
        assert_eq!(x, uint::<12>::new(132));
        x ^= uint::<12>::new(127);
        assert_eq!(x, uint::<12>::new(251));
    }

    #[test]
    fn test_bitand() {
        assert_eq!(
            int::<9>::new(-7) & int::<9>::new(-9),
            int::<9>::new((-7i8 & -9i8).into())
        );
        assert_eq!(
            &int::<9>::new(-7) & int::<9>::new(-9),
            int::<9>::new((&-7i8 & -9i8).into())
        );
        assert_eq!(
            int::<9>::new(-7) & &int::<9>::new(-9),
            int::<9>::new((-7i8 & &-9i8).into())
        );
        assert_eq!(
            &int::<9>::new(-7) & &int::<9>::new(-9),
            int::<9>::new((&-7i8 & &-9i8).into())
        );

        assert_eq!(uint::<9>::new(8) & uint::<9>::new(9), uint::<9>::new(8));
        assert_eq!(&uint::<9>::new(8) & uint::<9>::new(9), uint::<9>::new(8));
        assert_eq!(uint::<9>::new(8) & &uint::<9>::new(9), uint::<9>::new(8));
        assert_eq!(&uint::<9>::new(8) & &uint::<9>::new(9), uint::<9>::new(8));
    }

    #[test]
    fn test_bitand_assign() {
        let mut x = uint::<12>::new(255);
        x &= uint::<12>::new(127);
        assert_eq!(x, uint::<12>::new(127));
        x &= uint::<12>::new(7);
        assert_eq!(x, uint::<12>::new(7));
        x &= uint::<12>::new(127);
        assert_eq!(x, uint::<12>::new(7));
        x &= uint::<12>::new(4);
        assert_eq!(x, uint::<12>::new(4));
    }

    #[test]
    fn test_not() {
        assert_eq!(!uint::<7>::new(42), uint::<7>::new(85));
        assert_eq!(!uint::<7>::new(0x7F), uint::<7>::new(0));
        assert_eq!(!uint::<7>::new(0), uint::<7>::new(0x7F));
        assert_eq!(!uint::<7>::new(56), uint::<7>::new(71));
    }

    #[test]
    fn test_as() {
        assert_eq!(uint::<4>::as_(uint::<12>::new(0xFFF)), uint::<4>::new(0xF));
        assert_eq!(
            uint::<4>::as_(int::<68>::new(0x7FFFFFFFFFFFFFFFF)),
            uint::<4>::new(0xF)
        );
        assert_eq!(int::<3>::as_(int::<100>::new(-1)), int::<3>::new(-1));
        assert_eq!(int::<12>::as_(uint::<12>::new(0xFFF)), int::<12>::new(-1));
    }

    #[test]
    fn test_from() {
        assert_eq!(uint::<4>::from(uint::<3>::new(1)), uint::<4>::new(1));
        assert_eq!(uint::<11>::from(uint::<4>::new(2)), uint::<11>::new(2));
        assert_eq!(uint::<128>::from(uint::<4>::new(2)), uint::<128>::new(2));
        assert_eq!(
            uint::<128>::from(uint::<63>::new(512)),
            uint::<128>::new(512)
        );
        assert_eq!(uint::<62>::from(uint::<17>::new(128)), uint::<62>::new(128));

        assert_eq!(int::<4>::from(int::<3>::new(-1)), int::<4>::new(-1));
        assert_eq!(int::<4>::from(int::<3>::new(1)), int::<4>::new(1));
        assert_eq!(int::<11>::from(int::<4>::new(-2)), int::<11>::new(-2));
        assert_eq!(int::<128>::from(int::<4>::new(-2)), int::<128>::new(-2));
        assert_eq!(
            int::<128>::from(int::<63>::new(-512)),
            int::<128>::new(-512)
        );
        assert_eq!(int::<62>::from(int::<17>::new(-128)), int::<62>::new(-128));
    }
}

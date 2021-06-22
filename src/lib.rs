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
    Add, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Neg, Not, Shl, ShlAssign, Shr, ShrAssign, Sub,
};

use lib::core::hash::Hash;

use lib::core::cmp::{Ord, PartialOrd};

use lib::core::convert::{From, TryFrom};

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

#[derive(Debug, Clone, PartialEq)]
pub struct TryFromGenericIntError;

impl Display for TryFromGenericIntError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "out of range integral type conversion attempted")
    }
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

    macro_rules! impl_from_bitwise {
    ($name:ident, $bits:literal, $other:ident, [$($other_bits:literal,)*]) => {
        $(impl From<$other<$other_bits>> for $name<$bits> {
            fn from(value: $other<$other_bits>) -> Self {
                Self(value.0.into()).mask()
            }
        })*
    }}

    macro_rules! impl_from_basewise {
    ($name:ident, [$($other:ident,)*]) => {
        $(impl<const N: usize, const M: usize> From<$other<M>> for $name<N> {
            fn from(value: $other<M>) -> Self {
                Self(value.0.into()).mask()
            }
        })*
    }}

    macro_rules! impl_try_from_bitwise {
    ($name:ident, $base:ty, $bits:literal, [$($other_bits:literal,)*]) => {
        $(impl TryFrom<$name<$other_bits>> for $name<$bits> {
            type Error = TryFromGenericIntError;

            fn try_from(value: $name<$other_bits>) -> Result<Self, Self::Error> {
                let base_value = <$base>::try_from(value.0).map_err(|_| TryFromGenericIntError)?;
                if base_value < Self::MIN.0 || base_value > Self::MAX.0 {
                    Err(TryFromGenericIntError)
                } else {
                    Ok(Self(base_value).mask())
                }
            }
        })*
    }}

    macro_rules! impl_try_from_basewise {
    ($name:ident, $base:ty, [$($other:ident,)*]) => {
        $(impl<const N: usize, const M: usize> TryFrom<$other<M>> for $name<N> {
            type Error = TryFromGenericIntError;

            fn try_from(value: $other<M>) -> Result<Self, Self::Error> {
                let base_value = <$base>::try_from(value.0).map_err(|_| TryFromGenericIntError)?;
                if base_value < Self::MIN.0 || base_value > Self::MAX.0 {
                    Err(TryFromGenericIntError)
                } else {
                    Ok(Self(base_value).mask())
                }
            }
        })*
    }}

    impl_uint!(UInt8Impl, u8, 8, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_uint!(UInt16Impl, u16, 16, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_uint!(UInt32Impl, u32, 32, [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_uint!(
        UInt64Impl,
        u64,
        64,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
            60, 61, 62, 63, 64,
        ]
    );
    impl_uint!(
        UInt128Impl,
        u128,
        128,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );

    impl_int!(Int8Impl, i8, 8, [0, 1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_int!(Int16Impl, i16, 16, [9, 10, 11, 12, 13, 14, 15, 16,]);
    impl_int!(Int32Impl, i32, 32, [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_int!(
        Int64Impl,
        i64,
        64,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
            60, 61, 62, 63, 64,
        ]
    );
    impl_int!(
        Int128Impl,
        i128,
        128,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
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
    impl_from_bitwise!(UInt8Impl, 1, UInt8Impl, [0,]);
    impl_from_bitwise!(UInt8Impl, 2, UInt8Impl, [0, 1,]);
    impl_from_bitwise!(UInt8Impl, 3, UInt8Impl, [0, 1, 2,]);
    impl_from_bitwise!(UInt8Impl, 4, UInt8Impl, [0, 1, 2, 3,]);
    impl_from_bitwise!(UInt8Impl, 5, UInt8Impl, [0, 1, 2, 3, 4,]);
    impl_from_bitwise!(UInt8Impl, 6, UInt8Impl, [0, 1, 2, 3, 4, 5,]);
    impl_from_bitwise!(UInt8Impl, 7, UInt8Impl, [0, 1, 2, 3, 4, 5, 6,]);

    // UInt16Impl::from()
    impl_from_basewise!(UInt16Impl, [UInt8Impl,]);
    impl_from_bitwise!(UInt16Impl, 10, UInt16Impl, [9,]);
    impl_from_bitwise!(UInt16Impl, 11, UInt16Impl, [9, 10,]);
    impl_from_bitwise!(UInt16Impl, 12, UInt16Impl, [9, 10, 11,]);
    impl_from_bitwise!(UInt16Impl, 13, UInt16Impl, [9, 10, 11, 12,]);
    impl_from_bitwise!(UInt16Impl, 14, UInt16Impl, [9, 10, 11, 12, 13,]);
    impl_from_bitwise!(UInt16Impl, 15, UInt16Impl, [9, 10, 11, 12, 13, 14,]);

    // UInt32Impl::from()
    impl_from_basewise!(UInt32Impl, [UInt8Impl, UInt16Impl,]);
    impl_from_bitwise!(UInt32Impl, 18, UInt32Impl, [17,]);
    impl_from_bitwise!(UInt32Impl, 19, UInt32Impl, [17, 18,]);
    impl_from_bitwise!(UInt32Impl, 20, UInt32Impl, [17, 18, 19,]);
    impl_from_bitwise!(UInt32Impl, 21, UInt32Impl, [17, 18, 19, 20,]);
    impl_from_bitwise!(UInt32Impl, 22, UInt32Impl, [17, 18, 19, 20, 21,]);
    impl_from_bitwise!(UInt32Impl, 23, UInt32Impl, [17, 18, 19, 20, 21, 22,]);
    impl_from_bitwise!(UInt32Impl, 24, UInt32Impl, [17, 18, 19, 20, 21, 22, 23,]);
    impl_from_bitwise!(UInt32Impl, 25, UInt32Impl, [17, 18, 19, 20, 21, 22, 23, 24,]);
    impl_from_bitwise!(UInt32Impl, 26, UInt32Impl, [17, 18, 19, 20, 21, 22, 23, 24, 25,]);
    impl_from_bitwise!(UInt32Impl, 27, UInt32Impl, [17, 18, 19, 20, 21, 22, 23, 24, 25, 26,]);
    impl_from_bitwise!(UInt32Impl, 28, UInt32Impl, [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,]);
    impl_from_bitwise!(UInt32Impl, 29, UInt32Impl, [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,]);
    impl_from_bitwise!(UInt32Impl, 30, UInt32Impl, [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,]);
    impl_from_bitwise!(UInt32Impl, 31, UInt32Impl, [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,]);
    impl_from_bitwise!(UInt32Impl, 32, UInt32Impl, [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,]);

    // UInt64Impl::from()
    impl_from_basewise!(UInt64Impl, [UInt8Impl, UInt16Impl, UInt32Impl,]);
    impl_from_bitwise!(UInt64Impl, 34, UInt64Impl, [33,]);
    impl_from_bitwise!(UInt64Impl, 35, UInt64Impl, [33, 34,]);
    impl_from_bitwise!(UInt64Impl, 36, UInt64Impl, [33, 34, 35,]);
    impl_from_bitwise!(UInt64Impl, 37, UInt64Impl, [33, 34, 35, 36,]);
    impl_from_bitwise!(UInt64Impl, 38, UInt64Impl, [33, 34, 35, 36, 37,]);
    impl_from_bitwise!(UInt64Impl, 39, UInt64Impl, [33, 34, 35, 36, 37, 38,]);
    impl_from_bitwise!(UInt64Impl, 40, UInt64Impl, [33, 34, 35, 36, 37, 38, 39,]);
    impl_from_bitwise!(UInt64Impl, 41, UInt64Impl, [33, 34, 35, 36, 37, 38, 39, 40,]);
    impl_from_bitwise!(UInt64Impl, 42, UInt64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41,]);
    impl_from_bitwise!(UInt64Impl, 43, UInt64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41, 42,]);
    impl_from_bitwise!(UInt64Impl, 44, UInt64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43,]);
    impl_from_bitwise!(UInt64Impl, 45, UInt64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44,]);
    impl_from_bitwise!(UInt64Impl, 46, UInt64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,]);
    impl_from_bitwise!(UInt64Impl, 47, UInt64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46,]);
    impl_from_bitwise!(UInt64Impl, 48, UInt64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,]);
    impl_from_bitwise!(UInt64Impl, 49, UInt64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,]);
    impl_from_bitwise!(
        UInt64Impl,
        50,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,]
    );
    impl_from_bitwise!(
        UInt64Impl,
        51,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,]
    );
    impl_from_bitwise!(
        UInt64Impl,
        52,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,]
    );
    impl_from_bitwise!(
        UInt64Impl,
        53,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,]
    );
    impl_from_bitwise!(
        UInt64Impl,
        54,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53,]
    );
    impl_from_bitwise!(
        UInt64Impl,
        55,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,]
    );
    impl_from_bitwise!(
        UInt64Impl,
        56,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55,]
    );
    impl_from_bitwise!(
        UInt64Impl,
        57,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56,]
    );
    impl_from_bitwise!(
        UInt64Impl,
        58,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57,]
    );
    impl_from_bitwise!(
        UInt64Impl,
        59,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58,]
    );
    impl_from_bitwise!(
        UInt64Impl,
        60,
        UInt64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,]
    );
    impl_from_bitwise!(
        UInt64Impl,
        61,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
            60,
        ]
    );
    impl_from_bitwise!(
        UInt64Impl,
        62,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
            60, 61,
        ]
    );
    impl_from_bitwise!(
        UInt64Impl,
        63,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
            60, 61, 62,
        ]
    );
    impl_from_bitwise!(
        UInt64Impl,
        64,
        UInt64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
            60, 61, 62, 63,
        ]
    );

    // UInt128Impl::from()
    impl_from_basewise!(UInt128Impl, [UInt8Impl, UInt16Impl, UInt32Impl, UInt64Impl,]);
    impl_from_bitwise!(UInt128Impl, 66, UInt128Impl, [65,]);
    impl_from_bitwise!(UInt128Impl, 67, UInt128Impl, [65, 66,]);
    impl_from_bitwise!(UInt128Impl, 68, UInt128Impl, [65, 66, 67,]);
    impl_from_bitwise!(UInt128Impl, 69, UInt128Impl, [65, 66, 67, 68,]);
    impl_from_bitwise!(UInt128Impl, 70, UInt128Impl, [65, 66, 67, 68, 69,]);
    impl_from_bitwise!(UInt128Impl, 71, UInt128Impl, [65, 66, 67, 68, 69, 70,]);
    impl_from_bitwise!(UInt128Impl, 72, UInt128Impl, [65, 66, 67, 68, 69, 70, 71,]);
    impl_from_bitwise!(UInt128Impl, 73, UInt128Impl, [65, 66, 67, 68, 69, 70, 71, 72,]);
    impl_from_bitwise!(UInt128Impl, 74, UInt128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73,]);
    impl_from_bitwise!(UInt128Impl, 75, UInt128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73, 74,]);
    impl_from_bitwise!(UInt128Impl, 76, UInt128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,]);
    impl_from_bitwise!(UInt128Impl, 77, UInt128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,]);
    impl_from_bitwise!(UInt128Impl, 78, UInt128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77,]);
    impl_from_bitwise!(UInt128Impl, 79, UInt128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78,]);
    impl_from_bitwise!(UInt128Impl, 80, UInt128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,]);
    impl_from_bitwise!(UInt128Impl, 81, UInt128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,]);
    impl_from_bitwise!(
        UInt128Impl,
        82,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81,]
    );
    impl_from_bitwise!(
        UInt128Impl,
        83,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,]
    );
    impl_from_bitwise!(
        UInt128Impl,
        84,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83,]
    );
    impl_from_bitwise!(
        UInt128Impl,
        85,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84,]
    );
    impl_from_bitwise!(
        UInt128Impl,
        86,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85,]
    );
    impl_from_bitwise!(
        UInt128Impl,
        87,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,]
    );
    impl_from_bitwise!(
        UInt128Impl,
        88,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87,]
    );
    impl_from_bitwise!(
        UInt128Impl,
        89,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88,]
    );
    impl_from_bitwise!(
        UInt128Impl,
        90,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89,]
    );
    impl_from_bitwise!(
        UInt128Impl,
        91,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,]
    );
    impl_from_bitwise!(
        UInt128Impl,
        92,
        UInt128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,]
    );
    impl_from_bitwise!(
        UInt128Impl,
        93,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        94,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        95,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        96,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        97,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        98,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        99,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        100,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        101,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        102,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        103,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        104,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        105,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        106,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        107,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        108,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        109,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        110,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        111,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        112,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        113,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        114,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        115,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        116,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        117,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        118,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        119,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        120,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        121,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        122,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        123,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        124,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        125,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123, 124,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        126,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        127,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126,
        ]
    );
    impl_from_bitwise!(
        UInt128Impl,
        128,
        UInt128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
        ]
    );

    // Int8Impl::from()
    impl_from_bitwise!(Int8Impl, 1, Int8Impl, [0,]);
    impl_from_bitwise!(Int8Impl, 2, Int8Impl, [0, 1,]);
    impl_from_bitwise!(Int8Impl, 3, Int8Impl, [0, 1, 2,]);
    impl_from_bitwise!(Int8Impl, 4, Int8Impl, [0, 1, 2, 3,]);
    impl_from_bitwise!(Int8Impl, 5, Int8Impl, [0, 1, 2, 3, 4,]);
    impl_from_bitwise!(Int8Impl, 6, Int8Impl, [0, 1, 2, 3, 4, 5,]);
    impl_from_bitwise!(Int8Impl, 7, Int8Impl, [0, 1, 2, 3, 4, 5, 6,]);

    // Int16Impl::from()
    impl_from_basewise!(Int16Impl, [Int8Impl,]);
    impl_from_bitwise!(Int16Impl, 10, Int16Impl, [9,]);
    impl_from_bitwise!(Int16Impl, 11, Int16Impl, [9, 10,]);
    impl_from_bitwise!(Int16Impl, 12, Int16Impl, [9, 10, 11,]);
    impl_from_bitwise!(Int16Impl, 13, Int16Impl, [9, 10, 11, 12,]);
    impl_from_bitwise!(Int16Impl, 14, Int16Impl, [9, 10, 11, 12, 13,]);
    impl_from_bitwise!(Int16Impl, 15, Int16Impl, [9, 10, 11, 12, 13, 14,]);

    // Int32Impl::from()
    impl_from_basewise!(Int32Impl, [Int8Impl, Int16Impl,]);
    impl_from_bitwise!(Int32Impl, 18, Int32Impl, [17,]);
    impl_from_bitwise!(Int32Impl, 19, Int32Impl, [17, 18,]);
    impl_from_bitwise!(Int32Impl, 20, Int32Impl, [17, 18, 19,]);
    impl_from_bitwise!(Int32Impl, 21, Int32Impl, [17, 18, 19, 20,]);
    impl_from_bitwise!(Int32Impl, 22, Int32Impl, [17, 18, 19, 20, 21,]);
    impl_from_bitwise!(Int32Impl, 23, Int32Impl, [17, 18, 19, 20, 21, 22,]);
    impl_from_bitwise!(Int32Impl, 24, Int32Impl, [17, 18, 19, 20, 21, 22, 23,]);
    impl_from_bitwise!(Int32Impl, 25, Int32Impl, [17, 18, 19, 20, 21, 22, 23, 24,]);
    impl_from_bitwise!(Int32Impl, 26, Int32Impl, [17, 18, 19, 20, 21, 22, 23, 24, 25,]);
    impl_from_bitwise!(Int32Impl, 27, Int32Impl, [17, 18, 19, 20, 21, 22, 23, 24, 25, 26,]);
    impl_from_bitwise!(Int32Impl, 28, Int32Impl, [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,]);
    impl_from_bitwise!(Int32Impl, 29, Int32Impl, [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,]);
    impl_from_bitwise!(Int32Impl, 30, Int32Impl, [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,]);
    impl_from_bitwise!(Int32Impl, 31, Int32Impl, [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,]);
    impl_from_bitwise!(Int32Impl, 32, Int32Impl, [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,]);

    // Int64Impl::from()
    impl_from_basewise!(Int64Impl, [Int8Impl, Int16Impl, Int32Impl,]);
    impl_from_bitwise!(Int64Impl, 34, Int64Impl, [33,]);
    impl_from_bitwise!(Int64Impl, 35, Int64Impl, [33, 34,]);
    impl_from_bitwise!(Int64Impl, 36, Int64Impl, [33, 34, 35,]);
    impl_from_bitwise!(Int64Impl, 37, Int64Impl, [33, 34, 35, 36,]);
    impl_from_bitwise!(Int64Impl, 38, Int64Impl, [33, 34, 35, 36, 37,]);
    impl_from_bitwise!(Int64Impl, 39, Int64Impl, [33, 34, 35, 36, 37, 38,]);
    impl_from_bitwise!(Int64Impl, 40, Int64Impl, [33, 34, 35, 36, 37, 38, 39,]);
    impl_from_bitwise!(Int64Impl, 41, Int64Impl, [33, 34, 35, 36, 37, 38, 39, 40,]);
    impl_from_bitwise!(Int64Impl, 42, Int64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41,]);
    impl_from_bitwise!(Int64Impl, 43, Int64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41, 42,]);
    impl_from_bitwise!(Int64Impl, 44, Int64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43,]);
    impl_from_bitwise!(Int64Impl, 45, Int64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44,]);
    impl_from_bitwise!(Int64Impl, 46, Int64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,]);
    impl_from_bitwise!(Int64Impl, 47, Int64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46,]);
    impl_from_bitwise!(Int64Impl, 48, Int64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,]);
    impl_from_bitwise!(Int64Impl, 49, Int64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,]);
    impl_from_bitwise!(Int64Impl, 50, Int64Impl, [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,]);
    impl_from_bitwise!(
        Int64Impl,
        51,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,]
    );
    impl_from_bitwise!(
        Int64Impl,
        52,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,]
    );
    impl_from_bitwise!(
        Int64Impl,
        53,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,]
    );
    impl_from_bitwise!(
        Int64Impl,
        54,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53,]
    );
    impl_from_bitwise!(
        Int64Impl,
        55,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,]
    );
    impl_from_bitwise!(
        Int64Impl,
        56,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55,]
    );
    impl_from_bitwise!(
        Int64Impl,
        57,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56,]
    );
    impl_from_bitwise!(
        Int64Impl,
        58,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57,]
    );
    impl_from_bitwise!(
        Int64Impl,
        59,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58,]
    );
    impl_from_bitwise!(
        Int64Impl,
        60,
        Int64Impl,
        [33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,]
    );
    impl_from_bitwise!(
        Int64Impl,
        61,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
            60,
        ]
    );
    impl_from_bitwise!(
        Int64Impl,
        62,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
            60, 61,
        ]
    );
    impl_from_bitwise!(
        Int64Impl,
        63,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
            60, 61, 62,
        ]
    );
    impl_from_bitwise!(
        Int64Impl,
        64,
        Int64Impl,
        [
            33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
            60, 61, 62, 63,
        ]
    );

    // Int128Impl::from()
    impl_from_basewise!(Int128Impl, [Int8Impl, Int16Impl, Int32Impl, Int64Impl,]);
    impl_from_bitwise!(Int128Impl, 66, Int128Impl, [65,]);
    impl_from_bitwise!(Int128Impl, 67, Int128Impl, [65, 66,]);
    impl_from_bitwise!(Int128Impl, 68, Int128Impl, [65, 66, 67,]);
    impl_from_bitwise!(Int128Impl, 69, Int128Impl, [65, 66, 67, 68,]);
    impl_from_bitwise!(Int128Impl, 70, Int128Impl, [65, 66, 67, 68, 69,]);
    impl_from_bitwise!(Int128Impl, 71, Int128Impl, [65, 66, 67, 68, 69, 70,]);
    impl_from_bitwise!(Int128Impl, 72, Int128Impl, [65, 66, 67, 68, 69, 70, 71,]);
    impl_from_bitwise!(Int128Impl, 73, Int128Impl, [65, 66, 67, 68, 69, 70, 71, 72,]);
    impl_from_bitwise!(Int128Impl, 74, Int128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73,]);
    impl_from_bitwise!(Int128Impl, 75, Int128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73, 74,]);
    impl_from_bitwise!(Int128Impl, 76, Int128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,]);
    impl_from_bitwise!(Int128Impl, 77, Int128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,]);
    impl_from_bitwise!(Int128Impl, 78, Int128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77,]);
    impl_from_bitwise!(Int128Impl, 79, Int128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78,]);
    impl_from_bitwise!(Int128Impl, 80, Int128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,]);
    impl_from_bitwise!(Int128Impl, 81, Int128Impl, [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,]);
    impl_from_bitwise!(
        Int128Impl,
        82,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81,]
    );
    impl_from_bitwise!(
        Int128Impl,
        83,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,]
    );
    impl_from_bitwise!(
        Int128Impl,
        84,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83,]
    );
    impl_from_bitwise!(
        Int128Impl,
        85,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84,]
    );
    impl_from_bitwise!(
        Int128Impl,
        86,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85,]
    );
    impl_from_bitwise!(
        Int128Impl,
        87,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,]
    );
    impl_from_bitwise!(
        Int128Impl,
        88,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87,]
    );
    impl_from_bitwise!(
        Int128Impl,
        89,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88,]
    );
    impl_from_bitwise!(
        Int128Impl,
        90,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89,]
    );
    impl_from_bitwise!(
        Int128Impl,
        91,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,]
    );
    impl_from_bitwise!(
        Int128Impl,
        92,
        Int128Impl,
        [65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,]
    );
    impl_from_bitwise!(
        Int128Impl,
        93,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        94,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        95,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        96,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        97,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        98,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        99,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        100,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        101,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        102,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        103,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        104,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        105,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        106,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        107,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        108,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        109,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        110,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        111,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        112,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        113,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        114,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        115,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        116,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        117,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        118,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        119,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        120,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        121,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        122,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        123,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        124,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        125,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123, 124,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        126,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        127,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126,
        ]
    );
    impl_from_bitwise!(
        Int128Impl,
        128,
        Int128Impl,
        [
            65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
        ]
    );

    // UInt8Impl::try_from()
    impl_try_from_basewise!(
        UInt8Impl,
        u8,
        [UInt16Impl, UInt32Impl, UInt64Impl, UInt128Impl, Int8Impl, Int16Impl, Int32Impl, Int64Impl, Int128Impl,]
    );
    impl_try_from_bitwise!(UInt8Impl, u8, 0, [1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_try_from_bitwise!(UInt8Impl, u8, 1, [2, 3, 4, 5, 6, 7, 8,]);
    impl_try_from_bitwise!(UInt8Impl, u8, 2, [3, 4, 5, 6, 7, 8,]);
    impl_try_from_bitwise!(UInt8Impl, u8, 3, [4, 5, 6, 7, 8,]);
    impl_try_from_bitwise!(UInt8Impl, u8, 4, [5, 6, 7, 8,]);
    impl_try_from_bitwise!(UInt8Impl, u8, 5, [6, 7, 8,]);
    impl_try_from_bitwise!(UInt8Impl, u8, 6, [7, 8,]);
    impl_try_from_bitwise!(UInt8Impl, u8, 7, [8,]);

    // UInt16Impl::try_from()
    impl_try_from_basewise!(
        UInt16Impl,
        u16,
        [UInt32Impl, UInt64Impl, UInt128Impl, Int8Impl, Int16Impl, Int32Impl, Int64Impl, Int128Impl,]
    );
    impl_try_from_bitwise!(UInt16Impl, u16, 9, [10, 11, 12, 13, 14, 15, 16,]);
    impl_try_from_bitwise!(UInt16Impl, u16, 10, [11, 12, 13, 14, 15, 16,]);
    impl_try_from_bitwise!(UInt16Impl, u16, 11, [12, 13, 14, 15, 16,]);
    impl_try_from_bitwise!(UInt16Impl, u16, 12, [13, 14, 15, 16,]);
    impl_try_from_bitwise!(UInt16Impl, u16, 13, [14, 15, 16,]);
    impl_try_from_bitwise!(UInt16Impl, u16, 14, [15, 16,]);
    impl_try_from_bitwise!(UInt16Impl, u16, 15, [16,]);

    // UInt32Impl::try_from()
    impl_try_from_basewise!(
        UInt32Impl,
        u32,
        [UInt64Impl, UInt128Impl, Int8Impl, Int16Impl, Int32Impl, Int64Impl, Int128Impl,]
    );
    impl_try_from_bitwise!(UInt32Impl, u32, 17, [18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(UInt32Impl, u32, 18, [19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(UInt32Impl, u32, 19, [20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(UInt32Impl, u32, 20, [21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(UInt32Impl, u32, 21, [22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(UInt32Impl, u32, 22, [23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(UInt32Impl, u32, 23, [24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(UInt32Impl, u32, 24, [25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(UInt32Impl, u32, 25, [26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(UInt32Impl, u32, 26, [27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(UInt32Impl, u32, 27, [28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(UInt32Impl, u32, 28, [29, 30, 31, 32,]);
    impl_try_from_bitwise!(UInt32Impl, u32, 29, [30, 31, 32,]);
    impl_try_from_bitwise!(UInt32Impl, u32, 30, [31, 32,]);
    impl_try_from_bitwise!(UInt32Impl, u32, 31, [32,]);

    // UInt64Impl::try_from()
    impl_try_from_basewise!(UInt64Impl, u64, [UInt128Impl, Int8Impl, Int16Impl, Int32Impl, Int64Impl, Int128Impl,]);
    impl_try_from_bitwise!(
        UInt64Impl,
        u64,
        33,
        [
            34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
            61, 62, 63, 64,
        ]
    );
    impl_try_from_bitwise!(
        UInt64Impl,
        u64,
        34,
        [
            35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61,
            62, 63, 64,
        ]
    );
    impl_try_from_bitwise!(
        UInt64Impl,
        u64,
        35,
        [
            36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62,
            63, 64,
        ]
    );
    impl_try_from_bitwise!(
        UInt64Impl,
        u64,
        36,
        [
            37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
            64,
        ]
    );
    impl_try_from_bitwise!(
        UInt64Impl,
        u64,
        37,
        [38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        UInt64Impl,
        u64,
        38,
        [39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        UInt64Impl,
        u64,
        39,
        [40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        UInt64Impl,
        u64,
        40,
        [41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        UInt64Impl,
        u64,
        41,
        [42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        UInt64Impl,
        u64,
        42,
        [43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        UInt64Impl,
        u64,
        43,
        [44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        UInt64Impl,
        u64,
        44,
        [45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        UInt64Impl,
        u64,
        45,
        [46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        UInt64Impl,
        u64,
        46,
        [47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(UInt64Impl, u64, 47, [48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 48, [49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 49, [50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 50, [51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 51, [52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 52, [53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 53, [54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 54, [55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 55, [56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 56, [57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 57, [58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 58, [59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 59, [60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 60, [61, 62, 63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 61, [62, 63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 62, [63, 64,]);
    impl_try_from_bitwise!(UInt64Impl, u64, 63, [64,]);

    // UInt128Impl::try_from()
    impl_try_from_basewise!(UInt128Impl, u128, [Int8Impl, Int16Impl, Int32Impl, Int64Impl, Int128Impl,]);
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        65,
        [
            66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92,
            93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
            116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        66,
        [
            67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93,
            94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
            116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        67,
        [
            68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94,
            95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116,
            117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        68,
        [
            69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
            96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
            118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        69,
        [
            70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96,
            97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118,
            119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        70,
        [
            71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97,
            98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
            120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        71,
        [
            72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,
            99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
            120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        72,
        [
            73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
            100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120,
            121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        73,
        [
            74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
            100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120,
            121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        74,
        [
            75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100,
            101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
            122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        75,
        [
            76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101,
            102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122,
            123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        76,
        [
            77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102,
            103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
            124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        77,
        [
            78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103,
            104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124,
            125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        78,
        [
            79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103,
            104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124,
            125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        79,
        [
            80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104,
            105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
            126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        80,
        [
            81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105,
            106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126,
            127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        81,
        [
            82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
            128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        82,
        [
            83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107,
            108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        83,
        [
            84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107,
            108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        84,
        [
            85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108,
            109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        85,
        [
            86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
            110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        86,
        [
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
            111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        87,
        [
            88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
            112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        88,
        [
            89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
            112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        89,
        [
            90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
            113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        90,
        [
            91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
            114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        91,
        [
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        92,
        [
            93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
            116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        93,
        [
            94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
            116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        94,
        [
            95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116,
            117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        95,
        [
            96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
            118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        96,
        [
            97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118,
            119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        97,
        [
            98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
            120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        98,
        [
            99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
            120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        99,
        [
            100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120,
            121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        100,
        [
            101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
            122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        101,
        [
            102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122,
            123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        102,
        [
            103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
            124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        103,
        [
            104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124,
            125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        104,
        [
            105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
            126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        105,
        [
            106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126,
            127, 128,
        ]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        106,
        [107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        107,
        [108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        108,
        [109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        109,
        [110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        110,
        [111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        111,
        [112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        112,
        [113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        113,
        [114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        UInt128Impl,
        u128,
        114,
        [115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(UInt128Impl, u128, 115, [116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(UInt128Impl, u128, 116, [117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(UInt128Impl, u128, 117, [118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(UInt128Impl, u128, 118, [119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(UInt128Impl, u128, 119, [120, 121, 122, 123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(UInt128Impl, u128, 120, [121, 122, 123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(UInt128Impl, u128, 121, [122, 123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(UInt128Impl, u128, 122, [123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(UInt128Impl, u128, 123, [124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(UInt128Impl, u128, 124, [125, 126, 127, 128,]);
    impl_try_from_bitwise!(UInt128Impl, u128, 125, [126, 127, 128,]);
    impl_try_from_bitwise!(UInt128Impl, u128, 126, [127, 128,]);
    impl_try_from_bitwise!(UInt128Impl, u128, 127, [128,]);

    // Int8Impl::try_from()
    impl_try_from_basewise!(
        Int8Impl,
        i8,
        [UInt8Impl, UInt16Impl, UInt32Impl, UInt64Impl, UInt128Impl, Int16Impl, Int32Impl, Int64Impl, Int128Impl,]
    );
    impl_try_from_bitwise!(Int8Impl, i8, 0, [1, 2, 3, 4, 5, 6, 7, 8,]);
    impl_try_from_bitwise!(Int8Impl, i8, 1, [2, 3, 4, 5, 6, 7, 8,]);
    impl_try_from_bitwise!(Int8Impl, i8, 2, [3, 4, 5, 6, 7, 8,]);
    impl_try_from_bitwise!(Int8Impl, i8, 3, [4, 5, 6, 7, 8,]);
    impl_try_from_bitwise!(Int8Impl, i8, 4, [5, 6, 7, 8,]);
    impl_try_from_bitwise!(Int8Impl, i8, 5, [6, 7, 8,]);
    impl_try_from_bitwise!(Int8Impl, i8, 6, [7, 8,]);
    impl_try_from_bitwise!(Int8Impl, i8, 7, [8,]);

    // Int16Impl::try_from()
    impl_try_from_basewise!(
        Int16Impl,
        i16,
        [UInt8Impl, UInt16Impl, UInt32Impl, UInt64Impl, UInt128Impl, Int32Impl, Int64Impl, Int128Impl,]
    );
    impl_try_from_bitwise!(Int16Impl, i16, 9, [10, 11, 12, 13, 14, 15, 16,]);
    impl_try_from_bitwise!(Int16Impl, i16, 10, [11, 12, 13, 14, 15, 16,]);
    impl_try_from_bitwise!(Int16Impl, i16, 11, [12, 13, 14, 15, 16,]);
    impl_try_from_bitwise!(Int16Impl, i16, 12, [13, 14, 15, 16,]);
    impl_try_from_bitwise!(Int16Impl, i16, 13, [14, 15, 16,]);
    impl_try_from_bitwise!(Int16Impl, i16, 14, [15, 16,]);
    impl_try_from_bitwise!(Int16Impl, i16, 15, [16,]);

    // Int32Impl::try_from()
    impl_try_from_basewise!(
        Int32Impl,
        i32,
        [UInt8Impl, UInt16Impl, UInt32Impl, UInt64Impl, UInt128Impl, Int64Impl, Int128Impl,]
    );
    impl_try_from_bitwise!(Int32Impl, i32, 17, [18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(Int32Impl, i32, 18, [19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(Int32Impl, i32, 19, [20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(Int32Impl, i32, 20, [21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(Int32Impl, i32, 21, [22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(Int32Impl, i32, 22, [23, 24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(Int32Impl, i32, 23, [24, 25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(Int32Impl, i32, 24, [25, 26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(Int32Impl, i32, 25, [26, 27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(Int32Impl, i32, 26, [27, 28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(Int32Impl, i32, 27, [28, 29, 30, 31, 32,]);
    impl_try_from_bitwise!(Int32Impl, i32, 28, [29, 30, 31, 32,]);
    impl_try_from_bitwise!(Int32Impl, i32, 29, [30, 31, 32,]);
    impl_try_from_bitwise!(Int32Impl, i32, 30, [31, 32,]);
    impl_try_from_bitwise!(Int32Impl, i32, 31, [32,]);

    // Int64Impl::try_from()
    impl_try_from_basewise!(Int64Impl, i64, [UInt8Impl, UInt16Impl, UInt32Impl, UInt64Impl, UInt128Impl, Int128Impl,]);
    impl_try_from_bitwise!(
        Int64Impl,
        i64,
        33,
        [
            34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
            61, 62, 63, 64,
        ]
    );
    impl_try_from_bitwise!(
        Int64Impl,
        i64,
        34,
        [
            35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61,
            62, 63, 64,
        ]
    );
    impl_try_from_bitwise!(
        Int64Impl,
        i64,
        35,
        [
            36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62,
            63, 64,
        ]
    );
    impl_try_from_bitwise!(
        Int64Impl,
        i64,
        36,
        [
            37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
            64,
        ]
    );
    impl_try_from_bitwise!(
        Int64Impl,
        i64,
        37,
        [38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        Int64Impl,
        i64,
        38,
        [39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        Int64Impl,
        i64,
        39,
        [40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        Int64Impl,
        i64,
        40,
        [41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        Int64Impl,
        i64,
        41,
        [42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        Int64Impl,
        i64,
        42,
        [43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        Int64Impl,
        i64,
        43,
        [44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        Int64Impl,
        i64,
        44,
        [45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        Int64Impl,
        i64,
        45,
        [46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(
        Int64Impl,
        i64,
        46,
        [47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]
    );
    impl_try_from_bitwise!(Int64Impl, i64, 47, [48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 48, [49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 49, [50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 50, [51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 51, [52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 52, [53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 53, [54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 54, [55, 56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 55, [56, 57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 56, [57, 58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 57, [58, 59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 58, [59, 60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 59, [60, 61, 62, 63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 60, [61, 62, 63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 61, [62, 63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 62, [63, 64,]);
    impl_try_from_bitwise!(Int64Impl, i64, 63, [64,]);

    // Int128Impl::try_from()
    impl_try_from_basewise!(Int128Impl, i128, [UInt8Impl, UInt16Impl, UInt32Impl, UInt64Impl, UInt128Impl,]);
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        65,
        [
            66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92,
            93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
            116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        66,
        [
            67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93,
            94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
            116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        67,
        [
            68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94,
            95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116,
            117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        68,
        [
            69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
            96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
            118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        69,
        [
            70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96,
            97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118,
            119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        70,
        [
            71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97,
            98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
            120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        71,
        [
            72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,
            99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
            120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        72,
        [
            73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
            100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120,
            121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        73,
        [
            74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
            100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120,
            121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        74,
        [
            75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100,
            101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
            122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        75,
        [
            76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101,
            102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122,
            123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        76,
        [
            77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102,
            103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
            124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        77,
        [
            78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103,
            104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124,
            125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        78,
        [
            79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103,
            104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124,
            125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        79,
        [
            80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104,
            105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
            126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        80,
        [
            81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105,
            106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126,
            127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        81,
        [
            82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
            107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
            128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        82,
        [
            83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107,
            108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        83,
        [
            84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107,
            108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        84,
        [
            85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108,
            109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        85,
        [
            86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
            110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        86,
        [
            87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
            111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        87,
        [
            88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
            112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        88,
        [
            89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
            112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        89,
        [
            90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
            113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        90,
        [
            91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
            114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        91,
        [
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        92,
        [
            93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
            116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        93,
        [
            94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
            116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        94,
        [
            95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116,
            117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        95,
        [
            96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
            118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        96,
        [
            97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118,
            119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        97,
        [
            98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
            120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        98,
        [
            99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
            120, 121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        99,
        [
            100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120,
            121, 122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        100,
        [
            101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
            122, 123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        101,
        [
            102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122,
            123, 124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        102,
        [
            103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
            124, 125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        103,
        [
            104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124,
            125, 126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        104,
        [
            105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
            126, 127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        105,
        [
            106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126,
            127, 128,
        ]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        106,
        [107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        107,
        [108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        108,
        [109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        109,
        [110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        110,
        [111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        111,
        [112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        112,
        [113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        113,
        [114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(
        Int128Impl,
        i128,
        114,
        [115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]
    );
    impl_try_from_bitwise!(Int128Impl, i128, 115, [116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(Int128Impl, i128, 116, [117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(Int128Impl, i128, 117, [118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(Int128Impl, i128, 118, [119, 120, 121, 122, 123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(Int128Impl, i128, 119, [120, 121, 122, 123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(Int128Impl, i128, 120, [121, 122, 123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(Int128Impl, i128, 121, [122, 123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(Int128Impl, i128, 122, [123, 124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(Int128Impl, i128, 123, [124, 125, 126, 127, 128,]);
    impl_try_from_bitwise!(Int128Impl, i128, 124, [125, 126, 127, 128,]);
    impl_try_from_bitwise!(Int128Impl, i128, 125, [126, 127, 128,]);
    impl_try_from_bitwise!(Int128Impl, i128, 126, [127, 128,]);
    impl_try_from_bitwise!(Int128Impl, i128, 127, [128,]);
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
        assert_eq!(uint::<1>::MAX.wrapping_add(uint::<1>::new(1)), uint::<1>::new(0));
        assert_eq!(uint::<1>::MAX.wrapping_add(uint::<1>::new(0)), uint::<1>::new(1));

        assert_eq!(uint::<5>::MAX.wrapping_add(uint::<5>::new(1)), uint::<5>::new(0));
        assert_eq!(uint::<5>::MAX.wrapping_add(uint::<5>::new(4)), uint::<5>::new(3));

        assert_eq!(uint::<127>::MAX.wrapping_add(uint::<127>::new(100)), uint::<127>::new(99));
        assert_eq!(uint::<127>::MAX.wrapping_add(uint::<127>::new(1)), uint::<127>::new(0));

        assert_eq!(int::<1>::MAX.wrapping_add(int::<1>::new(0)), int::<1>::new(0));
        assert_eq!(int::<1>::MAX.wrapping_add(int::<1>::new(-1)), int::<1>::new(-1));

        assert_eq!(int::<7>::MAX.wrapping_add(int::<7>::new(1)), int::<7>::MIN);
        assert_eq!(int::<7>::MAX.wrapping_add(int::<7>::new(4)), int::<7>::new(-61));
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
        assert_eq!(uint::<7>::new(0x7F) ^ uint::<7>::new(42), uint::<7>::new(85));
        assert_eq!(&uint::<7>::new(0) ^ uint::<7>::new(42), uint::<7>::new(42));
        assert_eq!(uint::<7>::new(0x10) ^ &uint::<7>::new(0x1), uint::<7>::new(0x11));
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
        assert_eq!(int::<9>::new(-7) & int::<9>::new(-9), int::<9>::new((-7i8 & -9i8).into()));
        assert_eq!(&int::<9>::new(-7) & int::<9>::new(-9), int::<9>::new((&-7i8 & -9i8).into()));
        assert_eq!(int::<9>::new(-7) & &int::<9>::new(-9), int::<9>::new((-7i8 & &-9i8).into()));
        assert_eq!(&int::<9>::new(-7) & &int::<9>::new(-9), int::<9>::new((&-7i8 & &-9i8).into()));

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
        assert_eq!(uint::<4>::as_(int::<68>::new(0x7FFFFFFFFFFFFFFFF)), uint::<4>::new(0xF));
        assert_eq!(int::<3>::as_(int::<100>::new(-1)), int::<3>::new(-1));
        assert_eq!(int::<12>::as_(uint::<12>::new(0xFFF)), int::<12>::new(-1));
    }

    #[test]
    fn test_from() {
        assert_eq!(uint::<4>::from(uint::<3>::new(1)), uint::<4>::new(1));
        assert_eq!(uint::<11>::from(uint::<4>::new(2)), uint::<11>::new(2));
        assert_eq!(uint::<128>::from(uint::<4>::new(2)), uint::<128>::new(2));
        assert_eq!(uint::<128>::from(uint::<63>::new(512)), uint::<128>::new(512));
        assert_eq!(uint::<62>::from(uint::<17>::new(128)), uint::<62>::new(128));

        assert_eq!(int::<4>::from(int::<3>::new(-1)), int::<4>::new(-1));
        assert_eq!(int::<4>::from(int::<3>::new(1)), int::<4>::new(1));
        assert_eq!(int::<11>::from(int::<4>::new(-2)), int::<11>::new(-2));
        assert_eq!(int::<128>::from(int::<4>::new(-2)), int::<128>::new(-2));
        assert_eq!(int::<128>::from(int::<63>::new(-512)), int::<128>::new(-512));
        assert_eq!(int::<62>::from(int::<17>::new(-128)), int::<62>::new(-128));
    }

    #[test]
    fn test_try_from() {
        assert_eq!(uint::<4>::try_from(uint::<8>::new(1)).unwrap(), uint::<4>::new(1));
        assert_eq!(uint::<4>::try_from(uint::<16>::new(1)).unwrap(), uint::<4>::new(1));
        assert_eq!(uint::<4>::try_from(uint::<32>::new(1)).unwrap(), uint::<4>::new(1));
        assert_eq!(uint::<4>::try_from(uint::<64>::new(1)).unwrap(), uint::<4>::new(1));
        assert_eq!(uint::<4>::try_from(uint::<128>::new(1)).unwrap(), uint::<4>::new(1));
        assert_eq!(uint::<4>::try_from(int::<8>::new(1)).unwrap(), uint::<4>::new(1));
        assert_eq!(uint::<4>::try_from(int::<16>::new(1)).unwrap(), uint::<4>::new(1));
        assert_eq!(uint::<4>::try_from(int::<32>::new(1)).unwrap(), uint::<4>::new(1));
        assert_eq!(uint::<4>::try_from(int::<64>::new(1)).unwrap(), uint::<4>::new(1));
        assert_eq!(uint::<4>::try_from(int::<128>::new(1)).unwrap(), uint::<4>::new(1));

        assert_eq!(int::<4>::try_from(int::<8>::new(-1)).unwrap(), int::<4>::new(-1));
        assert_eq!(int::<4>::try_from(int::<12>::new(-1)).unwrap(), int::<4>::new(-1));
        assert_eq!(int::<4>::try_from(int::<16>::new(-1)).unwrap(), int::<4>::new(-1));
        assert_eq!(int::<4>::try_from(int::<32>::new(-1)).unwrap(), int::<4>::new(-1));
        assert_eq!(int::<4>::try_from(int::<64>::new(-1)).unwrap(), int::<4>::new(-1));
        assert_eq!(int::<4>::try_from(int::<128>::new(-1)).unwrap(), int::<4>::new(-1));
        assert_eq!(int::<4>::try_from(uint::<8>::new(1)).unwrap(), int::<4>::new(1));
        assert_eq!(int::<4>::try_from(uint::<16>::new(1)).unwrap(), int::<4>::new(1));
        assert_eq!(int::<4>::try_from(uint::<32>::new(1)).unwrap(), int::<4>::new(1));
        assert_eq!(int::<4>::try_from(uint::<64>::new(1)).unwrap(), int::<4>::new(1));
        assert_eq!(int::<4>::try_from(uint::<128>::new(1)).unwrap(), int::<4>::new(1));

        assert_eq!(uint::<4>::try_from(int::<128>::new(0x1F)), Err(TryFromGenericIntError));
        assert_eq!(int::<4>::try_from(int::<128>::new(0xF)), Err(TryFromGenericIntError));
    }
}

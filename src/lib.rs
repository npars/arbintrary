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

    // #[test]
    // fn test_from() {
    //     assert_eq!(uint::<4>::from(uint::<3>::new(1)), uint::<4>::new(1));
    // }
}

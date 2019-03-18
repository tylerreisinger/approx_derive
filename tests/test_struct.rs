use approx_derive::AbsDiffEq;
use approx::*;

#[derive(AbsDiffEq, PartialEq, Debug)]
struct TupleStruct(f32, f32);

#[derive(AbsDiffEq, PartialEq, Debug)]
#[approx(epsilon_ty = "f32")]
struct NamedStruct {
    x: f32,
    y: f32,
    z: f64,
}

#[derive(AbsDiffEq, PartialEq, Debug)]
#[approx(default_epsilon = 1e-8)]
struct ExactEqStruct {
    x: f32,
    y: f32,
    #[approx(exact_eq)]
    z: i64,
}

/*
#[derive(AbsDiffEq, PartialEq)]
enum MyEnum {
    X(f32),
    Y(f32),
}
*/

#[test]
fn test_tuple_struct() {
    let x = TupleStruct(0.5, 0.7);
    let y = TupleStruct(0.500001, 0.70000001);
    assert_abs_diff_eq!(x, y, epsilon=1e-5);
}

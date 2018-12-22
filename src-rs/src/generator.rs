// extern crate rand;

// use rand::Rng;
use core::arch::x86_64::_rdrand32_step;
 
fn main() {
    let currencies = ["MXN", "USD", "EUD", "GBP", "THB"];
    let numberOfAccounts = 1200000;

    // let mut rng = rand::thread_rng();
    let mut r1;
    let mut r2;
    let mut f1 : f32;
    let mut f2 : f32;

    for i in 0..numberOfAccounts {
        _rdrand32_step(r1);
        _rdrand32_step(r2);
        f1 = (*r1 as f32) / (std::u32::MAX as f32);
        f2 = (*r2 as f32) / (std::u32::MAX as f32);

        println!(
            "{}|{}|{}|Test name",
            i+10000, 
            f32::round(f1*1000.0),
            currencies[
                f32::round(
                    f2*((currencies.len() - 1) as f32)
                ) as u32
            ]
        );
    }
}
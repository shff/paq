use paq::queue::{run, Error};

#[test]
fn test_queue() {
    let result = run(1, |num| match num {
        1 => Ok(("one", vec![2])),
        2 => Ok(("two", vec![])),
        _ => Err(Error),
    })
    .unwrap();
    assert_eq!(result.get(&1), Some(&"one"));
    assert_eq!(result.get(&2), Some(&"two"));

    let result = run(1, |num| match num {
        1 => Ok((num, vec![1, 2, 3, 4, 5, 6, 7])),
        _ => Ok((num, vec![])),
    })
    .unwrap();
    assert_eq!(result.get(&1), Some(&1));
    assert_eq!(result.get(&2), Some(&2));
    assert_eq!(result.get(&3), Some(&3));
    assert_eq!(result.get(&4), Some(&4));
    assert_eq!(result.get(&5), Some(&5));
    assert_eq!(result.get(&6), Some(&6));
    assert_eq!(result.get(&7), Some(&7));
    assert_eq!(result.get(&8), None);

    let result = run(1, |num| match num {
        1 => Ok(("one", vec![2])),
        2 => Ok(("two", vec![])),
        _ => Ok(("", vec![])),
    });
    assert_eq!(result.unwrap().get(&1), Some(&"one"));

    let result = run("one", |num| Ok((num, vec![])));
    assert_eq!(result.unwrap().get(&"one"), Some(&"one"));

    let result = run(1, |num| match num {
        1 => Ok(("one", vec![2])),
        _ => Err(Error),
    });
    assert_eq!(result.is_err(), true);
}

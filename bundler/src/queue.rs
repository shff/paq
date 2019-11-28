use std::hash::{Hash};
use std::error::Error;
use std::collections::{HashMap, VecDeque};

pub fn run<A, B, F: FnMut(A) -> Result<(B, Vec<A>), Box<dyn Error>>>(first_job: A, mut perform: F) -> Result<HashMap<A, B>, Box<dyn Error>>
where A: Clone + Eq + Hash, B: Clone {
    let mut results = HashMap::<A, B>::default();
    let mut queue = VecDeque::<A>::new();
    queue.push_back(first_job);

    while let Some(job) = queue.pop_front() {
        let (result, new_jobs) = perform(job.clone())?;

        results.insert(job, result);
        for job in new_jobs {
            if !results.contains_key(&job) {
                queue.push_back(job);
            }
        }
    };
    Ok(results)
}

#[test]
fn test_queue() {
    let result = run(1, |num| {
        match num {
            1 => Ok(("one", vec![ 2 ])),
            2 => Ok(("two", vec![])),
            _ => Err(Box::new(std::io::Error::new(std::io::ErrorKind::Other, "Error")))
        }
    }).unwrap();
    assert_eq!(result.get(&1), Some(&"one"));
    assert_eq!(result.get(&2), Some(&"two"));

    let result = run("one", |num| { Ok((num, vec![])) });
    assert_eq!(result.unwrap().get(&"one"), Some(&"one"));

    let result = run(1, |num| {
        match num {
            1 => Ok(("one", vec![ 2 ])),
            _ => Err(Box::new(std::io::Error::new(std::io::ErrorKind::Other, "Error")))
        }
    });
    assert_eq!(result.is_err(), true);
}

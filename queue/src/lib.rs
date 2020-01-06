use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::sync::mpsc;
use std::sync::{Arc, RwLock};
use std::thread;

pub fn run<A, B, F: FnMut(A) -> Result<(B, Vec<A>), Box<dyn std::error::Error + Send>>>(
    first_job: A,
    mut perform: F,
) -> Result<HashMap<A, B>, Box<dyn std::error::Error + Send>>
where
    A: 'static + Clone + Eq + Hash + Send + Sync,
    B: 'static + Clone + Send,
    F: 'static + Send + Copy,
{
    let mut pending = HashSet::<A>::new();
    let queue = Arc::new(RwLock::new(VecDeque::new()));
    let (tx, rx) = mpsc::channel();
    pending.insert(first_job.clone());
    queue.write().unwrap().push_back(first_job);

    let _ = (0..4).map(|_| {
        let tx2 = tx.clone();
        let pending2 = pending.clone();
        let queue2 = queue.clone();
        thread::spawn(move || loop {
            match queue2.write().unwrap().pop_front() {
                Some(job) => tx2.send((job.clone(), perform(job))).unwrap(),
                None if pending2.is_empty() => return,
                None => thread::yield_now()
            }
        })
    }).collect::<Vec<_>>();

    let mut results = HashMap::<A, B>::default();
    while let Ok((job, result)) = rx.recv() {
        let (result, new_jobs) = result?;
        pending.remove(&job);
        results.insert(job, result);
        for job in new_jobs {
            if !results.contains_key(&job) {
                pending.insert(job.clone());
                queue.write().unwrap().push_back(job);
            }
        }
        if pending.is_empty() {
            break;
        }
    }
    Ok(results)
}

#[test]
fn test_queue() {
    let result = run(1, |num| match num {
        1 => Ok(("one", vec![2])),
        2 => Ok(("two", vec![])),
        _ => Err(Box::new(std::io::Error::new(
            std::io::ErrorKind::Other,
            "Error",
        ))),
    })
    .unwrap();
    assert_eq!(result.get(&1), Some(&"one"));
    assert_eq!(result.get(&2), Some(&"two"));

    let result = run(1, |num| match num {
        1 => Ok((num, vec![ 1, 2, 3, 4, 5, 6, 7 ])),
        _ => Ok((num, vec![]))
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
        _ => Err(Box::new(std::io::Error::new(
            std::io::ErrorKind::Other,
            "Error",
        ))),
    });
    assert_eq!(result.is_err(), true);
}

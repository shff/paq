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

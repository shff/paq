use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{mpsc, Arc, RwLock};
use std::thread::{spawn, yield_now};

pub type Error = Box<dyn std::error::Error + Send + Sync>;

pub fn run<A, B, F>(first_job: A, mut perform: F) -> Result<HashMap<A, B>, Error>
where
    A: 'static + Clone + Eq + Hash + Send + Sync,
    B: 'static + Clone + Send,
    F: 'static + Send + Copy + FnMut(A) -> Result<(B, Vec<A>), Error>,
{
    let mut pending = HashSet::<A>::new();
    let finished = Arc::new(AtomicBool::new(false));
    let queue = Arc::new(RwLock::new(VecDeque::new()));
    let (tx, rx) = mpsc::channel();
    pending.insert(first_job.clone());
    queue.write().unwrap().push_back(first_job);

    for _ in 0..4 {
        let tx = tx.clone();
        let finished = finished.clone();
        let queue = queue.clone();
        spawn(move || loop {
            match queue.write().unwrap().pop_front() {
                Some(job) => tx.send((job.clone(), perform(job))).unwrap(),
                None if finished.load(Ordering::Relaxed) => return,
                None => yield_now(),
            }
        });
    }

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
            finished.store(true, Ordering::Relaxed);
            break;
        }
    }
    Ok(results)
}

use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::sync::{mpsc, Arc, PoisonError, RwLock};
use std::thread::{spawn, yield_now};

pub fn run<A, B, F>(first_job: A, mut perform: F) -> Result<HashMap<A, B>, Error>
where
    A: 'static + Clone + Eq + Hash + Send + Sync,
    B: 'static + Clone + Send,
    F: 'static + Send + Copy + FnMut(A) -> Result<(B, Vec<A>), Error>,
{
    let mut pending = HashSet::<A>::new();
    let queue = Arc::new(RwLock::new(VecDeque::new()));
    let (tx, rx) = mpsc::channel();
    pending.insert(first_job.clone());
    queue.write()?.push_back(first_job);

    for _ in 0..4 {
        let tx2 = tx.clone();
        let pending2 = pending.clone();
        let queue2 = queue.clone();
        spawn(move || loop {
            match queue2.write().unwrap().pop_front() {
                Some(job) => tx2.send((job.clone(), perform(job))).unwrap(),
                None if pending2.is_empty() => return,
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
                queue.write()?.push_back(job);
            }
        }
        if pending.is_empty() {
            break;
        }
    }
    Ok(results)
}

#[derive(Debug)]
pub enum Error {
    InnerError,
    QueueError,
}
impl<T> From<PoisonError<T>> for Error {
    fn from(_: PoisonError<T>) -> Self {
        Self::QueueError
    }
}

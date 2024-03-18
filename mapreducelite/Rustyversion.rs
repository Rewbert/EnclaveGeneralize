use rand::{thread_rng, Rng};
use std::sync::mpsc::{channel, Receiver, Sender};

struct WorkRequest {
    sender: Sender<i32>,
}

struct VerySafePtr<T>(*mut T);
impl<T> VerySafePtr<T> {
    fn get_mut(&mut self) -> &mut T {
        unsafe { &mut *self.0 }
    }
}
unsafe impl<T> Sync for VerySafePtr<T> {}
unsafe impl<T> Send for VerySafePtr<T> {}

enum Status {
    Waiting,
    Working,
}
struct Worker {
    sender: Sender<WorkRequest>,
    status: Status,
}

fn work_thread(id: usize, work_recip: Receiver<WorkRequest>) {
    let mut rng = thread_rng();
    while let Ok(work) = work_recip.recv() {
        let value: i32 = rng.gen();
        let time = rng.gen_range(1..10);
        println!("Got work {id}: returning {value}, eeping for {time}s");
        std::thread::sleep(std::time::Duration::from_secs(time));

        let _ = work.sender.send(value);
    }
}

fn work_manager(work_recip: Receiver<WorkRequest>) {
    let mut workers = (0..2)
        .map(|i| {
            let (sender, reciver) = channel();
            std::thread::spawn(move || work_thread(i, reciver));
            Worker {
                status: Status::Waiting,
                sender,
            }
        })
        .collect::<Vec<_>>();

    while let Ok(work) = work_recip.recv() {
        'outer: loop {
            for worker in &mut workers {
                if matches!(worker.status, Status::Waiting) {
                    worker.status = Status::Working;
                    let (temp_sender, temp_reciever) = channel();
                    let _ = worker.sender.send(WorkRequest {
                        sender: temp_sender,
                    });

                    let work_sender = work.sender;
                    let mut very_safe_ptr = VerySafePtr(&mut worker.status);
                    std::thread::spawn(move || {
                        let res = temp_reciever.recv().unwrap();
                        let _ = work_sender.send(res);
                        *very_safe_ptr.get_mut() = Status::Waiting;
                    });
                    break 'outer;
                }
            }
        }
    }
}

fn main() {
    let (sender, reciever) = channel();
    std::thread::spawn(|| work_manager(reciever));

    for i in 0.. {
        println!("Work iteration: {i}");
        let mut vec = Vec::new();
        for i in 0..3 {
            let sender = sender.clone();
            vec.push(std::thread::spawn(move || {
                println!("Client {i}: sending work..");
                let (client_sender, client_reciever) = channel();
                let _ = sender.send(WorkRequest {
                    sender: client_sender,
                });
                println!("Client {i}: got {:?}", client_reciever.recv());
            }))
        }
        for t in vec {
            let _ = t.join();
        }
        println!();
    }
}

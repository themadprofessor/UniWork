use std::env;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{SocketAddr, TcpStream, ToSocketAddrs};
use std::sync::mpsc::{Receiver, Sender};
use std::sync::{Arc, Barrier};

fn attempt_conn(chan: Sender<TcpStream>, addr: SocketAddr) {
    let conn = match TcpStream::connect(addr) {
        Ok(c) => c,
        Err(_) => return,
    };

    if let Err(e) = chan.send(conn) {
        // Can only fail to send if channel is closed
        // Channel is closed if other connection wins the race
    }
}

fn send_http(chan: Receiver<TcpStream>, host: &str) {
    let mut conn = match chan.recv() {
        Ok(c) => c,
        Err(e) => {
            eprintln!("failed to get connection: {}", e);
            return;
        }
    };
    // Connection won the race so stop anymore attempts
    drop(chan);

    match write!(conn, "GET / HTTP/1.1\r\nHost: {}\r\n\r\n", host) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("failed to send request: {}", e);
            return;
        }
    };

    let mut data = String::new();
    let mut conn = BufReader::new(conn);
    let mut content_len = None;

    loop {
        data.clear();
        match conn.read_line(&mut data) {
            Ok(count) => {
                if count == 0 {
                    break;
                }
            }
            Err(e) => {
                eprintln!("failed to receive response: {}", e);
                return;
            }
        }

        // If only contains return, it is an empty line
        if &data == "\r\n" {
            break;
        }

        data.make_ascii_lowercase();
        if data.starts_with("content-length:") {
            content_len = data.split(':').nth(1).and_then(|n| n.trim().parse::<u64>().ok());
        }
    }

    data.clear();

    // If the header contained a content-length, read that many bytes, otherwise read till ed
    if let Some(len) = content_len {
        if let Err(e) = conn.take(len).read_to_string(&mut data) {
            eprintln!("failed to read {} bytes: {}", len, e);
        }
    } else {
        if let Err(e) = conn.read_to_string(&mut data) {
            eprintln!("failed to read: {}", e);
        }
    }

    println!("{}", data);
}

fn main() {
    // First arg is the program name
    for addr in env::args().skip(1) {
        // Iterators can only be used once, so keep track of valid addresses to avoid extra alloc
        let mut count = 0;
        let sock_addrs = match (addr.as_str(), 80).to_socket_addrs() {
            Ok(s) => {
                count += 1;
                s
            }
            Err(e) => {
                eprintln!("failed dns lookup [{}]: {}", addr, e);
                continue;
            }
        };

        let (sender, receiver) = std::sync::mpsc::channel();
        let barrier = Arc::new(Barrier::new(count));
        let mut threads = Vec::with_capacity(count + 1);

        threads.push(std::thread::spawn(move || {
            send_http(receiver, &addr);
        }));

        for sock_addr in sock_addrs {
            let b = barrier.clone();
            let chan = sender.clone();
            threads.push(std::thread::spawn(move || {
                // Wait until all threads have spawned
                b.wait();
                attempt_conn(chan, sock_addr);
            }));
        }

        for x in threads {
            // A join only returns Err if the thread panicked.
            x.join().unwrap();
        }
    }
}

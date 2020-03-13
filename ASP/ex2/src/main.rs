use std::env;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{SocketAddr, TcpStream, ToSocketAddrs};
use std::sync::mpsc::{self, Receiver, Sender};
use std::thread;

/// Attempt to open a TcpStream to the given SocketAddr, sending it over the
/// given if the stream successfully opened
fn attempt_conn(chan: Sender<TcpStream>, addr: SocketAddr) {
    let conn = match TcpStream::connect(addr) {
        Ok(c) => c,
        Err(_) => return,
    };

    if let Err(_e) = chan.send(conn) {
        // Can only fail to send if channel is closed
        // Channel is closed if other connection wins the race
    }
}

/// Send a HTTP 1.1 GET request to the first TcpStream from the given channel.
/// The given str will be used as the HTTP host header.
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

        // If only contains return, it is an empty line aka end of headers
        if &data == "\r\n" {
            break;
        }

        data.make_ascii_lowercase();
        if data.starts_with("content-length:") {
            // Convert the value of content-length header to a Some(u64).
            // If the conversion failed or content-length was empty, set content_len to None
            content_len = data
                .split(':')
                .nth(1)
                .and_then(|n| n.trim().parse::<u64>().ok());
        }
    }

    data.clear();
    let stdout = std::io::stdout();
    let mut lock = stdout.lock();

    // If the header contained a content-length, read that many bytes, otherwise read till end
    if let Some(len) = content_len {
        if let Err(e) = std::io::copy(&mut conn.take(len), &mut lock) {
            eprintln!("failed to read {} bytes: {}", len, e);
        }
    } else {
        if let Err(e) = std::io::copy(&mut conn, &mut lock) {
            eprintln!("failed to read: {}", e);
        }
    }
}

fn main() {
    // First arg is the program name
    for addr in env::args().skip(1) {
        let sock_addrs = match (addr.as_str(), 80).to_socket_addrs() {
            Ok(s) => s,
            Err(e) => {
                eprintln!("failed dns lookup [{}]: {}", addr, e);
                continue;
            }
        }
        .collect::<Vec<_>>();

        let (send_conn, recv_conn) = mpsc::channel();
        let mut senders = Vec::with_capacity(sock_addrs.len());
        let mut joins = Vec::with_capacity(sock_addrs.len() + 1);

        for _ in 0..sock_addrs.len() {
            let (send_addr, recv_addr) = mpsc::channel();
            let send_clone = send_conn.clone();
            senders.push(send_addr);
            joins.push(thread::spawn(move || {
                // Only fails if sender has been closed, which it isn't until main returns
                let sock_addr = recv_addr.recv().unwrap();
                attempt_conn(send_clone, sock_addr);
            }));
        }

        joins.push(thread::spawn(move || send_http(recv_conn, &addr)));

        for (sock_addr, sender) in sock_addrs.into_iter().zip(senders) {
            // Only fails if receiver has been closed, which it isn't until the receiver thread has
            // ended, which can't be before it receives an addr
            sender.send(sock_addr).unwrap();
        }

        for x in joins {
            x.join().unwrap(); // Only fails if thread panics.
        }
    }
}

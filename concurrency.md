# Semantics

- Process (normal and system)
- Link
- Link Set
- Monitor
- Message
- Error Signal (real and fake)

# Primitives and BIFs

- Basic Process Handling
    - `spawn/1` and `spawn/3`
    - `Pid ! Message` (send)
    - `receive`
- Links
    - `link/1`
    - `unlink/1`
    - `spawn_link/1` and `spawn_link/3`
- Monitors
    - `monitor/2`
    - `demonitor/2`
    - `spawn_monitor/1` and `spawn_monitor/3`
- TODO
    - `process_flag/2`
    - `exit/1` and `exit/2`

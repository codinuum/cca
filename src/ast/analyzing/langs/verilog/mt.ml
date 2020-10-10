
let _ =
  let mutex = Mutex.create() in
  Xthread.register_functions
    { Xthread.f_enter_critical_section = (fun () -> Mutex.lock mutex);
      Xthread.f_exit_critical_section  = (fun () -> Mutex.unlock mutex);
    }

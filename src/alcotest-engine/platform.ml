include Platform_intf

module Legacy (P : MAKER) (M : Monad.S) = struct
  include P (M)

  let with_log_trap ~root ~uuid ~name f =
    M.bind (M.return ()) (fun () ->
        prepare_log_trap ~root ~uuid ~name;
        M.bind (f ()) (fun (value, _) -> M.return value))
end

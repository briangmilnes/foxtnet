Decl 2
(b f%Tcp (l (h (d (a s%Trace (i f%Trace (h (a s%V (g s%B.V) 0)) F) 0) (f s%Trace s%SW s%B s%FoxMakestring s%FoxWord16 s%Lower s%System) (a s%Log (i f%Tcp_Log (h (a s%B (g s%B) 0)) F) 0) (a s%Tcp_Tcb (i f%Tcp_Tcb (h (d (a s%B (g s%B) 0) (a s%Tcp_Log (g s%Log) 0))) F) 0) (a s%Tcp_Action (i f%Tcp_Action (h (d (a s%Tcp_Tcb (g s%Tcp_Tcb) 0) (a s%B (g s%B) 0))) F) 0) (a s%Tcp_Retransmit (i f%Tcp_Retransmit (h (d (a s%Tcp_Tcb (g s%Tcp_Tcb) 0) (a s%B (g s%B) 0))) F) 0) (a s%Tcp_State (i f%Tcp_State (h (d (a s%Tcp_Tcb (g s%Tcp_Tcb) 0) (a s%Retransmit (g s%Tcp_Retransmit) 0) (a s%B (g s%B) 0))) F) 0) (a s%Tcp_Receive (i f%Tcp_Receive (h (d (a s%Tcp_Tcb (g s%Tcp_Tcb) 0) (a s%Retransmit (g s%Tcp_Retransmit) 0) (a s%B (g s%B) 0))) F) 0) (a s%Tcp_Send (i f%Tcp_Send (h (d (a s%Tcp_Tcb (g s%Tcp_Tcb) 0) (a s%Retransmit (g s%Tcp_Retransmit) 0) (a s%B (g s%B) 0))) F) 0) (f s%Tcp_Tcb s%Tcp_Action s%FoxWord32 s%Log s%FoxMakestring s%B s%FoxWord16 s%Lower) (a s%Conn (i f%Connection (h (d (f s%Lower) (a s%Trace (g s%Trace) 0) (a s%B (g s%B) 0))) F) 0) (f s%FoxMakestring s%Tcp_Receive s%FoxWord16 s%Tcp_State s%Tcp_Action s%FoxWord32 s%B s%Tcp_Send s%Lower s%Conn s%Log s%Tcp_Tcb s%System))) (g s$TCP_PROTOCOL) 0 (h (d (a s%Lower (g s$IP_PROTOCOL) 0) (a s%B (g s$FOX_BASIS) 0) (f s%B s%Lower s%FoxWord16)))))

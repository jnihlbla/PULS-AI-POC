000100 01  TRAN-WDP811.                                                         
000200*                                 KOMMUNIKATIONSREGISTER                  
000300*                                 TRANSAKTIONSSEGMENT                     
000400*                                 FYSISK NYCKEL: IDRADNR                  
000500     03 TRAN-IDRADNR         PIC S9(5)           COMP-3.                  
000600*                                 RADNUMMER                               
000700*                                 LINE NO                                 
000800     03 TRAN-IDUSER          PIC X(8).                                    
000900*                                 ANVÄNDARENS SÄKERHETS ID                
001000*                                 USER SECURITY-IDENTITY                  
001100     03 TRAN-WMSGAREA.                                                    
001200        05 TRAN-KVLL         PIC S9(4)           COMP.                    
001300*                                 LRECL I ETT VARIABELT RECORD            
001400*                                 LRECL I A VARIABLE RECORD               
001500        05 TRAN-KDZ1         PIC X.                                       
001600*                                 POS 3 I LRECL I MID/MOD                 
001700*                                 POS 3 I LRECL IN MID/MOD                
001800        05 TRAN-KDZ2         PIC X.                                       
001900*                                 POS 4 I LRECL I MID/MOD                 
002000*                                 POS 4 I LRECL IN MID/MOD                
002100        05 TRAN-TRANSDATA    PIC X(1000).                                 
002200*** END COPY WDP811CCC0  LENGTH=1015                                      

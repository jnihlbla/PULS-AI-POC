000100 01  MOD-MODPREF.                                                         
000200*                                 THE FIRST FIELDS IN AN IMS              
000300*                                 MESSAGE OUTPUT DESCRIPTION              
000400     03 MOD-KVLL             PIC S9(4)           COMP.                    
000500*                                 LRECL I ETT VARIABELT RECORD            
000600*                                 LRECL IN A VARIABLE RECORD              
000700     03 MOD-KDZ1             PIC X.                                       
000800*                                 POS 3 I LRECL I MID/MOD                 
000900*                                 POS 3 IN LRECL IN MID/MOD               
001000     03 MOD-KDZ2             PIC X.                                       
001100*                                 POS 4 I LRECL I MID/MOD                 
001200*                                 POS 4 IN LRECL IN MID/MOD               
001300     03 MOD-KDTRANS          PIC X(8).                                    
001400*                                 TRANSAKTIONSKOD                         
001500*                                 TRANSACTION CODE                        
001600     03 MOD-KDPGMACT         PIC X.                                       
001700*                                 TYP AV PROGRAMBEARBETNING               
001800*                                 TYPE OF PROGRAM ACTION                  
001900     03 MOD-KDDIASTATE       PIC X.                                       
002000*                                 DIALOGLÄGE                              
002100*                                 DIALOGUE STATE                          
002200     03 MOD-TEWEBERR         PIC X(80).                                   
002300*                                 WEB FELMEDDELANDE                       
002400*                                 WEB ERROR MESSAGE                       
002500     03 MOD-TEWEBINF         PIC X(80).                                   
002600*                                 WEB INFO-MEDDELANDE                     
002700*                                 WEB INFORMATION MESSAGE                 
002800*** END OF VILMAII-COPY LENGTH= 174 BYTES                                 

000100 01  MID-MIDPREF.                                                         
000200*                                 THE FIRST FIELDS IN AN IMS              
000300*                                 MESSAGE INPUT DESCRIPTION               
000400     03 MID-KVLL             PIC S9(4)           COMP.                    
000500*                                 LRECL I ETT VARIABELT RECORD            
000600*                                 LRECL IN A VARIABLE RECORD              
000700     03 MID-KDZ1             PIC X.                                       
000800*                                 POS 3 I LRECL I MID/MOD                 
000900*                                 POS 3 IN LRECL IN MID/MOD               
001000     03 MID-KDZ2             PIC X.                                       
001100*                                 POS 4 I LRECL I MID/MOD                 
001200*                                 POS 4 IN LRECL IN MID/MOD               
001300     03 MID-KDTRANS          PIC X(8).                                    
001400*                                 TRANSAKTIONSKOD                         
001500*                                 TRANSACTION CODE                        
001600     03 MID-KDPGMACT         PIC X.                                       
001700*                                 TYP AV PROGRAMBEARBETNING               
001800*                                 TYPE OF PROGRAM ACTION                  
001900     03 MID-KDDIASTATE       PIC X.                                       
002000*                                 DIALOGLÄGE                              
002100*                                 DIALOGUE STATE                          
002200     03 MID-IDUSER           PIC X(8).                                    
002300*                                 ANVÄNDARENS SÄKERHETS ID                
002400*                                 USER SECURITY-IDENTITY                  
002500     03 MID-IDSPRAK          PIC X(2).                                    
002600*                                 2-STÄLLIG ISO SPRÅKKOD                  
002700*                                 2-LETTER ISO LANGUAGE CODE              
002800*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  

000100* GENERATION OF COBOL HOST STRUCTURE FROM TZ1DIDA-TAB                     
000200  01 TZ1DIDA.                                                             
000300*              TZ1DIDA                                                    
000400   03 ADDISPABS        PIC X(50).                                         
000500*              ABSTRAKT ADRESS                                            
000600   03 TIDATETIME-DB2   PIC X(26).                                         
000700*              DATUM OCH TID DB2 FORMAT                                   
000800   03 IDLOPNR          PIC S9(4) COMP.                                    
000900*              LÖPNUMMER                                                  
001000   03 IDCALLER         PIC X(8).                                          
001100*              TRANSAKTIONSANROPARENS IDENTITET                           
001200   03 IDUSER           PIC X(8).                                          
001300*              ANVÄNDARENS SÄKERHETS ID                                   
001400   03 IDCPYTXT         PIC X(8).                                          
001500*              COPYTEXT IDENTITET                                         
001600   03 KDRC-HTTP        PIC X(10).                                         
001700   03 KDRC-PULS        PIC X(10).                                         
001800   03 KDKOMSTA         PIC X(1).                                          
001900*              KOMMUNIKATIONSSTATUS                                       
002000   03 MESSAGE.                                                            
002100*              MEDDELANDE                                                 
002200     49 MESSAGE-L        PIC S9(4) COMP.                                  
002300*              MEDDELANDE                                                 
002400     49 MESSAGE-D        PIC X(1024).                                     
002500*              MEDDELANDE                                                 
002600   03 DATA             SQL TYPE IS CLOB(5242880).                         
002700*              CLOB DATA ITEM FOR USE IN DB2 TABLES                       
002800*                                                                         
002900*** END OF VILMAII-COPY LENGTH= 1149 OLD LENGTH=                          

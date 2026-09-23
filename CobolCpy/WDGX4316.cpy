000100 01  4316-WDGX4316.                                                       
000200*                                 MELLAN ORDERVIS OCH KOLLIVIS            
000300*                                 PACKNINGSRAPPORTERING                   
000400*                                 NYCKEL: WDGXKEY                         
000500*                                         (IDPRODNR, IDPTYP,              
000600*                                          IDKOLLI, LOWVALUE)             
000700*                                 SÖKBEGREPP: IDPRODNR, IDPTYP,           
000800*                                             IDKOLLI, KDTRSTAT           
000900     03 4316-IDPRODNR        PIC S9(7)           COMP-3.                  
001000*                                 PRODUKTIONSNUMMER                       
001100     03 4316-IDPTYP          PIC X(3).                                    
001200*                                 POSTTYP                                 
001300     03 4316-IDKOLLI         PIC S9(5)           COMP-3.                  
001400*                                 KOLLINUMMER                             
001500     03 4316-LOWVALUE        PIC X(10).                                   
001600     03 4316-KDTRSTAT        PIC S9              COMP-3.                  
001700*                                 TRANSAKTIONSSTATUS                      
001800     03 4316-INTRANS.                                                     
001900        05 4316-LL           PIC S9(4)           COMP.                    
002000*                                 LRECL I ETT VARIABELT RECORD            
002100        05 4316-Z1           PIC X.                                       
002200*                                 POS 3 I LRECL                           
002300        05 4316-Z2           PIC X.                                       
002400*                                 POS 4 I LRECL                           
002500        05 4316-KDTRANS      PIC X(8).                                    
002600*                                 TRANSAKTIONSKOD                         
002700        05 4316-IDTRANS      PIC X(4).                                    
002800*                                 TRANSAKTIONSIDENTITET                   
002900        05 4316-KDMFSFOR     PIC X.                                       
003000*                                 MFS-FORMAT                              
003100        05 4316-FILLER       PIC X(332).                                  
003200*** END COPY WDGX4316C0  LENGTH=370                                       

000100 01  4110-WDGX4110.                                                       
000200*                                 LEVANM/RETUR ANALYSNUMMER               
000300*                                 ARTIKEL ANALYSNUMMER                    
000400*                                 FYSISK NYCKEL: KEY4110                  
000500*                                 (IDARTNR  + KDANMORS +                  
000600*                                  DAGILTIG-FOM)                          
000700     03 4110-IDARTNR         PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 4110-KDANMORS        PIC X(2).                                    
001100*                                 ORSAK TILL LEVERANSANMÄRKNING           
001200*                                 DISCREPANCY REPORT REASON CODE          
001300     03 4110-DAGILTIG-FOM    PIC 9(8).                                    
001400*                                 GILTIGHETSDATUM FOM  (ÅÅÅÅMMDD)         
001500*                                 DATE OF VALIDITY FROM(YYYYMMDD)         
001600     03 4110-IDANALYS        PIC X(12).                                   
001700*                                 ANALYSNUMMER                            
001800*                                 ANALYSIS NUMBER                         
001900     03 4110-IDKONTO         PIC S9(11)          COMP-3.                  
002000*                                 KONTO                                   
002100*                                 ACCOUNT                                 
002200     03 4110-IDKST           PIC X(10).                                   
002300*                                 KOSTNADSSTÄLLE                          
002400*                                 COST CENTRE                             
002500     03 4110-IDUSER          PIC X(8).                                    
002600*                                 ANVÄNDARENS SÄKERHETS ID                
002700*                                 USER SECURITY-IDENTITY                  
002800     03 4110-KVART           PIC S9(7)           COMP-3.                  
002900*                                 ANTAL ARTNR PER BRYTBEGREPP             
003000*                                 NO OF PARTNOS PER TYPE                  
003100     03 4110-DAGILTIG-TOM    PIC 9(8).                                    
003200*                                 GILTIGHETSDATUM TOM (ÅÅÅÅMMDD)          
003300*                                 DATE OF VALIDITY TO (YYYYMMDD)          
003400*** END OF VILMAII-COPY LENGTH= 63 BYTES                                  

000100 01  4142-WDGX4142.                                                       
000200*                                 TMS STEERING TABEL                      
000300*                                 PACKING INFO API ORDERS                 
000400*                                 NYCKEL = KY4142                         
000500*                                 (IDDC+KDORDKL+KDFRAKT+                  
000600*                                  IDDISTR+IDKUNDNR)                      
000700     03 4142-IDDC            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 4142-KDORDKL         PIC S9              COMP-3.                  
001100*                                 ORDERKLASS                              
001200*                                 ORDER CLASS                             
001300     03 4142-KDFRAKT         PIC S9(3)           COMP-3.                  
001400*                                 FRAKTSÄTT DC TILL KUND                  
001500*                                 FREIGHT CODE                            
001600     03 4142-IDDISTR         PIC S9(5)           COMP-3.                  
001700*                                 DISTRIKTNUMMER                          
001800*                                 DISTRICT NUMBER                         
001900     03 4142-IDKUNDNR        PIC S9(7)           COMP-3.                  
002000*                                 KUNDNUMMER                              
002100*                                 CUSTOMER NO                             
002200     03 4142-IDACCNT         PIC X(15).                                   
002300*                                 KONTOFÄLT                               
002400*                                 ACCOUNT FIELD                           
002500     03 4142-KDTRPPROC       PIC X(15).                                   
002600*                                 TRP KOD FÖR SPÅRNING AV KOLLIN          
002700*                                 CODE FOR TRACE/FOLLOW UP CASES          
002800     03 4142-KDTMSPROC       PIC X(15).                                   
002900*                                 TMS PROCESS INSTRUKTIONS KOD            
003000*                                 CODE FOR TMS PROCESSING                 
003100*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  

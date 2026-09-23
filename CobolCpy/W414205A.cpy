000100 01  205-W414205A.                                                        
000200*                                 205                                     
000300*                                 SKAPAS VID ANNULLATION                  
000400*                                 AV VOR RAD                              
000500*                                 ANVÄNDS VID TRANSAKTION-                
000600*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000700     03 205-IDARTNR          PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 205-IDDC             PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 205-IDDISTR          PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300     03 205-IDKUNDNR         PIC S9(7)           COMP-3.                  
001400*                                 KUNDNUMMER                              
001500     03 205-IDKUNDRF         PIC X(10).                                   
001600*                                 KUNDENS REFERENS (ORDERID)              
001700     03 205-KVAVBART         PIC S9(7)           COMP-3.                  
001800*                                 AVBOKAT ANTAL ARTIKLAR                  
001900     03 205-KVBEART-Q        PIC S9(7)           COMP-3.                  
002000*                                 BESTÄLLT KVANTANPASSAT ANTAL            
002100     03 205-FLDIRLEV         PIC X.                                       
002200*                                 DIREKTLEVERANS ?                        
002300     03 205-TIREGDAT         PIC S9(7)           COMP-3.                  
002400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002500     03 205-TIREGTID         PIC S9(7)           COMP-3.                  
002600*                                 REGISTRERINGSTID                        
002700*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  

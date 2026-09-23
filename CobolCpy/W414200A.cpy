000100 01  200-W414200A.                                                        
000200*                                 200                                     
000300*                                 SKAPAS FÖR ORDERHUVUD VID               
000400*                                 ORDERENTRY.                             
000500*                                 ANVÄNDS VID TRANSAKTION-                
000600*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000700     03 200-BEKUNDRF         PIC X(15).                                   
000800*                                 KUNDENS REFERENS                        
000900     03 200-BEVARREF         PIC X(10).                                   
001000*                                 VÅR REFERENS                            
001100     03 200-FLRESTN          PIC X.                                       
001200*                                 RESTNOTERING ?                          
001300     03 200-IDDISTR          PIC S9(5)           COMP-3.                  
001400*                                 DISTRIKTNUMMER                          
001500     03 200-IDKONTO          PIC S9(11)          COMP-3.                  
001600*                                 KONTO                                   
001700     03 200-IDKST            PIC X(10).                                   
001800*                                 KOSTNADSSTÄLLE                          
001900     03 200-IDKUNDNR         PIC S9(7)           COMP-3.                  
002000*                                 KUNDNUMMER                              
002100     03 200-IDKUNDRF         PIC X(10).                                   
002200*                                 KUNDENS REFERENS (ORDERID)              
002300     03 200-IDORDER          PIC S9(7)           COMP-3.                  
002400*                                 VOLVO PARTS ORDERNUMMER                 
002500     03 200-IDSKYLT          PIC X(3).                                    
002600*                                 NATIONALITETSTECKEN                     
002700*                                 SPRÅKIDENTIFIKATION                     
002800     03 200-IDDC             PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000     03 200-KDFAKTYP         PIC X.                                       
003100*                                 FAKTURATYP                              
003200     03 200-KDFRAKT          PIC S9(3)           COMP-3.                  
003300*                                 FRAKTSÄTT DC TILL KUND                  
003400     03 200-KDORDKL          PIC S9              COMP-3.                  
003500*                                 ORDERKLASS                              
003600     03 200-KDROPACK         PIC X.                                       
003700*                                 FRISLÄPPNINGSKOD RO/DO                  
003800     03 200-KDTULLVE         PIC S9              COMP-3.                  
003900*                                 TYP AV PRIS PÅ TULLFAKTURA              
004000     03 200-TIREGDAT         PIC S9(7)           COMP-3.                  
004100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004200     03 200-TIRFS            PIC S9(11)          COMP-3.                  
004300*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
004400     03 200-TITRPAVT.                                                     
004500*                                 TRANSPORTAVGÅNGSTIDPUNKT                
004600        05 200-TIAAMMDD      PIC S9(7)           COMP-3.                  
004700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004800        05 200-TIHHMM        PIC S9(5)           COMP-3.                  
004900*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
005000*** END OF VILMAII-COPY LENGTH= 91 BYTES                                  

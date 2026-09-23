000100 01  OHUV-W414001.                                                        
000200*                                 SKAPAS FÖR ORDERHUVUD VID               
000300*                                 TÖMNING TRANSAR.                        
000400*                                 ANVÄNDS VID TRANSAKTION-                
000500*                                 SKAPANDE TILL ÖVRIGA SYSTEM.            
000600     03 OHUV-IDPTYP          PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 OHUV-BEKUNDRF        PIC X(15).                                   
000900*                                 KUNDENS REFERENS                        
001000     03 OHUV-BEVARREF        PIC X(10).                                   
001100*                                 VÅR REFERENS                            
001200     03 OHUV-FLRESTN         PIC X.                                       
001300*                                 RESTNOTERING ?                          
001400     03 OHUV-IDDISTR         PIC S9(5)           COMP-3.                  
001500*                                 DISTRIKTNUMMER                          
001600     03 OHUV-IDKONTO         PIC S9(11)          COMP-3.                  
001700*                                 KONTO                                   
001800     03 OHUV-IDKST           PIC X(10).                                   
001900*                                 KOSTNADSSTÄLLE                          
002000     03 OHUV-IDKUNDNR        PIC S9(7)           COMP-3.                  
002100*                                 KUNDNUMMER                              
002200     03 OHUV-IDKUNDRF        PIC X(10).                                   
002300*                                 KUNDENS REFERENS (ORDERID)              
002400     03 OHUV-IDORDER         PIC S9(7)           COMP-3.                  
002500*                                 VOLVO PARTS ORDERNUMMER                 
002600     03 OHUV-IDSKYLT         PIC X(3).                                    
002700*                                 NATIONALITETSTECKEN                     
002800*                                 SPRÅKIDENTIFIKATION                     
002900     03 OHUV-IDDC            PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 OHUV-KDFAKTYP        PIC X.                                       
003200*                                 FAKTURATYP                              
003300     03 OHUV-KDFRAKT         PIC S9(3)           COMP-3.                  
003400*                                 FRAKTSÄTT DC TILL KUND                  
003500     03 OHUV-KDORDKL         PIC S9              COMP-3.                  
003600*                                 ORDERKLASS                              
003700     03 OHUV-KDROPACK        PIC X.                                       
003800*                                 FRISLÄPPNINGSKOD RO/DO                  
003900     03 OHUV-KDTULLVE        PIC S9              COMP-3.                  
004000*                                 TYP AV PRIS PÅ TULLFAKTURA              
004100     03 OHUV-TIREGDAT-ORDER  PIC S9(7)           COMP-3.                  
004200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004300     03 OHUV-TIRFS           PIC S9(11)          COMP-3.                  
004400*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
004500     03 OHUV-TITRPAVT.                                                    
004600*                                 TRANSPORTAVGÅNGSTIDPUNKT                
004700        05 OHUV-TIAAMMDD     PIC S9(7)           COMP-3.                  
004800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004900        05 OHUV-TIHHMM       PIC S9(5)           COMP-3.                  
005000*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
005100     03 OHUV-TIREGDAT        PIC S9(7)           COMP-3.                  
005200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005300     03 OHUV-TIKLOCK         PIC S9(9)           COMP-3.                  
005400*                                 KLOCKSLAG (TTMMSSTH)                    
005500*** END OF VILMAII-COPY LENGTH= 103 BYTES                                 

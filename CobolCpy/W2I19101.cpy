000100 01  MID-W2I19101.                                                        
000200*                                 COPYTEXT FÖR MID W2I19101               
000300*                                                                         
000400     03 MID-IDARTNR          PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-KDCLAGER         PIC X.                                       
000700*                                 CENTRALLAGERKOD                         
000800     03 MID-IDANSK           PIC X(3).                                    
000900*                                 ANSKAFFARNUMMER                         
001000     03 MID-TISENBEK.                                                     
001100*                                 SENASTE BEKRÄFTELSE TIDPUNKT            
001200        05 MID-TISENBEK-DAG  PIC X(6).                                    
001300*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001400        05 MID-TISENBEK-KL   PIC X(6).                                    
001500*                                 TIM - MIN - SEK   (HHMMSS)              
001600     03 MID-KDLARM           PIC X(3).                                    
001700*                                 LARMORSAKSKOD                           
001800     03 MID-IDGMTREF.                                                     
001900*                                 GODSMOTTAGAREREFERENS                   
002000        05 MID-IDDISTR       PIC X(4).                                    
002100*                                 DISTRIKTNUMMER                          
002200        05 MID-IDKUNDNR      PIC X(6).                                    
002300*                                 KUNDNUMMER                              
002400        05 MID-IDKUNDRF      PIC X(10).                                   
002500*                                 KUNDENS REFERENS (ORDERID)              
002600        05 MID-IDORDNR5-FILLER REDEFINES MID-IDKUNDRF.                    
002700           07 MID-IDORDNR5   PIC 9(5).                                    
002800*                                 ORDERNUMMER                             
002900           07 FILLER         PIC X(5).                                    
003000        05 MID-IDORDNR7-FILLER REDEFINES MID-IDKUNDRF.                    
003100           07 MID-IDORDNR7   PIC 9(7).                                    
003200*                                 ORDERNUMMER                             
003300           07 FILLER         PIC X(3).                                    
003400     03 MID-IDKR             PIC X(5).                                    
003500*                                 KONTROLLRAPPORT NUMMER                  
003600     03 MID-FLNYLARM         PIC X.                                       
003700*                                 ANGER OM ARTIKELLARMET ÄR NYTT          
003800     03 MID-IDDC             PIC X(2).                                    
003900*                                 IDENTIFIERARE LAGER                     
004000     03 MID-IDLEVNR          PIC X(5).                                    
004100*                                 LEVERANTÖRNUMMER                        
004200*** END OF VILMAII-COPY LENGTH= 61 BYTES                                  

000100 01  MID-W4I29501.                                                        
000200*                                 MID-COPYTEXT FÖR W40295                 
000300     03 MID-IDDISTR          PIC S9(5).                                   
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR         PIC S9(7).                                   
000600*                                 KUNDNUMMER                              
000700     03 MID-IDKUNDRF         PIC X(10).                                   
000800*                                 KUNDENS REFERENS (ORDERID)              
000900     03 MID-IDSID            PIC 9(3).                                    
001000*                                 SIDNUMRERING                            
001100     03 MID-IDSYSTEM         PIC X(4).                                    
001200*                                 VOLVO VCCS SYSTEMNUMMER                 
001300     03 MID-IDPRT            PIC X(3).                                    
001400*                                 LOGISK PRINTERIDENTITET                 
001500     03 MID-SUORDV           PIC 9(9)V9(2).                               
001600*                                 SUMMA ORDERVÄRDE                        
001700     03 MID-TIUPPDAT         PIC 9(6).                                    
001800*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001900     03 MID-TIUPPTID         PIC 9(8).                                    
002000*                                 UPPDATERINGSTID  (TTMMSSTH)             
002100     03 MID-NYCKEL-GRP.                                                   
002200        05 MID-IDORDER       PIC S9(7)           COMP-3.                  
002300*                                 VOLVO PARTS ORDERNUMMER                 
002400        05 MID-ADLAGOMR      PIC S9(3)           COMP-3.                  
002500*                                 LAGEROMRÅDE                             
002600        05 MID-ADGANG        PIC S9(3)           COMP-3.                  
002700*                                 GÅNG                                    
002800        05 MID-ADPLATS       PIC S9(5)           COMP-3.                  
002900*                                 LAGERPLATSNUMMER                        
003000        05 MID-IDARTNR       PIC S9(9)           COMP-3.                  
003100*                                 ARTIKELNUMMER                           
003200        05 MID-IDLOPNR       PIC S9(3)           COMP-3.                  
003300*                                 LÖPNUMMER                               
003400     03 MID-DEAL-PR-SUM.                                                  
003500*                                 DEALERPRIS (HUVUD)                      
003600        05 MID-SUORDV-LOC    PIC S9(9)V9(2)      COMP-3.                  
003700*                                 ORDERVÄRDE SLUTKUNDPRIS                 
003800*                                 I LOKAL VALUTA                          
003900        05 MID-SUORDV-LOCPREL                                             
004000                             PIC S9(9)V9(2)      COMP-3.                  
004100*                                 ORDERVÄRDE PREL SLUT-                   
004200*                                 KUNDPRIS, LOKAL VALUTA                  
004300        05 MID-KDVALISO      PIC X(3).                                    
004400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004500*** END OF VILMAII-COPY LENGTH= 90 BYTES                                  

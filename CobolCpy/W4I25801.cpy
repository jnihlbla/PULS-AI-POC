000100 01  MID-W4I25801.                                                        
000200*                                 MID-COPYTEXT F÷R W4I25801               
000300*                                 ORDERRADER                              
000400     03 MID-IDSYSTEM         PIC X(4).                                    
000500*                                 VOLVO VCCS SYSTEMNUMMER                 
000600     03 MID-IDDISTR          PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 MID-IDKUNDNR         PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 MID-IDORDNR7         PIC 9(7).                                    
001100*                                 ORDERNUMMER                             
001200     03 MID-TIREGDAT         PIC 9(6).                                    
001300*                                 REGISTRERINGSDATUM (≈≈MMDD OR ≈         
001400*                                 ≈≈≈-MM-DD)                              
001500     03 MID-FLSLUT           PIC X.                                       
001600*                                 AVSLUTNINGSFLAGGA                       
001700     03 MID-RADER            OCCURS 13 TIMES.                             
001800        05 MID-KDBEHX        PIC X.                                       
001900*                                 BEHANDLINGSKOD-X                        
002000        05 MID-IDARTNR       PIC X(9).                                    
002100*                                 ARTIKELNUMMER                           
002200        05 MID-REKSIFFR      PIC X.                                       
002300*                                 KONTROLLSIFFRA                          
002400        05 MID-KVBEART       PIC 9(6).                                    
002500*                                 BESTƒLLT ANTAL STYCKEN                  
002600        05 MID-PRARTNTO-LOC  PIC X(10).                                   
002700*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
002800        05 MID-KDVALISO      PIC X(3).                                    
002900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003000        05 MID-BERADREF      PIC X(10).                                   
003100*                                 KUNDENS RADREFERENS                     
003200*** END OF VILMAII-COPY LENGTH= 548 BYTES                                 

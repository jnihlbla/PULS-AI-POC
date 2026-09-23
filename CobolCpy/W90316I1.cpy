000100 01  REQU-W90316I1-CTX.                                                   
000200*                                 COPYTEXT TILL PROGRAM W9031600          
000300     03 REQU-IDAPIORDREF     PIC X(23).                                   
000400*                                 API ORDERID(DIS+KND+ORD+DAT)            
000500     03 REQU-IDAPIORDREF-REP REDEFINES REQU-IDAPIORDREF.                  
000600        05 REQU-IDDISTR      PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800        05 REQU-IDKUNDNR     PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000        05 REQU-IDORDNR7     PIC 9(7).                                    
001100*                                 ORDERNUMMER                             
001200        05 REQU-TIREGDAT     PIC 9(6).                                    
001300*                                 REGISTRERINGSDATUM (≈≈MMDD OR ≈         
001400*                                 ≈≈≈-MM-DD)                              
001500     03 REQU-KVRADER         PIC 9(5).                                    
001600*                                 ANTAL RADER                             
001700     03 REQU-W90316I1-001-GRP                                             
001800                             OCCURS 1 TO 999 TIMES                        
001900                             DEPENDING ON REQU-KVRADER.                   
002000        05 REQU-KDBEHX       PIC X.                                       
002100*                                 BEHANDLINGSKOD-X                        
002200        05 REQU-IDLEVART     PIC X(30).                                   
002300*                                 LEVERANT÷RENS ARTNR                     
002400        05 REQU-KVBEART      PIC 9(6).                                    
002500*                                 BESTƒLLT ANTAL STYCKEN                  
002600        05 REQU-PRARTNTO-LOC PIC 9(7)V9(2).                               
002700*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
002800        05 REQU-KDVALISO     PIC X(3).                                    
002900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003000        05 REQU-BERADREF     PIC X(10).                                   
003100*                                 KUNDENS RADREFERENS                     
003200*** END OF VILMAII-COPY LENGTH= 58969 BYTES                               

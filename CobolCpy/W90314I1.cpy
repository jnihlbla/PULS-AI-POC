000100 01  REQU-W90314I1-CTX.                                                   
000200*                                 COPYTEXT TILL PROGRAM W9031400          
000300     03 REQU-BEKUNDRF-001    PIC X(10).                                   
000400*                                 KUNDENS REFERENS                        
000500     03 REQU-KDORDKL-URS     PIC X.                                       
000600*                                 URSPRUNGLIG ORDERKLASS                  
000700     03 REQU-IDDISTR         PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 REQU-IDKUNDNR        PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 REQU-IDORDNR7        PIC 9(7).                                    
001200*                                 ORDERNUMMER                             
001300     03 REQU-KDFRAKT         PIC 9(2).                                    
001400*                                 FRAKTSÄTT DC TILL KUND                  
001500     03 REQU-BELAGINS        PIC X(60).                                   
001600*                                 DEL AV LAGERINSTRUKTION                 
001700     03 REQU-TIREPDAT        PIC X(10).                                   
001800*                                 REPAIR DATE                             
001900     03 REQU-TIAAAA-MM-DD REDEFINES REQU-TIREPDAT.                        
002000        05 REQU-TIAAAA       PIC 9(4).                                    
002100*                                 ÅRTAL (ÅÅÅÅ)                            
002200        05 REQU-TEHYPHEN     PIC X.                                       
002300         88 REQU-HYPHEN      VALUE '-'.                                   
002400*                                 BINDESTRECK                             
002500        05 REQU-TIMM         PIC 9(2).                                    
002600*                                 MÅNAD (MM)                              
002700        05 REQU-TEHYPHEN     PIC X.                                       
002800         88 REQU-HYPHEN      VALUE '-'.                                   
002900*                                 BINDESTRECK                             
003000        05 REQU-TIDD         PIC 9(2).                                    
003100*                                 DAG I MÅNAD (DD)                        
003200     03 REQU-W90314I1-001-GRP.                                            
003300        05 REQU-IDNAMN       PIC X(40).                                   
003400*                                 NAMN                                    
003500        05 REQU-IDMAIL       PIC X(60).                                   
003600*                                 MAIL ADRESS                             
003700        05 REQU-BETELNR      PIC X(20).                                   
003800*                                 TELEFONNUMMER                           
003900     03 REQU-W90314I1-002-GRP.                                            
004000        05 REQU-BEGMT-RAD1   PIC X(35).                                   
004100*                                 GODSMOTTAGARNAMN RAD 1                  
004200        05 REQU-BEGMT-RAD2   PIC X(35).                                   
004300*                                 GODSMOTTAGARNAMN RAD 2                  
004400        05 REQU-ADGMT-GATA   PIC X(35).                                   
004500*                                 GODSMOTTAGARADRESS GATA                 
004600        05 REQU-ADCITY       PIC X(20).                                   
004700        05 REQU-ADPOSTNR     PIC X(10).                                   
004800*                                 POSTNUMMER I ADRESS                     
004900        05 REQU-ADGMT-LAND   PIC X(35).                                   
005000*                                 GODSMOTTAGARADRESS LAND                 
005100        05 REQU-IDLANDX2     PIC X(2).                                    
005200*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
005300     03 REQU-KVRADER         PIC 9(5).                                    
005400*                                 ANTAL RADER                             
005500     03 REQU-W90314I1-003-GRP                                             
005600                             OCCURS 1 TO 999 TIMES                        
005700                             DEPENDING ON REQU-KVRADER.                   
005800        05 REQU-IDLEVART     PIC X(30).                                   
005900*                                 LEVERANTÖRENS ARTNR                     
006000        05 REQU-KVBEART      PIC 9(6).                                    
006100*                                 BESTÄLLT ANTAL STYCKEN                  
006200        05 REQU-PRARTNTO-LOC PIC 9(7)V9(2).                               
006300*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
006400        05 REQU-KDVALISO     PIC X(3).                                    
006500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006600        05 REQU-BERADREF     PIC X(10).                                   
006700*                                 KUNDENS RADREFERENS                     
006800*** END OF VILMAII-COPY LENGTH= 58339 BYTES                               

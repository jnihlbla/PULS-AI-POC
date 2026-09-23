000100 01  RESP-W40729O1.                                                       
000200*                                 RESPONS FROM PGM W40729 BUY BAC         
000300*                                 K NEW PRICE VALUES                      
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 RESP-IDDISTR-KEY     PIC Z(5).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 RESP-IDRAPPNR-KEY    PIC Z(6)9.                                   
000900*                                 RAPPORT NUMMER                          
001000     03 RESP-SUMINVD-BAS     PIC Z(2)9.                                   
001100*                                 MIN VÄRDE FÖR EN ORDERAD                
001200     03 RESP-SUMINVD-IN      PIC Z(2)9.                                   
001300*                                 MIN VÄRDE FÖR EN ORDERAD                
001400     03 RESP-REFOBNET-BAS    PIC Z(2)9.                                   
001500*                                 FOBNET I PROCENT                        
001600     03 RESP-REFOBNET-IN     PIC 9(3).                                    
001700*                                 FOBNET I PROCENT                        
001800     03 RESP-FLPRINT-BAS     PIC X.                                       
001900*                                 FLAGGA PRINTAD                          
002000     03 RESP-FLPERMIT-BAS    PIC X.                                       
002100*                                 FLAGGA RETURTILLSTÅND                   
002200     03 RESP-IDKUNDNR-BAS    PIC Z(5)9.                                   
002300*                                 KUNDNUMMER                              
002400     03 RESP-IDARTNR-NEXT    PIC Z(7)9.                                   
002500*                                 ARTIKELNUMMER                           
002600     03 RESP-FLMATCH-NEXT    PIC X.                                       
002700*                                 FLAGGA MATCH                            
002800     03 RESP-FLPRGRNS-NEXT   PIC X.                                       
002900*                                 RAD VÄRDE STÖRRE ÄN PRISGRÄNS           
003000     03 RESP-KVRADER         PIC Z(4)9.                                   
003100*                                 ANTAL RADER                             
003200     03 RESP-RAD-INFO        OCCURS 500 TIMES.                            
003300*                                 RAD-INFO                                
003400        05 RESP-IDARTNR-RAD  PIC Z(7)9.                                   
003500*                                 ARTIKELNUMMER                           
003600        05 RESP-BEART-RAD    PIC X(25).                                   
003700*                                 ARTIKELBENÄMNING                        
003800        05 RESP-KVANTAL-RAD  PIC Z(5)9.                                   
003900*                                 ANTAL                                   
004000        05 RESP-PRARTNTO-NEW-RAD                                          
004100                             PIC Z(6)9.9(2).                              
004200*                                 NYTT ARTIKELPRIS * FOBNET               
004300        05 RESP-PRARTNTO-TOT-RAD                                          
004400                             PIC Z(6)9.9(2).                              
004500*                                 NYTT ARTIKELPRIS * ANTAL                
004600        05 RESP-KDVALISO-RAD PIC X(3).                                    
004700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004800        05 RESP-FLMATCH-RAD  PIC X.                                       
004900*                                 FLAGGA MATCH                            
005000        05 RESP-FLPRGRNS-RAD PIC X.                                       
005100*                                 RAD VÄRDE STÖRRE ÄN PRISGRÄNS           
005200*** END OF VILMAII-COPY LENGTH= 32049 BYTES                               

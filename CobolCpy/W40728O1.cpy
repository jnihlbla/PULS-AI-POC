000100 01  RESP-W40728O1.                                                       
000200*                                 RESPONS FROM PGM W40728 BUY BAC         
000300*                                 K                                       
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 RESP-IDDISTR-KEY     PIC Z(5).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 RESP-IDRAPPNR-KEY    PIC Z(6)9.                                   
000900*                                 RAPPORT NUMMER                          
001000     03 RESP-FLMATCH-KEY     PIC X.                                       
001100*                                 FLAGGA MATCH                            
001200     03 RESP-FLPRGRNS-KEY    PIC X.                                       
001300*                                 RAD VÄRDE STÖRRE ÄN PRISGRÄNS           
001400     03 RESP-FLPRINT-IN      PIC X.                                       
001500*                                 FLAGGA PRINTAD                          
001600     03 RESP-FLPRINT-BAS     PIC X.                                       
001700*                                 FLAGGA PRINTAD                          
001800     03 RESP-FLPERMIT-IN     PIC X.                                       
001900*                                 FLAGGA RETURTILLSTÅND                   
002000     03 RESP-FLPERMIT-BAS    PIC X.                                       
002100*                                 FLAGGA RETURTILLSTÅND                   
002200     03 RESP-IDKUNDNR        PIC Z(5)9.                                   
002300*                                 KUNDNUMMER                              
002400     03 RESP-SUMINVD         PIC Z(2)9.                                   
002500*                                 MIN VÄRDE FÖR EN ORDERAD                
002600     03 RESP-REFOBNET        PIC Z(2)9.                                   
002700*                                 FOBNET I PROCENT                        
002800     03 RESP-IDARTNR-NEXT    PIC Z(7)9.                                   
002900*                                 ARTIKELNUMMER                           
003000     03 RESP-KVRADER         PIC Z(4)9.                                   
003100*                                 ANTAL RADER                             
003200     03 RESP-RAD-INFO        OCCURS 500 TIMES.                            
003300*                                 RAD-INFO                                
003400        05 RESP-KDCMD        PIC X.                                       
003500*                                 RAD-UPPDATERINGSKOMMANDO                
003600*                                  BLANK  = INGENTING                     
003700*                                  D , B  = DELETE                        
003800*                                  R , Ä  = REPLACE                       
003900*                                  I,N,A  = INSERT                        
004000*                                  S , V  = SELECT                        
004100*                                  P , P  = PRINT                         
004200*                                  C , K  = COPY                          
004300        05 RESP-IDRAPPNR-RAD PIC Z(6)9.                                   
004400*                                 RAPPORT NUMMER                          
004500        05 RESP-IDKUNDNR-RAD PIC Z(5)9.                                   
004600*                                 KUNDNUMMER                              
004700        05 RESP-TIREGDAT-RAD PIC 9(6).                                    
004800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004900        05 RESP-FLPRINT-RAD  PIC X.                                       
005000*                                 FLAGGA PRINTAD                          
005100        05 RESP-FLPERMIT-RAD PIC X.                                       
005200*                                 FLAGGA RETURTILLSTÅND                   
005300        05 RESP-IDARTNR-RAD  PIC Z(7)9.                                   
005400*                                 ARTIKELNUMMER                           
005500        05 RESP-BEART-RAD    PIC X(25).                                   
005600*                                 ARTIKELBENÄMNING                        
005700        05 RESP-KVANTAL-RAD  PIC Z(5)9.                                   
005800*                                 ANTAL                                   
005900        05 RESP-PRARTNTO-NEW-RAD                                          
006000                             PIC Z(6)9.9(2).                              
006100*                                 NYTT ARTIKELPRIS * FOBNET               
006200        05 RESP-PRARTNTO-TOT-RAD                                          
006300                             PIC Z(6)9.9(2).                              
006400*                                 NYTT ARTIKELPRIS * ANTAL                
006500        05 RESP-KDVALISO-RAD PIC X(3).                                    
006600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006700*** END OF VILMAII-COPY LENGTH= 42045 BYTES                               

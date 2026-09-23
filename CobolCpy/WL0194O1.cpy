000100 01  RESP-WL019401.                                                       
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 WL0194O1                                
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 RESP-IDDISTR-KEY     PIC Z(5).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 RESP-IDKUNDNR-KEY    PIC 9(7).                                    
000900*                                 KUNDNUMMER                              
001000     03 RESP-IDBYTRAP-KEY    PIC X(7).                                    
001100*                                 RAPPORTNUMMER  BYTES                    
001200     03 RESP-KDBYTSTA-KEY    PIC X.                                       
001300*                                 STATUSKOD BYTESOBJEKT                   
001400     03 RESP-KVRADER         PIC Z(4)9.                                   
001500*                                 ANTAL RADER                             
001600     03 RESP-DISTR-GRUPP     OCCURS 500 TIMES.                            
001700        05 RESP-KDCMD-RAD    PIC X.                                       
001800*                                 RAD-UPPDATERINGSKOMMANDO                
001900*                                  BLANK  = INGENTING                     
002000*                                  D , B  = DELETE                        
002100*                                  R , Ä  = REPLACE                       
002200*                                  I,N,A  = INSERT                        
002300*                                  S , V  = SELECT                        
002400*                                  P , P  = PRINT                         
002500*                                  C , K  = COPY                          
002600        05 RESP-IDDISTR      PIC Z(5).                                    
002700*                                 DISTRIKTNUMMER                          
002800        05 RESP-IDKUNDNR     PIC X(7).                                    
002900*                                 KUNDNUMMER                              
003000        05 RESP-IDFAKT       PIC Z(6)9.                                   
003100*                                 FAKTURANUMMER                           
003200        05 RESP-IDBYTRAP     PIC Z(6)9.                                   
003300*                                 RAPPORTNUMMER  BYTES                    
003400        05 RESP-KVRETUR-TOT  PIC -(6)9.                                   
003500*                                 ANTAL RETURER TOTALT                    
003600        05 RESP-TIREGDAT     PIC 9(6).                                    
003700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003800        05 RESP-KDBYTSTA     PIC X.                                       
003900*                                 STATUSKOD BYTESOBJEKT                   
004000        05 RESP-KDBYTSTA-IN  PIC X.                                       
004100*                                 STATUSKOD BYTESOBJEKT                   
004200        05 RESP-FLBYTGAR     PIC X.                                       
004300*                                 GARANTI RAPPORT FLAGGA                  
004400*                                 Y = GARANTI                             
004500*                                 N = EJ GARANTI                          
004600        05 RESP-ADBYTANK     PIC X(10).                                   
004700*                                 ANKOMSTADRESS BYTESOBJEKT               
004800        05 RESP-ADBYTANK-IN  PIC X(10).                                   
004900*                                 ANKOMSTADRESS BYTESOBJEKT               
005000        05 RESP-TIANKDAG     PIC 9(6).                                    
005100*                                 ANKOMSTDAG                              
005200        05 RESP-TIREGDAT-GODK                                             
005300                             PIC 9(6).                                    
005400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005500*** END OF VILMAII-COPY LENGTH= 37527 BYTES                               

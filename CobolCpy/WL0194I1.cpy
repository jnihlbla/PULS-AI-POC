000100 01  REQU-WL0194I1.                                                       
000200*                                 MID-COPYTEXT FÖR WL019400               
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDDISTR-KEY     PIC 9(5).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 REQU-IDKUNDNR-KEY    PIC 9(7).                                    
000800*                                 KUNDNUMMER                              
000900     03 REQU-IDBYTRAP-KEY    PIC 9(7).                                    
001000*                                 RAPPORTNUMMER  BYTES                    
001100     03 REQU-KDBYTSTA-KEY    PIC X.                                       
001200*                                 STATUSKOD BYTESOBJEKT                   
001300     03 REQU-KVRADER         PIC 9(5).                                    
001400*                                 ANTAL RADER                             
001500     03 REQU-INPUT           OCCURS 500 TIMES.                            
001600*                                 RADINFORMATION                          
001700        05 REQU-KDCMD-RAD    PIC X.                                       
001800         88 REQU-KDCMD-INGENTING                                          
001900                             VALUE ' '.                                   
002000         88 REQU-KDCMD-DELETE                                             
002100                             VALUE 'D'                                    
002200                             'B'.                                         
002300         88 REQU-KDCMD-REPLACE                                            
002400                             VALUE 'R'                                    
002500                             'Ä'.                                         
002600         88 REQU-KDCMD-INSERT                                             
002700                             VALUE 'I'                                    
002800                             'N'                                          
002900                             'A'.                                         
003000         88 REQU-KDCMD-SELECT                                             
003100                             VALUE 'S'                                    
003200                             'V'.                                         
003300         88 REQU-KDCMD-PRINT VALUE 'P'                                    
003400                             'P'.                                         
003500         88 REQU-KDCMD-COPY  VALUE 'C'                                    
003600                             'K'.                                         
003700*                                 RAD-UPPDATERINGSKOMMANDO                
003800*                                  BLANK  = INGENTING                     
003900*                                  D , B  = DELETE                        
004000*                                  R , Ä  = REPLACE                       
004100*                                  I,N,A  = INSERT                        
004200*                                  S , V  = SELECT                        
004300*                                  P , P  = PRINT                         
004400*                                  C , K  = COPY                          
004500        05 REQU-IDDISTR      PIC 9(5).                                    
004600*                                 DISTRIKTNUMMER                          
004700        05 REQU-IDBYTRAP     PIC 9(7).                                    
004800*                                 RAPPORTNUMMER  BYTES                    
004900        05 REQU-KDBYTSTA-IN  PIC X.                                       
005000*                                 STATUSKOD BYTESOBJEKT                   
005100        05 REQU-ADBYTANK-IN  PIC X(10).                                   
005200*                                 ANKOMSTADRESS BYTESOBJEKT               
005300*** END OF VILMAII-COPY LENGTH= 12027 BYTES                               

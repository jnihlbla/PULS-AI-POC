000100 01  REQU-WL0168I1.                                                       
000200*                                 MID-COPYTEXT FÖR WL016800               
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDDC-REC-KEY    PIC X(2).                                    
000600*                                 MOTTAGANDE LAGER                        
000700     03 REQU-IDDISTR-KEY     PIC 9(5).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 REQU-IDKUNDNR-KEY    PIC 9(7).                                    
001000*                                 KUNDNUMMER                              
001100     03 REQU-IDBYTRAP-KEY    PIC 9(7).                                    
001200*                                 RAPPORTNUMMER  BYTES                    
001300     03 REQU-VKORDBTO        PIC X(8).                                    
001400*                                 ORDERVIKT BRUTTO (KG)                   
001500     03 REQU-VLORDBTO        PIC X(8).                                    
001600*                                 ORDERVOLYM BRUTTO (M3)                  
001700     03 REQU-FLKLAR          PIC X.                                       
001800*                                 AVSLUTNINGSMARKERING                    
001900     03 REQU-KVRADER         PIC 9(5).                                    
002000*                                 ANTAL RADER                             
002100     03 REQU-INPUT           OCCURS 3000 TIMES.                           
002200*                                 RADINFORMATION                          
002300        05 REQU-KDCMD-RAD    PIC X.                                       
002400         88 REQU-KDCMD-INGENTING                                          
002500                             VALUE ' '.                                   
002600         88 REQU-KDCMD-DELETE                                             
002700                             VALUE 'D'                                    
002800                             'B'.                                         
002900         88 REQU-KDCMD-REPLACE                                            
003000                             VALUE 'R'                                    
003100                             'Ä'.                                         
003200         88 REQU-KDCMD-INSERT                                             
003300                             VALUE 'I'                                    
003400                             'N'                                          
003500                             'A'.                                         
003600         88 REQU-KDCMD-SELECT                                             
003700                             VALUE 'S'                                    
003800                             'V'.                                         
003900         88 REQU-KDCMD-PRINT VALUE 'P'                                    
004000                             'P'.                                         
004100         88 REQU-KDCMD-COPY  VALUE 'C'                                    
004200                             'K'.                                         
004300*                                 RAD-UPPDATERINGSKOMMANDO                
004400*                                  BLANK  = INGENTING                     
004500*                                  D , B  = DELETE                        
004600*                                  R , Ä  = REPLACE                       
004700*                                  I,N,A  = INSERT                        
004800*                                  S , V  = SELECT                        
004900*                                  P , P  = PRINT                         
005000*                                  C , K  = COPY                          
005100        05 REQU-IDDISTR      PIC 9(5).                                    
005200*                                 DISTRIKTNUMMER                          
005300        05 REQU-IDBYTRAP     PIC 9(7).                                    
005400*                                 RAPPORTNUMMER  BYTES                    
005500*** END OF VILMAII-COPY LENGTH= 39045 BYTES                               

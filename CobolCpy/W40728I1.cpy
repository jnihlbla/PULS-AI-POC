000100 01  REQU-W40728I1.                                                       
000200*                                 REQUEST TO PGM W40728                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDDISTR-KEY     PIC 9(5).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 REQU-IDRAPPNR-KEY    PIC 9(7).                                    
000800*                                 RAPPORT NUMMER                          
000900     03 REQU-FLMATCH-KEY     PIC X.                                       
001000*                                 FLAGGA MATCH                            
001100     03 REQU-FLPRGRNS-KEY    PIC X.                                       
001200*                                 RAD VÄRDE STÖRRE ÄN PRISGRÄNS           
001300     03 REQU-FLPRINT-IN      PIC X.                                       
001400*                                 FLAGGA PRINTAD                          
001500     03 REQU-FLPERMIT-IN     PIC X.                                       
001600*                                 FLAGGA RETURTILLSTÅND                   
001700     03 REQU-IDARTNR-NEXT    PIC 9(8).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 REQU-KVRADER         PIC 9(5).                                    
002000*                                 ANTAL RADER                             
002100     03 REQU-BUYBACK-GRP     OCCURS 500 TIMES.                            
002200*                                 INVENTERINGSARTIKELGRUPP                
002300        05 REQU-KDCMD        PIC X.                                       
002400*                                 RAD-UPPDATERINGSKOMMANDO                
002500*                                  BLANK  = INGENTING                     
002600*                                  D , B  = DELETE                        
002700*                                  R , Ä  = REPLACE                       
002800*                                  I,N,A  = INSERT                        
002900*                                  S , V  = SELECT                        
003000*                                  P , P  = PRINT                         
003100*                                  C , K  = COPY                          
003200        05 REQU-IDRAPPNR     PIC 9(7).                                    
003300*                                 RAPPORT NUMMER                          
003400*** END OF VILMAII-COPY LENGTH= 4031 BYTES                                

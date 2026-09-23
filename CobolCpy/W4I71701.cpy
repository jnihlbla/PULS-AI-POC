000100 01  MID-W4I71701.                                                        
000200*                                 MID-COPYTEXT FÖR W4071700               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDRAPPNR-IN      PIC X(7).                                    
000800*                                 RAPPORT NUMMER                          
000900     03 MID-IDUSER-IN        PIC X(8).                                    
001000*                                 ANVÄNDARENS SÄKERHETS ID                
001100     03 MID-FLTOT-IN         PIC X.                                       
001200*                                 INDIKERAR TOTALINFO VISNING             
001300     03 MID-INPUT.                                                        
001400        05 MID-FLGODK        PIC X.                                       
001500*                                 GODKÄNT?  JA/NEJ                        
001600        05 MID-KDCMD         OCCURS 14 TIMES                              
001700                             PIC X.                                       
001800         88 MID-KDCMD-INGENTING                                           
001900                             VALUE ' '.                                   
002000         88 MID-KDCMD-DELETE VALUE 'D'                                    
002100                             'B'.                                         
002200         88 MID-KDCMD-REPLACE                                             
002300                             VALUE 'R'                                    
002400                             'Ä'.                                         
002500         88 MID-KDCMD-INSERT VALUE 'I'                                    
002600                             'N'                                          
002700                             'A'.                                         
002800         88 MID-KDCMD-SELECT VALUE 'S'                                    
002900                             'V'.                                         
003000         88 MID-KDCMD-PRINT  VALUE 'P'                                    
003100                             'P'.                                         
003200         88 MID-KDCMD-COPY   VALUE 'C'                                    
003300                             'K'.                                         
003400*                                 RAD-UPPDATERINGSKOMMANDO                
003500*                                  BLANK  = INGENTING                     
003600*                                  D , B  = DELETE                        
003700*                                  R , Ä  = REPLACE                       
003800*                                  I,N,A  = INSERT                        
003900*                                  S , V  = SELECT                        
004000*                                  P , P  = PRINT                         
004100*                                  C , K  = COPY                          
004200     03 MID-RAD-INFO         OCCURS 14 TIMES.                             
004300        05 MID-IDDISTR       PIC X(4).                                    
004400*                                 DISTRIKTNUMMER                          
004500        05 MID-IDKUNDNR      PIC X(6).                                    
004600*                                 KUNDNUMMER                              
004700        05 MID-IDRAPPNR      PIC X(7).                                    
004800*                                 RAPPORT NUMMER                          
004900        05 MID-IDDC          PIC X(2).                                    
005000*                                 IDENTIFIERARE LAGER                     
005100        05 MID-KDKRENOT      PIC X(2).                                    
005200*                                 TYP AV KREDITERING                      
005300        05 MID-SUKRENOT      PIC X(10).                                   
005400*                                 KREDITNOTASUMMA                         
005500        05 MID-IDUSER        PIC X(8).                                    
005600*                                 ANVÄNDARENS SÄKERHETS ID                
005700*** END OF VILMAII-COPY LENGTH= 587 BYTES                                 

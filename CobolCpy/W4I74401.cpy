000100 01  MID-W4I74401.                                                        
000200*                                 MID-COPYTEXT FÖR W40744                 
000300     03 MID-IDANSV-IN        PIC X(6).                                    
000400     03 MID-IDANSV-UT        PIC X(6).                                    
000500     03 MID-IDDISTR-IN       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDDISTR-UT       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MID-KDRETSTA-IN      PIC X.                                       
001400*                                 STATUS RETURER                          
001500     03 MID-KDRETSTA-UT      PIC X.                                       
001600*                                 STATUS RETURER                          
001700     03 MID-FLSUM-IN         PIC X.                                       
001800*                                 ALLMÄN FLAGGA                           
001900     03 MID-FLSUM-UT         PIC X.                                       
002000*                                 ALLMÄN FLAGGA                           
002100     03 MID-INPUT.                                                        
002200*                                 INMATNINGSFÄLT                          
002300        05 MID-INPUTRAD      OCCURS 11 TIMES.                             
002400*                                 INMATNINGSFÄLT                          
002500           07 MID-KDCMD      PIC X.                                       
002600            88 MID-KDCMD-INGENTING                                        
002700                             VALUE ' '.                                   
002800            88 MID-KDCMD-DELETE                                           
002900                             VALUE 'D'                                    
003000                             'B'.                                         
003100            88 MID-KDCMD-REPLACE                                          
003200                             VALUE 'R'                                    
003300                             'Ä'.                                         
003400            88 MID-KDCMD-INSERT                                           
003500                             VALUE 'I'                                    
003600                             'N'.                                         
003700*                                 RAD-UPPDATERINGSKOMMANDO                
003800*                                  BLANK  = INGENTING                     
003900*                                  D , B  = DELETE                        
004000*                                  R , Ä  = REPLACE                       
004100*                                  I , N  = INSERT                        
004200     03 MID-KEYFIELD         OCCURS 11 TIMES.                             
004300*                                 NYCKELFÄLT PÅ RADEN                     
004400        05 MID-IDDISTR       PIC X(4).                                    
004500*                                 DISTRIKTNUMMER                          
004600        05 MID-IDKUNDNR      PIC X(6).                                    
004700*                                 KUNDNUMMER                              
004800        05 MID-IDRAPPNR      PIC X(7).                                    
004900*                                 RAPPORT NUMMER                          
005000*** END OF VILMAII-COPY LENGTH= 234 BYTES                                 

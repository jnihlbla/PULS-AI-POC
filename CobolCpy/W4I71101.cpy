000100 01  MID-W4I71101.                                                        
000200*                                 MID-COPYTEXT FÖR W4071100               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-KDLEVANM-IN      PIC X.                                       
001200*                                 STATUS LEVERANSANMÄRKNING               
001300     03 MID-KDLEVANM-UT      PIC X.                                       
001400*                                 STATUS LEVERANSANMÄRKNING               
001500     03 MID-IDDISTR-ENTER    PIC X(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 MID-IDKUNDNR-ENTER   PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MID-IDRAPPNR-ENTER   PIC X(7).                                    
002000*                                 RAPPORT NUMMER                          
002100     03 MID-KDLEVANM-ENTER   PIC X.                                       
002200*                                 STATUS LEVERANSANMÄRKNING               
002300     03 MID-IDDISTR-NEXT     PIC X(4).                                    
002400*                                 DISTRIKTNUMMER                          
002500     03 MID-IDKUNDNR-NEXT    PIC X(6).                                    
002600*                                 KUNDNUMMER                              
002700     03 MID-IDRAPPNR-NEXT    PIC X(7).                                    
002800*                                 RAPPORT NUMMER                          
002900     03 MID-KDLEVANM-NEXT    PIC X.                                       
003000*                                 STATUS LEVERANSANMÄRKNING               
003100     03 MID-RAD-INFO         OCCURS 11 TIMES.                             
003200*                                 NYCKELFÄLT PÅ RADEN                     
003300        05 MID-KDCMDVAL      PIC X(3).                                    
003400*                                 GENERELL KOMMANDOKOD                    
003500        05 MID-IDDISTR       PIC 9(4).                                    
003600*                                 DISTRIKTNUMMER                          
003700        05 MID-IDKUNDNR      PIC 9(6).                                    
003800*                                 KUNDNUMMER                              
003900        05 MID-IDRAPPNR      PIC 9(7).                                    
004000*                                 RAPPORT NUMMER                          
004100     03 MID-INPUT.                                                        
004200*                                 INMATNINGSFÄLT                          
004300        05 MID-IDDISTR-ANN   PIC X(4).                                    
004400*                                 DISTRIKTNUMMER                          
004500        05 MID-IDKUNDNR-ANN  PIC X(6).                                    
004600*                                 KUNDNUMMER                              
004700        05 MID-IDRAPPNR-ANN  PIC X(7).                                    
004800*                                 RAPPORT NUMMER                          
004900        05 MID-FLSVAR-ANN    PIC X.                                       
005000*                                 ALLMÄN SVARSFLAGGA                      
005100        05 MID-FLSVAR-GODKANN                                             
005200                             PIC X.                                       
005300*                                 ALLMÄN SVARSFLAGGA                      
005400*** END OF VILMAII-COPY LENGTH= 297 BYTES                                 

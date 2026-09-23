000100 01  MID-W4I73601.                                                        
000200*                                 MID-COPYTEXT FÖR W40736                 
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
001300     03 MID-KDLEVANM-FOM-IN  PIC X.                                       
001400*                                 STATUS LEVERANSANMÄRKNING               
001500     03 MID-KDLEVANM-FOM-UT  PIC X.                                       
001600*                                 STATUS LEVERANSANMÄRKNING               
001700     03 MID-KDLEVANM-TOM-IN  PIC X.                                       
001800*                                 STATUS LEVERANSANMÄRKNING               
001900     03 MID-KDLEVANM-TOM-UT  PIC X.                                       
002000*                                 STATUS LEVERANSANMÄRKNING               
002100     03 MID-FLSUM-IN         PIC X.                                       
002200*                                 ALLMÄN FLAGGA                           
002300     03 MID-FLSUM-UT         PIC X.                                       
002400*                                 ALLMÄN FLAGGA                           
002500     03 MID-IDDC-IN          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MID-IDDC-UT          PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900     03 MID-INPUT.                                                        
003000*                                 INMATNINGSFÄLT                          
003100        05 MID-IDPRT         PIC X(3).                                    
003200*                                 LOGISK PRINTERIDENTITET                 
003300        05 MID-KDCMD         OCCURS 11 TIMES                              
003400                             PIC X(4).                                    
003500     03 MID-KEYFIELD         OCCURS 11 TIMES.                             
003600*                                 NYCKELFÄLT PÅ RADEN                     
003700        05 MID-IDDISTR       PIC X(4).                                    
003800*                                 DISTRIKTNUMMER                          
003900        05 MID-IDKUNDNR      PIC X(6).                                    
004000*                                 KUNDNUMMER                              
004100        05 MID-IDRAPPNR      PIC X(7).                                    
004200*                                 RAPPORT NUMMER                          
004300*** END OF VILMAII-COPY LENGTH= 276 BYTES                                 

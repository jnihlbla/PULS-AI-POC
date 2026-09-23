000100 01  MID-W6I30101.                                                        
000200*                                                                         
000300     03 MID-TIFAKT-IN        PIC X(6).                                    
000400*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
000500     03 MID-TIFAKT-UT        PIC X(6).                                    
000600*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
000700     03 MID-IDLBBET-IN       PIC X(12).                                   
000800*                                 LASTBÄRARBETECKNING                     
000900     03 MID-IDLBBET-UT       PIC X(12).                                   
001000*                                 LASTBÄRARBETECKNING                     
001100     03 MID-IDDC-SEND-IN     PIC X(2).                                    
001200*                                 SÄNDANDE LAGER                          
001300     03 MID-IDDC-SEND-UT     PIC X(2).                                    
001400*                                 SÄNDANDE LAGER                          
001500     03 MID-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-TIBERANK-ENTER   PIC 9(6).                                    
002000*                                 BERÄKNAD ANKOMSTDATUM                   
002100     03 MID-TIBERANK-NEXT    PIC 9(6).                                    
002200*                                 BERÄKNAD ANKOMSTDATUM                   
002300     03 MID-IDFAKT-ENTER     PIC 9(7).                                    
002400*                                 FAKTURANUMMER                           
002500     03 MID-IDFAKT-NEXT      PIC 9(7).                                    
002600*                                 FAKTURANUMMER                           
002700     03 MID-IDDC-ENTER       PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900     03 MID-IDDC-NEXT        PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 MID-INPUT            OCCURS 13 TIMES.                             
003200*                                                                         
003300        05 MID-CMD-IN        PIC X(3).                                    
003400        05 MID-IDFAKT        PIC 9(7).                                    
003500*                                 FAKTURANUMMER                           
003600        05 MID-TIBERANK      PIC X(6).                                    
003700*                                 BERÄKNAD ANKOMSTDATUM                   
003800        05 MID-ADINLOMR      PIC X(4).                                    
003900*                                 INLEVERANSOMRÅDE                        
004000     03 MID-ADINLOMR-PRT     PIC X(4).                                    
004100*                                 PRINTERPLACERING                        
004200*** END OF VILMAII-COPY LENGTH= 338 BYTES                                 

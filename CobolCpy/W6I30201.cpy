000100 01  MID-W6I30201.                                                        
000200*                                                                         
000300     03 MID-IDFAKT-IN        PIC X(7).                                    
000400*                                 FAKTURANUMMER                           
000500     03 MID-IDFAKT-UT        PIC X(7).                                    
000600*                                 FAKTURANUMMER                           
000700     03 MID-IDKUNDRF-ENTER   PIC X(10).                                   
000800*                                 KUNDENS REFERENS (ORDERID)              
000900     03 MID-IDKUNDRF-NEXT    PIC X(10).                                   
001000*                                 KUNDENS REFERENS (ORDERID)              
001100     03 MID-IDKUNDNR-ENTER   PIC 9(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MID-IDKUNDNR-NEXT    PIC 9(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-IDKOLLI-ENTER    PIC 9(5).                                    
001600*                                 KOLLINUMMER                             
001700     03 MID-IDKOLLI-NEXT     PIC 9(5).                                    
001800*                                 KOLLINUMMER                             
001900     03 MID-IDDC-SPAR        PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MID-IDUSER-003       PIC X(5).                                    
002200*                                 ANSVARIGT USERID INLÄGGN.(R32)          
002300     03 MID-ADINLOMR-PRT     PIC X(4).                                    
002400*                                 PRINTERPLACERING                        
002500     03 MID-INPUT            OCCURS 12 TIMES.                             
002600*                                                                         
002700        05 MID-CMD-IN        PIC X(3).                                    
002800        05 MID-IDKUNDRF      PIC X(10).                                   
002900*                                 KUNDENS REFERENS (ORDERID)              
003000        05 MID-IDKUNDNR      PIC 9(6).                                    
003100*                                 KUNDNUMMER                              
003200        05 MID-IDKOLLI       PIC 9(5).                                    
003300*                                 KOLLINUMMER                             
003400        05 MID-ADINLOMR      PIC X(4).                                    
003500*                                 INLEVERANSOMRÅDE                        
003600*** END OF VILMAII-COPY LENGTH= 403 BYTES                                 

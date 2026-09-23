000100 01  MID-W1I11301.                                                        
000200     03 MID-IDARTNR-IN       PIC X(9).                                    
000300*                                 ARTIKELNUMMER                           
000400     03 MID-IDARTNR-UT       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDKORTNR-SPAR1   PIC X(3).                                    
000700*                                 KORTNUMMER                              
000800*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
000900     03 MID-IDKORTNR-SPAR2   PIC X(3).                                    
001000*                                 KORTNUMMER                              
001100*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
001200     03 MID-IDKORTNR-SPAR3   PIC X(3).                                    
001300*                                 KORTNUMMER                              
001400*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
001500     03 MID-DIERS-ERS        PIC X(7).                                    
001600*                                 KVANTITET I ERSÄTTN.                    
001700     03 MID-KDERS            PIC 9(2).                                    
001800*                                 ERSÄTTNINGSKOD                          
001900     03 MID-IDAO             PIC X(10).                                   
002000*                                 ÄNDRINGSORDERNUMMER                     
002100     03 MID-TIERSDAT-PREL    PIC X(5).                                    
002200*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
002300     03 MID-RAD              OCCURS 9 TIMES.                              
002400        05 MID-IDKORTNR      PIC X(3).                                    
002500*                                 KORTNUMMER                              
002600*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
002700        05 MID-FLTEXT        PIC X.                                       
002800*                                 FINNS TEXTINFORMATION ?                 
002900        05 MID-IDARTNR-TILLK PIC X(9).                                    
003000*                                 ARTIKELNUMMER                           
003100        05 MID-DIERS-TILLK   PIC X(7).                                    
003200*                                 KVANTITET I ERSÄTTN.                    
003300        05 MID-BEERS         PIC X(20).                                   
003400*                                 ERSÄTTNINGSTEXT                         
003500     03 MID-TEARTNOT         PIC X(40).                                   
003600*                                 ARTIKEL NOTERING                        
003700     03 MID-FLKLAR           PIC X.                                       
003800*                                 AVSLUTNINGSMARKERING                    
003900*** END COPY W1I11301C0  LENGTH=452                                       

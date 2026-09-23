000100 01  MID-W90408I1.                                                        
000200     03 MID-IDARTNR-IN       PIC X(9).                                    
000300*                                 ARTIKELNUMMER                           
000400     03 MID-IDARTNR-UT       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-FILLER           PIC X(3).                                    
000700     03 MID-FILLER           PIC X(3).                                    
000800     03 MID-FILLER           PIC X(3).                                    
000900     03 MID-DIERS-ERS        PIC X(7).                                    
001000*                                 KVANTITET I ERSÄTTN.                    
001100     03 MID-KDERS            PIC 9(2).                                    
001200*                                 ERSÄTTNINGSKOD                          
001300     03 MID-IDAO             PIC X(10).                                   
001400*                                 ÄNDRINGSORDERNUMMER                     
001500     03 MID-TIERSDAT-PREL    PIC X(5).                                    
001600*                                 PRELIMINÄRT ERSÄTTNINGSDATUM            
001700     03 MID-RAD              OCCURS 9 TIMES.                              
001800        05 MID-IDKORTNR      PIC X(3).                                    
001900*                                 KORTNUMMER                              
002000*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
002100        05 MID-FILLER        PIC X.                                       
002200        05 MID-IDARTNR-TILLK PIC X(9).                                    
002300*                                 ARTIKELNUMMER                           
002400        05 MID-DIERS-TILLK   PIC X(7).                                    
002500*                                 KVANTITET I ERSÄTTN.                    
002600        05 MID-BEERS         PIC X(20).                                   
002700*                                 ERSÄTTNINGSTEXT                         
002800     03 MID-TEARTNOT         PIC X(40).                                   
002900*                                 ARTIKEL NOTERING                        
003000     03 MID-FLKLAR           PIC X.                                       
003100*                                 AVSLUTNINGSMARKERING                    
003200*** END OF VILMAII-COPY LENGTH= 452 BYTES                                 

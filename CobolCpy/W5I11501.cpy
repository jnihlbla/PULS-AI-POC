000100 01  MID-W5I11501.                                                        
000200*                                 MID-COPY TEXT FÖR W5011500              
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MID-IDLEVNR-UT       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MID-KDPRBEH-IN       PIC X.                                       
001200*                                 PRIS BEHANDLAD ARTIKEL                  
001300     03 MID-KDPRBEH-UT       PIC X.                                       
001400*                                 PRIS BEHANDLAD ARTIKEL                  
001500     03 MID-REAENDR-IN       PIC X(6).                                    
001600*                                 ÄNDRINGSPROCENT                         
001700     03 MID-REAENDR-UT       PIC X(6).                                    
001800*                                 ÄNDRINGSPROCENT                         
001900     03 MID-IDLEVNR-ENTER    PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100     03 MID-IDLEVNR-NEXT     PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300     03 MID-IDARTNR-ENTER    PIC X(9).                                    
002400*                                 ARTIKELNUMMER                           
002500     03 MID-IDARTNR-NEXT     PIC X(9).                                    
002600*                                 ARTIKELNUMMER                           
002700     03 MID-KDPRBEH-ENTER    PIC X.                                       
002800*                                 PRIS BEHANDLAD ARTIKEL                  
002900     03 MID-KDPRBEH-NEXT     PIC X.                                       
003000*                                 PRIS BEHANDLAD ARTIKEL                  
003100     03 MID-REAENDR-ENTER    PIC X(6).                                    
003200*                                 ÄNDRINGSPROCENT                         
003300     03 MID-REAENDR-NEXT     PIC X(6).                                    
003400*                                 ÄNDRINGSPROCENT                         
003500     03 MID-INPUT            OCCURS 12 TIMES.                             
003600        05 MID-SELECT-URVAL  PIC X.                                       
003700        05 MID-IDARTNR       PIC X(9).                                    
003800*                                 ARTIKELNUMMER                           
003900        05 MID-KDPRBEH-IN-UT PIC X.                                       
004000*                                 PRIS BEHANDLAD ARTIKEL                  
004100*** END COPY W5I11501    LENGTH=216                                       

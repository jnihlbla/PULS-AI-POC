000100 01  MID-W4I26601.                                                        
000200*                                 MID-COPYTEXT FÖR W4026600               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDORDNR7-IN      PIC X(7).                                    
000800*                                 ORDERNUMMER                             
000900     03 MID-IDARTNR-IN       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MID-IDDISTR-UT       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-IDORDNR7-UT      PIC X(7).                                    
001600*                                 ORDERNUMMER                             
001700     03 MID-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MID-INPUT.                                                        
002000*                                 UPPDATERINGSFÄLT                        
002100        05 MID-VKORDBTO      PIC X(8).                                    
002200*                                 ORDERVIKT BRUTTO (KG)                   
002300        05 MID-VLORDBTO      PIC X(8).                                    
002400*                                 ORDERVOLYM BRUTTO (M3)                  
002500        05 MID-KDLEVVIL      PIC 9.                                       
002600*                                 LEVERANSVILLKOR                         
002700        05 MID-TIGILTIG      PIC 9(6).                                    
002800*                                 GILTIGHETSDATUM (ÅÅMMDD)                
002900        05 MID-TIFORDAT      PIC 9(6).                                    
003000*                                 FÖRFALLODATUM                           
003100     03 MID-FLAGGA-PRELPRIS  PIC X.                                       
003200*                                 ALLMÄN FLAGGA                           
003300     03 MID-IDPRT-UTSKRIFT   PIC X(3).                                    
003400*                                 LOGISK PRINTERIDENTITET                 
003500     03 MID-IDPRT-RELEASE    PIC X(3).                                    
003600*                                 LOGISK PRINTERIDENTITET                 
003700     03 MID-IDKUNDNR-NY      PIC X(6).                                    
003800*                                 KUNDNUMMER                              
003900*** END OF VILMAII-COPY LENGTH= 94 BYTES                                  

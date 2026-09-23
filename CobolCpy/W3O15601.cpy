000100 01  MOD-W3O15601.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 MOD-TIAAVV-FOM       PIC 9(4).                                    
000700*                                 ÅR - VECKA  (ÅÅVV)                      
000800     03 MOD-TIAAVV-TOM       PIC 9(4).                                    
000900*                                 ÅR - VECKA  (ÅÅVV)                      
001000     03 MOD-IDDISTR          PIC Z(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDARTNR          PIC Z(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 MOD-BEART            PIC X(25).                                   
001500*                                 ARTIKELBENÄMNING                        
001600     03 MOD-DIST-RAD         OCCURS 13 TIMES.                             
001700*                                 RADUPPGIFTER   FÖR 3156 BILDEN          
001800        05 MOD-CMD-UT-ATTR   PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000        05 MOD-CMD-UT        PIC X.                                       
002100        05 MOD-IDKUNDNR      PIC Z(7).                                    
002200*                                 KUNDNUMMER                              
002300        05 MOD-OUTLEV        PIC -(7)9.                                   
002400*                                 ANTAL                                   
002500        05 MOD-INLEV         PIC -(7)9.                                   
002600*                                 ANTAL                                   
002700        05 MOD-DIFF          PIC -(7)9.                                   
002800*                                 ANTAL                                   
002900        05 MOD-PROC          PIC -(3)9.                                   
003000*                                 ALLMÄNT PROCENTTALSFÄLT                 
003100     03 MOD-OUT-TOT          PIC -(7)9.                                   
003200*                                 ANTAL                                   
003300     03 MOD-IN-TOT           PIC -(7)9.                                   
003400*                                 ANTAL                                   
003500     03 MOD-DIFF-TOT         PIC -(7)9.                                   
003600*                                 ANTAL                                   
003700     03 MOD-PROC-TOT         PIC -(3)9.                                   
003800*                                 ALLMÄNT PROCENTTALSFÄLT                 
003900     03 MOD-TEMFSINF         PIC X(55).                                   
004000*                                 INFORMATIONSMEDDELANDE                  
004100*** END OF VILMAII-COPY LENGTH= 667 BYTES                                 

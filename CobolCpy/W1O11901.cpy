000100 01  MOD-W1O11901.                                                        
000200*                                 MOD-COPYTEXT FÖR W1O11900               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL-ATTR    PIC X(2).                                    
000600*                                 MFS ATTRIBUTFÄLT                        
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900     03 MOD-IDARTNR-IN-ATTR  PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100     03 MOD-IDARTNR-IN       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDARTNR-UT       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-RADER.                                                        
001600*                                 MOD-COPYTEXT FÖR W1O11900 MULTI         
001700*                                 RADER                                   
001800        05 MOD-UP            OCCURS 10 TIMES.                             
001900           07 MOD-UPDAT-ATTR PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100           07 MOD-UPDAT      PIC X.                                       
002200        05 MOD-ID            OCCURS 10 TIMES.                             
002300           07 MOD-IDARTV-ATTR                                             
002400                             PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600           07 MOD-IDARTV     PIC X(9).                                    
002700*                                 ARTIKELNUMMER                           
002800        05 MOD-BE            OCCURS 10 TIMES.                             
002900           07 MOD-BEARTV-ATTR                                             
003000                             PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200           07 MOD-BEARTV     PIC X(25).                                   
003300*                                 ARTIKELBENÄMNING                        
003400        05 MOD-PA            OCCURS 10 TIMES.                             
003500           07 MOD-PRODA-ATTR PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700           07 MOD-PRODA      PIC Z9.                                      
003800*                                 PRODUKTSLAG                             
003900        05 MOD-PN            OCCURS 10 TIMES.                             
004000           07 MOD-PRODN-ATTR PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200           07 MOD-PRODN      PIC Z(2).                                    
004300*                                 PRODUKTSLAG                             
004400        05 MOD-PB            OCCURS 10 TIMES.                             
004500           07 MOD-PRODB-ATTR PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700           07 MOD-PRODB      PIC X(2).                                    
004800*                                 PRODUKTSLAG                             
004900     03 MOD-TEMFSINF-ATTR    PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-TEMFSINF         PIC X(55).                                   
005200*                                 INFORMATIONSMEDDELANDE                  
005300*** END OF VILMAII-COPY LENGTH= 653 BYTES                                 

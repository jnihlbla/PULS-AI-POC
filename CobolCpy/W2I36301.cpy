000100 01  MID-W2I36301.                                                        
000200*                                 MIDCOPYTEXT 2363                        
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-INPUT.                                                        
001200*                                 MID-INDATA 2363                         
001300        05 MID-IDLEVNR-SLAG-GRP.                                          
001400           07 MID-FILLER     OCCURS 4 TIMES.                              
001500              09 MID-IDLEVNR-SLAG                                         
001600                             PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800        05 MID-FLORDSP-GRP.                                               
001900           07 MID-FILLER     OCCURS 4 TIMES.                              
002000              09 MID-FLORDSP PIC X.                                       
002100*                                 ORDERSPÄRR                              
002200        05 MID-FLSPBULK-GRP.                                              
002300           07 MID-FILLER     OCCURS 4 TIMES.                              
002400              09 MID-FLSPBULK                                             
002500                             PIC X.                                       
002600*                                 FLAGGA SPÄRR MOT BULKORDER              
002700        05 MID-IDPERSON-BUY-GRP.                                          
002800           07 MID-FILLER     OCCURS 4 TIMES.                              
002900              09 MID-IDPERSON-BUY                                         
003000                             PIC X(3).                                    
003100*                                 PERSONKOD REFILLANSVARIG                
003200        05 MID-FLREFERAL     PIC X.                                       
003300*                                 REFERALARTIKEL I LANDET                 
003400        05 MID-TEARTNOT-GRP.                                              
003500           07 MID-FILLER     OCCURS 4 TIMES.                              
003600              09 MID-TEARTNOT-ORDER                                       
003700                             PIC X(70).                                   
003800*                                 ARTIKEL NOTERING ORDER                  
003900*** END OF VILMAII-COPY LENGTH= 343 BYTES                                 

000100 01  REQU-WL0128I1.                                                       
000200*                                 REQUEST FOR PGM WL0128                  
000300*                                 PRINTING OF CASE LABEL                  
000400     03 REQU-L128-FLBG       PIC X.                                       
000500*                                 ALLMÄN FLAGGA                           
000600     03 REQU-L128-KVRADER    PIC 9(5).                                    
000700*                                 ANTAL RADER                             
000800     03 REQU-L128-RAD        OCCURS 50 TIMES.                             
000900*                                 REQUEST-COPYTEXT FÖR WL0128             
001000*                                                                         
001100        05 REQU-L128-IDDC-KEY                                             
001200                             PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400        05 REQU-L128-IDDISTR-KEY                                          
001500                             PIC 9(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700        05 REQU-L128-IDKUNDNR-KEY                                         
001800                             PIC 9(6).                                    
001900*                                 KUNDNUMMER                              
002000        05 REQU-L128-IDORDNR-KEY                                          
002100                             PIC 9(5).                                    
002200*                                 ORDERNUMMER UTGÅR PD90                  
002300        05 REQU-L128-IDKOLLI-KEY                                          
002400                             PIC 9(5).                                    
002500*                                 KOLLINUMMER                             
002600        05 REQU-L128-IDPRODNR-KEY                                         
002700                             PIC 9(7).                                    
002800*                                 PRODUKTIONSNUMMER                       
002900        05 REQU-L128-CLABEL  PIC X.                                       
003000*                                 J/Y = SKRIV BEGÄRD LISTA                
003100        05 REQU-L128-IDKOLLI-TOM                                          
003200                             PIC 9(5).                                    
003300*                                 KOLLINUMMER TILL OCH MED                
003400*** END OF VILMAII-COPY LENGTH= 1756 BYTES                                

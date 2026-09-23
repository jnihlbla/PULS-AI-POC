000100 01  REQU-WL0129I1.                                                       
000200*                                 REQUEST TO PGM  WL0129                  
000300*                                 PRINTING OF DELIVERY NOTE               
000400     03 REQU-L129-FLBG       PIC X.                                       
000500*                                 ALLMÄN FLAGGA                           
000600     03 REQU-L129-KVRADER    PIC 9(5).                                    
000700*                                 ANTAL RADER                             
000800     03 REQU-L129-RAD        OCCURS 50 TIMES.                             
000900*                                 REQUEST TILL PGM WL0129                 
001000*                                                                         
001100        05 REQU-L129-IDDC-KEY                                             
001200                             PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400        05 REQU-L129-IDDISTR-KEY                                          
001500                             PIC 9(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700        05 REQU-L129-IDKUNDNR-KEY                                         
001800                             PIC 9(6).                                    
001900*                                 KUNDNUMMER                              
002000        05 REQU-L129-IDORDNR-KEY                                          
002100                             PIC 9(5).                                    
002200*                                 ORDERNUMMER UTGÅR PD90                  
002300        05 REQU-L129-IDKOLLI-KEY                                          
002400                             PIC 9(5).                                    
002500*                                 KOLLINUMMER                             
002600        05 REQU-L129-IDKOLLI-TOM                                          
002700                             PIC 9(5).                                    
002800*                                 KOLLINUMMER                             
002900        05 REQU-L129-FLSKRIV-DELNOTE                                      
003000                             PIC X.                                       
003100*                                 J/Y = SKRIV BEGÄRD LISTA                
003200*** END OF VILMAII-COPY LENGTH= 1406 BYTES                                

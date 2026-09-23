000100 01  TRAEBCD-AREA.                                                        
000200*                                 LINK AREA TO PGM WTRAEBCD               
000300*                                 IT CONVERTS TEXT FROM UTF8              
000400*                                 TO VARIOUS CODE PAGES                   
000500*                                                                         
000600*                                 INPUT ARGUMENTS:                        
000700*                                  KDCP          OUTPUT CODE PAGE         
000800*                                  TECONV-FROM   INPUT TEXT.              
000900*                                                                         
001000*                                 OUTPUT ARGUMENTS:                       
001100*                                  TECONV-TO     CONVERTED TEXT           
001200*                                                                         
001300     03 TRAEBCD-KDCP         PIC X(5).                                    
001400*                                 CODE PAGE                               
001500*                                 CODE PAGE                               
001600     03 TRAEBCD-TECONV-FROM  PIC X(700).                                  
001700*                                 TEXT SOM SKA KONVERTERAS                
001800*                                 TEXT TO BE CONVERTED                    
001900     03 TRAEBCD-TECONV-TO    PIC X(700).                                  
002000*                                 TEXT SOM HAR KONVERTERAS                
002100*                                 TEXT WHICH HAS BEEN CONVERTED           
002200*** END OF VILMAII-COPY LENGTH= 1405 BYTES                                

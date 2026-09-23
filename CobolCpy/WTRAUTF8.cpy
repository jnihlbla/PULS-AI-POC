000100 01  TRAUTF8-AREA.                                                        
000200*                                 LINK AREA TO PGM  WTRAUTF8              
000300*                                 IT CONVERTS TEXT FROM VARIOUS           
000400*                                 CODE PAGES TO UNICODE UTF8              
000500*                                                                         
000600*                                 INPUT ARGUMENTS:                        
000700*                                  KDCP          INPUT CODE PAGE          
000800*                                  TECONV-FROM   INPUT TEXT.              
000900*                                                                         
001000*                                 OUTPUT ARGUMENTS:                       
001100*                                  TECONV-TO     CONVERTED TEXT           
001200*                                                                         
001300     03 TRAUTF8-KDCP         PIC X(5).                                    
001400*                                 CODE PAGE                               
001500*                                 CODE PAGE                               
001600     03 TRAUTF8-TECONV-FROM  PIC X(700).                                  
001700*                                 TEXT SOM SKA KONVERTERAS                
001800*                                 TEXT TO BE CONVERTED                    
001900     03 TRAUTF8-TECONV-TO    PIC X(700).                                  
002000*                                 TEXT SOM HAR KONVERTERAS                
002100*                                 TEXT WHICH HAS BEEN CONVERTED           
002200     03 TRAUTF8-KVMAXTL      PIC S9(3)           COMP-3.                  
002300*                                 MAX TILLÅTEN TEXTLÄNGD                  
002400*                                 MAX ALLOWED TEXT LENGTH                 
002500*** END OF VILMAII-COPY LENGTH= 1407 BYTES                                

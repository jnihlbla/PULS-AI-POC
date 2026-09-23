000100 01  WDL7-W005WDL7.                                                       
000200*                                 PARAMETRAR TILL W005WDL7 FÖR            
000300*                                 UPPLÄGG AV WDL7-SEGMENT                 
000400*                                 EXEMPEL PÅ ANROP:                       
000500*                                 MOVE ALL "+"  TO WDL7-W005WDL7          
000600*                                 MOVE N...   TO WDL7-IDARTNR             
000700*                                 MOVE X...   TO WDL7-IDDC                
000800*                                 CALL W005WDL7 USING                     
000900*                                               WDL7-W005WDL7             
001000*                                               WDL7-PCB                  
001100*                                 -------------------------------         
001200*                                 PARAMETERS TO W005WDL7 FOR              
001300*                                 INSERT OF WDL7-SEGMENT                  
001400*                                 EXAMPLE OF CALL:                        
001500*                                 MOVE ALL "+"  TO WDL7-W005WDL7          
001600*                                 MOVE N...   TO WDL7-IDARTNR             
001700*                                 MOVE X...   TO WDL7-IDDC                
001800*                                 CALL W005WDL7 USING                     
001900*                                               WDL7-W005WDL7             
002000*                                               WDL7-PCB                  
002100     03 WDL7-IDARTNR         PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300*                                 PART NUMBER                             
002400     03 WDL7-IDDC            PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600*                                 WAREHOUSE IDENTIFIER                    
002700*** END OF VILMAII-COPY LENGTH= 7 BYTES                                   

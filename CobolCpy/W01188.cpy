000100 01  TRCK-W01188.                                                         
000200*                                 UTDRAG UR WDK728                        
000300     03 TRCK-IDARTNR         PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500*                                 PART NUMBER                             
000600     03 TRCK-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 TRCK-WDK728.                                                      
001000*                                 CUSTOMS TRACKING SEGMENT                
001100*                                 FYSISK NYCKEL DAINLEV                   
001200        05 TRCK-DAINLEV      PIC 9(16).                                   
001300*                                 INLEVERANS NUMMER                       
001400*                                 CONSIGNMENT IDENTITY                    
001500*                                 (YYYYMMDD+HHMMSSTH)                     
001600        05 TRCK-KVANTMOT     PIC S9(7)           COMP-3.                  
001700*                                 ANTAL MOTTAGET                          
001800*                                 QUANTITY RECEIVED                       
001900        05 TRCK-KVAVIS       PIC S9(7)           COMP-3.                  
002000*                                 AVISERAT ANTAL                          
002100*                                 QUANTITY NOTIFIED                       
002200        05 TRCK-KVTRACK-KVAR PIC S9(7)           COMP-3.                  
002300*                                 ANTAL KVAR PER TRACKING-ID              
002400*                                 REMAINING QTY OF TRACKING-ID            
002500        05 TRCK-IDTRACK      PIC X(25).                                   
002600*                                 TRACKING ID FROM CUSTOMS                
002700*                                 CUSTOMS TRACKING ID                     
002800*** END OF VILMAII-COPY LENGTH= 60 BYTES                                  

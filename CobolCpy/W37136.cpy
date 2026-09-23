000100 01  AREA.                                                                
000200*                                 COPYTEXT FÖR KVITTNING AV               
000300*                                 BYTESARTIKLAR                           
000400*                                                                         
000500     03 W37136.                                                           
000600        05 IDPTYP            PIC X(3).                                    
000700*                                 POSTTYP                                 
000800        05 IDDISTR           PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 IDORDNR           PIC S9(5)           COMP-3.                  
001300*                                 ORDERNUMMER                             
001400        05 IDTABNR           PIC S9(3)           COMP-3.                  
001500*                                 TABELLNUMMER                            
001600        05 IDARTNR           PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800        05 TIAAMMDD-REG      PIC S9(7)           COMP-3.                  
001900*                                 REGISTRERINGSDATUM                      
002000        05 KDOBJEKT          PIC S9              COMP-3.                  
002100*                                 OBJEKTSKOD                              
002200        05 IDARTNR-OBJ       PIC S9(9)           COMP-3.                  
002300*                                 OBJEKTNUMMER                            
002400        05 KVANTAL-REST      PIC S9(7)           COMP-3.                  
002500*                                 ANTAL ALLMÄNT                           
002600        05 IDORDNR-URSPR     PIC S9(5)           COMP-3.                  
002700*                                 ORDERNUMMER                             
002800        05 IDKUNDRF          PIC X(10).                                   
002900*                                 KUNDENS REFERENS                        
003000*** END COPY W37136CCC0  LENGTH=47                                        

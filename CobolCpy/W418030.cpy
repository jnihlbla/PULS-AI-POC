000100 01  W418030.                                                             
000200*                                 KREDNOT INFO POSTTYP 030                
000300*                                                                         
000400     03 SORTAREA             PIC X(16).                                   
000500     03 FILLER REDEFINES SORTAREA.                                        
000600        05 IDDISTR           PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000        05 IDPTYP            PIC X(3).                                    
001100*                                 POSTTYP                                 
001200        05 IDARTNR-OBJ       PIC S9(9)           COMP-3.                  
001300*                                 OBJEKTNUMMER                            
001400        05 KDCLAGER          PIC S9              COMP-3.                  
001500*                                 CENTRALLAGERKOD                         
001600     03 IDKNOTNR             PIC S9(7)           COMP-3.                  
001700*                                 KREDITNOTANUMMER                        
001800     03 KVBYTANT             PIC S9(7)           COMP-3.                  
001900*                                 ANTAL BYTESOBJEKT                       
002000     03 PRKRED               PIC S9(5)V9(2)      COMP-3.                  
002100*                                 KREDITERINGSPRIS                        
002200     03 BEKUNDRF             PIC X(10).                                   
002300*                                 KUNDENS REFERENS                        
002400     03 IDARTNR              PIC S9(9)           COMP-3.                  
002500*                                 ARTIKELNUMMER                           
002600*** END COPY W418030CC0  LENGTH=43                                        

000100 01  POST.                                                                
000200*                                 SORTAREA FRÅN PROGRAM W3719000          
000300*                                           PLUS                          
000400*                                 WDA812-COPYTEXTEN                       
000500*                                                                         
000600     03 IDDISTR-SORT         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR-SORT        PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 IDARTNR-SORT         PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 TIAAMMDD-REG-SORT    PIC S9(7)           COMP-3.                  
001300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001400     03 KDOBJEKT-SORT        PIC S9              COMP-3.                  
001500*                                 OBJEKTSKOD                              
001600     03 IDORDNR-SORT         PIC S9(5)           COMP-3.                  
001700*                                 ORDERNUMMER                             
001800     03 IDPTYP-SORT          PIC X(3).                                    
001900*                                 POSTTYP                                 
002000     03 WDA812-KEY.                                                       
002100        05 IDORDNR           PIC S9(5)           COMP-3.                  
002200*                                 ORDERNUMMER                             
002300        05 IDARTNR-OBJ       PIC S9(9)           COMP-3.                  
002400*                                 OBJEKTNUMMER                            
002500     03 KVRETUR-DEB          PIC S9(7)           COMP-3.                  
002600*                                 ANTAL ATT FAKT.                         
002700     03 KDTDEB               PIC S9              COMP-3.                  
002800*                                 TILLÄGGSDEBITERING                      
002900     03 FLLIST               PIC S9              COMP-3.                  
003000*                                 ARTIKELN SKA LISTAS (1=JA)              
003100*** END COPY W3719512    LENGTH=37                                        

000100 01  4444-WDGX4444.                                                       
000200*                                 BESKRIVNING AV                          
000300*                                 PRODUKTIONSKLASSER                      
000400*                                 FYSISK NYCKEL WDGXKEY:                  
000500*                                 (IDRADNR + LOW-VALUE)                   
000600     03 4444-IDRADNR         PIC S9(5)           COMP-3.                  
000700*                                 RADNUMMER                               
000800*                                 LINE NO                                 
000900     03 4444-LOW-VALUE       PIC X(2).                                    
001000     03 4444-IDHLOTAB        PIC 9(2).                                    
001100*                                 HLOTABELLSIDENTITET                     
001200*                                 MAINAREA TABLE IDENTITY                 
001300     03 4444-KDORDKL         PIC S9              COMP-3.                  
001400*                                 ORDERKLASS                              
001500*                                 ORDER CLASS                             
001600     03 4444-KDPRODKL        PIC X.                                       
001700*                                 PRODUKTIONSKLASS                        
001800*                                 PRODUCTION CLASS                        
001900     03 4444-KVRADER         PIC S9(5)           COMP-3.                  
002000*                                 ANTAL RADER                             
002100*                                 NUMBER OF LINES                         
002200     03 4444-VKORDNTO        PIC S9(6)V9(1)      COMP-3.                  
002300*                                 ORDERVIKT NETTO (KG)                    
002400*                                 WEIGHT PER ORDER NETTO (KG)             
002500     03 4444-VLORDNTO        PIC S9(4)V9(3)      COMP-3.                  
002600*                                 ORDERVOLYM NETTO (M3)                   
002700*                                 NET VOLUME PER ORDER (M3)               
002800     03 4444-FILLER          PIC X(10).                                   
002900*** END COPY WDGX4444C0  LENGTH=30                                        

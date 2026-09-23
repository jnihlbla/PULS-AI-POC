000100 01  OBHUV-W461002.                                                       
000200*                                 ORDERBEKR. HUVUD TILL NOAC              
000300*                                 PT-002                                  
000400     03 OBHUV-IDPTYP         PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 OBHUV-IDDISTR        PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 OBHUV-IDKUNDNR       PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 OBHUV-KDFRAKT        PIC S9(3)           COMP-3.                  
001100*                                 FRAKTSÄTT C1-C2 TILL KUND               
001200     03 OBHUV-IDORDNR        PIC S9(7)           COMP-3.                  
001300*                                 ORDERNR             IDORDNR-002         
001400     03 OBHUV-BEVOLREF       PIC X(10).                                   
001500*                                 VOLVO REFERENS                          
001600     03 OBHUV-BEVARREF       PIC X(10).                                   
001700*                                 VÅR REFERENS                            
001800     03 OBHUV-KDORDKL        PIC S9              COMP-3.                  
001900*                                 ORDERKLASS                              
002000     03 OBHUV-TIORDREG       PIC S9(7)           COMP-3.                  
002100*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002200     03 OBHUV-KDFAKTYP       PIC X.                                       
002300*                                 FAKTURATYP                              
002400     03 FILLER               PIC X(6).                                    
002500*** END COPY W461002CC0  LENGTH=48                                        

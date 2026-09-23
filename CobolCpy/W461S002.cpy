000100 01  OBHUV-W461S002.                                                      
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 ORDERBEKRÄFTELSE-HUVUD                  
000400*                                 TILL NOAC                               
000500     03 OBHUV-SOR0-IDDISTR   PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 OBHUV-SOR0-IDKUNDNR  PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 OBHUV-SOR0-IDRONR    PIC S9(7)           COMP-3.                  
001000*                                 RESTORDERNUMMER      IDRONR-002         
001100     03 OBHUV-SOR0-TIRODAT   PIC S9(7)           COMP-3.                  
001200*                                 RESTORDERDATUM         (ÅÅMMDD)         
001300     03 OBHUV-SOR0-IDPTYP    PIC X(3).                                    
001400*                                 POSTTYP                                 
001500     03 OBHUV-SOR0-IDLOPNR   PIC S9(5)           COMP-3.                  
001600*                                 LÖPNUMMER          IDLOPNR-002          
001700     03 OBHUV-W461002.                                                    
001800*                                 ORDERBEKR. HUVUD TILL NOAC              
001900*                                 PT-002                                  
002000        05 OBHUV-IDPTYP      PIC X(3).                                    
002100*                                 POSTTYP                                 
002200        05 OBHUV-IDDISTR     PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400        05 OBHUV-IDKUNDNR    PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600        05 OBHUV-KDFRAKT     PIC S9(3)           COMP-3.                  
002700*                                 FRAKTSÄTT C1-C2 TILL KUND               
002800        05 OBHUV-IDORDNR     PIC S9(7)           COMP-3.                  
002900*                                 ORDERNR             IDORDNR-002         
003000        05 OBHUV-BEVOLREF    PIC X(10).                                   
003100*                                 VOLVO REFERENS                          
003200        05 OBHUV-BEVARREF    PIC X(10).                                   
003300*                                 VÅR REFERENS                            
003400        05 OBHUV-KDORDKL     PIC S9              COMP-3.                  
003500*                                 ORDERKLASS                              
003600        05 OBHUV-TIORDREG    PIC S9(7)           COMP-3.                  
003700*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
003800        05 OBHUV-KDFAKTYP    PIC X.                                       
003900*                                 FAKTURATYP                              
004000        05 OBHUV-FILLER      PIC X(6).                                    
004100*** END COPY W461S002C0  LENGTH=69                                        

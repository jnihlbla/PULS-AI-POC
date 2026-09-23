000100 01  W406971A-CTX.                                                        
000200*                                 BOLLA INFORMATION TRANS TO              
000300*                                 TRANSPORTER SUSA                        
000400     03 IDTRPBOR             PIC S9(5)           COMP-3.                  
000500*                                 BOLLA DOCUMENT REFERENCE NO.            
000600     03 KDFRAKT              PIC S9(3)           COMP-3.                  
000700*                                 FREIGHT CODE                            
000800     03 IDZON                PIC X(2).                                    
000900*                                 TRANSPORT ROUTE (ZONE)                  
001000     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001100*                                 CUSTOMER NO                             
001200     03 IDORDNR5             PIC 9(5).                                    
001300*                                 ORDER NUMBER                            
001400     03 TIORDREG             PIC S9(7)           COMP-3.                  
001500*                                 ORDER REGISTRATION DATE  YYMMDD         
001600     03 KVKOLLI              PIC S9(5)           COMP-3.                  
001700*                                 NBR OF CASES                            
001800     03 VKORDBTO             PIC S9(6)V9(1)      COMP-3.                  
001900*                                 GROSS WEIGHT (KG)                       
002000     03 VLORDBTO             PIC S9(4)V9(3)      COMP-3.                  
002100*                                 GROSS VOLUME PER ORDER (M3)             
002200     03 IDTRPBO              PIC X(8).                                    
002300     03 TIREGDAT             PIC S9(7)           COMP-3.                  
002400*                                 REGISTRATION DATE (YYMMDD)              
002500*** END OF VILMAII-COPY LENGTH= 43 BYTES                                  

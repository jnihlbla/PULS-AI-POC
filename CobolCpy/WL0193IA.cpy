000100 01  WL0193IA-CTX.                                                        
000200*                                 BOLLA INFORMATION TRANS TO              
000300*                                 TRANSPORTER SUSA                        
000400     03 IDTRPBOR             PIC S9(5)           COMP-3.                  
000500*                                 BOLLA DOCUMENT REFERENCE NO.            
000600     03 IDDC                 PIC X(2).                                    
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 KDFRAKT              PIC S9(3)           COMP-3.                  
000900*                                 FREIGHT CODE                            
001000     03 IDZON                PIC X(2).                                    
001100*                                 TRANSPORT ROUTE (ZONE)                  
001200     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001300*                                 CUSTOMER NO                             
001400     03 IDORDNR5             PIC 9(5).                                    
001500*                                 ORDER NUMBER                            
001600     03 TIORDREG             PIC S9(7)           COMP-3.                  
001700*                                 ORDER REGISTRATION DATE  YYMMDD         
001800     03 KVKOLLI              PIC S9(5)           COMP-3.                  
001900*                                 NBR OF CASES                            
002000     03 VKORDBTO             PIC S9(6)V9(1)      COMP-3.                  
002100*                                 GROSS WEIGHT (KG)                       
002200     03 VLORDBTO             PIC S9(4)V9(3)      COMP-3.                  
002300*                                 GROSS VOLUME PER ORDER (M3)             
002400     03 IDTRPBO              PIC X(8).                                    
002500     03 TIREGDAT             PIC S9(7)           COMP-3.                  
002600*                                 REGISTRATION DATE (YYMMDD)              
002700*** END OF VILMAII-COPY LENGTH= 45 BYTES                                  

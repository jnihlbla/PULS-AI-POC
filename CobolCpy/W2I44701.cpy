000100 01  MID-W2I44701.                                                        
000200     03 MID-IDANSK-FOM-IN    PIC X(3).                                    
000300*                                 LOWEST PURCHASE PLANNER NUMBER          
000400     03 MID-IDANSK-TOM-IN    PIC X(3).                                    
000500*                                 HIGHEST PURCHASE PLANNER NO             
000600     03 MID-IDLEVNR-IN       PIC X(5).                                    
000700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
000800     03 MID-KDLPORS-IN       PIC X(2).                                    
000900     03 MID-KDLEVPLF-IN      PIC X.                                       
001000*                                 CODE FOR APPROVAL OF SCHEDULE P         
001100*                                 ROPOSAL                                 
001200     03 MID-IDDC-IN          PIC X(2).                                    
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 MID-RAD              OCCURS 15 TIMES.                             
001500        05 MID-KDCMDVAL      PIC X.                                       
001600*                                 GENERAL COMMAND-CODE                    
001700        05 MID-IDDC          PIC X(2).                                    
001800*                                 WAREHOUSE IDENTIFIER                    
001900        05 MID-IDARTNR       PIC X(9).                                    
002000*                                 PART NUMBER                             
002100        05 MID-IDLEVNR       PIC X(5).                                    
002200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002300*** END OF VILMAII-COPY LENGTH= 271 BYTES                                 

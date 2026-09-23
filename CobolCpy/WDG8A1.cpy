000100 01  SEQA-WDG8A1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDG801             
000300*                                 ÅTERSTARTSREGISTER                      
000400*                                 FYSISK NYCKEL: WDG8A1KY                 
000500*                                 (IDLTERM + TIREGDAT + TIKLOCK)          
000600*                                 SEKUNDÄR NYCKEL: WDG8ASEQ               
000700*                                  (IDLTERM + TIREGDAT + TIKLOCK)         
000800     03 SEQA-IDLTERM         PIC X(8).                                    
000900*                                 LOGISKT TERMINALNAMN                    
001000*                                 IDENTITY OF LOGICAL TERMINAL            
001100     03 SEQA-TIREGDAT        PIC S9(7)           COMP-3.                  
001200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001300*                                 REGISTRATION DATE (YYMMDD)              
001400     03 SEQA-TIKLOCK-9KOMPL  PIC S9(9)           COMP-3.                  
001500*                                 TID LAGRAT SOM 9-KOMPLEMENT             
001600*                                 TIME SAVED AS 9-COMPLEMENT              
001700     03 SEQA-IDLIST          PIC X(10).                                   
001800*                                 LISTIDENTITET                           
001900*                                 LIST IDENTITY                           
002000     03 SEQA-IDPRTLST        PIC X(8).                                    
002100*                                 LOGISK PRINTER+LISTA IDENTITET          
002200*                                 LOGICAL PRINTER+LIST IDENTITY           
002300     03 SEQA-FLSKRIV         PIC X.                                       
002400*                                 JA = ÅTERSTART AV BEBÄRD LISTA          
002500     03 SEQA-KVANTEX-PRINTAD PIC S9              COMP-3.                  
002600*                                 ANTAL GÅNGER LISTAN ÄR PRINTAD          
002700*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  

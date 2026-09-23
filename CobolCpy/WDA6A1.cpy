000100 01  SEQA-WDA6A1.                                                         
000200*                                 VOR REGISTER                            
000300*                                                                         
000400*                                 FYSISK NYCKEL: WDA6A1KY                 
000500*                                                                         
000600*                                  (IDDISTR+ IDKUNDNR+ IDKUNDRF +         
000700*                                 TIREGDAT-URSP+IDARTNR+ TIREGTID         
000800*                                 -URSP+TIREGDAT-AVV+TIREGTID-AVV         
000900*                                 )                                       
001000*                                 SEKUNDÄR NYCKEL: WDA6ASEQ               
001100*                                  (IDDISTR+ IDKUNDNR+ IDKUNDRF+          
001200*                                 TIREGDAT-URSP+ IDARTNR+TIREGTID         
001300*                                 -URSP+TIREGDAT-AVV+TIREGTID-AVV         
001400*                                 )                                       
001500     03 SEQA-IDDISTR         PIC S9(5)           COMP-3.                  
001600*                                 DISTRIKTNUMMER                          
001700*                                 DISTRICT NUMBER                         
001800     03 SEQA-IDKUNDNR        PIC S9(7)           COMP-3.                  
001900*                                 KUNDNUMMER                              
002000*                                 CUSTOMER NO                             
002100     03 SEQA-IDKUNDRF        PIC X(10).                                   
002200*                                 KUNDENS REFERENS (ORDERID)              
002300*                                 CUSTOMER REFERENCE (ORDER ID)           
002400     03 SEQA-TIREGDAT-URSP   PIC S9(7)           COMP-3.                  
002500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002600*                                 REGISTRATION DATE (YYMMDD)              
002700     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
002800*                                 ARTIKELNUMMER                           
002900*                                 PART NUMBER                             
003000     03 SEQA-TIREGTID-URSP   PIC S9(9)           COMP-3.                  
003100*                                 KLOCKSLAG (TTMMSSTH)                    
003200*                                 TIME OF DAY (HHMMSSTH)                  
003300     03 SEQA-TIREGDAT-AVV    PIC S9(7)           COMP-3.                  
003400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003500*                                 REGISTRATION DATE (YYMMDD)              
003600     03 SEQA-TIREGTID-AVV    PIC S9(9)           COMP-3.                  
003700*                                 KLOCKSLAG (TTMMSSTH)                    
003800*                                 TIME OF DAY (HHMMSSTH)                  
003900*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  

000100 01  SEQF-WDA6F1.                                                         
000200*                                 VOR REGISTER                            
000300*                                                                         
000400*                                 FYSISK NYCKEL: WDA6F1KY                 
000500*                                                                         
000600*                                  (IDDISTR+ IDKUNDNR+ TIREGDAT-          
000700*                                  AVV9 + TIREGTID-AVV9 )                 
000800*                                 SEKUNDÄR NYCKEL: WDA6FSEQ               
000900*                                  (IDDISTR+ IDKUNDNR+ TIREGDAT-          
001000*                                  AVV9 + TIREGTID-AVV9 )                 
001100     03 SEQF-IDDISTR         PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 SEQF-IDKUNDNR        PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600*                                 CUSTOMER NO                             
001700     03 SEQF-TIREGDAT-AVV9   PIC S9(7)           COMP-3.                  
001800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001900*                                 REGISTRATION DATE (YYMMDD)              
002000     03 SEQF-TIREGTID-AVV9   PIC S9(9)           COMP-3.                  
002100*                                 KLOCKSLAG (TTMMSSTH)                    
002200*                                 TIME OF DAY (HHMMSSTH)                  
002300     03 SEQF-IDUSER          PIC X(8).                                    
002400*                                 ANVÄNDARENS SÄKERHETS ID                
002500*                                 USER SECURITY-IDENTITY                  
002600     03 SEQF-KDVORATG        PIC X.                                       
002700*                                 TYP AV ÅTGÄRD FÖR POST VOR-KÖN          
002800*                                 TYPE OF HANDLE ON VOR-QUE               
002900     03 SEQF-IDKUNDRF-GRP.                                                
003000*                                 KUNDENS REFERENS (ORDERID)              
003100*                                 CUSTOMER REFERENCE (ORDER ID)           
003200        05 SEQF-IDKUNDRF     PIC X(10).                                   
003300*                                 KUNDENS REFERENS (ORDERID)              
003400*                                 CUSTOMER REFERENCE (ORDER ID)           
003500        05 SEQF-IDORDNR5-FILLER REDEFINES SEQF-IDKUNDRF.                  
003600           07 SEQF-IDORDNR5  PIC 9(5).                                    
003700*                                 ORDERNUMMER                             
003800*                                 ORDER NUMBER                            
003900           07 FILLER         PIC X(5).                                    
004000        05 SEQF-IDORDNR7-FILLER REDEFINES SEQF-IDKUNDRF.                  
004100           07 SEQF-IDORDNR7  PIC 9(7).                                    
004200*                                 ORDERNUMMER                             
004300*                                 ORDER NUMBER                            
004400           07 FILLER         PIC X(3).                                    
004500*** END OF VILMAII-COPY LENGTH= 35 BYTES                                  

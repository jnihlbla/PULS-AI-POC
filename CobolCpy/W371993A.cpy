000100 01  W371993A.                                                            
000200*                                 COPYTEXT F÷R FILEN W37199               
000300*                                                                         
000400     03 IDPTYP-820           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 TIAAMMDD-REG         PIC S9(7)           COMP-3.                  
000700*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
000800     03 IDORDNR-KEY          PIC S9(5)           COMP-3.                  
000900*                                 ORDERNUMMER                             
001000     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
001100*                                 OBJEKTNUMMER                            
001200     03 KVRETUR              PIC S9(7)           COMP-3.                  
001300*                                 ANTAL I RETUR                           
001400     03 KVRETUR-AVBOK        PIC S9(7)           COMP-3.                  
001500*                                 AVBOKAT ANT.RETUR                       
001600     03 TIAAMMDD-RENS        PIC S9(7)           COMP-3.                  
001700*                                 RENSNINGSDATUM                          
001800     03 IDKUNDRF             PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000     03 FILLER               PIC X(43).                                   
002100*** END COPY W371993A    LENGTH=80                                        

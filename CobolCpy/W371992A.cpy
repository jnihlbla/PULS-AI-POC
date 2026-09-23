000100 01  W371992A.                                                            
000200*                                 COPYTEXT FÖR FILEN W37199               
000300*                                                                         
000400     03 IDPTYP-810           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 TIAAMMDD-REG810      PIC S9(7)           COMP-3.                  
000700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
000800     03 IDORDNR              PIC S9(5)           COMP-3.                  
000900*                                 ORDERNUMMER                             
001000     03 KVANTAL-FAKT         PIC S9(7)           COMP-3.                  
001100*                                 FAKTURERAT ANTAL                        
001200*                                                                         
001300     03 KVAVBART             PIC S9(7)           COMP-3.                  
001400*                                 AVBOKAT ANTAL ARTIKLAR                  
001500     03 KVLEVANM             PIC S9(7)           COMP-3.                  
001600*                                 LEVERANSANMÄRKNINGSANTAL                
001700     03 TIAAMMDD-FAKT        PIC S9(7)           COMP-3.                  
001800*                                 SENASTE FAKTURERINGSDATUM               
001900     03 TIAAMMDD-TDEB        PIC S9(7)           COMP-3.                  
002000*                                 TILLÄGGSDEBITERINGS-DATUM               
002100     03 FILLER               PIC X(50).                                   
002200*                                                                         
002300*** END COPY W371992A    LENGTH=80                                        

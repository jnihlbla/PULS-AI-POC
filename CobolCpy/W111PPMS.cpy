000100 01  W111PPMS.                                                            
000200*                                 ERSÄTTNINGSINF TILL PV                  
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
000800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
000900     03 TIKLOCK              PIC S9(9)           COMP-3.                  
001000*                                 KLOCKSLAG (HHMMSSTH)                    
001100     03 KDERS                PIC S9(3)           COMP-3.                  
001200*                                 ERSÄTTNINGSKOD                          
001300     03 ANMARKNING           PIC X(8).                                    
001400*                                 ARTIKELNUMMER                           
001500*** END COPY W111PPMSC0  LENGTH=27                                        

000100 01  W235L001.                                                            
000200     03 KDSVAR               PIC X.                                       
000300      88 LEV-INFO-FINNS      VALUE ' '.                                   
000400*                                 SVAR FRÅN SUBPROGRAM                    
000500     03 NYCKEL.                                                           
000600        05 IDLEVNR           PIC S9(5)           COMP-3.                  
000700*                                 LEVERANTÖRNUMMER                        
000800     03 IO-AREA.                                                          
000900*                                 WDF101                                  
001000        05 PGTABELL.                                                      
001100           07 IDANSK-PG      OCCURS 8 TIMES                               
001200                             PIC S9(3)           COMP-3.                  
001300*                                 ANSKAFFARNR PER PLANERINGSGRUPP         
001400*** END COPY W235L001C0  LENGTH=20                                        

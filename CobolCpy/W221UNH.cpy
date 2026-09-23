000100 01  W221UNH.                                                             
000200     03 IDPT                 PIC X(3).                                    
000300*                                 POSTTYP                  TB314          
000500     03 IDPTYP-LTH           PIC X(3) VALUE '073'.                        
000600*                                 LÄNGD PÅ FÄLT                           
000800     03 MESSAGE-ID-TYPE      PIC X(6).                                    
000900*                                 MEDDELANDETYP                           
001000     03 MESSAGE-ID-VERSION   PIC X(3).                                    
001100*                                 VERSION AV MEDDELANDE                   
001200     03 FILLER               PIC X(64) VALUE SPACE.                       
001300*                                 FILLER                                  
001400*                                                                         
001500*** END COPY W221UNH     LENGTH=79    OLD LENGTH=79                       

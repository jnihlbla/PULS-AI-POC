000100*** EDIT ALLOWED                                                          
020000*                            *************************************        
030000*                            *** ANVÄNDS VID TEST AV:                     
040000*                            *** DISTRIKT SOM SKALL EXKLUDERAS            
040000*                            *** FRÅN NORMAL HANTERING AV RETURER         
040000*                            *** OCH BUY BACK FÖR BYTES ARTIKLAR.         
050000*                            *************************************        
060000                                                                          
070000 01  DIST16-IDDISTR          PIC 9(5)     COMP-3.                         
080000                                                                          
090001     88  DIST16-OVERSEAS-EXCL VALUE   1110 5616 5619                      
091001                                      6200 6240 6251.                     
080000                                                                          
090001     88  DIST16-BUYBACK-EXCL  VALUE   0778 1378 1778.                     
130000                                                                          
259001                                                                          
260001*** END COPY WWDIST16    LENGTH=3                                         

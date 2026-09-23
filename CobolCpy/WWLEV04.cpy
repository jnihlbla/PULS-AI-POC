001000*** EDIT ALLOWED                                                          
010000*                            *************************************        
020001*                            *** ANVÄNDS VID TEST AV LEVERANTÖRER         
030001*                            *** SOM MAN SKICKAR REF.NR. ISTF             
040001*                            *** PRODNR I FIL TILL LEVA1 (SAP)            
100000*                            *************************************        
110000                                                                          
120001 01  LEV04-IDLEVNR           PIC X(5).                                    
130000*                                                                         
140001       88  LEV04-REFNR          VALUE 'AE4PC'                             
140001                                      'AFGQF'                             
140003                                      'BP8BA'                             
141101                                      'BP7YA'                             
141102                                      'BP3EA'                             
142001                                      'BQ9QB'                             
142002                                      'CWZYA'.                            
410001** END COPY WWLEV04  LENGTH=4                                             

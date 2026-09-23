000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** ANVÄNDS VID TEST AV LAND                 
000210*                            *** SOM SKALL HA:                            
000300*                            *** - MOMSREDOVISNING FÖRSÄLJNING            
000320*                            *** - INTRASTATREDOVISNING INFÖRSEL          
000330*                            *** - INTRASTATREDOVISNING UTFÖRSEL          
000400*                            ***                                          
000500*                            *************************************        
000600*                                                                         
000700 01  LAND09-IDLANDX3         PIC X(3).                                    
001800*                                                                         
001810     88  LAND09-MOMS         VALUE 'GB' 'IT'.                             
003000     88  LAND09-INT-IN       VALUE 'GB' 'GR' 'IE' 'IT' 'SE'.              
003100     88  LAND09-INT-UT       VALUE 'GB' 'IT' 'BE' 'DE' 'SE' 'IE'.         

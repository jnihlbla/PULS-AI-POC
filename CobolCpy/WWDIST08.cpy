001000*** EDIT ALLOWED                                                          
010000*                            *************************************        
020000*                            *** ANVÄNDS VID TEST AV:                     
030000*                            ***  - DISTRIKT DÄR URSPRUNG SKALL           
040000*                            ***    RAPPORTERAS VID PACKNINGSRAPP         
050000*                            *************************************        
060000 01  DIST08-IDDISTR          PIC 9(5)     COMP-3.                         
070000*                                                                         
080100       88  DIST08-URSP-RAPP         VALUE 4860                            
080110                                          7579                            
080200                                          8141 THRU 8149                  
080300                                          8171 THRU 8174.                 
091000*                                                                         
092000       88  DIST08-URSP-RAPP-CDC     VALUE 4115                            
092010                                          4850 5120                       
092100                                          7574 7575 8617.                 
093000*                                                                         
094000       88  DIST08-URSP-TRANSFER-NDC VALUE 8741 THRU 8749.                 
095000*                                                                         
096000       88  DIST08-URSP-RETUR-NDC    VALUE 8111 8211.                      
096100*                                                                         
097000       88  DIST08-URSP-SPX          VALUE 7510 8614.                      
097100*                                                                         
160000*** END COPY WWDIST08    LENGTH=3                                         

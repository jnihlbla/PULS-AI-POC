001000*** EDIT ALLOWED                                                          
010000*                            *************************************        
020000*                            *** ANVÄNDS VID TEST AV:                     
030001*                            ***  - SKROT DISTR                           
050000*                            *************************************        
060000                                                                          
070001 01  DIST18-IDDISTR             PIC 9(5)     COMP-3.                      
080000*                                                                         
090001     88  DIST18-SKROT                  VALUE 0070 0081 0090 8497.         
091001     88  DIST18-SKROT-KVAL-CDC         VALUE 0070.                        
092001     88  DIST18-SKROT-SDC              VALUE 0081.                        
121001     88  DIST18-SKROT-KVAL-SDC         VALUE 0090 8497.                   
121100     88  DIST18-SKROT-LDC              VALUE 0081.                        
121201     88  DIST18-SKROT-KVAL-LDC         VALUE 0090 8497.                   
122001*                                                                         
122101*   SKROTDISTRIKT FÖR NDC:ERNA ANVÄNDS I KOMBINATION MED                  
122201*   DC.                                                                   
122401*                                                                         
123001     88  DIST18-SCRAP-NDC              VALUE 8480 8481 8482 8490.         
124001     88  DIST18-SCRAP-NDC-SC           VALUE 8480.                        
124002     88  DIST18-SCRAP-NDC-SC-LOCAL     VALUE 8497.                        
124101     88  DIST18-SCRAP-NDC-DAM          VALUE 8481.                        
124201     88  DIST18-SCRAP-NDC-ECO          VALUE 8482.                        
125001     88  DIST18-SCRAP-NDC-QUAL         VALUE 8490.                        
130000*                                                                         
140001*** END COPY WWDIST18    LENGTH=3                                         

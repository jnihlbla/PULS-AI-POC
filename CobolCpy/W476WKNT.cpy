000100*** EDIT ALLOWED                                                          
010032 01  W476WKON.                                                            
020002**                                                                        
030036*  TNT-KONTOTABELL.                                                       
040000*                                                                         
050039   03  KONTO-TABELL.                                                      
060035      05  FILLER    PIC X(11)   VALUE                                     
071034                    '1258/081640'.                                        
060142      05  FILLER    PIC X(11)   VALUE                                     
060345                    '1678/081639'.                                        
060235      05  FILLER    PIC X(11)   VALUE                                     
060341                    '1778/081637'.                                        
070035      05  FILLER    PIC X(11)   VALUE                                     
060143                    '1958/081636'.                                        
060342      05  FILLER    PIC X(11)   VALUE                                     
060343                    '2120/081638'.                                        
060342      05  FILLER    PIC X(11)   VALUE                                     
060343                    '2178/081638'.                                        
060344      05  FILLER    PIC X(11)   VALUE                                     
060141                    '2278/836026'.                                        
590040   03  KONTOTAB  REDEFINES KONTO-TABELL.                                  
600041      05  KONTOTABELL OCCURS 7                                            
610038          ASCENDING KEY TAB-IDDISTR                                       
620033          INDEXED BY TAB-IX.                                              
630038          07  TAB-IDDISTR          PIC X(4).                              
640033          07  FILLER               PIC X.                                 
650038          07  TAB-KONTONR          PIC X(6).                              

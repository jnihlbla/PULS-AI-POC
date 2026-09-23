000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** ANVÄNDS FÖR ATT HÄMTA                    
000300*                            *** ADRESS TILL RESPEKTIVE                   
000400*                            *** LDC                                      
000500*                            *************************************        
001220                                                                          
001230 01  ADRESS-TAB.                                                          
001240   03 LDC-ADRESS-LDC1A.                                                   
001250      05 FILLER                             PIC X(35)                     
001260         VALUE 'DANZAS ASG SOLUTIONS               '.                     
001270      05 FILLER                             PIC X(35)                     
001280         VALUE 'STENVRETSGATAN 2 - 4               '.                     
001290      05 FILLER                             PIC X(35)                     
001291         VALUE 'BOX 929                            '.                     
001292      05 FILLER                             PIC X(35)                     
001293         VALUE '745 25 ENKÖPING                    '.                     
001294      05 FILLER                             PIC X(35)                     
001295         VALUE 'SVERIGE                            '.                     
001375                                                                          
001240   03 LDC-ADRESS-LDC1B.                                                   
001250      05 FILLER                             PIC X(35)                     
001260         VALUE 'VOLVO CARS LDC 1B                  '.                     
001270      05 FILLER                             PIC X(35)                     
001280         VALUE 'XXXXXXXXXXXXXXXXXXXX               '.                     
001290      05 FILLER                             PIC X(35)                     
001291         VALUE 'XXXXXXXXXXXXXXXXXXXX               '.                     
001292      05 FILLER                             PIC X(35)                     
001293         VALUE 'XXXXXXXXXXXXXXXXXXXX               '.                     
001294      05 FILLER                             PIC X(35)                     
001295         VALUE 'SWEDEN                             '.                     
001375                                                                          
001376 01  WS-ADRESS-RECORD  REDEFINES ADRESS-TAB.                              
001377   03  ADRESS-LDC      OCCURS 2.                                          
001378      05 LDC-BEGMT-RAD1                     PIC X(35).                    
001379      05 LDC-BEGMT-RAD2                     PIC X(35).                    
001380      05 LDC-ADGMT-GATA                     PIC X(35).                    
001381      05 LDC-ADGMT-PADR                     PIC X(35).                    
001382      05 LDC-ADGMT-LAND                     PIC X(35).                    
001385                                                                          

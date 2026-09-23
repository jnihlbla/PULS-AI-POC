000010*** EDIT ALLOWED                                                          
000100*                                                                         
000200*                             TABELL S-LAGER.                             
000300*                             ANVÄNDS FÖR INFORMATION SOM GÄLLER          
000400*                             FÖR ALLA ARTIKLAR PÅ ETT S-LAGER.           
000600*                                                                         
000700 01  S-LAGER-INFO.                                                        
000800     03 S1                    PIC X(8)   VALUE '11802103'.                
000900     03 S2                    PIC X(8)   VALUE '12802203'.                
001000     03 S3                    PIC X(8)   VALUE '13802303'.                
001100     03 S4                    PIC X(8)   VALUE 'XX000000'.                
001200     03 S5                    PIC X(8)   VALUE 'XX000000'.                
001300*                                                                         
001400 01  FILLER REDEFINES S-LAGER-INFO.                                       
001500     03  S-LAGER-DISTR OCCURS 5 INDEXED BY S-LAGER-IX.                    
001600         05  S-IDLAGER        PIC X(2).                                   
001700         05  S-IDDISTR        PIC 9(4).                                   
001710         05  S-KVDAGAR-LT     PIC 9(2).                                   
001800                                                                          
001900*** END COPY SLAGINFO    LENGTH=40                                        

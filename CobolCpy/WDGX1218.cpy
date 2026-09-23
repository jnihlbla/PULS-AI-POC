000010 01  1218-WDGX1118.                                                       
000020*                                 KATALOG VADIS                           
000030*                                 VARIANT I BILMODELL                     
000040*                                 FYSISK NYCKEL: WDGXKEY                  
000050*                                  (IDMODELL + TICHAAAR +                 
000060*                                  (KDCHATYP)                             
000070     03 1218-IDMODELL        PIC X(3).                                    
000080*                                 BILENS MODELBETECKNING ("XX0")          
000090*                                 MODEL ID FOR A VECHICLE ("XX0")         
000100     03 1218-TICHAAAR        PIC 9(4).                                    
000110*                                 CHASSI-еR (ееее)                        
000120*                                 CHASSI YEAR (YYYY)                      
000130     03 1218-KDCHATYP        PIC 9.                                       
000140*                                 CHASSINUMMER-TYP                        
000150*                                 CHASSI NUMBER TYPE                      
000160     03 1218-IDCHASSI-STA    PIC 9(6).                                    
000170*                                 CHASSINUMMER START                      
000180*                                 CHASSI NUMBER START                     
000190     03 1218-IDCHASSI-STO    PIC 9(6).                                    
000200*                                 CHASSINUMMER STOPP                      
000210*                                 CHASSI NUMBER STOP                      
000220*** END COPY WDGX1218  LENGTH=20                                          

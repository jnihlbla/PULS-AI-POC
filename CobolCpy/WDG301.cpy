000100 01  WDG301.                                                              
000200*                                 HÄNDELSE-REG ROTSEG                     
000300*                                 FYSISK NYCKEL WDG3KEY                   
000400*                                 OANVÄND DEL AV NYCKEL-VALFRI            
000500*                                 UTFYLLS MED MED LOW-VALUE               
000600     03 WDG3KEY.                                                          
000700*                                                                         
000800        05 IDHTYP            PIC X(4).                                    
000900*                                 HÄNDELSETYP                             
001000        05 NYCKEL-VALFRI     PIC X(26).                                   
001100*** END COPY WDG301CCC0  LENGTH=30                                        

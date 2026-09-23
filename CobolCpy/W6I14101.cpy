000010 01  MID-W6I14101.                                                        
000020*                                 MID-COPYTEXT FÖR W6014100               
000030     03 MID-IDINLVGN         PIC X(3).                                    
000040*                                 VAGNSIDENTITET                          
000050     03 MID-ADINLOMR         PIC X(4).                                    
000060*                                 INLEVERANSOMRÅDE                        
000070     03 MID-RADER            OCCURS 12 TIMES.                             
000080*                                 GRP FÖR MID W6I14101                    
000090        05 MID-IDLEVNR-KOLLI PIC X(5).                                    
000100*                                 LEVERANTÖRNUMMER KOLLI                  
000110        05 MID-IDOKOLLI      PIC X(9).                                    
000120*                                 ODETTE KOLLINUMMER                      
000130        05 MID-TEMFSMED      PIC X(20).                                   
000140*                                 INFO-MEDDELANDE FÖR FÄLT/RAD            
000150     03 MID-ADINLOMR-PRT-IN  PIC X(4).                                    
000160*                                 PRINTERPLACERING                        
000170     03 MID-ADINLOMR-PRT-UT  PIC X(4).                                    
000180*                                 PRINTERPLACERING                        
      *** END COPY W6I14101    LENGTH=423                                       

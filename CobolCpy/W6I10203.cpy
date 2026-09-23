000010 01  HTERM-MID-W6I10203.                                                  
000020*                                 COPYTEXT FÖR MID                        
000030*                                 W6I10203                                
000040     03 HTERM-MID-IDINLVGN-UT                                             
000050                             PIC X(3).                                    
000060*                                 VAGNSIDENTITET                          
000070     03 HTERM-MID-ADINLOMR-UT                                             
000080                             PIC X(4).                                    
000090*                                 INLEVERANSOMRÅDE                        
000100     03 HTERM-MID-ADINLOMR-NXT-UT                                         
000110                             PIC X(4).                                    
000120*                                 INLEVERANSOMRÅDE NÄSTA                  
000130     03 HTERM-MID-RAD        OCCURS 14 TIMES.                             
000140*                                 LINES                                   
000150        05 HTERM-MID-IDLEVNR PIC X(5).                                    
000160*                                 LEVERANTÖRNUMMER                        
000170        05 HTERM-MID-IDOKOLLI                                             
000180                             PIC X(9).                                    
000190*                                 ODETTE KOLLINUMMER                      
      *** END COPY W6I10203    LENGTH=207                                       

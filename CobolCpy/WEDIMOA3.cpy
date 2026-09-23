000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI MOA MONETARY AMOUNT                                              
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -TO SPECIFY MONETARY AMOUNTS, RELATED TO THE CNI-SEGMENT.            
000070*                                                                         
000100 01  WEDIMOA3.                                                            
000230     03 MOA3-IDPTYP                            PIC X(03).                 
000240*                                              MOA                        
000501     03 MOA3-LENGTH                            PIC 9(03).                 
000502*                                              LENGTH = 024               
000503*                                                                         
000515     03 MOA3-C516-MONETARY-AMOUNT.                                        
000516*                                                                         
000517        05 MOA3-5025-MONETARY-AMOUNT-QUAL      PIC X(03).                 
000518*                                                                         
000519        05 MOA3-5004-MONETARY-AMOUNT-TKN       PIC X(01).                 
000520*                                                                         
000521        05 MOA3-5004-MONETARY-AMOUNT           PIC 9(15)V9(02).           
000522*                                                                         
000523        05 MOA3-6345-CURRENCY-CODED            PIC X(03).                 
000530*                                                                         
000540*** END OF VILMAII-COPY LENGTH=30                                         

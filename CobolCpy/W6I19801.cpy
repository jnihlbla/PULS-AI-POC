000010 01  MID-W6I19801.                                                        
000020*                                 COPYTEXT FÖR MID                        
000030*                                 W6I19801                                
000040     03 MID-GROUP.                                                        
000050*                                 LINES                                   
000060        05 MID-ADINLOMR-PRT  PIC X(4).                                    
000070*                                 PRINTERPLACERING                        
000080        05 MID-IDINLVGN-IN   PIC X(3).                                    
000090*                                 VAGNSIDENTITET                          
000100        05 MID-IDINLVGN-UT   PIC X(3).                                    
000110*                                 VAGNSIDENTITET                          
000120        05 MID-ADINLOMR-IN   PIC X(4).                                    
000130*                                 INLEVERANSOMRÅDE                        
000140        05 MID-ADINLOMR-UT   PIC X(4).                                    
000150*                                 INLEVERANSOMRÅDE                        
000160        05 MID-ADINLOMR-NXT-IN                                            
000170                             PIC X(4).                                    
000180*                                 INLEVERANSOMRÅDE NÄSTA                  
000190        05 MID-ADINLOMR-NXT-UT                                            
000200                             PIC X(4).                                    
000210*                                 INLEVERANSOMRÅDE NÄSTA                  
000220        05 MID-KDINLQ-IN     PIC X.                                       
000230*                                 INLEVERANSKÖTYP KOLLI/PARTI             
000240        05 MID-KDINLQ-UT     PIC X.                                       
000250*                                 INLEVERANSKÖTYP KOLLI/PARTI             
000260        05 MID-BEFT-IN       PIC X(2).                                    
000270*                                 FÖRPACKNINGSTYP                         
000280        05 MID-BEFT-UT       PIC X(2).                                    
000290*                                 FÖRPACKNINGSTYP                         
000300        05 MID-FLINLFB-IN    PIC X.                                       
000310*                                 VALD TILL FÖRBEHANDLING                 
000320        05 MID-FLINLFB-UT    PIC X.                                       
000330*                                 VALD TILL FÖRBEHANDLING                 
000340        05 MID-IDLEVNR-KOLLI-IN                                           
000350                             PIC X(5).                                    
000360*                                 LEVERANTÖRNUMMER KOLLI                  
000370        05 MID-IDLEVNR-KOLLI-UT                                           
000380                             PIC X(5).                                    
000390*                                 LEVERANTÖRNUMMER KOLLI                  
000400        05 MID-IDOKOLLI-IN   PIC X(9).                                    
000410*                                 ODETTE KOLLINUMMER                      
000420        05 MID-IDOKOLLI-UT   PIC X(9).                                    
000430*                                 ODETTE KOLLINUMMER                      
000440        05 MID-IDLOPNRM-IN   PIC X(9).                                    
000450*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000460*                                 (0VVDLLLLK)                             
000470        05 MID-IDLOPNRM-UT   PIC X(9).                                    
000480*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000490*                                 (0VVDLLLLK)                             
000500     03 MID-IDLOPNRM-NEXT    PIC X(8).                                    
000510*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000520*                                 (0VVDLLLLK)                             
000530     03 MID-IDRADNR-NEXT     PIC X(4).                                    
000540*                                 RADNUMMER                               
000550     03 MID-IDLOPNRM-ENTER   PIC X(8).                                    
000560*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000570*                                 (0VVDLLLLK)                             
000580     03 MID-IDRADNR-ENTER    PIC X(4).                                    
000590*                                 RADNUMMER                               
000600     03 MID-RAD              OCCURS 12 TIMES.                             
000610        05 MID-IDARTNR       PIC X(8).                                    
000620*                                 ARTIKELNUMMER                           
000630        05 MID-KDKLIPRI      PIC X.                                       
000640*                                 PRIORITETSKOD KOLLI                     
000650     03 MID-ADINLOMR-TOMN    PIC X(4).                                    
000660*                                 INLEVERANSOMRÅDE                        
      *** END COPY W6I19801    LENGTH=216                                       

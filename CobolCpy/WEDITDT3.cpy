000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI TDT DETAILS OF TRANSPORT                                         
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -DETAILS OF TRANSPORT                                                
000070*                                                                         
000100 01  WEDITDT3.                                                            
000230     03 TDT3-IDPTYP                            PIC X(03).                 
000240*                                              TDT                        
000501     03 TDT3-LENGTH                            PIC 9(03).                 
000502*                                              LENGTH = 113               
000503*                                                                         
000515     03 TDT3-8051-TRANSP-STAGE-QUAL            PIC X(03).                 
000516*                                                                         
000517     03 TDT3-8028-CONVEYANCE-REF               PIC X(17).                 
000518*                                                                         
000519     03 TDT3-C220-MODE-OF-TRANSPORT.                                      
000520*                                                                         
000521        05 TDT3-8067-MODE-OF-TRANSPORT         PIC X(03).                 
000522*                                                                         
000523     03 TDT3-C040-CARRIER.                                                
000524*                                                                         
000525        05 TDT3-3127-CARRIER-ID                PIC X(17).                 
000526*                                                                         
000527        05 TDT3-1131-CODE-LIST-QUAL            PIC X(03).                 
000528*                                                                         
000529        05 TDT3-3055-CODE-LIST-AGENCY          PIC X(03).                 
000530*                                                                         
000531        05 TDT3-3128-CARRIER-NAME              PIC X(35).                 
000533*                                                                         
000534     03 TDT3-C222-TRANSPORT-ID.                                           
000535*                                                                         
000536        05 TDT3-8213-ID-MEANS-OF-TRPT          PIC X(09).                 
000537*                                                                         
000538        05 TDT3-1131-CODE-LIST-QUAL            PIC X(03).                 
000539*                                                                         
000540        05 TDT3-8212-ID-MEANS-OF-TRPT          PIC X(17).                 
000541*                                                                         
000542        05 TDT3-8453-NAT-MEANS-OF-TRPT         PIC X(03).                 
000543*                                                                         
000550*** END OF VILMAII-COPY LENGTH=119                                        

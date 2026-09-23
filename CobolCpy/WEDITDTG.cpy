000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI TDT DETAILS OF TRANSPORT                                         
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -DETAILS OF TRANSPORT                                                
000070*                                                                         
000100 01  WEDITDT.                                                             
000230     03 TDT-IDPTYP                             PIC X(03).                 
000240*                                              TDT                        
000501     03 TDT-LENGTH                             PIC 9(03).                 
000502*                                              LENGTH = 205               
000503*                                                                         
000515     03 TDT-8051-TRANSP-STAGE-QUAL             PIC X(03).                 
000516*                                                                         
000517     03 TDT-8028-CONVEYANCE-REF                PIC X(17).                 
000518*                                                                         
000519     03 TDT-C220-MODE-OF-TRANSPORT.                                       
000520*                                                                         
000521        05 TDT-8067-MODE-OF-TRANSP-COD         PIC X(03).                 
000520*                                                                         
000521        05 TDT-8066-MODE-OF-TRANSPORT          PIC X(17).                 
000522*                                                                         
000523     03 TDT-C228-TRANSPORT-MEANS.                                         
000524*                                                                         
000525        05 TDT-8179-TYPE-MEANS-TRP-ID          PIC X(08).                 
000526*                                                                         
000527        05 TDT-8178-TYPE-MEANS-TRP             PIC X(17).                 
000528*                                                                         
000523     03 TDT-C040-CARRIER.                                                 
000524*                                                                         
000525        05 TDT-3127-CARRIER-ID                 PIC X(17).                 
000526*                                                                         
000527        05 TDT-1131-CODE-LIST-QUAL             PIC X(03).                 
000528*                                                                         
000529        05 TDT-3055-CODE-LIST-AGENCY           PIC X(03).                 
000530*                                                                         
000531        05 TDT-3128-CARRIER-NAME               PIC X(35).                 
000533*                                                                         
000523     03 TDT-8141-TRANSIT-DIR-CODED             PIC X(03).                 
000524*                                                                         
000534     03 TDT-C401-TRANSPORT-ID.                                            
000535*                                                                         
000536        05 TDT-8457-EXCESS-TRPT-CODED          PIC X(03).                 
000537*                                                                         
000538        05 TDT-8459-EXCESS-TRPT-RESP           PIC X(03).                 
000539*                                                                         
000540        05 TDT-7130-CUSTOMER-AUTH-NO           PIC X(17).                 
000541*                                                                         
000534     03 TDT-C222-TRANSPORT-ID.                                            
000535*                                                                         
000536        05 TDT-8213-ID-MEANS-OF-TRPT           PIC X(09).                 
000537*                                                                         
000538        05 TDT-1131-CODE-LIST-QUAL             PIC X(03).                 
000537*                                                                         
000538        05 TDT-3055-CODE-LIST-CODED            PIC X(03).                 
000539*                                                                         
000540        05 TDT-8212-ID-MEANS-OF-TRPT           PIC X(35).                 
000541*                                                                         
000542        05 TDT-8453-NAT-MEANS-OF-TRPT          PIC X(03).                 
000543*                                                                         
000542        05 TDT-8281                            PIC X(03).                 
000543*                                                                         
000550*** END OF VILMAII-COPY LENGTH=211                                        

000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI LOC PLACE/LOCATION IDENTIFICATION                                
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -TO INDICATE A LOCATION SUCH AS PLACE OF DEPARTURE,                  
000061*     DESTINATION ETC, RELATED TO THIS LEG OF TRANSPORT.                  
000070*                                                                         
000100 01  WEDILOC3.                                                            
000230     03 LOC3-IDPTYP                            PIC X(03).                 
000240*                                              LOC                        
000501     03 LOC3-LENGTH                            PIC 9(03).                 
000502*                                              LENGTH = 051               
000503*                                                                         
000515     03 LOC3-3227-PLACE-LOC-QUAL               PIC X(03).                 
000516*                                                                         
000517     03 LOC3-C517-LOCATION-ID.                                            
000518*                                                                         
000519        05 LOC3-3225-PLACE-LOC-ID              PIC X(25).                 
000520*                                                                         
000521        05 LOC3-1131-CODE-LIST-QUAL            PIC X(03).                 
000522*                                                                         
000523        05 LOC3-3055-CODE-LIST-AGENCY          PIC X(03).                 
000524*                                                                         
000525        05 LOC3-3224-PLACE-LOCATION            PIC X(17).                 
000526*                                                                         
000540*** END OF VILMAII-COPY LENGTH=57                                         

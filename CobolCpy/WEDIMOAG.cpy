000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI MOA MONETARY AMOUNT                                              
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -TO SPECIFY MONETARY AMOUNTS                                         
000070*                                                                         
000100 01  WEDIMOA.                                                             
000230     03 MOA-IDPTYP                             PIC X(03).                 
000240*                                              MOA                        
000501     03 MOA-LENGTH                             PIC 9(03).                 
000502*                                              LENGTH = 030               
000503*                                                                         
000515     03 MOA-C516-MONETARY-AMOUNT.                                         
000516*                                                                         
000517        05 MOA-5025-MON-AMOUNT-QUAL            PIC X(03).                 
000518*                                                                         
000519        05 MOA-5004-MONETARY-AMOUNT            PIC 9(18).                 
000522*                                                                         
000523        05 MOA-6345-CURRENCY-CODED             PIC X(03).                 
000520*                                                                         
000521        05 MOA-6343-CURRENCY-QUAL              PIC X(03).                 
000520*                                                                         
000521        05 MOA-4405-STATUS-CODED               PIC X(03).                 
000530*                                                                         
000540*** END OF VILMAII-COPY LENGTH=36                                         

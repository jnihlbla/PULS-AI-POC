000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI NAD, NAME AND ADDDRESS                                           
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000031*                                                                         
000032*    FUNCTION,                                                            
000033*    -TO SPECIFY NAMES ANDADDRESSESOF THE PARTIES AND THIER               
000034*     RELATED FUNCTION - RELATED TO THE SPECIFIED TRANSPORT               
000035*     STAGE IN THE TDT-STATMENT.                                          
000040*                                                                         
000100 01  WEDINAD3.                                                            
000230     03 NAD3-IDPTYP                            PIC X(03).                 
000240*                                              NAD                        
000501     03 NAD3-LENGTH                            PIC 9(03).                 
000502*                                              LENGTH = 531               
000503*                                                                         
000515     03 NAD3-3035-PARTY-QUAL                   PIC X(03).                 
000516*                                                                         
000518     03 NAD3-C082-PARTY-ID-DETAILS.                                       
000519*                                                                         
000520        05 NAD3-3039-PARTY-ID                  PIC X(17).                 
000521*                                                                         
000522        05 NAD3-1131-CODE-LIST-QUAL            PIC X(03).                 
000523*                                                                         
000524        05 NAD3-3055-CODE-LIST-AGENCY          PIC X(03).                 
000525*                                                                         
000526*                                                                         
000527     03 NAD3-C058-NAME-AND-ADDRESS.                                       
000528*                                                                         
000530        05 NAD3-31241-NAME-AND-ADDR-1          PIC X(35).                 
000531*                                                                         
000532        05 NAD3-31242-NAME-AND-ADDR-2          PIC X(35).                 
000533*                                                                         
000534        05 NAD3-31243-NAME-AND-ADDR-3          PIC X(35).                 
000535*                                                                         
000536        05 NAD3-31244-NAME-AND-ADDR-4          PIC X(35).                 
000537*                                                                         
000538        05 NAD3-31245-NAME-AND-ADDR-5          PIC X(35).                 
000539*                                                                         
000540*                                                                         
000541     03 NAD3-C080-PARTY-NAME.                                             
000542*                                                                         
000543        05 NAD3-30361-PARTY-NAME-1             PIC X(35).                 
000544*                                                                         
000545        05 NAD3-30362-PARTY-NAME-2             PIC X(35).                 
000546*                                                                         
000547        05 NAD3-30362-PARTY-NAME-3             PIC X(35).                 
000548*                                                                         
000549        05 NAD3-30362-PARTY-NAME-4             PIC X(35).                 
000550*                                                                         
000551        05 NAD3-30362-PARTY-NAME-5             PIC X(35).                 
000552*                                                                         
000553        05 NAD3-3045-PARTY-FORMAT              PIC X(03).                 
000554*                                                                         
000555*                                                                         
000556     03 NAD3-C059-STREET.                                                 
000557*                                                                         
000558        05 NAD3-30421-STREET-PBOX-1            PIC X(35).                 
000559*                                                                         
000560        05 NAD3-30422-STREET-PBOX-2            PIC X(35).                 
000561*                                                                         
000562        05 NAD3-30423-STREET-PBOX-3            PIC X(35).                 
000563*                                                                         
000564*                                                                         
000565     03 NAD3-3164-CITY-NAME                    PIC X(35).                 
000566*                                                                         
000567     03 NAD3-3251-POSTCODE-ID                  PIC X(09).                 
000568*                                                                         
000569     03 NAD3-3207-COUNTRY-CODED                PIC X(03).                 
000570*                                                                         
000580*** END OF VILMAII-COPY LENGTH=537                                        

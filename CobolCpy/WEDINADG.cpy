000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI NAD, NAME AND ADDDRESS                                           
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000031*                                                                         
000032*    FUNCTION,                                                            
000033*    -TO SPECIFY NAMES AND ADDRESSES OF THE PARTIES AND THEIR             
000034*     RELATED FUNCTION - ONE SEGMENT FOR THE CONSIGNEE FOR                
000035*     EACH CNI-SEGMENT.                                                   
000040*                                                                         
000100 01  WEDINAD.                                                             
000230     03 NAD-IDPTYP                             PIC X(03).                 
000240*                                              NAD                        
000501     03 NAD-LENGTH                             PIC 9(03).                 
000502*                                              LENGTH = 558               
000503*                                                                         
000515     03 NAD-3035-PARTY-QUAL                    PIC X(03).                 
000516*                                                                         
000518     03 NAD-C082-PARTY-ID-DET.                                            
000519*                                                                         
000520        05 NAD-3039-PARTY-ID                   PIC X(35).                 
000521*                                                                         
000522        05 NAD-1131-CODE-LIST-QUAL             PIC X(03).                 
000523*                                                                         
000524        05 NAD-3055-CODE-LIST-AGEN             PIC X(03).                 
000525*                                                                         
000526*                                                                         
000527     03 NAD-C058-NAME-AND-ADDR.                                           
000528*                                                                         
000530        05 NAD-3124-NAME-ADDR-1                PIC X(35).                 
000531*                                                                         
000532        05 NAD-3124-NAME-ADDR-2                PIC X(35).                 
000533*                                                                         
000534        05 NAD-3124-NAME-ADDR-3                PIC X(35).                 
000535*                                                                         
000536        05 NAD-3124-NAME-ADDR-4                PIC X(35).                 
000537*                                                                         
000538        05 NAD-3124-NAME-ADDR-5                PIC X(35).                 
000539*                                                                         
000527     03 NAD-C080-PARTY-NAME.                                              
000528*                                                                         
000530        05 NAD-3036-PARTY-NAME-1               PIC X(35).                 
000528*                                                                         
000530        05 NAD-3036-PARTY-NAME-2               PIC X(35).                 
000528*                                                                         
000530        05 NAD-3036-PARTY-NAMNE-3              PIC X(35).                 
000528*                                                                         
000530        05 NAD-3036-PARTY-NAME-4               PIC X(35).                 
000528*                                                                         
000530        05 NAD-3036-PARTY-NAME-5               PIC X(35).                 
000540*                                                                         
000530        05 NAD-3045-PARTY-NAME-COD             PIC X(03).                 
000540*                                                                         
000556     03 NAD-C059-STREET.                                                  
000557*                                                                         
000558        05 NAD-3042-STREET-PBOX-1              PIC X(35).                 
000559*                                                                         
000560        05 NAD-3042-STREET-PBOX-2              PIC X(35).                 
000561*                                                                         
000562        05 NAD-3042-STREET-PBOX-3              PIC X(35).                 
000563*                                                                         
000564*                                                                         
000565     03 NAD-3164-CITY-NAME                     PIC X(35).                 
000566*                                                                         
000565     03 NAD-3229-COUNTRY-ID                    PIC X(09).                 
000566*                                                                         
000567     03 NAD-3251-POSTCODE-ID                   PIC X(09).                 
000568*                                                                         
000569     03 NAD-3207-COUNTRY-CODED                 PIC X(03).                 
000570*                                                                         
000580*** END OF VILMAII-COPY LENGTH=564                                        

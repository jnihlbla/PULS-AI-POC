000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI NAD, NAME AND ADDDRESS                                           
000030*    ANVÄNDS FÖR DESDAV MEDDELANDE, GENERELL COPYTEXT                     
000031*                                                                         
000032*    FUNCTION,                                                            
000033*    -TO SPECIFY NAMES AND ADDRESSES OF THE PARTIES AND THEIR             
000034*     RELATED FUNCTION                                                    
000040*                                                                         
000100 01  WEDINAD.                                                             
000230     03 NAD-IDPTYP                             PIC X(03).                 
000240*                                              NAD                        
000250     03 NAD-LENGTH                             PIC 9(03).                 
000260*                                              LENGTH = 198               
000503*                                                                         
000515     03 NAD-3035-PARTY-QUAL                    PIC X(03).                 
000516*    BY = IMPORTER REF.                                                   
000517*    CN = RECEIVER                                                        
000518*                                                                         
000532     03 NAD-C082-CONSIGNEE.                                               
000542*                                                                         
000543        05 NAD-3039-CONSIGNEE                  PIC X(35).                 
000544*       CN->DEALER NO. (7 BYTES)                                          
000545*                                                                         
000546     03 NAD-C080-COMP-NAME.                                               
000547*                                                                         
000548        05 NAD-3124-COMP-NAME-1                PIC X(35).                 
000549*       BY->IMPORTER REF.                                                 
000550*                                                                         
000551        05 NAD-3124-COMP-NAME-2                PIC X(35).                 
000552*       BY->IMPORTER REF.                                                 
000553*                                                                         
000556     03 NAD-C059-COMP-ADR.                                                
000557*                                                                         
000558        05 NAD-3042-STREET-PBOX-1              PIC X(35).                 
000559*                                                                         
000565     03 NAD-3164-CITY-NAME                     PIC X(35).                 
000566*                                                                         
000569     03 NAD-3251-POSTCODE-ID                   PIC X(17).                 
000570*                                                                         
000571     03 NAD-3207-COUNTRY-CODED                 PIC X(03).                 
000572*                                                                         
000573*** END OF VILMAII-COPY LENGTH=204                                        

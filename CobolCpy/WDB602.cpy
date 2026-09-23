000100 01  INV-WDB602.                                                          
000200*                                 DC STYRREGISTER                         
000300*                                 GOODS RECEIVER FOR INVOICE              
000400*                                 NO KEY: NO TWIN SEGMENT                 
000500     03 INV-ADGMT-INV.                                                    
000600*                                 GODSMOTTAGARADRESS FAKTURERING          
000700*                                 GOODS RECEIVER ADDRESS INVOICE          
000800        05 INV-ADGMT-GATA    PIC X(35).                                   
000900*                                 GODSMOTTAGARADRESS GATA                 
001000*                                 GOODS RECEIVER ADDRESS STREET           
001100        05 INV-ADGMT-PADR    PIC X(35).                                   
001200*                                 GODSMOTTAGARADRESS POSTADRESS           
001300*                                 GOODS RECEIVER ADDRESS TOWN             
001400        05 INV-ADPOST-PNRORT REDEFINES INV-ADGMT-PADR.                    
001500*                                 POSTNUMMER + ORT                        
001600*                                 POSTAL CODE + CITY                      
001700           07 INV-ADPOSTNR   PIC X(10).                                   
001800*                                 POSTNUMMER I ADRESS                     
001900*                                 POSTAL CODE IN ADDRESS                  
002000           07 INV-ADCITY     PIC X(25).                                   
002100*                                 BENÄMNING PÅ STAD                       
002200*                                 CITY                                    
002300        05 INV-ADPOST-ORTPNR REDEFINES INV-ADGMT-PADR.                    
002400*                                 ORT + POSTNUMMER                        
002500*                                 CITY + POSTAL CODE                      
002600           07 INV-ADCITY     PIC X(25).                                   
002700*                                 BENÄMNING PÅ STAD                       
002800*                                 CITY                                    
002900           07 INV-ADPOSTNR   PIC X(10).                                   
003000*                                 POSTNUMMER I ADRESS                     
003100*                                 POSTAL CODE IN ADDRESS                  
003200        05 INV-ADGMT-LAND    PIC X(35).                                   
003300*                                 GODSMOTTAGARADRESS LAND                 
003400*                                 GOODS RECEIVER ADDRESS COUNTRY          
003500     03 INV-BEGMT-INV.                                                    
003600*                                 GODSMOTTAGARNAMN FAKTURERING            
003700*                                 GOODS RECEIVER NAME INVOICING           
003800        05 INV-BEGMT-RAD1    PIC X(35).                                   
003900*                                 GODSMOTTAGARNAMN RAD 1                  
004000*                                 GOODS RECEIVER NAME LINE 1              
004100        05 INV-BEGMT-RAD2    PIC X(35).                                   
004200*                                 GODSMOTTAGARNAMN RAD 2                  
004300*                                 GOODS RECEIVER NAME LINE 2              
004400*** END OF VILMAII-COPY LENGTH= 175 BYTES                                 

000100 01  HDR-W476NAPH.                                                        
000200*                                 COPYTEXT FOR NA PROFORMA                
000300*                                 SHIPMENT HEADER                         
000400*                                 RECORD TYPE = H                         
000500     03 HDR-IDAFPRCD         PIC X(10).                                   
000600*                                 AFP-BLANKETT POSTTYP                    
000700*                                 AFP FORMS RECORD TYPE                   
000800     03 HDR-IDDC             PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 HDR-BEGMT-RAD1       PIC X(35).                                   
001200*                                 GODSMOTTAGARNAMN RAD 1                  
001300*                                 GOODS RECEIVER NAME LINE 1              
001400     03 HDR-BEGMT-RAD2       PIC X(35).                                   
001500*                                 GODSMOTTAGARNAMN RAD 2                  
001600*                                 GOODS RECEIVER NAME LINE 2              
001700     03 HDR-ADGMT-GATA       PIC X(35).                                   
001800*                                 GODSMOTTAGARADRESS GATA                 
001900*                                 GOODS RECEIVER ADDRESS STREET           
002000     03 HDR-ADGMT-PADR       PIC X(35).                                   
002100*                                 GODSMOTTAGARADRESS POSTADRESS           
002200*                                 GOODS RECEIVER ADDRESS TOWN             
002300     03 HDR-ADGMT-LAND       PIC X(35).                                   
002400*                                 GODSMOTTAGARADRESS LAND                 
002500*                                 GOODS RECEIVER ADDRESS COUNTRY          
002600     03 HDR-TISKEPPN         PIC 9(6).                                    
002700*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
002800*                                 SHIPPING DATE    (YYMMDD)               
002900     03 HDR-IDDISTR          PIC Z(3)9.                                   
003000*                                 DISTRIKTNUMMER                          
003100*                                 DISTRICT NUMBER                         
003200     03 HDR-IDSHIPM          PIC Z(6)9.                                   
003300*                                 SKEPPNINGSNUMMER                        
003400*                                 SHIPMENT NO                             
003500     03 HDR-IDTRPTNR         PIC Z(2)9.                                   
003600*                                 TRANSPORTIDENTITET                      
003700*                                 TRANSPORT IDENTITY                      
003800     03 HDR-IDLBBET          PIC X(12).                                   
003900*                                 LASTBÄRARBETECKNING                     
004000*                                 TRAILER NUMBER                          
004100*** END OF VILMAII-COPY LENGTH= 219 BYTES                                 

000100 01  MID-W4I23101.                                                        
000200*                                 MID-COPYTEXT FÖR W4I23101               
000300     03 MID-FLVORKO          PIC X.                                       
000400*                                 VOR-KÖ FLAGGA                           
000500     03 MID-IDDISTR          PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR         PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDORDNR          PIC X(5).                                    
001000*                                 ORDERNUMMER                             
001100     03 MID-KDORDKL          PIC X.                                       
001200*                                 ORDERKLASS                              
001300     03 MID-KDFRAKT          PIC X(2).                                    
001400*                                 FRAKTSÄTT DC TILL KUND                  
001500     03 MID-IDDC-TVS         PIC X(2).                                    
001600*                                 DISTRIBUTIONCENTER                      
001700*                                 TVÅNGSSTYRNING                          
001800     03 MID-KDFAKTYP         PIC X.                                       
001900*                                 FAKTURATYP                              
002000     03 MID-BEKUNDRF         PIC X(15).                                   
002100*                                 KUNDENS REFERENS                        
002200     03 MID-FLLSBOK          PIC X.                                       
002300*                                 LAGERAVBOKNING                          
002400     03 MID-FLAUTPAC         PIC X.                                       
002500*                                 AUTOMATISK PACKRAPPORTERING             
002600     03 MID-FLAUTFAK         PIC X.                                       
002700*                                 AUTOMATFAKTURERING ?                    
002800     03 MID-BEGMT.                                                        
002900*                                 GODSMOTTAGARNAMN                        
003000        05 MID-BEGMT-RAD1    PIC X(35).                                   
003100*                                 GODSMOTTAGARNAMN RAD 1                  
003200        05 MID-BEGMT-RAD2    PIC X(35).                                   
003300*                                 GODSMOTTAGARNAMN RAD 2                  
003400     03 MID-ADGMT.                                                        
003500*                                 GODSMOTTAGARADRESS                      
003600        05 MID-ADGMT-GATA    PIC X(35).                                   
003700*                                 GODSMOTTAGARADRESS GATA                 
003800        05 MID-ADGMT-PADR    PIC X(35).                                   
003900*                                 GODSMOTTAGARADRESS POSTADRESS           
004000        05 MID-ADPOST-PNRORT REDEFINES MID-ADGMT-PADR.                    
004100*                                 POSTNUMMER + ORT                        
004200           07 MID-ADPOSTNR   PIC X(10).                                   
004300*                                 POSTNUMMER I ADRESS                     
004400           07 MID-ADCITY     PIC X(25).                                   
004500*                                 BENÄMNING PÅ STAD                       
004600        05 MID-ADPOST-ORTPNR REDEFINES MID-ADGMT-PADR.                    
004700*                                 ORT + POSTNUMMER                        
004800           07 MID-ADCITY     PIC X(25).                                   
004900*                                 BENÄMNING PÅ STAD                       
005000           07 MID-ADPOSTNR   PIC X(10).                                   
005100*                                 POSTNUMMER I ADRESS                     
005200        05 MID-ADGMT-LAND    PIC X(35).                                   
005300*                                 GODSMOTTAGARADRESS LAND                 
005400     03 MID-BEGMRK.                                                       
005500*                                 GODSMÄRKE                               
005600        05 MID-BEGMRK-RAD1   PIC X(30).                                   
005700*                                 GODSMÄRKE  RAD1                         
005800        05 MID-BEGMRK-RAD2   PIC X(30).                                   
005900*                                 GODSMÄRKE  RAD2                         
006000     03 MID-IDFTG            PIC X(2).                                    
006100*                                 FÖRETAGSID EKONOM REDOVISNING           
006200     03 MID-IDKONTO          PIC X(10).                                   
006300*                                 KONTO                                   
006400     03 MID-IDANALYS         PIC X(12).                                   
006500*                                 ANALYSNUMMER                            
006600     03 MID-IDKST            PIC X(10).                                   
006700*                                 KOSTNADSSTÄLLE                          
006800     03 MID-BEVARREF         PIC X(10).                                   
006900*                                 VÅR REFERENS                            
007000     03 MID-IDLEVNR-EJLS     PIC X(5).                                    
007100*                                 LEVERANTÖR SPEC. ORDER EJ LS            
007200*** END OF VILMAII-COPY LENGTH= 324 BYTES                                 

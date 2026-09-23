000100 01  W4I22101.                                                            
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W4I22101                                
000400     03 FLVORKO              PIC X.                                       
000500*                                 VOR-KÖ FLAGGA                           
000600     03 FLFORBI              PIC X.                                       
000700*                                 FÖRBIORDERFLAGGA                        
000800     03 IDDISTR              PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 IDKUNDNR             PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 IDORDNR5             PIC X(5).                                    
001300*                                 ORDERNUMMER                             
001400     03 KDORDKL              PIC X.                                       
001500*                                 ORDERKLASS                              
001600     03 KDFRAKT              PIC X(2).                                    
001700*                                 FRAKTSÄTT DC TILL KUND                  
001800     03 IDDC-TVS             PIC X(2).                                    
001900*                                 DISTRIBUTIONCENTER                      
002000*                                 TVÅNGSSTYRNING                          
002100     03 KDFAKTYP             PIC X.                                       
002200*                                 FAKTURATYP                              
002300     03 TIRFS-DAT            PIC X(6).                                    
002400     03 TIRFS-TID            PIC X(4).                                    
002500     03 BEKUNDRF             PIC X(15).                                   
002600*                                 KUNDENS REFERENS                        
002700     03 NORMALORDER-JA-NEJ   PIC X.                                       
002800*                                 ALLMÄN FLAGGA                           
002900     03 BELAGINS-GRP.                                                     
003000*                                 LAGERINSTRUKTIONER                      
003100        05 BELAGINS-DEL1     PIC X(60).                                   
003200*                                 DEL AV LAGERINSTRUKTION                 
003300        05 BELAGINS-DEL2     PIC X(60).                                   
003400*                                 DEL AV LAGERINSTRUKTION                 
003500     03 BEGMT.                                                            
003600*                                 GODSMOTTAGARNAMN                        
003700        05 BEGMT-RAD1        PIC X(35).                                   
003800*                                 GODSMOTTAGARNAMN RAD 1                  
003900        05 BEGMT-RAD2        PIC X(35).                                   
004000*                                 GODSMOTTAGARNAMN RAD 2                  
004100     03 ADGMT.                                                            
004200*                                 GODSMOTTAGARADRESS                      
004300        05 ADGMT-GATA        PIC X(35).                                   
004400*                                 GODSMOTTAGARADRESS GATA                 
004500        05 ADGMT-PADR        PIC X(35).                                   
004600*                                 GODSMOTTAGARADRESS POSTADRESS           
004700        05 ADPOST-PNRORT REDEFINES ADGMT-PADR.                            
004800*                                 POSTNUMMER + ORT                        
004900           07 ADPOSTNR       PIC X(10).                                   
005000*                                 POSTNUMMER I ADRESS                     
005100           07 ADCITY         PIC X(25).                                   
005200*                                 BENÄMNING PÅ STAD                       
005300        05 ADPOST-ORTPNR REDEFINES ADGMT-PADR.                            
005400*                                 ORT + POSTNUMMER                        
005500           07 ADCITY         PIC X(25).                                   
005600*                                 BENÄMNING PÅ STAD                       
005700           07 ADPOSTNR       PIC X(10).                                   
005800*                                 POSTNUMMER I ADRESS                     
005900        05 ADGMT-LAND        PIC X(35).                                   
006000*                                 GODSMOTTAGARADRESS LAND                 
006100     03 BEGMRK.                                                           
006200*                                 GODSMÄRKE                               
006300        05 BEGMRK-RAD1       PIC X(30).                                   
006400*                                 GODSMÄRKE  RAD1                         
006500        05 BEGMRK-RAD2       PIC X(30).                                   
006600*                                 GODSMÄRKE  RAD2                         
006700     03 IDFTG                PIC X(2).                                    
006800*                                 FÖRETAGSID EKONOM REDOVISNING           
006900     03 IDKONTO              PIC X(10).                                   
007000*                                 KONTO                                   
007100     03 IDANALYS             PIC X(12).                                   
007200*                                 ANALYSNUMMER                            
007300     03 IDKST                PIC X(10).                                   
007400*                                 KOSTNADSSTÄLLE                          
007500     03 BEVARREF             PIC X(10).                                   
007600*                                 VÅR REFERENS                            
007700*** END OF VILMAII-COPY LENGTH= 448 BYTES                                 

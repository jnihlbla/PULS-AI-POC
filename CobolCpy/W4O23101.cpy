000100 01  MOD-W4O23101.                                                        
000200*                                 MOD-COPYTEXT FÖR W4O23101               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-FLVORKO          PIC X.                                       
000800*                                 VOR-KÖ FLAGGA                           
000900     03 MOD-TEDDI            PIC X(11).                                   
001000*                                 TEXTFÄLT DDI                            
001100     03 MOD-IDDISTR-ATTR     PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-IDDISTR          PIC Z(3)9.                                   
001400*                                 DISTRIKTNUMMER                          
001500     03 MOD-IDKUNDNR-ATTR    PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-IDKUNDNR         PIC Z(5)9.                                   
001800*                                 KUNDNUMMER                              
001900     03 MOD-IDORDNR-ATTR     PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-IDORDNR          PIC Z(4)9.                                   
002200*                                 ORDERNUMMER                             
002300     03 MOD-KDORDKL-ATTR     PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-KDORDKL          PIC 9.                                       
002600*                                 ORDERKLASS                              
002700     03 MOD-KDFRAKT-ATTR     PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-KDFRAKT          PIC Z9.                                      
003000*                                 FRAKTSÄTT DC TILL KUND                  
003100     03 MOD-IDDC-TVS-ATTR    PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-IDDC-TVS         PIC X(2).                                    
003400*                                 DISTRIBUTIONCENTER                      
003500*                                 TVÅNGSSTYRNING                          
003600     03 MOD-KDFAKTYP-ATTR    PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-KDFAKTYP         PIC X.                                       
003900*                                 FAKTURATYP                              
004000     03 MOD-BEKUNDRF         PIC X(15).                                   
004100*                                 KUNDENS REFERENS                        
004200     03 MOD-FLLSBOK-ATTR     PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-FLLSBOK          PIC X.                                       
004500*                                 LAGERAVBOKNING                          
004600     03 MOD-FLAUTPAC-ATTR    PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-FLAUTPAC         PIC X.                                       
004900*                                 AUTOMATISK PACKRAPPORTERING             
005000     03 MOD-FLAUTFAK-ATTR    PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-FLAUTFAK         PIC X.                                       
005300*                                 AUTOMATFAKTURERING ?                    
005400     03 MOD-BEGMT.                                                        
005500*                                 GODSMOTTAGARNAMN                        
005600        05 MOD-BEGMT-RAD1    PIC X(35).                                   
005700*                                 GODSMOTTAGARNAMN RAD 1                  
005800        05 MOD-BEGMT-RAD2    PIC X(35).                                   
005900*                                 GODSMOTTAGARNAMN RAD 2                  
006000     03 MOD-ADGMT.                                                        
006100*                                 GODSMOTTAGARADRESS                      
006200        05 MOD-ADGMT-GATA    PIC X(35).                                   
006300*                                 GODSMOTTAGARADRESS GATA                 
006400        05 MOD-ADGMT-PADR    PIC X(35).                                   
006500*                                 GODSMOTTAGARADRESS POSTADRESS           
006600        05 MOD-ADPOST-PNRORT REDEFINES MOD-ADGMT-PADR.                    
006700*                                 POSTNUMMER + ORT                        
006800           07 MOD-ADPOSTNR   PIC X(10).                                   
006900*                                 POSTNUMMER I ADRESS                     
007000           07 MOD-ADCITY     PIC X(25).                                   
007100*                                 BENÄMNING PÅ STAD                       
007200        05 MOD-ADPOST-ORTPNR REDEFINES MOD-ADGMT-PADR.                    
007300*                                 ORT + POSTNUMMER                        
007400           07 MOD-ADCITY     PIC X(25).                                   
007500*                                 BENÄMNING PÅ STAD                       
007600           07 MOD-ADPOSTNR   PIC X(10).                                   
007700*                                 POSTNUMMER I ADRESS                     
007800        05 MOD-ADGMT-LAND    PIC X(35).                                   
007900*                                 GODSMOTTAGARADRESS LAND                 
008000     03 MOD-BEGMRK.                                                       
008100*                                 GODSMÄRKE                               
008200        05 MOD-BEGMRK-RAD1   PIC X(30).                                   
008300*                                 GODSMÄRKE  RAD1                         
008400        05 MOD-BEGMRK-RAD2   PIC X(30).                                   
008500*                                 GODSMÄRKE  RAD2                         
008600     03 MOD-IDFTG-ATTR       PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800     03 MOD-IDFTG            PIC 9(2).                                    
008900*                                 FÖRETAGSID EKONOM REDOVISNING           
009000     03 MOD-IDKONTO-ATTR     PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200     03 MOD-IDKONTO          PIC Z(9)9.                                   
009300*                                 KONTO                                   
009400     03 MOD-IDANALYS-ATTR    PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600     03 MOD-IDANALYS         PIC X(12).                                   
009700*                                 ANALYSNUMMER                            
009800     03 MOD-IDKST-ATTR       PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000     03 MOD-IDKST            PIC X(10).                                   
010100*                                 KOSTNADSSTÄLLE                          
010200     03 MOD-BEVARREF         PIC X(10).                                   
010300*                                 VÅR REFERENS                            
010400     03 MOD-IDLEVNR-EJLS-ATTR                                             
010500                             PIC X(2).                                    
010600*                                 MFS ATTRIBUTFÄLT                        
010700     03 MOD-IDLEVNR-EJLS     PIC X(5).                                    
010800*                                 LEVERANTÖR SPEC. ORDER EJ LS            
010900     03 MOD-TEMFSINF         PIC X(55).                                   
011000*                                 INFORMATIONSMEDDELANDE                  
011100*** END OF VILMAII-COPY LENGTH= 464 BYTES                                 

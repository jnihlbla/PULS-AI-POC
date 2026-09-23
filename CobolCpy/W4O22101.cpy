000100 01  W4O22101.                                                            
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W4O22101                                
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 FLVORKO              PIC X.                                       
000900*                                 VOR-KÖ FLAGGA                           
001000     03 FLFORBI              PIC X.                                       
001100*                                 FÖRBIORDERFLAGGA                        
001200     03 TEDDI                PIC X(11).                                   
001300*                                 TEXTFÄLT DDI                            
001400     03 IDDISTR-ATTR         PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 IDDISTR-1-IN         PIC X(4).                                    
001700*                                 DISTRIKTNUMMER                          
001800     03 IDKUNDNR-ATTR        PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 IDKUNDNR             PIC X(6).                                    
002100*                                 KUNDNUMMER                              
002200     03 IDORDNR7-ATTR        PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 IDORDNR5             PIC X(5).                                    
002500*                                 ORDERNUMMER                             
002600     03 KDORDKL-ATTR         PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 KDORDKL              PIC X.                                       
002900*                                 ORDERKLASS                              
003000     03 KDFRAKT-ATTR         PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 KDFRAKT              PIC Z9.                                      
003300*                                 FRAKTSÄTT DC TILL KUND                  
003400     03 IDDC-ATTR            PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 IDDC-TVS             PIC X(2).                                    
003700*                                 DISTRIBUTIONCENTER                      
003800*                                 TVÅNGSSTYRNING                          
003900     03 KDFAKTYP-ATTR        PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 KDFAKTYP             PIC X.                                       
004200*                                 FAKTURATYP                              
004300     03 TIRFS-DAT-ATTR       PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 TIRFS-DAT            PIC X(6).                                    
004600     03 TIRFS-TID-ATTR       PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 TIRFS-TID            PIC X(4).                                    
004900     03 BEKUNDRF             PIC X(15).                                   
005000*                                 KUNDENS REFERENS                        
005100     03 NORMALORDER-ATTR     PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 NORMALORDER-JA-NEJ   PIC X.                                       
005400*                                 ALLMÄN FLAGGA                           
005500     03 BELAGINS-GRP.                                                     
005600*                                 LAGERINSTRUKTIONER                      
005700        05 BELAGINS-DEL1     PIC X(60).                                   
005800*                                 DEL AV LAGERINSTRUKTION                 
005900        05 BELAGINS-DEL2     PIC X(60).                                   
006000*                                 DEL AV LAGERINSTRUKTION                 
006100     03 BEGMT.                                                            
006200*                                 GODSMOTTAGARNAMN                        
006300        05 BEGMT-RAD1        PIC X(35).                                   
006400*                                 GODSMOTTAGARNAMN RAD 1                  
006500        05 BEGMT-RAD2        PIC X(35).                                   
006600*                                 GODSMOTTAGARNAMN RAD 2                  
006700     03 ADGMT.                                                            
006800*                                 GODSMOTTAGARADRESS                      
006900        05 ADGMT-GATA        PIC X(35).                                   
007000*                                 GODSMOTTAGARADRESS GATA                 
007100        05 ADGMT-PADR        PIC X(35).                                   
007200*                                 GODSMOTTAGARADRESS POSTADRESS           
007300        05 ADPOST-PNRORT REDEFINES ADGMT-PADR.                            
007400*                                 POSTNUMMER + ORT                        
007500           07 ADPOSTNR       PIC X(10).                                   
007600*                                 POSTNUMMER I ADRESS                     
007700           07 ADCITY         PIC X(25).                                   
007800*                                 BENÄMNING PÅ STAD                       
007900        05 ADPOST-ORTPNR REDEFINES ADGMT-PADR.                            
008000*                                 ORT + POSTNUMMER                        
008100           07 ADCITY         PIC X(25).                                   
008200*                                 BENÄMNING PÅ STAD                       
008300           07 ADPOSTNR       PIC X(10).                                   
008400*                                 POSTNUMMER I ADRESS                     
008500        05 ADGMT-LAND        PIC X(35).                                   
008600*                                 GODSMOTTAGARADRESS LAND                 
008700     03 BEGMRK.                                                           
008800*                                 GODSMÄRKE                               
008900        05 BEGMRK-RAD1       PIC X(30).                                   
009000*                                 GODSMÄRKE  RAD1                         
009100        05 BEGMRK-RAD2       PIC X(30).                                   
009200*                                 GODSMÄRKE  RAD2                         
009300     03 IDFTG-ATTR           PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 IDFTG                PIC 9(2).                                    
009600*                                 FÖRETAGSID EKONOM REDOVISNING           
009700     03 IDKONTO-ATTR         PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900     03 IDKONTO              PIC Z(9)9.                                   
010000*                                 KONTO                                   
010100     03 IDANALYS-ATTR        PIC X(2).                                    
010200*                                 MFS ATTRIBUTFÄLT                        
010300     03 IDANALYS             PIC X(12).                                   
010400*                                 ANALYSNUMMER                            
010500     03 IDKST-ATTR           PIC X(2).                                    
010600*                                 MFS ATTRIBUTFÄLT                        
010700     03 IDKST                PIC X(10).                                   
010800*                                 KOSTNADSSTÄLLE                          
010900     03 BEVARREF             PIC X(10).                                   
011000*                                 VÅR REFERENS                            
011100     03 TEMFSINF             PIC X(55).                                   
011200*                                 INFORMATIONSMEDDELANDE                  
011300*** END OF VILMAII-COPY LENGTH= 586 BYTES                                 

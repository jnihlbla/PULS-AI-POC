000100 01  MOD-W4O26401.                                                        
000200*                                 MOD-COPYTEXT FÖR W4O26401               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MOD-IDORDNR7-IN      PIC X(7).                                    
001200*                                 ORDERNUMMER                             
001300     03 MOD-IDARTNR          PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-IDDISTR-UT       PIC X(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MOD-IDORDNR7-UT      PIC X(7).                                    
002000*                                 ORDERNUMMER                             
002100     03 MOD-FLANNULL         PIC X.                                       
002200*                                 ANNULLATION                             
002300     03 MOD-VARNINGS-TEXT-ATTR                                            
002400                             PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-VARNINGS-TEXT    PIC X(60).                                   
002700     03 MOD-TEDDI            PIC X(9).                                    
002800     03 MOD-BEKUNDRF         PIC X(15).                                   
002900*                                 KUNDENS REFERENS                        
003000     03 MOD-IDFTG-ATTR       PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-IDFTG            PIC 9(2).                                    
003300*                                 FÖRETAGSID EKONOM REDOVISNING           
003400     03 MOD-KDORDKL          PIC 9.                                       
003500*                                 ORDERKLASS                              
003600     03 MOD-KDFRAKT          PIC Z9.                                      
003700*                                 FRAKTSÄTT DC TILL KUND                  
003800     03 MOD-KDPROTYP         PIC X.                                       
003900*                                 TYP AV PROFORMA                         
004000     03 MOD-REOMRTAL-ATTR    PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-REOMRTAL         PIC 9.9(3).                                  
004300*                                 OMRÄKNINGSTAL                           
004400     03 MOD-BEVARREF         PIC X(10).                                   
004500*                                 VÅR REFERENS                            
004600     03 MOD-IDKONTO-ATTR     PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-IDKONTO          PIC Z(9)9.                                   
004900*                                 KONTO                                   
005000     03 MOD-IDSKYLT-ATTR     PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-IDSKYLT          PIC X(3).                                    
005300*                                 NATIONALITETSTECKEN                     
005400*                                 SPRÅKIDENTIFIKATION                     
005500     03 MOD-FORFDAT-ATTR     PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-FORFDAT          PIC X(6).                                    
005800     03 MOD-IDANALYS-ATTR    PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-IDANALYS         PIC X(12).                                   
006100*                                 ANALYSNUMMER                            
006200     03 MOD-IDKST-ATTR       PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-IDKST            PIC X(10).                                   
006500*                                 KOSTNADSSTÄLLE                          
006600     03 MOD-KDFAKTYP-ATTR    PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-KDFAKTYP         PIC X.                                       
006900*                                 FAKTURATYP                              
007000     03 MOD-BEGMT.                                                        
007100*                                 GODSMOTTAGARNAMN                        
007200        05 MOD-BEGMT-RAD1    PIC X(35).                                   
007300*                                 GODSMOTTAGARNAMN RAD 1                  
007400        05 MOD-BEGMT-RAD2    PIC X(35).                                   
007500*                                 GODSMOTTAGARNAMN RAD 2                  
007600     03 MOD-ADGMT.                                                        
007700*                                 GODSMOTTAGARADRESS                      
007800        05 MOD-ADGMT-GATA    PIC X(35).                                   
007900*                                 GODSMOTTAGARADRESS GATA                 
008000        05 MOD-ADGMT-PADR    PIC X(35).                                   
008100*                                 GODSMOTTAGARADRESS POSTADRESS           
008200        05 MOD-ADPOST-PNRORT REDEFINES MOD-ADGMT-PADR.                    
008300*                                 POSTNUMMER + ORT                        
008400           07 MOD-ADPOSTNR   PIC X(10).                                   
008500*                                 POSTNUMMER I ADRESS                     
008600           07 MOD-ADCITY     PIC X(25).                                   
008700*                                 BENÄMNING PÅ STAD                       
008800        05 MOD-ADPOST-ORTPNR REDEFINES MOD-ADGMT-PADR.                    
008900*                                 ORT + POSTNUMMER                        
009000           07 MOD-ADCITY     PIC X(25).                                   
009100*                                 BENÄMNING PÅ STAD                       
009200           07 MOD-ADPOSTNR   PIC X(10).                                   
009300*                                 POSTNUMMER I ADRESS                     
009400        05 MOD-ADGMT-LAND    PIC X(35).                                   
009500*                                 GODSMOTTAGARADRESS LAND                 
009600     03 MOD-BEBET.                                                        
009700*                                 BETALNINGSANSVARIG NAMN                 
009800        05 MOD-BEBETRAD-1    PIC X(35).                                   
009900*                                 DEL AV BETALNINGSANSVARIGS NAMN         
010000        05 MOD-BEBETRAD-2    PIC X(35).                                   
010100*                                 DEL AV BETALNINGSANSVARIGS NAMN         
010200     03 MOD-ADBET.                                                        
010300*                                 BETALNINGSANSVARIG ADRESS               
010400        05 MOD-ADBETRAD-1    PIC X(35).                                   
010500*                                 ADRESSRAD BETALNINGSANSVARIG            
010600        05 MOD-ADBETRAD-2    PIC X(35).                                   
010700*                                 ADRESSRAD BETALNINGSANSVARIG            
010800        05 MOD-ADBETRAD-3    PIC X(35).                                   
010900*                                 ADRESSRAD BETALNINGSANSVARIG            
011000     03 MOD-FLBORT-ATTR      PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200     03 MOD-FLBORT           PIC X.                                       
011300*                                 BORTTAGNINGSFLAGGA                      
011400     03 MOD-TIORDDAT         PIC X(6).                                    
011500*                                 ORDERDATUM                              
011600     03 MOD-TEMFSINF         PIC X(55).                                   
011700*                                 INFORMATIONSMEDDELANDE                  
011800*** END OF VILMAII-COPY LENGTH= 667 BYTES                                 

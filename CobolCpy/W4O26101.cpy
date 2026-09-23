000100 01  MOD-W4O26101.                                                        
000200*                                 MOD-COPYTEXT FÖR W4O26101               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-ATTR     PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 MOD-IDDISTR          PIC Z(3)9.                                   
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-ATTR    PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-IDKUNDNR         PIC Z(5)9.                                   
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDORDNR-ATTR     PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-IDORDNR          PIC Z(6)9.                                   
001800*                                 ORDERNUMMER                             
001900     03 MOD-KDORDKL-ATTR     PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-KDORDKL          PIC 9.                                       
002200*                                 ORDERKLASS                              
002300     03 MOD-KDFRAKT-ATTR     PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-KDFRAKT          PIC Z9.                                      
002600*                                 FRAKTSÄTT DC TILL KUND                  
002700     03 MOD-KDPROTYP-ATTR    PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-KDPROTYP         PIC X.                                       
003000*                                 TYP AV PROFORMA                         
003100     03 MOD-KDFAKTYP-ATTR    PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-KDFAKTYP         PIC X.                                       
003400*                                 FAKTURATYP                              
003500     03 MOD-TEDDI            PIC X(11).                                   
003600*                                 TEXTFÄLT DDI                            
003700     03 MOD-BEKUNDRF         PIC X(15).                                   
003800*                                 KUNDENS REFERENS                        
003900     03 MOD-IDFTG-ATTR       PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-IDFTG            PIC 9(2).                                    
004200*                                 FÖRETAGSID EKONOM REDOVISNING           
004300     03 MOD-REOMRTAL-ATTR    PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-REOMRTAL         PIC 9.9(3).                                  
004600*                                 OMRÄKNINGSTAL                           
004700     03 MOD-BEVARREF         PIC X(10).                                   
004800*                                 VÅR REFERENS                            
004900     03 MOD-IDKONTO-ATTR     PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-IDKONTO          PIC Z(9)9.                                   
005200*                                 KONTO                                   
005300     03 MOD-IDANALYS-ATTR    PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-IDANALYS         PIC X(12).                                   
005600*                                 ANALYSNUMMER                            
005700     03 MOD-IDSKYLT-ATTR     PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900     03 MOD-IDSKYLT          PIC X(3).                                    
006000*                                 NATIONALITETSTECKEN                     
006100*                                 SPRÅKIDENTIFIKATION                     
006200     03 MOD-FORFDAT-ATTR     PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-FORFDAT          PIC X(6).                                    
006500     03 MOD-IDKST-ATTR       PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 MOD-IDKST            PIC X(10).                                   
006800*                                 KOSTNADSSTÄLLE                          
006900     03 MOD-BEGMT.                                                        
007000*                                 GODSMOTTAGARNAMN                        
007100        05 MOD-BEGMT-RAD1    PIC X(35).                                   
007200*                                 GODSMOTTAGARNAMN RAD 1                  
007300        05 MOD-BEGMT-RAD2    PIC X(35).                                   
007400*                                 GODSMOTTAGARNAMN RAD 2                  
007500     03 MOD-ADGMT.                                                        
007600*                                 GODSMOTTAGARADRESS                      
007700        05 MOD-ADGMT-GATA    PIC X(35).                                   
007800*                                 GODSMOTTAGARADRESS GATA                 
007900        05 MOD-ADGMT-PADR    PIC X(35).                                   
008000*                                 GODSMOTTAGARADRESS POSTADRESS           
008100        05 MOD-ADPOST-PNRORT REDEFINES MOD-ADGMT-PADR.                    
008200*                                 POSTNUMMER + ORT                        
008300           07 MOD-ADPOSTNR   PIC X(10).                                   
008400*                                 POSTNUMMER I ADRESS                     
008500           07 MOD-ADCITY     PIC X(25).                                   
008600*                                 BENÄMNING PÅ STAD                       
008700        05 MOD-ADPOST-ORTPNR REDEFINES MOD-ADGMT-PADR.                    
008800*                                 ORT + POSTNUMMER                        
008900           07 MOD-ADCITY     PIC X(25).                                   
009000*                                 BENÄMNING PÅ STAD                       
009100           07 MOD-ADPOSTNR   PIC X(10).                                   
009200*                                 POSTNUMMER I ADRESS                     
009300        05 MOD-ADGMT-LAND    PIC X(35).                                   
009400*                                 GODSMOTTAGARADRESS LAND                 
009500     03 MOD-BEBET.                                                        
009600*                                 BETALNINGSANSVARIG NAMN                 
009700        05 MOD-BEBETRAD-1    PIC X(35).                                   
009800*                                 DEL AV BETALNINGSANSVARIGS NAMN         
009900        05 MOD-BEBETRAD-2    PIC X(35).                                   
010000*                                 DEL AV BETALNINGSANSVARIGS NAMN         
010100     03 MOD-ADBET.                                                        
010200*                                 BETALNINGSANSVARIG ADRESS               
010300        05 MOD-ADBETRAD-1    PIC X(35).                                   
010400*                                 ADRESSRAD BETALNINGSANSVARIG            
010500        05 MOD-ADBETRAD-2    PIC X(35).                                   
010600*                                 ADRESSRAD BETALNINGSANSVARIG            
010700        05 MOD-ADBETRAD-3    PIC X(35).                                   
010800*                                 ADRESSRAD BETALNINGSANSVARIG            
010900     03 MOD-TEMFSINF         PIC X(55).                                   
011000*                                 INFORMATIONSMEDDELANDE                  
011100*** END OF VILMAII-COPY LENGTH= 583 BYTES                                 

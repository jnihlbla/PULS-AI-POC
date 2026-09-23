000100 01  MID-W4I26401.                                                        
000200*                                 MID-COPYTEXT FÖR W4I26401               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDORDNR7-IN      PIC X(7).                                    
000800*                                 ORDERNUMMER                             
000900     03 MID-IDARTNR          PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MID-IDDISTR-UT       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-IDORDNR7-UT      PIC X(7).                                    
001600*                                 ORDERNUMMER                             
001700     03 MID-FLANNULL         PIC X.                                       
001800*                                 ANNULLATION                             
001900     03 MID-VARNINGS-FAELT   PIC X(60).                                   
002000     03 MID-BEKUNDRF         PIC X(15).                                   
002100*                                 KUNDENS REFERENS                        
002200     03 MID-IDFTG            PIC X(2).                                    
002300*                                 FÖRETAGSID EKONOM REDOVISNING           
002400     03 MID-REOMRTAL         PIC X(5).                                    
002500*                                 OMRÄKNINGSTAL                           
002600     03 MID-BEVARREF         PIC X(10).                                   
002700*                                 VÅR REFERENS                            
002800     03 MID-IDKONTO          PIC X(10).                                   
002900*                                 KONTO                                   
003000     03 MID-IDSKYLT          PIC X(3).                                    
003100*                                 NATIONALITETSTECKEN                     
003200*                                 SPRÅKIDENTIFIKATION                     
003300     03 MID-FORFDAT          PIC X(6).                                    
003400     03 MID-IDANALYS         PIC X(12).                                   
003500*                                 ANALYSNUMMER                            
003600     03 MID-IDKST            PIC X(10).                                   
003700*                                 KOSTNADSSTÄLLE                          
003800     03 MID-KDFAKTYP         PIC X.                                       
003900*                                 FAKTURATYP                              
004000     03 MID-BEGMT.                                                        
004100*                                 GODSMOTTAGARNAMN                        
004200        05 MID-BEGMT-RAD1    PIC X(35).                                   
004300*                                 GODSMOTTAGARNAMN RAD 1                  
004400        05 MID-BEGMT-RAD2    PIC X(35).                                   
004500*                                 GODSMOTTAGARNAMN RAD 2                  
004600     03 MID-ADGMT.                                                        
004700*                                 GODSMOTTAGARADRESS                      
004800        05 MID-ADGMT-GATA    PIC X(35).                                   
004900*                                 GODSMOTTAGARADRESS GATA                 
005000        05 MID-ADGMT-PADR    PIC X(35).                                   
005100*                                 GODSMOTTAGARADRESS POSTADRESS           
005200        05 MID-ADPOST-PNRORT REDEFINES MID-ADGMT-PADR.                    
005300*                                 POSTNUMMER + ORT                        
005400           07 MID-ADPOSTNR   PIC X(10).                                   
005500*                                 POSTNUMMER I ADRESS                     
005600           07 MID-ADCITY     PIC X(25).                                   
005700*                                 BENÄMNING PÅ STAD                       
005800        05 MID-ADPOST-ORTPNR REDEFINES MID-ADGMT-PADR.                    
005900*                                 ORT + POSTNUMMER                        
006000           07 MID-ADCITY     PIC X(25).                                   
006100*                                 BENÄMNING PÅ STAD                       
006200           07 MID-ADPOSTNR   PIC X(10).                                   
006300*                                 POSTNUMMER I ADRESS                     
006400        05 MID-ADGMT-LAND    PIC X(35).                                   
006500*                                 GODSMOTTAGARADRESS LAND                 
006600     03 MID-BEBET.                                                        
006700*                                 BETALNINGSANSVARIG NAMN                 
006800        05 MID-BEBETRAD-1    PIC X(35).                                   
006900*                                 DEL AV BETALNINGSANSVARIGS NAMN         
007000        05 MID-BEBETRAD-2    PIC X(35).                                   
007100*                                 DEL AV BETALNINGSANSVARIGS NAMN         
007200     03 MID-ADBET.                                                        
007300*                                 BETALNINGSANSVARIG ADRESS               
007400        05 MID-ADBETRAD-1    PIC X(35).                                   
007500*                                 ADRESSRAD BETALNINGSANSVARIG            
007600        05 MID-ADBETRAD-2    PIC X(35).                                   
007700*                                 ADRESSRAD BETALNINGSANSVARIG            
007800        05 MID-ADBETRAD-3    PIC X(35).                                   
007900*                                 ADRESSRAD BETALNINGSANSVARIG            
008000     03 MID-FLBORT           PIC X.                                       
008100*                                 BORTTAGNINGSFLAGGA                      
008200*** END OF VILMAII-COPY LENGTH= 529 BYTES                                 

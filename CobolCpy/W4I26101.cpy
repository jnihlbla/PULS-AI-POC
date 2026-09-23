000100 01  MID-W4I26101.                                                        
000200*                                 MID-COPYTEXT FÖR W4I26101               
000300     03 MID-IDDISTR          PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR         PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDORDNR          PIC X(7).                                    
000800*                                 ORDERNUMMER                             
000900     03 MID-KDORDKL          PIC X.                                       
001000*                                 ORDERKLASS                              
001100     03 MID-KDFRAKT          PIC X(2).                                    
001200*                                 FRAKTSÄTT DC TILL KUND                  
001300     03 MID-KDPROTYP         PIC X.                                       
001400*                                 TYP AV PROFORMA                         
001500     03 MID-KDFAKTYP         PIC X.                                       
001600*                                 FAKTURATYP                              
001700     03 MID-BEKUNDRF         PIC X(15).                                   
001800*                                 KUNDENS REFERENS                        
001900     03 MID-IDFTG            PIC X(2).                                    
002000*                                 FÖRETAGSID EKONOM REDOVISNING           
002100     03 MID-REOMRTAL         PIC X(5).                                    
002200*                                 OMRÄKNINGSTAL                           
002300     03 MID-BEVARREF         PIC X(10).                                   
002400*                                 VÅR REFERENS                            
002500     03 MID-IDKONTO          PIC X(10).                                   
002600*                                 KONTO                                   
002700     03 MID-IDANALYS         PIC X(12).                                   
002800*                                 ANALYSNUMMER                            
002900     03 MID-IDSKYLT          PIC X(3).                                    
003000      88 MID-GODK-IDSKYLT    VALUE 'CZ '                                  
003100                             'D  '                                        
003200                             'DK '                                        
003300                             'E  '                                        
003400                             'FB '                                        
003500                             'GB '                                        
003600                             'GR '                                        
003700                             'H  '                                        
003800                             'I  '                                        
003900                             'IR '                                        
004000                             'J  '                                        
004100                             'KOR'                                        
004200                             'MAL'                                        
004300                             'NL '                                        
004400                             'P  '                                        
004500                             'PL '                                        
004600                             'RC '                                        
004700                             'RCN'                                        
004800                             'RO '                                        
004900                             'RUS'                                        
005000                             'S  '                                        
005100                             'SF '                                        
005200                             'T  '                                        
005300                             'TR '                                        
005400                             'USA'                                        
005500                             'YU '.                                       
005600*                                 NATIONALITETSTECKEN                     
005700*                                 SPRÅKIDENTIFIKATION                     
005800     03 MID-FORFDAT          PIC X(6).                                    
005900     03 MID-IDKST            PIC X(10).                                   
006000*                                 KOSTNADSSTÄLLE                          
006100     03 MID-BEGMT.                                                        
006200*                                 GODSMOTTAGARNAMN                        
006300        05 MID-BEGMT-RAD1    PIC X(35).                                   
006400*                                 GODSMOTTAGARNAMN RAD 1                  
006500        05 MID-BEGMT-RAD2    PIC X(35).                                   
006600*                                 GODSMOTTAGARNAMN RAD 2                  
006700     03 MID-ADGMT.                                                        
006800*                                 GODSMOTTAGARADRESS                      
006900        05 MID-ADGMT-GATA    PIC X(35).                                   
007000*                                 GODSMOTTAGARADRESS GATA                 
007100        05 MID-ADGMT-PADR    PIC X(35).                                   
007200*                                 GODSMOTTAGARADRESS POSTADRESS           
007300        05 MID-ADPOST-PNRORT REDEFINES MID-ADGMT-PADR.                    
007400*                                 POSTNUMMER + ORT                        
007500           07 MID-ADPOSTNR   PIC X(10).                                   
007600*                                 POSTNUMMER I ADRESS                     
007700           07 MID-ADCITY     PIC X(25).                                   
007800*                                 BENÄMNING PÅ STAD                       
007900        05 MID-ADPOST-ORTPNR REDEFINES MID-ADGMT-PADR.                    
008000*                                 ORT + POSTNUMMER                        
008100           07 MID-ADCITY     PIC X(25).                                   
008200*                                 BENÄMNING PÅ STAD                       
008300           07 MID-ADPOSTNR   PIC X(10).                                   
008400*                                 POSTNUMMER I ADRESS                     
008500        05 MID-ADGMT-LAND    PIC X(35).                                   
008600*                                 GODSMOTTAGARADRESS LAND                 
008700     03 MID-BEBET.                                                        
008800*                                 BETALNINGSANSVARIG NAMN                 
008900        05 MID-BEBETRAD-1    PIC X(35).                                   
009000*                                 DEL AV BETALNINGSANSVARIGS NAMN         
009100        05 MID-BEBETRAD-2    PIC X(35).                                   
009200*                                 DEL AV BETALNINGSANSVARIGS NAMN         
009300     03 MID-ADBET.                                                        
009400*                                 BETALNINGSANSVARIG ADRESS               
009500        05 MID-ADBETRAD-1    PIC X(35).                                   
009600*                                 ADRESSRAD BETALNINGSANSVARIG            
009700        05 MID-ADBETRAD-2    PIC X(35).                                   
009800*                                 ADRESSRAD BETALNINGSANSVARIG            
009900        05 MID-ADBETRAD-3    PIC X(35).                                   
010000*                                 ADRESSRAD BETALNINGSANSVARIG            
010100*** END OF VILMAII-COPY LENGTH= 445 BYTES                                 

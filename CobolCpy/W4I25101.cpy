000100 01  MID-W4I25101.                                                        
000200*                                 MID-COPYTEXT FÖR W4I25101               
000300*                                 ORDERHUVUD                              
000400*                                                                         
000500     03 MID-IDSYSTEM         PIC X(4).                                    
000600*                                 VOLVO VCCS SYSTEMNUMMER                 
000700     03 MID-IDDISTR          PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MID-IDKUNDNR         PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-IDORDNR          PIC X(7).                                    
001200*                                 ORDERNUMMER                             
001300     03 MID-KDORDKL          PIC X.                                       
001400*                                 ORDERKLASS                              
001500     03 MID-KDFRAKT          PIC X(2).                                    
001600*                                 FRAKTSÄTT DC TILL KUND                  
001700     03 MID-IDDC             PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-TIRFS            PIC X(6).                                    
002000     03 MID-BEKUNDRF         PIC X(15).                                   
002100*                                 KUNDENS REFERENS                        
002200     03 MID-KDFAKTYP         PIC X.                                       
002300*                                 FAKTURATYP                              
002400     03 MID-FLRESTN          PIC X.                                       
002500*                                 RESTNOTERING ?                          
002600     03 MID-KDTPOTYP         PIC X.                                       
002700*                                 TYP AV TIDPLANERAD ORDER                
002800     03 MID-TITPO            PIC X(6).                                    
002900*                                 PLANERAD ORDERDATUM                     
003000     03 MID-BELAGINS         PIC X(60).                                   
003100*                                 DEL AV LAGERINSTRUKTION                 
003200     03 MID-BEGMT.                                                        
003300*                                 GODSMOTTAGARNAMN                        
003400        05 MID-BEGMT-RAD1    PIC X(35).                                   
003500*                                 GODSMOTTAGARNAMN RAD 1                  
003600        05 MID-BEGMT-RAD2    PIC X(35).                                   
003700*                                 GODSMOTTAGARNAMN RAD 2                  
003800     03 MID-ADGMT-GATA       PIC X(35).                                   
003900*                                 GODSMOTTAGARADRESS GATA                 
004000     03 MID-ADGMT-PADR       PIC X(35).                                   
004100*                                 GODSMOTTAGARADRESS POSTADRESS           
004200     03 MID-BEBET.                                                        
004300*                                 BETALNINGSANSVARIG NAMN                 
004400        05 MID-BEBETRAD-1    PIC X(35).                                   
004500*                                 DEL AV BETALNINGSANSVARIGS NAMN         
004600        05 MID-BEBETRAD-2    PIC X(35).                                   
004700*                                 DEL AV BETALNINGSANSVARIGS NAMN         
004800        05 MID-BETELNR-FILLER REDEFINES MID-BEBETRAD-2.                   
004900           07 MID-BETELNR    PIC X(20).                                   
005000*                                 TELEFONNUMMER                           
005100           07 FILLER         PIC X(15).                                   
005200     03 MID-ADBET.                                                        
005300*                                 BETALNINGSANSVARIG ADRESS               
005400        05 MID-ADBETRAD-1    PIC X(35).                                   
005500*                                 ADRESSRAD BETALNINGSANSVARIG            
005600        05 MID-ADBETRAD-2    PIC X(35).                                   
005700*                                 ADRESSRAD BETALNINGSANSVARIG            
005800     03 MID-IDMAIL-FILLER REDEFINES MID-ADBET.                            
005900        05 MID-IDMAIL        PIC X(60).                                   
006000*                                 MAIL ADRESS                             
006100        05 FILLER            PIC X(10).                                   
006200     03 MID-KDROPACK         PIC X.                                       
006300*                                 FRISLÄPPNINGSKOD RO/DO                  
006400     03 MID-IDKONTO          PIC X(10).                                   
006500*                                 KONTO                                   
006600     03 MID-IDANALYS         PIC X(12).                                   
006700*                                 ANALYSNUMMER                            
006800     03 MID-IDKST            PIC X(10).                                   
006900*                                 KOSTNADSSTÄLLE                          
007000     03 MID-IDSKYLT          PIC X(3).                                    
007100      88 MID-GODK-IDSKYLT    VALUE 'CZ '                                  
007200                             'D  '                                        
007300                             'DK '                                        
007400                             'E  '                                        
007500                             'FB '                                        
007600                             'GB '                                        
007700                             'GR '                                        
007800                             'H  '                                        
007900                             'I  '                                        
008000                             'IR '                                        
008100                             'J  '                                        
008200                             'KOR'                                        
008300                             'MAL'                                        
008400                             'NL '                                        
008500                             'P  '                                        
008600                             'PL '                                        
008700                             'RC '                                        
008800                             'RCN'                                        
008900                             'RO '                                        
009000                             'RUS'                                        
009100                             'S  '                                        
009200                             'SF '                                        
009300                             'T  '                                        
009400                             'TR '                                        
009500                             'USA'                                        
009600                             'YU '.                                       
009700*                                 NATIONALITETSTECKEN                     
009800*                                 SPRÅKIDENTIFIKATION                     
009900     03 MID-BEVARREF         PIC X(10).                                   
010000*                                 VÅR REFERENS                            
010100     03 MID-KDTULLVE         PIC X.                                       
010200*                                 TYP AV PRIS PÅ TULLFAKTURA              
010300     03 MID-KDNOTES          PIC X(2).                                    
010400*                                 NOTERINGSKOD                            
010500     03 MID-FLAUTFAK         PIC X.                                       
010600*                                 AUTOMATFAKTURERING ?                    
010700     03 MID-FLAUTPAC         PIC X.                                       
010800*                                 AUTOMATISK PACKRAPPORTERING             
010900     03 MID-FLEMBORD         PIC X.                                       
011000*                                 EMBALLAGEORDER ?                        
011100     03 MID-FLOVRLEV         PIC X.                                       
011200*                                 ÖVERLEVERANS                            
011300     03 MID-FLFORBI          PIC X.                                       
011400*                                 FÖRBIORDERFLAGGA                        
011500     03 MID-IDKAMPRF         PIC X(7).                                    
011600*                                 KAMPANJREFERENS                         
011700     03 MID-IDFTG            PIC X(2).                                    
011800*                                 FÖRETAGSID EKONOM REDOVISNING           
011900     03 MID-FLLSBOK          PIC X.                                       
012000*                                 LAGERAVBOKNING                          
012100     03 MID-IDDEPT           PIC 9(2).                                    
012200*                                 AVDELNING I VERKSTAD                    
012300     03 MID-KDORDTYP-LDC     PIC X(2).                                    
012400*                                 ORDERTYP HOS DEALER                     
012500     03 MID-TIREPDAT         PIC 9(6).                                    
012600*                                 REPAIR DATE                             
012700     03 MID-FLORDTIL         PIC X.                                       
012800*                                 TVINGANDE TILÄGG ORDER                  
012900     03 MID-FILLERX2         PIC X(2).                                    
013000     03 MID-IDGROSS          PIC X(3).                                    
013100*                                 GROSSIST KUNDNUMMER FRÅN TACDIS         
013200     03 MID-IDBILREG         PIC X(10).                                   
013300*                                 BILENS REGISTRERINGSNUMMER              
013400     03 MID-IDVIN            PIC X(17).                                   
013500*                                 VIN ID FORDON                           
013600     03 MID-IDCISNR          PIC X(12).                                   
013700*                                 CIS NUMMER                              
013800*** END OF VILMAII-COPY LENGTH= 515 BYTES                                 

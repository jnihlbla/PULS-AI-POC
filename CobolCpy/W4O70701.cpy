000100 01  MOD-W4O70701.                                                        
000200*                                 MOD-COPYTEXT FOR PGM W4070700           
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDPARTNR-UT      PIC X(9).                                    
001600*                                 FINANCIELL KUND                         
001700     03 MOD-IDFTG-UT         PIC X(2).                                    
001800*                                 FÖRETAGSID EKONOM REDOVISNING           
001900     03 MOD-RADER            OCCURS 4 TIMES.                              
002000*                                 RADINFORMATION                          
002100        05 MOD-IDKUNDNR      PIC X(6).                                    
002200*                                 KUNDNUMMER                              
002300        05 MOD-KDANMORS      PIC X(2).                                    
002400*                                 ORSAK TILL LEVERANSANMÄRKNING           
002500        05 MOD-FLINVFEE      PIC X.                                       
002600*                                 INVOICE FLAG                            
002700        05 MOD-PRARTNTO      PIC Z(6)9.9(2).                              
002800*                                 ARTIKELPRIS NETTO                       
002900        05 MOD-FLINVLDC      PIC X.                                       
003000*                                 INVOICE FLAG LDC                        
003100        05 MOD-PRARTNTO-LDC  PIC Z(6)9.9(2).                              
003200*                                 ARTIKELPRIS NETTO                       
003300        05 MOD-IDUSER        PIC X(8).                                    
003400*                                 ANVÄNDARENS SÄKERHETS ID                
003500        05 MOD-DAUPPDAT      PIC 9(8).                                    
003600*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
003700     03 MOD-INDATA.                                                       
003800*                                 INDATA BILD 4707                        
003900        05 MOD-KDBEHX-ATTR   PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100        05 MOD-KDBEHX-UPD    PIC X.                                       
004200*                                 BEHANDLINGSKOD-X                        
004300        05 MOD-KDANMORS-ATTR PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-KDANMORS-UPD  PIC X(2).                                    
004600*                                 ORSAK TILL LEVERANSANMÄRKNING           
004700        05 MOD-FLINVFEE-ATTR PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-FLINVFEE-UPD  PIC X.                                       
005000*                                 INVOICE FLAG                            
005100        05 MOD-PRARTNTO-ATTR PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 MOD-PRARTNTO-UPD  PIC Z(6)9.9(2).                              
005400*                                 ARTIKELPRIS NETTO                       
005500        05 MOD-FLINVLDC-ATTR PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-FLINVLDC-UPD  PIC X.                                       
005800*                                 INVOICE FLAG LDC                        
005900        05 MOD-PRARTNTO-LDC-ATTR                                          
006000                             PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200        05 MOD-PRARTNTO-LDC-UPD                                           
006300                             PIC Z(6)9.9(2).                              
006400*                                 ARTIKELPRIS NETTO LDC                   
006500     03 MOD-RADER-2          OCCURS 4 TIMES.                              
006600*                                 RADINFORMATION                          
006700        05 MOD-IDKUNDNR-GRET PIC X(6).                                    
006800*                                 KUNDNUMMER                              
006900        05 MOD-KDANMORS-GRET PIC X(2).                                    
007000*                                 ORSAK TILL LEVERANSANMÄRKNING           
007100        05 MOD-SUARTBTO-GRET PIC Z(6)9.9(2).                              
007200*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
007300        05 MOD-FLINVLDC-GRET PIC X.                                       
007400*                                 INVOICE FLAG LDC                        
007500        05 MOD-SUARTBTO-LDC-GRET                                          
007600                             PIC Z(6)9.9(2).                              
007700*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
007800        05 MOD-IDUSER-GRET   PIC X(8).                                    
007900*                                 ANVÄNDARENS SÄKERHETS ID                
008000        05 MOD-DAUPPDAT-GRET PIC 9(8).                                    
008100*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
008200     03 MOD-INDATA-2.                                                     
008300*                                 INDATA BILD 4707                        
008400        05 MOD-KDBEHX-2-ATTR PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-KDBEHX-GRET-UPD                                            
008700                             PIC X.                                       
008800*                                 BEHANDLINGSKOD-X                        
008900        05 MOD-KDANMORS-2-ATTR                                            
009000                             PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200        05 MOD-KDANMORS-GRET-UPD                                          
009300                             PIC X(2).                                    
009400*                                 MFS BEHANDLING AV INPUTFÄLT             
009500        05 MOD-SUARTBTO-2-ATTR                                            
009600                             PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800        05 MOD-SUARTBTO-GRET-UPD                                          
009900                             PIC Z(6)9.9(2).                              
010000*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
010100        05 MOD-FLINVLDC-2-ATTR                                            
010200                             PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400        05 MOD-FLINVLDC-GRET-UPD                                          
010500                             PIC X.                                       
010600*                                 INVOICE FLAG LDC                        
010700        05 MOD-SUARTBTO-LDC-2-ATTR                                        
010800                             PIC X(2).                                    
010900*                                 MFS ATTRIBUTFÄLT                        
011000        05 MOD-SUARTBTO-LDC-GRET-UPD                                      
011100                             PIC Z(6)9.9(2).                              
011200*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
011300     03 MOD-TEMFSINF         PIC X(55).                                   
011400*                                 INFORMATIONSMEDDELANDE                  
011500*** END OF VILMAII-COPY LENGTH= 565 BYTES                                 

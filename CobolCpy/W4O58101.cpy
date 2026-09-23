000100 01  MOD-W4O58101.                                                        
000200*                                 MOD-COPYTEXT TILL W4058100              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDTRP-IN.                                                     
000800*                                 TRANSPORTIDENTITET                      
000900        05 MOD-IDTRPLOS      PIC X(3).                                    
001000*                                 TRANSPORTLÖSNING                        
001100        05 MOD-IDTRPVAR      PIC X(2).                                    
001200*                                 TRANSPORTLÖSNINGSGRUPP                  
001300     03 MOD-IDTRP-UT.                                                     
001400*                                 TRANSPORTIDENTITET                      
001500        05 MOD-IDTRPLOS      PIC X(3).                                    
001600*                                 TRANSPORTLÖSNING                        
001700        05 MOD-IDTRPVAR      PIC X(2).                                    
001800*                                 TRANSPORTLÖSNINGSGRUPP                  
001900     03 MOD-BETRPDST-IN      PIC X(15).                                   
002000*                                 TRANSPORTDESTINATION                    
002100     03 MOD-BETRPDST-UT      PIC X(15).                                   
002200*                                 TRANSPORTDESTINATION                    
002300     03 MOD-IDDC-IN          PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MOD-IDDC-UT          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MOD-TITRPAVG-NEXT    PIC X(7).                                    
002800*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
002900     03 MOD-TITRPAVG-ENTER   PIC X(7).                                    
003000*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
003100     03 MOD-KVLASTTI-IN-ATTR PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-KVLASTTI-IN      PIC X(2).                                    
003400*                                 MFS BEHANDLING AV INPUTFÄLT             
003500     03 MOD-KVLASTTI-UT      PIC Z9.9(2).                                 
003600*                                 TID DET TAR ATT LASTA                   
003700     03 MOD-KVADMFL-IN-ATTR  PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-KVADMFL-IN       PIC X(2).                                    
004000*                                 MFS BEHANDLING AV INPUTFÄLT             
004100     03 MOD-KVADMFL-UT       PIC Z9.9(2).                                 
004200*                                 ADM-TID FÖRE LASTNING                   
004300     03 MOD-KVADMEL-IN-ATTR  PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-KVADMEL-IN       PIC X(2).                                    
004600*                                 MFS BEHANDLING AV INPUTFÄLT             
004700     03 MOD-KVADMEL-UT       PIC Z9.9(2).                                 
004800*                                 ADM-TID EFTER LASTNING                  
004900     03 MOD-BETRPDST-ATTR    PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-BETRPDST         PIC X(15).                                   
005200*                                 TRANSPORTDESTINATION                    
005300     03 MOD-FLTABORT-TRP-UT-ATTR                                          
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-FLTABORT-TRP-UT  PIC X.                                       
005700*                                 BORTTAGSFLAGGA                          
005800     03 MOD-TITRPAVG-UT-ATTR PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-TITRPAVG-UT      PIC Z(2)BZBZ9B9(2).                          
006100*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
006200     03 MOD-BETRPFIR-UT-ATTR PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-BETRPFIR-UT      PIC X(15).                                   
006500*                                 TRANSPORTFIRMANS NAMN                   
006600     03 MOD-KDFARLIG-UT-ATTR PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-KDFARLIG-UT      PIC X.                                       
006900*                                 KOD FÖR FARLIGT GODS                    
007000     03 MOD-VLTRPMIN-UT-ATTR PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-VLTRPMIN-UT      PIC Z(3).                                    
007300*                                 MINSTA TILLÅTNA VOLYM I M3              
007400     03 MOD-TRPAVG-RAD       OCCURS 10 TIMES.                             
007500        05 MOD-TITRPAVG-RAD  PIC Z(2)BZBZ9B9(2).                          
007600*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
007700        05 MOD-BETRPFIR-RAD  PIC X(15).                                   
007800*                                 TRANSPORTFIRMANS NAMN                   
007900        05 MOD-KDFARLIG-RAD  PIC X.                                       
008000*                                 KOD FÖR FARLIGT GODS                    
008100        05 MOD-VLTRPMIN-RAD  PIC Z(3).                                    
008200*                                 MINSTA TILLÅTNA VOLYM I M3              
008300     03 MOD-TITRPAVG-IN-ATTR PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 MOD-TITRPAVG-IN      PIC X(2).                                    
008600*                                 MFS BEHANDLING AV INPUTFÄLT             
008700     03 MOD-BETRPFIR-IN-ATTR PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 MOD-BETRPFIR-IN      PIC X(2).                                    
009000*                                 MFS BEHANDLING AV INPUTFÄLT             
009100     03 MOD-KDFARLIG-IN-ATTR PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300     03 MOD-KDFARLIG-IN      PIC 9.                                       
009400*                                 KOD FÖR FARLIGT GODS                    
009500     03 MOD-VLTRPMIN-IN-ATTR PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700     03 MOD-VLTRPMIN-IN      PIC X(2).                                    
009800*                                 MFS BEHANDLING AV INPUTFÄLT             
009900     03 MOD-FLTABORT-AVG-UT-ATTR                                          
010000                             PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200     03 MOD-FLTABORT-AVG-UT  PIC X.                                       
010300*                                 BORTTAGSFLAGGA                          
010400     03 MOD-TEMFSINF         PIC X(55).                                   
010500*                                 INFORMATIONSMEDDELANDE                  
010600*** END OF VILMAII-COPY LENGTH= 549 BYTES                                 

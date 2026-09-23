000100 01  MOD-W4O70801.                                                        
000200*                                 MOD-COPYTEXT FOR PGM W4070800           
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDANSTNR-IN      PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDANSTNR-UT      PIC X(5).                                    
001400*                                 ANSTÄLLNINGSNUMMER                      
001500     03 MOD-RADER            OCCURS 10 TIMES.                             
001600*                                 RADINFORMATION                          
001700        05 MOD-KDCMD-ATTR    PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900        05 MOD-KDCMD-UPD     PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100        05 MOD-IDDISTR-FOM   PIC Z(3)9.                                   
002200*                                 LÄGSTA DISTRIKTNR I INTERVALL           
002300        05 MOD-IDDISTR-TOM   PIC Z(3)9.                                   
002400*                                 HÖGSTA DISTRIKTNR I INTERVALL           
002500        05 MOD-SUKRENOT-FOM  PIC Z(5)9.                                   
002600*                                 KREDITNOTASUMMA FOM                     
002700        05 MOD-SUKRENOT-TOM  PIC Z(5)9.                                   
002800*                                 KREDITNOTASUMMA TOM                     
002900        05 MOD-IDANSTNR-ADM  PIC Z(4)9.                                   
003000*                                 ANSTÄLLNINGSNUMMER                      
003100        05 MOD-BEANST        PIC X(25).                                   
003200*                                 ANSTÄLLDS NAMN                          
003300        05 MOD-FLKREPRT      PIC X.                                       
003400*                                 UTSKRIFTSFLAGGA KREDITNOTA              
003500        05 MOD-IDANSTNR-ATTUPD                                            
003600                             PIC Z(4)9.                                   
003700*                                 ANSTÄLLNINGSNUMMER                      
003800        05 MOD-TIUPPDAT      PIC 9(6).                                    
003900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
004000     03 MOD-INDATA.                                                       
004100*                                 INDATA BILD 4708                        
004200        05 MOD-KDCMD-RAD-19-ATTR                                          
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-KDCMD-RAD-19-UPD                                           
004600                             PIC X(2).                                    
004700*                                 MFS BEHANDLING AV INPUTFÄLT             
004800        05 MOD-IDDISTR-FOM-ATTR                                           
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-IDDISTR-FOM-UPD                                            
005200                             PIC X(2).                                    
005300*                                 MFS BEHANDLING AV INPUTFÄLT             
005400        05 MOD-IDDISTR-TOM-ATTR                                           
005500                             PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-IDDISTR-TOM-UPD                                            
005800                             PIC X(2).                                    
005900*                                 MFS BEHANDLING AV INPUTFÄLT             
006000        05 MOD-SUKRENOT-FOM-ATTR                                          
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-SUKRENOT-FOM-UPD                                           
006400                             PIC X(2).                                    
006500*                                 MFS BEHANDLING AV INPUTFÄLT             
006600        05 MOD-SUKRENOT-TOM-ATTR                                          
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-SUKRENOT-TOM-UPD                                           
007000                             PIC X(2).                                    
007100*                                 MFS BEHANDLING AV INPUTFÄLT             
007200        05 MOD-IDANSTNR-ADM-ATTR                                          
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 MOD-IDANSTNR-ADM-UPD                                           
007600                             PIC X(2).                                    
007700*                                 MFS BEHANDLING AV INPUTFÄLT             
007800        05 MOD-BEANST-ATTR   PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000        05 MOD-BEANST-UPD    PIC X(2).                                    
008100*                                 MFS BEHANDLING AV INPUTFÄLT             
008200        05 MOD-FLKREPRT-ATTR PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-FLKREPRT-UPD  PIC X(2).                                    
008500*                                 MFS BEHANDLING AV INPUTFÄLT             
008600     03 MOD-TEMFSINF         PIC X(55).                                   
008700*                                 INFORMATIONSMEDDELANDE                  
008800*** END OF VILMAII-COPY LENGTH= 804 BYTES                                 

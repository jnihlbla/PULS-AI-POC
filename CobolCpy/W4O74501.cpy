000100 01  MOD-W4O74501.                                                        
000200*                                 MOD-COPYTEXT FOR PGM W4074500           
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDRAPP-IN        PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDRAPP-UT        PIC X(10).                                   
001800*                                 RAPPORT ID                              
001900     03 MOD-IDKOLLI-IN       PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002200*                                 KOLLINUMMER                             
002300     03 MOD-IDDC-IN          PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 MOD-IDDC-UT          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MOD-IDAVS            PIC X(20).                                   
002800*                                 IDENTITET PÅ DEN PERSON SOM             
002900*                                 SKICKAT IVÄG GODS                       
003000     03 MOD-DAREGDAT         PIC 9(8).                                    
003100*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
003200     03 MOD-DARFSDAT         PIC 9(8).                                    
003300*                                 KLART FÖR TRANSPORT ÅÅÅÅMMDD            
003400     03 MOD-IDKOLLI          PIC Z(5).                                    
003500*                                 KOLLINUMMER                             
003600     03 MOD-KDKOLLI          PIC X(8).                                    
003700*                                 KOLLIKOD                                
003800     03 MOD-VKORDBTO-KOLLI   PIC Z(6).Z.                                  
003900*                                 ORDERVIKT BRUTTO PER KOLLI              
004000     03 MOD-INDATA.                                                       
004100*                                 INDATA BILD 4745                        
004200        05 MOD-IDAVS-ATTR    PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-IDAVS-UPD     PIC X(2).                                    
004500*                                 MFS BEHANDLING AV INPUTFÄLT             
004600        05 MOD-FLTABORT-ATTR PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800        05 MOD-FLTABORT      PIC X(2).                                    
004900*                                 MFS BEHANDLING AV INPUTFÄLT             
005000        05 MOD-IDKOLLI-ATTR  PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-IDKOLLI-UPD   PIC X(2).                                    
005300*                                 MFS BEHANDLING AV INPUTFÄLT             
005400        05 MOD-KDKOLLI-ATTR  PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-KDKOLLI-UPD   PIC X(2).                                    
005700*                                 MFS BEHANDLING AV INPUTFÄLT             
005800        05 MOD-VKORDBTO-KOLLI-ATTR                                        
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-VKORDBTO-KOLLI-UPD                                         
006200                             PIC X(2).                                    
006300*                                 MFS BEHANDLING AV INPUTFÄLT             
006400        05 MOD-FLKLAR-ATTR   PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-FLKLAR        PIC X(2).                                    
006700*                                 MFS BEHANDLING AV INPUTFÄLT             
006800        05 MOD-KDPRTVAL-ATTR PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-KDPRTVAL      PIC X(2).                                    
007100*                                 MFS BEHANDLING AV INPUTFÄLT             
007200     03 MOD-KDVALISO         PIC X(3).                                    
007300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007400     03 MOD-RADER            OCCURS 10 TIMES.                             
007500*                                 RADINFORMATION                          
007600        05 MOD-KDCMD-ATTR    PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-KDCMD         PIC X(2).                                    
007900*                                 MFS BEHANDLING AV INPUTFÄLT             
008000        05 MOD-IDKOLLI-RAD   PIC Z(4)9.                                   
008100*                                 KOLLINUMMER                             
008200        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-IDARTNR       PIC Z(7)9.                                   
008500*                                 ARTIKELNUMMER                           
008600        05 MOD-KVANTAL-ATTR  PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800        05 MOD-KVANTAL       PIC Z(5)9.                                   
008900*                                 ANTAL                                   
009000        05 MOD-PRARTBTO-ATTR PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200        05 MOD-PRARTBTO      PIC Z(6)9.9(2).                              
009300*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
009400        05 MOD-KDFEL-ATTR    PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600        05 MOD-KDFEL         PIC Z(3).                                    
009700*                                 FELKOD                                  
009800        05 MOD-TENOTE-ATTR   PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000        05 MOD-TENOTE        PIC X(40).                                   
010100*                                 NOTERINGSFÄLT                           
010200     03 MOD-TEMFSINF         PIC X(55).                                   
010300*                                 INFORMATIONSMEDDELANDE                  
010400*** END OF VILMAII-COPY LENGTH= 1084 BYTES                                

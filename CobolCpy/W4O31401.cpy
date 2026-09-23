000100 01  MOD-W4O31401.                                                        
000200*                                 MOD-COPYTEXT PGM W40314                 
000300*                                 KOLLIRAPPORTERING-1                     
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDANSTNR-IN      PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDANSTNR-UT      PIC X(5).                                    
001100*                                 ANSTÄLLNINGSNUMMER                      
001200     03 MOD-IDDISTR-IN       PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-IDDISTR-UT       PIC X(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001900*                                 KUNDNUMMER                              
002000     03 MOD-IDORDNR-IN       PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-IDORDNR-UT       PIC X(5).                                    
002300*                                 ORDERNUMMER UTGÅR PD90                  
002400     03 MOD-IDKOLLI-IN       PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002700*                                 KOLLINUMMER                             
002800     03 MOD-IDPRODNR-IN      PIC X(2).                                    
002900*                                 MFS BEHANDLING AV INPUTFÄLT             
003000     03 MOD-IDPRODNR-UT      PIC X(7).                                    
003100*                                 PRODUKTIONSNUMMER                       
003200     03 MOD-IDDC-IN          PIC X(2).                                    
003300*                                 MFS BEHANDLING AV INPUTFÄLT             
003400     03 MOD-IDDC-UT          PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600     03 MOD-IDTRANS-START    PIC X(4).                                    
003700*                                 BILDNUMMER                              
003800     03 MOD-FLSISTAK-ATTR    PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-FLSISTAK         PIC X(2).                                    
004100*                                 MFS BEHANDLING AV INPUTFÄLT             
004200     03 MOD-FLFORTSK-ATTR    PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-FLFORTSK         PIC X(2).                                    
004500*                                 MFS BEHANDLING AV INPUTFÄLT             
004600     03 MOD-IDRADNR-FOM-S    PIC Z(3)9.                                   
004700*                                 RADNUMMER                               
004800     03 MOD-IDRADNR-TOM-S    PIC Z(3)9.                                   
004900*                                 RADNUMMER                               
005000     03 MOD-KVLEVART-S       PIC Z(5)9.                                   
005100*                                 LEVERERAT ANTAL STYCK                   
005200     03 MOD-KDPRTVAL-ADRESSFL                                             
005300                             PIC X(2).                                    
005400*                                 PRINTER-VAL KOD                         
005500     03 MOD-KDPRTVAL-FOLJEFL-ATTR                                         
005600                             PIC X(2).                                    
005700*                                 MFS BEHANDLING AV INPUTFÄLT             
005800     03 MOD-KDPRTVAL-FOLJEFL PIC X(2).                                    
005900*                                 PRINTER-VAL KOD                         
006000     03 MOD-RAD              OCCURS 12 TIMES.                             
006100        05 MOD-IDRADNR-FOM-ATTR                                           
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-IDRADNR-FOM   PIC 9(4).                                    
006500*                                 RADNUMMER                               
006600        05 MOD-IDRADNR-TOM-ATTR                                           
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-IDRADNR-TOM   PIC 9(4).                                    
007000*                                 RADNUMMER                               
007100        05 MOD-KVLEVART-ATTR PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 MOD-KVLEVART      PIC 9(6).                                    
007400*                                 LEVERERAT ANTAL STYCK                   
007500     03 MOD-VKORDBTO-ATTR    PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 MOD-VKORDBTO-KOLLI   PIC Z(5)9.9.                                 
007800*                                 ORDERVIKT BRUTTO (KG)                   
007900     03 MOD-VKORDBTO-4315-ATTR                                            
008000                             PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 MOD-VKORDBTO-4315    PIC Z(5)9.9.                                 
008300*                                 ORDERVIKT BRUTTO (KG)                   
008400     03 MOD-TEMFSINF         PIC X(55).                                   
008500*                                 INFORMATIONSMEDDELANDE                  
008600*** END OF VILMAII-COPY LENGTH= 439 BYTES                                 

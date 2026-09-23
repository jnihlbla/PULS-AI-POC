000100 01  MOD-W4O31501.                                                        
000200*                                 KORT BESKRIVING                         
000300*                                 MAX 31 POS / RAD                        
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
003800     03 MOD-KDKOLLI-ATTR     PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-KDKOLLI          PIC X(8).                                    
004100*                                 KOLLIKOD                                
004200     03 MOD-VKORDBTO-ATTR    PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-VKORDBTO-KOLLI   PIC X(2).                                    
004500*                                 MFS BEHANDLING AV INPUTFÄLT             
004600     03 MOD-FLSISTAK-ATTR    PIC X(2).                                    
004700*                                 MFS BEHANDLING AV INPUTFÄLT             
004800     03 MOD-FLSISTAK         PIC X.                                       
004900*                                 SISTA KOLLI I ORDERN?                   
005000     03 MOD-KDEMBTYP-ATTR    PIC X(2).                                    
005100*                                 MFS BEHANDLING AV INPUTFÄLT             
005200     03 MOD-KDEMBTYP         PIC 9.                                       
005300*                                 EMBALLAGETYP                            
005400     03 MOD-DIKOLLIL-ATTR    PIC X(2).                                    
005500*                                 MFS BEHANDLING AV INPUTFÄLT             
005600     03 MOD-DIKOLLIL         PIC X(4).                                    
005700*                                 KOLLI-LÄNGD                             
005800     03 MOD-DIKOLLIB-ATTR    PIC X(2).                                    
005900*                                 MFS BEHANDLING AV INPUTFÄLT             
006000     03 MOD-DIKOLLIB         PIC X(3).                                    
006100*                                 KOLLI-BREDD                             
006200     03 MOD-DIKOLLIH-ATTR    PIC X(2).                                    
006300*                                 MFS BEHANDLING AV INPUTFÄLT             
006400     03 MOD-DIKOLLIH         PIC X(3).                                    
006500*                                 KOLLI-HÖJD                              
006600     03 MOD-ADFLGEO-ATTR     PIC X(2).                                    
006700*                                 MFS BEHANDLING AV INPUTFÄLT             
006800     03 MOD-ADFLGEO          PIC X(3).                                    
006900*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
007000     03 MOD-ADFLOMR-ATTR     PIC X(2).                                    
007100*                                 MFS BEHANDLING AV INPUTFÄLT             
007200     03 MOD-ADFLOMR          PIC X(3).                                    
007300*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
007400     03 MOD-ADRUTNIV-ATTR    PIC X(2).                                    
007500*                                 MFS BEHANDLING AV INPUTFÄLT             
007600     03 MOD-ADRUTNIV         PIC X(3).                                    
007700*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
007800     03 MOD-IDKOLLI-FOM-ATTR PIC X(2).                                    
007900*                                 MFS BEHANDLING AV INPUTFÄLT             
008000     03 MOD-IDKOLLI-FOM      PIC X(5).                                    
008100*                                 KOLLINUMMER                             
008200     03 MOD-IDKOLLI-TOM-ATTR PIC X(2).                                    
008300*                                 MFS BEHANDLING AV INPUTFÄLT             
008400     03 MOD-IDKOLLI-TOM      PIC X(5).                                    
008500*                                 KOLLINUMMER                             
008600     03 MOD-PRTVAL-ADRESSFL-ATTR                                          
008700                             PIC X(2).                                    
008800*                                 MFS BEHANDLING AV INPUTFÄLT             
008900     03 MOD-PRTVAL-ADRESSFL  PIC X(2).                                    
009000*                                 PRINTER-VAL KOD ADRESS FLAGGA           
009100     03 MOD-PRTVAL-FOLJEFL-ATTR                                           
009200                             PIC X(2).                                    
009300*                                 MFS BEHANDLING AV INPUTFÄLT             
009400     03 MOD-PRTVAL-FOLJEFL   PIC X(2).                                    
009500*                                 PRINTER-VAL KOD FÖLJESEDEL              
009600     03 MOD-RAD              OCCURS 12 TIMES.                             
009700        05 MOD-IDRADNR-FOM-ATTR                                           
009800                             PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000        05 MOD-IDRADNR-FOM   PIC X(2).                                    
010100*                                 MFS BEHANDLING AV INPUTFÄLT             
010200        05 MOD-IDRADNR-TOM-ATTR                                           
010300                             PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500        05 MOD-IDRADNR-TOM   PIC X(2).                                    
010600*                                 MFS BEHANDLING AV INPUTFÄLT             
010700        05 MOD-KVLEVART-ATTR PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900        05 MOD-KVLEVART      PIC X(2).                                    
011000*                                 MFS BEHANDLING AV INPUTFÄLT             
011100     03 MOD-TEMFSINF         PIC X(55).                                   
011200*                                 INFORMATIONSMEDDELANDE                  
011300*** END OF VILMAII-COPY LENGTH= 368 BYTES                                 

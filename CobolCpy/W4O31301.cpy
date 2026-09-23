000100 01  MOD-W4O31301.                                                        
000200*                                 COPYTEXT FÖR MOD W4O31301               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDANSTNR-ATTR    PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-IDANSTNR         PIC X(2).                                    
001400*                                 MFS BEHANDLING AV INPUTFÄLT             
001500     03 MOD-KDPRTVAL-ADR-ATTR                                             
001600                             PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 MOD-KDPRTVAL-ADR     PIC X(2).                                    
001900*                                 PRINTER-VAL KOD ADRESS FLAGGA           
002000     03 MOD-KDPRTVAL-FS-ATTR PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-KDPRTVAL-FS      PIC X(2).                                    
002300*                                 PRINTER-VAL KOD FÖLJESEDEL              
002400     03 MOD-RAD              OCCURS 13 TIMES.                             
002500*                                 COPYTEXT FÖR MOD W4O31301               
002600        05 MOD-IDDISTR-ATTR  PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800        05 MOD-IDDISTR       PIC X(2).                                    
002900*                                 MFS BEHANDLING AV INPUTFÄLT             
003000        05 MOD-IDPRODNR-ATTR PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 MOD-IDPRODNR      PIC X(2).                                    
003300*                                 MFS BEHANDLING AV INPUTFÄLT             
003400        05 MOD-IDPLKLST-ATTR PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-IDPLKLST      PIC X(2).                                    
003700*                                 MFS BEHANDLING AV INPUTFÄLT             
003800        05 MOD-IDKOLLI-ATTR  PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-IDKOLLI       PIC X(2).                                    
004100*                                 MFS BEHANDLING AV INPUTFÄLT             
004200        05 MOD-KDKOLLI-ATTR  PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-KDKOLLI       PIC X(8).                                    
004500*                                 KOLLIKOD                                
004600        05 MOD-VKORDBTO-KOLLI-ATTR                                        
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-VKORDBTO-KOLLI                                             
005000                             PIC Z(5)9.9.                                 
005100*                                 ORDERVIKT BRUTTO (KG)                   
005200        05 MOD-KDEMBTYP-ATTR PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-KDEMBTYP      PIC X(2).                                    
005500*                                 MFS BEHANDLING AV INPUTFÄLT             
005600        05 MOD-DIKOLLIL-ATTR PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-DIKOLLIL      PIC X(2).                                    
005900*                                 MFS BEHANDLING AV INPUTFÄLT             
006000        05 MOD-DIKOLLIB-ATTR PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200        05 MOD-DIKOLLIB      PIC X(2).                                    
006300*                                 MFS BEHANDLING AV INPUTFÄLT             
006400        05 MOD-DIKOLLIH-ATTR PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-DIKOLLIH      PIC X(2).                                    
006700*                                 MFS BEHANDLING AV INPUTFÄLT             
006800        05 MOD-KDPRTVAL-ADR-RAD-ATTR                                      
006900                             PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100        05 MOD-KDPRTVAL-ADR-RAD                                           
007200                             PIC X(2).                                    
007300*                                 MFS BEHANDLING AV INPUTFÄLT             
007400        05 MOD-KDPRTVAL-FS-RAD-ATTR                                       
007500                             PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700        05 MOD-KDPRTVAL-FS-RAD                                            
007800                             PIC X(2).                                    
007900*                                 MFS BEHANDLING AV INPUTFÄLT             
008000        05 MOD-FLAVSP        PIC X.                                       
008100*                                 ALLMÄN FELFLAGGA                        
008200     03 MOD-TEMFSINF         PIC X(55).                                   
008300*                                 INFORMATIONSMEDDELANDE                  
008400*** END OF VILMAII-COPY LENGTH= 908 BYTES                                 

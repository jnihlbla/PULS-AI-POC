000100 01  MOD-W4O30201-CTX.                                                    
000200*                                 COPYTEXT FÖR MOD W4O30201               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-PRTVAL-ADRESSFL-ATTR                                          
001200                             PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 MOD-PRTVAL-ADRESSFL  PIC X(2).                                    
001500*                                 PRINTER-VAL KOD                         
001600     03 MOD-W4O30201-001-GRP OCCURS 13 TIMES.                             
001700*                                 COPYTEXT FÖR MOD W0O70101               
001800        05 MOD-IDDISTR-ATTR  PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000        05 MOD-IDDISTR       PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200        05 MOD-IDPRODNR-ATTR PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400        05 MOD-IDPRODNR      PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600        05 MOD-FLAVVPACK-ATTR                                             
002700                             PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900        05 MOD-FLAVVPACK     PIC X(2).                                    
003000*                                 MFS BEHANDLING AV INPUTFÄLT             
003100        05 MOD-IDKOLLI-ATTR  PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-IDKOLLI       PIC X(2).                                    
003400*                                 MFS BEHANDLING AV INPUTFÄLT             
003500        05 MOD-KDKOLLI-ATTR  PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700        05 MOD-KDKOLLI       PIC X(2).                                    
003800*                                 MFS BEHANDLING AV INPUTFÄLT             
003900        05 MOD-VKORDBTO-KOLLI-ATTR                                        
004000                             PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200        05 MOD-VKORDBTO-KOLLI                                             
004300                             PIC X(2).                                    
004400*                                 MFS BEHANDLING AV INPUTFÄLT             
004500        05 MOD-ADRUTHYL-ATTR PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-ADRUTHYL      PIC X(2).                                    
004800*                                 MFS BEHANDLING AV INPUTFÄLT             
004900        05 MOD-ADFLGEO-ATTR  PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-ADFLGEO       PIC X(2).                                    
005200*                                 MFS BEHANDLING AV INPUTFÄLT             
005300        05 MOD-KDEMBTYP-ATTR PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500        05 MOD-KDEMBTYP      PIC X(2).                                    
005600*                                 MFS BEHANDLING AV INPUTFÄLT             
005700        05 MOD-DIKOLLIL-ATTR PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-DIKOLLIL      PIC X(2).                                    
006000*                                 MFS BEHANDLING AV INPUTFÄLT             
006100        05 MOD-DIKOLLIB-ATTR PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-DIKOLLIB      PIC X(2).                                    
006400*                                 MFS BEHANDLING AV INPUTFÄLT             
006500        05 MOD-DIKOLLIH-ATTR PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700        05 MOD-DIKOLLIH      PIC X(2).                                    
006800*                                 MFS BEHANDLING AV INPUTFÄLT             
006900     03 MOD-TEMFSINF         PIC X(55).                                   
007000*                                 INFORMATIONSMEDDELANDE                  
007100*** END OF VILMAII-COPY LENGTH= 731 BYTES                                 

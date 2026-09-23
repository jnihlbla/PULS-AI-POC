000100 01  MOD-W1O53301.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 1533              
000300*                                 GENERERINGSTABELL FÖR VADIS             
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDCATNR-FROM-ATTR                                             
000900                             PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100     03 MOD-IDCATNR-FROM-IN  PIC X(5).                                    
001200*                                 KATALOG-ID                              
001300     03 MOD-IDCATNR-FROM-UT  PIC X(5).                                    
001400*                                 KATALOG-ID                              
001500     03 MOD-IDCATNR-TO-ATTR  PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-IDCATNR-TO-IN    PIC X(5).                                    
001800*                                 KATALOG-ID                              
001900     03 MOD-IDCATNR-TO-UT    PIC X(5).                                    
002000*                                 KATALOG-ID                              
002100     03 MOD-FLKATVAD         PIC X.                                       
002200*                                 KATALOG TILL VADIS?                     
002300     03 MOD-KOL1             OCCURS 12 TIMES.                             
002400        05 MOD-KOL1-KDCATPUB-R                                            
002500                             PIC X(3).                                    
002600*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
002700        05 MOD-KOL1-FLVADGEN PIC X.                                       
002800*                                 AKTIVERA VADIS-GENERERING               
002900        05 MOD-KOL1-TIVADGEN-UPPD                                         
003000                             PIC X(6).                                    
003100*                                 VADIS UPPDATERAT (ÅÅMMDD)               
003200        05 MOD-KOL1-TIOMBRYT PIC X(6).                                    
003300*                                 OMBRYTNINGSDATUM                        
003400     03 MOD-KOL2             OCCURS 12 TIMES.                             
003500        05 MOD-KOL2-TIVADGEN-PLAN                                         
003600                             PIC X(6).                                    
003700*                                 PLANERAT KÖRDATUM (ÅÅMMDD)              
003800        05 MOD-KOL2-KDCATPUB-R                                            
003900                             PIC X(3).                                    
004000*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004100        05 MOD-KOL2-FLVADGEN-ATTR                                         
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-KOL2-FLVADGEN PIC X(2).                                    
004500*                                 MFS BEHANDLING AV INPUTFÄLT             
004600        05 MOD-KOL2-TIVADGEN-UPPD                                         
004700                             PIC X(6).                                    
004800*                                 VADIS UPPDATERAT (ÅÅMMDD)               
004900        05 MOD-KOL2-TIOMBRYT PIC X(6).                                    
005000*                                 OMBRYTNINGSDATUM                        
005100     03 MOD-KOL3             OCCURS 12 TIMES.                             
005200        05 MOD-KOL3-TIVADGEN-PLAN                                         
005300                             PIC X(6).                                    
005400*                                 PLANERAT KÖRDATUM (ÅÅMMDD)              
005500        05 MOD-KOL3-KDCATPUB-R                                            
005600                             PIC X(3).                                    
005700*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
005800        05 MOD-KOL3-FLVADGEN-ATTR                                         
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-KOL3-FLVADGEN PIC X(2).                                    
006200*                                 MFS BEHANDLING AV INPUTFÄLT             
006300     03 MOD-TEMFSINF         PIC X(55).                                   
006400*                                 INFORMATIONSMEDDELANDE                  
006500*** END OF VILMAII-COPY LENGTH= 772 BYTES                                 

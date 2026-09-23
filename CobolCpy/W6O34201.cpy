000100 01  MOD-W6O34201.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W6O34201                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDC             PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MOD-KDSTOR-IN        PIC X(3).                                    
001100*                                 STORAGE CODE                            
001200     03 MOD-KDSTOR-UT        PIC X(3).                                    
001300*                                 STORAGE CODE                            
001400     03 MOD-KDSTOR-SPAR      PIC X(3).                                    
001500*                                 STORAGE CODE                            
001600     03 MOD-KDSTOR-ATTR      PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 MOD-KDSTOR-MAIN      PIC X(2).                                    
001900*                                 MFS BEHANDLING AV INPUTFÄLT             
002000     03 MOD-DISTORD-ATTR     PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-DISTORD-MAIN     PIC X(2).                                    
002300*                                 MFS BEHANDLING AV INPUTFÄLT             
002400     03 MOD-DISTORB-ATTR     PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-DISTORB-MAIN     PIC X(2).                                    
002700*                                 MFS BEHANDLING AV INPUTFÄLT             
002800     03 MOD-DISTORH-ATTR     PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-DISTORH-MAIN     PIC X(2).                                    
003100*                                 MFS BEHANDLING AV INPUTFÄLT             
003200     03 MOD-TESTORAGE-ATTR   PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-TESTORAGE-MAIN   PIC X(2).                                    
003500*                                 MFS BEHANDLING AV INPUTFÄLT             
003600     03 MOD-KDVSOP1-ATTR     PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-KDVSOP1-MAIN     PIC X(2).                                    
003900*                                 MFS BEHANDLING AV INPUTFÄLT             
004000     03 MOD-KDVSOP2-ATTR     PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-KDVSOP2-MAIN     PIC X(2).                                    
004300*                                 MFS BEHANDLING AV INPUTFÄLT             
004400     03 MOD-KDVSOP3-ATTR     PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-KDVSOP3-MAIN     PIC X(2).                                    
004700*                                 MFS BEHANDLING AV INPUTFÄLT             
004800     03 MOD-KDVSOP4-ATTR     PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-KDVSOP4-MAIN     PIC X(2).                                    
005100*                                 MFS BEHANDLING AV INPUTFÄLT             
005200     03 MOD-KDVSOP5-ATTR     PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-KDVSOP5-MAIN     PIC X(2).                                    
005500*                                 MFS BEHANDLING AV INPUTFÄLT             
005600     03 MOD-KDANDR-ATTR      PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-KDANDR-MAIN      PIC X(2).                                    
005900*                                 MFS BEHANDLING AV INPUTFÄLT             
006000     03 MOD-INDATA           OCCURS 13 TIMES.                             
006100*                                 STORAGE INFORMATION                     
006200        05 MOD-KDSTOR-LINE   PIC X(3).                                    
006300*                                 STORAGE CODE                            
006400        05 MOD-DISTORD-LINE  PIC Z(2)9.9.                                 
006500*                                 STORAGE DEPTH                           
006600        05 MOD-DISTORB-LINE  PIC Z(2)9.9.                                 
006700*                                 LAGERPLATS BREDD                        
006800        05 MOD-DISTORH-LINE  PIC Z(2)9.9.                                 
006900*                                 STORAGE HEIGHT                          
007000        05 MOD-TESTORAGE-LINE                                             
007100                             PIC X(18).                                   
007200*                                 STORAGE INFORMATION                     
007300        05 MOD-KDVSOP1-LINE  PIC X(3).                                    
007400*                                 VSOP-KOD                                
007500        05 MOD-KDVSOP2-LINE  PIC X(3).                                    
007600*                                 VSOP-KOD                                
007700        05 MOD-KDVSOP3-LINE  PIC X(3).                                    
007800*                                 VSOP-KOD                                
007900        05 MOD-KDVSOP4-LINE  PIC X(3).                                    
008000*                                 VSOP-KOD                                
008100        05 MOD-KDVSOP5-LINE  PIC X(3).                                    
008200*                                 VSOP-KOD                                
008300     03 MOD-TEMFSINF         PIC X(55).                                   
008400*                                 INFORMATIONSMEDDELANDE                  
008500*** END OF VILMAII-COPY LENGTH= 817 BYTES                                 

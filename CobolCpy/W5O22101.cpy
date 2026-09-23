000100 01  MOD-W5O22101.                                                        
000200*                                 MOD-COPYTEXT FÖR W50221                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-KDEKHHT-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-KDEKHHT-UT       PIC X(3).                                    
001100*                                 EKONOMISK HUVUDHÄNDELSE                 
001200     03 MOD-KDEKSHT-IN       PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-KDEKSHT-UT       PIC X(3).                                    
001500*                                 EKONOMISK SUBHÄNDELSE                   
001600     03 MOD-KDEKNIVA-IN      PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-KDEKNIVA-UT      PIC X(5).                                    
001900*                                 EKONOMISK HÄNDELSENIVÅ                  
002000     03 MOD-IDSYSMOT-IN      PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-IDSYSMOT-UT      PIC X(6).                                    
002300*                                 PULS MOTTAGANDE SYSTEMNAMN              
002400     03 MOD-IDPTYP-IN        PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-IDPTYP-UT        PIC X(3).                                    
002700*                                 POSTTYP                                 
002800     03 MOD-IDFTG-UT         PIC 9(2).                                    
002900*                                 FÖRETAGSID EKONOM REDOVISNING           
003000     03 MOD-KDEKHHT          PIC X(3).                                    
003100*                                 EKONOMISK HUVUDHÄNDELSE                 
003200     03 MOD-BEEKHHT-ATTR     PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-BEEKHHT          PIC X(25).                                   
003500*                                 BESKR. EKONOMISK HUVUDHÄNDELSE          
003600     03 MOD-DELHHT-ATTR      PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-DELHHT           PIC X.                                       
003900     03 MOD-KDEKSHT          PIC X(3).                                    
004000*                                 EKONOMISK SUBHÄNDELSE                   
004100     03 MOD-BEEKSHT-ATTR     PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-BEEKSHT          PIC X(25).                                   
004400*                                 BESKR. EKONOMISK SUBHÄNDELSE            
004500     03 MOD-DELSHT-ATTR      PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-DELSHT           PIC X.                                       
004800     03 MOD-KDEKNIVA         PIC X(5).                                    
004900*                                 EKONOMISK HÄNDELSENIVÅ                  
005000     03 MOD-LEVELPRM-ATTR    PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-LEVELPRM         PIC X.                                       
005300     03 MOD-DELNIVA-ATTR     PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-DELNIVA          PIC X.                                       
005600     03 MOD-IDSYSMOT         PIC X(6).                                    
005700*                                 PULS MOTTAGANDE SYSTEMNAMN              
005800     03 MOD-DELKLIENT-ATTR   PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-DELKLIENT        PIC X.                                       
006100     03 MOD-IDPTYP           PIC X(3).                                    
006200*                                 POSTTYP                                 
006300     03 MOD-PROFIL-ATTR      PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 MOD-PROFIL           PIC X.                                       
006600     03 MOD-TEMFSINF         PIC X(55).                                   
006700*                                 INFORMATIONSMEDDELANDE                  
006800*** END OF VILMAII-COPY LENGTH= 223 BYTES                                 

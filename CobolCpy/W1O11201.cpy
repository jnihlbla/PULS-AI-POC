000100 01  MOD-W1O11201.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 MOD-IDARTNR-IN       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MOD-IDARTNR-UT       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-BEART-S          PIC X(25).                                   
001100*                                 ARTIKELBENÄMNING                        
001200     03 MOD-KDHOMONYM        PIC X.                                       
001300*                                 HOMONYMKOD                              
001400     03 MOD-BEART-NY-ATTR    PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 MOD-BEART-NY         PIC X(25).                                   
001700*                                 ARTIKELBENÄMNING                        
001800     03 MOD-KDHOMONYM-ATTR   PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 MOD-KDHOMONYM-NY     PIC X.                                       
002100*                                 HOMONYMKOD                              
002200     03 MOD-FLRSBEART-ATTR   PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-FLRSBEART        PIC X.                                       
002500*                                 RS-UNIK BENÄMNING                       
002600     03 MOD-IDSKYLT-RAD      OCCURS 11 TIMES.                             
002700        05 MOD-BEART-ATTR    PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900        05 MOD-BEART         PIC X(25).                                   
003000*                                 ARTIKELBENÄMNING                        
003100     03 MOD-BEART-CZ-FINNS   PIC X.                                       
003200*                                 JA/NEJ-FLAGGA                           
003300     03 MOD-BEART-GR-FINNS   PIC X.                                       
003400*                                 JA/NEJ-FLAGGA                           
003500     03 MOD-BEART-H-FINNS    PIC X.                                       
003600*                                 JA/NEJ-FLAGGA                           
003700     03 MOD-BEART-IR-FINNS   PIC X.                                       
003800*                                 JA/NEJ-FLAGGA                           
003900     03 MOD-BEART-J-FINNS    PIC X.                                       
004000*                                 JA/NEJ-FLAGGA                           
004100     03 MOD-BEART-KOR-FINNS  PIC X.                                       
004200*                                 JA/NEJ-FLAGGA                           
004300     03 MOD-BEART-PL-FINNS   PIC X.                                       
004400*                                 JA/NEJ-FLAGGA                           
004500     03 MOD-BEART-RC-FINNS   PIC X.                                       
004600*                                 JA/NEJ-FLAGGA                           
004700     03 MOD-BEART-RCN-FINNS  PIC X.                                       
004800*                                 JA/NEJ-FLAGGA                           
004900     03 MOD-BEART-RO-FINNS   PIC X.                                       
005000*                                 JA/NEJ-FLAGGA                           
005100     03 MOD-BEART-RUS-FINNS  PIC X.                                       
005200*                                 JA/NEJ-FLAGGA                           
005300     03 MOD-BEART-T-FINNS    PIC X.                                       
005400*                                 JA/NEJ-FLAGGA                           
005500     03 MOD-BEART-TR-FINNS   PIC X.                                       
005600*                                 JA/NEJ-FLAGGA                           
005700     03 MOD-BEART-YU-FINNS   PIC X.                                       
005800*                                 JA/NEJ-FLAGGA                           
005900     03 MOD-HOMONYM-RAD      OCCURS 2 TIMES.                              
006000        05 MOD-TEHOMONYM     PIC X(60).                                   
006100*                                 HOMONYMTEXT                             
006200     03 MOD-TEMFSINF         PIC X(55).                                   
006300*                                 INFORMATIONSMEDDELANDE                  
006400*** END OF VILMAII-COPY LENGTH= 607 BYTES                                 

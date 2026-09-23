000100 01  MOD-W1O12201.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 MOD-BEART-IN         PIC X(25).                                   
000700*                                 ARTIKELBENÄMNING                        
000800     03 MOD-BEART-UT         PIC X(25).                                   
000900*                                 ARTIKELBENÄMNING                        
001000     03 MOD-KDHOMONYM-IN     PIC X.                                       
001100*                                 HOMONYMKOD                              
001200     03 MOD-KDHOMONYM-UT     PIC X.                                       
001300*                                 HOMONYMKOD                              
001400     03 MOD-IDBENNR          PIC Z(6)9.                                   
001500*                                 BENÄMNINGSNUMMER                        
001600     03 MOD-UTRAD            OCCURS 11 TIMES.                             
001700        05 MOD-BEART-ATTR    PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900        05 MOD-BEART         PIC X(25).                                   
002000*                                 ARTIKELBENÄMNING                        
002100        05 MOD-FLOVERSATT    PIC X.                                       
002200*                                 KAN TEXT ÖVERSÄTTAS, I KATALOG          
002300*                                 ÄR  TEXT ÖVERSATT,   I BENREG.          
002400     03 MOD-BEART-CZ-FINNS   PIC X.                                       
002500*                                 JA/NEJ-FLAGGA                           
002600     03 MOD-FLOVERSATT-CZ    PIC X.                                       
002700*                                 JA/NEJ-FLAGGA                           
002800     03 MOD-BEART-GR-FINNS   PIC X.                                       
002900*                                 JA/NEJ-FLAGGA                           
003000     03 MOD-FLOVERSATT-GR    PIC X.                                       
003100*                                 JA/NEJ-FLAGGA                           
003200     03 MOD-BEART-H-FINNS    PIC X.                                       
003300*                                 JA/NEJ-FLAGGA                           
003400     03 MOD-FLOVERSATT-H     PIC X.                                       
003500*                                 JA/NEJ-FLAGGA                           
003600     03 MOD-BEART-IR-FINNS   PIC X.                                       
003700*                                 JA/NEJ-FLAGGA                           
003800     03 MOD-FLOVERSATT-IR    PIC X.                                       
003900*                                 JA/NEJ-FLAGGA                           
004000     03 MOD-BEART-J-FINNS    PIC X.                                       
004100*                                 JA/NEJ-FLAGGA                           
004200     03 MOD-FLOVERSATT-J     PIC X.                                       
004300*                                 JA/NEJ-FLAGGA                           
004400     03 MOD-BEART-KOR-FINNS  PIC X.                                       
004500*                                 JA/NEJ-FLAGGA                           
004600     03 MOD-FLOVERSATT-KOR   PIC X.                                       
004700*                                 JA/NEJ-FLAGGA                           
004800     03 MOD-BEART-PL-FINNS   PIC X.                                       
004900*                                 JA/NEJ-FLAGGA                           
005000     03 MOD-FLOVERSATT-PL    PIC X.                                       
005100*                                 JA/NEJ-FLAGGA                           
005200     03 MOD-BEART-RC-FINNS   PIC X.                                       
005300*                                 JA/NEJ-FLAGGA                           
005400     03 MOD-FLOVERSATT-RC    PIC X.                                       
005500*                                 JA/NEJ-FLAGGA                           
005600     03 MOD-BEART-RCN-FINNS  PIC X.                                       
005700*                                 JA/NEJ-FLAGGA                           
005800     03 MOD-FLOVERSATT-RCN   PIC X.                                       
005900*                                 JA/NEJ-FLAGGA                           
006000     03 MOD-BEART-RO-FINNS   PIC X.                                       
006100*                                 JA/NEJ-FLAGGA                           
006200     03 MOD-FLOVERSATT-RO    PIC X.                                       
006300*                                 JA/NEJ-FLAGGA                           
006400     03 MOD-BEART-RUS-FINNS  PIC X.                                       
006500*                                 JA/NEJ-FLAGGA                           
006600     03 MOD-FLOVERSATT-RUS   PIC X.                                       
006700*                                 JA/NEJ-FLAGGA                           
006800     03 MOD-BEART-T-FINNS    PIC X.                                       
006900*                                 JA/NEJ-FLAGGA                           
007000     03 MOD-FLOVERSATT-T     PIC X.                                       
007100*                                 JA/NEJ-FLAGGA                           
007200     03 MOD-BEART-TR-FINNS   PIC X.                                       
007300*                                 JA/NEJ-FLAGGA                           
007400     03 MOD-FLOVERSATT-TR    PIC X.                                       
007500*                                 JA/NEJ-FLAGGA                           
007600     03 MOD-BEART-YU-FINNS   PIC X.                                       
007700*                                 JA/NEJ-FLAGGA                           
007800     03 MOD-FLOVERSATT-YU    PIC X.                                       
007900*                                 JA/NEJ-FLAGGA                           
008000     03 MOD-UTRAD            OCCURS 2 TIMES.                              
008100        05 MOD-TEHOMONYM     PIC X(60).                                   
008200*                                 HOMONYMTEXT                             
008300     03 MOD-SKAPA-BEN        PIC X.                                       
008400     03 MOD-SKAPA-BEN-ATTR   PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600     03 MOD-TEMFSINF         PIC X(55).                                   
008700*                                 INFORMATIONSMEDDELANDE                  
008800*** END OF VILMAII-COPY LENGTH= 617 BYTES                                 

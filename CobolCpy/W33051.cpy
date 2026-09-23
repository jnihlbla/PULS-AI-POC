000100 01  W33051.                                                              
000200*                                 COPYTEXT FÖR ARTIKELSTATISTIK           
000300*                                 SUMMERADE POSTER AV A2-TYP              
000400     03 001-GRUPP.                                                        
000500*                                 ARTIKELSTATISTIK                        
000600*                                 IDENTIFIERING AV URVAL                  
000700*                                 OBS DENNA GRUPP ANVÄNDS I               
000800*                                 FLERA COPYTEXTER                        
000900        05 IDUSER            PIC X(8).                                    
001000*                                 ANVÄNDARENS SÄKERHETS ID                
001100        05 DAREGDAT          PIC 9(8).                                    
001200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001300        05 TIREGTID          PIC S9(7)           COMP-3.                  
001400*                                 REGISTRERINGSTID                        
001500        05 IDFSGURV          PIC X(8).                                    
001600*                                 URVALS IDENTITET                        
001700        05 IDPTYP            PIC X(3).                                    
001800*                                 POSTTYP                                 
001900        05 IDGTYP            PIC S9              COMP-3.                  
002000*                                 GRUPPTYP                                
002100     03 003-GRUPP.                                                        
002200*                                 ARTIKELSTATISTIK                        
002300*                                 OBS DENNA GRUPP ANVÄNDS I               
002400*                                 FLERA COPYTEXTER                        
002500        05 IDARTNR           PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700        05 BEART-SVE         PIC X(25).                                   
002800*                                 SVENSK ARTIKELBENÄMNING                 
002900        05 KDPRODSL          PIC S9(3)           COMP-3.                  
003000*                                 PRODUKTSLAG                             
003100        05 BEPRODSL          PIC X(15).                                   
003200*                                 PRODUKTSLAGSBENÄMNING                   
003300        05 IDDISTR           PIC S9(5)           COMP-3.                  
003400*                                 DISTRIKTNUMMER                          
003500        05 IDKONCNR          PIC S9(3)           COMP-3.                  
003600*                                 KONCERNNUMMER                           
003700        05 KDMARK-BUDG       PIC S9(3)           COMP-3.                  
003800*                                 MARKNADSKOD BUDGET 96 MARKNADER         
003900        05 BEMARK-BUDG       PIC X(15).                                   
004000*                                 NAMN PÅ     BUDGET 96 MARKNADER         
004100     03 008-GRUPP.                                                        
004200*                                 ARTIKELSTATISTIK                        
004300*                                 OBS DENNA GRUPP ANVÄNDS I               
004400*                                 FLERA COPYTEXTER                        
004500        05 SUARTFSG-PER      PIC S9(9)V9(2)      COMP-3.                  
004600*                                 SUMMA FSG/ART SENASTE PERIOD            
004700*                                 (AF5)                                   
004800        05 SUARTFSG-AAR      PIC S9(9)V9(2)      COMP-3.                  
004900*                                 SUMMA FSG/ART HITTILLS I ÅR             
005000*                                 (AF4)                                   
005100        05 SUARTFSG-FAAR     PIC S9(9)V9(2)      COMP-3.                  
005200*                                 SUMMA FSG "HITTILLS I ÅR"               
005300*                                 MEN FÖREGÅENDE ÅR (AF3)                 
005400        05 SUARTFSG-RAAR     PIC S9(9)V9(2)      COMP-3.                  
005500*                                 SUMMA FSG/ART  RULLANDE ÅR              
005600*                                 (AF2)                                   
005700        05 SUARTFSG-FRAAR    PIC S9(9)V9(2)      COMP-3.                  
005800*                                 SUMMA FSG/ARTIKEL FÖREG.                
005900*                                 RULLANDE ÅR (AF1)                       
006000        05 SULEVANT-PER      PIC S9(9)           COMP-3.                  
006100*                                 ANTAL LEV ART SENASTE PERIOD            
006200*                                 (AF5)                                   
006300        05 SULEVANT-AAR      PIC S9(9)           COMP-3.                  
006400*                                 ANTAL LEVERERADE ARTIKLAR               
006500*                                 HITTILLS DETTA ÅR  (AF4)                
006600        05 SULEVANT-FAAR     PIC S9(9)           COMP-3.                  
006700*                                 ANTAL LEV ART "HITTILLS I ÅR"           
006800*                                 MEN FÖREGÅENDE ÅR (AF3)                 
006900        05 SULEVANT-RAAR     PIC S9(9)           COMP-3.                  
007000*                                 ANTAL LEV ART RULLANDE ÅR               
007100*                                 (AF2)                                   
007200        05 SULEVANT-FRAAR    PIC S9(9)           COMP-3.                  
007300*                                 ANTAL LEVERERADE ARTIKLAR               
007400*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
007500        05 SUARTSJK-PER      PIC S9(9)V9(2)      COMP-3.                  
007600*                                 SUM (SJK * KVANT) SISTA PERIOD          
007700        05 SUARTSJK-AAR      PIC S9(9)V9(2)      COMP-3.                  
007800*                                 SUM (SJK * KVANT)                       
007900*                                 HITTILLS I ÅR (AF4)                     
008000        05 SUARTSJK-FAAR     PIC S9(9)V9(2)      COMP-3.                  
008100*                                 SUM (SJK * KVANT) "HITTILLS I Å         
008200*                                 R"                                      
008300*                                 MEN FÖREGÅENDE ÅR (AF3)                 
008400*                                                                         
008500        05 SUARTSJK-RAAR     PIC S9(9)V9(2)      COMP-3.                  
008600*                                 SUM (SJK * KVANT) RULLANDE ÅR           
008700        05 SUARTSJK-FRAAR    PIC S9(9)V9(2)      COMP-3.                  
008800*                                 SUM (SJK * KVANT)                       
008900*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
009000     03 009-GRUPP.                                                        
009100*                                 ARTIKELSTATISTIK                        
009200*                                 OBS DENNA GRUPP ANVÄNDS I               
009300*                                 FLERA COPYTEXTER                        
009400        05 REFSG-AAR         PIC S9(2)V9(1)      COMP-3.                  
009500*                                 FÖRSÄLJNINGSAVVIKELSE AF4 -AF3          
009600        05 REFSG-RAAR        PIC S9(2)V9(1)      COMP-3.                  
009700*                                 FÖRSÄLJNINGSAVVIKELSE AF2 -AF1          
009800        05 RELEVANT-RAAR     PIC S9(2)V9(1)      COMP-3.                  
009900*                                 ANTALSAVVIKELSE (AF2 - AF1)             
010000        05 SUTOTBV-PER       PIC S9(11)V9(2)     COMP-3.                  
010100*                                 TÄCKNINGSB SENASTE PERIOD (AF5)         
010200        05 SUTOTBV-RAAR      PIC S9(11)V9(2)     COMP-3.                  
010300*                                 TÄCKNINGSB RULLANDE ÅR    (AF2)         
010400        05 RETOTBV-RAAR-TB   PIC S9(2)V9(1)      COMP-3.                  
010500*                                 TB DIFF. RULLANDE ÅR (AF2/AF1)          
010600*                                                                         
010700        05 RETOTBV-PER       PIC S9(2)V9(1)      COMP-3.                  
010800*                                 TG SENASTE PERIOD  (AF5)                
010900*                                                                         
011000        05 RETOTBV-AAR       PIC S9(2)V9(1)      COMP-3.                  
011100*                                 TG HITTILLS I ÅR   (AF4)                
011200*                                                                         
011300        05 RETOTBV-RAAR      PIC S9(2)V9(1)      COMP-3.                  
011400*                                 TG RULLANDE ÅR     (AF2)                
011500        05 RETOTBV-RAAR-TG   PIC S9(2)V9(1)      COMP-3.                  
011600*                                 TG DIFF. RULLANDE ÅR (AF2/AF1)          
011700*                                                                         
011800*** END OF VILMAII-COPY LENGTH= 216 BYTES                                 

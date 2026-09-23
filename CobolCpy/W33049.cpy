000100 01  W33049.                                                              
000200*                                 COPYTEXT FÖR ARTIKELSTATISTIK           
000300*                                 SUMMERADE POSTER AV A1-TYP              
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
002100     03 IDARTNR              PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300     03 BEART-SVE            PIC X(25).                                   
002400*                                 SVENSK ARTIKELBENÄMNING                 
002500     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002600*                                 PRODUKTSLAG                             
002700     03 BEPRODSL             PIC X(15).                                   
002800*                                 PRODUKTSLAGSBENÄMNING                   
002900     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
003000*                                 FUNKTIONSGRUPP                          
003100     03 BEFKNGRP             PIC X(50).                                   
003200*                                 FUNKTIONSGRUPPSBENÄMNING                
003300     03 008-GRUPP.                                                        
003400*                                 ARTIKELSTATISTIK                        
003500*                                 OBS DENNA GRUPP ANVÄNDS I               
003600*                                 FLERA COPYTEXTER                        
003700        05 SUARTFSG-PER      PIC S9(9)V9(2)      COMP-3.                  
003800*                                 SUMMA FSG/ART SENASTE PERIOD            
003900*                                 (AF5)                                   
004000        05 SUARTFSG-AAR      PIC S9(9)V9(2)      COMP-3.                  
004100*                                 SUMMA FSG/ART HITTILLS I ÅR             
004200*                                 (AF4)                                   
004300        05 SUARTFSG-FAAR     PIC S9(9)V9(2)      COMP-3.                  
004400*                                 SUMMA FSG "HITTILLS I ÅR"               
004500*                                 MEN FÖREGÅENDE ÅR (AF3)                 
004600        05 SUARTFSG-RAAR     PIC S9(9)V9(2)      COMP-3.                  
004700*                                 SUMMA FSG/ART  RULLANDE ÅR              
004800*                                 (AF2)                                   
004900        05 SUARTFSG-FRAAR    PIC S9(9)V9(2)      COMP-3.                  
005000*                                 SUMMA FSG/ARTIKEL FÖREG.                
005100*                                 RULLANDE ÅR (AF1)                       
005200        05 SULEVANT-PER      PIC S9(9)           COMP-3.                  
005300*                                 ANTAL LEV ART SENASTE PERIOD            
005400*                                 (AF5)                                   
005500        05 SULEVANT-AAR      PIC S9(9)           COMP-3.                  
005600*                                 ANTAL LEVERERADE ARTIKLAR               
005700*                                 HITTILLS DETTA ÅR  (AF4)                
005800        05 SULEVANT-FAAR     PIC S9(9)           COMP-3.                  
005900*                                 ANTAL LEV ART "HITTILLS I ÅR"           
006000*                                 MEN FÖREGÅENDE ÅR (AF3)                 
006100        05 SULEVANT-RAAR     PIC S9(9)           COMP-3.                  
006200*                                 ANTAL LEV ART RULLANDE ÅR               
006300*                                 (AF2)                                   
006400        05 SULEVANT-FRAAR    PIC S9(9)           COMP-3.                  
006500*                                 ANTAL LEVERERADE ARTIKLAR               
006600*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
006700        05 SUARTSJK-PER      PIC S9(9)V9(2)      COMP-3.                  
006800*                                 SUM (SJK * KVANT) SISTA PERIOD          
006900        05 SUARTSJK-AAR      PIC S9(9)V9(2)      COMP-3.                  
007000*                                 SUM (SJK * KVANT)                       
007100*                                 HITTILLS I ÅR (AF4)                     
007200        05 SUARTSJK-FAAR     PIC S9(9)V9(2)      COMP-3.                  
007300*                                 SUM (SJK * KVANT) "HITTILLS I Å         
007400*                                 R"                                      
007500*                                 MEN FÖREGÅENDE ÅR (AF3)                 
007600*                                                                         
007700        05 SUARTSJK-RAAR     PIC S9(9)V9(2)      COMP-3.                  
007800*                                 SUM (SJK * KVANT) RULLANDE ÅR           
007900        05 SUARTSJK-FRAAR    PIC S9(9)V9(2)      COMP-3.                  
008000*                                 SUM (SJK * KVANT)                       
008100*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
008200     03 RELEVANT-AAR         PIC S9(2)V9(1)      COMP-3.                  
008300*                                 ANTALSAVVIKELSE (AF4 - AF3)             
008400     03 REFSG-AAR            PIC S9(2)V9(1)      COMP-3.                  
008500*                                 FÖRSÄLJNINGSAVVIKELSE AF4 -AF3          
008600     03 RELEVANT-RAAR        PIC S9(2)V9(1)      COMP-3.                  
008700*                                 ANTALSAVVIKELSE (AF2 - AF1)             
008800     03 RETOTBV-RAAR-TG      PIC S9(2)V9(1)      COMP-3.                  
008900*                                 TG DIFF. RULLANDE ÅR (AF2/AF1)          
009000*                                                                         
009100     03 RETOTBV-RAAR         PIC S9(2)V9(1)      COMP-3.                  
009200*                                 TG RULLANDE ÅR     (AF2)                
009300     03 SUTOTBV-RAAR         PIC S9(11)V9(2)     COMP-3.                  
009400*                                 TÄCKNINGSB RULLANDE ÅR    (AF2)         
009500*** END OF VILMAII-COPY LENGTH= 234 BYTES                                 

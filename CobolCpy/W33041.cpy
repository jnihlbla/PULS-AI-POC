000100 01  W33041.                                                              
000200*                                 COPYTEXT FÖR ARTIKELSTATISTIK           
000300*                                 URVALSPOSTER AV A OCH P TYP             
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
002500     03 002-GRUPP.                                                        
002600*                                 ARTIKELSTATISTIK                        
002700*                                 OBS DENNA GRUPP ANVÄNDS I               
002800*                                 FLERA COPYTEXTER                        
002900        05 KDPRODSL          PIC S9(3)           COMP-3.                  
003000*                                 PRODUKTSLAG                             
003100        05 BEPRODSL          PIC X(15).                                   
003200*                                 PRODUKTSLAGSBENÄMNING                   
003300        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
003400*                                 FUNKTIONSGRUPP                          
003500        05 BEFKNGRP          PIC X(50).                                   
003600*                                 FUNKTIONSGRUPPSBENÄMNING                
003700        05 IDDISTR           PIC S9(5)           COMP-3.                  
003800*                                 DISTRIKTNUMMER                          
003900        05 IDKONCNR          PIC S9(3)           COMP-3.                  
004000*                                 KONCERNNUMMER                           
004100        05 KDMARK-BUDG       PIC S9(3)           COMP-3.                  
004200*                                 MARKNADSKOD BUDGET 96 MARKNADER         
004300        05 BEMARK-BUDG       PIC X(15).                                   
004400*                                 NAMN PÅ     BUDGET 96 MARKNADER         
004500     03 008-GRUPP.                                                        
004600*                                 ARTIKELSTATISTIK                        
004700*                                 OBS DENNA GRUPP ANVÄNDS I               
004800*                                 FLERA COPYTEXTER                        
004900        05 SUARTFSG-PER      PIC S9(9)V9(2)      COMP-3.                  
005000*                                 SUMMA FSG/ART SENASTE PERIOD            
005100*                                 (AF5)                                   
005200        05 SUARTFSG-AAR      PIC S9(9)V9(2)      COMP-3.                  
005300*                                 SUMMA FSG/ART HITTILLS I ÅR             
005400*                                 (AF4)                                   
005500        05 SUARTFSG-FAAR     PIC S9(9)V9(2)      COMP-3.                  
005600*                                 SUMMA FSG "HITTILLS I ÅR"               
005700*                                 MEN FÖREGÅENDE ÅR (AF3)                 
005800        05 SUARTFSG-RAAR     PIC S9(9)V9(2)      COMP-3.                  
005900*                                 SUMMA FSG/ART  RULLANDE ÅR              
006000*                                 (AF2)                                   
006100        05 SUARTFSG-FRAAR    PIC S9(9)V9(2)      COMP-3.                  
006200*                                 SUMMA FSG/ARTIKEL FÖREG.                
006300*                                 RULLANDE ÅR (AF1)                       
006400        05 SULEVANT-PER      PIC S9(9)           COMP-3.                  
006500*                                 ANTAL LEV ART SENASTE PERIOD            
006600*                                 (AF5)                                   
006700        05 SULEVANT-AAR      PIC S9(9)           COMP-3.                  
006800*                                 ANTAL LEVERERADE ARTIKLAR               
006900*                                 HITTILLS DETTA ÅR  (AF4)                
007000        05 SULEVANT-FAAR     PIC S9(9)           COMP-3.                  
007100*                                 ANTAL LEV ART "HITTILLS I ÅR"           
007200*                                 MEN FÖREGÅENDE ÅR (AF3)                 
007300        05 SULEVANT-RAAR     PIC S9(9)           COMP-3.                  
007400*                                 ANTAL LEV ART RULLANDE ÅR               
007500*                                 (AF2)                                   
007600        05 SULEVANT-FRAAR    PIC S9(9)           COMP-3.                  
007700*                                 ANTAL LEVERERADE ARTIKLAR               
007800*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
007900        05 SUARTSJK-PER      PIC S9(9)V9(2)      COMP-3.                  
008000*                                 SUM (SJK * KVANT) SISTA PERIOD          
008100        05 SUARTSJK-AAR      PIC S9(9)V9(2)      COMP-3.                  
008200*                                 SUM (SJK * KVANT)                       
008300*                                 HITTILLS I ÅR (AF4)                     
008400        05 SUARTSJK-FAAR     PIC S9(9)V9(2)      COMP-3.                  
008500*                                 SUM (SJK * KVANT) "HITTILLS I Å         
008600*                                 R"                                      
008700*                                 MEN FÖREGÅENDE ÅR (AF3)                 
008800*                                                                         
008900        05 SUARTSJK-RAAR     PIC S9(9)V9(2)      COMP-3.                  
009000*                                 SUM (SJK * KVANT) RULLANDE ÅR           
009100        05 SUARTSJK-FRAAR    PIC S9(9)V9(2)      COMP-3.                  
009200*                                 SUM (SJK * KVANT)                       
009300*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
009400*** END OF VILMAII-COPY LENGTH= 239 BYTES                                 

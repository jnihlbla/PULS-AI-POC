000100 01  W33014.                                                              
000200*                                 ARTIKELSTATISTIK                        
000300*                                 SEKUNDÄWREGISTER                        
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 IDDISTR              PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKONCNR             PIC S9(3)           COMP-3.                  
000900*                                 KONCERNNUMMER                           
001000     03 KDMARK-BUDG          PIC S9(3)           COMP-3.                  
001100*                                 MARKNADSKOD BUDGET 96 MARKNADER         
001200     03 BEMARK-BUDG          PIC X(15).                                   
001300*                                 NAMN PÅ     BUDGET 96 MARKNADER         
001400     03 SUARTFSG-PER         PIC S9(9)V9(2)      COMP-3.                  
001500*                                 SUMMA FSG/ART SENASTE PERIOD            
001600*                                 (AF5)                                   
001700     03 SUARTFSG-AAR         PIC S9(9)V9(2)      COMP-3.                  
001800*                                 SUMMA FSG/ART HITTILLS I ÅR             
001900*                                 (AF4)                                   
002000     03 SUARTFSG-FAAR        PIC S9(9)V9(2)      COMP-3.                  
002100*                                 SUMMA FSG "HITTILLS I ÅR"               
002200*                                 MEN FÖREGÅENDE ÅR (AF3)                 
002300     03 SUARTFSG-RAAR        PIC S9(9)V9(2)      COMP-3.                  
002400*                                 SUMMA FSG/ART  RULLANDE ÅR              
002500*                                 (AF2)                                   
002600     03 SUARTFSG-FRAAR       PIC S9(9)V9(2)      COMP-3.                  
002700*                                 SUMMA FSG/ARTIKEL FÖREG.                
002800*                                 RULLANDE ÅR (AF1)                       
002900     03 SULEVANT-PER         PIC S9(9)           COMP-3.                  
003000*                                 ANTAL LEV ART SENASTE PERIOD            
003100*                                 (AF5)                                   
003200     03 SULEVANT-AAR         PIC S9(9)           COMP-3.                  
003300*                                 ANTAL LEVERERADE ARTIKLAR               
003400*                                 HITTILLS DETTA ÅR  (AF4)                
003500     03 SULEVANT-FAAR        PIC S9(9)           COMP-3.                  
003600*                                 ANTAL LEV ART "HITTILLS I ÅR"           
003700*                                 MEN FÖREGÅENDE ÅR (AF3)                 
003800     03 SULEVANT-RAAR        PIC S9(9)           COMP-3.                  
003900*                                 ANTAL LEV ART RULLANDE ÅR               
004000*                                 (AF2)                                   
004100     03 SULEVANT-FRAAR       PIC S9(9)           COMP-3.                  
004200*                                 ANTAL LEVERERADE ARTIKLAR               
004300*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
004400     03 SUARTSJK-PER         PIC S9(9)V9(2)      COMP-3.                  
004500*                                 SUM (SJK * KVANT) SISTA PERIOD          
004600     03 SUARTSJK-AAR         PIC S9(9)V9(2)      COMP-3.                  
004700*                                 SUM (SJK * KVANT)                       
004800*                                 HITTILLS I ÅR (AF4)                     
004900     03 SUARTSJK-FAAR        PIC S9(9)V9(2)      COMP-3.                  
005000*                                 SUM (SJK * KVANT) "HITTILLS I Å         
005100*                                 R"                                      
005200*                                 MEN FÖREGÅENDE ÅR (AF3)                 
005300*                                                                         
005400     03 SUARTSJK-RAAR        PIC S9(9)V9(2)      COMP-3.                  
005500*                                 SUM (SJK * KVANT) RULLANDE ÅR           
005600     03 SUARTSJK-FRAAR       PIC S9(9)V9(2)      COMP-3.                  
005700*                                 SUM (SJK * KVANT)                       
005800*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
005900     03 SULEVANT-RAAR-SPEC   PIC S9(9)           COMP-3.                  
006000*                                 ANTAL LEV. ART TILL SPECIAL             
006100*                                 PRIS RULLANDE ÅR (AF2)                  
006200     03 SULEVANT-RAAR-RAB    PIC S9(9)           COMP-3.                  
006300*                                 ANTAL LEVERERADE ARTIKLAR TILL          
006400*                                 RABATT/FKNGRP RULLANDE ÅR (AF2)         
006500     03 SULEVANT-RAAR-MAN    PIC S9(9)           COMP-3.                  
006600*                                 ANT LEV ART TILL MANUELLT               
006700*                                 PRIS RULLANDE ÅR (AF2)                  
006800     03 SULEVANT-RAAR-KRE    PIC S9(9)           COMP-3.                  
006900*                                 ANTAL LEV ART SOM KREDITERATS           
007000*                                 INNEVARANDE RULLANDE ÅR (AF2)           
007100     03 SUARTFSG-RAAR-SPEC   PIC S9(9)V9(2)      COMP-3.                  
007200*                                 SUMMA FSG/ARTIKEL TILL SPECIAL-         
007300*                                 PRIS RULLANDE ÅR (AF2)                  
007400     03 SUARTFSG-RAAR-RAB    PIC S9(9)V9(2)      COMP-3.                  
007500*                                 SUMMA FSG/ART TILL RABATT/              
007600*                                 FKNGRP RULLANDE ÅR (AF2)                
007700     03 SUARTFSG-RAAR-MAN    PIC S9(9)V9(2)      COMP-3.                  
007800*                                 SUMMA FSG/ARTIKEL TILL PRIS I           
007900*                                 RAD RULLANDE ÅR (AF2)                   
008000     03 SUARTFSG-RAAR-KRE    PIC S9(9)V9(2)      COMP-3.                  
008100*                                 SUMMA KREDITERAD FSG/ARTIKEL            
008200*                                 RULLANDE ÅR (AF2)                       
008300     03 SUARTFSG-DO-RAAR     PIC S9(9)V9(2)      COMP-3.                  
008400*                                 SUMMA FSG/ART  RULLANDE ÅR              
008500*                                 (AF2)                                   
008600*                                 DAGORDER                                
008700     03 SULEVANT-DO-RAAR     PIC S9(9)           COMP-3.                  
008800*                                 ANTAL LEV ART RULLANDE ÅR               
008900*                                 (AF2) DAGORDER                          
009000*** END COPY W33014      LENGTH=167                                       

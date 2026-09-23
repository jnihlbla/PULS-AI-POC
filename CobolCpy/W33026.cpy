000100 01  W33026-CTX.                                                          
000200*                                 ARTIKELSTATISTIK                        
000300*                                 SEKUNDÄRREGISTER                        
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 IDPROMR.                                                          
000700*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
000800        05 IDMARKBO          PIC X.                                       
000900*                                 MARKNADSBOLAGSKOD                       
001000        05 IDPROMRN          PIC X(2).                                    
001100*                                 PRISOMRÅDE LÖPNUMMER                    
001200     03 IDDISTR              PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400     03 IDKONCNR             PIC S9(3)           COMP-3.                  
001500*                                 KONCERNNUMMER                           
001600     03 KDMARK-BUDG          PIC S9(3)           COMP-3.                  
001700*                                 MARKNADSKOD BUDGET 96 MARKNADER         
001800     03 BEMARK-BUDG          PIC X(15).                                   
001900*                                 NAMN PÅ     BUDGET 96 MARKNADER         
002000     03 SUARTFSG-PER         PIC S9(9)V9(2)      COMP-3.                  
002100*                                 SUMMA FSG/ART SENASTE PERIOD            
002200*                                 (AF5)                                   
002300     03 SUARTFSG-AAR         PIC S9(9)V9(2)      COMP-3.                  
002400*                                 SUMMA FSG/ART HITTILLS I ÅR             
002500*                                 (AF4)                                   
002600     03 SUARTFSG-FAAR        PIC S9(9)V9(2)      COMP-3.                  
002700*                                 SUMMA FSG "HITTILLS I ÅR"               
002800*                                 MEN FÖREGÅENDE ÅR (AF3)                 
002900     03 SUARTFSG-RAAR        PIC S9(9)V9(2)      COMP-3.                  
003000*                                 SUMMA FSG/ART  RULLANDE ÅR              
003100*                                 (AF2)                                   
003200     03 SUARTFSG-FRAAR       PIC S9(9)V9(2)      COMP-3.                  
003300*                                 SUMMA FSG/ARTIKEL FÖREG.                
003400*                                 RULLANDE ÅR (AF1)                       
003500     03 SULEVANT-PER         PIC S9(9)           COMP-3.                  
003600*                                 ANTAL LEV ART SENASTE PERIOD            
003700*                                 (AF5)                                   
003800     03 SULEVANT-AAR         PIC S9(9)           COMP-3.                  
003900*                                 ANTAL LEVERERADE ARTIKLAR               
004000*                                 HITTILLS DETTA ÅR  (AF4)                
004100     03 SULEVANT-FAAR        PIC S9(9)           COMP-3.                  
004200*                                 ANTAL LEV ART "HITTILLS I ÅR"           
004300*                                 MEN FÖREGÅENDE ÅR (AF3)                 
004400     03 SULEVANT-RAAR        PIC S9(9)           COMP-3.                  
004500*                                 ANTAL LEV ART RULLANDE ÅR               
004600*                                 (AF2)                                   
004700     03 SULEVANT-FRAAR       PIC S9(9)           COMP-3.                  
004800*                                 ANTAL LEVERERADE ARTIKLAR               
004900*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
005000     03 SUARTSJK-PER         PIC S9(9)V9(2)      COMP-3.                  
005100*                                 SUM (SJK * KVANT) SISTA PERIOD          
005200     03 SUARTSJK-AAR         PIC S9(9)V9(2)      COMP-3.                  
005300*                                 SUM (SJK * KVANT)                       
005400*                                 HITTILLS I ÅR (AF4)                     
005500     03 SUARTSJK-FAAR        PIC S9(9)V9(2)      COMP-3.                  
005600*                                 SUM (SJK * KVANT) "HITTILLS I Å         
005700*                                 R"                                      
005800*                                 MEN FÖREGÅENDE ÅR (AF3)                 
005900*                                                                         
006000     03 SUARTSJK-RAAR        PIC S9(9)V9(2)      COMP-3.                  
006100*                                 SUM (SJK * KVANT) RULLANDE ÅR           
006200     03 SUARTSJK-FRAAR       PIC S9(9)V9(2)      COMP-3.                  
006300*                                 SUM (SJK * KVANT)                       
006400*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
006500     03 SULEVANT-RAAR-SPEC   PIC S9(9)           COMP-3.                  
006600*                                 ANTAL LEV. ART TILL SPECIAL             
006700*                                 PRIS RULLANDE ÅR (AF2)                  
006800     03 SULEVANT-RAAR-RAB    PIC S9(9)           COMP-3.                  
006900*                                 ANTAL LEVERERADE ARTIKLAR TILL          
007000*                                 RABATT/FKNGRP RULLANDE ÅR (AF2)         
007100     03 SULEVANT-RAAR-MAN    PIC S9(9)           COMP-3.                  
007200*                                 ANT LEV ART TILL MANUELLT               
007300*                                 PRIS RULLANDE ÅR (AF2)                  
007400     03 SULEVANT-RAAR-KRE    PIC S9(9)           COMP-3.                  
007500*                                 ANTAL LEV ART SOM KREDITERATS           
007600*                                 INNEVARANDE RULLANDE ÅR (AF2)           
007700     03 SUARTFSG-RAAR-SPEC   PIC S9(9)V9(2)      COMP-3.                  
007800*                                 SUMMA FSG/ARTIKEL TILL SPECIAL-         
007900*                                 PRIS RULLANDE ÅR (AF2)                  
008000     03 SUARTFSG-RAAR-RAB    PIC S9(9)V9(2)      COMP-3.                  
008100*                                 SUMMA FSG/ART TILL RABATT/              
008200*                                 FKNGRP RULLANDE ÅR (AF2)                
008300     03 SUARTFSG-RAAR-MAN    PIC S9(9)V9(2)      COMP-3.                  
008400*                                 SUMMA FSG/ARTIKEL TILL PRIS I           
008500*                                 RAD RULLANDE ÅR (AF2)                   
008600     03 SUARTFSG-RAAR-KRE    PIC S9(9)V9(2)      COMP-3.                  
008700*                                 SUMMA KREDITERAD FSG/ARTIKEL            
008800*                                 RULLANDE ÅR (AF2)                       
008900     03 SUARTFSG-DO-RAAR     PIC S9(9)V9(2)      COMP-3.                  
009000*                                 SUMMA FSG/ART  RULLANDE ÅR              
009100*                                 (AF2)                                   
009200*                                 DAGORDER                                
009300     03 SULEVANT-DO-RAAR     PIC S9(9)           COMP-3.                  
009400*                                 ANTAL LEV ART RULLANDE ÅR               
009500*                                 (AF2) DAGORDER                          
009600*** END OF VILMAII-COPY LENGTH= 170 BYTES                                 

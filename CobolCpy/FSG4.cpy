000100 01  FSG4-TAB.                                                            
000200*                                 FÖRSÄLJNINGSSTATISTIK                   
000300*                                 NYCKEL: ARTIKEL, DISTRIKT               
000400*                                                                         
000500*                                                                         
000600*                                 UPPGIFTERNA ÄR PÅ AF-NIVÅ, OCH          
000700*                                 KAN VARA TOTAL ELLER ANNAN TYP          
000800*                                 AV PRISSÄTTNING FSG ELLER FSG           
000900*                                 TILL SPECIALPRIS                        
001000     03 IDARTNR              PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200     03 IDDISTR              PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400     03 SUARTFSG-PER         PIC S9(9)V9(2)      COMP-3.                  
001500*                                 SUMMA FSG/ART SENASTE PERIOD            
001600*                                 (AF5)                                   
001700     03 SUARTFSG-AAR         PIC S9(9)V9(2)      COMP-3.                  
001800*                                 SUMMA FSG/ART HITTILLS I ÅR             
001900*                                 (AF4)                                   
002000     03 SUARTFSG-RAAR-SPEC   PIC S9(9)V9(2)      COMP-3.                  
002100*                                 SUMMA FSG/ARTIKEL TILL SPECIAL-         
002200*                                 PRIS HITTILLS I ÅR (AF4)                
002300     03 SUARTFSG-RAAR-RAB    PIC S9(9)V9(2)      COMP-3.                  
002400*                                 SUMMA FSG/ART TILL RABATT/              
002500*                                 FKNGRP HITTILLS I ÅR (AF4)              
002600     03 SUARTFSG-RAAR-MAN    PIC S9(9)V9(2)      COMP-3.                  
002700*                                 SUMMA FSG/ARTIKEL TILL PRIS I           
002800*                                 RAD HITTILLS I ÅR (AF4)                 
002900     03 SUARTFSG-RAAR-KRE    PIC S9(9)V9(2)      COMP-3.                  
003000*                                 SUMMA KREDITERAD FSG/ARTIKEL            
003100*                                 HITTILLS I ÅR (AF4)                     
003200     03 SUARTFSG-FAAR        PIC S9(9)V9(2)      COMP-3.                  
003300*                                 SUMMA FSG "HITTILLS I ÅR"               
003400*                                 MEN FÖREGÅENDE ÅR (AF3)                 
003500     03 SUARTFSG-RAAR        PIC S9(9)V9(2)      COMP-3.                  
003600*                                 SUMMA FSG/ART  RULLANDE ÅR              
003700*                                 (AF2)                                   
003800     03 SUARTFSG-FRAAR       PIC S9(9)V9(2)      COMP-3.                  
003900*                                 SUMMA FSG/ARTIKEL FÖREG.                
004000*                                 RULLANDE ÅR (AF1)                       
004100     03 SULEVANT-PER         PIC S9(9)           COMP-3.                  
004200*                                 ANTAL LEV ART SENASTE PERIOD            
004300*                                 (AF5)                                   
004400     03 SULEVANT-AAR         PIC S9(9)           COMP-3.                  
004500*                                 ANTAL LEVERERADE ARTIKLAR               
004600*                                 HITTILLS DETTA ÅR  (AF4)                
004700     03 SULEVANT-RAAR-SPEC   PIC S9(9)           COMP-3.                  
004800*                                 ANTAL LEV. ART TILL SPECIAL             
004900*                                 PRIS I ÅR                               
005000     03 SULEVANT-RAAR-RAB    PIC S9(9)           COMP-3.                  
005100*                                 ANTAL LEVERERADE ARTIKLAR TILL          
005200*                                 RABATT/FKNGRP I ÅR                      
005300     03 SULEVANT-RAAR-MAN    PIC S9(9)           COMP-3.                  
005400*                                 ANT LEV ART TILL                        
005500*                                 MANUELLT PRIS I ÅR                      
005600     03 SULEVANT-RAAR-KRE    PIC S9(9)           COMP-3.                  
005700*                                 ANTAL LEV ART SOM KREDITERATS           
005800*                                 I ÅR                                    
005900     03 SULEVANT-FAAR        PIC S9(9)           COMP-3.                  
006000*                                 ANTAL LEV ART "HITTILLS I ÅR"           
006100*                                 MEN FÖREGÅENDE ÅR (AF3)                 
006200     03 SULEVANT-RAAR        PIC S9(9)           COMP-3.                  
006300*                                 ANTAL LEV ART RULLANDE ÅR               
006400*                                 (AF2)                                   
006500     03 SULEVANT-FRAAR       PIC S9(9)           COMP-3.                  
006600*                                 ANTAL LEVERERADE ARTIKLAR               
006700*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
006800     03 SUTOTBV-PER          PIC S9(11)V9(2)     COMP-3.                  
006900*                                 TÄCKNINGSB SENASTE PERIOD (AF5)         
007000     03 SUTOTBV-AAR          PIC S9(11)V9(2)     COMP-3.                  
007100*                                 TÄCKNINGSB HITTILLS I ÅR  (AF4)         
007200     03 SUTOTBV-FAAR         PIC S9(11)V9(2)     COMP-3.                  
007300*                                 TÄCKNINGSBIDRAG "HITTILLS I ÅR"         
007400*                                 MEN FÖREGÅENDE ÅR (AF3)                 
007500     03 SUTOTBV-RAAR         PIC S9(11)V9(2)     COMP-3.                  
007600*                                 TÄCKNINGSB RULLANDE ÅR    (AF2)         
007700     03 SUTOTBV-FRAAR        PIC S9(11)V9(2)     COMP-3.                  
007800*                                 TÄCKNINGSBIDRAG FÖR RULLANDE            
007900*                                 FÖREGÅENDE ÅR (AF1)                     
008000*** END COPY FSG4CCCCC0  LENGTH=142                                       

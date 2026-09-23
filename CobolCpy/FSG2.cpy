000100 01  FSG2-TAB.                                                            
000200*                                 FÖRSÄLJNINGSSTATISTIK                   
000300*                                 NYCKEL: ARTIKEL                         
000400*                                                                         
000500*                                                                         
000600*                                 UPPGIFTERNA ÄR PÅ AF-NIVÅ,              
000700*                                 WORLD WIDE OCH KAN VARA TOTAL           
000800*                                 FSG ELLER FSG TILL SPECIALPRIS          
000900*                                 PRIS ELLER ANNAN TYP AV PRIS-           
001000*                                 SÄTTNING.                               
001100     03 IDARTNR              PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300     03 SUARTFSG-PER         PIC S9(9)V9(2)      COMP-3.                  
001400*                                 SUMMA FSG/ART SENASTE PERIOD            
001500*                                 (AF5)                                   
001600     03 SUARTFSG-AAR         PIC S9(9)V9(2)      COMP-3.                  
001700*                                 SUMMA FSG/ART HITTILLS I ÅR             
001800*                                 (AF4)                                   
001900     03 SUARTFSG-RAAR-SPEC   PIC S9(9)V9(2)      COMP-3.                  
002000*                                 SUMMA FSG/ARTIKEL TILL SPECIAL-         
002100*                                 PRIS HITTILLS I ÅR (AF4)                
002200     03 SUARTFSG-RAAR-RAB    PIC S9(9)V9(2)      COMP-3.                  
002300*                                 SUMMA FSG/ART TILL RABATT/              
002400*                                 FKNGRP HITTILLS I ÅR (AF4)              
002500     03 SUARTFSG-RAAR-MAN    PIC S9(9)V9(2)      COMP-3.                  
002600*                                 SUMMA FSG/ARTIKEL TILL PRIS I           
002700*                                 RAD HITTILLS I ÅR (AF4)                 
002800     03 SUARTFSG-RAAR-KRE    PIC S9(9)V9(2)      COMP-3.                  
002900*                                 SUMMA KREDITERAD FSG/ARTIKEL            
003000*                                 HITTILLS I ÅR (AF4)                     
003100     03 SUARTFSG-FAAR        PIC S9(9)V9(2)      COMP-3.                  
003200*                                 SUMMA FSG "HITTILLS I ÅR"               
003300*                                 MEN FÖREGÅENDE ÅR (AF3)                 
003400     03 SUARTFSG-RAAR        PIC S9(9)V9(2)      COMP-3.                  
003500*                                 SUMMA FSG/ART  RULLANDE ÅR              
003600*                                 (AF2)                                   
003700     03 SUARTFSG-FRAAR       PIC S9(9)V9(2)      COMP-3.                  
003800*                                 SUMMA FSG/ARTIKEL FÖREG.                
003900*                                 RULLANDE ÅR (AF1)                       
004000     03 SULEVANT-PER         PIC S9(9)           COMP-3.                  
004100*                                 ANTAL LEV ART SENASTE PERIOD            
004200*                                 (AF5)                                   
004300     03 SULEVANT-AAR         PIC S9(9)           COMP-3.                  
004400*                                 ANTAL LEVERERADE ARTIKLAR               
004500*                                 HITTILLS DETTA ÅR  (AF4)                
004600     03 SULEVANT-RAAR-SPEC   PIC S9(9)           COMP-3.                  
004700*                                 ANTAL LEV. ART TILL SPECIAL             
004800*                                 PRIS I ÅR                               
004900     03 SULEVANT-RAAR-RAB    PIC S9(9)           COMP-3.                  
005000*                                 ANTAL LEVERERADE ARTIKLAR TILL          
005100*                                 RABATT/FKNGRP I ÅR                      
005200     03 SULEVANT-RAAR-MAN    PIC S9(9)           COMP-3.                  
005300*                                 ANT LEV ART TILL                        
005400*                                 MANUELLT PRIS I ÅR                      
005500     03 SULEVANT-RAAR-KRE    PIC S9(9)           COMP-3.                  
005600*                                 ANTAL LEV ART SOM KREDITERATS           
005700*                                 I ÅR                                    
005800     03 SULEVANT-FAAR        PIC S9(9)           COMP-3.                  
005900*                                 ANTAL LEV ART "HITTILLS I ÅR"           
006000*                                 MEN FÖREGÅENDE ÅR (AF3)                 
006100     03 SULEVANT-RAAR        PIC S9(9)           COMP-3.                  
006200*                                 ANTAL LEV ART RULLANDE ÅR               
006300*                                 (AF2)                                   
006400     03 SULEVANT-FRAAR       PIC S9(9)           COMP-3.                  
006500*                                 ANTAL LEVERERADE ARTIKLAR               
006600*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
006700     03 SUTOTBV-PER          PIC S9(11)V9(2)     COMP-3.                  
006800*                                 TÄCKNINGSB SENASTE PERIOD (AF5)         
006900     03 SUTOTBV-AAR          PIC S9(11)V9(2)     COMP-3.                  
007000*                                 TÄCKNINGSB HITTILLS I ÅR  (AF4)         
007100     03 SUTOTBV-FAAR         PIC S9(11)V9(2)     COMP-3.                  
007200*                                 TÄCKNINGSBIDRAG "HITTILLS I ÅR"         
007300*                                 MEN FÖREGÅENDE ÅR (AF3)                 
007400     03 SUTOTBV-RAAR         PIC S9(11)V9(2)     COMP-3.                  
007500*                                 TÄCKNINGSB RULLANDE ÅR    (AF2)         
007600     03 SUTOTBV-FRAAR        PIC S9(11)V9(2)     COMP-3.                  
007700*                                 TÄCKNINGSBIDRAG FÖR RULLANDE            
007800*                                 FÖREGÅENDE ÅR (AF1)                     
007900*** END COPY FSG2CCCCC0  LENGTH=139                                       

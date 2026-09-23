000100 01  W33027.                                                              
000200*                                                                         
000300*                                 COPYTEXT FÖR FILEN W33027 EN KO         
000400*                                 PIA AV FILEN W33021                     
000500*                                 DETTA ÄR EN KOPIA AV KOPYTEXT F         
000600*                                 SG2(DB2-KOPYTEXT)                       
000700*                                                                         
000800*                                 DENNA FIL DISTRIBUERAS TILL PRI         
000900*                                 SSÄTTNINGSSYTEMET, WG10                 
001000*                                 FÖR ATT FÖRSE DETTA SYSTEM MED          
001100*                                 FÖRS.INFORMATION                        
001200*                                                                         
001300*                                 FÖRSÄLJNINGSSTATISTIK                   
001400*                                 NYCKEL: ARTIKEL                         
001500*                                 UPPGIFTERNA ÄR PÅ AF-NIVÅ,              
001600*                                 WORLD WIDE OCH KAN VARA TOTAL           
001700*                                 FSG ELLER FSG TILL SPECIALPRIS          
001800*                                 PRIS ELLER ANNAN TYP AV PRIS-           
001900*                                 SÄTTNING.                               
002000     03 IDARTNR              PIC S9(9)           COMP-3.                  
002100*                                 ARTIKELNUMMER                           
002200     03 SUARTFSG-PER         PIC S9(9)V9(2)      COMP-3.                  
002300*                                 SUMMA FSG/ART SENASTE PERIOD            
002400*                                 (AF5)                                   
002500     03 SUARTFSG-AAR         PIC S9(9)V9(2)      COMP-3.                  
002600*                                 SUMMA FSG/ART HITTILLS I ÅR             
002700*                                 (AF4)                                   
002800     03 SUARTFSG-RAAR-SPEC   PIC S9(9)V9(2)      COMP-3.                  
002900*                                 SUMMA FSG/ARTIKEL TILL SPECIAL-         
003000*                                 PRIS RULLANDE ÅR (AF2)                  
003100     03 SUARTFSG-RAAR-RAB    PIC S9(9)V9(2)      COMP-3.                  
003200*                                 SUMMA FSG/ART TILL RABATT/              
003300*                                 FKNGRP RULLANDE ÅR (AF2)                
003400     03 SUARTFSG-RAAR-MAN    PIC S9(9)V9(2)      COMP-3.                  
003500*                                 SUMMA FSG/ARTIKEL TILL PRIS I           
003600*                                 RAD RULLANDE ÅR (AF2)                   
003700     03 SUARTFSG-RAAR-KRE    PIC S9(9)V9(2)      COMP-3.                  
003800*                                 SUMMA KREDITERAD FSG/ARTIKEL            
003900*                                 RULLANDE ÅR (AF2)                       
004000     03 SUARTFSG-FAAR        PIC S9(9)V9(2)      COMP-3.                  
004100*                                 SUMMA FSG "HITTILLS I ÅR"               
004200*                                 MEN FÖREGÅENDE ÅR (AF3)                 
004300     03 SUARTFSG-RAAR        PIC S9(9)V9(2)      COMP-3.                  
004400*                                 SUMMA FSG/ART  RULLANDE ÅR              
004500*                                 (AF2)                                   
004600     03 SUARTFSG-FRAAR       PIC S9(9)V9(2)      COMP-3.                  
004700*                                 SUMMA FSG/ARTIKEL FÖREG.                
004800*                                 RULLANDE ÅR (AF1)                       
004900     03 SULEVANT-PER         PIC S9(9)           COMP-3.                  
005000*                                 ANTAL LEV ART SENASTE PERIOD            
005100*                                 (AF5)                                   
005200     03 SULEVANT-AAR         PIC S9(9)           COMP-3.                  
005300*                                 ANTAL LEVERERADE ARTIKLAR               
005400*                                 HITTILLS DETTA ÅR  (AF4)                
005500     03 SULEVANT-RAAR-SPEC   PIC S9(9)           COMP-3.                  
005600*                                 ANTAL LEV. ART TILL SPECIAL             
005700*                                 PRIS RULLANDE ÅR (AF2)                  
005800     03 SULEVANT-RAAR-RAB    PIC S9(9)           COMP-3.                  
005900*                                 ANTAL LEVERERADE ARTIKLAR TILL          
006000*                                 RABATT/FKNGRP RULLANDE ÅR (AF2)         
006100     03 SULEVANT-RAAR-MAN    PIC S9(9)           COMP-3.                  
006200*                                 ANT LEV ART TILL MANUELLT               
006300*                                 PRIS RULLANDE ÅR (AF2)                  
006400     03 SULEVANT-RAAR-KRE    PIC S9(9)           COMP-3.                  
006500*                                 ANTAL LEV ART SOM KREDITERATS           
006600*                                 INNEVARANDE RULLANDE ÅR (AF2)           
006700     03 SULEVANT-FAAR        PIC S9(9)           COMP-3.                  
006800*                                 ANTAL LEV ART "HITTILLS I ÅR"           
006900*                                 MEN FÖREGÅENDE ÅR (AF3)                 
007000     03 SULEVANT-RAAR        PIC S9(9)           COMP-3.                  
007100*                                 ANTAL LEV ART RULLANDE ÅR               
007200*                                 (AF2)                                   
007300     03 SULEVANT-FRAAR       PIC S9(9)           COMP-3.                  
007400*                                 ANTAL LEVERERADE ARTIKLAR               
007500*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
007600     03 SUTOTBV-PER          PIC S9(11)V9(2)     COMP-3.                  
007700*                                 TÄCKNINGSB SENASTE PERIOD (AF5)         
007800     03 SUTOTBV-AAR          PIC S9(11)V9(2)     COMP-3.                  
007900*                                 TÄCKNINGSB HITTILLS I ÅR  (AF4)         
008000     03 SUTOTBV-FAAR         PIC S9(11)V9(2)     COMP-3.                  
008100*                                 TÄCKNINGSBIDRAG "HITTILLS I ÅR"         
008200*                                 MEN FÖREGÅENDE ÅR (AF3)                 
008300     03 SUTOTBV-RAAR         PIC S9(11)V9(2)     COMP-3.                  
008400*                                 TÄCKNINGSB RULLANDE ÅR    (AF2)         
008500     03 SUTOTBV-FRAAR        PIC S9(11)V9(2)     COMP-3.                  
008600*                                 TÄCKNINGSBIDRAG FÖR RULLANDE            
008700*                                 FÖREGÅENDE ÅR (AF1)                     
008800*** END COPY W33027CCC0  LENGTH=139                                       

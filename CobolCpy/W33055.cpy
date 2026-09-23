000100 01  W33055.                                                              
000200*                                 COPYTEXT FÖR ARTIKELSTATISTIK           
000300*                                 SUMMERADE POSTER AV S1-TYP              
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
002100     03 005-GRUPP.                                                        
002200*                                 ARTIKELSTATISTIK                        
002300*                                 OBS DENNA GRUPP ANVÄNDS I               
002400*                                 FLERA COPYTEXTER                        
002500        05 KDPRODSL          PIC S9(3)           COMP-3.                  
002600*                                 PRODUKTSLAG                             
002700        05 IDRADNR           PIC S9(5)           COMP-3.                  
002800*                                 RADNUMMER                               
002900        05 IDARTNR           PIC S9(9)           COMP-3.                  
003000*                                 ARTIKELNUMMER                           
003100        05 BEART-SVE         PIC X(25).                                   
003200*                                 SVENSK ARTIKELBENÄMNING                 
003300        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
003400*                                 FUNKTIONSGRUPP                          
003500        05 SUARTFSG-RAAR     PIC S9(9)V9(2)      COMP-3.                  
003600*                                 SUMMA FSG/ART  RULLANDE ÅR              
003700*                                 (AF2)                                   
003800        05 SULEVANT-RAAR     PIC S9(9)           COMP-3.                  
003900*                                 ANTAL LEV ART RULLANDE ÅR               
004000*                                 (AF2)                                   
004100        05 SULEVANT-FRAAR    PIC S9(9)           COMP-3.                  
004200*                                 ANTAL LEVERERADE ARTIKLAR               
004300*                                 FÖREGÅENDE RULLANDE ÅR (AF1)            
004400        05 RELEVANT-RAAR     PIC S9(3)V9(1)      COMP-3.                  
004500*                                 ANTALSAVVIKELSE (AF2 - AF1)             
004600        05 SUARTSJK-RAAR     PIC S9(9)V9(2)      COMP-3.                  
004700*                                 SUM (SJK * KVANT) RULLANDE ÅR           
004800        05 RETOTBV-RAAR      PIC S9(2)V9(1)      COMP-3.                  
004900*                                 TG RULLANDE ÅR     (AF2)                
005000*** END OF VILMAII-COPY LENGTH= 97 BYTES                                  

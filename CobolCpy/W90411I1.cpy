000100 01  MID-W90411I1.                                                        
000200*                                 MID-COPYTEXT FÖR W1011700               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-INDEL.                                                        
000800*                                 INRAPPORTERINGSDEL 9411                 
000900        05 MID-INDEL2.                                                    
001000*                                 INRAPPORTERINGSDEL 9411                 
001100           07 MID-IDBERED    PIC X(2).                                    
001200*                                 BEREDARENUMMER                          
001300           07 MID-KDPRODSL   PIC X(2).                                    
001400*                                 PRODUKTSLAG                             
001500           07 MID-KDSORT     PIC X(2).                                    
001600*                                 SORT-KOD                                
001700           07 MID-IDPROENH   OCCURS 3 TIMES                               
001800                             INDEXED MID-IDPROENH-IND                     
001900                             PIC X(8).                                    
002000*                                 PRODUKTIONSENHET                        
002100           07 MID-KDUART     PIC X.                                       
002200*                                 UNDANTAGSARTIKEL                        
002300           07 MID-IDPROJ     PIC X(4).                                    
002400*                                 PARTS PROJEKTIDENTITET                  
002500           07 MID-FLPISK     PIC X.                                       
002600*                                 PISK ARTIKEL                            
002700           07 MID-IDKAT      OCCURS 3 TIMES                               
002800                             INDEXED MID-IDKAT-IND                        
002900                             PIC X(5).                                    
003000*                                 KATALOGBETECKNING                       
003100           07 MID-FLLSRDEL   PIC X.                                       
003200*                                 LEVERERAS SOM RESDEL                    
003300           07 MID-IDPROJK    PIC X(4).                                    
003400*                                 PROJEKTIDENTITET KONSTRUKTION           
003500           07 MID-KDBPSR     PIC 9.                                       
003600*                                 BASLAGERFÖRSLAGSNIVÅ                    
003700           07 MID-IDFKNGRP   PIC 9(4).                                    
003800*                                 FUNKTIONSGRUPP                          
003900           07 MID-IDPROJUP   PIC X(8).                                    
004000*                                 PROJEKTUPPDRAG                          
004100           07 MID-IDARTNR-MOTSV                                           
004200                             PIC X(9).                                    
004300*                                 MOTSVARANDE ARTIKEL                     
004400           07 MID-FILLER     PIC X.                                       
004500           07 MID-IDRITN     PIC X(10).                                   
004600*                                 RITNINGSNUMMER                          
004700           07 MID-KVARTVAGN  PIC 9(3).                                    
004800*                                 ANTAL ARTIKLAR PER VAGN                 
004900           07 MID-FILLER     PIC 9(7).                                    
005000           07 MID-KDAGE      PIC X.                                       
005100*                                 AGE-CODE                                
005200           07 MID-IDAO       OCCURS 5 TIMES                               
005300                             INDEXED MID-IDAO-IND                         
005400                             PIC X(10).                                   
005500*                                 ÄNDRINGSORDERNUMMER                     
005600           07 MID-TEORSAK    PIC X(50).                                   
005700*                                 INFO OM SLAG AV ÅTGÄRD                  
005800           07 MID-FILLER     PIC X(40).                                   
005900        05 MID-TISOP         PIC 9(5).                                    
006000*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
006100*** END OF VILMAII-COPY LENGTH= 263 BYTES                                 

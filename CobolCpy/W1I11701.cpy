000100 01  MID-W1I11701.                                                        
000200*                                 MID-COPYTEXT FÖR W1011700               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-INDEL.                                                        
000800*                                 INRAPPORTERINGSDEL 1117                 
000900        05 MID-INDEL2.                                                    
001000*                                 INRAPPORTERINGSDEL 1117                 
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
004400           07 MID-FLBYTES    PIC X.                                       
004500*                                 BYTESARTIKEL                            
004600           07 MID-IDRITN     PIC X(10).                                   
004700*                                 RITNINGSNUMMER                          
004800           07 MID-KVARTVAGN  PIC 9(3).                                    
004900*                                 ANTAL ARTIKLAR PER VAGN                 
005000           07 MID-KVPROG     PIC 9(7).                                    
005100*                                 ÅRSPROGNOS                              
005200           07 MID-KDAGE      PIC X.                                       
005300*                                 AGE-CODE                                
005400           07 MID-IDAO       OCCURS 5 TIMES                               
005500                             INDEXED MID-IDAO-IND                         
005600                             PIC X(10).                                   
005700*                                 ÄNDRINGSORDERNUMMER                     
005800           07 MID-TEORSAK    PIC X(50).                                   
005900*                                 INFO OM SLAG AV ÅTGÄRD                  
006000           07 MID-TEVARNOT   PIC X(40).                                   
006100*                                 ARTIKEL NOTERING                        
006200        05 MID-TISOP         PIC 9(4).                                    
006300*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
006400*** END OF VILMAII-COPY LENGTH= 262 BYTES                                 

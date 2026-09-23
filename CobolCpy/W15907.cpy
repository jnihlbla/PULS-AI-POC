000100 01  W15907.                                                              
000200*                                 ARTIKELDATA EXTRAKT                     
000300*                                 TILL NEVIS POSTTYP P01                  
000400*                                 OBS                                     
000500*                                 ALFANUM IDINK  IDINK-OLD                
000600     03 W15901.                                                           
000700*                                 ARTIKELDATA EXTRAKT                     
000800*                                 FÖR MATCHNING MOT NEVIS-REG             
000900*                                 OBS                                     
001000*                                 NUMERISK IDINK = IDINK-OLD              
001100        05 IDARTNR           PIC 9(9).                                    
001200*                                 ARTIKELNUMMER                           
001300        05 REKSIFFR          PIC 9.                                       
001400*                                 KONTROLLSIFFRA                          
001500        05 FLLSRDEL          PIC X.                                       
001600*                                 LEVERERAS SOM RESDEL                    
001700        05 IDFKNGRP          PIC 9(4).                                    
001800*                                 FUNKTIONSGRUPP                          
001900        05 FLIART            PIC X.                                       
002000*                                 ARTIKELN INGÅR I SATS                   
002100        05 IDBERED           PIC 9(3).                                    
002200*                                 BEREDARENUMMER                          
002300        05 IDAO-1            PIC X(10).                                   
002400*                                 ÄNDRINGSORDERNUMMER                     
002500        05 IDAO-2            PIC X(10).                                   
002600*                                 ÄNDRINGSORDERNUMMER                     
002700        05 IDAO-3            PIC X(10).                                   
002800*                                 ÄNDRINGSORDERNUMMER                     
002900        05 IDAO-4            PIC X(10).                                   
003000*                                 ÄNDRINGSORDERNUMMER                     
003100        05 IDAO-5            PIC X(10).                                   
003200*                                 ÄNDRINGSORDERNUMMER                     
003300        05 IDINK-OLD         PIC 9(3).                                    
003400*                                 INKÖPARNUMMER                           
003500        05 IDPROENH-1        PIC X(8).                                    
003600*                                 PRODUKTIONSENHET                        
003700        05 IDPROENH-2        PIC X(8).                                    
003800*                                 PRODUKTIONSENHET                        
003900        05 IDPROENH-3        PIC X(8).                                    
004000*                                 PRODUKTIONSENHET                        
004100        05 IDPROJ            PIC X(4).                                    
004200*                                 PARTS PROJEKTIDENTITET                  
004300        05 IDPROJUP          PIC X(8).                                    
004400*                                 PROJEKTUPPDRAG                          
004500        05 IDRITN            PIC X(10).                                   
004600*                                 RITNINGSNUMMER                          
004700        05 KDBPSR            PIC 9.                                       
004800*                                 BASLAGERFÖRSLAGSNIVÅ                    
004900        05 KDERS             PIC 9(3).                                    
005000*                                 ERSÄTTNINGSKOD                          
005100        05 KDERS-UTG         PIC 9(3).                                    
005200*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
005300        05 KDPRODSL          PIC 9(3).                                    
005400*                                 PRODUKTSLAG                             
005500        05 KDSORT            PIC X(2).                                    
005600*                                 SORT-KOD                                
005700        05 KDUART            PIC X.                                       
005800*                                 UNDANTAGSARTIKEL                        
005900        05 TIERSDAT          PIC 9(5).                                    
006000*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
006100        05 TIFINLV           PIC 9(5).                                    
006200*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
006300        05 TIREGDAT          PIC 9(6).                                    
006400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006500        05 TEARTNOT-3        PIC X(40).                                   
006600*                                 ARTIKEL NOTERING                        
006700        05 TEARTNOT-7        PIC X(40).                                   
006800*                                 ARTIKEL NOTERING                        
006900     03 FLNOSTOCK            PIC X.                                       
007000*** END OF VILMAII-COPY LENGTH= 228 BYTES                                 

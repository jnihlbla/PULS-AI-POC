000100 01  W15902.                                                              
000200*                                 ARTIKELDATA EXTRAKT                     
000300*                                 TILL NEVIS POSTTYP P01                  
000400*                                 OBS                                     
000500*                                 ALFANUM IDINK  IDINK-OLD                
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 KDUPPD               PIC X.                                       
000900*                                 UPPDATERINGSTYP                         
001000     03 W15901.                                                           
001100*                                 ARTIKELDATA EXTRAKT                     
001200*                                 FÖR MATCHNING MOT NEVIS-REG             
001300*                                 OBS                                     
001400*                                 NUMERISK IDINK = IDINK-OLD              
001500        05 IDARTNR           PIC 9(9).                                    
001600*                                 ARTIKELNUMMER                           
001700        05 REKSIFFR          PIC 9.                                       
001800*                                 KONTROLLSIFFRA                          
001900        05 FLLSRDEL          PIC X.                                       
002000*                                 LEVERERAS SOM RESDEL                    
002100        05 IDFKNGRP          PIC 9(4).                                    
002200*                                 FUNKTIONSGRUPP                          
002300        05 FLIART            PIC X.                                       
002400*                                 ARTIKELN INGÅR I SATS                   
002500        05 IDBERED           PIC 9(3).                                    
002600*                                 BEREDARENUMMER                          
002700        05 IDAO-1            PIC X(10).                                   
002800*                                 ÄNDRINGSORDERNUMMER                     
002900        05 IDAO-2            PIC X(10).                                   
003000*                                 ÄNDRINGSORDERNUMMER                     
003100        05 IDAO-3            PIC X(10).                                   
003200*                                 ÄNDRINGSORDERNUMMER                     
003300        05 IDAO-4            PIC X(10).                                   
003400*                                 ÄNDRINGSORDERNUMMER                     
003500        05 IDAO-5            PIC X(10).                                   
003600*                                 ÄNDRINGSORDERNUMMER                     
003700        05 IDINK-OLD         PIC 9(3).                                    
003800*                                 INKÖPARNUMMER                           
003900        05 IDPROENH-1        PIC X(8).                                    
004000*                                 PRODUKTIONSENHET                        
004100        05 IDPROENH-2        PIC X(8).                                    
004200*                                 PRODUKTIONSENHET                        
004300        05 IDPROENH-3        PIC X(8).                                    
004400*                                 PRODUKTIONSENHET                        
004500        05 IDPROJ            PIC X(4).                                    
004600*                                 PARTS PROJEKTIDENTITET                  
004700        05 IDPROJUP          PIC X(8).                                    
004800*                                 PROJEKTUPPDRAG                          
004900        05 IDRITN            PIC X(10).                                   
005000*                                 RITNINGSNUMMER                          
005100        05 KDBPSR            PIC 9.                                       
005200*                                 BASLAGERFÖRSLAGSNIVÅ                    
005300        05 KDERS             PIC 9(3).                                    
005400*                                 ERSÄTTNINGSKOD                          
005500        05 KDERS-UTG         PIC 9(3).                                    
005600*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
005700        05 KDPRODSL          PIC 9(3).                                    
005800*                                 PRODUKTSLAG                             
005900        05 KDSORT            PIC X(2).                                    
006000*                                 SORT-KOD                                
006100        05 KDUART            PIC X.                                       
006200*                                 UNDANTAGSARTIKEL                        
006300        05 TIERSDAT          PIC 9(5).                                    
006400*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
006500        05 TIFINLV           PIC 9(5).                                    
006600*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
006700        05 TIREGDAT          PIC 9(6).                                    
006800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006900        05 TEARTNOT-3        PIC X(40).                                   
007000*                                 ARTIKEL NOTERING                        
007100        05 TEARTNOT-7        PIC X(40).                                   
007200*                                 ARTIKEL NOTERING                        
007300     03 FLNOSTOCK            PIC X.                                       
007400*** END OF VILMAII-COPY LENGTH= 232 BYTES                                 

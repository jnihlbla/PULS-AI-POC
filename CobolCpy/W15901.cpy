000100 01  W15901.                                                              
000200*                                 ARTIKELDATA EXTRAKT                     
000300*                                 FÖR MATCHNING MOT NEVIS-REG             
000400*                                 OBS                                     
000500*                                 NUMERISK IDINK = IDINK-OLD              
000600     03 IDARTNR              PIC 9(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 REKSIFFR             PIC 9.                                       
000900*                                 KONTROLLSIFFRA                          
001000     03 FLLSRDEL             PIC X.                                       
001100*                                 LEVERERAS SOM RESDEL                    
001200     03 IDFKNGRP             PIC 9(4).                                    
001300*                                 FUNKTIONSGRUPP                          
001400     03 FLIART               PIC X.                                       
001500*                                 ARTIKELN INGÅR I SATS                   
001600     03 IDBERED              PIC 9(3).                                    
001700*                                 BEREDARENUMMER                          
001800     03 IDAO-1               PIC X(10).                                   
001900*                                 ÄNDRINGSORDERNUMMER                     
002000     03 IDAO-2               PIC X(10).                                   
002100*                                 ÄNDRINGSORDERNUMMER                     
002200     03 IDAO-3               PIC X(10).                                   
002300*                                 ÄNDRINGSORDERNUMMER                     
002400     03 IDAO-4               PIC X(10).                                   
002500*                                 ÄNDRINGSORDERNUMMER                     
002600     03 IDAO-5               PIC X(10).                                   
002700*                                 ÄNDRINGSORDERNUMMER                     
002800     03 IDINK-OLD            PIC 9(3).                                    
002900*                                 INKÖPARNUMMER                           
003000     03 IDPROENH-1           PIC X(8).                                    
003100*                                 PRODUKTIONSENHET                        
003200     03 IDPROENH-2           PIC X(8).                                    
003300*                                 PRODUKTIONSENHET                        
003400     03 IDPROENH-3           PIC X(8).                                    
003500*                                 PRODUKTIONSENHET                        
003600     03 IDPROJ               PIC X(4).                                    
003700*                                 PARTS PROJEKTIDENTITET                  
003800     03 IDPROJUP             PIC X(8).                                    
003900*                                 PROJEKTUPPDRAG                          
004000     03 IDRITN               PIC X(10).                                   
004100*                                 RITNINGSNUMMER                          
004200     03 KDBPSR               PIC 9.                                       
004300*                                 BASLAGERFÖRSLAGSNIVÅ                    
004400     03 KDERS                PIC 9(3).                                    
004500*                                 ERSÄTTNINGSKOD                          
004600     03 KDERS-UTG            PIC 9(3).                                    
004700*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
004800     03 KDPRODSL             PIC 9(3).                                    
004900*                                 PRODUKTSLAG                             
005000     03 KDSORT               PIC X(2).                                    
005100*                                 SORT-KOD                                
005200     03 KDUART               PIC X.                                       
005300*                                 UNDANTAGSARTIKEL                        
005400     03 TIERSDAT             PIC 9(5).                                    
005500*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
005600     03 TIFINLV              PIC 9(5).                                    
005700*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
005800     03 TIREGDAT             PIC 9(6).                                    
005900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006000     03 TEARTNOT-3           PIC X(40).                                   
006100*                                 ARTIKEL NOTERING                        
006200     03 TEARTNOT-7           PIC X(40).                                   
006300*                                 ARTIKEL NOTERING                        
006400*** END OF VILMAII-COPY LENGTH= 227 BYTES                                 

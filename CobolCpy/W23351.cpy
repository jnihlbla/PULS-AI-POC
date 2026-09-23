000100 01  W23351.                                                              
000200*                                 DAGLIGA TRANSAR TILL NEDCAR             
000300*                                 INSAMLAT DATA                           
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 REKSIFFR             PIC S9              COMP-3.                  
000700*                                 KONTROLLSIFFRA                          
000800     03 KDERS-UTG            PIC S9(3)           COMP-3.                  
000900*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
001000     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001100*                                 PRODUKTSLAG                             
001200     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001300*                                 FUNKTIONSGRUPP                          
001400     03 IDLEVNR              PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600     03 TIFINLV              PIC S9(5)           COMP-3.                  
001700*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
001800     03 KDSORT               PIC X(2).                                    
001900*                                 SORT-KOD                                
002000     03 IDAO-1               PIC X(10).                                   
002100*                                 ÄNDRINGSORDERNUMMER                     
002200     03 IDAO-2               PIC X(10).                                   
002300*                                 ÄNDRINGSORDERNUMMER                     
002400     03 IDPROJ               PIC X(4).                                    
002500*                                 PARTS PROJEKTIDENTITET                  
002600     03 IDBERED              PIC S9(3)           COMP-3.                  
002700*                                 BEREDARENUMMER                          
002800     03 IDRITN               PIC X(10).                                   
002900*                                 RITNINGSNUMMER                          
003000     03 IDKAT-1              PIC X(5).                                    
003100*                                 KATALOGBETECKNING                       
003200     03 IDKAT-2              PIC X(5).                                    
003300*                                 KATALOGBETECKNING                       
003400     03 IDKAT-3              PIC X(5).                                    
003500*                                 KATALOGBETECKNING                       
003600     03 IDPROENH-1           PIC X(8).                                    
003700*                                 PRODUKTIONSENHET                        
003800     03 TIURPROD             PIC S9(5)           COMP-3.                  
003900*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
004000     03 KDHF                 PIC S9              COMP-3.                  
004100*                                 HUVUDFÖRRÅDSMÄRKNING                    
004200     03 KDAVT                PIC S9              COMP-3.                  
004300*                                 AVTALSMÄRKNING                          
004400     03 BERNOT               PIC X(40).                                   
004500*                                 ARTIKEL NOTERING                        
004600     03 VARNOT               PIC X(40).                                   
004700*                                 ARTIKEL NOTERING                        
004800     03 IDKAT                PIC X(5).                                    
004900*                                 KATALOGBETECKNING                       
005000     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
005100*                                 ANTAL I Q1 FÖRPACKNING                  
005200     03 KDGK                 PIC S9              COMP-3.                  
005300*                                 GODSMOTTAGAREKOD                        
005400     03 FLLSRDEL             PIC X.                                       
005500*                                 LEVERERAS SOM RESDEL                    
005600     03 BEFT                 PIC S9(3)           COMP-3.                  
005700*                                 FÖRPACKNINGSTYP                         
005800     03 KDERS                PIC S9(3)           COMP-3.                  
005900*                                 ERSÄTTNINGSKOD                          
006000     03 KVPROG               PIC S9(7)           COMP-3.                  
006100*                                 ÅRSPROGNOS                              
006200     03 BEART-ENG            PIC X(25).                                   
006300*                                 ENGELSK ARTIKELBENÄMNING                
006400     03 BELEVART             PIC X(30).                                   
006500*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
006600*** END OF VILMAII-COPY LENGTH= 240 BYTES                                 

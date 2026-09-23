000100 01  KVAN-W411KVAN.                                                       
000200*                                 LÄNKAREA TILL W411KVAN -                
000300*                                 BESTÄMMER KVANTANPASSNING               
000400     03 KVAN-INDATA.                                                      
000500        05 KVAN-KDKVBRYT-IN  PIC S9              COMP-3.                  
000600*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
000700        05 KVAN-IDSYSTEM-IN  PIC X(4).                                    
000800*                                 VOLVO VCCS SYSTEMNUMMER                 
000900        05 KVAN-KVBEART-IN   PIC S9(7)           COMP-3.                  
001000*                                 BESTÄLLT ANTAL STYCKEN                  
001100        05 KVAN-KVQPACK-0-IN PIC S9(5)           COMP-3.                  
001200*                                 ANTAL I Q0 FÖRPACKNING                  
001300        05 KVAN-KVQPACK-1-IN PIC S9(5)           COMP-3.                  
001400*                                 ANTAL I Q1 FÖRPACKNING                  
001500        05 KVAN-KDPRODSL-IN  PIC S9(3)           COMP-3.                  
001600*                                 PRODUKTSLAG                             
001700        05 KVAN-KDSORT-IN    PIC X(2).                                    
001800*                                 SORT-KOD                                
001900        05 KVAN-KDORDKL-IN   PIC S9              COMP-3.                  
002000*                                 ORDERKLASS                              
002100        05 KVAN-FLEMBORD-IN  PIC X.                                       
002200*                                 EMBALLAGEORDER ?                        
002300        05 KVAN-FLFORBI-IN   PIC X.                                       
002400*                                 FÖRBIORDERFLAGGA                        
002500        05 KVAN-FLORDSPE-IN  PIC X.                                       
002600*                                 SPECIALORDERFLAGGA                      
002700        05 KVAN-FLOVRLEV-IN  PIC X.                                       
002800*                                 ÖVERLEVERANS                            
002900        05 KVAN-IDKAMPRF-IN  PIC S9(7)           COMP-3.                  
003000*                                 KAMPANJREFERENS                         
003100        05 KVAN-IDDC-IN      PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300        05 KVAN-IDDISTR-IN   PIC S9(5)           COMP-3.                  
003400*                                 DISTRIKTNUMMER                          
003500        05 KVAN-IDKUNDNR-IN  PIC S9(7)           COMP-3.                  
003600*                                 KUNDNUMMER                              
003700        05 KVAN-BERADREF-IN  PIC X(10).                                   
003800*                                 KUNDENS RADREFERENS                     
003900        05 KVAN-IDFKNGRP-IN  PIC S9(5)           COMP-3.                  
004000*                                 FUNKTIONSGRUPP                          
004100        05 KVAN-IDARTNR-IN   PIC S9(9)           COMP-3.                  
004200*                                 ARTIKELNUMMER                           
004300     03 KVAN-UTDATA.                                                      
004400        05 KVAN-KDKVBRYT-UT  PIC S9              COMP-3.                  
004500*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004600        05 KVAN-KVBEART-Q-UT PIC S9(7)           COMP-3.                  
004700*                                 BESTÄLLT KVANTANPASSAT ANTAL            
004800        05 KVAN-KVQPACK-UT   PIC S9(5)           COMP-3.                  
004900*                                 ANTAL KVANTITETFÖRPACKNINGAR            
005000        05 KVAN-KDORDBEK-UT  PIC 9(2).                                    
005100*                                 ORDERBEKRÄFTELSEKOD                     
005200*** END OF VILMAII-COPY LENGTH= 65 BYTES                                  

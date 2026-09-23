000100 01  W27277.                                                              
000200*                                 COPYTEXT TILL FILEN W27277,             
000300*                                 DATA FRÅN WDK6                          
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
000700*                                 FUNKTIONSGRUPP                          
000800     03 KDPRODSL             PIC S9(3)           COMP-3.                  
000900*                                 PRODUKTSLAG                             
001000     03 KDFARLIG             PIC S9              COMP-3.                  
001100*                                 KOD FÖR FARLIGT GODS                    
001200     03 KDUART               PIC X.                                       
001300*                                 UNDANTAGSARTIKEL                        
001400     03 IDPROJ               PIC X(4).                                    
001500*                                 PARTS PROJEKTIDENTITET                  
001600     03 IDPSN                PIC 9(3).                                    
001700*                                 PROPER SHIPPING NAME                    
001800     03 FILLER               PIC X(3).                                    
001900     03 IDANSK               PIC S9(3)           COMP-3.                  
002000*                                 ANSKAFFARNUMMER                         
002100     03 IDLEVNR              PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300     03 IDSTATNR             PIC S9(9)           COMP-3.                  
002400*                                 STATISTISKT NUMMER                      
002500*                                 1 = NORSKT                              
002600*                                 2 = ENGELSKT                            
002700*                                 3 = BELGISKT                            
002800*                                 4 = PERUANSKT                           
002900*                                 5 = SVENSKT                             
003000*                                 6 =                                     
003100     03 IDDC-REF             PIC X(2).                                    
003200*                                 SÄNDANDE LAGER FÖR REFILL               
003300     03 FLFLYG               PIC X.                                       
003400*                                 FLYGARTIKEL                             
003500     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
003600*                                 PERSONKOD REFILLANSVARIG                
003700     03 FLBUYUPD             PIC X.                                       
003800*                                 OM IDPERSONKOD ÄR LÅST                  
003900     03 FLTABUPD             PIC X.                                       
004000*                                 OM REFILLTABELL ÄR LÅST                 
004100     03 IDREFTAB             PIC X.                                       
004200*                                 IDENTITET REFILLTABELL                  
004300     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
004400*                                 ARTIKELVOLYM (CM3)                      
004500     03 KVPB-TOT             PIC S9(6)V9(1)      COMP-3.                  
004600*                                 TOTALT PERIODBEHOV                      
004700     03 TISOP                PIC S9(5)           COMP-3.                  
004800*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
004900     03 TIURPROD             PIC S9(5)           COMP-3.                  
005000*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
005100     03 FLBSNES              PIC X.                                       
005200*                                 ARTIKEL MED MER AFFÄRSVÄRDE             
005300     03 KVEOP                PIC 9(2).                                    
005400*                                 ANTAL ÅR KVAR FÖR ART EFTER EOP         
005500*** END OF VILMAII-COPY LENGTH= 60 BYTES                                  

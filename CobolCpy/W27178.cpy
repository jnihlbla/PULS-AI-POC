000100 01  W27178.                                                              
000200*                                 COPYTEXT TILL FILEN W27178,             
000300*                                 DATA FRÅN WDK7                          
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 IDLEVNR              PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
001100*                                 PERSONKOD REFILLANSVARIG                
001200     03 IDREFTAB             PIC X.                                       
001300*                                 IDENTITET REFILLTABELL                  
001400     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
001500*                                 LAGEROMRÅDE                             
001600     03 FLFLYG               PIC X.                                       
001700*                                 FLYGARTIKEL                             
001800     03 IDDC-REF             PIC X(2).                                    
001900*                                 SÄNDANDE LAGER FÖR REFILL               
002000     03 FLBUYUPD             PIC X.                                       
002100*                                 OM IDPERSONKOD ÄR LÅST                  
002200     03 FLTABUPD             PIC X.                                       
002300*                                 OM REFILLTABELL ÄR LÅST                 
002400     03 KVPB-REF-REOI-SUM    PIC S9(6)V9(1)      COMP-3.                  
002500*                                 TOTALT PERIODBEHOV                      
002600*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  

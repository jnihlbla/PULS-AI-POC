000100 01  SDCA-W411SDCA.                                                       
000200*                                 LÄNKAREA TILL W411SDCA -                
000300*                                 KONTROLL AV PRELIMINÄRBOKNING           
000400*                                 AV EN SDC-RAD                           
000500     03 SDCA-IXDCCLEAR       PIC 9.                                       
000600*                                 CLEARING DC SEKVENS                     
000700     03 SDCA-ADLAGOMR        PIC S9(3)           COMP-3.                  
000800*                                 LAGEROMRÅDE                             
000900     03 SDCA-ADGANG          PIC S9(3)           COMP-3.                  
001000*                                 GÅNG                                    
001100     03 SDCA-ADPLATS         PIC S9(5)           COMP-3.                  
001200*                                 LAGERPLATSNUMMER                        
001300     03 SDCA-FLFORBI         PIC X.                                       
001400*                                 FÖRBIORDERFLAGGA                        
001500     03 SDCA-FLORDSPE        PIC X.                                       
001600*                                 SPECIALORDERFLAGGA                      
001700     03 SDCA-FLREFILL        PIC X.                                       
001800*                                 REFILLARTIKEL                           
001900     03 SDCA-IDARTNR         PIC S9(9)           COMP-3.                  
002000*                                 ARTIKELNUMMER                           
002100     03 SDCA-IDDC            PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 SDCA-IDDC-TVS        PIC X(2).                                    
002400*                                 DISTRIBUTIONCENTER                      
002500*                                 TVÅNGSSTYRNING                          
002600     03 SDCA-IDDISTR         PIC S9(5)           COMP-3.                  
002700*                                 DISTRIKTNUMMER                          
002800     03 SDCA-IDLEVNR         PIC X(5).                                    
002900*                                 LEVERANTÖRNUMMER                        
003000     03 SDCA-IDSYSTEM        PIC X(4).                                    
003100*                                 VOLVO VCCS SYSTEMNUMMER                 
003200     03 SDCA-KDORDKL         PIC S9              COMP-3.                  
003300*                                 ORDERKLASS                              
003400     03 SDCA-KDOI            PIC X(2).                                    
003500*                                 ORDERINGÅNGSTYP                         
003600     03 SDCA-KDSORT          PIC X(2).                                    
003700*                                 SORT-KOD                                
003800     03 SDCA-CLEARGROUP.                                                  
003900*                                 CLEARINGAREA FÖR ORDERINGÅNG            
004000        05 SDCA-CLEARAREA    OCCURS 7 TIMES.                              
004100*                                 CLEARINGAREA FÖR ORDERINGÅNG            
004200           07 SDCA-IDDC-CLEAR                                             
004300                             PIC X(2).                                    
004400*                                 LAGERPRIORITERING VID                   
004500*                                 ORDERCLEARING                           
004600           07 SDCA-FLLF      PIC X.                                       
004700*                                 ARTIKEL LAGERFÖRES                      
004800           07 SDCA-FLCLEAR   PIC X.                                       
004900*                                 ORDERRAD CLEAR FLAGGA                   
005000     03 SDCA-KDORDBEK        PIC 9(2).                                    
005100*                                 ORDERBEKRÄFTELSEKOD                     
005200     03 SDCA-KDORDBEK-FIRST-SDC                                           
005300                             PIC 9(2).                                    
005400*                                 ORDERBEKRÄFTELSEKOD                     
005500     03 SDCA-KDORDBEK-SECOND-SDC                                          
005600                             PIC 9(2).                                    
005700*                                 ORDERBEKRÄFTELSEKOD                     
005800     03 SDCA-KDORDING        PIC S9              COMP-3.                  
005900*                                 UPPDATERING ORDERINGÅNG                 
006000     03 SDCA-KDPRODSL        PIC S9(3)           COMP-3.                  
006100*                                 PRODUKTSLAG                             
006200     03 SDCA-KVBEART-Q       PIC S9(7)           COMP-3.                  
006300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
006400     03 SDCA-KVQPACK-1       PIC S9(5)           COMP-3.                  
006500*                                 ANTAL I Q1 FÖRPACKNING                  
006600     03 SDCA-REDIRLEV        PIC S9V9(2)         COMP-3.                  
006700*                                 DIREKTLEVERANSANDEL                     
006800     03 SDCA-FLSDCLEV        PIC X.                                       
006900*                                 LEVERANSSTYRNING SDC                    
007000     03 SDCA-TIREPDAT        PIC S9(7)           COMP-3.                  
007100*                                 REPAIR DATE                             
007200     03 SDCA-KVOKS-PREL      PIC S9(7)           COMP-3.                  
007300*                                 PREL ORDERKÖSALDO VERKST.ORDER          
007400     03 SDCA-KDCALL          PIC S9(3)           COMP-3.                  
007500*                                 ANROPSTYP                               
007600*** END OF VILMAII-COPY LENGTH= 94 BYTES                                  

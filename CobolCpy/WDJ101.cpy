000100 01  STR-WDJ101.                                                          
000200*                                 SATSSTRUKTUWREGISTER                    
000300*                                 SATSSTRUKTURHUVUD SEGMENT               
000400*                                 FYSISK NYCKEL: IDARTNR                  
000500*                                 SÖKBEGREPP: IDLEVNR, IDUSER             
000600     03 STR-IDARTNR          PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 STR-BEART-SVE        PIC X(25).                                   
001000*                                 SVENSK ARTIKELBENÄMNING                 
001100     03 STR-FLEXFORP         PIC X.                                       
001200*                                 FLAGGA FÖR EXTERNFÖRPACKNING            
001300*                                 EXTERNAL PACKING FLAG                   
001400     03 STR-FLFORPQ          PIC X.                                       
001500*                                 FLAGGA FÖR FÖRPACKNINGENS KÖ            
001600*                                 PARTS PACKING QUEUE FLAG                
001700     03 STR-IDFKNGRP         PIC S9(5)           COMP-3.                  
001800*                                 FUNKTIONSGRUPP                          
001900*                                 FUNCTION GROUP                          
002000     03 STR-IDLEVNR          PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002300     03 STR-IDSTRTYP         PIC X.                                       
002400*                                 STRUKTURTYP                             
002500*                                 TYPE OF STRUCTURE                       
002600     03 STR-IDTSPEC          PIC X(15).                                   
002700*                                 LIKARTADE STRUKTURER                    
002800*                                 SIMILAR STRUCTURES                      
002900     03 STR-IDUSER           PIC X(8).                                    
003000*                                 ANVÄNDARENS SÄKERHETS ID                
003100*                                 USER SECURITY-IDENTITY                  
003200     03 STR-KDBENHOM         PIC S9              COMP-3.                  
003300*                                 HOMONYMKOD                              
003400*                                 HOMONYMOUS CODE                         
003500     03 STR-KDPRODSL         PIC S9(3)           COMP-3.                  
003600*                                 PRODUKTSLAG                             
003700*                                 PRODUCT GROUP                           
003800     03 STR-TIBORT           PIC S9(7)           COMP-3.                  
003900*                                 BORTTAGSDATUM  (ÅÅMMDD)                 
004000*                                 DELETE DATE    (YYMMDD)                 
004100     03 STR-TIREGDAT         PIC S9(7)           COMP-3.                  
004200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004300*                                 REGISTRATION DATE (YYMMDD)              
004400     03 STR-TIUPPDAT         PIC S9(7)           COMP-3.                  
004500*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
004600*                                 UPDATING DATE     (YYMMDD)              
004700     03 STR-TESTRNOT         OCCURS 2 TIMES                               
004800                             PIC X(70).                                   
004900*                                 STRUKTURNOTERING                        
005000*                                 STRUCTURE INFORMATION                   
005100     03 STR-KVBYGMIN         PIC S9(3)           COMP-3.                  
005200*                                 BYGGTID SATS MIN                        
005300*                                 PACKINGTIME KIT MIN                     
005400     03 STR-FILLER           PIC X(9).                                    
005500*** END OF VILMAII-COPY LENGTH= 230 BYTES                                 

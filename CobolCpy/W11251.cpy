000100 01  W11251.                                                              
000200*                                 COPYTEXT TILL FIL W11250                
000300*                                 HISTORIK STRUKTURER                     
000400*                                 INNEHÅLLER UPPGIFTER OM                 
000500*                                 STRUKTURNUMMER                          
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 BEART-SVE            PIC X(25).                                   
001100*                                 SVENSK ARTIKELBENÄMNING                 
001200     03 FLEXFORP             PIC X.                                       
001300*                                 FLAGGA FÖR EXTERNFÖRPACKNING            
001400     03 FLFORPQ              PIC X.                                       
001500*                                 FLAGGA FÖR FÖRPACKNINGENS KÖ            
001600     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001700*                                 FUNKTIONSGRUPP                          
001800     03 IDLEVNR              PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000     03 IDSTRTYP             PIC X.                                       
002100*                                 STRUKTURTYP                             
002200     03 IDTSPEC              PIC X(15).                                   
002300*                                 LIKARTADE STRUKTURER                    
002400     03 IDUSER               PIC X(8).                                    
002500*                                 ANVÄNDARENS SÄKERHETS ID                
002600     03 KDBENHOM             PIC S9              COMP-3.                  
002700*                                 HOMONYMKOD                              
002800     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002900*                                 PRODUKTSLAG                             
003000     03 TIBORT               PIC S9(7)           COMP-3.                  
003100*                                 BORTTAGSDATUM  (ÅÅMMDD)                 
003200     03 TIREGDAT             PIC S9(7)           COMP-3.                  
003300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003400     03 TIUPPDAT             PIC S9(7)           COMP-3.                  
003500*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003600     03 TESTRNOT             OCCURS 2 TIMES                               
003700                             PIC X(70).                                   
003800*                                 STRUKTURNOTERING                        
003900*** END OF VILMAII-COPY LENGTH= 222 BYTES                                 

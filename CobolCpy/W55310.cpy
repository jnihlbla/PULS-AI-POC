000100 01  W55310.                                                              
000200*                                 COPYTEXT FÖR PRISFILER FRÅN             
000300*                                 VCC, NEDCAR, LAND-ROVER M.FL            
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDLEVNR              PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000     03 PRARTBEL             PIC S9(9)           COMP-3.                  
001100*                                 BESTPRIS LEVERANTÖRENS VALUTA           
001200     03 KDANTENH             PIC X.                                       
001300*                                 KOD ANTAL PER ARTIKEL                   
001400     03 KDFPKPRI             PIC X.                                       
001500*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
001600     03 TIPRLIST             PIC S9(7)           COMP-3.                  
001700*                                 PRISLISTEDATUM (AAMMDD)                 
001800     03 KDVALISO             PIC X(3).                                    
001900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002000     03 IDUSER               PIC X(8).                                    
002100*                                 ANVÄNDARENS SÄKERHETS ID                
002200*** END OF VILMAII-COPY LENGTH= 35 BYTES                                  

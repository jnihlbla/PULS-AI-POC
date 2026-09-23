000100 01  W55312-CTX.                                                          
000200*                                 COPYTEXT FÖR ARBETSFIL                  
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 IDLEVNR              PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700     03 RETULF               PIC S9(3)V9(4)      COMP-3.                  
000800*                                 TULLFAKTOR                              
000900     03 PRARTBEL-PR          PIC S9(8)V9(5)      COMP-3.                  
001000*                                 DETTA BESTÄLLNINGSPRIS                  
001100*                                 (I LEVERANTÖRENS VALUTA)                
001200     03 KDVALISO             PIC X(3).                                    
001300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001400     03 TIREGDAT             PIC S9(7)           COMP-3.                  
001500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001600     03 FLHUVLEV             PIC X.                                       
001700*                                 HUVUDLEVERANTÖR                         
001800     03 IDUSER               PIC X(8).                                    
001900*                                 ANVÄNDARENS SÄKERHETS ID                
002000     03 KDFPKPRI             PIC X.                                       
002100*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
002200*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  

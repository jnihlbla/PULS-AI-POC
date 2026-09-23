000100 01  W55373-CTX.                                                          
000200*                                 COPYTEXT FÖR ARBETSFIL                  
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDLEVNR              PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 RETULF               PIC S9(3)V9(4)      COMP-3.                  
001000*                                 TULLFAKTOR                              
001100     03 PRARTBEL-PR          PIC S9(8)V9(5)      COMP-3.                  
001200*                                 DETTA BESTÄLLNINGSPRIS                  
001300*                                 (I LEVERANTÖRENS VALUTA)                
001400     03 KDVALISO             PIC X(3).                                    
001500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001600     03 TIREGDAT             PIC S9(7)           COMP-3.                  
001700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001800     03 FLHUVLEV             PIC X.                                       
001900*                                 HUVUDLEVERANTÖR                         
002000     03 IDUSER               PIC X(8).                                    
002100*                                 ANVÄNDARENS SÄKERHETS ID                
002200     03 KDFPKPRI             PIC X.                                       
002300*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
002400*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  

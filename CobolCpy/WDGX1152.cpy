000100 01  1152-WDGX1152.                                                       
000200*                                 LÅSNING AV ART KOMB USER                
000300*                                 FÖR UPPDATERING AV IN-                  
000400*                                 GÅENDE ART I SATSSTRUKTUR               
000500*                                 FYSISK NYCKEL = WDGXKEY                 
000600*                                 (IDLEVNR + BELEVART +                   
000700*                                  IDARTNR + IDUSER   +                   
000800*                                  LOW-VALUE)                             
000900     03 1152-IDLEVNR         PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001200     03 1152-BELEVART        PIC X(30).                                   
001300*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001400*                                 SUPPLIERS PART DESCRIPTION              
001500     03 1152-IDARTNR         PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700*                                 PART NUMBER                             
001800     03 1152-IDUSER          PIC X(8).                                    
001900*                                 ANVÄNDARENS SÄKERHETS ID                
002000*                                 USER SECURITY-IDENTITY                  
002100     03 1152-LOW-VALUE       PIC X(4).                                    
002200     03 1152-FLAGGA          PIC X.                                       
002300*                                 ALLMÄN FLAGGA                           
002400*                                 GENERAL FLAG                            
002500     03 1152-IDAO            PIC X(10).                                   
002600*                                 ÄNDRINGSORDERNUMMER                     
002700*                                 DESIGN CHANGE NOTICE                    
002800     03 1152-TIREGDAT        PIC S9(7)           COMP-3.                  
002900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003000*                                 REGISTRATION DATE (YYMMDD)              
003100     03 1152-TISTODAT        PIC S9(7)           COMP-3.                  
003200*                                 GENERELLT STOPPDATUM                    
003300*                                 GENERAL STOP DATE YYMMDD                
003400     03 FILLER               PIC X(21).                                   
003500*** END OF VILMAII-COPY LENGTH= 92 BYTES                                  

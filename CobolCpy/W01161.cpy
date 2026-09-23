000100 01  PRL-W01161.                                                          
000200*                                 UTDRAG UR WDK621                        
000300     03 PRL-IDARTNR          PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500*                                 PART NUMBER                             
000600     03 PRL-WDK621.                                                       
000700*                                 INFO OM GAMMALT ELLER                   
000800*                                 KOMMANDE BEST.PRIS                      
000900*                                 FYSISK NYCKEL: WDK621KY                 
001000*                                 (DAPRLIST + IDLEVNR )                   
001100        05 PRL-DAPRLIST-9KOMPL                                            
001200                             PIC 9(8).                                    
001300*                                 PRISLISTEDATUM (AAAAMMDD) 9KOMP         
001400*                                 PRICE LIST DATE (AAAAMMDD) 9COM         
001500        05 PRL-IDLEVNR       PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001800        05 PRL-IDUSER        PIC X(8).                                    
001900*                                 ANVÄNDARENS SÄKERHETS ID                
002000*                                 USER SECURITY-IDENTITY                  
002100        05 PRL-FLHUVLEV      PIC X.                                       
002200*                                 HUVUDLEVERANTÖR                         
002300*                                 HEAD SUPPLIER                           
002400        05 PRL-KDPRURSP      PIC X.                                       
002500*                                 PRISHÄRSTAMNING BESTÄLLNING             
002600*                                 ORIGINATE ORDER PRICE                   
002700        05 PRL-KDSTATUS-PR   PIC S9              COMP-3.                  
002800*                                 STATUS PÅ DETTA PRIS                    
002900*                                 0 = PRELIMINÄR  1 = DEFINITIV           
003000        05 PRL-KDVALISO      PIC X(3).                                    
003100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003200*                                 CURRENCY CODE BY ISO-STANDARD.          
003300        05 PRL-PRARTBES-PR   PIC S9(7)V9(2)      COMP-3.                  
003400*                                 DETTA BESTÄLLNINGSPRIS (KR)             
003500        05 PRL-PRARTBEL-PR   PIC S9(8)V9(5)      COMP-3.                  
003600*                                 DETTA BESTÄLLNINGSPRIS                  
003700*                                 (I LEVERANTÖRENS VALUTA)                
003800        05 PRL-SUINLEV-PR    PIC S9(3)           COMP-3.                  
003900*                                 ANTAL INLEV. TILL DETTA PRIS            
004000        05 PRL-KDFPKPRI      PIC X.                                       
004100*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
004200*                                 IF PACKING INCLUDED IN PRICE            
004300        05 PRL-TIREGDAT      PIC S9(7)           COMP-3.                  
004400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004500*                                 REGISTRATION DATE (YYMMDD)              
004600*** END OF VILMAII-COPY LENGTH= 51 BYTES                                  

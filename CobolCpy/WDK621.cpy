000100 01  PRL-WDK621.                                                          
000200*                                 INFO OM GAMMALT ELLER                   
000300*                                 KOMMANDE BEST.PRIS                      
000400*                                 FYSISK NYCKEL: WDK621KY                 
000500*                                 (DAPRLIST + IDLEVNR )                   
000600     03 PRL-DAPRLIST-9KOMPL  PIC 9(8).                                    
000700*                                 PRISLISTEDATUM (AAAAMMDD) 9KOMP         
000800*                                 PRICE LIST DATE (AAAAMMDD) 9COM         
000900     03 PRL-IDLEVNR          PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001200     03 PRL-IDUSER           PIC X(8).                                    
001300*                                 ANVÄNDARENS SÄKERHETS ID                
001400*                                 USER SECURITY-IDENTITY                  
001500     03 PRL-FLHUVLEV         PIC X.                                       
001600*                                 HUVUDLEVERANTÖR                         
001700*                                 HEAD SUPPLIER                           
001800     03 PRL-KDPRURSP         PIC X.                                       
001900*                                 PRISHÄRSTAMNING BESTÄLLNING             
002000*                                 ORIGINATE ORDER PRICE                   
002100     03 PRL-KDSTATUS-PR      PIC S9              COMP-3.                  
002200*                                 STATUS PÅ DETTA PRIS                    
002300*                                 0 = PRELIMINÄR  1 = DEFINITIV           
002400     03 PRL-KDVALISO         PIC X(3).                                    
002500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002600*                                 CURRENCY CODE BY ISO-STANDARD.          
002700     03 PRL-PRARTBES-PR      PIC S9(7)V9(2)      COMP-3.                  
002800*                                 DETTA BESTÄLLNINGSPRIS (KR)             
002900     03 PRL-PRARTBEL-PR      PIC S9(8)V9(5)      COMP-3.                  
003000*                                 DETTA BESTÄLLNINGSPRIS                  
003100*                                 (I LEVERANTÖRENS VALUTA)                
003200     03 PRL-SUINLEV-PR       PIC S9(3)           COMP-3.                  
003300*                                 ANTAL INLEV. TILL DETTA PRIS            
003400     03 PRL-KDFPKPRI         PIC X.                                       
003500*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
003600*                                 IF PACKING INCLUDED IN PRICE            
003700     03 PRL-TIREGDAT         PIC S9(7)           COMP-3.                  
003800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003900*                                 REGISTRATION DATE (YYMMDD)              
004000*** END OF VILMAII-COPY LENGTH= 46 BYTES                                  

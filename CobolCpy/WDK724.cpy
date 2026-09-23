000100 01  SPRL-WDK724.                                                         
000200*                                 INFO OM GAMMALT ELLER KOMMANDE          
000300*                                 BESTÄLLNINGS PRIS FÖR NDC:ER            
000400*                                 FYSISK NYCKEL: WDK724KY                 
000500*                                 (DAPRLIST-9KOMPL + IDLEVNR-PR)          
000600     03 SPRL-DAPRLIST-9KOMPL PIC 9(8).                                    
000700*                                 PRISLISTEDATUM (AAAAMMDD) 9KOMP         
000800*                                 PRICE LIST DATE (AAAAMMDD) 9COM         
000900     03 SPRL-IDLEVNR-PR      PIC X(5).                                    
001000*                                 LEVERANTÖRNR FÖR DETTA PRIS             
001100*                                 SUPPLIER NUMBER FOR THIS PRICE          
001200     03 SPRL-IDUSER          PIC X(8).                                    
001300*                                 ANVÄNDARENS SÄKERHETS ID                
001400*                                 USER SECURITY-IDENTITY                  
001500     03 SPRL-KDFPKPRI        PIC X.                                       
001600*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
001700*                                 IF PACKING INCLUDED IN PRICE            
001800     03 SPRL-KDPRURSP        PIC X.                                       
001900*                                 PRISHÄRSTAMNING BESTÄLLNING             
002000*                                 ORIGINATE ORDER PRICE                   
002100     03 SPRL-KDVALISO        PIC X(3).                                    
002200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002300*                                 CURRENCY CODE BY ISO-STANDARD.          
002400     03 SPRL-PRARTBES-PR     PIC S9(7)V9(2)      COMP-3.                  
002500*                                 DETTA BESTÄLLNINGSPRIS (KR)             
002600     03 SPRL-PRARTBEL-PR     PIC S9(8)V9(5)      COMP-3.                  
002700*                                 DETTA BESTÄLLNINGSPRIS                  
002800*                                 (I LEVERANTÖRENS VALUTA)                
002900     03 SPRL-SUINLEV-PR      PIC S9(3)           COMP-3.                  
003000*                                 ANTAL INLEV. TILL DETTA PRIS            
003100     03 SPRL-TIREGDAT        PIC S9(7)           COMP-3.                  
003200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003300*                                 REGISTRATION DATE (YYMMDD)              
003400*** END OF VILMAII-COPY LENGTH= 44 BYTES                                  

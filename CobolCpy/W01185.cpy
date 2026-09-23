000100 01  SPRL-W01185.                                                         
000200*                                 UTDRAG UR WDK724                        
000300     03 SPRL-IDARTNR         PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500*                                 PART NUMBER                             
000600     03 SPRL-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 SPRL-WDK724.                                                      
001000*                                 INFO OM GAMMALT ELLER KOMMANDE          
001100*                                 BESTÄLLNINGS PRIS FÖR NDC:ER            
001200*                                 FYSISK NYCKEL: WDK724KY                 
001300*                                 (DAPRLIST-9KOMPL + IDLEVNR-PR)          
001400        05 SPRL-DAPRLIST-9KOMPL                                           
001500                             PIC 9(8).                                    
001600*                                 PRISLISTEDATUM (AAAAMMDD) 9KOMP         
001700*                                 PRICE LIST DATE (AAAAMMDD) 9COM         
001800        05 SPRL-IDLEVNR-PR   PIC X(5).                                    
001900*                                 LEVERANTÖRNR FÖR DETTA PRIS             
002000*                                 SUPPLIER NUMBER FOR THIS PRICE          
002100        05 SPRL-IDUSER       PIC X(8).                                    
002200*                                 ANVÄNDARENS SÄKERHETS ID                
002300*                                 USER SECURITY-IDENTITY                  
002400        05 SPRL-KDFPKPRI     PIC X.                                       
002500*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
002600*                                 IF PACKING INCLUDED IN PRICE            
002700        05 SPRL-KDPRURSP     PIC X.                                       
002800*                                 PRISHÄRSTAMNING BESTÄLLNING             
002900*                                 ORIGINATE ORDER PRICE                   
003000        05 SPRL-KDVALISO     PIC X(3).                                    
003100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003200*                                 CURRENCY CODE BY ISO-STANDARD.          
003300        05 SPRL-PRARTBES-PR  PIC S9(7)V9(2)      COMP-3.                  
003400*                                 DETTA BESTÄLLNINGSPRIS (KR)             
003500        05 SPRL-PRARTBEL-PR  PIC S9(8)V9(5)      COMP-3.                  
003600*                                 DETTA BESTÄLLNINGSPRIS                  
003700*                                 (I LEVERANTÖRENS VALUTA)                
003800        05 SPRL-SUINLEV-PR   PIC S9(3)           COMP-3.                  
003900*                                 ANTAL INLEV. TILL DETTA PRIS            
004000        05 SPRL-TIREGDAT     PIC S9(7)           COMP-3.                  
004100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004200*                                 REGISTRATION DATE (YYMMDD)              
004300*** END OF VILMAII-COPY LENGTH= 51 BYTES                                  

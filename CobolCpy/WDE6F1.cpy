000100 01  SEQF-WDE6F1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE611             
000300*                                 EXIT FINNS NÄR KDVIA = 1                
000400*                                 OCH KDKOLSAT = 0 ELLER 1                
000500*                                 FYSISK NYCKEL: WDE6F1KY                 
000600*                                 (IDLEVNR, DASUPREF, IDSUPREF            
000700*                                  IDPRIDNR, IDKOLLI)                     
000800*                                 SECONDARY KEY: WDE6FSEQ                 
000900*                                 (IDLEVNR, DASUPREF, IDSUPREF)           
001000     03 SEQF-IDLEVNR         PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001300     03 SEQF-DASUPREF        PIC 9(8).                                    
001400*                                 SÄNDNINGSDATUM DIREKTLEVERANTÖR         
001500*                                 SHIPPING DATE DIRECT SUPPLIER           
001600     03 SEQF-IDSUPREF        PIC X(10).                                   
001700*                                 LEVERANTöRSREF.                         
001800*                                 SUPPLIER REF.                           
001900     03 SEQF-IDPRODNR        PIC S9(7)           COMP-3.                  
002000*                                 PRODUKTIONSNUMMER                       
002100*                                 PRODUCTION NUMBER                       
002200     03 SEQF-IDKOLLI         PIC S9(5)           COMP-3.                  
002300*                                 KOLLINUMMER                             
002400*                                 CASE NUMBER                             
002500     03 SEQF-TISUPTID        PIC S9(5)           COMP-3.                  
002600*                                 SÄNDNINGSTID DIREKTLEVERANTÖR           
002700*                                 SHIPPING TIME DIRECT SUPPLIER           
002800     03 SEQF-KDKOLSTA        PIC S9              COMP-3.                  
002900*                                 KOLLISTATUS                             
003000*                                 CASE STATUS                             
003100     03 SEQF-IDDISTR         PIC S9(5)           COMP-3.                  
003200*                                 DISTRIKTNUMMER                          
003300*                                 DISTRICT NUMBER                         
003400*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  

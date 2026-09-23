000100 01  SEQH-WDE6H1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE621             
000300*                                 FYSISK NYCKEL: WDE6H1KY                 
000400*                                 (IDDC-CROS,TIRFSDAT,IDDISTR,IDK         
000500*                                 UNDNR+                                  
000600*                                 (IDPRODNR,IDKOLLI)                      
000700*                                 SECONDARY KEY: WDE6HSEQ                 
000800*                                 (IDDC-CROS,TIRFSDAT,IDDISTR,IDK         
000900*                                 UNDNR)                                  
001000     03 SEQH-IDDC-CROSS      PIC X(2).                                    
001100*                                 DC FÖR CROSS DOCKING                    
001200*                                 DC FOR CROSS DOCKING                    
001300     03 SEQH-KDKOLSTA-CROSS  PIC S9              COMP-3.                  
001400*                                 KOLLISTATUS CROSS-DOCK KOLLI            
001500*                                 CASE STATUS FOR CROSS-DOCK CASE         
001600     03 SEQH-TIRFSDAT        PIC 9(6).                                    
001700*                                 KLART FÖR TRANSPORT ÅÅMMDD              
001800*                                 READY FOR SHIPMENT  YYMMDD              
001900     03 SEQH-IDDISTR         PIC S9(5)           COMP-3.                  
002000*                                 DISTRIKTNUMMER                          
002100*                                 DISTRICT NUMBER                         
002200     03 SEQH-IDKUNDNR        PIC S9(7)           COMP-3.                  
002300*                                 KUNDNUMMER                              
002400*                                 CUSTOMER NO                             
002500     03 SEQH-IDPRODNR        PIC S9(7)           COMP-3.                  
002600*                                 PRODUKTIONSNUMMER                       
002700*                                 PRODUCTION NUMBER                       
002800     03 SEQH-IDKOLLI         PIC S9(5)           COMP-3.                  
002900*                                 KOLLINUMMER                             
003000*                                 CASE NUMBER                             
003100     03 SEQH-IDLEVNR         PIC X(5).                                    
003200*                                 LEVERANTÖRNUMMER                        
003300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003400     03 SEQH-IDSUPREF        PIC X(10).                                   
003500*                                 LEVERANTöRSREF.                         
003600*                                 SUPPLIER REF.                           
003700     03 SEQH-IDTRPTNR-CROSS  PIC S9(3)           COMP-3.                  
003800*                                 TRANSPORTIDENTITET DCCROSS              
003900*                                 TRANSPORT IDENTITY DCCROSS              
004000     03 SEQH-TIRECXDAT       PIC 9(6).                                    
004100*                                 DATUM FÖR MOTTAG KLI I KROSS-DC         
004200*                                 DATE CASE BINNING IN CROSS-DC           
004300     03 SEQH-TIRECXTID       PIC 9(4).                                    
004400*                                 TID FÖR MOTTAG KOLLI I KROSS-DC         
004500*                                 TIME CASE BINNING IN CROSS-DC           
004600     03 SEQH-IDDC-SEND       PIC X(2).                                    
004700*                                 SÄNDANDE LAGER                          
004800*                                 SENDING WAREHOUSE                       
004900     03 SEQH-IDLBBET-CROSS   PIC X(12).                                   
005000*                                 LASTBÄRARBETECKNING DCCROSS             
005100*                                 TRAILER NUMBER                          
005200*** END OF VILMAII-COPY LENGTH= 64 BYTES                                  

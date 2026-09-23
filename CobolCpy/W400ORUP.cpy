000100 01  ORUP-W400ORUP.                                                       
000200*                                 LINK AREA FOR W400ORUP                  
000300     03 ORUP-INPUT-DATA.                                                  
000400        05 ORUP-KDCALL       PIC S9(3)           COMP-3.                  
000500*                                 ANROPSTYP                               
000600        05 ORUP-IDSYSTEM     PIC X(4).                                    
000700*                                 VOLVO VCCS SYSTEMNUMMER                 
000800        05 ORUP-IDAPIORDREF  PIC X(23).                                   
000900*                                 API ORDERID(DIS+KND+ORD+DAT)            
001000        05 ORUP-IDAPIORDREF-RED REDEFINES ORUP-IDAPIORDREF.               
001100           07 ORUP-IDDISTR   PIC 9(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300           07 ORUP-IDKUNDNR  PIC 9(6).                                    
001400*                                 KUNDNUMMER                              
001500           07 ORUP-IDORDNR7  PIC 9(7).                                    
001600*                                 ORDERNUMMER                             
001700           07 ORUP-TIREGDAT  PIC 9(6).                                    
001800*                                 REGISTRERINGSDATUM (≈≈MMDD OR ≈         
001900*                                 ≈≈≈-MM-DD)                              
002000        05 ORUP-KVRADER      PIC 9(5).                                    
002100*                                 ANTAL RADER                             
002200        05 ORUP-RADER        OCCURS 1 TO 999 TIMES                        
002300                             DEPENDING ON ORUP-KVRADER.                   
002400           07 ORUP-KDBEHX    PIC X.                                       
002500*                                 BEHANDLINGSKOD-X                        
002600           07 ORUP-IDLEVART  PIC X(30).                                   
002700*                                 LEVERANT÷RENS ARTNR                     
002800           07 ORUP-KVBEART   PIC S9(7)           COMP-3.                  
002900*                                 BESTƒLLT ANTAL STYCKEN                  
003000           07 ORUP-PRARTNTO-LOC                                           
003100                             PIC S9(7)V9(2)      COMP-3.                  
003200*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
003300           07 ORUP-KDVALISO  PIC X(3).                                    
003400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003500           07 ORUP-BERADREF  PIC X(10).                                   
003600*                                 KUNDENS RADREFERENS                     
003700     03 ORUP-OUTPUT-DATA.                                                 
003800        05 ORUP-KDSVAR       PIC X.                                       
003900         88 ORUP-KDSVAR-OK   VALUE ' '.                                   
004000         88 ORUP-KDSVAR-FEL  VALUE 'F'.                                   
004100*                                                       KDSVAR-88         
004200*                                 SVARSKOD FR≈N SUBPROGRAM                
004300        05 ORUP-IDMSG-ERROR  PIC X(3).                                    
004400*                                 FELMEDDELANDE ID                        
004500        05 ORUP-IDELMT-ERROR PIC X(16).                                   
004600*                                 DATAELEMENTIDENTITET                    
004700        05 ORUP-FEL-TEXT     PIC X(55).                                   
004800*** END OF VILMAII-COPY LENGTH= 53056 BYTES                               

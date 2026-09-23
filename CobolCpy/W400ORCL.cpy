000100 01  ORCL-W400ORCL.                                                       
000200*                                 LINK AREA FOR W400ORCL                  
000300     03 ORCL-INPUT-DATA.                                                  
000400        05 ORCL-KDCALL       PIC S9(3)           COMP-3.                  
000500*                                 ANROPSTYP                               
000600        05 ORCL-IDSYSTEM     PIC X(4).                                    
000700*                                 VOLVO VCCS SYSTEMNUMMER                 
000800        05 ORCL-IDAPIORDREF  PIC X(23).                                   
000900*                                 API ORDERID(DIS+KND+ORD+DAT)            
001000        05 ORCL-IDAPIORDREF REDEFINES ORCL-IDAPIORDREF.                   
001100           07 ORCL-IDDISTR   PIC 9(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300           07 ORCL-IDKUNDNR  PIC 9(6).                                    
001400*                                 KUNDNUMMER                              
001500           07 ORCL-IDORDNR7  PIC 9(7).                                    
001600*                                 ORDERNUMMER                             
001700           07 ORCL-TIREGDAT  PIC 9(6).                                    
001800*                                 REGISTRERINGSDATUM (≈≈MMDD OR ≈         
001900*                                 ≈≈≈-MM-DD)                              
002000        05 ORCL-KVRADER      PIC 9(3).                                    
002100*                                 ANTAL RADER                             
002200        05 ORCL-RADER        OCCURS 1 TO 999 TIMES                        
002300                             DEPENDING ON ORCL-KVRADER.                   
002400           07 ORCL-IDLEVART  PIC X(30).                                   
002500*                                 LEVERANT÷RENS ARTNR                     
002600           07 ORCL-KVBEART   PIC S9(7)           COMP-3.                  
002700*                                 BESTƒLLT ANTAL STYCKEN                  
002800           07 ORCL-IDDC      PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000     03 ORCL-OUTPUT-DATA.                                                 
003100        05 ORCL-KDSVAR       PIC X.                                       
003200         88 ORCL-KDSVAR-OK   VALUE ' '.                                   
003300         88 ORCL-KDSVAR-FEL  VALUE 'F'.                                   
003400*                                                       KDSVAR-88         
003500*                                 SVARSKOD FR≈N SUBPROGRAM                
003600        05 ORCL-IDMSG-ERROR  PIC X(3).                                    
003700*                                 FELMEDDELANDE ID                        
003800        05 ORCL-IDELMT-ERROR PIC X(16).                                   
003900*                                 DATAELEMENTIDENTITET                    
004000        05 ORCL-FEL-TEXT     PIC X(55).                                   
004100*** END OF VILMAII-COPY LENGTH= 36071 BYTES                               

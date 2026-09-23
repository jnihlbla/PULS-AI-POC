000100 01  SEQC-WDJ7C1.                                                         
000200*                                 ACS INVENTERINGS REGISTER               
000300*                                 SEKUNDÄR INGÅNG IDUSER-RCOUNT           
000400*                                 FYSISK NKL : WDJ7C1KY                   
000500*                                 (IDDC + IDUSER-RCOUNT +                 
000600*                                  TIREGDAT-RCOUNT +                      
000700*                                  TIREGTID-RCOUNT+ADART+IDARTNR)         
000800*                                 SEKUNDARY NKL: WDJ7CSEQ                 
000900*                                 (IDDC + IDUSER-RCOUNT +                 
001000*                                  TIREGDAT-RCOUNT +                      
001100*                                  TIREGTID-RCOUNT )                      
001200     03 SEQC-IDDC            PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 SEQC-IDUSER-RCOUNT   PIC X(8).                                    
001600*                                 ANVÄNDARENS SÄKERHETS ID                
001700*                                 USER SECURITY-IDENTITY                  
001800     03 SEQC-TIREGDAT-RCOUNT PIC S9(7)           COMP-3.                  
001900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002000*                                 REGISTRATION DATE (YYMMDD)              
002100     03 SEQC-TIREGTID-RCOUNT PIC S9(7)           COMP-3.                  
002200*                                 REGISTRERINGSTID                        
002300*                                 GENERAL REGISTRATION TIME               
002400     03 SEQC-ADLAGOMR        PIC 9(2).                                    
002500*                                 LAGEROMRÅDE                             
002600*                                 AREA                                    
002700     03 SEQC-ADGANG          PIC 9(2).                                    
002800*                                 GÅNG                                    
002900*                                 AISLE                                   
003000     03 SEQC-ADPLATS         PIC 9(5).                                    
003100*                                 LAGERPLATSNUMMER                        
003200*                                 LOCATION                                
003300     03 SEQC-IDARTNR         PIC S9(9)           COMP-3.                  
003400*                                 ARTIKELNUMMER                           
003500*                                 PART NUMBER                             
003600*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  

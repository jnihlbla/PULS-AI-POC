000100 01  SEQB-WDJ7B1.                                                         
000200*                                 ACS INVENTERINGS REGISTER               
000300*                                 SEKUNDÄR INGÅNG IDUSER-PCOUNT           
000400*                                 FYSISK NKL : WDJ7B1KY                   
000500*                                 (IDDC + IDUSER-PCOUNT +                 
000600*                                  TIREGDAT-PCOUNT +                      
000700*                                  TIREGTID-PCOUNT+ADART+IDARTNR)         
000800*                                 SEKUNDARY NKL: WDJ7BSEQ                 
000900*                                 (IDDC + IDUSER-PCOUNT +                 
001000*                                  TIREGDAT-PCOUNT +                      
001100*                                  TIREGTID-PCOUNT)                       
001200     03 SEQB-IDDC            PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 SEQB-IDUSER-PCOUNT   PIC X(8).                                    
001600*                                 ANVÄNDARENS SÄKERHETS ID                
001700*                                 USER SECURITY-IDENTITY                  
001800     03 SEQB-TIREGDAT-PCOUNT PIC S9(7)           COMP-3.                  
001900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002000*                                 REGISTRATION DATE (YYMMDD)              
002100     03 SEQB-TIREGTID-PCOUNT PIC S9(7)           COMP-3.                  
002200*                                 REGISTRERINGSTID                        
002300*                                 GENERAL REGISTRATION TIME               
002400     03 SEQB-ADLAGOMR        PIC 9(2).                                    
002500*                                 LAGEROMRÅDE                             
002600*                                 AREA                                    
002700     03 SEQB-ADGANG          PIC 9(2).                                    
002800*                                 GÅNG                                    
002900*                                 AISLE                                   
003000     03 SEQB-ADPLATS         PIC 9(5).                                    
003100*                                 LAGERPLATSNUMMER                        
003200*                                 LOCATION                                
003300     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
003400*                                 ARTIKELNUMMER                           
003500*                                 PART NUMBER                             
003600*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  

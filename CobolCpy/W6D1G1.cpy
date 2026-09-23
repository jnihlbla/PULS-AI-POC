000100 01  SEQG-W6D1G1.                                                         
000200*                                 INLEVERANSREGISTER                      
000300*                                 SEKUNDÄRT INDEX TILL W6D111             
000400*                                 PLACERINGS INGÅNG (NÄSTA)               
000500*                                 TAS BORT OM PLACERING SAKNAS            
000600*                                 EXIT: INDEX FINNS NÄR                   
000700*                                 ADINLOMR-NXT(ADINLOMN) NE SPACE         
000800*                                 FYSISK NYCKEL: W6D1G1KY                 
000900*                                 (ADINLOMN, KDINLPRI,                    
001000*                                  IDRADNR-INL, IDDC, IDLEVNR,            
001100*                                  IDFS, TIAVIDAT, IDRADNR)               
001200*                                 SECONDARY NYCKEL: W6D1GSEQ              
001300*                                 (ADINLOMN)                              
001400     03 SEQG-ADINLOMR-NXT    PIC X(4).                                    
001500*                                 INLEVERANSOMRÅDE NÄSTA                  
001600*                                 RECEIVING AREA NEXT                     
001700     03 SEQG-KDINLPRIO       PIC S9(3)           COMP-3.                  
001800*                                 PRIORITETSGRUPP                         
001900*                                 PRIORITY GROUP                          
002000     03 SEQG-IDRADNR-INL     PIC S9(5)           COMP-3.                  
002100*                                 RADNUMMER INLEVERANS                    
002200*                                 LINE NUMBER GOODS RECEIVING             
002300     03 SEQG-IDDC            PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500*                                 WAREHOUSE IDENTIFIER                    
002600     03 SEQG-IDLEVNR         PIC X(5).                                    
002700*                                 LEVERANTÖRNUMMER                        
002800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002900     03 SEQG-IDFS            PIC X(8).                                    
003000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003100*                                 ADVICE NOTE NUMBER ODETTE               
003200     03 SEQG-TIAVIDAT        PIC S9(7)           COMP-3.                  
003300*                                 AVISERINGSDATUM (YYMMDD)                
003400*                                 ADVICE NOTE DATE                        
003500     03 SEQG-IDRADNR         PIC S9(5)           COMP-3.                  
003600*                                 RADNUMMER                               
003700*                                 LINE NO                                 
003800     03 SEQG-FLINLFB         PIC X.                                       
003900*                                 VALD TILL FÖRBEHANDLING                 
004000*                                 SELECTED FOR PRETREATEMENT              
004100     03 SEQG-FLINLFP         PIC X.                                       
004200*                                 VALD TILL FÖRPACKNINGEN                 
004300*                                 SELECTED FOR PRE-PACKING                
004400     03 SEQG-KDINLSTA        PIC X(3).                                    
004500*                                 SYSTEMSTATUS INLEVERANS                 
004600*                                 SYSTEM STATUS RECEIVING                 
004700     03 SEQG-IDINLVGN        PIC 9(3).                                    
004800*                                 VAGNSIDENTITET                          
004900*                                 INTERNAL CARRIER ID                     
005000*** END OF VILMAII-COPY LENGTH= 39 BYTES                                  

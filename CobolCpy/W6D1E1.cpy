000100 01  SEQE-W6D1E1.                                                         
000200*                                 INLEVERANSREGISTER                      
000300*                                 SEKUNDÄRT INDEX TILL W6D111             
000400*                                 PLACERINGS INGÅNG                       
000500*                                 TAS BORT OM PLACERING SAKNAS            
000600*                                 EXIT: INDEX FINNS NÄR                   
000700*                                 ADINLOMR NE SPACE                       
000800*                                 FYSISK NYCKEL: W6D1E1KY                 
000900*                                 (ADINLOMR,  KDINLPRI,                   
001000*                                  IDRADNR-INL,IDDC    ,IDLEVNR,          
001100*                                  IDFS,     TIAVIDAT, IDRADNR)           
001200*                                 SECONDARY NYCKEL: W6D1ESEQ              
001300*                                 (ADINLOMR)                              
001400     03 SEQE-ADINLOMR        PIC X(4).                                    
001500*                                 INLEVERANSOMRÅDE                        
001600*                                 RECEIVING AREA                          
001700     03 SEQE-KDINLPRIO       PIC S9(3)           COMP-3.                  
001800*                                 PRIORITETSGRUPP                         
001900*                                 PRIORITY GROUP                          
002000     03 SEQE-IDRADNR-INL     PIC S9(5)           COMP-3.                  
002100*                                 RADNUMMER INLEVERANS                    
002200*                                 LINE NUMBER GOODS RECEIVING             
002300     03 SEQE-IDDC            PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500*                                 WAREHOUSE IDENTIFIER                    
002600     03 SEQE-IDLEVNR         PIC X(5).                                    
002700*                                 LEVERANTÖRNUMMER                        
002800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002900     03 SEQE-IDFS            PIC X(8).                                    
003000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003100*                                 ADVICE NOTE NUMBER ODETTE               
003200     03 SEQE-TIAVIDAT        PIC S9(7)           COMP-3.                  
003300*                                 AVISERINGSDATUM (YYMMDD)                
003400*                                 ADVICE NOTE DATE                        
003500     03 SEQE-IDRADNR         PIC S9(5)           COMP-3.                  
003600*                                 RADNUMMER                               
003700*                                 LINE NO                                 
003800     03 SEQE-FLINLFB         PIC X.                                       
003900*                                 VALD TILL FÖRBEHANDLING                 
004000*                                 SELECTED FOR PRETREATEMENT              
004100     03 SEQE-FLINLFP         PIC X.                                       
004200*                                 VALD TILL FÖRPACKNINGEN                 
004300*                                 SELECTED FOR PRE-PACKING                
004400     03 SEQE-KDINLSTA        PIC X(3).                                    
004500*                                 SYSTEMSTATUS INLEVERANS                 
004600*                                 SYSTEM STATUS RECEIVING                 
004700     03 SEQE-IDINLVGN        PIC 9(3).                                    
004800*                                 VAGNSIDENTITET                          
004900*                                 INTERNAL CARRIER ID                     
005000*** END OF VILMAII-COPY LENGTH= 39 BYTES                                  

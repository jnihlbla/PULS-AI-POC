000100 01  EMB-W461014.                                                         
000200*                                 EMBALLAGEPROFORMA                       
000300*                                 TILL NOAC PT-014                        
000400     03 EMB-IDPTYP           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 EMB-IDDISTR          PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 EMB-IDKUNDNR         PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 EMB-IDORDNR          PIC S9(7)           COMP-3.                  
001100*                                 ORDERNR             IDORDNR-002         
001200     03 EMB-IDDC             PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 EMB-KDFAKTYP         PIC X.                                       
001500*                                 FAKTURATYP                              
001600     03 EMB-IDFAKT           PIC S9(7)           COMP-3.                  
001700*                                 FAKTURANUMMER                           
001800     03 EMB-TIFAKT           PIC S9(7)           COMP-3.                  
001900*                                 FAKTURERINGSDATUM (≈≈MMDD)              
002000     03 EMB-KDPALL           PIC X.                                       
002100*                                 PALLTYP                                 
002200     03 EMB-KVPALL           PIC S9(5)           COMP-3.                  
002300*                                 ANTAL PALLAR         KVPALL-002         
002400     03 EMB-KVKRAG           PIC S9(5)           COMP-3.                  
002500*                                 ANTAL KRAGAR                            
002600     03 EMB-KVLOCK           PIC S9(5)           COMP-3.                  
002700*                                 ANTAL  LOCK                             
002800     03 FILLER               PIC X(6).                                    
002900*** END COPY W461014     LENGTH=41                                        

000100 01  SEQB-WDA2B1.                                                         
000200*                                 LEVERANSANMÄRKNING                      
000300*                                 RADKÖ                                   
000400*                                 EXIT: INDEX FINNS NÄR                   
000500*                                 KDKREBEH (POS 1) = R                    
000600*                                 FYSISK NYCKEL: WDA2B1KY                 
000700*                                  (IDFTG + KDARBTYP +                    
000800*                                   IDPERSON + DALEVANM +                 
000900*                                   IDLEVANM + IDARTNR +                  
001000*                                   IDRADNR  )                            
001100*                                 SEKUNDÄR NYCKEL: WDA2BSEQ               
001200*                                  (IDFTG + KDARBTYP +                    
001300*                                   IDPERSON + IDLEVANM)                  
001400     03 SEQB-IDFTG           PIC 9(2).                                    
001500*                                 FÖRETAGSID EKONOM REDOVISNING           
001600*                                 COMPANY IDENTITY ACCOUNTING             
001700     03 SEQB-KDARBTYP        PIC X(8).                                    
001800*                                 TYP AV ARBETE                           
001900*                                 CATEGORY OF WORK                        
002000     03 SEQB-IDPERSON        PIC S9(3)           COMP-3.                  
002100*                                 PERSONKOD                               
002200*                                 STAFF CODE                              
002300     03 SEQB-DALEVANM        PIC 9(8).                                    
002400*                                 DATUM LEV.ANMÄRKNING(YYYYMMDD)          
002500*                                 DISCREPANCY REPORT DATE                 
002600     03 SEQB-IDLEVANM.                                                    
002700*                                 LEVERANSANMÄRKNINGSIDENTITET            
002800*                                 DISCREPANCY REPORT IDENTITY             
002900        05 SEQB-IDDISTR      PIC S9(5)           COMP-3.                  
003000*                                 DISTRIKTNUMMER                          
003100*                                 DISTRICT NUMBER                         
003200        05 SEQB-IDKUNDNR     PIC S9(7)           COMP-3.                  
003300*                                 KUNDNUMMER                              
003400*                                 CUSTOMER NO                             
003500        05 SEQB-IDRAPPNR     PIC 9(7).                                    
003600*                                 RAPPORT NUMMER                          
003700*                                 DISCREPANCY REPORT NUMBER               
003800     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
003900*                                 ARTIKELNUMMER                           
004000*                                 PART NUMBER                             
004100     03 SEQB-IDRADNR         PIC S9(5)           COMP-3.                  
004200*                                 RADNUMMER                               
004300*                                 LINE NO                                 
004400     03 SEQB-KDANMORS        PIC X(2).                                    
004500*                                 ORSAK TILL LEVERANSANMÄRKNING           
004600*                                 DISCREPANCY REPORT REASON CODE          
004700     03 SEQB-KVLEVANM-BEKR   PIC S9(7)           COMP-3.                  
004800*                                 BEKRÄFTAT RETURANTAL                    
004900     03 SEQB-KDKREBEH        PIC X(3).                                    
005000*                                 BEHANDLINGSSTATUS                       
005100*                                 TREATMENT STATUS                        
005200     03 SEQB-FLTEXT          PIC X.                                       
005300*                                 FINNS TEXTINFORMATION ?                 
005400     03 SEQB-IDKUNDRF-GRP.                                                
005500*                                 KUNDENS REFERENS (ORDERID)              
005600*                                 CUSTOMER REFERENCE (ORDER ID)           
005700        05 SEQB-IDKUNDRF     PIC X(10).                                   
005800*                                 KUNDENS REFERENS (ORDERID)              
005900*                                 CUSTOMER REFERENCE (ORDER ID)           
006000        05 SEQB-IDORDNR5-FILLER REDEFINES SEQB-IDKUNDRF.                  
006100           07 SEQB-IDORDNR5  PIC 9(5).                                    
006200*                                 ORDERNUMMER                             
006300*                                 ORDER NUMBER                            
006400           07 FILLER         PIC X(5).                                    
006500        05 SEQB-IDORDNR7-FILLER REDEFINES SEQB-IDKUNDRF.                  
006600           07 SEQB-IDORDNR7  PIC 9(7).                                    
006700*                                 ORDERNUMMER                             
006800*                                 ORDER NUMBER                            
006900           07 FILLER         PIC X(3).                                    
007000*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  

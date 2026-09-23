000100 01  SEQA-WDQ1A1.                                                         
000200*                                 ORDERBEKRÄFTELSE REGISTER               
000300*                                 SEKUNDÄRT INDEX TILL WDQ101             
000400*                                 ARTIKEL-INGÅNG                          
000500*                                 FYSISK NYCKEL: WDQ1A1KY                 
000600*                                 (IDDISTR,  IDKUNDNR,                    
000700*                                  TITIREGD-9KOMPL,                       
000800*                                  IDARTNR,  IDLOPNR, IDSEKVNR,           
000900*                                  IDDC,     KDORDBEK, IDKUNDRF,          
001000*                                  IDORDER)                               
001100*                                 SECONDARY NYCKEL: WDQ1ASEQ              
001200*                                 (IDDISTR,  IDKUNDNR,                    
001300*                                  TITIREGD-9KOMPL,                       
001400*                                  IDARTNR,  IDLOPNR, IDSEKVNR,           
001500*                                  IDDC,     KDORDBEK, IDKUNDRF)          
001600     03 SEQA-IDDISTR         PIC S9(5)           COMP-3.                  
001700*                                 DISTRIKTNUMMER                          
001800*                                 DISTRICT NUMBER                         
001900     03 SEQA-IDKUNDNR        PIC S9(7)           COMP-3.                  
002000*                                 KUNDNUMMER                              
002100*                                 CUSTOMER NO                             
002200     03 SEQA-TITIREGD-9KOMPL PIC S9(9)           COMP-3.                  
002300*                                 DATUMETS 9-KOMPLEMENT                   
002400*                                 DATES 9-COMPLEMENT                      
002500     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700*                                 PART NUMBER                             
002800     03 SEQA-IDLOPNR         PIC S9(3)           COMP-3.                  
002900*                                 LÖPNUMMER                               
003000*                                 SEQUENCE NUMBER                         
003100     03 SEQA-IDSEKVNR        PIC S9(3)           COMP-3.                  
003200*                                 GENERELLT SEKVENSNUMMER                 
003300*                                 GENERAL SEQUENCE NUMBER                 
003400     03 SEQA-IDDC            PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600*                                 WAREHOUSE IDENTIFIER                    
003700     03 SEQA-KDORDBEK        PIC 9(2).                                    
003800*                                 ORDERBEKRÄFTELSEKOD                     
003900*                                 ORDERCONFIMATIONCODE                    
004000     03 SEQA-IDKUNDRF.                                                    
004100*                                 KUNDENS REFERENS (ORDERID)              
004200*                                 CUSTOMER REFERENCE (ORDER ID)           
004300        05 SEQA-FILLER       PIC X(10).                                   
004400        05 SEQA-IDORDNR5-FILLER REDEFINES SEQA-FILLER.                    
004500           07 SEQA-IDORDNR5  PIC 9(5).                                    
004600*                                 ORDERNUMMER                             
004700*                                 ORDER NUMBER                            
004800           07 FILLER         PIC X(5).                                    
004900        05 SEQA-IDORDNR7-FILLER REDEFINES SEQA-FILLER.                    
005000           07 SEQA-IDORDNR7  PIC 9(7).                                    
005100*                                 ORDERNUMMER                             
005200*                                 ORDER NUMBER                            
005300           07 FILLER         PIC X(3).                                    
005400     03 SEQA-IDORDER         PIC S9(7)           COMP-3.                  
005500*                                 VOLVO PARTS ORDERNUMMER                 
005600*                                 VOLVO PARTS ORDER NUMBER                
005700     03 SEQA-IDSYSTEM        PIC X(4).                                    
005800*                                 VOLVO VCAS SYSTEMNUMMER                 
005900*                                 VOLVO VCAS SYSTEM NUMBER                
006000     03 SEQA-IDWDQ101        PIC X(17).                                   
006100*                                 NYCKEL TILL WDQ101                      
006200*                                 KEY TO WDQ101                           
006300*** END OF VILMAII-COPY LENGTH= 60 BYTES                                  

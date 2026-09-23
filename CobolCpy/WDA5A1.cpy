000100 01  SEQA-WDA5A1.                                                         
000200*                                 ORDERRADSREGISTER                       
000300*                                 SEKUNDƒRT INDEX TILL WDA501             
000400*                                 ARTIKEL-PRIORITET                       
000500*                                 FYSISK NYCKEL: WDA5A1KY                 
000600*                                  (IDARTNR, IDDC, KDRAPRIO,              
000700*                                   DARODAT, TIREGTID, IDDISTR,           
000800*                                   IDKUNDNR, IDKUNDRF, IDLOPNR)          
000900*                                 SECONDARY NYCKEL: WDA5ASEQ              
001000*                                  (IDARTNR, IDDC, KDRAPRIO)              
001100     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300*                                 PART NUMBER                             
001400     03 SEQA-IDDC            PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600*                                 WAREHOUSE IDENTIFIER                    
001700     03 SEQA-KDRAPRIO        PIC S9(3)           COMP-3.                  
001800*                                 PRIORITETSKOD P≈ RADEN                  
001900*                                 PRIORITY CODE ON THE LINE               
002000     03 SEQA-DARODAT         PIC 9(8).                                    
002100*                                 RESTORDERDATUM       (≈≈≈≈MMDD)         
002200*                                 BACK ORDER DATE      (YYYYMMDD)         
002300     03 SEQA-TIREGTID        PIC S9(7)           COMP-3.                  
002400*                                 REGISTRERINGSTID                        
002500*                                 GENERAL REGISTRATION TIME               
002600     03 SEQA-IDDISTR         PIC S9(5)           COMP-3.                  
002700*                                 DISTRIKTNUMMER                          
002800*                                 DISTRICT NUMBER                         
002900     03 SEQA-IDKUNDNR        PIC S9(7)           COMP-3.                  
003000*                                 KUNDNUMMER                              
003100*                                 CUSTOMER NO                             
003200     03 SEQA-IDKUNDRF-GRP.                                                
003300*                                 KUNDENS REFERENS (ORDERID)              
003400*                                 CUSTOMER REFERENCE (ORDER ID)           
003500        05 SEQA-IDKUNDRF     PIC X(10).                                   
003600*                                 KUNDENS REFERENS (ORDERID)              
003700*                                 CUSTOMER REFERENCE (ORDER ID)           
003800        05 SEQA-IDORDNR5-FILLER REDEFINES SEQA-IDKUNDRF.                  
003900           07 SEQA-IDORDNR5  PIC 9(5).                                    
004000*                                 ORDERNUMMER                             
004100*                                 ORDER NUMBER                            
004200           07 FILLER         PIC X(5).                                    
004300        05 SEQA-IDORDNR7-FILLER REDEFINES SEQA-IDKUNDRF.                  
004400           07 SEQA-IDORDNR7  PIC 9(7).                                    
004500*                                 ORDERNUMMER                             
004600*                                 ORDER NUMBER                            
004700           07 FILLER         PIC X(3).                                    
004800     03 SEQA-IDLOPNR         PIC S9(3)           COMP-3.                  
004900*                                 L÷PNUMMER                               
005000*                                 SEQUENCE NUMBER                         
005100     03 SEQA-KDSTARAD        PIC X.                                       
005200*                                 RADSTATUSKOD                            
005300*                                 LINE STATUS CODE                        
005400     03 SEQA-KDTPOTYP        PIC S9              COMP-3.                  
005500*                                 TYP AV TIDPLANERAD ORDER                
005600*                                 TYPE OF TIME PLANNED ORDER              
005700     03 SEQA-KVART           PIC S9(7)           COMP-3.                  
005800*                                 ANTAL ARTNR PER BRYTBEGREPP             
005900*                                 NO OF PARTNOS PER TYPE                  
006000     03 SEQA-KDORDKL         PIC S9              COMP-3.                  
006100*                                 ORDERKLASS                              
006200*                                 ORDER CLASS                             
006300     03 SEQA-IDSYSTEM        PIC X(4).                                    
006400*                                 VOLVO VCCS SYSTEMNUMMER                 
006500*                                 VOLVO VCCS SYSTEM NUMBER                
006600     03 SEQA-KDORDTYP-LDC    PIC X(2).                                    
006700*                                 ORDERTYP HOS DEALER                     
006800     03 SEQA-TIREPDAT        PIC S9(7)           COMP-3.                  
006900*                                 REPAIR DATE                             
007000*                                 REPAIR DATE                             
007100     03 SEQA-IDWDA501        PIC X(24).                                   
007200*                                 NYCKEL TILL WDA501                      
007300*                                 KEY TO WDA501                           
007400*** END OF VILMAII-COPY LENGTH= 81 BYTES                                  

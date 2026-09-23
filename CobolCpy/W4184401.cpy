000100 01  W4184401.                                                            
000200*                                 LEVERANSANMÄRKNING                      
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 WDA201.                                                           
000700*                                 LEVERANSANMÄRKNINGAR                    
000800*                                 FYSISK NYCKEL: IDLEVANM                 
000900        05 IDLEVANM.                                                      
001000*                                 LEVERANSANMÄRKNINGSIDENTITET            
001100*                                 DISCREPANCY REPORT IDENTITY             
001200           07 IDDISTR        PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400*                                 DISTRICT NUMBER                         
001500           07 IDKUNDNR       PIC S9(7)           COMP-3.                  
001600*                                 KUNDNUMMER                              
001700*                                 CUSTOMER NO                             
001800           07 IDRAPPNR       PIC 9(7).                                    
001900*                                 RAPPORT NUMMER                          
002000*                                 DISCREPANCY REPORT NUMBER               
002100        05 IDFTG             PIC 9(2).                                    
002200*                                 FÖRETAGSID EKONOM REDOVISNING           
002300*                                 COMPANY IDENTITY ACCOUNTING             
002400        05 IDPERSON          PIC S9(3)           COMP-3.                  
002500*                                 PERSONKOD                               
002600*                                 STAFF CODE                              
002700        05 IDUSER            PIC X(8).                                    
002800*                                 ANVÄNDARENS SÄKERHETS ID                
002900*                                 USER SECURITY-IDENTITY                  
003000        05 KDARBTYP          PIC X(8).                                    
003100*                                 TYP AV ARBETE                           
003200*                                 CATEGORY OF WORK                        
003300        05 KDLEVANM          PIC X.                                       
003400*                                 STATUS LEVERANSANMÄRKNING               
003500*                                 STATUS DISCREPANCY                      
003600        05 KVRADER-OBEH      PIC S9(5)           COMP-3.                  
003700*                                 ANTAL OBEHANDLADE RADER                 
003800*                                 NUMBER OF NOT TREATED LINES             
003900        05 KVRADER-RT        PIC S9(5)           COMP-3.                  
004000*                                 ANTAL RADER RETURTILLSTÅND              
004100*                                 NUMBER OF LINES RETURNPERMIT            
004200        05 PRFOERS           PIC S9(7)V9(2)      COMP-3.                  
004300*                                 FÖRSÄKRINGSPREMIE                       
004400*                                 INSURANCE FEE                           
004500        05 PRFRAKT           PIC S9(7)V9(2)      COMP-3.                  
004600*                                 FRAKTKOSTNAD                            
004700*                                 FREIGHT COST                            
004800        05 PRLEGKST          PIC S9(7)V9(2)      COMP-3.                  
004900*                                 LEGALISERINSKOSTNAD                     
005000*                                 LEGALIZATION FEE                        
005100        05 REEMBHNT          PIC S9(2)V9(1)      COMP-3.                  
005200*                                 EMB OCH HANTERINGSKOST (%)              
005300*                                 PACKING AND HANDLING (%)                
005400        05 RELANDCO          PIC S9(3)V9(2)      COMP-3.                  
005500*                                 LANDING COST PROCENT                    
005600*                                 LANDING COST PERCENT                    
005700        05 DALEVANM          PIC 9(8).                                    
005800*                                 DATUM LEV.ANMÄRKNING(YYYYMMDD)          
005900*                                 DISCREPANCY REPORT DATE                 
006000        05 DARETANK          PIC 9(8).                                    
006100*                                 ANKOMSTDATUM (ÅÅÅÅMMDD)                 
006200*                                 DATE GOODS RECEIVING(YYYYMMDD)          
006300        05 DARETILL          PIC 9(8).                                    
006400*                                 RETURTILLSTÅNDSDATUM (AAAAMMDD)         
006500*                                 DATE RETURNPERMIT    (YYYYMMDD)         
006600        05 FLFARLIG          PIC X.                                       
006700*                                 FARLIGT GODS-FLAGGA                     
006800*                                 DENGEROUS GOODS FLAG                    
006900        05 DARTPMN           PIC 9(8).                                    
007000*                                 PÅMINNELSE RETURTILLSTÅNDSDAT.          
007100*                                 (YYYYMMDD)                              
007200*                                 DATE RETURNPERMIT REMINDER              
007300        05 KDLEVANM-UPD      PIC X.                                       
007400*                                 STATUS LEVERANSANMÄRKNING               
007500*                                 STATUS DISCREPANCY                      
007600        05 KDVALISO          PIC X(3).                                    
007700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007800*                                 CURRENCY CODE BY ISO-STANDARD.          
007900        05 BEANST            PIC X(25).                                   
008000*                                 ANSTÄLLDS NAMN                          
008100*                                 NAME OF EMPLOYED                        
008200        05 IDUSER-ADM        PIC X(8).                                    
008300*                                 ANVÄNDAR-ID ADMINISTRATIV KONTR         
008400*                                 USER ID ADMINISTRATIVE INSPEC.          
008500        05 KDLEVATT          PIC 9.                                       
008600*                                 ATTESTERING KOD LEVERANSANM.            
008700*                                 ATTEST CODE DISCREPANCY REPORT          
008800        05 IDDC-RET          PIC X(2).                                    
008900*                                 MOTTAGANDE LAGER FÖR RETURER            
009000*                                 RECEIVING WAREHOUSE FOR RETURNS         
009100        05 IXDCCLEAR         PIC 9.                                       
009200*                                 CLEARING DC SEKVENS                     
009300*                                 POSITION FOR CLEARING DC                
009400        05 FILLER            PIC X(10).                                   
009500*** END OF VILMAII-COPY LENGTH= 148 BYTES                                 

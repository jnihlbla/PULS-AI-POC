000100 01  ANM-WDA201.                                                          
000200*                                 LEVERANSANMÄRKNINGAR                    
000300*                                 FYSISK NYCKEL: IDLEVANM                 
000400     03 ANM-IDLEVANM.                                                     
000500*                                 LEVERANSANMÄRKNINGSIDENTITET            
000600*                                 DISCREPANCY REPORT IDENTITY             
000700        05 ANM-IDDISTR       PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900*                                 DISTRICT NUMBER                         
001000        05 ANM-IDKUNDNR      PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200*                                 CUSTOMER NO                             
001300        05 ANM-IDRAPPNR      PIC 9(7).                                    
001400*                                 RAPPORT NUMMER                          
001500*                                 DISCREPANCY REPORT NUMBER               
001600     03 ANM-IDFTG            PIC 9(2).                                    
001700*                                 FÖRETAGSID EKONOM REDOVISNING           
001800*                                 COMPANY IDENTITY ACCOUNTING             
001900     03 ANM-IDPERSON         PIC S9(3)           COMP-3.                  
002000*                                 PERSONKOD                               
002100*                                 STAFF CODE                              
002200     03 ANM-IDUSER           PIC X(8).                                    
002300*                                 ANVÄNDARENS SÄKERHETS ID                
002400*                                 USER SECURITY-IDENTITY                  
002500     03 ANM-KDARBTYP         PIC X(8).                                    
002600*                                 TYP AV ARBETE                           
002700*                                 CATEGORY OF WORK                        
002800     03 ANM-KDLEVANM         PIC X.                                       
002900*                                 STATUS LEVERANSANMÄRKNING               
003000*                                 STATUS DISCREPANCY                      
003100     03 ANM-KVRADER-OBEH     PIC S9(5)           COMP-3.                  
003200*                                 ANTAL OBEHANDLADE RADER                 
003300*                                 NUMBER OF NOT TREATED LINES             
003400     03 ANM-KVRADER-RT       PIC S9(5)           COMP-3.                  
003500*                                 ANTAL RADER RETURTILLSTÅND              
003600*                                 NUMBER OF LINES RETURNPERMIT            
003700     03 ANM-PRFOERS          PIC S9(7)V9(2)      COMP-3.                  
003800*                                 FÖRSÄKRINGSPREMIE                       
003900*                                 INSURANCE FEE                           
004000     03 ANM-PRFRAKT          PIC S9(7)V9(2)      COMP-3.                  
004100*                                 FRAKTKOSTNAD                            
004200*                                 FREIGHT COST                            
004300     03 ANM-PRLEGKST         PIC S9(7)V9(2)      COMP-3.                  
004400*                                 LEGALISERINSKOSTNAD                     
004500*                                 LEGALIZATION FEE                        
004600     03 ANM-REEMBHNT         PIC S9(2)V9(1)      COMP-3.                  
004700*                                 EMB OCH HANTERINGSKOST (%)              
004800*                                 PACKING AND HANDLING (%)                
004900     03 ANM-RELANDCO         PIC S9(3)V9(2)      COMP-3.                  
005000*                                 LANDING COST PROCENT                    
005100*                                 LANDING COST PERCENT                    
005200     03 ANM-DALEVANM         PIC 9(8).                                    
005300*                                 DATUM LEV.ANMÄRKNING(YYYYMMDD)          
005400*                                 DISCREPANCY REPORT DATE                 
005500     03 ANM-DARETANK         PIC 9(8).                                    
005600*                                 ANKOMSTDATUM (ÅÅÅÅMMDD)                 
005700*                                 DATE GOODS RECEIVING(YYYYMMDD)          
005800     03 ANM-DARETILL         PIC 9(8).                                    
005900*                                 RETURTILLSTÅNDSDATUM (AAAAMMDD)         
006000*                                 DATE RETURNPERMIT    (YYYYMMDD)         
006100     03 ANM-FLFARLIG         PIC X.                                       
006200*                                 FARLIGT GODS-FLAGGA                     
006300*                                 DENGEROUS GOODS FLAG                    
006400     03 ANM-DARTPMN          PIC 9(8).                                    
006500*                                 PÅMINNELSE RETURTILLSTÅNDSDAT.          
006600*                                 (YYYYMMDD)                              
006700*                                 DATE RETURNPERMIT REMINDER              
006800     03 ANM-KDLEVANM-UPD     PIC X.                                       
006900*                                 STATUS LEVERANSANMÄRKNING               
007000*                                 STATUS DISCREPANCY                      
007100     03 ANM-KDVALISO         PIC X(3).                                    
007200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007300*                                 CURRENCY CODE BY ISO-STANDARD.          
007400     03 ANM-BEANST           PIC X(25).                                   
007500*                                 ANSTÄLLDS NAMN                          
007600*                                 NAME OF EMPLOYED                        
007700     03 ANM-IDUSER-ADM       PIC X(8).                                    
007800*                                 ANVÄNDAR-ID ADMINISTRATIV KONTR         
007900*                                 USER ID ADMINISTRATIVE INSPEC.          
008000     03 ANM-KDLEVATT         PIC 9.                                       
008100*                                 ATTESTERING KOD LEVERANSANM.            
008200*                                 ATTEST CODE DISCREPANCY REPORT          
008300     03 ANM-IDDC-RET         PIC X(2).                                    
008400*                                 MOTTAGANDE LAGER FÖR RETURER            
008500*                                 RECEIVING WAREHOUSE FOR RETURNS         
008600     03 ANM-IXDCCLEAR        PIC 9.                                       
008700*                                 CLEARING DC SEKVENS                     
008800*                                 POSITION FOR CLEARING DC                
008900     03 ANM-IDSYSTEM         PIC X(4).                                    
009000*                                 VOLVO VCCS SYSTEMNUMMER                 
009100*                                 VOLVO VCCS SYSTEM NUMBER                
009200     03 ANM-FILLER           PIC X(6).                                    
009300*** END OF VILMAII-COPY LENGTH= 145 BYTES                                 

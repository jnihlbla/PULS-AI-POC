000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W440EMOH.                                                
000400 AUTHOR.         PRERNA DADHICH.                                          
000500 DATE-WRITTEN.   MAR 2020.                                                
000600                                                                          
000700*****************************************************************         
000800*                                                                         
000900*    PROGRAM READS ALL THE BACK ORDER WHICH HAVE BIPACK CODE AS 3         
001000*    STORES ALL THE  BO IN A TABLE AND IF A NEW BO COMES IN IT            
001100*    MATCHES WITH THE PREVIOUS BO SAVED IN TABLE ,IF MATCHED              
001200*    THEN DO NOT CREATE NEW ORDER HEAD , IF NOT MATHED THEN               
001300*    CREATE NEW ORDER HEAD                                                
001400*                                                                         
001500*                                                                         
001600*    REGISTER :  WDQ2 ORDER HEAD                                          
001700*             :                                                           
001800*                                                                         
001900*    LINKAREA :  W440EMOH                                                 
002000*                                                                         
002100*****************************************************************         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800*    -COPY WY2000W1                                                       
002900     SKIP3                                                                
003000 01   IDPGM                   PIC X(08)   VALUE 'W440EMOH'.               
003100 01   FELTEXT                 PIC X(80)   VALUE SPACE.                    
003200 01   RKOD-ABEND              PIC S9(4)   VALUE +33 COMP SYNC.            
003300 01   SAME-LINE-SW            PIC X       VALUE 'N'.                      
003400 01   W-FIRST-WRITE           PIC X       VALUE 'J'.                      
003500 01  GENERELLA-SUBPROGRAM.                                                
003600     03  CBLTDLI              PIC X(8)    VALUE 'CBLTDLI '.               
003700     03  FELLOG               PIC X(8)    VALUE 'FELLOG  '.               
003800     03  ABEND                PIC X(8)    VALUE 'ABEND   '.               
003900     EJECT                                                                
004000 77  ORD-IX                   PIC 999     VALUE 1.                        
004100 77  MAX-ORD-IX               PIC 999     VALUE 200.                      
004200                                                                          
004300*ORDER HEAD TABLE WHICH IS USED TO COMPARE WITH PREVIOUS RECORD           
004400*FROM WDA5  AND IF SAME THEN NOT CREATE ORDER HEAD                        
004500                                                                          
004600 01  ORDERHEAD-TABLE.                                                     
004700    03  TAB-OHUV-TAB        OCCURS 200 TIMES.                             
004800     05  TAB-IDDISTR          PIC S9(5)   COMP-3 VALUE ZERO.              
004900     05  TAB-IDKUNDNR         PIC S9(7)   COMP-3 VALUE ZERO.              
004910     05  TAB-IDDC             PIC X(2)    VALUE SPACE.                    
005000     05  TAB-IDORDNR7         PIC 9(7)    VALUE ZERO.                     
005100     05  TAB-KDORDKL          PIC S9      COMP-3 VALUE ZERO.              
005500     05 TAB-BEGMT.                                                        
005600        07 TAB-BEGMT-RAD1     PIC X(35).                                  
005700        07 TAB-BEGMT-RAD2     PIC X(35).                                  
005800     05 TAB-ADGMT.                                                        
005900        07 TAB-ADGMT-GATA     PIC X(35).                                  
006000        07 TAB-ADGMT-PADR     PIC X(35).                                  
006100        07 TAB-ADGMT-LAND     PIC X(35).                                  
006200     05 TAB-BELAGINS-GRP.                                                 
006300        07 TAB-BELAGINS-DEL1  PIC X(60).                                  
006400        07 TAB-BELAGINS-DEL2  PIC X(60).                                  
006800                                                                          
006900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007000*                                                                         
007100 01  FILLER                   PIC X(16)   VALUE 'IMS-WS'.                 
007200     SKIP2                                                                
007300*    --- STATUS-KOD FRÅN IMS                                              
007400 01  STATUS-WS                PIC XX.                                     
007500     88  SEGMENT-FINNS                    VALUE '  '.                     
007600     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
007700     88  SEGMENT-SLUT                     VALUE 'GB'.                     
007800     SKIP2                                                                
007900 01  GODK-STATUSKODER.                                                    
008000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008100     SKIP2                                                                
008200 01  SSA1                     PIC X(64).                                  
008300 01  SSA2                     PIC X(64).                                  
008400     EJECT                                                                
008500                                                                          
008600*    --- IMS FUNKTIONSKODER                                               
008700*01  -COPY W0003                                                          
008800     EJECT                                                                
008900                                                                          
009000 01  NYCKLAR-TILL-DLI.                                                    
009100                                                                          
009200     03  W-WDQ2CSEQ-X.                                                    
009300       05  W-IDDISTR          PIC S9(5)   COMP-3   VALUE ZERO.            
009400       05  W-IDKUNDNR         PIC S9(7)   COMP-3   VALUE ZERO.            
009500       05  W-IDKUNDRF.                                                    
009600         07  W-IDORDNR7       PIC 9(7)    VALUE ZERO.                     
009700         07  FILLER           PIC X(3)    VALUE SPACE.                    
009800     03  W-IDDC-X.                                                        
009900         05  W-IDDC           PIC X(2).                                   
010000     EJECT                                                                
010100                                                                          
010200*    ---  DLI INPUT-OUTPUT AREOR                                          
010300*                                                                         
010400 01  FILLER                   PIC X(16)   VALUE 'WDQ201-AREA'.            
010500 01  DLI-IO-WDQ201.                                                       
010600*    03  -COPY WDQ201                                                     
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011000*01 -COPY W440EMOH                                                        
011100                                                                          
011200*01  -COPY W0008      -PRE  WDQ2-                                         
011300       05  FILLER                PIC X.                                   
011400     EJECT                                                                
011500                                                                          
011600 PROCEDURE DIVISION  USING EMOH-W440EMOH                                  
011700                           WDQ2-PCB .                                     
011800                                                                          
011900 MAIN SECTION.                                                            
012000****************************************************************          
012600     INITIALIZE                     EMOH-KDSVAR                           
012610     MOVE EMOH-IDDISTR           TO W-IDDISTR                             
012700     MOVE EMOH-IDKUNDNR          TO W-IDKUNDNR                            
012800     MOVE EMOH-IDORDNR7          TO W-IDORDNR7                            
012900     PERFORM IMS-GU-WDQ201                                                
013000     IF TAB-IDDISTR(1) = ZERO                                             
013500       SET  EMOH-KDSVAR-CREATE   TO TRUE                                  
013600       MOVE OHUV-IDDISTR         TO TAB-IDDISTR(ORD-IX)                   
013700       MOVE OHUV-IDKUNDNR        TO TAB-IDKUNDNR(ORD-IX)                  
013710       MOVE EMOH-IDDC            TO TAB-IDDC(ORD-IX)                      
013800       MOVE W-IDORDNR7           TO TAB-IDORDNR7(ORD-IX)                  
013810       MOVE OHUV-KDORDKL         TO TAB-KDORDKL(ORD-IX)                   
014000       MOVE OHUV-BEGMT           TO TAB-BEGMT(ORD-IX)                     
014100       MOVE OHUV-ADGMT           TO TAB-ADGMT(ORD-IX)                     
014300       MOVE OHUV-BELAGINS-DEL1   TO TAB-BELAGINS-DEL1(ORD-IX)             
014310       MOVE OHUV-BELAGINS-DEL2   TO TAB-BELAGINS-DEL2(ORD-IX)             
014400       MOVE ORD-IX               TO EMOH-IXHALV                           
014500       PERFORM S01-MOVE-ORG-PROG                                          
014600     ELSE                                                                 
014800       PERFORM A-CHECK-PREV-ORDER                                         
014900     END-IF                                                               
015000     GOBACK                                                               
015100     .                                                                    
015200     EJECT                                                                
015300                                                                          
015400 A-CHECK-PREV-ORDER SECTION.                                              
015500****************************************************************          
015600* CHECK PREV ORDER ALREADY PRESENT  IN TABLE                              
015700* IF THE NEW ORDER IS NOT SAME THEN ADD IN TABLE                          
015800****************************************************************          
015900        PERFORM                                                           
016000        VARYING ORD-IX FROM 1 BY 1                                        
016100          UNTIL ORD-IX > MAX-ORD-IX                                       
016200          OR TAB-IDDISTR (ORD-IX) = ZERO                                  
016300          OR EMOH-KDSVAR-EXISTS                                           
016410            PERFORM AA-CHECK-FROM-TABLE                                   
016500        END-PERFORM                                                       
016600                                                                          
016900        IF NOT EMOH-KDSVAR-EXISTS                                         
017200          SET  EMOH-KDSVAR-CREATE  TO  TRUE                               
017300          MOVE ORD-IX              TO EMOH-IXHALV                         
017400          MOVE OHUV-IDDISTR        TO TAB-IDDISTR(ORD-IX)                 
017410          MOVE EMOH-IDDC           TO TAB-IDDC(ORD-IX)                    
017500          MOVE OHUV-IDKUNDNR       TO TAB-IDKUNDNR(ORD-IX)                
017600          MOVE OHUV-KDORDKL        TO TAB-KDORDKL(ORD-IX)                 
017700          MOVE W-IDORDNR7          TO TAB-IDORDNR7(ORD-IX)                
017900          MOVE OHUV-BEGMT          TO TAB-BEGMT(ORD-IX)                   
018000          MOVE OHUV-ADGMT          TO TAB-ADGMT(ORD-IX)                   
018200          MOVE OHUV-BELAGINS-DEL1  TO TAB-BELAGINS-DEL1(ORD-IX)           
018210          MOVE OHUV-BELAGINS-DEL2  TO TAB-BELAGINS-DEL2(ORD-IX)           
018300          PERFORM S01-MOVE-ORG-PROG                                       
018400        END-IF                                                            
018610                                                                          
018700     IF ORD-IX >= MAX-ORD-IX                                              
018800        MOVE 'OVERLOAD ON TABLE INCREASE SIZE'                            
018900                                   TO FELTEXT                             
019000        DISPLAY FELTEXT                                                   
019100        CALL FELLOG                                                       
019200     END-IF                                                               
019300     .                                                                    
019400     EJECT                                                                
019500                                                                          
019600 AA-CHECK-FROM-TABLE SECTION.                                             
019700****************************************************************          
019800* THE NEW RECORD IS SAME AS THE VALUE IN TABLE THN SAVE THE               
019900* IDORDNR VALUE FROM THE TABLE IN EMOH-IDORDNR AND THIS VALUE             
020000* WILL MOVE TO BEDARREF IN WDA5 AND IN W411BIPA IF BEDARREF               
020100* MATCHES THEN MERGE THIS ORDER IN THE EXISTING ORDER HEAD                
020200****************************************************************          
020340                                                                          
020400       IF (TAB-IDDISTR(ORD-IX )     =   OHUV-IDDISTR       AND            
020500           TAB-IDKUNDNR(ORD-IX)     =   OHUV-IDKUNDNR      AND            
020600           TAB-KDORDKL(ORD-IX)      =   OHUV-KDORDKL       AND            
020610           TAB-IDDC(ORD-IX)         =   EMOH-IDDC          AND            
020700           TAB-BEGMT(ORD-IX)        =   OHUV-BEGMT         AND            
020800           TAB-ADGMT(ORD-IX)        =   OHUV-ADGMT         AND            
020900           TAB-BELAGINS-DEL1(ORD-IX)=   OHUV-BELAGINS-DEL1 AND            
021000           TAB-BELAGINS-DEL2(ORD-IX)=   OHUV-BELAGINS-DEL2)               
021100                                                                          
021720           SET  EMOH-KDSVAR-EXISTS TO  TRUE                               
021800           MOVE ORD-IX             TO  EMOH-IXHALV                        
021904       END-IF                                                             
022000     .                                                                    
022100     EJECT                                                                
022200 S01-MOVE-ORG-PROG  SECTION.                                              
022300****************************************************************          
022400* IF NEW ORDER THEN SEND THE ORDER DETAILS TO W440300 PGM      *          
022500****************************************************************          
022700     MOVE TAB-IDDISTR(ORD-IX )     TO EMOH-IDDISTR                        
022710     MOVE TAB-IDDC(ORD-IX )        TO EMOH-IDDC                           
022800     MOVE TAB-IDKUNDNR(ORD-IX)     TO EMOH-IDKUNDNR                       
022900     MOVE TAB-IDORDNR7(ORD-IX)     TO EMOH-IDORDNR7                       
023000     MOVE TAB-KDORDKL(ORD-IX)      TO EMOH-KDORDKL                        
023100     MOVE TAB-BEGMT(ORD-IX)        TO EMOH-BEGMT                          
023200     MOVE TAB-ADGMT(ORD-IX)        TO EMOH-ADGMT                          
023410     MOVE TAB-BELAGINS-DEL1(ORD-IX)TO EMOH-BELAGINS-DEL1                  
023420     MOVE TAB-BELAGINS-DEL2(ORD-IX)TO EMOH-BELAGINS-DEL2                  
023430     MOVE OHUV-ADBET               TO EMOH-ADBET                          
023440     MOVE OHUV-BEBET               TO EMOH-BEBET                          
023500     .                                                                    
023600     EJECT                                                                
023700                                                                          
023800 IMS-GU-WDQ201 SECTION.                                                   
023900                                                                          
024000     STRING 'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
024100          DELIMITED BY SIZE INTO SSA1                                     
024200     MOVE '  GE'               TO GODK-STATUSKODER                        
024300     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
024400     MOVE WDQ2-STATUS-CODE    TO STATUS-WS                                
024500     PERFORM IMS-STATUSKONTROLL                                           
024600     .                                                                    
024700     SKIP2                                                                
024800 IMS-STATUSKONTROLL            SECTION.                                   
024900     SKIP2                                                                
025000     SET STATUS-IX             TO 1                                       
025100     SEARCH GODK-STATUS AT END                                            
025200         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
025300              DELIMITED BY SIZE INTO FELTEXT                              
025400         CALL FELLOG                                                      
025500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
025600     END-SEARCH                                                           
025700     .                                                                    
025800     EJECT                                                                
025900*    -COPY WY2000P1                                                       

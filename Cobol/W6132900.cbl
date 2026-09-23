000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6132900.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   11/05/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        PREPARING FILE FOR DAILY REFILL                                  
001000*                                                                         
001100*        THE PROGRAM READS     WDK6                                       
001200*        THE PROGRAM READS     WDK9                                       
001300*        THE PROGRAM READS     WDT1                                       
001400*        THE PROGRAM READS     WDD8                                       
001500*        THE PROGRAM READS     WDM5                                       
001600*        THE PROGRAM READS     WDE4                                       
001700*                                                                         
001800*    CHANGE LOG:                                                          
001900*      YY/MM/DD - INITIALS        - DESCRIPTION.                          
002000*                                                                         
002100*      14/02/26 - REDDY RAHUL     - ETRACKER 10194324                     
002200*                                   CHANGE REFILL PROPOSAL CALC.          
002300*                                   SUBTRACTION OF RESERVED               
002400*                                   QUANTITY(KVRESS) REMOVED FROM         
002500*                                   AVAIL STOCK (DISP-CDC) CALC.          
002600*                                   ADD SCRAP ORDER QUANTITY TO           
002700*                                   AVAILABLE STOCK CALC.                 
002800*                                                                         
002900                                                                          
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500     SKIP2                                                                
003600*          --- UNLOAD OF WDK6 DATABASE                                    
003700     SELECT W01160                     ASSIGN TO W61329D1.                
003800     SKIP2                                                                
003900*          --- DAILY REFILL FILE FOR WDT1 DATABASE                        
004000     SELECT W61329                     ASSIGN TO W61329D2.                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W01160                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  -COPY W01160      -L.                                                
005100     SKIP3                                                                
005200 FD  W61329                                                               
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500                                                                          
005600*01  POST -COPY WDT101 -PRE  W61329-  -L.                                 
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900                                                                          
006000 77  IDPGM                       PIC X(8)  VALUE 'W6132900'.              
006100 77  DISP-PICKING-AREA           PIC S9(9) COMP-3 VALUE ZERO.             
006200 77  W-QTY                       PIC S9(9) COMP-3 VALUE ZERO.             
006300 77  DISP-CDC                    PIC S9(9) COMP-3 VALUE ZERO.             
006400 77  W-SAVE-DATE                 PIC 9(8)  VALUE ZERO.                    
006500 77  W-REQD-QTY                  PIC S9(9) COMP-3 VALUE ZERO.             
006600 77  W-KVPACK-QTY                PIC S9(9) COMP-3 VALUE ZERO.             
006700 77  W-CHECK-WDD811-DATE         PIC S9(4) COMP-3 VALUE ZERO.             
006800 77  W-COUNTER-NO                PIC S9(4) COMP-3 VALUE ZERO.             
006900 77  W-BUFF-BAL                  PIC S9(7) COMP-3 VALUE ZERO.             
007000 77  W-REMAINIG-CASE             PIC S9(5) COMP-3 VALUE ZERO.             
007100 77  W-WDK901-KVOKS              PIC S9(7) COMP-3 VALUE ZERO.             
007200 77  WS-KVANTAL-SVS              PIC S9(9) VALUE ZERO.                    
007300 77  WS-KVANTAL-CDC              PIC S9(9) VALUE ZERO.                    
007400 77  WS-SALDO-KVBUFF-F           PIC S9(7) COMP-3 VALUE ZERO.             
007500 77  WS-SALDO-KVBUFF-OF          PIC S9(7) COMP-3 VALUE ZERO.             
007600 77  WS-KVAVBART                 PIC S9(9) COMP-3 VALUE ZERO.             
007700 77  WS-KVQ3                     PIC S9(2) COMP-3 VALUE ZERO.             
007800 77  WS-ADPLATS                  PIC 9(5)         VALUE ZERO.             
007900 77  WS-KVOKS                    PIC S9(7)           COMP-3.              
008000 77  YES                         PIC X     VALUE 'J'.                     
008100 77  NOO                         PIC X     VALUE 'N'.                     
008200     SKIP2                                                                
008300 01  ERRTEXT.                                                             
008400     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
008500     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
008600                                                                          
008700*                                                                         
008800* DATE AND TIME                                                           
008900 01  DAY-DATE-TIME               PIC 9(12)    VALUE ZERO.                 
009000 01  FILLER REDEFINES DAY-DATE-TIME.                                      
009100     03  WS-CURR-DATE.                                                    
009200         05 DAY-YEAR             PIC 9(2).                                
009300         05 DAY-MONTH            PIC 9(2).                                
009400         05 DAY-DATE             PIC 9(2).                                
009500     03  DAY-HOUR                PIC 9(2).                                
009600     03  DAY-MIN                 PIC 9(2).                                
009700     03  DAY-SEC                 PIC 9(2).                                
009800                                                                          
009900  01 W-ARB-TIRFS                 PIC 9(10).                               
010000  01 FILLER REDEFINES W-ARB-TIRFS.                                        
010100     03  W-ARB-TIRFS-YYMMDD      PIC 9(6).                                
010200     03  W-ARB-TIRFS-HHMM        PIC 9(4).                                
010300*                                                                         
010400 01  WS-SKIP-PRC                 PIC  X(4).                               
010500     88 SKIP-PRC                 VALUES '502Y'                            
010600                                        '8020'                            
010700                                        '9010'                            
010800                                        '9011'                            
010900                                        '9909'                            
011000                                        '990P'                            
011100                                        '990G'                            
011200                                        '990E'                            
011300                                        '990J'                            
011400                                        '990L'                            
011500                                        '990H'                            
011600                                        '9989'                            
011700                                        '9980'                            
011800                                        '9981'.                           
011900*                                                                         
012000 01  WS-SKIP-ADLAGOMR            PIC  9(3).                               
012100     88  SKIP-ADLAGOMR           VALUE 0 16 26                            
012200                                       40 THRU 49                         
012300                                       50 THRU 59                         
012400                                       60 THRU 69                         
012500                                       80 THRU 89                         
012600                                       90 99.                             
012700*                                                                         
012800 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
012900     88  END-OF-W01160                       VALUE 'Y'.                   
013000                                                                          
013100 77  WDK611-EOF-SW               PIC X       VALUE 'N'.                   
013200     88  NO-MORE-DATEGT-YES                  VALUE 'Y'.                   
013300     88  NO-MORE-DATEGT-NO                   VALUE 'N'.                   
013400                                                                          
013500 77  WDT101-FLAG                 PIC X       VALUE 'N'.                   
013600     88  VALID-WDT101-SEG                    VALUE 'J'.                   
013700     88  INVALID-WDT101-SEG                  VALUE 'N'.                   
013800                                                                          
013900 77  STOP-PROCESS-SW             PIC X       VALUE 'N'.                   
014000     88  STOP-PROCESS                        VALUE 'J'.                   
014100     88  STOP-PROCESS-NO                     VALUE 'N'.                   
014200                                                                          
014300 77  WDT1-WRITE-STATUS           PIC X       VALUE 'N'.                   
014400     88  RECD-WRITTEN-TO-WDT1                VALUE 'J'.                   
014500     88  RECD-NOT-WRITTEN-TO-WDT1            VALUE 'N'.                   
014600                                                                          
014700     EJECT                                                                
014800 01  GENERAL-SUBPROGRAMS.                                                 
014900*                                                                         
015000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
015300     EJECT                                                                
015400*    --- PARAMETRAR TILL POSTSUM                                          
015500*                                                                         
015600*01  -COPY W0005   -PRE  POSTSUM-                                         
015700     EJECT                                                                
015800 01  W01160-AREA-START           PIC X(24)   VALUE                        
015900                                             'W01160-AREA-START'.         
016000     SKIP2                                                                
016100                                                                          
016200*01  AREA -COPY W01160     -PRE W01160-                                   
016300     EJECT                                                                
016400 01  W61329-AREA-START           PIC X(24)   VALUE                        
016500                                             'W61329-AREA-START'.         
016600     SKIP2                                                                
016700                                                                          
016800*01  AREA -COPY WDT101     -PRE W61329-                                   
016900*                                                                         
017000     EJECT                                                                
017100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017200     SKIP3                                                                
017300 01  KEYS-TILL-DLI.                                                       
017400     03  W-IDARTNR-X.                                                     
017500         05  W-IDARTNR           PIC S9(9) COMP-3 VALUE ZERO.             
017600     03  W-WDK611KY-X.                                                    
017700         05  W-WDK611KY          PIC X(1)    VALUE '1'.                   
017800     03  W-WDD811KY-MIN-X.                                                
017900         05  W-IDDC-WDD8-MIN     PIC X(2)    VALUE '11'.                  
018000         05  W-ADBUFFOMR-MIN    PIC S9(3) COMP-3 VALUE ZERO.              
018100         05  W-DABUFPAF-MIN      PIC 9(8)    VALUE ZERO.                  
018200         05  W-ADBUFGAN-MIN     PIC S9(3) COMP-3 VALUE ZERO.              
018300         05  W-ADBUFPL-MIN      PIC S9(5) COMP-3 VALUE ZERO.              
018400     03  W-WDD811KY-MAX-X.                                                
018500         05  W-IDDC-WDD8-MAX     PIC X(2)    VALUE '11'.                  
018600         05  W-ADBUFFOMR-MAX      PIC S9(3) COMP-3                        
018700                                             VALUE +999.                  
018800         05  W-DABUFPAF-MAX      PIC 9(8)    VALUE 99999999.              
018900         05  W-ADBUFGAN-MAX      PIC S9(3) COMP-3                         
019000                                             VALUE +999.                  
019100         05  W-ADBUFPL-MAX       PIC S9(5) COMP-3                         
019200                                             VALUE +99999.                
019300     03  W-WDT101KY-MIN-X.                                                
019400         05  W-IDDC-WDT1-MIN     PIC X(2)   VALUE '11'.                   
019500         05  W-IDARTNR-MIN       PIC S9(9) COMP-3 VALUE ZERO.             
019600         05  W-TIORDTIME-MIN     PIC 9(12)  VALUE ZERO.                   
019700     03  W-WDT101KY-MAX-X.                                                
019800         05  W-IDDC-WDT1-MAX     PIC X(2)   VALUE '11'.                   
019900         05  W-IDARTNR-MAX       PIC S9(9) COMP-3                         
020000                                            VALUE +999999999.             
020100         05  W-TIORDTIME-MAX     PIC 9(12)  VALUE 999999999999.           
020200     03  W-DABUFPAF-X.                                                    
020300         05  W-DABUFPAF          PIC 9(8)   VALUE ZERO.                   
020400     03  W-ADBUFFOM-X.                                                    
020500         05  W-ADBUFFOM          PIC S9(3) COMP-3 VALUE ZERO.             
020600     03  W-ADBUFFOM-Y.                                                    
020700         05  W-ADBUFFOM-1        PIC S9(3) COMP-3 VALUE ZERO.             
020800     03  W-ADBUFFOM-59-X.                                                 
020900         05  W-ADBUFFOM-59       PIC S9(3) COMP-3 VALUE 59.               
021000     03  W-ADBUFFOM-50-X.                                                 
021100         05  W-ADBUFFOM-50       PIC S9(3) COMP-3 VALUE 50.               
021200     03  W-ADBUFFOM-52-X.                                                 
021300         05  W-ADBUFFOM-52       PIC S9(3) COMP-3 VALUE 52.               
021400     03  W-ADBUFFOM-7-X.                                                  
021500         05  IMS-GNP-WDD8        PIC S9(3) COMP-3 VALUE  7.               
021600     03  W-ADBUFFOM-8-X.                                                  
021700         05  W-ADBUFFOM-8        PIC S9(3) COMP-3 VALUE  8.               
021800     03  W-ADBUFFOM-6-X.                                                  
021900         05  W-ADBUFFOM-6        PIC S9(3) COMP-3 VALUE  6.               
022000                                                                          
022100     03  W-ADTRDEST-X.                                                    
022200         05  W-ADTRDEST          PIC X(3)    VALUE SPACE.                 
022300                                                                          
022400     03  W-IDTRPTNR-X.                                                    
022500         05  W-IDTRPTNR          PIC S9(5)   VALUE ZERO COMP-3.           
022600                                                                          
022700     03  W-WDE4C1KY-MIN-X.                                                
022800         05  W-IDARTNR-WDE4C-MIN PIC S9(9) COMP-3 VALUE ZERO.             
022900         05  FILLER              PIC X(7)  VALUE LOW-VALUE.               
023000                                                                          
023100     03  W-WDE4C1KY-MAX-X.                                                
023200         05  W-IDARTNR-WDE4C-MAX PIC S9(9) COMP-3 VALUE ZERO.             
023300         05  FILLER              PIC X(7)  VALUE HIGH-VALUE.              
023400                                                                          
023500     03  W-WDQ4B1KY-MAX.                                                  
023600         05  W-IDARTNR-WDQ4B-MAX PIC S9(9)    COMP-3.                     
023700         05  FILLER              PIC X(32)    VALUE HIGH-VALUE.           
023800                                                                          
023900     03  W-WDQ4B1KY-MIN.                                                  
024000         05  W-IDARTNR-WDQ4B-MIN PIC S9(9)    COMP-3.                     
024100         05  FILLER              PIC X(32)    VALUE LOW-VALUE.            
024200                                                                          
024300     03  W-IDDC-WDQ4-X.                                                   
024400         05  W-IDDC-WDQ4         PIC X(2)     VALUE '11'.                 
024500                                                                          
024600     03  W-IDORDER-WDQ2-X.                                                
024700         05 W-IDORDER-WDQ2       PIC S9(7)    COMP-3.                     
024800                                                                          
024900     03  W-ADLAGOMR-WDQ2-X.                                               
025000         05 W-ADLAGOMR-WDQ2      PIC S9(3)      COMP-3.                   
025100                                                                          
025200     03  W-IDDC-WDQ2-X.                                                   
025300         05  W-IDDC-WDQ2         PIC X(2).                                
025400                                                                          
025500     03  W-WDQ3DSEQ-X.                                                    
025600         05  W-IDPRODNR-WDQ3-X       PIC S9(7)  COMP-3.                   
025700         05  W-IDPLKLST-WDQ3-X       PIC S9(3)  COMP-3.                   
025800                                                                          
025900*PRINTED ORDERS                                                           
026000     03  W-KDRADSTA-X.                                                    
026100         05  W-KDRADSTA          PIC S9    COMP-3 VALUE +3.               
026200                                                                          
026300*SCRAP ORDERS                                                             
026400     03  W-IDDISTR-81-X.                                                  
026500         05  W-IDDISTR-81     PIC S9(5) COMP-3 VALUE +81.                 
026600     03  W-IDDISTR-98-X.                                                  
026700         05  W-IDDISTR-98     PIC S9(5) COMP-3 VALUE +98.                 
026800                                                                          
026900     03  W-WDE401KY-X.                                                    
027000         05  W-IDDISTR           PIC S9(5) COMP-3 VALUE ZERO.             
027100         05  W-IDKUNDNR          PIC S9(7) COMP-3 VALUE ZERO.             
027200         05  W-IDKUNDRF          PIC X(10) VALUE SPACE.                   
027300         05  W-IDPRODNR          PIC S9(7) COMP-3 VALUE ZERO.             
027400         05  W-IDPLKLST          PIC S9(3) COMP-3 VALUE ZERO.             
027500                                                                          
027600     03  W-IDPURAD-X.                                                     
027700         05  W-IDPURAD           PIC S9(5) COMP-3 VALUE ZERO.             
027800                                                                          
027900     SKIP2                                                                
028000*    --- STATUS-KOD FRÅN IMS                                              
028100 01  STATUS-WS                   PIC XX.                                  
028200     88  SEGMENT-FOUND                       VALUE '  '.                  
028300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
028400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
028500     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
028600     88  IMS-NOT-OK                          VALUE 'XD'.                  
028700     SKIP2                                                                
028800 01  GOOD-STATUSCODES.                                                    
028900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029000     SKIP3                                                                
029100 01  SSA1                        PIC X(400).                              
029200 01  SSA2                        PIC X(64).                               
029300     EJECT                                                                
029400*    --- IMS FUNCTION CODES                                               
029500*01  -COPY W0003                                                          
029600     EJECT                                                                
029700*    ---  DLI INPUT-OUTPUT AREA                                           
029800                                                                          
029900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
030000 01  DLI-IO-WDK601.                                                       
030100*    03  -COPY WDK601                                                     
030200                                                                          
030300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
030400 01  DLI-IO-WDK611.                                                       
030500*    03  -COPY WDK611                                                     
030600                                                                          
030700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT101'.                      
030800 01  DLI-IO-WDT101.                                                       
030900*    03  -COPY WDT101 -PRE WDT101-                                        
031000                                                                          
031100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
031200 01  DLI-IO-WDK901.                                                       
031300*    03  -COPY WDK901 -PRE WDK901-                                        
031400                                                                          
031500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD801'.                      
031600 01  DLI-IO-WDD801.                                                       
031700*    03  -COPY WDD801 -PRE WDD801-                                        
031800                                                                          
031900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD811'.                      
032000 01  DLI-IO-WDD811.                                                       
032100*    03  -COPY WDD811 -PRE WDD811-                                        
032200                                                                          
032300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM501'.                      
032400 01  DLI-IO-WDM501.                                                       
032500*    03  -COPY WDM501                                                     
032600     EJECT                                                                
032700                                                                          
032800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM511'.                      
032900 01  DLI-IO-WDM511.                                                       
033000*    03  -COPY WDM511                                                     
033100     EJECT                                                                
033200                                                                          
033300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM521'.                      
033400 01  DLI-IO-WDM521.                                                       
033500*    03  -COPY WDM521                                                     
033600     EJECT                                                                
033700                                                                          
033800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE4C1'.                      
033900 01  DLI-IO-WDE4C1.                                                       
034000*    03  -COPY WDE4C1                                                     
034100     EJECT                                                                
034200                                                                          
034300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDE411'.                      
034400 01  DLI-IO-WDE411.                                                       
034500*    03  -COPY WDE411                                                     
034600                                                                          
034700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDQ4B1'.                      
034800 01  DLI-IO-WDQ4B1.                                                       
034900*    03  -COPY WDQ4B1                                                     
035000                                                                          
035100 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDQ201'.         
035200 01  DLI-IO-WDQ201.                                                       
035300*    03  -COPY WDQ201                                                     
035400                                                                          
035500 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDQ212'.         
035600 01  DLI-IO-WDQ212.                                                       
035700*    03  -COPY WDQ212                                                     
035800                                                                          
035900 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDQ221'.         
036000 01  DLI-IO-WDQ221.                                                       
036100*    03  -COPY WDQ221                                                     
036200                                                                          
036300 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDQ301'.         
036400 01  DLI-IO-WDQ301.                                                       
036500*    03  -COPY WDQ301                                                     
036600     EJECT                                                                
036700 LINKAGE SECTION.                                                         
036800                                                                          
036900*01  -COPY W0008  -PRE WDK6-                                              
037000     05  FILLER                  PIC X.                                   
037100                                                                          
037200*01  -COPY W0008  -PRE WDT1-                                              
037300     05  FILLER                  PIC X.                                   
037400                                                                          
037500*01  -COPY W0008  -PRE WDD8-                                              
037600     05  FILLER                  PIC X.                                   
037700                                                                          
037800*01  -COPY W0008  -PRE WDK9-                                              
037900     05  FILLER                  PIC X.                                   
038000                                                                          
038100*01  -COPY W0008  -PRE WDM5-                                              
038200     05  FILLER                  PIC X.                                   
038300                                                                          
038400*01  -COPY W0008  -PRE WDQ2-                                              
038500     05  FILLER                  PIC X.                                   
038600                                                                          
038700*01  -COPY W0008  -PRE WDQ3-                                              
038800     05  FILLER                  PIC X.                                   
038900                                                                          
039000*01  -COPY W0008  -PRE WDE4-                                              
039100     05  FILLER                  PIC X.                                   
039200                                                                          
039300*01  -COPY W0008  -PRE WDE4C-                                             
039400     05  FILLER                  PIC X.                                   
039500                                                                          
039600*01  -COPY W0008  -PRE WDQ4B-                                             
039700     05  FILLER                  PIC X.                                   
039800                                                                          
039900 PROCEDURE DIVISION  USING WDK6-PCB WDT1-PCB                              
040000                           WDD8-PCB WDK9-PCB WDM5-PCB                     
040100                           WDQ2-PCB WDQ3-PCB                              
040200                           WDE4-PCB WDE4C-PCB WDQ4B-PCB.                  
040300 MAIN SECTION.                                                            
040400     ENTRY 'DLITCBL' USING WDK6-PCB WDT1-PCB                              
040500                           WDD8-PCB WDK9-PCB WDM5-PCB                     
040600                           WDQ2-PCB WDQ3-PCB                              
040700                           WDE4-PCB WDE4C-PCB WDQ4B-PCB.                  
040800                                                                          
040900     PERFORM A-INIT                                                       
041000     PERFORM S01-READ-W01160                                              
041100     PERFORM UNTIL END-OF-W01160                                          
041200       MOVE W01160-CLAG-ADLAGOMR TO WS-SKIP-ADLAGOMR                      
041300       IF W01160-CLAG-KDERS < 21 AND NOT SKIP-ADLAGOMR AND                
041400          (W01160-CLAG-KDLEVSP NOT = 20 AND 21)                           
041500          MOVE W01160-CLAG-IDARTNR TO W-IDARTNR                           
041600          PERFORM IMS-GU-WDK611                                           
041700          IF CLAG-KDERS < 21 AND NOT SKIP-ADLAGOMR AND                    
041800             (CLAG-KDLEVSP NOT = 20 AND 21) AND                           
041900             CLAG-KVREFBER-PLOCK > 0                                      
042000             SET RECD-NOT-WRITTEN-TO-WDT1  TO TRUE                        
042100             PERFORM S02-INIT-WORKING-VAR                                 
042200                                                                          
042300**           FIND CURRENT STOCK BALANCE IN PICKING AREA                   
042400             PERFORM S03-CALC-QTY-CDC                                     
042500*                                                                         
042600             MOVE CLAG-KVREFBER-PLOCK TO W-REQD-QTY                       
042700             MOVE CLAG-ADPLATS TO WS-ADPLATS                              
042800             IF (CLAG-ADLAGOMR = 20 AND                                   
042900                CLAG-KVQPACK-3 > 1 AND                                    
043000                CLAG-KVMAXPL > 0) OR                                      
043100                (CLAG-ADLAGOMR = 31 AND                                   
043200                CLAG-ADGANG > 39 AND CLAG-ADGANG < 42 AND                 
043300                CLAG-KVQPACK-3 > 1 AND                                    
043400                CLAG-KVMAXPL > 0) OR                                      
043500                (CLAG-ADLAGOMR = 11 AND                                   
043600                CLAG-KVQPACK-3 > 1 AND                                    
043700                CLAG-KVMAXPL > 0 AND                                      
043800                (CLAG-ADGANG = 2 AND (CLAG-ADPLATS > 104                  
043900                                 AND CLAG-ADPLATS < 136)                  
044000                                 AND (WS-ADPLATS (5:1) =                  
044100                                 1 OR 3 OR 5 OR 7 OR 9)) OR               
044200                (CLAG-ADGANG = 7 AND (CLAG-ADPLATS > 390                  
044300                                 AND CLAG-ADPLATS < 470)                  
044400                                 AND (WS-ADPLATS (5:1) =                  
044500                                 1 OR 3 OR 5 OR 7 OR 9)))                 
044600                COMPUTE WS-KVQ3 = (CLAG-KVMAXPL - DISP-CDC)               
044700                        / CLAG-KVQPACK-3                                  
044800                IF WS-KVQ3 > 0                                            
044900                  COMPUTE W-REQD-QTY = WS-KVQ3 *                          
045000                                        CLAG-KVQPACK-3                    
045100                END-IF                                                    
045200             END-IF                                                       
045300                                                                          
045400**           IF STOCK BALANCE IN PICKING AREA IS LESS THAN                
045500**           REFILL-POINT, PROCESS WDD811 SEGMENTS FOR REFILL             
045600             IF DISP-CDC <= CLAG-KVREFPKT-PLOCK                           
045700                PERFORM S05-FIND-KVQPACK                                  
045800                MOVE W01160-CLAG-IDARTNR TO W-IDARTNR-X                   
045900                PERFORM IMS-GU-WDD801                                     
046000                IF SEGMENT-FOUND                                          
046100                  PERFORM S06-REFILL-FROM-BUFFER                          
046200                END-IF                                                    
046300             END-IF                                                       
046400          ELSE                                                            
046500             CONTINUE                                                     
046600          END-IF                                                          
046700       ELSE                                                               
046800         CONTINUE                                                         
046900       END-IF                                                             
047000       PERFORM S01-READ-W01160                                            
047100     END-PERFORM                                                          
047200                                                                          
047300     PERFORM Z-FINIT                                                      
047400                                                                          
047500     MOVE ZERO TO RETURN-CODE                                             
047600     GOBACK                                                               
047700     .                                                                    
047800                                                                          
047900 A-INIT SECTION.                                                          
048000     OPEN INPUT W01160                                                    
048100                                                                          
048200     OPEN OUTPUT W61329                                                   
048300                                                                          
048400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
048500** MOVING CURRENT DATE AND TIME                                           
048600     MOVE FUNCTION CURRENT-DATE(3:12) TO DAY-DATE-TIME                    
048700**   ADD 10 MINS SO THAT TIMESTAMPS ARE UNIQUE FROM PREVIOUS              
048800**   RUN W61321 IN SAME ROTUINE                                           
048900     ADD 1000 TO DAY-DATE-TIME                                            
049000     MOVE 0 TO DAY-SEC                                                    
049100     .                                                                    
049200                                                                          
049300 Z-FINIT SECTION.                                                         
049400     CLOSE W01160                                                         
049500           W61329                                                         
049600                                                                          
049700     MOVE 'S' TO POSTSUM-OPKOD                                            
049800     CALL POSTSUM USING POSTSUM-PARM                                      
049900     .                                                                    
050000                                                                          
050100** READ INPUT FILE W01160                                                 
050200 S01-READ-W01160  SECTION.                                                
050300     READ W01160 INTO W01160-AREA                                         
050400     AT END                                                               
050500        SET END-OF-W01160 TO TRUE                                         
050600                                                                          
050700     NOT AT END                                                           
050800        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
050900        MOVE 'W61329D1' TO POSTSUM-DDNAMN2                                
051000        MOVE SPACE      TO POSTSUM-TRANSTYP                               
051100        CALL POSTSUM USING POSTSUM-PARM                                   
051200     END-READ                                                             
051300     .                                                                    
051400                                                                          
051500 S02-INIT-WORKING-VAR SECTION.                                            
051600     MOVE ZERO TO W-QTY                                                   
051700                  DISP-CDC                                                
051800                  DISP-PICKING-AREA                                       
051900                  WS-KVAVBART                                             
052000                                                                          
052100     SET INVALID-WDT101-SEG TO TRUE                                       
052200     .                                                                    
052300                                                                          
052400** CALCULATION TO FIND CURRENT STOCK BALANCE IN PICKING AREA              
052500 S03-CALC-QTY-CDC SECTION.                                                
052600     PERFORM S03A-CALC-QTY-CDC                                            
052700                                                                          
052800     MOVE LOW-VALUE             TO W-WDQ4B1KY-MIN                         
052900     MOVE HIGH-VALUE            TO W-WDQ4B1KY-MAX                         
053000                                                                          
053100     MOVE ZERO                  TO WS-KVOKS                               
053200                                                                          
053300**   READ WDQ4B, WDQ2 TO GET THE OKS (ORDER QUEUE BALANCE) FOR RFS        
053400**    <= TODAY.                                                           
053500*    MOVE W01160-CLAG-IDARTNR   TO W-IDARTNR-WDQ4B-MIN                    
053600*                                  W-IDARTNR-WDQ4B-MAX                    
053700*    PERFORM IMS-GU-WDQ4B1                                                
053800*    PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
053900*       MOVE SEQB-IDDC          TO W-IDDC-WDQ2-X                          
054000*       MOVE SEQB-IDORDER       TO W-IDORDER-WDQ2-X                       
054100*       PERFORM S03E-READ-WDQ2                                            
054200*       PERFORM IMS-GN-WDQ4B1                                             
054300*    END-PERFORM                                                          
054400                                                                          
054500**   CALCULATION OF STOCK IN BUFFER AREA - SALDO-KVBUFF-F/-OF             
054600     PERFORM S03B-GET-BUFFER-BALANCE                                      
054700                                                                          
054800**   CALCULATION OF VALUES FROM WDM521                                    
054900     PERFORM S03C-READ-WDM521                                             
055000                                                                          
055100**   GET THE EFR                                                          
055200     IF CLAG-KVEFRS > 0                                                   
055300       PERFORM S03D-READ-WDE4                                             
055400     END-IF                                                               
055500                                                                          
055600     COMPUTE DISP-CDC = DISP-PICKING-AREA                                 
055700                        - CLAG-KVLS-SVS                                   
055800                        - CLAG-KVUTRS                                     
055900                        - CLAG-KVSPARR-KVAL                               
056000                        - CLAG-KVLS-CD (1)                                
056100                        - CLAG-KVLS-CD (2)                                
056200                        - CLAG-KVLS-CD (3)                                
056300                        - CLAG-KVLS-CD (4)                                
056400*                       - WS-KVOKS                                        
056500                        - WS-SALDO-KVBUFF-F                               
056600                        - WS-SALDO-KVBUFF-OF                              
056700                        - WS-KVANTAL-SVS                                  
056800                        + WS-KVANTAL-CDC                                  
056900                        + WS-KVAVBART                                     
057000     .                                                                    
057100                                                                          
057200** CALCULATION TO FIND STOCK BALANCE IN PICKING AREA                      
057300 S03A-CALC-QTY-CDC SECTION.                                               
057400     MOVE W01160-CLAG-IDARTNR   TO W-IDARTNR-MIN                          
057500                                   W-IDARTNR-MAX                          
057600     PERFORM IMS-GU-WDT101                                                
057700     IF SEGMENT-FOUND                                                     
057800        PERFORM S04A-CALC-QTY                                             
057900        PERFORM IMS-GN-WDT101                                             
058000        PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                   
058100          PERFORM S04A-CALC-QTY                                           
058200          PERFORM IMS-GN-WDT101                                           
058300        END-PERFORM                                                       
058400*                                                                         
058500        COMPUTE DISP-PICKING-AREA = DISP-PICKING-AREA                     
058600                          + CLAG-KVLS                                     
058700     ELSE                                                                 
058800        COMPUTE DISP-PICKING-AREA = CLAG-KVLS                             
058900     END-IF                                                               
059000     .                                                                    
059100                                                                          
059200 S03B-GET-BUFFER-BALANCE SECTION.                                         
059300     MOVE ZERO TO WS-SALDO-KVBUFF-F                                       
059400                  WS-SALDO-KVBUFF-OF                                      
059500     MOVE W01160-CLAG-IDARTNR TO W-IDARTNR-X                              
059600     PERFORM IMS-GU-WDD801                                                
059700     IF SEGMENT-FOUND                                                     
059800       PERFORM IMS-GNP-WDD811                                             
059900       PERFORM UNTIL SEGMENT-MISSING                                      
060000         COMPUTE WS-SALDO-KVBUFF-F  = WS-SALDO-KVBUFF-F +                 
060100                                      WDD811-SALDO-KVBUFF-F               
060200         COMPUTE WS-SALDO-KVBUFF-OF = WS-SALDO-KVBUFF-OF +                
060300                                      WDD811-SALDO-KVBUFF-OF              
060400         PERFORM IMS-GNP-WDD811                                           
060500       END-PERFORM                                                        
060600     END-IF                                                               
060700                                                                          
060800     IF (WS-SALDO-KVBUFF-F + WS-SALDO-KVBUFF-OF) > CLAG-KVLS              
060900        MOVE CLAG-KVLS            TO WS-SALDO-KVBUFF-F                    
061000        MOVE ZERO                 TO WS-SALDO-KVBUFF-OF                   
061100     END-IF                                                               
061200     .                                                                    
061300                                                                          
061400** CALCULATION OF VALUES FROM WDM521                                      
061500 S03C-READ-WDM521 SECTION.                                                
061600*    FIND  QTY BEING MOVED TO CDC FROM SVS                                
061700     MOVE ZERO               TO WS-KVANTAL-CDC                            
061800     MOVE 'CDC'              TO W-ADTRDEST                                
061900     PERFORM IMS-GU-WDM501                                                
062000     PERFORM IMS-GNP-WDM521                                               
062100                                                                          
062200     PERFORM UNTIL SEGMENT-MISSING                                        
062300       IF AVG-KDTRPSTA = 'P' OR 'L'                                       
062400         IF AVG-IDARTNR = W-IDARTNR                                       
062500           ADD AVG-KVANTAL   TO WS-KVANTAL-CDC                            
062600         END-IF                                                           
062700       END-IF                                                             
062800       PERFORM IMS-GNP-WDM521                                             
062900     END-PERFORM                                                          
063000*                                                                         
063100*    FIND  QTY BEING MOVED TO SVS FROM CDC                                
063200     MOVE ZERO               TO WS-KVANTAL-SVS                            
063300     MOVE 'SVS'              TO W-ADTRDEST                                
063400     PERFORM IMS-GU-WDM501                                                
063500     PERFORM IMS-GNP-WDM521                                               
063600                                                                          
063700     PERFORM UNTIL SEGMENT-MISSING                                        
063800       IF AVG-KDTRPSTA = 'P' OR 'L'                                       
063900         IF AVG-IDARTNR = W-IDARTNR                                       
064000           ADD AVG-KVANTAL   TO WS-KVANTAL-SVS                            
064100         END-IF                                                           
064200       END-IF                                                             
064300       PERFORM IMS-GNP-WDM521                                             
064400     END-PERFORM                                                          
064500     .                                                                    
064600                                                                          
064700 S03D-READ-WDE4 SECTION.                                                  
064800                                                                          
064900     MOVE W01160-CLAG-IDARTNR    TO W-IDARTNR-WDE4C-MIN                   
065000                                    W-IDARTNR-WDE4C-MAX                   
065100     PERFORM IMS-GU-WDE4C1                                                
065200     PERFORM                                                              
065300       UNTIL SEGMENT-MISSING OR                                           
065400             SEGMENT-NOMORE                                               
065500       MOVE SEQC-IDDISTR         TO W-IDDISTR                             
065600       MOVE SEQC-IDKUNDNR        TO W-IDKUNDNR                            
065700       MOVE SEQC-IDKUNDRF        TO W-IDKUNDRF                            
065800       MOVE SEQC-IDPRODNR        TO W-IDPRODNR                            
065900                                    W-IDPRODNR-WDQ3-X                     
066000       MOVE SEQC-IDPLKLST        TO W-IDPLKLST                            
066100                                    W-IDPLKLST-WDQ3-X                     
066200       MOVE SEQC-IDPURAD         TO W-IDPURAD                             
066300       PERFORM IMS-GU-WDQ301-DSEQ                                         
066400       IF SEGMENT-FOUND                                                   
066500          MOVE ODEL-IDPRC           TO WS-SKIP-PRC                        
066600          IF ODEL-IDDC = '11' AND SKIP-PRC                                
066700             PERFORM IMS-GU-WDE411                                        
066800             IF SEGMENT-FOUND                                             
066900               ADD ORAD-KVAVBART       TO WS-KVAVBART                     
067000             END-IF                                                       
067100          END-IF                                                          
067200       END-IF                                                             
067300       PERFORM IMS-GN-WDE4C1                                              
067400     END-PERFORM                                                          
067500     .                                                                    
067600                                                                          
067700**   READ WDQ2 TO GET THE OKS (ORDER QUEUE BALANCE) FOR RFS               
067800**    <= TODAY.                                                           
067900                                                                          
068000 S03E-READ-WDQ2 SECTION.                                                  
068100                                                                          
068200     PERFORM IMS-GU-WDQ212                                                
068300                                                                          
068400     IF SEGMENT-FOUND                                                     
068500                                                                          
068600        MOVE ARB-TIRFS          TO W-ARB-TIRFS                            
068700                                                                          
068800        IF W-ARB-TIRFS-YYMMDD <= WS-CURR-DATE                             
068900                                                                          
069000           MOVE SEQB-ADLAGOMR   TO W-ADLAGOMR-WDQ2-X                      
069100                                                                          
069200           PERFORM IMS-GNP-WDQ221                                         
069300                                                                          
069400           IF SEGMENT-FOUND                                               
069500                                                                          
069600              MOVE LOR-IDPRC    TO WS-SKIP-PRC                            
069700                                                                          
069800              IF NOT SKIP-PRC                                             
069900                 COMPUTE WS-KVOKS = WS-KVOKS + SEQB-KVBEART-Q             
070000              END-IF                                                      
070100                                                                          
070200           END-IF                                                         
070300                                                                          
070400        END-IF                                                            
070500     END-IF                                                               
070600     .                                                                    
070700                                                                          
070800 S04A-CALC-QTY SECTION.                                                   
070900     IF WDT101-PF-KDSTAPF NOT EQUAL 'A'                                   
071000       SET VALID-WDT101-SEG TO TRUE                                       
071100**       TO CHECK IF PARTS ARE ON THE WAY TO PICKING AREA                 
071200         IF WDT101-PF-ADLAGOMR-TOM = CLAG-ADLAGOMR                        
071300           IF WDT101-PF-KVBEST-ANDR > 0                                   
071400             COMPUTE DISP-PICKING-AREA = DISP-PICKING-AREA +              
071500                                         WDT101-PF-KVBEST-ANDR            
071600           ELSE                                                           
071700             COMPUTE DISP-PICKING-AREA = DISP-PICKING-AREA +              
071800                                         WDT101-PF-KVBEST                 
071900           END-IF                                                         
072000         END-IF                                                           
072100     END-IF                                                               
072200     .                                                                    
072300     EJECT                                                                
072400 S05-FIND-KVQPACK SECTION.                                                
072500     MOVE 0 TO W-KVPACK-QTY                                               
072600                                                                          
072700     IF CLAG-ADLAGOMR = 10                                                
072800       CONTINUE                                                           
072900     ELSE                                                                 
073000       IF CLAG-KVQPACK-3 > 1                                              
073100         MOVE CLAG-KVQPACK-3             TO W-KVPACK-QTY                  
073200       ELSE                                                               
073300         IF CLAG-KVQPACK-2 > 1                                            
073400           MOVE CLAG-KVQPACK-2           TO W-KVPACK-QTY                  
073500         ELSE                                                             
073600           IF CLAG-KVQPACK-0 > 1                                          
073700             MOVE CLAG-KVQPACK-0           TO W-KVPACK-QTY                
073800           ELSE                                                           
073900             IF CLAG-KVQPACK-1 > 1                                        
074000                MOVE CLAG-KVQPACK-1           TO W-KVPACK-QTY             
074100             END-IF                                                       
074200           END-IF                                                         
074300         END-IF                                                           
074400       END-IF                                                             
074500     END-IF                                                               
074600     .                                                                    
074700     EJECT                                                                
074800 S06-REFILL-FROM-BUFFER SECTION.                                          
074900**     PROCESS WDD811 SEGMENTS FOR ADBUFFOMR EQ TO 1                      
075000     PERFORM S12-PROCESS-QTY-ADBUFFOMR                                    
075100                                                                          
075200     IF W-REQD-QTY > 0                                                    
075300**     PROCESS WDD811 SEGMENTS FOR DATE EQUAL TO ZERO                     
075400       MOVE ZERO TO W-SAVE-DATE                                           
075500       PERFORM S11-PROCESS-QTY-NODATE                                     
075600     END-IF                                                               
075700                                                                          
075800     IF W-REQD-QTY > 0                                                    
075900**   PROCESS WDD811 SEGMENTS FOR DATE > 0                                 
076000       MOVE ZERO TO W-SAVE-DATE                                           
076100       SET NO-MORE-DATEGT-NO TO TRUE                                      
076200       PERFORM S06A-PROCESS-QTY-DATE                                      
076300       PERFORM UNTIL NO-MORE-DATEGT-YES                                   
076400          PERFORM S06A-PROCESS-QTY-DATE                                   
076500       END-PERFORM                                                        
076600     END-IF                                                               
076700                                                                          
076800     IF W-REQD-QTY > 0                                                    
076900**     PROCESS WDD811 SEGMENTS FOR ADBUFFOMR EQ TO 50 OR 52               
077000       PERFORM S15-PROCESS-QTY-ADBUFFOMR                                  
077100     END-IF                                                               
077200                                                                          
077300     IF W-REQD-QTY > 0                                                    
077400**     PROCESS WDD811 SEGMENTS FOR ADBUFFOMR EQ TO 6 OR 8                 
077500       PERFORM S13-PROCESS-QTY-ADBUFFOMR                                  
077600     END-IF                                                               
077700                                                                          
077800     IF W-REQD-QTY > 0                                                    
077900**     PROCESS WDD811 SEGMENTS FOR ADBUFFOMR EQ TO 7                      
078000       PERFORM S14-PROCESS-QTY-ADBUFFOMR                                  
078100     END-IF                                                               
078200     .                                                                    
078300                                                                          
078400** FIND THE REQUIRED QTY IN WDD8 DATABASE WITH DATE GT ZERO               
078500 S06A-PROCESS-QTY-DATE SECTION.                                           
078600                                                                          
078700     MOVE W-SAVE-DATE TO W-DABUFPAF-X                                     
078800     MOVE +1 TO W-ADBUFFOM                                                
078900     MOVE 0 TO W-CHECK-WDD811-DATE                                        
079000               W-COUNTER-NO                                               
079100                                                                          
079200     PERFORM IMS-GNPF-WDD811-DATEGT                                       
079300     PERFORM UNTIL SEGMENT-MISSING                                        
079400       ADD +1 TO W-CHECK-WDD811-DATE                                      
079500       PERFORM S07-CALC-SAVE-DATE                                         
079600       PERFORM IMS-GNP-WDD811-DATEGT                                      
079700     END-PERFORM                                                          
079800                                                                          
079900     IF W-CHECK-WDD811-DATE > 0                                           
080000        MOVE W-SAVE-DATE TO W-DABUFPAF-X                                  
080100        MOVE +1 TO W-ADBUFFOM                                             
080200        PERFORM IMS-GNPF-WDD811                                           
080300        IF WDD811-SALDO-KVBUFF-F > 0                                      
080400           SET STOP-PROCESS-NO TO TRUE                                    
080500           PERFORM S08-PROCESS-BUFFER-QTY                                 
080600        END-IF                                                            
080700        IF W-REQD-QTY > 0                                                 
080800           PERFORM IMS-GNP-WDD811-DATEEQ                                  
080900           PERFORM UNTIL SEGMENT-MISSING OR W-REQD-QTY = 0                
081000             IF WDD811-SALDO-KVBUFF-F > 0                                 
081100                SET STOP-PROCESS-NO TO TRUE                               
081200                PERFORM S08-PROCESS-BUFFER-QTY                            
081300             END-IF                                                       
081400             PERFORM IMS-GNP-WDD811-DATEEQ                                
081500           END-PERFORM                                                    
081600        END-IF                                                            
081700     ELSE                                                                 
081800        SET NO-MORE-DATEGT-YES TO TRUE                                    
081900     END-IF                                                               
082000     .                                                                    
082100                                                                          
082200** COMPARE ALL WDD811-SALDO-DABUFPAF (DATE) TO FIND LEAST DATE            
082300 S07-CALC-SAVE-DATE SECTION.                                              
082400                                                                          
082500     ADD +1 TO W-COUNTER-NO                                               
082600     IF W-COUNTER-NO = 1                                                  
082700        MOVE WDD811-SALDO-DABUFPAF TO W-SAVE-DATE                         
082800     ELSE                                                                 
082900        IF WDD811-SALDO-DABUFPAF < W-SAVE-DATE                            
083000           MOVE WDD811-SALDO-DABUFPAF TO W-SAVE-DATE                      
083100        END-IF                                                            
083200     END-IF                                                               
083300     .                                                                    
083400                                                                          
083500** ALLOCATION OF THE BUFFER QTY (WDD8) FOR REFILL                         
083600 S08-PROCESS-BUFFER-QTY SECTION.                                          
083700                                                                          
083800     MOVE WDD811-SALDO-KVBUFF-F    TO W-BUFF-BAL                          
083900     MOVE WDD811-SALDO-KVKOLLI-F   TO W-REMAINIG-CASE                     
084000                                                                          
084100     IF VALID-WDT101-SEG                                                  
084200**      CHECK IF REFILLING PRPOSAL IS ALREADY MADE FOR THE PART           
084300**      IF SO, REDUCE THE BUFFER BALANCE                                  
084400        PERFORM IMS-GU-WDT101                                             
084500        IF SEGMENT-FOUND                                                  
084600           PERFORM S08A-CHECK-EARLIER-PROPOSALS                           
084700           PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                
084800             OR W-BUFF-BAL <= 0                                           
084900             PERFORM IMS-GN-WDT101                                        
085000             IF SEGMENT-FOUND                                             
085100               PERFORM S08A-CHECK-EARLIER-PROPOSALS                       
085200             END-IF                                                       
085300           END-PERFORM                                                    
085400        END-IF                                                            
085500     END-IF                                                               
085600                                                                          
085700     PERFORM UNTIL W-BUFF-BAL <= 0 OR                                     
085800                   STOP-PROCESS    OR                                     
085900                   W-REQD-QTY = 0                                         
086000       IF W-REMAINIG-CASE > 1 AND W-KVPACK-QTY > 1                        
086100          IF W-REQD-QTY >= W-BUFF-BAL                                     
086200             IF W-BUFF-BAL >= W-KVPACK-QTY                                
086300                MOVE W-KVPACK-QTY TO W-QTY                                
086400                PERFORM S09-WRITE-MOVE-WDD811                             
086500                PERFORM S10-WRITE-W61329                                  
086600                COMPUTE W-REQD-QTY = W-REQD-QTY - W-KVPACK-QTY            
086700                COMPUTE W-BUFF-BAL = W-BUFF-BAL - W-KVPACK-QTY            
086800                COMPUTE W-REMAINIG-CASE = W-REMAINIG-CASE - 1             
086900             ELSE                                                         
087000                MOVE W-BUFF-BAL TO W-QTY                                  
087100                PERFORM S09-WRITE-MOVE-WDD811                             
087200                PERFORM S10-WRITE-W61329                                  
087300                COMPUTE W-REQD-QTY = W-REQD-QTY - W-QTY                   
087400                COMPUTE W-BUFF-BAL = W-BUFF-BAL - W-QTY                   
087500                COMPUTE W-REMAINIG-CASE = W-REMAINIG-CASE - 1             
087600             END-IF                                                       
087700          ELSE                                                            
087800             IF W-REQD-QTY >= W-KVPACK-QTY                                
087900                MOVE W-KVPACK-QTY TO W-QTY                                
088000                PERFORM S09-WRITE-MOVE-WDD811                             
088100                PERFORM S10-WRITE-W61329                                  
088200                COMPUTE W-REQD-QTY = W-REQD-QTY - W-QTY                   
088300                COMPUTE W-BUFF-BAL = W-BUFF-BAL - W-QTY                   
088400                COMPUTE W-REMAINIG-CASE = W-REMAINIG-CASE - 1             
088500             ELSE                                                         
088600                IF RECD-WRITTEN-TO-WDT1                                   
088700                  SET STOP-PROCESS TO TRUE                                
088800                ELSE                                                      
088900                  IF W-KVPACK-QTY < W-BUFF-BAL                            
089000                    MOVE W-KVPACK-QTY TO W-QTY                            
089100                  ELSE                                                    
089200                    MOVE W-REQD-QTY  TO W-QTY                             
089300                  END-IF                                                  
089400                  PERFORM S09-WRITE-MOVE-WDD811                           
089500                  PERFORM S10-WRITE-W61329                                
089600                  COMPUTE W-REQD-QTY = W-REQD-QTY - W-QTY                 
089700                  COMPUTE W-BUFF-BAL = W-BUFF-BAL - W-QTY                 
089800                  COMPUTE W-REMAINIG-CASE = W-REMAINIG-CASE - 1           
089900                END-IF                                                    
090000             END-IF                                                       
090100          END-IF                                                          
090200       ELSE                                                               
090300          IF W-REQD-QTY >= W-BUFF-BAL                                     
090400             MOVE W-BUFF-BAL TO W-QTY                                     
090500             PERFORM S09-WRITE-MOVE-WDD811                                
090600             PERFORM S10-WRITE-W61329                                     
090700             COMPUTE W-REQD-QTY = W-REQD-QTY - W-QTY                      
090800             MOVE 0 TO W-BUFF-BAL                                         
090900          ELSE                                                            
091000             IF RECD-WRITTEN-TO-WDT1                                      
091100               SET STOP-PROCESS TO TRUE                                   
091200             ELSE                                                         
091300               IF W-REMAINIG-CASE > 1                                     
091400                 MOVE W-REQD-QTY  TO W-QTY                                
091500               ELSE                                                       
091600                 MOVE W-BUFF-BAL  TO W-QTY                                
091700               END-IF                                                     
091800               PERFORM S09-WRITE-MOVE-WDD811                              
091900               PERFORM S10-WRITE-W61329                                   
092000               MOVE 0 TO W-REQD-QTY                                       
092100             END-IF                                                       
092200          END-IF                                                          
092300       END-IF                                                             
092400     END-PERFORM                                                          
092500     .                                                                    
092600                                                                          
092700** CHECK IF THE PROPSAL IS ALREADY MADE AND REDUCE THE                    
092800** BUFFER BALANCE ACCORDINGLY                                             
092900 S08A-CHECK-EARLIER-PROPOSALS SECTION.                                    
093000     IF WDD811-SALDO-ADBUFFOMR = WDT101-PF-ADLAGOMR-FOM AND               
093100       WDD811-SALDO-ADBUFFGANG = WDT101-PF-ADGANG-FOM   AND               
093200       WDD811-SALDO-ADBUFFPL   = WDT101-PF-ADPLATS-FOM  AND               
093300       WDT101-PF-KDSTAPF NOT EQUAL 'A'                                    
093400       IF W-REMAINIG-CASE > 1                                             
093500         COMPUTE W-REMAINIG-CASE = W-REMAINIG-CASE - 1                    
093600       END-IF                                                             
093700       IF WDT101-PF-KVBEST-ANDR > 0                                       
093800         COMPUTE W-BUFF-BAL = W-BUFF-BAL - WDT101-PF-KVBEST-ANDR          
093900       ELSE                                                               
094000         COMPUTE W-BUFF-BAL = W-BUFF-BAL - WDT101-PF-KVBEST               
094100       END-IF                                                             
094200     END-IF                                                               
094300     .                                                                    
094400                                                                          
094500** MOVEMENT OF WDD811 (BUFFER) SEGMENTS INTO OUTPUT FILE FIELDS           
094600 S09-WRITE-MOVE-WDD811 SECTION.                                           
094700     MOVE '11' TO W61329-PF-IDDC                                          
094800     IF DISP-CDC < (CLAG-KVOI-PLOCK / 40)                                 
094900        MOVE '1' TO W61329-PF-KDPRIO-PF                                   
095000     ELSE                                                                 
095100        MOVE '2' TO W61329-PF-KDPRIO-PF                                   
095200     END-IF                                                               
095300                                                                          
095400** ADDING 1 TO SS FIELD IN THE TIME ORDERED                               
095500     ADD 1 TO DAY-SEC                                                     
095600     IF DAY-SEC > 59                                                      
095700        MOVE 0 TO DAY-SEC                                                 
095800        ADD 1 TO DAY-MIN                                                  
095900        IF DAY-MIN > 59                                                   
096000           MOVE 00 TO DAY-MIN                                             
096100           ADD 1 TO DAY-HOUR                                              
096200        END-IF                                                            
096300     END-IF                                                               
096400     MOVE DAY-DATE-TIME             TO W61329-PF-TIORDTIME                
096500     MOVE W01160-CLAG-IDARTNR       TO W61329-PF-IDARTNR                  
096600     MOVE WDD811-SALDO-ADBUFFOMR    TO W61329-PF-ADLAGOMR-FOM             
096700     MOVE WDD811-SALDO-ADBUFFGANG   TO W61329-PF-ADGANG-FOM               
096800     MOVE WDD811-SALDO-ADBUFFPL     TO W61329-PF-ADPLATS-FOM              
096900     MOVE CLAG-ADLAGOMR             TO W61329-PF-ADLAGOMR-TOM             
097000     MOVE CLAG-ADGANG               TO W61329-PF-ADGANG-TOM               
097100     MOVE CLAG-ADPLATS              TO W61329-PF-ADPLATS-TOM              
097200     MOVE 0                         TO W61329-PF-KVBEST-ANDR              
097300                                       W61329-PF-TIHOTIME                 
097400                                       W61329-PF-TIAVSL                   
097500     MOVE 'R'                       TO W61329-PF-KDSTAPF                  
097600     IF  CLAG-KVMAXPL > 0                                                 
097700       IF W-QTY > CLAG-KVREFBER-PLOCK                                     
097800          MOVE CLAG-KVREFBER-PLOCK TO W61329-PF-KVBEST                    
097900       ELSE                                                               
098000          MOVE W-QTY                     TO W61329-PF-KVBEST              
098100       END-IF                                                             
098200     ELSE                                                                 
098300        MOVE W-QTY                     TO W61329-PF-KVBEST                
098400     END-IF                                                               
098500                                                                          
098600     MOVE SPACES                    TO W61329-PF-IDUSER                   
098700                                       W61329-PF-FILLER                   
098800     .                                                                    
098900                                                                          
099000** WRITING REQUIRED QTY INTO OUTPUT FILE FROM BUFFER/CDC                  
099100 S10-WRITE-W61329 SECTION.                                                
099200     SKIP2                                                                
099300     SET RECD-WRITTEN-TO-WDT1 TO TRUE                                     
099400     WRITE W61329-POST FROM W61329-PF-WDT101                              
099500                                                                          
099600     MOVE SPACE TO POSTSUM-TRANSTYP                                       
099700     MOVE 'W61329 ' TO POSTSUM-FDNAMN                                     
099800     MOVE 'W61329D2' TO POSTSUM-DDNAMN2                                   
099900     CALL POSTSUM USING POSTSUM-PARM                                      
100000     .                                                                    
100100                                                                          
100200** FIND THE REQUIRED QTY IN WDD8 DATABASE WITH DATE EQ ZERO               
100300 S11-PROCESS-QTY-NODATE SECTION.                                          
100400                                                                          
100500     MOVE W-SAVE-DATE TO W-DABUFPAF-X                                     
100600     MOVE +1 TO W-ADBUFFOM                                                
100700                                                                          
100800     PERFORM IMS-GNPF-WDD811                                              
100900     IF SEGMENT-FOUND                                                     
101000        IF WDD811-SALDO-KVBUFF-F > 0                                      
101100           SET STOP-PROCESS-NO TO TRUE                                    
101200           PERFORM S08-PROCESS-BUFFER-QTY                                 
101300        END-IF                                                            
101400        IF W-REQD-QTY > 0                                                 
101500           PERFORM IMS-GNP-WDD811-DATEEQ                                  
101600           PERFORM UNTIL SEGMENT-MISSING OR W-REQD-QTY = 0                
101700                                   OR SEGMENT-NOMORE                      
101800             IF WDD811-SALDO-KVBUFF-F > 0                                 
101900                SET STOP-PROCESS-NO TO TRUE                               
102000                PERFORM S08-PROCESS-BUFFER-QTY                            
102100             END-IF                                                       
102200             PERFORM IMS-GNP-WDD811-DATEEQ                                
102300           END-PERFORM                                                    
102400        END-IF                                                            
102500     END-IF                                                               
102600     .                                                                    
102700                                                                          
102800** FIND THE REQUIRED QTY IN WDD8 DATABASE WITH ADBUFFOM EQ '1'            
102900 S12-PROCESS-QTY-ADBUFFOMR SECTION.                                       
103000                                                                          
103100     MOVE +1 TO W-ADBUFFOM                                                
103200                                                                          
103300     PERFORM IMS-GNPF-WDD811-ADEQ                                         
103400     IF SEGMENT-FOUND                                                     
103500        IF WDD811-SALDO-KVBUFF-F > 0                                      
103600           SET STOP-PROCESS-NO TO TRUE                                    
103700           PERFORM S08-PROCESS-BUFFER-QTY                                 
103800        END-IF                                                            
103900        IF W-REQD-QTY > 0                                                 
104000           PERFORM IMS-GNP-WDD811-ADEQ                                    
104100           PERFORM UNTIL SEGMENT-MISSING OR W-REQD-QTY = 0                
104200                                   OR SEGMENT-NOMORE                      
104300             IF WDD811-SALDO-KVBUFF-F > 0                                 
104400                SET STOP-PROCESS-NO TO TRUE                               
104500                PERFORM S08-PROCESS-BUFFER-QTY                            
104600             END-IF                                                       
104700             PERFORM IMS-GNP-WDD811-ADEQ                                  
104800           END-PERFORM                                                    
104900        END-IF                                                            
105000     END-IF                                                               
105100     .                                                                    
105200** FIND THE REQUIRED QTY IN WDD8 DATABASE WITH ADBUFFOM EQ '8'            
105300 S13-PROCESS-QTY-ADBUFFOMR SECTION.                                       
105400                                                                          
105500     MOVE +6 TO W-ADBUFFOM                                                
105600     MOVE +8 TO W-ADBUFFOM-1                                              
105700                                                                          
105800                                                                          
105900     PERFORM IMS-GNPF-WDD811-ADEQM                                        
106000     IF SEGMENT-FOUND                                                     
106100        IF WDD811-SALDO-KVBUFF-F > 0                                      
106200           SET STOP-PROCESS-NO TO TRUE                                    
106300           PERFORM S08-PROCESS-BUFFER-QTY                                 
106400        END-IF                                                            
106500        IF W-REQD-QTY > 0                                                 
106600           PERFORM IMS-GNP-WDD811-ADEQM                                   
106700           PERFORM UNTIL SEGMENT-MISSING OR W-REQD-QTY = 0                
106800                                   OR SEGMENT-NOMORE                      
106900             IF WDD811-SALDO-KVBUFF-F > 0                                 
107000                SET STOP-PROCESS-NO TO TRUE                               
107100                PERFORM S08-PROCESS-BUFFER-QTY                            
107200             END-IF                                                       
107300             PERFORM IMS-GNP-WDD811-ADEQM                                 
107400           END-PERFORM                                                    
107500        END-IF                                                            
107600     END-IF                                                               
107700     .                                                                    
107800** FIND THE REQUIRED QTY IN WDD8 DATABASE WITH ADBUFFOM EQ '7'            
107900 S14-PROCESS-QTY-ADBUFFOMR SECTION.                                       
108000                                                                          
108100     MOVE +7 TO W-ADBUFFOM                                                
108200                                                                          
108300     PERFORM IMS-GNPF-WDD811-ADEQ                                         
108400     IF SEGMENT-FOUND                                                     
108500        IF WDD811-SALDO-KVBUFF-F > 0                                      
108600           SET STOP-PROCESS-NO TO TRUE                                    
108700           PERFORM S08-PROCESS-BUFFER-QTY                                 
108800        END-IF                                                            
108900        IF W-REQD-QTY > 0                                                 
109000           PERFORM IMS-GNP-WDD811-ADEQ                                    
109100           PERFORM UNTIL SEGMENT-MISSING OR W-REQD-QTY = 0                
109200                                   OR SEGMENT-NOMORE                      
109300             IF WDD811-SALDO-KVBUFF-F > 0                                 
109400                SET STOP-PROCESS-NO TO TRUE                               
109500                PERFORM S08-PROCESS-BUFFER-QTY                            
109600             END-IF                                                       
109700             PERFORM IMS-GNP-WDD811-ADEQ                                  
109800           END-PERFORM                                                    
109900        END-IF                                                            
110000     END-IF                                                               
110100     .                                                                    
110200                                                                          
110300** FIND THE REQUIRED QTY IN WDD8 DATABASE WITH ADBUFFOM EQ '7'            
110400 S15-PROCESS-QTY-ADBUFFOMR SECTION.                                       
110500                                                                          
110600     MOVE +50 TO W-ADBUFFOM                                               
110700     MOVE +52 TO W-ADBUFFOM-1                                             
110800                                                                          
110900     PERFORM IMS-GNPF-WDD811-ADEQM                                        
111000     IF SEGMENT-FOUND                                                     
111100        IF WDD811-SALDO-KVBUFF-F > 0                                      
111200           SET STOP-PROCESS-NO TO TRUE                                    
111300           PERFORM S08-PROCESS-BUFFER-QTY                                 
111400        END-IF                                                            
111500        IF W-REQD-QTY > 0                                                 
111600           PERFORM IMS-GNP-WDD811-ADEQM                                   
111700           PERFORM UNTIL SEGMENT-MISSING OR W-REQD-QTY = 0                
111800                                   OR SEGMENT-NOMORE                      
111900             IF WDD811-SALDO-KVBUFF-F > 0                                 
112000                SET STOP-PROCESS-NO TO TRUE                               
112100                PERFORM S08-PROCESS-BUFFER-QTY                            
112200             END-IF                                                       
112300             PERFORM IMS-GNP-WDD811-ADEQM                                 
112400           END-PERFORM                                                    
112500        END-IF                                                            
112600     END-IF                                                               
112700     .                                                                    
112800* --- IMS SECTIONS  ---                                                   
112900 IMS-GU-WDK611 SECTION.                                                   
113000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
113100          DELIMITED BY SIZE INTO SSA1                                     
113200     STRING 'WDK611  (KDSEGKEY =' W-WDK611KY-X ')'                        
113300          DELIMITED BY SIZE INTO SSA2                                     
113400     MOVE '  GE' TO GOOD-STATUSCODES                                      
113500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
113600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
113700     PERFORM IMS-STATUSCHECK                                              
113800     .                                                                    
113900                                                                          
114000 IMS-GU-WDT101 SECTION.                                                   
114100     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
114200                    '&WDT101KY<=' W-WDT101KY-MAX-X ')'                    
114300          DELIMITED BY SIZE INTO SSA1                                     
114400     MOVE '  GE' TO GOOD-STATUSCODES                                      
114500     CALL CBLTDLI USING GU WDT1-PCB DLI-IO-WDT101 SSA1                    
114600     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
114700     PERFORM IMS-STATUSCHECK                                              
114800     .                                                                    
114900                                                                          
115000 IMS-GN-WDT101 SECTION.                                                   
115100     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
115200                    '&WDT101KY<=' W-WDT101KY-MAX-X ')'                    
115300          DELIMITED BY SIZE INTO SSA1                                     
115400     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
115500     CALL CBLTDLI USING GN WDT1-PCB DLI-IO-WDT101 SSA1                    
115600     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
115700     PERFORM IMS-STATUSCHECK                                              
115800     .                                                                    
115900                                                                          
116000 IMS-GU-WDD801 SECTION.                                                   
116100     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
116200          DELIMITED BY SIZE INTO SSA1                                     
116300     MOVE '  GE' TO GOOD-STATUSCODES                                      
116400     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-WDD801 SSA1                    
116500     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
116600     PERFORM IMS-STATUSCHECK                                              
116700     .                                                                    
116800                                                                          
116900 IMS-GU-WDK901 SECTION.                                                   
117000     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
117100          DELIMITED BY SIZE INTO SSA1                                     
117200     MOVE '  GE' TO GOOD-STATUSCODES                                      
117300     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
117400     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
117500     PERFORM IMS-STATUSCHECK                                              
117600     .                                                                    
117700                                                                          
117800 IMS-GNP-WDD811        SECTION.                                           
117900     STRING 'WDD811  (WDD811KY>=' W-WDD811KY-MIN-X                        
118000                    '&WDD811KY<=' W-WDD811KY-MAX-X ')'                    
118100          DELIMITED BY SIZE INTO SSA1                                     
118200     MOVE '  GE' TO GOOD-STATUSCODES                                      
118300     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD811 SSA1                   
118400     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
118500     PERFORM IMS-STATUSCHECK                                              
118600     .                                                                    
118700                                                                          
118800 IMS-GNP-WDD811-DATEGT SECTION.                                           
118900     STRING 'WDD811  (WDD811KY>=' W-WDD811KY-MIN-X                        
119000                    '&WDD811KY<=' W-WDD811KY-MAX-X                        
119100                    '&ADBUFFOMNE' W-ADBUFFOM-X                            
119200                    '&ADBUFFOMNE' W-ADBUFFOM-50-X                         
119300                    '&ADBUFFOMNE' W-ADBUFFOM-52-X                         
119400                    '&ADBUFFOMNE' W-ADBUFFOM-59-X                         
119500                    '&ADBUFFOMNE' W-ADBUFFOM-7-X                          
119600                    '&ADBUFFOMNE' W-ADBUFFOM-8-X                          
119700                    '&ADBUFFOMNE' W-ADBUFFOM-6-X                          
119800                    '&DABUFPAF> ' W-DABUFPAF-X ')'                        
119900          DELIMITED BY SIZE INTO SSA1                                     
120000     MOVE '  GE' TO GOOD-STATUSCODES                                      
120100     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD811 SSA1                   
120200     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
120300     PERFORM IMS-STATUSCHECK                                              
120400     .                                                                    
120500                                                                          
120600 IMS-GNPF-WDD811-DATEGT SECTION.                                          
120700     STRING 'WDD811  *F(WDD811KY>=' W-WDD811KY-MIN-X                      
120800                    '&WDD811KY<=' W-WDD811KY-MAX-X                        
120900                    '&ADBUFFOMNE' W-ADBUFFOM-X                            
121000                    '&ADBUFFOMNE' W-ADBUFFOM-50-X                         
121100                    '&ADBUFFOMNE' W-ADBUFFOM-52-X                         
121200                    '&ADBUFFOMNE' W-ADBUFFOM-59-X                         
121300                    '&ADBUFFOMNE' W-ADBUFFOM-7-X                          
121400                    '&ADBUFFOMNE' W-ADBUFFOM-8-X                          
121500                    '&ADBUFFOMNE' W-ADBUFFOM-6-X                          
121600                    '&DABUFPAF> ' W-DABUFPAF-X ')'                        
121700          DELIMITED BY SIZE INTO SSA1                                     
121800     MOVE '  GE' TO GOOD-STATUSCODES                                      
121900     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD811 SSA1                   
122000     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
122100     PERFORM IMS-STATUSCHECK                                              
122200     .                                                                    
122300                                                                          
122400 IMS-GNP-WDD811-DATEEQ SECTION.                                           
122500     STRING 'WDD811  (WDD811KY>=' W-WDD811KY-MIN-X                        
122600                    '&WDD811KY<=' W-WDD811KY-MAX-X                        
122700                    '&ADBUFFOMNE' W-ADBUFFOM-X                            
122800                    '&ADBUFFOMNE' W-ADBUFFOM-59-X                         
122900                    '&ADBUFFOMNE' W-ADBUFFOM-50-X                         
123000                    '&ADBUFFOMNE' W-ADBUFFOM-52-X                         
123100                    '&ADBUFFOMNE' W-ADBUFFOM-6-X                          
123200                    '&ADBUFFOMNE' W-ADBUFFOM-7-X                          
123300                    '&ADBUFFOMNE' W-ADBUFFOM-8-X                          
123400                    '&DABUFPAF= ' W-DABUFPAF-X ')'                        
123500          DELIMITED BY SIZE INTO SSA1                                     
123600     MOVE '  GE' TO GOOD-STATUSCODES                                      
123700     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD811 SSA1                   
123800     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
123900     PERFORM IMS-STATUSCHECK                                              
124000     .                                                                    
124100                                                                          
124200 IMS-GNPF-WDD811 SECTION.                                                 
124300     STRING 'WDD811  *F(WDD811KY>=' W-WDD811KY-MIN-X                      
124400                    '&WDD811KY<=' W-WDD811KY-MAX-X                        
124500                    '&ADBUFFOMNE' W-ADBUFFOM-X                            
124600                    '&ADBUFFOMNE' W-ADBUFFOM-59-X                         
124700                    '&ADBUFFOMNE' W-ADBUFFOM-50-X                         
124800                    '&ADBUFFOMNE' W-ADBUFFOM-52-X                         
124900                    '&ADBUFFOMNE' W-ADBUFFOM-6-X                          
125000                    '&ADBUFFOMNE' W-ADBUFFOM-7-X                          
125100                    '&ADBUFFOMNE' W-ADBUFFOM-8-X                          
125200                    '&DABUFPAF= ' W-DABUFPAF-X ')'                        
125300          DELIMITED BY SIZE INTO SSA1                                     
125400     MOVE '  GE' TO GOOD-STATUSCODES                                      
125500     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD811 SSA1                   
125600     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
125700     PERFORM IMS-STATUSCHECK                                              
125800     .                                                                    
125900                                                                          
126000 IMS-GNP-WDD811-ADEQ SECTION.                                             
126100     STRING 'WDD811  (WDD811KY>=' W-WDD811KY-MIN-X                        
126200                    '&WDD811KY<=' W-WDD811KY-MAX-X                        
126300                    '&ADBUFFOM= ' W-ADBUFFOM-X ')'                        
126400          DELIMITED BY SIZE INTO SSA1                                     
126500     MOVE '  GE' TO GOOD-STATUSCODES                                      
126600     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD811 SSA1                   
126700     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
126800     PERFORM IMS-STATUSCHECK                                              
126900     .                                                                    
127000                                                                          
127100 IMS-GNP-WDD811-ADEQM SECTION.                                            
127200     STRING 'WDD811  (WDD811KY>=' W-WDD811KY-MIN-X                        
127300                    '&WDD811KY<=' W-WDD811KY-MAX-X                        
127400                    '&ADBUFFOM= ' W-ADBUFFOM-X                            
127500                    '!WDD811KY>=' W-WDD811KY-MIN-X                        
127600                    '&WDD811KY<=' W-WDD811KY-MAX-X                        
127700                    '&ADBUFFOM= ' W-ADBUFFOM-Y ')'                        
127800          DELIMITED BY SIZE INTO SSA1                                     
127900     MOVE '  GE' TO GOOD-STATUSCODES                                      
128000     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD811 SSA1                   
128100     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
128200     PERFORM IMS-STATUSCHECK                                              
128300     .                                                                    
128400 IMS-GNPF-WDD811-ADEQ SECTION.                                            
128500     STRING 'WDD811  *F(WDD811KY>=' W-WDD811KY-MIN-X                      
128600                    '&WDD811KY<=' W-WDD811KY-MAX-X                        
128700                    '&ADBUFFOM= ' W-ADBUFFOM-X ')'                        
128800          DELIMITED BY SIZE INTO SSA1                                     
128900     MOVE '  GE' TO GOOD-STATUSCODES                                      
129000     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD811 SSA1                   
129100     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
129200     PERFORM IMS-STATUSCHECK                                              
129300     .                                                                    
129400 IMS-GNPF-WDD811-ADEQM SECTION.                                           
129500     STRING 'WDD811  *F(WDD811KY>=' W-WDD811KY-MIN-X                      
129600                    '&WDD811KY<=' W-WDD811KY-MAX-X                        
129700                    '&ADBUFFOM= ' W-ADBUFFOM-X                            
129800                    '!WDD811KY>=' W-WDD811KY-MIN-X                        
129900                    '&WDD811KY<=' W-WDD811KY-MAX-X                        
130000                    '&ADBUFFOM= ' W-ADBUFFOM-Y ')'                        
130100          DELIMITED BY SIZE INTO SSA1                                     
130200     MOVE '  GE' TO GOOD-STATUSCODES                                      
130300     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD811 SSA1                   
130400     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
130500     PERFORM IMS-STATUSCHECK                                              
130600     .                                                                    
130700 IMS-GU-WDM501   SECTION.                                                 
130800                                                                          
130900     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST ')'                          
131000          DELIMITED BY SIZE INTO SSA1                                     
131100     MOVE SPACE  TO GOOD-STATUSCODES                                      
131200     CALL CBLTDLI USING GU  WDM5-PCB DLI-IO-WDM501 SSA1                   
131300     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
131400     PERFORM IMS-STATUSCHECK                                              
131500     .                                                                    
131600     EJECT                                                                
131700 IMS-GNP-WDM521 SECTION.                                                  
131800                                                                          
131900     STRING 'WDM511    '                                                  
132000          DELIMITED BY SIZE INTO SSA1                                     
132100     STRING 'WDM521    '                                                  
132200          DELIMITED BY SIZE INTO SSA2                                     
132300     MOVE '  GE' TO GOOD-STATUSCODES                                      
132400     CALL CBLTDLI USING GNP WDM5-PCB DLI-IO-WDM521 SSA1 SSA2              
132500     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
132600     PERFORM IMS-STATUSCHECK                                              
132700     .                                                                    
132800     EJECT                                                                
132900                                                                          
133000 IMS-GU-WDE4C1 SECTION.                                                   
133100                                                                          
133200     STRING 'WDE4C1  (WDE4C1KY>=' W-WDE4C1KY-MIN-X                        
133300                    '&WDE4C1KY<=' W-WDE4C1KY-MAX-X                        
133400                    '&KDRADSTA =' W-KDRADSTA-X ')'                        
133500             DELIMITED BY SIZE INTO SSA1                                  
133600     MOVE '  GE'                 TO GOOD-STATUSCODES                      
133700     CALL CBLTDLI             USING GU                                    
133800                                    WDE4C-PCB                             
133900                                    DLI-IO-WDE4C1                         
134000                                    SSA1                                  
134100     MOVE WDE4C-STATUS-CODE      TO STATUS-WS                             
134200     PERFORM IMS-STATUSCHECK                                              
134300     .                                                                    
134400     SKIP2                                                                
134500 IMS-GN-WDE4C1 SECTION.                                                   
134600                                                                          
134700     STRING 'WDE4C1  (WDE4C1KY>=' W-WDE4C1KY-MIN-X                        
134800                    '&WDE4C1KY<=' W-WDE4C1KY-MAX-X                        
134900                    '&KDRADSTA =' W-KDRADSTA-X ')'                        
135000             DELIMITED BY SIZE INTO SSA1                                  
135100     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
135200     CALL CBLTDLI             USING GN                                    
135300                                    WDE4C-PCB                             
135400                                    DLI-IO-WDE4C1                         
135500                                    SSA1                                  
135600     MOVE WDE4C-STATUS-CODE      TO STATUS-WS                             
135700     PERFORM IMS-STATUSCHECK                                              
135800     .                                                                    
135900     SKIP2                                                                
136000 IMS-GU-WDE411 SECTION.                                                   
136100                                                                          
136200     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
136300             DELIMITED BY SIZE INTO SSA1                                  
136400     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
136500             DELIMITED BY SIZE INTO SSA2                                  
136600     MOVE '  GEGG'               TO GOOD-STATUSCODES                      
136700     CALL CBLTDLI             USING GU                                    
136800                                    WDE4-PCB                              
136900                                    DLI-IO-WDE411                         
137000                                    SSA1 SSA2                             
137100     MOVE WDE4-STATUS-CODE       TO STATUS-WS                             
137200     PERFORM IMS-STATUSCHECK                                              
137300     .                                                                    
137400     SKIP2                                                                
137500                                                                          
137600 IMS-GU-WDQ4B1 SECTION.                                                   
137700                                                                          
137800      STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4B1KY-MIN                         
137900                     '&WDQ4B1KY<=' W-WDQ4B1KY-MAX                         
138000                     '&IDDC     =' W-IDDC-WDQ4-X  ')'                     
138100              DELIMITED BY SIZE INTO SSA1                                 
138200      MOVE '  GEGB'               TO GOOD-STATUSCODES                     
138300      CALL CBLTDLI USING GU WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                  
138400      MOVE WDQ4B-STATUS-CODE      TO STATUS-WS                            
138500      PERFORM IMS-STATUSCHECK                                             
138600      .                                                                   
138700                                                                          
138800 IMS-GN-WDQ4B1 SECTION.                                                   
138900      STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4B1KY-MIN                         
139000                     '&WDQ4B1KY<=' W-WDQ4B1KY-MAX                         
139100                     '&IDDC     =' W-IDDC-WDQ4-X  ')'                     
139200              DELIMITED BY SIZE INTO SSA1                                 
139300      MOVE '  GEGB'               TO GOOD-STATUSCODES                     
139400      CALL CBLTDLI USING GN WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                  
139500      MOVE WDQ4B-STATUS-CODE      TO STATUS-WS                            
139600      PERFORM IMS-STATUSCHECK                                             
139700      .                                                                   
139800                                                                          
139900 IMS-GU-WDQ212 SECTION.                                                   
140000                                                                          
140100      STRING 'WDQ201  (IDORDER  =' W-IDORDER-WDQ2-X ')'                   
140200              DELIMITED BY SIZE INTO SSA1                                 
140300      STRING 'WDQ212  (IDDC     ='  W-IDDC-WDQ2-X ')'                     
140400              DELIMITED BY SIZE INTO SSA2                                 
140500                                                                          
140600      MOVE '  GE'                 TO GOOD-STATUSCODES                     
140700                                                                          
140800      CALL CBLTDLI             USING GU                                   
140900                                     WDQ2-PCB                             
141000                                     DLI-IO-WDQ212                        
141100                                     SSA1                                 
141200                                     SSA2                                 
141300      MOVE WDQ2-STATUS-CODE       TO STATUS-WS                            
141400      PERFORM IMS-STATUSCHECK                                             
141500      .                                                                   
141600                                                                          
141700 IMS-GNP-WDQ221 SECTION.                                                  
141800                                                                          
141900      STRING 'WDQ221  (ADLAGOMR =' W-ADLAGOMR-WDQ2-X ')'                  
142000              DELIMITED BY SIZE INTO SSA1                                 
142100      MOVE '  GE'                 TO GOOD-STATUSCODES                     
142200                                                                          
142300      CALL CBLTDLI             USING GNP                                  
142400                                  WDQ2-PCB                                
142500                                  DLI-IO-WDQ221                           
142600                                  SSA1                                    
142700      MOVE WDQ2-STATUS-CODE       TO STATUS-WS                            
142800      PERFORM IMS-STATUSCHECK                                             
142900      .                                                                   
143000                                                                          
143100 IMS-GU-WDQ301-DSEQ SECTION.                                              
143200                                                                          
143300      STRING 'WDQ301  (WDQ3DSEQ =' W-WDQ3DSEQ-X ')'                       
143400              DELIMITED BY SIZE INTO SSA1                                 
143500      MOVE '  GEGG'               TO GOOD-STATUSCODES                     
143600      CALL CBLTDLI             USING GU                                   
143700                                  WDQ3-PCB                                
143800                                  DLI-IO-WDQ301                           
143900                                  SSA1                                    
144000                                                                          
144100      MOVE WDQ3-STATUS-CODE       TO STATUS-WS                            
144200      PERFORM IMS-STATUSCHECK                                             
144300      .                                                                   
144400                                                                          
144500 IMS-STATUSCHECK SECTION.                                                 
144600     SKIP2                                                                
144700     SET STATUS-IX TO 1                                                   
144800     SEARCH GOOD-STATUS                                                   
144900       AT END                                                             
145000         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
145100           DELIMITED BY SIZE INTO ERRTEXT                                 
145200         DISPLAY ERRTEXT                                                  
145300         CALL FELLOG                                                      
145400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
145500         CONTINUE                                                         
145600     END-SEARCH                                                           
145700     .                                                                    

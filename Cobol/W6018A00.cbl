000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6018A00.                                                
000300 AUTHOR.         JOHAN LINDKVIST.                                         
000400 DATE-WRITTEN.   99/10/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        BACKGROUND MPP THAT PRINTS PACKING INSTRUCTION DOCUMENT          
000900*                                                                         
001000*        THE PROGRAM READS     WLARTC (WKD6)                              
001100*        THE PROGRAM READS     WLINST (WDD1)                              
001200*        THE PROGRAM READS     WLBENA (WDD3)                              
001300*        THE PROGRAM READS     WDP3                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: W6T18A                                              
001700*        MID:         W6I18A01                                            
001800*                                                                         
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100 DATA DIVISION.                                                           
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400*    -- CHECKED BY WY2000                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W6018A00'.            
002600                                                                          
002700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
002900                                                                          
003000 77  YES                         PIC X       VALUE 'Y'.                   
003100 77  NOO                         PIC X       VALUE 'N'.                   
003200                                                                          
003300*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
003400                                                                          
003500                                                                          
003600 01  WS-FILLER                   PIC X(7)    VALUE SPACE.                 
003610 01  WS-FILLER                   PIC X(15)   VALUE 'KEYS OK ?'.           
003700 77  KEYS-SW                     PIC X       VALUE 'J'.                   
003800     88  KEYS-OK                             VALUE 'Y'.                   
003900     88  KEYS-WRONG                          VALUE 'N'.                   
004000                                                                          
004100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004200     88  OWN-MID                             VALUE '618A'.                
004300     88  GOOD-MID                            VALUE '6181' '6182'          
004400                                                   '6183' '6184'          
004500                                                   '6185' '6186'          
004600                                                   '6187' '6188'          
004700                                                   '6189'.                
004800     88  HELP-MID                            VALUE '0551'.                
004900                                                                          
005000                                                                          
005100 77  EMB-IX                      PIC S9(9)  VALUE ZERO COMP SYNC.         
005200 77  DOC-IX                      PIC S9(9)  VALUE ZERO COMP SYNC.         
005300                                                                          
005400 77  WS-IDPERSON                 PIC X(3)    VALUE SPACE.                 
005500                                                                          
005600                                                                          
005700*                                                                         
005800 01  WS-FILLER                   PIC X(16)   VALUE 'DECLARATIONS'.        
005900 01  UNIQUE                      PIC X(4)    VALUE 'UNIK'.                
006000 01  GENERAL                     PIC X(4)    VALUE 'GENE'.                
006100*                                                                         
006200 01  WS-FILLER                   PIC X(16) VALUE 'UNIQUE/GENERAL'.        
006300 77  TYPE-OF-DOC-SW              PIC X(4)    VALUE SPACE.                 
006400     88  DOC-IS-UNIQUE                       VALUE 'UNIK'.                
006500     88  DOC-IS-GENERAL                      VALUE 'GENE'.                
006600*                                                                         
006700 01  WS-FILLER                   PIC X(16) VALUE 'DOC OK ? (Y/N)'.        
006800 77  IS-DOC-OK-SW                PIC X       VALUE SPACE.                 
006900     88  DOC-OK                              VALUE 'Y'.                   
007000     88  DOC-DATA-CONATINS-ERRORS            VALUE 'N'.                   
007100*                                                                         
007200 01  WS-FILLER                   PIC X(16)   VALUE 'B-CHECK-KEYS'.        
007300 01  WS-IDFPINST-TEST            PIC X(16)   VALUE 'ID:   ? '.            
007400 01  WS-PRTTEST                  PIC X(16)   VALUE 'PRT:  ? '.            
007500*                                                                         
007600 01  WS-PRINT-STATUS            PIC X(16) VALUE 'IS DOC PRINTED '.        
007700*                                                                         
007800 01  WS-TIUPPDAT-S               PIC 9(7).                                
007900 01  WS-TIUPPDAT-X.                                                       
008000     03 FILLER                   PIC X.                                   
008100     03 WS-TIUPPDAT2             PIC X(6).                                
008200     EJECT                                                                
008300*    --- AREA TO HANDLE PRINTING                                          
008400 01  PRINT-AREA-TABLE.                                                    
008500     03  PRINT-ROW-IX OCCURS 21.                                          
008600         05 FILLER               PIC X(100).                              
008700                                                                          
008800 01  PRINT-AREA-TABLE-FILL REDEFINES PRINT-AREA-TABLE.                    
008900     03  PRINT-RAD-08.                                                    
009000*        UTSKRIFTSFORMATET SKÖTER PLACERING AV ARTNR.                     
009100         05 PRINT-IDARTNR-08     PIC X(9).                                
009200         05 FILLER               PIC X(91).                               
009300                                                                          
009400     03  PRINT-RAD-12.                                                    
009500         05 FILLER               PIC X(9).                                
009600         05 PRINT-IDLEVNR-12     PIC X(5).                                
009700         05 FILLER               PIC X(5).                                
009800         05 PRINT-IDFPINST-12    PIC X(9).                                
009900         05 FILLER               PIC X(6).                                
010000         05 PRINT-BEART-12       PIC X(25).                               
010100         05 FILLER               PIC X(3).                                
010200         05 PRINT-DAREGDAT-12    PIC X(8).                                
010300         05 FILLER               PIC X(7).                                
010400         05 PRINT-TIUPPDAT-12    PIC X(6).                                
010500         05 FILLER               PIC X(17).                               
010600                                                                          
010700     03  PRINT-RAD-16.                                                    
010800         05 FILLER               PIC X(9).                                
010900         05 PRINT-KVQPACK-Q0-16  PIC X(5).                                
011000         05 FILLER               PIC X(1).                                
011100         05 PRINT-IDARTNR-Q0-16  PIC X(9).                                
011200         05 FILLER               PIC X(2).                                
011300         05 PRINT-BEART-Q0-16    PIC X(20).                               
011400         05 FILLER               PIC X(3).                                
011500         05 PRINT-KVQPACK-Q1-16  PIC X(5).                                
011600         05 FILLER               PIC X(2).                                
011700         05 PRINT-IDARTNR-Q1-16  PIC X(9).                                
011800         05 FILLER               PIC X(2).                                
011900         05 PRINT-BEART-Q1-16    PIC X(20).                               
012000         05 FILLER               PIC X(13).                               
012100                                                                          
012200     03  PRINT-RAD-17.                                                    
012300         05 FILLER               PIC X(9).                                
012400         05 PRINT-KVQPACK-Q2-17  PIC X(5).                                
012500         05 FILLER               PIC X(1).                                
012600         05 PRINT-IDARTNR-Q2-17  PIC X(9).                                
012700         05 FILLER               PIC X(2).                                
012800         05 PRINT-BEART-Q2-17    PIC X(20).                               
012900         05 FILLER               PIC X(54).                               
013000                                                                          
013100     03  PRINT-RAD-21.                                                    
013200         05 FILLER               PIC X(9).                                
013300         05 PRINT-KVQPACK-01-21  PIC X(5).                                
013400         05 FILLER               PIC X(1).                                
013500         05 PRINT-IDARTNR-01-21  PIC X(9).                                
013600         05 FILLER               PIC X(2).                                
013700         05 PRINT-BEART-01-21    PIC X(20).                               
013800         05 FILLER               PIC X(3).                                
013900         05 PRINT-KVQPACK-06-21  PIC X(5).                                
014000         05 FILLER               PIC X(2).                                
014100         05 PRINT-IDARTNR-06-21  PIC X(9).                                
014200         05 FILLER               PIC X(2).                                
014300         05 PRINT-BEART-06-21    PIC X(20).                               
014400         05 FILLER               PIC X(13).                               
014500                                                                          
014600     03  PRINT-RAD-22.                                                    
014700         05 FILLER               PIC X(9).                                
014800         05 PRINT-KVQPACK-02-22  PIC X(5).                                
014900         05 FILLER               PIC X(1).                                
015000         05 PRINT-IDARTNR-02-22  PIC X(9).                                
015100         05 FILLER               PIC X(2).                                
015200         05 PRINT-BEART-02-22    PIC X(20).                               
015300         05 FILLER               PIC X(3).                                
015400         05 PRINT-KVQPACK-07-22  PIC X(5).                                
015500         05 FILLER               PIC X(2).                                
015600         05 PRINT-IDARTNR-07-22  PIC X(9).                                
015700         05 FILLER               PIC X(2).                                
015800         05 PRINT-BEART-07-22    PIC X(20).                               
015900         05 FILLER               PIC X(13).                               
016000                                                                          
016100     03  PRINT-RAD-23.                                                    
016200         05 FILLER               PIC X(9).                                
016300         05 PRINT-KVQPACK-03-23  PIC X(5).                                
016400         05 FILLER               PIC X(1).                                
016500         05 PRINT-IDARTNR-03-23  PIC X(9).                                
016600         05 FILLER               PIC X(2).                                
016700         05 PRINT-BEART-03-23    PIC X(20).                               
016800         05 FILLER               PIC X(3).                                
016900         05 PRINT-KVQPACK-08-23  PIC X(5).                                
017000         05 FILLER               PIC X(2).                                
017100         05 PRINT-IDARTNR-08-23  PIC X(9).                                
017200         05 FILLER               PIC X(2).                                
017300         05 PRINT-BEART-08-23    PIC X(20).                               
017400         05 FILLER               PIC X(13).                               
017500                                                                          
017600     03  PRINT-RAD-24.                                                    
017700         05 FILLER               PIC X(9).                                
017800         05 PRINT-KVQPACK-04-24  PIC X(5).                                
017900         05 FILLER               PIC X(1).                                
018000         05 PRINT-IDARTNR-04-24  PIC X(9).                                
018100         05 FILLER               PIC X(2).                                
018200         05 PRINT-BEART-04-24    PIC X(20).                               
018300         05 FILLER               PIC X(3).                                
018400         05 PRINT-KVQPACK-09-24  PIC X(5).                                
018500         05 FILLER               PIC X(2).                                
018600         05 PRINT-IDARTNR-09-24  PIC X(9).                                
018700         05 FILLER               PIC X(2).                                
018800         05 PRINT-BEART-09-24    PIC X(20).                               
018900         05 FILLER               PIC X(13).                               
019000                                                                          
019100     03  PRINT-RAD-25.                                                    
019200         05 FILLER               PIC X(9).                                
019300         05 PRINT-KVQPACK-05-25  PIC X(5).                                
019400         05 FILLER               PIC X(1).                                
019500         05 PRINT-IDARTNR-05-25  PIC X(9).                                
019600         05 FILLER               PIC X(2).                                
019700         05 PRINT-BEART-05-25    PIC X(20).                               
019800         05 FILLER               PIC X(3).                                
019900         05 PRINT-KVQPACK-10-25  PIC X(5).                                
020000         05 FILLER               PIC X(2).                                
020100         05 PRINT-IDARTNR-10-25  PIC X(9).                                
020200         05 FILLER               PIC X(2).                                
020300         05 PRINT-BEART-10-25    PIC X(20).                               
020400         05 FILLER               PIC X(13).                               
020500                                                                          
020600     03  PRINT-RAD-29.                                                    
020700         05 FILLER               PIC X(8).                                
020800         05 PRINT-TEBEFT-01-29   PIC X(80).                               
020900         05 FILLER               PIC X(12).                               
021000                                                                          
021100     03  PRINT-RAD-30.                                                    
021200         05 FILLER               PIC X(8).                                
021300         05 PRINT-TEBEFT-02-30   PIC X(80).                               
021400         05 FILLER               PIC X(12).                               
021500                                                                          
021600     03  PRINT-RAD-31.                                                    
021700         05 FILLER               PIC X(8).                                
021800         05 PRINT-TEBEFT-03-31   PIC X(80).                               
021900         05 FILLER               PIC X(12).                               
022000                                                                          
022100     03  PRINT-RAD-32.                                                    
022200         05 FILLER               PIC X(8).                                
022300         05 PRINT-TEBEFT-04-32   PIC X(80).                               
022400         05 FILLER               PIC X(12).                               
022500                                                                          
022600     03  PRINT-RAD-33.                                                    
022700         05 FILLER               PIC X(8).                                
022800         05 PRINT-TEBEFT-05-33   PIC X(80).                               
022900         05 FILLER               PIC X(12).                               
023000                                                                          
023100     03  PRINT-RAD-34.                                                    
023200         05 FILLER               PIC X(8).                                
023300         05 PRINT-TEBEFT-06-34   PIC X(80).                               
023400         05 FILLER               PIC X(12).                               
023500                                                                          
023600     03  PRINT-RAD-35.                                                    
023700         05 FILLER               PIC X(8).                                
023800         05 PRINT-TEBEFT-07-35   PIC X(80).                               
023900         05 FILLER               PIC X(12).                               
024000                                                                          
024100     03  PRINT-RAD-36.                                                    
024200         05 FILLER               PIC X(8).                                
024300         05 PRINT-TEBEFT-08-36   PIC X(80).                               
024400         05 FILLER               PIC X(12).                               
024500                                                                          
024600     03  PRINT-RAD-37.                                                    
024700         05 FILLER               PIC X(8).                                
024800         05 PRINT-TEBEFT-09-37   PIC X(80).                               
024900         05 FILLER               PIC X(12).                               
025000                                                                          
025100     03  PRINT-RAD-38.                                                    
025200         05 FILLER               PIC X(8).                                
025300         05 PRINT-TEBEFT-10-38   PIC X(80).                               
025400         05 FILLER               PIC X(12).                               
025500                                                                          
025600     03  PRINT-RAD-44.                                                    
025700         05 FILLER               PIC X(60).                               
025800         05 PRINT-Q3EMB-44       PIC X(5).                                
025900         05 FILLER               PIC X(15).                               
026000         05 PRINT-KVQPACK-Q3-44  PIC X(5).                                
026100         05 FILLER               PIC X(15).                               
026200                                                                          
026300     03  PRINT-RAD-61.                                                    
026400         05 FILLER               PIC X(8).                                
026500         05 PRINT-AVDELNING-61   PIC X(5).                                
026600         05 PRINT-SLASH-61       PIC X(3).                                
026700         05 PRINT-INITIALER-61   PIC X(2).                                
026800         05 FILLER               PIC X(82).                               
026900                                                                          
027000     EJECT                                                                
027100*    --- AREA TO HANLDE PRT-AFTER                                         
027200 01  PRT-AFTER-TABELL.                                                    
027300     03  PRT-AFTER-IX OCCURS 21      PIC S9(3)   COMP-3.                  
027400                                                                          
027500 01  PRT-AFTER-TABELL-FILL REDEFINES PRT-AFTER-TABELL.                    
027600     03 PRT-AFTERVALUE-1             PIC S9(3)   COMP-3.                  
027700     03 PRT-AFTERVALUE-2             PIC S9(3)   COMP-3.                  
027800     03 PRT-AFTERVALUE-3             PIC S9(3)   COMP-3.                  
027900     03 PRT-AFTERVALUE-4             PIC S9(3)   COMP-3.                  
028000     03 PRT-AFTERVALUE-5             PIC S9(3)   COMP-3.                  
028100     03 PRT-AFTERVALUE-6             PIC S9(3)   COMP-3.                  
028200     03 PRT-AFTERVALUE-7             PIC S9(3)   COMP-3.                  
028300     03 PRT-AFTERVALUE-8             PIC S9(3)   COMP-3.                  
028400     03 PRT-AFTERVALUE-9             PIC S9(3)   COMP-3.                  
028500     03 PRT-AFTERVALUE-10            PIC S9(3)   COMP-3.                  
028600     03 PRT-AFTERVALUE-11            PIC S9(3)   COMP-3.                  
028700     03 PRT-AFTERVALUE-12            PIC S9(3)   COMP-3.                  
028800     03 PRT-AFTERVALUE-13            PIC S9(3)   COMP-3.                  
028900     03 PRT-AFTERVALUE-14            PIC S9(3)   COMP-3.                  
029000     03 PRT-AFTERVALUE-15            PIC S9(3)   COMP-3.                  
029100     03 PRT-AFTERVALUE-16            PIC S9(3)   COMP-3.                  
029200     03 PRT-AFTERVALUE-17            PIC S9(3)   COMP-3.                  
029300     03 PRT-AFTERVALUE-18            PIC S9(3)   COMP-3.                  
029400     03 PRT-AFTERVALUE-19            PIC S9(3)   COMP-3.                  
029500     03 PRT-AFTERVALUE-20            PIC S9(3)   COMP-3.                  
029600     03 PRT-AFTERVALUE-21            PIC S9(3)   COMP-3.                  
029700                                                                          
029800 01 PRINT2-HEADER1.                                                       
029900     03 FILLER                   PIC X(5).                                
030000     03 FILLER                   PIC X(40)                                
030100             VALUE 'THESE PARTS BELONG TO A GENERAL PRODUCT '.            
030200     03 FILLER                   PIC X(33)                                
030300             VALUE 'PACKAGING AGREEMENT INSTRUCTION.'.                    
030400     03 FILLER                   PIC X(22).                               
030500                                                                          
030600 01 PRINT2-HEADER2.                                                       
030700     03 FILLER                   PIC X(5).                                
030800     03 FILLER                   PIC X(21)                                
030900             VALUE 'NO. OF REGISTRATION: '.                               
031000     03 HEADER-IDFPINST          PIC X(9).                                
031100     03 FILLER                   PIC X(67).                               
031200                                                                          
031300 01 PRINT2-DATA.                                                          
031400     03 FILLER                   PIC X(5).                                
031500     03 FILLER                   PIC X(13) VALUE 'PART NUMBER: '.         
031600     03 PRINT2-IDARTNR           PIC X(15).                               
031700     03 FILLER                   PIC X(67).                               
031800                                                                          
031900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
032000 01  GENERAL-SUBPROGRAMS.                                                 
032100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
032200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
032300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
032400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
032500     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
032600     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
032700     EJECT                                                                
032800*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
032900*01 -COPY WMEDAREA                                                        
033000     SKIP3                                                                
033100 01  MESSAGE-CODES.                                                       
033200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
033300     EJECT                                                                
033400* VARIABLES TO SUBPROGRAM W006PRS1 AND W006PRT                            
033500*01  -COPY W006PRAR                                                       
033600     SKIP2                                                                
033700*01  -COPY W006PRT                                                        
033800     EJECT                                                                
033900*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
034000*                                                                         
034100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
034200     SKIP3                                                                
034300*01 -COPY WMSGINIT                                                        
034400     EJECT                                                                
034500*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
034600*                                                                         
034700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
034800     SKIP3                                                                
034900*01  MID -COPY W6I18A01                                                   
035000     EJECT                                                                
035100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
035200     SKIP3                                                                
035300*01  -COPY WMSGAREA                                                       
035400     EJECT                                                                
035500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
035600     SKIP3                                                                
035700*01  -COPY WMFSAREA                                                       
035800     EJECT                                                                
035900*    --- EMBALLAGE-KODER MED ÖVERSÄTTNINGAR                               
036000*                                                                         
036100*01  -COPY W611EMB3                                                       
036200     EJECT                                                                
036300*    --- WORK-AREAS FOR IMS-SECTIONS                                      
036400*                                                                         
036500     EJECT                                                                
036600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
036700     SKIP3                                                                
036800 01  KEYS-TO-DLI.                                                         
036900*    NYCKEL FÖR WDK6                                                      
037000     03  W-IDARTNR-X.                                                     
037100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
037200*    NYCKEL FÖR WDD1                                                      
037300     03  W-IDFPINST-X.                                                    
037400         05  W-IDFPINST          PIC S9(7)   VALUE ZERO COMP-3.           
037500*    NYCKEL FÖR WDD3 (BENÄMNINGSREGISTRET)                                
037600     03  W-IDARTNR-Y.                                                     
037700         05  W-IDARTNR2          PIC S9(9)   VALUE ZERO COMP-3.           
037800     03  W-IDSKYLT-X.                                                     
037900         05  W-IDSKYLT           PIC X(3)    VALUE 'GB'.                  
038000*    NYCKEL FÖR WDP3 (PERSONREGISTRET)                                    
038100     03  W-KDARBTYP-X.                                                    
038300         05    W-KDARBTYP         PIC X(8)   VALUE 'QUAL    '.            
038500     03  W-IDPERSON-X.                                                    
038600         05  W-IDPERSON           PIC S9(3)  VALUE ZERO COMP-3.           
038700     SKIP2                                                                
038800*    --- STATUS-KOD FRÅN IMS                                              
038900 01  STATUS-WS                   PIC XX.                                  
039000     88  SEGMENT-FOUND                       VALUE '  '.                  
039100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
039200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
039300     88  SEGMENT-MISSING2                    VALUE 'GP'.                  
039400     SKIP2                                                                
039500 01  GOOD-STATUSCODES.                                                    
039600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
039700     SKIP3                                                                
039800 01  SSA1                        PIC X(64).                               
039900 01  SSA2                        PIC X(64).                               
040000     EJECT                                                                
040100*    --- IMS FUNCTION CODES                                               
040200*01  -COPY W0003                                                          
040300     EJECT                                                                
040400*    ---  DLI INPUT-OUTPUT AREA                                           
040500                                                                          
040600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
040700 01  DLI-IO-WDK601.                                                       
040800*    03  -COPY WDK601  -PRE ARTC-                                         
040900     EJECT                                                                
041000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
041100 01  DLI-IO-WDK611.                                                       
041200*    03  -COPY WDK611  -PRE ARTC-                                         
041300     EJECT                                                                
041400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK613'.                      
041500 01  DLI-IO-WDK613.                                                       
041600*    03  -COPY WDK613  -PRE ARTC-                                         
041700     EJECT                                                                
041800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD101'.                      
041900 01  DLI-IO-WDD101.                                                       
042000*    03  -COPY WDD101  -PRE INST-                                         
042100     EJECT                                                                
042200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD111'.                      
042300 01  DLI-IO-WDD111.                                                       
042400*    03  -COPY WDD111  -PRE INST-                                         
042500     EJECT                                                                
042600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
042700 01  DLI-IO-WDD311.                                                       
042800*    03  -COPY WDD311  -PRE BENA-                                         
042900     EJECT                                                                
043000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
043100 01  DLI-IO-WDP311.                                                       
043200*    03  -COPY WDP311                                                     
043300     EJECT                                                                
043400 LINKAGE SECTION.                                                         
043500*01  -COPY W0009   -PRE MSG-                                              
043600                                                                          
043700*01  -COPY W0009   -PRE ALT-                                              
043800                                                                          
043900*01  -COPY W0008   -PRE USEA-                                             
044000     05  FILLER                  PIC X.                                   
044100                                                                          
044200*01  -COPY W0008   -PRE ARTC-                                             
044300     05  FILLER                  PIC X.                                   
044400                                                                          
044500*01  -COPY W0008   -PRE INST-                                             
044600     05  FILLER                  PIC X.                                   
044700                                                                          
044800*01  -COPY W0008   -PRE BENA-                                             
044900     05  FILLER                  PIC X.                                   
045000                                                                          
045100*01  -COPY W0008   -PRE WDP3-                                             
045200     05  FILLER                  PIC X.                                   
045300     EJECT                                                                
045400 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB ARTC-PCB              
045500     INST-PCB BENA-PCB WDP3-PCB.                                          
045600 MAIN SECTION.                                                            
045700     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB ARTC-PCB              
045800     INST-PCB BENA-PCB WDP3-PCB.                                          
045900                                                                          
046000     PERFORM IMS-GET-MSG                                                  
046100     IF SEGMENT-FOUND                                                     
046200       PERFORM A-INIT                                                     
046300       PERFORM B-CHECK-KEYS                                               
046400       IF KEYS-OK                                                         
046500          PERFORM F-READ-DATABASE-INFO                                    
046600          IF DOC-OK                                                       
046700             PERFORM E-PRINT-DOCUMENT                                     
046800          END-IF                                                          
046900       END-IF                                                             
047000     END-IF                                                               
047100*    COMPUTE W-IDFPINST = W-IDFPINST / 0                                  
047200     MOVE ZERO TO RETURN-CODE                                             
047300     GOBACK                                                               
047400     .                                                                    
047500     EJECT                                                                
047600 A-INIT SECTION.                                                          
047700                                                                          
047800     IF MSG-DOUBLE-TRANSACTIONS                                           
047900       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I18A01                 
048000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
048100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
048200     ELSE                                                                 
048300       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I18A01                  
048400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
048500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
048600     END-IF                                                               
048700                                                                          
048800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
048900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
049000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
049100                                                                          
049200     MOVE LOW-VALUE TO MSG-AREA                                           
049300                                                                          
049400     MOVE SPACE TO PRINT-AREA-TABLE                                       
049500                                                                          
049600     MOVE 1     TO  PRT-AFTERVALUE-1                                      
049700     MOVE 11    TO  PRT-AFTERVALUE-2                                      
049800     MOVE 4     TO  PRT-AFTERVALUE-3                                      
049900     MOVE 1     TO  PRT-AFTERVALUE-4                                      
050000     MOVE 4     TO  PRT-AFTERVALUE-5                                      
050100     MOVE 1     TO  PRT-AFTERVALUE-6                                      
050200     MOVE 1     TO  PRT-AFTERVALUE-7                                      
050300     MOVE 1     TO  PRT-AFTERVALUE-8                                      
050400     MOVE 1     TO  PRT-AFTERVALUE-9                                      
050500     MOVE 4     TO  PRT-AFTERVALUE-10                                     
050600     MOVE 1     TO  PRT-AFTERVALUE-11                                     
050700     MOVE 1     TO  PRT-AFTERVALUE-12                                     
050800     MOVE 1     TO  PRT-AFTERVALUE-13                                     
050900     MOVE 1     TO  PRT-AFTERVALUE-14                                     
051000     MOVE 1     TO  PRT-AFTERVALUE-15                                     
051100     MOVE 1     TO  PRT-AFTERVALUE-16                                     
051200     MOVE 1     TO  PRT-AFTERVALUE-17                                     
051300     MOVE 1     TO  PRT-AFTERVALUE-18                                     
051400     MOVE 1     TO  PRT-AFTERVALUE-19                                     
051500     MOVE 6     TO  PRT-AFTERVALUE-20                                     
051600     MOVE 17    TO  PRT-AFTERVALUE-21                                     
051700     .                                                                    
051800     EJECT                                                                
051900 B-CHECK-KEYS SECTION.                                                    
052000                                                                          
052100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
052200     MOVE '001'             TO MSGI-KDCALL                                
052300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
052400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
052500     MOVE '618A'            TO MSGI-IDTRANS                               
052600                                                                          
052700     MOVE YES TO KEYS-SW                                                  
052800                                                                          
052900*    -- CHECK OF IDFPINST                                                 
053000     INSPECT MID-IDFPINST   REPLACING LEADING SPACE BY ZERO               
053100     IF NOT MID-IDFPINST    NUMERIC                                       
053200        MOVE 'ID:: FEL  ' TO WS-IDFPINST-TEST                             
053300        MOVE NOO            TO KEYS-SW                                    
053400     ELSE                                                                 
053500        IF MID-IDFPINST = ZERO                                            
053600           MOVE 'ID:: NOLL ' TO WS-IDFPINST-TEST                          
053700           MOVE NOO         TO KEYS-SW                                    
053800        ELSE                                                              
053900           MOVE MID-IDFPINST TO W-IDFPINST                                
054000           MOVE 'ID:: OK!  ' TO WS-IDFPINST-TEST                          
054100        END-IF                                                            
054200     END-IF                                                               
054300                                                                          
054400*    -- CHECK OF IDPRTLST                                                 
054500     MOVE SPACE             TO PRT-IDPRTLST                               
054600     MOVE '6L'              TO PRT-IDPRTLST(1:2)                          
054700     MOVE MID-IDPRTLST      TO PRT-IDPRTLST(3:5)                          
054800     MOVE 001               TO PRT-KDCALL                                 
054900     CALL W006PRT USING PRT-W006PRT                                       
055000     IF PRT-KDSVAR = 'R'                                                  
055100        IF PRT-BEPRTLST(1:5) = 'IBMLA'                                    
055200           MOVE 'PRT: OK!  ' TO WS-PRTTEST                                
055300        ELSE                                                              
055400*          FEL, SKALL SKRIVAS PÅ LASER !                                  
055500           MOVE 'INTE LASER' TO WS-PRTTEST                                
055600           MOVE NOO         TO KEYS-SW                                    
055700        END-IF                                                            
055800     ELSE                                                                 
055900*          FEL, SKRIVARE SAKNAS                                           
056000        MOVE 'SKRIVARE SAKNAS' TO WS-PRTTEST                              
056100        MOVE NOO            TO KEYS-SW                                    
056200     END-IF                                                               
056300                                                                          
056400     .                                                                    
056500     EJECT                                                                
056600 F-READ-DATABASE-INFO SECTION.                                            
056700                                                                          
056800     PERFORM FA-READ-PACKING-INSTRUCTION                                  
056900     IF DOC-OK                                                            
057000        IF DOC-IS-UNIQUE                                                  
057100           PERFORM FB-READ-PART-DESCRIPTION                               
057200           PERFORM FC-READ-PACKING-MATERIAL                               
057300           PERFORM FD-READ-TRANSPORT-SPEC                                 
057400           PERFORM FE-READ-RESPONSIBLE-DATA                               
057500        END-IF                                                            
057600     END-IF                                                               
057700                                                                          
057800     .                                                                    
057900     EJECT                                                                
058000 FA-READ-PACKING-INSTRUCTION SECTION.                                     
058100                                                                          
058200     MOVE MID-IDFPINST TO W-IDFPINST                                      
058300                                                                          
058400     PERFORM IMS-GET-INST-FPI                                             
058500     IF SEGMENT-MISSING                                                   
058600         MOVE NOO TO IS-DOC-OK-SW                                         
058700     ELSE                                                                 
058800         MOVE YES TO IS-DOC-OK-SW                                         
058900         IF INST-FPI-IDLEVNR > SPACE                                      
059000            MOVE GENERAL           TO TYPE-OF-DOC-SW                      
059100            MOVE 'GENERAL'         TO PRINT-IDARTNR-08                    
059200*** ---->   HÄMTAR ARTIKELNUMMREN UNDER UTSKRIFTS-SEKTIONEN               
059300            PERFORM FE-READ-RESPONSIBLE-DATA                              
059400         ELSE                                                             
059500            MOVE UNIQUE            TO TYPE-OF-DOC-SW                      
059600            PERFORM IMS-GET-INST-FPA                                      
059700            MOVE INST-FPA-IDARTNR  TO  PRINT-IDARTNR-08                   
059800            INSPECT PRINT-IDARTNR-08 REPLACING LEADING ZEROES             
059900                                     BY SPACE                             
060000        END-IF                                                            
060100        PERFORM FA2-ALLOT-PRINT-DATA                                      
060200     END-IF                                                               
060300                                                                          
060400                                                                          
060500     .                                                                    
060600     EJECT                                                                
060700 FA2-ALLOT-PRINT-DATA        SECTION.                                     
060800                                                                          
060900     MOVE INST-FPI-IDLEVNR        TO  PRINT-IDLEVNR-12                    
061100                                                                          
061200     MOVE MID-IDFPINST            TO  PRINT-IDFPINST-12                   
061300     INSPECT PRINT-IDFPINST-12  REPLACING LEADING ZEROES BY SPACE         
061400                                                                          
061500     MOVE INST-FPI-DAREGDAT       TO  PRINT-DAREGDAT-12                   
061600     INSPECT PRINT-DAREGDAT-12  REPLACING LEADING ZEROES BY SPACE         
061700                                                                          
061800     MOVE INST-FPI-TIUPPDAT       TO  WS-TIUPPDAT-S                       
061900     MOVE WS-TIUPPDAT-S           TO  WS-TIUPPDAT-X                       
062000     MOVE WS-TIUPPDAT2            TO  PRINT-TIUPPDAT-12                   
062100                                                                          
062200                                                                          
062300     MOVE INST-FPI-TEFPINST(1)    TO  PRINT-TEBEFT-01-29                  
062400     MOVE INST-FPI-TEFPINST(2)    TO  PRINT-TEBEFT-02-30                  
062500     MOVE INST-FPI-TEFPINST(3)    TO  PRINT-TEBEFT-03-31                  
062600     MOVE INST-FPI-TEFPINST(4)    TO  PRINT-TEBEFT-04-32                  
062700     MOVE INST-FPI-TEFPINST(5)    TO  PRINT-TEBEFT-05-33                  
062800     MOVE INST-FPI-TEFPINST(6)    TO  PRINT-TEBEFT-06-34                  
062900     MOVE INST-FPI-TEFPINST(7)    TO  PRINT-TEBEFT-07-35                  
063000     MOVE INST-FPI-TEFPINST(8)    TO  PRINT-TEBEFT-08-36                  
063100     MOVE INST-FPI-TEFPINST(9)    TO  PRINT-TEBEFT-09-37                  
063200     MOVE INST-FPI-TEFPINST(10)   TO  PRINT-TEBEFT-10-38                  
063300     INSPECT PRINT-TEBEFT-01-29 REPLACING LEADING ZEROES BY SPACE         
063400     INSPECT PRINT-TEBEFT-02-30 REPLACING LEADING ZEROES BY SPACE         
063500     INSPECT PRINT-TEBEFT-03-31 REPLACING LEADING ZEROES BY SPACE         
063600     INSPECT PRINT-TEBEFT-04-32 REPLACING LEADING ZEROES BY SPACE         
063700     INSPECT PRINT-TEBEFT-05-33 REPLACING LEADING ZEROES BY SPACE         
063800     INSPECT PRINT-TEBEFT-06-34 REPLACING LEADING ZEROES BY SPACE         
063900     INSPECT PRINT-TEBEFT-07-35 REPLACING LEADING ZEROES BY SPACE         
064000     INSPECT PRINT-TEBEFT-08-36 REPLACING LEADING ZEROES BY SPACE         
064100     INSPECT PRINT-TEBEFT-09-37 REPLACING LEADING ZEROES BY SPACE         
064200     INSPECT PRINT-TEBEFT-10-38 REPLACING LEADING ZEROES BY SPACE         
064300     .                                                                    
064400     EJECT                                                                
064500 FB-READ-PART-DESCRIPTION    SECTION.                                     
064600                                                                          
064700     IF INST-FPA-IDARTNR NOT = ZERO                                       
064800        MOVE INST-FPA-IDARTNR TO W-IDARTNR2                               
064900        PERFORM IMS-GET-BENA-TEXT                                         
064910        IF SEGMENT-FOUND                                                  
065000           MOVE BENA-TEXT-BEART TO PRINT-BEART-12                         
065010        ELSE                                                              
065020           MOVE '?' TO PRINT-BEART-12                                     
065030        END-IF                                                            
065100     ELSE                                                                 
065200        MOVE SPACE           TO PRINT-BEART-12                            
065300     END-IF                                                               
065400     INSPECT PRINT-BEART-12     REPLACING LEADING ZEROES BY SPACE         
065500                                                                          
065600     .                                                                    
065700     EJECT                                                                
065800 FC-READ-PACKING-MATERIAL    SECTION.                                     
065900     MOVE INST-FPA-IDARTNR TO W-IDARTNR                                   
066000     PERFORM IMS-GET-ARTC-ROT                                             
066100     IF SEGMENT-FOUND                                                     
066200        PERFORM IMS-GET-ARTC-EMB                                          
066300        PERFORM UNTIL SEGMENT-MISSING                                     
066400           PERFORM FC1-READ-EMB-DESCRIPTION                               
066500           EVALUATE ARTC-EMB-KDEMBKEY                                     
066600              WHEN 'Q0 '                                                  
066700                  MOVE ARTC-EMB-IDARTNR-EMB TO PRINT-IDARTNR-Q0-16        
066800                  MOVE ARTC-EMB-KVQPACK-EMB TO PRINT-KVQPACK-Q0-16        
066900                   MOVE BENA-TEXT-BEART    TO PRINT-BEART-Q0-16           
067000              WHEN 'Q1 '                                                  
067100                  MOVE ARTC-EMB-IDARTNR-EMB TO PRINT-IDARTNR-Q1-16        
067200                  MOVE ARTC-EMB-KVQPACK-EMB TO PRINT-KVQPACK-Q1-16        
067300                   MOVE BENA-TEXT-BEART    TO PRINT-BEART-Q1-16           
067400              WHEN 'Q2 '                                                  
067500                  MOVE ARTC-EMB-IDARTNR-EMB TO PRINT-IDARTNR-Q2-17        
067600                  MOVE ARTC-EMB-KVQPACK-EMB TO PRINT-KVQPACK-Q2-17        
067700                   MOVE BENA-TEXT-BEART    TO PRINT-BEART-Q2-17           
067800              WHEN 'X01'                                                  
067900                  MOVE ARTC-EMB-IDARTNR-EMB TO PRINT-IDARTNR-01-21        
068000                  MOVE ARTC-EMB-KVQPACK-EMB TO PRINT-KVQPACK-01-21        
068100                   MOVE BENA-TEXT-BEART    TO PRINT-BEART-01-21           
068200              WHEN 'X02'                                                  
068300                  MOVE ARTC-EMB-IDARTNR-EMB TO PRINT-IDARTNR-02-22        
068400                  MOVE ARTC-EMB-KVQPACK-EMB TO PRINT-KVQPACK-02-22        
068500                   MOVE BENA-TEXT-BEART    TO PRINT-BEART-02-22           
068600              WHEN 'X03'                                                  
068700                  MOVE ARTC-EMB-IDARTNR-EMB TO PRINT-IDARTNR-03-23        
068800                  MOVE ARTC-EMB-KVQPACK-EMB TO PRINT-KVQPACK-03-23        
068900                   MOVE BENA-TEXT-BEART    TO PRINT-BEART-03-23           
069000              WHEN 'X04'                                                  
069100                  MOVE ARTC-EMB-IDARTNR-EMB TO PRINT-IDARTNR-04-24        
069200                  MOVE ARTC-EMB-KVQPACK-EMB TO PRINT-KVQPACK-04-24        
069300                   MOVE BENA-TEXT-BEART    TO PRINT-BEART-04-24           
069400              WHEN 'X05'                                                  
069500                  MOVE ARTC-EMB-IDARTNR-EMB TO PRINT-IDARTNR-05-25        
069600                  MOVE ARTC-EMB-KVQPACK-EMB TO PRINT-KVQPACK-05-25        
069700                   MOVE BENA-TEXT-BEART    TO PRINT-BEART-05-25           
069800              WHEN 'X06'                                                  
069900                  MOVE ARTC-EMB-IDARTNR-EMB TO PRINT-IDARTNR-06-21        
070000                  MOVE ARTC-EMB-KVQPACK-EMB TO PRINT-KVQPACK-06-21        
070100                   MOVE BENA-TEXT-BEART    TO PRINT-BEART-06-21           
070200              WHEN 'X07'                                                  
070300                  MOVE ARTC-EMB-IDARTNR-EMB TO PRINT-IDARTNR-07-22        
070400                  MOVE ARTC-EMB-KVQPACK-EMB TO PRINT-KVQPACK-07-22        
070500                   MOVE BENA-TEXT-BEART    TO PRINT-BEART-07-22           
070600              WHEN 'X08'                                                  
070700                  MOVE ARTC-EMB-IDARTNR-EMB TO PRINT-IDARTNR-08-23        
070800                  MOVE ARTC-EMB-KVQPACK-EMB TO PRINT-KVQPACK-08-23        
070900                   MOVE BENA-TEXT-BEART    TO PRINT-BEART-08-23           
071000              WHEN 'X09'                                                  
071100                  MOVE ARTC-EMB-IDARTNR-EMB TO PRINT-IDARTNR-09-24        
071200                  MOVE ARTC-EMB-KVQPACK-EMB TO PRINT-KVQPACK-09-24        
071300                   MOVE BENA-TEXT-BEART    TO PRINT-BEART-09-24           
071400              WHEN 'X10'                                                  
071500                  MOVE ARTC-EMB-IDARTNR-EMB TO PRINT-IDARTNR-10-25        
071600                  MOVE ARTC-EMB-KVQPACK-EMB TO PRINT-KVQPACK-10-25        
071700                   MOVE BENA-TEXT-BEART    TO PRINT-BEART-10-25           
071800           END-EVALUATE                                                   
071900           PERFORM IMS-GET-ARTC-EMB                                       
072000        END-PERFORM                                                       
072100     END-IF                                                               
072200                                                                          
072300     INSPECT PRINT-IDARTNR-Q0-16 REPLACING LEADING ZEROES BY SPACE        
072400     INSPECT PRINT-KVQPACK-Q0-16 REPLACING LEADING ZEROES BY SPACE        
072500     INSPECT PRINT-IDARTNR-Q1-16 REPLACING LEADING ZEROES BY SPACE        
072600     INSPECT PRINT-KVQPACK-Q1-16 REPLACING LEADING ZEROES BY SPACE        
072700     INSPECT PRINT-IDARTNR-Q2-17 REPLACING LEADING ZEROES BY SPACE        
072800     INSPECT PRINT-KVQPACK-Q2-17 REPLACING LEADING ZEROES BY SPACE        
072900     INSPECT PRINT-IDARTNR-01-21 REPLACING LEADING ZEROES BY SPACE        
073000     INSPECT PRINT-KVQPACK-01-21 REPLACING LEADING ZEROES BY SPACE        
073100     INSPECT PRINT-IDARTNR-02-22 REPLACING LEADING ZEROES BY SPACE        
073200     INSPECT PRINT-KVQPACK-02-22 REPLACING LEADING ZEROES BY SPACE        
073300     INSPECT PRINT-IDARTNR-03-23 REPLACING LEADING ZEROES BY SPACE        
073400     INSPECT PRINT-KVQPACK-03-23 REPLACING LEADING ZEROES BY SPACE        
073500     INSPECT PRINT-IDARTNR-04-24 REPLACING LEADING ZEROES BY SPACE        
073600     INSPECT PRINT-KVQPACK-04-24 REPLACING LEADING ZEROES BY SPACE        
073700     INSPECT PRINT-IDARTNR-05-25 REPLACING LEADING ZEROES BY SPACE        
073800     INSPECT PRINT-KVQPACK-05-25 REPLACING LEADING ZEROES BY SPACE        
073900     INSPECT PRINT-IDARTNR-06-21 REPLACING LEADING ZEROES BY SPACE        
074000     INSPECT PRINT-KVQPACK-06-21 REPLACING LEADING ZEROES BY SPACE        
074100     INSPECT PRINT-IDARTNR-07-22 REPLACING LEADING ZEROES BY SPACE        
074200     INSPECT PRINT-KVQPACK-07-22 REPLACING LEADING ZEROES BY SPACE        
074300     INSPECT PRINT-IDARTNR-08-23 REPLACING LEADING ZEROES BY SPACE        
074400     INSPECT PRINT-KVQPACK-08-23 REPLACING LEADING ZEROES BY SPACE        
074500     INSPECT PRINT-IDARTNR-09-24 REPLACING LEADING ZEROES BY SPACE        
074600     INSPECT PRINT-KVQPACK-09-24 REPLACING LEADING ZEROES BY SPACE        
074700     INSPECT PRINT-IDARTNR-10-25 REPLACING LEADING ZEROES BY SPACE        
074800     INSPECT PRINT-KVQPACK-10-25 REPLACING LEADING ZEROES BY SPACE        
074900                                                                          
075000     .                                                                    
075100     EJECT                                                                
075200 FC1-READ-EMB-DESCRIPTION      SECTION.                                   
075300                                                                          
075400     IF ARTC-EMB-IDARTNR-EMB NOT = ZERO                                   
075500        MOVE ARTC-EMB-IDARTNR-EMB  TO W-IDARTNR2                          
075600        PERFORM IMS-GET-BENA-TEXT                                         
075610        IF NOT SEGMENT-FOUND                                              
075620           MOVE '?' TO BENA-TEXT-BEART                                    
075630        END-IF                                                            
075700     ELSE                                                                 
075800        MOVE SPACE  TO  BENA-TEXT-BEART                                   
075900     END-IF                                                               
076000                                                                          
076100     .                                                                    
076200     EJECT                                                                
076300 FD-READ-TRANSPORT-SPEC      SECTION.                                     
076400                                                                          
076500     PERFORM IMS-GET-ARTC-CLAG                                            
076600     IF SEGMENT-FOUND                                                     
076700       MOVE 1                   TO EMB-IX                                 
076800       PERFORM UNTIL EMB-IX     >  TAB-EMBQ3-MAX OR                       
076900            TAB-KOD (EMB-IX) = ARTC-CLAG-IDARTNR-EMBQ3                    
077000         ADD 1                  TO EMB-IX                                 
077100       END-PERFORM                                                        
077200                                                                          
077300       IF EMB-IX                > TAB-EMBQ3-MAX                           
077310          IF ARTC-CLAG-IDARTNR-EMBQ3 = ZERO                               
077500             MOVE SPACE         TO PRINT-Q3EMB-44                         
077510          ELSE                                                            
077520***          FELMEDELANDE ?                                               
077600             MOVE '???'         TO PRINT-Q3EMB-44                         
077700***          FELMEDELANDE ?                                               
077800          END-IF                                                          
077810        ELSE                                                              
077900           IF TAB-KOD (EMB-IX)      =  ARTC-CLAG-IDARTNR-EMBQ3            
078000               MOVE TAB-TEXT (EMB-IX) TO PRINT-Q3EMB-44                   
078100           END-IF                                                         
078200       END-IF                                                             
078300       MOVE ARTC-CLAG-KVQPACK-3     TO PRINT-KVQPACK-Q3-44                
078400       INSPECT PRINT-KVQPACK-Q3-44  REPLACING LEADING ZEROES              
078500                                    BY SPACE                              
078510       IF PRINT-KVQPACK-Q3-44 = SPACE                                     
078520          MOVE '    0' TO PRINT-KVQPACK-Q3-44                             
078530       END-IF                                                             
078600                                                                          
078700     ELSE                                                                 
078800***         FELMEDELANDE ?                                                
078900*       CONTINUE                                                          
079000        MOVE '? 11 ?'          TO PRINT-Q3EMB-44                          
079100***         FELMEDELANDE ?                                                
079200     END-IF                                                               
079300                                                                          
079400     .                                                                    
079500     EJECT                                                                
079600 FE-READ-RESPONSIBLE-DATA    SECTION.                                     
079700     MOVE INST-FPI-IDUSER       TO WS-IDPERSON                            
079800     MOVE WS-IDPERSON           TO W-IDPERSON                             
079900     PERFORM IMS-GET-WDP311                                               
080000     IF SEGMENT-FOUND                                                     
080100        MOVE PERS-BEINIT     TO PRINT-INITIALER-61                        
080200        MOVE ' / '           TO PRINT-SLASH-61                            
080300        MOVE PERS-IDAVD      TO PRINT-AVDELNING-61                        
080400        INSPECT PRINT-AVDELNING-61  REPLACING LEADING ZEROES              
080500                                    BY SPACE                              
080600     ELSE                                                                 
080700*       FELMEDDELANDE ???                                                 
080800*       CONTINUE                                                          
080900        MOVE INST-FPI-IDUSER TO PRINT-INITIALER-61                        
081000        MOVE ' / '           TO PRINT-SLASH-61                            
081100        MOVE '?FEL?'         TO PRINT-AVDELNING-61                        
081200*       FELMEDDELANDE ???                                                 
081300     END-IF                                                               
081400                                                                          
081500     .                                                                    
081600     EJECT                                                                
081700 E-PRINT-DOCUMENT SECTION.                                                
081800                                                                          
081900     PERFORM EA-OPEN-PRINTER                                              
082000     IF DOC-IS-UNIQUE                                                     
082100        PERFORM EB-PRINT-UNIQUE-DOCUMENT                                  
082200     ELSE                                                                 
082300        PERFORM EC1-PRINT-GENERAL-DOCUMENT                                
082400        PERFORM EC2-PRINT-GENERAL-LIST                                    
082500     END-IF                                                               
082600     PERFORM ED-CLOSE-PRINTER                                             
082700                                                                          
082800     .                                                                    
082900     EJECT                                                                
083000 EA-OPEN-PRINTER SECTION.                                                 
083100                                                                          
083200     MOVE 'W61101  '  TO PRT-PFDEF-OVR                                    
083300                                                                          
083400     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN PRT-IDPRTLST              
083500                        ALT-PCB                                           
083600                        PRT-FILLER PRT-FILLER                             
083700     .                                                                    
083800     EJECT                                                                
083900 EB-PRINT-UNIQUE-DOCUMENT SECTION.                                        
084000     PERFORM S01-PRINT-DOCUMENT                                           
084100     .                                                                    
084200     EJECT                                                                
084300 EC1-PRINT-GENERAL-DOCUMENT SECTION.                                      
084400     PERFORM S01-PRINT-DOCUMENT                                           
084500     .                                                                    
084600     EJECT                                                                
084700 EC2-PRINT-GENERAL-LIST SECTION.                                          
084800                                                                          
084900     PERFORM IMS-GET-INST-FPA                                             
085000     IF SEGMENT-FOUND                                                     
085100       PERFORM EC3-REINIT-PRINTER                                         
085200       PERFORM UNTIL SEGMENT-MISSING                                      
085300          MOVE INST-FPA-IDARTNR TO PRINT2-IDARTNR                         
085400          INSPECT PRINT2-IDARTNR    REPLACING LEADING ZEROES              
085500                                    BY SPACE                              
085600          MOVE PRINT2-DATA      TO PRT-RAD                                
085700          PERFORM S02-WRITE-LINE                                          
085800          MOVE PRT-AFTER-1 TO PRT-RADSKIP                                 
085900          PERFORM IMS-GET-INST-FPA                                        
086000       END-PERFORM                                                        
086100     END-IF                                                               
086200                                                                          
086300     .                                                                    
086400     EJECT                                                                
086500 EC3-REINIT-PRINTER SECTION.                                              
086600*  CLOSE PRINTER WITH OLD PFDEF.                                          
086700     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE PRT-IDPRTLST             
086800                        ALT-PCB                                           
086900                        PRT-FILLER PRT-FILLER                             
087000                                                                          
087100*  REOPEN PRINTER WITH NEW PFDEF.                                         
087200     MOVE 'SPS06C  '  TO PRT-PFDEF-OVR                                    
087300     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-OPEN PRT-IDPRTLST              
087400                        ALT-PCB                                           
087500                        PRT-FILLER PRT-FILLER                             
087600*  PRINTS HEADERS                                                         
087700       MOVE PRT-NYSIDA-RAD1 TO PRT-RADSKIP                                
087800       MOVE PRINT2-HEADER1 TO PRT-RAD                                     
087900       PERFORM S02-WRITE-LINE                                             
088000                                                                          
088100       MOVE PRT-AFTER-1 TO PRT-RADSKIP                                    
088200       MOVE MID-IDFPINST    TO HEADER-IDFPINST                            
088300       INSPECT HEADER-IDFPINST      REPLACING LEADING ZEROES              
088400                                    BY SPACE                              
088500       MOVE PRINT2-HEADER2 TO PRT-RAD                                     
088600       PERFORM S02-WRITE-LINE                                             
088700       MOVE PRT-AFTER-2 TO PRT-RADSKIP                                    
088800     .                                                                    
088900     EJECT                                                                
089000 ED-CLOSE-PRINTER SECTION.                                                
089100     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE PRT-IDPRTLST             
089200                        ALT-PCB                                           
089300                        PRT-FILLER PRT-FILLER                             
089400     .                                                                    
089500     EJECT                                                                
089600 S01-PRINT-DOCUMENT SECTION.                                              
089700     MOVE 'DOCUMENT PRINTED' TO WS-PRINT-STATUS                           
089800     MOVE 1 TO DOC-IX                                                     
089900     PERFORM UNTIL DOC-IX > 21                                            
090000        MOVE PRT-AFTER-IX(DOC-IX) TO PRT-RADSKIP                          
090100        MOVE PRINT-ROW-IX(DOC-IX)   TO PRT-RAD                            
090200        PERFORM S02-WRITE-LINE                                            
090300        ADD +1 TO DOC-IX                                                  
090400     END-PERFORM                                                          
090500     .                                                                    
090600     EJECT                                                                
090700 S02-WRITE-LINE   SECTION.                                                
090800     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE PRT-IDPRTLST             
090900                        ALT-PCB                                           
091000                        PRT-RADSKIP PRT-RAD                               
091100     .                                                                    
091200     EJECT                                                                
091300* --- IMS SECTIONS ---                                                    
091400     SKIP3                                                                
091500 IMS-GET-MSG SECTION.                                                     
091600                                                                          
091700     MOVE '  QC' TO GOOD-STATUSCODES                                      
091800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
091900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
092000     PERFORM IMS-STATUSCHECK                                              
092100     .                                                                    
092200     SKIP3                                                                
092300 IMS-GET-ARTC-ROT SECTION.                                                
092400                                                                          
092500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X  ')'                        
092600          DELIMITED BY SIZE INTO SSA1                                     
092700     MOVE '  GEGP' TO GOOD-STATUSCODES                                    
092800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WDK601 SSA1                    
092900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
093000     PERFORM IMS-STATUSCHECK                                              
093100     .                                                                    
093200     EJECT                                                                
093300 IMS-GET-ARTC-CLAG SECTION.                                               
093400                                                                          
093500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
093600          DELIMITED BY SIZE INTO SSA1                                     
093700     MOVE 'WDK611' TO SSA2                                                
093800     MOVE '  GE' TO GOOD-STATUSCODES                                      
093900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WDK611 SSA1 SSA2               
094000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
094100     PERFORM IMS-STATUSCHECK                                              
094200     .                                                                    
094300     EJECT                                                                
094400 IMS-GET-ARTC-EMB SECTION.                                                
094500                                                                          
094600*    STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X  ')'                        
094700*         DELIMITED BY SIZE INTO SSA1                                     
094800     MOVE 'WDK613' TO SSA1                                                
094900     MOVE '  GE' TO GOOD-STATUSCODES                                      
095000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WDK613 SSA1                   
095100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
095200     PERFORM IMS-STATUSCHECK                                              
095300     .                                                                    
095400     EJECT                                                                
095500 IMS-GET-INST-FPI SECTION.                                                
095600                                                                          
095700     STRING 'WDD101  (IDFPINST =' W-IDFPINST-X ')'                        
095800          DELIMITED BY SIZE INTO SSA1                                     
095900     MOVE '  GEGP' TO GOOD-STATUSCODES                                    
096000     CALL CBLTDLI USING GU INST-PCB DLI-IO-WDD101 SSA1                    
096100     MOVE INST-STATUS-CODE TO STATUS-WS                                   
096200     PERFORM IMS-STATUSCHECK                                              
096300     .                                                                    
096400     EJECT                                                                
096500 IMS-GET-INST-FPA SECTION.                                                
096600                                                                          
096700     STRING 'WDD101  (IDFPINST =' W-IDFPINST-X ')'                        
096800          DELIMITED BY SIZE INTO SSA1                                     
096900     MOVE   'WDD111' TO SSA2                                              
097000     MOVE '  GEGP' TO GOOD-STATUSCODES                                    
097100     CALL CBLTDLI USING GNP INST-PCB DLI-IO-WDD111 SSA1 SSA2              
097200     MOVE INST-STATUS-CODE TO STATUS-WS                                   
097300     PERFORM IMS-STATUSCHECK                                              
097400     .                                                                    
097500     EJECT                                                                
097600 IMS-GET-BENA-TEXT SECTION.                                               
097700     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-Y ')'                         
097800             DELIMITED BY SIZE INTO SSA1                                  
097900     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
098000              DELIMITED BY SIZE INTO SSA2                                 
098100     MOVE '  GE' TO GOOD-STATUSCODES                                      
098200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WDD311 SSA1 SSA2               
098300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
098400     PERFORM IMS-STATUSCHECK                                              
098500     .                                                                    
098600     EJECT                                                                
098700 IMS-GET-WDP311 SECTION.                                                  
098800     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
098900            DELIMITED BY SIZE INTO SSA1                                   
099000     STRING 'WDP311  (IDPERSON ='  W-IDPERSON-X ')'                       
099100            DELIMITED BY SIZE INTO SSA2                                   
099200     MOVE '  GE' TO GOOD-STATUSCODES                                      
099300     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
099400     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
099500     PERFORM IMS-STATUSCHECK                                              
099600     .                                                                    
099700     EJECT                                                                
099800 IMS-STATUSCHECK SECTION.                                                 
099900                                                                          
100000     SET STATUS-IX TO 1                                                   
100100     SEARCH GOOD-STATUS                                                   
100200       AT END                                                             
100300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
100400         DELIMITED BY SIZE INTO ERROR-TEXT                                
100500         CALL FELLOG                                                      
100600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
100700         CONTINUE                                                         
100800     END-SEARCH                                                           
100900     .                                                                    

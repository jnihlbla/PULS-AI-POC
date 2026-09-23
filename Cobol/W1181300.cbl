000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1181300.                                                
000300 AUTHOR.         DADHICH PRERNA.                                          
000400 DATE-WRITTEN.   24/12/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        READS INPUT FILE W1181101 WITH NEW PARTS INFO FROM PRINS         
001000*        TCPLM AND CALLS 1116 PROGRAM FOR PART INSERT                     
001100*                                                                         
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- FILE WITH PART INFO                                        
002200     SELECT W11811                     ASSIGN TO W11813D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W11811                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W1181101  PRE IN- -L.                                          
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(8)    VALUE 'W1181300'.            
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 01  CHKP-VAR.                                                            
004000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004500     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004600 77  YES                         PIC X       VALUE 'J'.                   
004700 77  NOO                         PIC X       VALUE 'N'.                   
004800 77  WS-TIUPPDAT                 PIC S9(7) COMP-3 VALUE ZERO.             
004900 77  WS-TIUPPTID                 PIC S9(9) COMP-3 VALUE ZERO.             
005000     SKIP2                                                                
005100                                                                          
005200 77  W11811-EMPTY-SW             PIC X       VALUE 'Y'.                   
005300 77  W11811-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W11811                       VALUE 'Y'.                   
005500 77  SW-UPDATE                   PIC X       VALUE 'N'.                   
005600     88  UPDATE-JA                           VALUE 'J'.                   
005700     EJECT                                                                
005800 01  WS-WORKING.                                                          
005900     03  WS-IDBERED              PIC 9(2)    VALUE ZERO.                  
006000     03  WS-KDPRODSL             PIC 9(2)    VALUE ZERO.                  
006100     03  WS-KDYTBEH              PIC 9(2)    VALUE ZERO.                  
006200     03  WS-KDFARLIG             PIC 9(1)    VALUE ZERO.                  
006300     03  WS-KDBPSR               PIC 9(1)    VALUE ZERO.                  
006400     03  WS-TISOP                PIC 9(5)    VALUE ZERO.                  
006500     03  WS-IDFKNGRP             PIC 9(4)    VALUE ZERO.                  
006600     03  WS-IDARTNR-MOTSV        PIC 9(9)    VALUE ZERO.                  
006700     EJECT                                                                
006800 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006900 01  FILLER REDEFINES TODAYS-DATE.                                        
007000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
007100     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007200     03  TODAYS-DATE-DAY         PIC 9(2).                                
007300     EJECT                                                                
007400 01  GENERAL-SUBPROGRAMS.                                                 
007500*                                                                         
007600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007800     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     SKIP2                                                                
008200*    --- PARAMETERS TO ABEND                                              
008300                                                                          
008400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008700 01  WS-IDARTNR                  PIC X(9)  VALUE SPACE.                   
008800 01  ERROR-TEXT.                                                          
008900     03  FILLER                  PIC X(8)    VALUE 'ERRTXT'.              
009000     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
009100     EJECT                                                                
009200*-----------------------------------------------------------              
009300***                    MESSAGE AREA                                       
009400*-----------------------------------------------------------              
009500*                                                                         
009600*01  -COPY WMSGAREA                                                       
009700       05  FILLER REDEFINES MSG-MID-OUT.                                  
009800          07  -COPY W1I11601 -PRE 1116-                                   
009900       05  FILLER REDEFINES MSG-MID-OUT.                                  
010000          07  -COPY W1I11701 -PRE 1117-                                   
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL POSTSUM                                          
010300*                                                                         
010400*01  -COPY W0005   -PRE  POSTSUM-                                         
010500     EJECT                                                                
010600 01  FILLER                      PIC X(8)   VALUE                         
010700                                             'IN-AREA'.                   
010800*01  AREA -COPY W1181101   -PRE IN-                                       
011000     EJECT                                                                
011100*    ---  AREA FÖR W006KOM SUBMODUL                                       
011200 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
011300                                                                          
011400 01  KOM-IO-AREA.                                                         
011500*    03  -COPY WMSGKOM                                                    
011600     EJECT                                                                
011700*                                                                         
011800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011900                                                                          
012000 01  NYCKLAR-TILL-DLI.                                                    
012100     03  W-IDARTNR-X.                                                     
012200         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
012300                                                                          
012400     03  W-KDSEGKEY-X.                                                    
012500         05  W-KDSEGKEY          PIC  X      VALUE '1'.                   
012600                                                                          
012700     SKIP3                                                                
012800*    --- STATUS-CODE FROM IMS                                             
012900 01  STATUS-WS                   PIC XX.                                  
013000     88  SEGMENT-FOUND                       VALUE '  '.                  
013100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
013200     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
013300     88  IMS-NOT-OK                          VALUE 'XD'.                  
013400                                                                          
013500 01  GOOD-STATUSCODES.                                                    
013600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013700                                                                          
013800 01  SSA1                        PIC X(64).                               
013900 01  SSA2                        PIC X(64).                               
014000     EJECT                                                                
014100*    --- IMS FUNKTIONSKODER                                               
014200*01  -COPY W0003                                                          
014300     EJECT                                                                
014400                                                                          
014500*    ---  DLI INPUT-OUTPUT AREA                                           
014600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK60111'.                    
014700 01  DLI-IO-WDK60111.                                                     
014800*    03  -COPY WDK601                                                     
014900*    03  -COPY WDK611                                                     
015000                                                                          
015100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
015200 01  DLI-IO-WDD201.                                                       
015300*    03  -COPY WDD201     -PRE WDD2-                                      
015400                                                                          
015500                                                                          
015600 LINKAGE SECTION.                                                         
015700                                                                          
015800*01  -COPY W0009 -PRE MSG-                                                
015900     EJECT                                                                
016000*01  -COPY W0009 -PRE 0693X-                                              
016100     EJECT                                                                
016200*01  -COPY W0009 -PRE WDP8-                                               
016300     EJECT                                                                
016400*01  -COPY W0008 -PRE WDK6-                                               
016500     05  FILLER              PIC X.                                       
016600     EJECT                                                                
016700*01  -COPY W0008 -PRE WDD2-                                               
016800     05  FILLER              PIC X.                                       
016900     EJECT                                                                
017000                                                                          
017100 PROCEDURE DIVISION  USING MSG-PCB  0693X-PCB WDP8-PCB                    
017200                           WDK6-PCB WDD2-PCB.                             
017300     ENTRY 'DLITCBL' USING MSG-PCB  0693X-PCB WDP8-PCB                    
017400                           WDK6-PCB WDD2-PCB.                             
017500                                                                          
017600     SKIP2                                                                
017700     PERFORM A-INIT                                                       
017800     PERFORM S01-READ-W11811                                              
017900     PERFORM UNTIL END-OF-W11811                                          
018000       IF IN-IDPTYP = 'INS' OR 'SSI' OR 'EXI' OR 'SWI'                    
018100*         SWS NEED TO BE HANDLED HERE AS WELL IN SS PHASE-2               
018200          PERFORM B-CREATE-INS-TRANS                                      
018300          PERFORM E-SEND-TRANSACTION                                      
018400       ELSE                                                               
018500          IF IN-IDPTYP = 'UPD' OR 'SSU' OR 'EXU' OR 'EXS' OR              
018600                         'SWU' OR 'SWS'                                   
018700             PERFORM C-CREATE-UPD-TRANS                                   
018800             IF UPDATE-JA                                                 
018900                PERFORM E-SEND-TRANSACTION                                
019000             END-IF                                                       
019100          END-IF                                                          
019200       END-IF                                                             
019300       PERFORM S01-READ-W11811                                            
019400     END-PERFORM                                                          
019500                                                                          
019600     IF W11811-EMPTY-SW = 'Y'                                             
019700        DISPLAY 'INPUT FILE IS EMPTY FROM TCPLM/PRINS'                    
019800     END-IF                                                               
019900                                                                          
020000     PERFORM Z-FINIT                                                      
020100                                                                          
020200     MOVE ZERO TO RETURN-CODE                                             
020300     GOBACK                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 A-INIT SECTION.                                                          
020700     SKIP2                                                                
020800                                                                          
020900                                                                          
021000     OPEN INPUT  W11811                                                   
021100     MOVE ZERO TO CHKP-ANT                                                
021200     PERFORM IMS-RESTART                                                  
021300     PERFORM AA-WRITE-HEADER                                              
021400     .                                                                    
021500     EJECT                                                                
021600 AA-WRITE-HEADER SECTION.                                                 
021700     MOVE +54                   TO MSG-KOM-KVLL                           
021800     MOVE LOW-VALUE             TO MSG-KOM-KDZ1                           
021900                                   MSG-KOM-KDZ2                           
022000     MOVE SPACE                 TO MSG-KOM-KDTRANS                        
022100     MOVE 'TC PLM'              TO MSG-KOM-IDSNDNOD                       
022200     MOVE IDPGM                 TO MSG-KOM-IDSNDJOB                       
022300                                                                          
022400     ACCEPT WS-TIUPPDAT FROM DATE                                         
022500     ACCEPT WS-TIUPPTID FROM TIME                                         
022600     MOVE WS-TIUPPDAT           TO MSG-KOM-TIREGDAT                       
022700     MOVE WS-TIUPPTID           TO MSG-KOM-TIKLOCK                        
022800     MOVE SPACE                 TO MSG-KOM-IDMFSMED                       
022900                                   MSG-KOM-KDSVAR                         
023000     MOVE IDPGM                 TO POSTSUM-PROGNAMN                       
023100     .                                                                    
023200     EJECT                                                                
023300 B-CREATE-INS-TRANS SECTION.                                              
023400                                                                          
023500***  CALLS PGM W10116 THROUGH DISPACTHER                                  
023600***  MOVE  W11811 COPYBOOK TO W10116 INPUT COPYBOOK                       
023700***  WE FIRST LOAD MID-COPYBOOK WITH ALL + VALUES                         
023800                                                                          
023900     INITIALIZE MSG-IO-AREA                                               
024000     MOVE ALL '+'               TO 1116-MID-W1I11601                      
024100     PERFORM BA-CREATE-HEADER-1116                                        
024200*                                                                         
024300     MOVE IN-IDARTNR            TO 1116-MID-IDARTNR-IN                    
024400                                   W-IDARTNR                              
024500     MOVE IN-IDARTNR            TO 1116-MID-IDARTNR-UT                    
024600     MOVE IN-IDARTNR            TO 1116-MID-IDARTNR-NY                    
024700     MOVE IN-IDBERED            TO 1116-MID-IDBERED                       
024800     MOVE IN-KDPRODSL           TO 1116-MID-KDPRODSL                      
024900     MOVE IN-KDSORT             TO 1116-MID-KDSORT                        
025000     MOVE IN-IDPROENH           TO 1116-MID-IDPROENH-1                    
025100     MOVE IN-KDYTBEH            TO 1116-MID-KDYTBEH                       
025200     MOVE IN-IDPROJ             TO 1116-MID-IDPROJ                        
025300     MOVE IN-KDFARLIG           TO 1116-MID-KDFARLIG                      
025400     MOVE IN-KDBPSR             TO 1116-MID-KDBPSR                        
025500     MOVE IN-KDUART             TO 1116-MID-KDUART                        
025600     MOVE IN-IDPROJK            TO 1116-MID-IDPROJK                       
025700     MOVE IN-FLPISK             TO 1116-MID-FLPISK                        
025800     MOVE IN-IDAO               TO 1116-MID-IDAO                          
025900     MOVE IN-TISOP              TO 1116-MID-TISOP                         
026000     MOVE IN-IDSKYLT            TO 1116-MID-IDSKYLT                       
026100     MOVE IN-FLLSRDEL           TO 1116-MID-FLLSRDEL                      
026200     MOVE IN-IDPROJUP           TO 1116-MID-IDPROJUP                      
026300     MOVE IN-BEART              TO 1116-MID-BEART                         
026400     MOVE IN-FLRSBEART          TO 1116-MID-FLRSBEART                     
026500     MOVE IN-IDFKNGRP           TO 1116-MID-IDFKNGRP                      
026600     MOVE IN-TEORSAK-1          TO 1116-MID-TEORSAK-1                     
026700     MOVE IN-TEARTNOT-2         TO 1116-MID-TEARTNOT-2                    
026800     MOVE IN-IDRITN             TO 1116-MID-IDRITN                        
026900     MOVE IN-TEARTNOT-7         TO 1116-MID-TEARTNOT-7                    
027000     MOVE IN-TEARTNOT-4         TO 1116-MID-TEARTNOT-4                    
027100     MOVE IN-IDARTNR-MOTSV      TO 1116-MID-IDARTNR-MOTSV                 
027200     MOVE IN-IDKAT-1            TO 1116-MID-IDKAT-1                       
027300     MOVE IN-IDKAT-2            TO 1116-MID-IDKAT-2                       
027400     MOVE IN-IDKAT-3            TO 1116-MID-IDKAT-3                       
027500     MOVE IN-IDCDS              TO 1116-MID-IDCDS                         
027600     MOVE IN-KDARTSYS           TO 1116-MID-KDARTSYS                      
027700     .                                                                    
027800     EJECT                                                                
027900 BA-CREATE-HEADER-1116  SECTION.                                          
028000                                                                          
028100*                                                                         
028200***  FOR MSG-KOM-AREA                                                     
028300*                                                                         
028400     MOVE 'W1I11601'            TO MSG-KOM-IDCPYTXT                       
028500*                                                                         
028600***  FOR MSG-IO-AREA                                                      
028700*                                                                         
028800     COMPUTE MSG-KVLL  = LENGTH OF 1116-MID-W1I11601 + 17                 
028900     MOVE LOW-VALUE             TO MSG-KDZ1                               
029000                                   MSG-KDZ2                               
029100     MOVE 'W1T116X'             TO MSG-KDTRANS-1                          
029200     MOVE '1116'                TO MSG-IDTRANS-1                          
029300     MOVE '1'                   TO MSG-KDMFSFOR-1                         
029400     .                                                                    
029500     EJECT                                                                
029600                                                                          
029700 C-CREATE-UPD-TRANS SECTION.                                              
029800                                                                          
029900***  PART SHOULD ALWAYS BE FOUND IN WDK611                                
030000***  VALIDATED IN PREV PGM W11811                                         
030100***  IF PART NOT FOUND PGM SHOULD ABEND                                   
030200*                                                                         
030300     PERFORM S91-INIT-TEMP                                                
030400     MOVE IN-IDARTNR            TO W-IDARTNR                              
030500     PERFORM IMS-GU-WDK60111                                              
030600     PERFORM CA-VALIDATE-UPD-TRANS                                        
030700     .                                                                    
030800     EJECT                                                                
030900                                                                          
031000 CA-VALIDATE-UPD-TRANS SECTION.                                           
031100                                                                          
031200***  CALLS PGM W10117 THROUGH DISPACTHER                                  
031300***  MOVE  W11811 COPYBOOK TO W10117 INPUT COPYBOOK                       
031400***  WE FIRST LOAD MID-COPYBOOK WITH ALL + VALUES                         
031500                                                                          
031600     INITIALIZE MSG-IO-AREA                                               
031700     MOVE ALL '+'               TO 1117-MID-W1I11701                      
031800     PERFORM CAA-CREATE-HEADER-1117                                       
031900*                                                                         
032000     MOVE IN-IDARTNR            TO 1117-MID-IDARTNR-IN                    
032100                                   1117-MID-IDARTNR-UT                    
032200*                                                                         
032300     MOVE CLAG-IDBERED          TO WS-IDBERED                             
032400     IF IN-IDBERED  = WS-IDBERED                                          
032500        CONTINUE                                                          
032600     ELSE                                                                 
032700        MOVE IN-IDBERED         TO 1117-MID-IDBERED                       
032800        MOVE JA                 TO SW-UPDATE                              
032900     END-IF                                                               
033000*                                                                         
033100     MOVE ART-KDPRODSL          TO WS-KDPRODSL                            
033200     IF IN-KDPRODSL = WS-KDPRODSL                                         
033300        CONTINUE                                                          
033400     ELSE                                                                 
033500        MOVE IN-KDPRODSL        TO 1117-MID-KDPRODSL                      
033600        MOVE JA                 TO SW-UPDATE                              
033700     END-IF                                                               
033800*                                                                         
033900     IF IN-KDSORT   = ART-KDSORT                                          
034000        CONTINUE                                                          
034100     ELSE                                                                 
034200        MOVE IN-KDSORT          TO 1117-MID-KDSORT                        
034300        MOVE JA                 TO SW-UPDATE                              
034400     END-IF                                                               
034500*                                                                         
034600     IF IN-IDPTYP = 'EXU' OR 'EXS'                                        
034700        IF CLAG-IDPROENH (1)  = ZERO    AND                               
034800           IN-IDPROENH        > SPACES                                    
034900           MOVE IN-IDPROENH     TO 1117-MID-IDPROENH (1)                  
035000           MOVE JA              TO SW-UPDATE                              
035100        END-IF                                                            
035200     ELSE                                                                 
035300        IF IN-IDPROENH = CLAG-IDPROENH (1)                                
035400           CONTINUE                                                       
035500        ELSE                                                              
035600           MOVE IN-IDPROENH     TO 1117-MID-IDPROENH (1)                  
035700           MOVE JA              TO SW-UPDATE                              
035800        END-IF                                                            
035900     END-IF                                                               
036000*                                                                         
036100     IF IN-KDUART   = CLAG-KDUART                                         
036200        CONTINUE                                                          
036300     ELSE                                                                 
036400        MOVE IN-KDUART          TO 1117-MID-KDUART                        
036500        MOVE JA                 TO SW-UPDATE                              
036600     END-IF                                                               
036700*                                                                         
036800     IF IN-IDPROJ   = CLAG-IDPROJ                                         
036900        CONTINUE                                                          
037000     ELSE                                                                 
037100        MOVE IN-IDPROJ          TO 1117-MID-IDPROJ                        
037200        MOVE JA                 TO SW-UPDATE                              
037300     END-IF                                                               
037400*                                                                         
037500     IF IN-FLLSRDEL  = CLAG-FLLSRDEL                                      
037600        CONTINUE                                                          
037700     ELSE                                                                 
037800        MOVE IN-FLLSRDEL        TO 1117-MID-FLLSRDEL                      
037900        MOVE JA                 TO SW-UPDATE                              
038000     END-IF                                                               
038100*                                                                         
038200     MOVE CLAG-KDBPSR           TO WS-KDBPSR                              
038300     IF IN-KDBPSR   = WS-KDBPSR                                           
038400        CONTINUE                                                          
038500     ELSE                                                                 
038600        MOVE IN-KDBPSR          TO 1117-MID-KDBPSR                        
038700        MOVE JA                 TO SW-UPDATE                              
038800     END-IF                                                               
038900*                                                                         
039000     MOVE ART-IDFKNGRP          TO WS-IDFKNGRP                            
039100     IF IN-IDFKNGRP  = WS-IDFKNGRP                                        
039200        CONTINUE                                                          
039300     ELSE                                                                 
039400        MOVE IN-IDFKNGRP        TO 1117-MID-IDFKNGRP                      
039500        MOVE JA                 TO SW-UPDATE                              
039600     END-IF                                                               
039700*                                                                         
039800     IF IN-IDPROJUP  = CLAG-IDPROJUP                                      
039900        CONTINUE                                                          
040000     ELSE                                                                 
040100        MOVE IN-IDPROJUP        TO 1117-MID-IDPROJUP                      
040200        MOVE JA                 TO SW-UPDATE                              
040300     END-IF                                                               
040400*                                                                         
040500     IF IN-IDRITN    = CLAG-IDRITN                                        
040600        CONTINUE                                                          
040700     ELSE                                                                 
040800        MOVE IN-IDRITN          TO 1117-MID-IDRITN                        
040900        MOVE JA                 TO SW-UPDATE                              
041000     END-IF                                                               
041100*                                                                         
041200     IF IN-IDAO      = ART-IDAO (1)                                       
041300        CONTINUE                                                          
041400     ELSE                                                                 
041500        MOVE IN-IDAO            TO 1117-MID-IDAO (1)                      
041600        MOVE JA                 TO SW-UPDATE                              
041700     END-IF                                                               
041800*                                                                         
041900     MOVE ART-TISOP             TO WS-TISOP                               
042000     IF IN-TISOP     = WS-TISOP                                           
042100        CONTINUE                                                          
042200     ELSE                                                                 
042300        MOVE IN-TISOP (1:4)     TO 1117-MID-TISOP                         
042400        MOVE JA                 TO SW-UPDATE                              
042500     END-IF                                                               
042600*                                                                         
042700     IF IN-IDKAT-1   = CLAG-IDKAT (1)                                     
042800        CONTINUE                                                          
042900     ELSE                                                                 
043000        MOVE IN-IDKAT-1         TO 1117-MID-IDKAT (1)                     
043100        MOVE JA                 TO SW-UPDATE                              
043200     END-IF                                                               
043300*                                                                         
043400     IF IN-IDKAT-2   = CLAG-IDKAT (2)                                     
043500        CONTINUE                                                          
043600     ELSE                                                                 
043700        MOVE IN-IDKAT-2         TO 1117-MID-IDKAT (2)                     
043800        MOVE JA                 TO SW-UPDATE                              
043900     END-IF                                                               
044000*                                                                         
044100     IF IN-IDKAT-3   = CLAG-IDKAT (3)                                     
044200        CONTINUE                                                          
044300     ELSE                                                                 
044400        MOVE IN-IDKAT-3         TO 1117-MID-IDKAT (3)                     
044500        MOVE JA                 TO SW-UPDATE                              
044600     END-IF                                                               
044700*                                                                         
044800***  CHECK FIELDS FROM WDD2 (NYPON)                                       
044900*                                                                         
045000     IF IN-IDARTNR-MOTSV NUMERIC                                          
045100        MOVE IN-IDARTNR-MOTSV   TO WS-IDARTNR-MOTSV                       
045200     END-IF                                                               
045300*                                                                         
045400     IF IN-IDPROJK       > SPACES                                         
045500     OR WS-IDARTNR-MOTSV > ZERO                                           
045600        PERFORM IMS-GU-WDD201                                             
045700        IF SEGMENT-FOUND                                                  
045800           IF IN-IDPROJK > SPACES                                         
045900              IF IN-IDPROJK = WDD2-ART-IDPROJK                            
046000                 CONTINUE                                                 
046100              ELSE                                                        
046200                 MOVE IN-IDPROJK                                          
046300                                TO 1117-MID-IDPROJK                       
046400                 MOVE JA        TO SW-UPDATE                              
046500              END-IF                                                      
046600           END-IF                                                         
046700*                                                                         
046800           IF WS-IDARTNR-MOTSV > ZERO                                     
046900              IF WS-IDARTNR-MOTSV = WDD2-ART-IDARTNR-MOTSV                
047000                 CONTINUE                                                 
047100              ELSE                                                        
047200                 MOVE WS-IDARTNR-MOTSV                                    
047300                                TO 1117-MID-IDARTNR-MOTSV                 
047400                 MOVE JA        TO SW-UPDATE                              
047500              END-IF                                                      
047600           END-IF                                                         
047700        END-IF                                                            
047800     END-IF                                                               
047900*                                                                         
048000***  BELOW HARDCODED IN W11810                                            
048100*                                                                         
048200     MOVE IN-FLPISK             TO 1117-MID-FLPISK                        
048300     .                                                                    
048400     EJECT                                                                
048500                                                                          
048600 CAA-CREATE-HEADER-1117  SECTION.                                         
048700                                                                          
048800***  FOR MSG-KON-AREA                                                     
048900*                                                                         
049000     MOVE 'W1I11701'            TO MSG-KOM-IDCPYTXT                       
049100*                                                                         
049200***  FOR MSG-IO-AREA                                                      
049300*                                                                         
049400     COMPUTE MSG-KVLL  = LENGTH OF 1117-MID-W1I11701 + 17                 
049500     MOVE LOW-VALUE             TO MSG-KDZ1                               
049600                                   MSG-KDZ2                               
049700     MOVE 'W1T117X'             TO MSG-KDTRANS-1                          
049800     MOVE '1117'                TO MSG-IDTRANS-1                          
049900     MOVE '1'                   TO MSG-KDMFSFOR-1                         
050000     .                                                                    
050100     EJECT                                                                
050200                                                                          
050300 E-SEND-TRANSACTION SECTION.                                              
050400*    -- INITIALIZE W006KOM FIELDS                                         
050500     ADD +1                 TO MSG-KOM-TIKLOCK                            
050600     CALL W006KOM  USING MSG-PCB                                          
050700                         0693X-PCB                                        
050800                         WDP8-PCB                                         
050900                         MSG-KOM-WMSGKOM                                  
051000                         MSG-IO-AREA                                      
051100                                                                          
051200                                                                          
051300     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
051400        STRING ' ERROR FROM W006KOM. '  MSG-KOM-IDMFSMED                  
051500          DELIMITED BY SIZE  INTO ERROR-TEXT-STR                          
051600        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
051700     END-IF                                                               
051800     ADD +1                 TO CHKP-ANT                                   
051900                                                                          
052000     IF CHKP-ANT > CHKP-MAX                                               
052100        PERFORM IMS-CHECKPOINT                                            
052200        MOVE +0             TO CHKP-ANT                                   
052300        ADD +1              TO MSG-KOM-TIKLOCK                            
052400     END-IF                                                               
052500                                                                          
052600      .                                                                   
052700      EJECT                                                               
052800 Z-FINIT SECTION.                                                         
052900                                                                          
053000                                                                          
053100     CLOSE W11811                                                         
053200     SKIP2                                                                
053300     MOVE 'S' TO POSTSUM-OPKOD                                            
053400     CALL POSTSUM USING POSTSUM-PARM                                      
053500     .                                                                    
053600     EJECT                                                                
053700 S01-READ-W11811  SECTION.                                                
053800     SKIP2                                                                
053900     READ W11811 INTO IN-AREA                                             
054000     AT END                                                               
054100        MOVE HIGH-VALUE TO IN-AREA                                        
054200        SET END-OF-W11811 TO TRUE                                         
054300                                                                          
054400     NOT AT END                                                           
054500        MOVE NOO      TO W11811-EMPTY-SW                                  
054600        MOVE 'W11811' TO POSTSUM-FDNAMN                                   
054700        MOVE 'W11811D1' TO POSTSUM-DDNAMN2                                
054800        MOVE 'IN'      TO POSTSUM-TRANSTYP                                
054900        CALL POSTSUM USING POSTSUM-PARM                                   
055000                                                                          
055100     END-READ                                                             
055200     .                                                                    
055300     EJECT                                                                
055400 S91-INIT-TEMP SECTION.                                                   
055500                                                                          
055600     INITIALIZE    WS-IDBERED                                             
055700                   WS-KDPRODSL                                            
055800                   WS-KDYTBEH                                             
055900                   WS-KDFARLIG                                            
056000                   WS-KDBPSR                                              
056100                   WS-IDFKNGRP                                            
056200                   WS-TISOP                                               
056300                   WS-IDARTNR-MOTSV                                       
056400                                                                          
056500     MOVE NEJ   TO SW-UPDATE                                              
056600     .                                                                    
056700     EJECT                                                                
056800****************************************************************          
056900***                    IMS SECTION                                        
057000****************************************************************          
057100*                                                                         
057200 IMS-RESTART SECTION.                                                     
057300     SKIP2                                                                
057400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
057500     MOVE '  ' TO GOOD-STATUSCODES                                        
057600     CALL CBLTDLI USING XRST MSG-PCB                                      
057700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
057800                        CHKP-AREA-LENGTH CHKP-AREA                        
057900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
058000     PERFORM IMS-STATUSCHECK                                              
058100     .                                                                    
058200     SKIP3                                                                
058300 IMS-CHECKPOINT SECTION.                                                  
058400     SKIP2                                                                
058500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
058600     MOVE '  XD' TO GOOD-STATUSCODES                                      
058700     CALL CBLTDLI USING CHKP MSG-PCB                                      
058800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
058900                        CHKP-AREA-LENGTH CHKP-AREA                        
059000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
059100     PERFORM IMS-STATUSCHECK                                              
059200                                                                          
059300     IF IMS-NOT-OK                                                        
059400       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
059500                                           TO ERROR-TEXT-STR              
059600       DISPLAY ERROR-TEXT                                                 
059700       CALL FELLOG                                                        
059800     END-IF                                                               
059900     .                                                                    
060000     EJECT                                                                
060100 IMS-GU-WDK60111 SECTION.                                                 
060200                                                                          
060300     STRING 'WDK601  *D(IDARTNR  =' W-IDARTNR-X ')'                       
060400          DELIMITED BY SIZE INTO SSA1                                     
060500     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
060600          DELIMITED BY SIZE INTO SSA2                                     
060700     MOVE '  '              TO GOOD-STATUSCODES                           
060800     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK60111 SSA1 SSA2            
060900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
061000     PERFORM IMS-STATUSCHECK                                              
061100     .                                                                    
061200     EJECT                                                                
061300 IMS-GU-WDD201   SECTION.                                                 
061400                                                                          
061500     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
061600            DELIMITED BY SIZE INTO SSA1                                   
061700     MOVE '  GE'   TO GOOD-STATUSCODES                                    
061800     CALL CBLTDLI USING GU WDD2-PCB DLI-IO-WDD201 SSA1                    
061900     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
062000     PERFORM IMS-STATUSCHECK                                              
062100     .                                                                    
062200     EJECT                                                                
062300 IMS-STATUSCHECK SECTION.                                                 
062400     SKIP2                                                                
062500     SET STATUS-IX TO 1                                                   
062600     SEARCH GOOD-STATUS                                                   
062700       AT END                                                             
062800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
062900           DELIMITED BY SIZE INTO ERROR-TEXT                              
063000         DISPLAY ERROR-TEXT                                               
063100         CALL FELLOG                                                      
063200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
063300         CONTINUE                                                         
063400     END-SEARCH                                                           
063500     .                                                                    

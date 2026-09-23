000100 ID DIVISION.                                                             
000200* -- CHECKED BY WY2000                                                    
000300 PROGRAM-ID.     WZ01SUB.                                                 
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   01/12/14                                                 
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*      GENERAL SUBPROGRAM FOR RECEIVING AND REPLYING TO                   
001000*      SYNCHRONOUS MESSGES.                                               
001100*      IT IS PART OF THE NEW DISPATCHER FRAMEWORK USED IN                 
001200*      THE CAR PARTS SYSTEMS. CURRENTLY IT HANDLES COMMUNICATION          
001300*      VIA VCOM OR IMS IO-PCB (USED MAINLY BY ITOC)                       
001400*                                                                         
001500*      CALL SYNTAX:                                                       
001600*      (GETARG AND RETURN)                                                
001700*        CALL WZ01SUB USING SUB-CONTROL-AREA                              
001800*                           SUB-KVDLEN                                    
001900*                           MY-DATA-FIELD                                 
002000*                                                                         
002100*      THE PARAMETERS USED IN THE CALLS ARE DEFINED AND                   
002200*      EXPLAINED IN MORE DETAIL IN COPYTEXT WZ01SUB.                      
002300*                                                                         
002400                                                                          
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700                                                                          
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000 77  IDPGM                       PIC X(8)    VALUE 'WZ01SUB'.             
003100 77  YES                         PIC X       VALUE 'J'.                   
003200 77  NOO                         PIC X       VALUE 'N'.                   
003300                                                                          
003400 77  OK-SWITCH                   PIC X       VALUE 'J'.                   
003500 88  ALL-OK                                  VALUE 'J'.                   
003600 88  SOME-ERROR                              VALUE 'N'.                   
003700                                                                          
003800*    -- EMULATION OF EOF - IF LENGTH OF LAST RESULT IS < 9999             
003900*    -- THEN THIS WAS THE LAST RECORD                                     
004000*    -- (SINCE YOU CAN NOT READ MORE MESSAGES THAN WAS ACTUALLY           
004100*    -- SENT WHEN USING VCOM, WE MUST TRY TO GUESS IF THERE ARE           
004200*    -- ANY MORE MESSAGES TO BE READ BY EXAMINING THE LENGTH OF           
004300*    -- THE LAST RECORD).                                                 
004400 77  RECEIVE-EOF-SWITCH          PIC S9(9)   BINARY VALUE ZERO.           
004500   88 NO-MORE-TO-RECEIVE                    VALUE ZERO THRU 9998.         
004600   88 MORE-TO-RECEIVE                       VALUE 9999.                   
004700                                                                          
004800*    -- REMEMBER IF A VCOM CONNECTION HAS BEEN ESTABLISHED                
004900 01  VCOM-CONNECTION-SWITCH      PIC X       VALUE 'N'.                   
005000   88 VCOM-CONNECTION-EXISTS                 VALUE 'J'.                   
005100   88 VCOM-NO-CONNECTION                     VALUE 'N'.                   
005200                                                                          
005300                                                                          
005400 01  RC-DISPLAY                  PIC 9(9).                                
005500                                                                          
005600 01  FILLER                      PIC X(16)   VALUE 'ERROR-TEXT:'.         
005700 01  ERROR-TEXT                  PIC X(80).                               
005800                                                                          
005900*    -- FIELDS USED WHEN MERGES DATA THAT HAS BEEN CUT UP INTO            
006000*    -- PIECES OF TRANSMITTABLE SIZE (10K FOR VCOM)                       
006100 01  WLEN                        PIC S9(9)   BINARY.                      
006200 01  WSTART                      PIC S9(9)   BINARY.                      
006300 01  WFROM-POS                   PIC S9(9)   BINARY.                      
006400 01  W-DISP1                     PIC Z(8)9.                               
006500 01  W-DISP2                     PIC Z(8)9.                               
006600                                                                          
006700 01  WSEGMPREFIX                 PIC X(8).                                
006800 01  WDATA                       PIC X(32768).                            
006900                                                                          
007000*    -- MAX ALLOWED LENGTH SPECIFIED IN THE GETARG CALL                   
007100 01  MAX-RESULT-LEN              PIC S9(9)   BINARY.                      
007200                                                                          
007300*    -- INTERNAL COM TYPE CODE USED INSTEAD OF ATAB VALUE                 
007400 01  WKDCOMTYPE                  PIC X(10)   VALUE 'NO-VALUE'.            
007500                                                                          
007600*    -- DATA EXTRACTED FROM ATAB                                          
007700 01  WEXPEDITER                  PIC X(8).                                
007800 01  WMODNAME                    PIC X(8).                                
007900 01  WINITIATOR                  PIC X(8).                                
008000 01  WTIMEOUT                    PIC 9(5).                                
008100                                                                          
008200                                                                          
008300 01  DYNAMIC-SUBPROGRAMS.                                                 
008400   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
008500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
008600   03  WZ01ATAB                  PIC X(8)    VALUE 'WZ01ATAB'.            
008700   03  CSCONR                    PIC X(8)    VALUE 'CSCONR  '.            
008800   03  CSRECV                    PIC X(8)    VALUE 'CSRECV  '.            
008900   03  CSSEND                    PIC X(8)    VALUE 'CSSEND  '.            
009000   03  CSRLSE                    PIC X(8)    VALUE 'CSRLSE  '.            
009100   03  AIBTDLI                   PIC X(8)    VALUE 'AIBTDLI '.            
009200                                                                          
009300                                                                          
009400*    -- PARAMETERS TO ABEND                                               
009500                                                                          
009600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   BINARY VALUE +16.            
009700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   BINARY VALUE +1000.          
009800                                                                          
009900*    -- PARAMETERS TO WZ01ATAB                                            
010000                                                                          
010100*01  -COPY WZ01ATAB                                                       
010200                                                                          
010300 01  SMALL-LETTERS              PIC X(31)  VALUE                          
010400     'abcdefghijklmnopqrstuvwxyzåäöüé'.                                   
010500 01  CAPS-LETTERS               PIC X(31)  VALUE                          
010600     'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÜÉ'.                                   
010700                                                                          
010800     EJECT                                                                
010900 01  VCOM-AREA-START             PIC X(16)   VALUE                        
011000                                 'VCOM-AREA-START '.                      
011100*01  -COPY W0028 -PRE VCOM-                                               
011200                                                                          
011300*    -- 9990 = CA 9999 - 8                                                
011400 01  MAX-VCOM-DATA-LENGTH        PIC S9(9)   BINARY VALUE +9990.          
011500                                                                          
011600     EJECT                                                                
011700*    -- IMS FUNKTIONSKODER                                                
011800*    -COPY W0003                                                          
011900                                                                          
012000 01  STATUS-WS                   PIC XX.                                  
012100     88 SEGMENT-EXISTS           VALUE SPACE.                             
012200                                                                          
012300 01  VALID-STATUS-CODES.                                                  
012400   03  VALID-STATUS OCCURS 5 INDEXED BY STATUS-IX                         
012500                                 PIC XX.                                  
012600                                                                          
012700     EJECT                                                                
012800 01  AIB-AREA-START              PIC X(16)   VALUE                        
012900                                 'AIB-AREA-START  '.                      
013000                                                                          
013100*    -COPY W0031                                                          
013200                                                                          
013300     EJECT                                                                
013400 01  IMSMSG-AREA-START            PIC X(16)   VALUE                       
013500                                 'IMSMSG-AREA-STAR'.                      
013600                                                                          
013700*    -- AREA FOR SAVING THE DATA RECEIVED OR SEND                         
013800*    -- AS AN IMS MESSAGE.                                                
013900 01 IMSMSG-IO-AREA.                                                       
014000   03  IMSMSG-KVLL               PIC S9(4)   BINARY.                      
014100   03  IMSMSG-KDZZ               PIC XX.                                  
014200*   -- MAX LENGTH OF DATA THAT CAN BE RECEIVED = 32KB                     
014300   03  IMSMSG-IO-DATA            PIC X(32764).                            
014400                                                                          
014500*   -- MAX LENGTH OF DATA TO BE INSERTED TO IO-PCB                        
014600*   -- IF RESULT IS LONGER IT WILL BE SPLIT INTO MANY SEGMENTS            
014700 01 IMSMSG-MAX-DATA-LENGTH       PIC S9(9)   BINARY VALUE +27000.         
014800     EJECT                                                                
014900 LINKAGE SECTION.                                                         
015000                                                                          
015100*01 -COPY WZ01SUB1                                                        
015200     EJECT                                                                
015300 01  SUB-KVDLEN                  PIC S9(9) BINARY.                        
015400                                                                          
015500 01  SUB-DATA                    PIC X(4000000).                          
015600     EJECT                                                                
015700*    -COPY W0009  -PRE ALT-                                               
015800                                                                          
015900     EJECT                                                                
016000 PROCEDURE DIVISION USING                                                 
016100                      SUB-CONTROL-AREA                                    
016200                      SUB-KVDLEN                                          
016300                      SUB-DATA                                            
016400                     .                                                    
016500 MAIN SECTION.                                                            
016600                                                                          
016700     PERFORM A-INIT                                                       
016800                                                                          
016900     IF ALL-OK                                                            
017000       EVALUATE WKDCOMTYPE ALSO SUB-KDFUNC                                
017100         WHEN  ANY      ALSO  'GETARG'                                    
017200           IF WKDCOMTYPE = 'MIXED' OR 'IMSP2P'                            
017300*            -- TRY READING DATA VIA IO-PCB                               
017400             PERFORM C1-IMSP2P-INIT                                       
017500             IF SUB-KDRC = 0 AND WLEN > 8                                 
017600*            -- WHEN PROGRAM IS TRIGGERED VIA VCOM THE                    
017700*            -- MID-DATA FETCHED VIA THE IO-PCB CONTAINS THE              
017800*            -- EXPEDITER NAME. THIS DATA SHOULD BE IGNORED AND           
017900*            -- DATA SHOULD BE READ FROM VCOM INSTEAD                     
018000             AND WDATA(9:8) NOT = WEXPEDITER                              
018100               MOVE 'IMSP2P' TO WKDCOMTYPE                                
018200               PERFORM C2-IMSP2P-GET-FROM-IMS                             
018300             END-IF                                                       
018400           END-IF                                                         
018500           IF WKDCOMTYPE = 'MIXED' OR 'VCOM'                              
018600*            -- TRY VCOM                                                  
018700             MOVE ZERO     TO VCOM-RC                                     
018800             IF VCOM-NO-CONNECTION                                        
018900               PERFORM B1-VCOM-CONNECT                                    
019000             END-IF                                                       
019100             IF VCOM-RC = ZERO                                            
019200               MOVE 'VCOM'   TO WKDCOMTYPE                                
019300               SET VCOM-CONNECTION-EXISTS TO TRUE                         
019400               PERFORM B2-VCOM-RECV                                       
019500               IF VCOM-RC = 35                                            
019600*                -- CLEAN UP AFTER DISCONNECT BY PARTNER                  
019700                 PERFORM B4-VCOM-RELEASE                                  
019800                 SET VCOM-NO-CONNECTION TO TRUE                           
019900               END-IF                                                     
020000             ELSE                                                         
020100*              -- NO DATA ANYWHERE - SET IMS COMTYPE TO                   
020200*              -- MAKE RETURN POSSIBLE                                    
020300               MOVE 'IMSP2P'  TO WKDCOMTYPE                               
020400             END-IF                                                       
020500           END-IF                                                         
020600                                                                          
020700         WHEN  'IMSP2P'  ALSO  'OPEN'                                     
020800*         -- DUMMY CALL TO MAKE POSSIBLE REPLY WITHOUT GETARG             
020900           CONTINUE                                                       
021000                                                                          
021100         WHEN  'VCOM'    ALSO  'RETURN'                                   
021200           PERFORM B3-VCOM-SEND                                           
021300           PERFORM B4-VCOM-RELEASE                                        
021400           SET VCOM-NO-CONNECTION TO TRUE                                 
021500           MOVE 'NO-VALUE' TO WKDCOMTYPE                                  
021600                                                                          
021700         WHEN  'IMSP2P'  ALSO  'RETURN'                                   
021800           PERFORM C3-IMSP2P-INSERT-MESSAGE                               
021900           PERFORM C4-IMSP2P-CLOSE                                        
022000           MOVE 'NO-VALUE' TO WKDCOMTYPE                                  
022100                                                                          
022200         WHEN  'NO-VALUE'    ALSO  'RETURN'                               
022300*          -- NO GETARG BEFORE RETURN                                     
022400           MOVE 20 TO SUB-KDRC                                            
022500                                                                          
022600         WHEN OTHER                                                       
022700           MOVE 'INVALID COMBINATION IN EVALUATE'                         
022800                TO ERROR-TEXT                                             
022900           IF SUB-KDFUNC NOT = 'GETARG' AND 'RETURN'                      
023000             STRING 'INVALID FUNCTION CODE ' SUB-KDFUNC                   
023100             DELIMITED BY SIZE INTO ERROR-TEXT                            
023200           END-IF                                                         
023300           IF WKDCOMTYPE NOT =  'VCOM' AND 'IMSP2P' AND 'MIXED'           
023400             STRING 'INVALID COMTYPE IN ATAB ' WKDCOMTYPE                 
023500             DELIMITED BY SIZE INTO ERROR-TEXT                            
023600           END-IF                                                         
023700           CALL FELLOG                                                    
023800                                                                          
023900       END-EVALUATE                                                       
024000     END-IF                                                               
024100                                                                          
024200     IF SUB-KDRC = 0                                                      
024300       MOVE ZERO TO RETURN-CODE                                           
024400     ELSE                                                                 
024500       MOVE 12 TO RETURN-CODE                                             
024600     END-IF                                                               
024700                                                                          
024800     GOBACK                                                               
024900     .                                                                    
025000                                                                          
025100     EJECT                                                                
025200 A-INIT SECTION.                                                          
025300                                                                          
025400     SET ALL-OK TO TRUE                                                   
025500                                                                          
025600                                                                          
025700     IF SUB-KDFUNC = 'GETARG' OR 'OPEN'                                   
025800       PERFORM AA-SEARCH-ATAB                                             
025900                                                                          
026000       IF ALL-OK                                                          
026100*        -- SAVE DATA FOR LATER USE - VCOM EXPEDITER                      
026200         MOVE ZERO TO TALLY                                               
026300         INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                       
026400                 FOR CHARACTERS BEFORE INITIAL 'EXP:'                     
026500         MOVE SPACE TO WEXPEDITER                                         
026600         IF TALLY < LENGTH OF ATAB-ADDISPINT-RECV                         
026700           UNSTRING ATAB-ADDISPINT-RECV(TALLY + 5:)                       
026800           DELIMITED BY ';'                                               
026900           INTO WEXPEDITER                                                
027000         ELSE                                                             
027100           MOVE SPACE TO WEXPEDITER                                       
027200         END-IF                                                           
027300                                                                          
027400*        -- MOD NAME FOR RETURN MESSAGE                                   
027500         MOVE ZERO TO TALLY                                               
027600         INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                       
027700                 FOR CHARACTERS BEFORE INITIAL 'MOD:'                     
027800         MOVE SPACE TO WMODNAME                                           
027900         IF TALLY < LENGTH OF ATAB-ADDISPINT-RECV                         
028000           UNSTRING ATAB-ADDISPINT-RECV(TALLY + 5:)                       
028100           DELIMITED BY ';'                                               
028200           INTO WMODNAME                                                  
028300         ELSE                                                             
028400           MOVE SPACE TO WMODNAME                                         
028500         END-IF                                                           
028600       END-IF                                                             
028700                                                                          
028800*      -- IF COMTYPE IS IMSP2P AND AN EXPEDITER                           
028900*      -- IS ALSO SPECIFIED, TRY BOTH IMSP2P AND VCOM                     
029000       MOVE ATAB-KDCOMTYPE TO WKDCOMTYPE                                  
029100       IF WKDCOMTYPE = 'IMSP2P' AND WEXPEDITER NOT = SPACE                
029200         MOVE 'MIXED' TO WKDCOMTYPE                                       
029300       END-IF                                                             
029400                                                                          
029500*      -- SAVE MAX ALLOWED LENGTH OF RESULT                               
029600*      -- (IT WILL BE OVERLAID BY ACTUAL LENGTH)                          
029700       MOVE SUB-KVDLEN TO MAX-RESULT-LEN                                  
029800     END-IF                                                               
029900     .                                                                    
030000                                                                          
030100     EJECT                                                                
030200 AA-SEARCH-ATAB SECTION.                                                  
030300                                                                          
030400     MOVE SUB-ADDISPABS TO ATAB-ADDISPABS                                 
030500     INSPECT ATAB-ADDISPABS CONVERTING                                    
030600             SMALL-LETTERS TO CAPS-LETTERS                                
030700     CALL WZ01ATAB USING ATAB-WZ01ATAB                                    
030800                                                                          
030900     IF RETURN-CODE > ZERO                                                
031000*      -- ADDRESS NOT FOUND IN ATAB                                       
031100       MOVE 10 TO SUB-KDRC                                                
031200       SET SOME-ERROR TO TRUE                                             
031300     END-IF                                                               
031400     .                                                                    
031500                                                                          
031600     EJECT                                                                
031700 B1-VCOM-CONNECT SECTION.                                                 
031800                                                                          
031900     MOVE WEXPEDITER     TO VCOM-EXPEDITER                                
032000                                                                          
032100*    -- DEFAULT VALUE FOR TIMEOUT                                         
032200     MOVE 5              TO VCOM-TIMEOUT                                  
032300*    -- CHECK IF TIMEOUT IS SPECIFIED IN ATAB                             
032400     MOVE ZERO TO TALLY                                                   
032500     INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                           
032600             FOR ALL 'TOUT:'                                              
032700     IF TALLY > 0                                                         
032800*      -- YES, EXTRACT THE TIMEOUT VALUE                                  
032900       MOVE ZERO TO TALLY                                                 
033000       MOVE ZERO TO WTIMEOUT                                              
033100       INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                         
033200               FOR CHARACTERS BEFORE INITIAL 'TOUT:'                      
033300       UNSTRING ATAB-ADDISPINT-RECV(TALLY + 6:)                           
033400                DELIMITED BY ';'  INTO WTIMEOUT                           
033500       MOVE WTIMEOUT TO VCOM-TIMEOUT                                      
033600     END-IF                                                               
033700                                                                          
033800     MOVE ZERO TO TALLY                                                   
033900     INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                           
034000             FOR CHARACTERS BEFORE INITIAL 'INIT:'                        
034100     IF TALLY < LENGTH OF ATAB-ADDISPINT-RECV                             
034200*      -- USE SPECIFIED INITIATOR                                         
034300       MOVE SPACE TO VCOM-INITIATOR                                       
034400       UNSTRING ATAB-ADDISPINT-RECV(TALLY + 6:)  DELIMITED BY ';'         
034500       INTO VCOM-INITIATOR                                                
034600     ELSE                                                                 
034700*      --NO INITIATOR SPECIFIED, USE INIT41 (SW EBCDIC) AS DEFAULT        
034800       MOVE 'INIT41  '   TO VCOM-INITIATOR                                
034900     END-IF                                                               
035000                                                                          
035100     CALL CSCONR USING VCOM-RC                                            
035200                       VCOM-CONVID                                        
035300                       VCOM-SECUR                                         
035400                       VCOM-TIMEOUT                                       
035500                       VCOM-SENDERTAG                                     
035600                       VCOM-EXPEDITER                                     
035700                       VCOM-INITIATOR                                     
035800                                                                          
035900     IF VCOM-RC NOT = ZERO                                                
036000     AND VCOM-RC NOT = 31                                                 
036100     AND VCOM-RC NOT = 41                                                 
036200     AND VCOM-RC NOT = 141                                                
036300       MOVE VCOM-RC TO RC-DISPLAY                                         
036400       STRING 'RC FROM CSCONR ' RC-DISPLAY                                
036500              ' ' VCOM-CONVID                                             
036600              ' ' VCOM-EXPEDITER                                          
036700          DELIMITED BY SIZE                                               
036800          INTO ERROR-TEXT                                                 
036900       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
037000     END-IF                                                               
037100                                                                          
037200     IF VCOM-RC = 41 OR 31 OR 141                                         
037300*      -- NO DATA TO RECEIVE                                              
037400       MOVE 20 TO SUB-KDRC                                                
037500       MOVE VCOM-RC TO RC-DISPLAY                                         
037600       IF VCOM-RC = 41                                                    
037700         STRING                                                           
037800              'NO DATA TO RECEIVE FROM VCOM. RC='                         
037900              RC-DISPLAY                                                  
038000              DELIMITED BY SIZE                                           
038100              INTO ERROR-TEXT                                             
038200         DISPLAY ERROR-TEXT                                               
038300       ELSE                                                               
038400         STRING                                                           
038500              'VCOM CONNECT TIMEOUT. RC='                                 
038600              RC-DISPLAY                                                  
038700              DELIMITED BY SIZE                                           
038800              INTO ERROR-TEXT                                             
038900         DISPLAY ERROR-TEXT                                               
039000       END-IF                                                             
039100     END-IF                                                               
039200     .                                                                    
039300                                                                          
039400     EJECT                                                                
039500 B2-VCOM-RECV    SECTION.                                                 
039600                                                                          
039700*    -- THE DATA MAY BE SPLIT INTO SEGEMENT OF ABOUT 10K EACH.            
039800*    -- THE FIRST SEGMENT IS PURE DATA, BUT THE                           
039900*    -- FOLLOWING SEGMENTS MAY BE PREFIXED BY AN OPTIONAL                 
040000*    -- CONTINUATION PREFIX - 'WZ01CONT'                                  
040100                                                                          
040200*    -- TOTAL LENGTH OF DATA IS INITIALLY ZERO                            
040300     MOVE ZERO TO SUB-KVDLEN                                              
040400*    -- PUT DATA AT BEGINNING OF RECEVING DATA FIELD                      
040500     MOVE 1    TO WSTART                                                  
040600*    -- TAKE WHOLE RECORD FIRST TIME                                      
040700     MOVE 1    TO WFROM-POS                                               
040800                                                                          
040900*    -- FETCH AND PROCESS FIRST SEGMENT                                   
041000     PERFORM B2A-RECV-SEGMENT                                             
041100     PERFORM S01-MOVE-DATA                                                
041200*    -- FETCH AND PROCESS MORE SEGMENTS IF THEY EXIST                     
041300     PERFORM UNTIL VCOM-RC NOT = ZERO                                     
041400             OR NO-MORE-TO-RECEIVE                                        
041500                                                                          
041600       PERFORM B2A-RECV-SEGMENT                                           
041700*      -- SKIP FIRST 8 POSITIONS IF CONTINUATION MARKER                   
041800*      -- (THIS MARKER IS CURRENTLY ONLY USED BY WZ01SEND -               
041900*      -- NOT WZ01CALL OR JAVA CALLDISPATCHER)                            
042000       IF WDATA(1:8) = 'WZ01CONT'                                         
042100         MOVE 9 TO WFROM-POS                                              
042200       ELSE                                                               
042300         MOVE 1 TO WFROM-POS                                              
042400       END-IF                                                             
042500       PERFORM S01-MOVE-DATA                                              
042600                                                                          
042700     END-PERFORM                                                          
042800                                                                          
042900     IF WSTART > MAX-RESULT-LEN + 1                                       
043000*      -- TRUNCATION, SOME DATA NOT PROCESSED                             
043100       MOVE 21   TO SUB-KDRC                                              
043200       SUBTRACT 1 FROM WSTART GIVING W-DISP1                              
043300       MOVE MAX-RESULT-LEN TO W-DISP2                                     
043400       STRING 'DATA TRUNCATION. RECEIVED LENGTH: ' W-DISP1                
043500              '. MAX ALLOWED: ' W-DISP2                                   
043600         DELIMITED BY SIZE                                                
043700         INTO ERROR-TEXT                                                  
043800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
043900     END-IF                                                               
044000     .                                                                    
044100                                                                          
044200                                                                          
044300     EJECT                                                                
044400 B2A-RECV-SEGMENT  SECTION.                                               
044500                                                                          
044600     MOVE LENGTH OF VCOM-DATA  TO VCOM-MAXLENGTH                          
044700                                                                          
044800     CALL CSRECV USING VCOM-RC                                            
044900                       VCOM-CONVID                                        
045000                       VCOM-TIMEOUT                                       
045100                       VCOM-MAXLENGTH                                     
045200                       VCOM-ACTLENGTH                                     
045300                       VCOM-DATA                                          
045400                                                                          
045500     IF  VCOM-RC NOT = ZERO AND 35                                        
045600       MOVE VCOM-RC TO RC-DISPLAY                                         
045700       STRING 'RC FROM CSRECV ' RC-DISPLAY                                
045800              ' ' VCOM-CONVID                                             
045900              ' ' VCOM-EXPEDITER                                          
046000          DELIMITED BY SIZE                                               
046100          INTO ERROR-TEXT                                                 
046200       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
046300     END-IF                                                               
046400                                                                          
046500     IF VCOM-RC = 35                                                      
046600*      -- NO DATA, DISCONNECT FROM OTHER SIDE                             
046700       MOVE SPACE TO WDATA                                                
046800       MOVE ZERO  TO WLEN                                                 
046900     ELSE                                                                 
047000*      -- TAKE CARE OF THE RECEIVED DATA                                  
047100       MOVE VCOM-DATA(1:VCOM-ACTLENGTH) TO WDATA                          
047200       MOVE VCOM-ACTLENGTH              TO WLEN                           
047300       MOVE VCOM-ACTLENGTH              TO RECEIVE-EOF-SWITCH             
047400     END-IF                                                               
047500     .                                                                    
047600                                                                          
047700     EJECT                                                                
047800 B3-VCOM-SEND    SECTION.                                                 
047900                                                                          
048000*    -- IF THE DATA IS LONGER THAN CA 10000 BYTES, IT IS SPLIT            
048100*    -- INTO 10K SEGMENTS.                                                
048200                                                                          
048300     MOVE 1                    TO WSTART                                  
048400     MOVE MAX-VCOM-DATA-LENGTH TO WLEN                                    
048500     MOVE SPACE                TO WSEGMPREFIX                             
048600     PERFORM UNTIL WSTART > SUB-KVDLEN                                    
048700                                                                          
048800*      -- DON'T MOVE MORE THAN WHAT REMAINS OF THE DATA                   
048900       IF WSTART + WLEN > SUB-KVDLEN + 1                                  
049000         COMPUTE WLEN = SUB-KVDLEN - WSTART + 1                           
049100       END-IF                                                             
049200*      -- DON'T MOVE MORE THAN WHAT IS ALLOWED                            
049300       IF WSTART + WLEN > LENGTH OF SUB-DATA                              
049400         COMPUTE WLEN = LENGTH OF SUB-DATA - WSTART                       
049500       END-IF                                                             
049600                                                                          
049700       MOVE SUB-DATA(WSTART:WLEN) TO WDATA                                
049800       PERFORM B3A-SEND-SEGMENT                                           
049900                                                                          
050000**     MOVE 'WZ01CONT' TO WSEGMPREFIX                                     
050100       ADD WLEN TO  WSTART                                                
050200     END-PERFORM                                                          
050300     .                                                                    
050400                                                                          
050500                                                                          
050600     SKIP3                                                                
050700 B3A-SEND-SEGMENT  SECTION.                                               
050800                                                                          
050900     MOVE SPACE TO VCOM-DATA                                              
051000     STRING                                                               
051100       WSEGMPREFIX  DELIMITED BY SPACE                                    
051200       WDATA(WSTART:WLEN) DELIMITED BY SIZE                               
051300      INTO VCOM-DATA                                                      
051400                                                                          
051500     IF WSEGMPREFIX = SPACE                                               
051600       MOVE WLEN TO VCOM-ACTLENGTH                                        
051700     ELSE                                                                 
051800       COMPUTE VCOM-ACTLENGTH = WLEN + 8                                  
051900     END-IF                                                               
052000                                                                          
052100     CALL CSSEND USING VCOM-RC                                            
052200                       VCOM-CONVID                                        
052300                       VCOM-ACTLENGTH                                     
052400                       VCOM-DATA                                          
052500                                                                          
052600     IF VCOM-RC NOT = ZERO                                                
052700       MOVE VCOM-RC TO RC-DISPLAY                                         
052800       STRING 'RC FROM CSSEND ' RC-DISPLAY                                
052900              ' ' VCOM-CONVID                                             
053000              ' ' VCOM-EXPEDITER                                          
053100          DELIMITED BY SIZE                                               
053200          INTO ERROR-TEXT                                                 
053300       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
053400     END-IF                                                               
053500     .                                                                    
053600                                                                          
053700                                                                          
053800     EJECT                                                                
053900 B4-VCOM-RELEASE SECTION.                                                 
054000                                                                          
054100     MOVE ZERO                 TO VCOM-RC                                 
054200                                                                          
054300     CALL CSRLSE USING VCOM-RC                                            
054400                       VCOM-CONVID                                        
054500                                                                          
054600                                                                          
054700     IF VCOM-RC NOT = ZERO                                                
054800       MOVE VCOM-RC TO RC-DISPLAY                                         
054900       STRING 'RC FROM CSRLSE ' RC-DISPLAY                                
055000              ' ' VCOM-CONVID                                             
055100              ' ' VCOM-EXPEDITER                                          
055200          DELIMITED BY SIZE                                               
055300          INTO ERROR-TEXT                                                 
055400       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
055500     END-IF                                                               
055600     .                                                                    
055700                                                                          
055800     EJECT                                                                
055900 C1-IMSP2P-INIT  SECTION.                                                 
056000                                                                          
056100                                                                          
056200*    -- READ AND SAVE FIRST DATA SEGMENT                                  
056300     PERFORM IMS-GET-FIRST-SEGMENT                                        
056400                                                                          
056500     IF WLEN = 0                                                          
056600*      -- NO DATA TO RECEIVE                                              
056700       MOVE 20 TO SUB-KDRC                                                
056800       MOVE ZERO TO SUB-KVDLEN                                            
056900     ELSE                                                                 
057000       MOVE FUNCTION UPPER-CASE (WDATA (1:8)) TO SUB-KDTRANS              
057100     END-IF                                                               
057200     .                                                                    
057300                                                                          
057400     EJECT                                                                
057500 C2-IMSP2P-GET-FROM-IMS  SECTION.                                         
057600                                                                          
057700*    -- THE DATA MAY BE SPLIT INTO SEGMENTS OF ABOUT 28K EACH. THE        
057800*    -- FIRST SEGMENT IS PURE IMS DATA (WITH INITIAL TRANS CODE)          
057900*    -- BUT THE FOLLOWING SEGMENTS MAY OPTIONALLY BE PREFIXED BY A        
058000*    -- CONTINUATION PREFIX" - 'WZ01CONT' WHICH SHOULD BE SKIPPED.        
058100*    -- THE SEQUENCE OF POSSIBLE SEGMENTS LOOKS LIKE THIS:                
058200*    --   TRANS+DATA [, [WZ01CONT+] DATA]...                              
058300                                                                          
058400*    -- TOTAL LENGTH OF DATA IS INITIALLY ZERO                            
058500     MOVE ZERO TO SUB-KVDLEN                                              
058600*    -- PUT DATA AT BEGINNING OF RECEVING DATA FIELD                      
058700     MOVE 1    TO WSTART                                                  
058800                                                                          
058900     IF WLEN - 8 > MAX-RESULT-LEN AND                                     
059000        WKDCOMTYPE = 'IMSP2P'                                             
059100        COMPUTE WLEN = MAX-RESULT-LEN + 8                                 
059200     END-IF                                                               
059300                                                                          
059400     IF WLEN - 8 > MAX-RESULT-LEN                                         
059500                                                                          
059600*      -- TRUNCATION, SOME DATA NOT PROCESSED                             
059700       MOVE 21   TO SUB-KDRC                                              
059800       ADD WLEN 8 GIVING W-DISP1                                          
059900       MOVE MAX-RESULT-LEN TO W-DISP2                                     
060000       STRING 'DATA TRUNCATION. RECEIVED LENGTH: ' W-DISP1                
060100              '. MAX ALLOWED: ' W-DISP2                                   
060200         DELIMITED BY SIZE                                                
060300         INTO ERROR-TEXT                                                  
060400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
060500     ELSE                                                                 
060600*      -- FIRST USE DATA FROM PREVIOUS CALL                               
060700*      -- (EXCLUDE THE INITIAL TRANSACTION CODE)                          
060800                                                                          
060900       MOVE 9 TO WFROM-POS                                                
061000       PERFORM S01-MOVE-DATA                                              
061100                                                                          
061200*      -- ADD CONTINUATION SEGMENTS (IF THERE ARE ANY)                    
061300       PERFORM IMS-GET-NEXT-SEGMENT                                       
061400       PERFORM UNTIL NOT SEGMENT-EXISTS                                   
061500               OR WSTART > MAX-RESULT-LEN                                 
061600                                                                          
061700*      -- SKIP FIRST 8 POSITIONS IF CONTINUATION MARKER                   
061800*      -- (THIS MARKER IS CURRENTLY ONLY USED BY WZ01SEND -               
061900*      -- NOT WZ01CALL OR JAVA CALLDISPATCHER)                            
062000         IF WDATA(1:8) = 'WZ01CONT' OR '*CONTINU'                         
062100           MOVE 9 TO WFROM-POS                                            
062200         ELSE                                                             
062300           MOVE 1 TO WFROM-POS                                            
062400         END-IF                                                           
062500         PERFORM S01-MOVE-DATA                                            
062600                                                                          
062700         PERFORM IMS-GET-NEXT-SEGMENT                                     
062800       END-PERFORM                                                        
062900                                                                          
063000       IF WSTART > MAX-RESULT-LEN + 1                                     
063100*        -- TRUNCATION, SOME DATA NOT PROCESSED                           
063200         MOVE 21   TO SUB-KDRC                                            
063300         MOVE ZERO TO WLEN                                                
063400         MOVE WSTART TO W-DISP1                                           
063500         ADD MAX-RESULT-LEN 1 GIVING W-DISP2                              
063600         STRING 'DATA TRUNCATION AT POS '                                 
063700         W-DISP1 ' MAX:' W-DISP2                                          
063800           DELIMITED BY SIZE                                              
063900           INTO ERROR-TEXT                                                
064000         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
064100       END-IF                                                             
064200                                                                          
064300     END-IF                                                               
064400     .                                                                    
064500                                                                          
064600     EJECT                                                                
064700 C3-IMSP2P-INSERT-MESSAGE  SECTION.                                       
064800                                                                          
064900     MOVE 1 TO WSTART                                                     
065000     MOVE IMSMSG-MAX-DATA-LENGTH  TO WLEN                                 
065100                                                                          
065200     PERFORM UNTIL WSTART > SUB-KVDLEN                                    
065300                                                                          
065400*      -- DON'T MOVE MORE THAN WHAT REMAINS OF THE DATA                   
065500       IF WSTART + WLEN > SUB-KVDLEN + 1                                  
065600         COMPUTE WLEN = SUB-KVDLEN - WSTART + 1                           
065700       END-IF                                                             
065800*      -- DON'T MOVE MORE THAN WHAT IS ALLOWED                            
065900       IF WSTART + WLEN > LENGTH OF SUB-DATA                              
066000         COMPUTE WLEN = LENGTH OF SUB-DATA - WSTART                       
066100       END-IF                                                             
066200       STRING SUB-DATA(WSTART:WLEN)                                       
066300         DELIMITED BY SIZE                                                
066400         INTO IMSMSG-IO-DATA                                              
066500*      -- LENGTH OF WHOLE MESSAGE                                         
066600       ADD  WLEN  4   GIVING IMSMSG-KVLL                                  
066700       MOVE LOW-VALUE TO     IMSMSG-KDZZ                                  
066800                                                                          
066900       PERFORM IMS-INSERT-MESSAGE                                         
067000                                                                          
067100       ADD WLEN        TO WSTART                                          
067200     END-PERFORM                                                          
067300     .                                                                    
067400                                                                          
067500     EJECT                                                                
067600 C4-IMSP2P-CLOSE  SECTION.                                                
067700                                                                          
067800     CONTINUE                                                             
067900     .                                                                    
068000                                                                          
068100     EJECT                                                                
068200 S01-MOVE-DATA SECTION.                                                   
068300                                                                          
068400*    -- MOVE DATA FROM ONE SEGMENT TO THE RIGHT POSITION                  
068500*    -- IN THE OUTPUT FIELD. DO THIS ONLY IF THERE IS ANY DATA            
068600*    -- TO MOVE, AND IT DOES NOT OVERFLOW THE OUTPUT FIELD.               
068700*    -- THE FIRST 8 POSITIONS MAY CONTAIN A "CONTINUATION PREFIX"         
068800*    -- OR A TRANSACTION CODE THAT SHOULD BE SKIPPED - WFROM-POS          
068900*    -- IS SET TO EITHER 1 OR 9 IN THE CALLING SECTION, DEPENDING         
069000*    -- ON THE SITUATION IN THIS RESPECT.                                 
069100                                                                          
069200     IF WLEN >= WFROM-POS                                                 
069300     AND WSTART + WLEN - WFROM-POS <= MAX-RESULT-LEN                      
069400       COMPUTE WLEN = WLEN + 1 - WFROM-POS                                
069500       MOVE WDATA(WFROM-POS:WLEN) TO SUB-DATA(WSTART:WLEN)                
069600       ADD  WLEN                  TO SUB-KVDLEN                           
069700*      -- PREPARE POINTER FOR NEXT TIME                                   
069800       ADD  WLEN                 TO WSTART                                
069900     ELSE                                                                 
070000*      -- ADJUST WSTART SO IT CAN BE COMPARED WITH MAX-RESULT-LEN         
070100*      -- AND TRIGGER TRUNCATION ERROR - KDRC 21                          
070200       COMPUTE WSTART = WSTART + WLEN - WFROM-POS + 1                     
070300     END-IF                                                               
070400     .                                                                    
070500                                                                          
070600     EJECT                                                                
070700 IMS-GET-FIRST-SEGMENT SECTION.                                           
070800                                                                          
070900     MOVE 128         TO  AIB-LEN                                         
071000     MOVE SPACE       TO  AIB-SUB-FUNCTION                                
071100     MOVE 'IOPCB   '  TO  AIB-PCB-NAME                                    
071200     MOVE LENGTH OF IMSMSG-IO-AREA                                        
071300                      TO AIB-IOAREA-LENGTH                                
071400                                                                          
071500     CALL AIBTDLI USING GU AIB-AREA IMSMSG-IO-AREA                        
071600                                                                          
071700     SET ADDRESS OF ALT-PCB TO AIB-PCB-PTR                                
071800     MOVE ALT-STATUS-CODE   TO STATUS-WS                                  
071900     MOVE '  QC'            TO VALID-STATUS-CODES                         
072000     PERFORM IMS-STATUS-CHECK                                             
072100                                                                          
072200     IF SEGMENT-EXISTS                                                    
072300       COMPUTE WLEN = IMSMSG-KVLL - 4                                     
072400       MOVE IMSMSG-IO-DATA(1:IMSMSG-KVLL - 4) TO WDATA                    
072500     ELSE                                                                 
072600       MOVE ZERO  TO WLEN                                                 
072700       MOVE SPACE TO WDATA                                                
072800     END-IF                                                               
072900     .                                                                    
073000                                                                          
073100     EJECT                                                                
073200 IMS-GET-NEXT-SEGMENT SECTION.                                            
073300                                                                          
073400     MOVE 128         TO  AIB-LEN                                         
073500     MOVE SPACE       TO  AIB-SUB-FUNCTION                                
073600     MOVE 'IOPCB   '  TO  AIB-PCB-NAME                                    
073700     MOVE LENGTH OF IMSMSG-IO-AREA                                        
073800                      TO AIB-IOAREA-LENGTH                                
073900                                                                          
074000     CALL AIBTDLI USING GN AIB-AREA IMSMSG-IO-AREA                        
074100                                                                          
074200     SET ADDRESS OF ALT-PCB TO AIB-PCB-PTR                                
074300     MOVE ALT-STATUS-CODE   TO STATUS-WS                                  
074400     MOVE '  QD'            TO VALID-STATUS-CODES                         
074500     PERFORM IMS-STATUS-CHECK                                             
074600                                                                          
074700     IF SEGMENT-EXISTS                                                    
074800       COMPUTE WLEN = IMSMSG-KVLL - 4                                     
074900       MOVE IMSMSG-IO-DATA(1:IMSMSG-KVLL - 4) TO WDATA                    
075000     ELSE                                                                 
075100       MOVE ZERO  TO WLEN                                                 
075200       MOVE SPACE TO WDATA                                                
075300     END-IF                                                               
075400     .                                                                    
075500                                                                          
075600     EJECT                                                                
075700 IMS-INSERT-MESSAGE SECTION.                                              
075800                                                                          
075900     MOVE 128         TO AIB-LEN                                          
076000     MOVE SPACE       TO AIB-SUB-FUNCTION                                 
076100     MOVE 'IOPCB   '  TO AIB-PCB-NAME                                     
076200     MOVE IMSMSG-KVLL TO AIB-IOAREA-USED                                  
076300                                                                          
076400     MOVE SPACE       TO VALID-STATUS-CODES                               
076500                                                                          
076600     IF WMODNAME = SPACE                                                  
076700       CALL AIBTDLI USING ISRT AIB-AREA IMSMSG-IO-AREA                    
076800     ELSE                                                                 
076900       CALL AIBTDLI USING ISRT AIB-AREA IMSMSG-IO-AREA WMODNAME           
077000     END-IF                                                               
077100                                                                          
077200     SET ADDRESS OF ALT-PCB TO AIB-PCB-PTR                                
077300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
077400     PERFORM IMS-STATUS-CHECK                                             
077500     .                                                                    
077600                                                                          
077700     EJECT                                                                
077800 IMS-STATUS-CHECK   SECTION.                                              
077900                                                                          
078000     SET STATUS-IX TO 1                                                   
078100     SEARCH VALID-STATUS                                                  
078200       AT END                                                             
078300         STRING 'INVALID STATUS CODE FROM IMS:' STATUS-WS                 
078400         DELIMITED BY SIZE INTO ERROR-TEXT                                
078500         CALL FELLOG                                                      
078600       WHEN VALID-STATUS (STATUS-IX) = STATUS-WS                          
078700         CONTINUE                                                         
078800     END-SEARCH                                                           
078900     .                                                                    

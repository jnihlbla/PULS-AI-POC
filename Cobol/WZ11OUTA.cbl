000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ11OUTA.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   02/12/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM HANDLES OUTPUT TO ONLINE ARCHIVE.                   
000900*        IT SUBMITS A JOB WHICH USES ACIF AND CA-SPOOL                    
001000*        (PROC WZ11PDF) TO CREATE A PDF FILE                              
001100*        1-15 SIMULTANEOUS OUTPUTS CAN BE HANDLED                         
001200                                                                          
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP3                                                                
001600 CONFIGURATION SECTION.                                                   
001700 SPECIAL-NAMES.                                                           
001800     CLASS ALPHANUM-UPPER-AND-SPACE                                       
001900           IS ' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'.                    
002000                                                                          
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300     SKIP3                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'WZ11OUTA'.            
002600                                                                          
002700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002800 77  ERROR-TEXT-START            PIC X(8)  VALUE 'ERROR:  '.              
002900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003000 77  KDRC-DISPLAY                PIC Z(5).                                
003100                                                                          
003200 77  YES                         PIC X       VALUE 'J'.                   
003300 77  NOO                         PIC X       VALUE 'N'.                   
003400                                                                          
003500 01  SMALL-LETTERS              PIC X(31)  VALUE                          
003600     'abcdefghijklmnopqrstuvwxyzÂ‰ˆ¸È'.                                   
003700 01  CAPS-LETTERS               PIC X(31)  VALUE                          
003800     'ABCDEFGHIJKLMNOPQRSTUVWXYZ≈ƒ÷‹…'.                                   
003900                                                                          
004000 01  STRING-PTR                  PIC S9(4)   BINARY.                      
004100                                                                          
004200     EJECT                                                                
004300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004400 01  GENERAL-SUBPROGRAMS.                                                 
004500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004700     03  AIBTDLI                 PIC X(8)    VALUE 'AIBTDLI '.            
004800     03  VIMSID                  PIC X(8)    VALUE 'VIMSID  '.            
004900     03  WZ20SUBM                PIC X(8)    VALUE 'WZ20SUBM'.            
005000     03  W980USPA                PIC X(8)    VALUE 'W980USPA'.            
005100                                                                          
005200*    --- PARAMETERS TO ABEND                                              
005300 77  RCODE-ABEND-NO-DUMP         PIC S9(4)   COMP VALUE +16.              
005400 77  RCODE-ABEND-WITH-DUMP       PIC S9(4)   COMP VALUE +1000.            
005500                                                                          
005600*    --- PARAMETERS TO VIMSID                                             
005700 01  VIMSID-PARM.                                                         
005800   03  IMSID4                    PIC X(4)    VALUE SPACE.                 
005900   03  FILLER                    PIC X(4)    VALUE SPACE.                 
006000                                                                          
006100*    --- PARAMETERS TO WZ20SUBM                                           
006200 01  -COPY WZ20SUBM                                                       
006300                                                                          
006400     EJECT                                                                
006500 01  W-COMMON-AREA               PIC X(16)   VALUE                        
006600                                 'COMMON-START    '.                      
006700 01  W-YYMMDD                    PIC 9(6).                                
006800 01  W-HHMMSS                    PIC 9(6).                                
006900 01  W-TH                        PIC 9(2).                                
007000                                                                          
007100 01  ZONED-CHANNEL               PIC 9(3).                                
007200                                                                          
007300*    -- THE CHANNEL TABLE IS USED TO SAVE DATA FROM OPEN CALL             
007400*    -- UNTIL IT IS NEEDED AT CLOSING TIME                                
007500 01  W-CHANNEL-AREA              PIC X(16)   VALUE                        
007600                                 'CHANNELS-START  '.                      
007700 01  W-CHANNEL-IDOUTTYPE         PIC X(15).                               
007800 01  W-CHANNEL-IDOUTREC          PIC X(30).                               
007900 01  W-CHANNEL-IDLIST            PIC X(10).                               
008000 01  W-CHANNEL-TIREGDAT          PIC 9(6).                                
008100 01  W-CHANNEL-TIKLOCK           PIC 9(8).                                
008200 01  W-CHANNEL-TAB.                                                       
008300   03  FILLER        OCCURS 15.                                           
008400     05 W-CHANNEL-DSLUX          PIC X(30).                               
008500     05 W-CHANNEL-PATHNAME       PIC X(60).                               
008600     05 W-CHANNEL-PFDEF          PIC X(6).                                
008700     05 W-CHANNEL-ESFADDR        PIC X.                                   
008800     05 W-CHANNEL-FLCARRCNTL     PIC X.                                   
008900                                                                          
009000     EJECT                                                                
009100 01  WORK-CC                      PIC X(3).                               
009200 01  WORK-ACCOUNT                 PIC X(30)  VALUE                        
009300                                  '510WOS39000WZ11OUTA'.                  
009400 01  WORK-MSGCLASS-CLASS          PIC X(30)  VALUE                        
009500                                  'MSGCLASS=H,CLASS=N'.                   
009600 01  WORK-JOBFORMS                PIC X(4)   VALUE                        
009700                                  'STD '.                                 
009800 01  WORK-PROCLIBS                PIC X(50)  VALUE                        
009900                                  'W.XXXX.PROCLIB,W.PROD.PROCLIB'.        
010000 01  WORK-SYSTZ                  PIC X(8)   VALUE                         
010100                                  'SYSTZXX '.                             
010200 77  WORK-USERLIB                PIC X(50)   VALUE                        
010300                                  'W.XXXX.PSF'.                           
010400 01  WORK-ENV                     PIC X(8)   VALUE                        
010500                                  'ENVXXXX '.                             
010600 01  WORK-INDUSS                  PIC X(50)  VALUE                        
010700                                  '/app/vccs/xxxx/wz04/data'.             
010800 77  WORK-DEST                   PIC X(60).                               
010900 77  WORK-PFDEF                  PIC X(8).                                
011000 77  WORK-CARRCNTL               PIC X(1).                                
011100 01  WORK-HLQ                    PIC X(16)  VALUE                         
011200                                  'W.XXXX.PDF.'.                          
011300                                                                          
011400                                                                          
011500*    -- AREA FOR STORING 80-BYTE PIECES OF THE DATA RECORDS               
011600 01  WORK-DATALINE.                                                       
011700*                                -- X MEANS CONTINUATION                  
011800*                                -- SPACE MEANS FIRST PIECE               
011900   03  WORK-DATACONT             PIC X.                                   
012000*                                -- LENGTH OF THIS PIECE                  
012100   03  WORK-DATALENGTH           PIC 9(4)   BINARY.                       
012200   03  WORK-DATAPIECE            PIC X(77).                               
012300                                                                          
012400     EJECT                                                                
012500*    -- PARAMETERS TO W980USPA                                            
012600 01  -COPY  W980USPA                                                      
012700                                                                          
012800     EJECT                                                                
012900 LINKAGE SECTION.                                                         
013000*01  -COPY  WZ11OUTA                                                      
013100     EJECT                                                                
013200*    -COPY W0009  -PRE ALT-                                               
013300     EJECT                                                                
013400 PROCEDURE DIVISION  USING OUTA-WZ11OUT.                                  
013500 MAIN SECTION.                                                            
013600                                                                          
013700     PERFORM A-INIT                                                       
013800                                                                          
013900     EVALUATE OUTA-KDFUNC                                                 
014000       WHEN 'OPEN'   PERFORM B-OPEN-CHANNEL                               
014100       WHEN 'PUT'    PERFORM C-WRITE                                      
014200       WHEN 'CLOSE'  PERFORM E-CLOSE                                      
014300     END-EVALUATE                                                         
014400                                                                          
014500     MOVE OUTA-KDRC TO RETURN-CODE                                        
014600     GOBACK                                                               
014700     .                                                                    
014800     EJECT                                                                
014900 A-INIT SECTION.                                                          
015000                                                                          
015100     IF OUTA-IDCALL < 1 OR > 15                                           
015200       MOVE OUTA-IDCALL TO KDRC-DISPLAY                                   
015300       STRING 'WZ11OUTA INVALID IDCALL VALUE: '                           
015400              KDRC-DISPLAY                                                
015500          DELIMITED BY SIZE                                               
015600          INTO ERROR-TEXT                                                 
015700       CALL ABEND USING RCODE-ABEND-WITH-DUMP                             
015800     END-IF                                                               
015900                                                                          
016000*    -- WZ20SUBM USES SAME IDCALL NUMBER AS THIS PROGRAM                  
016100     MOVE OUTA-IDCALL      TO SUBM-IDCALL                                 
016200                                                                          
016300*    -- FIND OUT WHICH IMS SYSTEM WE ARE USING                            
016400     CALL VIMSID USING VIMSID-PARM                                        
016500                                                                          
016600*    -- SO FAR, SO GOOD                                                   
016700     MOVE ZERO TO OUTA-KDRC                                               
016800     .                                                                    
016900     EJECT                                                                
017000 B-OPEN-CHANNEL SECTION.                                                  
017100                                                                          
017200     MOVE 'DEFAULT'        TO WORK-DEST                                   
017300     IF OUTA-IDOUTDEST NOT = SPACE                                        
017400       INSPECT OUTA-IDOUTDEST CONVERTING                                  
017500             SMALL-LETTERS TO CAPS-LETTERS                                
017600       MOVE OUTA-IDOUTDEST TO WORK-DEST                                   
017700     END-IF                                                               
017800                                                                          
017900     MOVE 'SPS06C'         TO WORK-PFDEF                                  
018000     IF OUTA-IDPFDEF NOT = SPACE                                          
018100       INSPECT OUTA-IDPFDEF CONVERTING                                    
018200             SMALL-LETTERS TO CAPS-LETTERS                                
018300       MOVE OUTA-IDPFDEF   TO WORK-PFDEF                                  
018400     END-IF                                                               
018500                                                                          
018600*    -- USE CURRENT TIMESTAMP TO AS PART OF JOBNAME AND                   
018700*    -- ATTATCHED FILES. SAVE IT NOW FOR LATER USE.                       
018800*    -- DATE AND TIME MAY OPTIONALLY BE GIVEN AS INPUT                    
018900*    -- PARAMETERS.                                                       
019000     MOVE FUNCTION CURRENT-DATE(3:6)  TO W-YYMMDD                         
019100     IF OUTA-TIREGDAT NUMERIC AND OUTA-TIREGDAT > ZERO                    
019200       MOVE OUTA-TIREGDAT TO W-YYMMDD                                     
019300     END-IF                                                               
019400     MOVE FUNCTION CURRENT-DATE(9:6)  TO W-HHMMSS                         
019500     IF OUTA-TIREGTID NUMERIC AND OUTA-TIREGTID > ZERO                    
019600       MOVE OUTA-TIREGTID TO W-HHMMSS                                     
019700     END-IF                                                               
019800     MOVE FUNCTION CURRENT-DATE(15:2) TO W-TH                             
019900                                                                          
020000*    -- PREPARE JCL TO SUBMIT AN ACIF JOB                                 
020100                                                                          
020200     IF IMSID4 = 'IMG0'                                                   
020300       MOVE '540WZ010100WZ11OUTA'       TO WORK-ACCOUNT                   
020400       MOVE 'CLASS=K'                   TO WORK-MSGCLASS-CLASS            
020500       MOVE '1800'                      TO WORK-JOBFORMS                  
020600       MOVE 'W.QASE.PSF'                TO WORK-USERLIB                   
020700       MOVE 'W.QASE.PROCLIB'            TO WORK-PROCLIBS                  
020800       MOVE 'ENVQASE'                   TO WORK-ENV                       
020900       MOVE 'SYSTZ'                     TO WORK-SYSTZ                     
021000       MOVE '/app/vccs/qase/wz04/data'  TO WORK-INDUSS                    
021100       MOVE 'WZ11.PDF.'                 TO WORK-HLQ                       
021200                                                                          
021300     ELSE                                                                 
021400       MOVE '510WOS39000WZ11OUTA'     TO WORK-ACCOUNT                     
021500       MOVE 'MSGCLASS=H,CLASS=N'      TO WORK-MSGCLASS-CLASS              
021600       MOVE 'STD'                     TO WORK-JOBFORMS                    
021700       IF IMSID4 = 'IMB0'                                                 
021800         MOVE 'W.ACPT.PSF,W.QASE.PSF'     TO WORK-USERLIB                 
021900         MOVE 'W.ACPT.PROCLIB,W.QASE.PROCLIB' TO WORK-PROCLIBS            
022000         MOVE 'ENVACPT'                   TO WORK-ENV                     
022100         MOVE 'SYSTZAA'                   TO WORK-SYSTZ                   
022200         MOVE '/app/vccs/acpt/wz04/data'  TO WORK-INDUSS                  
022300         MOVE 'W.ACPT.PDF.'               TO WORK-HLQ                     
022400       ELSE                                                               
022500         IF IMSID4 = 'IMD0'                                               
022600           MOVE 'W.XDEV.PSF,W.PROD.PSF'    TO WORK-USERLIB                
022700           MOVE 'W.XDEV.PROCLIB,W.ACPT.PROCLIB,W.PROD.PROCLIB'            
022800                                           TO WORK-PROCLIBS               
022900           MOVE 'ENVXDEV'                  TO WORK-ENV                    
023000           MOVE 'SYSTZXX'                  TO WORK-SYSTZ                  
023100           MOVE '/app/vccs/xdev/wz04/data' TO WORK-INDUSS                 
023200           MOVE 'W.XDEV.PDF.'              TO WORK-HLQ                    
023300         ELSE                                                             
023400           IF IMSID4 = 'IMY0'                                             
023500             MOVE 'W.IGRT.PSF,W.PROD.PSF'    TO WORK-USERLIB              
023600             MOVE 'W.IGRT.PROCLIB,W.ACPT.PROCLIB,W.PROD.PROCLIB'          
023700                                             TO WORK-PROCLIBS             
023800             MOVE 'ENVIGRT'                  TO WORK-ENV                  
023900             MOVE 'SYSTZTT'                  TO WORK-SYSTZ                
024000             MOVE '/app/vccs/igrt/wz04/data' TO WORK-INDUSS               
024100             MOVE 'W.IGRT.PDF.'              TO WORK-HLQ                  
024200           ELSE                                                           
024300             MOVE 'W.DEVE.PSF,W.PROD.PSF'    TO WORK-USERLIB              
024400             MOVE 'W.DEVE.PROCLIB,W.IGRT.PROCLIB,W.PROD.PROCLIB'          
024500                                             TO WORK-PROCLIBS             
024600             MOVE 'ENVDEVE'                  TO WORK-ENV                  
024700             MOVE 'SYSTZDD'                  TO WORK-SYSTZ                
024800             MOVE '/app/vccs/deve/wz04/data' TO WORK-INDUSS               
024900             MOVE 'W.DEVE.PDF.'              TO WORK-HLQ                  
025000           END-IF                                                         
025100         END-IF                                                           
025200       END-IF                                                             
025300     END-IF                                                               
025400                                                                          
025500*    -- SAVE PARAMETERS FOR THIS CHANNEL TO BE USED                       
025600*    -- IN CLOSE-CALL                                                     
025700     MOVE SPACE TO  W-CHANNEL-PATHNAME(OUTA-IDCALL)                       
025800     STRING                                                               
025900       WORK-INDUSS                             DELIMITED BY SPACE         
026000       '/x'                                    DELIMITED BY SIZE          
026100       WORK-DEST                               DELIMITED BY SPACE         
026200       '.d'                                    DELIMITED BY SIZE          
026300       W-YYMMDD                                DELIMITED BY SIZE          
026400       '.t'                                    DELIMITED BY SIZE          
026500       W-HHMMSS                                DELIMITED BY SIZE          
026600       '.pdf'                                  DELIMITED BY SIZE          
026700       INTO  W-CHANNEL-PATHNAME(OUTA-IDCALL)                              
026800                                                                          
026900     MOVE OUTA-IDOUTTYPE  TO W-CHANNEL-IDOUTTYPE                          
027000     MOVE OUTA-IDOUTREC   TO W-CHANNEL-IDOUTREC                           
027100     MOVE OUTA-IDLIST     TO W-CHANNEL-IDLIST                             
027200     MOVE OUTA-TIREGDAT   TO W-CHANNEL-TIREGDAT                           
027300     MOVE OUTA-TIKLOCK    TO W-CHANNEL-TIKLOCK                            
027400                                                                          
027500*    -- THE NAME OF FILES IN THE LINUX ENVIRONMENT MUST                   
027600*    -- ALSO BE SPECIFIED. THIS NAME IS ALSO USED AS PART OF              
027700*    -- THE OUTPUT PDF FILE IN THE MAINFRAME ENVIRONMENT.                 
027800*    -- THE FORMAT OF THE DSLUX NAME IS Dyymmdd.Thhmmss.Hthccc            
027900     MOVE SPACE TO W-CHANNEL-DSLUX(OUTA-IDCALL)                           
028000     MOVE OUTA-IDCALL TO ZONED-CHANNEL                                    
028100     STRING 'D' W-YYMMDD '.T' W-HHMMSS                                    
028200            '.H' W-TH ZONED-CHANNEL                                       
028300                            DELIMITED BY SIZE                             
028400       INTO W-CHANNEL-DSLUX(OUTA-IDCALL)                                  
028500                                                                          
028600*    -- FIX FOR SOME LANDSCAPE FORMS. SKIP LEADING *                      
028700     IF WORK-PFDEF(1:1) = "*"                                             
028800       MOVE WORK-PFDEF(2:) TO W-CHANNEL-PFDEF   (OUTA-IDCALL)             
028900       MOVE YES            TO W-CHANNEL-ESFADDR (OUTA-IDCALL)             
029000     ELSE                                                                 
029100       MOVE WORK-PFDEF     TO W-CHANNEL-PFDEF   (OUTA-IDCALL)             
029200       MOVE NOO            TO W-CHANNEL-ESFADDR (OUTA-IDCALL)             
029300     END-IF                                                               
029400                                                                          
029500                                                                          
029600     MOVE OUTA-FLCARRCNTL                                                 
029700       TO W-CHANNEL-FLCARRCNTL(OUTA-IDCALL)                               
029800                                                                          
029900                                                                          
030000     MOVE 'OPEN' TO SUBM-KDFUNC                                           
030100     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
030200                                                                          
030300                                                                          
030400     MOVE 'PUT'  TO SUBM-KDFUNC                                           
030500     MOVE SPACE TO SUBM-LINE                                              
030600     STRING                                                               
030700       '//WZ11' W-HHMMSS(3:4) ' JOB ('         DELIMITED BY SIZE          
030800       WORK-ACCOUNT                            DELIMITED BY SPACE         
030900       ',W100),'                               DELIMITED BY SIZE          
031000       QUOTE 'RTN WZ11S9' QUOTE ','            DELIMITED BY SIZE          
031100       INTO SUBM-LINE                                                     
031200     END-STRING                                                           
031300     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
031400                                                                          
031500     CALL W980USPA USING USPA-AREA                                        
031600     MOVE SPACE TO SUBM-LINE                                              
031700     STRING                                                               
031800       '//  USER='                  DELIMITED BY SIZE                     
031900       USPA-IDUSER                  DELIMITED BY SPACE                    
032000       ',PASSWORD='                 DELIMITED BY SIZE                     
032100       USPA-IDPW                    DELIMITED BY SPACE                    
032200       ','                          DELIMITED BY SIZE                     
032300       INTO SUBM-LINE                                                     
032400     END-STRING                                                           
032500     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
032600                                                                          
032700                                                                          
032800     MOVE SPACE TO SUBM-LINE                                              
032900     STRING                                                               
033000       '// MSGLEVEL=(1,1),'                    DELIMITED BY SIZE          
033100       WORK-MSGCLASS-CLASS                     DELIMITED BY SPACE         
033200       INTO SUBM-LINE                                                     
033300     END-STRING                                                           
033400     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
033500                                                                          
033600     MOVE SPACE TO SUBM-LINE                                              
033700     STRING                                                               
033800       '/*JOBPARM FORMS='                      DELIMITED BY SIZE          
033900       WORK-JOBFORMS                           DELIMITED BY SPACE         
034000       ',LINECT=0'                             DELIMITED BY SIZE          
034100       INTO SUBM-LINE                                                     
034200     END-STRING                                                           
034300     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
034400                                                                          
034500     IF IMSID4 = 'IMG0' OR 'IMB0'                                         
034600       MOVE '//*+JBS BIND VCC1'   TO SUBM-LINE                            
034700     END-IF                                                               
034800     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
034900                                                                          
035000     MOVE SPACE TO SUBM-LINE                                              
035100     STRING                                                               
035200       '//PROC JCLLIB ORDER=('                 DELIMITED BY SIZE          
035300       WORK-PROCLIBS                           DELIMITED BY SPACE         
035400       ')'                                     DELIMITED BY SIZE          
035500       INTO SUBM-LINE                                                     
035600     END-STRING                                                           
035700     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
035800                                                                          
035900     MOVE SPACE TO SUBM-LINE                                              
036000     STRING                                                               
036100       '//ENV   INCL'                          DELIMITED BY SIZE          
036200       'UDE MEMBER='                           DELIMITED BY SIZE          
036300       WORK-ENV                                DELIMITED BY SPACE         
036400       INTO SUBM-LINE                                                     
036500     END-STRING                                                           
036600     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
036700                                                                          
036800     MOVE SPACE TO SUBM-LINE                                              
036900     STRING                                                               
037000       '//ENV   INCL'                          DELIMITED BY SIZE          
037100       'UDE MEMBER='                           DELIMITED BY SIZE          
037200       WORK-SYSTZ                              DELIMITED BY SPACE         
037300       INTO SUBM-LINE                                                     
037400     END-STRING                                                           
037500     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
037600                                                                          
037700     MOVE SPACE TO SUBM-LINE                                              
037800     STRING                                                               
037900       '//DATA    EXEC WZ14P005'               DELIMITED BY SIZE          
038000       INTO SUBM-LINE                                                     
038100     END-STRING                                                           
038200     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
038300                                                                          
038400     .                                                                    
038500     EJECT                                                                
038600 C-WRITE             SECTION.                                             
038700                                                                          
038800*    When the record is meta data, dont process it. It's not              
038900*    needed here.                                                         
039000     IF OUTA-TEOUTDATA (1:5) = '§META' OR                                 
039100        OUTA-TEOUTDATA (2:5) = '§META'                                    
039200       CONTINUE                                                           
039300     ELSE                                                                 
039400       MOVE 'PUT'                TO SUBM-KDFUNC                           
039500       MOVE 1                    TO STRING-PTR                            
039600       MOVE SPACE                TO WORK-DATALINE                         
039700       PERFORM UNTIL STRING-PTR > OUTA-TEOUTDATA-L                        
039800         COMPUTE WORK-DATALENGTH = OUTA-TEOUTDATA-L                       
039900                                   - STRING-PTR + 1                       
040000         IF WORK-DATALENGTH > 77                                          
040100           MOVE 77               TO WORK-DATALENGTH                       
040200         END-IF                                                           
040300         MOVE OUTA-TEOUTDATA(STRING-PTR:WORK-DATALENGTH)                  
040400                                 TO WORK-DATAPIECE                        
040500         MOVE WORK-DATALINE      TO SUBM-LINE                             
040600                                                                          
040700         CALL WZ20SUBM USING SUBM-WZ20SUBM                                
040800                                                                          
040900         MOVE '*'                TO WORK-DATACONT                         
041000         ADD WORK-DATALENGTH     TO STRING-PTR                            
041100       END-PERFORM                                                        
041200     END-IF                                                               
041300     .                                                                    
041400     EJECT                                                                
041500 E-CLOSE             SECTION.                                             
041600                                                                          
041700     MOVE 'PUT'    TO SUBM-KDFUNC                                         
041800                                                                          
041900     MOVE SPACE TO SUBM-LINE                                              
042000     STRING                                                               
042100          '//IFDATA I'                                                    
042200          'F (DATA.WZ1405.RC = 0) THEN'                                   
042300       DELIMITED BY SIZE                                                  
042400       INTO SUBM-LINE                                                     
042500     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
042600                                                                          
042700     MOVE '//PDF    EXEC WZ11PDF,DSIN=&&DATA,'                            
042800       TO SUBM-LINE                                                       
042900     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
043000                                                                          
043100     IF W-CHANNEL-ESFADDR(OUTA-IDCALL) = YES                              
043200       MOVE '//            ESFADDR=,'                                     
043300         TO SUBM-LINE                                                     
043400       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
043500     END-IF                                                               
043600                                                                          
043700     MOVE SPACE TO SUBM-LINE                                              
043800     STRING                                                               
043900       '// DSOUTPDF='                        DELIMITED BY SIZE            
044000       WORK-HLQ                              DELIMITED BY SPACE           
044100       W-CHANNEL-DSLUX(OUTA-IDCALL)          DELIMITED BY SPACE           
044200*      ','                                   DELIMITED BY SIZE            
044300       INTO SUBM-LINE                                                     
044400     END-STRING                                                           
044500     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
044600                                                                          
044700     MOVE '//ACIF.ACIFCMDS DD *'         TO SUBM-LINE                     
044800     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
044900                                                                          
045000     IF W-CHANNEL-FLCARRCNTL(OUTA-IDCALL) = YES                           
045100       MOVE 'YES'  TO WORK-CC                                             
045200     ELSE                                                                 
045300       MOVE 'NO'   TO WORK-CC                                             
045400     END-IF                                                               
045500     MOVE SPACE TO SUBM-LINE                                              
045600     STRING                                                               
045700       ' CC='                                  DELIMITED BY SIZE          
045800       WORK-CC                                 DELIMITED BY SPACE         
045900       INTO SUBM-LINE                                                     
046000     END-STRING                                                           
046100     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
046200                                                                          
046300     MOVE SPACE TO SUBM-LINE                                              
046400     STRING                                                               
046500       ' USERLIB='                             DELIMITED BY SIZE          
046600       WORK-USERLIB                            DELIMITED BY SPACE         
046700       INTO SUBM-LINE                                                     
046800     END-STRING                                                           
046900     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
047000                                                                          
047100     MOVE SPACE TO SUBM-LINE                                              
047200     STRING                                                               
047300       ' FORMDEF=F1'                           DELIMITED BY SIZE          
047400       W-CHANNEL-PFDEF(OUTA-IDCALL)            DELIMITED BY SPACE         
047500       INTO SUBM-LINE                                                     
047600     END-STRING                                                           
047700     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
047800                                                                          
047900     MOVE SPACE TO SUBM-LINE                                              
048000     STRING                                                               
048100       ' PAGEDEF=P1'                           DELIMITED BY SIZE          
048200       W-CHANNEL-PFDEF(OUTA-IDCALL)            DELIMITED BY SPACE         
048300       INTO SUBM-LINE                                                     
048400     END-STRING                                                           
048500     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
048600                                                                          
048700     MOVE SPACE TO SUBM-LINE                                              
048800     STRING                                                               
048900       '// EXEC W001HFSC,MODE=BINARY,'         DELIMITED BY SIZE          
049000       INTO SUBM-LINE                                                     
049100     END-STRING                                                           
049200     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
049300                                                                          
049400     MOVE SPACE TO SUBM-LINE                                              
049500     STRING                                                               
049600       '// DSIN='                            DELIMITED BY SIZE            
049700       WORK-HLQ                              DELIMITED BY SPACE           
049800       W-CHANNEL-DSLUX(OUTA-IDCALL)          DELIMITED BY SPACE           
049900       ','                                   DELIMITED BY SIZE            
050000       INTO SUBM-LINE                                                     
050100     END-STRING                                                           
050200     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
050300                                                                          
050400     MOVE SPACE TO SUBM-LINE                                              
050500     STRING                                                               
050600       '// PATHOUT=' QUOTE                     DELIMITED BY SIZE          
050700       W-CHANNEL-PATHNAME(OUTA-IDCALL)         DELIMITED BY SPACE         
050800       QUOTE                                   DELIMITED BY SIZE          
050900       INTO SUBM-LINE                                                     
051000     END-STRING                                                           
051100     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
051200                                                                          
051300     MOVE SPACE TO SUBM-LINE                                              
051400     STRING                                                               
051500          '//SIGNAL I'                                                    
051600          'F ABEND THEN '                    DELIMITED BY SIZE            
051700       INTO SUBM-LINE                                                     
051800     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
051900                                                                          
052000     MOVE '//SIGNAL  EXEC WSOP'        TO SUBM-LINE                       
052100     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
052200                                                                          
052300     MOVE ' ORDER WZ11JABE SYMBOLS'    TO SUBM-LINE                       
052400     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
052500                                                                          
052600     MOVE ' ACTION(sending a pdf file)'       TO SUBM-LINE                
052700     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
052800                                                                          
052900     MOVE SPACE TO SUBM-LINE                                              
053000     STRING                                                               
053100       ' DEST('                              DELIMITED BY SIZE            
053200       FUNCTION TRIM(W-CHANNEL-PATHNAME(OUTA-IDCALL))                     
053300                                             DELIMITED BY SIZE            
053400       ')'                                   DELIMITED BY SIZE            
053500       INTO SUBM-LINE                                                     
053600     END-STRING                                                           
053700     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
053800                                                                          
053900     MOVE SPACE TO SUBM-LINE                                              
054000     STRING                                                               
054100       ' TIMESTAMP('                         DELIMITED BY SIZE            
054200       W-YYMMDD                              DELIMITED BY SIZE            
054300       ' '                                   DELIMITED BY SIZE            
054400       W-HHMMSS                              DELIMITED BY SIZE            
054500       ') JOBNAME(WZ11'                      DELIMITED BY SIZE            
054600       W-HHMMSS(3:4)                         DELIMITED BY SIZE            
054700       ')'                                   DELIMITED BY SIZE            
054800       INTO SUBM-LINE                                                     
054900     END-STRING                                                           
055000     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
055100                                                                          
055200     IF W-CHANNEL-IDOUTTYPE = SPACES                                      
055300       MOVE ' INFO( )'                 TO SUBM-LINE                       
055400       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
055500                                                                          
055600       MOVE ' TYPE( )'                 TO SUBM-LINE                       
055700       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
055800                                                                          
055900       MOVE ' REC( )'                  TO SUBM-LINE                       
056000       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
056100                                                                          
056200       MOVE ' LIST( )'                 TO SUBM-LINE                       
056300       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
056400                                                                          
056500       MOVE ' REGDAT( )'               TO SUBM-LINE                       
056600       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
056700                                                                          
056800       MOVE ' KLOCK( )'                TO SUBM-LINE                       
056900       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
057000     ELSE                                                                 
057100       MOVE ' INFO(Restart Keys:)'     TO SUBM-LINE                       
057200       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
057300                                                                          
057400       MOVE SPACE TO SUBM-LINE                                            
057500       STRING                                                             
057600         ' TYPE(Output Type    : '           DELIMITED BY SIZE            
057700         FUNCTION TRIM(W-CHANNEL-IDOUTTYPE)                               
057800                                             DELIMITED BY SIZE            
057900         ')'                                 DELIMITED BY SIZE            
058000         INTO SUBM-LINE                                                   
058100       END-STRING                                                         
058200       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
058300                                                                          
058400       MOVE SPACE TO SUBM-LINE                                            
058500       STRING                                                             
058600         ' REC(Output SubType : '            DELIMITED BY SIZE            
058700         FUNCTION TRIM(W-CHANNEL-IDOUTREC)                                
058800                                             DELIMITED BY SIZE            
058900         ')'                                 DELIMITED BY SIZE            
059000         INTO SUBM-LINE                                                   
059100       END-STRING                                                         
059200       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
059300                                                                          
059400       MOVE SPACE TO SUBM-LINE                                            
059500       STRING                                                             
059600         ' LIST(Output Id      : '           DELIMITED BY SIZE            
059700         FUNCTION TRIM(W-CHANNEL-IDLIST)                                  
059800                                             DELIMITED BY SIZE            
059900         ')'                                 DELIMITED BY SIZE            
060000         INTO SUBM-LINE                                                   
060100       END-STRING                                                         
060200       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
060300                                                                          
060400       MOVE SPACE TO SUBM-LINE                                            
060500       STRING                                                             
060600         ' REGDAT(Date           : '         DELIMITED BY SIZE            
060700         W-CHANNEL-TIREGDAT                  DELIMITED BY SPACE           
060800         ')'                                 DELIMITED BY SIZE            
060900         INTO SUBM-LINE                                                   
061000       END-STRING                                                         
061100       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
061200                                                                          
061300       MOVE SPACE TO SUBM-LINE                                            
061400       STRING                                                             
061500         ' KLOCK(Time           : '          DELIMITED BY SIZE            
061600         W-CHANNEL-TIKLOCK                   DELIMITED BY SPACE           
061700         ')'                                 DELIMITED BY SIZE            
061800         INTO SUBM-LINE                                                   
061900       END-STRING                                                         
062000       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
062100     END-IF                                                               
062200                                                                          
062300     MOVE ' END-ORDER'                 TO SUBM-LINE                       
062400     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
062500                                                                          
062600     MOVE '//SIGNAL ENDIF'             TO SUBM-LINE                       
062700     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
062800                                                                          
062900                                                                          
063000     MOVE '//IFDATA ENDIF' TO SUBM-LINE                                   
063100     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
063200                                                                          
063300     MOVE 'CLOSE'  TO SUBM-KDFUNC                                         
063400     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
063500     .                                                                    

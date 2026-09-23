000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ11OUTP.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   02/08/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM HANDLES OUTPUT TO PRINTERS.                         
000900*        IT USES EITHER:                                                  
001000*         1. IMS IAFP AND DL1 CALLS TO DYNAMICALLY                        
001100*            ALLOCATE AND SEND DATA TO A PRINTER.                         
001200*        OR                                                               
001300*         2. SUBMITS A JOB USING ACIF TO FORMAT THE DATA                  
001400*            AND SEND IT TO THE PRINTER.                                  
001500*        1-15 SIMULTANEOUS PRINTINGS CAN BE HANDLED                       
001600                                                                          
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP3                                                                
002000 CONFIGURATION SECTION.                                                   
002100 SPECIAL-NAMES.                                                           
002200     CLASS ALPHANUM-UPPER-AND-SPACE                                       
002300           IS ' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'.                    
002400                                                                          
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'WZ11OUTP'.            
003000                                                                          
003100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003200 77  ERROR-TEXT-START            PIC X(8)  VALUE 'ERROR:  '.              
003300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003400 77  KDRC-DISPLAY                PIC Z(5).                                
003500                                                                          
003600 77  YES                         PIC X       VALUE 'J'.                   
003700 77  NOO                         PIC X       VALUE 'N'.                   
003800                                                                          
003900 01  SMALL-LETTERS              PIC X(31)  VALUE                          
004000     'abcdefghijklmnopqrstuvwxyzåäöüé'.                                   
004100 01  CAPS-LETTERS               PIC X(31)  VALUE                          
004200     'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÜÉ'.                                   
004300                                                                          
004400 01  TEST-PRINTER-NAME       PIC X(8).                                    
004500* V6+VT PRINTRAR UTGÅTT. LÄMNAR KVAR VX FÖR EV FRAMTIDA BEHOV.            
004600 88  VX-PRINTER              VALUE 'QSE00000'.                            
004700                                                                          
004800 01  W-YYMMDD                    PIC X(6).                                
004900 01  W-HHMMSS                    PIC X(6).                                
005000                                                                          
005100*    --- ERROR FEEDBACK INFO FROM IAFP                                    
005200 01  IAFP-FEEDBACK-START          PIC X(16)   VALUE                       
005300                                 'IAFP-FEEDBACK: '.                       
005400 01  IAFP-FEEDBACK.                                                       
005500   03  FILLER                    PIC S9(4)   BINARY VALUE +204.           
005600   03  FILLER                    PIC S9(4)   BINARY VALUE ZERO.           
005700   03  IAFP-FEEDBACK-TEXT        PIC X(200)  VALUE SPACE.                 
005800                                                                          
005900 01  STRING-PTR                  PIC S9(4)   BINARY.                      
006000                                                                          
006100     EJECT                                                                
006200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006300 01  GENERAL-SUBPROGRAMS.                                                 
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006600     03  AIBTDLI                 PIC X(8)    VALUE 'AIBTDLI '.            
006700     03  VIMSID                  PIC X(8)    VALUE 'VIMSID  '.            
006800     03  WZ20SUBM                PIC X(8)    VALUE 'WZ20SUBM'.            
006900     03  W980USPA                PIC X(8)    VALUE 'W980USPA'.            
007000                                                                          
007100*    --- PARAMETERS TO ABEND                                              
007200 77  RCODE-ABEND-NO-DUMP         PIC S9(4)   COMP VALUE +16.              
007300 77  RCODE-ABEND-WITH-DUMP       PIC S9(4)   COMP VALUE +1000.            
007400                                                                          
007500*    --- PARAMETERS TO VIMSID                                             
007600 01  VIMSID-PARM.                                                         
007700   03  IMSID4                    PIC X(4)    VALUE SPACE.                 
007800   03  FILLER                    PIC X(4)    VALUE SPACE.                 
007900                                                                          
008000*    --- PARAMETERS TO WZ20SUBM                                           
008100 01  -COPY WZ20SUBM                                                       
008200                                                                          
008300     EJECT                                                                
008400 01  W-COMMON-AREA               PIC X(16)   VALUE                        
008500                                 'COMMON-START    '.                      
008600 77  W-DESTINATION               PIC X(8).                                
008700 77  W-CLASS                     PIC X.                                   
008800 77  W-FORMS                     PIC X(8).                                
008900 77  W-COPIES                    PIC X.                                   
009000 77  W-PFDEF                     PIC X(8).                                
009100 77  W-USERLIB                   PIC X(50).                               
009200 77  W-OUTDISP                   PIC X(8).                                
009300 77  W-FLACIF                    PIC X.                                   
009400                                                                          
009500*    -- SPECIFIED PCB NAMES, AND METHOD FOR EACH OUTPUT CHANNEL           
009600 01  W-CHANNEL-IDOUTTYPE         PIC X(15).                               
009700 01  W-CHANNEL-IDOUTREC          PIC X(30).                               
009800 01  W-CHANNEL-IDLIST            PIC X(10).                               
009900 01  W-CHANNEL-TIREGDAT          PIC 9(6).                                
010000 01  W-CHANNEL-TIKLOCK           PIC 9(8).                                
010100                                                                          
010200 01  W-CHANNEL-TAB.                                                       
010300   03  FILLER        OCCURS 15.                                           
010400     05  W-CHANNEL-ALTPCB-NAME    PIC X(8).                               
010500     05  W-CHANNEL-FLCARRCNTL     PIC X(1).                               
010600     05  W-CHANNEL-DESTINATION    PIC X(8).                               
010700     05  W-CHANNEL-CLASS          PIC X.                                  
010800     05  W-CHANNEL-FORMS          PIC X(8).                               
010900     05  W-CHANNEL-COPIES         PIC X.                                  
011000     05  W-CHANNEL-PFDEF          PIC X(6).                               
011100     05  W-CHANNEL-USERLIB        PIC X(50).                              
011200     05  W-CHANNEL-OUTDISP        PIC X(8).                               
011300     05  W-CHANNEL-FLACIF         PIC X(1).                               
011400                                                                          
011500     EJECT                                                                
011600*    -- STANDARD PCB NAME WHEN NO PCB IS SPECIFIED                        
011700 01  W-ALTPCB.                                                            
011800   03  W-ALTPCB-START            PIC X(3)    VALUE 'OUT'.                 
011900   03  W-IDCALL                  PIC 99.                                  
012000   03  W-ALTPCB-REST             PIC X(3)    VALUE SPACE.                 
012100     EJECT                                                                
012200*    -- IMS FUNKTIONSKODER                                                
012300*    -COPY W0003                                                          
012400                                                                          
012500 01  STATUS-WS                   PIC XX.                                  
012600     88 SEGMENT-EXISTS           VALUE SPACE.                             
012700                                                                          
012800 01  VALID-STATUS-CODES.                                                  
012900   03  VALID-STATUS OCCURS 5 INDEXED BY STATUS-IX                         
013000                                 PIC XX.                                  
013100                                                                          
013200     EJECT                                                                
013300 01  AIB-AREA-START              PIC X(16)   VALUE                        
013400                                 'AIB-AREA-START  '.                      
013500*    -COPY W0031                                                          
013600                                                                          
013700     EJECT                                                                
013800 01  IAFP-AREA-START              PIC X(16)   VALUE                       
013900                                 'IAFP-AREA-START '.                      
014000                                                                          
014100 01  IAFP-DEST                    PIC X(8)    VALUE 'IAFPCC  '.           
014200                                                                          
014300 01  IAFP-OPT-LIST.                                                       
014400   03  IAFP-OPT-FIRST-PART.                                               
014500     05 IAFP-OPT-LL               PIC S9(4)   BINARY.                     
014600     05 FILLER                    PIC S9(4)   BINARY VALUE ZERO.          
014700     05 FILLER                    PIC X(5)    VALUE 'IAFP='.              
014800     05 IAFP-OPT-CARRCNTL         PIC X.                                  
014900     05 IAFP-OPT-INTEGRITY        PIC X       VALUE '1'.                  
015000     05 IAFP-OPT-MSGPROCESS       PIC X       VALUE 'M'.                  
015100     05 FILLER                    PIC X(6)    VALUE ',PRTO='.             
015200   03  IAFP-OPT-SECOND-PART.                                              
015300     05 IAFP-PRTO-LL              PIC S9(4)   BINARY.                     
015400     05 IAFP-PRTO-OUTDES-PARMS    PIC X(2000).                            
015500                                                                          
015600 01  IAFP-MISC                  PIC X(100)  VALUE                         
015700       'LINECT(00),PRTY(12)'.                                             
015800     EJECT                                                                
015900 01  ACIF-CC                      PIC X(3).                               
016000 01  ACIF-ACCOUNT                 PIC X(30)  VALUE                        
016100                                  '510WOS39000WZ11OUTP'.                  
016200 01  ACIF-MSGCLASS-CLASS          PIC X(30)  VALUE                        
016300                                  'MSGCLASS=H,CLASS=N'.                   
016400 01  ACIF-JOBFORMS                PIC X(4)   VALUE                        
016500                                  'STD '.                                 
016600 01  ACIF-PROCLIBS                PIC X(50)  VALUE                        
016700                                  'W.XXXX.PROCLIB,W.PROD.PROCLIB'.        
016800 01  ACIF-ENV                     PIC X(8)   VALUE                        
016900                                  'ENVXXXX'.                              
017000 01  ACIF-ROUTE                   PIC X(8)   VALUE                        
017100                                  'LOCAL  '.                              
017200                                                                          
017300*    -- AREA FOR STORING 80-BYTE PIECES OF THE DATA RECORDS               
017400 01  ACIF-DATALINE.                                                       
017500*                                -- X MEANS CONTINUATION                  
017600*                                -- SPACE MEANS FIRST PIECE               
017700   03  ACIF-DATACONT             PIC X.                                   
017800*                                -- LENGTH OF THIS PIECE                  
017900   03  ACIF-DATALENGTH           PIC 9(4)   BINARY.                       
018000   03  ACIF-DATAPIECE            PIC X(77).                               
018100                                                                          
018200                                                                          
018300     EJECT                                                                
018400 01  IMSMSG-AREA-START            PIC X(16)   VALUE                       
018500                                 'IMSMSG-AREA-STAR'.                      
018600                                                                          
018700*    -- AREA FOR SAVING THE DATA TO BE PRINTED                            
018800 01 IMSMSG-IO-AREA.                                                       
018900   03  IMSMSG-BLOCK-LL           PIC S9(4)   BINARY.                      
019000   03  FILLER                    PIC S9(4)   BINARY VALUE ZERO.           
019100   03  IMSMSG-IO-RECORD.                                                  
019200     05 IMSMSG-RECORD-LL         PIC S9(4)   BINARY.                      
019300     05 FILLER                   PIC S9(4)   BINARY VALUE ZERO.           
019400*    -- MAX LENGTH OF DATA THAT CAN BE WRITTEN = 32KB                     
019500     05 IMSMSG-IO-DATA           PIC X(32764).                            
019600                                                                          
019700                                                                          
019800     EJECT                                                                
019900 01  -COPY  W980USPA                                                      
020000                                                                          
020100     EJECT                                                                
020200 LINKAGE SECTION.                                                         
020300*01  -COPY  WZ11OUTP                                                      
020400     EJECT                                                                
020500*    -COPY W0009  -PRE ALT-                                               
020600     EJECT                                                                
020700 PROCEDURE DIVISION  USING OUTP-WZ11OUT.                                  
020800 MAIN SECTION.                                                            
020900                                                                          
021000     PERFORM A-INIT                                                       
021100                                                                          
021200     EVALUATE OUTP-KDFUNC                                                 
021300       WHEN 'OPEN'   PERFORM B-OPEN-CHANNEL                               
021400       WHEN 'PUT'                                                         
021500*                    -- COPIES=0 INDICATES DUMMY/NO ACTION                
021600                     IF W-COPIES NOT ='0'                                 
021700                       IF W-FLACIF  = YES                                 
021800                          PERFORM C-WRITE-VIA-ACIF                        
021900                       ELSE                                               
022000                          PERFORM D-WRITE-VIA-IAFP                        
022100                       END-IF                                             
022200                     END-IF                                               
022300       WHEN 'CLOSE'                                                       
022400                     IF W-COPIES NOT ='0'                                 
022500                       IF W-FLACIF    = YES                               
022600                          PERFORM E-CLOSE-ACIF                            
022700                       END-IF                                             
022800                     END-IF                                               
022900     END-EVALUATE                                                         
023000                                                                          
023100     MOVE OUTP-KDRC TO RETURN-CODE                                        
023200     GOBACK                                                               
023300     .                                                                    
023400     EJECT                                                                
023500 A-INIT SECTION.                                                          
023600                                                                          
023700     IF OUTP-IDCALL < 1 OR > 15                                           
023800       MOVE OUTP-IDCALL TO KDRC-DISPLAY                                   
023900       STRING 'WZ11OUTP INVALID IDCALL VALUE: '                           
024000              KDRC-DISPLAY                                                
024100          DELIMITED BY SIZE                                               
024200          INTO ERROR-TEXT                                                 
024300       CALL ABEND USING RCODE-ABEND-WITH-DUMP                             
024400     END-IF                                                               
024500                                                                          
024600*    -- THE CALLING  PROGRAM MUST HAVE A PSB CONTAINING                   
024700*    -- ALT-PCB:S WITH NAME OUT* WHERE *=0-9 AND CORRESPONDS              
024800*    -- TO THE VALUE OF IDCALL USED BY THE CALLING PROGRAM.               
024900*    -- OR ALTERNATIVELY PCB NAME IS PROVIDED VIA ARGUMENT                
025000*    -- IN THE OPEN CALL                                                  
025100                                                                          
025200     IF OUTP-KDFUNC = 'OPEN'                                              
025300       IF OUTP-IDPCB = SPACE OR LOW-VALUE                                 
025400*        -- COMPUTE STANDARD PCB NAME                                     
025500         MOVE 'OUT'        TO W-ALTPCB-START                              
025600         MOVE OUTP-IDCALL  TO W-IDCALL                                    
025700         MOVE SPACE        TO W-ALTPCB-REST                               
025800       ELSE                                                               
025900*        -- USE GIVEN PCB NAME                                            
026000         MOVE OUTP-IDPCB   TO W-ALTPCB                                    
026100       END-IF                                                             
026200                                                                          
026300*       -- SAVE PCB NAME FOR THIS AND OTHER CALLS                         
026400       MOVE W-ALTPCB       TO  W-CHANNEL-ALTPCB-NAME(OUTP-IDCALL)         
026500       MOVE OUTP-FLACIF    TO  W-CHANNEL-FLACIF(OUTP-IDCALL)              
026600       IF OUTP-KVCOPIES IS NUMERIC                                        
026700         MOVE OUTP-KVCOPIES TO  W-CHANNEL-COPIES(OUTP-IDCALL)             
026800       ELSE                                                               
026900         MOVE '1'           TO  W-CHANNEL-COPIES(OUTP-IDCALL)             
027000       END-IF                                                             
027100                                                                          
027200     END-IF                                                               
027300                                                                          
027400*    -- WZ20SUBM USES SAME IDCALL NUMBER AS THIS PROGRAM                  
027500     MOVE OUTP-IDCALL      TO SUBM-IDCALL                                 
027600                                                                          
027700*    -- SET DEFAULT VALUES FOR OUTPUT PARAMETERS                          
027800     MOVE 'LOCAL'          TO W-DESTINATION                               
027900     MOVE 'A'              TO W-CLASS                                     
028000     MOVE 'STD'            TO W-FORMS                                     
028100     MOVE SPACE            TO W-PFDEF                                     
028200     MOVE 'WRITE'          TO W-OUTDISP                                   
028300                                                                          
028400*    -- FETCH DATA FOR THIS CHANNEL                                       
028500     MOVE W-CHANNEL-ALTPCB-NAME(OUTP-IDCALL) TO W-ALTPCB                  
028600     MOVE W-CHANNEL-FLACIF(OUTP-IDCALL)      TO W-FLACIF                  
028700     MOVE W-CHANNEL-COPIES(OUTP-IDCALL)      TO W-COPIES                  
028800                                                                          
028900*    -- FIND OUT WHICH IMS SYSTEM WE ARE USING                            
029000     CALL VIMSID USING VIMSID-PARM                                        
029100                                                                          
029200*    -- SO FAR, SO GOOD                                                   
029300     MOVE ZERO TO OUTP-KDRC                                               
029400     .                                                                    
029500     EJECT                                                                
029600 B-OPEN-CHANNEL SECTION.                                                  
029700                                                                          
029800     IF OUTP-IDOUTDEST NOT = SPACE                                        
029900       INSPECT OUTP-IDOUTDEST CONVERTING                                  
030000             SMALL-LETTERS TO CAPS-LETTERS                                
030100       MOVE OUTP-IDOUTDEST TO W-DESTINATION                               
030200     END-IF                                                               
030300     IF OUTP-IDOUTDEST = 'LOCALH' OR 'NJOV2' OR 'NJOV1'                   
030400       MOVE 'LOCAL'        TO W-DESTINATION                               
030500       MOVE 'H'            TO W-CLASS                                     
030600       MOVE 'HOLD'         TO W-OUTDISP                                   
030700     END-IF                                                               
030800     IF OUTP-IDOUTDEST (1:4) = 'NJO0'                                     
030900       MOVE 'E'  TO W-CLASS                                               
031000     END-IF                                                               
031100                                                                          
031200     IF OUTP-KVCOPIES IS NUMERIC                                          
031300       MOVE OUTP-KVCOPIES  TO W-COPIES                                    
031400     END-IF                                                               
031500                                                                          
031600     IF OUTP-IDPFDEF NOT = SPACE                                          
031700       INSPECT OUTP-IDPFDEF CONVERTING                                    
031800             SMALL-LETTERS TO CAPS-LETTERS                                
031900       MOVE OUTP-IDPFDEF   TO W-PFDEF                                     
032000     END-IF                                                               
032100                                                                          
032200     IF OUTP-IDFORMSNM NOT = SPACE                                        
032300       INSPECT OUTP-IDFORMSNM CONVERTING                                  
032400             SMALL-LETTERS TO CAPS-LETTERS                                
032500       MOVE OUTP-IDFORMSNM TO W-FORMS                                     
032600     END-IF                                                               
032700                                                                          
032800*    -- SELECT SIMPLE PRINTING VIA ALT-PCB AND IMS AFP INTERFACE          
032900*    -- OR EXTENDED PRINTING VIA ACIF IN A SUBMITTED JOB.                 
033000*    -- COPIES=0 INDICATES DUMMY/NO ACTION                                
033100     IF W-COPIES NOT ='0'                                                 
033200       IF W-FLACIF = YES                                                  
033300          PERFORM BA-OPEN-ACIF                                            
033400       ELSE                                                               
033500          PERFORM BB-OPEN-IAFP                                            
033600       END-IF                                                             
033700     END-IF                                                               
033800                                                                          
033900*    -- SAVE VALUES FOR CLOSE CALL (ONLY USED FOR ACIF JCL)               
034000     MOVE OUTP-FLCARRCNTL TO W-CHANNEL-FLCARRCNTL(OUTP-IDCALL)            
034100     MOVE W-DESTINATION   TO W-CHANNEL-DESTINATION(OUTP-IDCALL)           
034200     MOVE W-CLASS         TO W-CHANNEL-CLASS(OUTP-IDCALL)                 
034300     MOVE W-FORMS         TO W-CHANNEL-FORMS(OUTP-IDCALL)                 
034400     MOVE W-COPIES        TO W-CHANNEL-COPIES(OUTP-IDCALL)                
034500     MOVE W-PFDEF         TO W-CHANNEL-PFDEF(OUTP-IDCALL)                 
034600     MOVE W-USERLIB       TO W-CHANNEL-USERLIB(OUTP-IDCALL)               
034700     MOVE W-OUTDISP       TO W-CHANNEL-OUTDISP(OUTP-IDCALL)               
034800                                                                          
034900     MOVE OUTP-IDOUTTYPE  TO W-CHANNEL-IDOUTTYPE                          
035000     MOVE OUTP-IDOUTREC   TO W-CHANNEL-IDOUTREC                           
035100     MOVE OUTP-IDLIST     TO W-CHANNEL-IDLIST                             
035200     MOVE OUTP-TIREGDAT   TO W-CHANNEL-TIREGDAT                           
035300     MOVE OUTP-TIKLOCK    TO W-CHANNEL-TIKLOCK                            
035400                                                                          
035500     .                                                                    
035600     EJECT                                                                
035700 BA-OPEN-ACIF     SECTION.                                                
035800                                                                          
035900*    -- USE CURRENT TIMESTAMP TO AS PART OF JOBNAME AND                   
036000*    -- ATTATCHED FILES. SAVE IT NOW FOR LATER USE.                       
036100     MOVE FUNCTION CURRENT-DATE(3:6)  TO W-YYMMDD                         
036200     MOVE FUNCTION CURRENT-DATE(9:6)  TO W-HHMMSS                         
036300                                                                          
036400*    -- PREPARE JCL TO SUBMIT AN ACIF JOB                                 
036500                                                                          
036600     IF IMSID4 = 'IMG0'                                                   
036700       MOVE '540WZ010100WZ11OUTP'       TO ACIF-ACCOUNT                   
036800       MOVE 'CLASS=K'                   TO ACIF-MSGCLASS-CLASS            
036900       MOVE '1800'                      TO ACIF-JOBFORMS                  
037000****** MOVE 'NJEV2'                     TO ACIF-ROUTE                     
037100       MOVE 'W.QASE.PSF'                TO W-USERLIB                      
037200       MOVE 'W.QASE.PROCLIB'            TO ACIF-PROCLIBS                  
037300       MOVE 'ENVQASE'                   TO ACIF-ENV                       
037400     ELSE                                                                 
037500       MOVE '510WOS39000WZ11OUTP'       TO ACIF-ACCOUNT                   
037600       MOVE 'MSGCLASS=H,CLASS=N'        TO ACIF-MSGCLASS-CLASS            
037700       MOVE 'STD'                       TO ACIF-JOBFORMS                  
037800       IF IMSID4 = 'IMB0'                                                 
037900         MOVE 'W.ACPT.PSF,W.QASE.PSF'   TO W-USERLIB                      
038000         MOVE 'W.ACPT.PROCLIB,W.QASE.PROCLIB' TO ACIF-PROCLIBS            
038100         MOVE 'ENVACPT'                 TO ACIF-ENV                       
038200       ELSE                                                               
038300         IF IMSID4 = 'IMD0'                                               
038400           MOVE 'W.XDEV.PSF,W.PROD.PSF'  TO W-USERLIB                     
038500           MOVE 'W.XDEV.PROCLIB,W.ACPT.PROCLIB,W.PROD.PROCLIB'            
038600                TO ACIF-PROCLIBS                                          
038700           MOVE 'ENVXDEV'                TO ACIF-ENV                      
038800         ELSE                                                             
038900           IF IMSID4 = 'IMP0'                                             
039000             MOVE 'W.DEVE.PSF,W.PROD.PSF' TO W-USERLIB                    
039100             MOVE 'W.DEVE.PROCLIB,W.IGRT.PROCLIB,W.PROD.PROCLIB'          
039200                  TO ACIF-PROCLIBS                                        
039300             MOVE 'ENVDEVE'                TO ACIF-ENV                    
039400           ELSE                                                           
039500             MOVE 'W.IGRT.PSF,W.PROD.PSF' TO W-USERLIB                    
039600             MOVE 'W.IGRT.PROCLIB,W.ACPT.PROCLIB,W.PROD.PROCLIB'          
039700                  TO ACIF-PROCLIBS                                        
039800             MOVE 'ENVIGRT'                TO ACIF-ENV                    
039900           END-IF                                                         
040000         END-IF                                                           
040100       END-IF                                                             
040200     END-IF                                                               
040300                                                                          
040400     MOVE 'OPEN' TO SUBM-KDFUNC                                           
040500     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
040600                                                                          
040700     MOVE SPACE TO SUBM-LINE                                              
040800     STRING                                                               
040900       '//WZ11' W-HHMMSS(3:4) ' JOB ('         DELIMITED BY SIZE          
041000       ACIF-ACCOUNT                            DELIMITED BY SPACE         
041100       ',W100),'                               DELIMITED BY SIZE          
041200       QUOTE 'RTN WZ11S9' QUOTE ','            DELIMITED BY SIZE          
041300       INTO SUBM-LINE                                                     
041400     END-STRING                                                           
041500     MOVE 'PUT'  TO SUBM-KDFUNC                                           
041600     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
041700                                                                          
041800     MOVE SPACE TO SUBM-LINE                                              
041900     CALL W980USPA USING USPA-AREA                                        
042000     MOVE SPACE TO SUBM-LINE                                              
042100     STRING                                                               
042200       '// USER='                   DELIMITED BY SIZE                     
042300       USPA-IDUSER                  DELIMITED BY SPACE                    
042400       ',PASSWORD='                 DELIMITED BY SIZE                     
042500       USPA-IDPW                    DELIMITED BY SPACE                    
042600       ','                          DELIMITED BY SIZE                     
042700       INTO SUBM-LINE                                                     
042800     END-STRING                                                           
042900     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
043000                                                                          
043100     MOVE SPACE TO SUBM-LINE                                              
043200     STRING                                                               
043300       '// MSGLEVEL=(1,1),'                    DELIMITED BY SIZE          
043400       ACIF-MSGCLASS-CLASS                     DELIMITED BY SPACE         
043500       INTO SUBM-LINE                                                     
043600     END-STRING                                                           
043700     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
043800                                                                          
043900     MOVE SPACE TO SUBM-LINE                                              
044000     STRING                                                               
044100       '/*JOBPARM FORMS='                      DELIMITED BY SIZE          
044200       ACIF-JOBFORMS                           DELIMITED BY SPACE         
044300       ',LINECT=0,LINES=9999'                  DELIMITED BY SIZE          
044400       INTO SUBM-LINE                                                     
044500     END-STRING                                                           
044600     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
044700                                                                          
044800     MOVE SPACE TO SUBM-LINE                                              
044900     STRING                                                               
045000       '/*ROUTE XEQ '                          DELIMITED BY SIZE          
045100       ACIF-ROUTE                              DELIMITED BY SPACE         
045200       INTO SUBM-LINE                                                     
045300     END-STRING                                                           
045400     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
045500                                                                          
045600     IF IMSID4 = 'IMG0' OR 'IMB0'                                         
045700       MOVE '//*+JBS BIND VCC1'   TO SUBM-LINE                            
045800     END-IF                                                               
045900     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
046000                                                                          
046100     MOVE SPACE TO SUBM-LINE                                              
046200     STRING                                                               
046300       '//PROC JCLLIB ORDER=('                 DELIMITED BY SIZE          
046400       ACIF-PROCLIBS                           DELIMITED BY SPACE         
046500       ')'                                     DELIMITED BY SIZE          
046600       INTO SUBM-LINE                                                     
046700     END-STRING                                                           
046800     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
046900                                                                          
047000     MOVE SPACE TO SUBM-LINE                                              
047100     STRING                                                               
047200       '//ENV   INCL'                          DELIMITED BY SIZE          
047300       'UDE MEMBER='                           DELIMITED BY SIZE          
047400       ACIF-ENV                                DELIMITED BY SPACE         
047500       INTO SUBM-LINE                                                     
047600     END-STRING                                                           
047700     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
047800                                                                          
047900     MOVE SPACE TO SUBM-LINE                                              
048000     STRING                                                               
048100       '//DATA    EXEC WZ14P005'               DELIMITED BY SIZE          
048200       INTO SUBM-LINE                                                     
048300     END-STRING                                                           
048400     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
048500                                                                          
048600     .                                                                    
048700     EJECT                                                                
048800 BB-OPEN-IAFP     SECTION.                                                
048900                                                                          
049000*    -- USE IMS AND IAFP TO PRINT VIA AN ALTERNATE PCB                    
049100                                                                          
049200     IF OUTP-FLCARRCNTL = YES                                             
049300       MOVE 'A'    TO IAFP-OPT-CARRCNTL                                   
049400     ELSE                                                                 
049500       MOVE 'N'    TO IAFP-OPT-CARRCNTL                                   
049600     END-IF                                                               
049700                                                                          
049800     IF W-FORMS = 'SOSI'                                                  
049900       STRING                                                             
050000          IAFP-MISC       DELIMITED BY SPACE                              
050100          ',PRMODE(SOSI1)' DELIMITED BY SIZE                              
050200         INTO IAFP-MISC                                                   
050300       MOVE 'STD '        TO W-FORMS                                      
050400     END-IF                                                               
050500                                                                          
050600     MOVE SPACE        TO IAFP-PRTO-OUTDES-PARMS.                         
050700     MOVE 1            TO STRING-PTR                                      
050800     STRING                                                               
050900       'DEST('               DELIMITED BY SIZE                            
051000       W-DESTINATION         DELIMITED BY SPACE                           
051100       '),CLASS('            DELIMITED BY SIZE                            
051200       W-CLASS               DELIMITED BY SIZE                            
051300       '),FORMS('            DELIMITED BY SIZE                            
051400       W-FORMS               DELIMITED BY SPACE                           
051500       '),COPIES('           DELIMITED BY SIZE                            
051600       W-COPIES              DELIMITED BY SIZE                            
051700       '),OUTDISP('          DELIMITED BY SIZE                            
051800       W-OUTDISP             DELIMITED BY SPACE                           
051900       ','                   DELIMITED BY SIZE                            
052000       W-OUTDISP             DELIMITED BY SPACE                           
052100       '),'                  DELIMITED BY SIZE                            
052200       IAFP-MISC             DELIMITED BY SPACE                           
052300       INTO IAFP-PRTO-OUTDES-PARMS                                        
052400       WITH POINTER STRING-PTR                                            
052500     END-STRING                                                           
052600                                                                          
052700     IF W-PFDEF NOT = SPACE                                               
052800       STRING ',PAGEDEF('   DELIMITED BY SIZE                             
052900              W-PFDEF      DELIMITED BY SPACE                             
053000              '),FORMDEF(' DELIMITED BY SIZE                              
053100              W-PFDEF      DELIMITED BY SPACE                             
053200              ')'          DELIMITED BY SIZE                              
053300         INTO IAFP-PRTO-OUTDES-PARMS                                      
053400         WITH POINTER STRING-PTR                                          
053500       END-STRING                                                         
053600                                                                          
053700       MOVE W-DESTINATION TO TEST-PRINTER-NAME                            
053800       IF IMSID4 = 'IMG0'                                                 
053900         IF VX-PRINTER                                                    
054000* FÖR EV FRAMTIDA BEHOV                                                   
054100           MOVE 'W??.????.PSF'              TO W-USERLIB                  
054200         ELSE                                                             
054300           MOVE 'W.QASE.PSF'                TO W-USERLIB                  
054400         END-IF                                                           
054500       ELSE                                                               
054600         IF VX-PRINTER                                                    
054700* FÖR EV FRAMTIDA BEHOV                                                   
054800           MOVE 'W??.????.PSF,W??.????.PSF' TO W-USERLIB                  
054900         ELSE                                                             
055000           EVALUATE TRUE                                                  
055100             WHEN IMSID4 = 'IMB0'                                         
055200               MOVE 'W.ACPT.PSF,W.PROD.PSF' TO W-USERLIB                  
055300             WHEN IMSID4 = 'IMP0'                                         
055400               MOVE 'W.DEVE.PSF,W.PROD.PSF' TO W-USERLIB                  
055500             WHEN IMSID4 = 'IMD0'                                         
055600               MOVE 'W.XDEV.PSF,W.PROD.PSF' TO W-USERLIB                  
055700             WHEN OTHER                                                   
055800               MOVE 'W.IGRT.PSF,W.PROD.PSF' TO W-USERLIB                  
055900           END-EVALUATE                                                   
056000         END-IF                                                           
056100       END-IF                                                             
056200       STRING                                                             
056300         ',USERLIB('           DELIMITED BY SIZE                          
056400         W-USERLIB             DELIMITED BY SPACE                         
056500         ')'                   DELIMITED BY SIZE                          
056600         INTO IAFP-PRTO-OUTDES-PARMS                                      
056700         WITH POINTER STRING-PTR                                          
056800       END-STRING                                                         
056900     END-IF                                                               
057000                                                                          
057100*    --  LL = LENGTH OF LL-FIELD + STRING-PTR - 1                         
057200     ADD 1 STRING-PTR GIVING IAFP-PRTO-LL                                 
057300     COMPUTE IAFP-OPT-LL  = LENGTH OF IAFP-OPT-FIRST-PART                 
057400                          + IAFP-PRTO-LL                                  
057500                                                                          
057600     PERFORM IMS-CHANGE-SPOOL                                             
057700                                                                          
057800     IF STATUS-WS NOT = SPACE                                             
057900       MOVE 12 TO OUTP-KDRC                                               
058000       STRING                                                             
058100         'STATUS CODE ' STATUS-WS                                         
058200         ' ' IAFP-FEEDBACK-TEXT(1:100)                                    
058300         DELIMITED SIZE                                                   
058400         INTO OUTP-TEOUTDATA                                              
058500       END-STRING                                                         
058600     END-IF                                                               
058700     .                                                                    
058800     EJECT                                                                
058900 C-WRITE-VIA-ACIF    SECTION.                                             
059000                                                                          
059100*    When the record is meta data, dont process it. It's not              
059200*    needed here.                                                         
059300     IF OUTP-TEOUTDATA (1:5) = '¤META' OR                                 
059400        OUTP-TEOUTDATA (2:5) = '¤META'                                    
059500       CONTINUE                                                           
059600     ELSE                                                                 
059700       MOVE 'PUT'                TO SUBM-KDFUNC                           
059800       MOVE 1                    TO STRING-PTR                            
059900       MOVE SPACE                TO ACIF-DATALINE                         
060000       PERFORM UNTIL STRING-PTR > OUTP-TEOUTDATA-L                        
060100         COMPUTE ACIF-DATALENGTH = OUTP-TEOUTDATA-L                       
060200                                   - STRING-PTR + 1                       
060300         IF ACIF-DATALENGTH > 77                                          
060400           MOVE 77               TO ACIF-DATALENGTH                       
060500         END-IF                                                           
060600         MOVE OUTP-TEOUTDATA(STRING-PTR:ACIF-DATALENGTH)                  
060700                                 TO ACIF-DATAPIECE                        
060800         MOVE ACIF-DATALINE      TO SUBM-LINE                             
060900                                                                          
061000         CALL WZ20SUBM        USING SUBM-WZ20SUBM                         
061100                                                                          
061200         MOVE '*'                TO ACIF-DATACONT                         
061300         ADD ACIF-DATALENGTH     TO STRING-PTR                            
061400       END-PERFORM                                                        
061500     END-IF                                                               
061600     .                                                                    
061700     EJECT                                                                
061800 D-WRITE-VIA-IAFP    SECTION.                                             
061900                                                                          
062000*    When the record is meta data, dont process it. It's not              
062100*    needed here.                                                         
062200     IF OUTP-TEOUTDATA (1:5) = '¤META' OR                                 
062300        OUTP-TEOUTDATA (2:5) = '¤META'                                    
062400       CONTINUE                                                           
062500     ELSE                                                                 
062600       MOVE OUTP-TEOUTDATA (1:OUTP-TEOUTDATA-L) TO                        
062700            IMSMSG-IO-DATA                                                
062800       COMPUTE IMSMSG-RECORD-LL = OUTP-TEOUTDATA-L + 4                    
062900       COMPUTE IMSMSG-BLOCK-LL = IMSMSG-RECORD-LL + 4                     
063000                                                                          
063100       PERFORM IMS-WRITE-LINE                                             
063200                                                                          
063300       IF STATUS-WS NOT = SPACE                                           
063400         MOVE 12                 TO OUTP-KDRC                             
063500         STRING                                                           
063600           'STATUS CODE ' STATUS-WS                                       
063700           ' ' IAFP-FEEDBACK-TEXT(1:100)                                  
063800           DELIMITED SIZE                                                 
063900                               INTO OUTP-TEOUTDATA                        
064000         END-STRING                                                       
064100       END-IF                                                             
064200     END-IF                                                               
064300     .                                                                    
064400     EJECT                                                                
064500 E-CLOSE-ACIF        SECTION.                                             
064600                                                                          
064700     MOVE 'PUT'    TO SUBM-KDFUNC                                         
064800                                                                          
064900     MOVE SPACE TO SUBM-LINE                                              
065000     STRING                                                               
065100          '//IFDATA I'                                                    
065200          'F (DATA.WZ1405.RC = 0) THEN'       DELIMITED BY SIZE           
065300       INTO SUBM-LINE                                                     
065400     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
065500                                                                          
065600     MOVE SPACE TO SUBM-LINE                                              
065700     STRING                                                               
065800       '// EXEC WZ11ACIF,DSIN=&&DATA,'        DELIMITED BY SIZE           
065900       INTO SUBM-LINE                                                     
066000     END-STRING                                                           
066100     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
066200                                                                          
066300     IF  W-CHANNEL-PFDEF(OUTP-IDCALL) NOT = SPACE                         
066400       MOVE SPACE TO SUBM-LINE                                            
066500       STRING                                                             
066600         '// ACIFINDX='                        DELIMITED BY SIZE          
066700         W-CHANNEL-PFDEF(OUTP-IDCALL)          DELIMITED BY SPACE         
066800         ','                                   DELIMITED BY SIZE          
066900         INTO SUBM-LINE                                                   
067000       END-STRING                                                         
067100       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
067200     END-IF                                                               
067300                                                                          
067400     MOVE SPACE TO SUBM-LINE                                              
067500     STRING                                                               
067600       '// CLASS='                             DELIMITED BY SIZE          
067700       W-CHANNEL-CLASS(OUTP-IDCALL)            DELIMITED BY SIZE          
067800       ',DEST='                                DELIMITED BY SIZE          
067900       W-CHANNEL-DESTINATION(OUTP-IDCALL)      DELIMITED BY SPACE         
068000       ',COPIES='                              DELIMITED BY SIZE          
068100       W-CHANNEL-COPIES(OUTP-IDCALL)           DELIMITED BY SPACE         
068200       ',FORMS='                               DELIMITED BY SIZE          
068300       W-CHANNEL-FORMS(OUTP-IDCALL)            DELIMITED BY SPACE         
068400       ',OUTDISP='                             DELIMITED BY SIZE          
068500       W-CHANNEL-OUTDISP(OUTP-IDCALL)          DELIMITED BY SPACE         
068600       INTO SUBM-LINE                                                     
068700     END-STRING                                                           
068800     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
068900                                                                          
069000     MOVE '//ACIFCMDS DD *'  TO SUBM-LINE                                 
069100     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
069200                                                                          
069300     IF W-CHANNEL-FLCARRCNTL(OUTP-IDCALL) = YES                           
069400       MOVE 'YES'  TO ACIF-CC                                             
069500     ELSE                                                                 
069600       MOVE 'NO'   TO ACIF-CC                                             
069700     END-IF                                                               
069800     MOVE SPACE TO SUBM-LINE                                              
069900     STRING                                                               
070000       ' CC='                                  DELIMITED BY SIZE          
070100       ACIF-CC                                 DELIMITED BY SPACE         
070200       INTO SUBM-LINE                                                     
070300     END-STRING                                                           
070400     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
070500                                                                          
070600     MOVE SPACE TO SUBM-LINE                                              
070700     STRING                                                               
070800       ' USERLIB='                             DELIMITED BY SIZE          
070900       W-CHANNEL-USERLIB(OUTP-IDCALL)          DELIMITED BY SPACE         
071000       INTO SUBM-LINE                                                     
071100     END-STRING                                                           
071200     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
071300                                                                          
071400     IF  W-CHANNEL-PFDEF(OUTP-IDCALL) NOT = SPACE                         
071500       MOVE SPACE TO SUBM-LINE                                            
071600       STRING                                                             
071700         ' FORMDEF=F1'                         DELIMITED BY SIZE          
071800         W-CHANNEL-PFDEF(OUTP-IDCALL)          DELIMITED BY SPACE         
071900         INTO SUBM-LINE                                                   
072000       END-STRING                                                         
072100       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
072200                                                                          
072300       MOVE SPACE TO SUBM-LINE                                            
072400       STRING                                                             
072500         ' PAGEDEF=P1'                         DELIMITED BY SIZE          
072600         W-CHANNEL-PFDEF(OUTP-IDCALL)          DELIMITED BY SPACE         
072700         INTO SUBM-LINE                                                   
072800       END-STRING                                                         
072900       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
073000     END-IF                                                               
073100                                                                          
073200     MOVE SPACE TO SUBM-LINE                                              
073300     STRING                                                               
073400          '//SIGNAL I'                                                    
073500          'F ABEND THEN '                    DELIMITED BY SIZE            
073600       INTO SUBM-LINE                                                     
073700     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
073800                                                                          
073900     MOVE '//SIGNAL  EXEC WSOP'        TO SUBM-LINE                       
074000     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
074100                                                                          
074200     MOVE ' ORDER WZ11JABE SYMBOLS'    TO SUBM-LINE                       
074300     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
074400                                                                          
074500     MOVE ' ACTION(printing)'          TO SUBM-LINE                       
074600     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
074700                                                                          
074800     MOVE SPACE TO SUBM-LINE                                              
074900     STRING                                                               
075000       ' DEST('                              DELIMITED BY SIZE            
075100       FUNCTION TRIM(W-CHANNEL-DESTINATION(OUTP-IDCALL))                  
075200                                             DELIMITED BY SIZE            
075300       ')'                                   DELIMITED BY SIZE            
075400       INTO SUBM-LINE                                                     
075500     END-STRING                                                           
075600     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
075700                                                                          
075800     MOVE SPACE TO SUBM-LINE                                              
075900     STRING                                                               
076000       ' TIMESTAMP('                         DELIMITED BY SIZE            
076100       W-YYMMDD                              DELIMITED BY SIZE            
076200       ' '                                   DELIMITED BY SIZE            
076300       W-HHMMSS                              DELIMITED BY SIZE            
076400       ') JOBNAME(WZ11'                      DELIMITED BY SIZE            
076500       W-HHMMSS(3:4)                         DELIMITED BY SIZE            
076600       ')'                                   DELIMITED BY SIZE            
076700       INTO SUBM-LINE                                                     
076800     END-STRING                                                           
076900     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
077000                                                                          
077100     IF W-CHANNEL-IDOUTTYPE = SPACES                                      
077200       MOVE ' INFO( )'                 TO SUBM-LINE                       
077300       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
077400                                                                          
077500       MOVE ' TYPE( )'                 TO SUBM-LINE                       
077600       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
077700                                                                          
077800       MOVE ' REC( )'                  TO SUBM-LINE                       
077900       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
078000                                                                          
078100       MOVE ' LIST( )'                 TO SUBM-LINE                       
078200       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
078300                                                                          
078400       MOVE ' REGDAT( )'               TO SUBM-LINE                       
078500       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
078600                                                                          
078700       MOVE ' KLOCK( )'                TO SUBM-LINE                       
078800       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
078900     ELSE                                                                 
079000       MOVE ' INFO(Restart Keys:)'     TO SUBM-LINE                       
079100       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
079200                                                                          
079300       MOVE SPACE TO SUBM-LINE                                            
079400       STRING                                                             
079500         ' TYPE(Output Type    : '           DELIMITED BY SIZE            
079600         FUNCTION TRIM(W-CHANNEL-IDOUTTYPE)                               
079700                                             DELIMITED BY SIZE            
079800         ')'                                 DELIMITED BY SIZE            
079900         INTO SUBM-LINE                                                   
080000       END-STRING                                                         
080100       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
080200                                                                          
080300       MOVE SPACE TO SUBM-LINE                                            
080400       STRING                                                             
080500         ' REC(Output SubType : '            DELIMITED BY SIZE            
080600         FUNCTION TRIM(W-CHANNEL-IDOUTREC)                                
080700                                             DELIMITED BY SIZE            
080800         ')'                                 DELIMITED BY SIZE            
080900         INTO SUBM-LINE                                                   
081000       END-STRING                                                         
081100       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
081200                                                                          
081300       MOVE SPACE TO SUBM-LINE                                            
081400       STRING                                                             
081500         ' LIST(Output Id      : '           DELIMITED BY SIZE            
081600         FUNCTION TRIM(W-CHANNEL-IDLIST)                                  
081700                                             DELIMITED BY SIZE            
081800         ')'                                 DELIMITED BY SIZE            
081900         INTO SUBM-LINE                                                   
082000       END-STRING                                                         
082100       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
082200                                                                          
082300       MOVE SPACE TO SUBM-LINE                                            
082400       STRING                                                             
082500         ' REGDAT(Date           : '         DELIMITED BY SIZE            
082600         W-CHANNEL-TIREGDAT                  DELIMITED BY SPACE           
082700         ')'                                 DELIMITED BY SIZE            
082800         INTO SUBM-LINE                                                   
082900       END-STRING                                                         
083000       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
083100                                                                          
083200       MOVE SPACE TO SUBM-LINE                                            
083300       STRING                                                             
083400         ' KLOCK(Time           : '          DELIMITED BY SIZE            
083500         W-CHANNEL-TIKLOCK                   DELIMITED BY SPACE           
083600         ')'                                 DELIMITED BY SIZE            
083700         INTO SUBM-LINE                                                   
083800       END-STRING                                                         
083900       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
084000     END-IF                                                               
084100                                                                          
084200     MOVE ' END-ORDER'                 TO SUBM-LINE                       
084300     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
084400                                                                          
084500     MOVE '//SIGNAL ENDIF'             TO SUBM-LINE                       
084600     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
084700                                                                          
084800     MOVE '//IFDATA ENDIF' TO SUBM-LINE                                   
084900     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
085000                                                                          
085100                                                                          
085200     MOVE 'CLOSE'  TO SUBM-KDFUNC                                         
085300     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
085400     .                                                                    
085500     EJECT                                                                
085600 IMS-CHANGE-SPOOL SECTION.                                                
085700                                                                          
085800     MOVE 128         TO AIB-LEN                                          
085900     MOVE SPACE       TO AIB-SUB-FUNCTION                                 
086000     MOVE W-ALTPCB    TO AIB-PCB-NAME                                     
086100     MOVE IMSMSG-BLOCK-LL TO AIB-IOAREA-USED                              
086200                                                                          
086300     MOVE '  AX' TO VALID-STATUS-CODES                                    
086400                                                                          
086500     CALL AIBTDLI USING CHNG  AIB-AREA                                    
086600                        IAFP-DEST                                         
086700                        IAFP-OPT-LIST                                     
086800                        IAFP-FEEDBACK                                     
086900                                                                          
087000     SET ADDRESS OF ALT-PCB TO AIB-PCB-PTR                                
087100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
087200     PERFORM IMS-STATUS-CHECK                                             
087300     .                                                                    
087400     EJECT                                                                
087500 IMS-WRITE-LINE     SECTION.                                              
087600                                                                          
087700     MOVE 128         TO AIB-LEN                                          
087800     MOVE SPACE       TO AIB-SUB-FUNCTION                                 
087900     MOVE W-ALTPCB    TO AIB-PCB-NAME                                     
088000     MOVE IMSMSG-BLOCK-LL TO AIB-IOAREA-USED                              
088100                                                                          
088200     MOVE '  AX' TO VALID-STATUS-CODES                                    
088300                                                                          
088400     CALL AIBTDLI USING ISRT AIB-AREA                                     
088500                        IMSMSG-IO-AREA                                    
088600                                                                          
088700     SET ADDRESS OF ALT-PCB TO AIB-PCB-PTR                                
088800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
088900     PERFORM IMS-STATUS-CHECK                                             
089000     .                                                                    
089100                                                                          
089200     EJECT                                                                
089300 IMS-STATUS-CHECK   SECTION.                                              
089400                                                                          
089500     IF STATUS-WS = LOW-VALUE                                             
089600       STRING ' CAN NOT FIND PCB WITH NAME: '                             
089700              AIB-PCB-NAME ' IN THE PSB.'                                 
089800         DELIMITED BY SIZE INTO ERROR-TEXT                                
089900         DISPLAY ERROR-TEXT UPON CONSOLE                                  
090000         CALL FELLOG                                                      
090100     ELSE                                                                 
090200       SET STATUS-IX TO 1                                                 
090300       SEARCH VALID-STATUS                                                
090400         AT END                                                           
090500           STRING 'INVALID STATUS CODE FROM IMS:' STATUS-WS               
090600           '. (ALSO SEE IAFP-FEEDBACK AREA BELOW)'                        
090700           DELIMITED BY SIZE INTO ERROR-TEXT                              
090800           DISPLAY ERROR-TEXT UPON CONSOLE                                
090900           DISPLAY IAFP-FEEDBACK-TEXT(1:100) UPON CONSOLE                 
091000           CALL FELLOG                                                    
091100         WHEN VALID-STATUS (STATUS-IX) = STATUS-WS                        
091200           CONTINUE                                                       
091300       END-SEARCH                                                         
091400     END-IF                                                               
091500     .                                                                    
091600                                                                          

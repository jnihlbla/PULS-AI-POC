000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ11OUTM.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   02/12/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM HANDLES PRINT-OUTPUT TO MAIL.                       
000900*        IT SUBMITS A JOB WHITH VARYING JCL DEPENDING ON THE TYPE         
001000*        OF MAIL. THE JOB MAY USE ACIF AND CA-SPOOL                       
001100*        (PROC WZ11PDF) TO CREATE A PDF FILE.                             
001200*        AND/OR WZ11TZIP OR WZ11BZIP TO CREATE A ZIP FILE,                
001300*        AND FINALLY SMTPAPIX (WMAILSND) TO SEND A MAIL WITH              
001400*        THE DATA INLINE OR AS AN ATTACHMENT.                             
001500*        1-15 SIMULTANEOUS OUTPUTS ("CHANNELS") CAN BE HANDLED            
001600*                                                                         
001700*        CCID:                                                            
001800*        5173755 - AVVECKLING AV MEMO.                                    
001900*                  OMD÷PT FR≈N TIDIGARE NAMN WZ11OUTN.                    
002000*                  GAMLA WZ11OUTM UTG≈R                                   
002100*        6489902 - ZIPPADE ATTACHMENTS TILLAGT                            
002200*        9098722 - (jan-feb 2010) NEW PDF TOOL AND NEW PROCEDURES         
002300                                                                          
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 CONFIGURATION SECTION.                                                   
002800 SPECIAL-NAMES.                                                           
002900     CLASS ALPHANUM-UPPER-AND-SPACE                                       
003000           IS ' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'.                    
003100                                                                          
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'WZ11OUTM'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003900 77  ERROR-TEXT-START            PIC X(8)  VALUE 'ERROR:  '.              
004000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004100 77  KDRC-DISPLAY                PIC Z(5).                                
004200                                                                          
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004500 77  TRIPPEL-APOSTROPH           PIC XXX     VALUE ''''''''.              
004600                                                                          
004700 01  SMALL-LETTERS               PIC X(31)  VALUE                         
004800     'abcdefghijklmnopqrstuvwxyzÂ‰ˆ¸È'.                                   
004900 01  CAPS-LETTERS                PIC X(31)  VALUE                         
005000     'ABCDEFGHIJKLMNOPQRSTUVWXYZ≈ƒ÷‹…'.                                   
005100                                                                          
005200 01  STRING-PTR                  PIC S9(4)   BINARY.                      
005300 01  IX                          PIC S9(4)   BINARY.                      
005400 01  PCOUNTER                    PIC S9(4)   BINARY.                      
005500                                                                          
005600 01  W-YYMMDD                    PIC X(6).                                
005700 01  W-HHMMSS                    PIC X(6).                                
005800 01  W-TH                        PIC X(2).                                
005900 01  W-COMMA-OR-SPACE            PIC X.                                   
006000 01  W-ATTACH-SUFF               PIC X(8).                                
006100 01  W-CONTENT-SUFF              PIC X(8).                                
006200 01  W-IDPFDEF                   PIC X(8).                                
006300 01  ZONED-CHANNEL               PIC 9(3).                                
006400                                                                          
006500                                                                          
006600     EJECT                                                                
006700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007100     03  VIMSID                  PIC X(8)    VALUE 'VIMSID  '.            
007200     03  WZ20SUBM                PIC X(8)    VALUE 'WZ20SUBM'.            
007300     03  W980USPA                PIC X(8)    VALUE 'W980USPA'.            
007400                                                                          
007500*    --- PARAMETERS TO ABEND                                              
007600 77  RCODE-ABEND-NO-DUMP         PIC S9(4)   COMP VALUE +16.              
007700 77  RCODE-ABEND-WITH-DUMP       PIC S9(4)   COMP VALUE +1000.            
007800                                                                          
007900*    --- PARAMETERS TO VIMSID                                             
008000 01  VIMSID-PARM.                                                         
008100   03  IMSID4                    PIC X(4)    VALUE SPACE.                 
008200   03  FILLER                    PIC X(4)    VALUE SPACE.                 
008300                                                                          
008400*    --- PARAMETERS TO WZ20SUBM                                           
008500 01  -COPY WZ20SUBM                                                       
008600                                                                          
008700     EJECT                                                                
008800*    -- THE CHANNEL TABLE IS USED TO SAVE DATA FROM OPEN CALL             
008900*    -- UNTIL IT IS NEEDED AT CLOSING TIME                                
009000 01  W-CHANNEL-AREA              PIC X(16)   VALUE                        
009100                                 'CHANNELS-START  '.                      
009200 01  W-CHANNEL-IDOUTTYPE         PIC X(15).                               
009300 01  W-CHANNEL-IDOUTREC          PIC X(30).                               
009400 01  W-CHANNEL-IDLIST            PIC X(10).                               
009500 01  W-CHANNEL-TIREGDAT          PIC 9(6).                                
009600 01  W-CHANNEL-TIKLOCK           PIC 9(8).                                
009700 01  W-CHANNEL-TAB.                                                       
009800   03  FILLER        OCCURS 15.                                           
009900     05 W-CHANNEL-DSLUX          PIC X(30).                               
010000     05 W-CHANNEL-ATTNAME        PIC X(30).                               
010100     05 W-CHANNEL-CONTENT        PIC X(30).                               
010200     05 W-CHANNEL-JCLTYPE        PIC X(8).                                
010300     05 W-CHANNEL-IDOUTDEST      PIC X(60).                               
010400     05 W-CHANNEL-IDMAIL-SENDER  PIC X(50).                               
010500     05 W-CHANNEL-IDMAILTTL      PIC X(50).                               
010600     05 W-CHANNEL-LAYOUT         PIC X(6).                                
010700     05 W-CHANNEL-FLCARRCNTL     PIC X.                                   
010800     05 W-CHANNEL-EMULATE-CC     PIC X.                                   
010900     05 W-CHANNEL-ESFADDR        PIC X.                                   
011000     05 W-CHANNEL-TEFAX          OCCURS 5 TIMES                           
011100                                 PIC X(50).                               
011200                                                                          
011300     EJECT                                                                
011400 01  WORK-CC                     PIC X(3).                                
011500                                                                          
011600 01  WORK-JOBNAME                PIC X(8)   VALUE                         
011700                                  'WZ11NNNN'.                             
011800 01  WORK-ACCOUNT                PIC X(30)  VALUE                         
011900                                  '510WOS39000WZ11OUTM'.                  
012000 01  WORK-MSGCLASS-CLASS         PIC X(30)  VALUE                         
012100                                  'MSGCLASS=H,CLASS=S'.                   
012200 01  WORK-JOBFORMS               PIC X(4)   VALUE                         
012300                                  'STD '.                                 
012400 01  WORK-PROCLIBS               PIC X(50)  VALUE                         
012500                                  'W.XXXX.PROCLIB,W.PROD.PROCLIB'.        
012600 01  WORK-ENV                    PIC X(8)   VALUE                         
012700                                  'ENVXXXX '.                             
012800 01  WORK-SYSTZ                  PIC X(8)   VALUE                         
012900                                  'SYSTZXX '.                             
013000 01  WORK-USERLIB                PIC X(50)  VALUE                         
013100                                  'W.XXXX.PSF'.                           
013200 01  WORK-ROUTE-XEQ              PIC X(8)   VALUE                         
013300                                  'LOCAL'.                                
013400 01  WORK-HLQ                    PIC X(16)  VALUE                         
013500                                  'W.XXXX.PDF.'.                          
013600                                                                          
013700*    -- AREA FOR STORING 80-BYTE PIECES OF THE DATA RECORDS               
013800 01  WORK-DATALINE.                                                       
013900*                                -- X MEANS CONTINUATION                  
014000*                                -- SPACE MEANS FIRST PIECE               
014100   03  WORK-DATACONT             PIC X.                                   
014200*                                -- LENGTH OF THIS PIECE                  
014300   03  WORK-DATALENGTH           PIC 9(4)   BINARY.                       
014400   03  WORK-DATAPIECE            PIC X(77).                               
014500                                                                          
014600     EJECT                                                                
014700 01  -COPY  W980USPA                                                      
014800                                                                          
014900     EJECT                                                                
015000 LINKAGE SECTION.                                                         
015100*01  -COPY  WZ11OUTM                                                      
015200     EJECT                                                                
015300*    -COPY W0009  -PRE ALT-                                               
015400     EJECT                                                                
015500 PROCEDURE DIVISION  USING OUTM-WZ11OUT.                                  
015600 MAIN SECTION.                                                            
015700                                                                          
015800     PERFORM A-INIT                                                       
015900                                                                          
016000     EVALUATE OUTM-KDFUNC                                                 
016100       WHEN 'OPEN'   PERFORM B-OPEN-CHANNEL                               
016200       WHEN 'PUT'    PERFORM C-WRITE                                      
016300       WHEN 'CLOSE'  PERFORM E-CLOSE                                      
016400     END-EVALUATE                                                         
016500                                                                          
016600     MOVE OUTM-KDRC TO RETURN-CODE                                        
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000 A-INIT SECTION.                                                          
017100                                                                          
017200     IF OUTM-IDCALL < 1 OR > 15                                           
017300       MOVE OUTM-IDCALL TO KDRC-DISPLAY                                   
017400       STRING 'WZ11OUTM INVALID IDCALL VALUE: '                           
017500              KDRC-DISPLAY                                                
017600          DELIMITED BY SIZE                                               
017700          INTO ERROR-TEXT                                                 
017800       CALL ABEND USING RCODE-ABEND-WITH-DUMP                             
017900     END-IF                                                               
018000                                                                          
018100*    -- WZ20SUBM USES SAME IDCALL NUMBER AS THIS PROGRAM                  
018200     MOVE OUTM-IDCALL      TO SUBM-IDCALL                                 
018300                                                                          
018400*    -- FIND OUT WHICH IMS SYSTEM WE ARE USING                            
018500     CALL VIMSID USING VIMSID-PARM                                        
018600                                                                          
018700*    -- SO FAR, SO GOOD                                                   
018800     MOVE ZERO TO OUTM-KDRC                                               
018900     .                                                                    
019000     EJECT                                                                
019100 B-OPEN-CHANNEL SECTION.                                                  
019200                                                                          
019300*    -- USE CURRENT TIMESTAMP TO AS PART OF JOBNAME AND                   
019400*    -- ATTATCHED FILES. SAVE IT NOW FOR LATER USE.                       
019500     MOVE FUNCTION CURRENT-DATE(3:6)  TO W-YYMMDD                         
019600     MOVE FUNCTION CURRENT-DATE(9:6)  TO W-HHMMSS                         
019700     MOVE FUNCTION CURRENT-DATE(15:2) TO W-TH                             
019800                                                                          
019900     MOVE OUTM-IDOUTTYPE   TO W-CHANNEL-IDOUTTYPE                         
020000     MOVE OUTM-IDOUTREC    TO W-CHANNEL-IDOUTREC                          
020100     MOVE OUTM-IDLIST      TO W-CHANNEL-IDLIST                            
020200     MOVE OUTM-TIREGDAT    TO W-CHANNEL-TIREGDAT                          
020300     MOVE OUTM-TIKLOCK     TO W-CHANNEL-TIKLOCK                           
020400                                                                          
020500     MOVE 'no.dest'        TO W-CHANNEL-IDOUTDEST(OUTM-IDCALL)            
020600     IF OUTM-IDOUTDEST NOT = SPACE                                        
020700       MOVE OUTM-IDOUTDEST TO W-CHANNEL-IDOUTDEST(OUTM-IDCALL)            
020800     END-IF                                                               
020900                                                                          
021000     IF OUTM-IDMAIL-SENDER = SPACE OR ALL '+'                             
021100       MOVE SPACE        TO W-CHANNEL-IDMAIL-SENDER(OUTM-IDCALL)          
021200     ELSE                                                                 
021300       MOVE OUTM-IDMAIL-SENDER                                            
021400                         TO W-CHANNEL-IDMAIL-SENDER(OUTM-IDCALL)          
021500       IF OUTM-IDMAIL-SENDER(50:1) NOT = SPACE                            
021600         MOVE 'sender.too.long@volvo.com'                                 
021700                         TO W-CHANNEL-IDMAIL-SENDER(OUTM-IDCALL)          
021800       END-IF                                                             
021900     END-IF                                                               
022000                                                                          
022100     MOVE  SPACE TO W-COMMA-OR-SPACE                                      
022200     MOVE  SPACE TO W-CONTENT-SUFF                                        
022300     MOVE  SPACE TO W-ATTACH-SUFF                                         
022400     IF OUTM-IDPFDEF NOT = SPACE                                          
022500*      -- A PAGEDEF/FORMSDEF LAYOUT IS SPECIFIED                          
022600       IF OUTM-IDPFDEF(1:1) = '.'                                         
022700*         -- IT IS A FILE TYPE, NOT A LAYOUT                              
022800         MOVE FUNCTION UPPER-CASE(OUTM-IDPFDEF) TO W-IDPFDEF              
022900         MOVE ZERO TO TALLY                                               
023000         INSPECT W-IDPFDEF TALLYING TALLY FOR CHARACTERS                  
023100                 BEFORE 'ZIP'                                             
023200         IF TALLY < 8                                                     
023300*          -- A ZIPPED ATTACHMENT EXTRACT FILE SUFFIX                     
023400           MOVE OUTM-IDPFDEF(1:TALLY) TO W-CONTENT-SUFF                   
023500           IF W-CONTENT-SUFF = '.'                                        
023600             MOVE '.TXT'  TO W-CONTENT-SUFF                               
023700           END-IF                                                         
023800           MOVE '.zip' TO W-ATTACH-SUFF                                   
023900         ELSE                                                             
024000*          -- USE FILE TYPE AS IT IS                                      
024100           MOVE OUTM-IDPFDEF TO W-ATTACH-SUFF                             
024200         END-IF                                                           
024300         MOVE NOO          TO W-CHANNEL-EMULATE-CC(OUTM-IDCALL)           
024400                                                                          
024500         EVALUATE W-IDPFDEF                                               
024600          WHEN  '.ZIP'                                                    
024700          WHEN  '.TXTZIP'                                                 
024800          WHEN  '.CSVZIP'                                                 
024900          WHEN  '.HTMZIP'                                                 
025000          WHEN  '.HTMLZIP'                                                
025100*          -- ZIPPED TEXT ATTACHMENT                                      
025200           MOVE 'TXTZIP'     TO W-CHANNEL-JCLTYPE(OUTM-IDCALL)            
025300           MOVE YES          TO W-CHANNEL-EMULATE-CC(OUTM-IDCALL)         
025400                                                                          
025500          WHEN '.PDFZIP'                                                  
025600*          -- ZIPPED PDF FILE ATTACHMENT                                  
025700*          -- USE A STANDARD LAYOUT FOR PDF                               
025800           MOVE 'SLS08C'     TO W-CHANNEL-LAYOUT(OUTM-IDCALL)             
025900           MOVE 'PDFZIP'     TO W-CHANNEL-JCLTYPE(OUTM-IDCALL)            
026000                                                                          
026100          WHEN OTHER                                                      
026200            IF TALLY = 8                                                  
026300*          -- LAYOUT IS A NORMAL FILE TYPE.                               
026400*          -- SEND UNFORMATTED AS ATTACHMENT WITH SPECIFIED TYPE          
026500              MOVE 'TXTATT'  TO W-CHANNEL-JCLTYPE(OUTM-IDCALL)            
026600              MOVE YES       TO W-CHANNEL-EMULATE-CC(OUTM-IDCALL)         
026700              MOVE  ','      TO W-COMMA-OR-SPACE                          
026800            ELSE                                                          
026900*          -- LAYOUT IS ZIP FILE WITH PROBABLY BINARY CONTENT             
027000              MOVE 'BINZIP'  TO W-CHANNEL-JCLTYPE(OUTM-IDCALL)            
027100            END-IF                                                        
027200         END-EVALUATE                                                     
027300       ELSE                                                               
027400*        -- NORMAL FORMDEF - SEND FORMATTED AS PDF ATTACHMENT             
027500         MOVE 'PDFATT'     TO W-CHANNEL-JCLTYPE(OUTM-IDCALL)              
027600         IF OUTM-IDPFDEF(1:1) = "*"                                       
027700*          -- FIX FOR SOME LANDSCAPE FORMS. SKIP LEADING *                
027800           MOVE OUTM-IDPFDEF(2:) TO W-CHANNEL-LAYOUT(OUTM-IDCALL)         
027900           MOVE YES             TO W-CHANNEL-ESFADDR(OUTM-IDCALL)         
028000         ELSE                                                             
028100           MOVE OUTM-IDPFDEF    TO W-CHANNEL-LAYOUT(OUTM-IDCALL)          
028200           MOVE NOO             TO W-CHANNEL-ESFADDR(OUTM-IDCALL)         
028300         END-IF                                                           
028400         INSPECT W-CHANNEL-LAYOUT(OUTM-IDCALL)                            
028500           CONVERTING SMALL-LETTERS TO CAPS-LETTERS                       
028600         MOVE NOO          TO W-CHANNEL-EMULATE-CC(OUTM-IDCALL)           
028700         MOVE '.pdf'       TO W-ATTACH-SUFF                               
028800       END-IF                                                             
028900     ELSE                                                                 
029000*      -- NO LAYOUT - SEND AS UNFORMATTED INLINE TEXT                     
029100       MOVE 'INLINE'       TO W-CHANNEL-JCLTYPE(OUTM-IDCALL)              
029200       MOVE SPACE          TO W-CHANNEL-LAYOUT(OUTM-IDCALL)               
029300       MOVE YES            TO W-CHANNEL-EMULATE-CC(OUTM-IDCALL)           
029400     END-IF                                                               
029500                                                                          
029600     MOVE OUTM-TEFAX(1)    TO W-CHANNEL-TEFAX(OUTM-IDCALL, 1)             
029700     MOVE OUTM-TEFAX(2)    TO W-CHANNEL-TEFAX(OUTM-IDCALL, 2)             
029800     MOVE OUTM-TEFAX(3)    TO W-CHANNEL-TEFAX(OUTM-IDCALL, 3)             
029900     MOVE OUTM-TEFAX(4)    TO W-CHANNEL-TEFAX(OUTM-IDCALL, 4)             
030000     MOVE OUTM-TEFAX(5)    TO W-CHANNEL-TEFAX(OUTM-IDCALL, 5)             
030100                                                                          
030200     MOVE 'no title'       TO W-CHANNEL-IDMAILTTL(OUTM-IDCALL)            
030300     IF OUTM-IDMAILTTL NOT = SPACE                                        
030400       MOVE OUTM-IDMAILTTL TO W-CHANNEL-IDMAILTTL(OUTM-IDCALL)            
030500     END-IF                                                               
030600                                                                          
030700*    -- SAVE CARRIAGE CONTROL USAGE FOR LATER USE                         
030800     MOVE OUTM-FLCARRCNTL TO W-CHANNEL-FLCARRCNTL(OUTM-IDCALL)            
030900                                                                          
031000*    -- PREPARE JCL TO SUBMIT AN ACIF JOB                                 
031100                                                                          
031200     MOVE 'LOCAL'                     TO WORK-ROUTE-XEQ                   
031300     IF IMSID4 = 'IMG0'                                                   
031400       MOVE '540WZ010100WZ11OUTM'     TO WORK-ACCOUNT                     
031500       MOVE 'CLASS=K'                 TO WORK-MSGCLASS-CLASS              
031600       MOVE '1800'                    TO WORK-JOBFORMS                    
031700       MOVE 'W.QASE.PSF'              TO WORK-USERLIB                     
031800       MOVE 'W.QASE.PROCLIB'          TO WORK-PROCLIBS                    
031900       MOVE 'ENVQASE'                 TO WORK-ENV                         
032000       MOVE 'SYSTZ'                   TO WORK-SYSTZ                       
032100       MOVE 'WZ11.PDF.'               TO WORK-HLQ                         
032200     ELSE                                                                 
032300       MOVE '510WOS39000WZ11OUTM'     TO WORK-ACCOUNT                     
032400       MOVE 'MSGCLASS=H,CLASS=N'      TO WORK-MSGCLASS-CLASS              
032500       MOVE 'STD'                     TO WORK-JOBFORMS                    
032600       IF IMSID4 = 'IMB0'                                                 
032700         MOVE 'W.ACPT.PSF,W.QASE.PSF'    TO WORK-USERLIB                  
032800         MOVE 'W.ACPT.PROCLIB,W.QASE.PROCLIB' TO WORK-PROCLIBS            
032900         MOVE 'ENVACPT'                   TO WORK-ENV                     
033000         MOVE 'SYSTZAA'                   TO WORK-SYSTZ                   
033100         MOVE 'W.ACPT.PDF.'               TO WORK-HLQ                     
033200       ELSE                                                               
033300         IF IMSID4 = 'IMD0'                                               
033400           MOVE 'W.XDEV.PSF,W.PROD.PSF'  TO WORK-USERLIB                  
033500           MOVE 'W.XDEV.PROCLIB,W.ACPT.PROCLIB,W.PROD.PROCLIB'            
033600                 TO WORK-PROCLIBS                                         
033700           MOVE 'ENVXDEV'                TO WORK-ENV                      
033800           MOVE 'SYSTZXX'                TO WORK-SYSTZ                    
033900           MOVE 'W.XDEV.PDF.'            TO WORK-HLQ                      
034000         ELSE                                                             
034100           IF IMSID4 = 'IMP0'                                             
034200             MOVE 'W.DEVE.PSF,W.PROD.PSF' TO WORK-USERLIB                 
034300             MOVE 'W.DEVE.PROCLIB,W.IGRT.PROCLIB,W.PROD.PROCLIB'          
034400                  TO WORK-PROCLIBS                                        
034500             MOVE 'ENVDEVE'                TO WORK-ENV                    
034600             MOVE 'SYSTZDD'                TO WORK-SYSTZ                  
034700             MOVE 'W.DEVE.PDF.'            TO WORK-HLQ                    
034800           ELSE                                                           
034900             MOVE 'W.IGRT.PSF,W.PROD.PSF' TO WORK-USERLIB                 
035000             MOVE 'W.IGRT.PROCLIB,W.ACPT.PROCLIB,W.PROD.PROCLIB'          
035100                  TO WORK-PROCLIBS                                        
035200             MOVE 'ENVIGRT'                TO WORK-ENV                    
035300             MOVE 'SYSTZTT'                TO WORK-SYSTZ                  
035400             MOVE 'W.IGRT.PDF.'            TO WORK-HLQ                    
035500           END-IF                                                         
035600         END-IF                                                           
035700       END-IF                                                             
035800     END-IF                                                               
035900                                                                          
036000                                                                          
036100*    -- ATTACHMENT NAME IS Dyymmdd.Thhmmss.type                           
036200*    -- (THIS NAME IS MAKES IT POSSIBLE TO DIFFRENTIATE BETWEEN           
036300*    -- ATTACHMENTS FROM DIFFERENT TRANSMISSIONS BUT IS THE SAME          
036400*    -- FOR ALL CHANNELS/MAILS IN THIS TRANSMISSION)                      
036500     MOVE SPACE TO W-CHANNEL-ATTNAME(OUTM-IDCALL)                         
036600     STRING 'D' W-YYMMDD '-T' W-HHMMSS                                    
036700                            DELIMITED BY SIZE                             
036800            W-ATTACH-SUFF   DELIMITED BY SPACE                            
036900       INTO W-CHANNEL-ATTNAME(OUTM-IDCALL)                                
037000                                                                          
037100*    -- FOR ZIP, THE NAME OF THE CONTENT MUST ALSO BE SPECIFIED           
037200*    -- THE CONTENT NAME IS Dyymmdd-Thhmmss.type                          
037300     MOVE SPACE TO W-CHANNEL-CONTENT(OUTM-IDCALL)                         
037400     STRING 'D' W-YYMMDD '.T' W-HHMMSS                                    
037500                            DELIMITED BY SIZE                             
037600            W-CONTENT-SUFF   DELIMITED BY SPACE                           
037700       INTO W-CHANNEL-CONTENT(OUTM-IDCALL)                                
037800                                                                          
037900*    -- FOR PDF, THE NAME OF FILES IN THE LINUX ENVIRONMENT MUST          
038000*    -- ALSO BE SPECIFIED. THIS NAME IS ALSO USED AS PART OF              
038100*    -- THE OUTPUT PDF FILE IN THE MAINFRAME ENVIRONMENT.                 
038200*    -- THE FORMAT OF THE DSLUX NAME IS Dyymmdd.Thhmmss.Hthccc            
038300     MOVE SPACE TO W-CHANNEL-DSLUX(OUTM-IDCALL)                           
038400     MOVE OUTM-IDCALL TO ZONED-CHANNEL                                    
038500     STRING 'D' W-YYMMDD '.T' W-HHMMSS                                    
038600            '.H' W-TH ZONED-CHANNEL                                       
038700                            DELIMITED BY SIZE                             
038800       INTO W-CHANNEL-DSLUX(OUTM-IDCALL)                                  
038900                                                                          
039000     MOVE 'OPEN' TO SUBM-KDFUNC                                           
039100     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
039200                                                                          
039300     MOVE 'PUT'  TO SUBM-KDFUNC                                           
039400     MOVE SPACE TO SUBM-LINE                                              
039500     STRING 'WZ11' W-HHMMSS(3:4) DELIMITED BY SIZE                        
039600                                 INTO WORK-JOBNAME                        
039700     STRING                                                               
039800       '//WZ11' W-HHMMSS(3:4) ' JOB ('         DELIMITED BY SIZE          
039900       WORK-ACCOUNT                            DELIMITED BY SPACE         
040000       ',W100),'                               DELIMITED BY SIZE          
040100       QUOTE 'RTN WZ11S9' QUOTE ','            DELIMITED BY SIZE          
040200       INTO SUBM-LINE                                                     
040300     END-STRING                                                           
040400     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
040500                                                                          
040600     MOVE SPACE TO SUBM-LINE                                              
040700     CALL W980USPA USING USPA-AREA                                        
040800     MOVE SPACE TO SUBM-LINE                                              
040900     STRING                                                               
041000       '//  USER='                  DELIMITED BY SIZE                     
041100       USPA-IDUSER                  DELIMITED BY SPACE                    
041200       ',PASSWORD='                 DELIMITED BY SIZE                     
041300       USPA-IDPW                    DELIMITED BY SPACE                    
041400       ','                          DELIMITED BY SIZE                     
041500       INTO SUBM-LINE                                                     
041600     END-STRING                                                           
041700     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
041800                                                                          
041900     MOVE SPACE TO SUBM-LINE                                              
042000     STRING                                                               
042100       '// MSGLEVEL=(1,1),'                    DELIMITED BY SIZE          
042200       WORK-MSGCLASS-CLASS                     DELIMITED BY SPACE         
042300       INTO SUBM-LINE                                                     
042400     END-STRING                                                           
042500     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
042600                                                                          
042700     MOVE SPACE TO SUBM-LINE                                              
042800     STRING                                                               
042900       '/*JOBPARM FORMS='                      DELIMITED BY SIZE          
043000       WORK-JOBFORMS                           DELIMITED BY SPACE         
043100       ',LINECT=0,LINES=9999'                  DELIMITED BY SIZE          
043200       INTO SUBM-LINE                                                     
043300     END-STRING                                                           
043400     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
043500                                                                          
043600     MOVE SPACE TO SUBM-LINE                                              
043700     STRING                                                               
043800       '/*ROUTE XEQ '                          DELIMITED BY SIZE          
043900       WORK-ROUTE-XEQ                          DELIMITED BY SPACE         
044000       INTO SUBM-LINE                                                     
044100     END-STRING                                                           
044200     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
044300                                                                          
044400     IF IMSID4 = 'IMG0' OR 'IMB0'                                         
044500       MOVE '//*+JBS BIND VCC1'   TO SUBM-LINE                            
044600     END-IF                                                               
044700     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
044800                                                                          
044900     MOVE SPACE TO SUBM-LINE                                              
045000     STRING                                                               
045100       '//PROC JCLLIB ORDER=('                 DELIMITED BY SIZE          
045200       WORK-PROCLIBS                           DELIMITED BY SPACE         
045300       ')'                                     DELIMITED BY SIZE          
045400       INTO SUBM-LINE                                                     
045500     END-STRING                                                           
045600     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
045700                                                                          
045800     MOVE SPACE TO SUBM-LINE                                              
045900     STRING                                                               
046000       '//ENV   INCL'                          DELIMITED BY SIZE          
046100       'UDE MEMBER='                           DELIMITED BY SIZE          
046200       WORK-ENV                                DELIMITED BY SPACE         
046300       INTO SUBM-LINE                                                     
046400     END-STRING                                                           
046500     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
046600                                                                          
046700     MOVE SPACE TO SUBM-LINE                                              
046800     STRING                                                               
046900       '//ENV   INCL'                          DELIMITED BY SIZE          
047000       'UDE MEMBER='                           DELIMITED BY SIZE          
047100       WORK-SYSTZ                              DELIMITED BY SPACE         
047200       INTO SUBM-LINE                                                     
047300     END-STRING                                                           
047400     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
047500                                                                          
047600*    -- USE SPECIFIED SENDER INSTEAD OF DEFAULT FOR ENVIRONMENT           
047700     IF W-CHANNEL-IDMAIL-SENDER(OUTM-IDCALL) NOT = SPACE                  
047800       MOVE SPACE TO SUBM-LINE                                            
047900       STRING                                                             
048000         '// SET FROMDFLT='                    DELIMITED BY SIZE          
048100         TRIPPEL-APOSTROPH                     DELIMITED BY SIZE          
048200         'FROM='                               DELIMITED BY SIZE          
048300         W-CHANNEL-IDMAIL-SENDER(OUTM-IDCALL)  DELIMITED BY SPACE         
048400         TRIPPEL-APOSTROPH                     DELIMITED BY SIZE          
048500         INTO SUBM-LINE                                                   
048600       END-STRING                                                         
048700       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
048800     END-IF                                                               
048900                                                                          
049000     MOVE SPACE TO SUBM-LINE                                              
049100     STRING                                                               
049200       '//DATA    EXEC WZ14P005'               DELIMITED BY SIZE          
049300       W-COMMA-OR-SPACE                        DELIMITED BY SPACE         
049400       INTO SUBM-LINE                                                     
049500     END-STRING                                                           
049600     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
049700                                                                          
049800*    -- FOR MAIL WITH SIMPLE TEXT ATTACHMENTS, THE OUTPUT FROM            
049900*    -- WZ14P005 IS SAVED IN A TEMPORARY ATTACHMENT FILE                  
050000     IF W-CHANNEL-JCLTYPE(OUTM-IDCALL) = 'TXTATT'                         
050100       MOVE SPACE TO SUBM-LINE                                            
050200       STRING                                                             
050300         '// DSOUT=&&ATT'                      DELIMITED BY SIZE          
050400         INTO SUBM-LINE                                                   
050500       END-STRING                                                         
050600       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
050700     END-IF                                                               
050800                                                                          
050900     IF W-CHANNEL-JCLTYPE(OUTM-IDCALL) = 'INLINE'                         
051000*      -- WRITE "HEADER" DATA BEFORE APPLICATION DATA                     
051100       MOVE SPACE TO WORK-DATACONT                                        
051200       MOVE 50    TO WORK-DATALENGTH                                      
051300       MOVE 1 TO IX                                                       
051400       PERFORM UNTIL IX > 5                                               
051500         IF W-CHANNEL-TEFAX(OUTM-IDCALL, IX ) NOT = SPACE                 
051600           MOVE W-CHANNEL-TEFAX(OUTM-IDCALL, IX) TO WORK-DATAPIECE        
051700           MOVE WORK-DATALINE  TO SUBM-LINE                               
051800           CALL WZ20SUBM USING SUBM-WZ20SUBM                              
051900         END-IF                                                           
052000         ADD 1 TO IX                                                      
052100       END-PERFORM                                                        
052200     END-IF                                                               
052300     .                                                                    
052400     EJECT                                                                
052500 C-WRITE             SECTION.                                             
052600                                                                          
052700*    When the record is meta data, dont process it. It's not              
052800*    needed here.                                                         
052900     IF OUTM-TEOUTDATA (1:5) = '§META' OR                                 
053000        OUTM-TEOUTDATA (2:5) = '§META'                                    
053100       CONTINUE                                                           
053200     ELSE                                                                 
053300       MOVE 'PUT'                TO SUBM-KDFUNC                           
053400                                                                          
053500*    -- EMULATE ASA CARRIAGE-CONTROL FUNCTIONALITY, IF SPECIFIED,         
053600*    -- BY PUTTING EXTRA BLANK LINES BEFORE THE DATA LINE                 
053700       IF W-CHANNEL-FLCARRCNTL(OUTM-IDCALL) = YES                         
053800       AND W-CHANNEL-EMULATE-CC(OUTM-IDCALL) = YES                        
053900         MOVE SPACE              TO WORK-DATALINE                         
054000         MOVE 2                  TO WORK-DATALENGTH                       
054100         MOVE WORK-DATALINE      TO SUBM-LINE                             
054200         IF OUTM-TEOUTDATA(1:1) = '0' OR '-' OR '1'                       
054300           CALL WZ20SUBM      USING SUBM-WZ20SUBM                         
054400         END-IF                                                           
054500         IF OUTM-TEOUTDATA(1:1) = '-' OR '1'                              
054600           CALL WZ20SUBM      USING SUBM-WZ20SUBM                         
054700         END-IF                                                           
054800       END-IF                                                             
054900                                                                          
055000       MOVE 1                    TO STRING-PTR                            
055100       MOVE SPACE                TO WORK-DATALINE                         
055200                                                                          
055300       PERFORM UNTIL STRING-PTR > OUTM-TEOUTDATA-L                        
055400         COMPUTE WORK-DATALENGTH = OUTM-TEOUTDATA-L                       
055500                                   - STRING-PTR + 1                       
055600         IF WORK-DATALENGTH > 77                                          
055700           MOVE 77               TO WORK-DATALENGTH                       
055800         END-IF                                                           
055900         MOVE OUTM-TEOUTDATA(STRING-PTR:WORK-DATALENGTH)                  
056000                                 TO WORK-DATAPIECE                        
056100         IF W-CHANNEL-FLCARRCNTL(OUTM-IDCALL) = YES                       
056200         AND W-CHANNEL-EMULATE-CC(OUTM-IDCALL) = YES                      
056300         AND WORK-DATACONT NOT = '*'                                      
056400*          -- BLANK OUT CC DATA IF CC IS EMULATED                         
056500*          -- (START LINE ONLY, NOT ON CONTINUATION LINES)                
056600           MOVE SPACE            TO WORK-DATAPIECE(1:1)                   
056700         END-IF                                                           
056800                                                                          
056900         MOVE WORK-DATALINE      TO SUBM-LINE                             
057000         CALL WZ20SUBM        USING SUBM-WZ20SUBM                         
057100                                                                          
057200         MOVE '*'                TO WORK-DATACONT                         
057300         ADD WORK-DATALENGTH     TO STRING-PTR                            
057400       END-PERFORM                                                        
057500     END-IF                                                               
057600     .                                                                    
057700     EJECT                                                                
057800 E-CLOSE             SECTION.                                             
057900                                                                          
058000     MOVE 'PUT'    TO SUBM-KDFUNC                                         
058100                                                                          
058200     MOVE SPACE TO SUBM-LINE                                              
058300     STRING                                                               
058400          '//IFDATA I'                                                    
058500          'F (DATA.WZ1405.RC = 0) THEN'                                   
058600       DELIMITED BY SIZE                                                  
058700       INTO SUBM-LINE                                                     
058800     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
058900                                                                          
059000                                                                          
059100     IF W-CHANNEL-JCLTYPE(OUTM-IDCALL) = 'PDFATT' OR 'PDFZIP'             
059200*      -- JCL FOR ACIF AND PDF CREATION                                   
059300       MOVE '//PDF    EXEC WZ11PDF,DSIN=&&DATA,'                          
059400         TO SUBM-LINE                                                     
059500       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
059600                                                                          
059700       IF W-CHANNEL-ESFADDR(OUTM-IDCALL) = YES                            
059800         MOVE '//            ESFADDR=,'                                   
059900           TO SUBM-LINE                                                   
060000         CALL WZ20SUBM USING SUBM-WZ20SUBM                                
060100       END-IF                                                             
060200                                                                          
060300       MOVE SPACE TO SUBM-LINE                                            
060400       STRING                                                             
060500         '// DSOUTPDF='                        DELIMITED BY SIZE          
060600         WORK-HLQ                              DELIMITED BY SPACE         
060700         W-CHANNEL-DSLUX(OUTM-IDCALL)          DELIMITED BY SPACE         
060800*        ','                                   DELIMITED BY SIZE          
060900         INTO SUBM-LINE                                                   
061000       END-STRING                                                         
061100       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
061200                                                                          
061300*      MOVE SPACE TO SUBM-LINE                                            
061400*      STRING                                                             
061500*        '// DSLUX='                           DELIMITED BY SIZE          
061600*        W-CHANNEL-DSLUX(OUTM-IDCALL)          DELIMITED BY SPACE         
061700*        INTO SUBM-LINE                                                   
061800*      CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
061900                                                                          
062000       MOVE '//ACIF.ACIFCMDS DD *' TO SUBM-LINE                           
062100       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
062200                                                                          
062300       IF W-CHANNEL-FLCARRCNTL(OUTM-IDCALL) = YES                         
062400         MOVE 'YES' TO WORK-CC                                            
062500       ELSE                                                               
062600         MOVE 'NO' TO WORK-CC                                             
062700       END-IF                                                             
062800       MOVE SPACE TO SUBM-LINE                                            
062900       STRING                                                             
063000         ' CC='                                DELIMITED BY SIZE          
063100         WORK-CC                               DELIMITED BY SPACE         
063200         INTO SUBM-LINE                                                   
063300       END-STRING                                                         
063400       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
063500                                                                          
063600       MOVE SPACE TO SUBM-LINE                                            
063700       STRING                                                             
063800         ' USERLIB='                           DELIMITED BY SIZE          
063900         WORK-USERLIB                          DELIMITED BY SPACE         
064000         INTO SUBM-LINE                                                   
064100       END-STRING                                                         
064200       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
064300                                                                          
064400       MOVE SPACE TO SUBM-LINE                                            
064500       STRING                                                             
064600         ' FORMDEF=F1'                         DELIMITED BY SIZE          
064700         W-CHANNEL-LAYOUT(OUTM-IDCALL)         DELIMITED BY SPACE         
064800         INTO SUBM-LINE                                                   
064900       END-STRING                                                         
065000       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
065100                                                                          
065200       MOVE SPACE TO SUBM-LINE                                            
065300       STRING                                                             
065400         ' PAGEDEF=P1'                         DELIMITED BY SIZE          
065500         W-CHANNEL-LAYOUT(OUTM-IDCALL)         DELIMITED BY SPACE         
065600         INTO SUBM-LINE                                                   
065700       END-STRING                                                         
065800       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
065900     END-IF                                                               
066000                                                                          
066100                                                                          
066200     IF W-CHANNEL-JCLTYPE(OUTM-IDCALL) = 'TXTZIP'                         
066300*      -- JCL FOR ZIPPED TEXT FILE ATTACHMENT -----                       
066400       MOVE '//TZIP   EXEC WZ11TZIP,DSIN=&&DATA,'                         
066500         TO SUBM-LINE                                                     
066600       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
066700                                                                          
066800       MOVE                                                               
066900         '// DSOUTZIP=&&ATT,ZIPDISP=(NEW,PASS,DELETE),'                   
067000         TO SUBM-LINE                                                     
067100       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
067200                                                                          
067300       MOVE SPACE TO SUBM-LINE                                            
067400       STRING                                                             
067500         '// CONTENT='                         DELIMITED BY SIZE          
067600         W-CHANNEL-CONTENT(OUTM-IDCALL)        DELIMITED BY SPACE         
067700         INTO SUBM-LINE                                                   
067800       END-STRING                                                         
067900       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
068000                                                                          
068100     END-IF                                                               
068200     IF W-CHANNEL-JCLTYPE(OUTM-IDCALL) =  'PDFZIP'                        
068300                                                                          
068400*      -- JCL FOR ZIPPED PDF FILE ATTACHMENT -----                        
068500       MOVE SPACE TO SUBM-LINE                                            
068600       STRING                                                             
068700         '//BZIP   EXEC WZ11BZIP,'             DELIMITED BY SIZE          
068800         INTO SUBM-LINE                                                   
068900       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
069000                                                                          
069100       MOVE SPACE TO SUBM-LINE                                            
069200       STRING                                                             
069300         '// DSIN='                            DELIMITED BY SIZE          
069400         WORK-HLQ                              DELIMITED BY SPACE         
069500         W-CHANNEL-DSLUX(OUTM-IDCALL)          DELIMITED BY SPACE         
069600         ','                                   DELIMITED BY SIZE          
069700         INTO SUBM-LINE                                                   
069800       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
069900                                                                          
070000       MOVE                                                               
070100         '// DSOUTZIP=&&ATT,ZIPDISP=(NEW,PASS,DELETE),'                   
070200         TO SUBM-LINE                                                     
070300       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
070400                                                                          
070500       MOVE SPACE TO SUBM-LINE                                            
070600       STRING                                                             
070700         '// CONTENT='                         DELIMITED BY SIZE          
070800         W-CHANNEL-CONTENT(OUTM-IDCALL)        DELIMITED BY SPACE         
070900         INTO SUBM-LINE                                                   
071000       END-STRING                                                         
071100       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
071200                                                                          
071300     END-IF                                                               
071400                                                                          
071500     IF W-CHANNEL-JCLTYPE(OUTM-IDCALL) =  'BINZIP'                        
071600*      -- JCL FOR ZIPPED UNSPECIFIED BINARY FILE ATTACHMENT -----         
071700                                                                          
071800       MOVE                                                               
071900         '//BZIP   EXEC WZ11BZIP,DSIN=&&DATA,'                            
072000         TO SUBM-LINE                                                     
072100       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
072200                                                                          
072300       MOVE                                                               
072400         '// DSOUTZIP=&&ATT,ZIPDISP=(NEW,PASS,DELETE),'                   
072500         TO SUBM-LINE                                                     
072600       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
072700                                                                          
072800       MOVE SPACE TO SUBM-LINE                                            
072900       STRING                                                             
073000         '// CONTENT='                         DELIMITED BY SIZE          
073100         W-CHANNEL-CONTENT(OUTM-IDCALL)        DELIMITED BY SPACE         
073200         INTO SUBM-LINE                                                   
073300       END-STRING                                                         
073400       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
073500                                                                          
073600     END-IF                                                               
073700                                                                          
073800                                                                          
073900     IF W-CHANNEL-JCLTYPE(OUTM-IDCALL) = 'INLINE'                         
074000*      -- ALL DATA VIA FILE                                               
074100       MOVE '//MAIL    EXEC WMAILSND,DSIN=&&DATA'                         
074200         TO SUBM-LINE                                                     
074300       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
074400     ELSE                                                                 
074500*      -- DATA VIA SYSIN AND PERHAPS ATTACHMENT                           
074600       MOVE '//MAIL    EXEC WMAILSND'                                     
074700         TO SUBM-LINE                                                     
074800       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
074900     END-IF                                                               
075000                                                                          
075100     IF W-CHANNEL-JCLTYPE(OUTM-IDCALL) =                                  
075200       'TXTATT' OR 'TXTZIP' OR 'PDFZIP' OR 'BINZIP'                       
075300       MOVE '//ATT       DD DSN=&&ATT,DISP=(OLD,DELETE)'                  
075400         TO SUBM-LINE                                                     
075500       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
075600     END-IF                                                               
075700     IF W-CHANNEL-JCLTYPE(OUTM-IDCALL) = 'PDFATT'                         
075800       MOVE SPACE TO SUBM-LINE                                            
075900       STRING                                                             
076000         '//ATT       DD DSN='                 DELIMITED BY SIZE          
076100         WORK-HLQ                              DELIMITED BY SPACE         
076200         W-CHANNEL-DSLUX(OUTM-IDCALL)          DELIMITED BY SPACE         
076300         ',DISP=(OLD,DELETE)'                  DELIMITED BY SIZE          
076400         INTO SUBM-LINE                                                   
076500       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
076600     END-IF                                                               
076700                                                                          
076800                                                                          
076900     MOVE ')SEND'                                                         
077000       TO SUBM-LINE                                                       
077100     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
077200                                                                          
077300     MOVE SPACE TO SUBM-LINE                                              
077400     STRING                                                               
077500       ' SUBJECT '                             DELIMITED BY SIZE          
077600       W-CHANNEL-IDMAILTTL(OUTM-IDCALL)        DELIMITED BY SIZE          
077700       INTO SUBM-LINE                                                     
077800     END-STRING                                                           
077900     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
078000                                                                          
078100     MOVE SPACE TO SUBM-LINE                                              
078200     STRING                                                               
078300       ' TO '                                  DELIMITED BY SIZE          
078400       W-CHANNEL-IDOUTDEST(OUTM-IDCALL)        DELIMITED BY SIZE          
078500       INTO SUBM-LINE                                                     
078600     END-STRING                                                           
078700     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
078800                                                                          
078900     IF W-CHANNEL-JCLTYPE(OUTM-IDCALL) = 'TXTATT'                         
079000*      -- SUPPLY DATA AS ATTACHMENT                                       
079100       MOVE SPACE TO SUBM-LINE                                            
079200       STRING                                                             
079300         ' ATTACH ATT '                        DELIMITED BY SIZE          
079400         W-CHANNEL-ATTNAME(OUTM-IDCALL)        DELIMITED BY SPACE         
079500         ' TEXT'                               DELIMITED BY SIZE          
079600         INTO SUBM-LINE                                                   
079700       END-STRING                                                         
079800       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
079900     END-IF                                                               
080000     IF W-CHANNEL-JCLTYPE(OUTM-IDCALL)                                    
080100        = 'PDFATT' OR 'TXTZIP' OR 'PDFZIP' OR 'BINZIP'                    
080200*      -- SUPPLY DATA AS ATTACHMENT                                       
080300       MOVE SPACE TO SUBM-LINE                                            
080400       STRING                                                             
080500         ' ATTACH ATT '                        DELIMITED BY SIZE          
080600         W-CHANNEL-ATTNAME(OUTM-IDCALL)        DELIMITED BY SPACE         
080700         ' BIN'                                DELIMITED BY SIZE          
080800         INTO SUBM-LINE                                                   
080900       END-STRING                                                         
081000       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
081100     END-IF                                                               
081200                                                                          
081300     IF W-CHANNEL-JCLTYPE(OUTM-IDCALL) = 'INLINE'                         
081400*      -- BOTH FIXED HEADER AND VARIABLE DATA VIA SEPARATE FILE           
081500       MOVE ' MAIL SEND'    TO SUBM-LINE                                  
081600       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
081700                                                                          
081800     ELSE                                                                 
081900*      -- FOR ALL OTHER TYPES,                                            
082000*      -- WRITE INLINE "HEADER" DATA FOR THE MAIL                         
082100*      -- THE VARIABLE DATA WAS ATTACHED ABOVE                            
082200       MOVE ' MAIL'         TO SUBM-LINE                                  
082300       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
082400                                                                          
082500       MOVE 1 TO IX                                                       
082600       PERFORM UNTIL IX > 5                                               
082700         MOVE W-CHANNEL-TEFAX(OUTM-IDCALL, IX) TO SUBM-LINE               
082800         CALL WZ20SUBM USING SUBM-WZ20SUBM                                
082900         ADD 1 TO IX                                                      
083000       END-PERFORM                                                        
083100     END-IF                                                               
083200                                                                          
083300     MOVE ')END'          TO SUBM-LINE                                    
083400     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
083500                                                                          
083600                                                                          
083700     MOVE SPACE TO SUBM-LINE                                              
083800     STRING                                                               
083900          '//SIGNAL I'                                                    
084000          'F ABEND THEN '                    DELIMITED BY SIZE            
084100       INTO SUBM-LINE                                                     
084200     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
084300                                                                          
084400     MOVE '//SIGNAL  EXEC WSOP'        TO SUBM-LINE                       
084500     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
084600                                                                          
084700     MOVE ' ORDER WZ11JABE SYMBOLS'    TO SUBM-LINE                       
084800     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
084900                                                                          
085000     MOVE ' ACTION(sending mail)'      TO SUBM-LINE                       
085100     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
085200                                                                          
085300     MOVE SPACE TO SUBM-LINE                                              
085400     STRING                                                               
085500       ' DEST('                              DELIMITED BY SIZE            
085600       FUNCTION TRIM(W-CHANNEL-IDOUTDEST(OUTM-IDCALL))                    
085700                                             DELIMITED BY SIZE            
085800       ')'                                   DELIMITED BY SIZE            
085900       INTO SUBM-LINE                                                     
086000     END-STRING                                                           
086100     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
086200                                                                          
086300     MOVE SPACE TO SUBM-LINE                                              
086400     STRING                                                               
086500       ' TIMESTAMP('                         DELIMITED BY SIZE            
086600       W-YYMMDD                              DELIMITED BY SIZE            
086700       ' '                                   DELIMITED BY SIZE            
086800       W-HHMMSS                              DELIMITED BY SIZE            
086900       ') JOBNAME(WZ11'                      DELIMITED BY SIZE            
087000       W-HHMMSS(3:4)                         DELIMITED BY SIZE            
087100       ')'                                   DELIMITED BY SIZE            
087200       INTO SUBM-LINE                                                     
087300     END-STRING                                                           
087400     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
087500                                                                          
087600     IF W-CHANNEL-IDOUTTYPE = SPACES                                      
087700       MOVE ' INFO( )'                 TO SUBM-LINE                       
087800       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
087900                                                                          
088000       MOVE ' TYPE( )'                 TO SUBM-LINE                       
088100       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
088200                                                                          
088300       MOVE ' REC( )'                  TO SUBM-LINE                       
088400       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
088500                                                                          
088600       MOVE ' LIST( )'                 TO SUBM-LINE                       
088700       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
088800                                                                          
088900       MOVE ' REGDAT( )'               TO SUBM-LINE                       
089000       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
089100                                                                          
089200       MOVE ' KLOCK( )'                TO SUBM-LINE                       
089300       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
089400     ELSE                                                                 
089500       MOVE ' INFO(Restart Keys:)'     TO SUBM-LINE                       
089600       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
089700                                                                          
089800       MOVE SPACE TO SUBM-LINE                                            
089900       STRING                                                             
090000         ' TYPE(Output Type    : '           DELIMITED BY SIZE            
090100         FUNCTION TRIM(W-CHANNEL-IDOUTTYPE)                               
090200                                             DELIMITED BY SIZE            
090300         ')'                                 DELIMITED BY SIZE            
090400         INTO SUBM-LINE                                                   
090500       END-STRING                                                         
090600       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
090700                                                                          
090800       MOVE SPACE TO SUBM-LINE                                            
090900       STRING                                                             
091000         ' REC(Output SubType : '            DELIMITED BY SIZE            
091100         FUNCTION TRIM(W-CHANNEL-IDOUTREC)                                
091200                                             DELIMITED BY SIZE            
091300         ')'                                 DELIMITED BY SIZE            
091400         INTO SUBM-LINE                                                   
091500       END-STRING                                                         
091600       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
091700                                                                          
091800       MOVE SPACE TO SUBM-LINE                                            
091900       STRING                                                             
092000         ' LIST(Output Id      : '           DELIMITED BY SIZE            
092100         FUNCTION TRIM(W-CHANNEL-IDLIST)                                  
092200                                             DELIMITED BY SIZE            
092300         ')'                                 DELIMITED BY SIZE            
092400         INTO SUBM-LINE                                                   
092500       END-STRING                                                         
092600       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
092700                                                                          
092800       MOVE SPACE TO SUBM-LINE                                            
092900       STRING                                                             
093000         ' REGDAT(Date           : '         DELIMITED BY SIZE            
093100         W-CHANNEL-TIREGDAT                  DELIMITED BY SPACE           
093200         ')'                                 DELIMITED BY SIZE            
093300         INTO SUBM-LINE                                                   
093400       END-STRING                                                         
093500       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
093600                                                                          
093700       MOVE SPACE TO SUBM-LINE                                            
093800       STRING                                                             
093900         ' KLOCK(Time           : '          DELIMITED BY SIZE            
094000         W-CHANNEL-TIKLOCK                   DELIMITED BY SPACE           
094100         ')'                                 DELIMITED BY SIZE            
094200         INTO SUBM-LINE                                                   
094300       END-STRING                                                         
094400       CALL WZ20SUBM USING SUBM-WZ20SUBM                                  
094500     END-IF                                                               
094600                                                                          
094700     MOVE ' END-ORDER'                 TO SUBM-LINE                       
094800     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
094900                                                                          
095000     MOVE '//SIGNAL ENDIF'             TO SUBM-LINE                       
095100     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
095200                                                                          
095300     MOVE '//IFDATA ENDIF'             TO SUBM-LINE                       
095400     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
095500                                                                          
095600                                                                          
095700     MOVE 'CLOSE'  TO SUBM-KDFUNC                                         
095800     CALL WZ20SUBM USING SUBM-WZ20SUBM                                    
095900     .                                                                    

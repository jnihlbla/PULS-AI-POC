000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ11OUTO.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   02/10/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM HANDLES OUTPUT TO ON DEMAND                         
000900*        IT USES THE GENERAL PRINT MODULE WZ11OUTP                        
001000*        TO DO MOST OF THE JOB.                                           
001100                                                                          
001200     EJECT                                                                
001300 DATA DIVISION.                                                           
001400     SKIP3                                                                
001500 WORKING-STORAGE SECTION.                                                 
001600 77  IDPGM                       PIC X(08) VALUE 'WZ11OUTO'.              
001700                                                                          
001800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
001900 77  ERROR-TEXT-START            PIC X(8)  VALUE 'ERROR:  '.              
002000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
002100 77  KDRC-DISPLAY                PIC Z(5).                                
002200                                                                          
002300 77  YES                         PIC X     VALUE 'J'.                     
002400 77  NOO                         PIC X     VALUE 'N'.                     
002500 77  TRIPPEL-APOSTROPH           PIC XXX   VALUE ''''''''.                
002600                                                                          
002700 01  SMALL-LETTERS               PIC X(31) VALUE                          
002800     'abcdefghijklmnopqrstuvwxyzåäöüé'.                                   
002900 01  CAPS-LETTERS                PIC X(31) VALUE                          
003000     'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÜÉ'.                                   
003100                                                                          
003200 01  STRING-PTR                  PIC S9(4) BINARY.                        
003300 01  IX2                         PIC S9(4) BINARY.                        
003400 01  PCOUNTER                    PIC S9(4) BINARY.                        
003500                                                                          
003600 01  W-YYMMDD                    PIC X(6).                                
003700 01  W-HHMMSS                    PIC X(6).                                
003800 01  W-TH                        PIC X(2).                                
003900 01  W-COMMA-OR-SPACE            PIC X.                                   
004000 01  W-ATTACH-SUFF               PIC X(8).                                
004100 01  W-CONTENT-SUFF              PIC X(8).                                
004200 01  W-IDPFDEF                   PIC X(8).                                
004300 01  ZONED-CHANNEL               PIC 9(3).                                
004400                                                                          
004500 01  INDX                        PIC S9(4) BINARY.                        
004600                                                                          
004700 01  WORK-FIELDS.                                                         
004800     03  WS-OPEN-CURL            PIC X     VALUE '{'.                     
004900     03  WS-CLOSE-CURL           PIC X     VALUE '}'.                     
005000     03  WS-COLON                PIC X     VALUE ':'.                     
005100     03  WS-QUOTE                PIC X     VALUE ''''.                    
005200     03  WS-DOUBLEQUOTE          PIC X     VALUE '"'.                     
005300     03  WS-EQUAL                PIC X     VALUE '='.                     
005400     03  WS-META-PREFIX          PIC X(20) VALUE SPACES.                  
005500                                                                          
005600     03  WS-HOST                 PIC X(32) VALUE                          
005700         'https://smartfacts.volvocars.net'.                              
005800     03  WS-PATH1                PIC X(22) VALUE                          
005900         '/api/selfservice/send/'.                                        
006000     03  WS-PATH2                PIC X(5)  VALUE                          
006100         '/file'.                                                         
006200     03  WS-QRY1                 PIC X(8)  VALUE 'site=VCC'.              
006300     03  WS-QRY2-KEY             PIC X(8)  VALUE 'filename'.              
006400                                                                          
006500     03  WS-HDR1-APIKEY          PIC X(11) VALUE '"x-api-key"'.           
006600     03  WS-HDR1-APIKEY-VALUE    PIC X(40).                               
006700                                                                          
006800*    --- For smartfacts 2.0 (Azure)                                       
006900     03  WS-SMARTFACT2-PREFIX    PIC X(09) VALUE                          
007000         'x-ms-meta'.                                                     
007100                                                                          
007200     03  WS-AZ-STORAGE-ACC       PIC X(30).                               
007300     03  WS-AZ-CONTAINER         PIC X(30).                               
007400     03  WS-AZ-SAS               PIC X(300) VALUE SPACES.                 
007500     03  WS-AZ-SAS-PROD.                                                  
007600         05  FILLER              PIC X(50)  VALUE                         
007700         '**************************************************'.            
007800         05  FILLER              PIC X(50)  VALUE                         
007900         '**************************************************'.            
008000         05  FILLER              PIC X(50)  VALUE                         
008100         '**************************************************'.            
008200         05  FILLER              PIC X(50)  VALUE                         
008300         '**************************************************'.                                               
008400         05  FILLER              PIC X(50)  VALUE SPACES.                 
008500         05  FILLER              PIC X(50)  VALUE SPACES.                 
008600                                                                          
008610     03  WS-AZ-SAS-QA.                                                    
008620         05  FILLER              PIC X(50)  VALUE                         
008630         '**************************************************'.            
008640         05  FILLER              PIC X(50)  VALUE                         
008650         '**************************************************'.            
008660         05  FILLER              PIC X(50)  VALUE                         
008670         '**************************************************'.            
008680         05  FILLER              PIC X(50)  VALUE                         
008690         '**************************************************'.                                                        
008691         05  FILLER              PIC X(50)  VALUE SPACES.                 
008692         05  FILLER              PIC X(50)  VALUE SPACES.                 
008693                                                                          
008700     03  WS-SL-HOST              PIC X(33)  VALUE SPACES.                 
008800     03  WS-SL-HOST-TEST         PIC X(33)  VALUE                         
008900                         'https://se-test-api.volvocars.net'.             
009000     03  WS-SL-HOST-PROD         PIC X(33)  VALUE                         
009100                         'https://se-api.volvocars.net'.                  
009200                                                                          
009300     03  WS-SL-UKEY              PIC X(32)  VALUE SPACES.                 
009400     03  WS-SL-UKEY-TEST         PIC X(32)  VALUE                         
009500                         '********************************'.              
009600     03  WS-SL-UKEY-PROD         PIC X(32)  VALUE                         
009700                         '********************************'.              
009800                                                                          
009900     03  WS-SL-PKEY              PIC X(32)  VALUE SPACES.                 
010000     03  WS-SL-PKEY-TEST         PIC X(32)  VALUE                         
010100                         '********************************'.              
010200     03  WS-SL-PKEY-PROD         PIC X(32)  VALUE                         
010300                         '********************************'.              
010400                                                                          
010500     03  WS-CURL-TEXT-278        PIC X(3000).                             
010600     03  WS-CURL-TEXT            PIC X(3000).                             
010700     03  CURL-TEXT-PTR           PIC S9(4) BINARY.                        
010800     03  CURL-TEXT-LEN           PIC S9(4) BINARY.                        
010900                                                                          
011000                                                                          
011100     03  WS-KEY                  PIC X(50) VALUE SPACES.                  
011200     03  WS-VALUE                PIC X(60) VALUE SPACES.                  
011300                                                                          
011400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
011500 01  GENERAL-SUBPROGRAMS.                                                 
011600     03  FELLOG                  PIC X(8)  VALUE 'FELLOG  '.              
011700     03  ABEND                   PIC X(8)  VALUE 'ABEND   '.              
011800     03  WZ20SUBM                PIC X(8)  VALUE 'WZ20SUBM'.              
011900     03  W980USPA                PIC X(8)  VALUE 'W980USPA'.              
012000     03  WZ11OUTP                PIC X(8)  VALUE 'WZ11OUTP'.              
012100     03  WURLCONV                PIC X(8)  VALUE 'WURLCONV'.              
012200     03  VIMSID                  PIC X(8)  VALUE 'VIMSID  '.              
012300                                                                          
012400*    --- PARAMETERS TO ABEND                                              
012500 77  RCODE-ABEND-NO-DUMP         PIC S9(4) COMP VALUE +16.                
012600 77  RCODE-ABEND-WITH-DUMP       PIC S9(4) COMP VALUE +1000.              
012700                                                                          
012800*    --- PARAMETERS TO VIMSID                                             
012900 01  VIMSID-PARM.                                                         
013000     03  IMSID4                  PIC X(4)  VALUE SPACE.                   
013100     03  FILLER                  PIC X(4)  VALUE SPACE.                   
013200                                                                          
013300 01  OUTP-AREA-START             PIC X(16) VALUE                          
013400                                 'OUTP-AREA-START '.                      
013500*01  -COPY WZ11OUTP.                                                      
013600                                                                          
013700*    --- PARAMETERS TO WZ20SUBM                                           
013800 01  -COPY WZ20SUBM.                                                      
013900                                                                          
014000                                                                          
014100*    --- PARAMETERS TO WURLCONV                                           
014200      COPY WURLCONV.                                                      
014300                                                                          
014400*    -- THE CHANNEL TABLE IS USED TO SAVE DATA FROM OPEN CALL             
014500*    -- UNTIL IT IS NEEDED AT CLOSING TIME                                
014600 01  W-CHANNEL-AREA              PIC X(16)   VALUE                        
014700                                 'CHANNELS-START  '.                      
014800 01  W-CHANNEL-IDOUTTYPE         PIC X(15).                               
014900 01  W-CHANNEL-IDOUTREC          PIC X(30).                               
015000 01  W-CHANNEL-IDLIST            PIC X(10).                               
015100 01  W-CHANNEL-TIREGDAT          PIC 9(6).                                
015200 01  W-CHANNEL-TIKLOCK           PIC 9(8).                                
015300 01  W-CHANNEL-TAB.                                                       
015400     03  FILLER OCCURS 15 INDEXED BY IX.                                  
015500         05  W-CHANNEL-DSLUX     PIC X(30).                               
015600         05  W-CHANNEL-ATTNAME   PIC X(30).                               
015700         05  W-CHANNEL-CONTENT   PIC X(30).                               
015800         05  W-CHANNEL-JCLTYPE   PIC X(8).                                
015900         05  W-CHANNEL-IDOUTDEST PIC X(60).                               
016000         05  W-CHANNEL-IDMAIL-SENDER                                      
016100                                 PIC X(50).                               
016200         05  W-CHANNEL-IDMAILTTL PIC X(50).                               
016300         05  W-CHANNEL-LAYOUT    PIC X(6).                                
016400         05  W-CHANNEL-IDPFDEF   PIC X(8).                                
016500         05  W-CHANNEL-IDFORMSNM PIC X(8).                                
016600         05  W-CHANNEL-FLACIF    PIC X.                                   
016700         05  W-CHANNEL-IDPCB     PIC X(8).                                
016800         05  W-CHANNEL-FLCARRCNTL                                         
016900                                 PIC X.                                   
017000         05  W-CHANNEL-EMULATE-CC                                         
017100                                 PIC X.                                   
017200         05  W-CHANNEL-ESFADDR   PIC X.                                   
017300         05  W-CHANNEL-TEFAX     PIC X(50) OCCURS 5 TIMES.                
017400         05  W-CHANNEL-FILENAME  PIC X(50).                               
017500         05  W-CHANNEL-SUFF      PIC X(8).                                
017600         05  W-CHANNEL-TIX       PIC S9(4) BINARY VALUE ZERO.             
017700         05  W-CHANNEL-META-TAB.                                          
017800             07  FILLER OCCURS 20 TIMES INDEXED BY TAB-IX.                
017900                 09  W-TAB-KEY   PIC X(50) VALUE SPACES.                  
018000                 09  W-TAB-VALUE PIC X(60) VALUE SPACES.                  
018100         05  W-CHANNEL-DOC-NBR   PIC X(20).                               
018200                                                                          
018300                                                                          
018400 01  WS-TEXT-TO-ENC              PIC X(300).                              
018500 01  WS-ENC-TEXT                 PIC X(300).                              
018600                                                                          
018700 01  WORK-CC                     PIC X(3).                                
018800                                                                          
018900 01  WORK-JOBNAME                PIC X(8)   VALUE                         
019000                                  'WZ11NNNN'.                             
019100 01  WORK-ACCOUNT                PIC X(30)  VALUE                         
019200                                  '510WOS39000WZ11OUTO'.                  
019300 01  WORK-MSGCLASS-CLASS         PIC X(30)  VALUE                         
019400                                  'MSGCLASS=H,CLASS=S'.                   
019500 01  WORK-JOBFORMS               PIC X(4)   VALUE                         
019600                                  'STD '.                                 
019700 01  WORK-PROCLIBS               PIC X(50)  VALUE                         
019800                                  'W.XXXX.PROCLIB,W.PROD.PROCLIB'.        
019900 01  WORK-ENV                    PIC X(8)   VALUE                         
020000                                  'ENVXXXX '.                             
020100 01  WORK-SYSTZ                  PIC X(8)   VALUE                         
020200                                  'SYSTZXX '.                             
020300 01  WORK-USERLIB                PIC X(50)  VALUE                         
020400                                  'W.XXXX.PSF'.                           
020500 01  WORK-ROUTE-XEQ              PIC X(8)   VALUE                         
020600                                  'LOCAL'.                                
020700 01  WORK-HLQ                    PIC X(16)  VALUE                         
020800                                  'W.XXXX.'.                              
020900                                                                          
021000 01  WORK-QUAL-PDF               PIC X(4)   VALUE                         
021100                                  'PDF.'.                                 
021200 01  WORK-QUAL-CURL              PIC X(5)   VALUE                         
021300                                  'CURL.'.                                
021400 01  WORK-QUAL-CURL2             PIC X(6)   VALUE                         
021500                                  'CURL2.'.                               
021600                                                                          
021700*    -- AREA FOR STORING 80-BYTE PIECES OF THE DATA RECORDS               
021800 01  WORK-DATALINE.                                                       
021900*                                -- X MEANS CONTINUATION                  
022000*                                -- SPACE MEANS FIRST PIECE               
022100   03  WORK-DATACONT             PIC X.                                   
022200*                                -- LENGTH OF THIS PIECE                  
022300   03  WORK-DATALENGTH           PIC 9(4)   BINARY.                       
022400   03  WORK-DATAPIECE            PIC X(77).                               
022500                                                                          
022600 01  -COPY W980USPA.                                                      
022700                                                                          
022800 LINKAGE SECTION.                                                         
022900*01  -COPY WZ11OUTO.                                                      
023000                                                                          
023100 PROCEDURE DIVISION  USING OUTO-WZ11OUT.                                  
023200 MAIN SECTION.                                                            
023300                                                                          
023400     PERFORM A-INIT                                                       
023500     EVALUATE OUTO-KDFUNC                                                 
023600       WHEN 'OPEN'   PERFORM B-OPEN-CHANNEL                               
023700       WHEN 'PUT'    PERFORM C-WRITE-ONE-RECORD                           
023800       WHEN 'CLOSE'  PERFORM D-CLOSE-CHANNEL                              
023900     END-EVALUATE                                                         
024000                                                                          
024100     MOVE OUTP-KDRC TO OUTO-KDRC                                          
024200     GOBACK                                                               
024300     .                                                                    
024400                                                                          
024500 A-INIT SECTION.                                                          
024600                                                                          
024700     IF OUTO-IDCALL < 1 OR > 15                                           
024800       MOVE OUTO-IDCALL          TO KDRC-DISPLAY                          
024900       STRING 'WZ11OUTO INVALID IDCALL VALUE: '                           
025000              KDRC-DISPLAY                                                
025100         DELIMITED BY SIZE                                                
025200                               INTO ERROR-TEXT                            
025300       CALL ABEND             USING RCODE-ABEND-WITH-DUMP                 
025400     END-IF                                                               
025500                                                                          
025600     SET IX                      TO OUTO-IDCALL                           
025700                                                                          
025800*    -- WZ20SUBM USES SAME IDCALL NUMBER AS THIS PROGRAM                  
025900     MOVE OUTO-IDCALL            TO SUBM-IDCALL                           
026000                                                                          
026100*    -- INITIALIZE GENERAL ARGUMENTS TO THE PRINT MODULE (OUTP)           
026200     MOVE OUTO-IDCALL            TO OUTP-IDCALL                           
026300     MOVE OUTO-KDFUNC            TO OUTP-KDFUNC                           
026400                                                                          
026500*    -- FIND OUT WHICH IMS SYSTEM WE ARE USING                            
026600     CALL VIMSID              USING VIMSID-PARM                           
026700                                                                          
026800*    -- SO FAR, SO GOOD                                                   
026900     MOVE ZERO                   TO OUTO-KDRC                             
027000     .                                                                    
027100                                                                          
027200 B-OPEN-CHANNEL SECTION.                                                  
027300                                                                          
027400     MOVE OUTO-IDOUTTYPE         TO W-CHANNEL-IDOUTTYPE                   
027500     MOVE OUTO-IDOUTREC          TO W-CHANNEL-IDOUTREC                    
027600     MOVE OUTO-IDLIST            TO W-CHANNEL-IDLIST                      
027700     MOVE OUTO-TIREGDAT          TO W-CHANNEL-TIREGDAT                    
027800     MOVE OUTO-TIKLOCK           TO W-CHANNEL-TIKLOCK                     
027900                                                                          
028000*    -- USE CURRENT TIMESTAMP TO AS PART OF JOBNAME AND                   
028100*    -- ATTATCHED FILES. SAVE IT NOW FOR LATER USE.                       
028200     MOVE FUNCTION CURRENT-DATE(3:6)                                      
028300                                 TO W-YYMMDD                              
028400     MOVE FUNCTION CURRENT-DATE(9:6)                                      
028500                                 TO W-HHMMSS                              
028600     MOVE FUNCTION CURRENT-DATE(15:2)                                     
028700                                 TO W-TH                                  
028800                                                                          
028900     MOVE SPACES                 TO W-CHANNEL-IDOUTDEST  (IX)             
029000     IF OUTO-IDOUTDEST NOT = SPACE                                        
029100       MOVE OUTO-IDOUTDEST       TO W-CHANNEL-IDOUTDEST  (IX)             
029200     END-IF                                                               
029300     MOVE OUTO-IDPFDEF           TO W-CHANNEL-IDPFDEF    (IX)             
029400     MOVE OUTO-IDFORMSNM         TO W-CHANNEL-IDFORMSNM  (IX)             
029500     MOVE OUTO-FLCARRCNTL        TO W-CHANNEL-FLCARRCNTL (IX)             
029600     MOVE OUTO-FLACIF            TO W-CHANNEL-FLACIF     (IX)             
029700     MOVE OUTO-IDPCB             TO W-CHANNEL-IDPCB      (IX)             
029800     MOVE ZERO                   TO W-CHANNEL-TIX        (IX)             
029900     MOVE SPACES                 TO W-CHANNEL-META-TAB   (IX)             
030000     MOVE SPACES                 TO W-CHANNEL-FILENAME   (IX)             
030100     MOVE SPACES                 TO W-CHANNEL-DOC-NBR    (IX)             
030200                                                                          
030300     IF OUTO-IDFORMSNM > SPACES                                           
030400       PERFORM BA-OPEN-CHANNEL-ONDEMAND                                   
030500     END-IF                                                               
030600     IF OUTO-IDOUTDEST > SPACES                                           
030700       PERFORM BB-OPEN-CHANNEL-SMARTFACTS                                 
030800     END-IF                                                               
030900     .                                                                    
031000                                                                          
031100 BA-OPEN-CHANNEL-ONDEMAND SECTION.                                        
031200                                                                          
031300*    -- INITIALIZE OPEN ARGUMENTS TO THE PRINT MODULE (OUTP)              
031400*    -- THE ON DEMAND "PRINTER":                                          
031500     IF IMSID4 = 'IMG1' OR 'IMG0'                                         
031600*      NEDANSTÅENDE ÄR VCC PROD DEST                                      
031700       MOVE 'QSEC3136'           TO OUTP-IDOUTDEST                        
031800     ELSE                                                                 
031900       MOVE 'QSE03136'           TO OUTP-IDOUTDEST                        
032000* OVANSTÅENDE ÄR VOLVO AB PROD DEST. KAN ANVÄNDAS I SD-MILJÖN.            
032100* NEDANSTÅENDE ÄR TEST-DEST SOM KRÄVER MANUELLA INSATSER FÖR              
032200* ATT KOMMA UT.                                                           
032300*      MOVE 'QSET3136'           TO OUTP-IDOUTDEST                        
032400     END-IF                                                               
032500                                                                          
032600*    -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE                    
032700*    -- THERE IS A PROBLEM IN THE DISTRIBUTION                            
032800     MOVE W-CHANNEL-IDOUTTYPE    TO OUTP-IDOUTTYPE                        
032900     MOVE W-CHANNEL-IDOUTREC     TO OUTP-IDOUTREC                         
033000     MOVE W-CHANNEL-IDLIST       TO OUTP-IDLIST                           
033100     MOVE W-CHANNEL-TIREGDAT     TO OUTP-TIREGDAT                         
033200     MOVE W-CHANNEL-TIKLOCK      TO OUTP-TIKLOCK                          
033300                                                                          
033400     MOVE '1'                    TO OUTP-KVCOPIES                         
033500     MOVE W-CHANNEL-IDPFDEF (IX)                                          
033600                                 TO OUTP-IDPFDEF                          
033700     MOVE W-CHANNEL-IDFORMSNM (IX)                                        
033800                                 TO OUTP-IDFORMSNM                        
033900     MOVE W-CHANNEL-FLCARRCNTL (IX)                                       
034000                                 TO OUTP-FLCARRCNTL                       
034100     MOVE W-CHANNEL-FLACIF (IX)                                           
034200                                 TO OUTP-FLACIF                           
034300     MOVE W-CHANNEL-IDPCB (IX)                                            
034400                                 TO OUTP-IDPCB                            
034500                                                                          
034600*    -- LET OUTP DO THE JOB                                               
034700     CALL WZ11OUTP            USING OUTP-WZ11OUT                          
034800                                                                          
034900     .                                                                    
035000                                                                          
035100 BB-OPEN-CHANNEL-SMARTFACTS SECTION.                                      
035200                                                                          
035300     MOVE SPACE                  TO W-COMMA-OR-SPACE                      
035400     MOVE SPACE                  TO W-CONTENT-SUFF                        
035500     MOVE SPACE                  TO W-ATTACH-SUFF                         
035600     IF W-CHANNEL-IDPFDEF (IX) NOT = SPACE                                
035700*      -- A PAGEDEF/FORMSDEF LAYOUT IS SPECIFIED                          
035800       IF W-CHANNEL-IDPFDEF (IX) (1:1) = '.'                              
035900*         -- IT IS A FILE TYPE, NOT A LAYOUT                              
036000         MOVE FUNCTION UPPER-CASE(W-CHANNEL-IDPFDEF (IX))                 
036100                                 TO W-IDPFDEF                             
036200         MOVE ZERO               TO TALLY                                 
036300         INSPECT W-IDPFDEF TALLYING TALLY FOR CHARACTERS                  
036400                 BEFORE 'ZIP'                                             
036500         IF TALLY < 8                                                     
036600*          -- A ZIPPED ATTACHMENT EXTRACT FILE SUFFIX                     
036700           MOVE W-CHANNEL-IDPFDEF (IX) (1:TALLY)                          
036800                                 TO W-CONTENT-SUFF                        
036900           IF W-CONTENT-SUFF = '.'                                        
037000             MOVE '.TXT'         TO W-CONTENT-SUFF                        
037100           END-IF                                                         
037200           MOVE '.zip'           TO W-ATTACH-SUFF                         
037300         ELSE                                                             
037400*          -- USE FILE TYPE AS IT IS                                      
037500           MOVE W-CHANNEL-IDPFDEF (IX)                                    
037600                                 TO W-ATTACH-SUFF                         
037700         END-IF                                                           
037800         MOVE NOO                TO W-CHANNEL-EMULATE-CC(IX)              
037900                                                                          
038000         EVALUATE W-IDPFDEF                                               
038100           WHEN  '.ZIP'                                                   
038200           WHEN  '.TXTZIP'                                                
038300           WHEN  '.CSVZIP'                                                
038400           WHEN  '.HTMZIP'                                                
038500           WHEN  '.HTMLZIP'                                               
038600*          -- ZIPPED TEXT ATTACHMENT                                      
038700             MOVE 'TXTZIP'       TO W-CHANNEL-JCLTYPE(IX)                 
038800             MOVE YES            TO W-CHANNEL-EMULATE-CC(IX)              
038900                                                                          
039000           WHEN '.PDFZIP'                                                 
039100*          -- ZIPPED PDF FILE ATTACHMENT                                  
039200*          -- USE A STANDARD LAYOUT FOR PDF                               
039300             MOVE 'SLS08C'       TO W-CHANNEL-LAYOUT(IX)                  
039400             MOVE 'PDFZIP'       TO W-CHANNEL-JCLTYPE(IX)                 
039500                                                                          
039600           WHEN OTHER                                                     
039700             IF TALLY = 8                                                 
039800*          -- LAYOUT IS A NORMAL FILE TYPE.                               
039900*          -- SEND UNFORMATTED AS ATTACHMENT WITH SPECIFIED TYPE          
040000               MOVE 'TXTATT'     TO W-CHANNEL-JCLTYPE(IX)                 
040100               MOVE YES          TO W-CHANNEL-EMULATE-CC(IX)              
040200               MOVE  ','         TO W-COMMA-OR-SPACE                      
040300             ELSE                                                         
040400*          -- LAYOUT IS ZIP FILE WITH PROBABLY BINARY CONTENT             
040500               MOVE 'BINZIP'     TO W-CHANNEL-JCLTYPE(IX)                 
040600             END-IF                                                       
040700         END-EVALUATE                                                     
040800       ELSE                                                               
040900*        -- NORMAL FORMDEF - SEND FORMATTED AS PDF ATTACHMENT             
041000         MOVE 'PDFATT'           TO W-CHANNEL-JCLTYPE(IX)                 
041100         IF W-CHANNEL-IDPFDEF (IX) (1:1) = "*"                            
041200*          -- FIX FOR SOME LANDSCAPE FORMS. SKIP LEADING *                
041300           MOVE W-CHANNEL-IDPFDEF (IX) (2:)                               
041400                                 TO W-CHANNEL-LAYOUT(IX)                  
041500           MOVE YES              TO W-CHANNEL-ESFADDR(IX)                 
041600         ELSE                                                             
041700           MOVE W-CHANNEL-IDPFDEF (IX)                                    
041800                                 TO W-CHANNEL-LAYOUT(IX)                  
041900           MOVE NOO              TO W-CHANNEL-ESFADDR(IX)                 
042000         END-IF                                                           
042100         INSPECT W-CHANNEL-LAYOUT(IX)                                     
042200           CONVERTING SMALL-LETTERS TO CAPS-LETTERS                       
042300         MOVE NOO                TO W-CHANNEL-EMULATE-CC(IX)              
042400         MOVE '.pdf'             TO W-ATTACH-SUFF                         
042500       END-IF                                                             
042600     ELSE                                                                 
042700*      -- NO LAYOUT - SEND AS UNFORMATTED INLINE TEXT                     
042800       MOVE 'INLINE'             TO W-CHANNEL-JCLTYPE(IX)                 
042900       MOVE SPACE                TO W-CHANNEL-LAYOUT(IX)                  
043000       MOVE YES                  TO W-CHANNEL-EMULATE-CC(IX)              
043100     END-IF                                                               
043200                                                                          
043300*    -- PREPARE JCL TO SUBMIT AN ACIF JOB                                 
043400                                                                          
043500     MOVE 'LOCAL'                TO WORK-ROUTE-XEQ                        
043600     IF IMSID4 = 'IMG0'                                                   
043700       MOVE '540WZ010100WZ11OUTO'                                         
043800                                 TO WORK-ACCOUNT                          
043900       MOVE 'CLASS=K'            TO WORK-MSGCLASS-CLASS                   
044000       MOVE '1800'               TO WORK-JOBFORMS                         
044100       MOVE 'W.QASE.PSF'         TO WORK-USERLIB                          
044200       MOVE 'W.QASE.PROCLIB'     TO WORK-PROCLIBS                         
044300       MOVE 'ENVQASE'            TO WORK-ENV                              
044400       MOVE 'SYSTZ'              TO WORK-SYSTZ                            
044500       MOVE 'WZ11.'              TO WORK-HLQ                              
044600       MOVE '****************************************'                    
044700                                 TO WS-HDR1-APIKEY-VALUE                  
044800       MOVE WS-AZ-SAS-PROD       TO WS-AZ-SAS                             
044900       MOVE WS-SL-HOST-PROD      TO WS-SL-HOST                            
045000       MOVE WS-SL-UKEY-PROD      TO WS-SL-UKEY                            
045100       MOVE WS-SL-PKEY-PROD      TO WS-SL-PKEY                            
045200     ELSE                                                                 
045300       MOVE '510WOS39000WZ11OUTO'                                         
045400                                 TO WORK-ACCOUNT                          
045500       MOVE 'MSGCLASS=H,CLASS=N' TO WORK-MSGCLASS-CLASS                   
045600       MOVE 'STD'                TO WORK-JOBFORMS                         
045700       IF IMSID4 = 'IMB0'                                                 
045800         MOVE 'W.ACPT.PSF,W.QASE.PSF'                                     
045900                                 TO WORK-USERLIB                          
046000         MOVE 'W.ACPT.PROCLIB,W.QASE.PROCLIB'                             
046100                                 TO WORK-PROCLIBS                         
046200         MOVE 'ENVACPT'          TO WORK-ENV                              
046300         MOVE 'SYSTZAA'          TO WORK-SYSTZ                            
046400         MOVE 'W.ACPT.'          TO WORK-HLQ                              
046500         MOVE '****************************************'                  
046600                                 TO WS-HDR1-APIKEY-VALUE                  
046700         MOVE WS-AZ-SAS-QA       TO WS-AZ-SAS                             
046800         MOVE WS-SL-HOST-TEST    TO WS-SL-HOST                            
046900         MOVE WS-SL-UKEY-TEST    TO WS-SL-UKEY                            
047000         MOVE WS-SL-PKEY-TEST    TO WS-SL-PKEY                            
047100       ELSE                                                               
047200         IF IMSID4 = 'IMD0'                                               
047300           MOVE 'W.XDEV.PSF,W.PROD.PSF'                                   
047400                                 TO WORK-USERLIB                          
047500           MOVE 'W.XDEV.PROCLIB,W.ACPT.PROCLIB,W.PROD.PROCLIB'            
047600                                 TO WORK-PROCLIBS                         
047700           MOVE 'ENVXDEV'        TO WORK-ENV                              
047800           MOVE 'SYSTZXX'        TO WORK-SYSTZ                            
047900           MOVE 'W.XDEV.'        TO WORK-HLQ                              
048000           MOVE '****************************************'                
048100                                 TO WS-HDR1-APIKEY-VALUE                  
048200           MOVE WS-AZ-SAS-QA     TO WS-AZ-SAS                             
048300           MOVE WS-SL-HOST-TEST  TO WS-SL-HOST                            
048400           MOVE WS-SL-UKEY-TEST  TO WS-SL-UKEY                            
048500           MOVE WS-SL-PKEY-TEST  TO WS-SL-PKEY                            
048600         ELSE                                                             
048700           IF IMSID4 = 'IMP0'                                             
048800             MOVE 'W.DEVE.PSF,W.PROD.PSF'                                 
048900                                 TO WORK-USERLIB                          
049000             MOVE 'W.DEVE.PROCLIB,W.IGRT.PROCLIB,W.PROD.PROCLIB'          
049100                                 TO WORK-PROCLIBS                         
049200             MOVE 'ENVDEVE'      TO WORK-ENV                              
049300             MOVE 'SYSTZDD'      TO WORK-SYSTZ                            
049400             MOVE 'W.DEVE.'      TO WORK-HLQ                              
049500             MOVE '****************************************'              
049600                                 TO WS-HDR1-APIKEY-VALUE                  
049700             MOVE WS-AZ-SAS-QA   TO WS-AZ-SAS                             
049800             MOVE WS-SL-HOST-TEST                                         
049900                                 TO WS-SL-HOST                            
050000             MOVE WS-SL-UKEY-TEST                                         
050100                                 TO WS-SL-UKEY                            
050200             MOVE WS-SL-PKEY-TEST                                         
050300                                 TO WS-SL-PKEY                            
050400           ELSE                                                           
050500             MOVE 'W.IGRT.PSF,W.PROD.PSF'                                 
050600                                 TO WORK-USERLIB                          
050700             MOVE 'W.IGRT.PROCLIB,W.ACPT.PROCLIB,W.PROD.PROCLIB'          
050800                                 TO WORK-PROCLIBS                         
050900             MOVE 'ENVIGRT'      TO WORK-ENV                              
051000             MOVE 'SYSTZTT'      TO WORK-SYSTZ                            
051100             MOVE 'W.IGRT.'      TO WORK-HLQ                              
051200             MOVE '****************************************'              
051300                                 TO WS-HDR1-APIKEY-VALUE                  
051400             MOVE WS-AZ-SAS-QA   TO WS-AZ-SAS                             
051500             MOVE WS-SL-HOST-TEST                                         
051600                                 TO WS-SL-HOST                            
051700             MOVE WS-SL-UKEY-TEST                                         
051800                                 TO WS-SL-UKEY                            
051900             MOVE WS-SL-PKEY-TEST                                         
052000                                 TO WS-SL-PKEY                            
052100           END-IF                                                         
052200         END-IF                                                           
052300       END-IF                                                             
052400     END-IF                                                               
052500                                                                          
052600     MOVE W-ATTACH-SUFF          TO W-CHANNEL-SUFF   (IX)                 
052700*    -- ATTACHMENT NAME IS Dyymmdd.Thhmmss.type                           
052800*    -- (THIS NAME IS MAKES IT POSSIBLE TO DIFFRENTIATE BETWEEN           
052900*    -- ATTACHMENTS FROM DIFFERENT TRANSMISSIONS BUT IS THE SAME          
053000*    -- FOR ALL CHANNELS/MAILS IN THIS TRANSMISSION)                      
053100     MOVE SPACE                  TO W-CHANNEL-ATTNAME(IX)                 
053200     STRING 'D' W-YYMMDD '-T' W-HHMMSS                                    
053300                            DELIMITED BY SIZE                             
053400            W-ATTACH-SUFF   DELIMITED BY SPACE                            
053500                               INTO W-CHANNEL-ATTNAME(IX)                 
053600                                                                          
053700*    -- FOR ZIP, THE NAME OF THE CONTENT MUST ALSO BE SPECIFIED           
053800*    -- THE CONTENT NAME IS Dyymmdd-Thhmmss.type                          
053900     MOVE SPACE                  TO W-CHANNEL-CONTENT(IX)                 
054000     STRING 'D' W-YYMMDD '.T' W-HHMMSS                                    
054100                            DELIMITED BY SIZE                             
054200            W-CONTENT-SUFF   DELIMITED BY SPACE                           
054300                               INTO W-CHANNEL-CONTENT(IX)                 
054400                                                                          
054500*    -- FOR PDF, THE NAME OF FILES IN THE LINUX ENVIRONMENT MUST          
054600*    -- ALSO BE SPECIFIED. THIS NAME IS ALSO USED AS PART OF              
054700*    -- THE OUTPUT PDF FILE IN THE MAINFRAME ENVIRONMENT.                 
054800*    -- THE FORMAT OF THE DSLUX NAME IS Dyymmdd.Thhmmss.Hthccc            
054900     MOVE SPACE                  TO W-CHANNEL-DSLUX(IX)                   
055000     MOVE OUTO-IDCALL            TO ZONED-CHANNEL                         
055100     STRING 'D' W-YYMMDD '.T' W-HHMMSS                                    
055200            '.H' W-TH ZONED-CHANNEL                                       
055300                            DELIMITED BY SIZE                             
055400                               INTO W-CHANNEL-DSLUX(IX)                   
055500                                                                          
055600     MOVE 'OPEN'                 TO SUBM-KDFUNC                           
055700     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
055800                                                                          
055900     MOVE 'PUT'                  TO SUBM-KDFUNC                           
056000     MOVE SPACE                  TO SUBM-LINE                             
056100     STRING 'WZ11' W-HHMMSS(3:4) DELIMITED BY SIZE                        
056200                               INTO WORK-JOBNAME                          
056300     STRING                                                               
056400       '//WZ11' W-HHMMSS(3:4) ' JOB ('         DELIMITED BY SIZE          
056500*      '//WZ110001'           ' JOB ('         DELIMITED BY SIZE          
056600       WORK-ACCOUNT                            DELIMITED BY SPACE         
056700       ',W100),'                               DELIMITED BY SIZE          
056800       QUOTE 'RTN WZ11S9' QUOTE ','            DELIMITED BY SIZE          
056900                               INTO SUBM-LINE                             
057000     END-STRING                                                           
057100     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
057200                                                                          
057300     MOVE SPACE                  TO SUBM-LINE                             
057400     CALL W980USPA            USING USPA-AREA                             
057500     MOVE SPACE                  TO SUBM-LINE                             
057600     STRING                                                               
057700       '//  USER='                  DELIMITED BY SIZE                     
057800       USPA-IDUSER                  DELIMITED BY SPACE                    
057900       ',PASSWORD='                 DELIMITED BY SIZE                     
058000       USPA-IDPW                    DELIMITED BY SPACE                    
058100       ','                          DELIMITED BY SIZE                     
058200                               INTO SUBM-LINE                             
058300     END-STRING                                                           
058400     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
058500                                                                          
058600     MOVE SPACE                  TO SUBM-LINE                             
058700     STRING                                                               
058800       '// MSGLEVEL=(1,1),'                    DELIMITED BY SIZE          
058900       WORK-MSGCLASS-CLASS                     DELIMITED BY SPACE         
059000                               INTO SUBM-LINE                             
059100     END-STRING                                                           
059200     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
059300                                                                          
059400     MOVE SPACE                  TO SUBM-LINE                             
059500     STRING                                                               
059600       '/*JOBPARM FORMS='                      DELIMITED BY SIZE          
059700       WORK-JOBFORMS                           DELIMITED BY SPACE         
059800       ',LINECT=0,LINES=9999'                  DELIMITED BY SIZE          
059900                               INTO SUBM-LINE                             
060000     END-STRING                                                           
060100     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
060200                                                                          
060300     MOVE SPACE                  TO SUBM-LINE                             
060400     STRING                                                               
060500       '/*ROUTE XEQ '                          DELIMITED BY SIZE          
060600       WORK-ROUTE-XEQ                          DELIMITED BY SPACE         
060700                               INTO SUBM-LINE                             
060800     END-STRING                                                           
060900     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
061000                                                                          
061100     IF IMSID4 = 'IMG0' OR 'IMB0'                                         
061200       MOVE '//*+JBS BIND VCC1'  TO SUBM-LINE                             
061300     ELSE                                                                 
061400       MOVE '//*+JBS BIND SG01'  TO SUBM-LINE                             
061500     END-IF                                                               
061600     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
061700                                                                          
061800     MOVE SPACE                  TO SUBM-LINE                             
061900     STRING                                                               
062000       '//PROC JCLLIB ORDER=('                 DELIMITED BY SIZE          
062100       WORK-PROCLIBS                           DELIMITED BY SPACE         
062200       ')'                                     DELIMITED BY SIZE          
062300                               INTO SUBM-LINE                             
062400     END-STRING                                                           
062500     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
062600                                                                          
062700     MOVE SPACE                  TO SUBM-LINE                             
062800     STRING                                                               
062900       '//ENV   INCL'                          DELIMITED BY SIZE          
063000       'UDE MEMBER='                           DELIMITED BY SIZE          
063100       WORK-ENV                                DELIMITED BY SPACE         
063200                               INTO SUBM-LINE                             
063300     END-STRING                                                           
063400     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
063500                                                                          
063600     MOVE SPACE                  TO SUBM-LINE                             
063700     STRING                                                               
063800       '//ENV   INCL'                          DELIMITED BY SIZE          
063900       'UDE MEMBER='                           DELIMITED BY SIZE          
064000       WORK-SYSTZ                              DELIMITED BY SPACE         
064100                               INTO SUBM-LINE                             
064200     END-STRING                                                           
064300     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
064400                                                                          
064500     MOVE SPACE                  TO SUBM-LINE                             
064600     STRING                                                               
064700       '//DATA    EXEC WZ14P005'               DELIMITED BY SIZE          
064800       W-COMMA-OR-SPACE                        DELIMITED BY SPACE         
064900                               INTO SUBM-LINE                             
065000     END-STRING                                                           
065100     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
065200                                                                          
065300*    -- FOR MAIL WITH SIMPLE TEXT ATTACHMENTS, THE OUTPUT FROM            
065400*    -- WZ14P005 IS SAVED IN A TEMPORARY ATTACHMENT FILE                  
065500     IF W-CHANNEL-JCLTYPE(IX) = 'TXTATT'                                  
065600       MOVE SPACE                TO SUBM-LINE                             
065700       STRING                                                             
065800         '// DSOUT=&&ATT'                      DELIMITED BY SIZE          
065900                               INTO SUBM-LINE                             
066000       END-STRING                                                         
066100       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
066200     END-IF                                                               
066300                                                                          
066400     IF W-CHANNEL-JCLTYPE(IX) = 'INLINE'                                  
066500*      -- WRITE "HEADER" DATA BEFORE APPLICATION DATA                     
066600       MOVE SPACE                TO WORK-DATACONT                         
066700       MOVE 50                   TO WORK-DATALENGTH                       
066800       MOVE 1                    TO IX2                                   
066900       PERFORM UNTIL IX2 > 5                                              
067000         IF W-CHANNEL-TEFAX(IX, IX2 ) NOT = SPACE                         
067100           MOVE W-CHANNEL-TEFAX(IX, IX2)                                  
067200                                 TO WORK-DATAPIECE                        
067300           MOVE WORK-DATALINE    TO SUBM-LINE                             
067400           CALL WZ20SUBM      USING SUBM-WZ20SUBM                         
067500         END-IF                                                           
067600         ADD 1                   TO IX2                                   
067700       END-PERFORM                                                        
067800     END-IF                                                               
067900                                                                          
068000     .                                                                    
068100                                                                          
068200 C-WRITE-ONE-RECORD SECTION.                                              
068300                                                                          
068400     IF W-CHANNEL-IDFORMSNM (IX) > SPACES                                 
068500       PERFORM CA-WRITE-ONE-RECORD-ONDEMAND                               
068600     END-IF                                                               
068700     IF W-CHANNEL-IDOUTDEST (IX) > SPACES                                 
068800       PERFORM CB-WRITE-ONE-RECORD-SMARTFACTS                             
068900     END-IF                                                               
069000     .                                                                    
069100                                                                          
069200 CA-WRITE-ONE-RECORD-ONDEMAND SECTION.                                    
069300                                                                          
069400*    When the record is meta data, dont process it. It's not              
069500*    needed here.                                                         
069600     IF OUTO-TEOUTDATA (1:5) = '¤META' OR                                 
069700        OUTO-TEOUTDATA (2:5) = '¤META'                                    
069800       CONTINUE                                                           
069900     ELSE                                                                 
070000*    -- INITIALIZE PUT ARGUMENTS TO THE PRINT MODULE (OUTP)               
070100       MOVE OUTO-PUT-PARAMETERS  TO OUTP-PUT-PARAMETERS                   
070200                                                                          
070300*    -- LET OUTP DO THE JOB                                               
070400       CALL WZ11OUTP          USING OUTP-WZ11OUT                          
070500     END-IF                                                               
070600     .                                                                    
070700                                                                          
070800 CB-WRITE-ONE-RECORD-SMARTFACTS SECTION.                                  
070900                                                                          
071000     IF OUTO-TEOUTDATA (1:5) = '¤META' OR                                 
071100        OUTO-TEOUTDATA (2:5) = '¤META'                                    
071200       IF OUTO-TEOUTDATA (1:5) = '¤META'                                  
071300         UNSTRING OUTO-TEOUTDATA (6: OUTO-TEOUTDATA-L - 5)                
071400              DELIMITED BY '=' INTO WS-KEY                                
071500                                    WS-VALUE                              
071600       ELSE                                                               
071700         UNSTRING OUTO-TEOUTDATA (7: OUTO-TEOUTDATA-L - 6)                
071800              DELIMITED BY '=' INTO WS-KEY                                
071900                                    WS-VALUE                              
072000       END-IF                                                             
072100       INSPECT WS-KEY                                                     
072200         CONVERTING CAPS-LETTERS TO SMALL-LETTERS                         
072300       IF WS-KEY = 'file_name'                                            
072400         STRING FUNCTION TRIM (WS-VALUE)                                  
072500                FUNCTION TRIM (W-CHANNEL-SUFF (IX))                       
072600                       DELIMITED BY SIZE                                  
072700                               INTO W-CHANNEL-FILENAME (IX)               
072800         END-STRING                                                       
072900       END-IF                                                             
073000       IF WS-KEY = 'document_number'                                      
073100*        Save document number for later use                               
073200         MOVE WS-VALUE           TO W-CHANNEL-DOC-NBR (IX)                
073300         IF W-CHANNEL-FILENAME (IX) = SPACES                              
073400           STRING FUNCTION TRIM (WS-VALUE)                                
073500                  '_'                                                     
073600                  FUNCTION TRIM (W-CHANNEL-ATTNAME(IX))                   
073700                       DELIMITED BY SIZE                                  
073800                               INTO W-CHANNEL-FILENAME (IX)               
073900           END-STRING                                                     
074000         END-IF                                                           
074100       END-IF                                                             
074200       IF WS-KEY = 'file_name'                                            
074300         CONTINUE                                                         
074400       ELSE                                                               
074500         COMPUTE W-CHANNEL-TIX (IX) = W-CHANNEL-TIX (IX) + 1              
074600         SET TAB-IX              TO W-CHANNEL-TIX (IX)                    
074700         MOVE FUNCTION TRIM (WS-KEY)                                      
074800                                 TO W-TAB-KEY   (IX, TAB-IX)              
074900         MOVE FUNCTION TRIM (WS-VALUE)                                    
075000                                 TO W-TAB-VALUE (IX, TAB-IX)              
075100       END-IF                                                             
075200     ELSE                                                                 
075300       MOVE 'PUT'                TO SUBM-KDFUNC                           
075400                                                                          
075500*    -- EMULATE ASA CARRIAGE-CONTROL FUNCTIONALITY, IF SPECIFIED,         
075600*    -- BY PUTTING EXTRA BLANK LINES BEFORE THE DATA LINE                 
075700       IF W-CHANNEL-FLCARRCNTL(IX) = YES                                  
075800       AND W-CHANNEL-EMULATE-CC(IX) = YES                                 
075900         MOVE SPACE              TO WORK-DATALINE                         
076000         MOVE 2                  TO WORK-DATALENGTH                       
076100         MOVE WORK-DATALINE      TO SUBM-LINE                             
076200         IF OUTO-TEOUTDATA(1:1) = '0' OR '-' OR '1'                       
076300           CALL WZ20SUBM      USING SUBM-WZ20SUBM                         
076400         END-IF                                                           
076500         IF OUTO-TEOUTDATA(1:1) = '-' OR '1'                              
076600           CALL WZ20SUBM      USING SUBM-WZ20SUBM                         
076700         END-IF                                                           
076800       END-IF                                                             
076900                                                                          
077000       MOVE 1                    TO STRING-PTR                            
077100       MOVE SPACE                TO WORK-DATALINE                         
077200                                                                          
077300       PERFORM UNTIL STRING-PTR > OUTO-TEOUTDATA-L                        
077400         COMPUTE WORK-DATALENGTH = OUTO-TEOUTDATA-L                       
077500                                   - STRING-PTR + 1                       
077600         IF WORK-DATALENGTH > 77                                          
077700           MOVE 77               TO WORK-DATALENGTH                       
077800         END-IF                                                           
077900         MOVE OUTO-TEOUTDATA(STRING-PTR:WORK-DATALENGTH)                  
078000                                 TO WORK-DATAPIECE                        
078100         IF W-CHANNEL-FLCARRCNTL(IX) = YES                                
078200         AND W-CHANNEL-EMULATE-CC(IX) = YES                               
078300         AND WORK-DATACONT NOT = '*'                                      
078400*          -- BLANK OUT CC DATA IF CC IS EMULATED                         
078500*          -- (START LINE ONLY, NOT ON CONTINUATION LINES)                
078600           MOVE SPACE            TO WORK-DATAPIECE(1:1)                   
078700         END-IF                                                           
078800                                                                          
078900         MOVE WORK-DATALINE      TO SUBM-LINE                             
079000         CALL WZ20SUBM        USING SUBM-WZ20SUBM                         
079100                                                                          
079200         MOVE '*'                TO WORK-DATACONT                         
079300         ADD WORK-DATALENGTH     TO STRING-PTR                            
079400       END-PERFORM                                                        
079500     END-IF                                                               
079600                                                                          
079700     .                                                                    
079800                                                                          
079900 D-CLOSE-CHANNEL SECTION.                                                 
080000                                                                          
080100     IF W-CHANNEL-IDFORMSNM (IX) > SPACES                                 
080200       PERFORM DA-CLOSE-CHANNEL-ONDEMAND                                  
080300     END-IF                                                               
080400     IF W-CHANNEL-IDOUTDEST (IX) > SPACES                                 
080500       PERFORM DB-CLOSE-CHANNEL-SMARTFACTS                                
080600     END-IF                                                               
080700     .                                                                    
080800                                                                          
080900 DA-CLOSE-CHANNEL-ONDEMAND SECTION.                                       
081000                                                                          
081100*    -- LET OUTP DO THE JOB                                               
081200     CALL WZ11OUTP               USING OUTP-WZ11OUT                       
081300     .                                                                    
081400                                                                          
081500 DB-CLOSE-CHANNEL-SMARTFACTS SECTION.                                     
081600                                                                          
081700     MOVE 1                      TO CURL-TEXT-PTR                         
081800                                                                          
081900     IF W-CHANNEL-FILENAME (IX) = SPACES                                  
082000       MOVE W-CHANNEL-ATTNAME (IX)                                        
082100                                 TO W-CHANNEL-FILENAME (IX)               
082200     END-IF                                                               
082300     INSPECT W-CHANNEL-FILENAME (IX) CONVERTING '-' TO '_'                
082400                                                                          
082500                                                                          
082600     IF W-CHANNEL-IDOUTDEST (IX) (1:5) = 'SHELF'                          
082700       STRING 'SH curl -k --fail --retry 5 --retry-delay 10 '             
082800                          DELIMITED BY SIZE                               
082900            ' -L "'                                                       
083000                          DELIMITED BY SIZE                               
083100            WS-HOST       DELIMITED BY SIZE                               
083200            WS-PATH1      DELIMITED BY SIZE                               
083300            W-CHANNEL-IDOUTDEST (IX)                                      
083400                          DELIMITED BY SPACE                              
083500            WS-PATH2      DELIMITED BY SIZE                               
083600            '?site=VCC&filename='                                         
083700                          DELIMITED BY SIZE                               
083800            W-CHANNEL-FILENAME (IX)                                       
083900                          DELIMITED BY SPACE                              
084000            '"'           DELIMITED BY SIZE                               
084100                                                                          
084200            ' --data-binary "@/tmp/'                                      
084300                          DELIMITED BY SIZE                               
084400            W-CHANNEL-FILENAME (IX)                                       
084500                          DELIMITED BY SPACE                              
084600            '"'           DELIMITED BY SIZE                               
084700                                                                          
084800            ' -H '                                                        
084900                          DELIMITED BY SIZE                               
085000            '"x-api-key: '                                                
085100                          DELIMITED BY SIZE                               
085200            WS-HDR1-APIKEY-VALUE                                          
085300                          DELIMITED BY SPACE                              
085400            '"'           DELIMITED BY SIZE                               
085500                               INTO WS-CURL-TEXT-278                      
085600         WITH POINTER CURL-TEXT-PTR                                       
085700       END-STRING                                                         
085800       MOVE 'smart-fact-meta-'   TO WS-META-PREFIX                        
085900     ELSE                                                                 
086000                                                                          
086100       UNSTRING W-CHANNEL-IDOUTDEST (IX)                                  
086200                          DELIMITED BY '/'                                
086300                               INTO WS-AZ-STORAGE-ACC                     
086400                                    WS-AZ-CONTAINER                       
086500                                                                          
086600       STRING 'SH curl -k --fail --retry 5 --retry-delay 10 '             
086700                          DELIMITED BY SIZE                               
086800            ' -L --request PUT "https://'                                 
086900                          DELIMITED BY SIZE                               
087000            FUNCTION LOWER-CASE(WS-AZ-STORAGE-ACC)                        
087100                          DELIMITED BY SPACE                              
087200            '.blob.core.windows.net/'                                     
087300                          DELIMITED BY SIZE                               
087400            FUNCTION LOWER-CASE(WS-AZ-CONTAINER)                          
087500                          DELIMITED BY SPACE                              
087600            '/'           DELIMITED BY SIZE                               
087700            W-CHANNEL-FILENAME (IX)                                       
087800                          DELIMITED BY SPACE                              
087900            WS-AZ-SAS     DELIMITED BY SPACE                              
088000            '"'           DELIMITED BY SIZE                               
088100                                                                          
088200            ' --data-binary "@/tmp/'                                      
088300                          DELIMITED BY SIZE                               
088400            W-CHANNEL-FILENAME (IX)                                       
088500                          DELIMITED BY SPACE                              
088600            '"'           DELIMITED BY SIZE                               
088700                                                                          
088800            ' -H '                                                        
088900                          DELIMITED BY SIZE                               
089000            '"x-ms-blob-type: blockblob"'                                 
089100                          DELIMITED BY SIZE                               
089200                                                                          
089300                               INTO WS-CURL-TEXT-278                      
089400         WITH POINTER CURL-TEXT-PTR                                       
089500       END-STRING                                                         
089600       MOVE 'x-ms-meta-'         TO WS-META-PREFIX                        
089700     END-IF                                                               
089800                                                                          
089900     PERFORM                                                              
090000     VARYING IX2 FROM 1 BY 1                                              
090100       UNTIL IX2 > 15 OR                                                  
090200             W-TAB-KEY (IX, IX2) = SPACES                                 
090300       IF W-TAB-VALUE (IX, IX2) = SPACES                                  
090400         CONTINUE                                                         
090500       ELSE                                                               
090600         MOVE 001                TO URL-KDCALL                            
090700         COMPUTE URL-KVDLEN-IN = FUNCTION LENGTH (                        
090800                                 FUNCTION TRIM (                          
090900                                   W-TAB-VALUE (IX, IX2)))                
091000         MOVE FUNCTION TRIM(W-TAB-VALUE (IX, IX2))                        
091100                                 TO WS-TEXT-TO-ENC                        
091200                                                                          
091300         CALL WURLCONV        USING URL-CONTROL-AREA                      
091400                                    URL-KVDLEN-IN                         
091500                                    WS-TEXT-TO-ENC                        
091600                                    URL-KVDLEN-OUT                        
091700                                    WS-ENC-TEXT                           
091800                                                                          
091900         STRING ' -H "'                                                   
092000                          DELIMITED BY SIZE                               
092100                WS-META-PREFIX                                            
092200                          DELIMITED BY SPACE                              
092300                W-TAB-KEY (IX, IX2)                                       
092400                          DELIMITED BY SPACES                             
092500                ': '    DELIMITED BY SIZE                                 
092600                WS-ENC-TEXT (1:URL-KVDLEN-OUT)                            
092700                          DELIMITED BY SIZE                               
092800                '"'       DELIMITED BY SIZE                               
092900                               INTO WS-CURL-TEXT-278                      
093000           WITH POINTER CURL-TEXT-PTR                                     
093100         END-STRING                                                       
093200       END-IF                                                             
093300     END-PERFORM                                                          
093400                                                                          
093500     MOVE FUNCTION DISPLAY-OF (                                           
093600          FUNCTION NATIONAL-OF (WS-CURL-TEXT-278, 278)                    
093700                                , 1047)                                   
093800                                 TO WS-CURL-TEXT                          
093900                                                                          
094000     COMPUTE CURL-TEXT-LEN = CURL-TEXT-PTR - 1                            
094100                                                                          
094200D    DISPLAY WS-CURL-TEXT (1 : CURL-TEXT-LEN)                             
094300                                                                          
094400     MOVE 'PUT'                  TO SUBM-KDFUNC                           
094500                                                                          
094600     MOVE SPACE                  TO SUBM-LINE                             
094700     STRING                                                               
094800          '//IFDATA I'                                                    
094900          'F (DATA.WZ1405.RC = 0) THEN'                                   
095000       DELIMITED BY SIZE                                                  
095100                               INTO SUBM-LINE                             
095200     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
095300                                                                          
095400                                                                          
095500     IF W-CHANNEL-JCLTYPE(IX) = 'PDFATT' OR 'PDFZIP'                      
095600*      -- JCL FOR ACIF AND PDF CREATION                                   
095700       MOVE '//PDF    EXEC WZ11PDF,DSIN=&&DATA,'                          
095800                                 TO SUBM-LINE                             
095900       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
096000                                                                          
096100       IF W-CHANNEL-ESFADDR(IX) = YES                                     
096200         MOVE '//            ESFADDR=,'                                   
096300                                 TO SUBM-LINE                             
096400         CALL WZ20SUBM        USING SUBM-WZ20SUBM                         
096500       END-IF                                                             
096600                                                                          
096700       MOVE SPACE                TO SUBM-LINE                             
096800       STRING                                                             
096900         '// DSOUTPDF='                        DELIMITED BY SIZE          
097000         WORK-HLQ                              DELIMITED BY SPACE         
097100         WORK-QUAL-PDF                         DELIMITED BY SPACE         
097200         W-CHANNEL-DSLUX(IX)                   DELIMITED BY SPACE         
097300         ','                                   DELIMITED BY SIZE          
097400                               INTO SUBM-LINE                             
097500       END-STRING                                                         
097600       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
097700                                                                          
097800       MOVE '// PDFDISP=(NEW,PASS,DELETE)'                                
097900                                 TO SUBM-LINE                             
098000       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
098100                                                                          
098200       MOVE '//ACIF.ACIFCMDS DD *'                                        
098300                                 TO SUBM-LINE                             
098400       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
098500                                                                          
098600       IF W-CHANNEL-FLCARRCNTL(IX) = YES                                  
098700         MOVE 'YES'              TO WORK-CC                               
098800       ELSE                                                               
098900         MOVE 'NO'               TO WORK-CC                               
099000       END-IF                                                             
099100       MOVE SPACE                TO SUBM-LINE                             
099200       STRING                                                             
099300         ' CC='                                DELIMITED BY SIZE          
099400         WORK-CC                               DELIMITED BY SPACE         
099500                               INTO SUBM-LINE                             
099600       END-STRING                                                         
099700       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
099800                                                                          
099900       MOVE SPACE                TO SUBM-LINE                             
100000       STRING                                                             
100100         ' USERLIB='                           DELIMITED BY SIZE          
100200         WORK-USERLIB                          DELIMITED BY SPACE         
100300                               INTO SUBM-LINE                             
100400       END-STRING                                                         
100500       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
100600                                                                          
100700       MOVE SPACE                TO SUBM-LINE                             
100800       STRING                                                             
100900         ' FORMDEF=F1'                         DELIMITED BY SIZE          
101000         W-CHANNEL-LAYOUT(IX)         DELIMITED BY SPACE                  
101100                               INTO SUBM-LINE                             
101200       END-STRING                                                         
101300       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
101400                                                                          
101500       MOVE SPACE                TO SUBM-LINE                             
101600       STRING                                                             
101700         ' PAGEDEF=P1'                         DELIMITED BY SIZE          
101800         W-CHANNEL-LAYOUT(IX)         DELIMITED BY SPACE                  
101900                               INTO SUBM-LINE                             
102000       END-STRING                                                         
102100       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
102200     END-IF                                                               
102300                                                                          
102400                                                                          
102500     IF W-CHANNEL-JCLTYPE(IX) = 'TXTZIP'                                  
102600*      -- JCL FOR ZIPPED TEXT FILE ATTACHMENT -----                       
102700       MOVE '//TZIP   EXEC WZ11TZIP,DSIN=&&DATA,'                         
102800                                 TO SUBM-LINE                             
102900       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
103000                                                                          
103100       MOVE                                                               
103200         '// DSOUTZIP=&&ATT,ZIPDISP=(NEW,PASS,DELETE),'                   
103300                                 TO SUBM-LINE                             
103400       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
103500                                                                          
103600       MOVE SPACE                TO SUBM-LINE                             
103700       STRING                                                             
103800         '// CONTENT='                         DELIMITED BY SIZE          
103900         W-CHANNEL-CONTENT(IX)        DELIMITED BY SPACE                  
104000                               INTO SUBM-LINE                             
104100       END-STRING                                                         
104200       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
104300     END-IF                                                               
104400                                                                          
104500     IF W-CHANNEL-JCLTYPE(IX) =  'PDFZIP'                                 
104600                                                                          
104700*      -- JCL FOR ZIPPED PDF FILE ATTACHMENT -----                        
104800       MOVE SPACE                TO SUBM-LINE                             
104900       STRING                                                             
105000         '//BZIP   EXEC WZ11BZIP,'             DELIMITED BY SIZE          
105100                               INTO SUBM-LINE                             
105200       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
105300                                                                          
105400       MOVE SPACE                TO SUBM-LINE                             
105500       STRING                                                             
105600         '// DSIN='                            DELIMITED BY SIZE          
105700         WORK-HLQ                              DELIMITED BY SPACE         
105800         WORK-QUAL-PDF                         DELIMITED BY SPACE         
105900         W-CHANNEL-DSLUX(IX)                   DELIMITED BY SPACE         
106000         ','                                   DELIMITED BY SIZE          
106100                               INTO SUBM-LINE                             
106200       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
106300                                                                          
106400       MOVE                                                               
106500         '// DSOUTZIP=&&ATT,ZIPDISP=(NEW,PASS,DELETE),'                   
106600                                 TO SUBM-LINE                             
106700       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
106800                                                                          
106900       MOVE SPACE                TO SUBM-LINE                             
107000       STRING                                                             
107100         '// CONTENT='                         DELIMITED BY SIZE          
107200         W-CHANNEL-CONTENT(IX)                 DELIMITED BY SPACE         
107300                               INTO SUBM-LINE                             
107400       END-STRING                                                         
107500       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
107600                                                                          
107700     END-IF                                                               
107800                                                                          
107900     IF W-CHANNEL-JCLTYPE(IX) =  'BINZIP'                                 
108000*      -- JCL FOR ZIPPED UNSPECIFIED BINARY FILE ATTACHMENT -----         
108100                                                                          
108200       MOVE                                                               
108300         '//BZIP   EXEC WZ11BZIP,DSIN=&&DATA,'                            
108400                                 TO SUBM-LINE                             
108500       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
108600                                                                          
108700       MOVE                                                               
108800         '// DSOUTZIP=&&ATT,ZIPDISP=(NEW,PASS,DELETE),'                   
108900                                 TO SUBM-LINE                             
109000       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
109100                                                                          
109200       MOVE SPACE                TO SUBM-LINE                             
109300       STRING                                                             
109400         '// CONTENT='                         DELIMITED BY SIZE          
109500         W-CHANNEL-CONTENT(IX)                 DELIMITED BY SPACE         
109600                               INTO SUBM-LINE                             
109700       END-STRING                                                         
109800       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
109900                                                                          
110000     END-IF                                                               
110100                                                                          
110200                                                                          
110300                                                                          
110400     MOVE '//USSCPY EXEC W001HFSC,CONV=''NO'',MODE=''BINARY'','           
110500                                 TO SUBM-LINE                             
110600     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
110700                                                                          
110800     MOVE SPACES                 TO SUBM-LINE                             
110900     STRING                                                               
111000       '// DSIN='                              DELIMITED BY SIZE          
111100       WORK-HLQ                                DELIMITED BY SPACE         
111200       WORK-QUAL-PDF                           DELIMITED BY SPACE         
111300       W-CHANNEL-DSLUX(IX)                     DELIMITED BY SPACE         
111400       ','                                     DELIMITED BY SIZE          
111500                               INTO SUBM-LINE                             
111600     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
111700                                                                          
111800     MOVE SPACES                 TO SUBM-LINE                             
111900     STRING                                                               
112000       '// PATHOUT=''/tmp/'                    DELIMITED BY SIZE          
112100       W-CHANNEL-FILENAME (IX)                 DELIMITED BY SPACE         
112200       ''''                                    DELIMITED BY SIZE          
112300                               INTO SUBM-LINE                             
112400     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
112500                                                                          
112600     MOVE '//CURL EXEC WZ14P005,'                                         
112700                                 TO SUBM-LINE                             
112800     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
112900                                                                          
113000     MOVE SPACES                 TO SUBM-LINE                             
113100     STRING                                                               
113200       '// DSOUT='                             DELIMITED BY SIZE          
113300       WORK-HLQ                                DELIMITED BY SPACE         
113400       WORK-QUAL-CURL                          DELIMITED BY SPACE         
113500       W-CHANNEL-DSLUX(IX)                     DELIMITED BY SPACE         
113600       ','                                     DELIMITED BY SIZE          
113700                               INTO SUBM-LINE                             
113800     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
113900                                                                          
114000     MOVE '// DISPOUT=CATLG'     TO SUBM-LINE                             
114100     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
114200                                                                          
114300     MOVE 1                      TO CURL-TEXT-PTR                         
114400     MOVE SPACE                  TO WORK-DATALINE                         
114500                                                                          
114600     PERFORM                                                              
114700       UNTIL CURL-TEXT-PTR > CURL-TEXT-LEN                                
114800       COMPUTE WORK-DATALENGTH = CURL-TEXT-LEN                            
114900                                   - CURL-TEXT-PTR + 1                    
115000       IF WORK-DATALENGTH > 77                                            
115100         MOVE 77                 TO WORK-DATALENGTH                       
115200       END-IF                                                             
115300       MOVE WS-CURL-TEXT(CURL-TEXT-PTR:WORK-DATALENGTH)                   
115400                                 TO WORK-DATAPIECE                        
115500       MOVE WORK-DATALINE        TO SUBM-LINE                             
115600       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
115700                                                                          
115800       MOVE '*'                  TO WORK-DATACONT                         
115900       ADD WORK-DATALENGTH       TO CURL-TEXT-PTR                         
116000     END-PERFORM                                                          
116100                                                                          
116200     MOVE '//CURLEXEC EXEC PGM=BPXBATCH,REGION=0M'                        
116300                                 TO SUBM-LINE                             
116400     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
116500                                                                          
116600     MOVE '//STDOUT DD SYSOUT=*' TO SUBM-LINE                             
116700     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
116800                                                                          
116900     MOVE '//STDERR DD SYSOUT=*' TO SUBM-LINE                             
117000     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
117100                                                                          
117200     MOVE SPACES                 TO SUBM-LINE                             
117300     STRING                                                               
117400       '//STDPARM DD DSN='                     DELIMITED BY SIZE          
117500       WORK-HLQ                                DELIMITED BY SPACE         
117600       WORK-QUAL-CURL                          DELIMITED BY SPACE         
117700       W-CHANNEL-DSLUX(IX)                     DELIMITED BY SPACE         
117800       ','                                     DELIMITED BY SIZE          
117900                               INTO SUBM-LINE                             
118000     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
118100                                                                          
118200     MOVE '// DISP=(SHR,DELETE,DELETE)'                                   
118300                                 TO SUBM-LINE                             
118400     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
118500                                                                          
118600     MOVE SPACES                 TO SUBM-LINE                             
118700     STRING                                                               
118800       '//DD1 DD PATH=''/tmp/'                 DELIMITED BY SIZE          
118900       W-CHANNEL-FILENAME (IX)                 DELIMITED BY SPACE         
119000       ''','                                   DELIMITED BY SIZE          
119100                               INTO SUBM-LINE                             
119200     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
119300                                                                          
119400     MOVE '// PATHDISP=(DELETE,DELETE)'                                   
119500                                 TO SUBM-LINE                             
119600     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
119700                                                                          
119800     MOVE SPACE                  TO SUBM-LINE                             
119900     STRING                                                               
120000          '//SIGNAL IF (ABEND OR CURLEXEC.RC > 0)'                        
120100          ' THEN '                           DELIMITED BY SIZE            
120200                               INTO SUBM-LINE                             
120300     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
120400                                                                          
120500     MOVE '//SIGNAL  EXEC WSOP'  TO SUBM-LINE                             
120600     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
120700                                                                          
120800     MOVE ' ORDER WZ11JABE SYMBOLS'                                       
120900                                 TO SUBM-LINE                             
121000     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
121100                                                                          
121200     MOVE ' ACTION(sending to smartfacts)'                                
121300                                 TO SUBM-LINE                             
121400     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
121500                                                                          
121600     MOVE SPACE                  TO SUBM-LINE                             
121700     STRING                                                               
121800       ' DEST('                              DELIMITED BY SIZE            
121900       FUNCTION TRIM(W-CHANNEL-IDOUTDEST(IX))                             
122000                                             DELIMITED BY SIZE            
122100       ')'                                   DELIMITED BY SIZE            
122200                               INTO SUBM-LINE                             
122300     END-STRING                                                           
122400     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
122500                                                                          
122600     MOVE SPACE                  TO SUBM-LINE                             
122700     STRING                                                               
122800       ' TIMESTAMP('                         DELIMITED BY SIZE            
122900       W-YYMMDD                              DELIMITED BY SIZE            
123000       ' '                                   DELIMITED BY SIZE            
123100       W-HHMMSS                              DELIMITED BY SIZE            
123200       ') JOBNAME(WZ11'                      DELIMITED BY SIZE            
123300       W-HHMMSS(3:4)                         DELIMITED BY SIZE            
123400       ')'                                   DELIMITED BY SIZE            
123500                               INTO SUBM-LINE                             
123600     END-STRING                                                           
123700     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
123800                                                                          
123900     IF W-CHANNEL-IDOUTTYPE = SPACES                                      
124000       MOVE ' INFO( )'           TO SUBM-LINE                             
124100       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
124200                                                                          
124300       MOVE ' TYPE( )'           TO SUBM-LINE                             
124400       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
124500                                                                          
124600       MOVE ' REC( )'            TO SUBM-LINE                             
124700       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
124800                                                                          
124900       MOVE ' LIST( )'           TO SUBM-LINE                             
125000       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
125100                                                                          
125200       MOVE ' REGDAT( )'         TO SUBM-LINE                             
125300       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
125400                                                                          
125500       MOVE ' KLOCK( )'          TO SUBM-LINE                             
125600       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
125700     ELSE                                                                 
125800       MOVE ' INFO(Restart Keys:)'                                        
125900                                 TO SUBM-LINE                             
126000       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
126100                                                                          
126200       MOVE SPACE                TO SUBM-LINE                             
126300       STRING                                                             
126400         ' TYPE(Output Type    : '           DELIMITED BY SIZE            
126500         FUNCTION TRIM(W-CHANNEL-IDOUTTYPE)                               
126600                                             DELIMITED BY SIZE            
126700         ')'                                 DELIMITED BY SIZE            
126800                               INTO SUBM-LINE                             
126900       END-STRING                                                         
127000       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
127100                                                                          
127200       MOVE SPACE                TO SUBM-LINE                             
127300       STRING                                                             
127400         ' REC(Output SubType : '            DELIMITED BY SIZE            
127500         FUNCTION TRIM(W-CHANNEL-IDOUTREC)                                
127600                                             DELIMITED BY SIZE            
127700         ')'                                 DELIMITED BY SIZE            
127800                               INTO SUBM-LINE                             
127900       END-STRING                                                         
128000       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
128100                                                                          
128200       MOVE SPACE                TO SUBM-LINE                             
128300       STRING                                                             
128400         ' LIST(Output Id      : '           DELIMITED BY SIZE            
128500         FUNCTION TRIM(W-CHANNEL-IDLIST)                                  
128600                                             DELIMITED BY SIZE            
128700         ')'                                 DELIMITED BY SIZE            
128800                               INTO SUBM-LINE                             
128900       END-STRING                                                         
129000       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
129100                                                                          
129200       MOVE SPACE                TO SUBM-LINE                             
129300       STRING                                                             
129400         ' REGDAT(Date           : '         DELIMITED BY SIZE            
129500         W-CHANNEL-TIREGDAT                  DELIMITED BY SPACE           
129600         ')'                                 DELIMITED BY SIZE            
129700                               INTO SUBM-LINE                             
129800       END-STRING                                                         
129900       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
130000                                                                          
130100       MOVE SPACE                TO SUBM-LINE                             
130200       STRING                                                             
130300         ' KLOCK(Time           : '          DELIMITED BY SIZE            
130400         W-CHANNEL-TIKLOCK                   DELIMITED BY SPACE           
130500         ')'                                 DELIMITED BY SIZE            
130600                               INTO SUBM-LINE                             
130700       END-STRING                                                         
130800       CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
130900     END-IF                                                               
131000                                                                          
131100     MOVE ' END-ORDER'           TO SUBM-LINE                             
131200     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
131300                                                                          
131400     MOVE ZERO                   TO TALLY                                 
131500     INSPECT W-CHANNEL-IDOUTDEST(IX) TALLYING TALLY                       
131600             FOR ALL 'VCCS_INVOICE'                                       
131700     IF TALLY = 0                                                         
131800       INSPECT W-CHANNEL-IDOUTDEST(IX) TALLYING TALLY                     
131900             FOR ALL 'VCSC_INVOICE'                                       
132000     END-IF                                                               
132100                                                                          
132200*    IF TALLY > 0 AND W-CHANNEL-DOC-NBR (IX) > SPACES                     
132300*      MOVE 1                    TO CURL-TEXT-PTR                         
132400*                                                                         
132500*      STRING 'SH curl -k --fail --retry 5 --retry-delay 10 '             
132600*                         DELIMITED BY SIZE                               
132700*           ' -L --request POST "'                                        
132800*                         DELIMITED BY SIZE                               
132900*           WS-SL-HOST    DELIMITED BY SPACE                              
133000*           '/b2b/puls/bakeryEUwest/parceltransport/invoice/nsc/'         
133100*                         DELIMITED BY SIZE                               
133200*           FUNCTION TRIM(W-CHANNEL-DOC-NBR (IX))                         
133300*                         DELIMITED BY SIZE                               
133400*           '/confirmed"'                                                 
133500*                         DELIMITED BY SIZE                               
133600*                                                                         
133700*           ' -d ""'                                                      
133800*                         DELIMITED BY SIZE                               
133900*                                                                         
134000*           ' -H "user-key: '                                             
134100*                         DELIMITED BY SIZE                               
134200*           WS-SL-UKEY                                                    
134300*                         DELIMITED BY SPACE                              
134400*           '"'           DELIMITED BY SIZE                               
134500*                                                                         
134600*           ' -H "proxy-key: '                                            
134700*                         DELIMITED BY SIZE                               
134800*           WS-SL-PKEY                                                    
134900*                         DELIMITED BY SPACE                              
135000*           '"'           DELIMITED BY SIZE                               
135100*                              INTO WS-CURL-TEXT-278                      
135200*        WITH POINTER CURL-TEXT-PTR                                       
135300*      END-STRING                                                         
135400*                                                                         
135500*      MOVE FUNCTION DISPLAY-OF (                                         
135600*           FUNCTION NATIONAL-OF (WS-CURL-TEXT-278, 278)                  
135700*                                 , 1047)                                 
135800*                                TO WS-CURL-TEXT                          
135900*                                                                         
136000*      COMPUTE CURL-TEXT-LEN = CURL-TEXT-PTR - 1                          
136100*                                                                         
136200*      MOVE '//SIGNAL ELSE'      TO SUBM-LINE                             
136300*      CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
136400*                                                                         
136500*      MOVE '//CURL EXEC WZ14P005,'                                       
136600*                                TO SUBM-LINE                             
136700*      CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
136800*                                                                         
136900*      MOVE SPACES               TO SUBM-LINE                             
137000*      STRING                                                             
137100*        '// DSOUT='                           DELIMITED BY SIZE          
137200*        WORK-HLQ                              DELIMITED BY SPACE         
137300*        WORK-QUAL-CURL2                       DELIMITED BY SPACE         
137400*        W-CHANNEL-DSLUX(IX)                   DELIMITED BY SPACE         
137500*        ','                                   DELIMITED BY SIZE          
137600*                              INTO SUBM-LINE                             
137700*      CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
137800*                                                                         
137900*      MOVE '// DISPOUT=CATLG'   TO SUBM-LINE                             
138000*      CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
138100*                                                                         
138200*      MOVE 1                    TO CURL-TEXT-PTR                         
138300*      MOVE SPACE                TO WORK-DATALINE                         
138400*                                                                         
138500*      PERFORM                                                            
138600*        UNTIL CURL-TEXT-PTR > CURL-TEXT-LEN                              
138700*        COMPUTE WORK-DATALENGTH = CURL-TEXT-LEN                          
138800*                                  - CURL-TEXT-PTR + 1                    
138900*        IF WORK-DATALENGTH > 77                                          
139000*          MOVE 77               TO WORK-DATALENGTH                       
139100*        END-IF                                                           
139200*        MOVE WS-CURL-TEXT(CURL-TEXT-PTR:WORK-DATALENGTH)                 
139300*                                  TO WORK-DATAPIECE                      
139400*        MOVE WORK-DATALINE      TO SUBM-LINE                             
139500*        CALL WZ20SUBM        USING SUBM-WZ20SUBM                         
139600*                                                                         
139700*        MOVE '*'                TO WORK-DATACONT                         
139800*        ADD WORK-DATALENGTH     TO CURL-TEXT-PTR                         
139900*      END-PERFORM                                                        
140000*                                                                         
140100*      MOVE '//CURLEXEC EXEC PGM=BPXBATCH,REGION=0M'                      
140200*                                TO SUBM-LINE                             
140300*      CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
140400*                                                                         
140500*      MOVE '//STDOUT DD SYSOUT=*'                                        
140600*                                TO SUBM-LINE                             
140700*      CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
140800*                                                                         
140900*      MOVE '//STDERR DD SYSOUT=*'                                        
141000*                                TO SUBM-LINE                             
141100*      CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
141200*                                                                         
141300*      MOVE SPACES               TO SUBM-LINE                             
141400*      STRING                                                             
141500*        '//STDPARM DD DSN='                   DELIMITED BY SIZE          
141600*        WORK-HLQ                              DELIMITED BY SPACE         
141700*        WORK-QUAL-CURL2                       DELIMITED BY SPACE         
141800*        W-CHANNEL-DSLUX(IX)                   DELIMITED BY SPACE         
141900*        ','                                   DELIMITED BY SIZE          
142000*                              INTO SUBM-LINE                             
142100*      CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
142200*                                                                         
142300*      MOVE '// DISP=(SHR,DELETE,DELETE)'                                 
142400*                                TO SUBM-LINE                             
142500*      CALL WZ20SUBM          USING SUBM-WZ20SUBM                         
142600*                                                                         
142700*    END-IF                                                               
142800                                                                          
142900     MOVE '//SIGNAL ENDIF'       TO SUBM-LINE                             
143000     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
143100                                                                          
143200     MOVE '//IFDATA ENDIF'       TO SUBM-LINE                             
143300     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
143400                                                                          
143500                                                                          
143600     MOVE 'CLOSE'                TO SUBM-KDFUNC                           
143700     CALL WZ20SUBM            USING SUBM-WZ20SUBM                         
143800     .                                                                    
143900                                                                          
144000                                                                          
144100                                                                          
144200                                                                          

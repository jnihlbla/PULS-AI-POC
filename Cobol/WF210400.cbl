000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF210400.                                                
000300 AUTHOR.         BERNT LUNDH.                                             
000400 DATE-WRITTEN.   2002-05-02.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*    THE PGM                                                              
000900*    - READS FILE WITH FEEDBACK DATA RECORDS                              
001000*    - SENDS FEEDBACK DATA RECORDS FOR VCCS TO:                           
001100*      PULS (W476)        BY WZ01SEND (CARPARTS.PULS.SAVEIT)              
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
002100 DATA DIVISION.                                                           
002200                                                                          
002300 FILE SECTION.                                                            
003000                                                                          
003100 WORKING-STORAGE SECTION.                                                 
003200*** - CONSTANTS                                                           
003300 77  IDPGM                       PIC X(8)    VALUE 'WF210400'.            
003400 77  WS-ADRESS-W476              PIC X(50)                                
003500                                VALUE 'CARPARTS.PULS.SAVEIT'.             
003600 77  WS-W476                     PIC X(4)    VALUE 'W476'.                
003700                                                                          
003800 77  INDATA-EOF-SW               PIC X       VALUE 'N'.                   
003900     88  END-OF-INDATA                       VALUE 'Y'.                   
004000                                                                          
004100 01  W476-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004200                                                                          
004300 01  ERRTEXT.                                                             
004400     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
004500     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
004600 01  KDRC-DISPLAY                PIC Z(5).                                
004700     EJECT                                                                
004800                                                                          
004900 01  CHKP-VAR.                                                            
005000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005400     03 GSAM-PCB-LENGTH          PIC S9(9)   VALUE +48 COMP SYNC.         
005600                                                                          
005700 77  TYPE-OF-RUN                 PIC X       VALUE 'N'.                   
005800     88  NORMAL-RUN                          VALUE 'N'.                   
005900     88  RESTART-RUN                         VALUE 'R'.                   
006000                                                                          
006100 01  GENERAL-SUBPROGRAMS.                                                 
006200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     EJECT                                                                
006800                                                                          
006900*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007000                                                                          
007100 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
007200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
007300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
007400                                                                          
007500 77  SAVE-DAFINDOC               PIC X(8)   VALUE  SPACE.                 
007600 77  SAVE-IDFINDOC               PIC S9(9)  COMP-3 VALUE +0.              
007700 77  SAVE-IDOPTION-1             PIC X(8)   VALUE  SPACE.                 
007800 77  SAVE-IDOPTION-2             PIC X(8)   VALUE  SPACE.                 
007900     EJECT                                                                
008000                                                                          
008100*    --- STATUS-KOD FRÅN IMS                                              
008200 01  STATUS-WS                   PIC XX.                                  
008300     88  SEGMENT-FOUND                       VALUE '  '.                  
008400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008600     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
008700     88  IMS-NOT-OK                          VALUE 'XD'.                  
008800                                                                          
008900 01  GOOD-STATUSCODES.                                                    
009000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500                                                                          
009600*    --- IMS FUNCTION CODES                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
009900                                                                          
010000*    --- PARAMETRAR TILL POSTSUM                                          
010100*01  -COPY W0005   -PRE  POSTSUM-                                         
010200     EJECT                                                                
010300                                                                          
010400*    --- AREOR FÖR KOMMUNIKATION                                          
010500 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
010600*01  -COPY WZ01SEND                                                       
010700     EJECT                                                                
010800                                                                          
010900 01  IN-REC.                                                              
011100     03  IN-DATA                 PIC X(3000).                             
011200                                                                          
011300 01  IN-AREA-START               PIC X(24)  VALUE 'IN-AREA-START'.        
011400 01  IN-AREA.                                                             
011500*    03  -COPY WF2017                                                     
011600     EJECT                                                                
011700                                                                          
011800 01  W476UT-AREA-START            PIC X(24) VALUE                         
011900                                            'W476UT-AREA-START'.          
012000 01  W476UT-AREA.                                                         
012100*    03  -COPY WZ01REQU         -PRE W476UT-                              
012200*    03  -COPY WF2104I1         -PRE W476UT-                              
012300     EJECT                                                                
012400                                                                          
012500 LINKAGE SECTION.                                                         
012600*01  -COPY W0009   -PRE MSG-                                              
012700                                                                          
012800*01  -COPY W0009   -PRE SAVEIT-                                           
012900     EJECT                                                                
013000*01  -COPY W0008  -PRE IN-GSAM-                                           
013100     05  KEYFB-RSA               PIC X(12).                               
013200* KEYFB-RSA WILL HAVE THE POSITION OF LAST READ RECORD FROM INPUT         
013300* GSAM FILE. THIS IS REQUIRED TO REPOSITION THE POINTER IN INPUT          
013400* FILE DURING RESTART AFTER AN ABEND RUN.                                 
013500                                                                          
013600 PROCEDURE DIVISION  USING MSG-PCB SAVEIT-PCB IN-GSAM-PCB.                
013700 MAIN SECTION.                                                            
013800                                                                          
013900     ENTRY 'DLITCBL' USING MSG-PCB SAVEIT-PCB IN-GSAM-PCB.                
014000                                                                          
014100     PERFORM A-INIT                                                       
014200     PERFORM B-EXECUTE                                                    
014300     PERFORM Z-FINIT                                                      
014400                                                                          
014500     MOVE ZERO TO RETURN-CODE                                             
014600     GOBACK                                                               
014700     .                                                                    
014900                                                                          
015000 A-INIT SECTION.                                                          
015100     PERFORM IMS-RESTART                                                  
015200                                                                          
015300     MOVE 1                TO W476UT-REQU-IDMSGVER                        
015400     MOVE SPACE            TO W476UT-REQU-KDPGMACT                        
015500     MOVE IDPGM            TO W476UT-REQU-IDUSER                          
015800     .                                                                    
015900     EJECT                                                                
016000                                                                          
016100 B-EXECUTE SECTION.                                                       
016200     IF NORMAL-RUN                                                        
016300       PERFORM IMS-GN-INDATA                                              
016400     ELSE                                                                 
016500       PERFORM IMS-GU-INDATA                                              
016600     END-IF                                                               
016700                                                                          
016800     IF END-OF-INDATA                                                     
016900       CONTINUE                                                           
017000     ELSE                                                                 
017100       PERFORM UNTIL END-OF-INDATA                                        
017200         IF FEED-IDSYSTEM-REC = WS-W476                                   
017300           PERFORM BB-HANDLE-W476                                         
017400         END-IF                                                           
017500         PERFORM IMS-GN-INDATA                                            
017600       END-PERFORM                                                        
017700     END-IF                                                               
017800     .                                                                    
018000                                                                          
018100 BB-HANDLE-W476 SECTION.                                                  
018200     IF W476-SEND-IDCOM = ZERO                                            
018300       PERFORM S71-OPEN-W476                                              
018400       MOVE SEND-IDCOM TO W476-SEND-IDCOM                                 
018500       MOVE FEED-DAFINDOC       TO SAVE-DAFINDOC                          
018600       MOVE FEED-IDFINDOC       TO SAVE-IDFINDOC                          
018700       MOVE FEED-IDOPTION (1)   TO SAVE-IDOPTION-1                        
018800       MOVE FEED-IDOPTION (2)   TO SAVE-IDOPTION-2                        
018900     END-IF                                                               
019000                                                                          
019100     PERFORM BBA-FLYTTA                                                   
019200                                                                          
019300     IF FEED-DAFINDOC     = SAVE-DAFINDOC   AND                           
019400        FEED-IDFINDOC     = SAVE-IDFINDOC   AND                           
019500        FEED-IDOPTION (1) = SAVE-IDOPTION-1 AND                           
019600        FEED-IDOPTION (2) = SAVE-IDOPTION-2                               
019700       PERFORM S81-PUT-W476                                               
019800     ELSE                                                                 
019900*    IF NEW INVOICE/PROD.NO/CASE                                          
020000*    THEN CLOSE TRANSACTION AND START NEW TRANSACTION                     
020100       IF FEED-DAFINDOC NOT = SAVE-DAFINDOC OR                            
020200          FEED-IDFINDOC NOT = SAVE-IDFINDOC                               
020300         MOVE HIGH-VALUE        TO W476UT-WF2104I1                        
020400         PERFORM S81-PUT-W476                                             
020500         PERFORM BBA-FLYTTA                                               
020600       END-IF                                                             
020700       PERFORM S91-CLOSE-W476                                             
020800**** TAKE A CHECKPOINT FOR EACH INVOICE HEADER                            
020900       PERFORM X-TAKE-CHECKPOINT                                          
021000                                                                          
021100       PERFORM S71-OPEN-W476                                              
021200       PERFORM S81-PUT-W476                                               
021300       MOVE FEED-DAFINDOC       TO SAVE-DAFINDOC                          
021400       MOVE FEED-IDFINDOC       TO SAVE-IDFINDOC                          
021500       MOVE FEED-IDOPTION (1)   TO SAVE-IDOPTION-1                        
021600       MOVE FEED-IDOPTION (2)   TO SAVE-IDOPTION-2                        
021700     END-IF                                                               
021800     .                                                                    
022000                                                                          
022100 BBA-FLYTTA   SECTION.                                                    
022200     MOVE FEED-DAEXDAT          TO W476UT-DAEXDAT                         
022300     MOVE FEED-TIEXTID          TO W476UT-TIEXTID                         
022400     MOVE FEED-DAFINDOC         TO W476UT-DAFINDOC                        
022500     MOVE FEED-IDFINDOC         TO W476UT-IDFINDOC                        
022600     MOVE FEED-KDVALISO         TO W476UT-KDVALISO                        
022700     MOVE FEED-IDPARTNR         TO W476UT-IDPARTNR                        
022800     COMPUTE W476UT-SUNTO-PART = FEED-SUNTO-PART + FEED-SUNTO-SERV        
023000     MOVE FEED-SUVAT-BILLIT-TOT TO W476UT-SUVAT-BILLIT-TOT                
023100     MOVE FEED-SUBTO-TOT        TO W476UT-SUBTO-TOT                       
023200     MOVE FEED-IDBUNDLE         TO W476UT-IDBUNDLE                        
023300     MOVE FEED-IDEXCUST(1)      TO W476UT-IDEXCUST-1                      
023400     MOVE FEED-IDEXCUST(2)      TO W476UT-IDEXCUST-2                      
023500     MOVE FEED-IDOPTION(1)      TO W476UT-IDOPTION-1                      
023600     MOVE FEED-IDOPTION(2)      TO W476UT-IDOPTION-2                      
023700     MOVE FEED-IDOPTION(3)      TO W476UT-IDOPTION-3                      
023800     MOVE FEED-IDOPTION(4)      TO W476UT-IDOPTION-4                      
023900     MOVE FEED-IDOPTION(5)      TO W476UT-IDOPTION-5                      
024000     MOVE FEED-IDARTNR-FINANCE  TO W476UT-IDARTNR-FINANCE                 
024100     MOVE FEED-BEART            TO W476UT-BEART                           
024200     MOVE FEED-VKARTNTO         TO W476UT-VKARTNTO                        
024300     MOVE FEED-SUBTO            TO W476UT-SUBTO                           
024400     MOVE FEED-SUNTO            TO W476UT-SUNTO                           
024500     MOVE FEED-SUVAT-BILLIT     TO W476UT-SUVAT-BILLIT                    
024600     MOVE FEED-KDVALISO-BET     TO W476UT-KDVALISO-BET                    
024700     MOVE FEED-PRKURS-BET       TO W476UT-PRKURS-BET                      
024800     MOVE FEED-PRKURS           TO W476UT-PRKURS                          
024900     MOVE FEED-PRKURS-FAKBET    TO W476UT-PRKURS-FAKBET                   
025000     MOVE FEED-KDVALISO-SND     TO W476UT-KDVALISO-SND                    
025100     MOVE FEED-PRKURS-SND       TO W476UT-PRKURS-SND                      
025200     MOVE FEED-IDLEVNR-ART      TO W476UT-IDLEVNR-ART                     
025301**** NEW FIELDS                                                           
025401     MOVE FEED-IDVAT-LEG       TO W476UT-IDVAT-LEG                        
025501     MOVE FEED-IDVAT-RESP      TO W476UT-IDVAT-RESP                       
025601     MOVE FEED-IDVAT-BET       TO W476UT-IDVAT-BET                        
025701     MOVE FEED-IDVAT-AGENT     TO W476UT-IDVAT-AGENT                      
025801     MOVE FEED-IDVAT-DDGS-RESP TO W476UT-IDVAT-DDGS-RESP                  
025802**** IF PRICE MODEL IS DNI                                                
025803     IF FEED-KDPRMOD = '02'                                               
025901       MOVE FEED-SUNTO         TO W476UT-SUNTO-PART-LOC                   
026004       MOVE FEED-SUVAT-BILLIT  TO W476UT-SUVAT-BILLIT-TOT-PART-L          
026101       MOVE FEED-SUBTO         TO W476UT-SUBTO-TOT-PART-LOC               
026201       MOVE FEED-SUNTO-TOT     TO W476UT-SUNTO-TOT-LOC                    
026301       MOVE FEED-SUVAT-BILLIT-TOT TO W476UT-SUVAT-BILLIT-TOT-LOC          
026401       MOVE FEED-SUBTO-TOT     TO W476UT-SUBTO-TOT-LOC                    
026501       MOVE FEED-KDVALISO      TO W476UT-KDVALISO-LOC                     
026601       MOVE FEED-PRKURS        TO W476UT-PRKURS-LOC                       
026602       MOVE FEED-PRARTNTO      TO W476UT-PRARTNTO-LOC                     
026603     ELSE                                                                 
026604       MOVE ZERO               TO W476UT-SUNTO-PART-LOC                   
026605       MOVE ZERO               TO W476UT-SUVAT-BILLIT-TOT-PART-L          
026606       MOVE ZERO               TO W476UT-SUBTO-TOT-PART-LOC               
026607       MOVE ZERO               TO W476UT-SUNTO-TOT-LOC                    
026608       MOVE ZERO               TO W476UT-SUVAT-BILLIT-TOT-LOC             
026609       MOVE ZERO               TO W476UT-SUBTO-TOT-LOC                    
026610       MOVE SPACE              TO W476UT-KDVALISO-LOC                     
026611       MOVE ZERO               TO W476UT-PRKURS-LOC                       
026612       MOVE ZERO               TO W476UT-PRARTNTO-LOC                     
026613     END-IF                                                               
026620****                                                                      
026701     MOVE ZERO                 TO W476UT-SUNTO-LOCC                       
026801     MOVE SPACE                TO W476UT-KDSIGN-LOCC                      
026802****                                                                      
026803**** RECALCULATED INVOICE                                                 
027201     MOVE FEED-SUNTO-TOT-RECALC TO W476UT-SUNTO-TOT-RECALC                
027301     MOVE FEED-SUVAT-BILLIT-TOT-REC                                       
027302                    TO W476UT-SUVAT-BILLIT-TOT-RECALC                     
027401     MOVE FEED-SUBTO-TOT-RECALC TO W476UT-SUBTO-TOT-RECALC                
027501     MOVE FEED-KDVALISO-RECALC TO W476UT-KDVALISO-RECALC                  
027502     IF FEED-SUNTO-TOT-RECALC > ZERO                                      
027503       MOVE W476UT-SUNTO-PART  TO W476UT-SUNTO-PART-RECALC                
027504       MOVE FEED-SUVAT-BILLIT  TO W476UT-SUVAT-BILLIT-TOT-PART-R          
027505       MOVE FEED-SUBTO         TO W476UT-SUBTO-TOT-PART-RECALC            
027601       MOVE FEED-PRKURS        TO W476UT-PRKURS-RECALC                    
027701       MOVE FEED-SUNTO         TO W476UT-SUNTO-RECALC                     
027702       IF FEED-SUNTO < ZERO                                               
027703         MOVE '-'              TO W476UT-KDSIGN-RECALC                    
027704       ELSE                                                               
027801         MOVE SPACE            TO W476UT-KDSIGN-RECALC                    
027802       END-IF                                                             
027803     ELSE                                                                 
027804       MOVE ZERO               TO W476UT-SUNTO-PART-RECALC                
027805       MOVE ZERO               TO W476UT-SUVAT-BILLIT-TOT-PART-R          
027806       MOVE ZERO               TO W476UT-SUBTO-TOT-PART-RECALC            
027807       MOVE ZERO               TO W476UT-PRKURS-RECALC                    
027808       MOVE ZERO               TO W476UT-SUNTO-RECALC                     
027809       MOVE SPACE              TO W476UT-KDSIGN-RECALC                    
027810     END-IF                                                               
027811****                                                                      
027820**** AVERAGE COST                                                         
027901     MOVE FEED-PRAVCOST        TO W476UT-PRAVCOST                         
028001     MOVE FEED-KDVALISO-AVC    TO W476UT-KDVALISO-AVC                     
028002****                                                                      
028003     IF FEED-KDPRMOD = '01'                                               
028101       MOVE FEED-PRARTNTO      TO W476UT-PRARTNTO                         
028201       MOVE FEED-KDVALISO      TO W476UT-KDVALISO-NTO                     
028202     ELSE                                                                 
028203       MOVE ZERO               TO W476UT-PRARTNTO                         
028204       MOVE SPACE              TO W476UT-KDVALISO-NTO                     
028205     END-IF                                                               
028206****                                                                      
028501     MOVE FEED-IDDC            TO W476UT-IDDC                             
028601     MOVE FEED-KVLEVART        TO W476UT-KVLEVART                         
028602     MOVE FEED-KDARTURS        TO W476UT-KDARTURS                         
028604     IF FEED-FLPCOO   = 'Y' OR 'J'                                        
028605       MOVE '*'                TO W476UT-FLPCOO                           
028606     ELSE                                                                 
028607       MOVE SPACE              TO W476UT-FLPCOO                           
028608     END-IF                                                               
028701     .                                                                    
028801                                                                          
028901 Z-FINIT SECTION.                                                         
029001     IF W476-SEND-IDCOM > ZERO                                            
029101       MOVE HIGH-VALUE        TO W476UT-WF2104I1                          
029201       PERFORM S81-PUT-W476                                               
029301       PERFORM S91-CLOSE-W476                                             
029401**** TAKE A CHECKPOINT FOR EACH INVOICE HEADER                            
029501       PERFORM X-TAKE-CHECKPOINT                                          
029601     END-IF                                                               
029701     .                                                                    
029801     EJECT                                                                
029901                                                                          
030001 S71-OPEN-W476 SECTION.                                                   
030101     MOVE WS-ADRESS-W476                  TO SEND-ADDISPABS               
030201     MOVE 'OPEN'                          TO SEND-KDFUNC                  
030301     CALL WZ01SEND USING SEND-CONTROL-AREA                                
030401                         SEND-OPEN-AREA                                   
030501     IF SEND-KDRC > ZERO                                                  
030601       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
030701       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
030801       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
030901       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031001     END-IF                                                               
031101     .                                                                    
031201                                                                          
031301 S81-PUT-W476 SECTION.                                                    
031401     MOVE 'PUT'                           TO SEND-KDFUNC                  
031501     MOVE W476-SEND-IDCOM                 TO SEND-IDCOM                   
031601     MOVE LENGTH OF W476UT-AREA           TO SEND-KVDLEN                  
031701     CALL WZ01SEND USING SEND-CONTROL-AREA                                
031801                         SEND-KVDLEN                                      
031901                         W476UT-AREA                                      
032001     IF SEND-KDRC > ZERO                                                  
032101       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
032201       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
032301       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
032401       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
032501     END-IF                                                               
032601     .                                                                    
032701                                                                          
032801 S91-CLOSE-W476 SECTION.                                                  
032901     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
033001     MOVE W476-SEND-IDCOM                 TO SEND-IDCOM                   
033101     CALL WZ01SEND USING SEND-CONTROL-AREA                                
033201     .                                                                    
033301                                                                          
033401 S99-POSTSUM  SECTION.                                                    
033501     MOVE 'INDATA'      TO POSTSUM-FDNAMN                                 
033601     MOVE 'WDS3G'       TO POSTSUM-DDNAMN2                                
033701     MOVE SPACE         TO POSTSUM-TRANSTYP                               
033801     CALL POSTSUM    USING POSTSUM-PARM                                   
033901     .                                                                    
034001                                                                          
034101 X-TAKE-CHECKPOINT   SECTION.                                             
034201     PERFORM IMS-CHECKPOINT                                               
034301     .                                                                    
034401                                                                          
034501 IMS-GU-INDATA SECTION.                                                   
034601* EXECUTES DURING A RESTART TO READ THE FIRST RECORD AFTER THE            
034701* LAST CHECKPOINT                                                         
034801                                                                          
034901     MOVE 'IMS-GU-INDATA'        TO SSA1                                  
035001     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
035101     CALL CBLTDLI             USING GU                                    
035201                                    IN-GSAM-PCB                           
035301                                    IN-REC                                
035401                                    KEYFB-RSA                             
035501     MOVE IN-GSAM-STATUS-CODE    TO STATUS-WS                             
035601     IF SEGMENT-FOUND                                                     
035701       MOVE IN-DATA              TO IN-AREA                               
035801     ELSE                                                                 
035901       SET END-OF-INDATA         TO TRUE                                  
036001       MOVE SPACES               TO IN-AREA                               
036101     END-IF                                                               
036201     PERFORM S99-POSTSUM                                                  
036301     PERFORM IMS-STATUSCHECK                                              
036401     .                                                                    
036501                                                                          
036601 IMS-GN-INDATA SECTION.                                                   
036701     MOVE 'IMS-GN-INDATA'        TO SSA1                                  
036801     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
036901     CALL CBLTDLI             USING GN                                    
037001                                    IN-GSAM-PCB                           
037101                                    IN-REC                                
037201     MOVE IN-GSAM-STATUS-CODE    TO STATUS-WS                             
037301     IF SEGMENT-FOUND                                                     
037401       MOVE IN-DATA              TO IN-AREA                               
037501     ELSE                                                                 
037601       SET END-OF-INDATA         TO TRUE                                  
037701       MOVE SPACES               TO IN-AREA                               
037801     END-IF                                                               
037901     PERFORM S99-POSTSUM                                                  
038001     PERFORM IMS-STATUSCHECK                                              
038101      .                                                                   
038201                                                                          
038301 IMS-RESTART SECTION.                                                     
038401     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
038501     MOVE '  ' TO GOOD-STATUSCODES                                        
038601     CALL CBLTDLI USING XRST MSG-PCB                                      
038701                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
038801                        CHKP-AREA-LENGTH CHKP-AREA GSAM-PCB-LENGTH        
038901                        IN-GSAM-PCB                                       
039001                                                                          
039101     IF CHKP-MSG-IO-AREA = SPACES                                         
039201       SET NORMAL-RUN            TO TRUE                                  
039301     ELSE                                                                 
039401       SET RESTART-RUN           TO TRUE                                  
039501     END-IF                                                               
039601                                                                          
039701     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039801     PERFORM IMS-STATUSCHECK                                              
039901     .                                                                    
040001                                                                          
040101 IMS-CHECKPOINT SECTION.                                                  
040201     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
040301     MOVE '  XD' TO GOOD-STATUSCODES                                      
040401     CALL CBLTDLI USING CHKP MSG-PCB                                      
040501                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
040601                        CHKP-AREA-LENGTH CHKP-AREA GSAM-PCB-LENGTH        
040701                        IN-GSAM-PCB                                       
040801     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040901     PERFORM IMS-STATUSCHECK                                              
041001                                                                          
041101     IF IMS-NOT-OK                                                        
041201       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO ERRTEXT-STR         
041301       DISPLAY ERRTEXT                                                    
041401       CALL FELLOG                                                        
041501     END-IF                                                               
041601     .                                                                    
041701                                                                          
041801 IMS-STATUSCHECK SECTION.                                                 
041901     SET STATUS-IX TO 1                                                   
042001     SEARCH GOOD-STATUS                                                   
042101       AT END                                                             
042201         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
042301           DELIMITED BY SIZE INTO ERRTEXT                                 
042401         DISPLAY ERRTEXT                                                  
042501         CALL FELLOG                                                      
042601       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
042701         CONTINUE                                                         
042801     END-SEARCH                                                           
043000     .                                                                    
050000                                                                          

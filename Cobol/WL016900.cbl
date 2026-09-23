000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WL016900.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   JUNI 2005.                                               
000600                                                                          
000700     REMARKS.                                                             
000800* WL016900 PROGRAM IS A REPLICA OF W5030300 PROGRAM                       
000900* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
001000*                                                                         
001100*    NAMN:       CARPARTS.LDC.SHOWINVENTORYQUEUE3                         
001200*                                                                         
001300                                                                          
001400*    FUNKTION.                                                            
001500*        FRÅGEPROGRAM FÖR INVENTERINGEN.                                  
001600*        SAMMANSTÄLLNING AV INVENTERINGSKÖN                               
001700*        'EJ INVENTERADE ARTIKLAR', PER VOLYM-                            
001800*        VÄRDESKLASS OCH OMRÅDE.                                          
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: WL0169                                              
002200*        REQU:        WL0169I1                                            
002300*    UTDATA.                                                              
002400*        RESP:        WL0169O1                                            
002500*    SUBPROGRAM.                                                          
002600*        FELLOG                                                           
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP3                                                                
003000 DATA DIVISION.                                                           
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                   PIC X(8)    VALUE 'WL016900'.                
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
004000 77  FILLER                      PIC X(8)    VALUE 'AAAAAAAA'.            
004100 77  PGM-POS                     PIC X(32)   VALUE SPACE.                 
004200 77  KDRC-DISPLAY                PIC Z(5)    VALUE ZERO.                  
004300                                                                          
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  IX                          PIC S9(3)   VALUE +1   COMP-3.           
004700 77  IX1                         PIC S9(3)   VALUE ZERO COMP-3.           
004800 77  IX2                         PIC S9(3)   VALUE ZERO COMP-3.           
004900 77  TAB-MAX                     PIC S9(3)   VALUE ZERO COMP-3.           
005000 77  TAB-IX                      PIC S9(3)   VALUE ZERO COMP-3.           
005100 77  SUM-UTSKR                   PIC S9(5)   VALUE ZERO.                  
005200 77  SUM-EJ-UTSKR                PIC S9(5)   VALUE ZERO.                  
005300 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +374 COMP-3.           
005400                                                                          
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600     03 CBLTDLI                  PIC X(8)   VALUE 'CBLTDLI '.             
005700     03 FELLOG                   PIC X(8)   VALUE 'FELLOG  '.             
005800     03 ABEND                    PIC X(8)   VALUE 'ABEND   '.             
005900     03 WZ01SEND                 PIC X(8)   VALUE 'WZ01SEND'.             
006000     03 WZ01SUB                  PIC X(8)   VALUE 'WZ01SUB '.             
006100     SKIP3                                                                
006200*                                                                         
006300*    --- PARAMETERS TO ABEND                                              
006400*                                                                         
006500 77  FILLER                      PIC X(08)   VALUE 'ABENDKOD'.            
006600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +33.              
006700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006900     SKIP2                                                                
007000 77  FILLER                      PIC X(08)   VALUE 'MESSAGES'.            
007100 01  MESSAGE-CODES.                                                       
007200     03  ERR-MISSING              PIC X(3)    VALUE '041'.                
007300     03  ERR-WRONG-KEY            PIC X(3)    VALUE '022'.                
007400     03  ERR-CORR-FIELDS          PIC X(3)    VALUE '023'.                
007500     03  ERR-NO-STOCKTAKINGS-EXISTS   PIC X(3)    VALUE '318'.            
007600     03  ERR-WRONG-AREA-FOR-ORDER PIC X(3)    VALUE '317'.                
007700     03  ERR-NOTHING             PIC X(3)    VALUE '005'.                 
007800     03  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.                 
007900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008100     EJECT                                                                
008200                                                                          
008300 01  FILLER                      PIC X(08)   VALUE 'IMS-WS'.              
008400 01  SPAR-FALF.                                                           
008500     03  SOK-ADLAGOMR-X      PIC X(3).                                    
008600     03  FILLER REDEFINES SOK-ADLAGOMR-X.                                 
008700         05  FILLER          PIC 9.                                       
008800         05  SOK-ADLAGOMR    PIC 9(2).                                    
008900     03  SOK-KDVVKL          PIC 9(1)   VALUE ZERO.                       
009000     EJECT                                                                
009100 01  W-WDH1A1KY-MIN.                                                      
009200     03  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                     
009300     03  FILLER              PIC X(21)   VALUE LOW-VALUE.                 
009400                                                                          
009500 01  W-WDH1A1KY-MAX.                                                      
009600     03  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                     
009700     03  FILLER              PIC X(21)   VALUE HIGH-VALUE.                
009800                                                                          
009900     EJECT                                                                
010000 01  FILLER                  PIC X(16)   VALUE ALL '1'.                   
010100                                                                          
010200 01  TABELL.                                                              
010300     03  TABELLRAD          OCCURS 250.                                   
010400         05  KDVVKL          PIC S9                      COMP-3.          
010500         05  ADLAGOMR        PIC S9(3)                   COMP-3.          
010600         05  KVANTAL         PIC S9(7)                   COMP-3.          
010700     EJECT                                                                
010800 01  INDEX-FALT.                                                          
010900                                                                          
011000     03  TYP                 PIC S9(9)   VALUE +1    COMP SYNC.           
011100                                                                          
011600 01  FELMEDDELANDE.                                                       
011700                                                                          
011800     03  FEL1.                                                            
011900         05  FILLER PIC X(40)                                             
012000             VALUE 'INGA INVENTERINGAR FINNS UPPLAGDA      '.             
012100         05  FILLER PIC X(40)                                             
012200             VALUE 'NO STOCKTAKINGS EXISTS                 '.             
012300     03  FILLER REDEFINES FEL1.                                           
012400         05  FEL-1  PIC X(40)  OCCURS 2.                                  
012500                                                                          
012600 01  MEDDELANDE.                                                          
012700                                                                          
012800     03  MED1.                                                            
012900         05  FILLER PIC X(40)                                             
013000             VALUE 'FLER SIDOR FINNS, TRYCK PF8            '.             
013100         05  FILLER PIC X(40)                                             
013200             VALUE 'MORE PAGES EXISTS, PRESS PF8           '.             
013300     03  FILLER REDEFINES MED1.                                           
013400         05  MED-1  PIC X(40)  OCCURS 2.                                  
013500     EJECT                                                                
013600****************************************************************          
013700*                                                                         
013800*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
013900*                                                                         
014000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
014100     SKIP3                                                                
014200*01  -COPY WZ01SUB                                                        
014300     EJECT                                                                
014400 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
014500     SKIP3                                                                
014600 01  REQU-AREA.                                                           
014700*    03  -COPY WZ01REQU                                                   
014800*    03  -COPY WL0169I1                                                   
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
015100     SKIP3                                                                
015200 01  RESP-AREA.                                                           
015300*    03  -COPY WZ01RESP                                                   
015400*    03  -COPY WL0169O1                                                   
015500     SKIP3                                                                
015600*****************************************************************         
015700*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
015800*                                                                         
015900 01      IMS-WS.                                                          
016000   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
016100     SKIP3                                                                
016200*                            *** STATUSKOD FRÅN IMS                       
016300   03    STATUS-WS       PIC XX.                                          
016400     88  SEGMENT-FINNS               VALUE '  '.                          
016500     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
016600     88  BASEN-SLUT                  VALUE 'GB'.                          
016700     SKIP3                                                                
016800   03    GODK-STATUSKODER.                                                
016900     05  GODK-STATUS OCCURS 5    INDEXED BY STATUS-IX PIC XX.             
017000     SKIP3                                                                
017100 01      SSA1            PIC X(128).                                      
017200     EJECT                                                                
017300*                            *** IMS FUNKTIONSKODER                       
017400*01      -COPY W0003                                                      
017500     SKIP2                                                                
017600*                            *** WDH1 AREA             *******            
017700 01  FILLER                  PIC X(16)   VALUE 'WDH1A1 AREA  '.           
017800     SKIP2                                                                
017900*01  WDH1A1    -COPY WDH1A1   -PRE INV-                                   
018000     SKIP2                                                                
018100 LINKAGE SECTION.                                                         
018200*01      -COPY W0009     -PRE MSG-                                        
018300                                                                          
018400*01      -COPY W0008     -PRE INV-                                        
018500      05 FILLER          PIC X.                                           
018600     SKIP2                                                                
018700 PROCEDURE DIVISION USING MSG-PCB INV-PCB.                                
018800 MAIN SECTION.                                                            
018900     ENTRY 'DLITCBL' USING MSG-PCB INV-PCB.                               
019000                                                                          
019100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
019200     IF SUB-KDRC = 0                                                      
019300                                                                          
019400       PERFORM A-INIT-SPARA-INPUT                                         
019500       PERFORM B-NOLLA-TABELLER                                           
019600       PERFORM C-LAS-BAS-LAGRA-TABELL                                     
019700                                                                          
019800       IF TAB-MAX > ZERO                                                  
019900                                                                          
020500             MOVE ZERO                 TO SOK-ADLAGOMR-X                  
020600                                          SOK-KDVVKL                      
020800                                                                          
021200           PERFORM DA-LAGG-UT-NY-SIDA                                     
021400       ELSE                                                               
021500         MOVE ERR-NO-STOCKTAKINGS-EXISTS TO RESP-IDMSG-ERROR              
021600       END-IF                                                             
021700       PERFORM S02-RETURN-RESPONSE                                        
021800     END-IF                                                               
021900                                                                          
022000     MOVE ZERO TO RETURN-CODE                                             
022100     GOBACK                                                               
022200     .                                                                    
022300     EJECT                                                                
022400 A-INIT-SPARA-INPUT SECTION.                                              
022500                                                                          
022600     MOVE 'STA A-INIT        ' TO PGM-POS                                 
022700                                                                          
022800     MOVE ALL '+'              TO RESP-WL0169O1                           
022900     MOVE 001                  TO RESP-IDMSGVER                           
023000     MOVE SPACE                TO RESP-IDMSG-ERROR                        
023100                                  RESP-IDMSG-INFO                         
023200                                  RESP-IDELMT-ERROR                       
023300                                                                          
023600                                                                          
023700     IF REQU-IDDC-KEY = ALL '+'                                           
023800       MOVE SPACE              TO RESP-IDDC-KEY                           
024000     ELSE                                                                 
024100       MOVE REQU-IDDC-KEY      TO RESP-IDDC-KEY                           
024200     END-IF                                                               
024300                                                                          
024400     MOVE REQU-IDDC-KEY        TO W-IDDC-MIN                              
024500                                  W-IDDC-MAX                              
024700                                                                          
024800     INSPECT RESP-IDDC-KEY REPLACING LEADING ZERO BY SPACE                
024801                                                                          
024900                                                                          
025000     MOVE +2             TO TYP                                           
025100     .                                                                    
025200     EJECT                                                                
025300 B-NOLLA-TABELLER SECTION.                                                
025400                                                                          
025500     MOVE +1 TO IX                                                        
025600     PERFORM UNTIL IX > 250                                               
025700       MOVE ZERO              TO KDVVKL  (IX)                             
025800                                 ADLAGOMR(IX)                             
025900                                 KVANTAL (IX)                             
026000       ADD +1 TO IX                                                       
026100     END-PERFORM                                                          
026200     .                                                                    
026300     EJECT                                                                
026400 C-LAS-BAS-LAGRA-TABELL  SECTION.                                         
026500                                                                          
026600     PERFORM IMS-LAS-INV                                                  
026700     PERFORM UNTIL NOT SEGMENT-FINNS                                      
026800       IF (INV-SEQA-KDINVKAT = 50 OR 60 OR 61 OR 99)                      
026900          OR INV-SEQA-FLINVBEH  = JA                                      
027000         CONTINUE                                                         
027100       ELSE                                                               
027200         IF INV-SEQA-FLINVSKR = JA                                        
028500            ADD +1        TO SUM-UTSKR                                    
028700         ELSE                                                             
030500            PERFORM CB-LAGG-IN-I-TABELL                                   
030700            ADD +1        TO SUM-EJ-UTSKR                                 
030900         END-IF                                                           
031000       END-IF                                                             
031100       PERFORM IMS-LAS-INV                                                
031200     END-PERFORM                                                          
031300     MOVE SUM-UTSKR        TO RESP-KVANTAL-UTSKR                          
031400     MOVE SUM-EJ-UTSKR     TO RESP-KVANTAL-EJUTSKR                        
031500     .                                                                    
031600     EJECT                                                                
036300 CB-LAGG-IN-I-TABELL  SECTION.                                            
036400                                                                          
036500     MOVE +1 TO TAB-IX                                                    
036600     PERFORM UNTIL (ADLAGOMR(TAB-IX) >= INV-SEQA-ADLAGOMR) OR             
036700                   (TAB-IX > TAB-MAX)                                     
036800       ADD +1 TO TAB-IX                                                   
036900     END-PERFORM                                                          
037000                                                                          
037100     IF TAB-MAX < TAB-IX                                                  
037200**** LÄGG TILL NY TABELLRAD I SLUTET AV TABELLEN                          
037300       ADD +1                    TO TAB-MAX                               
037400       MOVE ZERO                 TO KDVVKL  (TAB-MAX)                     
037500       MOVE INV-SEQA-ADLAGOMR    TO ADLAGOMR(TAB-MAX)                     
037600       ADD  +1                   TO KVANTAL (TAB-MAX)                     
037700                                                                          
037800     ELSE                                                                 
037900       IF ADLAGOMR(TAB-IX) = INV-SEQA-ADLAGOMR                            
038000****  ADDERA TILL BEFINTLIG TABELLRAD                                     
038100         ADD +1                TO KVANTAL(TAB-IX)                         
038200                                                                          
038300       ELSE                                                               
038400         MOVE TAB-MAX          TO IX1                                     
038500         ADD +1                TO TAB-MAX                                 
038600         MOVE TAB-MAX          TO IX2                                     
038700         PERFORM UNTIL                                                    
038800          ( IX1 < TAB-IX )                                                
038900           MOVE KDVVKL  (IX1) TO KDVVKL  (IX2)                            
039000           MOVE ADLAGOMR(IX1) TO ADLAGOMR(IX2)                            
039100           MOVE KVANTAL (IX1) TO KVANTAL (IX2)                            
039200           SUBTRACT +1        FROM IX1                                    
039300                                   IX2                                    
039400         END-PERFORM                                                      
039500         MOVE ZERO              TO KDVVKL  (IX2)                          
039600         MOVE INV-SEQA-ADLAGOMR TO ADLAGOMR(IX2)                          
039700         MOVE +1                TO KVANTAL (IX2)                          
039800       END-IF                                                             
039900     END-IF                                                               
040000     .                                                                    
040100     EJECT                                                                
043800 DA-LAGG-UT-NY-SIDA SECTION.                                              
043900                                                                          
044000     MOVE +1 TO IX1                                                       
044100     IF SOK-KDVVKL NUMERIC AND SOK-ADLAGOMR NUMERIC                       
044200       PERFORM UNTIL (ADLAGOMR(IX1) >= SOK-ADLAGOMR) OR                   
044300                     IX1 > TAB-MAX                                        
044400         ADD +1 TO IX1                                                    
044500       END-PERFORM                                                        
044600     END-IF                                                               
044800     MOVE ADLAGOMR(IX1)         TO SOK-ADLAGOMR-X                         
045000                                                                          
045100     MOVE +1 TO IX2                                                       
045200     PERFORM UNTIL IX2 = 28 OR IX1 > TAB-MAX                              
045300       MOVE ZERO              TO RESP-KDVVKL (IX2)                        
045400       MOVE ADLAGOMR(IX1)     TO RESP-ADLAGOMR(IX2)                       
045500       MOVE KVANTAL (IX1)     TO RESP-KVANTAL (IX2)                       
045600            ADD +1 TO IX1                                                 
045700                      IX2                                                 
045800     END-PERFORM                                                          
045900                                                                          
045910     COMPUTE RESP-KVRADER = IX2 - 1                                       
045920     END-COMPUTE                                                          
045930                                                                          
046000     IF IX1 <= TAB-MAX                                                    
046200       MOVE ADLAGOMR(IX1)     TO SOK-ADLAGOMR-X                           
046900     END-IF                                                               
047000     .                                                                    
047100     EJECT                                                                
047200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
047300     MOVE 'STA S01-FETCH-REQUEST          ' TO PGM-POS                    
047400                                                                          
047500     MOVE 'GETARG'               TO SUB-KDFUNC                            
047600     MOVE 'CARPARTS.LDC.SHOWINVENTORYQUEUE3'                              
047700       TO SUB-ADDISPABS                                                   
047800     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
047900                                                                          
048000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
048100                                                                          
048200     IF SUB-KDRC > 0                                                      
048300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
048400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
048500       DELIMITED BY SIZE INTO FELTEXT                                     
048600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
048700     END-IF                                                               
048800     MOVE 'END S01-FETCH-REQUEST          ' TO PGM-POS                    
048900     .                                                                    
049000     SKIP3                                                                
049100 S02-RETURN-RESPONSE SECTION.                                             
049200     MOVE 'STA S02-RETURN-RESPONSE        ' TO PGM-POS                    
049300                                                                          
049400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
049500     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
049600                                                                          
049700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
049800                                                                          
049900     IF SUB-KDRC > 0                                                      
050000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
050100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
050200       DELIMITED BY SIZE INTO FELTEXT                                     
050300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
050400     END-IF                                                               
050500     MOVE 'END S02-RETURN-RESPONSE        ' TO PGM-POS                    
050600     .                                                                    
050700     SKIP3                                                                
050800* IMS SECTIONER                                                           
050900     SKIP3                                                                
051000 IMS-LAS-INV    SECTION.                                                  
051100                                                                          
051200     STRING 'WDH1A1  (WDH1A1KY >' W-WDH1A1KY-MIN                          
051300                    '&WDH1A1KY <' W-WDH1A1KY-MAX ')'                      
051400     DELIMITED BY SIZE INTO SSA1                                          
051500     MOVE '  GE'             TO GODK-STATUSKODER                          
051600     CALL CBLTDLI USING GN INV-PCB INV-SEQA-WDH1A1 SSA1                   
051700     MOVE INV-STATUS-CODE    TO STATUS-WS                                 
051800     PERFORM IMS-STATUS-KONTROLL                                          
051900     .                                                                    
052000     EJECT                                                                
052100 IMS-STATUS-KONTROLL SECTION.                                             
052200     SET STATUS-IX TO 1                                                   
052300     SEARCH GODK-STATUS                                                   
052400       AT END                                                             
052500         CALL FELLOG                                                      
052600     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
052700       CONTINUE                                                           
052800     END-SEARCH                                                           
052900     .                                                                    

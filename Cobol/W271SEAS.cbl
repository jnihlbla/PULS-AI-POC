000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W271SEAS                                                 
000400 AUTHOR.         STEFAN ANDREASSON FRONTEC.                               
000500 DATE-WRITTEN.   JUNI 1996.                                               
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.                                                            
000900*        SUBPROGRAM FÖR ATT LÄSA ORDERSTATISTIK OCH                       
001000*        RÄKNA UT SÄSONGSINDEX SAMT TA FRAM EN                            
001100*        OSÄKERHETSFAKTOR                                                 
001200*                                                                         
001300*    INDATA.                                                              
001400*        CALL-PARAMETRAR FRÅN KALLANDE PROGRAM.                           
001500*          W271SEAS                                                       
001600*          WDL7-PCB                                                       
001700*          WDL4-PCB                                                       
001800*    UTDATA.                                                              
001900*        W271SEAS                                                         
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(8)    VALUE 'W271SEAS'.            
002900 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200                                                                          
003300 77  KVARTAL-SW                  PIC X       VALUE 'J'.                   
003400     88  KVARTAL                             VALUE 'J'.                   
003500     88  EJ-KVARTAL                          VALUE 'N'.                   
003600                                                                          
003700 77  KOLLA-KVOI-SW               PIC X       VALUE 'N'.                   
003800     88  KOLLA-KVOI-OK                       VALUE 'J'.                   
003900                                                                          
004000 01  DAGENS-DATUM                PIC 9(6).                                
004100 01  FILLER                      REDEFINES DAGENS-DATUM.                  
004200   03 DAGENS-AA                  PIC 9(2).                                
004300   03 DAGENS-MM                  PIC 9(2).                                
004400   03 DAGENS-DD                  PIC 9(2).                                
004500 01  WS-ARBETSFAELT.                                                      
004600   03 WS-TIME-START              PIC 9(10)   VALUE ZERO.                  
004700   03 WS-TIME-SLUT               PIC 9(10)   VALUE ZERO.                  
004800   03 WS-AAPP                    PIC 9(4)    VALUE ZERO.                  
004900   03 FILLER REDEFINES           WS-AAPP.                                 
005000    05 WS-AA                     PIC 9(2).                                
005100    05 WS-PP                     PIC 9(2).                                
005200   03 WS-KVOI-RED                PIC S9(7)V99 COMP-3 VALUE ZERO.          
005300   03 WS-PER                     PIC  9(2)    VALUE ZERO.                 
005400   03 WS-VV                      PIC  9(2)   VALUE ZERO.                  
005500   03 WS-TABELL    OCCURS 12.                                             
005600    05 WS-FORSTA-V               PIC  9(2)   VALUE ZERO.                  
005700    05 WS-SISTA-V                PIC  9(2)   VALUE ZERO.                  
005800    05 WS-KVVIPER                PIC 9       VALUE ZERO.                  
005900    05 WS-KVOI                   PIC S9(7)   VALUE ZERO COMP-3.           
006000                                                                          
006100   03  WS-ARS-FORBRUKN.                                                   
006200    04 FILLER                    OCCURS 6.                                
006300     05  WS-KVOI-PER             OCCURS 12.                               
006400       07  WS-KVOI-DC            PIC S9(7)   COMP-3  VALUE ZERO.          
006500       07  WS-KVOI-TOT-AR        PIC S9(7)   COMP-3  VALUE ZERO.          
006600       07  WS-RESEASON-PER       PIC S9(5)V9(3)                           
006700                                             COMP-3  VALUE ZERO.          
006800   03  WS-NOLLA-ARS-FORBRUKN.                                             
006900    04 FILLER                    OCCURS 6.                                
007000     05  FILLER                  OCCURS 12.                               
007100       07  FILLER                PIC S9(7)   COMP-3  VALUE ZERO.          
007200       07  FILLER                PIC S9(7)   COMP-3  VALUE ZERO.          
007300       07  FILLER                PIC S9(5)V9(3)                           
007400                                             COMP-3  VALUE ZERO.          
007500   03  WS-KVOI-TOTALT-2AR        PIC S9(7)   COMP-3  VALUE ZERO.          
007600   03  WS-KVOI-TOTALT            PIC S9(7)   COMP-3  VALUE ZERO.          
007700   03  WS-KVOI-MEDEL-PER         PIC S9(7)   COMP-3  VALUE ZERO.          
007800   03  WS-RESEASON-AVR           OCCURS 12                                
007900                                 PIC S9(5)   COMP-3  VALUE ZERO.          
008000*****                                                                     
008100*****  WS-RESEASON-JUST (MED EN DECIMAL) BEHÖVS                           
008200*****  VID UTRÄKNING AV OSÄKERHETSFAKTORN                                 
008300*****                                                                     
008400   03  WS-RESEASON-JUST          OCCURS 12                                
008500                                 PIC S9(5)V9 COMP-3  VALUE ZERO.          
008600   03  WS-RESEASON-SNITT         OCCURS 12                                
008700                                 PIC S9(5)V9 COMP-3  VALUE ZERO.          
008800   03  WS-RESEASON-SUM           PIC S9(5)V9 COMP-3  VALUE ZERO.          
008900   03  WS-RESEASON-TOTAL         PIC S9(5)   COMP-3  VALUE ZERO.          
009000   03  WS-RESEASON-TOT-AVR       PIC S9(5)   COMP-3  VALUE ZERO.          
009100   03  WS-MAX1                   PIC S9(5)   COMP-3  VALUE ZERO.          
009200   03  WS-MAX2                   PIC S9(5)   COMP-3  VALUE ZERO.          
009300   03  WS-MAX3                   PIC S9(5)   COMP-3  VALUE ZERO.          
009400   03  WS-MIN1                   PIC S9(5)   COMP-3  VALUE ZERO.          
009500   03  WS-MIN2                   PIC S9(5)   COMP-3  VALUE ZERO.          
009600   03  WS-MIN3                   PIC S9(5)   COMP-3  VALUE ZERO.          
009700   03  WS-A                      PIC X(1)           VALUE SPACE.          
009800   03  WS-KVOI-SNITT             OCCURS 12                                
009900                                 PIC S9(7)   COMP-3  VALUE ZERO.          
010000   03  WS-KVOI-SUM               PIC S9(7)   COMP-3  VALUE ZERO.          
010100   03  WS-KVOI-RULL-TOT          PIC S9(7)   COMP-3  VALUE ZERO.          
010200   03  WS-KVOI-GRAENS            PIC S9(7)   COMP-3  VALUE ZERO.          
010300   03  WS-SKILLNAD               PIC S9(3)V9(3)                           
010400                                             COMP-3  VALUE ZERO.          
010500   03  WS-KVADRAT                PIC S9(9)V9(4)                           
010600                                             COMP-3  VALUE ZERO.          
010700   03  WS-KVADRAT-SUM            PIC S9(9)V9(4)                           
010800                                             COMP-3  VALUE ZERO.          
010900   03  WS-OSAKERHET              PIC S9(9)V9(4)                           
011000                                             COMP-3  VALUE ZERO.          
011100   03  WS-ANTAL-HIST-AR          PIC S9(3)   COMP-3  VALUE ZERO.          
011200   03  WS-ANTAL-HIST-MAN         PIC S9(3)   COMP-3  VALUE ZERO.          
011300   03  WS-JUSTERA                PIC S9V9(2) COMP-3  VALUE ZERO.          
011400   03  WS-NOLLSTAELL-PARM.                                                
011500     05 FILLER                   OCCURS 12 TIMES                          
011600                                 PIC 9(4)            VALUE ZERO.          
011700*                                 SÄSONGSINDEX                            
011800     05 FILLER                   OCCURS 12 TIMES                          
011900                                 PIC S9(7)   COMP-3  VALUE ZERO.          
012000*                                 ORDERINGÅNG I STYCK PER TIDSENH         
012100     05 FILLER                   PIC 9               VALUE ZERO.          
012200     05 FILLER                   PIC 9(2)            VALUE ZERO.          
012300     05 FILLER                   PIC 9(3)V9(1)       VALUE ZERO.          
012400     05 FILLER                   PIC X               VALUE SPACE.         
012500 01  IX                          PIC S9(3)           VALUE ZERO.          
012600 01  IX-AR                       PIC S9(3)           VALUE ZERO.          
012700 01  IX-FRAN-AR                  PIC S9(3)           VALUE ZERO.          
012800 01  IX-PER                      PIC S9(3)           VALUE ZERO.          
012900 01  IX-START-AR                 PIC S9(3)           VALUE ZERO.          
013000 01  IX-START-PER                PIC S9(3)           VALUE ZERO.          
013100 01  IX-SLUT-AR                  PIC S9(3)           VALUE ZERO.          
013200 01  IX-SLUT-PER                 PIC S9(3)           VALUE ZERO.          
013300 01  IX-SEASON-AR                PIC S9(3)           VALUE ZERO.          
013400 01  IX-SEASON-PER               PIC S9(3)           VALUE ZERO.          
013500 01  IX-ARB1-AR                  PIC S9(3)           VALUE ZERO.          
013600 01  IX-ARB1-PER                 PIC S9(3)           VALUE ZERO.          
013700 01  IX-ARB2-AR                  PIC S9(3)           VALUE ZERO.          
013800 01  IX-ARB2-PER                 PIC S9(3)           VALUE ZERO.          
013900 01  IX-HIST-AR                  PIC S9(3)           VALUE ZERO.          
014000 01  IX-HIST-PER                 PIC S9(3)           VALUE ZERO.          
014100 01  IX-TEMP-AR                  PIC S9(3)           VALUE ZERO.          
014200 01  IX-TEMP-PER                 PIC S9(3)           VALUE ZERO.          
014300 01  IX-RULL                     PIC S9(3)           VALUE ZERO.          
014400 01  IX-ANTAL                    PIC S9(3)           VALUE ZERO.          
014500 01  IX-HAMTA-AR                 PIC S9(3)           VALUE ZERO.          
014600     SKIP3                                                                
014700*      --- VALID IDDC CODES                                               
014800*                                                                         
014900*01    -COPY WWDC99                                                       
015000       EJECT                                                              
015100 01  DYNAMISKA-SUBPROGRAM.                                                
015200   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
015300   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
015400   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
015500     EJECT                                                                
015600*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
015700*01  -COPY WDATAREA                                                       
015800     EJECT                                                                
015900******************************************************************        
016000*                                                                         
016100*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016200*                                                                         
016300 01  IMS-WS.                                                              
016400   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
016500     SKIP3                                                                
016600*                        **** STATUS-KOD FRÅN IMS                         
016700   03  STATUS-WS                 PIC XX.                                  
016800     88  SEGMENT-FINNS                       VALUE '  '.                  
016900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017000     88  INSERTEN-OK                         VALUE '  '.                  
017100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017200                                                                          
017300   03  GODK-STATUSKODER.                                                  
017400     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
017500     SKIP3                                                                
017600 01  NYCKLAR-TILL-DLI.                                                    
017700     03  W-IDARTNR-X.                                                     
017800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017900     03  W-IDDC-X.                                                        
018000         05  W-IDDC              PIC X(02)    VALUE SPACE.                
018100     SKIP3                                                                
018200 01  SSA1                        PIC X(64).                               
018300 01  SSA2                        PIC X(64).                               
018400     EJECT                                                                
018500*    ---  DLI INPUT-OUTPUT AREA                                           
018600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK701'.             
018700     SKIP3                                                                
018800 01  DLI-IO-AREA-WDK701.                                                  
018900*        05  -COPY WDK701                                                 
019000     EJECT                                                                
019100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
019200     SKIP3                                                                
019300 01  DLI-IO-AREA-WDK711.                                                  
019400*        05  -COPY WDK711                                                 
019500     EJECT                                                                
019600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL711'.             
019700     SKIP3                                                                
019800 01  DLI-IO-AREA-WDL711.                                                  
019900*        05  -COPY WDL711                                                 
020000     EJECT                                                                
020100                                                                          
020200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL411'.             
020300     SKIP3                                                                
020400 01  DLI-IO-AREA-WDL411.                                                  
020500*        05  -COPY WDL411                                                 
020600     EJECT                                                                
020700                                                                          
020800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
020900 01  DLI-IO-WDK601.                                                       
021000*    03  -COPY WDK601                                                     
021100     EJECT                                                                
021200*                            IMS FUNKTIONSKODER                           
021300*01    -COPY W0003                                                        
021400     EJECT                                                                
021500 LINKAGE SECTION.                                                         
021600     EJECT                                                                
021700*01  -COPY W271SEAS                                                       
021800     EJECT                                                                
021900*01  -COPY W0008 -PRE WDK7-                                               
022000     05  FILLER                  PIC X.                                   
022100     EJECT                                                                
022200*01  -COPY W0008 -PRE WDL7-                                               
022300     05  FILLER                  PIC X.                                   
022400     EJECT                                                                
022500*01  -COPY W0008 -PRE WDL4-                                               
022600     05  FILLER                  PIC X.                                   
022700     EJECT                                                                
022800*01  -COPY W0008 -PRE WDK6-                                               
022900     05  FILLER                  PIC X.                                   
023000     EJECT                                                                
023100 PROCEDURE DIVISION USING  SEAS-W271SEAS                                  
023200                           WDK7-PCB WDL7-PCB WDL4-PCB WDK6-PCB.           
023300 STYR SECTION.                                                            
023400                                                                          
023500     PERFORM A-INIT                                                       
023600     IF SEAS-KDSVAR = SPACE                                               
023700       PERFORM B-BEARBETA                                                 
023800     ELSE                                                                 
023900       MOVE WS-NOLLSTAELL-PARM                                            
024000                             TO SEAS-UTDATA                               
024100     END-IF                                                               
024200     ACCEPT WS-TIME-SLUT  FROM TIME                                       
024300                                                                          
024400     MOVE ZERO TO RETURN-CODE                                             
024500     GOBACK                                                               
024600     .                                                                    
024700     EJECT                                                                
024800 A-INIT SECTION.                                                          
024900                                                                          
025000     PERFORM AB-NOLLSTAELL                                                
025100                                                                          
025200     ACCEPT DAGENS-DATUM FROM DATE                                        
025300     ACCEPT WS-TIME-START FROM TIME                                       
025400                                                                          
025500     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
025600     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
025700                                                                          
025800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
025900                     DAT-O-TIDATUM DAT-KDSVAR                             
026000                                                                          
026100     IF DAT-KDSVAR-OK                                                     
026200                                                                          
026300       MOVE DAT-TIAARP(3:2)  TO WS-PER                                    
026400                                                                          
026500     ELSE                                                                 
026600         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
026700         DELIMITED BY SIZE INTO FELTEXT                                   
026800         CALL FELLOG                                                      
026900     END-IF                                                               
027000                                                                          
027100     MOVE SEAS-IDARTNR       TO W-IDARTNR                                 
027200     MOVE SEAS-IDDC          TO W-IDDC                                    
027300     MOVE SEAS-FLKVARTAL     TO KVARTAL-SW                                
027400                                                                          
027500     PERFORM IMS-GU-WDL711                                                
027600     IF SEGMENT-FINNS                                                     
027700       PERFORM IMS-GU-WDL411                                              
027800       IF SEGMENT-SAKNAS                                                  
027900******** IF PART/DC FROM WDL7 IS MISSING IN WDL4 THEN L411-AREA           
028000******** MUST BE ZEROED BCZ SOME ITEMS ARE USED IN COMPUTES BELOW         
028100         INITIALIZE OIHD-WDL411                                           
028200       END-IF                                                             
028300       MOVE SPACE            TO SEAS-KDSVAR                               
028400       PERFORM AA-HAMTA-VV-I-PER                                          
028500     ELSE                                                                 
028600       MOVE NEJ              TO SEAS-KDSVAR                               
028700     END-IF                                                               
028800     .                                                                    
028900     EJECT                                                                
029000 AA-HAMTA-VV-I-PER SECTION.                                               
029100                                                                          
029200*    --- FYLL I VECKONR FÖR PERIODERNA                                    
029300     MOVE DAGENS-DATUM(1:2)  TO WS-AAPP(1:2)                              
029400     MOVE 01                 TO WS-AAPP(3:2)                              
029500     MOVE WS-AAPP            TO DAT-I-TIDATUM                             
029600     MOVE 'AARP'             TO DAT-KDDATFORM                             
029700     MOVE +1                 TO WS-PP                                     
029800                                                                          
029900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
030000                     DAT-O-TIDATUM DAT-KDSVAR                             
030100                                                                          
030200     IF DAT-KDSVAR-OK                                                     
030300                                                                          
030400       MOVE DAT-KVVIPER      TO WS-KVVIPER(WS-PP)                         
030500       MOVE 1                TO WS-FORSTA-V(WS-PP)                        
030600                                                                          
030700     ELSE                                                                 
030800         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
030900         DELIMITED BY SIZE INTO FELTEXT                                   
031000         CALL FELLOG                                                      
031100     END-IF                                                               
031200                                                                          
031300     PERFORM UNTIL WS-PP      >  WS-PER                                   
031400       ADD +1                 TO WS-PP                                    
031500       IF WS-PP = +13                                                     
031600         MOVE 53             TO WS-SISTA-V(12)                            
031700       ELSE                                                               
031800                                                                          
031900         MOVE WS-AAPP         TO DAT-I-TIDATUM                            
032000         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
032100                             DAT-O-TIDATUM DAT-KDSVAR                     
032200         IF DAT-KDSVAR-OK                                                 
032300             COMPUTE WS-SISTA-V(WS-PP - 1) = DAT-TIVV - 1                 
032400             IF WS-PP > +1                                                
032500               MOVE DAT-TIVV  TO WS-FORSTA-V(WS-PP)                       
032600               MOVE DAT-KVVIPER TO WS-KVVIPER(WS-PP)                      
032700             END-IF                                                       
032800         ELSE                                                             
032900           MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                   
033000           CALL FELLOG                                                    
033100         END-IF                                                           
033200       END-IF                                                             
033300     END-PERFORM                                                          
033400     .                                                                    
033500     EJECT                                                                
033600 AB-NOLLSTAELL SECTION.                                                   
033700                                                                          
033800     MOVE +1                 TO IX-PER                                    
033900     PERFORM UNTIL IX-PER > +12                                           
034000       MOVE ZERO             TO WS-FORSTA-V (IX-PER)                      
034100                                WS-SISTA-V (IX-PER)                       
034200                                WS-KVVIPER (IX-PER)                       
034300                                WS-KVOI (IX-PER)                          
034400                                WS-RESEASON-AVR (IX-PER)                  
034500                                WS-RESEASON-JUST (IX-PER)                 
034600                                WS-RESEASON-SNITT (IX-PER)                
034700                                WS-KVOI-SNITT (IX-PER)                    
034800       ADD +1                TO IX-PER                                    
034900     END-PERFORM                                                          
035000                                                                          
035100                                                                          
035200     MOVE WS-NOLLA-ARS-FORBRUKN                                           
035300                             TO WS-ARS-FORBRUKN                           
035400                                                                          
035500                                                                          
035600     MOVE ZERO               TO WS-AAPP                                   
035700                                WS-KVOI-RED                               
035800                                WS-PER                                    
035900                                WS-VV                                     
036000                                WS-KVOI-TOTALT-2AR                        
036100                                WS-KVOI-TOTALT                            
036200                                WS-KVOI-MEDEL-PER                         
036300                                WS-RESEASON-SUM                           
036400                                WS-RESEASON-TOTAL                         
036500                                WS-RESEASON-TOT-AVR                       
036600                                WS-KVOI-SUM                               
036700                                WS-SKILLNAD                               
036800                                WS-KVADRAT                                
036900                                WS-KVADRAT-SUM                            
037000                                WS-OSAKERHET                              
037100                                WS-ANTAL-HIST-AR                          
037200                                WS-ANTAL-HIST-MAN                         
037300                                WS-JUSTERA                                
037400                                IX                                        
037500                                IX-AR                                     
037600                                IX-FRAN-AR                                
037700                                IX-PER                                    
037800                                IX-START-AR                               
037900                                IX-START-PER                              
038000                                IX-SLUT-AR                                
038100                                IX-SLUT-PER                               
038200                                IX-SEASON-AR                              
038300                                IX-SEASON-PER                             
038400                                IX-ARB1-AR                                
038500                                IX-ARB1-PER                               
038600                                IX-ARB2-AR                                
038700                                IX-ARB2-PER                               
038800                                IX-HIST-AR                                
038900                                IX-HIST-PER                               
039000                                IX-TEMP-AR                                
039100                                IX-TEMP-PER                               
039200                                IX-RULL                                   
039300                                IX-ANTAL                                  
039400                                IX-HAMTA-AR                               
039500     .                                                                    
039600     EJECT                                                                
039700 B-BEARBETA SECTION.                                                      
039800                                                                          
039900****                                                                      
040000****                                                                      
040100***************  TA REDA PÅ HUR MYCKET HISTORIK SOM FINNS                 
040200****                                                                      
040300****             < 1 ÅR ELLER                                             
040400****               1 ÅR ELLER                                             
040500****         MINST 2 ÅR                                                   
040600****                                                                      
040700                                                                          
040800     MOVE +5                 TO IX-AR                                     
040900                                                                          
041000     IF KVARTAL                                                           
041100     AND WS-PER = 12                                                      
041200       MOVE +6               TO WS-ANTAL-HIST-AR                          
041300       MOVE 72               TO WS-ANTAL-HIST-MAN                         
041400     ELSE                                                                 
041500       MOVE +5               TO WS-ANTAL-HIST-AR                          
041600       IF KVARTAL                                                         
041700         COMPUTE WS-ANTAL-HIST-MAN = 60 + WS-PER                          
041800       ELSE                                                               
041900         COMPUTE WS-ANTAL-HIST-MAN = 60 + (WS-PER - 1)                    
042000       END-IF                                                             
042100     END-IF                                                               
042200                                                                          
042300     MOVE +1                 TO IX-PER                                    
042400     MOVE ZERO               TO IX-HIST-AR                                
042500                                IX-HIST-PER                               
042600     PERFORM UNTIL IX-AR < +1                                             
042702     OR OIHD-KVOI (IX-AR, IX-PER) NOT = ZERO                              
042800                                                                          
042900       PERFORM UNTIL IX-PER > +12                                         
043002       OR OIHD-KVOI (IX-AR, IX-PER) NOT = ZERO                            
043100                                                                          
043200         IF KVARTAL                                                       
043300                                                                          
043400****                                                                      
043500****   VID KVARTALSKÖRNING SKA ÄVEN INNEVARANDE PERIOD TAS MED            
043600****                                                                      
043700                                                                          
043800           IF (WS-PER = 12                                                
043900           AND IX-PER = 1)                                                
044000           OR IX-PER = WS-PER + 1                                         
044100              SUBTRACT +1    FROM WS-ANTAL-HIST-AR                        
044200           END-IF                                                         
044300         ELSE                                                             
044400           IF IX-PER = WS-PER                                             
044500              SUBTRACT +1    FROM WS-ANTAL-HIST-AR                        
044600           END-IF                                                         
044700         END-IF                                                           
044800                                                                          
044900         SUBTRACT 1          FROM WS-ANTAL-HIST-MAN                       
045000         ADD +1              TO IX-PER                                    
045100       END-PERFORM                                                        
045200                                                                          
045300       IF IX-PER > +12                                                    
045400         SUBTRACT +1         FROM IX-AR                                   
045500         MOVE +1             TO IX-PER                                    
045600       END-IF                                                             
045700     END-PERFORM                                                          
045800                                                                          
045900     IF IX-AR < +1                                                        
046000       CONTINUE                                                           
046100     ELSE                                                                 
046200       MOVE IX-AR            TO IX-HIST-AR                                
046300       MOVE IX-PER           TO IX-HIST-PER                               
046400     END-IF                                                               
046500                                                                          
046600     IF WS-ANTAL-HIST-AR > +1                                             
046700****                                                                      
046800****     MINST TVÅ ÅRS HISTORIK                                           
046900****                                                                      
047000        PERFORM BA-AVANCERAD-BERAKNING                                    
047100        PERFORM BC-UPD-PARM-MINST-2AR                                     
047200     ELSE                                                                 
047300       IF WS-ANTAL-HIST-AR = +1                                           
047400****                                                                      
047500****     MER ÄN ETT ÅRS HISTORIK MEN MINDRE ÄN TVÅ                        
047600****     (MAN BERÄKNAR SOM OM DET FANNS TVÅ ÅRS HISTORIK)                 
047700****                                                                      
047800          PERFORM BB-ENKEL-BERAKNING                                      
047900          PERFORM BD-UPD-PARM-1AR                                         
048000       ELSE                                                               
048100****                                                                      
048200****     MINDRE ÄN ETT ÅRS HISTORIK                                       
048300****     FÖR LITE HISTORIK FÖR ATT KUNNA BERÄKNAS MASKINELLT              
048400****                                                                      
048500          PERFORM BE-UPD-PARM-MINDRE-1AR                                  
048600       END-IF                                                             
048700     END-IF                                                               
048800                                                                          
048900***** TVINGA FRAM EN DUMP FÖR ATT SE TESTRESULTAT START                   
049000*    DIVIDE WS-PER BY DC-KVOI-CDC-RULL (53) GIVING WS-PER                 
049100***** TVINGA FRAM EN DUMP FÖR ATT SE TESTRESULTAT SLUT                    
049200                                                                          
049300     .                                                                    
049400     EJECT                                                                
049500****                                                                      
049600****                                                                      
049700****                                                                      
049800 BA-AVANCERAD-BERAKNING SECTION.                                          
049900****                                                                      
050000****                                                                      
050100****     MINST TVÅ ÅRS HISTORIK                                           
050200****                                                                      
050300****                                                                      
050400                                                                          
050500                                                                          
050600****                                                                      
050700*******  FYLL PÅ AKTUELLT ÅR,                                             
050800*******  GÖRS SOM ÅR 1 I TABELLEN                                         
050900****                                                                      
051000                                                                          
051100     MOVE 1                  TO IX-AR                                     
051200                                IX-RULL                                   
051300     MOVE WS-PER             TO IX-PER                                    
051400                                                                          
051500     IF KVARTAL                                                           
051600                                                                          
051700****                                                                      
051800****   VID KVARTALSKÖRNING SKA ÄVEN INNEVARANDE PERIOD TAS MED            
051900****                                                                      
052000                                                                          
052100       MOVE IX-AR            TO IX-START-AR                               
052200       MOVE IX-PER           TO IX-START-PER                              
052300     ELSE                                                                 
052400       SUBTRACT +1           FROM IX-PER                                  
052500                                                                          
052600       IF IX-PER = ZERO                                                   
052700         MOVE +2             TO IX-START-AR                               
052800         MOVE +12            TO IX-START-PER                              
052900       ELSE                                                               
053000         MOVE IX-AR          TO IX-START-AR                               
053100         MOVE IX-PER         TO IX-START-PER                              
053200       END-IF                                                             
053300     END-IF                                                               
053400                                                                          
053500     PERFORM C-HAMTA-HIST-I-AR                                            
053600                                                                          
053700****                                                                      
053800*******  FYLL PÅ HISTORIK (UPP TILL 5 ÅR)                                 
053900*******  GÖRS SOM ÅR 2 - 6 I TABELLEN                                     
054000****                                                                      
054100                                                                          
054200     MOVE +1                 TO IX-FRAN-AR                                
054300     MOVE +2                 TO IX-AR                                     
054400     MOVE +12                TO IX-PER                                    
054500                                                                          
054600     PERFORM UNTIL IX-FRAN-AR > IX-HIST-AR                                
054700     OR           (IX-FRAN-AR = IX-HIST-AR                                
054800     AND           IX-PER < IX-HIST-PER)                                  
054900       PERFORM UNTIL (IX-FRAN-AR > IX-HIST-AR                             
055000       OR            (IX-FRAN-AR = IX-HIST-AR                             
055100       AND            IX-PER < IX-HIST-PER))                              
055200       OR             IX-PER < +1                                         
055302         IF OIHD-KVOI (IX-FRAN-AR, IX-PER) > ZERO                         
055400           COMPUTE WS-KVOI-RED ROUNDED =                                  
055502               (OIHD-KVOI (IX-FRAN-AR, IX-PER) * +4.33)                   
055602                     / OIHD-KVVIPER (IX-FRAN-AR, IX-PER)                  
055700           COMPUTE WS-KVOI-DC (IX-AR, IX-PER) ROUNDED =                   
055800                   WS-KVOI-RED * 1                                        
055900         END-IF                                                           
056000         SUBTRACT +1         FROM IX-PER                                  
056100       END-PERFORM                                                        
056200       ADD +1                TO IX-FRAN-AR                                
056300                                IX-AR                                     
056400       MOVE +12              TO IX-PER                                    
056500     END-PERFORM                                                          
056600                                                                          
056700     MOVE IX-HIST-AR         TO IX-SLUT-AR                                
056800     ADD +1                  TO IX-SLUT-AR                                
056900     MOVE IX-HIST-PER        TO IX-SLUT-PER                               
057000                                                                          
057100                                                                          
057200****                                                                      
057300*******  RÄKNA UT SÄSONGSINDEX FÖR RESP PERIOD                            
057400*******  INITIERA INDEX                                                   
057500****                                                                      
057600****                                                                      
057700****                                                                      
057800****     SÄSONGSINDEX SKA BERÄKNAS FÖR VARJE PERIOD                       
057900****     SOM HAR HISTORIK 5 PERIODER EFTER SAMT 6 PERIODER FÖRE           
058000****                                                                      
058100****     ALLA PERIODER SOM UPPFYLLER OVANSTÅENDE VILLKOR                  
058200****     SKA JÄMFÖRA FÖRSÄLJNINGEN MOT EN                                 
058300****     ÅRSTOTAL SOM OMFATTAR 5 PERIODER EFTER                           
058400****     SAMT 6 PERIODER FÖRE (TOTALT 12)                                 
058500****     ÅRSTOTALEN (WS-KVOI-TOT-AR) ÄR UNIK FÖR VARJE PERIOD             
058600****                                                                      
058700****                                                                      
058800****     DÄRFÖR BÖRJAR MAN MED ATT BYGGA UPP EN ÅRSTOTAL                  
058900****     (WS-KVOI-TOTALT) FÖR DOM 12 FÖRSTA PERIODERNA                    
059000****     SAMT HÄMTAR FÖRSTA RELEVANTA PERIOD NÄR IX = 7                   
059100****     (FÖRSTA PERIOD SOM SKA BERÄKNA SÄSONGSINDEX)                     
059200****                                                                      
059300****     DÄREFTER TAR MAN EN PERIOD I TAGET OCH LÄGGER                    
059400****     TILL NY PERIOD MHA IX-ARB2 SAMT DRAR IFRÅN "FÖRBRUKAD"           
059500****     PERIOD MHA IX-ARB1                                               
059600****                                                                      
059700****                                                                      
059800                                                                          
059900     MOVE +1                 TO IX                                        
060000     MOVE IX-START-AR        TO IX-ARB1-AR                                
060100                                IX-ARB2-AR                                
060200     MOVE IX-START-PER       TO IX-ARB1-PER                               
060300                                IX-ARB2-PER                               
060400     MOVE ZERO               TO WS-KVOI-TOTALT                            
060500     PERFORM UNTIL IX > +12                                               
060600       ADD WS-KVOI-DC (IX-ARB2-AR, IX-ARB2-PER)                           
060700                             TO WS-KVOI-TOTALT                            
060800       IF IX = +7                                                         
060900         MOVE IX-ARB2-AR     TO IX-SEASON-AR                              
061000         MOVE IX-ARB2-PER    TO IX-SEASON-PER                             
061100       END-IF                                                             
061200       IF IX-ARB2-PER = +1                                                
061300          ADD +1             TO IX-ARB2-AR                                
061400          MOVE +12           TO IX-ARB2-PER                               
061500       ELSE                                                               
061600          SUBTRACT +1        FROM IX-ARB2-PER                             
061700       END-IF                                                             
061800       ADD +1                TO IX                                        
061900     END-PERFORM                                                          
062000                                                                          
062100****                                                                      
062200****   UPPDATERA FÖRSTA PERIOD SOM HAR EN ÅRSTOTAL (ENLIGT                
062300****   FASTSTÄLLDA REGLER)                                                
062400****                                                                      
062500                                                                          
062600     MOVE WS-KVOI-TOTALT     TO                                           
062700                    WS-KVOI-TOT-AR (IX-SEASON-AR, IX-SEASON-PER)          
062800     COMPUTE WS-RESEASON-PER (IX-SEASON-AR, IX-SEASON-PER)                
062900         ROUNDED = (WS-KVOI-DC  (IX-SEASON-AR, IX-SEASON-PER) /           
063000             (WS-KVOI-TOT-AR (IX-SEASON-AR, IX-SEASON-PER) / +12))        
063100                 * 100                                                    
063200                  ON SIZE ERROR                                           
063300                    MOVE ZERO TO                                          
063400                     WS-RESEASON-PER (IX-SEASON-AR, IX-SEASON-PER)        
063500     END-COMPUTE                                                          
063600****                                                                      
063700****   IX-START-AR OCH IX-START-PER UPPDATERAS MED FÖRSTA PERIOD          
063800****   SOM HAR EN ÅRSTOTAL (ENLIGT FASTSTÄLLDA REGLER)                    
063900****                                                                      
064000                                                                          
064100     MOVE IX-SEASON-AR       TO IX-START-AR                               
064200     MOVE IX-SEASON-PER      TO IX-START-PER                              
064300                                                                          
064400     IF IX-SEASON-PER = +1                                                
064500       ADD +1                TO IX-SEASON-AR                              
064600       MOVE +12              TO IX-SEASON-PER                             
064700     ELSE                                                                 
064800       SUBTRACT +1           FROM IX-SEASON-PER                           
064900     END-IF                                                               
065000                                                                          
065100     PERFORM UNTIL  IX-ARB2-AR > IX-SLUT-AR                               
065200     OR            (IX-ARB2-AR = IX-SLUT-AR                               
065300     AND            IX-ARB2-PER < IX-SLUT-PER)                            
065400       MOVE IX-SEASON-AR     TO IX-TEMP-AR                                
065500       MOVE IX-SEASON-PER    TO IX-TEMP-PER                               
065600       SUBTRACT WS-KVOI-DC (IX-ARB1-AR, IX-ARB1-PER)                      
065700                             FROM WS-KVOI-TOTALT                          
065800       ADD WS-KVOI-DC (IX-ARB2-AR, IX-ARB2-PER)                           
065900                             TO WS-KVOI-TOTALT                            
066000       MOVE WS-KVOI-TOTALT   TO                                           
066100                    WS-KVOI-TOT-AR (IX-SEASON-AR, IX-SEASON-PER)          
066200       COMPUTE WS-RESEASON-PER (IX-SEASON-AR, IX-SEASON-PER)              
066300         ROUNDED = (WS-KVOI-DC  (IX-SEASON-AR, IX-SEASON-PER) /           
066400             (WS-KVOI-TOT-AR (IX-SEASON-AR, IX-SEASON-PER) / +12))        
066500                 * 100                                                    
066600                  ON SIZE ERROR                                           
066700                    MOVE ZERO TO                                          
066800                     WS-RESEASON-PER (IX-SEASON-AR, IX-SEASON-PER)        
066900       END-COMPUTE                                                        
067000       PERFORM BAA-ADDERA-TILL-INDEX                                      
067100     END-PERFORM                                                          
067200                                                                          
067300****                                                                      
067400****   IX-SLUT-AR OCH IX-SLUT-PER UPPDATERAS MED SISTA PERIOD             
067500****   SOM HAR EN ÅRSTOTAL (ENLIGT FASTSTÄLLDA REGLER)                    
067600****                                                                      
067700                                                                          
067800     MOVE IX-TEMP-AR         TO IX-SLUT-AR                                
067900     MOVE IX-TEMP-PER        TO IX-SLUT-PER                               
068000                                                                          
068100****                                                                      
068200****   RÄKNA UT SÄSONGSINDEX FÖR SAMTLIGA PERIODER                        
068300****   SOM HAR EN ÅRSTOTAL (DVS FÖRSÄLJNING 5 PERIODER EFTER              
068400****   SAMT 6 PERIODER FÖRE)                                              
068500****   RÄKNA UT MEDELVÄRDET AV SÄSONGSINDEXEN PER PERIOD                  
068600****                                                                      
068700                                                                          
068800     MOVE IX-START-AR        TO IX-ARB1-AR                                
068900     MOVE 1                  TO IX-ARB1-PER                               
069000                                                                          
069100****                                                                      
069200****   TA FÖRST ALLA PERIODER FROM PERIOD 1                               
069300****   TOM PERIOD = IX-START-PER                                          
069400****                                                                      
069500                                                                          
069600     MOVE ZERO               TO WS-RESEASON-TOTAL                         
069700     PERFORM UNTIL IX-ARB1-PER > IX-START-PER                             
069800       PERFORM BAB-BERAKNA-MEDELVARDE                                     
069900       MOVE IX-START-AR      TO IX-ARB1-AR                                
070000       ADD +1                TO IX-ARB1-PER                               
070100     END-PERFORM                                                          
070200                                                                          
070300                                                                          
070400****                                                                      
070500****   TA NU ALLA RESTERANDE PERIODER                                     
070600****   (ALLA PERIODER ÄR REDAN GENOMGÅNGA OM IX-START-PER = 1,            
070700****    IX-ARB1-PER KOMMER ATT VARA > 12 SÅ MAN KOMMER INTE               
070800****    IN I NÄSTA ITERATION)                                             
070900****   ALLA PERIODER FROM IX-START-PER + 1                                
071000****   TOM PERIOD = 12                                                    
071100****                                                                      
071200                                                                          
071300     MOVE IX-START-AR        TO IX-ARB1-AR                                
071400     ADD +1                  TO IX-ARB1-AR                                
071500                                                                          
071600     PERFORM UNTIL IX-ARB1-PER > 12                                       
071700       PERFORM BAB-BERAKNA-MEDELVARDE                                     
071800       MOVE IX-START-AR      TO IX-ARB1-AR                                
071900       ADD +1                TO IX-ARB1-AR                                
072000       ADD +1                TO IX-ARB1-PER                               
072100     END-PERFORM                                                          
072200                                                                          
072300                                                                          
072400****                                                                      
072500****   NORMERA SÄSONGSINDEXEN SÅ ATT TOTALEN BLIR 12 * 100                
072600****                                                                      
072700                                                                          
072800     PERFORM D-NORMERA-SASONGSINDEX                                       
072900                                                                          
073000                                                                          
073100                                                                          
073200****                                                                      
073300****   RÄKNA UT OSÄKERHETSFAKTORN MHA STANDARDAVVIKELSEN                  
073400****                                                                      
073500                                                                          
073600****                                                                      
073700****   1) RÄKNA UT SKILLNADEN MELLAN VARJE INDEX (WS-RESEASON-PER)        
073800****      OCH MEDELVÄRDET FÖR PERIODEN (WS-RESEASON-SNITT)                
073900****   2) TA KVADRATEN (** 2) PÅ RESULTATET AV 1) FÖR                     
074000****      RESPEKTIVE SÄSONGSINDEX (WS-RESEASON-PER)                       
074100****   3) SUMMERA RESULTATEN FRÅN 2)                                      
074200****   4) DIVIDERA SUMMAN FRÅN 3)                                         
074300****      MED ANTALET SÄSONGSINDEX (IX-ANTAL)                             
074400****   5) TA ROTEN (** 0,5) UR RESULTATET AV 4)                           
074500****                                                                      
074600                                                                          
074700     MOVE IX-START-AR        TO IX-ARB1-AR                                
074800     MOVE IX-START-PER       TO IX-ARB1-PER                               
074900     MOVE ZERO               TO IX-ANTAL                                  
075000                                WS-KVADRAT-SUM                            
075100                                                                          
075200     PERFORM UNTIL IX-ARB1-AR > IX-SLUT-AR                                
075300     OR           (IX-ARB1-AR = IX-SLUT-AR                                
075400     AND           IX-ARB1-PER < IX-SLUT-PER)                             
075500       PERFORM UNTIL (IX-ARB1-AR > IX-SLUT-AR                             
075600       OR            (IX-ARB1-AR = IX-SLUT-AR                             
075700       AND            IX-ARB1-PER < IX-SLUT-PER))                         
075800       OR             IX-ARB1-PER < +1                                    
075900         ADD +1              TO IX-ANTAL                                  
076000         COMPUTE WS-SKILLNAD ROUNDED =                                    
076100                 WS-RESEASON-PER (IX-ARB1-AR, IX-ARB1-PER) -              
076200                 WS-RESEASON-SNITT (IX-ARB1-PER)                          
076300         COMPUTE WS-KVADRAT ROUNDED = WS-SKILLNAD ** 2                    
076400         ADD WS-KVADRAT      TO WS-KVADRAT-SUM                            
076500         SUBTRACT +1         FROM IX-ARB1-PER                             
076600                                                                          
076700       END-PERFORM                                                        
076800       ADD +1                TO IX-ARB1-AR                                
076900       MOVE +12              TO IX-ARB1-PER                               
077000     END-PERFORM                                                          
077100                                                                          
077200     COMPUTE WS-KVADRAT-SUM ROUNDED = WS-KVADRAT-SUM / IX-ANTAL           
077300                  ON SIZE ERROR                                           
077400                    MOVE ZERO TO                                          
077500                     WS-KVADRAT-SUM                                       
077600     END-COMPUTE                                                          
077700                                                                          
077800****                                                                      
077900****   TA ROTEN UR SUMMAN FÖR ATT FÅ FRAM                                 
078000****   OSÄKERHETSTALET DVS STANDARDAVVIKELSEN                             
078100****                                                                      
078200     COMPUTE WS-OSAKERHET ROUNDED = WS-KVADRAT-SUM ** 0.5                 
078300                                                                          
078400                                                                          
078500***** TVINGA FRAM EN DUMP FÖR ATT SE TESTRESULTAT START                   
078600*    DIVIDE WS-PER BY DC-KVOI-CDC-RULL (53) GIVING WS-PER                 
078700***** TVINGA FRAM EN DUMP FÖR ATT SE TESTRESULTAT SLUT                    
078800                                                                          
078900                                                                          
079000     .                                                                    
079100     EJECT                                                                
079200                                                                          
079300****                                                                      
079400****                                                                      
079500****                                                                      
079600 BAA-ADDERA-TILL-INDEX SECTION.                                           
079700****                                                                      
079800****                                                                      
079900                                                                          
080000                                                                          
080100     IF IX-ARB1-PER = +1                                                  
080200       ADD +1                TO IX-ARB1-AR                                
080300       MOVE +12              TO IX-ARB1-PER                               
080400     ELSE                                                                 
080500       SUBTRACT +1           FROM IX-ARB1-PER                             
080600     END-IF                                                               
080700                                                                          
080800     IF IX-SEASON-PER = +1                                                
080900       ADD +1                TO IX-SEASON-AR                              
081000       MOVE +12              TO IX-SEASON-PER                             
081100     ELSE                                                                 
081200       SUBTRACT +1           FROM IX-SEASON-PER                           
081300     END-IF                                                               
081400                                                                          
081500     IF IX-ARB2-PER = +1                                                  
081600       ADD +1                TO IX-ARB2-AR                                
081700       MOVE +12              TO IX-ARB2-PER                               
081800     ELSE                                                                 
081900       SUBTRACT +1           FROM IX-ARB2-PER                             
082000     END-IF                                                               
082100     .                                                                    
082200     EJECT                                                                
082300 BAB-BERAKNA-MEDELVARDE SECTION.                                          
082400****                                                                      
082500****   ITERATION LODRÄTT I TABELLEN                                       
082600****                                                                      
082700       MOVE ZERO             TO WS-RESEASON-SUM                           
082800                                WS-KVOI-SUM                               
082900                                IX-ANTAL                                  
083000       PERFORM UNTIL IX-ARB1-AR > IX-SLUT-AR                              
083100       OR           (IX-ARB1-AR = IX-SLUT-AR                              
083200       AND           IX-ARB1-PER < IX-SLUT-PER)                           
083300****                                                                      
083400****   ITERATION HORISONTELLT I TABELLEN                                  
083500****                                                                      
083600                                                                          
083700         ADD +1              TO IX-ANTAL                                  
083800         ADD WS-RESEASON-PER (IX-ARB1-AR, IX-ARB1-PER)                    
083900                             TO WS-RESEASON-SUM                           
084000         ADD WS-KVOI-DC (IX-ARB1-AR, IX-ARB1-PER)                         
084100                             TO WS-KVOI-SUM                               
084200         ADD +1              TO IX-ARB1-AR                                
084300       END-PERFORM                                                        
084400                                                                          
084500****                                                                      
084600****   RÄKNA UT MEDELVÄRDET FÖR SÄSONGSINDEXEN                            
084700****                        OCH ORDERINGÅNGEN (KVOI)                      
084800****                                                                      
084900                                                                          
085000       COMPUTE WS-KVOI-SNITT (IX-ARB1-PER) ROUNDED =                      
085100               WS-KVOI-SUM / IX-ANTAL                                     
085200                  ON SIZE ERROR                                           
085300                    MOVE ZERO TO                                          
085400                     WS-KVOI-SNITT (IX-ARB1-PER)                          
085500       END-COMPUTE                                                        
085600                                                                          
085700       COMPUTE WS-RESEASON-SNITT (IX-ARB1-PER) ROUNDED =                  
085800               WS-RESEASON-SUM / IX-ANTAL                                 
085900                  ON SIZE ERROR                                           
086000                    MOVE ZERO TO                                          
086100                     WS-RESEASON-SNITT (IX-ARB1-PER)                      
086200       END-COMPUTE                                                        
086300                                                                          
086400       ADD WS-RESEASON-SNITT (IX-ARB1-PER)                                
086500                             TO WS-RESEASON-TOTAL                         
086600     .                                                                    
086700     EJECT                                                                
086800****                                                                      
086900****                                                                      
087000****                                                                      
087100 BB-ENKEL-BERAKNING SECTION.                                              
087200****                                                                      
087300****                                                                      
087400****     MER ÄN ETT ÅRS HISTORIK MEN MINDRE ÄN TVÅ                        
087500****                                                                      
087600****                                                                      
087700                                                                          
087800                                                                          
087900****                                                                      
088000****     FYLL PÅ AKTUELLT ÅR,                                             
088100****                                                                      
088200                                                                          
088300     MOVE ZERO               TO WS-KVOI-TOTALT-2AR                        
088400     MOVE +1                 TO IX-AR                                     
088500                                IX-RULL                                   
088600     MOVE WS-PER             TO IX-PER                                    
088700                                                                          
088800     IF KVARTAL                                                           
088900                                                                          
089000****                                                                      
089100****   VID KVARTALSKÖRNING SKA ÄVEN INNEVARANDE PERIOD TAS MED            
089200****                                                                      
089300                                                                          
089400       MOVE IX-AR            TO IX-START-AR                               
089500       MOVE IX-PER           TO IX-START-PER                              
089600     ELSE                                                                 
089700       SUBTRACT +1           FROM IX-PER                                  
089800                                                                          
089900       IF IX-PER = ZERO                                                   
090000         MOVE +2             TO IX-START-AR                               
090100         MOVE +12            TO IX-START-PER                              
090200       ELSE                                                               
090300         MOVE IX-AR          TO IX-START-AR                               
090400         MOVE IX-PER         TO IX-START-PER                              
090500       END-IF                                                             
090600     END-IF                                                               
090700                                                                          
090800     PERFORM C-HAMTA-HIST-I-AR                                            
090900                                                                          
091000     MOVE +1                 TO IX-PER                                    
091100     PERFORM UNTIL  IX-PER > WS-PER                                       
091200       ADD WS-KVOI-DC (1, IX-PER)                                         
091300                             TO WS-KVOI-TOTALT-2AR                        
091400       ADD +1                TO IX-PER                                    
091500     END-PERFORM                                                          
091600                                                                          
091700****                                                                      
091800*******  FYLL PÅ DEN HISTORIK SOM FINNS                                   
091900****                                                                      
092000                                                                          
092100     MOVE ZERO               TO IX-FRAN-AR                                
092200     MOVE +1                 TO IX-AR                                     
092300     MOVE +12                TO IX-PER                                    
092400                                                                          
092500     PERFORM UNTIL IX-FRAN-AR > IX-HIST-AR                                
092600     OR           (IX-FRAN-AR = IX-HIST-AR                                
092700     AND           IX-PER < IX-HIST-PER)                                  
092800                                                                          
092900       ADD +1                TO IX-FRAN-AR                                
093000                                IX-AR                                     
093100       MOVE +12              TO IX-PER                                    
093200       PERFORM UNTIL (IX-FRAN-AR > IX-HIST-AR                             
093300       OR            (IX-FRAN-AR = IX-HIST-AR                             
093400       AND            IX-PER < IX-HIST-PER))                              
093500       OR            IX-PER < +1                                          
093602         IF OIHD-KVOI (IX-FRAN-AR, IX-PER) > ZERO                         
093700           COMPUTE WS-KVOI-RED ROUNDED =                                  
093802               (OIHD-KVOI (IX-FRAN-AR, IX-PER) * +4.33)                   
093902                     / OIHD-KVVIPER (IX-FRAN-AR, IX-PER)                  
094000                  ON SIZE ERROR                                           
094100                    MOVE ZERO    TO WS-KVOI-RED                           
094200           END-COMPUTE                                                    
094300           COMPUTE WS-KVOI-DC (IX-AR, IX-PER) ROUNDED =                   
094400                   WS-KVOI-RED * 1                                        
094500         END-IF                                                           
094600         ADD WS-KVOI-DC (IX-AR, IX-PER)                                   
094700                             TO WS-KVOI-TOTALT-2AR                        
094800         SUBTRACT +1         FROM IX-PER                                  
094900       END-PERFORM                                                        
095000                                                                          
095100     END-PERFORM                                                          
095200                                                                          
095300                                                                          
095400****                                                                      
095500*******  FYLL UT HISTORIKTABELLEN TILL TVÅ ÅR MHA                         
095600*******  DEN HISTORIK MAN HAR (MINDRE ÄN TVÅ ÅR),                         
095700****                                                                      
095800                                                                          
095900     MOVE +3                 TO IX-SLUT-AR                                
096000*    MOVE DAGENS-DATUM (3:2) TO IX-SLUT-PER                               
096100     MOVE WS-PER             TO IX-SLUT-PER                               
096200                                                                          
096300     PERFORM UNTIL IX-AR > IX-SLUT-AR                                     
096400     OR           (IX-AR = IX-SLUT-AR                                     
096500     AND           IX-PER < IX-SLUT-PER)                                  
096600                                                                          
096700       SUBTRACT +1           FROM IX-AR                                   
096800                             GIVING IX-HAMTA-AR                           
096900                                                                          
097000       PERFORM UNTIL (IX-AR > IX-SLUT-AR                                  
097100       OR            (IX-AR = IX-SLUT-AR                                  
097200       AND            IX-PER < IX-SLUT-PER))                              
097300       OR             IX-PER < +1                                         
097400         MOVE WS-KVOI-DC (IX-HAMTA-AR, IX-PER)                            
097500                             TO WS-KVOI-DC (IX-AR, IX-PER)                
097600         ADD WS-KVOI-DC (IX-AR, IX-PER)                                   
097700                             TO WS-KVOI-TOTALT-2AR                        
097800         SUBTRACT +1         FROM IX-PER                                  
097900       END-PERFORM                                                        
098000                                                                          
098100       ADD +1                TO IX-AR                                     
098200       MOVE +12              TO IX-PER                                    
098300     END-PERFORM                                                          
098400                                                                          
098500                                                                          
098600****                                                                      
098700*******  RÄKNA UT MEDELVÄRDET FÖR TVÅ ÅR                                  
098800****                                                                      
098900                                                                          
099000                                                                          
099100     COMPUTE WS-KVOI-MEDEL-PER ROUNDED =                                  
099200                           WS-KVOI-TOTALT-2AR / +24                       
099300                  ON SIZE ERROR                                           
099400                      MOVE ZERO   TO WS-KVOI-MEDEL-PER                    
099500     END-COMPUTE                                                          
099600                                                                          
099700                                                                          
099800                                                                          
099900****                                                                      
100000*******  RÄKNA UT MEDELVÄRDEN FÖR SÄSONGSINDEX                            
100100****                          OCH ORDERINGÅNGEN (KVOI)                    
100200****                                                                      
100300                                                                          
100400                                                                          
100500     MOVE IX-START-AR        TO IX-AR                                     
100600     MOVE IX-START-PER       TO IX-PER                                    
100700     MOVE ZERO               TO WS-RESEASON-TOTAL                         
100800                                                                          
100900     PERFORM UNTIL IX-AR + 1 > IX-SLUT-AR                                 
101000     OR           (IX-AR + 1 = IX-SLUT-AR                                 
101100     AND           IX-PER < IX-SLUT-PER)                                  
101200                                                                          
101300       PERFORM UNTIL (IX-AR + 1 > IX-SLUT-AR                              
101400       OR            (IX-AR + 1 = IX-SLUT-AR                              
101500       AND            IX-PER < IX-SLUT-PER))                              
101600       OR             IX-PER < +1                                         
101700         COMPUTE WS-KVOI-SNITT (IX-PER) ROUNDED =                         
101800                (WS-KVOI-DC (IX-AR, IX-PER) +                             
101900                 WS-KVOI-DC (IX-AR + 1, IX-PER)) / 2                      
102000         COMPUTE WS-RESEASON-SNITT (IX-PER) ROUNDED =                     
102100                 WS-KVOI-SNITT (IX-PER) * +100                            
102200                 / WS-KVOI-MEDEL-PER                                      
102300                  ON SIZE ERROR                                           
102400                      MOVE ZERO   TO WS-RESEASON-SNITT (IX-PER)           
102500         END-COMPUTE                                                      
102600         ADD WS-RESEASON-SNITT (IX-PER)                                   
102700                             TO WS-RESEASON-TOTAL                         
102800         SUBTRACT +1         FROM IX-PER                                  
102900       END-PERFORM                                                        
103000                                                                          
103100       ADD +1                TO IX-AR                                     
103200       MOVE +12              TO IX-PER                                    
103300     END-PERFORM                                                          
103400                                                                          
103500****                                                                      
103600****   NORMERA SÄSONGSINDEXEN SÅ ATT TOTALEN BLIR 12 * 100                
103700****                                                                      
103800                                                                          
103900     PERFORM D-NORMERA-SASONGSINDEX                                       
104000     .                                                                    
104100     EJECT                                                                
104200****                                                                      
104300****                                                                      
104400****                                                                      
104500 BC-UPD-PARM-MINST-2AR SECTION.                                           
104600****                                                                      
104700****                                                                      
104800                                                                          
104900     MOVE +1                 TO IX                                        
105000     PERFORM UNTIL IX > +12                                               
105100       MOVE WS-RESEASON-AVR (IX)                                          
105200                             TO SEAS-RESEASON (IX)                        
105300       MOVE WS-KVOI-SNITT (IX)                                            
105400                             TO SEAS-KVOI (IX)                            
105500       ADD +1                TO IX                                        
105600     END-PERFORM                                                          
105700     MOVE WS-ANTAL-HIST-AR   TO SEAS-ANT-HIST-AR                          
105800     MOVE WS-ANTAL-HIST-MAN  TO SEAS-ANT-HIST-MAN                         
105900     COMPUTE SEAS-OSAKERHET ROUNDED = WS-OSAKERHET * 1                    
106000                                                                          
106100     PERFORM E-KOLLA-OM-SAESONG                                           
106200     .                                                                    
106300     EJECT                                                                
106400****                                                                      
106500****                                                                      
106600****                                                                      
106700 BD-UPD-PARM-1AR SECTION.                                                 
106800****                                                                      
106900****                                                                      
107000                                                                          
107100     MOVE +1                 TO IX                                        
107200     PERFORM UNTIL IX > +12                                               
107300       MOVE WS-RESEASON-AVR (IX)                                          
107400                             TO SEAS-RESEASON (IX)                        
107500       MOVE WS-KVOI-SNITT (IX)                                            
107600                             TO SEAS-KVOI (IX)                            
107700       ADD +1                TO IX                                        
107800     END-PERFORM                                                          
107900     MOVE WS-ANTAL-HIST-AR   TO SEAS-ANT-HIST-AR                          
108000     MOVE WS-ANTAL-HIST-MAN  TO SEAS-ANT-HIST-MAN                         
108100     MOVE ZERO               TO SEAS-OSAKERHET                            
108200                                                                          
108300     PERFORM E-KOLLA-OM-SAESONG                                           
108400     .                                                                    
108500     EJECT                                                                
108600****                                                                      
108700****                                                                      
108800****                                                                      
108900 BE-UPD-PARM-MINDRE-1AR SECTION.                                          
109000****                                                                      
109100****                                                                      
109200                                                                          
109300     MOVE +1                 TO IX                                        
109400     PERFORM UNTIL IX > +12                                               
109500       MOVE 100              TO SEAS-RESEASON (IX)                        
109600                                SEAS-KVOI (IX)                            
109700       ADD +1                TO IX                                        
109800     END-PERFORM                                                          
109900     MOVE WS-ANTAL-HIST-AR   TO SEAS-ANT-HIST-AR                          
110000     MOVE WS-ANTAL-HIST-MAN  TO SEAS-ANT-HIST-MAN                         
110100     MOVE ZERO               TO SEAS-OSAKERHET                            
110200     MOVE NEJ                TO SEAS-SEASON-ARTIKEL                       
110300     .                                                                    
110400     EJECT                                                                
110500 C-HAMTA-HIST-I-AR SECTION.                                               
110600     MOVE +1                   TO IX-PER                                  
110700                                                                          
110800     PERFORM UNTIL IX-PER      =  WS-PER                                  
110900       MOVE WS-FORSTA-V(IX-PER)                                           
111000                               TO WS-VV                                   
111100       PERFORM UNTIL WS-VV     >  WS-SISTA-V(IX-PER)                      
111200         ADD DC-KVOI-RULL(WS-VV)                                          
111300                               TO WS-KVOI(IX-PER)                         
111400         ADD +1                TO WS-VV                                   
111500       END-PERFORM                                                        
111600       IF WS-KVOI(IX-PER) > ZERO                                          
111700         COMPUTE WS-KVOI-DC (1, IX-PER) ROUNDED =                         
111800               WS-KVOI(IX-PER) / WS-KVVIPER(IX-PER) * 4.33                
111900                  ON SIZE ERROR                                           
112000                    MOVE ZERO TO  WS-KVOI-DC (1, IX-PER)                  
112100         END-COMPUTE                                                      
112200       END-IF                                                             
112300       ADD +1                  TO IX-PER                                  
112400     END-PERFORM                                                          
112500                                                                          
112600     IF KVARTAL                                                           
112700                                                                          
112800****                                                                      
112900****   VID KVARTALSKÖRNING SKA ÄVEN INNEVARANDE PERIOD TAS MED            
113000****                                                                      
113100                                                                          
113200       MOVE DC-KVOI-INNEV(1)    TO WS-KVOI(IX-PER)                        
113300       ADD  DC-KVOI-INNEV(2)    TO WS-KVOI(IX-PER)                        
113400       ADD  DC-KVOI-INNEV(3)    TO WS-KVOI(IX-PER)                        
113500       ADD  DC-KVOI-INNEV(4)    TO WS-KVOI(IX-PER)                        
113600       ADD  DC-KVOI-INNEV(5)    TO WS-KVOI(IX-PER)                        
113610                                                                          
113620       ADD  DC-KVOI-PP-INNEV(1) TO WS-KVOI(IX-PER)                        
113630       ADD  DC-KVOI-PP-INNEV(2) TO WS-KVOI(IX-PER)                        
113640       ADD  DC-KVOI-PP-INNEV(3) TO WS-KVOI(IX-PER)                        
113650       ADD  DC-KVOI-PP-INNEV(4) TO WS-KVOI(IX-PER)                        
113660       ADD  DC-KVOI-PP-INNEV(5) TO WS-KVOI(IX-PER)                        
113700                                                                          
113800       IF WS-KVOI(IX-PER) > ZERO                                          
113900         COMPUTE WS-KVOI-DC (1, IX-PER) ROUNDED =                         
114000               WS-KVOI(IX-PER) / WS-KVVIPER(IX-PER) * 4.33                
114100                  ON SIZE ERROR                                           
114200                    MOVE ZERO TO  WS-KVOI-DC (1, IX-PER)                  
114300         END-COMPUTE                                                      
114400       END-IF                                                             
114500     END-IF                                                               
114600     .                                                                    
114700     EJECT                                                                
114800 D-NORMERA-SASONGSINDEX SECTION.                                          
114900                                                                          
115000****                                                                      
115100****   NORMERA SÄSONGSINDEXEN SÅ ATT TOTALEN BLIR 12 * 100                
115200****                                                                      
115300                                                                          
115400                                                                          
115500     MOVE +1                 TO IX-PER                                    
115600     MOVE ZERO               TO WS-RESEASON-TOT-AVR                       
115700     PERFORM UNTIL IX-PER > +12                                           
115800       COMPUTE WS-RESEASON-JUST (IX-PER) ROUNDED =                        
115900               WS-RESEASON-SNITT (IX-PER) * +1200                         
116000                            / WS-RESEASON-TOTAL                           
116100                  ON SIZE ERROR                                           
116200                      MOVE ZERO   TO WS-RESEASON-JUST (IX-PER)            
116300       END-COMPUTE                                                        
116400       COMPUTE WS-RESEASON-AVR (IX-PER) ROUNDED =                         
116500               WS-RESEASON-JUST (IX-PER) * +1                             
116600       ADD WS-RESEASON-AVR (IX-PER)                                       
116700                             TO WS-RESEASON-TOT-AVR                       
116800       ADD +1                TO IX-PER                                    
116900     END-PERFORM                                                          
117000                                                                          
117100     IF WS-RESEASON-TOT-AVR = ZERO                                        
117200     OR WS-RESEASON-TOT-AVR < +800                                        
117300     OR WS-RESEASON-TOT-AVR > +1400                                       
117400                                                                          
117500****                                                                      
117600****   OM TOTALEN ÄR MINDRE +800 ELLER STÖRRE ÄN +1400                    
117700****   BEROR AVVIKELSEN PÅ PROBLEM VID AVRUNDINGEN                        
117800****   I COMPUTESATS, TA EJ HÄNSYN TILL DENNA                             
117900****   DVS SÄTT DEN SOM ICKE SÄSONGSARTIKEL                               
118000****                                                                      
118100                                                                          
118200       MOVE +1               TO IX-PER                                    
118300       MOVE +1200            TO WS-RESEASON-TOT-AVR                       
118400       PERFORM UNTIL IX-PER > +12                                         
118500         MOVE +100           TO WS-RESEASON-AVR (IX-PER)                  
118600         ADD +1              TO IX-PER                                    
118700       END-PERFORM                                                        
118800     END-IF                                                               
118900                                                                          
119000     IF WS-RESEASON-TOT-AVR > +1200                                       
119100       MOVE -1               TO WS-JUSTERA                                
119200     ELSE                                                                 
119300       MOVE +1               TO WS-JUSTERA                                
119400     END-IF                                                               
119500                                                                          
119600     PERFORM UNTIL WS-RESEASON-TOT-AVR = +1200                            
119700     OR            WS-RESEASON-TOT-AVR = ZERO                             
119800                                                                          
119900       MOVE +1               TO IX-PER                                    
120000       PERFORM UNTIL WS-RESEASON-TOT-AVR = +1200                          
120100       OR IX-PER > +12                                                    
120200         IF WS-RESEASON-AVR (IX-PER) > +1                                 
120300           ADD WS-JUSTERA    TO WS-RESEASON-AVR (IX-PER)                  
120400                                WS-RESEASON-TOT-AVR                       
120500         END-IF                                                           
120600         ADD +1              TO IX-PER                                    
120700       END-PERFORM                                                        
120800     END-PERFORM                                                          
120900     .                                                                    
121000     EJECT                                                                
121100 E-KOLLA-OM-SAESONG SECTION.                                              
121200                                                                          
121300****                                                                      
121400****   MIN OCH MAX ÄR ETT KOMPLEMENT TILL OSÄKERHETSFAKTORN               
121500****   FÖR ATT HITTA SÄSONGSARTIKLAR                                      
121600****   3 PERIODER I RAD SKA VARA ÖVER 400 ELLER UNDER 200                 
121700****                                                                      
121800                                                                          
121900                                                                          
122000     PERFORM IMS-GU-K601                                                  
122100                                                                          
122200     IF SEGMENT-FINNS                                                     
122300                                                                          
122400       PERFORM IMS-GU-WDK711                                              
122500                                                                          
122600       IF SEGMENT-FINNS                                                   
122700                                                                          
122800         MOVE ZERO           TO WS-KVOI-RULL-TOT                          
122900         MOVE 1              TO IX                                        
123000         PERFORM UNTIL IX > 52                                            
123100           ADD DC-KVOI-RULL (IX)                                          
123200                               TO WS-KVOI-RULL-TOT                        
123300           ADD 1             TO IX                                        
123400         END-PERFORM                                                      
123500                                                                          
123600         MOVE SEAS-IDDC    TO WS-IDDC                                     
123700*                                                                         
123800*   SÄSONGSBERÄKNINGEN KÖRS EJ FÖR LDC                                    
123900*   DETTA STYRS FRÅN W2713200                                             
124000*   OM MAN SKA BÖRJA KÖRA FÖR LDC'ER OCH MAN VILL HA                      
124100*   UNDANTAGSREGLER SÅ FÅR MAN LÄGGA UPP NYA PARAMETRAR                   
124200*   PÅ WDB601                                                             
124300*                                                                         
124400*   / OKTOBER 2004 STEFAN Å                                               
124500*                                                                         
124600*                                                                         
124700*        IF LDC                                                           
124800*           IF WS-ANTAL-HIST-MAN >= 30                                    
124900****     DET MÅSTE FINNAS 2,5 ÅRS HISTORIK DVS 30 MÅNADER                 
125000*           AND WS-KVOI-RULL-TOT > 50                                     
125100****     ÅRSFÖRSÄLJNING ETT ÅR TILLBAKA I TIDEN MÅSTE VARA ÖVER 50        
125200*           AND ((SEAS-OSAKERHET > ZERO                                   
125300*           AND SEAS-OSAKERHET < 15)                                      
125400*           OR (SEAS-OSAKERHET < 80                                       
125500*           AND ((WS-RESEASON-AVR (1) +                                   
125600*                WS-RESEASON-AVR (2) +                                    
125700*                WS-RESEASON-AVR (3)) > 400)                              
125800*           OR ((WS-RESEASON-AVR (1) +                                    
125900*                WS-RESEASON-AVR (2) +                                    
126000*                WS-RESEASON-AVR (3)) < 200)                              
126100*           OR ((WS-RESEASON-AVR (2) +                                    
126200*                WS-RESEASON-AVR (3) +                                    
126300*                WS-RESEASON-AVR (4)) > 400)                              
126400*           OR ((WS-RESEASON-AVR (2) +                                    
126500*                WS-RESEASON-AVR (3) +                                    
126600*                WS-RESEASON-AVR (4)) < 200)                              
126700*           OR ((WS-RESEASON-AVR (3) +                                    
126800*                WS-RESEASON-AVR (4) +                                    
126900*                WS-RESEASON-AVR (5)) > 400)                              
127000*           OR ((WS-RESEASON-AVR (3) +                                    
127100*                WS-RESEASON-AVR (4) +                                    
127200*                WS-RESEASON-AVR (5)) < 200)                              
127300*           OR ((WS-RESEASON-AVR (4) +                                    
127400*                WS-RESEASON-AVR (5) +                                    
127500*                WS-RESEASON-AVR (6)) > 400)                              
127600*           OR ((WS-RESEASON-AVR (4) +                                    
127700*                WS-RESEASON-AVR (5) +                                    
127800*                WS-RESEASON-AVR (6)) < 200)                              
127900*           OR ((WS-RESEASON-AVR (5) +                                    
128000*                WS-RESEASON-AVR (6) +                                    
128100*                WS-RESEASON-AVR (7)) > 400)                              
128200*           OR ((WS-RESEASON-AVR (5) +                                    
128300*                WS-RESEASON-AVR (6) +                                    
128400*                WS-RESEASON-AVR (7)) < 200)                              
128500*           OR ((WS-RESEASON-AVR (6) +                                    
128600*                WS-RESEASON-AVR (7) +                                    
128700*                WS-RESEASON-AVR (8)) > 400)                              
128800*           OR ((WS-RESEASON-AVR (6) +                                    
128900*                WS-RESEASON-AVR (7) +                                    
129000*                WS-RESEASON-AVR (8)) < 200)                              
129100*           OR ((WS-RESEASON-AVR (7) +                                    
129200*                WS-RESEASON-AVR (8) +                                    
129300*                WS-RESEASON-AVR (9)) > 400)                              
129400*           OR ((WS-RESEASON-AVR (7) +                                    
129500*                WS-RESEASON-AVR (8) +                                    
129600*                WS-RESEASON-AVR (9)) < 200)                              
129700*           OR ((WS-RESEASON-AVR (8) +                                    
129800*                WS-RESEASON-AVR (9) +                                    
129900*                WS-RESEASON-AVR (10)) > 400)                             
130000*           OR ((WS-RESEASON-AVR (8) +                                    
130100*                WS-RESEASON-AVR (9) +                                    
130200*                WS-RESEASON-AVR (10)) < 200)                             
130300*           OR ((WS-RESEASON-AVR (9) +                                    
130400*                WS-RESEASON-AVR (10) +                                   
130500*                WS-RESEASON-AVR (11)) > 400)                             
130600*           OR ((WS-RESEASON-AVR (9) +                                    
130700*                WS-RESEASON-AVR (10) +                                   
130800*                WS-RESEASON-AVR (11)) < 200)                             
130900*           OR ((WS-RESEASON-AVR (10) +                                   
131000*                WS-RESEASON-AVR (11) +                                   
131100*                WS-RESEASON-AVR (12)) > 400)                             
131200*           OR ((WS-RESEASON-AVR (10) +                                   
131300*                WS-RESEASON-AVR (11) +                                   
131400*                WS-RESEASON-AVR (12)) < 200)                             
131500*           OR ((WS-RESEASON-AVR (11) +                                   
131600*                WS-RESEASON-AVR (12) +                                   
131700*                WS-RESEASON-AVR (1)) > 400)                              
131800*           OR ((WS-RESEASON-AVR (11) +                                   
131900*                WS-RESEASON-AVR (12) +                                   
132000*                WS-RESEASON-AVR (1)) < 200)                              
132100*           OR ((WS-RESEASON-AVR (12) +                                   
132200*                WS-RESEASON-AVR (1) +                                    
132300*                WS-RESEASON-AVR (2)) > 400)                              
132400*           OR ((WS-RESEASON-AVR (12) +                                   
132500*                WS-RESEASON-AVR (1) +                                    
132600*                WS-RESEASON-AVR (2)) < 200)))                            
132700*             MOVE JA        TO SEAS-SEASON-ARTIKEL                       
132800*           ELSE                                                          
132900*             MOVE NEJ       TO SEAS-SEASON-ARTIKEL                       
133000*           END-IF                                                        
133100*        ELSE                                                             
133200            MOVE NEJ         TO KOLLA-KVOI-SW                             
133300            IF  NDC                                                       
133400            AND ART-IDFKNGRP >= 8700                                      
133500            AND ART-IDFKNGRP <= 8749                                      
133600                                                                          
133700              IF NDC-CA                                                   
133800****            KANADA                                                    
133900                IF WS-KVOI-RULL-TOT > 30                                  
134000                AND WS-ANTAL-HIST-MAN >= 24                               
134100****       DET MÅSTE FINNAS 2 ÅRS HISTORIK DVS 24 MÅNADER                 
134200                  MOVE JA    TO KOLLA-KVOI-SW                             
134300                ELSE                                                      
134400                  MOVE NEJ   TO KOLLA-KVOI-SW                             
134500                END-IF                                                    
134600              ELSE                                                        
134700                IF WS-KVOI-RULL-TOT > 50                                  
134800                AND WS-ANTAL-HIST-MAN >= 24                               
134900****       DET MÅSTE FINNAS 2 ÅRS HISTORIK DVS 24 MÅNADER                 
135000                  MOVE JA    TO KOLLA-KVOI-SW                             
135100                ELSE                                                      
135200                  MOVE NEJ   TO KOLLA-KVOI-SW                             
135300                END-IF                                                    
135400              END-IF                                                      
135500            ELSE                                                          
135600              IF SDC-NL OR NDC-US                                         
135700              OR NDC-JP OR NDC-CN                                         
135800                 MOVE 100    TO WS-KVOI-GRAENS                            
135900              ELSE                                                        
136000                 MOVE 50     TO WS-KVOI-GRAENS                            
136100              END-IF                                                      
136200****     DET MÅSTE FINNAS 2,5 ÅRS HISTORIK DVS 30 MÅNADER                 
136300****      ÅRSFÖRSÄLJNING ETT ÅR TILLBAKA I TIDEN                          
136400****      MÅSTE VARA ÖVER 100 ALT. 50                                     
136500              IF (WS-KVOI-RULL-TOT > WS-KVOI-GRAENS)                      
136600              AND WS-ANTAL-HIST-MAN >= 30                                 
136700                MOVE JA      TO KOLLA-KVOI-SW                             
136800              ELSE                                                        
136900                MOVE NEJ     TO KOLLA-KVOI-SW                             
137000              END-IF                                                      
137100            END-IF                                                        
137200                                                                          
137300            IF KOLLA-KVOI-OK                                              
137400            AND ((SEAS-OSAKERHET > ZERO                                   
137500            AND SEAS-OSAKERHET < 15)                                      
137600            OR ((WS-RESEASON-AVR (1) +                                    
137700                 WS-RESEASON-AVR (2) +                                    
137800                 WS-RESEASON-AVR (3)) > 400)                              
137900            OR ((WS-RESEASON-AVR (1) +                                    
138000                 WS-RESEASON-AVR (2) +                                    
138100                 WS-RESEASON-AVR (3)) < 200)                              
138200            OR ((WS-RESEASON-AVR (2) +                                    
138300                 WS-RESEASON-AVR (3) +                                    
138400                 WS-RESEASON-AVR (4)) > 400)                              
138500            OR ((WS-RESEASON-AVR (2) +                                    
138600                 WS-RESEASON-AVR (3) +                                    
138700                 WS-RESEASON-AVR (4)) < 200)                              
138800            OR ((WS-RESEASON-AVR (3) +                                    
138900                 WS-RESEASON-AVR (4) +                                    
139000                 WS-RESEASON-AVR (5)) > 400)                              
139100            OR ((WS-RESEASON-AVR (3) +                                    
139200                 WS-RESEASON-AVR (4) +                                    
139300                 WS-RESEASON-AVR (5)) < 200)                              
139400            OR ((WS-RESEASON-AVR (4) +                                    
139500                 WS-RESEASON-AVR (5) +                                    
139600                 WS-RESEASON-AVR (6)) > 400)                              
139700            OR ((WS-RESEASON-AVR (4) +                                    
139800                 WS-RESEASON-AVR (5) +                                    
139900                 WS-RESEASON-AVR (6)) < 200)                              
140000            OR ((WS-RESEASON-AVR (5) +                                    
140100                 WS-RESEASON-AVR (6) +                                    
140200                 WS-RESEASON-AVR (7)) > 400)                              
140300            OR ((WS-RESEASON-AVR (5) +                                    
140400                 WS-RESEASON-AVR (6) +                                    
140500                 WS-RESEASON-AVR (7)) < 200)                              
140600            OR ((WS-RESEASON-AVR (6) +                                    
140700                 WS-RESEASON-AVR (7) +                                    
140800                 WS-RESEASON-AVR (8)) > 400)                              
140900            OR ((WS-RESEASON-AVR (6) +                                    
141000                 WS-RESEASON-AVR (7) +                                    
141100                 WS-RESEASON-AVR (8)) < 200)                              
141200            OR ((WS-RESEASON-AVR (7) +                                    
141300                 WS-RESEASON-AVR (8) +                                    
141400                 WS-RESEASON-AVR (9)) > 400)                              
141500            OR ((WS-RESEASON-AVR (7) +                                    
141600                 WS-RESEASON-AVR (8) +                                    
141700                 WS-RESEASON-AVR (9)) < 200)                              
141800            OR ((WS-RESEASON-AVR (8) +                                    
141900                 WS-RESEASON-AVR (9) +                                    
142000                 WS-RESEASON-AVR (10)) > 400)                             
142100            OR ((WS-RESEASON-AVR (8) +                                    
142200                 WS-RESEASON-AVR (9) +                                    
142300                 WS-RESEASON-AVR (10)) < 200)                             
142400            OR ((WS-RESEASON-AVR (9) +                                    
142500                 WS-RESEASON-AVR (10) +                                   
142600                 WS-RESEASON-AVR (11)) > 400)                             
142700            OR ((WS-RESEASON-AVR (9) +                                    
142800                 WS-RESEASON-AVR (10) +                                   
142900                 WS-RESEASON-AVR (11)) < 200)                             
143000            OR ((WS-RESEASON-AVR (10) +                                   
143100                 WS-RESEASON-AVR (11) +                                   
143200                 WS-RESEASON-AVR (12)) > 400)                             
143300            OR ((WS-RESEASON-AVR (10) +                                   
143400                 WS-RESEASON-AVR (11) +                                   
143500                 WS-RESEASON-AVR (12)) < 200)                             
143600            OR ((WS-RESEASON-AVR (11) +                                   
143700                 WS-RESEASON-AVR (12) +                                   
143800                 WS-RESEASON-AVR (1)) > 400)                              
143900            OR ((WS-RESEASON-AVR (11) +                                   
144000                 WS-RESEASON-AVR (12) +                                   
144100                 WS-RESEASON-AVR (1)) < 200)                              
144200            OR ((WS-RESEASON-AVR (12) +                                   
144300                 WS-RESEASON-AVR (1) +                                    
144400                 WS-RESEASON-AVR (2)) > 400)                              
144500            OR ((WS-RESEASON-AVR (12) +                                   
144600                 WS-RESEASON-AVR (1) +                                    
144700                 WS-RESEASON-AVR (2)) < 200))                             
144800              MOVE JA        TO SEAS-SEASON-ARTIKEL                       
144900            ELSE                                                          
145000              MOVE NEJ       TO SEAS-SEASON-ARTIKEL                       
145100            END-IF                                                        
145200*        END-IF                                                           
145300       ELSE                                                               
145400         MOVE NEJ            TO SEAS-SEASON-ARTIKEL                       
145500       END-IF                                                             
145600     ELSE                                                                 
145700       MOVE NEJ              TO SEAS-SEASON-ARTIKEL                       
145800     END-IF                                                               
145900                                                                          
146000     .                                                                    
146100     EJECT                                                                
146200* IMS SEKTIONER                                                           
146300                                                                          
146400 IMS-GU-WDL711 SECTION.                                                   
146500                                                                          
146600     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
146700          DELIMITED BY SIZE INTO SSA1                                     
146800     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
146900          DELIMITED BY SIZE INTO SSA2                                     
147000     MOVE '  GE' TO GODK-STATUSKODER                                      
147100     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-AREA-WDL711 SSA1 SSA2          
147200     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
147300     PERFORM IMS-STATUSKONTROLL                                           
147400     .                                                                    
147500     EJECT                                                                
147600 IMS-GU-WDL411 SECTION.                                                   
147700                                                                          
147800     STRING 'WDL401  (IDARTNR  =' W-IDARTNR-X ')'                         
147900          DELIMITED BY SIZE INTO SSA1                                     
148000     STRING 'WDL411  (IDDC     =' W-IDDC-X ')'                            
148100          DELIMITED BY SIZE INTO SSA2                                     
148200     MOVE '  GE' TO GODK-STATUSKODER                                      
148300     CALL CBLTDLI USING GU WDL4-PCB DLI-IO-AREA-WDL411 SSA1 SSA2          
148400     MOVE WDL4-STATUS-CODE TO STATUS-WS                                   
148500     PERFORM IMS-STATUSKONTROLL                                           
148600     .                                                                    
148700     EJECT                                                                
148800 IMS-GU-WDK711 SECTION.                                                   
148900                                                                          
149000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
149100          DELIMITED BY SIZE INTO SSA1                                     
149200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
149300          DELIMITED BY SIZE INTO SSA2                                     
149400     MOVE '  GE' TO GODK-STATUSKODER                                      
149500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
149600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
149700     PERFORM IMS-STATUSKONTROLL                                           
149800     .                                                                    
149900     EJECT                                                                
150000 IMS-GU-K601 SECTION.                                                     
150100                                                                          
150200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
150300          DELIMITED BY SIZE INTO SSA1                                     
150400     MOVE '  GE' TO GODK-STATUSKODER                                      
150500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
150600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
150700     PERFORM IMS-STATUSKONTROLL                                           
150800     .                                                                    
150900     EJECT                                                                
151000 IMS-STATUSKONTROLL SECTION.                                              
151100     SET STATUS-IX TO 1                                                   
151200     SEARCH GODK-STATUS                                                   
151300       AT END                                                             
151400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
151500         DELIMITED BY SIZE INTO FELTEXT                                   
151600         CALL FELLOG                                                      
151700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
151800         CONTINUE                                                         
151900     END-SEARCH                                                           
152000     .                                                                    

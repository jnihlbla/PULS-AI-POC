000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.      W6117800.                                               
000400 AUTHOR.          EWA ELIASSON.                                           
000500 DATE-WRITTEN.    DECEMBER 1984.                                          
000600                  ÄNDRAT I SAMBAND MED SPRAK I JUNI 86.                   
000700                  A WALLIN.                                               
000800                  OMGJORT TILL SB MARS 1987.                              
000900                  LASSE C.                                                
001000                                                                          
001100     REMARKS.                                                             
001200*                                                                         
001300*    FUNKTION:                                                            
001400*                                                                         
001500*       PROGRAM SKRIVET FÖR PÄR ÅSEMAR                                    
001600*                                                                         
001700*       PROGRAMET LÄSER IGENOM WDL2 OCH                                   
001800*       SKRIVER EN POST FÖR VAR R31,310,R32                               
001900*       SOM ÄR REGISTRERADE I INNEVARANDE PERIOD.                         
002000*                                                                         
002100********  SKAPAR SEDAN OKTOBER -97 OCKSÅ EN R32-FIL SOM GÅR TILL          
002200******  VIR FÖR INLEVERANSSTATISTIK.                                      
002300*                                   /KENT JEBSEN                          
002400******************************************************************        
002500     EJECT                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP3                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*- - - - - - - - - - - - - - UTFIL:                                       
003300     SELECT W61178-INLEV                 ASSIGN TO UT-S-W61178D1.         
003400     EJECT                                                                
003500     SELECT W611VIR                      ASSIGN TO W61178D2.              
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004000 FD  W61178-INLEV                                                         
004100     LABEL RECORD   STANDARD                                              
004200     RECORDING      F                                                     
004300     BLOCK CONTAINS 0.                                                    
004400     SKIP2                                                                
004500*01  POST  -COPY W6117801 -PRE UT-  -L.                                   
004600     EJECT                                                                
004700 FD  W611VIR                                                              
004800     LABEL RECORD   STANDARD                                              
004900     RECORDING      F                                                     
005000     BLOCK CONTAINS 0.                                                    
005100     SKIP2                                                                
005200*01  POST  -COPY PI30INL1 -PRE VIR1-  -L.                                 
005300*01  POST  -COPY PI30INL2 -PRE VIR2-  -L.                                 
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700*    -- CHECKED BY WY2000                                                 
005800 77   FELTEXT                    PIC X(30)   VALUE SPACE.                 
005900 77   PROGRAM-NAMN               PIC X(8)    VALUE 'W6117800'.            
006000     SKIP2                                                                
006100*- - - - - - - - - - - - - - GENERELLA KONSTANTER                         
006200                                                                          
006300 01  JA                          PIC X       VALUE 'J'.                   
006400 01  NEJ                         PIC X       VALUE 'N'.                   
006500 01  TIAA-IDAG                   PIC 9(2)    VALUE ZERO.                  
006600 01  TIVECKA-IDAG                PIC 9(2)    VALUE ZERO.                  
006700 01  SPAR-IDARTNR                PIC S9(9)   COMP-3 VALUE +0.             
006800 01  WS-FL-FORSTA-VIR            PIC X       VALUE 'J'.                   
006900                                                                          
007000 01  VIR-POST                    PIC X(20) VALUE SPACE.                   
007100 01  STARTPOST-VIR                           REDEFINES VIR-POST.          
007200   03 SEKEL-VIR                  PIC 9(2).                                
007300   03 AA-VIR                     PIC 9(2).                                
007400   03 VV-VIR                     PIC 9(2).                                
007500   03 PLANT                      PIC X.                                   
007600   03 FILLER                     PIC X(13).                               
007700     SKIP2                                                                
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007900   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
008000   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
008100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
008200   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
008300     EJECT                                                                
008400*      --- VALID IDDC CODES                                               
008500*                                                                         
008600*01    -COPY WWDC99                                                       
008700       EJECT                                                              
008800                                                                          
008900 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
009000                                                                          
009100 01    W-IDLOPNRM-X.                                                      
009200   03  W-IDLOPNRM                PIC S9(9)   COMP-3.                      
009300                                                                          
009400 01  IMS-WS.                                                              
009500                                                                          
009600                                                                          
009700   03  STATUS-WS                 PIC X(2).                                
009800      88  SEGMENT-FINNS                      VALUE '  '.                  
009900      88  SEGMENT-SAKNAS                     VALUE 'GE'.                  
010000      88  SEGMENT-SLUT                       VALUE 'GB'.                  
010100      88  SEGMENT-OK                         VALUE 'GA'.                  
010200                                                                          
010300   03 GODK-STATUSKODER.                                                   
010400      05 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).              
010500                                                                          
010600   03 SSA1                       PIC X(32)   VALUE SPACE.                 
010700                                                                          
010800     EJECT                                                                
010900*01        -COPY W0003                                                    
011000     EJECT                                                                
011100*- - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                      
011200                                                                          
011300*    -COPY W0005       -PRE POSTSUM-                                      
011400     EJECT                                                                
011500 01  FILLER                      PIC X(20) VALUE 'DATUM'.                 
011600*        -COPY WDATAREA.                                                  
011700     EJECT                                                                
011800 01  FILLER                      PIC X(24)  VALUE                         
011900                                            'UTRESTO-AREA-START'.         
012000     SKIP2                                                                
012100*01  AREA  -COPY W6117801 -PRE UT-                                        
012200     EJECT                                                                
012300 01  FILLER                      PIC X(24)  VALUE                         
012400                                            'VIR-AREA'.                   
012500     SKIP2                                                                
012600*01  AREA  -COPY PI30INL1 -PRE VIR1-                                      
012700*01  AREA  -COPY PI30INL2 -PRE VIR2-                                      
012800     EJECT                                                                
012900 01  IO-AREA.                                                             
013000   03  IO-AREA1                  PIC X(150).                              
013100                                                                          
013200*  03  ART-AREA   -COPY WDL201     -RED IO-AREA1                          
013300     EJECT                                                                
013400*  03  MOT-AREA   -COPY WDL221     -RED IO-AREA1                          
013500     EJECT                                                                
013600                                                                          
013700 LINKAGE SECTION.                                                         
013800                                                                          
013900*01  -COPY W0008       -PRE INLE-                                         
014000       05 FILLER                 PIC X(1).                                
014100     EJECT                                                                
014200 PROCEDURE DIVISION USING INLE-PCB.                                       
014300     ENTRY 'DLITCBL' USING INLE-PCB.                                      
014400                                                                          
014500     PERFORM A-INIT                                                       
014600     PERFORM IMS-GET-WDL2                                                 
014700                                                                          
014800     PERFORM UNTIL SEGMENT-SLUT                                           
014900                                                                          
015000        EVALUATE INLE-SEG-NAME-FB                                         
015100          WHEN  'WDL201'                                                  
015200                MOVE ART-IDARTNR      TO SPAR-IDARTNR                     
015300          WHEN  'WDL221'                                                  
015400                PERFORM B-BEARBETA                                        
015500        END-EVALUATE                                                      
015600                                                                          
015700        PERFORM IMS-GET-WDL2                                              
015800     END-PERFORM                                                          
015900                                                                          
016000     PERFORM Z-FINIT                                                      
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600     SKIP2                                                                
016700     OPEN OUTPUT W61178-INLEV                                             
016800                 W611VIR                                                  
016900                                                                          
017000     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
017100     MOVE 'J'          TO WS-FL-FORSTA-VIR                                
017200                                                                          
017300* FIX FÖR OMKÖRNING                                                       
017400* ANGE AAMMDD OCH ANNAN DATUM I STÄLLET FÖR 'IDAG' VID                    
017500* ANROP TILL WDATKONV                                                     
017600*    MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
017700*    MOVE 990911               TO DAT-I-TIDATUM                           
017800     MOVE 'IDAG  '             TO DAT-KDDATFORM                           
017900     CALL WDATKONV USING       DAT-KDDATFORM                              
018000                               DAT-I-TIDATUM                              
018100                               DAT-O-TIDATUM                              
018200                               DAT-KDSVAR                                 
018300     IF DAT-KDSVAR-OK                                                     
018400         MOVE DAT-TIAA-VECKA TO TIAA-IDAG                                 
018500         MOVE DAT-TIVV       TO TIVECKA-IDAG                              
018600     ELSE                                                                 
018700         CALL FELLOG                                                      
018800         MOVE 'FEL FRÅN DATKONV' TO FELTEXT                               
018900         DISPLAY FELTEXT                                                  
019000     END-IF                                                               
019100                                                                          
019110     MOVE DAT-TISEKEL                  TO SEKEL-VIR                       
019120     MOVE DAT-TIAA-VECKA               TO AA-VIR                          
019130     MOVE DAT-TIVV                     TO VV-VIR                          
019140     MOVE 'R'                          TO PLANT                           
019150     MOVE STARTPOST-VIR                TO VIR1-PI30INL1                   
019160                                                                          
019170     WRITE VIR1-POST FROM VIR1-AREA                                       
019180     MOVE SPACE TO POSTSUM-TRANSTYP                                       
019190     MOVE 'VIR-R32 ' TO POSTSUM-FDNAMN                                    
019191     MOVE 'W61178D2' TO POSTSUM-DDNAMN2                                   
019192     CALL POSTSUM USING POSTSUM-PARM                                      
019200     .                                                                    
019300     EJECT                                                                
019400 B-BEARBETA SECTION.                                                      
019500                                                                          
019600     MOVE SPACE TO UT-AREA                                                
019700                                                                          
019800     IF MOT-IDPTYP = 'R31' OR '310'                                       
019900         MOVE 'AAMMDD'          TO DAT-KDDATFORM                          
020000         MOVE MOT-TIAVIDAT      TO DAT-I-TIDATUM                          
020100         CALL WDATKONV USING    DAT-KDDATFORM                             
020200                                DAT-I-TIDATUM                             
020300                                DAT-O-TIDATUM                             
020400                                DAT-KDSVAR                                
020500                                                                          
020600         IF DAT-KDSVAR-OK                                                 
020700             IF TIAA-IDAG = DAT-TIAA-VECKA AND                            
020800                TIVECKA-IDAG = DAT-TIVV                                   
020900                 PERFORM BA-FLYTTA-R31-310                                
021000                 PERFORM S01-SKRIV-W61178-FILEN                           
021100             END-IF                                                       
021200         END-IF                                                           
021300     END-IF                                                               
021400     IF MOT-IDPTYP = 'R32'                                                
021500         MOVE 'AAMMDD'          TO DAT-KDDATFORM                          
021600         MOVE MOT-TIUPPDAT      TO DAT-I-TIDATUM                          
021700         CALL WDATKONV USING    DAT-KDDATFORM                             
021800                                DAT-I-TIDATUM                             
021900                                DAT-O-TIDATUM                             
022000                                DAT-KDSVAR                                
022100                                                                          
022200         IF DAT-KDSVAR-OK                                                 
022300             IF TIAA-IDAG = DAT-TIAA-VECKA AND                            
022400                TIVECKA-IDAG = DAT-TIVV                                   
022500                 PERFORM BB-FLYTTA-R32                                    
022600                 PERFORM S01-SKRIV-W61178-FILEN                           
022700                 PERFORM BC-BEARBETA-VIR                                  
022800             END-IF                                                       
022900         END-IF                                                           
023000     END-IF                                                               
023100     .                                                                    
023200     EJECT                                                                
023300 Z-FINIT SECTION.                                                         
023400     SKIP2                                                                
023500     CLOSE W61178-INLEV                                                   
023600           W611VIR                                                        
023700                                                                          
023800     MOVE 'S' TO POSTSUM-OPKOD                                            
023900                                                                          
024000     CALL POSTSUM USING POSTSUM-PARM                                      
024100     .                                                                    
024200     EJECT                                                                
024300 BA-FLYTTA-R31-310 SECTION.                                               
024400                                                                          
024500     MOVE MOT-IDLOPNRM                   TO UT-IDLOPNRM                   
024600     MOVE DAT-TIAAVVD                    TO UT-TIAAVVD                    
024700     MOVE DAT-TIRP                       TO UT-TIPP                       
024800                                                                          
024900     IF MOT-IDPTYP = 'R31'                                                
025000        MOVE 'R31'                       TO UT-IDPTYP                     
025100     ELSE                                                                 
025200        MOVE '310'                       TO UT-IDPTYP                     
025300     END-IF                                                               
025400                                                                          
025500     MOVE MOT-IDDC                       TO UT-IDDC                       
025600     MOVE SPAR-IDARTNR                   TO UT-IDARTNR                    
025700     MOVE +0                             TO UT-IDPLFORM                   
025800     MOVE MOT-IDLEVNR                    TO UT-IDLEVNR-INL                
025900                                                                          
026000     MOVE MOT-KDRT                       TO UT-KDRT                       
026100     MOVE MOT-TIAVIDAT                   TO UT-TIAVSDAT                   
026200     MOVE MOT-IDKONTO                    TO UT-IDKONTO                    
026300     MOVE MOT-IDAVINR                    TO UT-IDAVINR                    
026400                                                                          
026500     MOVE MOT-KVAVIS                     TO UT-KVAVIS                     
026600     MOVE MOT-KDAVVANT                   TO UT-KDAVVANT                   
026700     MOVE MOT-KVANTMOT                   TO UT-KVANTMOT                   
026800     MOVE MOT-KVFORDEL                   TO UT-KVFORDEL                   
026900                                                                          
027000     MOVE +0                             TO UT-IDKOLLI                    
027100     MOVE MOT-KDAVVKV                    TO UT-KDAVVKV                    
027200     MOVE MOT-KVRETUR                    TO UT-KVRETUR                    
027300     MOVE MOT-ADLAGOMR                   TO UT-ADLAGOMR                   
027400     .                                                                    
027500     EJECT                                                                
027600 BB-FLYTTA-R32 SECTION.                                                   
027700                                                                          
027800     MOVE MOT-IDLOPNRM                   TO UT-IDLOPNRM                   
027900     MOVE DAT-TIAAVVD                    TO UT-TIAAVVD                    
028000     MOVE DAT-TIRP                       TO UT-TIPP                       
028100     MOVE 'R32'                          TO UT-IDPTYP                     
028200                                                                          
028300     MOVE MOT-IDDC                       TO UT-IDDC                       
028400     MOVE SPAR-IDARTNR                   TO UT-IDARTNR                    
028500     MOVE +0                             TO UT-IDPLFORM                   
028600     MOVE MOT-IDLEVNR                    TO UT-IDLEVNR-INL                
028700     MOVE MOT-KDRT                       TO UT-KDRT                       
028800                                                                          
028900     MOVE MOT-TIAVIDAT                   TO UT-TIAVSDAT                   
029000     MOVE MOT-IDKONTO                    TO UT-IDKONTO                    
029100     MOVE MOT-IDAVINR                    TO UT-IDAVINR                    
029200     MOVE MOT-KVAVIS                     TO UT-KVAVIS                     
029300     MOVE MOT-KDAVVANT                   TO UT-KDAVVANT                   
029400                                                                          
029500     MOVE MOT-KVANTMOT                   TO UT-KVANTMOT                   
029600     MOVE MOT-KVFORDEL                   TO UT-KVFORDEL                   
029700     MOVE +0                             TO UT-IDKOLLI                    
029800     MOVE MOT-KDAVVKV                    TO UT-KDAVVKV                    
029900     MOVE MOT-KVRETUR                    TO UT-KVRETUR                    
030000     MOVE MOT-ADLAGOMR                   TO UT-ADLAGOMR                   
030100     .                                                                    
030200     EJECT                                                                
030300 BC-BEARBETA-VIR SECTION.                                                 
030400                                                                          
030500     IF MOT-IDLEVNR NOT = '1002 ' OR '1441 ' OR 'BP2TW'                   
030600       IF MOT-KDRT = 0 OR 9 OR 10                                         
032300         MOVE SPAR-IDARTNR                   TO VIR2-PARTNO               
032400         MOVE MOT-KVANTMOT                   TO VIR2-DELQUANT             
032500         MOVE MOT-IDLEVNR                    TO VIR2-SUPPNO               
032600         PERFORM S02-SKRIV-W611VIR-FILEN                                  
032700       END-IF                                                             
032800     END-IF                                                               
032900     .                                                                    
033000     EJECT                                                                
033100 S01-SKRIV-W61178-FILEN SECTION.                                          
033200     SKIP2                                                                
033300     WRITE UT-POST FROM UT-AREA                                           
033400                                                                          
033500     MOVE SPACE TO POSTSUM-TRANSTYP                                       
033600                                                                          
033700     MOVE 'UT-INLEV' TO POSTSUM-FDNAMN                                    
033800                                                                          
033900     MOVE 'W61178D1' TO POSTSUM-DDNAMN2                                   
034000                                                                          
034100     CALL POSTSUM USING POSTSUM-PARM                                      
034200     .                                                                    
034300     EJECT                                                                
034400 S02-SKRIV-W611VIR-FILEN SECTION.                                         
034500     SKIP2                                                                
034600     WRITE VIR2-POST FROM VIR2-AREA                                       
034700                                                                          
034800     MOVE SPACE TO POSTSUM-TRANSTYP                                       
034900                                                                          
035000     MOVE 'VIR-R32 ' TO POSTSUM-FDNAMN                                    
035100                                                                          
035200     MOVE 'W61178D2' TO POSTSUM-DDNAMN2                                   
035300                                                                          
035400     CALL POSTSUM USING POSTSUM-PARM                                      
035500     .                                                                    
035600     EJECT                                                                
035700 IMS-GET-WDL2 SECTION.                                                    
035800                                                                          
035900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
036000     CALL CBLTDLI USING GN INLE-PCB IO-AREA                               
036100     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
036200     PERFORM IMS-STATUSKONTROLL                                           
036300                                                                          
036400     .                                                                    
036500 IMS-STATUSKONTROLL SECTION.                                              
036600                                                                          
036700     SET STATUS-IX TO 1                                                   
036800     SEARCH GODK-STATUS AT END CALL FELLOG                                
036900     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
037000     END-SEARCH                                                           
037100     .                                                                    

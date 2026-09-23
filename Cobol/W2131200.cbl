000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2131200.                                    
000300 AUTHOR.                     IDK, GÖTEBORG.                               
000400 DATE-WRITTEN.               NOV 1978.                                    
000500     REMARKS.                                                             
000600*    PROGRAMMET UPPDATERAR ROTSEGMENTET (ANSKAFFNINGSINFORMATION)         
000700*    PÅ LEVERANTÖRSREGISTRET. EN DEL AV DATAELEMENTEN I                   
000800*    LEVERANTÖRSREGISTRET FINNS ÄVEN PÅ ARTIKELNIVÅ. DESSA                
000900*    UPPDATERAS PÅ LEVERANTÖRENS SAMTLIGA ARTIKLAR OCH                    
001000*    HÄNDELSEPOST '2213' SKAPAS FÖR OMRÄKNING AV BESTÄLLNINGS-,           
001100*    FRYS- OCH INLEVERANSTID.                                             
001200*                                                                         
001300*    SUBPROGRAM.                                                          
001400*                                                                         
001500*    W2131210    HANDHAR SAMTLIGA IMS-ANROP.                              
001600*                UPPDATERINGARNA GÖRS I FÖJANDE PROGRAM.                  
001700*    POSTSUM     BERÄKNAR OCH REDOVISAR ANTAL LÄSTA OCH                   
001800*                SKRIVNA POSTER.                                          
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002200 FILE-CONTROL.                                                            
002300     SKIP3                                                                
002400*--------------------------------------- INFILER                          
002500     SKIP1                                                                
002600*--------------------------------------- TRANSAKTIONER                    
002700     SKIP1                                                                
002800     SELECT W21302 ASSIGN UT-S-W21312D1.                                  
002900     SKIP3                                                                
003000*--------------------------------------- UTFILER                          
003100     SKIP1                                                                
003200*--------------------------------------- FELFIL                           
003300     SKIP1                                                                
003400     SELECT W21313 ASSIGN UT-S-W21312D2.                                  
003500*--------------------------------------- UTFIL ART VID ANSKBYTE           
003600     SKIP1                                                                
003700     SELECT W21315 ASSIGN UT-S-W21312D5.                                  
003800     SKIP3                                                                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W21302                                                               
004400     RECORDING V                                                          
004500     BLOCK 0                                                              
004600     LABEL RECORDS STANDARD.                                              
004700     SKIP1                                                                
004800*01  -COPY W213R02  -L                                                    
004900*    -COPY W213R17T  -L                                                   
005000     SKIP3                                                                
005100 FD  W21313                                                               
005200     RECORDING V                                                          
005300     BLOCK 0                                                              
005400     LABEL RECORDS STANDARD.                                              
005500     SKIP1                                                                
005600 01  FEL-POST           PIC X(76).                                        
005700     SKIP1                                                                
005800 FD  W21315                                                               
005900     RECORDING F                                                          
006000     BLOCK 0                                                              
006100     LABEL RECORDS STANDARD.                                              
006200     SKIP1                                                                
006300 01  ANSKBYTE-POST.                                                       
006400*03   -COPY W2212204    -L                                                
006500     EJECT                                                                
006600 WORKING-STORAGE SECTION.                                                 
006700     SKIP1                                                                
006800                                                                          
006900*    -- CHECKED BY WY2000                                                 
007000 01  IXPG                    PIC S9(4)               COMP SYNC.           
007100 01  W-IX                    PIC S9(4)               COMP SYNC.           
007200     SKIP1                                                                
007300 01  KONSTANTER.                                                          
007400     03  JA                  PIC X       VALUE 'J'.                       
007500     03  NEJ                 PIC X       VALUE 'N'.                       
007600     SKIP1                                                                
007700 01  SW-EOF-W21302           PIC X       VALUE 'N'.                       
007800 01  SW-ARTIKEL-UPPDATERAD   PIC X.                                       
007900 01  R17-RATT                PIC X.                                       
008000     SKIP1                                                                
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008200     03  W2131210            PIC X(8)    VALUE 'W2131210'.                
008300     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
008400     SKIP1                                                                
008500 01  SPAR-FAELT.                                                          
008600     03  SPAR-KVDAGAR-TTC1   PIC S9(3)   COMP-3.                          
008700     03  SPAR-KVDAGAR-TTC2   PIC S9(3)   COMP-3.                          
008800     03  WS-SPAR-IDANSK      PIC 9(3).                                    
008900     EJECT                                                                
009000*01  -COPY W0005      -PRE POSTSUM-                                       
009100     EJECT                                                                
009200 01  TRANS-AREA                  PIC X(50).                               
009300     SKIP1                                                                
009400*01  AREA -COPY W213R17T   -PRE R17- -RED TRANS-AREA                      
009500     EJECT                                                                
009600*01  AREA -COPY W213R02    -PRE R02- -RED TRANS-AREA                      
009700     EJECT                                                                
009800*01  AREA -COPY W092W001   -PRE FEL-                                      
009900     EJECT                                                                
010000*    04  R02TRATT -COPY W213R02T -PRE FEL02-                              
010100     EJECT                                                                
010200*    04  R17TRATT -COPY W213R17T -PRE FEL17-  -RED FEL02-R02TRATT         
010300     EJECT                                                                
010400*01  AREA -COPY W213L121   -PRE IMSLNK-                                   
010500     EJECT                                                                
010600*01  AREA  -COPY W2212204  -PRE ANSK-                                     
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900*    -COPY W0008 -PRE LEVA-                                               
011000         05  FILLER          PIC X.                                       
011100*    -COPY W0008 -PRE ARTC-                                               
011200         05  FILLER          PIC X.                                       
011300     EJECT                                                                
011400*    -COPY W0008 -PRE WDK6G-                                              
011410         05  FILLER          PIC X.                                       
011420     EJECT                                                                
011500 PROCEDURE DIVISION  USING LEVA-PCB ARTC-PCB WDK6G-PCB.                   
011600     ENTRY 'DLITCBL' USING LEVA-PCB ARTC-PCB WDK6G-PCB.                   
011700     SKIP3                                                                
011800     PERFORM A-INITIERA                                                   
011900     PERFORM S01-LAS-W21302                                               
012000     SKIP1                                                                
012100     PERFORM UNTIL (SW-EOF-W21302 = JA) OR                                
012200                   (R17-IDPTYP NOT = 'R17')                               
012300         IF R17-KDBEHX = 'N'                                              
012400             PERFORM B-NYUPPLAGG                                          
012500         ELSE                                                             
012600             PERFORM C-ANDRING-BORTTAG                                    
012700         END-IF                                                           
012800         PERFORM S01-LAS-W21302                                           
012900     END-PERFORM                                                          
013000     SKIP1                                                                
013100     PERFORM UNTIL (SW-EOF-W21302 = JA) OR                                
013200                   (R02-IDPTYP NOT = 'R02')                               
013300         PERFORM D-UPPDATERA-ANSKAFFARE                                   
013400         PERFORM S01-LAS-W21302                                           
013500     END-PERFORM                                                          
013600     SKIP1                                                                
013700     PERFORM Z-AVSLUTA                                                    
013800     MOVE ZERO TO RETURN-CODE                                             
013900     GOBACK                                                               
014000     EJECT                                                                
014100     .                                                                    
014200 A-INITIERA SECTION.                                                      
014300     OPEN INPUT W21302                                                    
014400          OUTPUT W21313                                                   
014500                 W21315                                                   
014600                                                                          
014700     MOVE +001 TO IMSLNK-KDCALL                                           
014800     CALL W2131210 USING IMSLNK-W213L121                                  
014900                         LEVA-PCB                                         
015000                         ARTC-PCB                                         
015010                         WDK6G-PCB                                        
015100                                                                          
015200     MOVE 'W21312' TO POSTSUM-PROGNAMN                                    
015300     EJECT                                                                
015400     .                                                                    
015500 B-NYUPPLAGG SECTION.                                                     
015600     SKIP1                                                                
015700     MOVE JA TO R17-RATT                                                  
015800     PERFORM BA-FELKONTROLL                                               
015900     IF R17-RATT = JA                                                     
016000     SKIP1                                                                
016100         MOVE R17-IDLEVNR TO IMSLNK-IDLEVNR                               
016200         PERFORM S03-LAS-WLLEVA01                                         
016300     SKIP1                                                                
016400         IF IMSLNK-FLJANEJ-ANROP = JA                                     
016500             MOVE ZERO        TO FEL-W092W001                             
016600             MOVE R17-IDPTYP  TO FEL-IDPTYP                               
016700             MOVE R17-IDLEVNR TO FEL-SORTBGP                              
016800             MOVE '001'       TO FEL-IDFELKODX                            
016900             PERFORM S04-FLYTTA-R17-TILL-FELPOST                          
017000             PERFORM S02-SKRIV-W21313                                     
017100         ELSE                                                             
017200             MOVE R17-IDLEVNR      TO IMSLNK-IDLEVNR                      
017300             MOVE R17-KDLEVTYP     TO IMSLNK-KDLEVTYP                     
017400             MOVE R17-FLEMBPOL     TO IMSLNK-FLEMBPOL                     
017500             MOVE R17-KDSPRAK      TO IMSLNK-KDSPRAK                      
017600             MOVE R17-FLRSADR      TO IMSLNK-FLRSADR                      
017700             MOVE R17-KDGK         TO IMSLNK-KDGK                         
017800             MOVE R17-KVDAGAR-TTC1 TO IMSLNK-KVDAGAR-TTC1                 
017900             MOVE R17-KVDAGAR-TTC2 TO IMSLNK-KVDAGAR-TTC2                 
018000             MOVE R17-KVVECKOR-LT  TO IMSLNK-KVVECKOR-LT                  
018100             MOVE R17-KVVECKOR-AT  TO IMSLNK-KVVECKOR-AT                  
018200             MOVE R17-IDLPKOLL     TO IMSLNK-IDLPKOLL                     
018300*                                                                         
018400             MOVE ZERO TO IXPG                                            
018500             PERFORM UNTIL IXPG NOT < 8                                   
018600               ADD 1 TO IXPG                                              
018700               MOVE ZERO TO IMSLNK-IDANSK-PG (IXPG)                       
018800             END-PERFORM                                                  
018900*                                                                         
019000             MOVE +1 TO IXPG                                              
019100             PERFORM UNTIL IXPG > 5                                       
019200               MOVE ZERO TO IMSLNK-TILEVDAG (IXPG)                        
019300               ADD 1     TO IXPG                                          
019400             END-PERFORM                                                  
019500*                                                                         
019600             MOVE +102 TO IMSLNK-KDCALL                                   
019700             CALL W2131210 USING IMSLNK-W213L121                          
019800                                 LEVA-PCB                                 
019900                                 ARTC-PCB                                 
019910                                 WDK6G-PCB                                
020000                                                                          
020100             MOVE 'W21314'   TO POSTSUM-FDNAMN                            
020200             MOVE 'WDF101  ' TO POSTSUM-DDNAMN2                           
020300             MOVE 'ISRT'     TO POSTSUM-TRANSTYP                          
020400             CALL POSTSUM USING POSTSUM-PARM                              
020500         END-IF                                                           
020600     END-IF                                                               
020700     EJECT                                                                
020800     .                                                                    
020900 BA-FELKONTROLL SECTION.                                                  
021000     IF R17-KVVECKOR-LT NOT NUMERIC OR                                    
021100        R17-KVVECKOR-AT NOT NUMERIC OR                                    
021200        R17-KVDAGAR-TTC1 NOT NUMERIC OR                                   
021300        R17-KVDAGAR-TTC2 NOT NUMERIC OR                                   
021400        R17-KDLEVTYP NOT NUMERIC OR                                       
021500        R17-KDGK NOT NUMERIC OR                                           
021600        R17-IDLPKOLL NOT NUMERIC OR                                       
021700        R17-KDSPRAK NOT NUMERIC OR                                        
021800        R17-FLRSADR = SPACE OR                                            
021900        R17-FLEMBPOL = SPACE                                              
022000         MOVE NEJ TO R17-RATT                                             
022100         MOVE ZERO TO FEL-W092W001                                        
022200         MOVE R17-IDPTYP TO FEL-IDPTYP                                    
022300         MOVE R17-IDLEVNR TO FEL-SORTBGP                                  
022400         MOVE '004' TO FEL-IDFELKODX                                      
022500         PERFORM S04-FLYTTA-R17-TILL-FELPOST                              
022600         PERFORM S02-SKRIV-W21313                                         
022700     END-IF                                                               
022800     EJECT                                                                
022900     .                                                                    
023000 C-ANDRING-BORTTAG SECTION.                                               
023100     SKIP1                                                                
023200     MOVE R17-IDLEVNR TO IMSLNK-IDLEVNR                                   
023300     PERFORM S03-LAS-WLLEVA01                                             
023400     SKIP1                                                                
023500     IF IMSLNK-FLJANEJ-ANROP = JA                                         
023600         IF R17-KDBEHX = 'Ä'                                              
023700             PERFORM CA-UPPDATERA                                         
023800         ELSE                                                             
023900             IF R17-KDBEHX = 'B'                                          
024000                 MOVE +105 TO IMSLNK-KDCALL                               
024100                 CALL W2131210 USING IMSLNK-W213L121                      
024200                                     LEVA-PCB                             
024300                                     ARTC-PCB                             
024310                                     WDK6G-PCB                            
024400                 IF IMSLNK-FLJANEJ-ANROP = JA                             
024500                     MOVE ZERO TO FEL-W092W001                            
024600                     MOVE R17-IDPTYP TO FEL-IDPTYP                        
024700                     MOVE R17-IDLEVNR TO FEL-SORTBGP                      
024800                     MOVE '003' TO FEL-IDFELKODX                          
024900                     PERFORM S04-FLYTTA-R17-TILL-FELPOST                  
025000                     PERFORM S02-SKRIV-W21313                             
025100                 ELSE                                                     
025200                     MOVE +103 TO IMSLNK-KDCALL                           
025300                     CALL W2131210 USING IMSLNK-W213L121                  
025400                                         LEVA-PCB                         
025500                                         ARTC-PCB                         
025510                                         WDK6G-PCB                        
025600                                                                          
025700                     MOVE 'W21314'   TO POSTSUM-FDNAMN                    
025800                     MOVE 'WDF101  ' TO POSTSUM-DDNAMN2                   
025900                     MOVE 'DLET'     TO POSTSUM-TRANSTYP                  
026000                     CALL POSTSUM USING POSTSUM-PARM                      
026100                 END-IF                                                   
026200             END-IF                                                       
026300         END-IF                                                           
026400     ELSE                                                                 
026500         MOVE ZERO TO FEL-W092W001                                        
026600         MOVE R17-IDPTYP TO FEL-IDPTYP                                    
026700         MOVE R17-IDLEVNR TO FEL-SORTBGP                                  
026800         MOVE '002' TO FEL-IDFELKODX                                      
026900         PERFORM S04-FLYTTA-R17-TILL-FELPOST                              
027000         PERFORM S02-SKRIV-W21313                                         
027100     END-IF                                                               
027200     EJECT                                                                
027300     .                                                                    
027400 CA-UPPDATERA SECTION.                                                    
027500     SKIP1                                                                
027600     MOVE NEJ TO SW-ARTIKEL-UPPDATERAD                                    
027700     SKIP1                                                                
027800     IF R17-KVVECKOR-LT NUMERIC                                           
027900         MOVE R17-KVVECKOR-LT TO IMSLNK-KVVECKOR-LT                       
028000         MOVE JA TO SW-ARTIKEL-UPPDATERAD                                 
028100     END-IF                                                               
028200     SKIP1                                                                
028300     IF R17-KVVECKOR-AT NUMERIC                                           
028400         MOVE R17-KVVECKOR-AT TO IMSLNK-KVVECKOR-AT                       
028500         MOVE JA TO SW-ARTIKEL-UPPDATERAD                                 
028600     END-IF                                                               
028700     SKIP1                                                                
028800     SKIP1                                                                
028900     IF R17-KVDAGAR-TTC1 NUMERIC                                          
029000         MOVE R17-KVDAGAR-TTC1 TO IMSLNK-KVDAGAR-TTC1                     
029100         MOVE JA TO SW-ARTIKEL-UPPDATERAD                                 
029200     END-IF                                                               
029300     SKIP1                                                                
029400     IF R17-KVDAGAR-TTC2 NUMERIC                                          
029500         MOVE R17-KVDAGAR-TTC2 TO IMSLNK-KVDAGAR-TTC2                     
029600         MOVE JA TO SW-ARTIKEL-UPPDATERAD                                 
029700     END-IF                                                               
029800     SKIP1                                                                
029900     IF R17-KDLEVTYP NUMERIC                                              
030000         MOVE R17-KDLEVTYP TO IMSLNK-KDLEVTYP                             
030100         MOVE JA TO SW-ARTIKEL-UPPDATERAD                                 
030200     END-IF                                                               
030300     SKIP1                                                                
030400     IF R17-KDGK NUMERIC                                                  
030500         MOVE R17-KDGK TO IMSLNK-KDGK                                     
030600         MOVE JA TO SW-ARTIKEL-UPPDATERAD                                 
030700     END-IF                                                               
030800     SKIP1                                                                
030900     IF R17-IDLPKOLL NUMERIC                                              
031000         MOVE R17-IDLPKOLL TO IMSLNK-IDLPKOLL                             
031100     END-IF                                                               
031200     SKIP1                                                                
031300     IF R17-FLRSADR NOT = SPACE                                           
031400         MOVE R17-FLRSADR TO IMSLNK-FLRSADR                               
031500     END-IF                                                               
031600     SKIP1                                                                
031700     IF R17-FLEMBPOL NOT = SPACE                                          
031800         MOVE R17-FLEMBPOL TO IMSLNK-FLEMBPOL                             
031900     END-IF                                                               
032000     SKIP1                                                                
032100     IF R17-KDSPRAK NUMERIC                                               
032200         MOVE R17-KDSPRAK TO IMSLNK-KDSPRAK                               
032300     END-IF                                                               
032400     SKIP1                                                                
032500     MOVE +104 TO IMSLNK-KDCALL                                           
032600     CALL W2131210 USING IMSLNK-W213L121                                  
032700                         LEVA-PCB                                         
032800                         ARTC-PCB                                         
032810                         WDK6G-PCB                                        
032900                                                                          
033000         MOVE 'W21314'   TO POSTSUM-FDNAMN                                
033100         MOVE 'WDF101  ' TO POSTSUM-DDNAMN2                               
033200         MOVE 'REPL'     TO POSTSUM-TRANSTYP                              
033300         CALL POSTSUM USING POSTSUM-PARM                                  
033400                                                                          
033500     IF SW-ARTIKEL-UPPDATERAD = JA                                        
033501**       WORK WITH MFG SUPPLIER                                           
033510         MOVE +105 TO IMSLNK-KDCALL                                       
033520         CALL W2131210 USING IMSLNK-W213L121                              
033530                             LEVA-PCB                                     
033540                             ARTC-PCB                                     
033550         PERFORM UNTIL IMSLNK-FLJANEJ-ANROP = NEJ                         
033560             PERFORM CAA-UPPDATERA-ARTIKLAR-MFG                           
033570             MOVE +105 TO IMSLNK-KDCALL                                   
033580             CALL W2131210 USING IMSLNK-W213L121                          
033590                                 LEVA-PCB                                 
033591                                 ARTC-PCB                                 
033592         END-PERFORM                                                      
033593*                                                                         
033594**       WORK WITH SHP SUPPLIER                                           
033600         MOVE +109 TO IMSLNK-KDCALL                                       
033610         MOVE R17-IDLEVNR TO IMSLNK-IDLEVNR                               
033700         CALL W2131210 USING IMSLNK-W213L121                              
033800                             LEVA-PCB                                     
033900                             ARTC-PCB                                     
033910                             WDK6G-PCB                                    
034000         PERFORM UNTIL IMSLNK-FLJANEJ-ANROP = NEJ                         
034100             PERFORM CAB-UPPDATERA-ARTIKLAR-SHP                           
034200             MOVE +109 TO IMSLNK-KDCALL                                   
034300             CALL W2131210 USING IMSLNK-W213L121                          
034400                                 LEVA-PCB                                 
034500                                 ARTC-PCB                                 
034510                                 WDK6G-PCB                                
034600         END-PERFORM                                                      
034700     END-IF                                                               
034800     EJECT                                                                
034900     .                                                                    
035000 CAA-UPPDATERA-ARTIKLAR-MFG SECTION.                                      
035100     SKIP1                                                                
038600     IF R17-KDLEVTYP NUMERIC AND                                          
038700                            (IMSLNK-KDAVT-ARTNR = 0 OR 2 OR 3)            
038800         IF IMSLNK-KDAVT-ARTNR = 0 AND IMSLNK-KDKSP-ARTNR > 1             
038900             MOVE ZERO TO IMSLNK-KDKSP-ARTNR                              
039000         END-IF                                                           
039100         MOVE R17-KDLEVTYP TO IMSLNK-KDAVT-ARTNR                          
039200     END-IF                                                               
039300     SKIP1                                                                
039400     MOVE +110 TO IMSLNK-KDCALL                                           
039500     CALL W2131210 USING IMSLNK-W213L121                                  
039600                         LEVA-PCB                                         
039700                         ARTC-PCB                                         
039710                         WDK6G-PCB                                        
039800                                                                          
039900     MOVE 'W21314'   TO POSTSUM-FDNAMN                                    
040000     MOVE 'WDK611  ' TO POSTSUM-DDNAMN2                                   
040100     MOVE 'REPL'     TO POSTSUM-TRANSTYP                                  
040200     CALL POSTSUM USING POSTSUM-PARM                                      
041400     EJECT                                                                
041500     .                                                                    
041510 CAB-UPPDATERA-ARTIKLAR-SHP SECTION.                                      
041520     SKIP1                                                                
041530     IF R17-KDGK NUMERIC                                                  
041540     AND IMSLNK-FLMANGK-ARTNR = NEJ                                       
041550         MOVE R17-KDGK TO IMSLNK-KDGK-ARTNR                               
041560         IF IMSLNK-KDGK-ARTNR = 1                                         
041570             MOVE SPAR-KVDAGAR-TTC1 TO IMSLNK-KVDAGAR-TT-ARTNR            
041580         ELSE                                                             
041590             MOVE SPAR-KVDAGAR-TTC2 TO IMSLNK-KVDAGAR-TT-ARTNR            
041591         END-IF                                                           
041592     END-IF                                                               
041593     SKIP1                                                                
041594     IF R17-KVDAGAR-TTC1 NUMERIC                                          
041595     AND (IMSLNK-KDGK-ARTNR = 1)                                          
041596         MOVE R17-KVDAGAR-TTC1 TO IMSLNK-KVDAGAR-TT-ARTNR                 
041597     END-IF                                                               
041598     SKIP1                                                                
041599     IF R17-KVDAGAR-TTC2 NUMERIC                                          
041600     AND IMSLNK-KDGK-ARTNR = 2                                            
041601         MOVE R17-KVDAGAR-TTC2 TO IMSLNK-KVDAGAR-TT-ARTNR                 
041602     END-IF                                                               
041603     SKIP1                                                                
041604     IF R17-KVVECKOR-LT NUMERIC                                           
041605     AND IMSLNK-FLMANLT-ARTNR = NEJ                                       
041606       IF IMSLNK-KDHF > ZERO                                              
041607         CONTINUE                                                         
041608       ELSE                                                               
041609         MOVE R17-KVVECKOR-LT TO IMSLNK-KVVECKOR-LT-ARTNR                 
041610       END-IF                                                             
041620     END-IF                                                               
041621     SKIP1                                                                
041622     IF R17-KVVECKOR-AT NUMERIC                                           
041623     AND IMSLNK-FLMANAT-ARTNR = NEJ                                       
041624         MOVE R17-KVVECKOR-AT TO IMSLNK-KVVECKOR-AT-ARTNR                 
041625     END-IF                                                               
041626     SKIP1                                                                
041627     MOVE +111 TO IMSLNK-KDCALL                                           
041628     CALL W2131210 USING IMSLNK-W213L121                                  
041629                         LEVA-PCB                                         
041630                         ARTC-PCB                                         
041631                         WDK6G-PCB                                        
041632                                                                          
041633     MOVE 'W21314'   TO POSTSUM-FDNAMN                                    
041634     MOVE 'WDK611  ' TO POSTSUM-DDNAMN2                                   
041635     MOVE 'REPL'     TO POSTSUM-TRANSTYP                                  
041636     CALL POSTSUM USING POSTSUM-PARM                                      
041637*--------------------------------------- UPPDATERA HÄNDELSEREG.           
041638     SKIP1                                                                
041639     MOVE +108           TO IMSLNK-KDCALL                                 
041640     CALL W2131210 USING IMSLNK-W213L121                                  
041641                         LEVA-PCB                                         
041642                         ARTC-PCB                                         
041643                         WDK6G-PCB                                        
041644                                                                          
041645     MOVE 'W21312'   TO POSTSUM-FDNAMN                                    
041646     MOVE '2214    ' TO POSTSUM-DDNAMN2                                   
041647     MOVE 'ISRT'     TO POSTSUM-TRANSTYP                                  
041648     CALL POSTSUM USING POSTSUM-PARM                                      
041649     EJECT                                                                
041650     .                                                                    
041660 D-UPPDATERA-ANSKAFFARE SECTION.                                          
041700     SKIP1                                                                
041800     MOVE R02-IDLEVNR TO IMSLNK-IDLEVNR                                   
041900     PERFORM S03-LAS-WLLEVA01                                             
042000     SKIP1                                                                
042100     IF IMSLNK-FLJANEJ-ANROP = NEJ                                        
042200         MOVE ZERO TO FEL-W092W001                                        
042300         MOVE R02-IDPTYP TO FEL-IDPTYP                                    
042400         MOVE R02-IDLEVNR TO FEL-SORTBGP                                  
042500         MOVE '001' TO FEL-IDFELKODX                                      
042600         PERFORM S05-FLYTTA-R02-TILL-FELPOST                              
042700         PERFORM S02-SKRIV-W21313                                         
042800     ELSE                                                                 
042900         MOVE +1 TO IXPG                                                  
043000         PERFORM UNTIL IXPG > 8                                           
043100     SKIP1                                                                
043200         IF R02-IDPLANGR-ANSK (IXPG) NUMERIC                              
043300         AND R02-IDPLANGR-ANSK (IXPG) NOT LESS 1                          
043400         AND R02-IDPLANGR-ANSK (IXPG) NOT GREATER 8                       
043500             MOVE R02-IDPLANGR-ANSK (IXPG) TO W-IX                        
043600             MOVE R02-IDANSK (IXPG) TO IMSLNK-IDANSK-PG (W-IX)            
043700         END-IF                                                           
043800     SKIP1                                                                
043900         ADD +1 TO IXPG                                                   
044000     END-PERFORM                                                          
044100     MOVE +204                 TO IMSLNK-KDCALL                           
044200     CALL W2131210 USING IMSLNK-W213L121                                  
044300                         LEVA-PCB                                         
044400                         ARTC-PCB                                         
044410                         WDK6G-PCB                                        
044500                                                                          
044600         MOVE 'W21314'   TO POSTSUM-FDNAMN                                
044700         MOVE 'WDF101  ' TO POSTSUM-DDNAMN2                               
044800         MOVE 'REPL'     TO POSTSUM-TRANSTYP                              
044900         CALL POSTSUM USING POSTSUM-PARM                                  
045000                                                                          
045100     MOVE +105 TO IMSLNK-KDCALL                                           
045200     CALL W2131210 USING IMSLNK-W213L121                                  
045300                         LEVA-PCB                                         
045400                         ARTC-PCB                                         
045410                         WDK6G-PCB                                        
045500     SKIP1                                                                
045600     MOVE ZERO TO WS-SPAR-IDANSK                                          
045700     PERFORM UNTIL IMSLNK-FLJANEJ-ANROP = NEJ                             
045800         MOVE +1 TO IXPG                                                  
045900         PERFORM UNTIL IXPG > 8                                           
046000             IF R02-IDPLANGR-ANSK (IXPG) NUMERIC                          
046100             AND R02-IDPLANGR-ANSK (IXPG) > ZERO AND < 9                  
046200             AND R02-IDPLANGR-ANSK (IXPG)                                 
046300             = IMSLNK-IDPLANGR-AG-ARTNR                                   
046400                 MOVE IMSLNK-IDANSK-ARTNR TO WS-SPAR-IDANSK               
046500                 MOVE R02-IDANSK (IXPG)                                   
046600                             TO IMSLNK-IDANSK-ARTNR                       
046700             END-IF                                                       
046800             ADD +1 TO IXPG                                               
046900         END-PERFORM                                                      
047000         MOVE +206 TO IMSLNK-KDCALL                                       
047100         CALL W2131210 USING IMSLNK-W213L121                              
047200                             LEVA-PCB                                     
047300                             ARTC-PCB                                     
047310                             WDK6G-PCB                                    
047400                                                                          
047500         MOVE 'W21314'   TO POSTSUM-FDNAMN                                
047600         MOVE 'WDK611  ' TO POSTSUM-DDNAMN2                               
047700         MOVE 'REPL'     TO POSTSUM-TRANSTYP                              
047800         CALL POSTSUM USING POSTSUM-PARM                                  
047900                                                                          
048000         IF WS-SPAR-IDANSK = 701 OR 702 OR 703                            
048100            PERFORM S06-SKRIV-21315                                       
048200            MOVE ZERO TO WS-SPAR-IDANSK                                   
048300         END-IF                                                           
048400                                                                          
048500         MOVE +105 TO IMSLNK-KDCALL                                       
048600         CALL W2131210 USING IMSLNK-W213L121                              
048700                             LEVA-PCB                                     
048800                             ARTC-PCB                                     
048810                             WDK6G-PCB                                    
048900     END-PERFORM                                                          
049000     END-IF                                                               
049100     EJECT                                                                
049200     .                                                                    
049300 Z-AVSLUTA SECTION.                                                       
049400     SKIP1                                                                
049500     CLOSE W21302 W21313 W21315                                           
049600                                                                          
049700     MOVE +999 TO IMSLNK-KDCALL                                           
049800     CALL W2131210 USING IMSLNK-W213L121                                  
049900                         LEVA-PCB                                         
050000                         ARTC-PCB                                         
050010                         WDK6G-PCB                                        
050100                                                                          
050200     MOVE 'S' TO POSTSUM-OPKOD                                            
050300     CALL POSTSUM USING POSTSUM-PARM                                      
050400     EJECT                                                                
050500     .                                                                    
050600 S01-LAS-W21302 SECTION.                                                  
050700     SKIP1                                                                
050800     READ W21302 INTO TRANS-AREA                                          
050900         END MOVE JA TO SW-EOF-W21302.                                    
051000     SKIP1                                                                
051100     IF SW-EOF-W21302 = NEJ                                               
051200         MOVE 'W21302' TO POSTSUM-FDNAMN                                  
051300         MOVE 'W21302D1' TO POSTSUM-DDNAMN2                               
051400         MOVE R17-IDPTYP TO POSTSUM-TRANSTYP                              
051500         CALL POSTSUM USING POSTSUM-PARM                                  
051600     END-IF                                                               
051700     EJECT                                                                
051800     .                                                                    
051900 S02-SKRIV-W21313 SECTION.                                                
052000     SKIP1                                                                
052100     WRITE FEL-POST FROM FEL-AREA                                         
052200     MOVE 'W21313' TO POSTSUM-FDNAMN                                      
052300     MOVE 'W21313D2' TO POSTSUM-DDNAMN2                                   
052400     MOVE SPACE TO POSTSUM-TRANSTYP                                       
052500     CALL POSTSUM USING POSTSUM-PARM                                      
052600     EJECT                                                                
052700     .                                                                    
052800 S03-LAS-WLLEVA01 SECTION.                                                
052900     SKIP1                                                                
053000     MOVE +101 TO IMSLNK-KDCALL                                           
053100     CALL W2131210 USING IMSLNK-W213L121                                  
053200                         LEVA-PCB                                         
053300                         ARTC-PCB                                         
053310                         WDK6G-PCB                                        
053400     IF IMSLNK-FLJANEJ-ANROP = JA                                         
053500         MOVE IMSLNK-KVDAGAR-TTC1 TO SPAR-KVDAGAR-TTC1                    
053600         MOVE IMSLNK-KVDAGAR-TTC2 TO SPAR-KVDAGAR-TTC2                    
053700     END-IF                                                               
053800     EJECT                                                                
053900     .                                                                    
054000 S04-FLYTTA-R17-TILL-FELPOST SECTION.                                     
054100     SKIP1                                                                
054200     MOVE R17-W213R17T             TO FEL17-W213R17T                      
054300     EJECT                                                                
054400     .                                                                    
054500 S05-FLYTTA-R02-TILL-FELPOST SECTION.                                     
054600     SKIP1                                                                
054700     MOVE R02-IDPTYP               TO FEL02-IDPTYP                        
054800     MOVE R02-IDLEVNR              TO FEL02-IDLEVNR                       
054900     MOVE +1                       TO IXPG                                
055000     PERFORM UNTIL IXPG > 8                                               
055100       MOVE R02-IDPLANGR-ANSK (IXPG) TO FEL02-IDPLANGR-ANSK (IXPG)        
055200       MOVE R02-IDANSK (IXPG)        TO FEL02-IDANSK (IXPG)               
055300       ADD +1 TO IXPG                                                     
055400     END-PERFORM                                                          
055500     .                                                                    
055600 S06-SKRIV-21315 SECTION.                                                 
055700     SKIP1                                                                
055800                                                                          
055900     MOVE IMSLNK-IDARTNR       TO ANSK-IDARTNR                            
056000     MOVE 15                   TO ANSK-KDLPORS                            
056100     MOVE '2204'               TO ANSK-IDHTYP                             
056200                                                                          
056300     WRITE ANSKBYTE-POST FROM ANSK-AREA                                   
056400                                                                          
056500     MOVE 'W21315'   TO POSTSUM-FDNAMN                                    
056600     MOVE 'W21312D5' TO POSTSUM-DDNAMN2                                   
056700     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
056800     CALL POSTSUM USING POSTSUM-PARM                                      
056900     .                                                                    
057000     EJECT                                                                

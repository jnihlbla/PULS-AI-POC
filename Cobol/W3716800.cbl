000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3716800.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   DEC-1999.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION: PGM UPPDATERAR SEGMENT PÅ WDA7                             
000800*                                                                         
001100*    INDATA:   W37111 ORDERTRANSAKTIONER TILL EV. FAKTURERING             
001200*              W37133 SIGNALTRANSAKTIONER FÖR KONS.DISTRIKT               
001210*                     SOM FÅR FAKTURA                                     
001300*                                                                         
001400*    RETURKODER:                                                          
001500*                 999 (DUMPKOD VID FELLOG ).                              
001600     EJECT                                                                
001610                                                                          
001700 ENVIRONMENT DIVISION.                                                    
001800 INPUT-OUTPUT SECTION.                                                    
001900 FILE-CONTROL.                                                            
002000*- - - - - - - - - - - - - - - - - - - - - - INFILER.                     
002010     SELECT W37111     ASSIGN  TO  UT-S-W37168D1.                         
002051                                                                          
002053     SELECT W37133     ASSIGN  TO  UT-S-W37168D2.                         
002054     EJECT                                                                
002055                                                                          
002060 DATA DIVISION.                                                           
002070 FILE SECTION.                                                            
002080                                                                          
002090*  ORDERTRANSAKTIONER TILL FAKTURERING                                    
002100*                                                                         
002200 FD  W37111                                                               
002300     RECORDING F                                                          
002400     BLOCK 0 RECORDS.                                                     
002500                                                                          
002600*01  -COPY W37111  -L.                                                    
002700     EJECT                                                                
002710                                                                          
002720*  SIGNALTRANSAKTIONER                                                    
002730*                                                                         
002740 FD  W37133                                                               
002750     RECORDING F                                                          
002760     BLOCK 0 RECORDS.                                                     
002770                                                                          
002780 01  -COPY W37133  -L.                                                    
002790     EJECT                                                                
002791                                                                          
002800 WORKING-STORAGE SECTION.                                                 
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                   PIC X(8)            VALUE 'W3716800'.        
003300 77  JA                      PIC X               VALUE 'J'.               
003400 77  NEJ                     PIC X               VALUE 'N'.               
003500                                                                          
003600 01  CHKP-VAR.                                                            
003700   03  CHKP-MSG-IO-AREA-LENGTH   PIC S9(9)   VALUE +32 COMP SYNC.         
003800   03  CHKP-MSG-IO-AREA          PIC X(32)   VALUE SPACE.                 
003900   03  CHKP-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
004000   03  CHKP-AREA                 PIC X(32)   VALUE SPACE.                 
004100   03  CHKP-ANT                  PIC S9(3)   VALUE +0.                    
004200   03  CHKP-MAX                  PIC S9(3)   VALUE +100.                  
004300                                                                          
004400 01  FELTEXT.                                                             
004500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004700                                                                          
004800 01  DIVERSE.                                                             
004900     03  W37111-EOF          PIC X               VALUE 'N'.               
004910     03  W37133-EOF          PIC X               VALUE 'N'.               
005000     03  RETURKOD            PIC S9(4) COMP SYNC VALUE +0.                
005100     03  DAGENS-DATUM        PIC 9(8).                                    
005200     03  DAGENS-TID          PIC 9(8).                                    
005300     EJECT                                                                
007700                                                                          
007800 01  FILLER PIC X(16) VALUE '*****NYCKLAR****'.                           
007900                                                                          
007910 01  NYCKLAR-TILL-DLI.                                                    
007911     03  W-WDA701KY-X.                                                    
007920         05  W-IDPTYP            PIC  X(3)  VALUE 'ADJ'.                  
007960         05  W-IDDISTR           PIC  S9(5) VALUE ZERO COMP-3.            
007970         05  W-IDARTNR           PIC  S9(9) VALUE ZERO COMP-3.            
007980         05  W-IDKUNDNR          PIC  S9(7) VALUE ZERO COMP-3.            
007990         05  W-IDBYTRAD          PIC  S9(5) VALUE ZERO COMP-3.            
008900     EJECT                                                                
008910                                                                          
009000 01  GENERELLA-SUBPROGRAM.                                                
009100     03  FELLOG                  PIC X(8)      VALUE 'FELLOG  '.          
009200     03  CBLTDLI                 PIC X(8)      VALUE 'CBLTDLI '.          
009300     03  POSTSUM                 PIC X(8)      VALUE 'POSTSUM '.          
009500     03  ABEND                   PIC X(8)      VALUE 'ABEND   '.          
009700                                                                          
009800*---- PARAMETRAR TILL ABEND                                               
009900 01  RETURKODER.                                                          
010000     03  RKOD                    PIC S9(4) COMP SYNC VALUE ZERO.          
010100     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.           
010200     03  RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +1000.         
010400     EJECT                                                                
010410                                                                          
010500 01  FILLER                      PIC X(16) VALUE '*****W37111'.           
010600*01  -COPY W37111  -PRE ORDER-.                                           
010700     EJECT                                                                
010701                                                                          
010710 01  FILLER                      PIC X(16) VALUE '*****W37133'.           
010711*01  -COPY W37133  -PRE SIGNAL-.                                          
010730     EJECT                                                                
010740                                                                          
010800*-----------------------------------------PARAMETRAR TILL                 
010900*                                         SUBPROGRAM POSTSUM              
011000 01  FILLER             PIC X(7)   VALUE 'POSTSUM'.                       
011100*01  -COPY W0005   -PRE POSTSUM-.                                         
011200     EJECT                                                                
011210                                                                          
011300*-----------------------------------------PARAMETRAR TILL                 
011400*                                         SUBPROGRAM WDATKONV             
011500 01  FILLER             PIC X(8)   VALUE 'WDATKONV'.                      
011600*01  -COPY WDATAREA.                                                      
011700     EJECT                                                                
011710                                                                          
011800*    SPARAREOR FÖR DATABASSEGMENT                                         
011900*                                                                         
012000 01  FILLER             PIC X(16) VALUE  'WDA701  '.                      
012110 01  DLI-IO-WDA701.                                                       
012120*    03  -COPY WDA701                                                     
012130     EJECT                                                                
012200                                                                          
012600*****                                                                     
012700*****    IN-AREA TILL IMS-SEKTIONERNA                                     
012800*****                                                                     
012900 01  IMS-WORKAREOR.                                                       
013000     03  FILLER          PIC X(16)   VALUE '*-*-*IMS-WS*-*-*'.            
013100     03  STATUS-WS       PIC XX.                                          
013200         88  SEGMENT-FINNS       VALUE '  '.                              
013210         88  SEGMENT-FINNS-REDAN VALUE 'II'.                              
013220         88  INDEX-FINNS-REDAN   VALUE 'NI'.                              
013300         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
013400         88  BASEN-SLUT          VALUE 'GB'.                              
013500         88  IMS-EJ-OK           VALUE 'XD'.                              
013600                                                                          
013700     03  GODK-STATUSKODER.                                                
013800         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.            
013900                                                                          
014000 01      SSA1            PIC X(64).                                       
014100 01      SSA2            PIC X(64).                                       
014200     EJECT                                                                
014300*                                                                         
014400*        IMS FUNKTIONSKODER                                               
014500*                                                                         
014600*01      -COPY W0003                                                      
014700     EJECT                                                                
014800                                                                          
015000 LINKAGE SECTION.                                                         
015100*  MSG                                                                    
015200*01  -COPY W0009     -PRE MSG-                                            
015300   EJECT                                                                  
015400                                                                          
015500*01  -COPY W0008     -PRE WDA7-                                           
015600     05  FILLER          PIC X(1).                                        
015700   EJECT                                                                  
015710                                                                          
015800 PROCEDURE DIVISION  USING MSG-PCB WDA7-PCB.                              
015900 MAIN SECTION.                                                            
016000     ENTRY 'DLITCBL' USING MSG-PCB WDA7-PCB.                              
016100                                                                          
016400     PERFORM A-INIT                                                       
016500                                                                          
016600     PERFORM B-BEARBETNING                                                
016700                                                                          
016710     PERFORM Z-FINI                                                       
016800                                                                          
016900     MOVE +0 TO RETURN-CODE                                               
017000     GOBACK                                                               
017100     .                                                                    
017200     EJECT                                                                
017400                                                                          
017410 A-INIT SECTION.                                                          
017420     MOVE FUNCTION CURRENT-DATE(1:8)  TO   DAGENS-DATUM                   
017430     ACCEPT DAGENS-TID                FROM TIME                           
017440                                                                          
017500     PERFORM IMS-RESTART                                                  
017600                                                                          
018400     MOVE 'W37168'                    TO   POSTSUM-PROGNAMN               
018500                                                                          
018600     OPEN INPUT W37111                                                    
018610                W37133                                                    
018700     .                                                                    
018800     EJECT                                                                
019000                                                                          
019010 B-BEARBETNING SECTION.                                                   
019100     PERFORM S01-READ-W37111                                              
019110     PERFORM S02-READ-W37133                                              
019200                                                                          
019300     PERFORM UNTIL W37111-EOF = JA                                        
019400       IF ORDER-IDDISTR-BET = SIGNAL-IDDISTR-BET AND                      
019410          ORDER-KDEXCHA     = SIGNAL-KDEXCHA                              
019500         PERFORM BA-BYGG-WDA701                                           
019510                                                                          
019600         PERFORM IMS-ISRT-WDA701                                          
019700         IF INDEX-FINNS-REDAN                                             
019800           PERFORM UNTIL NOT INDEX-FINNS-REDAN                            
019900             ADD +1             TO ROT-IDBYTRAD                           
020000             PERFORM IMS-ISRT-WDA701                                      
020100           END-PERFORM                                                    
020200         END-IF                                                           
020534         ADD +1                 TO CHKP-ANT                               
020535                                                                          
021700         IF CHKP-ANT > CHKP-MAX                                           
021800           PERFORM X-TAG-CHECKPOINT                                       
021900         END-IF                                                           
022000         PERFORM S01-READ-W37111                                          
022010       ELSE                                                               
022020         IF ORDER-IDDISTR-BET > SIGNAL-IDDISTR-BET OR                     
022021           (ORDER-IDDISTR-BET = SIGNAL-IDDISTR-BET AND                    
022022            ORDER-KDEXCHA     > SIGNAL-KDEXCHA)                           
022030           PERFORM S02-READ-W37133                                        
022040         ELSE                                                             
022050           PERFORM S01-READ-W37111                                        
022060         END-IF                                                           
022070       END-IF                                                             
022100     END-PERFORM                                                          
022300     .                                                                    
022400     EJECT                                                                
022500                                                                          
024000 BA-BYGG-WDA701 SECTION.                                                  
024102     MOVE 'ADJ'             TO ROT-IDPTYP                                 
024103     MOVE ORDER-IDDISTR     TO ROT-IDDISTR                                
024104     MOVE ORDER-IDARTNR     TO ROT-IDARTNR-BYT                            
024105     MOVE ORDER-IDKUNDNR    TO ROT-IDKUNDNR                               
024106     MOVE +1                TO ROT-IDBYTRAD                               
024107     MOVE ZERO              TO ROT-DAFAKT                                 
024108     MOVE SPACE             TO ROT-IDDC                                   
024109     MOVE ZERO              TO ROT-IDORDER                                
024110     MOVE ORDER-KDEXCHA     TO ROT-KDEXCHA                                
024111     MOVE 1                 TO ROT-KVANTAL                                
024112     COMPUTE ROT-KVPOINT = ORDER-KVBEART * -1                             
024113     END-COMPUTE                                                          
024114     MOVE 'SURCHARGE '      TO ROT-TENOTE                                 
024115     MOVE 'W3716800'        TO ROT-IDUSER                                 
024116     MOVE DAGENS-DATUM      TO ROT-DAREGDAT                               
024120     MOVE DAGENS-TID        TO ROT-TIKLOCK                                
024600     .                                                                    
024700     EJECT                                                                
024720                                                                          
024721 Z-FINI SECTION.                                                          
024722     CLOSE W37111                                                         
024723           W37133                                                         
024724                                                                          
024725     MOVE 'S' TO POSTSUM-OPKOD                                            
024726     CALL POSTSUM USING POSTSUM-PARM                                      
024727     .                                                                    
024728     EJECT                                                                
024729                                                                          
024730 S01-READ-W37111 SECTION.                                                 
024731     READ W37111 INTO ORDER-W37111                                        
024740     AT END                                                               
024750        MOVE JA         TO W37111-EOF                                     
024760     NOT AT END                                                           
024770        MOVE 'W37111'   TO POSTSUM-FDNAMN                                 
024780        MOVE 'W37168D1' TO POSTSUM-DDNAMN2                                
024790        CALL POSTSUM USING POSTSUM-PARM                                   
024791     END-READ                                                             
024792     .                                                                    
024793     EJECT                                                                
024794                                                                          
024795 S02-READ-W37133 SECTION.                                                 
024796     READ W37133 INTO SIGNAL-W37133                                       
024797     AT END                                                               
024798        MOVE JA         TO W37133-EOF                                     
024799                           W37111-EOF                                     
024800     NOT AT END                                                           
024801        MOVE 'W37133'   TO POSTSUM-FDNAMN                                 
024810        MOVE 'W37168D2' TO POSTSUM-DDNAMN2                                
024820        CALL POSTSUM USING POSTSUM-PARM                                   
024830     END-READ                                                             
024840     .                                                                    
024850     EJECT                                                                
024860                                                                          
024900 X-TAG-CHECKPOINT   SECTION.                                              
025000     PERFORM IMS-CHECKPOINT                                               
025100     MOVE ZERO TO CHKP-ANT                                                
025200     .                                                                    
025300     EJECT                                                                
025310                                                                          
025400 IMS-RESTART SECTION.                                                     
025600     MOVE SPACE           TO CHKP-MSG-IO-AREA                             
025700     MOVE '  '            TO GODK-STATUSKODER                             
025800     CALL CBLTDLI USING XRST MSG-PCB                                      
025900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026000                        CHKP-AREA-LENGTH CHKP-AREA                        
026100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026200     PERFORM IMS-STATUSKONTROLL                                           
026300     .                                                                    
026400                                                                          
026500 IMS-CHECKPOINT SECTION.                                                  
026700     MOVE SPACE           TO CHKP-MSG-IO-AREA                             
026800     MOVE '  XD'          TO GODK-STATUSKODER                             
026900     CALL CBLTDLI USING CHKP MSG-PCB                                      
027000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027100                        CHKP-AREA-LENGTH CHKP-AREA                        
027200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027300     PERFORM IMS-STATUSKONTROLL                                           
027400                                                                          
027500     IF IMS-EJ-OK                                                         
027600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
027700       DISPLAY FELTEXT                                                    
027800       CALL FELLOG                                                        
027900     END-IF                                                               
028000     .                                                                    
028100     EJECT                                                                
029500                                                                          
034001 IMS-ISRT-WDA701 SECTION.                                                 
034002     MOVE 'WDA701   '      TO SSA1                                        
034003     MOVE '  NI'           TO GODK-STATUSKODER                            
034004     CALL CBLTDLI USING ISRT WDA7-PCB DLI-IO-WDA701 SSA1                  
034005     MOVE WDA7-STATUS-CODE TO STATUS-WS                                   
034006     PERFORM IMS-STATUSKONTROLL                                           
034007     .                                                                    
034800     EJECT                                                                
034810                                                                          
034900 IMS-STATUSKONTROLL SECTION.                                              
035100     SET STATUS-IX TO 1                                                   
035200     SEARCH GODK-STATUS AT END CALL FELLOG                                
035300     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
035400     CONTINUE                                                             
035500     END-SEARCH                                                           
035600     .                                                                    

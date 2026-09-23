000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2253000.                                                
000400 AUTHOR.         ANN JORDEBO.                                             
000500 DATE-WRITTEN.   90/11/05.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        BMP-PROGRAM SOM LÄSER NER WDM4 (WLURVA).                         
001100*        SKRIVER EN POST PER URVAL, TAR SEDAN BORT                        
001200*        URVALET FRÅN WDM4.                                               
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- FIL MED URVAL (TPO 1-6)                                    
002700     SELECT W22530                     ASSIGN TO W22530D1.                
002800     SKIP2                                                                
002900*          --- FIL MED URVAL (TPO 3)                                      
003000     SELECT W22531                     ASSIGN TO W22530D2.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W22530                                                               
003700     LABEL RECORD    STANDARD                                             
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000     SKIP2                                                                
004100*01  POST -COPY W22530    -PRE  UT1-  -L                                  
004300                                                                          
004400     SKIP3                                                                
004500 FD  W22531                                                               
004600     LABEL RECORD    STANDARD                                             
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900     SKIP2                                                                
005000*01  POST -COPY W22531    -PRE  UT2-  -L                                  
005200                                                                          
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500     SKIP2                                                                
005501                                                                          
005510*    -- CHECKED BY WY2000                                                 
005600 77  IDPGM                       PIC X(8)    VALUE 'W2253000'.            
005700 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000     EJECT                                                                
006100 01  ARBETSFAELT.                                                         
006200     03  ANTAL                   PIC S9(3)   VALUE ZERO COMP-3.           
006300     03  IX                      PIC S9(3)   VALUE ZERO COMP-3.           
006400 01  DAGENS-DATUM.                                                        
006500     03  DAGENS-DATUM-AAR        PIC X(2)    VALUE SPACE.                 
006600     03  DAGENS-DATUM-MAANAD     PIC X(2)    VALUE SPACE.                 
006700     03  DAGENS-DATUM-DAG        PIC X(2)    VALUE SPACE.                 
006800     EJECT                                                                
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000*                                                                         
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007400     03  VIMSREGT                PIC X(8)    VALUE 'VIMSREGT'.            
007500     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL ABEND                                            
007900     SKIP2                                                                
008000 01  RKOD-ABEND                  PIC S9(4)   VALUE +33.                   
008100*    --- PARAMETRAR TILL VIMSREGT                                         
008200     SKIP2                                                                
008300 01  FILLER                      PIC X(16)   VALUE                        
008400                                             'VIMSREGT-AREA'.             
008500     SKIP2                                                                
008600 01  IMS-VIMSREGT                PIC S9(9)   COMP SYNC.                   
008700     88  BMP                                 VALUE +8.                    
008800     88  BATCH                               VALUE +16 THRU +64.          
008900     EJECT                                                                
009000*    --- PARAMETRAR TILL DATKORT                                          
009100*                                                                         
009200 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22530'.              
009300     SKIP2                                                                
009400 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
009500     SKIP2                                                                
009600*01  -COPY WDATKORT                                                       
009800     EJECT                                                                
009900*    --- PARAMETRAR TILL POSTSUM                                          
010000*                                                                         
010100*01  -COPY W0005      -PRE  POSTSUM-                                      
010300     EJECT                                                                
010400 01  UT1-AREA-START              PIC X(24)   VALUE                        
010500                                             'UT1-AREA-START'.            
010600     SKIP2                                                                
010700                                                                          
010800*01  AREA -COPY W22530         -PRE UT1-                                  
011000     EJECT                                                                
011100 01  UT2-AREA-START              PIC X(24)   VALUE                        
011200                                             'UT2-AREA-START'.            
011300     SKIP2                                                                
011400                                                                          
011500*01  AREA -COPY W22531         -PRE UT2-                                  
011700*                                                                         
011800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011900     SKIP3                                                                
012000 01  NYCKLAR-TILL-DLI.                                                    
012100     03  W-WDM401KY-X.                                                    
012200         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
012300         05  W-TIREGDAT          PIC S9(7)   VALUE ZERO COMP-3.           
012400         05  W-TIREGTID          PIC S9(7)   VALUE ZERO COMP-3.           
012900     SKIP2                                                                
013000*    --- STATUS-KOD FRÅN IMS                                              
013100 01  STATUS-WS                   PIC XX.                                  
013200     88  SEGMENT-FINNS                       VALUE '  '.                  
013300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013600     88  IMS-EJ-OK                           VALUE 'XD'.                  
013700     SKIP2                                                                
013800 01  GODK-STATUSKODER.                                                    
013900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014000     SKIP3                                                                
014100 01  SSA1                        PIC X(64).                               
014200 01  SSA2                        PIC X(64).                               
014300     EJECT                                                                
014400*    --- IMS FUNKTIONSKODER                                               
014500*01  -COPY W0003                                                          
014700     EJECT                                                                
014800*    ---  DLI INPUT-OUTPUT AREA                                           
014900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
015000     SKIP3                                                                
015100 01  DLI-IO-AREA.                                                         
015200     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
015300     SKIP3                                                                
015400     03  WLURVA01 REDEFINES IO-AREA.                                      
015500*        05  -COPY WDM401     -PRE URVA-                                  
015700     SKIP3                                                                
015800     03  WLURVA11 REDEFINES IO-AREA.                                      
015900*        05  -COPY WDM411     -PRE URVA-                                  
016100     SKIP3                                                                
016200     03  WLURVA12 REDEFINES IO-AREA.                                      
016300*        05  -COPY WDM412     -PRE URVA-                                  
016500     EJECT                                                                
016600 LINKAGE SECTION.                                                         
016700                                                                          
016800*01  -COPY W0008      -PRE MSG-                                           
017000     05 FILLER                   PIC X(4).                                
017100     EJECT                                                                
017200*01  -COPY W0008      -PRE URVA-                                          
017400     05  FILLER                  PIC X.                                   
017500     EJECT                                                                
017600 PROCEDURE DIVISION  USING MSG-PCB URVA-PCB.                              
017700     ENTRY 'DLITCBL' USING MSG-PCB URVA-PCB.                              
017800                                                                          
017900     SKIP2                                                                
018000     PERFORM A-INIT                                                       
018100     MOVE +1 TO ANTAL                                                     
018200     PERFORM IMS-GN-URVA-USER                                             
018300     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
018400                   OR ANTAL > +200                                        
018500       MOVE URVA-USER-IDUSER   TO W-IDUSER                                
018600       MOVE URVA-USER-TIREGDAT TO W-TIREGDAT                              
018700       MOVE URVA-USER-TIREGTID TO W-TIREGTID                              
018800       PERFORM IMS-GNP-URVA-URV1                                          
018900       IF  URVA-URV1-KDTPOTYP-FOM = 3                                     
019000       AND URVA-URV1-KDTPOTYP-TOM = 3                                     
019100         PERFORM B-SKRIV-POST-W22531                                      
019200       ELSE                                                               
019300         PERFORM C-SKRIV-POST-W22530                                      
019400       END-IF                                                             
019500       PERFORM IMS-GHU-URVA-USER                                          
019600       PERFORM IMS-DLET-URVA                                              
019700       PERFORM IMS-GN-URVA-USER                                           
019800       ADD +1 TO ANTAL                                                    
019900     END-PERFORM                                                          
020000                                                                          
020100     IF SEGMENT-FINNS AND ANTAL > +200                                    
020200       MOVE ' TABELLEN FYLLD - FÖR MÅNGA URVAL' TO FELTEXT                
020300       CALL ABEND USING RKOD-ABEND                                        
020400     END-IF                                                               
020500                                                                          
020600     PERFORM Z-FINIT                                                      
020700                                                                          
020800     MOVE ZERO TO RETURN-CODE                                             
020900     GOBACK                                                               
021000     .                                                                    
021100     EJECT                                                                
021200 A-INIT SECTION.                                                          
021300     SKIP2                                                                
021400     CALL VIMSREGT                                                        
021500     MOVE RETURN-CODE TO IMS-VIMSREGT                                     
021600     IF BMP                                                               
021700                                                                          
021800       OPEN OUTPUT W22530                                                 
021900                                                                          
022000                   W22531                                                 
022100                                                                          
022200       ACCEPT DAGENS-DATUM   FROM DATE                                    
022300                                                                          
022400     ELSE                                                                 
022500                                                                          
022600       OPEN OUTPUT W22530                                                 
022700                                                                          
022800                   W22531                                                 
022900                                                                          
023000       CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT             
023100       MOVE D-AAR       TO DAGENS-DATUM-AAR                               
023200       MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                            
023300       MOVE D-DAG       TO DAGENS-DATUM-DAG                               
023400     END-IF                                                               
023500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023600                                                                          
023700     INITIALIZE UT1-AREA                                                  
023800     INITIALIZE UT2-AREA                                                  
023900     .                                                                    
024000     EJECT                                                                
024100 B-SKRIV-POST-W22531 SECTION.                                             
024200                                                                          
024300     MOVE W-IDUSER               TO UT2-IDUSER                            
024400     MOVE W-TIREGDAT             TO UT2-TIREGDAT                          
024500     MOVE W-TIREGTID             TO UT2-TIREGTID                          
024600                                                                          
024700     MOVE URVA-URV1-IDANSK-FOM   TO UT2-IDANSK-FOM                        
024800     MOVE URVA-URV1-IDANSK-TOM   TO UT2-IDANSK-TOM                        
024900     MOVE URVA-URV1-KDSORT1      TO UT2-KDSORT1                           
025000     MOVE URVA-URV1-IDLEVNR      TO UT2-IDLEVNR                           
025100     MOVE URVA-URV1-KDPRODSL     TO UT2-KDPRODSL                          
025200     MOVE URVA-URV1-KDBASLM-FOM  TO UT2-KDBASLM-FOM                       
025300     MOVE URVA-URV1-KDBASLM-TOM  TO UT2-KDBASLM-TOM                       
025400     MOVE URVA-URV1-TITPO-FOM    TO UT2-TITPO-FOM                         
025500     MOVE URVA-URV1-TITPO-TOM    TO UT2-TITPO-TOM                         
025600                                                                          
025700     PERFORM IMS-GNP-URVA-ART                                             
025800     MOVE +1 TO IX                                                        
025900     PERFORM UNTIL SEGMENT-SAKNAS OR IX > +100                            
026000       MOVE URVA-ART-IDARTNR    TO UT2-IDARTNR(IX)                        
026100       ADD +1 TO IX                                                       
026200       PERFORM IMS-GNP-URVA-ART                                           
026300     END-PERFORM                                                          
026400                                                                          
026500     PERFORM S12-SKRIV-W22531                                             
026600     PERFORM BA-NOLLA-UT2-POST                                            
026700     .                                                                    
026800     EJECT                                                                
026900                                                                          
027000 BA-NOLLA-UT2-POST SECTION.                                               
027100                                                                          
027200     MOVE SPACE  TO UT2-IDUSER                                            
027300                    UT2-KDBASLM-FOM                                       
027400                    UT2-KDBASLM-TOM                                       
027410                    UT2-IDLEVNR                                           
027500     MOVE ZERO   TO UT2-TIREGDAT                                          
027600                    UT2-TIREGTID                                          
027700                    UT2-IDANSK-FOM                                        
027800                    UT2-IDANSK-TOM                                        
027900                    UT2-KDSORT1                                           
028100                    UT2-KDPRODSL                                          
028200                    UT2-TITPO-FOM                                         
028300                    UT2-TITPO-TOM                                         
028400     MOVE +1 TO IX                                                        
028500     PERFORM UNTIL IX > +100                                              
028600       MOVE ZERO TO UT2-IDARTNR(IX)                                       
028700       ADD +1 TO IX                                                       
028800     END-PERFORM                                                          
028900     .                                                                    
029000     EJECT                                                                
029100 C-SKRIV-POST-W22530 SECTION.                                             
029200                                                                          
029300     MOVE W-IDUSER               TO UT1-IDUSER                            
029400     MOVE W-TIREGDAT             TO UT1-TIREGDAT                          
029500     MOVE W-TIREGTID             TO UT1-TIREGTID                          
029600                                                                          
029700     MOVE URVA-URV1-IDANSK-FOM   TO UT1-IDANSK-FOM                        
029800     MOVE URVA-URV1-IDANSK-TOM   TO UT1-IDANSK-TOM                        
029900     MOVE URVA-URV1-KDSORT1      TO UT1-KDSORT1                           
030000     MOVE URVA-URV1-IDLEVNR      TO UT1-IDLEVNR                           
030100     MOVE URVA-URV1-KDPRODSL     TO UT1-KDPRODSL                          
030200     MOVE URVA-URV1-IDDISTR-FOM  TO UT1-IDDISTR-FOM                       
030300     MOVE URVA-URV1-IDDISTR-TOM  TO UT1-IDDISTR-TOM                       
030400     MOVE URVA-URV1-KDTPOTYP-FOM TO UT1-KDTPOTYP-FOM                      
030500     MOVE URVA-URV1-KDTPOTYP-TOM TO UT1-KDTPOTYP-TOM                      
030600     MOVE URVA-URV1-TITPO-FOM    TO UT1-TITPO-FOM                         
030700     MOVE URVA-URV1-TITPO-TOM    TO UT1-TITPO-TOM                         
030800                                                                          
030900     PERFORM IMS-GNP-URVA-ART                                             
031000     MOVE +1 TO IX                                                        
031100     PERFORM UNTIL SEGMENT-SAKNAS OR IX > +100                            
031200       MOVE URVA-ART-IDARTNR    TO UT1-IDARTNR(IX)                        
031300       ADD +1 TO IX                                                       
031400       PERFORM IMS-GNP-URVA-ART                                           
031500     END-PERFORM                                                          
031600                                                                          
031700     PERFORM S11-SKRIV-W22530                                             
031800     PERFORM CA-NOLLA-UT1-POST                                            
031900     .                                                                    
032000     EJECT                                                                
032100 CA-NOLLA-UT1-POST SECTION.                                               
032200                                                                          
032300     MOVE SPACE  TO UT1-IDUSER                                            
032400     MOVE ZERO   TO UT1-TIREGDAT                                          
032500                    UT1-TIREGTID                                          
032600                    UT1-IDANSK-FOM                                        
032700                    UT1-IDANSK-TOM                                        
032800                    UT1-KDSORT1                                           
032900     MOVE SPACE  TO UT1-IDLEVNR                                           
033000     MOVE ZERO   TO UT1-KDPRODSL                                          
033100                    UT1-IDDISTR-FOM                                       
033200                    UT1-IDDISTR-TOM                                       
033300                    UT1-KDTPOTYP-FOM                                      
033400                    UT1-KDTPOTYP-TOM                                      
033500                    UT1-TITPO-FOM                                         
033600                    UT1-TITPO-TOM                                         
033700     MOVE +1 TO IX                                                        
033800     PERFORM UNTIL IX > +100                                              
033900       MOVE ZERO TO UT1-IDARTNR(IX)                                       
034000       ADD +1 TO IX                                                       
034100     END-PERFORM                                                          
034200     .                                                                    
034300     EJECT                                                                
034400 Z-FINIT SECTION.                                                         
034500                                                                          
034600     IF BMP                                                               
034700                                                                          
034800       CLOSE W22530                                                       
034900                                                                          
035000             W22531                                                       
035100     ELSE                                                                 
035200       CLOSE W22530                                                       
035300             W22531                                                       
035400     END-IF                                                               
035500     SKIP2                                                                
035600     MOVE 'S' TO POSTSUM-OPKOD                                            
035700     CALL POSTSUM USING POSTSUM-PARM                                      
035800     .                                                                    
035900     EJECT                                                                
036000 S11-SKRIV-W22530 SECTION.                                                
036100     SKIP2                                                                
036200     WRITE UT1-POST FROM UT1-AREA                                         
036300                                                                          
036400     MOVE 'W22530 ' TO POSTSUM-FDNAMN                                     
036500     MOVE 'W22530D1' TO POSTSUM-DDNAMN2                                   
036600     CALL POSTSUM USING POSTSUM-PARM                                      
036700     .                                                                    
036800     EJECT                                                                
036900 S12-SKRIV-W22531 SECTION.                                                
037000     SKIP2                                                                
037100     WRITE UT2-POST FROM UT2-AREA                                         
037200                                                                          
037300     MOVE 'W22531 ' TO POSTSUM-FDNAMN                                     
037400     MOVE 'W22530D2' TO POSTSUM-DDNAMN2                                   
037500     CALL POSTSUM USING POSTSUM-PARM                                      
037600     .                                                                    
037700     EJECT                                                                
037800* --- IMS-SEKTIONER ---                                                   
037900     SKIP3                                                                
038000     EJECT                                                                
038100 IMS-GHU-URVA-USER SECTION.                                               
038200     STRING 'WLURVA01(WDM401KY =' W-WDM401KY-X ')'                        
038300          DELIMITED BY SIZE INTO SSA1                                     
038400     MOVE '  GE' TO GODK-STATUSKODER                                      
038500     CALL CBLTDLI USING GHU URVA-PCB DLI-IO-AREA SSA1                     
038600     MOVE URVA-STATUS-CODE TO STATUS-WS                                   
038700     PERFORM IMS-STATUSKONTROLL                                           
038800     .                                                                    
038900     EJECT                                                                
039000 IMS-GN-URVA-USER  SECTION.                                               
039100     MOVE 'WLURVA01 ' TO SSA1                                             
039200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
039300     CALL CBLTDLI USING GN  URVA-PCB DLI-IO-AREA SSA1                     
039400     MOVE URVA-STATUS-CODE TO STATUS-WS                                   
039500     PERFORM IMS-STATUSKONTROLL                                           
039600     .                                                                    
039700     EJECT                                                                
039800 IMS-GNP-URVA-URV1 SECTION.                                               
039900     MOVE 'WLURVA11 ' TO SSA1                                             
040000     MOVE '  GE' TO GODK-STATUSKODER                                      
040100     CALL CBLTDLI USING GNP URVA-PCB DLI-IO-AREA SSA1                     
040200     MOVE URVA-STATUS-CODE TO STATUS-WS                                   
040300     PERFORM IMS-STATUSKONTROLL                                           
040400     .                                                                    
040500     EJECT                                                                
040600 IMS-GNP-URVA-ART SECTION.                                                
040700     MOVE 'WLURVA12 ' TO SSA1                                             
040800     MOVE '  GE' TO GODK-STATUSKODER                                      
040900     CALL CBLTDLI USING GNP URVA-PCB DLI-IO-AREA SSA1                     
041000     MOVE URVA-STATUS-CODE TO STATUS-WS                                   
041100     PERFORM IMS-STATUSKONTROLL                                           
041200     .                                                                    
041300     SKIP3                                                                
041400 IMS-DLET-URVA SECTION.                                                   
041500                                                                          
041600     MOVE '  ' TO GODK-STATUSKODER                                        
041700     CALL CBLTDLI USING DLET URVA-PCB DLI-IO-AREA                         
041800     MOVE URVA-STATUS-CODE TO STATUS-WS                                   
041900     PERFORM IMS-STATUSKONTROLL                                           
042000     .                                                                    
042100     EJECT                                                                
042200 IMS-STATUSKONTROLL SECTION.                                              
042300     SKIP2                                                                
042400     SET STATUS-IX TO 1                                                   
042500     SEARCH GODK-STATUS                                                   
042600       AT END CALL FELLOG                                                 
042700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
042800     END-SEARCH                                                           
042900     .                                                                    

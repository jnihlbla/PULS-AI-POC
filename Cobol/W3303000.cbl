000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.             W3303000.                                        
000400 AUTHOR.                 P.DAHLÖF.                                        
000500     DATE-WRITTEN.       DECEMBER 1988.                                   
000600*                                                                         
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*    IMS BMP-PROGRAM I ARTIKELSTATISTIKEN.                                
001100*    LÄSER WLFSGB-BASEN. OM FLAGGA = JA SKAPAS EN UT-FIL                  
001200*    OCH FLAGGGAN SÄTTS TILL NEJ.                                         
001210*    KOLLAR OCKSÅ OM URVAL ÄR STORSÄLJARE OCH FLER ÄN ETT                 
001220*    PRODUKTSLAG HAR VALTS SÅ SÄTTS KDSVAR TILL JA.                       
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700     SKIP2                                                                
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000     SELECT  W33031                   ASSIGN TO    W33030D1.              
002100     SELECT  W33035                   ASSIGN TO    W33030D2.              
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400     SKIP2                                                                
002500 FILE SECTION.                                                            
002600     SKIP2                                                                
002700 FD  W33031                                                               
002800     LABEL RECORD STANDARD                                                
002900     RECORDING      V                                                     
003000     BLOCK CONTAINS 0.                                                    
003100     SKIP2                                                                
003200*01  POST -COPY W3303103 -PRE 3103- -L                                    
003400     SKIP2                                                                
003500*01  POST -COPY W3303102 -PRE 3102- -L                                    
003700     SKIP2                                                                
003800*01  POST -COPY W3303104 -PRE 3104- -L                                    
004000     EJECT                                                                
004100 FD  W33035                                                               
004200     LABEL RECORD STANDARD                                                
004300     RECORDING      V                                                     
004400     BLOCK CONTAINS 0.                                                    
004500     SKIP2                                                                
004600*01  POST -COPY W3303503 -PRE 3503- -L                                    
004800     SKIP2                                                                
004900*01  POST -COPY W3303502 -PRE 3502- -L                                    
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300     SKIP2                                                                
005301                                                                          
005310*    -- CHECKED BY WY2000                                                 
005400 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3303000'.               
005500     SKIP2                                                                
005600*    ---- GENERELLA KONSTANTER ---------------------------------          
005700 77  RAD-IX                      PIC S9(9) VALUE +0 COMP SYNC.            
005800 77  ANTAL-IX                    PIC S9(9) VALUE +0 COMP SYNC.            
005810 77  KOLLA-PS                    PIC S9    VALUE +0 COMP SYNC.            
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100 01  NYCKLAR-TILL-DLI.                                                    
006200   03  FILLER                    PIC X(16)   VALUE                        
006300                                            'NYCKLAR-TILL-DLI'.           
006400   03  W-WDM301KY-MIN.                                                    
006500     05  FILLER                  PIC  X(20)  VALUE LOW-VALUE.             
006600   03  W-WDM301KY-MAX.                                                    
006700     05  FILLER                  PIC  X(20)  VALUE HIGH-VALUE.            
006800   03  W-KDSEGKEY-X.                                                      
006900     05  W-KDSEGKEY              PIC  X(1)   VALUE '1'.                   
007000   03  W-IDARTNR-X.                                                       
007100     05  FILLER                  PIC  X(5)   VALUE LOW-VALUE.             
007200   03  W-IDFKNGRP-X.                                                      
007300     05  FILLER                  PIC  X(6)   VALUE LOW-VALUE.             
007400*--------------------------------------------------------------*          
007500*    SUBPROGRAM OCH PARAMETERAREOR                                        
007600*--------------------------------------------------------------*          
007700     SKIP2                                                                
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007900   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
008000   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
008100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
008200     EJECT                                                                
008300*    ---- PARAMETRAR TILL POSTSUM                                         
008400 01  FILLER               PIC X(16)   VALUE  'POSTSUM-AREA'.              
008500*01  -COPY W0005 -PRE POSTSUM-.                                           
008700     EJECT                                                                
008800*--------------------------------------------------------------*          
008900*    W33038-AREA                                               *          
009000*--------------------------------------------------------------*          
009100 01  FILLER             PIC X(16)       VALUE 'W33031-AREA'.              
009200*01  AREA -COPY W3303102 -PRE 3102-                                       
009400     EJECT                                                                
009500*01  AREA -COPY W3303103 -PRE 3103-                                       
009700     EJECT                                                                
009800*01  AREA -COPY W3303104 -PRE 3104-                                       
010000     EJECT                                                                
010100*01  AREA -COPY W3303502 -PRE 3502-                                       
010300     EJECT                                                                
010400*01  AREA -COPY W3303503 -PRE 3503-                                       
010600     EJECT                                                                
010700* *********************ARBETSAREOR IMS                                    
010800 01  IMS-WS.                                                              
010900   03  FILLER                    PIC X(8)   VALUE 'IMS-WS'.               
011000   03  STATUS-WS                 PIC X(2).                                
011100     88  SEGMENT-FINNS                      VALUE '  '.                   
011200     88  SEGMENT-SAKNAS                     VALUE 'GE' 'GB'.              
011300   03  SSA1                      PIC X(64).                               
011400   03  SSA2                      PIC X(64).                               
011500   03  GODK-STATUSKODER.                                                  
011600     05  GODK-STATUS     OCCURS 5 INDEXED BY STATUS-IX PIC XX.            
011700*01  -COPY W0003                                                          
011800 ++INCLUDE W0003CCCC0                                                     
011900     EJECT                                                                
012000*--------------------------------------------------------------*          
012100*    DLI-IO-AREA / IO-AREOR 1, 2, 3 ,4                                    
012200*--------------------------------------------------------------*          
012300 01  DLI-IO-AREA.                                                         
012400   03 IO-AREA                    PIC X(200)   VALUE SPACE.                
012500     SKIP2                                                                
012600*  03  WLFSGA01 -COPY WDM301 -RED IO-AREA.                                
012800     EJECT                                                                
012900*  03  WLFSGA11 -COPY WDM311 -RED IO-AREA.                                
013100     EJECT                                                                
013200*  03  WLFSGA12 -COPY WDM312 -RED IO-AREA.                                
013400     EJECT                                                                
013500*  03  WLFSGA13 -COPY WDM313 -RED IO-AREA.                                
013700     EJECT                                                                
013800*  03  WLFSGA14 -COPY WDM314 -RED IO-AREA.                                
014000     EJECT                                                                
014100*  03  WLFSGA15 -COPY WDM315 -RED IO-AREA.                                
014300     EJECT                                                                
014400 LINKAGE SECTION.                                                         
014500* - - - - - - - - -LOGISK TERMINAL PCB-COPYTEXT FÖR BMP                   
014600*01      -COPY W0008     -PRE MSG-                                        
014800      05 FILLER          PIC X(4).                                        
014900     SKIP2                                                                
015000*01      -COPY W0008     -PRE FSGA-                                       
015200      05 FILLER          PIC X(4).                                        
015300     EJECT                                                                
015400 PROCEDURE DIVISION USING MSG-PCB FSGA-PCB.                               
015500     ENTRY 'DLITCBL' USING MSG-PCB FSGA-PCB.                              
015600     SKIP2                                                                
015700     PERFORM A-INIT                                                       
015800     MOVE +1                     TO ANTAL-IX                              
015900     PERFORM IMS-GHN-FSGA01                                               
016000     PERFORM UNTIL SEGMENT-SAKNAS OR ANTAL-IX > +100                      
016100        IF USER-FLLISTA = 'J'                                             
016200           MOVE 'N'              TO USER-FLLISTA                          
016300           PERFORM IMS-REPL-FSGA                                          
016400           PERFORM B-SKRIV-UTFIL                                          
016500           ADD  +1               TO ANTAL-IX                              
016600        ELSE                                                              
016700           IF USER-IDFSGURV = SPACE                                       
016800              PERFORM IMS-DLET-FSGA                                       
016900           END-IF                                                         
017000        END-IF                                                            
017100        PERFORM IMS-GHN-FSGA01                                            
017200     END-PERFORM                                                          
017300     PERFORM Z-FINIT                                                      
017400     MOVE ZERO                            TO RETURN-CODE                  
017500     GOBACK                                                               
017600     .                                                                    
017700     EJECT                                                                
017800 A-INIT   SECTION.                                                        
017900     SKIP2                                                                
018000     OPEN OUTPUT W33031                                                   
018100     OPEN OUTPUT W33035                                                   
018200     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
018300     .                                                                    
018400     EJECT                                                                
018500 B-SKRIV-UTFIL    SECTION.                                                
018600     SKIP2                                                                
018700     IF USER-IDTRANS = '3202'                                             
018800        PERFORM BA-LAES-11-12-SEGMENT                                     
018900     ELSE                                                                 
019000        IF USER-IDTRANS = '3203'                                          
019100           PERFORM BB-LAES-13-14-SEGMENT                                  
019200        ELSE                                                              
019300           IF USER-IDTRANS = '3204'                                       
019400              PERFORM BC-LAES-15-SEGMENT                                  
019500           END-IF                                                         
019600        END-IF                                                            
019700     END-IF                                                               
019800     .                                                                    
019900     EJECT                                                                
020000 BA-LAES-11-12-SEGMENT   SECTION.                                         
020100     SKIP2                                                                
020200     MOVE +4                        TO 3102-IDGTYP                        
020300     MOVE USER-IDUSER               TO 3102-IDUSER                        
020400     MOVE USER-DAREGDAT             TO 3102-DAREGDAT                      
020500     MOVE USER-TIREGTID             TO 3102-TIREGTID                      
020600     MOVE USER-IDFSGURV             TO 3102-IDFSGURV                      
020700     MOVE USER-IDTRANS              TO 3102-IDTRANS                       
020800     PERFORM IMS-GNP-FSGA11                                               
020900     IF SEGMENT-FINNS                                                     
021000        IF URV1-TIFSGVV-TOM = ZERO                                        
021100           PERFORM BAA-FLYTTA-T-3102-TEXT                                 
021200           PERFORM BAB-SKRIV-POST                                         
021300        ELSE                                                              
021400           PERFORM BAC-FLYTTA-T-3502-TEXT                                 
021500           PERFORM BAD-SKRIV-POST                                         
021600        END-IF                                                            
021700     END-IF                                                               
021800     .                                                                    
021900     EJECT                                                                
022000 BAA-FLYTTA-T-3102-TEXT  SECTION.                                         
022100     SKIP2                                                                
022200     MOVE URV1-IDPTYP               TO 3102-IDPTYP                        
022300     MOVE URV1-KDNIVA               TO 3102-KDNIVA                        
022310     MOVE URV1-KDSVAR               TO 3102-KDSVAR                        
022400     MOVE +1                        TO RAD-IX                             
022500     PERFORM UNTIL RAD-IX > +8                                            
022600        IF RAD-IX > +7                                                    
022700           MOVE URV1-IDLKTO    (RAD-IX) TO                                
022800                                     3102-IDLKTO    (RAD-IX)              
022900           MOVE URV1-IDLEVNR   (RAD-IX) TO                                
023000                                     3102-IDLEVNR   (RAD-IX)              
023100           MOVE URV1-IDKONCNR  (RAD-IX) TO                                
023200                                     3102-IDKONCNR  (RAD-IX)              
023300           MOVE URV1-KDMARK-FOM (RAD-IX) TO                               
023400                              3102-KDMARK-BUDG-FOM  (RAD-IX)              
023500           MOVE URV1-KDMARK-TOM (RAD-IX) TO                               
023600                              3102-KDMARK-BUDG-TOM  (RAD-IX)              
023700           IF URV1-IDKONCNR  (RAD-IX) > ZERO                              
023800              MOVE +2                   TO 3102-IDGTYP                    
023900           END-IF                                                         
024000        ELSE                                                              
024100           IF RAD-IX > +5                                                 
024200              MOVE URV1-IDLKTO (RAD-IX) TO                                
024300                                        3102-IDLKTO (RAD-IX)              
024400              MOVE URV1-IDLEVNR (RAD-IX) TO                               
024500                                        3102-IDLEVNR (RAD-IX)             
024600              MOVE URV1-IDKONCNR (RAD-IX) TO                              
024700                                        3102-IDKONCNR (RAD-IX)            
024800              MOVE URV1-KDPRODSL (RAD-IX) TO                              
024900                                        3102-KDPRODSL (RAD-IX)            
025000              MOVE URV1-KDMARK-FOM (RAD-IX) TO                            
025100                                3102-KDMARK-BUDG-FOM  (RAD-IX)            
025200              MOVE URV1-KDMARK-TOM (RAD-IX) TO                            
025300                                3102-KDMARK-BUDG-TOM  (RAD-IX)            
025400              IF URV1-IDKONCNR (RAD-IX) > ZERO                            
025500                 MOVE +2                TO 3102-IDGTYP                    
025600              END-IF                                                      
025700           ELSE                                                           
025800              IF RAD-IX > +4                                              
025900                 MOVE URV1-IDLKTO (RAD-IX) TO                             
026000                                        3102-IDLKTO (RAD-IX)              
026100                 MOVE URV1-IDLEVNR (RAD-IX) TO                            
026200                                        3102-IDLEVNR (RAD-IX)             
026300                 MOVE URV1-IDKONCNR (RAD-IX) TO                           
026400                                        3102-IDKONCNR (RAD-IX)            
026500                 MOVE URV1-KDPRODSL (RAD-IX) TO                           
026600                                        3102-KDPRODSL (RAD-IX)            
026700                 MOVE URV1-KDVVKL (RAD-IX) TO                             
026800                                        3102-KDVVKL (RAD-IX)              
026900                 MOVE URV1-KDMARK-FOM (RAD-IX) TO                         
027000                              3102-KDMARK-BUDG-FOM  (RAD-IX)              
027100                 MOVE URV1-KDMARK-TOM (RAD-IX) TO                         
027200                              3102-KDMARK-BUDG-TOM  (RAD-IX)              
027300                 IF URV1-IDKONCNR (RAD-IX) > ZERO                         
027400                    MOVE +2                TO 3102-IDGTYP                 
027500                 END-IF                                                   
027600              ELSE                                                        
027700                 PERFORM BAAA-FLYTTA-TILL-CTEXT                           
027800              END-IF                                                      
027900           END-IF                                                         
028000        END-IF                                                            
028100        ADD +1                      TO RAD-IX                             
028200     END-PERFORM                                                          
028300     PERFORM IMS-GNP-FSGA12                                               
028400     MOVE   +1                      TO RAD-IX                             
028500     PERFORM UNTIL RAD-IX > +99                                           
028600        IF SEGMENT-FINNS                                                  
028700           MOVE URV1-IDFKNGRP-FOM    TO                                   
028800                              3102-IDFKNGRP-FOM (RAD-IX)                  
028900           MOVE URV1-IDFKNGRP-TOM    TO                                   
029000                              3102-IDFKNGRP-TOM (RAD-IX)                  
029100           PERFORM IMS-GNP-FSGA12                                         
029200        ELSE                                                              
029300           MOVE ZERO                 TO                                   
029400                              3102-IDFKNGRP-FOM (RAD-IX)                  
029500                              3102-IDFKNGRP-TOM (RAD-IX)                  
029600        END-IF                                                            
029700        ADD +1                      TO RAD-IX                             
029800     END-PERFORM                                                          
029900     .                                                                    
030000     EJECT                                                                
030100 BAAA-FLYTTA-TILL-CTEXT  SECTION.                                         
030200     SKIP2                                                                
030300     MOVE URV1-IDKONCNR (RAD-IX)    TO                                    
030400                                     3102-IDKONCNR  (RAD-IX)              
030500     MOVE URV1-KDMARK-FOM  (RAD-IX) TO                                    
030600                              3102-KDMARK-BUDG-FOM  (RAD-IX)              
030700     MOVE URV1-KDMARK-TOM  (RAD-IX) TO                                    
030800                              3102-KDMARK-BUDG-TOM  (RAD-IX)              
030900     MOVE URV1-IDDISTR-FOM (RAD-IX) TO                                    
031000                                  3102-IDDISTR-FOM  (RAD-IX)              
031100     MOVE URV1-IDDISTR-TOM (RAD-IX) TO                                    
031200                                  3102-IDDISTR-TOM  (RAD-IX)              
031300     MOVE URV1-KDPRODSL  (RAD-IX) TO 3102-KDPRODSL  (RAD-IX)              
031400     MOVE URV1-KDVVKL    (RAD-IX) TO 3102-KDVVKL    (RAD-IX)              
031500     MOVE URV1-IDLKTO    (RAD-IX) TO 3102-IDLKTO    (RAD-IX)              
031600     MOVE URV1-IDLEVNR   (RAD-IX) TO 3102-IDLEVNR   (RAD-IX)              
031700     MOVE URV1-IDANSK-FOM (RAD-IX) TO                                     
031800                                   3102-IDANSK-FOM  (RAD-IX)              
031900     MOVE URV1-IDANSK-TOM (RAD-IX) TO                                     
032000                                   3102-IDANSK-TOM  (RAD-IX)              
032100     IF URV1-IDDISTR-TOM   (RAD-IX) > ZERO                                
032200        MOVE +1                      TO 3102-IDGTYP                       
032300     ELSE                                                                 
032400        IF URV1-IDKONCNR  (RAD-IX) > ZERO                                 
032500           MOVE +2                   TO 3102-IDGTYP                       
032600        ELSE                                                              
032700           IF URV1-KDMARK-TOM(RAD-IX) > ZERO OR                           
032710              URV1-IDPTYP = 'PLV' OR 'PPV'                                
032800              MOVE +3                TO 3102-IDGTYP                       
032900           END-IF                                                         
033000        END-IF                                                            
033100     END-IF                                                               
033200     .                                                                    
033300     EJECT                                                                
033400 BAB-SKRIV-POST  SECTION.                                                 
033500     SKIP2                                                                
033600     WRITE 3102-POST FROM 3102-AREA.                                      
033700     MOVE SPACE TO POSTSUM-TRANSTYP                                       
033800     MOVE 'W33031' TO POSTSUM-FDNAMN                                      
033900     MOVE 'W33030D1' TO POSTSUM-DDNAMN2                                   
034000     CALL POSTSUM USING POSTSUM-PARM                                      
034100                                                                          
034200     DISPLAY 3102-IDUSER                                                  
034300     DISPLAY 3102-IDPTYP                                                  
034400     DISPLAY 3102-DAREGDAT                                                
034500     DISPLAY 3102-TIREGTID                                                
034600     DISPLAY 3102-IDFSGURV                                                
034700     DISPLAY 3102-IDTRANS                                                 
034800     DISPLAY '-----------'                                                
034900     .                                                                    
035000     EJECT                                                                
035100 BAC-FLYTTA-T-3502-TEXT  SECTION.                                         
035200     SKIP2                                                                
035300     MOVE 3102-IDGTYP               TO 3502-IDGTYP                        
035400     MOVE 3102-IDUSER               TO 3502-IDUSER                        
035500     MOVE 3102-DAREGDAT             TO 3502-DAREGDAT                      
035600     MOVE 3102-TIREGTID             TO 3502-TIREGTID                      
035700     MOVE 3102-IDFSGURV             TO 3502-IDFSGURV                      
035800     MOVE 3102-IDTRANS              TO 3502-IDTRANS                       
035900     MOVE URV1-IDPTYP               TO 3502-IDPTYP                        
036000     MOVE URV1-KDNIVA               TO 3502-KDNIVA                        
036100     MOVE URV1-TIFSGVV-FOM          TO 3502-DAFSGVV-FOM                   
036110     IF 3502-DAFSGVV-FOM > 7000                                           
036120        ADD              190000     TO 3502-DAFSGVV-FOM                   
036130     ELSE                                                                 
036140        ADD              200000     TO 3502-DAFSGVV-FOM                   
036150     END-IF                                                               
036200     MOVE URV1-TIFSGVV-TOM          TO 3502-DAFSGVV-TOM                   
036210     IF 3502-DAFSGVV-TOM > 7000                                           
036220        ADD              190000     TO 3502-DAFSGVV-TOM                   
036230     ELSE                                                                 
036240        ADD              200000     TO 3502-DAFSGVV-TOM                   
036250     END-IF                                                               
036300     MOVE URV1-KDPRTYP              TO 3502-KDPRTYPG                      
036310     MOVE URV1-KDSVAR               TO 3502-KDSVAR                        
036400     MOVE +1                        TO RAD-IX                             
036500     PERFORM UNTIL RAD-IX > +8                                            
036600        IF RAD-IX > +7                                                    
036700           MOVE URV1-IDLKTO    (RAD-IX) TO                                
036800                                     3502-IDLKTO    (RAD-IX)              
036900           MOVE URV1-IDLEVNR   (RAD-IX) TO                                
037000                                     3502-IDLEVNR   (RAD-IX)              
037100           MOVE URV1-IDKONCNR  (RAD-IX) TO                                
037200                                     3502-IDKONCNR  (RAD-IX)              
037300           MOVE URV1-KDMARK-FOM  (RAD-IX) TO                              
037400                              3502-KDMARK-BUDG-FOM  (RAD-IX)              
037500           MOVE URV1-KDMARK-TOM  (RAD-IX) TO                              
037600                              3502-KDMARK-BUDG-TOM  (RAD-IX)              
037700           IF URV1-IDKONCNR  (RAD-IX) > ZERO                              
037800              MOVE +2                   TO 3502-IDGTYP                    
037900           END-IF                                                         
038000        ELSE                                                              
038100           IF RAD-IX > +5                                                 
038200              MOVE URV1-IDLKTO (RAD-IX) TO                                
038300                                        3502-IDLKTO (RAD-IX)              
038400              MOVE URV1-IDLEVNR (RAD-IX) TO                               
038500                                        3502-IDLEVNR (RAD-IX)             
038600              MOVE URV1-IDKONCNR (RAD-IX) TO                              
038700                                        3502-IDKONCNR (RAD-IX)            
038800              MOVE URV1-KDPRODSL (RAD-IX) TO                              
038900                                        3502-KDPRODSL (RAD-IX)            
039000              MOVE URV1-KDMARK-FOM  (RAD-IX) TO                           
039100                                3502-KDMARK-BUDG-FOM  (RAD-IX)            
039200              MOVE URV1-KDMARK-TOM  (RAD-IX) TO                           
039300                                3502-KDMARK-BUDG-TOM  (RAD-IX)            
039400              IF URV1-IDKONCNR (RAD-IX) > ZERO                            
039500                 MOVE +2                TO 3502-IDGTYP                    
039600              END-IF                                                      
039700           ELSE                                                           
039800              IF RAD-IX > +4                                              
039900                 MOVE URV1-IDLKTO (RAD-IX) TO                             
040000                                        3502-IDLKTO (RAD-IX)              
040100                 MOVE URV1-IDLEVNR (RAD-IX) TO                            
040200                                        3502-IDLEVNR (RAD-IX)             
040300                 MOVE URV1-IDKONCNR (RAD-IX) TO                           
040400                                        3502-IDKONCNR (RAD-IX)            
040500                 MOVE URV1-KDPRODSL (RAD-IX) TO                           
040600                                        3502-KDPRODSL (RAD-IX)            
040700                 MOVE URV1-KDVVKL (RAD-IX) TO                             
040800                                        3502-KDVVKL (RAD-IX)              
040900                 MOVE URV1-KDMARK-FOM  (RAD-IX) TO                        
041000                              3502-KDMARK-BUDG-FOM  (RAD-IX)              
041100                 MOVE URV1-KDMARK-TOM  (RAD-IX) TO                        
041200                              3502-KDMARK-BUDG-TOM  (RAD-IX)              
041300                 IF URV1-IDKONCNR (RAD-IX) > ZERO                         
041400                    MOVE +2                TO 3502-IDGTYP                 
041500                 END-IF                                                   
041600              ELSE                                                        
041700                 PERFORM BACA-FLYTTA-TILL-CTEXT                           
041800              END-IF                                                      
041900           END-IF                                                         
042000        END-IF                                                            
042100        ADD +1                      TO RAD-IX                             
042200     END-PERFORM                                                          
042300     PERFORM IMS-GNP-FSGA12                                               
042400     MOVE   +1                      TO RAD-IX                             
042500     PERFORM UNTIL RAD-IX > +99                                           
042600        IF SEGMENT-FINNS                                                  
042700           MOVE URV1-IDFKNGRP-FOM    TO                                   
042800                              3502-IDFKNGRP-FOM (RAD-IX)                  
042900           MOVE URV1-IDFKNGRP-TOM    TO                                   
043000                              3502-IDFKNGRP-TOM (RAD-IX)                  
043100           PERFORM IMS-GNP-FSGA12                                         
043200        ELSE                                                              
043300           MOVE ZERO                 TO                                   
043400                              3502-IDFKNGRP-FOM (RAD-IX)                  
043500                              3502-IDFKNGRP-TOM (RAD-IX)                  
043600        END-IF                                                            
043700        ADD +1                      TO RAD-IX                             
043800     END-PERFORM                                                          
043900     .                                                                    
044000     EJECT                                                                
044100 BACA-FLYTTA-TILL-CTEXT  SECTION.                                         
044200     SKIP2                                                                
044300     MOVE URV1-IDKONCNR (RAD-IX)    TO                                    
044400                                     3502-IDKONCNR  (RAD-IX)              
044500     MOVE URV1-KDMARK-FOM  (RAD-IX) TO                                    
044600                              3502-KDMARK-BUDG-FOM  (RAD-IX)              
044700     MOVE URV1-KDMARK-TOM  (RAD-IX) TO                                    
044800                              3502-KDMARK-BUDG-TOM  (RAD-IX)              
044900     MOVE URV1-IDDISTR-FOM (RAD-IX) TO                                    
045000                                  3502-IDDISTR-FOM  (RAD-IX)              
045100     MOVE URV1-IDDISTR-TOM (RAD-IX) TO                                    
045200                                  3502-IDDISTR-TOM  (RAD-IX)              
045300     MOVE URV1-KDPRODSL  (RAD-IX) TO 3502-KDPRODSL  (RAD-IX)              
045400     MOVE URV1-KDVVKL    (RAD-IX) TO 3502-KDVVKL    (RAD-IX)              
045500     MOVE URV1-IDLKTO    (RAD-IX) TO 3502-IDLKTO    (RAD-IX)              
045600     MOVE URV1-IDLEVNR   (RAD-IX) TO 3502-IDLEVNR   (RAD-IX)              
045700     MOVE URV1-IDANSK-FOM (RAD-IX) TO                                     
045800                                   3502-IDANSK-FOM  (RAD-IX)              
045900     MOVE URV1-IDANSK-TOM (RAD-IX) TO                                     
046000                                   3502-IDANSK-TOM  (RAD-IX)              
046100     IF URV1-IDDISTR-TOM   (RAD-IX) > ZERO                                
046200        MOVE +1                    TO 3502-IDGTYP                         
046300     ELSE                                                                 
046400        IF URV1-IDKONCNR  (RAD-IX) > ZERO                                 
046500           MOVE +2                 TO 3502-IDGTYP                         
046600        ELSE                                                              
046700           IF URV1-KDMARK-TOM    (RAD-IX) > ZERO                          
046800              MOVE +3              TO 3502-IDGTYP                         
046900           END-IF                                                         
047000        END-IF                                                            
047100     END-IF                                                               
047200     .                                                                    
047300     EJECT                                                                
047400 BAD-SKRIV-POST  SECTION.                                                 
047500     SKIP2                                                                
047600     WRITE 3502-POST FROM 3502-AREA.                                      
047700     MOVE SPACE TO POSTSUM-TRANSTYP                                       
047800     MOVE 'W33035' TO POSTSUM-FDNAMN                                      
047900     MOVE 'W33030D2' TO POSTSUM-DDNAMN2                                   
048000     CALL POSTSUM USING POSTSUM-PARM                                      
048100                                                                          
048200     DISPLAY 3502-IDUSER                                                  
048300     DISPLAY 3502-IDPTYP                                                  
048400     DISPLAY 3502-DAREGDAT                                                
048500     DISPLAY 3502-TIREGTID                                                
048600     DISPLAY 3502-IDFSGURV                                                
048700     DISPLAY 3502-IDTRANS                                                 
048800     DISPLAY '-----------'                                                
048900     .                                                                    
049000     EJECT                                                                
049100 BB-LAES-13-14-SEGMENT   SECTION.                                         
049200     SKIP2                                                                
049300     MOVE +4                        TO 3103-IDGTYP                        
049400     MOVE USER-IDUSER               TO 3103-IDUSER                        
049500     MOVE USER-DAREGDAT             TO 3103-DAREGDAT                      
049600     MOVE USER-TIREGTID             TO 3103-TIREGTID                      
049700     MOVE USER-IDFSGURV             TO 3103-IDFSGURV                      
049800     MOVE USER-IDTRANS              TO 3103-IDTRANS                       
049900     PERFORM IMS-GNP-FSGA13                                               
050000     IF SEGMENT-FINNS                                                     
050100        IF URV2-TIFSGVV-TOM = ZERO                                        
050200           PERFORM BBA-FLYTTA-T-3103-TEXT                                 
050300           PERFORM BBB-SKRIV-POST                                         
050400        ELSE                                                              
050500           PERFORM BBC-FLYTTA-T-3503-TEXT                                 
050600           PERFORM BBD-SKRIV-POST                                         
050700        END-IF                                                            
050800     END-IF                                                               
050900     .                                                                    
051000     EJECT                                                                
051100 BBA-FLYTTA-T-3103-TEXT  SECTION.                                         
051200     SKIP2                                                                
051300     MOVE URV2-IDPTYP               TO 3103-IDPTYP                        
051400     MOVE +1                        TO RAD-IX                             
051500     PERFORM UNTIL RAD-IX > +8                                            
051600        IF RAD-IX > +4                                                    
051700           MOVE URV2-IDKONCNR  (RAD-IX)   TO                              
051800                                     3103-IDKONCNR  (RAD-IX)              
051900           MOVE URV2-KDMARK-BUDG-FOM (RAD-IX) TO                          
052000                              3103-KDMARK-BUDG-FOM  (RAD-IX)              
052100           MOVE URV2-KDMARK-BUDG-TOM (RAD-IX) TO                          
052200                              3103-KDMARK-BUDG-TOM  (RAD-IX)              
052300           IF URV2-IDKONCNR  (RAD-IX) > ZERO                              
052400              MOVE +2                     TO 3103-IDGTYP                  
052500           END-IF                                                         
052600        ELSE                                                              
052700           MOVE URV2-IDKONCNR (RAD-IX)    TO                              
052800                                     3103-IDKONCNR  (RAD-IX)              
052900           MOVE URV2-KDMARK-BUDG-FOM (RAD-IX) TO                          
053000                              3103-KDMARK-BUDG-FOM  (RAD-IX)              
053100           MOVE URV2-KDMARK-BUDG-TOM (RAD-IX) TO                          
053200                              3103-KDMARK-BUDG-TOM  (RAD-IX)              
053300           MOVE URV2-IDDISTR-FOM (RAD-IX) TO                              
053400                                  3103-IDDISTR-FOM  (RAD-IX)              
053500           MOVE URV2-IDDISTR-TOM (RAD-IX) TO                              
053600                                  3103-IDDISTR-TOM  (RAD-IX)              
053700           IF URV2-IDDISTR-TOM   (RAD-IX) > ZERO                          
053800              MOVE +1                     TO 3103-IDGTYP                  
053900           ELSE                                                           
054000              IF URV2-IDKONCNR  (RAD-IX) > ZERO                           
054100                 MOVE +2                  TO 3103-IDGTYP                  
054200              ELSE                                                        
054300                 IF URV2-KDMARK-BUDG-TOM  (RAD-IX) > ZERO OR              
054310                    URV2-IDPTYP = 'PLV' OR 'PPV'                          
054400                    MOVE +3               TO 3103-IDGTYP                  
054500                 END-IF                                                   
054600              END-IF                                                      
054700           END-IF                                                         
054800        END-IF                                                            
054900        ADD +1                      TO RAD-IX                             
055000     END-PERFORM                                                          
055100     PERFORM IMS-GNP-FSGA14                                               
055200     MOVE   +1                      TO RAD-IX                             
055300     PERFORM UNTIL RAD-IX > +500                                          
055400        IF SEGMENT-FINNS                                                  
055500           MOVE ART-IDARTNR          TO                                   
055600                              3103-IDARTNR (RAD-IX)                       
055700           PERFORM IMS-GNP-FSGA14                                         
055800        ELSE                                                              
055900           MOVE ZERO                 TO                                   
056000                              3103-IDARTNR (RAD-IX)                       
056100        END-IF                                                            
056200        ADD +1                      TO RAD-IX                             
056300     END-PERFORM                                                          
056400     .                                                                    
056500     EJECT                                                                
056600 BBB-SKRIV-POST  SECTION.                                                 
056700     SKIP2                                                                
056800     WRITE 3103-POST FROM 3103-AREA.                                      
056900     MOVE SPACE TO POSTSUM-TRANSTYP                                       
057000     MOVE 'W33031' TO POSTSUM-FDNAMN                                      
057100     MOVE 'W33030D1' TO POSTSUM-DDNAMN2                                   
057200     CALL POSTSUM USING POSTSUM-PARM                                      
057300                                                                          
057400     DISPLAY 3103-IDUSER                                                  
057500     DISPLAY 3103-IDPTYP                                                  
057600     DISPLAY 3103-DAREGDAT                                                
057700     DISPLAY 3103-TIREGTID                                                
057800     DISPLAY 3103-IDFSGURV                                                
057900     DISPLAY 3103-IDTRANS                                                 
058000     DISPLAY '-----------'                                                
058100     .                                                                    
058200     EJECT                                                                
058300 BBC-FLYTTA-T-3503-TEXT  SECTION.                                         
058400     SKIP2                                                                
058500     MOVE 3103-IDGTYP               TO 3503-IDGTYP                        
058600     MOVE 3103-IDUSER               TO 3503-IDUSER                        
058700     MOVE 3103-DAREGDAT             TO 3503-DAREGDAT                      
058800     MOVE 3103-TIREGTID             TO 3503-TIREGTID                      
058900     MOVE 3103-IDFSGURV             TO 3503-IDFSGURV                      
059000     MOVE 3103-IDTRANS              TO 3503-IDTRANS                       
059100     MOVE URV2-IDPTYP               TO 3503-IDPTYP                        
059200     MOVE URV2-TIFSGVV-FOM          TO 3503-DAFSGVV-FOM                   
059210     IF 3503-DAFSGVV-FOM > 7000                                           
059220        ADD              190000     TO 3503-DAFSGVV-FOM                   
059230     ELSE                                                                 
059240        ADD              200000     TO 3503-DAFSGVV-FOM                   
059250     END-IF                                                               
059300     MOVE URV2-TIFSGVV-TOM          TO 3503-DAFSGVV-TOM                   
059310     IF 3503-DAFSGVV-TOM > 7000                                           
059320        ADD              190000     TO 3503-DAFSGVV-TOM                   
059330     ELSE                                                                 
059340        ADD              200000     TO 3503-DAFSGVV-TOM                   
059350     END-IF                                                               
059400     MOVE URV2-KDPRTYP              TO 3503-KDPRTYPG                      
059500     MOVE +1                        TO RAD-IX                             
059600     PERFORM UNTIL RAD-IX > +8                                            
059700        IF RAD-IX > +4                                                    
059800           MOVE URV2-IDKONCNR  (RAD-IX) TO                                
059900                                     3503-IDKONCNR  (RAD-IX)              
060000           MOVE URV2-KDMARK-BUDG-FOM (RAD-IX) TO                          
060100                              3503-KDMARK-BUDG-FOM  (RAD-IX)              
060200           MOVE URV2-KDMARK-BUDG-TOM (RAD-IX) TO                          
060300                              3503-KDMARK-BUDG-TOM  (RAD-IX)              
060400           IF URV2-IDKONCNR  (RAD-IX) > ZERO                              
060500              MOVE +2                     TO 3503-IDGTYP                  
060600           END-IF                                                         
060700        ELSE                                                              
060800           MOVE URV2-IDKONCNR (RAD-IX)    TO                              
060900                                     3503-IDKONCNR  (RAD-IX)              
061000           MOVE URV2-KDMARK-BUDG-FOM (RAD-IX) TO                          
061100                              3503-KDMARK-BUDG-FOM  (RAD-IX)              
061200           MOVE URV2-KDMARK-BUDG-TOM (RAD-IX) TO                          
061300                              3503-KDMARK-BUDG-TOM  (RAD-IX)              
061400           MOVE URV2-IDDISTR-FOM (RAD-IX) TO                              
061500                                  3503-IDDISTR-FOM  (RAD-IX)              
061600           MOVE URV2-IDDISTR-TOM (RAD-IX) TO                              
061700                                  3503-IDDISTR-TOM  (RAD-IX)              
061800           IF URV2-IDDISTR-TOM   (RAD-IX) > ZERO                          
061900              MOVE +1                     TO 3503-IDGTYP                  
062000           ELSE                                                           
062100              IF URV2-IDKONCNR  (RAD-IX) > ZERO                           
062200                 MOVE +2                  TO 3503-IDGTYP                  
062300              ELSE                                                        
062400                 IF URV2-KDMARK-BUDG-TOM (RAD-IX) > ZERO                  
062500                    MOVE +3               TO 3503-IDGTYP                  
062600                 END-IF                                                   
062700              END-IF                                                      
062800           END-IF                                                         
062900        END-IF                                                            
063000        ADD +1                            TO RAD-IX                       
063100     END-PERFORM                                                          
063200     PERFORM IMS-GNP-FSGA14                                               
063300     MOVE   +1                            TO RAD-IX                       
063400     PERFORM UNTIL RAD-IX > +500                                          
063500        IF SEGMENT-FINNS                                                  
063600           MOVE ART-IDARTNR          TO                                   
063700                              3503-IDARTNR (RAD-IX)                       
063800           PERFORM IMS-GNP-FSGA14                                         
063900        ELSE                                                              
064000           MOVE ZERO                 TO                                   
064100                              3503-IDARTNR (RAD-IX)                       
064200        END-IF                                                            
064300        ADD +1                      TO RAD-IX                             
064400     END-PERFORM                                                          
064500     .                                                                    
064600     EJECT                                                                
064700 BBD-SKRIV-POST  SECTION.                                                 
064800     SKIP2                                                                
064900     WRITE 3503-POST FROM 3503-AREA.                                      
065000     MOVE SPACE TO POSTSUM-TRANSTYP                                       
065100     MOVE 'W33035' TO POSTSUM-FDNAMN                                      
065200     MOVE 'W33030D2' TO POSTSUM-DDNAMN2                                   
065300     CALL POSTSUM USING POSTSUM-PARM                                      
065400                                                                          
065500     DISPLAY 3503-IDUSER                                                  
065600     DISPLAY 3503-IDPTYP                                                  
065700     DISPLAY 3503-DAREGDAT                                                
065800     DISPLAY 3503-TIREGTID                                                
065900     DISPLAY 3503-IDFSGURV                                                
066000     DISPLAY 3503-IDTRANS                                                 
066100     DISPLAY '-----------'                                                
066200     .                                                                    
066300     EJECT                                                                
066400 BC-LAES-15-SEGMENT   SECTION.                                            
066500     SKIP2                                                                
066600     MOVE +4                        TO 3104-IDGTYP                        
066700     MOVE USER-IDUSER               TO 3104-IDUSER                        
066800     MOVE USER-DAREGDAT             TO 3104-DAREGDAT                      
066900     MOVE USER-TIREGTID             TO 3104-TIREGTID                      
067000     MOVE USER-IDFSGURV             TO 3104-IDFSGURV                      
067100     MOVE USER-IDTRANS              TO 3104-IDTRANS                       
067200     PERFORM IMS-GNP-FSGA15                                               
067300     IF SEGMENT-FINNS                                                     
067400        PERFORM BCA-FLYTTA-T-3104-TEXT                                    
067410        PERFORM BCB-KOLLA-SAMMANSL-PS                                     
067500        PERFORM BCC-SKRIV-POST                                            
067600     END-IF                                                               
067700     .                                                                    
067800     EJECT                                                                
067900 BCA-FLYTTA-T-3104-TEXT  SECTION.                                         
068000     SKIP2                                                                
068100     MOVE TOP-IDPTYP                TO 3104-IDPTYP                        
068200     MOVE TOP-KVART                 TO 3104-KVART                         
068400     MOVE TOP-IDFKNGRP-FOM          TO 3104-IDFKNGRP-FOM                  
068500     MOVE TOP-IDFKNGRP-TOM          TO 3104-IDFKNGRP-TOM                  
068600     MOVE +1                        TO RAD-IX                             
068700     PERFORM UNTIL RAD-IX > +8                                            
068800        IF RAD-IX > +4                                                    
068900           MOVE TOP-IDKONCNR   (RAD-IX) TO                                
069000                                     3104-IDKONCNR  (RAD-IX)              
069100           MOVE TOP-IDLEVNR    (RAD-IX) TO                                
069200                                     3104-IDLEVNR   (RAD-IX)              
069300           IF TOP-IDKONCNR  (RAD-IX) > ZERO                               
069400              MOVE +2                   TO 3104-IDGTYP                    
069500           END-IF                                                         
069600        ELSE                                                              
069700           MOVE TOP-KDPRODSL (RAD-IX)     TO                              
069800                                     3104-KDPRODSL  (RAD-IX)              
069810           MOVE TOP-IDKONCNR (RAD-IX)     TO                              
069820                                     3104-IDKONCNR  (RAD-IX)              
069900           MOVE TOP-IDLEVNR  (RAD-IX)     TO                              
070000                                     3104-IDLEVNR   (RAD-IX)              
070100           MOVE TOP-KDMARK-FOM   (RAD-IX) TO                              
070200                              3104-KDMARK-BUDG-FOM  (RAD-IX)              
070300           MOVE TOP-KDMARK-TOM   (RAD-IX) TO                              
070400                              3104-KDMARK-BUDG-TOM  (RAD-IX)              
070500           MOVE TOP-IDDISTR-FOM  (RAD-IX) TO                              
070600                                  3104-IDDISTR-FOM  (RAD-IX)              
070700           MOVE TOP-IDDISTR-TOM  (RAD-IX) TO                              
070800                                  3104-IDDISTR-TOM  (RAD-IX)              
070810           MOVE TOP-IDANSK-FOM   (RAD-IX) TO                              
070820                                  3104-IDANSK-FOM   (RAD-IX)              
070830           MOVE TOP-IDANSK-TOM   (RAD-IX) TO                              
070840                                  3104-IDANSK-TOM   (RAD-IX)              
070900           IF TOP-IDDISTR-TOM   (RAD-IX) > ZERO                           
071000              MOVE +1             TO 3104-IDGTYP                          
071100           ELSE                                                           
071200              IF TOP-IDKONCNR   (RAD-IX) > ZERO                           
071300                 MOVE +2          TO 3104-IDGTYP                          
071400              ELSE                                                        
071500                 IF TOP-KDMARK-TOM   (RAD-IX) > ZERO                      
071600                    MOVE +3       TO 3104-IDGTYP                          
071700                 END-IF                                                   
071800              END-IF                                                      
071900           END-IF                                                         
072000        END-IF                                                            
072100        ADD +1                           TO RAD-IX                        
072200     END-PERFORM                                                          
072300     .                                                                    
072400     EJECT                                                                
072410 BCB-KOLLA-SAMMANSL-PS SECTION.                                           
072420     SKIP2                                                                
072421     MOVE ZERO TO KOLLA-PS                                                
072422     IF TOP-KDPRODSL(1) > 0                                               
072423       ADD +1 TO KOLLA-PS                                                 
072424     END-IF                                                               
072425     IF TOP-KDPRODSL(2) > 0                                               
072426       ADD +1 TO KOLLA-PS                                                 
072427     END-IF                                                               
072428     IF TOP-KDPRODSL(3) > 0                                               
072429       ADD +1 TO KOLLA-PS                                                 
072430     END-IF                                                               
072431     IF TOP-KDPRODSL(4) > 0                                               
072432       ADD +1 TO KOLLA-PS                                                 
072433     END-IF                                                               
072434     IF  KOLLA-PS >  +1                                                   
072435       MOVE JA TO 3104-KDSVAR                                             
072436     END-IF                                                               
072437     MOVE ZERO TO KOLLA-PS                                                
072438     .                                                                    
072440     EJECT                                                                
072500 BCC-SKRIV-POST  SECTION.                                                 
072600     SKIP2                                                                
072700     WRITE 3104-POST FROM 3104-AREA.                                      
072800     MOVE SPACE TO POSTSUM-TRANSTYP                                       
072900     MOVE 'W33031' TO POSTSUM-FDNAMN                                      
073000     MOVE 'W33030D1' TO POSTSUM-DDNAMN2                                   
073100     CALL POSTSUM USING POSTSUM-PARM                                      
073200                                                                          
073300     DISPLAY 3104-IDUSER                                                  
073400     DISPLAY 3104-IDPTYP                                                  
073500     DISPLAY 3104-DAREGDAT                                                
073600     DISPLAY 3104-TIREGTID                                                
073700     DISPLAY 3104-IDFSGURV                                                
073800     DISPLAY 3104-IDTRANS                                                 
073900     DISPLAY '-----------'                                                
074000     .                                                                    
074100     EJECT                                                                
074200 Z-FINIT SECTION.                                                         
074300     SKIP2                                                                
074400     CLOSE   W33031                                                       
074500     CLOSE   W33035                                                       
074600     MOVE 'S' TO POSTSUM-OPKOD                                            
074700     CALL POSTSUM USING POSTSUM-PARM                                      
074800     .                                                                    
074900     EJECT                                                                
075000 IMS-GHN-FSGA01 SECTION.                                                  
075100     SKIP1                                                                
075200     MOVE   'WLFSGA01 '        TO SSA1                                    
075300     MOVE '  GB' TO GODK-STATUSKODER                                      
075400     CALL CBLTDLI USING GHN FSGA-PCB DLI-IO-AREA SSA1                     
075500     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
075600     PERFORM IMS-STATUSKONTROLL                                           
075700     .                                                                    
075800     SKIP3                                                                
075900 IMS-REPL-FSGA SECTION.                                                   
076000     SKIP1                                                                
076100     MOVE '  ' TO GODK-STATUSKODER                                        
076200     CALL CBLTDLI USING REPL FSGA-PCB DLI-IO-AREA                         
076300     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
076400     PERFORM IMS-STATUSKONTROLL                                           
076500     .                                                                    
076600     SKIP3                                                                
076700 IMS-GNP-FSGA11 SECTION.                                                  
076800     SKIP1                                                                
076900     MOVE   'WLFSGA11 '        TO SSA1                                    
077000     MOVE '  GE' TO GODK-STATUSKODER                                      
077100     CALL CBLTDLI USING GNP FSGA-PCB DLI-IO-AREA SSA1                     
077200     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
077300     PERFORM IMS-STATUSKONTROLL                                           
077400     .                                                                    
077500     SKIP3                                                                
077600 IMS-GNP-FSGA12 SECTION.                                                  
077700     SKIP1                                                                
077800     MOVE   'WLFSGA12 '        TO SSA1                                    
077900     MOVE '  GE' TO GODK-STATUSKODER                                      
078000     CALL CBLTDLI USING GNP FSGA-PCB DLI-IO-AREA SSA1                     
078100     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
078200     PERFORM IMS-STATUSKONTROLL                                           
078300     .                                                                    
078400     SKIP3                                                                
078500 IMS-GNP-FSGA13 SECTION.                                                  
078600     SKIP1                                                                
078700     MOVE   'WLFSGA13 '        TO SSA1                                    
078800     MOVE '  GE' TO GODK-STATUSKODER                                      
078900     CALL CBLTDLI USING GNP FSGA-PCB DLI-IO-AREA SSA1                     
079000     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
079100     PERFORM IMS-STATUSKONTROLL                                           
079200     .                                                                    
079300     SKIP3                                                                
079400 IMS-GNP-FSGA14 SECTION.                                                  
079500     SKIP1                                                                
079600     MOVE   'WLFSGA14 '        TO SSA1                                    
079700     MOVE '  GE' TO GODK-STATUSKODER                                      
079800     CALL CBLTDLI USING GNP FSGA-PCB DLI-IO-AREA SSA1                     
079900     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
080000     PERFORM IMS-STATUSKONTROLL                                           
080100     .                                                                    
080200     SKIP3                                                                
080300 IMS-GNP-FSGA15 SECTION.                                                  
080400     SKIP1                                                                
080500     MOVE   'WLFSGA15 '        TO SSA1                                    
080600     MOVE '  GE' TO GODK-STATUSKODER                                      
080700     CALL CBLTDLI USING GNP FSGA-PCB DLI-IO-AREA SSA1                     
080800     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
080900     PERFORM IMS-STATUSKONTROLL                                           
081000     .                                                                    
081100     SKIP3                                                                
081200 IMS-DLET-FSGA   SECTION.                                                 
081300     MOVE '  ' TO GODK-STATUSKODER                                        
081400     CALL CBLTDLI USING DLET FSGA-PCB DLI-IO-AREA                         
081500     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
081600     PERFORM IMS-STATUSKONTROLL                                           
081700     .                                                                    
081800     SKIP3                                                                
081900 IMS-STATUSKONTROLL SECTION.                                              
082000     SKIP1                                                                
082100     SET STATUS-IX TO 1                                                   
082200     SEARCH GODK-STATUS AT END CALL FELLOG                                
082300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS NEXT SENTENCE             
082400     END-SEARCH                                                           
082500     .                                                                    

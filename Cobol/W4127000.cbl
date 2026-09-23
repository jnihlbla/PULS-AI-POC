000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4127000.                                                 
000400 AUTHOR.        ELAINE CURTSSON.                                          
000500 DATE-WRITTEN.  MARS 1993.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    FUNKTION:                                                            
001100*                                                                         
001200*        PROGRAMMET LÄSER WDE4 MED SB.                                    
001300*        SKRIVER EN POST PER DIREKTLEVERANSRAD OCH EJ                     
001310*        FÄRDIGPACKAD ORDER SOM ÄR ÄLDRE ÄN EN VECKA.                     
001400*                                                                         
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900*                                                                         
002000 FILE-CONTROL.                                                            
002100                                                                          
002200*                                                                         
002300     SELECT W41270    ASSIGN TO W41270D1.                                 
002900*                                                                         
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400                                                                          
003500 FD  W41270                                                               
003600     LABEL RECORD    STANDARD                                             
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS 0.                                                    
003900                                                                          
004000*01  UT-POST  -COPY W41270   -L                                           
004100     SKIP3                                                                
008200                                                                          
008300 WORKING-STORAGE SECTION.                                                 
008301*    -COPY WY2000W1                                                       
008310     SKIP3                                                                
008400*                                                                         
008500 77   PROGRAM-NAMN               PIC X(6)    VALUE 'W41270'.              
008600                                                                          
008700 77  JA                          PIC X(1)    VALUE 'J'.                   
008800 77  NEJ                         PIC X(1)    VALUE 'N'.                   
009800*                                                                         
009900 77  SPAR-IDDISTR                PIC S9(5) COMP-3.                        
010000 77  SPAR-IDKUNDNR               PIC S9(7) COMP-3.                        
010100 77  SPAR-IDORDNR5               PIC 9(5).                                
010120 77  SPAR-TIORDREG               PIC S9(7) COMP-3.                        
010200*                                                                         
010201 01  W-TIAAVVD                   PIC 9(5).                                
010202 01  FILLER REDEFINES W-TIAAVVD.                                          
010203     03 W-TIAA                   PIC 9(2).                                
010204     03 W-TIVV                   PIC 9(2).                                
010205     03 W-TID                    PIC 9.                                   
010206*                                                                         
010207 01  JMFR-DATUM                  PIC 9(6).                                
010208*                                                                         
010210 01  ORDER-SKA-MED-SW            PIC X(1).                                
010220     88  ORDER-SKA-MED           VALUE 'J'.                               
010221*      --- VALID IDDC CODES                                               
010222*                                                                         
010223*01    -COPY WWDC99                                                       
010224       EJECT                                                              
010230*                                                                         
010300 01  DYNAMISKA-SUBPROGRAM.                                                
010400*                                                                         
010500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
010700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011400     EJECT                                                                
011410*    ----  PARAMETRAR TILL DATUMKORT                                      
011420                                                                          
011430 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
011440     SKIP3                                                                
011450*    -COPY WDATAREAC0                                                     
011460*++INCLUDE WDATAREAC0                                                     
011470     EJECT                                                                
011500 01  FILLER                      PIC X(8)    VALUE 'UT-AREA '.            
011600     SKIP2                                                                
011700*01  AREA -COPY W41270  -PRE UT-                                          
012800     EJECT                                                                
012900 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
013000                                                                          
013100 01  IMS-WS.                                                              
013200                                                                          
013300     03  STATUS-WS               PIC X(2).                                
013400        88  SEGMENT-FINNS                    VALUE '  '.                  
013500        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
013600        88  SEGMENT-SLUT                     VALUE 'GB'.                  
013700                                                                          
013800     03  GODK-STATUSKODER.                                                
013900         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
014000                                                                          
014100     EJECT                                                                
014200*01  -COPY W0003                                                          
014300     EJECT                                                                
014400*01  -COPY W0005    -PRE POSTSUM-                                         
014500     EJECT                                                                
014600 01  DLI-IO-AREA.                                                         
014700     03  IO-AREA             PIC X(500).                                  
014800     SKIP3                                                                
014900*    03  WDE401    -COPY WDE401    -RED IO-AREA                           
015000     EJECT                                                                
015300*    03  WDE411    -COPY WDE411    -RED IO-AREA                           
015400     EJECT                                                                
015700 LINKAGE SECTION.                                                         
015800     SKIP3                                                                
015900*01  -COPY W0008   -PRE WDE4-                                             
016000         05  FILLER          PIC X(1).                                    
016100     EJECT                                                                
016200 PROCEDURE DIVISION  USING WDE4-PCB.                                      
016300     ENTRY 'DLITCBL' USING WDE4-PCB.                                      
016400                                                                          
016500     PERFORM A-INIT                                                       
016600     PERFORM IMS-GET-WDE4                                                 
016700                                                                          
016800     PERFORM UNTIL SEGMENT-SLUT                                           
016900                                                                          
017000       EVALUATE WDE4-SEG-NAME-FB                                          
017100         WHEN 'WDE401'                                                    
017200           PERFORM B-KOLLA-STATUS                                         
018800         WHEN 'WDE411'                                                    
018810           IF ORDER-SKA-MED                                               
018900              PERFORM C-KOLLA-DIREKTLEV                                   
019000           END-IF                                                         
019400                                                                          
019700       END-EVALUATE                                                       
019800                                                                          
019900       PERFORM IMS-GET-WDE4                                               
020000     END-PERFORM                                                          
020100                                                                          
020200     IF ORDER-SKA-MED                                                     
020300       PERFORM C-KOLLA-DIREKTLEV                                          
020400     END-IF                                                               
020500                                                                          
020600     PERFORM Z-FINIT                                                      
020700     MOVE ZERO TO RETURN-CODE                                             
020800     GOBACK                                                               
020900     .                                                                    
021000     EJECT                                                                
021100 A-INIT SECTION.                                                          
021200                                                                          
021300     OPEN OUTPUT W41270                                                   
021900                                                                          
022000     MOVE PROGRAM-NAMN     TO POSTSUM-PROGNAMN                            
022001                                                                          
022010     MOVE 'IDAG'       TO DAT-KDDATFORM                                   
022020     CALL WDATKONV USING  DAT-KDDATFORM                                   
022030                          DAT-I-TIDATUM                                   
022040                          DAT-O-TIDATUM                                   
022050                          DAT-KDSVAR                                      
022060                                                                          
022070     MOVE DAT-TIAAVVD  TO W-TIAAVVD                                       
022080     SUBTRACT 1 FROM W-TIVV                                               
022090     IF W-TIVV = ZERO                                                     
022091        MOVE 52 TO W-TIVV                                                 
022092        IF W-TIAA = 00                                                    
022093          MOVE 99 TO W-TIAA                                               
022094        ELSE                                                              
022095          SUBTRACT 1 FROM W-TIAA                                          
022096        END-IF                                                            
022097     END-IF                                                               
022098                                                                          
022099     MOVE 'AAVVD'      TO DAT-KDDATFORM                                   
022100     MOVE W-TIAAVVD    TO DAT-I-TIDATUM                                   
022101     CALL WDATKONV USING  DAT-KDDATFORM                                   
022102                          DAT-I-TIDATUM                                   
022103                          DAT-O-TIDATUM                                   
022104                          DAT-KDSVAR                                      
022105                                                                          
022106     MOVE DAT-TIAAMMDD TO JMFR-DATUM                                      
022107     DISPLAY ' DATUM 1 VECKA BAKÅT ' JMFR-DATUM                           
022200                                                                          
024300     .                                                                    
036100     EJECT                                                                
036130 B-KOLLA-STATUS SECTION.                                                  
036131                                                                          
036132     MOVE KORD-IDDC       TO WS-IDDC                                      
036133     IF CDC-SE                                                            
036134       MOVE KORD-TIORDREG   TO TMP1-YYMMDD                                
036135       MOVE JMFR-DATUM      TO TMP2-YYMMDD                                
036136       PERFORM WY2000P1                                                   
036138       IF KORD-KVORDRAD-PACK < KORD-KVORDRAD-LEVPL                        
036139             AND TMP1-YYMMDD < TMP2-YYMMDD                                
036141          MOVE JA TO ORDER-SKA-MED-SW                                     
036142          MOVE KORD-IDDISTR   TO SPAR-IDDISTR                             
036143          MOVE KORD-IDKUNDNR  TO SPAR-IDKUNDNR                            
036144          MOVE KORD-IDORDNR5  TO SPAR-IDORDNR5                            
036145          MOVE KORD-TIORDREG  TO SPAR-TIORDREG                            
036146       ELSE                                                               
036147          MOVE NEJ TO ORDER-SKA-MED-SW                                    
036148       END-IF                                                             
036149     ELSE                                                                 
036150        MOVE NEJ TO ORDER-SKA-MED-SW                                      
036151     END-IF                                                               
036152                                                                          
036153     .                                                                    
036160     EJECT                                                                
036200 C-KOLLA-DIREKTLEV      SECTION.                                          
036300                                                                          
036301*SOFTWARE LEVNR 1441 SKA EJ MED                                           
036310     IF ORAD-IDLEVNR = SPACE OR                                           
036311     ((ORAD-IDLEVNR = '1441 ' OR ORAD-IDLEVNR = 'BP2TW')                  
036312      AND ORAD-IDBIL > SPACE)                                             
036320        CONTINUE                                                          
036330     ELSE                                                                 
036331        IF ORAD-KDRADSTA < 4                                              
036332           MOVE ORAD-IDLEVNR      TO UT-IDLEVNR                           
036340           MOVE SPAR-IDDISTR      TO UT-IDDISTR                           
036350           MOVE SPAR-IDKUNDNR     TO UT-IDKUNDNR                          
036351           MOVE SPAR-IDORDNR5     TO UT-IDORDNR5                          
036370           MOVE ORAD-IDPRODNR     TO UT-IDPRODNR                          
036600           MOVE ORAD-IDARTNR      TO UT-IDARTNR                           
039100           MOVE ORAD-KVBEART      TO UT-KVBEART                           
039210           MOVE SPAR-TIORDREG     TO UT-TIORDREG                          
039220           PERFORM CA-SKRIV-POST                                          
039230        END-IF                                                            
039300     END-IF                                                               
040300                                                                          
040310     .                                                                    
040320     EJECT                                                                
040330 CA-SKRIV-POST      SECTION.                                              
040340                                                                          
040400     WRITE UT-POST          FROM UT-AREA                                  
040500                                                                          
040600     MOVE 'W41270'          TO POSTSUM-FDNAMN                             
040700     MOVE 'W41270D1'        TO POSTSUM-DDNAMN2                            
040800     MOVE '   '             TO POSTSUM-TRANSTYP                           
040900                                                                          
041000     CALL POSTSUM USING POSTSUM-PARM                                      
041100     .                                                                    
041200     EJECT                                                                
059200 Z-FINIT  SECTION.                                                        
059300                                                                          
059400     CLOSE W41270                                                         
060000                                                                          
060100     MOVE 'S'          TO POSTSUM-OPKOD                                   
060200                                                                          
060300     CALL POSTSUM USING POSTSUM-PARM                                      
060400     .                                                                    
060500     EJECT                                                                
060600                                                                          
060700*         * I M S  S E C T I O N                                          
060800                                                                          
060900                                                                          
061000 IMS-GET-WDE4             SECTION.                                        
061100                                                                          
061200     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
061300     CALL CBLTDLI USING GN WDE4-PCB IO-AREA                               
061400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
061500     PERFORM IMS-STATUSKONTROLL.                                          
061600     SKIP3                                                                
061700                                                                          
061800 IMS-STATUSKONTROLL       SECTION.                                        
061900                                                                          
062000     SET STATUS-IX TO 1                                                   
062100     SEARCH GODK-STATUS AT END CALL FELLOG                                
062200     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
062300     END-SEARCH.                                                          
062310     EJECT                                                                
062400*    -COPY WY2000P1                                                       
